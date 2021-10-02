/* picoc variable storage. This provides ways of defining and accessing
 * variables */
module picocd.variable;

import core.stdc.string: memcpy, memset, strncpy, strlen;

import picocd.interpreter;
import picocd.table;
import picocd.platform;
import picocd.heap;
import picocd.parse;
import picocd.type;

@nogc:

/* maximum size of a value to temporarily copy while we create a variable */
enum MAX_TMP_COPY_BUF = 256;


/* initialize the variable system */
void VariableInit(Picoc *pc)
{
    TableInitTable(&(pc.GlobalTable), pc.GlobalHashTable.ptr, GLOBAL_TABLE_SIZE, true);

    TableInitTable(&pc.StringLiteralTable, pc.StringLiteralHashTable.ptr, STRING_LITERAL_TABLE_SIZE, true);
    pc.TopStackFrame = NULL;
}

/* deallocate the contents of a variable */
void VariableFree(Picoc *pc, Value *Val)
{
    if (Val.ValOnHeap || Val.AnyValOnHeap) {
        /* free function bodies */
        if (Val.Typ == &pc.FunctionType &&
                Val.Val.FuncDef_.Intrinsic == NULL &&
                Val.Val.FuncDef_.Body.Pos != NULL)
            HeapFreeMem(pc, cast(void*)Val.Val.FuncDef_.Body.Pos);

        /* free macro bodies */
        if (Val.Typ == &pc.MacroType)
            HeapFreeMem(pc, cast(void*)Val.Val.MacroDef_.Body.Pos);

        /* free the AnyValue */
        if (Val.AnyValOnHeap)
            HeapFreeMem(pc, Val.Val);
    }

    /* free the value */
    if (Val.ValOnHeap)
        HeapFreeMem(pc, Val);
}

/* deallocate the global table and the string literal table */
void VariableTableCleanup(Picoc *pc, Table *HashTable)
{
    int Count;
    TableEntry *Entry;
    TableEntry *NextEntry;

    for (Count = 0; Count < HashTable.Size; Count++) {
        for (Entry = HashTable.HashTable[Count];
                Entry != NULL;
                Entry = NextEntry) {
            NextEntry = Entry.Next;
            VariableFree(pc, Entry.p.v.Val);

            /* free the hash table entry */
            HeapFreeMem(pc, Entry);
        }
    }
}

void VariableCleanup(Picoc *pc)
{
    VariableTableCleanup(pc, &pc.GlobalTable);
    VariableTableCleanup(pc, &pc.StringLiteralTable);
}

/* allocate some memory, either on the heap or the stack
    and check if we've run out */
void *VariableAlloc(Picoc *pc, ParseState *Parser, size_t Size, int OnHeap)
{
    void *NewValue;

    if (OnHeap)
        NewValue = HeapAllocMem(pc, cast(int) Size);
    else
        NewValue = HeapAllocStack(pc, cast(int) Size);

    if (NewValue == NULL)
        ProgramFail(Parser, "(VariableAlloc) out of memory");

    return NewValue;
}

/* allocate a value either on the heap or the stack using space
    dependent on what type we want */
Value *VariableAllocValueAndData(Picoc *pc, ParseState *Parser,
    int DataSize, int IsLValue, Value *LValueFrom, int OnHeap)
{
    Value *NewValue = cast(Value*) VariableAlloc(pc, Parser,
        MEM_ALIGN((Value.sizeof)) + DataSize, OnHeap);
    NewValue.Val = cast(AnyValue*)(cast(char*)NewValue +
        MEM_ALIGN((Value.sizeof)));
    NewValue.ValOnHeap = cast(char)OnHeap;
    NewValue.AnyValOnHeap = false;
    NewValue.ValOnStack = !OnHeap;
    NewValue.IsLValue = cast(char)IsLValue;
    NewValue.LValueFrom = LValueFrom;
    if (Parser)
        NewValue.ScopeID = Parser.ScopeID;

    NewValue.OutOfScope = false;

    return NewValue;
}

/* allocate a value given its type */
Value *VariableAllocValueFromType(Picoc *pc, ParseState *Parser,
    ValueType *Typ, int IsLValue, Value *LValueFrom, int OnHeap)
{
    int Size = TypeSize(Typ, Typ.ArraySize, false);
    Value *NewValue = VariableAllocValueAndData(pc, Parser, Size,
        IsLValue, LValueFrom, OnHeap);
    assert(Size >= 0 || Typ == &pc.VoidType);
    NewValue.Typ = Typ;

    return NewValue;
}

/* allocate a value either on the heap or the stack and copy
    its value. handles overlapping data */
Value *VariableAllocValueAndCopy(Picoc *pc, ParseState *Parser,
    Value *FromValue, int OnHeap)
{
    int CopySize = TypeSizeValue(FromValue, true);
    char[MAX_TMP_COPY_BUF] TmpBuf;
    ValueType *DType = FromValue.Typ;
    Value *NewValue;

    assert(CopySize <= MAX_TMP_COPY_BUF);
    memcpy(cast(void*)&TmpBuf[0], cast(void*)FromValue.Val, CopySize);
    NewValue = VariableAllocValueAndData(pc, Parser, CopySize,
        FromValue.IsLValue, FromValue.LValueFrom, OnHeap);
    NewValue.Typ = DType;
    memcpy(cast(void*)NewValue.Val, cast(void*)&TmpBuf[0], CopySize);

    return NewValue;
}

/* allocate a value either on the heap or the stack from an
    existing AnyValue and type */
Value *VariableAllocValueFromExistingData(ParseState *Parser,
    ValueType *Typ, AnyValue *FromValue, int IsLValue,
    Value *LValueFrom)
{
    Value *NewValue = cast(Value*) VariableAlloc(Parser.pc, Parser, Value.sizeof, false);
    NewValue.Typ = Typ;
    NewValue.Val = FromValue;
    NewValue.ValOnHeap = false;
    NewValue.AnyValOnHeap = false;
    NewValue.ValOnStack = false;
    NewValue.IsLValue = cast(char)IsLValue;
    NewValue.LValueFrom = LValueFrom;

    return NewValue;
}

/* allocate a value either on the heap or the stack from an
    existing Value, sharing the value */
Value *VariableAllocValueShared(ParseState *Parser,
    Value *FromValue)
{
    return VariableAllocValueFromExistingData(Parser, FromValue.Typ,
        FromValue.Val, FromValue.IsLValue,
        FromValue.IsLValue ? FromValue : NULL);
}

/* reallocate a variable so its data has a new size */
void VariableRealloc(ParseState *Parser, Value *FromValue,
    int NewSize)
{
    if (FromValue.AnyValOnHeap)
        HeapFreeMem(Parser.pc, FromValue.Val);

    FromValue.Val = cast(AnyValue*) VariableAlloc(Parser.pc, Parser, NewSize, true);
    FromValue.AnyValOnHeap = true;
}

int VariableScopeBegin(ParseState *Parser, int* OldScopeID)
{
    int Count;
    TableEntry *Entry;
    TableEntry *NextEntry;

    if (Parser.ScopeID == -1)
        return -1;

    Table *HashTable = (Parser.pc.TopStackFrame == NULL) ?
        &(Parser.pc.GlobalTable) : &(Parser.pc.TopStackFrame).LocalTable;

    /* XXX dumb hash, let's hope for no collisions... */
    *OldScopeID = Parser.ScopeID;
    Parser.ScopeID = cast(int)( cast(size_t)(Parser.SourceText) * (cast(int)cast(size_t)(Parser.Pos) / (char*).sizeof) );
    /* or maybe a more human-readable hash for debugging? */
    /* Parser.ScopeID = Parser.Line * 0x10000 + Parser.CharacterPos; */

    for (Count = 0; Count < HashTable.Size; Count++) {
        for (Entry = HashTable.HashTable[Count];
                Entry != NULL; Entry = NextEntry) {
            NextEntry = Entry.Next;
            if (Entry.p.v.Val.ScopeID == Parser.ScopeID &&
                    Entry.p.v.Val.OutOfScope == true) {
                Entry.p.v.Val.OutOfScope = false;
                Entry.p.v.Key = cast(char*)(cast(size_t)Entry.p.v.Key & ~1);
            }
        }
    }

    return Parser.ScopeID;
}

void VariableScopeEnd(ParseState *Parser, int ScopeID, int PrevScopeID)
{
    int Count;
    TableEntry *Entry;
    TableEntry *NextEntry = NULL;

    if (ScopeID == -1)
        return;

    Table *HashTable = (Parser.pc.TopStackFrame == NULL) ?
        &(Parser.pc.GlobalTable) : &(Parser.pc.TopStackFrame).LocalTable;

    for (Count = 0; Count < HashTable.Size; Count++) {
        for (Entry = HashTable.HashTable[Count]; Entry != NULL;
            Entry = NextEntry) {
            NextEntry = Entry.Next;
            if ((Entry.p.v.Val.ScopeID == ScopeID) &&
                    (Entry.p.v.Val.OutOfScope == false)) {
                Entry.p.v.Val.OutOfScope = true;
                Entry.p.v.Key = cast(char*)(cast(size_t)Entry.p.v.Key | 1); /* alter the key so it won't be found by normal searches */
            }
        }
    }

    Parser.ScopeID = PrevScopeID;
}

int VariableDefinedAndOutOfScope(Picoc *pc, const char* Ident)
{
    int Count;
    TableEntry *Entry;

    Table * HashTable = (pc.TopStackFrame == NULL) ?
        &(pc.GlobalTable) : &(pc.TopStackFrame).LocalTable;

    for (Count = 0; Count < HashTable.Size; Count++) {
        for (Entry = HashTable.HashTable[Count]; Entry != NULL;
            Entry = Entry.Next) {
            if (Entry.p.v.Val.OutOfScope == true &&
                    cast(char*)(cast(size_t)Entry.p.v.Key & ~1) == Ident)
                return true;
        }
    }
    return false;
}

/* define a variable. Ident must be registered */
Value *VariableDefine(Picoc *pc, ParseState *Parser, char *Ident,
    Value *InitValue, ValueType *Typ, int MakeWritable)
{
    int ScopeID = Parser ? Parser.ScopeID : -1;
    Value * AssignValue;
    Table * currentTable = (pc.TopStackFrame == NULL) ?
        &(pc.GlobalTable) : &(pc.TopStackFrame).LocalTable;

    if (InitValue != NULL)
        AssignValue = VariableAllocValueAndCopy(pc, Parser, InitValue,
            pc.TopStackFrame == NULL);
    else
        AssignValue = VariableAllocValueFromType(pc, Parser, Typ, MakeWritable,
            NULL, pc.TopStackFrame == NULL);

    AssignValue.IsLValue = cast(char)MakeWritable;
    AssignValue.ScopeID = ScopeID;
    AssignValue.OutOfScope = false;

    if (!TableSet(pc, currentTable, Ident, AssignValue, Parser ?
            (cast(char*)Parser.FileName) : NULL, Parser ? Parser.Line : 0,
            Parser ? Parser.CharacterPos : 0))
        ProgramFail(Parser, "'%s' is already defined", Ident);

    return AssignValue;
}

/* define a variable. Ident must be registered. If it's a redefinition
    from the same declaration don't throw an error */
Value *VariableDefineButIgnoreIdentical(ParseState *Parser,
    char *Ident, ValueType *Typ, int IsStatic, int *FirstVisit)
{
    int DeclLine;
    int DeclColumn;
    const(char) *DeclFileName;
    Picoc *pc = Parser.pc;
    Value *ExistingValue;

    /* is the type a forward declaration? */
    if (TypeIsForwardDeclared(Parser, Typ))
        ProgramFail(Parser, "type '%t' isn't defined", Typ);

    if (IsStatic) {
        char[LINEBUFFER_MAX] MangledName;
        char *MNPos = &MangledName[0];
        char *MNEnd = &MangledName[LINEBUFFER_MAX-1];
        const(char)*RegisteredMangledName;

        /* make the mangled static name (avoiding using sprintf()
            to minimise library impact) */
        memset(cast(void*)&MangledName, '\0', (MangledName.sizeof));
        *MNPos++ = '/';
        strncpy(MNPos, cast(char*)Parser.FileName, MNEnd - MNPos);
        MNPos += strlen(MNPos);

        if (pc.TopStackFrame != NULL) {
            /* we're inside a function */
            if (MNEnd - MNPos > 0)
                *MNPos++ = '/';
            strncpy(MNPos, cast(char*)pc.TopStackFrame.FuncName, MNEnd - MNPos);
            MNPos += strlen(MNPos);
        }

        if (MNEnd - MNPos > 0) *MNPos++ = '/';
        strncpy(MNPos, Ident, MNEnd - MNPos);
        RegisteredMangledName = TableStrRegister(pc, MangledName.ptr);

        /* is this static already defined? */
        if (!TableGet(&pc.GlobalTable, RegisteredMangledName, &ExistingValue,
                &DeclFileName, &DeclLine, &DeclColumn)) {
            /* define the mangled-named static variable store in the global scope */
            ExistingValue = VariableAllocValueFromType(Parser.pc, Parser, Typ,
                true, NULL, true);
            TableSet(pc, &pc.GlobalTable, cast(char*)RegisteredMangledName,
                ExistingValue, cast(char *)Parser.FileName, Parser.Line,
                Parser.CharacterPos);
            *FirstVisit = true;
        }

        /* static variable exists in the global scope - now make a
            mirroring variable in our own scope with the short name */
        VariableDefinePlatformVar(Parser.pc, Parser, Ident, ExistingValue.Typ,
            ExistingValue.Val, true);
        return ExistingValue;
    } else {
        if (Parser.Line != 0 && TableGet((pc.TopStackFrame == NULL) ?
                    &pc.GlobalTable : &pc.TopStackFrame.LocalTable, Ident,
                    &ExistingValue, &DeclFileName, &DeclLine, &DeclColumn)
                && DeclFileName == Parser.FileName && DeclLine == Parser.Line &&
                DeclColumn == Parser.CharacterPos)
            return ExistingValue;
        else
            return VariableDefine(Parser.pc, Parser, Ident, NULL, Typ, true);
    }
}

/* check if a variable with a given name is defined. Ident must be registered */
int VariableDefined(Picoc *pc, const char *Ident)
{
    Value *FoundValue;

    if (pc.TopStackFrame == NULL || !TableGet(&pc.TopStackFrame.LocalTable,
            Ident, &FoundValue, NULL, NULL, NULL)) {
        if (!TableGet(&pc.GlobalTable, Ident, &FoundValue, NULL, NULL, NULL))
            return false;
    }

    return true;
}

/* get the value of a variable. must be defined. Ident must be registered */
void VariableGet(Picoc *pc, ParseState *Parser, const(char)*Ident,
    Value **LVal)
{
    if (pc.TopStackFrame == NULL || !TableGet(&pc.TopStackFrame.LocalTable,
            Ident, LVal, NULL, NULL, NULL)) {
        if (!TableGet(&pc.GlobalTable, Ident, LVal, NULL, NULL, NULL)) {
            if (VariableDefinedAndOutOfScope(pc, Ident))
                ProgramFail(Parser, "'%s' is out of scope", Ident);
            else
                ProgramFail(Parser, "VariableGet Ident: '%s' is undefined", Ident);
        }
    }
}

/* define a global variable shared with a platform global. Ident will be registered */
void VariableDefinePlatformVar(Picoc *pc, ParseState *Parser, const(char)* Ident,
    ValueType *Typ, AnyValue *FromValue, bool IsWritable)
{
    Value *SomeValue = VariableAllocValueAndData(pc, NULL, 0, IsWritable,
        NULL, true);
    SomeValue.Typ = Typ;
    SomeValue.Val = FromValue;

    if (!TableSet(pc,
            (pc.TopStackFrame == NULL) ? &pc.GlobalTable : &pc.TopStackFrame.LocalTable,
            TableStrRegister(pc, Ident), SomeValue,
            Parser ? Parser.FileName : NULL,
            Parser ? Parser.Line : 0, Parser ? Parser.CharacterPos : 0))
        ProgramFail(Parser, "'%s' is already defined", Ident);
}

/* free and/or pop the top value off the stack. Var must be
    the top value on the stack! */
void VariableStackPop(ParseState *Parser, Value *Var)
{
    int Success;

    if (Var.ValOnHeap) {
        if (Var.Val != NULL)
            HeapFreeMem(Parser.pc, Var.Val);
        Success = HeapPopStack(Parser.pc, Var, (Value.sizeof));  /* free from heap */
    } else if (Var.ValOnStack)
        Success = HeapPopStack(Parser.pc, Var,
        (Value.sizeof)+TypeSizeValue(Var, false));  /* free from stack */
    else
        Success = HeapPopStack(Parser.pc, Var, (Value.sizeof));  /* value isn't our problem */

    if (!Success)
        ProgramFail(Parser, "stack underrun");
}

/* add a stack frame when doing a function call */
void VariableStackFrameAdd(ParseState *Parser, const char *FuncName, int NumParams)
{
    StackFrame *NewFrame;

    HeapPushStackFrame(Parser.pc);
    NewFrame = cast(StackFrame*) HeapAllocStack(Parser.pc, (StackFrame.sizeof)+(Value*).sizeof*NumParams);
    if (NewFrame == NULL)
        ProgramFail(Parser, "(VariableStackFrameAdd) out of memory");

    ParserCopy(&NewFrame.ReturnParser, Parser);
    NewFrame.FuncName = FuncName;
    NewFrame.Parameter = cast(Value**) (  (NumParams > 0) ? (cast(void*)(cast(char*)NewFrame+(StackFrame.sizeof))) : NULL  );
    TableInitTable(&NewFrame.LocalTable, NewFrame.LocalHashTable.ptr, LOCAL_TABLE_SIZE, false);
    NewFrame.PreviousStackFrame = Parser.pc.TopStackFrame;
    Parser.pc.TopStackFrame = NewFrame;
}

/* remove a stack frame */
void VariableStackFramePop(ParseState *Parser)
{
    if (Parser.pc.TopStackFrame == NULL)
        ProgramFail(Parser, "stack is empty - can't go back");

    ParserCopy(Parser, &Parser.pc.TopStackFrame.ReturnParser);
    Parser.pc.TopStackFrame = Parser.pc.TopStackFrame.PreviousStackFrame;
    HeapPopStackFrame(Parser.pc);
}

/* get a string literal. assumes that Ident is already
    registered. NULL if not found */
Value *VariableStringLiteralGet(Picoc *pc, char *Ident)
{
    Value *LVal = NULL;

    if (TableGet(&pc.StringLiteralTable, Ident, &LVal, NULL, NULL, NULL))
        return LVal;
    else
        return NULL;
}

/* define a string literal. assumes that Ident is already registered */
void VariableStringLiteralDefine(Picoc *pc, char *Ident, Value *Val)
{
    TableSet(pc, &pc.StringLiteralTable, Ident, Val, NULL, 0, 0);
}

/* check a pointer for validity and dereference it for use */
void *VariableDereferencePointer(Value *PointerValue, Value **DerefVal,
    int *DerefOffset, ValueType **DerefType, int *DerefIsLValue)
{
    if (DerefVal != NULL)
        *DerefVal = NULL;

    if (DerefType != NULL)
        *DerefType = PointerValue.Typ.FromType;

    if (DerefOffset != NULL)
        *DerefOffset = 0;

    if (DerefIsLValue != NULL)
        *DerefIsLValue = true;

    return PointerValue.Val.Pointer;
}

