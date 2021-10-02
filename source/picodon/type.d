/* picoc data type module. This manages a tree of data types and has facilities
 * for parsing data types. */
module picodon.type;

import core.stdc.string: memset;

import picodon.interpreter;
import picodon.variable;
import picodon.platform;
import picodon.lex;
import picodon.heap;
import picodon.table;
import picodon.parse;
import picodon.expression;

@nogc:

/* some basic types */
__gshared int PointerAlignBytes;
__gshared int IntAlignBytes;


/* add a new type to the set of types we know about */
ValueType *TypeAdd(Picoc *pc, ParseState *Parser,
    ValueType *ParentType, BaseType Base, size_t ArraySize,
    const(char)*Identifier, size_t Sizeof, size_t AlignBytes)
{
    ValueType *NewType = cast(ValueType*) VariableAlloc(pc, Parser, ValueType.sizeof, true);
    NewType.Base = Base;
    NewType.ArraySize = cast(int) ArraySize;
    NewType.Sizeof = cast(int) Sizeof;
    NewType.AlignBytes = cast(int) AlignBytes;
    NewType.Identifier = Identifier;
    NewType.Members = NULL;
    NewType.FromType = ParentType;
    NewType.DerivedTypeList = NULL;
    NewType.OnHeap = true;
    NewType.Next = ParentType.DerivedTypeList;
    ParentType.DerivedTypeList = NewType;

    return NewType;
}

/* given a parent type, get a matching derived type and make one if necessary.
 * Identifier should be registered with the shared string table. */
ValueType *TypeGetMatching(Picoc *pc, ParseState *Parser,
    ValueType *ParentType, BaseType Base, int ArraySize,
    const char *Identifier, int AllowDuplicates)
{
    int Sizeof;
    int AlignBytes;
    ValueType *ThisType = ParentType.DerivedTypeList;
    while (ThisType != NULL && (ThisType.Base != Base ||
            ThisType.ArraySize != ArraySize || ThisType.Identifier != Identifier))
        ThisType = ThisType.Next;

    if (ThisType != NULL) {
        if (AllowDuplicates)
            return ThisType;
        else
            ProgramFail(Parser, "data type '%s' is already defined", Identifier);
    }

    switch (Base) {
    case TypePointer:
        Sizeof = (void*).sizeof;
        AlignBytes = PointerAlignBytes;
        break;
    case TypeArray:
        Sizeof = ArraySize * ParentType.Sizeof;
        AlignBytes = ParentType.AlignBytes;
        break;
    case TypeEnum:
        Sizeof = (int).sizeof;
        AlignBytes = IntAlignBytes;
        break;
    default:
        Sizeof = 0; AlignBytes = 0;
        break;  /* structs and unions will get bigger
                    when we add members to them */
    }

    return TypeAdd(pc, Parser, ParentType, Base, ArraySize, Identifier, Sizeof,
        AlignBytes);
}

/* stack space used by a value */
int TypeStackSizeValue(Value *Val)
{
    if (Val != NULL && Val.ValOnStack)
        return TypeSizeValue(Val, false);
    else
        return 0;
}

/* memory used by a value */
int TypeSizeValue(Value *Val, int Compact)
{
    if (IS_INTEGER_NUMERIC(Val) && !Compact)
        return (ALIGN_TYPE.sizeof);  /* allow some extra room for type extension */
    else if (Val.Typ.Base != TypeArray)
        return Val.Typ.Sizeof;
    else
        return Val.Typ.FromType.Sizeof * Val.Typ.ArraySize;
}

/* memory used by a variable given its type and array size */
int TypeSize(ValueType *Typ, int ArraySize, int Compact)
{
    if (IS_INTEGER_NUMERIC_TYPE(Typ) && !Compact)
        return (ALIGN_TYPE.sizeof);  /* allow some extra room for type extension */
    else if (Typ.Base != TypeArray)
        return Typ.Sizeof;
    else
        return Typ.FromType.Sizeof * ArraySize;
}

/* add a base type */
void TypeAddBaseType(Picoc *pc, ValueType *TypeNode, BaseType Base,
            size_t Sizeof, size_t AlignBytes)
{
    TypeNode.Base = Base;
    TypeNode.ArraySize = 0;
    TypeNode.Sizeof = cast(int) Sizeof;
    TypeNode.AlignBytes = cast(int) AlignBytes;
    TypeNode.Identifier = pc.StrEmpty;
    TypeNode.Members = NULL;
    TypeNode.FromType = NULL;
    TypeNode.DerivedTypeList = NULL;
    TypeNode.OnHeap = false;
    TypeNode.Next = pc.UberType.DerivedTypeList;
    pc.UberType.DerivedTypeList = TypeNode;
}

/* initialize the type system */
void TypeInit(Picoc *pc)
{
    struct IntAlign {char x; int y;}
    IntAlign ia;
    struct ShortAlign {char x; short y;} 
    ShortAlign sa;
    struct CharAlign {char x; char y;} 
    CharAlign ca;
    struct LongAlign {char x; long y;}
    LongAlign la;
    struct DoubleAlign {char x; double y;}
    DoubleAlign da;
    struct PointerAlign {char x; void *y;}
    PointerAlign pa;

    IntAlignBytes = cast(int) ( cast(char*)&ia.y - &ia.x );
    PointerAlignBytes = cast(int)( cast(char*)&pa.y - &pa.x );

    pc.UberType.DerivedTypeList = NULL;
    TypeAddBaseType(pc, &pc.IntType, TypeInt, int.sizeof, IntAlignBytes);
    TypeAddBaseType(pc, &pc.ShortType, TypeShort, short.sizeof,
        cast(char*)&sa.y - &sa.x);
    TypeAddBaseType(pc, &pc.CharType, TypeChar, char.sizeof,
        cast(char*)&ca.y - &ca.x);
    TypeAddBaseType(pc, &pc.LongType, TypeLong, long.sizeof,
        cast(char*)&la.y - &la.x);
    TypeAddBaseType(pc, &pc.UnsignedIntType, TypeUnsignedInt,
        (uint.sizeof), IntAlignBytes);
    TypeAddBaseType(pc, &pc.UnsignedShortType, TypeUnsignedShort,
        (ushort.sizeof), cast(char*)&sa.y - &sa.x);
    TypeAddBaseType(pc, &pc.UnsignedLongType, TypeUnsignedLong,
        (ulong.sizeof), cast(char*)&la.y - &la.x);
    TypeAddBaseType(pc, &pc.UnsignedCharType, TypeUnsignedChar,
        (char.sizeof), cast(char*)&ca.y - &ca.x);
    TypeAddBaseType(pc, &pc.VoidType, TypeVoid, 0, 1);
    TypeAddBaseType(pc, &pc.FunctionType, TypeFunction, int.sizeof,
        IntAlignBytes);
    TypeAddBaseType(pc, &pc.MacroType, TypeMacro, int.sizeof, IntAlignBytes);
    TypeAddBaseType(pc, &pc.GotoLabelType, TypeGotoLabel, 0, 1);
    TypeAddBaseType(pc, &pc.FPType, TypeFP, (double.sizeof),
        cast(char*)&da.y - &da.x);
    TypeAddBaseType(pc, &pc.TypeType, Type_Type, (double.sizeof),
    cast(char*)&da.y - &da.x);  /* must be large enough to cast to a double */
    pc.CharArrayType = TypeAdd(pc, NULL, &pc.CharType, TypeArray, 0,
        pc.StrEmpty, (char.sizeof), cast(char*)&ca.y - &ca.x);
    pc.CharPtrType = TypeAdd(pc, NULL, &pc.CharType, TypePointer, 0,
        pc.StrEmpty, (void*).sizeof, PointerAlignBytes);
    pc.CharPtrPtrType = TypeAdd(pc, NULL, pc.CharPtrType, TypePointer, 0,
        pc.StrEmpty, (void*).sizeof, PointerAlignBytes);
    pc.VoidPtrType = TypeAdd(pc, NULL, &pc.VoidType, TypePointer, 0,
        pc.StrEmpty, (void*).sizeof, PointerAlignBytes);
}

/* deallocate heap-allocated types */
void TypeCleanupNode(Picoc *pc, ValueType *Typ)
{
    ValueType *SubType;
    ValueType *NextSubType;

    /* clean up and free all the sub-nodes */
    for (SubType = Typ.DerivedTypeList; SubType != NULL;
            SubType = NextSubType) {
        NextSubType = SubType.Next;
        TypeCleanupNode(pc, SubType);
        if (SubType.OnHeap) {
            /* if it's a struct or union deallocate all the member values */
            if (SubType.Members != NULL) {
                VariableTableCleanup(pc, SubType.Members);
                HeapFreeMem(pc, SubType.Members);
            }

            /* free this node */
            HeapFreeMem(pc, SubType);
        }
    }
}

void TypeCleanup(Picoc *pc)
{
    TypeCleanupNode(pc, &pc.UberType);
}

/* parse a struct or union declaration */
void TypeParseStruct(ParseState *Parser, ValueType **Typ,
    int IsStruct)
{
    char *MemberIdentifier;
    char *StructIdentifier;
    LexToken Token;
    int AlignBoundary;
    Value *MemberValue;
    Picoc *pc = Parser.pc;
    Value *LexValue;
    ValueType *MemberType;

    Token = LexGetToken(Parser, &LexValue, false);
    if (Token == TokenIdentifier) {
        LexGetToken(Parser, &LexValue, true);
        StructIdentifier = LexValue.Val.Identifier;
        Token = LexGetToken(Parser, NULL, false);
    } 
    else 
    {
        StructIdentifier = PlatformMakeTempName(pc, pc.lastAnonymousIdentStruct.ptr);
    }

    *Typ = TypeGetMatching(pc, Parser, &Parser.pc.UberType,
        IsStruct ? TypeStruct : TypeUnion, 0, StructIdentifier, true);
    if (Token == TokenLeftBrace && (*Typ).Members != NULL)
        ProgramFail(Parser, "data type '%t' is already defined", *Typ);

    Token = LexGetToken(Parser, NULL, false);
    if (Token != TokenLeftBrace) {
        /* use the already defined structure */
        return;
    }

    if (pc.TopStackFrame != NULL)
        ProgramFail(Parser, "struct/union definitions can only be globals");

    LexGetToken(Parser, NULL, true);
    (*Typ).Members = cast(Table*) VariableAlloc(pc, Parser, (Table.sizeof)+STRUCT_TABLE_SIZE*(TableEntry.sizeof), true);
    (*Typ).Members.HashTable = cast(TableEntry**)(cast(char*)(*Typ).Members + (Table.sizeof));
    TableInitTable((*Typ).Members, cast(TableEntry**)(cast(char*)(*Typ).Members + (Table.sizeof)),
        STRUCT_TABLE_SIZE, true);

    do {
        TypeParse(Parser, &MemberType, &MemberIdentifier, NULL);
        if (MemberType == NULL || MemberIdentifier == NULL)
            ProgramFail(Parser, "invalid type in struct");

        MemberValue = VariableAllocValueAndData(pc, Parser, int.sizeof, false,
            NULL, true);
        MemberValue.Typ = MemberType;
        if (IsStruct) {
            /* allocate this member's location in the struct */
            AlignBoundary = MemberValue.Typ.AlignBytes;
            if (((*Typ).Sizeof & (AlignBoundary-1)) != 0)
                (*Typ).Sizeof +=
                    AlignBoundary - ((*Typ).Sizeof & (AlignBoundary-1));

            MemberValue.Val.Integer = (*Typ).Sizeof;
            (*Typ).Sizeof += TypeSizeValue(MemberValue, true);
        } else {
            /* union members always start at 0, make sure it's big enough
                to hold the largest member */
            MemberValue.Val.Integer = 0;
            if (MemberValue.Typ.Sizeof > (*Typ).Sizeof)
                (*Typ).Sizeof = TypeSizeValue(MemberValue, true);
        }

        /* make sure to align to the size of the largest member's alignment */
        if ((*Typ).AlignBytes < MemberValue.Typ.AlignBytes)
            (*Typ).AlignBytes = MemberValue.Typ.AlignBytes;

        /* define it */
        if (!TableSet(pc, (*Typ).Members, MemberIdentifier, MemberValue,
                Parser.FileName, Parser.Line, Parser.CharacterPos))
            ProgramFail(Parser, "member '%s' already defined", &MemberIdentifier);

        if (LexGetToken(Parser, NULL, true) != TokenSemicolon)
            ProgramFail(Parser, "semicolon expected");

    } while (LexGetToken(Parser, NULL, false) != TokenRightBrace);

    /* now align the structure to the size of its largest member's alignment */
    AlignBoundary = (*Typ).AlignBytes;
    if (((*Typ).Sizeof & (AlignBoundary-1)) != 0)
        (*Typ).Sizeof += AlignBoundary - ((*Typ).Sizeof & (AlignBoundary-1));

    LexGetToken(Parser, NULL, true);
}

/* create a system struct which has no user-visible members */
ValueType *TypeCreateOpaqueStruct(Picoc *pc, ParseState *Parser,
    const char *StructName, int Size)
{
    ValueType *Typ = TypeGetMatching(pc, Parser, &pc.UberType,
        TypeStruct, 0, StructName, false);

    /* create the (empty) table */
    Typ.Members = cast(Table*) VariableAlloc(pc,
        Parser,
        (Table.sizeof)+STRUCT_TABLE_SIZE*(TableEntry.sizeof), true);
    Typ.Members.HashTable = cast(TableEntry**)(cast(char*)Typ.Members +
        (Table.sizeof));
    TableInitTable(Typ.Members,
        cast(TableEntry**)(cast(char*)Typ.Members+(Table.sizeof)),
        STRUCT_TABLE_SIZE, true);
    Typ.Sizeof = Size;

    return Typ;
}

/* parse an enum declaration */
void TypeParseEnum(ParseState *Parser, ValueType **Typ)
{
    int EnumValue = 0;
    char *EnumIdentifier;
    LexToken Token;
    Value *LexValue;
    Value InitValue;
    Picoc *pc = Parser.pc;

    Token = LexGetToken(Parser, &LexValue, false);
    if (Token == TokenIdentifier) {
        LexGetToken(Parser, &LexValue, true);
        EnumIdentifier = LexValue.Val.Identifier;
        Token = LexGetToken(Parser, NULL, false);
    } else {
        EnumIdentifier = PlatformMakeTempName(pc, pc.lastAnonymousIdentEnum.ptr);
    }

    TypeGetMatching(pc, Parser, &pc.UberType, TypeEnum, 0, EnumIdentifier,
        Token != TokenLeftBrace);
    *Typ = &pc.IntType;
    if (Token != TokenLeftBrace) {
        /* use the already defined enum */
        if ((*Typ).Members == NULL)
            ProgramFail(Parser, "enum '%s' isn't defined", EnumIdentifier);

        return;
    }

    if (pc.TopStackFrame != NULL)
        ProgramFail(Parser, "enum definitions can only be globals");

    LexGetToken(Parser, NULL, true);
    (*Typ).Members = &pc.GlobalTable;
    memset(cast(void*)&InitValue, '\0', Value.sizeof);
    InitValue.Typ = &pc.IntType;
    InitValue.Val = cast(AnyValue*)&EnumValue;
    do {
        if (LexGetToken(Parser, &LexValue, true) != TokenIdentifier)
            ProgramFail(Parser, "identifier expected");

        EnumIdentifier = LexValue.Val.Identifier;
        if (LexGetToken(Parser, NULL, false) == TokenAssign) {
            LexGetToken(Parser, NULL, true);
            EnumValue = cast(int) ExpressionParseInt(Parser);
        }

        VariableDefine(pc, Parser, EnumIdentifier, &InitValue, NULL, false);

        Token = LexGetToken(Parser, NULL, true);
        if (Token != TokenComma && Token != TokenRightBrace)
            ProgramFail(Parser, "comma expected");

        EnumValue++;
    } while (Token == TokenComma);
}

/* parse a type - just the basic type */
int TypeParseFront(ParseState *Parser, ValueType **Typ,
    int *IsStatic)
{
    int Unsigned = false;
    int StaticQualifier = false;
    LexToken Token;
    ParseState Before;
    Value *LexerValue;
    Value *VarValue;
    Picoc *pc = Parser.pc;
    *Typ = NULL;

    /* ignore leading type qualifiers */
    ParserCopy(&Before, Parser);
    Token = LexGetToken(Parser, &LexerValue, true);
    while (Token == TokenStaticType || Token == TokenAutoType ||
            Token == TokenRegisterType || Token == TokenExternType) {
        if (Token == TokenStaticType)
            StaticQualifier = true;

        Token = LexGetToken(Parser, &LexerValue, true);
    }

    if (IsStatic != NULL)
        *IsStatic = StaticQualifier;

    /* handle signed/unsigned with no trailing type */
    if (Token == TokenSignedType || Token == TokenUnsignedType) {
        LexToken FollowToken = LexGetToken(Parser, &LexerValue, false);
        Unsigned = (Token == TokenUnsignedType);

        if (FollowToken != TokenIntType && FollowToken != TokenLongType &&
                FollowToken != TokenShortType && FollowToken != TokenCharType) {
            if (Token == TokenUnsignedType)
                *Typ = &pc.UnsignedIntType;
            else
                *Typ = &pc.IntType;

            return true;
        }

        Token = LexGetToken(Parser, &LexerValue, true);
    }

    switch (Token) {
    case TokenIntType:
        *Typ = Unsigned ? &pc.UnsignedIntType : &pc.IntType;
        break;
    case TokenShortType:
        *Typ = Unsigned ? &pc.UnsignedShortType : &pc.ShortType;
        break;
    case TokenCharType:
        *Typ = Unsigned ? &pc.UnsignedCharType : &pc.CharType;
        break;
    case TokenLongType:
        *Typ = Unsigned ? &pc.UnsignedLongType : &pc.LongType;
        break;
    case TokenFloatType:
    case TokenDoubleType:
        *Typ = &pc.FPType;
        break;
    case TokenVoidType:
        *Typ = &pc.VoidType;
        break;
    case TokenStructType: case TokenUnionType:
        if (*Typ != NULL)
            ProgramFail(Parser, "bad type declaration");
        TypeParseStruct(Parser, Typ, Token == TokenStructType);
        break;
    case TokenEnumType:
        if (*Typ != NULL)
            ProgramFail(Parser, "bad type declaration");

        TypeParseEnum(Parser, Typ);
        break;
    case TokenIdentifier:
        /* we already know it's a typedef-defined type because we got here */
        VariableGet(pc, Parser, LexerValue.Val.Identifier, &VarValue);
        *Typ = VarValue.Val.Typ;
        break;

    default:
        ParserCopy(Parser, &Before);
        return false;
    }

    return true;
}

/* parse a type - the part at the end after the identifier. eg.
    array specifications etc. */
ValueType *TypeParseBack(ParseState *Parser,
    ValueType *FromType)
{
    LexToken Token;
    ParseState Before;

    ParserCopy(&Before, Parser);
    Token = LexGetToken(Parser, NULL, true);
    if (Token == TokenLeftSquareBracket) {
        /* add another array bound */
        if (LexGetToken(Parser, NULL, false) == TokenRightSquareBracket) {
            /* an unsized array */
            LexGetToken(Parser, NULL, true);
            return TypeGetMatching(Parser.pc, Parser,
                TypeParseBack(Parser, FromType), TypeArray, 0,
                    Parser.pc.StrEmpty, true);
        } else {
            /* get a numeric array size */
            RunMode OldMode = Parser.Mode;
            int ArraySize;
            Parser.Mode = RunModeRun;
            ArraySize = cast(int) ExpressionParseInt(Parser);
            Parser.Mode = OldMode;

            if (LexGetToken(Parser, NULL, true) != TokenRightSquareBracket)
                ProgramFail(Parser, "']' expected");

            return TypeGetMatching(Parser.pc, Parser,
                TypeParseBack(Parser, FromType), TypeArray, ArraySize,
                    Parser.pc.StrEmpty, true);
        }
    } else {
        /* the type specification has finished */
        ParserCopy(Parser, &Before);
        return FromType;
    }
}

/* parse a type - the part which is repeated with each
    identifier in a declaration list */
void TypeParseIdentPart(ParseState *Parser, ValueType *BasicTyp,
    ValueType **Typ, char **Identifier)
{
    int Done = false;
    LexToken Token;
    Value *LexValue;
    ParseState Before;
    *Typ = BasicTyp;
    *Identifier = Parser.pc.StrEmpty;

    while (!Done) {
        ParserCopy(&Before, Parser);
        Token = LexGetToken(Parser, &LexValue, true);
        switch (Token) {
        case TokenOpenBracket:
            if (*Typ != NULL)
                ProgramFail(Parser, "bad type declaration");

            TypeParse(Parser, Typ, Identifier, NULL);
            if (LexGetToken(Parser, NULL, true) != TokenCloseBracket)
                ProgramFail(Parser, "')' expected");
            break;

        case TokenAsterisk:
            if (*Typ == NULL)
                ProgramFail(Parser, "bad type declaration");

            *Typ = TypeGetMatching(Parser.pc, Parser, *Typ, TypePointer, 0,
                Parser.pc.StrEmpty, true);
            break;

        case TokenIdentifier:
            if (*Typ == NULL || *Identifier != Parser.pc.StrEmpty)
                ProgramFail(Parser, "bad type declaration");

            *Identifier = LexValue.Val.Identifier;
            Done = true;
            break;

        default: ParserCopy(Parser, &Before); Done = true; break;
        }
    }

    if (*Typ == NULL)
        ProgramFail(Parser, "bad type declaration");

    if (*Identifier != Parser.pc.StrEmpty) {
        /* parse stuff after the identifier */
        *Typ = TypeParseBack(Parser, *Typ);
    }
}

/* parse a type - a complete declaration including identifier */
void TypeParse(ParseState *Parser, ValueType **Typ,
    char **Identifier, int *IsStatic)
{
    ValueType *BasicType;

    TypeParseFront(Parser, &BasicType, IsStatic);
    TypeParseIdentPart(Parser, BasicType, Typ, Identifier);
}

/* check if a type has been fully defined - otherwise it's
    just a forward declaration */
int TypeIsForwardDeclared(ParseState *Parser, ValueType *Typ)
{
    if (Typ.Base == TypeArray)
        return TypeIsForwardDeclared(Parser, Typ.FromType);

    if ((Typ.Base == TypeStruct || Typ.Base == TypeUnion) &&
            Typ.Members == NULL)
        return true;

    return false;
}
