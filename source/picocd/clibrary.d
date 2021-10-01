module picocd.clibrary;

import core.stdc.string: strlen;
import picocd.interpreter;
import picocd.variable;
import picocd.table;
import picocd.lex;
import picocd.parse;
import picocd.heap;
import picocd.type;
import picocd.platform;

import picocd.cstdlib.stdio;


/* endian-ness checking */
__gshared int BigEndian;
__gshared int LittleEndian;


/* global initialisation for libraries */
void LibraryInit(Picoc *pc)
{

    /* define the version number macro */
    pc.VersionString = TableStrRegister(pc, PICOC_VERSION);
    VariableDefinePlatformVar(pc, NULL, "PICOC_VERSION", pc.CharPtrType,
        cast(AnyValue*)&pc.VersionString, false);

    /* define endian-ness macros */
    version(LittleEndian)
    {
        BigEndian = 0;
        LittleEndian = 1;
    }
    else
    {
        BigEndian = 1;
        LittleEndian = 0;
    }

    VariableDefinePlatformVar(pc, NULL, "BIG_ENDIAN", &pc.IntType,
        cast(AnyValue*)&BigEndian, false);
    VariableDefinePlatformVar(pc, NULL, "LITTLE_ENDIAN", &pc.IntType,
        cast(AnyValue*)&LittleEndian, false);
}

/* add a library */
void LibraryAdd(Picoc *pc, LibraryFunction *FuncList)
{
    ParseState Parser;
    int Count;
    char *Identifier;
    ValueType *ReturnType;
    Value *NewValue;
    void *Tokens;
    char *IntrinsicName = TableStrRegister(pc, "c library");

    /* read all the library definitions */
    for (Count = 0; FuncList[Count].Prototype != NULL; Count++) {
        Tokens = LexAnalyse(pc,
            cast(const char*)IntrinsicName, FuncList[Count].Prototype,
            cast(int) strlen(cast(char*)FuncList[Count].Prototype), NULL);
        LexInitParser(&Parser, pc, FuncList[Count].Prototype, Tokens,
            IntrinsicName, true, false);
        TypeParse(&Parser, &ReturnType, &Identifier, NULL);
        NewValue = ParseFunctionDefinition(&Parser, ReturnType, Identifier);
        NewValue.Val.FuncDef_.Intrinsic = cast(void function()) FuncList[Count].Func;
        HeapFreeMem(pc, Tokens);
    }
}

/* print a type to a stream without using printf/sprintf */
void PrintType(ValueType *Typ, IOFILE *Stream)
{
    switch (Typ.Base) {
    case TypeVoid:
        PrintStr("void", Stream);
        break;
    case TypeInt:
        PrintStr("int", Stream);
        break;
    case TypeShort:
        PrintStr("short", Stream);
        break;
    case TypeChar:
        PrintStr("char", Stream);
        break;
    case TypeLong:
        PrintStr("long", Stream);
        break;
    case TypeUnsignedInt:
        PrintStr("unsigned int", Stream);
        break;
    case TypeUnsignedShort:
        PrintStr("unsigned short", Stream);
        break;
    case TypeUnsignedLong:
        PrintStr("unsigned long", Stream);
        break;
    case TypeUnsignedChar:
        PrintStr("unsigned char", Stream);
        break;
    case TypeFP:
        PrintStr("double", Stream);
        break;
    case TypeFunction:
        PrintStr("function", Stream);
        break;
    case TypeMacro:
        PrintStr("macro", Stream);
        break;
    case TypePointer:
        if (Typ.FromType)
            PrintType(Typ.FromType, Stream);
        PrintCh('*', Stream);
        break;
    case TypeArray:
        PrintType(Typ.FromType, Stream);
        PrintCh('[', Stream);
        if (Typ.ArraySize != 0)
            PrintSimpleInt(Typ.ArraySize, Stream);
        PrintCh(']', Stream);
        break;
    case TypeStruct:
        PrintStr("struct ", Stream);
        PrintStr(Typ.Identifier, Stream);
        break;
    case TypeUnion:
        PrintStr("union ", Stream);
        PrintStr(Typ.Identifier, Stream);
        break;
    case TypeEnum:
        PrintStr("enum ", Stream);
        PrintStr(Typ.Identifier, Stream);
        break;
    case TypeGotoLabel:
        PrintStr("goto label ", Stream);
        break;
    case Type_Type:
        PrintStr("type ", Stream);
        break;
    default:
        assert(false);
    }
}
