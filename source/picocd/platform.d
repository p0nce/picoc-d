/* all platform-specific includes and defines go in this file */
module picocd.platform;

import core.stdc.stdio;
import core.stdc.stdarg;
import core.stdc.stdlib: malloc, free;
import core.stdc.string: memset, strlen;

import picocd.interpreter;
import picocd.heap;
import picocd.table;
import picocd.variable;
import picocd.lex;
import picocd.parse;
import picocd.type;
import picocd.include;
import picocd.clibrary;

import picocd.cstdlib.stdio;

enum PICOC_VERSION = "v2.3.2";


alias ALIGN_TYPE = void*;


enum GLOBAL_TABLE_SIZE = 97;                /* global variable table */
enum STRING_TABLE_SIZE = 97;                /* shared string table size */
enum STRING_LITERAL_TABLE_SIZE = 97;        /* string literal table size */
enum RESERVED_WORD_TABLE_SIZE = 97;         /* reserved word table size */
enum PARAMETER_MAX = 16;                    /* maximum number of parameters to a function */
enum LINEBUFFER_MAX = 256;                  /* maximum number of characters on a line */
enum LOCAL_TABLE_SIZE = 11;                 /* size of local variable table (can expand) */
enum STRUCT_TABLE_SIZE = 11;                /* size of struct/union member table (can expand) */

enum INTERACTIVE_PROMPT_START = "starting picoc " ~ PICOC_VERSION ~ " (Ctrl+D to exit)\n";
enum INTERACTIVE_PROMPT_STATEMENT = "picoc> ";
enum INTERACTIVE_PROMPT_LINE = "     > ";


enum gEnableDebugger = true;

/* initialize everything */
void PicocInitialize(Picoc *pc, int StackSize)
{
    memset(pc, 0, (*pc).sizeof);
    PlatformInit(pc);
    BasicIOInit(pc);
    HeapInit(pc, StackSize);
    TableInit(pc);
    VariableInit(pc);
    LexInit(pc);
    TypeInit(pc);
    IncludeInit(pc);
    LibraryInit(pc);
    //PlatformLibraryInit(pc);
    //static if (gEnableDebugger)
    //    DebugInit(pc);
}

/* free memory */
void PicocCleanup(Picoc *pc)
{
//    static if (gEnableDebugger)
//        DebugCleanup(pc);
    IncludeCleanup(pc);
    ParseCleanup(pc);
    LexCleanup(pc);
    VariableCleanup(pc);
    TypeCleanup(pc);
    TableStrFree(pc);
    HeapCleanup(pc);
    PlatformCleanup(pc);
}


enum CALL_MAIN_NO_ARGS_RETURN_VOID = "main();";
enum CALL_MAIN_WITH_ARGS_RETURN_VOID = "main(__argc,__argv);";
enum CALL_MAIN_NO_ARGS_RETURN_INT = "__exit_value = main();";
enum CALL_MAIN_WITH_ARGS_RETURN_INT = "__exit_value = main(__argc,__argv);";

void PicocCallMain(Picoc *pc, int argc, const(char)** argv)
{
    /* check if the program wants arguments */
    Value *FuncValue = NULL;

    if (!VariableDefined(pc, TableStrRegister(pc, "main")))
        ProgramFailNoParser(pc, "main() is not defined");

    VariableGet(pc, NULL, TableStrRegister(pc, "main"), &FuncValue);
    if (FuncValue.Typ.Base != TypeFunction)
        ProgramFailNoParser(pc, "main is not a function - can't call it");

    if (FuncValue.Val.FuncDef_.NumParams != 0) {
        /* define the arguments */
        VariableDefinePlatformVar(pc, NULL, "__argc", &pc.IntType,
            cast(AnyValue*)&argc, false);
        VariableDefinePlatformVar(pc, NULL, "__argv", pc.CharPtrPtrType,
            cast(AnyValue*)&argv, false);
    }

    if (FuncValue.Val.FuncDef_.ReturnType == &pc.VoidType) {
        if (FuncValue.Val.FuncDef_.NumParams == 0)
            PicocParse(pc, "startup", CALL_MAIN_NO_ARGS_RETURN_VOID,
                cast(int) strlen(CALL_MAIN_NO_ARGS_RETURN_VOID), true, true, false,
                gEnableDebugger);
        else
            PicocParse(pc, "startup", CALL_MAIN_WITH_ARGS_RETURN_VOID,
                cast(int) strlen(CALL_MAIN_WITH_ARGS_RETURN_VOID), true, true, false,
                gEnableDebugger);
    } else {
        VariableDefinePlatformVar(pc, NULL, "__exit_value", &pc.IntType,
            cast(AnyValue *)&pc.PicocExitValue, true);

        if (FuncValue.Val.FuncDef_.NumParams == 0)
            PicocParse(pc, "startup", CALL_MAIN_NO_ARGS_RETURN_INT,
                cast(int) strlen(CALL_MAIN_NO_ARGS_RETURN_INT), true, true, false,
                gEnableDebugger);
        else
            PicocParse(pc, "startup", CALL_MAIN_WITH_ARGS_RETURN_INT,
                cast(int) strlen(CALL_MAIN_WITH_ARGS_RETURN_INT), true, true, false,
                gEnableDebugger);
    }
}

void PrintSourceTextErrorLine(IOFILE *Stream, const(char)*FileName,
    const(char)*SourceText, int Line, int CharacterPos)
{
    int LineCount;
    int CCount;
    const(char)*LinePos;
    const(char)*CPos;

    if (SourceText != NULL) {
        /* find the source line */
        for (LinePos = SourceText, LineCount = 1; *LinePos != '\0' &&
                LineCount < Line; LinePos++) {
            if (*LinePos == '\n')
                LineCount++;
        }

        /* display the line */
        for (CPos = LinePos; *CPos != '\n' && *CPos != '\0'; CPos++)
            PrintCh(*CPos, Stream);
        PrintCh('\n', Stream);

        /* display the error position */
        for (CPos = LinePos, CCount = 0; *CPos != '\n' && *CPos != '\0' &&
                (CCount < CharacterPos || *CPos == ' '); CPos++, CCount++) {
            if (*CPos == '\t')
                PrintCh('\t', Stream);
            else
                PrintCh(' ', Stream);
        }
    } else {
        /* assume we're in interactive mode - try to make the arrow match
            up with the input text */
        for (CCount = 0;
                CCount < CharacterPos+cast(int)strlen(INTERACTIVE_PROMPT_STATEMENT);
                CCount++)
            PrintCh(' ', Stream);
    }
    PlatformPrintf(Stream, "^\n%s:%d:%d ", FileName, Line, CharacterPos);
}



/* exit with a message */
void ProgramFail(ParseState *Parser, const(char) *Message, ...)
{
    va_list Args;
    PrintSourceTextErrorLine(Parser.pc.CStdOut, Parser.FileName,
        Parser.SourceText, Parser.Line, Parser.CharacterPos);
    va_start(Args, Message);
    PlatformVPrintf(Parser.pc.CStdOut, Message, Args);
    va_end(Args);
    PlatformPrintf(Parser.pc.CStdOut, "\n");
    PlatformExit(Parser.pc, 1);
}

/* exit with a message, when we're not parsing a program */
void ProgramFailNoParser(Picoc *pc, const(char) *Message, ...)
{
    va_list Args;

    va_start(Args, Message);
    PlatformVPrintf(pc.CStdOut, Message, Args);
    va_end(Args);
    PlatformPrintf(pc.CStdOut, "\n");
    PlatformExit(pc, 1);
}

/* like ProgramFail() but gives descriptive error messages for assignment */
void AssignFail(ParseState *Parser, const(char) *Format,
    ValueType *Type1, ValueType *Type2, int Num1, int Num2,
    const char *FuncName, int ParamNo)
{
    IOFILE *Stream = Parser.pc.CStdOut;

    PrintSourceTextErrorLine(Parser.pc.CStdOut, Parser.FileName,
        Parser.SourceText, Parser.Line, Parser.CharacterPos);
    PlatformPrintf(Stream, "can't %s ", (FuncName == NULL) ? "assign" : "set");

    if (Type1 != NULL)
        PlatformPrintf(Stream, Format, Type1, Type2);
    else
        PlatformPrintf(Stream, Format, Num1, Num2);

    if (FuncName != NULL)
        PlatformPrintf(Stream, " in argument %d of call to %s()", ParamNo,
            FuncName);

    PlatformPrintf(Stream, "\n");
    PlatformExit(Parser.pc, 1);
}

/* exit lexing with a message */
void LexFail(Picoc *pc, LexState *Lexer, const(char) *Message, ...)
{
    va_list Args;

    PrintSourceTextErrorLine(pc.CStdOut, Lexer.FileName, Lexer.SourceText,
        Lexer.Line, Lexer.CharacterPos);
    va_start(Args, Message);
    PlatformVPrintf(pc.CStdOut, Message, Args);
    va_end(Args);
    PlatformPrintf(pc.CStdOut, "\n");
    PlatformExit(pc, 1);
}

extern (C) void PlatformPrintf(const(char)* fmt, ...) nothrow @nogc
{
    import core.stdc.stdio;

    char[256] buffer;
    va_list args;
    va_start (args, fmt);
    vsnprintf (buffer.ptr, 256, fmt, args);
    va_end (args);

    version(Windows)
    {
        import core.sys.windows.windows;
        OutputDebugStringA(buffer.ptr);
    }
    else
    {        
        printf("%s\n", buffer.ptr);
    }
}


/* printf for compiler error reporting */
void PlatformPrintf(IOFILE *Stream, const(char)*fmt, ...)
{
    import core.stdc.stdio;

    char[256] buffer;
    va_list args;
    va_start (args, fmt);
    vsnprintf (buffer.ptr, 256, fmt, args);
    va_end (args);

    version(Windows)
    {
        import core.sys.windows.windows;
        OutputDebugStringA(buffer.ptr);
    }
    else
    {        
        printf("%s\n", buffer.ptr);
    }
}


void PlatformVPrintf(IOFILE *Stream, const char *Format, va_list Args)
{
    const(char)* FPos;

    for (FPos = Format; *FPos != '\0'; FPos++) {
        if (*FPos == '%') {
            FPos++;
            switch (*FPos) {
            case 's':
                PrintStr(va_arg!(char*)(Args), Stream);
                break;
            case 'd':
                PrintSimpleInt(va_arg!int(Args), Stream);
                break;
            case 'c':
                PrintCh( cast(char) va_arg!int(Args), Stream);
                break;
            case 't':
                PrintType(va_arg!(ValueType*)(Args), Stream);
                break;
            case 'f':
                PrintFP(va_arg!double(Args), Stream);
                break;
            case '%':
                PrintCh('%', Stream);
                break;
            case '\0':
                FPos--;
                break;
            default:
                break;
            }
        } else
            PrintCh(*FPos, Stream);
    }
}

/* make a new temporary name. takes a static buffer of char [7] as a parameter.
 * should be initialized to "XX0000"
 * where XX can be any characters */
char *PlatformMakeTempName(Picoc *pc, char *TempNameBuffer)
{
    int CPos = 5;

    while (CPos > 1) {
        if (TempNameBuffer[CPos] < '9') {
            TempNameBuffer[CPos]++;
            return TableStrRegister(pc, TempNameBuffer);
        } else {
            TempNameBuffer[CPos] = '0';
            CPos--;
        }
    }

    return TableStrRegister(pc, TempNameBuffer);
}

void PlatformInit(Picoc *pc)
{
}

void PlatformCleanup(Picoc *pc)
{
}

/* get a line of interactive input */
char *PlatformGetLine(char *Buf, int MaxLen, const(char)*Prompt)
{
    if (Prompt != NULL)
        printf("%s", Prompt);

    fflush(stdout);
    return fgets(Buf, MaxLen, stdin);
}

/* get a character of interactive input */
int PlatformGetCharacter()
{
    fflush(stdout);
    return getchar();
}

/* write a character to the console */
void PlatformPutc(char OutCh, OutputStreamInfo *Stream)
{
    putchar(OutCh);
}

/* read a file into memory */
char *PlatformReadFile(Picoc *pc, const(char)* FileName)
{
    ubyte[] content = readFile(FileName);

    char* ReadText = cast(char*) content.ptr;
    char* p;

    // ???
    version(Windows)
    {
        if ((ReadText[0] == '#') && (ReadText[1] == '!')) 
        {
            for (p = ReadText; (*p != '\r') && (*p != '\n'); ++p) 
            {
                *p = ' ';
            }
        }
    }
    else
    {
        if ((ReadText[0] == '#') && (ReadText[1] == '!')) 
        {
            for (p = ReadText; (*p != '\0') && (*p != '\r') && (*p != '\n'); ++p) 
            {
                *p = ' ';
            }
        }
    }
    return ReadText;
}

/* read and scan a file for definitions */
void PicocPlatformScanFile(Picoc *pc, const(char)* FileName)
{
    char *SourceStr = PlatformReadFile(pc, FileName);
    PicocParse(pc, FileName, SourceStr, cast(int) strlen(SourceStr), true, false, true,
        gEnableDebugger);
}

/* exit the program */
void PlatformExit(Picoc *pc, int RetVal)
{
    pc.PicocExitValue = RetVal;
    //longjmp(pc->PicocExitBuf, 1);
}


/// Replacement for `std.file.read`.
/// Returns: File contents, allocated with malloc. `null` on error.
ubyte[] readFile(const(char)* fileNameZ)
{
    // assuming that fileNameZ is zero-terminated, since it will in practice be
    // a static string
    FILE* file = fopen(fileNameZ, "rb".ptr);
    if (file)
    {
        scope(exit) fclose(file);

        // finds the size of the file
        fseek(file, 0, SEEK_END);
        long size = ftell(file);
        fseek(file, 0, SEEK_SET);

        // Is this too large to read? 
        // Refuse to read more than 1gb file (if it happens, it's probably a bug).
        if (size > 1024*1024*1024)
            return null;

        // Read whole file in a mallocated slice
        ubyte[] fileBytes = ( cast(ubyte*) malloc(cast(int)size + 1) )[0..cast(int)size+1];
        size_t remaining = cast(size_t)size;

        ubyte* p = fileBytes.ptr;

        while (remaining > 0)
        {
            size_t bytesRead = fread(p, 1, remaining, file);
            if (bytesRead == 0)
            {
                free(fileBytes.ptr);
                return null;
            }
            p += bytesRead;
            remaining -= bytesRead;
        }

        p[size] = 0;

        return fileBytes;
    }
    else
        return null;
}
