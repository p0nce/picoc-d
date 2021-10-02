/* picoc main header file - this has all the main data structures and
 * function prototypes. If you're just calling picoc you should look at the
 * external interface instead, in picoc.h */
module picocd.interpreter;

import core.stdc.stdio;

import picocd.platform;

nothrow:
@nogc:

enum NULL = null;

ulong MEM_ALIGN(ulong x)
{
    x = ( (x + (ALIGN_TYPE.sizeof) - 1) & ~((ALIGN_TYPE.sizeof)-1));
    return x;
}

uint MEM_ALIGN(uint x)
{
    x = cast(uint) ( (x + (ALIGN_TYPE.sizeof) - 1) & ~((ALIGN_TYPE.sizeof)-1));
    return x;
}


/* for debugging */
/*
#define PRINT_SOURCE_POS() { \
                                PrintSourceTextErrorLine(Parser->pc->CStdOut, \
                                                         Parser->FileName, \
                                                         Parser->SourceText, \
                                                         Parser->Line, \
                                                         Parser->CharacterPos); \
                                PlatformPrintf(Parser->pc->CStdOut, "\n"); \
                            }
*/

//#define PRINT_TYPE(typ) PlatformPrintf(Parser->pc->CStdOut, "%t\n", typ);

alias IOFILE = FILE;

/* coercion of numeric types to other numeric types */
bool IS_FP(Value* v)
{
    return v.Typ.Base == TypeFP;
}

double FP_VAL(Value* v)
{
    return v.Val.FP;
}

bool IS_POINTER_COERCIBLE(Value* v, int allowPointerCoercion)
{
    return allowPointerCoercion ? (v.Typ.Base == TypePointer) : false;
}
/*
#define POINTER_COERCE(v) ((int)(v)->Val->Pointer)
*/

bool IS_INTEGER_NUMERIC_TYPE(ValueType* t)
{
    return ((t).Base >= TypeInt && (t).Base <= TypeUnsignedLong);
}

bool IS_INTEGER_NUMERIC(Value* v)
{
    return IS_INTEGER_NUMERIC_TYPE(v.Typ);
}

bool IS_NUMERIC_COERCIBLE(Value* v)
{
    return (IS_INTEGER_NUMERIC(v) || IS_FP(v));
}

bool IS_NUMERIC_COERCIBLE_PLUS_POINTERS(Value* v, int allowPointerCoercion)
{
    return IS_NUMERIC_COERCIBLE(v) || IS_POINTER_COERCIBLE(v, allowPointerCoercion);
}

/* lexical tokens */
alias LexToken = int; 
enum : LexToken {
    /* 0x00 */ TokenNone,
    /* 0x01 */ TokenComma,
    /* 0x02 */ TokenAssign,
               TokenAddAssign,
               TokenSubtractAssign,
               TokenMultiplyAssign,
               TokenDivideAssign,
               TokenModulusAssign,
    /* 0x08 */ TokenShiftLeftAssign,
               TokenShiftRightAssign,
               TokenArithmeticAndAssign,
               TokenArithmeticOrAssign,
               TokenArithmeticExorAssign,
    /* 0x0d */ TokenQuestionMark,
               TokenColon,
    /* 0x0f */ TokenLogicalOr,
    /* 0x10 */ TokenLogicalAnd,
    /* 0x11 */ TokenArithmeticOr,
    /* 0x12 */ TokenArithmeticExor,
    /* 0x13 */ TokenAmpersand,
    /* 0x14 */ TokenEqual,
               TokenNotEqual,
    /* 0x16 */ TokenLessThan,
               TokenGreaterThan,
               TokenLessEqual,
               TokenGreaterEqual,
    /* 0x1a */ TokenShiftLeft,
               TokenShiftRight,
    /* 0x1c */ TokenPlus,
               TokenMinus,
    /* 0x1e */ TokenAsterisk,
               TokenSlash,
               TokenModulus,
    /* 0x21 */ TokenIncrement,
               TokenDecrement,
               TokenUnaryNot,
               TokenUnaryExor,
               TokenSizeof,
               TokenCast,
    /* 0x27 */ TokenLeftSquareBracket,
               TokenRightSquareBracket,
               TokenDot,
               TokenArrow,
    /* 0x2b */ TokenOpenBracket,
               TokenCloseBracket,
    /* 0x2d */ TokenIdentifier,
               TokenIntegerConstant,
               TokenFPConstant,
               TokenStringConstant,
               TokenCharacterConstant,
    /* 0x32 */ TokenSemicolon,
               TokenEllipsis,
    /* 0x34 */ TokenLeftBrace,
               TokenRightBrace,
    /* 0x36 */ TokenIntType,
               TokenCharType,
               TokenFloatType,
               TokenDoubleType,
               TokenVoidType,
               TokenEnumType,
    /* 0x3c */ TokenLongType,
               TokenSignedType,
               TokenShortType,
               TokenStaticType,
               TokenAutoType,
               TokenRegisterType,
               TokenExternType,
               TokenStructType,
               TokenUnionType,
               TokenUnsignedType,
               TokenTypedef,
    /* 0x46 */ TokenContinue,
               TokenDo,
               TokenElse,
               TokenFor,
               TokenGoto,
               TokenIf,
               TokenWhile,
               TokenBreak,
               TokenSwitch,
               TokenCase,
               TokenDefault,
               TokenReturn,
    /* 0x52 */ TokenHashDefine,
               TokenHashInclude,
               TokenHashIf,
               TokenHashIfdef,
               TokenHashIfndef,
               TokenHashElse,
               TokenHashEndif,
    /* 0x59 */ TokenNew,
               TokenDelete,
    /* 0x5b */ TokenOpenMacroBracket,
    /* 0x5c */ TokenEOF,
               TokenEndOfLine,
               TokenEndOfFunction,
               TokenBackSlash
}

/* used in dynamic memory allocation */
struct AllocNode 
{
    uint Size;
    AllocNode *NextFree;
}

/* whether we're running or skipping code */
alias RunMode = int;
enum : RunMode 
{
    RunModeRun,                 /* we're running code as we parse it */
    RunModeSkip,                /* skipping code, not running */
    RunModeReturn,              /* returning from a function */
    RunModeCaseSearch,          /* searching for a case label */
    RunModeBreak,               /* breaking out of a switch/while/do */
    RunModeContinue,            /* as above but repeat the loop */
    RunModeGoto                 /* searching for a goto label */
}

/* parser state - has all this detail so we can parse nested files */
struct ParseState 
{
    Picoc *pc;                  /* the picoc instance this parser is a part of */
    const(char) *Pos;            /* the character position in the source text */
    char *FileName;             /* what file we're executing (registered string) */
    short Line;                 /* line number we're executing */
    short CharacterPos;         /* character/column in the line we're executing */
    RunMode Mode;          /* whether to skip or run code */
    int SearchLabel;            /* what case label we're searching for */
    const(char) *SearchGotoLabel;/* what goto label we're searching for */
    const(char) *SourceText;     /* the entire source text */
    short HashIfLevel;           /* how many "if"s we're nested down */
    short HashIfEvaluateToLevel; /* if we're not evaluating an if branch,
                                          what the last evaluated level was */
    char DebugMode;              /* debugging mode */
    int ScopeID;   /* for keeping track of local variables (free them after t
                      hey go out of scope) */
}

/* values */
alias BaseType = int;
enum : BaseType 
{
    TypeVoid,                   /* no type */
    TypeInt,                    /* integer */
    TypeShort,                  /* short integer */
    TypeChar,                   /* a single character (signed) */
    TypeLong,                   /* long integer */
    TypeUnsignedInt,            /* unsigned integer */
    TypeUnsignedShort,          /* unsigned short integer */
    TypeUnsignedChar,           /* unsigned 8-bit number */ /* must be before unsigned long */
    TypeUnsignedLong,           /* unsigned long integer */
    TypeFP,                     /* floating point */
    TypeFunction,               /* a function */
    TypeMacro,                  /* a macro */
    TypePointer,                /* a pointer */
    TypeArray,                  /* an array of a sub-type */
    TypeStruct,                 /* aggregate type */
    TypeUnion,                  /* merged type */
    TypeEnum,                   /* enumerated integer type */
    TypeGotoLabel,              /* a label we can "goto" */
    Type_Type                   /* a type for storing types */
}

/* data type */
struct ValueType 
{
    BaseType Base;                  /* what kind of type this is */
    int ArraySize;                  /* the size of an array type */
    int Sizeof;                     /* the storage required */
    int AlignBytes;                 /* the alignment boundary of this type */
    const(char)* Identifier;         /* the name of a struct or union */
    ValueType *FromType;            /* the type we're derived from (or NULL) */
    ValueType *DerivedTypeList;     /* first in a list of types derived from this one */
    ValueType *Next;                /* next item in the derived type list */
    Table *Members;                 /* members of a struct or union */
    int OnHeap;                     /* true if allocated on the heap */
    int StaticQualifier;            /* true if it's a static */
}

/* function definition */
struct FuncDef 
{
    ValueType *ReturnType;   /* the return value type */
    int NumParams;                  /* the number of parameters */
    int VarArgs;                    /* has a variable number of arguments after
                                        the explicitly specified ones */
    ValueType **ParamType;   /* array of parameter types */
    char **ParamName;               /* array of parameter names */
    void function() Intrinsic;      /* intrinsic call address or NULL */
    ParseState Body;         /* lexical tokens of the function body if
                                        not intrinsic */
}

/* macro definition */
struct MacroDef 
{
    int NumParams;              /* the number of parameters */
    char **ParamName;           /* array of parameter names */
     ParseState Body;     /* lexical tokens of the function body
                                        if not intrinsic */
}

/* values */
union AnyValue 
{
    char Character;
    short ShortInteger;
    int Integer;
    long LongInteger;
    ushort UnsignedShortInteger;
    uint UnsignedInteger;
    ulong UnsignedLongInteger;
    char UnsignedCharacter;
    char *Identifier;
    char[2] ArrayMem;       /* placeholder for where the data starts,
                                doesn't point to it */
    ValueType *Typ;
    FuncDef FuncDef_;
    MacroDef MacroDef_;
    double FP;
    void *Pointer;      /* unsafe native pointers */
}

struct Value 
{
    ValueType *Typ;      /* the type of this value */
    AnyValue *Val;        /* pointer to the AnyValue which holds the actual content */
    Value *LValueFrom;   /* if an LValue, this is a Value our LValue is contained within (or NULL) */
    char ValOnHeap;             /* this Value is on the heap */
    char ValOnStack;            /* the AnyValue is on the stack along with this Value */
    char AnyValOnHeap;          /* the AnyValue is separately allocated from the Value on the heap */
    char IsLValue;              /* is modifiable and is allocated somewhere we can usefully modify it */
    int ScopeID;                /* to know when it goes out of scope */
    char OutOfScope;
}

/* hash table data structure */
struct TableEntry 
{
    TableEntry *Next;        /* next item in this hash chain */
    const(char) *DeclFileName;       /* where the variable was declared */
    ushort DeclLine;
    ushort DeclColumn;

    union TableEntryPayload 
    {
        struct ValueEntry 
        {
            char *Key;              /* points to the shared string table */
            Value *Val;             /* the value we're storing */
        } 
        ValueEntry v;              /* used for tables of values */

        char[1] Key;                /* dummy size - used for the shared string table */

        /* defines a breakpoint */
        struct BreakpointEntry 
        {
            const(char) *FileName;
            short Line;
            short CharacterPos;
        } 
        BreakpointEntry b;

    } 
    TableEntryPayload p;
}

struct Table 
{
    short Size;
    short OnHeap;
    TableEntry **HashTable;
}

/* stack frame for function calls */
struct StackFrame 
{
    ParseState ReturnParser;         /* how we got here */
    const(char)*FuncName;            /* the name of the function we're in */
    Value *ReturnValue;              /* copy the return value here */
    Value **Parameter;               /* array of parameter values */
    int NumParams;                   /* the number of parameters */
    Table LocalTable;                /* the local variables and parameters */
    TableEntry*[LOCAL_TABLE_SIZE] LocalHashTable;
    StackFrame *PreviousStackFrame;  /* the next lower stack frame */
}

/* lexer state */
alias LexMode = int;
enum : LexMode {
    LexModeNormal,
    LexModeHashInclude,
    LexModeHashDefine,
    LexModeHashDefineSpace,
    LexModeHashDefineSpaceIdent
}

struct LexState {
    const(char) *Pos;
    const(char) *End;
    const(char) *FileName;
    int Line;
    int CharacterPos;
    const(char) *SourceText;
    LexMode Mode;
    int EmitExtraNewlines;
}

/* library function definition */
struct LibraryFunction 
{
    void function(ParseState *Parser, Value *, Value **, int) @nogc Func;
    const(char) *Prototype;
}

/* output stream-type specific state information */
union OutputStreamInfo 
{
    struct StringOutputStream 
    {
        ParseState *Parser;
        char *WritePos;
    } 
    StringOutputStream Str;
}

/* stream-specific method for writing characters to the console */

alias CharWriter = void function(char, OutputStreamInfo *);

/* used when writing output to a string - eg. sprintf() */
struct OutputStream 
{
    CharWriter *Putch;
    OutputStreamInfo i;
}

/* possible results of parsing a statement */
alias ParseResult = int; 
enum : ParseResult 
{ 
    ParseResultEOF, 
    ParseResultError, 
    ParseResultOk 
}

/* a chunk of heap-allocated tokens we'll cleanup when we're done */
struct CleanupTokenNode 
{
    void *Tokens;
    const(char) *SourceText;
    CleanupTokenNode *Next;
}

/* linked list of lexical tokens used in interactive mode */
struct TokenLine 
{
    TokenLine *Next;
    char *Tokens;
    int NumBytes;
}


/* a list of libraries we can include */
struct IncludeLibrary 
{
    char *IncludeName;
    void function(Picoc *pc) @nogc SetupFunction ;
    LibraryFunction *FuncList;
    const(char) *SetupCSource;
    IncludeLibrary *NextLib;
}

enum FREELIST_BUCKETS = (8);        /* freelists for 4, 8, 12 ... 32 byte allocs */
enum SPLIT_MEM_THRESHOLD = (16);    /* don't split memory which is close in size */
enum BREAKPOINT_TABLE_SIZE = (21);


/* the entire state of the picoc system */
struct Picoc
{
nothrow @nogc:
    /* parser global data */
    Table GlobalTable;
    CleanupTokenNode *CleanupTokenList;
    TableEntry* [GLOBAL_TABLE_SIZE] GlobalHashTable;

    /* lexer global data */
    TokenLine *InteractiveHead;
    TokenLine *InteractiveTail;
    TokenLine *InteractiveCurrentLine;
    int LexUseStatementPrompt;
    AnyValue LexAnyValue;
    Value LexValue;
    Table ReservedWordTable;
    TableEntry*[RESERVED_WORD_TABLE_SIZE] ReservedWordHashTable;

    /* the table of string literal values */
    Table StringLiteralTable;
    TableEntry*[STRING_LITERAL_TABLE_SIZE] StringLiteralHashTable;

    /* the stack */
    StackFrame *TopStackFrame;

    /* the value passed to exit() */
    int PicocExitValue;

    /* a list of libraries we can include */
    IncludeLibrary *IncludeLibList;

    /* heap memory */
    char *HeapMemory;  /* stack memory since our heap is malloc()ed */
    void *HeapBottom;           /* the bottom of the (downward-growing) heap */
    void *StackFrame_;           /* the current stack frame */
    void *HeapStackTop;         /* the top of the stack */

    AllocNode*[FREELIST_BUCKETS] FreeListBucket; /* we keep a pool of freelist buckets to reduce fragmentation */
    AllocNode *FreeListBig;    /* free memory which doesn't fit in a bucket */

    /* types */
    ValueType UberType;
    ValueType IntType;
    ValueType ShortType;
    ValueType CharType;
    ValueType LongType;
    ValueType UnsignedIntType;
    ValueType UnsignedShortType;
    ValueType UnsignedLongType;
    ValueType UnsignedCharType;
    ValueType FPType;
    ValueType VoidType;
    ValueType TypeType;
    ValueType FunctionType;
    ValueType MacroType;
    ValueType EnumType;
    ValueType GotoLabelType;
    ValueType *CharPtrType;
    ValueType *CharPtrPtrType;
    ValueType *CharArrayType;
    ValueType *VoidPtrType;

    /* debugger */
    Table BreakpointTable;
    TableEntry*[BREAKPOINT_TABLE_SIZE] BreakpointHashTable;
    int BreakpointCount;
    int DebugManualBreak;

    /* C library */
    int BigEndian;
    int LittleEndian;

    IOFILE *CStdOut;
    IOFILE CStdOutBase;

    /* the picoc version string */
    const(char) *VersionString;

    //    jmp_buf PicocExitBuf;

    /* string table */
    Table StringTable;
    TableEntry*[STRING_TABLE_SIZE] StringHashTable;
    char *StrEmpty;

    char[7] lastAnonymousIdentStruct; // previous name given to an anonymous struct
    char[7] lastAnonymousIdentEnum;   // previous name given to an anonymous enum
}