/* picoc lexer - converts source text into a tokenised form */
module picocd.lex;

import core.stdc.ctype;
import core.stdc.string: strlen, memcpy;
import std.math: pow;

import picocd.interpreter;
import picocd.heap;
import picocd.table;
import picocd.platform;
import picocd.parse;
import picocd.variable;

@nogc:

bool isCidstart(int c)
{
    return (isalpha(c) || (c)=='_' || (c)=='#');
}

bool isCident(int c)
{
    return (isalnum(c) || (c)=='_');
}

bool IS_HEX_ALPHA_DIGIT(int c)
{
    return  (((c) >= 'a' && (c) <= 'f') || ((c) >= 'A' && (c) <= 'F'));
}

bool IS_BASE_DIGIT(int c, long b)
{
    return (((c) >= '0' && (c) < '0' + (((b)<10)?(b):10)) || (((b) > 10) ? IS_HEX_ALPHA_DIGIT(c) : false));
}

long GET_BASE_DIGIT(int c)
{
    return (((c) <= '9') ? ((c) - '0') : (((c) <= 'F') ? ((c) - 'A' + 10) : ((c) - 'a' + 10)));
}

void LEXER_INC(LexState* l) @nogc
{
    l.Pos++;
    l.CharacterPos++;
}

void LEXER_INCN(LexState* l, int n) @nogc
{
    l.Pos += n;
    l.CharacterPos += n;
}

enum TOKEN_DATA_OFFSET = 2;

/* maximum value which can be represented by a "char" data type */
enum MAX_CHAR_VALUE = 255;

struct ReservedWord 
{
    const(char) *Word;
    LexToken Token;
}

static immutable ReservedWord[] ReservedWords = 
[
    /* wtf, when optimizations are set escaping certain chars is required or they disappear */
    {"#define", TokenHashDefine},
    {"#else", TokenHashElse},
    {"#endif", TokenHashEndif},
    {"#if", TokenHashIf},
    {"#ifdef", TokenHashIfdef},
    {"#ifndef", TokenHashIfndef},
    {"#include", TokenHashInclude},
    {"auto", TokenAutoType},
    {"break", TokenBreak},
    {"case", TokenCase},
    {"char", TokenCharType},
    {"continue", TokenContinue},
    {"default", TokenDefault},
    {"delete", TokenDelete},
    {"do", TokenDo},
    {"double", TokenDoubleType},
    {"else", TokenElse},
    {"enum", TokenEnumType},
    {"extern", TokenExternType},
    {"float", TokenFloatType},
    {"for", TokenFor},
    {"goto", TokenGoto},
    {"if", TokenIf},
    {"int", TokenIntType},
    {"long", TokenLongType},
    {"new", TokenNew},
    {"register", TokenRegisterType},
    {"return", TokenReturn},
    {"short", TokenShortType},
    {"signed", TokenSignedType},
    {"sizeof", TokenSizeof},
    {"static", TokenStaticType},
    {"struct", TokenStructType},
    {"switch", TokenSwitch},
    {"typedef", TokenTypedef},
    {"union", TokenUnionType},
    {"unsigned", TokenUnsignedType},
    {"void", TokenVoidType},
    {"while", TokenWhile}
];



/* initialize the lexer */
void LexInit(Picoc *pc)
{
    int Count;
    TableInitTable(&pc.ReservedWordTable, pc.ReservedWordHashTable.ptr, ReservedWords.length / 2, true);

    for (Count = 0; Count < ReservedWords.length; Count++) 
    {
        TableSet(pc, &pc.ReservedWordTable,
            TableStrRegister(pc, ReservedWords[Count].Word),
            cast(Value*)&ReservedWords[Count], NULL, 0, 0);
    }

    pc.LexValue.Typ = NULL;
    pc.LexValue.Val = &pc.LexAnyValue;
    pc.LexValue.LValueFrom = null;
    pc.LexValue.ValOnHeap = false;
    pc.LexValue.ValOnStack = false;
    pc.LexValue.AnyValOnHeap = false;
    pc.LexValue.IsLValue = false;
}

/* deallocate */
void LexCleanup(Picoc *pc)
{
    int Count;

    LexInteractiveClear(pc, NULL);

    for (Count = 0; Count < (ReservedWords.sizeof) / (ReservedWord.sizeof);
            Count++)
        TableDelete(pc, &pc.ReservedWordTable,
            TableStrRegister(pc, ReservedWords[Count].Word));
}

/* check if a word is a reserved word - used while scanning */
LexToken LexCheckReservedWord(Picoc *pc, const char *Word)
{
    Value *val;

    if (TableGet(&pc.ReservedWordTable, Word, &val, NULL, NULL, NULL))
        return (cast(ReservedWord*)val).Token;
    else
        return TokenNone;
}

/* get a numeric literal - used while scanning */
LexToken LexGetNumber(Picoc *pc, LexState *Lexer, Value *Value)
{
    long Result = 0;
    long Base = 10;
    LexToken ResultToken;
    double FPResult;
    double FPDiv;
    /* long/unsigned flags */

    if (*Lexer.Pos == '0') {
        /* a binary, octal or hex literal */
        LEXER_INC(Lexer);
        if (Lexer.Pos != Lexer.End) {
            if (*Lexer.Pos == 'x' || *Lexer.Pos == 'X') {
                Base = 16; LEXER_INC(Lexer);
            } else if (*Lexer.Pos == 'b' || *Lexer.Pos == 'B') {
                Base = 2; LEXER_INC(Lexer);
            } else if (*Lexer.Pos != '.')
                Base = 8;
        }
    }

    /* get the value */
    for (; Lexer.Pos != Lexer.End && IS_BASE_DIGIT(*Lexer.Pos, Base);
            LEXER_INC(Lexer))
        Result = Result * Base + GET_BASE_DIGIT(*Lexer.Pos);

    if (*Lexer.Pos == 'u' || *Lexer.Pos == 'U') {
        LEXER_INC(Lexer);
        /* IsUnsigned = 1; */
    }
    if (*Lexer.Pos == 'l' || *Lexer.Pos == 'L') {
        LEXER_INC(Lexer);
        /* IsLong = 1; */
    }

    Value.Typ = &pc.LongType; /* ignored? */
    Value.Val.LongInteger = Result;

    ResultToken = TokenIntegerConstant;

    if (Lexer.Pos == Lexer.End)
        return ResultToken;

    if (Lexer.Pos == Lexer.End) {
        return ResultToken;
    }

    if (*Lexer.Pos != '.' && *Lexer.Pos != 'e' && *Lexer.Pos != 'E') {
        return ResultToken;
    }

    Value.Typ = &pc.FPType;
    FPResult = cast(double)Result;

    if (*Lexer.Pos == '.') {
        LEXER_INC(Lexer);
        for (FPDiv = 1.0/Base; Lexer.Pos != Lexer.End && IS_BASE_DIGIT(*Lexer.Pos, Base);
                LEXER_INC(Lexer), FPDiv /= cast(double)Base) {
            FPResult += GET_BASE_DIGIT(*Lexer.Pos) * FPDiv;
        }
    }

    if (Lexer.Pos != Lexer.End && (*Lexer.Pos == 'e' || *Lexer.Pos == 'E')) {
        int ExponentSign = 1;

        LEXER_INC(Lexer);
        if (Lexer.Pos != Lexer.End && *Lexer.Pos == '-') {
            ExponentSign = -1;
            LEXER_INC(Lexer);
        }

        Result = 0;
        while (Lexer.Pos != Lexer.End && IS_BASE_DIGIT(*Lexer.Pos, Base)) {
            Result = Result * Base + GET_BASE_DIGIT(*Lexer.Pos);
            LEXER_INC(Lexer);
        }

        // huh? should use scanf from libc instead
        FPResult *= pow(cast(double)Base, cast(double)Result * ExponentSign);
    }

    Value.Val.FP = FPResult;

    if (*Lexer.Pos == 'f' || *Lexer.Pos == 'F')
        LEXER_INC(Lexer);

    return TokenFPConstant;
}

/* get a reserved word or identifier - used while scanning */
LexToken LexGetWord(Picoc *pc, LexState *Lexer, Value *Value)
{
    const char *StartPos = Lexer.Pos;
    LexToken Token;

    do {
        LEXER_INC(Lexer);
    } while (Lexer.Pos != Lexer.End && isCident(cast(int)*Lexer.Pos));

    Value.Typ = NULL;
    Value.Val.Identifier = TableStrRegister2(pc, StartPos, cast(int)( Lexer.Pos - StartPos) );

    Token = LexCheckReservedWord(pc, Value.Val.Identifier);
    switch (Token) {
    case TokenHashInclude:
        Lexer.Mode = LexModeHashInclude;
        break;
    case TokenHashDefine:
        Lexer.Mode = LexModeHashDefine;
        break;
    default:
        break;
    }

    if (Token != TokenNone)
        return Token;

    if (Lexer.Mode == LexModeHashDefineSpace)
        Lexer.Mode = LexModeHashDefineSpaceIdent;

    return TokenIdentifier;
}

/* unescape a character from an octal character constant */
char LexUnEscapeCharacterConstant(const(char)**From,
    char FirstChar, int Base)
{
    int CCount;
    char Total = cast(char)GET_BASE_DIGIT(FirstChar);
    for (CCount = 0; IS_BASE_DIGIT(**From, Base) && CCount < 2; CCount++, (*From)++)
        Total = cast(char)(Total * Base + GET_BASE_DIGIT(**From));

    return Total;
}

/* unescape a character from a string or character constant */
char LexUnEscapeCharacter(const(char)**From, const(char)*End)
{
    char ThisChar;

    while (*From != End && **From == '\\' &&
            &(*From)[1] != End && (*From)[1] == '\n' )
        (*From) += 2;  /* skip escaped end of lines with LF line termination */

    while (*From != End && **From == '\\' &&
            &(*From)[1] != End &&
            &(*From)[2] != End && (*From)[1] == '\r' && (*From)[2] == '\n')
        (*From) += 3;  /* skip escaped end of lines with CR/LF line termination */

    if (*From == End)
        return '\\';

    if (**From == '\\') {
        /* it's escaped */
        (*From)++;
        if (*From == End)
            return '\\';

        ThisChar = *(*From)++;
        switch (ThisChar) {
        case '\\':
            return '\\';
        case '\'':
            return '\'';
        case '"':
            return '"';
        case 'a':
            return '\a';
        case 'b':
            return '\b';
        case 'f':
            return '\f';
        case 'n':
            return '\n';
        case 'r':
            return '\r';
        case 't':
            return '\t';
        case 'v':
            return '\v';
        case '0':
        case '1':
        case '2':
        case '3':
            return LexUnEscapeCharacterConstant(From, ThisChar, 8);
        case 'x':
            return LexUnEscapeCharacterConstant(From, '0', 16);
        default:
            return ThisChar;
        }
    }
    else
        return *(*From)++;
}

/* get a string constant - used while scanning */
LexToken LexGetStringConstant(Picoc *pc, LexState *Lexer,
    Value *Value_, char EndChar)
{
    int Escape = false;
    const(char) *StartPos = Lexer.Pos;
    const(char) *EndPos;
    char *EscBuf;
    char *EscBufPos;
    char *RegString;
    Value *ArrayValue;

    while (Lexer.Pos != Lexer.End && (*Lexer.Pos != EndChar || Escape)) {
        /* find the end */
        if (Escape) {
            if (*Lexer.Pos == '\r' && Lexer.Pos+1 != Lexer.End)
                Lexer.Pos++;

            if (*Lexer.Pos == '\n' && Lexer.Pos+1 != Lexer.End) {
                Lexer.Line++;
                Lexer.Pos++;
                Lexer.CharacterPos = 0;
                Lexer.EmitExtraNewlines++;
            }

            Escape = false;
        } else if (*Lexer.Pos == '\\')
            Escape = true;

        LEXER_INC(Lexer);
    }
    EndPos = Lexer.Pos;

    EscBuf = cast(char*) HeapAllocStack(pc, cast(int)( EndPos - StartPos) );
    if (EscBuf == NULL)
        LexFail(pc, Lexer, "(LexGetStringConstant) out of memory");

    for (EscBufPos = EscBuf, Lexer.Pos = StartPos; Lexer.Pos != EndPos;)
        *EscBufPos++ = LexUnEscapeCharacter(&Lexer.Pos, EndPos);

    /* try to find an existing copy of this string literal */
    RegString = TableStrRegister2(pc, EscBuf, cast(int)(EscBufPos - EscBuf));
    HeapPopStack(pc, EscBuf, cast(int)(EndPos - StartPos));
    ArrayValue = VariableStringLiteralGet(pc, RegString);
    if (ArrayValue == NULL) {
        /* create and store this string literal */
        ArrayValue = VariableAllocValueAndData(pc, NULL, 0, false, NULL, true);
        ArrayValue.Typ = pc.CharArrayType;
        ArrayValue.Val = cast(AnyValue *)RegString;
        VariableStringLiteralDefine(pc, RegString, ArrayValue);
    }

    /* create the the pointer for this char* */
    Value_.Typ = pc.CharPtrType;
    Value_.Val.Pointer = RegString;
    if (*Lexer.Pos == EndChar)
        LEXER_INC(Lexer);

    return TokenStringConstant;
}

/* get a character constant - used while scanning */
LexToken LexGetCharacterConstant(Picoc *pc, LexState *Lexer,
    Value *Value_)
{
    Value_.Typ = &pc.CharType;
    Value_.Val.Character = LexUnEscapeCharacter(&Lexer.Pos, Lexer.End);
    if (Lexer.Pos != Lexer.End && *Lexer.Pos != '\'')
        LexFail(pc, Lexer, "expected \"'\"");

    LEXER_INC(Lexer);
    return TokenCharacterConstant;
}

/* skip a comment - used while scanning */
void LexSkipComment(LexState *Lexer, char NextChar)
{
    if (NextChar == '*') {
        /* conventional C comment */
        while (Lexer.Pos != Lexer.End &&
                (*(Lexer.Pos-1) != '*' || *Lexer.Pos != '/')) {
            if (*Lexer.Pos == '\n')
                Lexer.EmitExtraNewlines++;
            LEXER_INC(Lexer);
        }

        if (Lexer.Pos != Lexer.End)
            LEXER_INC(Lexer);

        Lexer.Mode = LexModeNormal;
    } else {
        /* C++ style comment */
        while (Lexer.Pos != Lexer.End && *Lexer.Pos != '\n')
            LEXER_INC(Lexer);
    }
}

/* skip a line continuation - used while scanning */
void LexSkipLineCont(LexState *Lexer, char NextChar)
{
    while (Lexer.Pos != Lexer.End && *Lexer.Pos != '\n') {
        LEXER_INC(Lexer);
    }
}

/* get a single token from the source - used while scanning */
LexToken LexScanGetToken(Picoc *pc, LexState *Lexer,
    Value **Value)
{
    char ThisChar;
    char NextChar;
    LexToken GotToken = TokenNone;

    /* handle cases line multi-line comments or string constants
        which mess up the line count */
    if (Lexer.EmitExtraNewlines > 0) {
        Lexer.EmitExtraNewlines--;
        return TokenEndOfLine;
    }

    void NEXTIS(char c, LexToken x, LexToken y) @nogc
    {
        if (NextChar == c) 
        { 
            LEXER_INC(Lexer); 
            GotToken = x; 
        } 
        else 
            GotToken = y;
    }

    void NEXTIS3(char c, LexToken x, char d, LexToken y, LexToken z) @nogc
    {
         if (NextChar == c) 
         { 
            LEXER_INC(Lexer); 
            GotToken = (x); 
         } 
         else 
             NEXTIS(d,y,z);
    }

    void NEXTIS4(char c, LexToken x, char d, LexToken y, char e, LexToken z, LexToken a) @nogc
    {
        if (NextChar == c) 
        { 
            LEXER_INC(Lexer); 
            GotToken = (x); 
        } 
        else 
            NEXTIS3(d,y,e,z,a);
    }

    void NEXTIS3PLUS(char c, LexToken x, char d, LexToken y, char e, LexToken z, LexToken a) @nogc
    { 
        if (NextChar == (c)) 
        { 
            LEXER_INC(Lexer); 
            GotToken = (x); 
        } 
        else if (NextChar == (d)) 
        { 
            if (Lexer.Pos[1] == (e)) 
            { 
                LEXER_INCN(Lexer, 2); 
                GotToken = (z); 
            } 
            else 
            { 
                LEXER_INC(Lexer); 
                GotToken = (y); 
            } 
        } 
        else 
            GotToken = (a); 
    }

    void NEXTISEXACTLY3(char c, char d, LexToken y, LexToken z) @nogc
    { 
        if (NextChar == (c) && Lexer.Pos[1] == (d)) 
        { 
            LEXER_INCN(Lexer, 2); 
            GotToken = (y); 
        } 
        else 
            GotToken = (z); 
    }

    /* scan for a token */
    do {
        *Value = &pc.LexValue;
        while (Lexer.Pos != Lexer.End && isspace(cast(int)*Lexer.Pos)) {
            if (*Lexer.Pos == '\n') {
                Lexer.Line++;
                Lexer.Pos++;
                Lexer.Mode = LexModeNormal;
                Lexer.CharacterPos = 0;
                return TokenEndOfLine;
            } else if (Lexer.Mode == LexModeHashDefine ||
                                    Lexer.Mode == LexModeHashDefineSpace)
                Lexer.Mode = LexModeHashDefineSpace;
            else if (Lexer.Mode == LexModeHashDefineSpaceIdent)
                Lexer.Mode = LexModeNormal;

            LEXER_INC(Lexer);
        }

        if (Lexer.Pos == Lexer.End || *Lexer.Pos == '\0')
            return TokenEOF;

        ThisChar = *Lexer.Pos;
        if (isCidstart(cast(int)ThisChar))
            return LexGetWord(pc, Lexer, *Value);

        if (isdigit(cast(int)ThisChar))
            return LexGetNumber(pc, Lexer, *Value);

        NextChar = (Lexer.Pos+1 != Lexer.End) ? *(Lexer.Pos+1) : 0;
        LEXER_INC(Lexer);
        switch (ThisChar) {
        case '"':
            GotToken = LexGetStringConstant(pc, Lexer, *Value, '"');
            break;
        case '\'':
            GotToken = LexGetCharacterConstant(pc, Lexer, *Value);
            break;
        case '(':
            if (Lexer.Mode == LexModeHashDefineSpaceIdent)
                GotToken = TokenOpenMacroBracket;
            else
                GotToken = TokenOpenBracket;
            Lexer.Mode = LexModeNormal;
            break;
        case ')':
            GotToken = TokenCloseBracket;
            break;
        case '=':
            NEXTIS('=', TokenEqual, TokenAssign);
            break;
        case '+':
            NEXTIS3('=', TokenAddAssign, '+', TokenIncrement, TokenPlus);
            break;
        case '-':
            NEXTIS4('=', TokenSubtractAssign, '>', TokenArrow, '-',
                TokenDecrement, TokenMinus);
            break;
        case '*':
            NEXTIS('=', TokenMultiplyAssign, TokenAsterisk); break;
        case '/':
            if (NextChar == '/' || NextChar == '*') {
                LEXER_INC(Lexer);
                LexSkipComment(Lexer, NextChar);
            } else
                NEXTIS('=', TokenDivideAssign, TokenSlash);
            break;
        case '%':
            NEXTIS('=', TokenModulusAssign, TokenModulus); break;
        case '<':
            if (Lexer.Mode == LexModeHashInclude)
                GotToken = LexGetStringConstant(pc, Lexer, *Value, '>');
            else {
                NEXTIS3PLUS('=', TokenLessEqual, '<', TokenShiftLeft, '=',
                    TokenShiftLeftAssign, TokenLessThan);
            }
            break;
        case '>':
            NEXTIS3PLUS('=', TokenGreaterEqual, '>', TokenShiftRight, '=',
                TokenShiftRightAssign, TokenGreaterThan);
            break;
        case ';':
            GotToken = TokenSemicolon;
            break;
        case '&':
            NEXTIS3('=', TokenArithmeticAndAssign, '&', TokenLogicalAnd,
                TokenAmpersand);
            break;
        case '|':
            NEXTIS3('=', TokenArithmeticOrAssign, '|', TokenLogicalOr,
                TokenArithmeticOr);
            break;
        case '{':
            GotToken = TokenLeftBrace;
            break;
        case '}':
            GotToken = TokenRightBrace;
            break;
        case '[':
            GotToken = TokenLeftSquareBracket;
            break;
        case ']':
            GotToken = TokenRightSquareBracket;
            break;
        case '!':
            NEXTIS('=', TokenNotEqual, TokenUnaryNot);
            break;
        case '^':
            NEXTIS('=', TokenArithmeticExorAssign, TokenArithmeticExor);
            break;
        case '~':
            GotToken = TokenUnaryExor;
            break;
        case ',':
            GotToken = TokenComma;
            break;
        case '.':
            NEXTISEXACTLY3('.', '.', TokenEllipsis, TokenDot);
            break;
        case '?':
            GotToken = TokenQuestionMark;
            break;
        case ':':
            GotToken = TokenColon;
            break;
// XXX: line continuation feature
        case '\\':
            if (NextChar == ' ' || NextChar == '\n') {
                LEXER_INC(Lexer);
                LexSkipLineCont(Lexer, NextChar);
            } else
                LexFail(pc, Lexer, "illegal character '%c'", ThisChar);
            break;
        default:
            LexFail(pc, Lexer, "illegal character '%c'", ThisChar);
            break;
        }
    } while (GotToken == TokenNone);

    return GotToken;
}

/* what size value goes with each token */
int LexTokenSize(LexToken Token)
{
    switch (Token) {
    case TokenIdentifier: case TokenStringConstant: return (char*).sizeof;
    case TokenIntegerConstant: return long.sizeof;
    case TokenCharacterConstant: return char.sizeof;
    case TokenFPConstant: return double.sizeof;
    default: return 0;
    }
}

/* produce tokens from the lexer and return a heap buffer with
    the result - used for scanning */
void *LexTokenize(Picoc *pc, LexState *Lexer, int *TokenLen)
{
    int MemUsed = 0;
    int ValueSize;
    int LastCharacterPos = 0;
    int ReserveSpace = cast(int) ( (Lexer.End - Lexer.Pos) * 4 + 16 );
    void *HeapMem;
    void *TokenSpace = HeapAllocStack(pc, ReserveSpace);
    LexToken Token;
    Value *GotValue;
    char *TokenPos = cast(char*)TokenSpace;

    if (TokenSpace == NULL)
        LexFail(pc, Lexer, "(LexTokenize TokenSpace == NULL) out of memory");

    do {
        /* store the token at the end of the stack area */
        Token = LexScanGetToken(pc, Lexer, &GotValue);

        *cast(char*)TokenPos = cast(char) Token;
        TokenPos++;
        MemUsed++;

        *cast(char*)TokenPos = cast(char)LastCharacterPos;
        TokenPos++;
        MemUsed++;

        ValueSize = LexTokenSize(Token);
        if (ValueSize > 0) {
            /* store a value as well */
            memcpy(cast(void*)TokenPos, cast(void*)GotValue.Val, ValueSize);
            TokenPos += ValueSize;
            MemUsed += ValueSize;
        }

        LastCharacterPos = Lexer.CharacterPos;

    } while (Token != TokenEOF);

    HeapMem = HeapAllocMem(pc, MemUsed);
    if (HeapMem == NULL)
        LexFail(pc, Lexer, "(LexTokenize HeapMem == NULL) out of memory");

    assert(ReserveSpace >= MemUsed);
    memcpy(HeapMem, TokenSpace, MemUsed);
    HeapPopStack(pc, TokenSpace, ReserveSpace);
    if (TokenLen)
        *TokenLen = MemUsed;

    return HeapMem;
}

/* lexically analyse some source text */
void *LexAnalyse(Picoc *pc, const char *FileName, const char *Source,
    int SourceLen, int *TokenLen)
{
    LexState Lexer;

    Lexer.Pos = Source;
    Lexer.End = Source + SourceLen;
    Lexer.Line = 1;
    Lexer.FileName = FileName;
    Lexer.Mode = LexModeNormal;
    Lexer.EmitExtraNewlines = 0;
    Lexer.CharacterPos = 1;
    Lexer.SourceText = Source;

    return LexTokenize(pc, &Lexer, TokenLen);
}

/* prepare to parse a pre-tokenised buffer */
void LexInitParser(ParseState *Parser, Picoc *pc, const(char)* SourceText,
    void *TokenSource, char *FileName, int RunIt, int EnableDebugger)
{
    Parser.pc = pc;
    Parser.Pos = cast(const(char)*) TokenSource;
    Parser.Line = 1;
    Parser.FileName = FileName;
    Parser.Mode = RunIt ? RunModeRun : RunModeSkip;
    Parser.SearchLabel = 0;
    Parser.HashIfLevel = 0;
    Parser.HashIfEvaluateToLevel = 0;
    Parser.CharacterPos = 0;
    Parser.SourceText = SourceText;
    Parser.DebugMode = cast(char) EnableDebugger;
}

/* get the next token, without pre-processing */
LexToken LexGetRawToken(ParseState *Parser, Value **Value,
    int IncPos)
{
    int ValueSize;
    const(char)*Prompt = NULL;
    LexToken Token = TokenNone;
    Picoc *pc = Parser.pc;

    do {
        /* get the next token */
        if (Parser.Pos == NULL && pc.InteractiveHead != NULL)
            Parser.Pos = pc.InteractiveHead.Tokens;

        if (Parser.FileName != pc.StrEmpty || pc.InteractiveHead != NULL) {
            /* skip leading newlines */
            while ((Token = cast(LexToken)*cast(char*)Parser.Pos) == TokenEndOfLine) {
                Parser.Line++;
                Parser.Pos += TOKEN_DATA_OFFSET;
            }
        }

        if (Parser.FileName == pc.StrEmpty &&
                (pc.InteractiveHead == NULL || Token == TokenEOF)) {
            /* we're at the end of an interactive input token list */
            char[LINEBUFFER_MAX] LineBuffer;
            void *LineTokens;
            int LineBytes;
            TokenLine *LineNode;

            if (pc.InteractiveHead == NULL ||
                    cast(char*)Parser.Pos ==
                    &pc.InteractiveTail.Tokens[pc.InteractiveTail.NumBytes-TOKEN_DATA_OFFSET]) {
                /* get interactive input */
                if (pc.LexUseStatementPrompt) {
                    Prompt = INTERACTIVE_PROMPT_STATEMENT.ptr;
                    pc.LexUseStatementPrompt = false;
                } else
                    Prompt = INTERACTIVE_PROMPT_LINE.ptr;

                if (PlatformGetLine(&LineBuffer[0], LINEBUFFER_MAX, Prompt) == NULL)
                    return TokenEOF;

                /* put the new line at the end of the linked list of interactive lines */
                LineTokens = LexAnalyse(pc, pc.StrEmpty, &LineBuffer[0],
                    cast(int) strlen(LineBuffer.ptr), &LineBytes);
                LineNode = cast(TokenLine*) VariableAlloc(pc, Parser, TokenLine.sizeof, true);
                LineNode.Tokens = cast(char*) LineTokens;
                LineNode.NumBytes = LineBytes;
                if (pc.InteractiveHead == NULL) {
                    /* start a new list */
                    pc.InteractiveHead = LineNode;
                    Parser.Line = 1;
                    Parser.CharacterPos = 0;
                } else
                    pc.InteractiveTail.Next = LineNode;

                pc.InteractiveTail = LineNode;
                pc.InteractiveCurrentLine = LineNode;
                Parser.Pos = cast(char*)LineTokens;
            } else {
                /* go to the next token line */
                if (Parser.Pos != &pc.InteractiveCurrentLine.Tokens[pc.InteractiveCurrentLine.NumBytes-TOKEN_DATA_OFFSET]) {
                    /* scan for the line */
                    for (pc.InteractiveCurrentLine = pc.InteractiveHead;
                            Parser.Pos != &pc.InteractiveCurrentLine.Tokens[pc.InteractiveCurrentLine.NumBytes-TOKEN_DATA_OFFSET];
                            pc.InteractiveCurrentLine = pc.InteractiveCurrentLine.Next) {
                        assert(pc.InteractiveCurrentLine.Next != NULL);
                    }
                }

                assert(pc.InteractiveCurrentLine != NULL);
                pc.InteractiveCurrentLine = pc.InteractiveCurrentLine.Next;
                assert(pc.InteractiveCurrentLine != NULL);
                Parser.Pos = pc.InteractiveCurrentLine.Tokens;
            }

            Token = cast(LexToken)*cast(char*)Parser.Pos;
        }
    } while ((Parser.FileName == pc.StrEmpty && Token == TokenEOF) ||
        Token == TokenEndOfLine);

    Parser.CharacterPos = *(cast(char*)Parser.Pos + 1);
    ValueSize = LexTokenSize(Token);
    if (ValueSize > 0) {
        /* this token requires a value - unpack it */
        if (Value != NULL) {
            switch (Token) {
            case TokenStringConstant:
                pc.LexValue.Typ = pc.CharPtrType;
                break;
            case TokenIdentifier:
                pc.LexValue.Typ = NULL;
                break;
            case TokenIntegerConstant:
                pc.LexValue.Typ = &pc.LongType;
                break;
            case TokenCharacterConstant:
                pc.LexValue.Typ = &pc.CharType;
                break;
            case TokenFPConstant:
                pc.LexValue.Typ = &pc.FPType;
                break;
            default:
                break;
            }

            memcpy(cast(void*)pc.LexValue.Val,
                cast(void*)(cast(char*)Parser.Pos+TOKEN_DATA_OFFSET), ValueSize);
            pc.LexValue.ValOnHeap = false;
            pc.LexValue.ValOnStack = false;
            pc.LexValue.IsLValue = false;
            pc.LexValue.LValueFrom = NULL;
            *Value = &pc.LexValue;
        }

        if (IncPos)
            Parser.Pos += ValueSize + TOKEN_DATA_OFFSET;
    } else {
        if (IncPos && Token != TokenEOF)
            Parser.Pos += TOKEN_DATA_OFFSET;
    }

    assert(Token >= TokenNone && Token <= TokenEndOfFunction);
    return Token;
}

/* correct the token position depending if we already incremented the position */
void LexHashIncPos(ParseState *Parser, int IncPos)
{
    if (!IncPos)
        LexGetRawToken(Parser, NULL, true);
}

/* handle a #ifdef directive */
void LexHashIfdef(ParseState *Parser, int IfNot)
{
    /* get symbol to check */
    int IsDefined;
    Value *IdentValue;
    Value *SavedValue;
    LexToken Token = LexGetRawToken(Parser, &IdentValue, true);

    if (Token != TokenIdentifier)
        ProgramFail(Parser, "identifier expected");

    /* is the identifier defined? */
    IsDefined = TableGet(&Parser.pc.GlobalTable, IdentValue.Val.Identifier,
        &SavedValue, NULL, NULL, NULL);
    if (Parser.HashIfEvaluateToLevel == Parser.HashIfLevel &&
            ((IsDefined && !IfNot) || (!IsDefined && IfNot))) {
        /* #if is active, evaluate to this new level */
        Parser.HashIfEvaluateToLevel++;
    }

    Parser.HashIfLevel++;
}

/* handle a #if directive */
void LexHashIf(ParseState *Parser)
{
    /* get symbol to check */
    Value *IdentValue;
    Value *SavedValue = NULL;
    ParseState MacroParser;
    LexToken Token = LexGetRawToken(Parser, &IdentValue, true);

    if (Token == TokenIdentifier) {
        /* look up a value from a macro definition */
        if (!TableGet(&Parser.pc.GlobalTable, IdentValue.Val.Identifier,
                &SavedValue, NULL, NULL, NULL))
            ProgramFail(Parser, "'%s' is undefined", IdentValue.Val.Identifier);

        if (SavedValue.Typ.Base != TypeMacro)
            ProgramFail(Parser, "value expected");

        ParserCopy(&MacroParser, &SavedValue.Val.MacroDef_.Body);
        Token = LexGetRawToken(&MacroParser, &IdentValue, true);
    }

    if (Token != TokenCharacterConstant && Token != TokenIntegerConstant)
        ProgramFail(Parser, "value expected");

    /* is the identifier defined? */
    if (Parser.HashIfEvaluateToLevel == Parser.HashIfLevel &&
            IdentValue.Val.Character) {
        /* #if is active, evaluate to this new level */
        Parser.HashIfEvaluateToLevel++;
    }

    Parser.HashIfLevel++;
}

/* handle a #else directive */
void LexHashElse(ParseState *Parser)
{
    if (Parser.HashIfEvaluateToLevel == Parser.HashIfLevel - 1)
        Parser.HashIfEvaluateToLevel++;  /* #if was not active, make
                                            this next section active */
    else if (Parser.HashIfEvaluateToLevel == Parser.HashIfLevel) {
        /* #if was active, now go inactive */
        if (Parser.HashIfLevel == 0)
            ProgramFail(Parser, "#else without #if");

        Parser.HashIfEvaluateToLevel--;
    }
}

/* handle a #endif directive */
void LexHashEndif(ParseState *Parser)
{
    if (Parser.HashIfLevel == 0)
        ProgramFail(Parser, "#endif without #if");

    Parser.HashIfLevel--;
    if (Parser.HashIfEvaluateToLevel > Parser.HashIfLevel)
        Parser.HashIfEvaluateToLevel = Parser.HashIfLevel;
}

/* get the next token given a parser state, pre-processing as we go */
LexToken LexGetToken(ParseState *Parser, Value **Value,
    int IncPos)
{
    int TryNextToken;
    LexToken Token;

    /* implements the pre-processor #if commands */
    do {
        int WasPreProcToken = true;

        Token = LexGetRawToken(Parser, Value, IncPos);
        switch (Token) {
        case TokenHashIfdef:
            LexHashIncPos(Parser, IncPos); LexHashIfdef(Parser, false);
            break;
        case TokenHashIfndef:
            LexHashIncPos(Parser, IncPos); LexHashIfdef(Parser, true);
            break;
        case TokenHashIf:
            LexHashIncPos(Parser, IncPos); LexHashIf(Parser);
            break;
        case TokenHashElse:
            LexHashIncPos(Parser, IncPos); LexHashElse(Parser);
            break;
        case TokenHashEndif:
            LexHashIncPos(Parser, IncPos); LexHashEndif(Parser);
            break;
        default:
            WasPreProcToken = false;
            break;
        }

        /* if we're going to reject this token, increment the token
            pointer to the next one */
        TryNextToken = (Parser.HashIfEvaluateToLevel < Parser.HashIfLevel &&
                Token != TokenEOF) || WasPreProcToken;
        if (!IncPos && TryNextToken)
            LexGetRawToken(Parser, NULL, true);

    } while (TryNextToken);

    return Token;
}

/* take a quick peek at the next token, skipping any pre-processing */
LexToken LexRawPeekToken(ParseState *Parser)
{
    return cast(LexToken)*cast(char*)Parser.Pos;
}

/* find the end of the line */
void LexToEndOfMacro(ParseState *Parser)
{
// XXX: line continuation feature
    bool isContinued = false;
    while (true) {
        LexToken Token = cast(LexToken)*cast(char*)Parser.Pos;
        if (Token == TokenEOF)
            return;
        else if (Token == TokenEndOfLine) {
            if (!isContinued)
                return;
            isContinued = false;
        }
        if (Token == TokenBackSlash)
            isContinued = true;
        LexGetRawToken(Parser, NULL, true);
    }
}

/* copy the tokens from StartParser to EndParser into new memory, removing
    TokenEOFs and terminate with a TokenEndOfFunction */
void *LexCopyTokens(ParseState *StartParser, ParseState *EndParser)
{
    int MemSize = 0;
    int CopySize;
    char *Pos = cast(char*)StartParser.Pos;
    char *NewTokens;
    char *NewTokenPos;
    TokenLine *ILine;
    Picoc *pc = StartParser.pc;

    if (pc.InteractiveHead == NULL) {
        /* non-interactive mode - copy the tokens */
        MemSize = cast(int)(EndParser.Pos - StartParser.Pos);
        NewTokens = cast(char*) VariableAlloc(pc, StartParser, MemSize + TOKEN_DATA_OFFSET, true);
        memcpy(NewTokens, cast(void*)StartParser.Pos, MemSize);
    } else {
        /* we're in interactive mode - add up line by line */
        for (pc.InteractiveCurrentLine = pc.InteractiveHead;
                pc.InteractiveCurrentLine != NULL &&
                (Pos < &pc.InteractiveCurrentLine.Tokens[0] ||
                    Pos >= &pc.InteractiveCurrentLine.Tokens[pc.InteractiveCurrentLine.NumBytes]);
                pc.InteractiveCurrentLine = pc.InteractiveCurrentLine.Next) {
        } /* find the line we just counted */

        if (EndParser.Pos >= StartParser.Pos &&
                EndParser.Pos < &pc.InteractiveCurrentLine.Tokens[pc.InteractiveCurrentLine.NumBytes]) {
            /* all on a single line */
            MemSize = cast(int)(EndParser.Pos - StartParser.Pos);
            NewTokens = cast(char*) VariableAlloc(pc, StartParser, MemSize + TOKEN_DATA_OFFSET, true);
            memcpy(NewTokens, cast(void*)StartParser.Pos, MemSize);
        } else {
            /* it's spread across multiple lines */
            MemSize = cast(int)( &pc.InteractiveCurrentLine.Tokens[pc.InteractiveCurrentLine.NumBytes-TOKEN_DATA_OFFSET] - Pos);

            for (ILine = pc.InteractiveCurrentLine.Next;
                    ILine != NULL &&
                    (EndParser.Pos < &ILine.Tokens[0] || EndParser.Pos >= &ILine.Tokens[ILine.NumBytes]);
                    ILine = ILine.Next)
                MemSize += ILine.NumBytes - TOKEN_DATA_OFFSET;

            assert(ILine != NULL);
            MemSize += EndParser.Pos - &ILine.Tokens[0];
            NewTokens = cast(char*) VariableAlloc(pc, StartParser, MemSize + TOKEN_DATA_OFFSET, true);

            CopySize = cast(int)( &pc.InteractiveCurrentLine.Tokens[pc.InteractiveCurrentLine.NumBytes-TOKEN_DATA_OFFSET] - Pos);
            memcpy(NewTokens, Pos, CopySize);
            NewTokenPos = NewTokens + CopySize;
            for (ILine = pc.InteractiveCurrentLine.Next; ILine != NULL &&
                    (EndParser.Pos < &ILine.Tokens[0] || EndParser.Pos >= &ILine.Tokens[ILine.NumBytes]);
                    ILine = ILine.Next) {
                memcpy(NewTokenPos, &ILine.Tokens[0], ILine.NumBytes - TOKEN_DATA_OFFSET);
                NewTokenPos += ILine.NumBytes-TOKEN_DATA_OFFSET;
            }
            assert(ILine != NULL);
            memcpy(NewTokenPos, &ILine.Tokens[0], EndParser.Pos - &ILine.Tokens[0]);
        }
    }

    NewTokens[MemSize] = cast(char)TokenEndOfFunction;

    return NewTokens;
}

/* indicate that we've completed up to this point in the interactive input
    and free expired tokens */
void LexInteractiveClear(Picoc *pc, ParseState *Parser)
{
    while (pc.InteractiveHead != NULL) {
        TokenLine *NextLine = pc.InteractiveHead.Next;

        HeapFreeMem(pc, pc.InteractiveHead.Tokens);
        HeapFreeMem(pc, pc.InteractiveHead);
        pc.InteractiveHead = NextLine;
    }

    if (Parser != NULL)
        Parser.Pos = NULL;

    pc.InteractiveTail = NULL;
}

/* indicate that we've completed up to this point in the interactive
    input and free expired tokens */
void LexInteractiveCompleted(Picoc *pc, ParseState *Parser)
{
    while (pc.InteractiveHead != NULL &&
            !(Parser.Pos >= &pc.InteractiveHead.Tokens[0] &&
                Parser.Pos < &pc.InteractiveHead.Tokens[pc.InteractiveHead.NumBytes])) {
        /* this token line is no longer needed - free it */
        TokenLine *NextLine = pc.InteractiveHead.Next;

        HeapFreeMem(pc, pc.InteractiveHead.Tokens);
        HeapFreeMem(pc, pc.InteractiveHead);
        pc.InteractiveHead = NextLine;

        if (pc.InteractiveHead == NULL) {
            /* we've emptied the list */
            Parser.Pos = NULL;
            pc.InteractiveTail = NULL;
        }
    }
}

/* the next time we prompt, make it the full statement prompt */
void LexInteractiveStatementPrompt(Picoc *pc)
{
    pc.LexUseStatementPrompt = true;
}
