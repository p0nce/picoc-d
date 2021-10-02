module picocd.cstdlib.stdio;

import core.stdc.stdio;
import core.stdc.string;
import core.stdc.inttypes;
import core.stdc.errno;
import picocd.interpreter;
import picocd.type;
import picocd.expression;
import picocd.table;
import picocd.platform;
import picocd.variable;

@nogc:

enum MAX_FORMAT = 80;
enum MAX_SCANF_ARGS = 10;

static int Stdio_ZeroValue = 0;
static int EOFValue = EOF;
static int SEEK_SETValue = SEEK_SET;
static int SEEK_CURValue = SEEK_CUR;
static int SEEK_ENDValue = SEEK_END;
static int BUFSIZValue = BUFSIZ;
static int FILENAME_MAXValue = FILENAME_MAX;
static int _IOFBFValue = _IOFBF;
static int _IOLBFValue = _IOLBF;
static int _IONBFValue = _IONBF;
static int L_tmpnamValue = L_tmpnam;
static int GETS_MAXValue = 255;  /* arbitrary maximum size of a gets() file */

static FILE *stdinValue;
static FILE *stdoutValue;
static FILE *stderrValue;


/* our own internal output stream which can output to FILE * or strings */
struct StdOutStream
{
    FILE *FilePtr;
    char *StrOutPtr;
    int StrOutLen;
    int CharCount;
}

/* our representation of varargs within picoc */
struct StdVararg
{
    Value **Param;
    int NumArgs;
}

/* initializes the I/O system so error reporting works */
void BasicIOInit(Picoc *pc)
{
    pc.CStdOut = stdout;
    stdinValue = stdin;
    stdoutValue = stdout;
    stderrValue = stderr;
}

/* output a single character to either a FILE * or a string */
void StdioOutPutc(int OutCh, StdOutStream *Stream)
{
    if (Stream.FilePtr != NULL) {
        /* output to stdio stream */
        putc(OutCh, Stream.FilePtr);
        Stream.CharCount++;
    } else if (Stream.StrOutLen < 0 || Stream.StrOutLen > 1) {
        /* output to a string */
        *Stream.StrOutPtr = cast(char)OutCh;
        Stream.StrOutPtr++;

        if (Stream.StrOutLen > 1)
            Stream.StrOutLen--;

        Stream.CharCount++;
    }
}

/* output a string to either a FILE * or a string */
void StdioOutPuts(const(char) *Str, StdOutStream *Stream)
{
    if (Stream.FilePtr != NULL) {
        /* output to stdio stream */
        fputs(Str, Stream.FilePtr);
    } else {
        /* output to a string */
        while (*Str != '\0') {
            if (Stream.StrOutLen < 0 || Stream.StrOutLen > 1) {
                /* output to a string */
                *Stream.StrOutPtr = *Str;
                Str++;
                Stream.StrOutPtr++;

                if (Stream.StrOutLen > 1)
                    Stream.StrOutLen--;

                Stream.CharCount++;
            }
        }
    }
}

/* printf-style format of an int or other word-sized object */
void StdioFprintfWord(StdOutStream *Stream, const(char)*Format, uint Value)
{
    if (Stream.FilePtr != NULL)
        Stream.CharCount += fprintf(Stream.FilePtr, Format, Value);
    else if (Stream.StrOutLen >= 0) {
	    int CCount = snprintf(Stream.StrOutPtr, Stream.StrOutLen,
            Format, Value);
		Stream.StrOutPtr += CCount;
        Stream.StrOutLen -= CCount;
        Stream.CharCount += CCount;
    } else {
        int CCount = sprintf(Stream.StrOutPtr, Format, Value);
        Stream.CharCount += CCount;
        Stream.StrOutPtr += CCount;
    }
}

/* printf-style format of a long */
void StdioFprintfLong(StdOutStream *Stream, const(char)*Format, uint64_t Value) {
    char[MAX_FORMAT+1] PlatformFormat;
    char *FPos = PlatformFormat.ptr;

    while (*Format) {
        const(char) *UseFormat = NULL;

        switch (*Format) {
        case 'd':
            UseFormat = PRId64;
            break;
        case 'i':
            UseFormat = PRIi64;
            break;
        case 'o':
            UseFormat = PRIo64;
            break;
        case 'u':
            UseFormat = PRIu64;
            break;
        case 'x':
            UseFormat = PRIx64;
            break;
        case 'X':
            UseFormat = PRIX64;
            break;
        /* Ignore the %l (long) specifier, because of course we're doing longs in this function */
        case 'l':
            break;
        default:
            *FPos++ = *Format;
            break;
        }
        ++Format;
        if (UseFormat) {
            strcpy(FPos, UseFormat);
            FPos += strlen(UseFormat);
        }
    }

    if (Stream.FilePtr != NULL)
        Stream.CharCount += fprintf(Stream.FilePtr, PlatformFormat.ptr, Value);
    else if (Stream.StrOutLen >= 0) {
        int CCount = snprintf(Stream.StrOutPtr, Stream.StrOutLen, PlatformFormat.ptr, Value);
        Stream.StrOutPtr += CCount;
        Stream.StrOutLen -= CCount;
        Stream.CharCount += CCount;
    } else {
        int CCount = sprintf(Stream.StrOutPtr, PlatformFormat.ptr, Value);
        Stream.CharCount += CCount;
        Stream.StrOutPtr += CCount;
    }
}

/* printf-style format of a floating point number */
void StdioFprintfFP(StdOutStream *Stream, const char *Format, double Value)
{
    if (Stream.FilePtr != NULL)
        Stream.CharCount += fprintf(Stream.FilePtr, Format, Value);
    else if (Stream.StrOutLen >= 0) {
        int CCount = snprintf(Stream.StrOutPtr, Stream.StrOutLen,
            Format, Value);
		Stream.StrOutPtr += CCount;
        Stream.StrOutLen -= CCount;
        Stream.CharCount += CCount;
    } else {
        int CCount = sprintf(Stream.StrOutPtr, Format, Value);
        Stream.CharCount += CCount;
        Stream.StrOutPtr += CCount;
    }
}

/* printf-style format of a pointer */
void StdioFprintfPointer(StdOutStream *Stream, const char *Format, void *Value)
{
    if (Stream.FilePtr != NULL)
        Stream.CharCount += fprintf(Stream.FilePtr, Format, Value);
    else if (Stream.StrOutLen >= 0) {
        int CCount = snprintf(Stream.StrOutPtr, Stream.StrOutLen,
            Format, Value);
        Stream.StrOutPtr += CCount;
        Stream.StrOutLen -= CCount;
        Stream.CharCount += CCount;
    } else {
        int CCount = sprintf(Stream.StrOutPtr, Format, Value);
        Stream.CharCount += CCount;
        Stream.StrOutPtr += CCount;
    }
}

/* internal do-anything v[s][n]printf() formatting system with output
    to strings or FILE * */
int StdioBasePrintf(ParseState *Parser, FILE *Stream, char *StrOut,
    int StrOutLen, const(char) *Format, StdVararg *Args)
{
    Value *ThisArg = Args.Param[0];
    int ArgCount = 0;
    const(char) *FPos;
    char[MAX_FORMAT+1] OneFormatBuf;
    int OneFormatCount;
	int ShowLong = 0;
    ValueType *ShowType;
    StdOutStream SOStream;
    Picoc *pc = Parser.pc;

    if (Format == NULL)
        Format = "[null format]\n".ptr;

    FPos = Format;
    SOStream.FilePtr = Stream;
    SOStream.StrOutPtr = StrOut;
    SOStream.StrOutLen = StrOutLen;
    SOStream.CharCount = 0;

    while (*FPos != '\0') {
        if (*FPos == '%') {
            /* work out what type we're printing */
            FPos++;
            ShowType = NULL;
            OneFormatBuf[0] = '%';
            OneFormatCount = 1;

            do {
                switch (*FPos) {
                case 'd':
                case 'i':
                    if (ShowLong) {
                        ShowLong = 0;
                        ShowType = &pc.LongType;
                    } else {
                        ShowType = &pc.IntType;
                    }
                    break;
                case 'u':
                    if (ShowLong) {
                        ShowLong = 0;
                        ShowType = &pc.UnsignedLongType;
                        break;
                    }
                    goto case 'o';
                case 'o':
                case 'x':
                case 'X':
                    ShowType = &pc.IntType;
                    break; /* integer base conversions */
                case 'l':
                    ShowLong = 1;
                    break; /* long integer */
                case 'e':
                case 'E':
                    ShowType = &pc.FPType;
                    break;      /* double, exponent form */
                case 'f':
                case 'F':
                    ShowType = &pc.FPType;
                    break;      /* double, fixed-point */
                case 'g':
                case 'G':
                    ShowType = &pc.FPType;
                    break;      /* double, flexible format */
                case 'a':
                case 'A':
                    ShowType = &pc.IntType;
                    break;     /* hexadecimal, 0x- format */
                case 'c':
                    ShowType = &pc.IntType;
                break;     /* character */
                case 's':
                    ShowType = pc.CharPtrType;
                    break;  /* string */
                case 'p':
                    ShowType = pc.VoidPtrType;
                    break;  /* pointer */
                case 'n':
                    ShowType = &pc.VoidType;
                    break;    /* number of characters written */
                case 'm':
                    ShowType = &pc.VoidType;
                    break;    /* strerror(errno) */
                case '%':
                    ShowType = &pc.VoidType;
                    break;    /* just a '%' character */
                case '\0':
                    ShowType = &pc.VoidType;
                    break;    /* end of format string */
                default:
                    break;
                }

                /* copy one character of format across to the OneFormatBuf */
                if (*FPos != 'l') {
                    OneFormatBuf[OneFormatCount] = *FPos;
                    OneFormatCount++;
                }
                /* do special actions depending on the conversion type */
                if (ShowType == &pc.VoidType) {
                    switch (*FPos) {
                    case 'm':
                        StdioOutPuts(strerror(errno), &SOStream);
                        break;
                    case '%':
                        StdioOutPutc(*FPos, &SOStream);
                        break;
                    case '\0':
                        OneFormatBuf[OneFormatCount] = '\0';
                        StdioOutPutc(*FPos, &SOStream);
                        break;
                    case 'n':
                        ThisArg = cast(Value*)(cast(char*)ThisArg + MEM_ALIGN( (Value.sizeof) + TypeStackSizeValue(ThisArg)));
                        if (ThisArg.Typ.Base == TypeArray &&
                                        ThisArg.Typ.FromType.Base == TypeInt)
                            *cast(int *)ThisArg.Val.Pointer = SOStream.CharCount;
                        break;
                    default:
                        assert(false);
                    }

                }

                FPos++;

            } while (ShowType == NULL && OneFormatCount < MAX_FORMAT);

            if (ShowType != &pc.VoidType) {
                if (ArgCount >= Args.NumArgs)
                    StdioOutPuts("XXX", &SOStream);
                else {
                    /* null-terminate the buffer */
                    OneFormatBuf[OneFormatCount] = '\0';

                    /* print this argument */
                    ThisArg = cast(Value*)(cast(char*)ThisArg +
                        MEM_ALIGN((Value.sizeof)+TypeStackSizeValue(ThisArg)));

                    if (ShowType == &pc.LongType) {
                        /* show a signed long */
                        if (IS_NUMERIC_COERCIBLE(ThisArg))
                                StdioFprintfLong(&SOStream, OneFormatBuf.ptr, ThisArg.Val.LongInteger);
                        else
                            StdioOutPuts("XXX", &SOStream);
                    } else if (ShowType == &pc.UnsignedLongType) {
                         /* show a unsigned long */
                        if (IS_NUMERIC_COERCIBLE(ThisArg))
                                StdioFprintfLong(&SOStream, OneFormatBuf.ptr, ThisArg.Val.UnsignedLongInteger);
                        else
                            StdioOutPuts("XXX", &SOStream);
                    } else if (ShowType == &pc.IntType) {
                        /* show a signed integer */
                        if (IS_NUMERIC_COERCIBLE(ThisArg))
                                StdioFprintfWord(&SOStream, OneFormatBuf.ptr, cast(uint)ExpressionCoerceUnsignedInteger(ThisArg));
                        else
                            StdioOutPuts("XXX", &SOStream);
                    } else if (ShowType == &pc.FPType) {
                        /* show a floating point number */
                        if (IS_NUMERIC_COERCIBLE(ThisArg))
                            StdioFprintfFP(&SOStream, OneFormatBuf.ptr,
                                ExpressionCoerceFP(ThisArg));
                        else
                            StdioOutPuts("XXX", &SOStream);
                    } else if (ShowType == pc.CharPtrType) {
                        if (ThisArg.Typ.Base == TypePointer)
                            StdioFprintfPointer(&SOStream, OneFormatBuf.ptr,
                                ThisArg.Val.Pointer);
                        else if (ThisArg.Typ.Base == TypeArray &&
                                    ThisArg.Typ.FromType.Base == TypeChar)
                            StdioFprintfPointer(&SOStream, OneFormatBuf.ptr,
                                &ThisArg.Val.ArrayMem[0]);
                        else
                            StdioOutPuts("XXX", &SOStream);
                    } else if (ShowType == pc.VoidPtrType) {
                        if (ThisArg.Typ.Base == TypePointer)
                            StdioFprintfPointer(&SOStream, OneFormatBuf.ptr,
                                ThisArg.Val.Pointer);
                        else if (ThisArg.Typ.Base == TypeArray)
                            StdioFprintfPointer(&SOStream, OneFormatBuf.ptr,
                                &ThisArg.Val.ArrayMem[0]);
                        else
                            StdioOutPuts("XXX", &SOStream);
                    }

                    ArgCount++;
                }
            }
        } else {
            /* just output a normal character */
            StdioOutPutc(*FPos, &SOStream);
            FPos++;
        }
    }

    /* null-terminate */
    if (SOStream.StrOutPtr != NULL && SOStream.StrOutLen > 0)
        *SOStream.StrOutPtr = '\0';

    return SOStream.CharCount;
}

/* internal do-anything v[s][n]scanf() formatting system with input
    from strings or FILE * */
int StdioBaseScanf(ParseState *Parser, FILE *Stream, char *StrIn, char *Format, StdVararg *Args)
{
    Value *ThisArg = Args.Param[0];
    int ArgCount = 0;
    void*[MAX_SCANF_ARGS] ScanfArg;

    if (Args.NumArgs > MAX_SCANF_ARGS)
        ProgramFail(Parser, "too many arguments to scanf() - %d max", MAX_SCANF_ARGS);

    for (ArgCount = 0; ArgCount < Args.NumArgs; ArgCount++) {
        ThisArg = cast(Value*)(cast(char*)ThisArg +
            MEM_ALIGN((Value.sizeof) + TypeStackSizeValue(ThisArg)));

        if (ThisArg.Typ.Base == TypePointer)
            ScanfArg[ArgCount] = ThisArg.Val.Pointer;
        else if (ThisArg.Typ.Base == TypeArray)
            ScanfArg[ArgCount] = &ThisArg.Val.ArrayMem[0];
        else
            ProgramFail(Parser, "non-pointer argument to scanf() - argument %d after format", ArgCount+1);
    }

    if (Stream != NULL)
        return fscanf(Stream, Format, ScanfArg[0], ScanfArg[1], ScanfArg[2],
            ScanfArg[3], ScanfArg[4], ScanfArg[5], ScanfArg[6], ScanfArg[7],
            ScanfArg[8], ScanfArg[9]);
    else
        return sscanf(StrIn, Format, ScanfArg[0], ScanfArg[1], ScanfArg[2],
            ScanfArg[3], ScanfArg[4], ScanfArg[5], ScanfArg[6], ScanfArg[7],
            ScanfArg[8], ScanfArg[9]);
}

/* stdio calls */
void StdioFopen(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = cast(void*) fopen(cast(const(char)*)Param[0].Val.Pointer, cast(const(char)*)Param[1].Val.Pointer);
}

/*
void StdioFreopen(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = freopen(Param[0].Val.Pointer,
        Param[1].Val.Pointer, Param[2].Val.Pointer);
}
*/

void StdioFclose(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = fclose(cast(FILE*) Param[0].Val.Pointer);
}

void StdioFread(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = cast(int) fread(Param[0].Val.Pointer,
        Param[1].Val.Integer, Param[2].Val.Integer, cast(FILE*) Param[3].Val.Pointer);
}

void StdioFwrite(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = cast(int) fwrite(Param[0].Val.Pointer,
        Param[1].Val.Integer, Param[2].Val.Integer, cast(FILE*) Param[3].Val.Pointer);
}

void StdioFgetc(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = fgetc(cast(FILE*) Param[0].Val.Pointer);
}

void StdioFgets(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = fgets(cast(char*)Param[0].Val.Pointer, Param[1].Val.Integer, cast(FILE*) Param[2].Val.Pointer);
}

void StdioRemove(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = remove(cast(const(char)*) Param[0].Val.Pointer);
}

void StdioRename(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = rename(cast(const(char)*) Param[0].Val.Pointer, cast(const(char)*) Param[1].Val.Pointer);
}

void StdioRewind(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    rewind( cast(FILE*) Param[0].Val.Pointer);
}

void StdioTmpfile(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = cast(void*) tmpfile();
}

void StdioClearerr(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    clearerr(cast(FILE *)Param[0].Val.Pointer);
}

void StdioFeof(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = feof(cast(FILE *)Param[0].Val.Pointer);
}

void StdioFerror(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = ferror(cast(FILE *)Param[0].Val.Pointer);
}

void StdioFileno(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = fileno(cast(FILE*) Param[0].Val.Pointer);
}

void StdioFflush(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = fflush(cast(FILE*) Param[0].Val.Pointer);
}

void StdioFgetpos(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = fgetpos(cast(FILE*) Param[0].Val.Pointer, cast(long*) Param[1].Val.Pointer);
}

void StdioFsetpos(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = fsetpos(cast(FILE*) Param[0].Val.Pointer, cast(long*) Param[1].Val.Pointer);
}

void StdioFputc(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = fputc(Param[0].Val.Integer, cast(FILE*) Param[1].Val.Pointer);
}

void StdioFputs(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = fputs(cast(const(char)*) Param[0].Val.Pointer, cast(FILE*) Param[1].Val.Pointer);
}

void StdioFtell(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = ftell(cast(FILE*) Param[0].Val.Pointer);
}

void StdioFseek(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = fseek(cast(FILE*) Param[0].Val.Pointer, Param[1].Val.Integer, Param[2].Val.Integer);
}

void StdioPerror(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    perror(cast(const(char)*) Param[0].Val.Pointer);
}

void StdioPutc(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = putc(Param[0].Val.Integer, cast(FILE*) Param[1].Val.Pointer);
}

void StdioPutchar(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = putchar(Param[0].Val.Integer);
}

void StdioSetbuf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    setbuf(cast(FILE*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer);
}

void StdioSetvbuf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    setvbuf(cast(FILE*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer, Param[2].Val.Integer, Param[3].Val.Integer);
}

void StdioUngetc(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = ungetc(Param[0].Val.Integer, cast(FILE*) Param[1].Val.Pointer);
}

void StdioPuts(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = puts( cast(const(char)*) Param[0].Val.Pointer);
}

void StdioGets(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = fgets(cast(char*) Param[0].Val.Pointer, GETS_MAXValue, stdin);
    if (ReturnValue.Val.Pointer != NULL) 
    {
        char *EOLPos = strchr(cast(char*) Param[0].Val.Pointer, '\n');
        if (EOLPos != NULL)
            *EOLPos = '\0';
    }
}

void StdioGetchar(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = getchar();
}

void StdioPrintf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    StdVararg PrintfArgs;

    PrintfArgs.Param = Param;
    PrintfArgs.NumArgs = NumArgs - 1;
    ReturnValue.Val.Integer = StdioBasePrintf(Parser, stdout, NULL, 0,
        cast(char*) Param[0].Val.Pointer, &PrintfArgs);
}

void StdioVprintf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = StdioBasePrintf(Parser, stdout, NULL, 0,
        cast(char*) Param[0].Val.Pointer, cast(StdVararg*) Param[1].Val.Pointer);
}

void StdioFprintf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    StdVararg PrintfArgs;

    PrintfArgs.Param = Param + 1;
    PrintfArgs.NumArgs = NumArgs - 2;
    ReturnValue.Val.Integer = StdioBasePrintf(Parser, cast(FILE*) Param[0].Val.Pointer,
        NULL, 0, cast(char*) Param[1].Val.Pointer, &PrintfArgs);
}

void StdioVfprintf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = StdioBasePrintf(Parser, cast(FILE*) Param[0].Val.Pointer,
        NULL, 0, cast(char*)Param[1].Val.Pointer, cast(StdVararg*) Param[2].Val.Pointer);
}

void StdioSprintf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    StdVararg PrintfArgs;

    PrintfArgs.Param = Param + 1;
    PrintfArgs.NumArgs = NumArgs - 2;
    ReturnValue.Val.Integer = StdioBasePrintf(Parser, NULL,
        cast(char*) Param[0].Val.Pointer, -1, cast(char*) Param[1].Val.Pointer, &PrintfArgs);
}

void StdioSnprintf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    StdVararg PrintfArgs;

    PrintfArgs.Param = Param + 2;
    PrintfArgs.NumArgs = NumArgs - 3;
    ReturnValue.Val.Integer = StdioBasePrintf(Parser, NULL,
        cast(char*) Param[0].Val.Pointer, Param[1].Val.Integer, cast(char*) Param[2].Val.Pointer,
        &PrintfArgs);
}

void StdioScanf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    StdVararg ScanfArgs;

    ScanfArgs.Param = Param;
    ScanfArgs.NumArgs = NumArgs - 1;
    ReturnValue.Val.Integer = StdioBaseScanf(Parser, stdin, NULL,
        cast(char*) Param[0].Val.Pointer, &ScanfArgs);
}

void StdioFscanf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    StdVararg ScanfArgs;

    ScanfArgs.Param = Param+1;
    ScanfArgs.NumArgs = NumArgs - 2;
    ReturnValue.Val.Integer = StdioBaseScanf(Parser, cast(FILE*) Param[0].Val.Pointer,
        NULL, cast(char*) Param[1].Val.Pointer, &ScanfArgs);
}

void StdioSscanf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    StdVararg ScanfArgs;

    ScanfArgs.Param = Param+1;
    ScanfArgs.NumArgs = NumArgs - 2;
    ReturnValue.Val.Integer = StdioBaseScanf(Parser, NULL,
        cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer, &ScanfArgs);
}

void StdioVsprintf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = StdioBasePrintf(Parser, NULL,
        cast(char*) Param[0].Val.Pointer, -1, cast(const(char)*) Param[1].Val.Pointer,
        cast(StdVararg*) Param[2].Val.Pointer);
}

void StdioVsnprintf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = StdioBasePrintf(Parser, NULL,
        cast(char*) Param[0].Val.Pointer, Param[1].Val.Integer, cast(const(char)*) Param[2].Val.Pointer,
        cast(StdVararg*) Param[3].Val.Pointer);
}

void StdioVscanf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = StdioBaseScanf(Parser, stdin, NULL,
        cast(char*) Param[0].Val.Pointer, cast(StdVararg*) Param[1].Val.Pointer);
}

void StdioVfscanf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = StdioBaseScanf(Parser, cast(FILE*) Param[0].Val.Pointer,
        NULL, cast(char*) Param[1].Val.Pointer, cast(StdVararg*) Param[2].Val.Pointer);
}

void StdioVsscanf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = StdioBaseScanf(Parser, NULL,
        cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer, cast(StdVararg*) Param[2].Val.Pointer);
}

/* handy structure definitions */
static string StdioDefs = "typedef struct __va_listStruct va_list; typedef struct __FILEStruct FILE;";

/* all stdio functions */
static immutable LibraryFunction[] StdioFunctions =
[
    {&StdioFopen, "FILE *fopen(char *, char *);"},
    //{&StdioFreopen, "FILE *freopen(char *, char *, FILE *);"},
    {&StdioFclose, "int fclose(FILE *);"},
    {&StdioFread, "int fread(void *, int, int, FILE *);"},
    {&StdioFwrite, "int fwrite(void *, int, int, FILE *);"},
    {&StdioFgetc, "int fgetc(FILE *);"},
    {&StdioFgetc, "int getc(FILE *);"},
    {&StdioFgets, "char *fgets(char *, int, FILE *);"},
    {&StdioFputc, "int fputc(int, FILE *);"},
    {&StdioFputs, "int fputs(char *, FILE *);"},
    {&StdioRemove, "int remove(char *);"},
    {&StdioRename, "int rename(char *, char *);"},
    {&StdioRewind, "void rewind(FILE *);"},
    {&StdioTmpfile, "FILE *tmpfile();"},
    {&StdioClearerr,"void clearerr(FILE *);"},
    {&StdioFeof, "int feof(FILE *);"},
    {&StdioFerror, "int ferror(FILE *);"},
    {&StdioFileno, "int fileno(FILE *);"},
    {&StdioFflush, "int fflush(FILE *);"},
    {&StdioFgetpos, "int fgetpos(FILE *, int *);"},
    {&StdioFsetpos, "int fsetpos(FILE *, int *);"},
    {&StdioFtell, "int ftell(FILE *);"},
    {&StdioFseek, "int fseek(FILE *, int, int);"},
    {&StdioPerror, "void perror(char *);"},
    {&StdioPutc, "int putc(char *, FILE *);"},
    {&StdioPutchar, "int putchar(int);"},
    {&StdioPutchar, "int fputchar(int);"},
    {&StdioSetbuf, "void setbuf(FILE *, char *);"},
    {&StdioSetvbuf, "void setvbuf(FILE *, char *, int, int);"},
    {&StdioUngetc, "int ungetc(int, FILE *);"},
    {&StdioPuts, "int puts(char *);"},
    {&StdioGets, "char *gets(char *);"},
    {&StdioGetchar, "int getchar();"},
    {&StdioPrintf, "int printf(char *, ...);"},
    {&StdioFprintf, "int fprintf(FILE *, char *, ...);"},
    {&StdioSprintf, "int sprintf(char *, char *, ...);"},
    {&StdioSnprintf,"int snprintf(char *, int, char *, ...);"},
    {&StdioScanf, "int scanf(char *, ...);"},
    {&StdioFscanf, "int fscanf(FILE *, char *, ...);"},
    {&StdioSscanf, "int sscanf(char *, char *, ...);"},
    {&StdioVprintf, "int vprintf(char *, va_list);"},
    {&StdioVfprintf,"int vfprintf(FILE *, char *, va_list);"},
    {&StdioVsprintf,"int vsprintf(char *, char *, va_list);"},
    {&StdioVsnprintf,"int vsnprintf(char *, int, char *, va_list);"},
    {&StdioVscanf, "int vscanf(char *, va_list);"},
    {&StdioVfscanf, "int vfscanf(FILE *, char *, va_list);"},
    {&StdioVsscanf, "int vsscanf(char *, char *, va_list);"},
    {NULL, NULL}
];

/* creates various system-dependent definitions */
void StdioSetupFunc(Picoc *pc)
{
    ValueType *StructFileType;
    ValueType *FilePtrType;

    /* make a "struct __FILEStruct" which is the same size as a
        native FILE structure */
    StructFileType = TypeCreateOpaqueStruct(pc, NULL, TableStrRegister(pc, "__FILEStruct"), (FILE.sizeof));

    /* get a FILE * type */
    FilePtrType = TypeGetMatching(pc, NULL, StructFileType, TypePointer, 0, pc.StrEmpty, true);

    /* make a "struct __va_listStruct" which is the same size as
        our StdVararg */
    TypeCreateOpaqueStruct(pc, NULL, TableStrRegister(pc, "__va_listStruct"), FILE.sizeof);

    /* define EOF equal to the system EOF */
    VariableDefinePlatformVar(pc, NULL, "EOF", &pc.IntType,
        cast(AnyValue*)&EOFValue, false);
    VariableDefinePlatformVar(pc, NULL, "SEEK_SET", &pc.IntType,
        cast(AnyValue*)&SEEK_SETValue, false);
    VariableDefinePlatformVar(pc, NULL, "SEEK_CUR", &pc.IntType,
        cast(AnyValue*)&SEEK_CURValue, false);
    VariableDefinePlatformVar(pc, NULL, "SEEK_END", &pc.IntType,
        cast(AnyValue*)&SEEK_ENDValue, false);
    VariableDefinePlatformVar(pc, NULL, "BUFSIZ", &pc.IntType,
        cast(AnyValue*)&BUFSIZValue, false);
    VariableDefinePlatformVar(pc, NULL, "FILENAME_MAX", &pc.IntType,
        cast(AnyValue*)&FILENAME_MAXValue, false);
    VariableDefinePlatformVar(pc, NULL, "_IOFBF", &pc.IntType,
        cast(AnyValue*)&_IOFBFValue, false);
    VariableDefinePlatformVar(pc, NULL, "_IOLBF", &pc.IntType,
        cast(AnyValue*)&_IOLBFValue, false);
    VariableDefinePlatformVar(pc, NULL, "_IONBF", &pc.IntType,
        cast(AnyValue*)&_IONBFValue, false);
    VariableDefinePlatformVar(pc, NULL, "L_tmpnam", &pc.IntType,
        cast(AnyValue*)&L_tmpnamValue, false);
    VariableDefinePlatformVar(pc, NULL, "GETS_MAX", &pc.IntType,
        cast(AnyValue*)&GETS_MAXValue, false);

    /* define stdin, stdout and stderr */
    VariableDefinePlatformVar(pc, NULL, "stdin", FilePtrType,
        cast(AnyValue*)&stdinValue, false);
    VariableDefinePlatformVar(pc, NULL, "stdout", FilePtrType,
        cast(AnyValue*)&stdoutValue, false);
    VariableDefinePlatformVar(pc, NULL, "stderr", FilePtrType,
        cast(AnyValue*)&stderrValue, false);

    /* define NULL, true and false */
    if (!VariableDefined(pc, TableStrRegister(pc, "NULL")))
        VariableDefinePlatformVar(pc, NULL, "NULL", &pc.IntType,
            cast(AnyValue*)&Stdio_ZeroValue, false);
}

/* portability-related I/O calls */
void PrintCh(char OutCh, FILE *Stream)
{
    putc(OutCh, Stream);
}

void PrintSimpleInt(long Num, FILE *Stream)
{
    fprintf(Stream, "%lld", Num);
}

void PrintStr(const(char) *Str, FILE *Stream)
{
    fputs(Str, Stream);
}

void PrintFP(double Num, FILE *Stream)
{
    fprintf(Stream, "%f", Num);
}

