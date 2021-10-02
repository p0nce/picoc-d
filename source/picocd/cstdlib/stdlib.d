module picocd.cstdlib.stdlib;

import core.stdc.stdlib;
import picocd.interpreter;
import picocd.platform;
import picocd.variable;
import picocd.table;

@nogc:

static immutable int Stdlib_ZeroValue = 0;


void StdlibAtof(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = atof(cast(const(char)*)Param[0].Val.Pointer);
}

void StdlibAtoi(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = atoi(cast(const(char)*)Param[0].Val.Pointer);
}

void StdlibAtol(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = atol(cast(const(char)*)Param[0].Val.Pointer);
}

void StdlibStrtod(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = strtod(cast(char*)Param[0].Val.Pointer, cast(char**)Param[1].Val.Pointer);
}

void StdlibStrtol(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = strtol(cast(char*) Param[0].Val.Pointer, cast(char**) Param[1].Val.Pointer, Param[2].Val.Integer);
}

void StdlibStrtoul(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = strtoul(cast(char*) Param[0].Val.Pointer, cast(char**) Param[1].Val.Pointer, Param[2].Val.Integer);
}

void StdlibMalloc(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = malloc(Param[0].Val.Integer);
}

void StdlibCalloc(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = calloc(Param[0].Val.Integer,
        Param[1].Val.Integer);
}

void StdlibRealloc(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = realloc(Param[0].Val.Pointer,
        Param[1].Val.Integer);
}

void StdlibFree(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    free(Param[0].Val.Pointer);
}

void StdlibRand(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = rand();
}

void StdlibSrand(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    srand(Param[0].Val.Integer);
}

void StdlibAbort(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ProgramFail(Parser, "abort");
}

void StdlibExit(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    PlatformExit(Parser.pc, Param[0].Val.Integer);
}

void StdlibGetenv(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = getenv(cast(char*) Param[0].Val.Pointer);
}

void StdlibSystem(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = system(cast(char*) Param[0].Val.Pointer);
}

void StdlibAbs(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = abs(Param[0].Val.Integer);
}

void StdlibLabs(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = labs(Param[0].Val.Integer);
}

/* all stdlib.h functions */
static immutable LibraryFunction[] StdlibFunctions =
[
    {&StdlibAtof, "float atof(char *);"},
    {&StdlibStrtod, "float strtod(char *,char **);"},
    {&StdlibAtoi, "int atoi(char *);"},
    {&StdlibAtol, "int atol(char *);"},
    {&StdlibStrtol, "int strtol(char *,char **,int);"},
    {&StdlibStrtoul, "int strtoul(char *,char **,int);"},
    {&StdlibMalloc, "void *malloc(int);"},
    {&StdlibCalloc, "void *calloc(int,int);"},
    {&StdlibRealloc, "void *realloc(void *,int);"},
    {&StdlibFree, "void free(void *);"},
    {&StdlibRand, "int rand();"},
    {&StdlibSrand, "void srand(int);"},
    {&StdlibAbort, "void abort();"},
    {&StdlibExit, "void exit(int);"},
    {&StdlibGetenv, "char *getenv(char *);"},
    {&StdlibSystem, "int system(char *);"},
    {&StdlibAbs, "int abs(int);"},
    {&StdlibLabs, "int labs(int);"},
    {NULL, NULL}
];

/* creates various system-dependent definitions */
void StdlibSetupFunc(Picoc *pc)
{
    /* define NULL, TRUE and FALSE */
    if (!VariableDefined(pc, TableStrRegister(pc, "NULL")))
        VariableDefinePlatformVar(pc, NULL, "NULL", &pc.IntType,
            cast(AnyValue*)&Stdlib_ZeroValue, false);
}

