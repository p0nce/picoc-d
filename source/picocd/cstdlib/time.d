module picocd.cstdlib.time;

import core.stdc.time;

import picocd.interpreter;

/+
static int CLOCKS_PER_SECValue = CLOCKS_PER_SEC;

void StdAsctime(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = asctime(Param[0].Val.Pointer);
}

void StdClock(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = clock();
}

void StdCtime(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = ctime(Param[0].Val.Pointer);
}

void StdDifftime(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = difftime(cast(time_t)Param[0].Val.Integer,
        Param[1].Val.Integer);
}

void StdGmtime(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = gmtime(Param[0].Val.Pointer);
}

void StdLocaltime(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = localtime(Param[0].Val.Pointer);
}

void StdMktime(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = cast(int)mktime(Param[0].Val.Pointer);
}

void StdTime(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = cast(int)time(Param[0].Val.Pointer);
}

void StdStrftime(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = strftime(Param[0].Val.Pointer,
        Param[1].Val.Integer, Param[2].Val.Pointer, Param[3].Val.Pointer);
}

/* handy structure definitions */
static immutable string StdTimeDefs = "typedef int time_t; typedef int clock_t;";

/* all string.h functions */
static immutable LibraryFunction[] StdTimeFunctions =
[
    {&StdAsctime, "char *asctime(struct tm *);"},
    {&StdClock, "time_t clock();"},
    {&StdCtime, "char *ctime(int *);"},
    {&StdDifftime, "double difftime(int, int);"},
    {&StdGmtime, "struct tm *gmtime(int *);"},
    {&StdLocaltime, "struct tm *localtime(int *);"},
    {&StdMktime, "int mktime(struct tm *ptm);"},
    {&StdTime, "int time(int *);"},
    {&StdStrftime, "int strftime(char *, int, char *, struct tm *);"},
    {NULL, NULL}
];
+/

/* creates various system-dependent definitions */
void StdTimeSetupFunc(Picoc *pc)
{
    /+
    /* make a "struct tm" which is the same size as a native tm structure */
    TypeCreateOpaqueStruct(pc, NULL, TableStrRegister(pc, "tm"), (tm.sizeof));

    /* define CLK_PER_SEC etc. */
    VariableDefinePlatformVar(pc, NULL, "CLOCKS_PER_SEC", &pc.IntType,
        cast(AnyValue*)&CLOCKS_PER_SECValue, false);
    +/

}

