module picocd.cstdlib.string;

import core.stdc.string;
import picocd.interpreter;
import picocd.variable;
import picocd.table;

static immutable String_ZeroValue = 0;

void StringStrcpy(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = strcpy(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer);
}

void StringStrncpy(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = strncpy(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer, Param[2].Val.Integer);
}

void StringStrcmp(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = strcmp(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer);
}

void StringStrncmp(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = strncmp(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer, Param[2].Val.Integer);
}

void StringStrcat(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = strcat(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer);
}

void StringStrncat(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = strncat(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer, Param[2].Val.Integer);
}

void StringIndex(ParseState *Parser, Value *ReturnValue,
                 Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = index_private(cast(char*) Param[0].Val.Pointer, Param[1].Val.Integer);
}

void StringRindex(ParseState *Parser, Value *ReturnValue,
                  Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = rindex_private(cast(char*) Param[0].Val.Pointer, Param[1].Val.Integer);
}

void StringStrlen(ParseState *Parser, Value *ReturnValue, Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = cast(int) strlen(cast(char*) Param[0].Val.Pointer);
}

void StringMemset(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = memset(Param[0].Val.Pointer,
        Param[1].Val.Integer, Param[2].Val.Integer);
}

void StringMemcpy(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = memcpy(Param[0].Val.Pointer,
        Param[1].Val.Pointer, Param[2].Val.Integer);
}

void StringMemcmp(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = memcmp(Param[0].Val.Pointer,
        Param[1].Val.Pointer, Param[2].Val.Integer);
}

void StringMemmove(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = memmove(Param[0].Val.Pointer,
        Param[1].Val.Pointer, Param[2].Val.Integer);
}

void StringMemchr(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = memchr(Param[0].Val.Pointer,
        Param[1].Val.Integer, Param[2].Val.Integer);
}

void StringStrchr(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = strchr(cast(char*) Param[0].Val.Pointer,
        Param[1].Val.Integer);
}

void StringStrrchr(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = strrchr(cast(char*) Param[0].Val.Pointer,
        Param[1].Val.Integer);
}

void StringStrcoll(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = strcoll(cast(char*) Param[0].Val.Pointer,cast(char*) Param[1].Val.Pointer);
}

void StringStrerror(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = strerror(Param[0].Val.Integer);
}

void StringStrspn(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = cast(int) strspn(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer);
}

void StringStrcspn(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = cast(int) strcspn(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer);
}

void StringStrpbrk(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = strpbrk(cast(char*) Param[0].Val.Pointer,
        cast(char*) Param[1].Val.Pointer);
}

void StringStrstr(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = strstr(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer);
}

void StringStrtok(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.Pointer = strtok(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer);
}

void StringStrxfrm(ParseState *Parser, Value *ReturnValue, Value **Param, int NumArgs)
{
    ReturnValue.Val.Integer = cast(int) strxfrm(cast(char*) Param[0].Val.Pointer, cast(char*) Param[1].Val.Pointer, Param[2].Val.Integer);
}

/* all string.h functions */
static immutable LibraryFunction[] StringFunctions =
[
    {&StringMemcpy,  "void *memcpy(void *,void *,int);"},
    {&StringMemmove, "void *memmove(void *,void *,int);"},
    {&StringMemchr,  "void *memchr(char *,int,int);"},
    {&StringMemcmp,  "int memcmp(void *,void *,int);"},
    {&StringMemset,  "void *memset(void *,int,int);"},
    {&StringStrcat,  "char *strcat(char *,char *);"},
    {&StringStrncat, "char *strncat(char *,char *,int);"},
    {&StringIndex,   "char *index(char *,int);"},
    {&StringRindex,  "char *rindex(char *,int);"},    
    {&StringStrchr,  "char *strchr(char *,int);"},
    {&StringStrrchr, "char *strrchr(char *,int);"},
    {&StringStrcmp,  "int strcmp(char *,char *);"},
    {&StringStrncmp, "int strncmp(char *,char *,int);"},
    {&StringStrcoll, "int strcoll(char *,char *);"},
    {&StringStrcpy,  "char *strcpy(char *,char *);"},
    {&StringStrncpy, "char *strncpy(char *,char *,int);"},
    {&StringStrerror,"char *strerror(int);"},
    {&StringStrlen,  "int strlen(char *);"},
    {&StringStrspn,  "int strspn(char *,char *);"},
    {&StringStrcspn, "int strcspn(char *,char *);"},
    {&StringStrpbrk, "char *strpbrk(char *,char *);"},
    {&StringStrstr,  "char *strstr(char *,char *);"},
    {&StringStrtok,  "char *strtok(char *,char *);"},
    {&StringStrxfrm, "int strxfrm(char *,char *,int);"},
    {NULL,          NULL }
];

/* creates various system-dependent definitions */
void StringSetupFunc(Picoc *pc)
{
    /* define NULL */
    if (!VariableDefined(pc, TableStrRegister(pc, "NULL")))
        VariableDefinePlatformVar(pc, NULL, "NULL", &pc.IntType,
            cast(AnyValue*)&String_ZeroValue, false);
}


// The rindex() function locates the last occurrence of c (converted to an unsigned char) in the string pointed to by string.
// The string argument to the function must contain a NULL character (\0) marking the end of the string.
// The rindex() function is identical to strrchr() — Find last occurrence of character in string.
// Note: The rindex() function has been moved to the Legacy Option group in Single UNIX Specification, Version 3 and may be withdrawn in a future version. The strrchr() function is preferred for portability.
//
// Returned value
//    If successful, rindex() returns a pointer to the first occurrence of c (converted to an unsigned character) in the string pointed to by string.
//    If c was not found, rindex() returns a NULL pointer.
//    There are no errno values defined.

char* rindex_private(const(char)* str, int c)
{
    const(char)* result;
    char ch = cast(char)c;
    size_t len = strlen(str);
    result = str + len;
    assert(*result == '\0');

    while (result >= str)
    {
        if (*result == ch)
            return cast(char*) result;
        --result;
    }
    return null;
}
unittest
{
    string imba = "iimba";
    assert( rindex_private(imba.ptr, 'i') == imba.ptr + 1);
}

char* index_private(const(char)* str, int c)
{
    const(char)* result = str;
    char ch = cast(char)c;
    size_t n = 0;
    while(true)
    {
        if (*result == ch)
            return cast(char*) result;

        if (*result == '\0')
            return null;

        result ++;
    }
}
unittest
{
    string imba = "imbaa";
    assert( index_private(imba.ptr, 'a') == imba.ptr + 3);
    assert( index_private(imba.ptr, 'r') == null);
}