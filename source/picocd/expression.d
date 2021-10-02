/* picoc expression evaluator - a stack-based expression evaluation system
 * which handles operator precedence */
module picocd.expression;

import core.stdc.string;
import picocd.interpreter;
import picocd.variable;
import picocd.platform;
import picocd.heap;
import picocd.type;
import picocd.lex;
import picocd.table;
import picocd.parse;

@nogc:

/* whether evaluation is left to right for a given precedence level */
bool IS_LEFT_TO_RIGHT(int precedence)
{
    return precedence != 2 && precedence != 14;
}

enum BRACKET_PRECEDENCE = 20;

enum DEEP_PRECEDENCE = (BRACKET_PRECEDENCE*1000);


/* local prototypes */
alias OperatorOrder = int; 
enum : OperatorOrder
{
    OrderNone,
    OrderPrefix,
    OrderInfix,
    OrderPostfix
};

/* a stack of expressions we use in evaluation */
struct ExpressionStack 
{
    ExpressionStack *Next;  /* the next lower item on the stack */
    Value *Val;  /* the value for this stack node */
    LexToken Op;  /* the operator */
    ushort Precedence;  /* the operator precedence of this node */
    char Order;  /* the evaluation order of this operator */
};

/* operator precedence definitions */
struct OpPrecedence
{
    uint PrefixPrecedence;
    uint PostfixPrecedence;
    uint InfixPrecedence;
    char *Name;
}

/* NOTE: the order of this array must correspond exactly to the order of
    these tokens in LexToken   */
static immutable OpPrecedence[] OperatorPrecedence = 
[
    /* TokenNone, */ {0, 0, 0, "none"},
    /* TokenComma, */ {0, 0, 0, ","},
    /* TokenAssign, */ {0, 0, 2, "="},
    /* TokenAddAssign, */ {0, 0, 2, "+="},
    /* TokenSubtractAssign, */ {0, 0, 2, "-="},
    /* TokenMultiplyAssign, */ {0, 0, 2, "*="},
    /* TokenDivideAssign, */ { 0, 0, 2, "/=" },
    /* TokenModulusAssign, */ { 0, 0, 2, "%=" },
    /* TokenShiftLeftAssign, */ {0, 0, 2, "<<="},
    /* TokenShiftRightAssign, */ { 0, 0, 2, ">>=" },
    /* TokenArithmeticAndAssign, */ { 0, 0, 2, "&=" },
    /* TokenArithmeticOrAssign, */ {0, 0, 2, "|="},
    /* TokenArithmeticExorAssign, */ { 0, 0, 2, "^=" },
    /* TokenQuestionMark, */ {0, 0, 3, "?"},
    /* TokenColon, */ {0, 0, 3, ":" },
    /* TokenLogicalOr, */ {0, 0, 4, "||"},
    /* TokenLogicalAnd, */ {0, 0, 5, "&&"},
    /* TokenArithmeticOr, */ {0, 0, 6, "|"},
    /* TokenArithmeticExor, */ {0, 0, 7, "^"},
    /* TokenAmpersand, */ {14, 0, 8, "&"},
    /* TokenEqual, */  {0, 0, 9, "=="},
    /* TokenNotEqual, */ {0, 0, 9, "!="},
    /* TokenLessThan, */ {0, 0, 10, "<"},
    /* TokenGreaterThan, */ {0, 0, 10, ">"},
    /* TokenLessEqual, */ {0, 0, 10, "<="},
    /* TokenGreaterEqual, */ {0, 0, 10, ">="},
    /* TokenShiftLeft, */ {0, 0, 11, "<<"},
    /* TokenShiftRight, */ {0, 0, 11, ">>"},
    /* TokenPlus, */ {14, 0, 12, "+"},
    /* TokenMinus, */ {14, 0, 12, "-"},
    /* TokenAsterisk, */ {14, 0, 13, "*"},
    /* TokenSlash, */ {0, 0, 13, "/"},
    /* TokenModulus, */ {0, 0, 13, "%"},
    /* TokenIncrement, */ {14, 15, 0, "++"},
    /* TokenDecrement, */ {14, 15, 0, "--"},
    /* TokenUnaryNot, */ {14, 0, 0, "!"},
    /* TokenUnaryExor, */ {14, 0, 0, "~"},
    /* TokenSizeof, */ {14, 0, 0, "sizeof"},
    /* TokenCast, */ {14, 0, 0, "cast"},
    /* TokenLeftSquareBracket, */ {0, 0, 15, "["},
    /* TokenRightSquareBracket, */ {0, 15, 0, "]"},
    /* TokenDot, */ {0, 0, 15, "."},
    /* TokenArrow, */ {0, 0, 15, ".  "},
    /* TokenOpenBracket, */ {15, 0, 0, "("},
    /* TokenCloseBracket, */ {0, 15, 0, ")"}
];

int IsTypeToken(ParseState *Parser, LexToken   t,
    Value  * LexValue)
{
    if (t >= TokenIntType && t <= TokenUnsignedType)
        return 1; /* base type */

    /* typedef'ed type? */
    if (t == TokenIdentifier) {
        /* see TypeParseFront, case TokenIdentifier and ParseTypedef */
        Value  * VarValue;
        if (VariableDefined(Parser.pc, cast(char*) LexValue.Val.Pointer)) 
        {
            VariableGet(Parser.pc, Parser, cast(char*) LexValue.Val.Pointer, &VarValue);
            if (VarValue.  Typ == &Parser.  pc.  TypeType)
                return 1;
        }
    }

    return 0;
}

long ExpressionCoerceInteger(Value  *Val)
{
    switch (Val.  Typ.  Base) {
    case TypeInt:
        return cast(long)Val.  Val.  Integer;
    case TypeChar:
        return cast(long)Val.  Val.  Character;
    case TypeShort:
        return cast(long)Val.  Val.  ShortInteger;
    case TypeLong:
        return cast(long)Val.  Val.  LongInteger;
    case TypeUnsignedInt:
        return cast(long)Val.  Val.  UnsignedInteger;
    case TypeUnsignedShort:
        return cast(long)Val.  Val.  UnsignedShortInteger;
    case TypeUnsignedLong:
        return cast(long)Val.  Val.  UnsignedLongInteger;
    case TypeUnsignedChar:
        return cast(long)Val.  Val.  UnsignedCharacter;
    case TypePointer:
        return cast(long)Val.  Val.  Pointer;
    case TypeFP:
        return cast(long)Val.  Val.  FP;
    default:
        return 0;
    }
}

ulong ExpressionCoerceUnsignedInteger(Value  *Val)
{
    switch (Val.  Typ.  Base) {
    case TypeInt:
        return cast(ulong)Val.  Val.  Integer;
    case TypeChar:
        return cast(ulong)Val.  Val.  Character;
    case TypeShort:
        return cast(ulong)Val.  Val.  ShortInteger;
    case TypeLong:
        return cast(ulong)Val.  Val.  LongInteger;
    case TypeUnsignedInt:
        return cast(ulong)Val.  Val.  UnsignedInteger;
    case TypeUnsignedShort:
        return cast(ulong)Val.  Val.  UnsignedShortInteger;
    case TypeUnsignedLong:
        return cast(ulong)Val.  Val.  UnsignedLongInteger;
    case TypeUnsignedChar:
        return cast(ulong)Val.  Val.  UnsignedCharacter;
    case TypePointer:
        return cast(ulong)Val.  Val.  Pointer;
    case TypeFP:
        return cast(ulong)Val.  Val.  FP;
    default:
        return 0;
    }
}

double ExpressionCoerceFP(Value *Val)
{
    switch (Val.  Typ.  Base) {
    case TypeInt:
        return cast(double)Val.  Val.  Integer;
    case TypeChar:
        return cast(double)Val.  Val.  Character;
    case TypeShort:
        return cast(double)Val.  Val.  ShortInteger;
    case TypeLong:
        return cast(double)Val.  Val.  LongInteger;
    case TypeUnsignedInt:
        return cast(double)Val.  Val.  UnsignedInteger;
    case TypeUnsignedShort:
        return cast(double)Val.  Val.  UnsignedShortInteger;
    case TypeUnsignedLong:
        return cast(double)Val.  Val.  UnsignedLongInteger;
    case TypeUnsignedChar:
        return cast(double)Val.  Val.  UnsignedCharacter;
    case TypeFP:
        return Val.  Val.  FP;
    default:
        return 0.0;
    }
}

/* assign an integer value */
long ExpressionAssignInt(ParseState *Parser, Value  *DestValue, long FromInt, int After)
{
    long Result;

    if (!DestValue.IsLValue)
        ProgramFail(Parser, "can't assign to this");

    if (After)
        Result = ExpressionCoerceInteger(DestValue);
    else
        Result = FromInt;

    switch (DestValue.  Typ.  Base) {
    case TypeInt:
        DestValue.  Val.  Integer = cast(int)FromInt;
        break;
    case TypeShort:
        DestValue.  Val.  ShortInteger = cast(short)FromInt;
        break;
    case TypeChar:
        DestValue.  Val.  Character = cast(char)FromInt;
        break;
    case TypeLong:
        DestValue.  Val.  LongInteger = cast(long)FromInt;
        break;
    case TypeUnsignedInt:
        DestValue.  Val.  UnsignedInteger = cast(uint)FromInt;
        break;
    case TypeUnsignedShort:
        DestValue.  Val.  UnsignedShortInteger = cast(ushort)FromInt;
        break;
    case TypeUnsignedLong:
        DestValue.  Val.  UnsignedLongInteger = cast(ulong)FromInt;
        break;
    case TypeUnsignedChar:
        DestValue.  Val.  UnsignedCharacter = cast(char)FromInt;
        break;
    default:
        break;
    }
    return Result;
}

/* assign a floating point value */
double ExpressionAssignFP(ParseState *Parser, Value  *DestValue,
    double FromFP)
{
    if (!DestValue.  IsLValue)
        ProgramFail(Parser, "can't assign to this");

    DestValue.  Val.  FP = FromFP;
    return FromFP;
}

/* push a node on to the expression stack */
void ExpressionStackPushValueNode(ParseState *Parser,
    ExpressionStack **StackTop, Value  *ValueLoc)
{
    ExpressionStack *StackNode = cast(ExpressionStack*) VariableAlloc(Parser.  pc, Parser, ExpressionStack.sizeof, false);
    StackNode.  Next = *StackTop;
    StackNode.  Val = ValueLoc;
    *StackTop = StackNode;
    //StackNode.Line = Parser.Line;
    //StackNode.CharacterPos = Parser.CharacterPos;
}

/* push a blank value on to the expression stack by type */
Value  *ExpressionStackPushValueByType(ParseState *Parser,
    ExpressionStack **StackTop, ValueType *PushType)
{
    Value  *ValueLoc = VariableAllocValueFromType(Parser.  pc, Parser,
            PushType, false, NULL, false);
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);

    return ValueLoc;
}

/* push a value on to the expression stack */
void ExpressionStackPushValue(ParseState *Parser,
    ExpressionStack **StackTop, Value  *PushValue)
{
    Value  *ValueLoc = VariableAllocValueAndCopy(Parser.  pc, Parser,
        PushValue, false);
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);
}

void ExpressionStackPushLValue(ParseState *Parser,
    ExpressionStack **StackTop, Value  *PushValue, int Offset)
{
    Value  *ValueLoc = VariableAllocValueShared(Parser, PushValue);
    ValueLoc.  Val = cast(AnyValue*) (cast(void *)(cast(char *)ValueLoc.  Val + Offset));
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);
}

void ExpressionStackPushDereference(ParseState *Parser,
    ExpressionStack **StackTop, Value  *DereferenceValue)
{
    int Offset;
    int DerefIsLValue;
    Value  *DerefVal;
    Value  *ValueLoc;
    ValueType *DerefType;
    void *DerefDataLoc = VariableDereferencePointer(DereferenceValue, &DerefVal,
        &Offset, &DerefType, &DerefIsLValue);
    if (DerefDataLoc == NULL)
        ProgramFail(Parser, "NULL pointer dereference");

    ValueLoc = VariableAllocValueFromExistingData(Parser, DerefType,
                    cast(AnyValue*)DerefDataLoc, DerefIsLValue, DerefVal);
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);
}

void ExpressionPushInt(ParseState *Parser,
            ExpressionStack **StackTop, long IntValue)
{
    Value  *ValueLoc = VariableAllocValueFromType(Parser.  pc, Parser,
                            &Parser.  pc.  IntType, false, NULL, false);
    // jdp: an ugly hack to a) assign the correct value and b) properly print long values
    ValueLoc.  Val.  UnsignedLongInteger = cast(ulong)IntValue;
    ValueLoc.  Val.  LongInteger = cast(long)IntValue;
    ValueLoc.  Val.  Integer = cast(int)IntValue;
    ValueLoc.  Val.  ShortInteger = cast(short)IntValue;
    ValueLoc.  Val.  UnsignedShortInteger = cast(ushort)IntValue;
    ValueLoc.  Val.  UnsignedInteger = cast(uint)IntValue;
    ValueLoc.  Val.  UnsignedCharacter = cast(char)IntValue;
    ValueLoc.  Val.  Character = cast(char)IntValue;

    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);
}

void ExpressionPushFP(ParseState *Parser,
    ExpressionStack **StackTop, double FPValue)
{
    Value  *ValueLoc = VariableAllocValueFromType(Parser.  pc, Parser,
                                 &Parser.  pc.  FPType, false, NULL, false);
    ValueLoc.  Val.  FP = FPValue;
    ExpressionStackPushValueNode(Parser, StackTop, ValueLoc);
}

/* assign to a pointer */
void ExpressionAssignToPointer(ParseState *Parser, Value  *ToValue,
    Value  *FromValue, const(char) *FuncName, int ParamNo,
    int AllowPointerCoercion)
{
    ValueType *PointedToType = ToValue.  Typ.  FromType;

    if (FromValue.  Typ == ToValue.  Typ ||
            FromValue.  Typ == Parser.  pc.  VoidPtrType ||
            (ToValue.  Typ == Parser.  pc.  VoidPtrType &&
            FromValue.  Typ.  Base == TypePointer))
        ToValue.  Val.  Pointer = FromValue.  Val.  Pointer; /* plain old pointer assignment */
    else if (FromValue.  Typ.  Base == TypeArray &&
            (PointedToType == FromValue.  Typ.  FromType ||
            ToValue.  Typ == Parser.  pc.  VoidPtrType)) {
        /* the form is: blah *x = array of blah */
        ToValue.  Val.  Pointer = cast(void *)&FromValue.  Val.  ArrayMem[0];
    } else if (FromValue.  Typ.  Base == TypePointer &&
                FromValue.  Typ.  FromType.  Base == TypeArray &&
               (PointedToType == FromValue.  Typ.  FromType.  FromType ||
                ToValue.  Typ == Parser.  pc.  VoidPtrType) ) {
        /* the form is: blah *x = pointer to array of blah */
        ToValue.  Val.  Pointer = VariableDereferencePointer(FromValue, NULL,
            NULL, NULL, NULL);
    } else if (IS_NUMERIC_COERCIBLE(FromValue) &&
            ExpressionCoerceInteger(FromValue) == 0) {
        /* null pointer assignment */
        ToValue.  Val.  Pointer = NULL;
    } else if (AllowPointerCoercion && IS_NUMERIC_COERCIBLE(FromValue)) {
        /* assign integer to native pointer */
        ToValue.  Val.  Pointer =
            cast(void*)cast(ulong)ExpressionCoerceUnsignedInteger(FromValue);
    } else if (AllowPointerCoercion && FromValue.  Typ.  Base == TypePointer) {
        /* assign a pointer to a pointer to a different type */
        ToValue.  Val.  Pointer = FromValue.  Val.  Pointer;
    } else
        AssignFail(Parser, "%t from %t", ToValue.  Typ, FromValue.  Typ, 0, 0,
            FuncName, ParamNo);
}

/* assign any kind of value */
void ExpressionAssign(ParseState *Parser, Value  *DestValue,
    Value  *SourceValue, int Force, const char *FuncName, int ParamNo,
    int AllowPointerCoercion)
{
    if (!DestValue.  IsLValue && !Force)
        AssignFail(Parser, "not an lvalue", NULL, NULL, 0, 0, FuncName, ParamNo);

    if (IS_NUMERIC_COERCIBLE(DestValue) &&
            !IS_NUMERIC_COERCIBLE_PLUS_POINTERS(SourceValue, AllowPointerCoercion))
        AssignFail(Parser, "%t from %t", DestValue.  Typ, SourceValue.  Typ, 0, 0,
            FuncName, ParamNo);

    switch (DestValue.  Typ.  Base) {
    case TypeInt:
        DestValue.  Val.  Integer = cast(int)ExpressionCoerceInteger(SourceValue);
        break;
    case TypeShort:
        DestValue.  Val.  ShortInteger = cast(short)ExpressionCoerceInteger(SourceValue);
        break;
    case TypeChar:
        DestValue.  Val.  Character = cast(char)ExpressionCoerceInteger(SourceValue);
        break;
    case TypeLong:
        DestValue.  Val.  LongInteger = SourceValue.  Val.  LongInteger;
        break;
    case TypeUnsignedInt:
        DestValue.  Val.  UnsignedInteger =
            cast(uint)ExpressionCoerceUnsignedInteger(SourceValue);
        break;
    case TypeUnsignedShort:
        DestValue.  Val.  UnsignedShortInteger =
            cast(ushort)ExpressionCoerceUnsignedInteger(SourceValue);
        break;
    case TypeUnsignedLong:
        DestValue.  Val.  UnsignedLongInteger = SourceValue.  Val.  UnsignedLongInteger;
        break;
    case TypeUnsignedChar:
        DestValue.  Val.  UnsignedCharacter =
            cast(char)ExpressionCoerceUnsignedInteger(SourceValue);
        break;
    case TypeFP:
        if (!IS_NUMERIC_COERCIBLE_PLUS_POINTERS(SourceValue, AllowPointerCoercion))
            AssignFail(Parser, "%t from %t", DestValue.  Typ, SourceValue.  Typ,
                0, 0, FuncName, ParamNo);
        DestValue.  Val.  FP = cast(double)ExpressionCoerceFP(SourceValue);
        break;
    case TypePointer:
        ExpressionAssignToPointer(Parser, DestValue, SourceValue, FuncName,
            ParamNo, AllowPointerCoercion);
        break;
    case TypeArray:
        if (SourceValue.  Typ.  Base == TypeArray && DestValue.  Typ.  ArraySize == 0) {
            /* destination array is unsized - need to resize the destination
                array to the same size as the source array */
            DestValue.  Typ = SourceValue.  Typ;
            VariableRealloc(Parser, DestValue, TypeSizeValue(DestValue, false));

            if (DestValue.  LValueFrom != NULL) {
                /* copy the resized value back to the LValue */
                DestValue.  LValueFrom.  Val = DestValue.  Val;
                DestValue.  LValueFrom.  AnyValOnHeap = DestValue.  AnyValOnHeap;
            }
        }
        /* char array = "abcd" */
        if (DestValue.  Typ.  FromType.  Base == TypeChar &&
                SourceValue.  Typ.  Base == TypePointer &&
                SourceValue.  Typ.  FromType.  Base == TypeChar) {
            if (DestValue.  Typ.  ArraySize == 0) { /* char x[] = "abcd", x is unsized */
                int Size = cast(int)( strlen( cast(char*) SourceValue.Val.Pointer) + 1 );
                DestValue.  Typ = TypeGetMatching(Parser.  pc, Parser,
                            DestValue.  Typ.  FromType, DestValue.  Typ.  Base,
                            Size, DestValue.  Typ.  Identifier, true);
                VariableRealloc(Parser, DestValue, TypeSizeValue(DestValue,
                    false));
            }
            /* else, it's char x[10] = "abcd" */

            memcpy(cast(void*)DestValue.  Val, SourceValue.  Val.  Pointer,
                TypeSizeValue(DestValue, false));
            break;
        }

        if (DestValue.  Typ != SourceValue.  Typ)
            AssignFail(Parser, "%t from %t", DestValue.  Typ, SourceValue.  Typ,
                0, 0, FuncName, ParamNo);

        if (DestValue.  Typ.  ArraySize != SourceValue.  Typ.  ArraySize)
            AssignFail(Parser, "from an array of size %d to one of size %d",
                NULL, NULL, DestValue.  Typ.  ArraySize,
                SourceValue.  Typ.  ArraySize, FuncName, ParamNo);

        memcpy(cast(void*)DestValue.  Val, cast(void*)SourceValue.  Val,
                TypeSizeValue(DestValue, false));
        break;
    case TypeStruct:
    case TypeUnion:
        if (DestValue.  Typ != SourceValue.  Typ)
            AssignFail(Parser, "%t from %t", DestValue.  Typ, SourceValue.  Typ,
                        0, 0, FuncName, ParamNo);
        memcpy(cast(void*)DestValue.  Val, cast(void*)SourceValue.  Val,
                TypeSizeValue(SourceValue, false));
        break;
    default:
        AssignFail(Parser, "%t", DestValue.  Typ, NULL, 0, 0, FuncName, ParamNo);
        break;
    }
}

/* evaluate the first half of a ternary operator x ? y : z */
void ExpressionQuestionMarkOperator(ParseState *Parser,
    ExpressionStack **StackTop, Value  *BottomValue,
    Value  *TopValue)
{
    if (!IS_NUMERIC_COERCIBLE(TopValue))
        ProgramFail(Parser, "first argument to '?' should be a number");

    if (ExpressionCoerceInteger(TopValue)) {
        /* the condition's true, return the BottomValue */
        ExpressionStackPushValue(Parser, StackTop, BottomValue);
    } else {
        /* the condition's false, return void */
        ExpressionStackPushValueByType(Parser, StackTop, &Parser.  pc.  VoidType);
    }
}

/* evaluate the second half of a ternary operator x ? y : z */
void ExpressionColonOperator(ParseState *Parser,
    ExpressionStack **StackTop, Value  *BottomValue,
    Value  *TopValue)
{
    if (TopValue.  Typ.  Base == TypeVoid) {
        /* invoke the "else" part - return the BottomValue */
        ExpressionStackPushValue(Parser, StackTop, BottomValue);
    } else {
        /* it was a "then" - return the TopValue */
        ExpressionStackPushValue(Parser, StackTop, TopValue);
    }
}

/* evaluate a prefix operator */
void ExpressionPrefixOperator(ParseState *Parser,
    ExpressionStack **StackTop, LexToken   Op, Value  *TopValue)
{
    Value  *Result;
    AnyValue *ValPtr;
    ValueType *Typ;

    switch (Op) {
    case TokenAmpersand:
        if (!TopValue.  IsLValue)
            ProgramFail(Parser, "can't get the address of this");

        ValPtr = TopValue.  Val;
        Result = VariableAllocValueFromType(Parser.  pc, Parser,
                    TypeGetMatching(Parser.  pc, Parser, TopValue.  Typ,
                        TypePointer, 0, Parser.  pc.  StrEmpty, true),
                    false, NULL, false);
        Result.  Val.  Pointer = cast(void*)ValPtr;
        ExpressionStackPushValueNode(Parser, StackTop, Result);
        break;
    case TokenAsterisk:
        if(StackTop != NULL && (*StackTop) != NULL && (*StackTop).  Op == TokenSizeof)
            /* ignored */
            ExpressionStackPushValueByType(Parser, StackTop, TopValue.  Typ);
        else
            ExpressionStackPushDereference(Parser, StackTop, TopValue);
        break;
    case TokenSizeof:
        /* return the size of the argument */
        if (TopValue.  Typ == &Parser.  pc.  TypeType)
            Typ = TopValue.  Val.  Typ;
        else
            Typ = TopValue.  Typ;
        if (Typ.  FromType != NULL && Typ.  FromType.  Base == TypeStruct)
            Typ = Typ.  FromType;
        ExpressionPushInt(Parser, StackTop, TypeSize(Typ, Typ.  ArraySize, true));
        break;
    default:
        /* an arithmetic operator */
        if (TopValue.  Typ == &Parser.  pc.  FPType) {
            /* floating point prefix arithmetic */
            double ResultFP = 0.0;
            switch (Op) {
            case TokenPlus:
                ResultFP = TopValue.  Val.  FP;
                break;
            case TokenMinus:
                ResultFP = -TopValue.  Val.  FP;
                break;
            case TokenIncrement:
                ResultFP = ExpressionAssignFP(Parser, TopValue,
                    TopValue.  Val.  FP+1);
                break;
            case TokenDecrement:
                ResultFP = ExpressionAssignFP(Parser, TopValue,
                    TopValue.  Val.  FP-1);
                break;
            case TokenUnaryNot:
                ResultFP = !TopValue.  Val.  FP;
                break;
            default:
                ProgramFail(Parser, "invalid operation");
                break;
            }
            ExpressionPushFP(Parser, StackTop, ResultFP);
        } else if (IS_NUMERIC_COERCIBLE(TopValue)) {
            /* integer prefix arithmetic */
            long ResultInt = 0;
            long TopInt = 0;
            if (TopValue.  Typ.  Base == TypeLong)
                TopInt = TopValue.  Val.  LongInteger;
            else
                TopInt = ExpressionCoerceInteger(TopValue);
            switch (Op) {
            case TokenPlus:
                ResultInt = TopInt;
                break;
            case TokenMinus:
                ResultInt = -TopInt;
                break;
            case TokenIncrement:
                ResultInt = ExpressionAssignInt(Parser, TopValue,
                    TopInt+1, false);
                break;
            case TokenDecrement:
                ResultInt = ExpressionAssignInt(Parser, TopValue,
                    TopInt-1, false);
                break;
            case TokenUnaryNot:
                ResultInt = !TopInt;
                break;
            case TokenUnaryExor:
                ResultInt = ~TopInt;
                break;
            default:
                ProgramFail(Parser, "invalid operation");
                break;
            }
            ExpressionPushInt(Parser, StackTop, ResultInt);
        } else if (TopValue.  Typ.  Base == TypePointer) {
            /* pointer prefix arithmetic */
            int Size = TypeSize(TopValue.  Typ.  FromType, 0, true);
            Value  *StackValue;
            void *ResultPtr = null;
            if (Op != TokenUnaryNot && TopValue.  Val.  Pointer == NULL)
                ProgramFail(Parser, "a. invalid use of a NULL pointer");
            if (!TopValue.  IsLValue)
                ProgramFail(Parser, "can't assign to this");
            switch (Op) {
            case TokenIncrement:
                ResultPtr = TopValue.  Val.  Pointer =
                    cast(void*)(cast(char*)TopValue.  Val.  Pointer+Size);
                break;
            case TokenDecrement:
                ResultPtr = TopValue.  Val.  Pointer =
                    cast(void*)(cast(char*)TopValue.  Val.  Pointer-Size);
                break;
            case TokenUnaryNot:
                /* conditionally checking a pointer's value, we only want
                    to change the stack value (ResultPtr) and not the pointer's
                    actual value  */
                TopValue.  Val.  Pointer =
                    cast(void*)(cast(char*)TopValue.  Val.  Pointer);
                    ResultPtr = TopValue.  Val.  Pointer ? NULL : cast(void*)0x01;
                break;
            default:
                ProgramFail(Parser, "invalid operation");
                break;
            }
            StackValue = ExpressionStackPushValueByType(Parser, StackTop,
                TopValue.  Typ);
            StackValue.  Val.  Pointer = ResultPtr;
        } else {
            ProgramFail(Parser, "invalid operation");
        }
        break;
    }
}

/* evaluate a postfix operator */
void ExpressionPostfixOperator(ParseState *Parser,
    ExpressionStack **StackTop, LexToken   Op, Value  *TopValue)
{
    if (TopValue.  Typ == &Parser.  pc.  FPType) {
        /* floating point prefix arithmetic */
        double ResultFP = 0.0;

        switch (Op) {
        case TokenIncrement:
            ResultFP = ExpressionAssignFP(Parser, TopValue, TopValue.  Val.  FP+1);
            break;
        case TokenDecrement:
            ResultFP = ExpressionAssignFP(Parser, TopValue, TopValue.  Val.  FP-1);
            break;
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }
        ExpressionPushFP(Parser, StackTop, ResultFP);
    } else if (IS_NUMERIC_COERCIBLE(TopValue)) {
        long ResultInt = 0;
        long TopInt = ExpressionCoerceInteger(TopValue);
        switch (Op) {
        case TokenIncrement:
            ResultInt = ExpressionAssignInt(Parser, TopValue, TopInt+1, true);
            break;
        case TokenDecrement:
            ResultInt = ExpressionAssignInt(Parser, TopValue, TopInt-1, true);
            break;
        case TokenRightSquareBracket:
            ProgramFail(Parser, "not supported");
            break;  /* XXX */
        case TokenCloseBracket:
            ProgramFail(Parser, "not supported");
            break;  /* XXX */
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }
        ExpressionPushInt(Parser, StackTop, ResultInt);
    } else if (TopValue.  Typ.  Base == TypePointer) {
        /* pointer postfix arithmetic */
        int Size = TypeSize(TopValue.  Typ.  FromType, 0, true);
        Value  *StackValue;
        void *OrigPointer = TopValue.  Val.  Pointer;

        if (TopValue.  Val.  Pointer == NULL)
            ProgramFail(Parser, "b. invalid use of a NULL pointer");

        if (!TopValue.  IsLValue)
            ProgramFail(Parser, "can't assign to this");

        switch (Op) {
        case TokenIncrement:
            TopValue.  Val.  Pointer = cast(void*)(cast(char*)TopValue.  Val.  Pointer+Size);
            break;
        case TokenDecrement:
            TopValue.  Val.  Pointer = cast(void*)(cast(char*)TopValue.  Val.  Pointer-Size);
            break;
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }
        StackValue = ExpressionStackPushValueByType(Parser, StackTop,
            TopValue.  Typ);
        StackValue.  Val.  Pointer = OrigPointer;
    } else
        ProgramFail(Parser, "invalid operation");
}

/* evaluate an infix operator */
void ExpressionInfixOperator(ParseState *Parser,
    ExpressionStack **StackTop, LexToken   Op,
    Value  *BottomValue, Value  *TopValue)
{
    long ResultInt = 0;
    Value  *StackValue;
    void *Pointer;

    if (BottomValue == NULL || TopValue == NULL)
        ProgramFail(Parser, "invalid expression");

    if (Op == TokenLeftSquareBracket) {
        /* array index */
        int ArrayIndex;
        Value  *Result = NULL;

        if (!IS_NUMERIC_COERCIBLE(TopValue))
            ProgramFail(Parser, "array index must be an integer");

        ArrayIndex = cast(int) ExpressionCoerceInteger(TopValue);

        /* make the array element result */
        switch (BottomValue.  Typ.  Base) {
        case TypeArray:
            Result = VariableAllocValueFromExistingData(Parser,
            BottomValue.  Typ.  FromType,
            cast(AnyValue*)(&BottomValue.  Val.  ArrayMem[0] +
                TypeSize(BottomValue.  Typ,
            ArrayIndex, true)),
            BottomValue.  IsLValue, BottomValue.  LValueFrom);
            break;
        case TypePointer: Result = VariableAllocValueFromExistingData(Parser,
            BottomValue.  Typ.  FromType,
            cast(AnyValue*)(cast(char*)BottomValue.  Val.  Pointer +
                TypeSize(BottomValue.  Typ.  FromType,
            0, true) * ArrayIndex),
            BottomValue.  IsLValue, BottomValue.  LValueFrom);
            break;
        default:
            ProgramFail(Parser, "this %t is not an array", BottomValue.  Typ);
            break;
        }

        ExpressionStackPushValueNode(Parser, StackTop, Result);
    } else if (Op == TokenQuestionMark)
        ExpressionQuestionMarkOperator(Parser, StackTop, TopValue, BottomValue);
    else if (Op == TokenColon)
        ExpressionColonOperator(Parser, StackTop, TopValue, BottomValue);
    else if ((TopValue.  Typ == &Parser.  pc.  FPType &&
                 BottomValue.  Typ == &Parser.  pc.  FPType) ||
              (TopValue.  Typ == &Parser.  pc.  FPType
                    && IS_NUMERIC_COERCIBLE(BottomValue)) ||
              (IS_NUMERIC_COERCIBLE(TopValue)
                && BottomValue.  Typ == &Parser.  pc.  FPType) ) {
        /* floating point infix arithmetic */
        int ResultIsInt = false;
        double ResultFP = 0.0;
        double TopFP = (TopValue.  Typ == &Parser.  pc.  FPType) ?
            TopValue.  Val.  FP : cast(double)ExpressionCoerceInteger(TopValue);
        double BottomFP = (BottomValue.  Typ == &Parser.  pc.  FPType) ?
            BottomValue.  Val.  FP : cast(double)ExpressionCoerceInteger(BottomValue);

        /* If the destination is not float, we can't assign a floating value to it,
          we need to convert it to integer instead */

       

        switch (Op) {
        case TokenAssign:
            if (IS_FP(BottomValue)) 
            {
                ResultFP = ExpressionAssignFP(Parser, BottomValue, TopFP);
            } 
            else 
            {
                ResultInt = ExpressionAssignInt(Parser, BottomValue, cast(long)(TopFP), false);
                ResultIsInt = true; 
            }
            break;
        case TokenAddAssign:
            if (IS_FP(BottomValue)) 
            {
                ResultFP = ExpressionAssignFP(Parser, BottomValue, BottomFP + TopFP);
            } 
            else 
            {
                ResultInt = ExpressionAssignInt(Parser, BottomValue, cast(long)(BottomFP + TopFP), false);
                ResultIsInt = true; 
            }
            break;
        case TokenSubtractAssign:
            if (IS_FP(BottomValue)) 
            {
                ResultFP = ExpressionAssignFP(Parser, BottomValue, BottomFP - TopFP);
            } 
            else 
            {
                ResultInt = ExpressionAssignInt(Parser, BottomValue, cast(long)(BottomFP - TopFP), false);
                ResultIsInt = true; 
            }
            break;
        case TokenMultiplyAssign:
            if (IS_FP(BottomValue)) 
            {
                ResultFP = ExpressionAssignFP(Parser, BottomValue, BottomFP * TopFP);
            } 
            else 
            {
                ResultInt = ExpressionAssignInt(Parser, BottomValue, cast(long)(BottomFP * TopFP), false);
                ResultIsInt = true; 
            }
            break;
        case TokenDivideAssign:
            if (IS_FP(BottomValue)) 
            {
                ResultFP = ExpressionAssignFP(Parser, BottomValue, BottomFP / TopFP);
            } 
            else 
            {
                ResultInt = ExpressionAssignInt(Parser, BottomValue, cast(long)(BottomFP / TopFP), false);
                ResultIsInt = true; 
            }
            break;
        case TokenEqual:
            ResultInt = BottomFP == TopFP;
            ResultIsInt = true;
            break;
        case TokenNotEqual:
            ResultInt = BottomFP != TopFP;
            ResultIsInt = true;
            break;
        case TokenLessThan:
            ResultInt = BottomFP < TopFP;
            ResultIsInt = true;
            break;
        case TokenGreaterThan:
            ResultInt = BottomFP > TopFP;
            ResultIsInt = true;
            break;
        case TokenLessEqual:
            ResultInt = BottomFP <= TopFP;
            ResultIsInt = true;
            break;
        case TokenGreaterEqual:
            ResultInt = BottomFP >= TopFP;
            ResultIsInt = true;
            break;
        case TokenPlus:
            ResultFP = BottomFP + TopFP;
            break;
        case TokenMinus:
            ResultFP = BottomFP - TopFP;
            break;
        case TokenAsterisk:
            ResultFP = BottomFP * TopFP;
            break;
        case TokenSlash:
            ResultFP = BottomFP / TopFP;
            break;
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }

        if (ResultIsInt)
            ExpressionPushInt(Parser, StackTop, ResultInt);
        else
            ExpressionPushFP(Parser, StackTop, ResultFP);
    } else if (IS_NUMERIC_COERCIBLE(TopValue) && IS_NUMERIC_COERCIBLE(BottomValue)) {
        /* integer operation */
        long TopInt = ExpressionCoerceInteger(TopValue);
        long BottomInt = ExpressionCoerceInteger(BottomValue);
        switch (Op) {
        case TokenAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue, TopInt, false);
            break;
        case TokenAddAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt + TopInt, false);
            break;
        case TokenSubtractAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt-TopInt, false);
            break;
        case TokenMultiplyAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt*TopInt, false);
            break;
        case TokenDivideAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt/TopInt, false);
            break;
        case TokenModulusAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt%TopInt, false);
            break;
        case TokenShiftLeftAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt<<TopInt, false);
            break;
        case TokenShiftRightAssign:
            //ResultInt = ExpressionAssignInt(Parser, BottomValue,
            //    BottomInt>>TopInt, false);
            if (BottomValue.  Typ.  Base == TypeUnsignedInt || BottomValue.  Typ.  Base == TypeUnsignedLong)
                ResultInt = ExpressionAssignInt(Parser, BottomValue, cast(ulong) BottomInt >> TopInt, false);
            else
                ResultInt = ExpressionAssignInt(Parser, BottomValue, BottomInt >> TopInt, false);
            break;
        case TokenArithmeticAndAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt&TopInt, false);
            break;
        case TokenArithmeticOrAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt|TopInt, false);
            break;
        case TokenArithmeticExorAssign:
            ResultInt = ExpressionAssignInt(Parser, BottomValue,
                BottomInt^TopInt, false);
            break;
        case TokenLogicalOr:
            ResultInt = BottomInt || TopInt;
            break;
        case TokenLogicalAnd:
            ResultInt = BottomInt && TopInt;
            break;
        case TokenArithmeticOr:
            ResultInt = BottomInt | TopInt;
            break;
        case TokenArithmeticExor:
            ResultInt = BottomInt ^ TopInt;
            break;
        case TokenAmpersand:
            ResultInt = BottomInt & TopInt;
            break;
        case TokenEqual:
            ResultInt = BottomInt == TopInt;
            break;
        case TokenNotEqual:
            ResultInt = BottomInt != TopInt;
            break;
        case TokenLessThan:
            ResultInt = BottomInt < TopInt;
            break;
        case TokenGreaterThan:
            ResultInt = BottomInt > TopInt;
            break;
        case TokenLessEqual:
            ResultInt = BottomInt <= TopInt;
            break;
        case TokenGreaterEqual:
            ResultInt = BottomInt >= TopInt;
            break;
        case TokenShiftLeft:
            ResultInt = BottomInt << TopInt;
            break;
        case TokenShiftRight:
            ResultInt = BottomInt >> TopInt;
            break;
        case TokenPlus:
            ResultInt = BottomInt + TopInt;
            break;
        case TokenMinus:
            ResultInt = BottomInt - TopInt;
            break;
        case TokenAsterisk:
            ResultInt = BottomInt * TopInt;
            break;
        case TokenSlash:
            ResultInt = BottomInt / TopInt;
            break;
        case TokenModulus:
            ResultInt = BottomInt % TopInt;
            break;
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }
        ExpressionPushInt(Parser, StackTop, ResultInt);
    } else if (BottomValue.  Typ.  Base == TypePointer &&
            IS_NUMERIC_COERCIBLE(TopValue)) {
        /* pointer/integer infix arithmetic */
        long TopInt = ExpressionCoerceInteger(TopValue);

        if (Op == TokenEqual || Op == TokenNotEqual) {
            /* comparison to a NULL pointer */
            if (TopInt != 0)
                ProgramFail(Parser, "invalid operation");

            if (Op == TokenEqual)
                ExpressionPushInt(Parser, StackTop,
                    BottomValue.  Val.  Pointer == NULL);
            else
                ExpressionPushInt(Parser, StackTop,
                    BottomValue.  Val.  Pointer != NULL);
        } else if (Op == TokenPlus || Op == TokenMinus) {
            /* pointer arithmetic */
            int Size = TypeSize(BottomValue.  Typ.  FromType, 0, true);

            Pointer = BottomValue.  Val.  Pointer;
            if (Pointer == NULL)
                ProgramFail(Parser, "c. invalid use of a NULL pointer");

            if (Op == TokenPlus)
                Pointer = cast(void*)(cast(char*)Pointer + TopInt * Size);
            else
                Pointer = cast(void*)(cast(char*)Pointer - TopInt * Size);

            StackValue = ExpressionStackPushValueByType(Parser, StackTop,
                BottomValue.  Typ);
            StackValue.  Val.  Pointer = Pointer;
        } else if (Op == TokenAssign && TopInt == 0) {
            /* assign a NULL pointer */
            HeapUnpopStack(Parser.  pc, (Value.sizeof));
            ExpressionAssign(Parser, BottomValue, TopValue, false, NULL, 0, false);
            ExpressionStackPushValueNode(Parser, StackTop, BottomValue);
        } else if (Op == TokenAddAssign || Op == TokenSubtractAssign) {
            /* pointer arithmetic */
            int Size = TypeSize(BottomValue.  Typ.  FromType, 0, true);

            Pointer = BottomValue.  Val.  Pointer;
            if (Pointer == NULL)
                ProgramFail(Parser, "d. invalid use of a NULL pointer");

            if (Op == TokenAddAssign)
                Pointer = cast(void*)(cast(char*)Pointer + TopInt * Size);
            else
                Pointer = cast(void*)(cast(char*)Pointer - TopInt * Size);

            HeapUnpopStack(Parser.  pc, (Value.sizeof));
            BottomValue.  Val.  Pointer = Pointer;
            ExpressionStackPushValueNode(Parser, StackTop, BottomValue);
        } else
            ProgramFail(Parser, "invalid operation");
    } else if (BottomValue.  Typ.  Base == TypePointer &&
            TopValue.  Typ.  Base == TypePointer && Op != TokenAssign) {
        /* pointer/pointer operations */
        char *TopLoc = cast(char*)TopValue.  Val.  Pointer;
        char *BottomLoc = cast(char*)BottomValue.  Val.  Pointer;

        switch (Op) {
        case TokenEqual:
            ExpressionPushInt(Parser, StackTop, BottomLoc == TopLoc);
            break;
        case TokenNotEqual:
            ExpressionPushInt(Parser, StackTop, BottomLoc != TopLoc);
            break;
        case TokenMinus:
            ExpressionPushInt(Parser, StackTop, BottomLoc - TopLoc);
            break;
        default:
            ProgramFail(Parser, "invalid operation");
            break;
        }
    } else if (Op == TokenAssign) {
        /* assign a non-numeric type */
        HeapUnpopStack(Parser.  pc, Value.sizeof);
        /* XXX - possible bug if lvalue is a temp value and takes more
            than sizeof(Value) */
        ExpressionAssign(Parser, BottomValue, TopValue, false, NULL, 0, false);
        ExpressionStackPushValueNode(Parser, StackTop, BottomValue);
    } else if (Op == TokenCast) {
        /* cast a value to a different type */
        /* XXX - possible bug if the destination type takes more than s
            izeof(Value) + sizeof(ValueType *) */
        Value  *ValueLoc = ExpressionStackPushValueByType(Parser, StackTop,
            BottomValue.  Val.  Typ);
        ExpressionAssign(Parser, ValueLoc, TopValue, true, NULL, 0, true);
    } else
        ProgramFail(Parser, "invalid operation");
}

/* take the contents of the expression stack and compute the top until
    there's nothing greater than the given precedence */
void ExpressionStackCollapse(ParseState *Parser,
    ExpressionStack **StackTop, int Precedence, int *IgnorePrecedence)
{
    int FoundPrecedence = Precedence;
    Value  *TopValue;
    Value  *BottomValue;
    ExpressionStack *TopStackNode = *StackTop;
    ExpressionStack *TopOperatorNode;

    while (TopStackNode != NULL && TopStackNode.  Next != NULL &&
            FoundPrecedence >= Precedence) {
        /* find the top operator on the stack */
        if (TopStackNode.  Order == OrderNone)
            TopOperatorNode = TopStackNode.  Next;
        else
            TopOperatorNode = TopStackNode;

        FoundPrecedence = TopOperatorNode.  Precedence;

        /* does it have a high enough precedence? */
        if (FoundPrecedence >= Precedence && TopOperatorNode != NULL) {
            /* execute this operator */
            switch (TopOperatorNode.  Order) {
            case OrderPrefix:
                /* prefix evaluation */
                TopValue = TopStackNode.  Val;

                /* pop the value and then the prefix operator - assume
                    they'll still be there until we're done */
                HeapPopStack(Parser.  pc, NULL, cast(int)((ExpressionStack.sizeof) + (Value.sizeof) + TypeStackSizeValue(TopValue)) );
                HeapPopStack(Parser.  pc, TopOperatorNode, cast(int) (ExpressionStack.sizeof));
                *StackTop = TopOperatorNode.  Next;

                /* do the prefix operation */
                if (Parser.  Mode == RunModeRun /* && FoundPrecedence < *IgnorePrecedence */) {
                    /* run the operator */
                    ExpressionPrefixOperator(Parser, StackTop,
                        TopOperatorNode.  Op, TopValue);
                } else {
                    /* we're not running it so just return 0 */
                    ExpressionPushInt(Parser, StackTop, 0);
                }
                break;
            case OrderPostfix:
                /* postfix evaluation */
                TopValue = TopStackNode.  Next.  Val;

                /* pop the postfix operator and then the value - assume
                    they'll still be there until we're done */
                HeapPopStack(Parser.  pc, NULL, (ExpressionStack.sizeof));
                HeapPopStack(Parser.  pc, TopValue, cast(int)(ExpressionStack.sizeof + Value.sizeof + TypeStackSizeValue(TopValue)));
                *StackTop = TopStackNode.  Next.  Next;

                /* do the postfix operation */
                if (Parser.  Mode == RunModeRun /* && FoundPrecedence < *IgnorePrecedence */) {
                    /* run the operator */
                    ExpressionPostfixOperator(Parser, StackTop,
                        TopOperatorNode.  Op, TopValue);
                } else {
                    /* we're not running it so just return 0 */
                    ExpressionPushInt(Parser, StackTop, 0);
                }
                break;
            case OrderInfix:
                /* infix evaluation */
                TopValue = TopStackNode.  Val;
                if (TopValue != NULL) {
                    BottomValue = TopOperatorNode.  Next.  Val;

                    /* pop a value, the operator and another value - assume
                        they'll still be there until we're done */
                    HeapPopStack(Parser.  pc, NULL, cast(int)( ExpressionStack.sizeof + Value.sizeof + TypeStackSizeValue(TopValue)));
                    HeapPopStack(Parser.  pc, NULL, (ExpressionStack.sizeof));
                    HeapPopStack(Parser.  pc, BottomValue, cast(int)( ExpressionStack.sizeof + Value.sizeof + TypeStackSizeValue(BottomValue)));
                    *StackTop = TopOperatorNode.  Next.  Next;

                    /* do the infix operation */
                    if (Parser.  Mode == RunModeRun /* && FoundPrecedence <= *IgnorePrecedence */) {
                        /* run the operator */
                        ExpressionInfixOperator(Parser, StackTop,
                            TopOperatorNode.  Op, BottomValue, TopValue);
                    } else {
                        /* we're not running it so just return 0 */
                        ExpressionPushInt(Parser, StackTop, 0);
                    }
                } else
                    FoundPrecedence = -1;
                break;
            case OrderNone:
            default:
                /* this should never happen */
                assert(TopOperatorNode.  Order != OrderNone);
                break;
            }

            /* if we've returned above the ignored precedence level
                turn ignoring off */
            if (FoundPrecedence <= *IgnorePrecedence)
                *IgnorePrecedence = DEEP_PRECEDENCE;
        }
        TopStackNode = *StackTop;
    }
}

/* push an operator on to the expression stack */
void ExpressionStackPushOperator(ParseState *Parser,
    ExpressionStack **StackTop, OperatorOrder Order,
    LexToken   Token, int Precedence)
{
    ExpressionStack *StackNode = cast(ExpressionStack*) VariableAlloc(Parser.  pc, Parser, ExpressionStack.sizeof, false);
    StackNode.  Next = *StackTop;
    assert(Order <= 255);
    StackNode.  Order = cast(char) Order;
    StackNode.  Op = Token;
    StackNode.  Precedence = cast(ushort) Precedence;
    *StackTop = StackNode;

//    StackNode.  Line = Parser.  Line;
//    StackNode.  CharacterPos = Parser.  CharacterPos;

}

/* do the '.' and '.  ' operators */
void ExpressionGetStructElement(ParseState *Parser,
    ExpressionStack **StackTop, LexToken   Token)
{
    Value  *Ident;

    /* get the identifier following the '.' or '.  ' */
    if (LexGetToken(Parser, &Ident, true) != TokenIdentifier)
        ProgramFail(Parser, "need an structure or union member after '%s'",
            (Token == TokenDot) ? "." : ".  ");

    if (Parser.  Mode == RunModeRun) {
        /* look up the struct element */
        Value  *ParamVal = (*StackTop).  Val;
        Value  *StructVal = ParamVal;
        ValueType *StructType = ParamVal.  Typ;
        char *DerefDataLoc = cast(char *)ParamVal.  Val;
        Value  *MemberValue = NULL;
        Value  *Result;

        /* if we're doing '.  ' dereference the struct pointer first */
        if (Token == TokenArrow)
            DerefDataLoc = cast(char*) VariableDereferencePointer(ParamVal, &StructVal,
                NULL, &StructType, NULL);

        if (StructType.  Base != TypeStruct && StructType.  Base != TypeUnion)
            ProgramFail(Parser,
                "can't use '%s' on something that's not a struct or union %s : it's a %t",
                (Token == TokenDot) ? "." : ".  ",
                (Token == TokenArrow) ? "pointer" : "", ParamVal.  Typ);

        if (!TableGet(StructType.  Members, Ident.  Val.  Identifier,
                &MemberValue, NULL, NULL, NULL))
            ProgramFail(Parser, "doesn't have a member called '%s'",
                Ident.  Val.  Identifier);

        /* pop the value - assume it'll still be there until we're done */
        HeapPopStack(Parser.  pc, ParamVal, cast(int)( ExpressionStack.sizeof + Value.sizeof + TypeStackSizeValue(StructVal)));
        *StackTop = (*StackTop).  Next;

        /* make the result value for this member only */
        Result = VariableAllocValueFromExistingData(Parser, MemberValue.  Typ,
            cast(AnyValue*) cast(void*)(DerefDataLoc + MemberValue.  Val.  Integer), true,
            (StructVal != NULL) ? StructVal.  LValueFrom : NULL);
        ExpressionStackPushValueNode(Parser, StackTop, Result);
    }
}

/* parse an expression with operator precedence */
int ExpressionParse(ParseState *Parser, Value  **Result)
{
    int PrefixState = true;
    int Done = false;
    int BracketPrecedence = 0;
    int LocalPrecedence;
    int Precedence = 0;
    int IgnorePrecedence = DEEP_PRECEDENCE;
    int TernaryDepth = 0;
    Value  *LexValue;
    ExpressionStack *StackTop = NULL;
    do 
    {
        ParseState PreState;
        LexToken   Token;

        ParserCopy(&PreState, Parser);
        Token = LexGetToken(Parser, &LexValue, true);
        if (((cast(int)Token > TokenComma && cast(int)Token <= cast(int)TokenOpenBracket) ||
               (Token == TokenCloseBracket && BracketPrecedence != 0)) &&
               (Token != TokenColon || TernaryDepth > 0)) {
            /* it's an operator with precedence */
            if (PrefixState) {
                /* expect a prefix operator */
                if (OperatorPrecedence[cast(int)Token].PrefixPrecedence == 0)
                    ProgramFail(Parser, "operator not expected here");

                LocalPrecedence = OperatorPrecedence[cast(int)Token].PrefixPrecedence;
                Precedence = BracketPrecedence + LocalPrecedence;

                if (Token == TokenOpenBracket) {
                    /* it's either a new bracket level or a cast */
                    LexToken   BracketToken = LexGetToken(Parser, &LexValue, false);
                    if (IsTypeToken(Parser, BracketToken, LexValue) &&
                            (StackTop == NULL || StackTop.  Op != TokenSizeof)) {
                        /* it's a cast - get the new type */
                        ValueType *CastType;
                        char *CastIdentifier;
                        Value  *CastTypeValue;

                        TypeParse(Parser, &CastType, &CastIdentifier, NULL);
                        if (LexGetToken(Parser, &LexValue, true) != TokenCloseBracket)
                            ProgramFail(Parser, "brackets not closed");

                        /* scan and collapse the stack to the precedence of
                            this infix cast operator, then push */
                        Precedence = BracketPrecedence +
                            OperatorPrecedence[cast(int)TokenCast].PrefixPrecedence;

                        ExpressionStackCollapse(Parser, &StackTop, Precedence+1,
                            &IgnorePrecedence);
                        CastTypeValue = VariableAllocValueFromType(Parser.  pc,
                            Parser, &Parser.  pc.  TypeType, false, NULL, false);
                        CastTypeValue.  Val.  Typ = CastType;
                        ExpressionStackPushValueNode(Parser, &StackTop, CastTypeValue);
                        ExpressionStackPushOperator(Parser, &StackTop, OrderInfix,
                            TokenCast, Precedence);
                    } else {
                        /* boost the bracket operator precedence */
                        BracketPrecedence += BRACKET_PRECEDENCE;
                    }
                } else {
                    /* scan and collapse the stack to the precedence of
                        this operator, then push */

                    /* take some extra care for double prefix operators,
                        e.g. x = - -5, or x = **y */
                    int NextToken = LexGetToken(Parser, NULL, false);
                    int TempPrecedenceBoost = 0;
                    if (NextToken > TokenComma && NextToken < TokenOpenBracket) {
                        int NextPrecedence =
                            OperatorPrecedence[cast(int)NextToken].PrefixPrecedence;

                        /* two prefix operators with equal precedence? make
                            sure the innermost one runs first */
                        /* XXX - probably not correct, but can't find a
                            test that fails at this */
                        if (LocalPrecedence == NextPrecedence)
                            TempPrecedenceBoost = -1;
                    }

                    ExpressionStackCollapse(Parser, &StackTop, Precedence,
                        &IgnorePrecedence);
                    ExpressionStackPushOperator(Parser, &StackTop, OrderPrefix,
                        Token, Precedence + TempPrecedenceBoost);
                }
            } else {
                /* expect an infix or postfix operator */
                if (OperatorPrecedence[cast(int)Token].PostfixPrecedence != 0) {
                    switch (Token) {
                    case TokenCloseBracket:
                    case TokenRightSquareBracket:
                        if (BracketPrecedence == 0) {
                            /* assume this bracket is after the end of the
                                expression */
                            ParserCopy(Parser, &PreState);
                            Done = true;
                        } else {
                            /* collapse to the bracket precedence */
                            ExpressionStackCollapse(Parser, &StackTop,
                                BracketPrecedence, &IgnorePrecedence);
                            BracketPrecedence -= BRACKET_PRECEDENCE;
                        }
                        break;
                    default:
                        /* scan and collapse the stack to the precedence of
                            this operator, then push */
                        Precedence = BracketPrecedence +
                            OperatorPrecedence[cast(int)Token].PostfixPrecedence;
                        ExpressionStackCollapse(Parser, &StackTop, Precedence,
                            &IgnorePrecedence);
                        ExpressionStackPushOperator(Parser, &StackTop,
                            OrderPostfix, Token, Precedence);
                        break;
                    }
                } else if (OperatorPrecedence[cast(int)Token].InfixPrecedence != 0) {
                    /* scan and collapse the stack, then push */
                    Precedence = BracketPrecedence +
                        OperatorPrecedence[cast(int)Token].InfixPrecedence;

                    /* for right to left order, only go down to the next
                        higher precedence so we evaluate it in reverse order */
                    /* for left to right order, collapse down to this precedence
                        so we evaluate it in forward order */
                    if (IS_LEFT_TO_RIGHT(OperatorPrecedence[cast(int)Token].InfixPrecedence))
                        ExpressionStackCollapse(Parser, &StackTop, Precedence,
                            &IgnorePrecedence);
                    else
                        ExpressionStackCollapse(Parser, &StackTop, Precedence+1,
                            &IgnorePrecedence);

                    if (Token == TokenDot || Token == TokenArrow) {
                        /* this operator is followed by a struct element so
                            handle it as a special case */
                        ExpressionGetStructElement(Parser, &StackTop, Token);
                    } else {
                        /* if it's a && or || operator we may not need to
                            evaluate the right hand side of the expression */
                        if ((Token == TokenLogicalOr || Token == TokenLogicalAnd) &&
                                IS_NUMERIC_COERCIBLE(StackTop.  Val)) {
                            long LHSInt = ExpressionCoerceInteger(StackTop.  Val);
                            if (((Token == TokenLogicalOr && LHSInt) ||
                                    (Token == TokenLogicalAnd && !LHSInt)) &&
                                 (IgnorePrecedence > Precedence) )
                                IgnorePrecedence = Precedence;
                        }

                        /* push the operator on the stack */
                        ExpressionStackPushOperator(Parser, &StackTop,
                            OrderInfix, Token, Precedence);
                        PrefixState = true;

                        switch (Token) {
                        case TokenQuestionMark:
                            TernaryDepth++;
                            break;
                        case TokenColon:
                            TernaryDepth--;
                            break;
                        default:
                            break;
                        }
                    }

                    /* treat an open square bracket as an infix array index
                        operator followed by an open bracket */
                    if (Token == TokenLeftSquareBracket) {
                        /* boost the bracket operator precedence, then push */
                        BracketPrecedence += BRACKET_PRECEDENCE;
                    }
                } else
                    ProgramFail(Parser, "operator not expected here");
            }
        } else if (Token == TokenIdentifier) {
            /* it's a variable, function or a macro */
            if (!PrefixState)
                ProgramFail(Parser, "identifier not expected here");

            if (LexGetToken(Parser, NULL, false) == TokenOpenBracket) {
                ExpressionParseFunctionCall(Parser, &StackTop,
                    LexValue.  Val.  Identifier,
                    Parser.  Mode == RunModeRun && Precedence < IgnorePrecedence);
            } else {
                if (Parser.  Mode == RunModeRun /* && Precedence < IgnorePrecedence */) 
                {
                    Value  *VariableValue = NULL;

                    VariableGet(Parser.pc, Parser, LexValue.Val.Identifier, &VariableValue);
                    if (VariableValue.Typ.Base == TypeMacro) 
                    {
                        /* evaluate a macro as a kind of simple subroutine */
                        ParseState MacroParser;
                        Value  *MacroResult;

                        ParserCopy(&MacroParser, &VariableValue.Val.MacroDef_.Body);
                        MacroParser.Mode = Parser.  Mode;
                        if (VariableValue.Val.MacroDef_.NumParams != 0)
                            ProgramFail(&MacroParser, "macro arguments missing");

                        if (!ExpressionParse(&MacroParser, &MacroResult) ||
                                LexGetToken(&MacroParser, NULL, false) !=
                                    TokenEndOfFunction)
                            ProgramFail(&MacroParser, "expression expected");

                        ExpressionStackPushValueNode(Parser, &StackTop, MacroResult);
                    } else if (VariableValue.  Typ == &Parser.  pc.  VoidType)
                        ProgramFail(Parser, "a void value isn't much use here");
                    else
                        ExpressionStackPushLValue(Parser, &StackTop,
                        VariableValue, 0); /* it's a value variable */
                } else /* push a dummy value */
                    ExpressionPushInt(Parser, &StackTop, 0);

            }

             /* if we've successfully ignored the RHS turn ignoring off */
            if (Precedence <= IgnorePrecedence)
                IgnorePrecedence = DEEP_PRECEDENCE;

            PrefixState = false;
        } else if (cast(int)Token > TokenCloseBracket &&
                                        cast(int)Token <= TokenCharacterConstant) {
            /* it's a value of some sort, push it */
            if (!PrefixState)
                ProgramFail(Parser, "value not expected here");

            PrefixState = false;
            ExpressionStackPushValue(Parser, &StackTop, LexValue);
        } else if (IsTypeToken(Parser, Token, LexValue)) {
            /* it's a type. push it on the stack like a value.
                this is used in sizeof() */
            ValueType *Typ;
            char *Identifier;
            Value  *TypeValue;

            if (!PrefixState)
                ProgramFail(Parser, "type not expected here");

            PrefixState = false;
            ParserCopy(Parser, &PreState);
            TypeParse(Parser, &Typ, &Identifier, NULL);
            TypeValue = VariableAllocValueFromType(Parser.  pc, Parser,
                &Parser.  pc.  TypeType, false, NULL, false);
            TypeValue.  Val.  Typ = Typ;
            ExpressionStackPushValueNode(Parser, &StackTop, TypeValue);
        } else {
            /* it isn't a token from an expression */
            ParserCopy(Parser, &PreState);
            Done = true;
        }
    } while (!Done);

    /* check that brackets have been closed */
    if (BracketPrecedence > 0)
        ProgramFail(Parser, "brackets not closed");

    /* scan and collapse the stack to precedence 0 */
    ExpressionStackCollapse(Parser, &StackTop, 0, &IgnorePrecedence);

    /* fix up the stack and return the result if we're in run mode */
    if (StackTop != NULL) {
        /* all that should be left is a single value on the stack */
        if (Parser.  Mode == RunModeRun) {
            if (StackTop.  Order != OrderNone || StackTop.  Next != NULL)
                ProgramFail(Parser, "invalid expression");

            *Result = StackTop.  Val;
            HeapPopStack(Parser.  pc, StackTop, (ExpressionStack.sizeof));
        } else
            HeapPopStack(Parser.  pc, StackTop.  Val, cast(int)( ExpressionStack.sizeof + Value.sizeof + TypeStackSizeValue(StackTop.  Val)));
    }

    return StackTop != NULL;
}

/* do a parameterized macro call */
void ExpressionParseMacroCall(ParseState *Parser,
    ExpressionStack **StackTop, const(char) *MacroName,
    MacroDef *MDef)
{
    int ArgCount;
    LexToken   Token;
    Value  *ReturnValue = NULL;
    Value  *Param;
    Value  **ParamArray = NULL;

    if (Parser.  Mode == RunModeRun) {
        /* create a stack frame for this macro */
        /* largest return type there is */
        ExpressionStackPushValueByType(Parser, StackTop, &Parser.  pc.  FPType);
        ReturnValue = (*StackTop).  Val;
        HeapPushStackFrame(Parser.  pc);
        ParamArray = cast(Value**) HeapAllocStack(Parser.pc, cast(int) ( (Value*).sizeof * MDef.NumParams ) );
        if (ParamArray == NULL)
            ProgramFail(Parser, "(ExpressionParseMacroCall) out of memory");
    } else
        ExpressionPushInt(Parser, StackTop, 0);

    /* parse arguments */
    ArgCount = 0;
    do {
        if (ExpressionParse(Parser, &Param)) {
            if (Parser.  Mode == RunModeRun) {
                if (ArgCount < MDef.  NumParams)
                    ParamArray[ArgCount] = Param;
                else
                    ProgramFail(Parser, "too many arguments to %s()", MacroName);
            }

            ArgCount++;
            Token = LexGetToken(Parser, NULL, true);
            if (Token != TokenComma && Token != TokenCloseBracket)
                ProgramFail(Parser, "comma expected");
        } else {
            /* end of argument list? */
            Token = LexGetToken(Parser, NULL, true);
            if (Token != TokenCloseBracket)
                ProgramFail(Parser, "bad argument");
        }

    } while (Token != TokenCloseBracket);

    if (Parser.  Mode == RunModeRun) {
        /* evaluate the macro */
        ParseState MacroParser;
        int Count;
        Value  *EvalValue;

        if (ArgCount < MDef.  NumParams)
            ProgramFail(Parser, "not enough arguments to '%s'", MacroName);

        if (MDef.  Body.Pos == NULL)
            ProgramFail(Parser,
                "ExpressionParseMacroCall MacroName: '%s' is undefined",
                MacroName);

        ParserCopy(&MacroParser, &MDef.  Body);
        MacroParser.Mode = Parser.  Mode;
        VariableStackFrameAdd(Parser, MacroName, 0);
        Parser.  pc.  TopStackFrame.  NumParams = ArgCount;
        Parser.  pc.  TopStackFrame.  ReturnValue = ReturnValue;
        for (Count = 0; Count < MDef.  NumParams; Count++)
            VariableDefine(Parser.  pc, Parser, MDef.  ParamName[Count],
                ParamArray[Count], NULL, true);

        ExpressionParse(&MacroParser, &EvalValue);
        ExpressionAssign(Parser, ReturnValue, EvalValue, true, MacroName, 0, false);
        VariableStackFramePop(Parser);
        HeapPopStackFrame(Parser.  pc);
    }
}

/* do a function call */
void ExpressionParseFunctionCall(ParseState *Parser,
    ExpressionStack **StackTop, const(char)*FuncName, int RunIt)
{
    int ArgCount;
    LexToken   Token = LexGetToken(Parser, NULL, true);    /* open bracket */
    RunMode OldMode = Parser.Mode;
    Value  *ReturnValue = NULL;
    Value  *FuncValue = NULL;
    Value  *Param;
    Value  **ParamArray = NULL;

    if (RunIt) {
        /* get the function definition */
        VariableGet(Parser.  pc, Parser, FuncName, &FuncValue);

        if (FuncValue.  Typ.  Base == TypeMacro) {
            /* this is actually a macro, not a function */
            ExpressionParseMacroCall(Parser, StackTop, FuncName,
                &FuncValue.  Val.  MacroDef_);
            return;
        }

        if (FuncValue.  Typ.  Base != TypeFunction)
            ProgramFail(Parser, "%t is not a function - can't call",
                FuncValue.  Typ);

        ExpressionStackPushValueByType(Parser, StackTop,
            FuncValue.  Val.  FuncDef_.ReturnType);
        ReturnValue = (*StackTop).  Val;
        HeapPushStackFrame(Parser.  pc);
        ParamArray = cast(Value**) HeapAllocStack(Parser.pc, cast(int) ( (Value*).sizeof * FuncValue.Val.FuncDef_.NumParams) );
        if (ParamArray == NULL)
            ProgramFail(Parser, "(ExpressionParseFunctionCall) out of memory");
    } else {
        ExpressionPushInt(Parser, StackTop, 0);
        Parser.  Mode = RunModeSkip;
    }

    /* parse arguments */
    ArgCount = 0;
    do {
        if (RunIt && ArgCount < FuncValue.  Val.  FuncDef_.NumParams)
            ParamArray[ArgCount] = VariableAllocValueFromType(Parser.  pc, Parser,
                FuncValue.  Val.  FuncDef_.ParamType[ArgCount], false, NULL, false);

        if (ExpressionParse(Parser, &Param)) {
            if (RunIt) {
                if (ArgCount < FuncValue.  Val.  FuncDef_.NumParams) {
                    ExpressionAssign(Parser, ParamArray[ArgCount], Param, true,
                        FuncName, ArgCount+1, false);
                    VariableStackPop(Parser, Param);
                } else {
                    if (!FuncValue.  Val.  FuncDef_.VarArgs)
                        ProgramFail(Parser, "too many arguments to %s()", FuncName);
                }
            }

            ArgCount++;
            Token = LexGetToken(Parser, NULL, true);
            if (Token != TokenComma && Token != TokenCloseBracket)
                ProgramFail(Parser, "comma expected");
        } else {
            /* end of argument list? */
            Token = LexGetToken(Parser, NULL, true);
            if (Token != TokenCloseBracket)
                ProgramFail(Parser, "bad argument");
        }

    } while (Token != TokenCloseBracket);

    if (RunIt) {
        /* run the function */
        if (ArgCount < FuncValue.  Val.  FuncDef_.NumParams)
            ProgramFail(Parser, "not enough arguments to '%s'", FuncName);

        if (FuncValue.Val.FuncDef_.Intrinsic == NULL) {
            /* run a user-defined function */
            int Count;
            int OldScopeID = Parser.  ScopeID;
            ParseState FuncParser;

            if (FuncValue.  Val.  FuncDef_.Body.Pos == NULL)
                ProgramFail(Parser,
                    "ExpressionParseFunctionCall FuncName: '%s' is undefined",
                    FuncName);

            ParserCopy(&FuncParser, &FuncValue.  Val.  FuncDef_.Body);
            VariableStackFrameAdd(Parser, FuncName,
                FuncValue.  Val.  FuncDef_.Intrinsic ? FuncValue.  Val.  FuncDef_.NumParams : 0);
            Parser.  pc.  TopStackFrame.  NumParams = ArgCount;
            Parser.  pc.  TopStackFrame.  ReturnValue = ReturnValue;

            /* Function parameters should not go out of scope */
            Parser.  ScopeID = -1;

            for (Count = 0; Count < FuncValue.  Val.  FuncDef_.NumParams; Count++)
                VariableDefine(Parser.  pc, Parser,
                    FuncValue.  Val.  FuncDef_.ParamName[Count], ParamArray[Count],
                    NULL, true);

            Parser.  ScopeID = OldScopeID;

            if (ParseStatement(&FuncParser, true) != ParseResultOk)
                ProgramFail(&FuncParser, "function body expected");

            if (RunIt) {
                if (FuncParser.Mode == RunModeRun &&
                        FuncValue.  Val.  FuncDef_.ReturnType != &Parser.  pc.  VoidType)
                    ProgramFail(&FuncParser,
                        "no value returned from a function returning %t",
                        FuncValue.  Val.  FuncDef_.ReturnType);

                else if (FuncParser.Mode == RunModeGoto)
                    ProgramFail(&FuncParser, "couldn't find goto label '%s'",
                        FuncParser.SearchGotoLabel);
            }

            VariableStackFramePop(Parser);
        } 
        else
        {
            // FIXME: too many parameters?
            alias intrinsic_function_t = void function(ParseState *Parser, Value *ReturnValue, Value **Param, int NumArgs) @nogc;
            intrinsic_function_t fun = cast(intrinsic_function_t)(FuncValue.Val.FuncDef_.Intrinsic);
            fun(Parser, ReturnValue, ParamArray, ArgCount);
        }

        HeapPopStackFrame(Parser.  pc);
    }

    Parser.  Mode = OldMode;
}

/* parse an expression */
long ExpressionParseInt(ParseState *Parser)
{
    long Result = 0;
    Value  *Val;

    if (!ExpressionParse(Parser, &Val))
        ProgramFail(Parser, "expression expected");

    if (Parser.  Mode == RunModeRun) {
        if (!IS_NUMERIC_COERCIBLE_PLUS_POINTERS(Val, true))
            ProgramFail(Parser, "integer value expected instead of %t", Val.  Typ);

        Result = ExpressionCoerceInteger(Val);
        VariableStackPop(Parser, Val);
    }

    return Result;
}

