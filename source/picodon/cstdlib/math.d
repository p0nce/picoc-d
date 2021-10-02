module picodon.cstdlib.math;

import core.stdc.math;
import picodon.interpreter;
import picodon.variable;

@nogc:

static immutable double M_EValue = 2.7182818284590452354;   /* e */
static immutable double M_LOG2EValue = 1.4426950408889634074;   /* log_2 e */
static immutable double M_LOG10EValue = 0.43429448190325182765;  /* log_10 e */
static immutable double M_LN2Value = 0.69314718055994530942;  /* log_e 2 */
static immutable double M_LN10Value = 2.30258509299404568402;  /* log_e 10 */
static immutable double M_PIValue = 3.14159265358979323846;  /* pi */
static immutable double M_PI_2Value = 1.57079632679489661923;  /* pi/2 */
static immutable double M_PI_4Value = 0.78539816339744830962;  /* pi/4 */
static immutable double M_1_PIValue = 0.31830988618379067154;  /* 1/pi */
static immutable double M_2_PIValue = 0.63661977236758134308;  /* 2/pi */
static immutable double M_2_SQRTPIValue = 1.12837916709551257390;  /* 2/sqrt(pi) */
static immutable double M_SQRT2Value = 1.41421356237309504880;  /* sqrt(2) */
static immutable double M_SQRT1_2Value =  0.70710678118654752440;  /* 1/sqrt(2) */


void MathSin(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = sin(Param[0].Val.FP);
}

void MathCos(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = cos(Param[0].Val.FP);
}

void MathTan(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = tan(Param[0].Val.FP);
}

void MathAsin(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = asin(Param[0].Val.FP);
}

void MathAcos(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = acos(Param[0].Val.FP);
}

void MathAtan(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = atan(Param[0].Val.FP);
}

void MathAtan2(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = atan2(Param[0].Val.FP, Param[1].Val.FP);
}

void MathSinh(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = sinh(Param[0].Val.FP);
}

void MathCosh(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = cosh(Param[0].Val.FP);
}

void MathTanh(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = tanh(Param[0].Val.FP);
}

void MathExp(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = exp(Param[0].Val.FP);
}

void MathFabs(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = fabs(Param[0].Val.FP);
}

void MathFmod(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = fmod(Param[0].Val.FP, Param[1].Val.FP);
}

void MathFrexp(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = frexp(Param[0].Val.FP, cast(int*) Param[1].Val.Pointer);
}

void MathLdexp(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = ldexp(Param[0].Val.FP, Param[1].Val.Integer);
}

void MathLog(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = log(Param[0].Val.FP);
}

void MathLog10(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = log10(Param[0].Val.FP);
}

void MathModf(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = modf(Param[0].Val.FP, cast(double*) Param[0].Val.Pointer);
}

void MathPow(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = pow(Param[0].Val.FP, Param[1].Val.FP);
}

void MathSqrt(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = sqrt(Param[0].Val.FP);
}

void MathRound(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    /* this awkward definition of "round()" due to it being inconsistently
     * declared in math.h */
    ReturnValue.Val.FP = ceil(Param[0].Val.FP - 0.5);
}

void MathCeil(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = ceil(Param[0].Val.FP);
}

void MathFloor(ParseState *Parser, Value *ReturnValue,
    Value **Param, int NumArgs)
{
    ReturnValue.Val.FP = floor(Param[0].Val.FP);
}

/* all math.h functions */
LibraryFunction[] MathFunctions =
[
     {&MathAcos, "float acos(float);"},
     {&MathAsin, "float asin(float);"},
     {&MathAtan, "float atan(float);"},
     {&MathAtan2, "float atan2(float, float);"},
     {&MathCeil, "float ceil(float);"},
     {&MathCos, "float cos(float);"},
     {&MathCosh, "float cosh(float);"},
     {&MathExp, "float exp(float);"},
     {&MathFabs, "float fabs(float);"},
     {&MathFloor, "float floor(float);"},
     {&MathFmod, "float fmod(float, float);"},
     {&MathFrexp, "float frexp(float, int *);"},
     {&MathLdexp, "float ldexp(float, int);"},
     {&MathLog, "float log(float);"},
     {&MathLog10, "float log10(float);"},
     {&MathModf, "float modf(float, float *);"},
     {&MathPow, "float pow(float,float);"},
     {&MathRound, "float round(float);"},
     {&MathSin, "float sin(float);"},
     {&MathSinh, "float sinh(float);"},
     {&MathSqrt, "float sqrt(float);"},
     {&MathTan,  "float tan(float);"},
     {&MathTanh, "float tanh(float);"},
     {NULL,  NULL }
];

/* creates various system-dependent definitions */
void MathSetupFunc(Picoc *pc)
{
    VariableDefinePlatformVar(pc, NULL, "M_E", &pc.FPType, cast(AnyValue*)&M_EValue, false);
    VariableDefinePlatformVar(pc, NULL, "M_LOG2E", &pc.FPType, cast(AnyValue*)&M_LOG2EValue, false);
    VariableDefinePlatformVar(pc, NULL, "M_LOG10E", &pc.FPType, cast(AnyValue*)&M_LOG10EValue, false);
    VariableDefinePlatformVar(pc, NULL, "M_LN2", &pc.FPType, cast(AnyValue*)&M_LN2Value, false);
    VariableDefinePlatformVar(pc, NULL, "M_LN10", &pc.FPType, cast(AnyValue*)&M_LN10Value, false);
    VariableDefinePlatformVar(pc, NULL, "M_PI", &pc.FPType, cast(AnyValue*)&M_PIValue, false);
    VariableDefinePlatformVar(pc, NULL, "M_PI_2", &pc.FPType, cast(AnyValue*)&M_PI_2Value, false);
    VariableDefinePlatformVar(pc, NULL, "M_PI_4", &pc.FPType, cast(AnyValue*)&M_PI_4Value, false);
    VariableDefinePlatformVar(pc, NULL, "M_1_PI", &pc.FPType, cast(AnyValue*)&M_1_PIValue, false);
    VariableDefinePlatformVar(pc, NULL, "M_2_PI", &pc.FPType, cast(AnyValue*)&M_2_PIValue, false);
    VariableDefinePlatformVar(pc, NULL, "M_2_SQRTPI", &pc.FPType, cast(AnyValue*)&M_2_SQRTPIValue, false);
    VariableDefinePlatformVar(pc, NULL, "M_SQRT2", &pc.FPType, cast(AnyValue*)&M_SQRT2Value, false);
    VariableDefinePlatformVar(pc, NULL, "M_SQRT1_2", &pc.FPType, cast(AnyValue*)&M_SQRT1_2Value, false);
}