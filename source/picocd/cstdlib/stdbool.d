module picocd.cstdlib.stdbool;

import picocd.interpreter;
import picocd.variable;

static immutable int trueValue = 1;
static immutable int falseValue = 0;


/* structure definitions */
static string StdboolDefs = "typedef int bool;";

/* creates various system-dependent definitions */
void StdboolSetupFunc(Picoc *pc)
{
    /* defines */
    VariableDefinePlatformVar(pc, NULL, "true", &pc.IntType, cast(AnyValue*)&trueValue, false);
    VariableDefinePlatformVar(pc, NULL, "false", &pc.IntType, cast( AnyValue*)&falseValue, false);
    VariableDefinePlatformVar(pc, NULL, "__bool_true_false_are_defined", &pc.IntType, cast(AnyValue*)&trueValue, false);
}
