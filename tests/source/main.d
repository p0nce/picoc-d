import std.stdio;
import std.process;
import std.file;
import std.string;

import picocd;

void main()
{
    // check crashes
    foreach(test; TESTS)
    {
        runTestInternally(test);
    }

/*    foreach(test; TESTS)
    {
        runTestAndCheckOutput(test);
    }  */  
}

void runTestInternally(string path)
{
    writefln("Running test %s", path);
    enum PICOC_STACK_SIZE = (128000*4);

    string cfile = path ~ ".c";

    int StackSize = PICOC_STACK_SIZE;

    try
    {
        Picoc pc;
        PicocInitialize(&pc, StackSize);
        PicocPlatformScanFile(&pc, toStringz(cfile));
        PicocCallMain(&pc, 0, null);
        PicocCleanup(&pc);
    }
    catch(ProgramExitedException e)
    {
        writefln("Exited with exit code: %s", e.exitCode);
    }
    catch(Exception e)
    {
    }
}

void runTestAndCheckOutput(string path)
{
    writefln("Running test %s", path);
    enum PICOC_STACK_SIZE = (128000*4);

    string cfile = path ~ ".c";
    string expectfile = path ~ ".expect";

    string desiredOutput = cast(string) std.file.read(expectfile).idup;

    string executable = "..\\picoc-d.exe";

    auto res = executeShell(format("%s %s", executable, cfile));

    string output = res.output;
    if (output != desiredOutput)
    {
        writefln("Output = (%d bytes)", cast(int)output.length);
        writeln(output);
        writeln(cast(ubyte[])output);
        writeln;

        writefln("Expected = (%d bytes)", cast(int)desiredOutput.length);
        writeln(desiredOutput);
        writeln(cast(ubyte[])desiredOutput);
        writeln;

        assert(false);
    }
}




string[] TESTS =
[
    "00_assignment",
    "01_comment",
    "02_printf",
    "03_struct",
    "04_for",
    "05_array",
    "06_case",
    "07_function",
    "08_while",
    "09_do_while",
    "10_pointer",
    "11_precedence",
    "12_hashdefine",
    "13_integer_literals",
    "14_if",
    "15_recursion",
    "16_nesting",
    "17_enum",
    "18_include",
    "19_pointer_arithmetic",
    "20_pointer_comparison",
    "21_char_array",
    "22_floating_point",
    "23_type_coercion",
    "24_math_library",
    "25_quicksort",
    "26_character_constants",
    "28_strings",
    "29_array_address",
    "30_hanoi",
    "31_args",
    "32_led",
    "33_ternary_op",
    "34_array_assignment",
    "35_sizeof",
    "36_array_initializers",
    "37_sprintf",
    "38_multiple_array_index",
    "39_typedef",
    "40_stdio",
    "41_hashif",
    "43_void_param",
    "44_scoped_declarations",
    "45_empty_for",
    "47_switch_return",
    "48_nested_break",
    "49_bracket_evaluation",
    "50_logical_second_arg",
    "51_static",
    "52_unnamed_enum",
    "54_goto",
    "55_array_initializer",
    "56_cross_structure",
    "57_macro_bug",
    "58_return_outside",
    "59_break_before_loop",
    "60_local_vars",
    "61_initializers",
    "62_float",
    "63_typedef",
    "64_double_prefix_op",
    "66_printf_undefined",
    "67_macro_crash",
    "68_return",
    "69_shebang_script",
];