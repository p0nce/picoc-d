/* picoc include system - can emulate system includes from built-in libraries
 * or it can include and parse files if the system has files */
module picocd.include;

import core.stdc.string: strcmp, strlen;

import picocd.interpreter;
import picocd.parse;
import picocd.heap;
import picocd.table;
import picocd.variable;
import picocd.clibrary;
import picocd.platform;

import picocd.cstdlib.ctype;
import picocd.cstdlib.errno;
import picocd.cstdlib.math;
import picocd.cstdlib.stdbool;
import picocd.cstdlib.stdio;
import picocd.cstdlib.stdlib;
import picocd.cstdlib.string;
import picocd.cstdlib.time;
import picocd.cstdlib.unistd;

@nogc:


/* initialize the built-in include libraries */
void IncludeInit(Picoc *pc)
{
    IncludeRegister(pc, "ctype.h", NULL, &StdCtypeFunctions[0], NULL);
    IncludeRegister(pc, "errno.h", &StdErrnoSetupFunc, NULL, NULL);
    IncludeRegister(pc, "math.h", &MathSetupFunc, &MathFunctions[0], NULL);
    IncludeRegister(pc, "stdbool.h", &StdboolSetupFunc, NULL, StdboolDefs.ptr);
    IncludeRegister(pc, "stdio.h", &StdioSetupFunc, cast(LibraryFunction*) &StdioFunctions[0], StdioDefs.ptr); // const_cast here
    IncludeRegister(pc, "stdlib.h", &StdlibSetupFunc, cast(LibraryFunction*)&StdlibFunctions[0], NULL); // const_cast here
    IncludeRegister(pc, "string.h", &StringSetupFunc, cast(LibraryFunction*) &StringFunctions[0], NULL); // const_cast here
//    IncludeRegister(pc, "time.h", &StdTimeSetupFunc, &StdTimeFunctions[0], StdTimeDefs);
//# ifndef WIN32
    //IncludeRegister(pc, "unistd.h", &UnistdSetupFunc, &UnistdFunctions[0], UnistdDefs);
//# endif
}

/* clean up space used by the include system */
void IncludeCleanup(Picoc *pc)
{
    IncludeLibrary *ThisInclude = pc.IncludeLibList;
    IncludeLibrary *NextInclude;

    while (ThisInclude != NULL) 
    {
        NextInclude = ThisInclude.NextLib;
        HeapFreeMem(pc, ThisInclude);
        ThisInclude = NextInclude;
    }

    pc.IncludeLibList = NULL;
}

/* register a new build-in include file */
void IncludeRegister(Picoc *pc, const(char)*IncludeName,
    void function(Picoc *pc) @nogc SetupFunction ,
    LibraryFunction* FuncList,
    const(char) *SetupCSource)
{
    IncludeLibrary *NewLib = cast(IncludeLibrary*) HeapAllocMem(pc, IncludeLibrary.sizeof);
    NewLib.IncludeName = TableStrRegister(pc, IncludeName);
    NewLib.SetupFunction = SetupFunction;
    NewLib.FuncList = FuncList;
    NewLib.SetupCSource = SetupCSource;
    NewLib.NextLib = pc.IncludeLibList;
    pc.IncludeLibList = NewLib;
}

/* include all of the system headers */
void PicocIncludeAllSystemHeaders(Picoc *pc)
{
    IncludeLibrary *ThisInclude = pc.IncludeLibList;

    for (; ThisInclude != NULL; ThisInclude = ThisInclude.NextLib)
        IncludeFile(pc, ThisInclude.IncludeName);
}

/* include one of a number of predefined libraries, or perhaps an actual file */
void IncludeFile(Picoc *pc, char *FileName)
{
    IncludeLibrary *LInclude;

    /* scan for the include file name to see if it's in our list
        of predefined includes */
    for (LInclude = pc.IncludeLibList; LInclude != NULL;
            LInclude = LInclude.NextLib) {
        if (strcmp(LInclude.IncludeName, FileName) == 0) {
            /* found it - protect against multiple inclusion */
            if (!VariableDefined(pc, FileName)) {
                VariableDefine(pc, NULL, FileName, NULL, &pc.VoidType, false);

                /* run an extra startup function if there is one */
                if (LInclude.SetupFunction != NULL)
                    (*LInclude.SetupFunction)(pc);

                /* parse the setup C source code - may define types etc. */
                if (LInclude.SetupCSource != NULL)
                    PicocParse(pc, FileName, LInclude.SetupCSource, cast(int) strlen(LInclude.SetupCSource), true, true, false, false);

                /* set up the library functions */
                if (LInclude.FuncList != NULL)
                    LibraryAdd(pc, LInclude.FuncList);
            }

            return;
        }
    }

    /* not a predefined file, read a real file */
    PicocPlatformScanFile(pc, FileName);
}

