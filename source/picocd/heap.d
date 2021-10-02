/* picoc heap memory allocation. */
module picocd.heap;

import core.stdc.stdlib: malloc, free, calloc;
import core.stdc.string: memset;
import picocd.interpreter;
import picocd.platform;

/* stack grows up from the bottom and heap grows down from
    the top of heap space */

nothrow:
@nogc:

/* initialize the stack and heap storage */
void HeapInit(Picoc *pc, int StackOrHeapSize)
{
    int Count;
    int AlignOffset = 0;

    pc.HeapMemory = cast(char*) malloc(StackOrHeapSize);
    pc.HeapBottom = NULL;  /* the bottom of the (downward-growing) heap */
    pc.StackFrame_ = NULL;  /* the current stack frame */
    pc.HeapStackTop = NULL;  /* the top of the stack */

    while ((cast(ulong)&pc.HeapMemory[AlignOffset] & ((ALIGN_TYPE.sizeof)-1)) != 0)
        AlignOffset++;

    pc.StackFrame_ = &(pc.HeapMemory)[AlignOffset];
    pc.HeapStackTop = &(pc.HeapMemory)[AlignOffset];
    *cast(void**)(pc.StackFrame_) = NULL;
    pc.HeapBottom =
        &(pc.HeapMemory)[StackOrHeapSize-(ALIGN_TYPE.sizeof)+AlignOffset];
    pc.FreeListBig = NULL;
    for (Count = 0; Count < FREELIST_BUCKETS; Count++)
        pc.FreeListBucket[Count] = NULL;
}

void HeapCleanup(Picoc *pc)
{
    free(pc.HeapMemory);
}

/* allocate some space on the stack, in the current stack frame
 * clears memory. can return NULL if out of stack space */
void *HeapAllocStack(Picoc *pc, size_t Size)
{
    char *NewMem = cast(char*) pc.HeapStackTop;
    char *NewTop = cast(char*)pc.HeapStackTop + MEM_ALIGN(Size);

    if (NewTop > cast(char*)pc.HeapBottom)
        return NULL;

    pc.HeapStackTop = cast(void*)NewTop;
    memset(cast(void*)NewMem, '\0', Size);
    return NewMem;
}

/* allocate some space on the stack, in the current stack frame */
void HeapUnpopStack(Picoc *pc, size_t Size)
{
    pc.HeapStackTop = cast(void*)(cast(char*)pc.HeapStackTop + MEM_ALIGN(Size));
}

/* free some space at the top of the stack */
int HeapPopStack(Picoc *pc, void *Addr, size_t Size)
{
    size_t ToLose = MEM_ALIGN(Size);
    if (ToLose > (cast(char*)pc.HeapStackTop - cast(char*)&(pc.HeapMemory)[0]))
        return false;

    pc.HeapStackTop = cast(void*)(cast(char*)pc.HeapStackTop - ToLose);
    assert(Addr == NULL || pc.HeapStackTop == Addr);

    return true;
}

/* push a new stack frame on to the stack */
void HeapPushStackFrame(Picoc *pc)
{
    *cast(void**)pc.HeapStackTop = pc.StackFrame_;
    pc.StackFrame_ = pc.HeapStackTop;
    pc.HeapStackTop = cast(void*)(cast(char*)pc.HeapStackTop +
        MEM_ALIGN(ALIGN_TYPE.sizeof));
}

/* pop the current stack frame, freeing all memory in the
    frame. can return NULL */
int HeapPopStackFrame(Picoc *pc)
{
    if (*cast(void**)pc.StackFrame_ != NULL) {
        pc.HeapStackTop = pc.StackFrame_;
        pc.StackFrame_ = *cast(void**)pc.StackFrame_;
        return true;
    } else
        return false;
}

/* allocate some dynamically allocated memory. memory is cleared.
    can return NULL if out of memory */
void *HeapAllocMem(Picoc *pc, size_t Size)
{
    return calloc(Size, 1);
}

/* free some dynamically allocated memory */
void HeapFreeMem(Picoc *pc, void *Mem)
{
    free(Mem);
}

