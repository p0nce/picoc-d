/* picoc hash table module. This hash table code is used for both symbol tables
 * and the shared string table. */
module picocd.table;

import core.stdc.string;
import picocd.interpreter;
import picocd.variable;
import picocd.platform;
import picocd.heap;
import picocd.table;

@nogc:

/* initialize the shared string system */
void TableInit(Picoc *pc)
{
    TableInitTable(&pc.StringTable, pc.StringHashTable.ptr, STRING_TABLE_SIZE, true);
    pc.StrEmpty = TableStrRegister(pc, "");
}

/* hash function for strings */
uint TableHash(const(char)*Key, int Len)
{
    uint Hash = Len;
    int Offset;
    int Count;

    for (Count = 0, Offset = 8; Count < Len; Count++, Offset+=7) {
        if (Offset > (uint.sizeof) * 8 - 7)
            Offset -= (uint.sizeof) * 8 - 6;

        Hash ^= *Key++ << Offset;
    }

    return Hash;
}

/* initialize a table */
void TableInitTable(Table *Tbl, TableEntry **HashTable, int Size, int OnHeap)
{
    assert(HashTable != null);
    Tbl.Size = cast(short)Size;
    Tbl.OnHeap = cast(short)OnHeap;
    Tbl.HashTable = HashTable;
    memset(cast(void*)HashTable, '\0', (TableEntry*).sizeof * Size);
}

/* check a hash table entry for a key */
TableEntry *TableSearch(Table *Tbl, const(char) *Key, int *AddAt)
{
    /* shared strings have unique addresses so we don't need to hash them */
    size_t HashValue = (cast(size_t)Key % Tbl.Size);
    assert(HashValue >= 0 && HashValue < Tbl.Size);
    TableEntry *Entry;

    for (Entry = Tbl.HashTable[HashValue]; Entry != NULL; Entry = Entry.Next) {
        if (Entry.p.v.Key == Key)
            return Entry;   /* found */
    }

    *AddAt = cast(int)HashValue;    /* didn't find it in the chain */
    return NULL;
}

/* set an identifier to a value. returns FALSE if it already exists.
 * Key must be a shared string from TableStrRegister() */
int TableSet(Picoc *pc, Table *Tbl, char *Key, Value *Val,
    const char *DeclFileName, int DeclLine, int DeclColumn)
{
    int AddAt;
    TableEntry *FoundEntry = TableSearch(Tbl, Key, &AddAt);

    if (FoundEntry == NULL) 
    {   /* add it to the table */
        TableEntry* NewEntry = cast(TableEntry*) VariableAlloc(pc, NULL, TableEntry.sizeof, Tbl.OnHeap);
        NewEntry.DeclFileName = DeclFileName;
        NewEntry.DeclLine = cast(ushort) DeclLine;
        NewEntry.DeclColumn = cast(ushort) DeclColumn;
        NewEntry.p.v.Key = Key;
        NewEntry.p.v.Val = Val;
        NewEntry.Next = Tbl.HashTable[AddAt];
        Tbl.HashTable[AddAt] = NewEntry;
        return true;
    }

    return false;
}

/* find a value in a table. returns FALSE if not found.
 * Key must be a shared string from TableStrRegister() */
int TableGet(Table *Tbl, const(char) *Key, Value **Val,
    const(char) **DeclFileName, int *DeclLine, int *DeclColumn)
{
    int AddAt;
    TableEntry *FoundEntry = TableSearch(Tbl, Key, &AddAt);
    if (FoundEntry == NULL)
        return false;

    *Val = FoundEntry.p.v.Val;

    if (DeclFileName != NULL) 
    {
        *DeclFileName = FoundEntry.DeclFileName;
        *DeclLine = FoundEntry.DeclLine;
        *DeclColumn = FoundEntry.DeclColumn;
    }

    return true;
}

/* remove an entry from the table */
Value *TableDelete(Picoc *pc, Table *Tbl, const(char)* Key)
{
    /* shared strings have unique addresses so we don't need to hash them */
    size_t HashValue = (cast(size_t)Key % Tbl.Size);
    assert(HashValue >= 0 && HashValue < Tbl.Size);
    TableEntry **EntryPtr;

    for (EntryPtr = &Tbl.HashTable[HashValue];
            *EntryPtr != NULL; EntryPtr = &(*EntryPtr).Next) {
        if ((*EntryPtr).p.v.Key == Key) {
            TableEntry *DeleteEntry = *EntryPtr;
            Value *Val = DeleteEntry.p.v.Val;
            *EntryPtr = DeleteEntry.Next;
            HeapFreeMem(pc, DeleteEntry);

            return Val;
        }
    }

    return NULL;
}

/* check a hash table entry for an identifier */
TableEntry *TableSearchIdentifier(Table *Tbl,
    const char *Key, int Len, int *AddAt)
{
    int HashValue = TableHash(Key, Len) % Tbl.Size;
    assert(HashValue >= 0 && HashValue < Tbl.Size);
    TableEntry *Entry;

    for (Entry = Tbl.HashTable[HashValue]; Entry != NULL; Entry = Entry.Next) {
        if (strncmp(&Entry.p.Key[0], cast(char*)Key, Len) == 0 &&
                Entry.p.Key.ptr[Len] == '\0')
            return Entry;   /* found */
    }

    *AddAt = HashValue;    /* didn't find it in the chain */
    return NULL;
}

/* set an identifier and return the identifier. share if possible */
char *TableSetIdentifier(Picoc *pc, Table *Tbl, const(char)*Ident, int IdentLen)
{
    int AddAt;
    TableEntry *FoundEntry = TableSearchIdentifier(Tbl, Ident, IdentLen, &AddAt);

    int len = cast(int) strlen(Ident);

    if (FoundEntry != NULL)
        return &FoundEntry.p.Key[0];
    else {
        /* add it to the table - we economise by not allocating
            the whole structure here */
        size_t size = TableEntry.sizeof - TableEntry.TableEntryPayload.sizeof + IdentLen + 1;
        TableEntry *NewEntry = cast(TableEntry*) HeapAllocMem(pc, size);
        if (NewEntry == NULL)
            ProgramFailNoParser(pc, "(TableSetIdentifier) out of memory");

        strncpy(NewEntry.p.Key.ptr, Ident, IdentLen);
        NewEntry.p.Key.ptr[IdentLen] = '\0';
        NewEntry.Next = Tbl.HashTable[AddAt];
        Tbl.HashTable[AddAt] = NewEntry;
        return &NewEntry.p.Key[0];
    }
}

/* register a string in the shared string store */
char *TableStrRegister2(Picoc *pc, const char *Str, int Len)
{
    return TableSetIdentifier(pc, &pc.StringTable, Str, Len);
}

char *TableStrRegister(Picoc *pc, const(char)*Str)
{
    return TableStrRegister2(pc, Str, cast(int) strlen(cast(char *)Str));
}

/* free all the strings */
void TableStrFree(Picoc *pc)
{
    int Count;
    TableEntry *Entry;
    TableEntry *NextEntry;

    for (Count = 0; Count < pc.StringTable.Size; Count++) {
        for (Entry = pc.StringTable.HashTable[Count];
                Entry != NULL; Entry = NextEntry) {
            NextEntry = Entry.Next;
            HeapFreeMem(pc, Entry);
        }
    }
}
