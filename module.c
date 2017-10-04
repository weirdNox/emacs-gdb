#include <assert.h>
#include "emacs-module.h"
#include "gdbwire.c"

#define internal static

#define ArrayCount(Array) (sizeof(Array) / sizeof(*(Array)))

#define InvalidCodePath assert(0)
#define InvalidDefaultCase default: { InvalidCodePath(); } break
#define IgnoreCase(Case) case (Case): break
#define IgnoreDefaultCase default: break

#define WriteVariableKey(Key, Name) Variable_##Key
#define WriteVariableName(Key, Name) #Name

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef size_t memory_size;

u32 plugin_is_GPL_compatible;

internal emacs_value Nil;
internal struct gdbwire_mi_parser *GdbMiParser;
internal struct gdbwire_mi_output *ParserOutput;
internal bool IgnoreNextPrompt;

typedef struct
{
    char *Contents;
    u32 Length;
    memory_size TotalSize;
} string_builder;

internal string_builder
AllocateString()
{
    string_builder Result = {};

    Result.TotalSize = 1<<13;
    Result.Contents = malloc(Result.TotalSize);
    assert(Result.Contents);

    return Result;
}

internal void
PushString(string_builder *Builder, char *Format, ...)
{
    if(Builder && Builder->Contents && Format)
    {
        va_list VariadicArguments;
        va_start(VariadicArguments, Format);
        u32 Length = vsnprintf(0, 0, Format, VariadicArguments);
        va_end(VariadicArguments);
        s32 Needed = Builder->Length + Length + 1 - Builder->TotalSize;
        if(Needed > 0)
        {
            Builder->TotalSize += Needed + (1<<10);
            char *TempContents = realloc(Builder->Contents, Builder->TotalSize);
            assert(TempContents);
            Builder->Contents = TempContents;
        }

        va_start(VariadicArguments, Format);
        vsnprintf(Builder->Contents + Builder->Length, Length + 1, Format, VariadicArguments);
        va_end(VariadicArguments);
        Builder->Length += Length;
    }
}

internal void
FreeString(string_builder *Builder)
{
    if(Builder && Builder->Contents)
    {
        free(Builder->Contents);
        *Builder = (string_builder){};
    }
}

internal void
GdbWireCallback(void *Context, struct gdbwire_mi_output *Output)
{
    struct gdbwire_mi_output **WriteTo = &ParserOutput;
    while(*WriteTo)
    {
        WriteTo = &(*WriteTo)->next;
    }
    *WriteTo = Output;
}

internal void *
GetResultVariable_(struct gdbwire_mi_result *Result, char *Variable,
                   enum gdbwire_mi_result_kind Kind)
{
    void *ToReturn = 0;
    if(Result && Variable)
    {
        for(;
            Result;
            Result = Result->next)
        {
            if(Result->variable && Result->kind == Kind &&
               strcmp(Variable, Result->variable) == 0)
            {
                ToReturn = Result->variant.result;
                break;
            }
        }
    }

    return ToReturn;
}
#define GetResultTuple(Result, Variable)                                \
    (struct gdbwire_mi_result *)GetResultVariable_(Result, Variable, GDBWIRE_MI_TUPLE)
#define GetResultList(Result, Variable)                                 \
    (struct gdbwire_mi_result *)GetResultVariable_(Result, Variable, GDBWIRE_MI_LIST)
#define GetResultString(Result, Variable)                               \
    (char *)GetResultVariable_(Result, Variable, GDBWIRE_MI_CSTRING)

internal void
GetBatchResultString(struct gdbwire_mi_result *Result, char *Variables[], char *Values[],
                     u32 NumberOfVariables)
{
    if(Result && Variables && Values)
    {
        u32 StartAt = 0;
        for(struct gdbwire_mi_result *Iterator = Result;
            Iterator;
            Iterator = Iterator->next)
        {
            if(Iterator->kind == GDBWIRE_MI_CSTRING && Iterator->variable)
            {
                for(int VariableIndex = StartAt;
                    VariableIndex < NumberOfVariables;
                    ++VariableIndex)
                {
                    char *Variable = Variables[VariableIndex];
                    if(!Values[VariableIndex] && strcmp(Variable, Iterator->variable) == 0)
                    {
                        if(VariableIndex == StartAt)
                        {
                            ++StartAt;
                        }
                        Values[VariableIndex] = Iterator->variant.cstring;
                        break;
                    }
                }
            }
        }
    }
}

internal inline emacs_value
GetEmacsString_(emacs_env *Environment, char *String, bool EmptyIfNull)
{
    emacs_value Result = Nil;

    if(!String && EmptyIfNull)
    {
        String = "";
    }

    if(String)
    {
        Result = Environment->make_string(Environment, String, strlen(String));
    }

    return Result;
}
#define GetEmacsString(Environment, String) GetEmacsString_(Environment, String, false)
#define GetEmacsStringEmpty(Environment, String) GetEmacsString_(Environment, String, true)

internal inline void
GetEmacsStrings(emacs_env *Environment, char *Values[], emacs_value EmacsValues[], u32 Size,
                bool EmptyIfNull)
{
    if(Values && EmacsValues)
    {
        for(int Index = 0;
            Index < Size;
            ++Index)
        {
            EmacsValues[Index] = GetEmacsString_(Environment, Values[Index], EmptyIfNull);
        }
    }
}

internal inline emacs_value
Intern(emacs_env *Environment, char *Symbol)
{
    emacs_value Result = Environment->intern(Environment, Symbol);
    return Result;
}

internal inline emacs_value
Funcall(emacs_env *Environment, char *Function, u32 NumArguments, emacs_value *Arguments)
{
    emacs_value Result = Environment->funcall(Environment, Intern(Environment, Function),
                                              NumArguments, Arguments);
    return Result;
}

internal void
BreakpointChange(emacs_env *Environment, struct gdbwire_mi_result *Breakpoint)
{
#define BreakpointVariablesWriter(W) W(Number, number), \
        W(Type, type),                                  \
        W(Disposition, disp),                           \
        W(Enabled, enabled),                            \
        W(Address, addr),                               \
        W(Function, func),                              \
        W(File, fullname),                              \
        W(Line, line),                                  \
        W(At, at),                                      \
        W(Pending, pending),                            \
        W(Thread, thread),                              \
        W(Condition, cond),                             \
        W(Times, times),                                \
        W(What, what)
    enum { BreakpointVariablesWriter(WriteVariableKey) };

    char *Variables[] = { BreakpointVariablesWriter(WriteVariableName) };
    char *Values[ArrayCount(Variables)] = {};
    GetBatchResultString(Breakpoint, Variables, Values, ArrayCount(Variables));

    emacs_value Arguments[ArrayCount(Variables)];
    GetEmacsStrings(Environment, Values, Arguments, ArrayCount(Variables), false);
    Funcall(Environment, "gdb--breakpoint-changed", ArrayCount(Arguments), Arguments);
}

internal void
BreakpointDeletion(emacs_env *Environment, char *Id)
{
    emacs_value Argument = GetEmacsString(Environment, Id);
    Funcall(Environment, "gdb--breakpoint-deleted", 1, &Argument);
}

internal void
ThreadInfo(emacs_env *Environment, struct gdbwire_mi_result *Result)
{
    struct gdbwire_mi_result *Threads = GetResultList(Result, "threads");

#define ThreadVariablesWriter(W) W(Id, id),     \
        W(TargetId, target-id),                 \
        W(Name, name),                          \
        W(State, state),                        \
        W(Core, core)
    enum { ThreadVariablesWriter(WriteVariableKey) };
    char *Variables[] = { ThreadVariablesWriter(WriteVariableName) };

    for(struct gdbwire_mi_result *Thread = Threads;
        Thread && Thread->kind == GDBWIRE_MI_TUPLE;
        Thread = Thread->next)
    {
        struct gdbwire_mi_result *ThreadContents = Thread->variant.result;
        char *Values[ArrayCount(Variables)] = {};
        GetBatchResultString(ThreadContents, Variables, Values, ArrayCount(Variables));

        emacs_value Arguments[ArrayCount(Values)];
        GetEmacsStrings(Environment, Values, Arguments, ArrayCount(Variables), false);
        Funcall(Environment, "gdb--update-thread", ArrayCount(Arguments), Arguments);
    }
}

internal void
FrameInfo(emacs_env *Environment, struct gdbwire_mi_result *Result, emacs_value ThreadId)
{
    Funcall(Environment, "gdb--clear-thread-frames", 1, &ThreadId);

#define FrameVariablesWriter(W) W(Level, level),    \
        W(Address, addr),                           \
        W(Function, func),                          \
        W(File, fullname),                          \
        W(Line, line),                              \
        W(From, from)
    enum { FrameVariablesWriter(WriteVariableKey) };
    char *Variables[] = { FrameVariablesWriter(WriteVariableName) };

    for(struct gdbwire_mi_result *Frame = Result;
        Frame;
        Frame = Frame->next)
    {
        struct gdbwire_mi_result *FrameContents = Frame->variant.result;
        char *Values[ArrayCount(Variables)] = {};
        GetBatchResultString(FrameContents, Variables, Values, ArrayCount(Variables));

        emacs_value Arguments[1 + ArrayCount(Variables)];
        Arguments[0] = ThreadId;
        GetEmacsStrings(Environment, Values, Arguments + 1, ArrayCount(Variables), false);
        Funcall(Environment, "gdb--add-frame-to-thread", ArrayCount(Arguments), Arguments);
    }

    Funcall(Environment, "gdb--finalize-thread-frames", 1, &ThreadId);
}

internal void
Disassemble(emacs_env *Environment, struct gdbwire_mi_result *List, emacs_value Buffer)
{
#define SourceInfoVariablesWriter(W) W(Line, line), W(File, fullname)
    enum { SourceInfoVariablesWriter(WriteVariableKey) };

#define AssemblyVariablesWriter(W) W(Address, address),         \
        W(Function, func-name),                                 \
        W(Offset, offset),                                      \
        W(Instruction, inst)
    enum { AssemblyVariablesWriter(WriteVariableKey) };

    bool HasSourceInfo = !strcmp(List->variable, "src_and_asm_line");
    for(struct gdbwire_mi_result *Iterator;
        Iterator;
        Iterator = Iterator->next)
    {
        // IMPORTANT TODO(nox): Finish this
        if(HasSourceInfo)
        {
        }
        else
        {
        }
    }
}

internal void
HandleMiOobRecord(emacs_env *Environment, struct gdbwire_mi_oob_record *Record,
                  string_builder *PrintString)
{
    switch(Record->kind)
    {
        case GDBWIRE_MI_ASYNC:
        {
            struct gdbwire_mi_async_record *AsyncRecord = Record->variant.async_record;
            struct gdbwire_mi_result *Result = AsyncRecord->result;
            switch(AsyncRecord->kind)
            {
                case GDBWIRE_MI_EXEC:
                {
                    switch(AsyncRecord->async_class)
                    {
                        case GDBWIRE_MI_ASYNC_STOPPED:
                        {
                            for(struct gdbwire_mi_result *Iterator = Result;
                                Iterator;
                                Iterator = Iterator->next)
                            {
                                if(Iterator->variable &&
                                   strcmp(Iterator->variable, "stopped-threads") == 0)
                                {
                                    char *ThreadInfoFunc = "gdb--get-thread-info";
                                    if(Iterator->kind == GDBWIRE_MI_CSTRING)
                                    {
                                        Funcall(Environment, ThreadInfoFunc, 0, 0);
                                    }
                                    else
                                    {
                                        for(struct gdbwire_mi_result *Thread = Iterator->variant.result;
                                            Thread;
                                            Thread = Thread->next)
                                        {
                                            emacs_value Argument =
                                                GetEmacsString(Environment, Thread->variant.cstring);
                                            Funcall(Environment, ThreadInfoFunc, 1, &Argument);
                                        }
                                    }

                                    break;
                                }
                            }

                            char *ThreadId = GetResultString(Result, "thread-id");
                            emacs_value Argument = GetEmacsString(Environment, ThreadId);
                            Funcall(Environment, "gdb--stopped", 1, &Argument);

                            // NOTE(nox): *stopped does not end with a prompt...
                            PushString(PrintString, "(gdb) ");
                        } break;

                        case GDBWIRE_MI_ASYNC_RUNNING:
                        {
                            emacs_value Argument = GetEmacsString(Environment,
                                                                  GetResultString(Result, "thread-id"));
                            Funcall(Environment, "gdb--running", 1, &Argument);
                        } break;

                        IgnoreDefaultCase;
                    }
                } break;

                case GDBWIRE_MI_NOTIFY:
                {
                    switch(AsyncRecord->async_class)
                    {
                        // TODO(nox): Check if we need more handlers
                        case GDBWIRE_MI_ASYNC_THREAD_CREATED:
                        {
                            char *ThreadId = GetResultString(Result, "id");
                            emacs_value Argument = GetEmacsString(Environment, ThreadId);
                            Funcall(Environment, "gdb--get-thread-info", 1, &Argument);
                        } break;

                        case GDBWIRE_MI_ASYNC_THREAD_EXITED:
                        {
                            char *ThreadId = GetResultString(Result, "id");
                            emacs_value Argument = GetEmacsString(Environment, ThreadId);
                            Funcall(Environment, "gdb--thread-exited", 1, &Argument);
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_CREATED:
                        case GDBWIRE_MI_ASYNC_BREAKPOINT_MODIFIED:
                        {
                            BreakpointChange(Environment, GetResultTuple(Result, "bkpt"));
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_DELETED:
                        {
                            BreakpointDeletion(Environment, GetResultString(Result, "id"));
                        } break;

                        IgnoreDefaultCase;
                    }
                } break;

                IgnoreCase(GDBWIRE_MI_STATUS); // NOTE(nox): May be ignored
            }
        } break;

        case GDBWIRE_MI_STREAM:
        {
            struct gdbwire_mi_stream_record *StreamRecord = Record->variant.stream_record;
            switch(StreamRecord->kind)
            {
                case GDBWIRE_MI_CONSOLE:
                {
                    PushString(PrintString, "%s", StreamRecord->cstring);
                } break;

                case GDBWIRE_MI_TARGET:
                {
                    // TODO(nox): If we are outputting to another tty, will we ever receive this?
                    PushString(PrintString, "Target: %s", StreamRecord->cstring);
                } break;

                IgnoreCase(GDBWIRE_MI_LOG);
            }
        } break;
    }
}

typedef struct
{
    enum
    {
        Context_NoContext,

        Context_Ignore,
        Context_InitialFile,
        Context_BreakpointInsert,
        Context_ThreadInfo,
        Context_FrameInfo,
        Context_Disassemble,

        Context_Size,
    } Type;

    emacs_value Data;
} token_context;

internal token_context
GetTokenContext(emacs_env *Environment, char *TokenString)
{
    token_context Result = {};
    if(TokenString)
    {
        emacs_value Argument = GetEmacsString(Environment, TokenString);
        emacs_value ContextCons = Funcall(Environment, "gdb--extract-context", 1, &Argument);

        Result.Type = Environment->extract_integer(Environment,
                                                   Funcall(Environment, "car", 1, &ContextCons));
        assert(Result.Type < Context_Size);

        Result.Data = Funcall(Environment, "cdr", 1, &ContextCons);
    }
    return Result;
}

internal void
HandleMiResultRecord(emacs_env *Environment, struct gdbwire_mi_result_record *Record,
                     string_builder *PrintString)
{
    token_context Context = GetTokenContext(Environment, Record->token);

    struct gdbwire_mi_result *Result = Record->result;
    switch(Record->result_class)
    {
        case GDBWIRE_MI_DONE:
        case GDBWIRE_MI_RUNNING:
        case GDBWIRE_MI_CONNECTED:
        {
            switch(Context.Type)
            {
                case Context_InitialFile:
                {
                    char *File = GetResultString(Result, "fullname");
                    char *Line = GetResultString(Result, "line");
                    if(File)
                    {
                        emacs_value Arguments[2];
                        Arguments[0] = GetEmacsString(Environment, File);
                        Arguments[1] = GetEmacsStringEmpty(Environment, Line);
                        Funcall(Environment, "gdb--set-initial-file", 2, Arguments);
                    }
                } break;

                case Context_BreakpointInsert:
                {
                    BreakpointChange(Environment, GetResultTuple(Result, "bkpt"));
                } break;

                case Context_ThreadInfo:
                {
                    ThreadInfo(Environment, Result);
                } break;

                case Context_FrameInfo:
                {
                    FrameInfo(Environment, GetResultList(Result, "stack"), Context.Data);
                } break;

                case Context_Disassemble:
                {
                    Disassemble(Environment, GetResultList(Result, "asm_insns"), Context.Data);
                } break;

                IgnoreDefaultCase;
            }

            Funcall(Environment, "gdb--done", 0, 0);
        } break;

        case GDBWIRE_MI_ERROR:
        {
            // TODO(nox): Docs say we should update every GUI information
            switch(Context.Type)
            {
                IgnoreCase(Context_Ignore);
                IgnoreCase(Context_InitialFile);

                default:
                {
                    char *Message = GetResultString(Result, "msg");
                    if(Message)
                    {
                        PushString(PrintString, "%s\n", Result->variant.cstring);
                    }
                } break;
            }

            Funcall(Environment, "gdb--error", 0, 0);
        } break;

        case GDBWIRE_MI_EXIT:
        {
            PushString(PrintString, "Bye bye :)\n");
        } break;

        case GDBWIRE_MI_UNSUPPORTED:
        {
            PushString(PrintString, "Error while parsing, GdbWire couldn't figure out class of result record!");
        } break;
    }

    if(Context.Type != Context_NoContext)
    {
        IgnoreNextPrompt = true;
    }
}

// NOTE(weirdNox): Because this is an internal helper function, we will assume that
// everything passed in is valid.
#define HANDLE_GDB_MI_OUTPUT_NAME "gdb--handle-mi-output"
#define HANDLE_GDB_MI_OUTPUT_DOC "(" HANDLE_GDB_MI_OUTPUT_NAME " output)" \
    "\nHandle GDB/MI output and return string to print."
internal emacs_value
HandleGdbMiOutput(emacs_env *Environment, ptrdiff_t NumberOfArguments,
                  emacs_value Arguments[], void *Closure)
{
    ptrdiff_t Size;
    Environment->copy_string_contents(Environment, Arguments[0], 0, &Size);
    char *OutputString = malloc(Size);
    Environment->copy_string_contents(Environment, Arguments[0], OutputString, &Size);

    gdbwire_mi_parser_push(GdbMiParser, OutputString);

    string_builder PrintString = AllocateString();
    for(struct gdbwire_mi_output *Output = ParserOutput;
        Output;
        Output = Output->next)
    {
        switch(Output->kind)
        {
            case GDBWIRE_MI_OUTPUT_OOB:
            {
                HandleMiOobRecord(Environment, Output->variant.oob_record, &PrintString);
            } break;

            case GDBWIRE_MI_OUTPUT_RESULT:
            {
                HandleMiResultRecord(Environment, Output->variant.result_record, &PrintString);
            } break;

            case GDBWIRE_MI_OUTPUT_PARSE_ERROR:
            {
                PushString(&PrintString, "An error occurred on token %s\n",
                           Output->variant.error.token);
            } break;

            case GDBWIRE_MI_OUTPUT_PROMPT:
            {
                if(!IgnoreNextPrompt)
                {
                    PushString(&PrintString, "(gdb) ");
                }
                IgnoreNextPrompt = false;
            } break;
        }
    }
    gdbwire_mi_output_free(ParserOutput);
    ParserOutput = 0;

    emacs_value Result = Environment->make_string(Environment, PrintString.Contents,
                                                  PrintString.Length);
    FreeString(&PrintString);

    return Result;
}

s32 emacs_module_init(struct emacs_runtime *EmacsRuntime)
{
    emacs_env *Environment = EmacsRuntime->get_environment(EmacsRuntime);

    emacs_value Arguments[2];
    Arguments[0] = Intern(Environment, HANDLE_GDB_MI_OUTPUT_NAME);
    Arguments[1] = Environment->make_function(Environment, 1, 1, HandleGdbMiOutput,
                                              HANDLE_GDB_MI_OUTPUT_DOC, 0);
    Funcall(Environment, "fset", 2, Arguments);

    struct gdbwire_mi_parser_callbacks Callbacks = {0, GdbWireCallback};
    GdbMiParser = gdbwire_mi_parser_create(Callbacks);

    Nil = Intern(Environment, "nil");

    emacs_value Feature = Intern(Environment, "emacs-gdb-module");
    Funcall(Environment, "provide", 1, &Feature);
    return 0;
}
