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
    char *BreakpointVariables[] = { BreakpointVariablesWriter(WriteVariableName) };

    char *Values[ArrayCount(BreakpointVariables)] = {};
    GetBatchResultString(Breakpoint, BreakpointVariables, Values, ArrayCount(BreakpointVariables));

    emacs_value Arguments[ArrayCount(BreakpointVariables)];
    for(int Index = 0;
        Index < ArrayCount(BreakpointVariables);
        ++Index)
    {
        Arguments[Index] = GetEmacsString(Environment, Values[Index]);
    }
    emacs_value BreakpointChangedFunc = Environment->intern(Environment, "gdb--breakpoint-changed");
    Environment->funcall(Environment, BreakpointChangedFunc, ArrayCount(Arguments), Arguments);
}

internal void
BreakpointDeletion(emacs_env *Environment, char *Id)
{
    emacs_value Argument = GetEmacsString(Environment, Id);
    emacs_value BreakpointDeletedFunc = Environment->intern(Environment, "gdb--breakpoint-deleted");
    Environment->funcall(Environment, BreakpointDeletedFunc, 1, &Argument);
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
    char *ThreadVariables[] = { ThreadVariablesWriter(WriteVariableName) };

#define FrameVariablesWriter(W) W(Address, addr),   \
        W(Function, func),                          \
        W(File, fullname),                          \
        W(Line, line),                              \
        W(From, from)
    enum { FrameVariablesWriter(WriteVariableKey) };
    char *FrameVariables[] = { FrameVariablesWriter(WriteVariableName) };

    for(struct gdbwire_mi_result *Thread = Threads;
        Thread && Thread->kind == GDBWIRE_MI_TUPLE;
        Thread = Thread->next)
    {
        struct gdbwire_mi_result *ThreadContents = Thread->variant.result;
        char *ThreadValues[ArrayCount(ThreadVariables)] = {};
        char *FrameValues[ArrayCount(FrameVariables)] = {};
        emacs_value Arguments[ArrayCount(ThreadValues) + ArrayCount(FrameValues)];

        GetBatchResultString(ThreadContents, ThreadVariables, ThreadValues,
                             ArrayCount(ThreadVariables));

        if(strcmp("stopped", ThreadValues[Variable_State]) == 0)
        {
            struct gdbwire_mi_result *Frame = GetResultTuple(ThreadContents, "frame");
            GetBatchResultString(Frame, FrameVariables, FrameValues, ArrayCount(FrameVariables));
            // TODO(nox): Fetch arguments
        }

        for(int ThreadValueIndex = 0;
            ThreadValueIndex < ArrayCount(ThreadVariables);
            ++ThreadValueIndex)
        {
            Arguments[ThreadValueIndex] = GetEmacsString(Environment,
                                                         ThreadValues[ThreadValueIndex]);
        }
        for(int FrameValueIndex = 0;
            FrameValueIndex < ArrayCount(FrameVariables);
            ++FrameValueIndex)
        {
            Arguments[ArrayCount(ThreadVariables) + FrameValueIndex] =
                GetEmacsString(Environment, FrameValues[FrameValueIndex]);
        }

        emacs_value UpdateThreadFunction = Environment->intern(Environment, "gdb--thread-updated");
        Environment->funcall(Environment, UpdateThreadFunction, ArrayCount(Arguments), Arguments);
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
                            // TODO(nox): This will eventually receive more parameters, to
                            // update the current frame and thread and etc
                            Environment->funcall(Environment,
                                                 Environment->intern(Environment, "gdb--stopped"),
                                                 0, 0);

                            // NOTE(nox): *stopped does not end with a prompt...
                            PushString(PrintString, "(gdb) ");
                        } break;

                        case GDBWIRE_MI_ASYNC_RUNNING:
                        {
                            Environment->funcall(Environment,
                                                 Environment->intern(Environment, "gdb--running"),
                                                 0, 0);
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
                            // NOTE(nox): This notification doesn't provide enough information
                            emacs_value GdbCommandFunc = Environment->intern(Environment,
                                                                             "gdb--command");
                            emacs_value Arguments[2];
                            Arguments[0] = GetEmacsString(Environment, "-thread-info");
                            Arguments[1] = Environment->intern(Environment,
                                                               "gdb--context-thread-info");
                            Environment->funcall(Environment, GdbCommandFunc, 2, Arguments);
                        } break;

                        case GDBWIRE_MI_ASYNC_THREAD_EXITED:
                        {
                            // TODO(nox): Remove and update UI
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_CREATED:
                        case GDBWIRE_MI_ASYNC_BREAKPOINT_MODIFIED:
                        {
                            BreakpointChange(Environment, Result->variant.result);
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_DELETED:
                        {
                            BreakpointDeletion(Environment, Result->variant.cstring);
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

typedef enum
{
    Context_NoContext,

    Context_Ignore,
    Context_InitialFile,
    Context_BreakpointInsert,
    Context_ThreadInfo,

    Context_Size,
} token_context;

internal token_context
GetTokenContext(emacs_env *Environment, char *TokenString)
{
    token_context Result = Context_NoContext;
    if(TokenString)
    {
        emacs_value Argument = GetEmacsString(Environment, TokenString);
        emacs_value ContextExtractor = Environment->intern(Environment, "gdb--extract-context");
        emacs_value Context = Environment->funcall(Environment, ContextExtractor,
                                                   1, &Argument);
        Result = Environment->extract_integer(Environment, Context);
        assert(Result < Context_Size);
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
            switch(Context)
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
                        emacs_value InitialFileFunc = Environment->intern(Environment,
                                                                          "gdb--set-initial-file");
                        Environment->funcall(Environment, InitialFileFunc, 2, Arguments);
                    }
                } break;

                case Context_BreakpointInsert:
                {
                    BreakpointChange(Environment, Result->variant.result);
                } break;

                case Context_ThreadInfo:
                {
                    ThreadInfo(Environment, Result);
                } break;

                IgnoreDefaultCase;
            }

            Environment->funcall(Environment, Environment->intern(Environment, "gdb--done"), 0, 0);
        } break;

        case GDBWIRE_MI_ERROR:
        {
            // TODO(nox): Docs say we should update every GUI information
            switch(Context)
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

            Environment->funcall(Environment,
                                 Environment->intern(Environment, "gdb--error"),
                                 0, 0);
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

    if(Context != Context_NoContext)
    {
        IgnoreNextPrompt = true;
    }
}

// NOTE(weirdNox): Because this is an internal helper function, we will assume that
// everything passed in is valid.
#define PARSE_GDB_MI_OUTPUT_NAME "gdb--handle-mi-output"
#define PARSE_GDB_MI_OUTPUT_DOC "(" PARSE_GDB_MI_OUTPUT_NAME " output)" \
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

    emacs_value FSet = Environment->intern(Environment, "fset");
    emacs_value Arguments[2];
    Arguments[0] = Environment->intern(Environment, PARSE_GDB_MI_OUTPUT_NAME);
    Arguments[1] = Environment->make_function(Environment, 1, 1, HandleGdbMiOutput,
                                              PARSE_GDB_MI_OUTPUT_DOC, 0);
    Environment->funcall(Environment, FSet, 2, Arguments);

    struct gdbwire_mi_parser_callbacks Callbacks = {0, GdbWireCallback};
    GdbMiParser = gdbwire_mi_parser_create(Callbacks);

    Nil = Environment->intern(Environment, "nil");

    emacs_value Provide = Environment->intern(Environment, "provide");
    emacs_value Feature = Environment->intern(Environment, "emacs-gdb-module");
    Environment->funcall(Environment, Provide, 1, &Feature);
    return 0;
}
