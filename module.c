#include <assert.h>
#include "emacs-module.h"
#include "gdbwire.c"

#define internal static

#define InvalidCodePath assert(0)
#define InvalidDefaultCase default: { InvalidCodePath(); } break
#define IgnoreCase(x) case (x): break
#define IgnoreDefaultCase default: break

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef size_t memory_size;

int plugin_is_GPL_compatible;

internal struct gdbwire_mi_parser *GdbMiParser;
internal struct gdbwire_mi_output *ParserOutput;
internal bool IgnorePrompt;

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
        int Length = vsnprintf(0, 0, Format, VariadicArguments);
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

internal char *
GdbWireGetResultString(struct gdbwire_mi_result *Result, char *Variable)
{
    char *ToReturn = 0;
    if(Variable)
    {
        for(;
            Result;
            Result = Result->next)
        {
            if(Result->variable && Result->kind == GDBWIRE_MI_CSTRING &&
               strcmp(Variable, Result->variable) == 0)
            {
                ToReturn = Result->variant.cstring;
                break;
            }
        }
    }

    return ToReturn;
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
            switch(AsyncRecord->kind)
            {
                case GDBWIRE_MI_EXEC:
                {
                    switch(AsyncRecord->async_class)
                    {
                        case GDBWIRE_MI_ASYNC_STOPPED:
                        {
                            // NOTE(nox): *stopped does not end with a prompt...
                            PushString(PrintString, "(gdb) ");
                        } break;

                        case GDBWIRE_MI_ASYNC_RUNNING:
                        {
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
                        } break;

                        case GDBWIRE_MI_ASYNC_THREAD_SELECTED:
                        {
                        } break;

                        case GDBWIRE_MI_ASYNC_THREAD_EXITED:
                        {
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_CREATED:
                        {
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_MODIFIED:
                        {
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_DELETED:
                        {
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

    Context_Size,
} token_context;

internal token_context
GetTokenContext(emacs_env *Environment, char *TokenString)
{
    token_context Result = Context_NoContext;
    if(TokenString)
    {
        emacs_value String = Environment->make_string(Environment, TokenString, strlen(TokenString));
        emacs_value ContextExtractor = Environment->intern(Environment, "gdb--extract-token-context");
        emacs_value Context = Environment->funcall(Environment, ContextExtractor,
                                                   1, (emacs_value[]){String});
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
                    char *File = GdbWireGetResultString(Result, "fullname");
                    char *Line = GdbWireGetResultString(Result, "line");
                    if(File)
                    {
                        int NumArguments = 1;
                        emacs_value Arguments[2];
                        Arguments[0] = Environment->make_string(Environment, File, strlen(File));
                        if(Line)
                        {
                            ++NumArguments;
                            Arguments[1] = Environment->make_string(Environment, Line, strlen(Line));
                        }
                        emacs_value InitialFileFunc = Environment->intern(Environment,
                                                                          "gdb--set-initial-file");
                        Environment->funcall(Environment, InitialFileFunc, NumArguments, Arguments);
                    }
                } break;

                IgnoreDefaultCase;
            }
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
                    char *Message = GdbWireGetResultString(Result, "msg");
                    if(Message)
                    {
                        PushString(PrintString, "%s\n", Result->variant.cstring);
                    }
                } break;
            }
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
        IgnorePrompt = true;
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
                if(!IgnorePrompt)
                {
                    PushString(&PrintString, "(gdb) ");
                }
                IgnorePrompt = false;
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

int emacs_module_init(struct emacs_runtime *EmacsRuntime)
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

    emacs_value Provide = Environment->intern(Environment, "provide");
    emacs_value Feature = Environment->intern(Environment, "emacs-gdb-module");
    Environment->funcall(Environment, Provide, 1, &Feature);
    return 0;
}
