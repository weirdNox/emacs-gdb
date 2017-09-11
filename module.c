#include <assert.h>
#include "emacs-module.h"
#include "gdbwire.c"

#define internal static

#define InvalidCodePath assert(0)
#define InvalidDefaultCase default: { InvalidCodePath(); } break
#define IgnoreCase(x) case (x): break
#define IgnoredDefaultCase default: break

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

internal void
HandleMiOobRecord(emacs_env *Environment, struct gdbwire_mi_oob_record *Record,
                  string_builder *PrintString, u8 Debug)
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
                        } break;

                        case GDBWIRE_MI_ASYNC_RUNNING:
                        {
                        } break;

                        IgnoredDefaultCase;
                    }
                } break;

                case GDBWIRE_MI_NOTIFY:
                {
                    switch(AsyncRecord->async_class)
                    {
                        // TODO(nox): Add handlers to some of these notifications
                        IgnoredDefaultCase;
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

                case GDBWIRE_MI_LOG:
                {
                    if(Debug)
                    {
                        PushString(PrintString, "**GDB LOG** %s", StreamRecord->cstring);
                    }
                } break;

                case GDBWIRE_MI_TARGET:
                {
                    // TODO(nox): If we are outputting to another tty, will we ever receive this?
                    PushString(PrintString, "Target: %s", StreamRecord->cstring);
                } break;
            }
        } break;
    }
}

internal void
HandleMiResultRecord(emacs_env *Environment, struct gdbwire_mi_result_record *Record,
                     string_builder *PrintString)
{
    // TODO(nox): Handle tokens... I don't know what's their purpose
    switch(Record->result_class)
    {
        case GDBWIRE_MI_DONE:
        case GDBWIRE_MI_RUNNING:
        case GDBWIRE_MI_CONNECTED:
        {
            // TODO(nox): Complete this!!
        } break;

        case GDBWIRE_MI_ERROR:
        {
            struct gdbwire_mi_result *Result = Record->result;
            while(Result)
            {
                if(Result && strcmp(Result->variable, "msg") == 0 &&
                   Result->kind == GDBWIRE_MI_CSTRING)
                {
                    PushString(PrintString, "%s\n", Result->variant.cstring);
                    break;
                }
                Result = Result->next;
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
}

// NOTE(weirdNox): Because this is an internal helper function, we will assume that
// everything passed in is valid.
#define PARSE_GDB_MI_OUTPUT_NAME "gdb--parse-mi-output"
#define PARSE_GDB_MI_OUTPUT_DOC "(" PARSE_GDB_MI_OUTPUT_NAME " string)" \
    "\nParse GDB/MI output."
internal emacs_value
ParseGdbMiOutput(emacs_env *Environment, ptrdiff_t NumberOfArguments,
                 emacs_value Arguments[], void *Closure)
{
    emacs_value SymbolValue = Environment->intern(Environment, "symbol-value");
    u8 Debug = Environment->is_not_nil(Environment,
                                       Environment->funcall(Environment, SymbolValue, 1,
                                                            (emacs_value[]){Environment->intern(Environment, "gdb-debug-output")}));

    ptrdiff_t Size;
    Environment->copy_string_contents(Environment, Arguments[0], 0, &Size);
    char *String = malloc(Size);
    Environment->copy_string_contents(Environment, Arguments[0], String, &Size);

    string_builder PrintString = AllocateString();

    gdbwire_mi_parser_push(GdbMiParser, String);
    for(struct gdbwire_mi_output *Output = ParserOutput;
        Output;
        Output = Output->next)
    {
        switch(Output->kind)
        {
            case GDBWIRE_MI_OUTPUT_OOB:
            {
                HandleMiOobRecord(Environment, Output->variant.oob_record, &PrintString, Debug);
            } break;

            case GDBWIRE_MI_OUTPUT_RESULT:
            {
                HandleMiResultRecord(Environment, Output->variant.result_record, &PrintString);
            } break;

            case GDBWIRE_MI_OUTPUT_PROMPT:
            {
                PushString(&PrintString, "(gdb) ");
            } break;

            case GDBWIRE_MI_OUTPUT_PARSE_ERROR:
            {
                PushString(&PrintString, "An error occurred on token %s\n",
                           Output->variant.error.token);
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
    Arguments[1] = Environment->make_function(Environment, 1, 1, ParseGdbMiOutput,
                                              PARSE_GDB_MI_OUTPUT_DOC, 0);
    Environment->funcall(Environment, FSet, 2, Arguments);

    struct gdbwire_mi_parser_callbacks Callbacks = {0, GdbWireCallback};
    GdbMiParser = gdbwire_mi_parser_create(Callbacks);

    emacs_value Provide = Environment->intern(Environment, "provide");
    emacs_value Feature = Environment->intern(Environment, "emacs-gdb-module");
    Environment->funcall(Environment, Provide, 1, &Feature);
    return 0;
}
