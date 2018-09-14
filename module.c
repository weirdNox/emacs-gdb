#include <assert.h>
#include "emacs-module.h"
#include "gdbwire.c"

#define ArrayCount(Array) (sizeof(Array) / sizeof(*(Array)))

#define InvalidCodePath assert(0)
#define InvalidDefaultCase default: { InvalidCodePath(); } break
#define IgnoreCase(Case) case (Case): break
#define IgnoreDefaultCase default: break

#define WriteVariableKey(Key, Name, ...) Variable_##Key
#define WriteVariableName(Key, Name, ...) #Name
#define WriteElispStructKey(Key, Name, Elisp) Intern(Env, ":" #Elisp)

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t  s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef struct emacs_runtime emacs_runtime;

typedef struct gdbwire_mi_result mi_result;
typedef struct gdbwire_mi_result_record mi_result_record;
typedef struct gdbwire_mi_output mi_output;
typedef struct gdbwire_mi_oob_record mi_oob_record;
typedef struct gdbwire_mi_async_record mi_async_record;
typedef struct gdbwire_mi_stream_record mi_stream_record;
typedef struct gdbwire_mi_parser_callbacks gdbwire_callbacks;


u32 plugin_is_GPL_compatible;

static emacs_value Nil;
static emacs_value T;
static struct gdbwire_mi_parser *GdbMiParser;
static mi_output *ParserOutput;
static bool IgnoreNextPrompt;

static void *xMalloc(size_t N) {
    void *Result = malloc(N);
    if(!Result) {
        perror("xMalloc - not enough memory available\n");
        exit(1);
    }
    return Result;
}

static void *xRealloc(void *Ptr, size_t N) {
    void *Result = realloc(Ptr, N);
    if(!Result) {
        perror("xRealloc - not enough memory available\n");
        exit(1);
    }
    return Result;
}

typedef struct string {
    char *Contents;
    u32 Len;
    size_t Cap;
} string;

static string AllocateString() {
    string Result = {
        .Cap = 1<<13
    };
    Result.Contents = xMalloc(Result.Cap);
    return Result;
}

static void PushString(string *Str, char *Format, ...) {
    if(Str && Str->Contents && Format) {
        va_list Args;
        va_start(Args, Format);
        size_t Avail = Str->Cap - Str->Len;
        size_t NewLen  = 1 + vsnprintf(Str->Contents + Str->Len, Avail, Format, Args);
        va_end(Args);

        if(NewLen > Avail) {
            Str->Cap += NewLen + (1<<10);
            Str->Contents = xRealloc(Str->Contents, Str->Cap);

            va_start(Args, Format);
            size_t NewAvail = Str->Cap - Str->Len;
            NewLen  = 1 + vsnprintf(Str->Contents + Str->Len, NewAvail, Format, Args);
            va_end(Args);
        }

        Str->Len += NewLen - 1;
    }
}

static void FreeString(string *Str) {
    if(Str && Str->Contents) {
        free(Str->Contents);
        *Str = (string){};
    }
}

static void GdbWireCallback(void *Ctx, mi_output *Output) {
    mi_output **WriteTo = &ParserOutput;
    while(*WriteTo) {
        WriteTo = &(*WriteTo)->next;
    }
    *WriteTo = Output;
}

static void *GetResultVariable_(mi_result *Result, char *Variable,
                                enum gdbwire_mi_result_kind Kind) {
    void *ToReturn = 0;
    if(Result && Variable) {
        for(; Result; Result = Result->next) {
            if(Result->variable && Result->kind == Kind && strcmp(Variable, Result->variable) == 0) {
                ToReturn = Result->variant.result;
                break;
            }
        }
    }
    return ToReturn;
}

#define GetResultTuple(Result, Variable)                                \
    (mi_result *)GetResultVariable_(Result, Variable, GDBWIRE_MI_TUPLE)
#define GetResultList(Result, Variable)                                 \
    (mi_result *)GetResultVariable_(Result, Variable, GDBWIRE_MI_LIST)
#define GetResultString(Result, Variable)                               \
    (char *)GetResultVariable_(Result, Variable, GDBWIRE_MI_CSTRING)

static void GetBatchResultString(mi_result *Result, char *Variables[], char *Values[],
                                 u32 NumberOfVariables) {
    if(Result && Variables && Values) {
        u32 StartAt = 0;
        for(mi_result *Iterator = Result; Iterator; Iterator = Iterator->next) {
            if(Iterator->kind == GDBWIRE_MI_CSTRING && Iterator->variable) {
                for(int VariableIndex = StartAt; VariableIndex < NumberOfVariables; ++VariableIndex) {
                    char *Variable = Variables[VariableIndex];
                    if(!Values[VariableIndex] && strcmp(Variable, Iterator->variable) == 0) {
                        if(VariableIndex == StartAt) {
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

static inline emacs_value GetEmacsString_(emacs_env *Env, char *String, bool EmptyIfNull) {
    emacs_value Result = Nil;

    if(!String && EmptyIfNull) {
        String = "";
    }

    if(String) {
        Result = Env->make_string(Env, String, strlen(String));
    }

    return Result;
}
#define GetEmacsString(Env, String) GetEmacsString_(Env, String, false)
#define GetEmacsStringEmpty(Env, String) GetEmacsString_(Env, String, true)

static inline void GetEmacsStrings(emacs_env *Env, char *Vals[], emacs_value EmacsVals[], u32 Size,
                                   bool EmptyIfNull) {
    if(Vals && EmacsVals) {
        for(int Index = 0; Index < Size; ++Index) {
            EmacsVals[Index] = GetEmacsString_(Env, Vals[Index], EmptyIfNull);
        }
    }
}

static inline emacs_value Intern(emacs_env *Env, char *SymName) {
    emacs_value Result = Env->intern(Env, SymName);
    return Result;
}

static inline emacs_value Funcall(emacs_env *Env, char *Func, u32 NumArgs, emacs_value *Args) {
    emacs_value Result = Env->funcall(Env, Intern(Env, Func), NumArgs, Args);
    return Result;
}

static inline emacs_value FuncallInterned(emacs_env *Env, emacs_value Func, u32 NumArgs, emacs_value *Args) {
    emacs_value Result = Env->funcall(Env, Func, NumArgs, Args);
    return Result;
}

static void BreakpointChange(emacs_env *Env, mi_result *Breakpoint) {
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
    GetEmacsStrings(Env, Values, Arguments, ArrayCount(Variables), false);
    Funcall(Env, "gdb--breakpoint-changed", ArrayCount(Arguments), Arguments);
}

static void BreakpointDeletion(emacs_env *Env, char *Id) {
    emacs_value Argument = GetEmacsString(Env, Id);
    Funcall(Env, "gdb--breakpoint-deleted", 1, &Argument);
}

static void ThreadInfo(emacs_env *Env, mi_result *Result) {
    mi_result *Threads = GetResultList(Result, "threads");

#define ThreadVariablesWriter(W) W(Id, id),     \
        W(TargetId, target-id),                 \
        W(Name, name),                          \
        W(State, state),                        \
        W(Core, core)

    enum { ThreadVariablesWriter(WriteVariableKey) };
    char *Variables[] = { ThreadVariablesWriter(WriteVariableName) };

    for(mi_result *Thread = Threads; Thread && Thread->kind == GDBWIRE_MI_TUPLE; Thread = Thread->next) {
        mi_result *ThreadContents = Thread->variant.result;
        char *Values[ArrayCount(Variables)] = {};
        GetBatchResultString(ThreadContents, Variables, Values, ArrayCount(Variables));

        emacs_value Arguments[ArrayCount(Values)];
        GetEmacsStrings(Env, Values, Arguments, ArrayCount(Variables), false);
        Funcall(Env, "gdb--update-thread", ArrayCount(Arguments), Arguments);
    }
}

static void FrameInfo(emacs_env *Env, mi_result *Result, emacs_value ThreadId) {
    Funcall(Env, "gdb--clear-thread-frames", 1, &ThreadId);

#define FrameVariablesWriter(W) W(Level, level),    \
        W(Address, addr),                           \
        W(Function, func),                          \
        W(File, fullname),                          \
        W(Line, line),                              \
        W(From, from)

    enum { FrameVariablesWriter(WriteVariableKey) };
    char *Variables[] = { FrameVariablesWriter(WriteVariableName) };

    for(mi_result *Frame = Result; Frame; Frame = Frame->next) {
        mi_result *FrameContents = Frame->variant.result;
        char *Values[ArrayCount(Variables)] = {};
        GetBatchResultString(FrameContents, Variables, Values, ArrayCount(Variables));

        emacs_value Arguments[1 + ArrayCount(Variables)];
        Arguments[0] = ThreadId;
        GetEmacsStrings(Env, Values, Arguments + 1, ArrayCount(Variables), false);
        Funcall(Env, "gdb--add-frame-to-thread", ArrayCount(Arguments), Arguments);
    }

    Funcall(Env, "gdb--finalize-thread-frames", 1, &ThreadId);
}

static void Disassemble(emacs_env *Env, mi_result *List, emacs_value Buffer) {

#define SourceInfoVariablesWriter(W) W(Line, line, line), W(File, fullname, file)

    enum { SourceInfoVariablesWriter(WriteVariableKey) };
    char *SourceVariables[] = { SourceInfoVariablesWriter(WriteVariableName) };
    emacs_value SourceInfoElispKeys[] = { SourceInfoVariablesWriter(WriteElispStructKey) };

#define InstructionVariablesWriter(W) W(Address, address, address), \
        W(Function, func-name, function),                           \
        W(Offset, offset, offset),                                  \
        W(Instruction, inst, instruction)

    enum { InstructionVariablesWriter(WriteVariableKey) };
    char *InstructionVariables[] = { InstructionVariablesWriter(WriteVariableName) };
    emacs_value InstructionElispKeys[] = { InstructionVariablesWriter(WriteElispStructKey) };

    bool HasSourceInfo = !strcmp(List->variable, "src_and_asm_line");

    u32 SourceInfoInstrListIndex = 3;
    emacs_value DisassemblyList = Nil;
    emacs_value Cons = Intern(Env, "cons");
    emacs_value MakeInstruction = Intern(Env, "make-gdb--instruction");
    emacs_value MakeSourceInfo = Intern(Env, "make-gdb--source-instr-info");

    for(mi_result *Iterator = List; Iterator; Iterator = Iterator->next) {
        mi_result *Contents = Iterator->variant.result;
        if(HasSourceInfo) {
            char *SourceValues[ArrayCount(SourceVariables)] = {};
            emacs_value SourceInfoObj;
            GetBatchResultString(Contents, SourceVariables, SourceValues, ArrayCount(SourceVariables));

            {
                emacs_value Arguments[ArrayCount(SourceVariables)*2];
                for(int Index = 0;
                    Index < ArrayCount(SourceVariables);
                    ++Index)
                {
                    Arguments[Index*2] = SourceInfoElispKeys[Index];
                    Arguments[Index*2 + 1] = GetEmacsString(Env, SourceValues[Index]);
                }
                SourceInfoObj = FuncallInterned(Env, MakeSourceInfo,
                                                ArrayCount(Arguments), Arguments);
            }

            mi_result *InstrList = GetResultList(Contents, "line_asm_insn");
            for(mi_result *InstrIterator = InstrList; InstrIterator; InstrIterator = InstrIterator->next) {
                mi_result *Instruction = InstrIterator->variant.result;
                emacs_value InstructionObj;

                char *InstructionValues[ArrayCount(InstructionVariables)] = {};
                GetBatchResultString(Instruction, InstructionVariables,
                                     InstructionValues, ArrayCount(InstructionVariables));

                emacs_value Arguments[ArrayCount(InstructionVariables)*2];
                for(int Index = 0; Index < ArrayCount(InstructionVariables); ++Index) {
                    Arguments[Index*2] = InstructionElispKeys[Index];
                    Arguments[Index*2 + 1] = GetEmacsString(Env, InstructionValues[Index]);
                }
                InstructionObj = FuncallInterned(Env, MakeInstruction, ArrayCount(Arguments), Arguments);

                Env->vec_set(Env, SourceInfoObj, SourceInfoInstrListIndex,
                             FuncallInterned(Env, Cons, 2, (emacs_value[]){InstructionObj, Env->vec_get(Env, SourceInfoObj, SourceInfoInstrListIndex)}));
            }

            {
                emacs_value Arguments[2] = {SourceInfoObj, DisassemblyList};
                DisassemblyList = FuncallInterned(Env, Cons, 2, Arguments);
            }
        } else {
            // TODO(nox): Finish this section
        }
    }

    {
        emacs_value Arguments[3] = {Buffer, DisassemblyList, HasSourceInfo ? T : Nil};
        Funcall(Env, "gdb--set-disassembly", 3, Arguments);
    }
}

static void miAsyncStopped(emacs_env *Env, mi_result *Result, string *PrintString) {
    for(mi_result *Iter = Result; Iter; Iter = Iter->next) {
        if(Iter->variable && strcmp(Iter->variable, "stopped-threads") == 0) {
            char *ThreadInfoFunc = "gdb--get-thread-info";
            if(Iter->kind == GDBWIRE_MI_CSTRING) {
                Funcall(Env, ThreadInfoFunc, 0, 0);
            } else {
                for(mi_result *Thread = Iter->variant.result; Thread; Thread = Thread->next)
                {
                    emacs_value Argument =
                        GetEmacsString(Env, Thread->variant.cstring);
                    Funcall(Env, ThreadInfoFunc, 1, &Argument);
                }
            }

            break;
        }
    }

    char *ThreadId = GetResultString(Result, "thread-id");
    emacs_value Argument = GetEmacsString(Env, ThreadId);
    Funcall(Env, "gdb--stopped", 1, &Argument);

    // NOTE(nox): *stopped does not end with a prompt...
    PushString(PrintString, "(gdb) ");
}

static void HandleMiOobRecord(emacs_env *Env, mi_oob_record *Record, string *PrintString) {
    switch(Record->kind) {
        case GDBWIRE_MI_ASYNC: {
            mi_async_record *AsyncRecord = Record->variant.async_record;
            mi_result *Result = AsyncRecord->result;
            switch(AsyncRecord->kind) {
                case GDBWIRE_MI_EXEC: {
                    switch(AsyncRecord->async_class) {
                        case GDBWIRE_MI_ASYNC_STOPPED: {
                            miAsyncStopped(Env, Result, PrintString);
                        } break;

                        case GDBWIRE_MI_ASYNC_RUNNING: {
                            emacs_value Argument = GetEmacsString(Env, GetResultString(Result, "thread-id"));
                            Funcall(Env, "gdb--running", 1, &Argument);
                        } break;

                        IgnoreDefaultCase;
                    }
                } break;

                case GDBWIRE_MI_NOTIFY: {
                    switch(AsyncRecord->async_class) {
                        case GDBWIRE_MI_ASYNC_THREAD_CREATED: {
                            char *ThreadId = GetResultString(Result, "id");
                            emacs_value Argument = GetEmacsString(Env, ThreadId);
                            Funcall(Env, "gdb--get-thread-info", 1, &Argument);
                        } break;

                        case GDBWIRE_MI_ASYNC_THREAD_EXITED: {
                            char *ThreadId = GetResultString(Result, "id");
                            emacs_value Argument = GetEmacsString(Env, ThreadId);
                            Funcall(Env, "gdb--thread-exited", 1, &Argument);
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_CREATED:
                        case GDBWIRE_MI_ASYNC_BREAKPOINT_MODIFIED: {
                            BreakpointChange(Env, GetResultTuple(Result, "bkpt"));
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_DELETED: {
                            BreakpointDeletion(Env, GetResultString(Result, "id"));
                        } break;

                        IgnoreDefaultCase;
                    }
                } break;

                IgnoreCase(GDBWIRE_MI_STATUS); // NOTE(nox): May be ignored
            }
        } break;

        case GDBWIRE_MI_STREAM: {
            mi_stream_record *StreamRecord = Record->variant.stream_record;
            switch(StreamRecord->kind) {
                case GDBWIRE_MI_CONSOLE: {
                    PushString(PrintString, "%s", StreamRecord->cstring);
                } break;

                case GDBWIRE_MI_TARGET: {
                    // TODO(nox): If we are outputting to another tty, will we ever receive this?
                    PushString(PrintString, "Target: %s", StreamRecord->cstring);
                } break;

                IgnoreCase(GDBWIRE_MI_LOG);
            }
        } break;
    }
}

typedef struct token_context {
    enum {
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

static token_context GetTokenContext(emacs_env *Env, char *TokenString) {
    token_context Result = {};
    if(TokenString) {
        emacs_value Argument = GetEmacsString(Env, TokenString);
        emacs_value ContextCons = Funcall(Env, "gdb--extract-context", 1, &Argument);

        Result.Type = Env->extract_integer(Env,
                                           Funcall(Env, "car", 1, &ContextCons));
        assert(Result.Type < Context_Size);

        Result.Data = Funcall(Env, "cdr", 1, &ContextCons);
    }
    return Result;
}

static void HandleMiResultRecord(emacs_env *Env, mi_result_record *Record, string *PrintString) {
    token_context Context = GetTokenContext(Env, Record->token);

    mi_result *Result = Record->result;
    switch(Record->result_class) {
        case GDBWIRE_MI_DONE:
        case GDBWIRE_MI_RUNNING:
        case GDBWIRE_MI_CONNECTED: {
            switch(Context.Type) {
                case Context_InitialFile: {
                    char *File = GetResultString(Result, "fullname");
                    char *Line = GetResultString(Result, "line");
                    if(File) {
                        emacs_value Arguments[2];
                        Arguments[0] = GetEmacsString(Env, File);
                        Arguments[1] = GetEmacsStringEmpty(Env, Line);
                        Funcall(Env, "gdb--set-initial-file", 2, Arguments);
                    }
                } break;

                case Context_BreakpointInsert: {
                    BreakpointChange(Env, GetResultTuple(Result, "bkpt"));
                } break;

                case Context_ThreadInfo: {
                    ThreadInfo(Env, Result);
                } break;

                case Context_FrameInfo: {
                    FrameInfo(Env, GetResultList(Result, "stack"), Context.Data);
                } break;

                case Context_Disassemble: {
                    Disassemble(Env, GetResultList(Result, "asm_insns"), Context.Data);
                } break;

                IgnoreDefaultCase;
            }
        } break;

        case GDBWIRE_MI_ERROR: {
            switch(Context.Type) {
                IgnoreCase(Context_Ignore);
                IgnoreCase(Context_InitialFile);

                default: {
                    char *Message = GetResultString(Result, "msg");
                    if(Message) {
                        PushString(PrintString, "%s\n", Result->variant.cstring);
                    }
                } break;
            }
        } break;

        case GDBWIRE_MI_EXIT: {
            PushString(PrintString, "Bye bye :)\n");
        } break;

        case GDBWIRE_MI_UNSUPPORTED: {
            PushString(PrintString, "Error while parsing, GdbWire couldn't figure out class of result record!");
        } break;
    }

    if(Context.Type != Context_NoContext) {
        IgnoreNextPrompt = true;
    }
}

// NOTE(weirdNox): Because this is an internal helper function, we will assume that
// everything passed in is valid.
#define HANDLE_GDB_MI_OUTPUT_NAME "gdb--handle-mi-output"
#define HANDLE_GDB_MI_OUTPUT_DOC "(" HANDLE_GDB_MI_OUTPUT_NAME " output)" \
    "\nHandle GDB/MI output and return string to print."
static emacs_value HandleGdbMiOutput(emacs_env *Env, ptrdiff_t NumberOfArguments,
                                     emacs_value Arguments[], void *Closure) {
    ptrdiff_t Size;
    Env->copy_string_contents(Env, Arguments[0], 0, &Size);
    char *OutputString = xMalloc(Size);
    Env->copy_string_contents(Env, Arguments[0], OutputString, &Size);

    gdbwire_mi_parser_push(GdbMiParser, OutputString);
    free(OutputString);

    string PrintString = AllocateString();
    for(mi_output *Output = ParserOutput; Output; Output = Output->next) {
        switch(Output->kind) {
            case GDBWIRE_MI_OUTPUT_OOB: {
                HandleMiOobRecord(Env, Output->variant.oob_record, &PrintString);
            } break;

            case GDBWIRE_MI_OUTPUT_RESULT: {
                HandleMiResultRecord(Env, Output->variant.result_record, &PrintString);
            } break;

            case GDBWIRE_MI_OUTPUT_PARSE_ERROR: {
                PushString(&PrintString, "An error occurred on token %s\n",
                           Output->variant.error.token);
            } break;

            case GDBWIRE_MI_OUTPUT_PROMPT: {
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

    emacs_value Result = Env->make_string(Env, PrintString.Contents, PrintString.Len);
    FreeString(&PrintString);

    return Result;
}

s32 emacs_module_init(emacs_runtime *EmacsRuntime) {
    emacs_env *Env = EmacsRuntime->get_environment(EmacsRuntime);

    emacs_value Arguments[2];
    Arguments[0] = Intern(Env, HANDLE_GDB_MI_OUTPUT_NAME);
    Arguments[1] = Env->make_function(Env, 1, 1, HandleGdbMiOutput, HANDLE_GDB_MI_OUTPUT_DOC, 0);
    Funcall(Env, "fset", 2, Arguments);

    gdbwire_callbacks Callbacks = {0, GdbWireCallback};
    GdbMiParser = gdbwire_mi_parser_create(Callbacks);

    Nil = Intern(Env, "nil");
    T = Intern(Env, "t");

    emacs_value Feature = Intern(Env, "gdb-module");
    Funcall(Env, "provide", 1, &Feature);
    return 0;
}
