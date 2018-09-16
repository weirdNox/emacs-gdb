#include <assert.h>
#include "emacs-module.h"
#include "gdbwire.c"

#define arrayCount(Array) (sizeof(Array) / sizeof(*(Array)))

#define invalidCodePath() assert(0)
#define invalidDefaultCase() default: { invalidCodePath(); } break
#define ignoreCase(Case) case (Case): break
#define ignoreDefaultCase() default: break

#define writeVariableKey(Key, Name, ...) Variable_##Key
#define writeVariableName(Key, Name, ...) #Name
#define writeElispStructKey(Key, Name, Elisp) intern(Env, ":" #Elisp)

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

#define apiFunctionsWriter(W) W(ExtractContext, gdb--extract-context)   \
        W(SetInitialFile, gdb--set-initial-file)                        \
        W(RunningFunc, gdb--running)                                    \
        W(GetThreadInfo, gdb--get-thread-info)                          \
        W(UpdateThread, gdb--update-thread)                             \
        W(ClearThreadFrames, gdb--clear-thread-frames)                  \
        W(AddFrameToThread, gdb--add-frame-to-thread)                   \
        W(FinalizeThreadFrames, gdb--finalize-thread-frames)            \
        W(ThreadExited, gdb--thread-exited)                             \
        W(BreakpointChanged, gdb--breakpoint-changed)                   \
        W(BreakpointDeleted, gdb--breakpoint-deleted)                   \
        W(SetDisassembly, gdb--set-disassembly)                         \
        W(MakeInstruction, make-gdb--instruction)                       \
        W(MakeSourceInfo, make-gdb--source-instr-info)
    ; // Weird indentation...

#define apiFunctionVariableWriter(Name, ...) static emacs_value Name;
#define apiFunctionInternWriter(Name, ElispName) Name = intern(Env, #ElispName);

apiFunctionsWriter(apiFunctionVariableWriter);

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

static string allocateString() {
    string Result = {
        .Cap = 1<<13
    };
    Result.Contents = xMalloc(Result.Cap);
    return Result;
}

static void pushString(string *Str, char *Format, ...) {
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

static void freeString(string *Str) {
    if(Str && Str->Contents) {
        free(Str->Contents);
        *Str = (string){};
    }
}

static void gdbWireCallback(void *Ctx, mi_output *Output) {
    mi_output **WriteTo = &ParserOutput;
    while(*WriteTo) {
        WriteTo = &(*WriteTo)->next;
    }
    *WriteTo = Output;
}

static void *getResultVariable_(mi_result *Result, char *Variable,
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

#define getResultTuple(Result, Variable)                                \
    (mi_result *)getResultVariable_(Result, Variable, GDBWIRE_MI_TUPLE)
#define getResultList(Result, Variable)                                 \
    (mi_result *)getResultVariable_(Result, Variable, GDBWIRE_MI_LIST)
#define getResultString(Result, Variable)                               \
    (char *)getResultVariable_(Result, Variable, GDBWIRE_MI_CSTRING)

static void getBatchResultString(mi_result *Result, char *Variables[], char *Values[],
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

static inline emacs_value getEmacsString_(emacs_env *Env, char *String, bool EmptyIfNull) {
    emacs_value Result = Nil;

    if(!String && EmptyIfNull) {
        String = "";
    }

    if(String) {
        Result = Env->make_string(Env, String, strlen(String));
    }

    return Result;
}
#define getEmacsString(Env, String) getEmacsString_(Env, String, false)
#define getEmacsStringEmpty(Env, String) getEmacsString_(Env, String, true)

static inline void getEmacsStrings(emacs_env *Env, char *Vals[], emacs_value EmacsVals[], u32 Size,
                                   bool EmptyIfNull) {
    if(Vals && EmacsVals) {
        for(int Index = 0; Index < Size; ++Index) {
            EmacsVals[Index] = getEmacsString_(Env, Vals[Index], EmptyIfNull);
        }
    }
}

static inline emacs_value intern(emacs_env *Env, char *SymName) {
    emacs_value Result = Env->intern(Env, SymName);
    return Result;
}

static inline emacs_value funcall(emacs_env *Env, emacs_value Func, u32 NumArgs, emacs_value *Args) {
    emacs_value Result = Env->funcall(Env, Func, NumArgs, Args);
    return Result;
}

static inline emacs_value internFuncall(emacs_env *Env, char *Func, u32 NumArgs, emacs_value *Args) {
    emacs_value Result = Env->funcall(Env, intern(Env, Func), NumArgs, Args);
    return Result;
}

static void breakpointChange(emacs_env *Env, mi_result *Breakpoint) {
#define breakpointVariablesWriter(W) W(Number, number), \
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

    enum { breakpointVariablesWriter(writeVariableKey) };

    char *Variables[] = { breakpointVariablesWriter(writeVariableName) };
    char *Values[arrayCount(Variables)] = {};
    getBatchResultString(Breakpoint, Variables, Values, arrayCount(Variables));

    emacs_value Args[arrayCount(Variables)];
    getEmacsStrings(Env, Values, Args, arrayCount(Variables), false);
    funcall(Env, BreakpointChanged, arrayCount(Args), Args);
}

static void breakpointDeletion(emacs_env *Env, char *Id) {
    emacs_value Argument = getEmacsString(Env, Id);
    funcall(Env, BreakpointDeleted, 1, &Argument);
}

static void threadInfo(emacs_env *Env, mi_result *Result) {
    mi_result *Threads = getResultList(Result, "threads");

#define threadVariablesWriter(W) W(Id, id),     \
        W(TargetId, target-id),                 \
        W(Name, name),                          \
        W(State, state),                        \
        W(Core, core)

    enum { threadVariablesWriter(writeVariableKey) };
    char *Variables[] = { threadVariablesWriter(writeVariableName) };

    for(mi_result *Thread = Threads; Thread && Thread->kind == GDBWIRE_MI_TUPLE; Thread = Thread->next) {
        mi_result *ThreadContents = Thread->variant.result;
        char *Values[arrayCount(Variables)] = {};
        getBatchResultString(ThreadContents, Variables, Values, arrayCount(Variables));

        emacs_value Args[arrayCount(Values)];
        getEmacsStrings(Env, Values, Args, arrayCount(Variables), false);
        funcall(Env, UpdateThread, arrayCount(Args), Args);
    }
}

static void frameInfo(emacs_env *Env, mi_result *Result, emacs_value ThreadId) {
    funcall(Env, ClearThreadFrames, 1, &ThreadId);

#define frameVariablesWriter(W) W(Level, level),    \
        W(Address, addr),                           \
        W(Function, func),                          \
        W(File, fullname),                          \
        W(Line, line),                              \
        W(From, from)

    enum { frameVariablesWriter(writeVariableKey) };
    char *Variables[] = { frameVariablesWriter(writeVariableName) };

    for(mi_result *Frame = Result; Frame; Frame = Frame->next) {
        mi_result *FrameContents = Frame->variant.result;
        char *Values[arrayCount(Variables)] = {};
        getBatchResultString(FrameContents, Variables, Values, arrayCount(Variables));

        emacs_value Args[1 + arrayCount(Variables)];
        Args[0] = ThreadId;
        getEmacsStrings(Env, Values, Args+1, arrayCount(Variables), false);
        funcall(Env, AddFrameToThread, arrayCount(Args), Args);
    }

    funcall(Env, FinalizeThreadFrames, 1, &ThreadId);
}

static void disassemble(emacs_env *Env, mi_result *List, emacs_value Buffer) {

#define sourceInfoVariablesWriter(W) W(Line, line, line), W(File, fullname, file)

    enum { sourceInfoVariablesWriter(writeVariableKey) };
    char *SourceVariables[] = { sourceInfoVariablesWriter(writeVariableName) };
    emacs_value SourceInfoElispKeys[] = { sourceInfoVariablesWriter(writeElispStructKey) };

#define instructionVariablesWriter(W) W(Address, address, address), \
        W(Function, func-name, function),                           \
        W(Offset, offset, offset),                                  \
        W(Instruction, inst, instruction)

    enum { instructionVariablesWriter(writeVariableKey) };
    char *InstructionVariables[] = { instructionVariablesWriter(writeVariableName) };
    emacs_value InstructionElispKeys[] = { instructionVariablesWriter(writeElispStructKey) };

    bool HasSourceInfo = !strcmp(List->variable, "src_and_asm_line");

    u32 SourceInfoInstrListIndex = 3;
    emacs_value DisassemblyList = Nil;
    emacs_value Cons = intern(Env, "cons");

    for(mi_result *Iterator = List; Iterator; Iterator = Iterator->next) {
        mi_result *Contents = Iterator->variant.result;
        if(HasSourceInfo) {
            char *SourceValues[arrayCount(SourceVariables)] = {};
            emacs_value SourceInfoObj;
            getBatchResultString(Contents, SourceVariables, SourceValues, arrayCount(SourceVariables));

            {
                emacs_value Args[arrayCount(SourceVariables)*2];
                for(int Index = 0;
                    Index < arrayCount(SourceVariables);
                    ++Index)
                {
                    Args[Index*2] = SourceInfoElispKeys[Index];
                    Args[Index*2 + 1] = getEmacsString(Env, SourceValues[Index]);
                }
                SourceInfoObj = funcall(Env, MakeSourceInfo, arrayCount(Args), Args);
            }

            mi_result *InstrList = getResultList(Contents, "line_asm_insn");
            for(mi_result *InstrIterator = InstrList; InstrIterator; InstrIterator = InstrIterator->next) {
                mi_result *Instruction = InstrIterator->variant.result;
                emacs_value InstructionObj;

                char *InstructionValues[arrayCount(InstructionVariables)] = {};
                getBatchResultString(Instruction, InstructionVariables,
                                     InstructionValues, arrayCount(InstructionVariables));

                emacs_value Args[arrayCount(InstructionVariables)*2];
                for(int Index = 0; Index < arrayCount(InstructionVariables); ++Index) {
                    Args[Index*2] = InstructionElispKeys[Index];
                    Args[Index*2 + 1] = getEmacsString(Env, InstructionValues[Index]);
                }
                InstructionObj = funcall(Env, MakeInstruction, arrayCount(Args), Args);

                Env->vec_set(Env, SourceInfoObj, SourceInfoInstrListIndex,
                             funcall(Env, Cons, 2, (emacs_value[]){InstructionObj, Env->vec_get(Env, SourceInfoObj, SourceInfoInstrListIndex)}));
            }

            {
                emacs_value Args[2] = {SourceInfoObj, DisassemblyList};
                DisassemblyList = funcall(Env, Cons, 2, Args);
            }
        } else {
            // TODO(nox): Finish this section
        }
    }

    {
        emacs_value Args[3] = {Buffer, DisassemblyList, HasSourceInfo ? T : Nil};
        funcall(Env, SetDisassembly, 3, Args);
    }
}

static void miAsyncStopped(emacs_env *Env, mi_result *Result, string *PrintString) {
    for(mi_result *Iter = Result; Iter; Iter = Iter->next) {
        if(Iter->variable && strcmp(Iter->variable, "stopped-threads") == 0) {
            if(Iter->kind == GDBWIRE_MI_CSTRING) {
                funcall(Env, GetThreadInfo, 0, 0);
            } else {
                for(mi_result *Thread = Iter->variant.result; Thread; Thread = Thread->next) {
                    emacs_value Arg = getEmacsString(Env, Thread->variant.cstring);
                    funcall(Env, GetThreadInfo, 1, &Arg);
                }
            }

            break;
        }
    }

    // NOTE(nox): *stopped does not end with a prompt...
    pushString(PrintString, "(gdb) ");
}

static void handleMiOobRecord(emacs_env *Env, mi_oob_record *Record, string *PrintString) {
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
                            emacs_value Argument = getEmacsString(Env, getResultString(Result, "thread-id"));
                            funcall(Env, RunningFunc, 1, &Argument);
                        } break;

                        ignoreDefaultCase();
                    }
                } break;

                case GDBWIRE_MI_NOTIFY: {
                    switch(AsyncRecord->async_class) {
                        case GDBWIRE_MI_ASYNC_THREAD_CREATED: {
                            char *ThreadId = getResultString(Result, "id");
                            emacs_value Argument = getEmacsString(Env, ThreadId);
                            funcall(Env, GetThreadInfo, 1, &Argument);
                        } break;

                        case GDBWIRE_MI_ASYNC_THREAD_EXITED: {
                            char *ThreadId = getResultString(Result, "id");
                            emacs_value Argument = getEmacsString(Env, ThreadId);
                            funcall(Env, ThreadExited, 1, &Argument);
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_CREATED:
                        case GDBWIRE_MI_ASYNC_BREAKPOINT_MODIFIED: {
                            breakpointChange(Env, getResultTuple(Result, "bkpt"));
                        } break;

                        case GDBWIRE_MI_ASYNC_BREAKPOINT_DELETED: {
                            breakpointDeletion(Env, getResultString(Result, "id"));
                        } break;

                        ignoreDefaultCase();
                    }
                } break;

                ignoreCase(GDBWIRE_MI_STATUS); // NOTE(nox): May be ignored
            }
        } break;

        case GDBWIRE_MI_STREAM: {
            mi_stream_record *StreamRecord = Record->variant.stream_record;
            switch(StreamRecord->kind) {
                case GDBWIRE_MI_CONSOLE: {
                    pushString(PrintString, "%s", StreamRecord->cstring);
                } break;

                case GDBWIRE_MI_TARGET: {
                    // TODO(nox): If we are outputting to another tty, will we ever receive this?
                    pushString(PrintString, "Target: %s", StreamRecord->cstring);
                } break;

                ignoreCase(GDBWIRE_MI_LOG);
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
        Context_threadInfo,
        Context_frameInfo,
        Context_disassemble,

        Context_Size,
    } Type;

    emacs_value Data;
} token_context;

static token_context getTokenContext(emacs_env *Env, char *TokenString) {
    token_context Result = {};
    if(TokenString) {
        emacs_value Argument = getEmacsString(Env, TokenString);
        emacs_value ContextCons = funcall(Env, ExtractContext, 1, &Argument);

        Result.Type = Env->extract_integer(Env, internFuncall(Env, "car", 1, &ContextCons));
        assert(Result.Type < Context_Size);

        Result.Data = internFuncall(Env, "cdr", 1, &ContextCons);
    }
    return Result;
}

static void handleMiResultRecord(emacs_env *Env, mi_result_record *Record, string *PrintString) {
    token_context Context = getTokenContext(Env, Record->token);

    mi_result *Result = Record->result;
    switch(Record->result_class) {
        case GDBWIRE_MI_DONE:
        case GDBWIRE_MI_RUNNING:
        case GDBWIRE_MI_CONNECTED: {
            switch(Context.Type) {
                case Context_InitialFile: {
                    char *File = getResultString(Result, "fullname");
                    char *Line = getResultString(Result, "line");
                    if(File) {
                        emacs_value Args[2];
                        Args[0] = getEmacsString(Env, File);
                        Args[1] = getEmacsStringEmpty(Env, Line);
                        funcall(Env, SetInitialFile, 2, Args);
                    }
                } break;

                case Context_BreakpointInsert: {
                    breakpointChange(Env, getResultTuple(Result, "bkpt"));
                } break;

                case Context_threadInfo: {
                    threadInfo(Env, Result);
                } break;

                case Context_frameInfo: {
                    frameInfo(Env, getResultList(Result, "stack"), Context.Data);
                } break;

                case Context_disassemble: {
                    disassemble(Env, getResultList(Result, "asm_insns"), Context.Data);
                } break;

                ignoreDefaultCase();
            }
        } break;

        case GDBWIRE_MI_ERROR: {
            switch(Context.Type) {
                ignoreCase(Context_Ignore);
                ignoreCase(Context_InitialFile);

                default: {
                    char *Message = getResultString(Result, "msg");
                    if(Message) {
                        pushString(PrintString, "%s\n", Result->variant.cstring);
                    }
                } break;
            }
        } break;

        case GDBWIRE_MI_EXIT: {
            pushString(PrintString, "Bye bye :)\n");
        } break;

        case GDBWIRE_MI_UNSUPPORTED: {
            pushString(PrintString, "Error while parsing, GdbWire couldn't figure out class of result record!");
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
static emacs_value handleGdbMiOutput(emacs_env *Env, ptrdiff_t NumberOfArgs,
                                     emacs_value Args[], void *Closure) {
    ptrdiff_t Size;
    Env->copy_string_contents(Env, Args[0], 0, &Size);
    char *OutputString = xMalloc(Size);
    Env->copy_string_contents(Env, Args[0], OutputString, &Size);

    gdbwire_mi_parser_push(GdbMiParser, OutputString);
    free(OutputString);

    string PrintString = allocateString();
    for(mi_output *Output = ParserOutput; Output; Output = Output->next) {
        switch(Output->kind) {
            case GDBWIRE_MI_OUTPUT_OOB: {
                handleMiOobRecord(Env, Output->variant.oob_record, &PrintString);
            } break;

            case GDBWIRE_MI_OUTPUT_RESULT: {
                handleMiResultRecord(Env, Output->variant.result_record, &PrintString);
            } break;

            case GDBWIRE_MI_OUTPUT_PARSE_ERROR: {
                pushString(&PrintString, "An error occurred on token %s\n",
                           Output->variant.error.token);
            } break;

            case GDBWIRE_MI_OUTPUT_PROMPT: {
                if(!IgnoreNextPrompt)
                {
                    pushString(&PrintString, "(gdb) ");
                }
                IgnoreNextPrompt = false;
            } break;
        }
    }
    gdbwire_mi_output_free(ParserOutput);
    ParserOutput = 0;

    emacs_value Result = Env->make_string(Env, PrintString.Contents, PrintString.Len);
    freeString(&PrintString);

    return Result;
}

s32 emacs_module_init(emacs_runtime *EmacsRuntime) {
    emacs_env *Env = EmacsRuntime->get_environment(EmacsRuntime);

    apiFunctionsWriter(apiFunctionInternWriter);

    emacs_value Args[2];
    Args[0] = intern(Env, HANDLE_GDB_MI_OUTPUT_NAME);
    Args[1] = Env->make_function(Env, 1, 1, handleGdbMiOutput, HANDLE_GDB_MI_OUTPUT_DOC, 0);
    internFuncall(Env, "defalias", 2, Args);

    gdbwire_callbacks Callbacks = {0, gdbWireCallback};
    GdbMiParser = gdbwire_mi_parser_create(Callbacks);

    Nil = intern(Env, "nil");
    T = intern(Env, "t");

    emacs_value Feature = intern(Env, "gdb-module");
    internFuncall(Env, "provide", 1, &Feature);
    return 0;
}
