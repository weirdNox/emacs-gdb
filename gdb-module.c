#include <assert.h>
#include "emacs-module.h"
#include "gdbwire.c"

#define arrayCount(Array) (sizeof(Array) / sizeof(*(Array)))

#define invalidCodePath() assert(0)
#define invalidDefaultCase() default: { invalidCodePath(); } break
#define ignoreCase(Case) case (Case): break
#define ignoreDefaultCase() default: break

#define min(x, y) ((x) <= (y) ? (x) : (y))
#define max(x, y) ((x) >= (y) ? (x) : (y))
#define clampMax(X, Max) min(X, Max)
#define clampMin(X, Min) max(X, Min)

#define writeResultKey(EnumKey, GdbName, ...) Result_##EnumKey
#define writeGdbName(EnumKey, GdbName, ...) #GdbName
#define writeElispStructKey(EnumKey, GdbName, EStructKey) intern(Env, ":" #EStructKey)

#define enumWriterPrefix(ResultVariables, Prefix) enum { ResultVariables(writeResultKey), Prefix##_Count}
#define enumWriter(ResultVariables) enumWriterPrefix(ResultVariables, Result)
#define gdbNamesWriter(Name, ResultVariables) char *Name[] = { ResultVariables(writeGdbName) }

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

#define interns(W) W(Nil, nil)                                      \
        W(T, t)                                                     \
        W(Cons, cons)                                               \
        W(Car, car)                                                 \
        W(Cdr, cdr)                                                 \
        W(VecFunc, vector)                                          \
        W(ListFunc, list)                                           \
        W(Set, set)                                                 \
        W(Eval, eval)                                               \
        W(Eq, eq)                                                   \
        W(DeleteProcess, delete-process)                            \
                                                                    \
        W(GdbCmd, gdb--command)                                     \
        W(ExtractContext, gdb--extract-context)                     \
        W(Error, error)                                             \
        W(LogError, gdb--log-error)                                 \
                                                                    \
        W(GdbData, gdb--data)                                       \
        W(ConsoleOutputToData, to-data)                             \
        W(OmitConsoleOutput, gdb--omit-console-output)              \
                                                                    \
        W(SwitchToThread, gdb--switch-to-thread)                    \
        W(SwitchToFrame, gdb--switch-to-frame)                      \
                                                                    \
        W(SetInitialFile, gdb--set-initial-file)                    \
        W(RunningFunc, gdb--running)                                \
        W(GetThreadInfo, gdb--get-thread-info)                      \
        W(UpdateThread, gdb--update-thread)                         \
        W(AddFramesToThread, gdb--add-frames-to-thread)             \
        W(ThreadExited, gdb--thread-exited)                         \
        W(BreakpointChanged, gdb--breakpoint-changed)               \
        W(BreakpointEnableDisable, gdb--breakpoint-enable-disable)  \
        W(BreakpointDeleted, gdb--breakpoint-deleted)               \
        W(AddVariablesToFrame, gdb--add-variables-to-frame)         \
        W(NewWatcherInfo, gdb--new-watcher-info)                    \
        W(WatchersUpdateInfo, gdb--watchers-update-info)            \
        W(WatcherAddChildren, gdb--watcher-add-children)            \
        W(WatcherFormatChange, gdb--watcher-format-change)          \
        W(SetRegisterNames, gdb--set-register-names)                \
        W(UpdateRegisters, gdb--update-registers)                   \
        W(SetDisassembly, gdb--set-disassembly)                     \
        W(MakeInstr, make-gdb--disassembly-instr)                   \
        W(MakeSrc, make-gdb--disassembly-src)                       \
        W(PersistThread, gdb--persist-thread)
    ; // Weird indentation...

#define internVariableWriter(Name, ...) static emacs_value Name;
#define internSetterWriter(Name, ElispName) Name = Env->make_global_ref(Env, intern(Env, #ElispName));

interns(internVariableWriter);

static struct gdbwire_mi_parser *GdbMiParser;
static mi_output *ParserOutput;

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

typedef struct buf_header {
    size_t Len;
    size_t Cap;
    char Buf[];
} buf_header;

#define bufHdr_(B) ((buf_header *)((char *)(B) - sizeof(buf_header)))

#define bufLen(B) ((B) ? bufHdr_(B)->Len : 0)
#define bufCap(B) ((B) ? bufHdr_(B)->Cap : 0)
#define bufEnd(B) ((B) + bufLen(B))
#define bufSizeof(B) ((B) ? bufLen(B)*sizeof(*B) : 0)

#define bufFree(B) ((B) ? (free(bufHdr_(B)), (B) = 0) : 0)
#define bufFit(B, N) ((N) <= bufCap(B) ? 0 : ((B) = bufGrow_((B), (N), sizeof(*(B)))))
#define bufPush(B, ...) (bufFit((B), 1 + bufLen(B)), (B)[bufHdr_(B)->Len++] = (__VA_ARGS__))
#define bufPrintf(B, ...) ((B) = bufPrintf_((B), __VA_ARGS__))
#define bufClear(B) ((B) ? bufHdr_(B)->Len = 0 : 0)

void *bufGrow_(const void *Buf, size_t NewLen, size_t ElemSize) {
    assert(bufCap(Buf) <= (SIZE_MAX - 1)/2);
    size_t NewCap = clampMin(2*bufCap(Buf), clampMin(NewLen, 16));
    assert(NewLen <= NewCap);
    assert(NewCap <= (SIZE_MAX - sizeof(buf_header))/ElemSize);
    size_t NewSize = offsetof(buf_header, Buf) + NewCap*ElemSize;
    buf_header *NewHdr;
    if(Buf) {
        NewHdr = xRealloc(bufHdr_(Buf), NewSize);
    } else {
        NewHdr = xMalloc(NewSize);
        NewHdr->Len = 0;
    }
    NewHdr->Cap = NewCap;
    return NewHdr->Buf;
}

char *bufPrintf_(char *Buf, const char *Fmt, ...) {
    va_list Args;
    va_start(Args, Fmt);
    size_t Available = bufCap(Buf) - bufLen(Buf);
    size_t N = 1 + vsnprintf(bufEnd(Buf), Available, Fmt, Args);
    va_end(Args);
    if(N > Available) {
        bufFit(Buf, N + bufLen(Buf));
        va_start(Args, Fmt);
        Available = bufCap(Buf) - bufLen(Buf);
        N = 1 + vsnprintf(bufEnd(Buf), Available, Fmt, Args);
        assert(N <= Available);
        va_end(Args);
    }
    bufHdr_(Buf)->Len += N - 1;
    return Buf;
}

static void gdbWireCallback(void *Ctx, mi_output *Output) {
    mi_output **WriteTo = &ParserOutput;
    while(*WriteTo) {
        WriteTo = &(*WriteTo)->next;
    }
    *WriteTo = Output;
}

static void *getResultVariable_(mi_result *Result, char *Variable, enum gdbwire_mi_result_kind Kind) {
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

#define getResultTuple(Result, Variable) (mi_result *)getResultVariable_(Result, Variable, GDBWIRE_MI_TUPLE)
#define getResultList(Result, Variable)  (mi_result *)getResultVariable_(Result, Variable, GDBWIRE_MI_LIST)
#define getResultString(Result, Variable)     (char *)getResultVariable_(Result, Variable, GDBWIRE_MI_CSTRING)

static void getBatchResultString(mi_result *Result, char *Variables[], char *Values[], u32 NumberOfVariables) {
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

static inline bool isNil(emacs_env *Env, emacs_value Thing) {
    return !Env->is_not_nil(Env, Thing);
}

static inline bool isNotNil(emacs_env *Env, emacs_value Thing) {
    return Env->is_not_nil(Env, Thing);
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
        W(IgnoreCount, ignore),                         \
        W(What, what)

    enum { breakpointVariablesWriter(writeResultKey) };

    char *Variables[] = { breakpointVariablesWriter(writeGdbName) };
    char *Values[arrayCount(Variables)] = {};
    getBatchResultString(Breakpoint, Variables, Values, arrayCount(Variables));

    emacs_value Args[arrayCount(Variables)];
    getEmacsStrings(Env, Values, Args, arrayCount(Variables), false);
    funcall(Env, BreakpointChanged, arrayCount(Args), Args);
}

static void breakpointDeletion(emacs_env *Env, char *Id) {
    emacs_value Arg = getEmacsString(Env, Id);
    funcall(Env, BreakpointDeleted, 1, &Arg);
}

static void threadInfo(emacs_env *Env, mi_result *Result) {
    mi_result *Threads = getResultList(Result, "threads");

#define threadVariablesWriter(W) W(Id, id),     \
        W(TargetId, target-id),                 \
        W(Name, name),                          \
        W(State, state),                        \
        W(Core, core)

    enum { threadVariablesWriter(writeResultKey) };
    char *Variables[] = { threadVariablesWriter(writeGdbName) };

    for(mi_result *Thread = Threads; Thread && Thread->kind == GDBWIRE_MI_TUPLE; Thread = Thread->next) {
        mi_result *ThreadContents = Thread->variant.result;
        char *Values[arrayCount(Variables)] = {};
        getBatchResultString(ThreadContents, Variables, Values, arrayCount(Variables));

        emacs_value Args[arrayCount(Values)];
        getEmacsStrings(Env, Values, Args, arrayCount(Variables), false);
        funcall(Env, UpdateThread, arrayCount(Args), Args);
    }
}

static void frameInfo(emacs_env *Env, mi_result *List, emacs_value Thread) {
    emacs_value *Args = 0;
    bufPush(Args, Thread);

#define resultKeys(W) W(Level, level),          \
        W(Address, addr),                       \
        W(Function, func),                      \
        W(File, fullname),                      \
        W(Line, line),                          \
        W(From, from)

    gdbNamesWriter(GdbKeys, resultKeys);
    enumWriter(resultKeys);
#undef resultKeys

    for(mi_result *Frame = List; Frame; Frame = Frame->next) {
        char *Values[Result_Count] = {};
        mi_result *Tuple = Frame->variant.result;
        getBatchResultString(Tuple, GdbKeys, Values, Result_Count);

        emacs_value ListValues[Result_Count];
        getEmacsStrings(Env, Values, ListValues, Result_Count, false);
        bufPush(Args, funcall(Env, ListFunc, Result_Count, ListValues));
    }

    funcall(Env, AddFramesToThread, bufLen(Args), Args);
    bufFree(Args);
}

static void getVariables(emacs_env *Env, mi_result *List, emacs_value Frame) {
    emacs_value *Args = 0;
    bufPush(Args, Frame);

#define resultKeys(W) W(Name, name), W(Type, type), W(Value, value)
    gdbNamesWriter(GdbKeys, resultKeys);
    enumWriter(resultKeys);
#undef  resultKeys

    for(mi_result *Variable = List; Variable; Variable = Variable->next) {
        char *Values[Result_Count] = {};
        mi_result *Tuple = Variable->variant.result;
        getBatchResultString(Tuple, GdbKeys, Values, Result_Count);

        emacs_value ListValues[Result_Count];
        getEmacsStrings(Env, Values, ListValues, Result_Count, false);
        bufPush(Args, funcall(Env, ListFunc, Result_Count, ListValues));
    }

    funcall(Env, AddVariablesToFrame, bufLen(Args), Args);
    bufFree(Args);
}

static void watcherCreate(emacs_env *Env, mi_result *Tuple, emacs_value Data) {
#define resultKeys(W) W(Name, name),            \
        W(NumChild, numchild),                  \
        W(Value, value),                        \
        W(Type, type),                          \
        W(ThreadId, thread-id)

    gdbNamesWriter(GdbKeys, resultKeys);
    enumWriter(resultKeys);
#undef resultKeys

    char *Values[Result_Count] = {};
    getBatchResultString(Tuple, GdbKeys, Values, Result_Count);

    emacs_value Args[1 + Result_Count];
    Args[0] = Data;
    getEmacsStrings(Env, Values, Args+1, Result_Count, false);
    funcall(Env, NewWatcherInfo, arrayCount(Args), Args);
}

static void watchersUpdate(emacs_env *Env, mi_result *List, emacs_value ShouldHighlight) {
    emacs_value *Args = 0;
    bufPush(Args, ShouldHighlight);

#define resultKeys(W) W(Name, name),            \
        W(Value, value),                        \
        W(InScope, in_scope),                   \
        W(TypeChanged, type_changed),           \
        W(NewType, new_type),                   \
        W(NewNumChildren, new_num_children)

    gdbNamesWriter(GdbKeys, resultKeys);
    enumWriter(resultKeys);
#undef  resultKeys

    for(mi_result *Variable = List; Variable; Variable = Variable->next) {
        char *Values[Result_Count] = {};
        mi_result *Tuple = Variable->variant.result;
        getBatchResultString(Tuple, GdbKeys, Values, Result_Count);

        emacs_value ListValues[Result_Count];
        getEmacsStrings(Env, Values, ListValues, Result_Count, false);
        bufPush(Args, funcall(Env, ListFunc, Result_Count, ListValues));
    }

    funcall(Env, WatchersUpdateInfo, bufLen(Args), Args);
    bufFree(Args);
}

static void watcherAddChildren(emacs_env *Env, mi_result *List, emacs_value Watcher) {
    emacs_value *Args = 0;
    bufPush(Args, Watcher);

#define resultKeys(W) W(Name, name),            \
        W(Expr, exp),                           \
        W(NumChildren, numchild),               \
        W(Value, value),                        \
        W(Type, type),                          \
        W(ThreadId, thread-id)

    gdbNamesWriter(GdbKeys, resultKeys);
    enumWriter(resultKeys);
#undef  resultKeys

    for(mi_result *Child = List; Child; Child = Child->next) {
        char *Values[Result_Count] = {};
        mi_result *Tuple = Child->variant.result;
        getBatchResultString(Tuple, GdbKeys, Values, Result_Count);

        emacs_value ListValues[Result_Count];
        getEmacsStrings(Env, Values, ListValues, Result_Count, false);
        bufPush(Args, funcall(Env, ListFunc, Result_Count, ListValues));
    }

    funcall(Env, WatcherAddChildren, bufLen(Args), Args);
    bufFree(Args);
}

static void setRegisterNames(emacs_env *Env, mi_result *List, emacs_value Thread) {
    emacs_value *Args = 0;
    bufPush(Args, Thread);

    int Count = 0;
    for(mi_result *Element = List; Element; Element = Element->next) {
        ++Count;
    }
    bufPush(Args, Env->make_integer(Env, Count));

    for(mi_result *Element = List; Element; Element = Element->next) {
        bufPush(Args, getEmacsString(Env, Element->variant.cstring));
    }

    funcall(Env, SetRegisterNames, bufLen(Args), Args);
    bufFree(Args);
}

static void updateRegisters(emacs_env *Env, mi_result *List, emacs_value Thread) {
    emacs_value *Args = 0;
    bufPush(Args, Thread);

#define resultKeys(W) W(Number, number),        \
        W(Value, value)

    gdbNamesWriter(GdbKeys, resultKeys);
    enumWriter(resultKeys);
#undef  resultKeys

    for(mi_result *Register = List; Register; Register = Register->next) {
        char *Values[Result_Count] = {};
        mi_result *Tuple = Register->variant.result;
        getBatchResultString(Tuple, GdbKeys, Values, Result_Count);

        if(*Values[Result_Value] == '{') {
            Values[Result_Value] = "<Composite register>";
        }

        emacs_value ConsValues[Result_Count];
        getEmacsStrings(Env, Values, ConsValues, Result_Count, false);
        bufPush(Args, funcall(Env, Cons, Result_Count, ConsValues));
    }

    funcall(Env, UpdateRegisters, bufLen(Args), Args);
    bufFree(Args);
}

static void getChangedRegisters(emacs_env *Env, mi_result *List, emacs_value DataCons) {
    emacs_value Thread = funcall(Env, Cdr, 1, &DataCons);
    if(List) {
        char Type[2];
        ptrdiff_t TypeSize = arrayCount(Type);
        Env->copy_string_contents(Env, funcall(Env, Car, 1, &DataCons), Type, &TypeSize);

        char *Cmd = 0;
        bufPrintf(Cmd, "-data-list-register-values --skip-unavailable %s", Type);
        for(mi_result *Element = List;
            Element;
            Element = Element->next)
        {
            bufPrintf(Cmd, " %s", Element->variant.cstring);
        }

        emacs_value Args[] = {
            getEmacsString(Env, Cmd),
            funcall(Env, Cons, 2, (emacs_value[]){intern(Env, "gdb--context-registers-update"), Thread}),
            Thread
        };
        funcall(Env, GdbCmd, arrayCount(Args), Args);
        bufFree(Cmd);
    } else {
        // NOTE(nox): Update registers nonetheless in order to update tick and remove modified highlight
        updateRegisters(Env, 0, Thread);
    }
}

typedef struct instrs_info {
    int AddrWidth;
    int InstrWidth;
    int OpcodeWidth;
    emacs_value List;
} instrs_info;

static instrs_info instructionsInfo(emacs_env *Env, mi_result *InstrsList) {
    emacs_value *Args = 0;

#define resultKeys(W) W(Addr, address, addr),   \
        W(Func, func-name, func),               \
        W(Offset, offset, offset),              \
        W(Instr, inst, instr),                  \
        W(RawOp, opcodes, opcodes)

    gdbNamesWriter(InstrKeys, resultKeys);
    enumWriterPrefix(resultKeys, InstrKey);
    emacs_value InstrInterns[] = {resultKeys(writeElispStructKey)};
#undef  resultKeys

    instrs_info Result = {};

    if(InstrsList) {
        char *FirstAddr = getResultString(InstrsList->variant.result, InstrKeys[Result_Addr]);
        Result.AddrWidth = strlen(FirstAddr ? FirstAddr : "");
    }

    for(mi_result *Instr = InstrsList; Instr; Instr = Instr->next) {
        char *InstrValues[InstrKey_Count] = {};
        mi_result *InstrTuple = Instr->variant.result;
        getBatchResultString(InstrTuple, InstrKeys, InstrValues, InstrKey_Count);

        if(InstrValues[Result_Instr]) {
            int Width = 0;
            for(char *Iter = InstrValues[Result_Instr]; *Iter; ++Iter) {
                if(*Iter == '\t') {
                    *Iter = ' ';
                }
                ++Width;
            }
            Result.InstrWidth = max(Result.InstrWidth, Width);
        }

        if(InstrValues[Result_RawOp]) {
            int Width = strlen(InstrValues[Result_RawOp]);
            Result.OpcodeWidth = max(Result.OpcodeWidth, Width);
        }

        emacs_value MakeArgs[] = {
            InstrInterns[Result_Addr],   getEmacsString(Env, InstrValues[Result_Addr]),
            InstrInterns[Result_Func],   getEmacsString(Env, InstrValues[Result_Func]),
            InstrInterns[Result_Offset], getEmacsString(Env, InstrValues[Result_Offset]),
            InstrInterns[Result_Instr],  getEmacsString(Env, InstrValues[Result_Instr]),
            InstrInterns[Result_RawOp],  getEmacsString(Env, InstrValues[Result_RawOp])
        };
        bufPush(Args, funcall(Env, MakeInstr, arrayCount(MakeArgs), MakeArgs));
    }

    Result.List = funcall(Env, ListFunc, bufLen(Args), Args);
    bufFree(Args);
    return Result;
}

static void disassemble(emacs_env *Env, mi_result *List, emacs_value BuffData) {
#define resultKeys(W) W(Line, line, line-str),                          \
        W(File, fullname, file),                                        \
        W(Instrs, line_asm_insn, instrs) // NOTE(nox): This is a tuple!

    gdbNamesWriter(SrcKeys, resultKeys);
    enumWriterPrefix(resultKeys, SrcKey);
    emacs_value SrcInterns[] = {resultKeys(writeElispStructKey)};
#undef  resultKeys

    // NOTE(nox): If it has a name, then it is the tuple with the sources
    bool HasSourceInfo = List->variable;
    if(HasSourceInfo) {
        emacs_value *Args = 0;
        bufPush(Args, BuffData);
        bufPush(Args, 0); // NOTE(nox): Width vector

        int AddrWidth   = 0;
        int OpcodeWidth = 0;
        int InstrWidth  = 0;

        for(mi_result *Src = List; Src; Src = Src->next) {
            char *SrcValues[SrcKey_Count-1] = {};
            mi_result *SrcTuple = Src->variant.result;
            getBatchResultString(SrcTuple, SrcKeys, SrcValues, arrayCount(SrcValues));

            mi_result *Instrs = getResultList(SrcTuple, SrcKeys[Result_Instrs]);
            instrs_info InstrsInfo = instructionsInfo(Env, Instrs);

            AddrWidth   = max(AddrWidth,   InstrsInfo.AddrWidth);
            OpcodeWidth = max(OpcodeWidth, InstrsInfo.OpcodeWidth);
            InstrWidth  = max(InstrWidth,  InstrsInfo.InstrWidth);

            emacs_value MakeArgs[] = {
                SrcInterns[Result_File],   getEmacsString(Env, SrcValues[Result_File]),
                SrcInterns[Result_Line],   getEmacsString(Env, SrcValues[Result_Line]),
                SrcInterns[Result_Instrs], InstrsInfo.List
            };
            bufPush(Args, funcall(Env, MakeSrc, arrayCount(MakeArgs), MakeArgs));
        }

        emacs_value VecArgs[] = {
            Env->make_integer(Env, AddrWidth),
            Env->make_integer(Env, OpcodeWidth),
            Env->make_integer(Env, InstrWidth)
        };
        Args[1] = funcall(Env, VecFunc, arrayCount(VecArgs), VecArgs);

        funcall(Env, SetDisassembly, bufLen(Args), Args);
        bufFree(Args);
    } else {
        instrs_info InstrsInfo = instructionsInfo(Env, List);
        emacs_value VecArgs[] = {
            Env->make_integer(Env, InstrsInfo.AddrWidth),
            Env->make_integer(Env, InstrsInfo.OpcodeWidth),
            Env->make_integer(Env, InstrsInfo.InstrWidth)
        };
        emacs_value Args[] = {
            BuffData,
            funcall(Env, VecFunc, arrayCount(VecArgs), VecArgs),
            InstrsInfo.List
        };

        funcall(Env, SetDisassembly, arrayCount(Args), Args);
    }
}

static void logError(emacs_env *Env, mi_result *Result, char **PrintString, char *Key) {
    char *Message = getResultString(Result, Key);
    if(Message) {
        bufPrintf(*PrintString, "%s\n(gdb) ", Result->variant.cstring);
        funcall(Env, LogError, 1, (emacs_value[]){getEmacsString(Env, Message)});
        funcall(Env, Set, 2, (emacs_value[]){OmitConsoleOutput, T});
    }
}

static void miAsyncStopped(emacs_env *Env, mi_result *Result, char **PrintString) {
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
}

static void handleMiOobRecord(emacs_env *Env, mi_oob_record *Record, char **PrintString) {
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
                            emacs_value Arg = getEmacsString(Env, getResultString(Result, "thread-id"));
                            funcall(Env, RunningFunc, 1, &Arg);
                        } break;

                        ignoreDefaultCase();
                    }
                } break;

                case GDBWIRE_MI_NOTIFY: {
                    switch(AsyncRecord->async_class) {
                        case GDBWIRE_MI_ASYNC_THREAD_CREATED: {
                            char *ThreadId = getResultString(Result, "id");
                            emacs_value Args[] = {getEmacsString(Env, ThreadId), Nil, Nil, Nil, Nil};
                            funcall(Env, UpdateThread, arrayCount(Args), Args);
                        } break;

                        case GDBWIRE_MI_ASYNC_THREAD_SELECTED: {
                            char *Id = getResultString(Result, "id");
                            mi_result *Frame = getResultTuple(Result, "frame");

                            if(Frame) {
                                char *ArgsStrings[] = {Id, getResultString(Frame, "level")};
                                emacs_value ConsArgs[arrayCount(ArgsStrings)] = {};
                                getEmacsStrings(Env, ArgsStrings, ConsArgs, arrayCount(ArgsStrings), false);

                                emacs_value FrameCons = funcall(Env, Cons, arrayCount(ConsArgs), ConsArgs);
                                emacs_value Args[arrayCount(ArgsStrings)] = {FrameCons, T};
                                funcall(Env, SwitchToFrame, arrayCount(Args), Args);
                            }
                            else {
                                emacs_value Args[] = {getEmacsString(Env, Id), T};
                                funcall(Env, SwitchToThread, arrayCount(Args), Args);
                            }
                        } break;

                        case GDBWIRE_MI_ASYNC_THREAD_EXITED: {
                            char *ThreadId = getResultString(Result, "id");
                            emacs_value Arg = getEmacsString(Env, ThreadId);
                            funcall(Env, ThreadExited, 1, &Arg);
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
                    emacs_value Omit = funcall(Env, Eval, 1, (emacs_value[]){OmitConsoleOutput});
                    if(isNil(Env, funcall(Env, Eval, 1, (emacs_value[]){OmitConsoleOutput}))) {
                        bufPrintf(*PrintString, "%s", StreamRecord->cstring);
                    }
                    else if(funcall(Env, Eq, 2, (emacs_value[]){Omit, ConsoleOutputToData})) {
                        emacs_value *Array = 0;

                        emacs_value UserPtr = funcall(Env, Eval, 1, (emacs_value[]){GdbData});
                        bool UserPtrExists = isNotNil(Env, UserPtr);
                        if(UserPtrExists) {
                            Array = Env->get_user_ptr(Env, UserPtr);
                        }

                        if(StreamRecord->cstring) {
                            // NOTE(nox): Remove newlines
                            StreamRecord->cstring[strlen(StreamRecord->cstring) - 1] = 0;
                        }
                        bufPush(Array, getEmacsString(Env, StreamRecord->cstring));
                        if(UserPtrExists) {
                            Env->set_user_ptr(Env, UserPtr, Array);
                        }
                        else {
                            UserPtr = Env->make_user_ptr(Env, 0, Array);
                            funcall(Env, Set, 2, (emacs_value[]){GdbData, UserPtr});
                        }
                    }
                } break;

                case GDBWIRE_MI_TARGET: {
                    bufPrintf(*PrintString, "Target: %s", StreamRecord->cstring);
                } break;

                ignoreCase(GDBWIRE_MI_LOG);
            }
        } break;
    }
}

typedef struct token_context {
    enum {
        Context_NoContext,

        Context_TtySet,
        Context_InitialFile,
        Context_ThreadInfo,
        Context_FrameInfo,
        Context_BreakpointInsert,
        Context_BreakpointEnableDisable,
        Context_BreakpointDelete,
        Context_GetVariables,
        Context_WatcherCreate,
        Context_WatcherUpdate,
        Context_WatcherListChildren,
        Context_WatcherChangeFormat,
        Context_RegistersListNames,
        Context_RegistersGetChanged,
        Context_RegistersUpdate,
        Context_Disassemble,
        Context_PersistThread,
        Context_GetData,
        Context_GetConsoleData,
        Context_IgnoreErrors,

        Context_Size,
    } Type;

    emacs_value Data;
} token_context;

static token_context getTokenContext(emacs_env *Env, char *TokenString) {
    token_context Result = {};
    if(TokenString) {
        emacs_value Arg = getEmacsString(Env, TokenString);
        emacs_value ContextCons = funcall(Env, ExtractContext, 1, &Arg);

        Result.Type = Env->extract_integer(Env, funcall(Env, Car, 1, &ContextCons));
        assert(Result.Type < Context_Size);

        Result.Data = funcall(Env, Cdr, 1, &ContextCons);
    }
    return Result;
}

static void handleMiResultRecord(emacs_env *Env, mi_result_record *Record, char **PrintString) {
    token_context Context = getTokenContext(Env, Record->token);

    mi_result *Result = Record->result;
    switch(Record->result_class) {
        case GDBWIRE_MI_DONE:
        case GDBWIRE_MI_RUNNING:
        case GDBWIRE_MI_CONNECTED: {
            switch(Context.Type) {
                case Context_TtySet: {
                    if(Context.Data != Nil) {
                        funcall(Env, DeleteProcess, 1, (emacs_value[]){Context.Data});
                    }
                } break;

                case Context_InitialFile: {
                    char *File = getResultString(Result, "fullname");
                    if(File) {
                        funcall(Env, SetInitialFile, 1, (emacs_value[]){getEmacsString(Env, File)});
                    }
                } break;

                case Context_BreakpointInsert: {
                    breakpointChange(Env, getResultTuple(Result, "bkpt"));
                } break;

                case Context_BreakpointEnableDisable: {
                    funcall(Env, BreakpointEnableDisable, 1, &Context.Data);
                } break;

                case Context_BreakpointDelete: {
                    funcall(Env, BreakpointDeleted, 1, (emacs_value[]){Context.Data});
                } break;

                case Context_ThreadInfo: {
                    threadInfo(Env, Result);
                } break;

                case Context_FrameInfo: {
                    frameInfo(Env, getResultList(Result, "stack"), Context.Data);
                } break;

                case Context_GetVariables: {
                    getVariables(Env, getResultList(Result, "variables"), Context.Data);
                } break;

                case Context_WatcherCreate: {
                    watcherCreate(Env, Result, Context.Data);
                } break;

                case Context_WatcherUpdate: {
                    watchersUpdate(Env, getResultList(Result, "changelist"), Context.Data);
                } break;

                case Context_WatcherListChildren: {
                    watcherAddChildren(Env, getResultList(Result, "children"), Context.Data);
                } break;

                case Context_WatcherChangeFormat: {
                    char *Value = getResultString(Result, "value");
                    funcall(Env, WatcherFormatChange, 2,
                            (emacs_value[]){Context.Data, getEmacsString(Env, Value)});
                } break;

                case Context_RegistersListNames: {
                    setRegisterNames(Env, getResultList(Result, "register-names"), Context.Data);
                } break;

                case Context_RegistersGetChanged: {
                    getChangedRegisters(Env, getResultList(Result, "changed-registers"), Context.Data);
                } break;

                case Context_RegistersUpdate: {
                    updateRegisters(Env, getResultList(Result, "register-values"), Context.Data);
                } break;

                case Context_Disassemble: {
                    disassemble(Env, getResultList(Result, "asm_insns"), Context.Data);
                } break;

                case Context_PersistThread: {
                    funcall(Env, PersistThread, 0, 0);
                } break;

                case Context_GetData: {
                    ptrdiff_t Size;
                    Env->copy_string_contents(Env, Context.Data, 0, &Size);
                    char *Key = xMalloc(Size);
                    Env->copy_string_contents(Env, Context.Data, Key, &Size);

                    char *String = getResultString(Result, Key);
                    free(Key);

                    funcall(Env, Set, 2, (emacs_value[]){GdbData, getEmacsString(Env, String)});
                } break;

                case Context_GetConsoleData: {
                    emacs_value UserPtr = funcall(Env, Eval, 1, (emacs_value[]){GdbData});
                    if(isNotNil(Env, UserPtr)) {
                        emacs_value *Array = Env->get_user_ptr(Env, UserPtr);
                        emacs_value List = funcall(Env, ListFunc, bufLen(Array), Array);
                        bufFree(Array);
                        funcall(Env, Set, 2, (emacs_value[]){GdbData, List});
                    } else {
                        funcall(Env, Set, 2, (emacs_value[]){GdbData, T});
                    }

                    funcall(Env, Set, 2, (emacs_value[]){OmitConsoleOutput, T});
                } break;

                ignoreDefaultCase();
            }
        } break;

        case GDBWIRE_MI_ERROR: {
            switch(Context.Type) {
                ignoreCase(Context_InitialFile);
                ignoreCase(Context_IgnoreErrors);

                case Context_GetData: {
                    emacs_value UserPtr = funcall(Env, Eval, 1, (emacs_value[]){GdbData});
                    if(isNotNil(Env, UserPtr)) {
                        emacs_value *Array = Env->get_user_ptr(Env, UserPtr);
                        bufFree(Array);
                    }
                    funcall(Env, Set, 2, (emacs_value[]){GdbData, Error});
                    funcall(Env, Set, 2, (emacs_value[]){OmitConsoleOutput, T});
                    logError(Env, Result, PrintString, "msg");
                } break;

                case Context_GetConsoleData: {
                    funcall(Env, Set, 2, (emacs_value[]){GdbData, Error});
                    logError(Env, Result, PrintString, "msg");
                } break;

                default: { logError(Env, Result, PrintString, "msg"); } break;
            }
        } break;

        case GDBWIRE_MI_EXIT: {
            bufPrintf(*PrintString, "Bye bye :)\n");
        } break;

        case GDBWIRE_MI_UNSUPPORTED: {
            bufPrintf(*PrintString, "Error while parsing, GdbWire couldn't figure out class of result record!");
        } break;
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

    char *PrintString = 0;
    for(mi_output *Output = ParserOutput; Output; Output = Output->next) {
        switch(Output->kind) {
            case GDBWIRE_MI_OUTPUT_OOB: {
                handleMiOobRecord(Env, Output->variant.oob_record, &PrintString);
            } break;

            case GDBWIRE_MI_OUTPUT_RESULT: {
                handleMiResultRecord(Env, Output->variant.result_record, &PrintString);
            } break;

            case GDBWIRE_MI_OUTPUT_PARSE_ERROR: {
                bufPrintf(PrintString, "An error occurred on token %s\n", Output->variant.error.token);
            } break;

            case GDBWIRE_MI_OUTPUT_PROMPT: {
                if(isNil(Env, funcall(Env, Eval, 1, (emacs_value[]){OmitConsoleOutput}))) {
                    bufPrintf(PrintString, "(gdb) ");
                    funcall(Env, Set, 2, (emacs_value[]){OmitConsoleOutput, T});
                }
            } break;
        }
    }
    gdbwire_mi_output_free(ParserOutput);
    ParserOutput = 0;

    emacs_value Result = Env->make_string(Env, PrintString, bufLen(PrintString));
    bufFree(PrintString);

    return Result;
}

s32 emacs_module_init(emacs_runtime *EmacsRuntime) {
    emacs_env *Env = EmacsRuntime->get_environment(EmacsRuntime);

    interns(internSetterWriter);

    emacs_value Args[2];
    Args[0] = intern(Env, HANDLE_GDB_MI_OUTPUT_NAME);
    Args[1] = Env->make_function(Env, 1, 1, handleGdbMiOutput, HANDLE_GDB_MI_OUTPUT_DOC, 0);
    internFuncall(Env, "defalias", 2, Args);

    gdbwire_callbacks Callbacks = {0, gdbWireCallback};
    GdbMiParser = gdbwire_mi_parser_create(Callbacks);

    emacs_value Feature = intern(Env, "gdb-module");
    internFuncall(Env, "provide", 1, &Feature);
    return 0;
}
