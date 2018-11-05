; ModuleID = 'Shoo'
source_filename = "Shoo"

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@str = private unnamed_addr constant [12 x i8] c"Hello World\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %env = bitcast i8* %malloccall to i8**
  store i8* null, i8** %env
  %env_p = load i8*, i8** %env
  %malloccall1 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %main = bitcast i8* %malloccall1 to { i32 ()*, i8* }*
  %tmp__ = insertvalue { i32 ()*, i8* } { i32 ()* @main, i8* null }, i8* %env_p, 1
  store { i32 ()*, i8* } %tmp__, { i32 ()*, i8* }* %main
  %malloccall2 = tail call i8* @malloc(i32 0)
  %tmp_ = bitcast i8* %malloccall2 to {}*
  store {} zeroinitializer, {}* %tmp_
  %env_p3 = bitcast {}* %tmp_ to i8*
  %tmp__4 = insertvalue { i8* (i8*)*, i8* } { i8* (i8*)* @f0, i8* null }, i8* %env_p3, 1
  %malloccall5 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %foo = bitcast i8* %malloccall5 to { i8* (i8*)*, i8* }*
  store { i8* (i8*)*, i8* } %tmp__4, { i8* (i8*)*, i8* }* %foo
  %clsr_val = load { i8* (i8*)*, i8* }, { i8* (i8*)*, i8* }* %foo
  %fp = extractvalue { i8* (i8*)*, i8* } %clsr_val, 0
  %envp = extractvalue { i8* (i8*)*, i8* } %clsr_val, 1
  %foo_result = call i8* %fp(i8* %envp)
  %0 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* %foo_result)
  ret i32 0
}

define i8* @f0(i8* %env) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i32))
  %env1 = bitcast i8* %malloccall to i8**
  store i8* %env, i8** %env1
  %env_p = load i8*, i8** %env1
  %malloccall2 = tail call i8* @malloc(i32 trunc (i64 mul nuw (i64 ptrtoint (i1** getelementptr (i1*, i1** null, i32 1) to i64), i64 2) to i32))
  %foo = bitcast i8* %malloccall2 to { i8* (i8*)*, i8* }*
  %tmp__ = insertvalue { i8* (i8*)*, i8* } { i8* (i8*)* @f0, i8* null }, i8* %env_p, 1
  store { i8* (i8*)*, i8* } %tmp__, { i8* (i8*)*, i8* }* %foo
  ret i8* getelementptr inbounds ([12 x i8], [12 x i8]* @str, i32 0, i32 0)
}

declare noalias i8* @malloc(i32)
