; ModuleID = 'header.cpp'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%"class.std::ios_base::Init" = type { i8 }
%struct.hash_tbl = type { i64, i64, %struct.UT_hash_handle }
%struct.UT_hash_handle = type { %struct.UT_hash_table*, i8*, i8*, %struct.UT_hash_handle*, %struct.UT_hash_handle*, i8*, i32, i32 }
%struct.UT_hash_table = type { %struct.UT_hash_bucket*, i32, i32, i32, %struct.UT_hash_handle*, i64, i32, i32, i32, i32, i32 }
%struct.UT_hash_bucket = type { %struct.UT_hash_handle*, i32, i32 }

@_ZStL8__ioinit = internal global %"class.std::ios_base::Init" zeroinitializer, align 1
@__dso_handle = external global i8
@memory_cap = global i64 0, align 8
@.str = private unnamed_addr constant [25 x i8] c"library run-time error: \00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.2 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.3 = private unnamed_addr constant [28 x i8] c"Memory cap exceeded 256 mb.\00", align 1
@.str.4 = private unnamed_addr constant [117 x i8] c"One of the variables was given a value that was larger than the maximum value of an int causing an integer overflow.\00", align 1
@.str.5 = private unnamed_addr constant [5 x i8] c"%lu\0A\00", align 1
@.str.6 = private unnamed_addr constant [68 x i8] c"Expected value: null (in expect_args0). Prim cannot take arguments.\00", align 1
@.str.7 = private unnamed_addr constant [79 x i8] c"Expected cons value (in expect_args1). Prim applied on an empty argument list.\00", align 1
@.str.8 = private unnamed_addr constant [70 x i8] c"Expected null value (in expect_args1). Prim can only take 1 argument.\00", align 1
@.str.9 = private unnamed_addr constant [37 x i8] c"Expected a cons value. (expect_cons)\00", align 1
@.str.10 = private unnamed_addr constant [51 x i8] c"Expected a vector or special value. (expect_other)\00", align 1
@.str.11 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.12 = private unnamed_addr constant [13 x i8] c"#<procedure>\00", align 1
@.str.13 = private unnamed_addr constant [2 x i8] c"(\00", align 1
@.str.14 = private unnamed_addr constant [4 x i8] c" . \00", align 1
@.str.15 = private unnamed_addr constant [2 x i8] c")\00", align 1
@.str.16 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.17 = private unnamed_addr constant [5 x i8] c"\22%s\22\00", align 1
@.str.18 = private unnamed_addr constant [3 x i8] c"#(\00", align 1
@.str.19 = private unnamed_addr constant [2 x i8] c",\00", align 1
@.str.20 = private unnamed_addr constant [5 x i8] c"HERE\00", align 1
@.str.21 = private unnamed_addr constant [36 x i8] c"(print.. v); unrecognized value %lu\00", align 1
@.str.22 = private unnamed_addr constant [4 x i8] c"'()\00", align 1
@.str.23 = private unnamed_addr constant [3 x i8] c"'(\00", align 1
@.str.24 = private unnamed_addr constant [4 x i8] c"'%s\00", align 1
@.str.25 = private unnamed_addr constant [7 x i8] c"#hash(\00", align 1
@.str.26 = private unnamed_addr constant [10 x i8] c"(%d . %d)\00", align 1
@.str.27 = private unnamed_addr constant [11 x i8] c"(%d . %d) \00", align 1
@.str.28 = private unnamed_addr constant [34 x i8] c"(print v); unrecognized value %lu\00", align 1
@.str.29 = private unnamed_addr constant [49 x i8] c"first argument to make-vector must be an integer\00", align 1
@.str.30 = private unnamed_addr constant [39 x i8] c"prim applied on more than 2 arguments.\00", align 1
@.str.31 = private unnamed_addr constant [49 x i8] c"second argument to vector-ref must be an integer\00", align 1
@.str.32 = private unnamed_addr constant [46 x i8] c"first argument to vector-ref must be a vector\00", align 1
@.str.33 = private unnamed_addr constant [46 x i8] c"vector-ref not given a properly formed vector\00", align 1
@.str.34 = private unnamed_addr constant [31 x i8] c"illegal reference of a vector.\00", align 1
@.str.35 = private unnamed_addr constant [48 x i8] c"first argument to vector-ref must be an integer\00", align 1
@.str.36 = private unnamed_addr constant [34 x i8] c"(prim + a b); a is not an integer\00", align 1
@.str.37 = private unnamed_addr constant [34 x i8] c"(prim + a b); b is not an integer\00", align 1
@.str.38 = private unnamed_addr constant [36 x i8] c"Tried to apply + on non list value.\00", align 1
@.str.39 = private unnamed_addr constant [34 x i8] c"(prim - a b); b is not an integer\00", align 1
@.str.40 = private unnamed_addr constant [34 x i8] c"(prim * a b); a is not an integer\00", align 1
@.str.41 = private unnamed_addr constant [34 x i8] c"(prim * a b); b is not an integer\00", align 1
@.str.42 = private unnamed_addr constant [34 x i8] c"(prim / a b); a is not an integer\00", align 1
@.str.43 = private unnamed_addr constant [34 x i8] c"(prim / a b); b is not an integer\00", align 1
@.str.44 = private unnamed_addr constant [41 x i8] c"Division by 0 error. Cannot divide by 0.\00", align 1
@.str.45 = private unnamed_addr constant [34 x i8] c"(prim = a b); a is not an integer\00", align 1
@.str.46 = private unnamed_addr constant [34 x i8] c"(prim = a b); b is not an integer\00", align 1
@.str.47 = private unnamed_addr constant [34 x i8] c"(prim < a b); a is not an integer\00", align 1
@.str.48 = private unnamed_addr constant [34 x i8] c"(prim < a b); b is not an integer\00", align 1
@.str.49 = private unnamed_addr constant [35 x i8] c"(prim <= a b); a is not an integer\00", align 1
@.str.50 = private unnamed_addr constant [35 x i8] c"(prim <= a b); b is not an integer\00", align 1
@.str.51 = private unnamed_addr constant [17 x i8] c"not given a list\00", align 1
@.str.52 = private unnamed_addr constant [18 x i8] c"Not given a hash.\00", align 1
@.str.53 = private unnamed_addr constant [47 x i8] c"Given a key that is not found within the hash.\00", align 1
@llvm.global_ctors = appending global [1 x { i32, void ()*, i8* }] [{ i32, void ()*, i8* } { i32 65535, void ()* @_GLOBAL__sub_I_header.cpp, i8* null }]

; Function Attrs: uwtable
define internal void @__cxx_global_var_init() #0 section ".text.startup" {
  call void @_ZNSt8ios_base4InitC1Ev(%"class.std::ios_base::Init"* @_ZStL8__ioinit)
  %1 = call i32 @__cxa_atexit(void (i8*)* bitcast (void (%"class.std::ios_base::Init"*)* @_ZNSt8ios_base4InitD1Ev to void (i8*)*), i8* getelementptr inbounds (%"class.std::ios_base::Init", %"class.std::ios_base::Init"* @_ZStL8__ioinit, i32 0, i32 0), i8* @__dso_handle) #2
  ret void
}

declare void @_ZNSt8ios_base4InitC1Ev(%"class.std::ios_base::Init"*) #1

declare void @_ZNSt8ios_base4InitD1Ev(%"class.std::ios_base::Init"*) #1

; Function Attrs: nounwind
declare i32 @__cxa_atexit(void (i8*)*, i8*, i8*) #2

; Function Attrs: uwtable
define void @fatal_err(i8* %msg) #0 {
  %1 = alloca i8*, align 8
  store i8* %msg, i8** %1, align 8
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str, i32 0, i32 0))
  %3 = load i8*, i8** %1, align 8
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0), i8* %3)
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 1) #9
  unreachable
                                                  ; No predecessors!
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noreturn nounwind
declare void @exit(i32) #3

; Function Attrs: uwtable
define i64* @alloc(i64 %m) #0 {
  %1 = alloca i64, align 8
  store i64 %m, i64* %1, align 8
  %2 = load i64, i64* @memory_cap, align 8
  %3 = load i64, i64* %1, align 8
  %4 = add i64 %2, %3
  store i64 %4, i64* @memory_cap, align 8
  %5 = load i64, i64* @memory_cap, align 8
  %6 = icmp ugt i64 %5, 268435456
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([28 x i8], [28 x i8]* @.str.3, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %1, align 8
  %10 = call noalias i8* @malloc(i64 %9) #2
  %11 = bitcast i8* %10 to i64*
  ret i64* %11
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #4

; Function Attrs: uwtable
define void @check_size(i64 %val) #0 {
  %1 = alloca i64, align 8
  store i64 %val, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = and i64 %2, -8
  %4 = lshr i64 %3, 32
  %5 = trunc i64 %4 to i32
  %6 = icmp sgt i32 %5, 2147483647
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([117 x i8], [117 x i8]* @.str.4, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  ret void
}

; Function Attrs: uwtable
define void @print_u64(i64 %i) #0 {
  %1 = alloca i64, align 8
  store i64 %i, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.5, i32 0, i32 0), i64 %2)
  ret void
}

; Function Attrs: uwtable
define i64 @expect_args0(i64 %args) #0 {
  %1 = alloca i64, align 8
  store i64 %args, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = icmp ne i64 %2, 0
  br i1 %3, label %4, label %5

; <label>:4                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([68 x i8], [68 x i8]* @.str.6, i32 0, i32 0))
  br label %5

; <label>:5                                       ; preds = %4, %0
  ret i64 0
}

; Function Attrs: uwtable
define i64 @expect_args1(i64 %args) #0 {
  %1 = alloca i64, align 8
  %p = alloca i64*, align 8
  store i64 %args, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = and i64 %2, 7
  %4 = icmp ne i64 %3, 1
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([79 x i8], [79 x i8]* @.str.7, i32 0, i32 0))
  br label %6

; <label>:6                                       ; preds = %5, %0
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, -8
  %9 = inttoptr i64 %8 to i64*
  store i64* %9, i64** %p, align 8
  %10 = load i64*, i64** %p, align 8
  %11 = getelementptr inbounds i64, i64* %10, i64 1
  %12 = load i64, i64* %11, align 8
  %13 = icmp ne i64 %12, 0
  br i1 %13, label %14, label %15

; <label>:14                                      ; preds = %6
  call void @fatal_err(i8* getelementptr inbounds ([70 x i8], [70 x i8]* @.str.8, i32 0, i32 0))
  br label %15

; <label>:15                                      ; preds = %14, %6
  %16 = load i64*, i64** %p, align 8
  %17 = getelementptr inbounds i64, i64* %16, i64 0
  %18 = load i64, i64* %17, align 8
  ret i64 %18
}

; Function Attrs: uwtable
define i64 @expect_cons(i64 %p, i64* %rest) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64*, align 8
  %pp = alloca i64*, align 8
  store i64 %p, i64* %1, align 8
  store i64* %rest, i64** %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 1
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.9, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %1, align 8
  %9 = and i64 %8, -8
  %10 = inttoptr i64 %9 to i64*
  store i64* %10, i64** %pp, align 8
  %11 = load i64*, i64** %pp, align 8
  %12 = getelementptr inbounds i64, i64* %11, i64 1
  %13 = load i64, i64* %12, align 8
  %14 = load i64*, i64** %2, align 8
  store i64 %13, i64* %14, align 8
  %15 = load i64*, i64** %pp, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 0
  %17 = load i64, i64* %16, align 8
  ret i64 %17
}

; Function Attrs: uwtable
define i64 @expect_other(i64 %v, i64* %rest) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64*, align 8
  %p = alloca i64*, align 8
  store i64 %v, i64* %1, align 8
  store i64* %rest, i64** %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 6
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([51 x i8], [51 x i8]* @.str.10, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %1, align 8
  %9 = and i64 %8, -8
  %10 = inttoptr i64 %9 to i64*
  store i64* %10, i64** %p, align 8
  %11 = load i64*, i64** %p, align 8
  %12 = getelementptr inbounds i64, i64* %11, i64 1
  %13 = load i64, i64* %12, align 8
  %14 = load i64*, i64** %2, align 8
  store i64 %13, i64* %14, align 8
  %15 = load i64*, i64** %p, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 0
  %17 = load i64, i64* %16, align 8
  ret i64 %17
}

; Function Attrs: nounwind uwtable
define i64 @const_init_int(i64 %i) #5 {
  %1 = alloca i64, align 8
  store i64 %i, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = trunc i64 %2 to i32
  %4 = zext i32 %3 to i64
  %5 = shl i64 %4, 32
  %6 = or i64 %5, 2
  ret i64 %6
}

; Function Attrs: nounwind uwtable
define i64 @const_init_void() #5 {
  ret i64 39
}

; Function Attrs: nounwind uwtable
define i64 @const_init_null() #5 {
  ret i64 0
}

; Function Attrs: nounwind uwtable
define i64 @const_init_true() #5 {
  ret i64 31
}

; Function Attrs: nounwind uwtable
define i64 @const_init_false() #5 {
  ret i64 15
}

; Function Attrs: nounwind uwtable
define i64 @const_init_string(i8* %s) #5 {
  %1 = alloca i8*, align 8
  store i8* %s, i8** %1, align 8
  %2 = load i8*, i8** %1, align 8
  %3 = ptrtoint i8* %2 to i64
  %4 = or i64 %3, 3
  ret i64 %4
}

; Function Attrs: nounwind uwtable
define i64 @const_init_symbol(i8* %s) #5 {
  %1 = alloca i8*, align 8
  store i8* %s, i8** %1, align 8
  %2 = load i8*, i8** %1, align 8
  %3 = ptrtoint i8* %2 to i64
  %4 = or i64 %3, 4
  ret i64 %4
}

; Function Attrs: uwtable
define i64 @prim_print_aux(i64 %v) #0 {
  %1 = alloca i64, align 8
  %p = alloca i64*, align 8
  %vec = alloca i64*, align 8
  %len = alloca i64, align 8
  %i = alloca i64, align 8
  store i64 %v, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = icmp eq i64 %2, 0
  br i1 %3, label %4, label %6

; <label>:4                                       ; preds = %0
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.11, i32 0, i32 0))
  br label %122

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 7
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %10, label %12

; <label>:10                                      ; preds = %6
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.12, i32 0, i32 0))
  br label %121

; <label>:12                                      ; preds = %6
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, 7
  %15 = icmp eq i64 %14, 1
  br i1 %15, label %16, label %31

; <label>:16                                      ; preds = %12
  %17 = load i64, i64* %1, align 8
  %18 = and i64 %17, -8
  %19 = inttoptr i64 %18 to i64*
  store i64* %19, i64** %p, align 8
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.13, i32 0, i32 0))
  %21 = load i64*, i64** %p, align 8
  %22 = getelementptr inbounds i64, i64* %21, i64 0
  %23 = load i64, i64* %22, align 8
  %24 = call i64 @prim_print_aux(i64 %23)
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.14, i32 0, i32 0))
  %26 = load i64*, i64** %p, align 8
  %27 = getelementptr inbounds i64, i64* %26, i64 1
  %28 = load i64, i64* %27, align 8
  %29 = call i64 @prim_print_aux(i64 %28)
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.15, i32 0, i32 0))
  br label %120

; <label>:31                                      ; preds = %12
  %32 = load i64, i64* %1, align 8
  %33 = and i64 %32, 7
  %34 = icmp eq i64 %33, 2
  br i1 %34, label %35, label %40

; <label>:35                                      ; preds = %31
  %36 = load i64, i64* %1, align 8
  %37 = lshr i64 %36, 32
  %38 = trunc i64 %37 to i32
  %39 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.16, i32 0, i32 0), i32 %38)
  br label %119

; <label>:40                                      ; preds = %31
  %41 = load i64, i64* %1, align 8
  %42 = and i64 %41, 7
  %43 = icmp eq i64 %42, 3
  br i1 %43, label %44, label %49

; <label>:44                                      ; preds = %40
  %45 = load i64, i64* %1, align 8
  %46 = and i64 %45, -8
  %47 = inttoptr i64 %46 to i8*
  %48 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.17, i32 0, i32 0), i8* %47)
  br label %118

; <label>:49                                      ; preds = %40
  %50 = load i64, i64* %1, align 8
  %51 = and i64 %50, 7
  %52 = icmp eq i64 %51, 4
  br i1 %52, label %53, label %58

; <label>:53                                      ; preds = %49
  %54 = load i64, i64* %1, align 8
  %55 = and i64 %54, -8
  %56 = inttoptr i64 %55 to i8*
  %57 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0), i8* %56)
  br label %117

; <label>:58                                      ; preds = %49
  %59 = load i64, i64* %1, align 8
  %60 = and i64 %59, 7
  %61 = icmp eq i64 %60, 6
  br i1 %61, label %62, label %99

; <label>:62                                      ; preds = %58
  %63 = load i64, i64* %1, align 8
  %64 = and i64 %63, -8
  %65 = inttoptr i64 %64 to i64*
  %66 = getelementptr inbounds i64, i64* %65, i64 0
  %67 = load i64, i64* %66, align 8
  %68 = and i64 %67, 7
  %69 = icmp eq i64 1, %68
  br i1 %69, label %70, label %99

; <label>:70                                      ; preds = %62
  %71 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.18, i32 0, i32 0))
  %72 = load i64, i64* %1, align 8
  %73 = and i64 %72, -8
  %74 = inttoptr i64 %73 to i64*
  store i64* %74, i64** %vec, align 8
  %75 = load i64*, i64** %vec, align 8
  %76 = getelementptr inbounds i64, i64* %75, i64 0
  %77 = load i64, i64* %76, align 8
  %78 = lshr i64 %77, 3
  store i64 %78, i64* %len, align 8
  %79 = load i64*, i64** %vec, align 8
  %80 = getelementptr inbounds i64, i64* %79, i64 1
  %81 = load i64, i64* %80, align 8
  %82 = call i64 @prim_print_aux(i64 %81)
  store i64 2, i64* %i, align 8
  br label %83

; <label>:83                                      ; preds = %94, %70
  %84 = load i64, i64* %i, align 8
  %85 = load i64, i64* %len, align 8
  %86 = icmp ule i64 %84, %85
  br i1 %86, label %87, label %97

; <label>:87                                      ; preds = %83
  %88 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.19, i32 0, i32 0))
  %89 = load i64, i64* %i, align 8
  %90 = load i64*, i64** %vec, align 8
  %91 = getelementptr inbounds i64, i64* %90, i64 %89
  %92 = load i64, i64* %91, align 8
  %93 = call i64 @prim_print_aux(i64 %92)
  br label %94

; <label>:94                                      ; preds = %87
  %95 = load i64, i64* %i, align 8
  %96 = add i64 %95, 1
  store i64 %96, i64* %i, align 8
  br label %83

; <label>:97                                      ; preds = %83
  %98 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.15, i32 0, i32 0))
  br label %116

; <label>:99                                      ; preds = %62, %58
  %100 = load i64, i64* %1, align 8
  %101 = and i64 %100, 7
  %102 = icmp eq i64 %101, 6
  br i1 %102, label %103, label %112

; <label>:103                                     ; preds = %99
  %104 = load i64, i64* %1, align 8
  %105 = and i64 %104, -8
  %106 = inttoptr i64 %105 to i64*
  %107 = getelementptr inbounds i64, i64* %106, i64 0
  %108 = load i64, i64* %107, align 8
  %109 = icmp eq i64 2, %108
  br i1 %109, label %110, label %112

; <label>:110                                     ; preds = %103
  %111 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.20, i32 0, i32 0))
  br label %115

; <label>:112                                     ; preds = %103, %99
  %113 = load i64, i64* %1, align 8
  %114 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.21, i32 0, i32 0), i64 %113)
  br label %115

; <label>:115                                     ; preds = %112, %110
  br label %116

; <label>:116                                     ; preds = %115, %97
  br label %117

; <label>:117                                     ; preds = %116, %53
  br label %118

; <label>:118                                     ; preds = %117, %44
  br label %119

; <label>:119                                     ; preds = %118, %35
  br label %120

; <label>:120                                     ; preds = %119, %16
  br label %121

; <label>:121                                     ; preds = %120, %10
  br label %122

; <label>:122                                     ; preds = %121, %4
  ret i64 39
}

; Function Attrs: uwtable
define i64 @prim_print(i64 %v) #0 {
  %1 = alloca i64, align 8
  %p = alloca i64*, align 8
  %vec = alloca i64*, align 8
  %len = alloca i64, align 8
  %i = alloca i64, align 8
  %vec1 = alloca i64*, align 8
  %ptr_to_hash = alloca i64*, align 8
  %tbls = alloca %struct.hash_tbl*, align 8
  %tbl = alloca %struct.hash_tbl*, align 8
  store i64 %v, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = icmp eq i64 %2, 0
  br i1 %3, label %4, label %6

; <label>:4                                       ; preds = %0
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.22, i32 0, i32 0))
  br label %179

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 7
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %10, label %12

; <label>:10                                      ; preds = %6
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.12, i32 0, i32 0))
  br label %178

; <label>:12                                      ; preds = %6
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, 7
  %15 = icmp eq i64 %14, 1
  br i1 %15, label %16, label %31

; <label>:16                                      ; preds = %12
  %17 = load i64, i64* %1, align 8
  %18 = and i64 %17, -8
  %19 = inttoptr i64 %18 to i64*
  store i64* %19, i64** %p, align 8
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.23, i32 0, i32 0))
  %21 = load i64*, i64** %p, align 8
  %22 = getelementptr inbounds i64, i64* %21, i64 0
  %23 = load i64, i64* %22, align 8
  %24 = call i64 @prim_print_aux(i64 %23)
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.14, i32 0, i32 0))
  %26 = load i64*, i64** %p, align 8
  %27 = getelementptr inbounds i64, i64* %26, i64 1
  %28 = load i64, i64* %27, align 8
  %29 = call i64 @prim_print_aux(i64 %28)
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.15, i32 0, i32 0))
  br label %177

; <label>:31                                      ; preds = %12
  %32 = load i64, i64* %1, align 8
  %33 = and i64 %32, 7
  %34 = icmp eq i64 %33, 2
  br i1 %34, label %35, label %40

; <label>:35                                      ; preds = %31
  %36 = load i64, i64* %1, align 8
  %37 = lshr i64 %36, 32
  %38 = trunc i64 %37 to i32
  %39 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.16, i32 0, i32 0), i32 %38)
  br label %176

; <label>:40                                      ; preds = %31
  %41 = load i64, i64* %1, align 8
  %42 = and i64 %41, 7
  %43 = icmp eq i64 %42, 3
  br i1 %43, label %44, label %49

; <label>:44                                      ; preds = %40
  %45 = load i64, i64* %1, align 8
  %46 = and i64 %45, -8
  %47 = inttoptr i64 %46 to i8*
  %48 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.17, i32 0, i32 0), i8* %47)
  br label %175

; <label>:49                                      ; preds = %40
  %50 = load i64, i64* %1, align 8
  %51 = and i64 %50, 7
  %52 = icmp eq i64 %51, 4
  br i1 %52, label %53, label %58

; <label>:53                                      ; preds = %49
  %54 = load i64, i64* %1, align 8
  %55 = and i64 %54, -8
  %56 = inttoptr i64 %55 to i8*
  %57 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.24, i32 0, i32 0), i8* %56)
  br label %174

; <label>:58                                      ; preds = %49
  %59 = load i64, i64* %1, align 8
  %60 = and i64 %59, 7
  %61 = icmp eq i64 %60, 6
  br i1 %61, label %62, label %99

; <label>:62                                      ; preds = %58
  %63 = load i64, i64* %1, align 8
  %64 = and i64 %63, -8
  %65 = inttoptr i64 %64 to i64*
  %66 = getelementptr inbounds i64, i64* %65, i64 0
  %67 = load i64, i64* %66, align 8
  %68 = and i64 %67, 7
  %69 = icmp eq i64 1, %68
  br i1 %69, label %70, label %99

; <label>:70                                      ; preds = %62
  %71 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.18, i32 0, i32 0))
  %72 = load i64, i64* %1, align 8
  %73 = and i64 %72, -8
  %74 = inttoptr i64 %73 to i64*
  store i64* %74, i64** %vec, align 8
  %75 = load i64*, i64** %vec, align 8
  %76 = getelementptr inbounds i64, i64* %75, i64 0
  %77 = load i64, i64* %76, align 8
  %78 = lshr i64 %77, 3
  store i64 %78, i64* %len, align 8
  %79 = load i64*, i64** %vec, align 8
  %80 = getelementptr inbounds i64, i64* %79, i64 1
  %81 = load i64, i64* %80, align 8
  %82 = call i64 @prim_print(i64 %81)
  store i64 2, i64* %i, align 8
  br label %83

; <label>:83                                      ; preds = %94, %70
  %84 = load i64, i64* %i, align 8
  %85 = load i64, i64* %len, align 8
  %86 = icmp ule i64 %84, %85
  br i1 %86, label %87, label %97

; <label>:87                                      ; preds = %83
  %88 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.19, i32 0, i32 0))
  %89 = load i64, i64* %i, align 8
  %90 = load i64*, i64** %vec, align 8
  %91 = getelementptr inbounds i64, i64* %90, i64 %89
  %92 = load i64, i64* %91, align 8
  %93 = call i64 @prim_print(i64 %92)
  br label %94

; <label>:94                                      ; preds = %87
  %95 = load i64, i64* %i, align 8
  %96 = add i64 %95, 1
  store i64 %96, i64* %i, align 8
  br label %83

; <label>:97                                      ; preds = %83
  %98 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.15, i32 0, i32 0))
  br label %173

; <label>:99                                      ; preds = %62, %58
  %100 = load i64, i64* %1, align 8
  %101 = and i64 %100, 7
  %102 = icmp eq i64 %101, 6
  br i1 %102, label %103, label %169

; <label>:103                                     ; preds = %99
  %104 = load i64, i64* %1, align 8
  %105 = and i64 %104, -8
  %106 = inttoptr i64 %105 to i64*
  %107 = getelementptr inbounds i64, i64* %106, i64 0
  %108 = load i64, i64* %107, align 8
  %109 = icmp eq i64 2, %108
  br i1 %109, label %110, label %169

; <label>:110                                     ; preds = %103
  %111 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.25, i32 0, i32 0))
  %112 = load i64, i64* %1, align 8
  %113 = and i64 %112, -8
  %114 = inttoptr i64 %113 to i64*
  store i64* %114, i64** %vec1, align 8
  %115 = load i64*, i64** %vec1, align 8
  %116 = getelementptr inbounds i64, i64* %115, i64 1
  %117 = load i64, i64* %116, align 8
  %118 = and i64 %117, -8
  %119 = inttoptr i64 %118 to i64*
  store i64* %119, i64** %ptr_to_hash, align 8
  %120 = load i64*, i64** %ptr_to_hash, align 8
  %121 = bitcast i64* %120 to %struct.hash_tbl*
  store %struct.hash_tbl* %121, %struct.hash_tbl** %tbls, align 8
  %122 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  store %struct.hash_tbl* %122, %struct.hash_tbl** %tbl, align 8
  br label %123

; <label>:123                                     ; preds = %161, %110
  %124 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %125 = icmp ne %struct.hash_tbl* %124, null
  br i1 %125, label %126, label %167

; <label>:126                                     ; preds = %123
  %127 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %128 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %127, i32 0, i32 2
  %129 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %128, i32 0, i32 2
  %130 = load i8*, i8** %129, align 8
  %131 = icmp eq i8* %130, null
  br i1 %131, label %132, label %146

; <label>:132                                     ; preds = %126
  %133 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %134 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %133, i32 0, i32 0
  %135 = load i64, i64* %134, align 8
  %136 = and i64 %135, -8
  %137 = lshr i64 %136, 32
  %138 = trunc i64 %137 to i32
  %139 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %140 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %139, i32 0, i32 1
  %141 = load i64, i64* %140, align 8
  %142 = and i64 %141, -8
  %143 = lshr i64 %142, 32
  %144 = trunc i64 %143 to i32
  %145 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.26, i32 0, i32 0), i32 %138, i32 %144)
  br label %160

; <label>:146                                     ; preds = %126
  %147 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %148 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %147, i32 0, i32 0
  %149 = load i64, i64* %148, align 8
  %150 = and i64 %149, -8
  %151 = lshr i64 %150, 32
  %152 = trunc i64 %151 to i32
  %153 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %154 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %153, i32 0, i32 1
  %155 = load i64, i64* %154, align 8
  %156 = and i64 %155, -8
  %157 = lshr i64 %156, 32
  %158 = trunc i64 %157 to i32
  %159 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.27, i32 0, i32 0), i32 %152, i32 %158)
  br label %160

; <label>:160                                     ; preds = %146, %132
  br label %161

; <label>:161                                     ; preds = %160
  %162 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %163 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %162, i32 0, i32 2
  %164 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %163, i32 0, i32 2
  %165 = load i8*, i8** %164, align 8
  %166 = bitcast i8* %165 to %struct.hash_tbl*
  store %struct.hash_tbl* %166, %struct.hash_tbl** %tbl, align 8
  br label %123

; <label>:167                                     ; preds = %123
  %168 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.15, i32 0, i32 0))
  br label %172

; <label>:169                                     ; preds = %103, %99
  %170 = load i64, i64* %1, align 8
  %171 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.28, i32 0, i32 0), i64 %170)
  br label %172

; <label>:172                                     ; preds = %169, %167
  br label %173

; <label>:173                                     ; preds = %172, %97
  br label %174

; <label>:174                                     ; preds = %173, %53
  br label %175

; <label>:175                                     ; preds = %174, %44
  br label %176

; <label>:176                                     ; preds = %175, %35
  br label %177

; <label>:177                                     ; preds = %176, %16
  br label %178

; <label>:178                                     ; preds = %177, %10
  br label %179

; <label>:179                                     ; preds = %178, %4
  ret i64 39
}

; Function Attrs: uwtable
define i64 @applyprim_print(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_print(i64 %4)
  ret i64 %5
}

; Function Attrs: uwtable
define i64 @prim_halt(i64 %v) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %v, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = call i64 @prim_print(i64 %3)
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 0) #9
  unreachable
                                                  ; No predecessors!
  %7 = load i64, i64* %1, align 8
  ret i64 %7
}

; Function Attrs: uwtable
define i64 @applyprim_vector(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %buffer = alloca i64*, align 8
  %l = alloca i64, align 8
  %mem = alloca i64*, align 8
  %i = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* @memory_cap, align 8
  %3 = add i64 %2, 4096
  store i64 %3, i64* @memory_cap, align 8
  %4 = load i64, i64* @memory_cap, align 8
  %5 = icmp ugt i64 %4, 268435456
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([28 x i8], [28 x i8]* @.str.3, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = call noalias i8* @malloc(i64 4096) #2
  %9 = bitcast i8* %8 to i64*
  store i64* %9, i64** %buffer, align 8
  store i64 0, i64* %l, align 8
  br label %10

; <label>:10                                      ; preds = %19, %7
  %11 = load i64, i64* %1, align 8
  %12 = and i64 %11, 7
  %13 = icmp eq i64 %12, 1
  br i1 %13, label %14, label %17

; <label>:14                                      ; preds = %10
  %15 = load i64, i64* %l, align 8
  %16 = icmp ult i64 %15, 512
  br label %17

; <label>:17                                      ; preds = %14, %10
  %18 = phi i1 [ false, %10 ], [ %16, %14 ]
  br i1 %18, label %19, label %26

; <label>:19                                      ; preds = %17
  %20 = load i64, i64* %1, align 8
  %21 = call i64 @expect_cons(i64 %20, i64* %1)
  %22 = load i64, i64* %l, align 8
  %23 = add i64 %22, 1
  store i64 %23, i64* %l, align 8
  %24 = load i64*, i64** %buffer, align 8
  %25 = getelementptr inbounds i64, i64* %24, i64 %22
  store i64 %21, i64* %25, align 8
  br label %10

; <label>:26                                      ; preds = %17
  %27 = load i64, i64* %l, align 8
  %28 = add i64 %27, 1
  %29 = mul i64 %28, 8
  %30 = call i64* @alloc(i64 %29)
  store i64* %30, i64** %mem, align 8
  %31 = load i64, i64* %l, align 8
  %32 = shl i64 %31, 3
  %33 = or i64 %32, 1
  %34 = load i64*, i64** %mem, align 8
  %35 = getelementptr inbounds i64, i64* %34, i64 0
  store i64 %33, i64* %35, align 8
  store i64 0, i64* %i, align 8
  br label %36

; <label>:36                                      ; preds = %49, %26
  %37 = load i64, i64* %i, align 8
  %38 = load i64, i64* %l, align 8
  %39 = icmp ult i64 %37, %38
  br i1 %39, label %40, label %52

; <label>:40                                      ; preds = %36
  %41 = load i64, i64* %i, align 8
  %42 = load i64*, i64** %buffer, align 8
  %43 = getelementptr inbounds i64, i64* %42, i64 %41
  %44 = load i64, i64* %43, align 8
  %45 = load i64, i64* %i, align 8
  %46 = add i64 %45, 1
  %47 = load i64*, i64** %mem, align 8
  %48 = getelementptr inbounds i64, i64* %47, i64 %46
  store i64 %44, i64* %48, align 8
  br label %49

; <label>:49                                      ; preds = %40
  %50 = load i64, i64* %i, align 8
  %51 = add i64 %50, 1
  store i64 %51, i64* %i, align 8
  br label %36

; <label>:52                                      ; preds = %36
  %53 = load i64*, i64** %buffer, align 8
  %54 = icmp eq i64* %53, null
  br i1 %54, label %57, label %55

; <label>:55                                      ; preds = %52
  %56 = bitcast i64* %53 to i8*
  call void @_ZdaPv(i8* %56) #10
  br label %57

; <label>:57                                      ; preds = %55, %52
  %58 = load i64*, i64** %mem, align 8
  %59 = ptrtoint i64* %58 to i64
  %60 = or i64 %59, 6
  ret i64 %60
}

; Function Attrs: nobuiltin nounwind
declare void @_ZdaPv(i8*) #6

; Function Attrs: uwtable
define i64 @prim_make_45vector(i64 %lenv, i64 %iv) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %l = alloca i64, align 8
  %vec = alloca i64*, align 8
  %i = alloca i64, align 8
  store i64 %lenv, i64* %1, align 8
  store i64 %iv, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.29, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %1, align 8
  %9 = and i64 %8, -8
  %10 = lshr i64 %9, 32
  %11 = trunc i64 %10 to i32
  %12 = sext i32 %11 to i64
  store i64 %12, i64* %l, align 8
  %13 = load i64, i64* %l, align 8
  %14 = add i64 %13, 1
  %15 = mul i64 %14, 8
  %16 = call i64* @alloc(i64 %15)
  store i64* %16, i64** %vec, align 8
  %17 = load i64, i64* %l, align 8
  %18 = shl i64 %17, 3
  %19 = or i64 %18, 1
  %20 = load i64*, i64** %vec, align 8
  %21 = getelementptr inbounds i64, i64* %20, i64 0
  store i64 %19, i64* %21, align 8
  store i64 1, i64* %i, align 8
  br label %22

; <label>:22                                      ; preds = %31, %7
  %23 = load i64, i64* %i, align 8
  %24 = load i64, i64* %l, align 8
  %25 = icmp ule i64 %23, %24
  br i1 %25, label %26, label %34

; <label>:26                                      ; preds = %22
  %27 = load i64, i64* %2, align 8
  %28 = load i64, i64* %i, align 8
  %29 = load i64*, i64** %vec, align 8
  %30 = getelementptr inbounds i64, i64* %29, i64 %28
  store i64 %27, i64* %30, align 8
  br label %31

; <label>:31                                      ; preds = %26
  %32 = load i64, i64* %i, align 8
  %33 = add i64 %32, 1
  store i64 %33, i64* %i, align 8
  br label %22

; <label>:34                                      ; preds = %22
  %35 = load i64*, i64** %vec, align 8
  %36 = ptrtoint i64* %35 to i64
  %37 = or i64 %36, 6
  ret i64 %37
}

; Function Attrs: uwtable
define i64 @applyprim_make_45vector(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.30, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_make_45vector(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @prim_vector_45ref(i64 %v, i64 %i) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %vec = alloca i64*, align 8
  %len = alloca i64, align 8
  store i64 %v, i64* %1, align 8
  store i64 %i, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.31, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %1, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 6
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.32, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, -8
  %15 = inttoptr i64 %14 to i64*
  %16 = getelementptr inbounds i64, i64* %15, i64 0
  %17 = load i64, i64* %16, align 8
  %18 = and i64 %17, 7
  %19 = icmp ne i64 %18, 1
  br i1 %19, label %20, label %21

; <label>:20                                      ; preds = %12
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.33, i32 0, i32 0))
  br label %21

; <label>:21                                      ; preds = %20, %12
  %22 = load i64, i64* %1, align 8
  %23 = and i64 %22, -8
  %24 = inttoptr i64 %23 to i64*
  store i64* %24, i64** %vec, align 8
  %25 = load i64*, i64** %vec, align 8
  %26 = getelementptr inbounds i64, i64* %25, i64 0
  %27 = load i64, i64* %26, align 8
  %28 = lshr i64 %27, 3
  store i64 %28, i64* %len, align 8
  %29 = load i64, i64* %2, align 8
  %30 = and i64 %29, -8
  %31 = lshr i64 %30, 32
  %32 = trunc i64 %31 to i32
  %33 = sext i32 %32 to i64
  %34 = load i64, i64* %len, align 8
  %35 = icmp ugt i64 %33, %34
  br i1 %35, label %42, label %36

; <label>:36                                      ; preds = %21
  %37 = load i64, i64* %2, align 8
  %38 = and i64 %37, -8
  %39 = lshr i64 %38, 32
  %40 = trunc i64 %39 to i32
  %41 = icmp slt i32 %40, 0
  br i1 %41, label %42, label %43

; <label>:42                                      ; preds = %36, %21
  call void @fatal_err(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.34, i32 0, i32 0))
  br label %43

; <label>:43                                      ; preds = %42, %36
  %44 = load i64, i64* %2, align 8
  %45 = and i64 %44, -8
  %46 = lshr i64 %45, 32
  %47 = trunc i64 %46 to i32
  %48 = add nsw i32 1, %47
  %49 = sext i32 %48 to i64
  %50 = load i64, i64* %1, align 8
  %51 = and i64 %50, -8
  %52 = inttoptr i64 %51 to i64*
  %53 = getelementptr inbounds i64, i64* %52, i64 %49
  %54 = load i64, i64* %53, align 8
  ret i64 %54
}

; Function Attrs: uwtable
define i64 @applyprim_vector_45ref(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.30, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_vector_45ref(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @prim_vector_45set_33(i64 %a, i64 %i, i64 %v) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %vec = alloca i64*, align 8
  %len = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 %i, i64* %2, align 8
  store i64 %v, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.31, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %1, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 6
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([48 x i8], [48 x i8]* @.str.35, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %1, align 8
  %15 = and i64 %14, -8
  %16 = inttoptr i64 %15 to i64*
  %17 = getelementptr inbounds i64, i64* %16, i64 0
  %18 = load i64, i64* %17, align 8
  %19 = and i64 %18, 7
  %20 = icmp ne i64 %19, 1
  br i1 %20, label %21, label %22

; <label>:21                                      ; preds = %13
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.33, i32 0, i32 0))
  br label %22

; <label>:22                                      ; preds = %21, %13
  %23 = load i64, i64* %1, align 8
  %24 = and i64 %23, -8
  %25 = inttoptr i64 %24 to i64*
  store i64* %25, i64** %vec, align 8
  %26 = load i64*, i64** %vec, align 8
  %27 = getelementptr inbounds i64, i64* %26, i64 0
  %28 = load i64, i64* %27, align 8
  %29 = lshr i64 %28, 3
  store i64 %29, i64* %len, align 8
  %30 = load i64, i64* %2, align 8
  %31 = and i64 %30, -8
  %32 = lshr i64 %31, 32
  %33 = trunc i64 %32 to i32
  %34 = sext i32 %33 to i64
  %35 = load i64, i64* %len, align 8
  %36 = icmp ugt i64 %34, %35
  br i1 %36, label %43, label %37

; <label>:37                                      ; preds = %22
  %38 = load i64, i64* %2, align 8
  %39 = and i64 %38, -8
  %40 = lshr i64 %39, 32
  %41 = trunc i64 %40 to i32
  %42 = icmp slt i32 %41, 0
  br i1 %42, label %43, label %44

; <label>:43                                      ; preds = %37, %22
  call void @fatal_err(i8* getelementptr inbounds ([31 x i8], [31 x i8]* @.str.34, i32 0, i32 0))
  br label %44

; <label>:44                                      ; preds = %43, %37
  %45 = load i64, i64* %3, align 8
  %46 = load i64, i64* %2, align 8
  %47 = and i64 %46, -8
  %48 = lshr i64 %47, 32
  %49 = trunc i64 %48 to i32
  %50 = add nsw i32 1, %49
  %51 = sext i32 %50 to i64
  %52 = load i64, i64* %1, align 8
  %53 = and i64 %52, -8
  %54 = inttoptr i64 %53 to i64*
  %55 = getelementptr inbounds i64, i64* %54, i64 %51
  store i64 %45, i64* %55, align 8
  ret i64 39
}

; Function Attrs: uwtable
define i64 @applyprim_vector_45set_33(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  %v2 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %rest)
  store i64 %7, i64* %v2, align 8
  %8 = load i64, i64* %rest, align 8
  %9 = icmp ne i64 %8, 0
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.30, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %0
  %12 = load i64, i64* %v0, align 8
  %13 = load i64, i64* %v1, align 8
  %14 = load i64, i64* %v2, align 8
  %15 = call i64 @prim_vector_45set_33(i64 %12, i64 %13, i64 %14)
  ret i64 %15
}

; Function Attrs: nounwind uwtable
define i64 @prim_void() #5 {
  ret i64 39
}

; Function Attrs: nounwind uwtable
define i64 @prim_eq_63(i64 %a, i64 %b) #5 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %4, %5
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %9

; <label>:8                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %9

; <label>:9                                       ; preds = %8, %7
  %10 = load i64, i64* %1, align 8
  ret i64 %10
}

; Function Attrs: uwtable
define i64 @applyprim_eq_63(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.30, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_eq_63(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: nounwind uwtable
define i64 @prim_eqv_63(i64 %a, i64 %b) #5 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = load i64, i64* %3, align 8
  %6 = icmp eq i64 %4, %5
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %9

; <label>:8                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %9

; <label>:9                                       ; preds = %8, %7
  %10 = load i64, i64* %1, align 8
  ret i64 %10
}

; Function Attrs: uwtable
define i64 @applyprim_eqv_63(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.30, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_eqv_63(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: nounwind uwtable
define i64 @prim_number_63(i64 %a) #5 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp eq i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %8

; <label>:7                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %1, align 8
  ret i64 %9
}

; Function Attrs: uwtable
define i64 @applyprim_number_63(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_number_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_integer_63(i64 %a) #5 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp eq i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %8

; <label>:7                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %1, align 8
  ret i64 %9
}

; Function Attrs: uwtable
define i64 @applyprim_integer_63(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_integer_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_void_63(i64 %a) #5 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 39
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %7

; <label>:6                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %7

; <label>:7                                       ; preds = %6, %5
  %8 = load i64, i64* %1, align 8
  ret i64 %8
}

; Function Attrs: uwtable
define i64 @applyprim_void_63(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_void_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_procedure_63(i64 %a) #5 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp eq i64 %4, 0
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %8

; <label>:7                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %1, align 8
  ret i64 %9
}

; Function Attrs: uwtable
define i64 @applyprim_procedure_63(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_procedure_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_null_63(i64 %p) #5 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %p, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 0
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %7

; <label>:6                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %7

; <label>:7                                       ; preds = %6, %5
  %8 = load i64, i64* %1, align 8
  ret i64 %8
}

; Function Attrs: uwtable
define i64 @applyprim_null_63(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_null_63(i64 %4)
  ret i64 %5
}

; Function Attrs: nounwind uwtable
define i64 @prim_cons_63(i64 %p) #5 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %p, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp eq i64 %4, 1
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %8

; <label>:7                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %8

; <label>:8                                       ; preds = %7, %6
  %9 = load i64, i64* %1, align 8
  ret i64 %9
}

; Function Attrs: uwtable
define i64 @applyprim_cons_63(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_cons_63(i64 %4)
  ret i64 %5
}

; Function Attrs: uwtable
define i64 @prim_cons(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %p = alloca i64*, align 8
  store i64 %a, i64* %1, align 8
  store i64 %b, i64* %2, align 8
  %3 = call i64* @alloc(i64 16)
  store i64* %3, i64** %p, align 8
  %4 = load i64, i64* %1, align 8
  %5 = load i64*, i64** %p, align 8
  %6 = getelementptr inbounds i64, i64* %5, i64 0
  store i64 %4, i64* %6, align 8
  %7 = load i64, i64* %2, align 8
  %8 = load i64*, i64** %p, align 8
  %9 = getelementptr inbounds i64, i64* %8, i64 1
  store i64 %7, i64* %9, align 8
  %10 = load i64*, i64** %p, align 8
  %11 = ptrtoint i64* %10 to i64
  %12 = or i64 %11, 1
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @applyprim_cons(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.30, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_cons(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @prim_car(i64 %p) #0 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %p, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  ret i64 %4
}

; Function Attrs: uwtable
define i64 @applyprim_car(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_car(i64 %4)
  ret i64 %5
}

; Function Attrs: uwtable
define i64 @prim_cdr(i64 %p) #0 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %p, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  ret i64 %4
}

; Function Attrs: uwtable
define i64 @applyprim_cdr(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_cdr(i64 %4)
  ret i64 %5
}

; Function Attrs: uwtable
define i64 @prim__43(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 %b, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.36, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.37, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %1, align 8
  call void @check_size(i64 %13)
  %14 = load i64, i64* %2, align 8
  call void @check_size(i64 %14)
  %15 = load i64, i64* %1, align 8
  %16 = and i64 %15, -8
  %17 = lshr i64 %16, 32
  %18 = trunc i64 %17 to i32
  %19 = load i64, i64* %2, align 8
  %20 = and i64 %19, -8
  %21 = lshr i64 %20, 32
  %22 = trunc i64 %21 to i32
  %23 = add nsw i32 %18, %22
  %24 = zext i32 %23 to i64
  %25 = shl i64 %24, 32
  %26 = or i64 %25, 2
  ret i64 %26
}

; Function Attrs: uwtable
define i64 @applyprim__43(i64 %p) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %pp = alloca i64*, align 8
  store i64 %p, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 0
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 2, i64* %1, align 8
  br label %32

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %2, align 8
  %8 = and i64 %7, 7
  %9 = icmp ne i64 %8, 1
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %6
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.38, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %6
  %12 = load i64, i64* %2, align 8
  %13 = and i64 %12, -8
  %14 = inttoptr i64 %13 to i64*
  store i64* %14, i64** %pp, align 8
  %15 = load i64*, i64** %pp, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 0
  %17 = load i64, i64* %16, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = load i64*, i64** %pp, align 8
  %22 = getelementptr inbounds i64, i64* %21, i64 1
  %23 = load i64, i64* %22, align 8
  %24 = call i64 @applyprim__43(i64 %23)
  %25 = and i64 %24, -8
  %26 = lshr i64 %25, 32
  %27 = trunc i64 %26 to i32
  %28 = add nsw i32 %20, %27
  %29 = zext i32 %28 to i64
  %30 = shl i64 %29, 32
  %31 = or i64 %30, 2
  store i64 %31, i64* %1, align 8
  br label %32

; <label>:32                                      ; preds = %11, %5
  %33 = load i64, i64* %1, align 8
  ret i64 %33
}

; Function Attrs: uwtable
define i64 @prim__45(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 %b, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.36, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.39, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, -8
  %15 = lshr i64 %14, 32
  %16 = trunc i64 %15 to i32
  %17 = load i64, i64* %2, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = sub nsw i32 %16, %20
  %22 = zext i32 %21 to i64
  %23 = shl i64 %22, 32
  %24 = or i64 %23, 2
  ret i64 %24
}

; Function Attrs: uwtable
define i64 @applyprim__45(i64 %p) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %pp = alloca i64*, align 8
  store i64 %p, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 0
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 2, i64* %1, align 8
  br label %48

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %2, align 8
  %8 = and i64 %7, 7
  %9 = icmp ne i64 %8, 1
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %6
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.38, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %6
  %12 = load i64, i64* %2, align 8
  %13 = and i64 %12, -8
  %14 = inttoptr i64 %13 to i64*
  store i64* %14, i64** %pp, align 8
  %15 = load i64*, i64** %pp, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 1
  %17 = load i64, i64* %16, align 8
  %18 = icmp eq i64 %17, 0
  br i1 %18, label %19, label %30

; <label>:19                                      ; preds = %11
  %20 = load i64*, i64** %pp, align 8
  %21 = getelementptr inbounds i64, i64* %20, i64 0
  %22 = load i64, i64* %21, align 8
  %23 = and i64 %22, -8
  %24 = lshr i64 %23, 32
  %25 = trunc i64 %24 to i32
  %26 = sub nsw i32 0, %25
  %27 = zext i32 %26 to i64
  %28 = shl i64 %27, 32
  %29 = or i64 %28, 2
  store i64 %29, i64* %1, align 8
  br label %48

; <label>:30                                      ; preds = %11
  %31 = load i64*, i64** %pp, align 8
  %32 = getelementptr inbounds i64, i64* %31, i64 0
  %33 = load i64, i64* %32, align 8
  %34 = and i64 %33, -8
  %35 = lshr i64 %34, 32
  %36 = trunc i64 %35 to i32
  %37 = load i64*, i64** %pp, align 8
  %38 = getelementptr inbounds i64, i64* %37, i64 1
  %39 = load i64, i64* %38, align 8
  %40 = call i64 @applyprim__43(i64 %39)
  %41 = and i64 %40, -8
  %42 = lshr i64 %41, 32
  %43 = trunc i64 %42 to i32
  %44 = sub nsw i32 %36, %43
  %45 = zext i32 %44 to i64
  %46 = shl i64 %45, 32
  %47 = or i64 %46, 2
  store i64 %47, i64* %1, align 8
  br label %48

; <label>:48                                      ; preds = %30, %19, %5
  %49 = load i64, i64* %1, align 8
  ret i64 %49
}

; Function Attrs: uwtable
define i64 @prim__42(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 %b, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.40, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.41, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %1, align 8
  %14 = and i64 %13, -8
  %15 = lshr i64 %14, 32
  %16 = trunc i64 %15 to i32
  %17 = load i64, i64* %2, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = mul nsw i32 %16, %20
  %22 = zext i32 %21 to i64
  %23 = shl i64 %22, 32
  %24 = or i64 %23, 2
  ret i64 %24
}

; Function Attrs: uwtable
define i64 @applyprim__42(i64 %p) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %pp = alloca i64*, align 8
  store i64 %p, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 0
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 4294967298, i64* %1, align 8
  br label %32

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %2, align 8
  %8 = and i64 %7, 7
  %9 = icmp ne i64 %8, 1
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %6
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.38, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %6
  %12 = load i64, i64* %2, align 8
  %13 = and i64 %12, -8
  %14 = inttoptr i64 %13 to i64*
  store i64* %14, i64** %pp, align 8
  %15 = load i64*, i64** %pp, align 8
  %16 = getelementptr inbounds i64, i64* %15, i64 0
  %17 = load i64, i64* %16, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = load i64*, i64** %pp, align 8
  %22 = getelementptr inbounds i64, i64* %21, i64 1
  %23 = load i64, i64* %22, align 8
  %24 = call i64 @applyprim__42(i64 %23)
  %25 = and i64 %24, -8
  %26 = lshr i64 %25, 32
  %27 = trunc i64 %26 to i32
  %28 = mul nsw i32 %20, %27
  %29 = zext i32 %28 to i64
  %30 = shl i64 %29, 32
  %31 = or i64 %30, 2
  store i64 %31, i64* %1, align 8
  br label %32

; <label>:32                                      ; preds = %11, %5
  %33 = load i64, i64* %1, align 8
  ret i64 %33
}

; Function Attrs: uwtable
define i64 @prim__47(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %1, align 8
  store i64 %b, i64* %2, align 8
  %3 = load i64, i64* %1, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.42, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.43, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %2, align 8
  %14 = icmp eq i64 %13, 2
  br i1 %14, label %15, label %16

; <label>:15                                      ; preds = %12
  call void @fatal_err(i8* getelementptr inbounds ([41 x i8], [41 x i8]* @.str.44, i32 0, i32 0))
  br label %16

; <label>:16                                      ; preds = %15, %12
  %17 = load i64, i64* %1, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = load i64, i64* %2, align 8
  %22 = and i64 %21, -8
  %23 = lshr i64 %22, 32
  %24 = trunc i64 %23 to i32
  %25 = sdiv i32 %20, %24
  %26 = zext i32 %25 to i64
  %27 = shl i64 %26, 32
  %28 = or i64 %27, 2
  ret i64 %28
}

; Function Attrs: uwtable
define i64 @prim__61(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.45, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.46, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %2, align 8
  %15 = and i64 %14, -8
  %16 = lshr i64 %15, 32
  %17 = trunc i64 %16 to i32
  %18 = load i64, i64* %3, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = trunc i64 %20 to i32
  %22 = icmp eq i32 %17, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %13
  store i64 31, i64* %1, align 8
  br label %25

; <label>:24                                      ; preds = %13
  store i64 15, i64* %1, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %1, align 8
  ret i64 %26
}

; Function Attrs: uwtable
define i64 @prim__60(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.47, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.48, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %2, align 8
  %15 = and i64 %14, -8
  %16 = lshr i64 %15, 32
  %17 = trunc i64 %16 to i32
  %18 = load i64, i64* %3, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = trunc i64 %20 to i32
  %22 = icmp slt i32 %17, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %13
  store i64 31, i64* %1, align 8
  br label %25

; <label>:24                                      ; preds = %13
  store i64 15, i64* %1, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %1, align 8
  ret i64 %26
}

; Function Attrs: uwtable
define i64 @prim__60_61(i64 %a, i64 %b) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  store i64 %b, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 2
  br i1 %6, label %7, label %8

; <label>:7                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.49, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.50, i32 0, i32 0))
  br label %13

; <label>:13                                      ; preds = %12, %8
  %14 = load i64, i64* %2, align 8
  %15 = and i64 %14, -8
  %16 = lshr i64 %15, 32
  %17 = trunc i64 %16 to i32
  %18 = load i64, i64* %3, align 8
  %19 = and i64 %18, -8
  %20 = lshr i64 %19, 32
  %21 = trunc i64 %20 to i32
  %22 = icmp sle i32 %17, %21
  br i1 %22, label %23, label %24

; <label>:23                                      ; preds = %13
  store i64 31, i64* %1, align 8
  br label %25

; <label>:24                                      ; preds = %13
  store i64 15, i64* %1, align 8
  br label %25

; <label>:25                                      ; preds = %24, %23
  %26 = load i64, i64* %1, align 8
  ret i64 %26
}

; Function Attrs: nounwind uwtable
define i64 @prim_not(i64 %a) #5 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  store i64 %a, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = icmp eq i64 %3, 15
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  store i64 31, i64* %1, align 8
  br label %7

; <label>:6                                       ; preds = %0
  store i64 15, i64* %1, align 8
  br label %7

; <label>:7                                       ; preds = %6, %5
  %8 = load i64, i64* %1, align 8
  ret i64 %8
}

; Function Attrs: uwtable
define i64 @applyprim_not(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_not(i64 %4)
  ret i64 %5
}

; Function Attrs: uwtable
define i64 @prim_make_45hash(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %new_pair = alloca %struct.hash_tbl*, align 8
  %new_table = alloca %struct.hash_tbl*, align 8
  %pair = alloca i64, align 8
  %vec = alloca i64*, align 8
  %result = alloca %struct.hash_tbl*, align 8
  %key = alloca i64, align 8
  %_hf_hashv = alloca i32, align 4
  %_hj_i = alloca i32, align 4
  %_hj_j = alloca i32, align 4
  %_hj_k = alloca i32, align 4
  %_hj_key = alloca i8*, align 8
  %_hf_bkt = alloca i32, align 4
  %_ha_hashv = alloca i32, align 4
  %_hj_i1 = alloca i32, align 4
  %_hj_j2 = alloca i32, align 4
  %_hj_k3 = alloca i32, align 4
  %_hj_key4 = alloca i8*, align 8
  %_ha_bkt = alloca i32, align 4
  %_ha_head = alloca %struct.UT_hash_bucket*, align 8
  %_he_bkt = alloca i32, align 4
  %_he_bkt_i = alloca i32, align 4
  %_he_thh = alloca %struct.UT_hash_handle*, align 8
  %_he_hh_nxt = alloca %struct.UT_hash_handle*, align 8
  %_he_new_buckets = alloca %struct.UT_hash_bucket*, align 8
  %_he_newbkt = alloca %struct.UT_hash_bucket*, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = and i64 %2, 7
  %4 = icmp ne i64 %3, 1
  br i1 %4, label %5, label %6

; <label>:5                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.51, i32 0, i32 0))
  br label %6

; <label>:6                                       ; preds = %5, %0
  store %struct.hash_tbl* null, %struct.hash_tbl** %new_pair, align 8
  store %struct.hash_tbl* null, %struct.hash_tbl** %new_table, align 8
  %7 = load i64, i64* @memory_cap, align 8
  %8 = add i64 %7, 16
  store i64 %8, i64* @memory_cap, align 8
  %9 = load i64, i64* @memory_cap, align 8
  %10 = icmp ugt i64 %9, 268435456
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %6
  call void @fatal_err(i8* getelementptr inbounds ([28 x i8], [28 x i8]* @.str.3, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %6
  %13 = call i64* @alloc(i64 16)
  store i64* %13, i64** %vec, align 8
  %14 = load i64*, i64** %vec, align 8
  %15 = getelementptr inbounds i64, i64* %14, i64 0
  store i64 2, i64* %15, align 8
  br label %16

; <label>:16                                      ; preds = %1354, %12
  %17 = load i64, i64* %1, align 8
  %18 = and i64 %17, 7
  %19 = icmp eq i64 %18, 1
  br i1 %19, label %20, label %1355

; <label>:20                                      ; preds = %16
  store %struct.hash_tbl* null, %struct.hash_tbl** %result, align 8
  %21 = call noalias i8* @malloc(i64 72) #2
  %22 = bitcast i8* %21 to %struct.hash_tbl*
  store %struct.hash_tbl* %22, %struct.hash_tbl** %new_pair, align 8
  %23 = load i64, i64* %1, align 8
  %24 = call i64 @expect_cons(i64 %23, i64* %1)
  store i64 %24, i64* %pair, align 8
  %25 = load i64, i64* %pair, align 8
  %26 = call i64 @expect_cons(i64 %25, i64* %pair)
  store i64 %26, i64* %key, align 8
  %27 = load i64, i64* %key, align 8
  %28 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %29 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %28, i32 0, i32 0
  store i64 %27, i64* %29, align 8
  %30 = load i64, i64* %pair, align 8
  %31 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %32 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %31, i32 0, i32 1
  store i64 %30, i64* %32, align 8
  br label %33

; <label>:33                                      ; preds = %20
  br label %34

; <label>:34                                      ; preds = %33
  br label %35

; <label>:35                                      ; preds = %34
  %36 = bitcast i64* %key to i8*
  store i8* %36, i8** %_hj_key, align 8
  store i32 -17973521, i32* %_hf_hashv, align 4
  store i32 -1640531527, i32* %_hj_j, align 4
  store i32 -1640531527, i32* %_hj_i, align 4
  store i32 8, i32* %_hj_k, align 4
  br label %37

; <label>:37                                      ; preds = %204, %35
  %38 = load i32, i32* %_hj_k, align 4
  %39 = icmp uge i32 %38, 12
  br i1 %39, label %40, label %209

; <label>:40                                      ; preds = %37
  %41 = load i8*, i8** %_hj_key, align 8
  %42 = getelementptr inbounds i8, i8* %41, i64 0
  %43 = load i8, i8* %42, align 1
  %44 = zext i8 %43 to i32
  %45 = load i8*, i8** %_hj_key, align 8
  %46 = getelementptr inbounds i8, i8* %45, i64 1
  %47 = load i8, i8* %46, align 1
  %48 = zext i8 %47 to i32
  %49 = shl i32 %48, 8
  %50 = add i32 %44, %49
  %51 = load i8*, i8** %_hj_key, align 8
  %52 = getelementptr inbounds i8, i8* %51, i64 2
  %53 = load i8, i8* %52, align 1
  %54 = zext i8 %53 to i32
  %55 = shl i32 %54, 16
  %56 = add i32 %50, %55
  %57 = load i8*, i8** %_hj_key, align 8
  %58 = getelementptr inbounds i8, i8* %57, i64 3
  %59 = load i8, i8* %58, align 1
  %60 = zext i8 %59 to i32
  %61 = shl i32 %60, 24
  %62 = add i32 %56, %61
  %63 = load i32, i32* %_hj_i, align 4
  %64 = add i32 %63, %62
  store i32 %64, i32* %_hj_i, align 4
  %65 = load i8*, i8** %_hj_key, align 8
  %66 = getelementptr inbounds i8, i8* %65, i64 4
  %67 = load i8, i8* %66, align 1
  %68 = zext i8 %67 to i32
  %69 = load i8*, i8** %_hj_key, align 8
  %70 = getelementptr inbounds i8, i8* %69, i64 5
  %71 = load i8, i8* %70, align 1
  %72 = zext i8 %71 to i32
  %73 = shl i32 %72, 8
  %74 = add i32 %68, %73
  %75 = load i8*, i8** %_hj_key, align 8
  %76 = getelementptr inbounds i8, i8* %75, i64 6
  %77 = load i8, i8* %76, align 1
  %78 = zext i8 %77 to i32
  %79 = shl i32 %78, 16
  %80 = add i32 %74, %79
  %81 = load i8*, i8** %_hj_key, align 8
  %82 = getelementptr inbounds i8, i8* %81, i64 7
  %83 = load i8, i8* %82, align 1
  %84 = zext i8 %83 to i32
  %85 = shl i32 %84, 24
  %86 = add i32 %80, %85
  %87 = load i32, i32* %_hj_j, align 4
  %88 = add i32 %87, %86
  store i32 %88, i32* %_hj_j, align 4
  %89 = load i8*, i8** %_hj_key, align 8
  %90 = getelementptr inbounds i8, i8* %89, i64 8
  %91 = load i8, i8* %90, align 1
  %92 = zext i8 %91 to i32
  %93 = load i8*, i8** %_hj_key, align 8
  %94 = getelementptr inbounds i8, i8* %93, i64 9
  %95 = load i8, i8* %94, align 1
  %96 = zext i8 %95 to i32
  %97 = shl i32 %96, 8
  %98 = add i32 %92, %97
  %99 = load i8*, i8** %_hj_key, align 8
  %100 = getelementptr inbounds i8, i8* %99, i64 10
  %101 = load i8, i8* %100, align 1
  %102 = zext i8 %101 to i32
  %103 = shl i32 %102, 16
  %104 = add i32 %98, %103
  %105 = load i8*, i8** %_hj_key, align 8
  %106 = getelementptr inbounds i8, i8* %105, i64 11
  %107 = load i8, i8* %106, align 1
  %108 = zext i8 %107 to i32
  %109 = shl i32 %108, 24
  %110 = add i32 %104, %109
  %111 = load i32, i32* %_hf_hashv, align 4
  %112 = add i32 %111, %110
  store i32 %112, i32* %_hf_hashv, align 4
  br label %113

; <label>:113                                     ; preds = %40
  %114 = load i32, i32* %_hj_j, align 4
  %115 = load i32, i32* %_hj_i, align 4
  %116 = sub i32 %115, %114
  store i32 %116, i32* %_hj_i, align 4
  %117 = load i32, i32* %_hf_hashv, align 4
  %118 = load i32, i32* %_hj_i, align 4
  %119 = sub i32 %118, %117
  store i32 %119, i32* %_hj_i, align 4
  %120 = load i32, i32* %_hf_hashv, align 4
  %121 = lshr i32 %120, 13
  %122 = load i32, i32* %_hj_i, align 4
  %123 = xor i32 %122, %121
  store i32 %123, i32* %_hj_i, align 4
  %124 = load i32, i32* %_hf_hashv, align 4
  %125 = load i32, i32* %_hj_j, align 4
  %126 = sub i32 %125, %124
  store i32 %126, i32* %_hj_j, align 4
  %127 = load i32, i32* %_hj_i, align 4
  %128 = load i32, i32* %_hj_j, align 4
  %129 = sub i32 %128, %127
  store i32 %129, i32* %_hj_j, align 4
  %130 = load i32, i32* %_hj_i, align 4
  %131 = shl i32 %130, 8
  %132 = load i32, i32* %_hj_j, align 4
  %133 = xor i32 %132, %131
  store i32 %133, i32* %_hj_j, align 4
  %134 = load i32, i32* %_hj_i, align 4
  %135 = load i32, i32* %_hf_hashv, align 4
  %136 = sub i32 %135, %134
  store i32 %136, i32* %_hf_hashv, align 4
  %137 = load i32, i32* %_hj_j, align 4
  %138 = load i32, i32* %_hf_hashv, align 4
  %139 = sub i32 %138, %137
  store i32 %139, i32* %_hf_hashv, align 4
  %140 = load i32, i32* %_hj_j, align 4
  %141 = lshr i32 %140, 13
  %142 = load i32, i32* %_hf_hashv, align 4
  %143 = xor i32 %142, %141
  store i32 %143, i32* %_hf_hashv, align 4
  %144 = load i32, i32* %_hj_j, align 4
  %145 = load i32, i32* %_hj_i, align 4
  %146 = sub i32 %145, %144
  store i32 %146, i32* %_hj_i, align 4
  %147 = load i32, i32* %_hf_hashv, align 4
  %148 = load i32, i32* %_hj_i, align 4
  %149 = sub i32 %148, %147
  store i32 %149, i32* %_hj_i, align 4
  %150 = load i32, i32* %_hf_hashv, align 4
  %151 = lshr i32 %150, 12
  %152 = load i32, i32* %_hj_i, align 4
  %153 = xor i32 %152, %151
  store i32 %153, i32* %_hj_i, align 4
  %154 = load i32, i32* %_hf_hashv, align 4
  %155 = load i32, i32* %_hj_j, align 4
  %156 = sub i32 %155, %154
  store i32 %156, i32* %_hj_j, align 4
  %157 = load i32, i32* %_hj_i, align 4
  %158 = load i32, i32* %_hj_j, align 4
  %159 = sub i32 %158, %157
  store i32 %159, i32* %_hj_j, align 4
  %160 = load i32, i32* %_hj_i, align 4
  %161 = shl i32 %160, 16
  %162 = load i32, i32* %_hj_j, align 4
  %163 = xor i32 %162, %161
  store i32 %163, i32* %_hj_j, align 4
  %164 = load i32, i32* %_hj_i, align 4
  %165 = load i32, i32* %_hf_hashv, align 4
  %166 = sub i32 %165, %164
  store i32 %166, i32* %_hf_hashv, align 4
  %167 = load i32, i32* %_hj_j, align 4
  %168 = load i32, i32* %_hf_hashv, align 4
  %169 = sub i32 %168, %167
  store i32 %169, i32* %_hf_hashv, align 4
  %170 = load i32, i32* %_hj_j, align 4
  %171 = lshr i32 %170, 5
  %172 = load i32, i32* %_hf_hashv, align 4
  %173 = xor i32 %172, %171
  store i32 %173, i32* %_hf_hashv, align 4
  %174 = load i32, i32* %_hj_j, align 4
  %175 = load i32, i32* %_hj_i, align 4
  %176 = sub i32 %175, %174
  store i32 %176, i32* %_hj_i, align 4
  %177 = load i32, i32* %_hf_hashv, align 4
  %178 = load i32, i32* %_hj_i, align 4
  %179 = sub i32 %178, %177
  store i32 %179, i32* %_hj_i, align 4
  %180 = load i32, i32* %_hf_hashv, align 4
  %181 = lshr i32 %180, 3
  %182 = load i32, i32* %_hj_i, align 4
  %183 = xor i32 %182, %181
  store i32 %183, i32* %_hj_i, align 4
  %184 = load i32, i32* %_hf_hashv, align 4
  %185 = load i32, i32* %_hj_j, align 4
  %186 = sub i32 %185, %184
  store i32 %186, i32* %_hj_j, align 4
  %187 = load i32, i32* %_hj_i, align 4
  %188 = load i32, i32* %_hj_j, align 4
  %189 = sub i32 %188, %187
  store i32 %189, i32* %_hj_j, align 4
  %190 = load i32, i32* %_hj_i, align 4
  %191 = shl i32 %190, 10
  %192 = load i32, i32* %_hj_j, align 4
  %193 = xor i32 %192, %191
  store i32 %193, i32* %_hj_j, align 4
  %194 = load i32, i32* %_hj_i, align 4
  %195 = load i32, i32* %_hf_hashv, align 4
  %196 = sub i32 %195, %194
  store i32 %196, i32* %_hf_hashv, align 4
  %197 = load i32, i32* %_hj_j, align 4
  %198 = load i32, i32* %_hf_hashv, align 4
  %199 = sub i32 %198, %197
  store i32 %199, i32* %_hf_hashv, align 4
  %200 = load i32, i32* %_hj_j, align 4
  %201 = lshr i32 %200, 15
  %202 = load i32, i32* %_hf_hashv, align 4
  %203 = xor i32 %202, %201
  store i32 %203, i32* %_hf_hashv, align 4
  br label %204

; <label>:204                                     ; preds = %113
  %205 = load i8*, i8** %_hj_key, align 8
  %206 = getelementptr inbounds i8, i8* %205, i64 12
  store i8* %206, i8** %_hj_key, align 8
  %207 = load i32, i32* %_hj_k, align 4
  %208 = sub i32 %207, 12
  store i32 %208, i32* %_hj_k, align 4
  br label %37

; <label>:209                                     ; preds = %37
  %210 = load i32, i32* %_hf_hashv, align 4
  %211 = add i32 %210, 8
  store i32 %211, i32* %_hf_hashv, align 4
  %212 = load i32, i32* %_hj_k, align 4
  switch i32 %212, label %299 [
    i32 11, label %213
    i32 10, label %221
    i32 9, label %229
    i32 8, label %237
    i32 7, label %245
    i32 6, label %253
    i32 5, label %261
    i32 4, label %268
    i32 3, label %276
    i32 2, label %284
    i32 1, label %292
  ]

; <label>:213                                     ; preds = %209
  %214 = load i8*, i8** %_hj_key, align 8
  %215 = getelementptr inbounds i8, i8* %214, i64 10
  %216 = load i8, i8* %215, align 1
  %217 = zext i8 %216 to i32
  %218 = shl i32 %217, 24
  %219 = load i32, i32* %_hf_hashv, align 4
  %220 = add i32 %219, %218
  store i32 %220, i32* %_hf_hashv, align 4
  br label %221

; <label>:221                                     ; preds = %209, %213
  %222 = load i8*, i8** %_hj_key, align 8
  %223 = getelementptr inbounds i8, i8* %222, i64 9
  %224 = load i8, i8* %223, align 1
  %225 = zext i8 %224 to i32
  %226 = shl i32 %225, 16
  %227 = load i32, i32* %_hf_hashv, align 4
  %228 = add i32 %227, %226
  store i32 %228, i32* %_hf_hashv, align 4
  br label %229

; <label>:229                                     ; preds = %209, %221
  %230 = load i8*, i8** %_hj_key, align 8
  %231 = getelementptr inbounds i8, i8* %230, i64 8
  %232 = load i8, i8* %231, align 1
  %233 = zext i8 %232 to i32
  %234 = shl i32 %233, 8
  %235 = load i32, i32* %_hf_hashv, align 4
  %236 = add i32 %235, %234
  store i32 %236, i32* %_hf_hashv, align 4
  br label %237

; <label>:237                                     ; preds = %209, %229
  %238 = load i8*, i8** %_hj_key, align 8
  %239 = getelementptr inbounds i8, i8* %238, i64 7
  %240 = load i8, i8* %239, align 1
  %241 = zext i8 %240 to i32
  %242 = shl i32 %241, 24
  %243 = load i32, i32* %_hj_j, align 4
  %244 = add i32 %243, %242
  store i32 %244, i32* %_hj_j, align 4
  br label %245

; <label>:245                                     ; preds = %209, %237
  %246 = load i8*, i8** %_hj_key, align 8
  %247 = getelementptr inbounds i8, i8* %246, i64 6
  %248 = load i8, i8* %247, align 1
  %249 = zext i8 %248 to i32
  %250 = shl i32 %249, 16
  %251 = load i32, i32* %_hj_j, align 4
  %252 = add i32 %251, %250
  store i32 %252, i32* %_hj_j, align 4
  br label %253

; <label>:253                                     ; preds = %209, %245
  %254 = load i8*, i8** %_hj_key, align 8
  %255 = getelementptr inbounds i8, i8* %254, i64 5
  %256 = load i8, i8* %255, align 1
  %257 = zext i8 %256 to i32
  %258 = shl i32 %257, 8
  %259 = load i32, i32* %_hj_j, align 4
  %260 = add i32 %259, %258
  store i32 %260, i32* %_hj_j, align 4
  br label %261

; <label>:261                                     ; preds = %209, %253
  %262 = load i8*, i8** %_hj_key, align 8
  %263 = getelementptr inbounds i8, i8* %262, i64 4
  %264 = load i8, i8* %263, align 1
  %265 = zext i8 %264 to i32
  %266 = load i32, i32* %_hj_j, align 4
  %267 = add i32 %266, %265
  store i32 %267, i32* %_hj_j, align 4
  br label %268

; <label>:268                                     ; preds = %209, %261
  %269 = load i8*, i8** %_hj_key, align 8
  %270 = getelementptr inbounds i8, i8* %269, i64 3
  %271 = load i8, i8* %270, align 1
  %272 = zext i8 %271 to i32
  %273 = shl i32 %272, 24
  %274 = load i32, i32* %_hj_i, align 4
  %275 = add i32 %274, %273
  store i32 %275, i32* %_hj_i, align 4
  br label %276

; <label>:276                                     ; preds = %209, %268
  %277 = load i8*, i8** %_hj_key, align 8
  %278 = getelementptr inbounds i8, i8* %277, i64 2
  %279 = load i8, i8* %278, align 1
  %280 = zext i8 %279 to i32
  %281 = shl i32 %280, 16
  %282 = load i32, i32* %_hj_i, align 4
  %283 = add i32 %282, %281
  store i32 %283, i32* %_hj_i, align 4
  br label %284

; <label>:284                                     ; preds = %209, %276
  %285 = load i8*, i8** %_hj_key, align 8
  %286 = getelementptr inbounds i8, i8* %285, i64 1
  %287 = load i8, i8* %286, align 1
  %288 = zext i8 %287 to i32
  %289 = shl i32 %288, 8
  %290 = load i32, i32* %_hj_i, align 4
  %291 = add i32 %290, %289
  store i32 %291, i32* %_hj_i, align 4
  br label %292

; <label>:292                                     ; preds = %209, %284
  %293 = load i8*, i8** %_hj_key, align 8
  %294 = getelementptr inbounds i8, i8* %293, i64 0
  %295 = load i8, i8* %294, align 1
  %296 = zext i8 %295 to i32
  %297 = load i32, i32* %_hj_i, align 4
  %298 = add i32 %297, %296
  store i32 %298, i32* %_hj_i, align 4
  br label %299

; <label>:299                                     ; preds = %292, %209
  br label %300

; <label>:300                                     ; preds = %299
  %301 = load i32, i32* %_hj_j, align 4
  %302 = load i32, i32* %_hj_i, align 4
  %303 = sub i32 %302, %301
  store i32 %303, i32* %_hj_i, align 4
  %304 = load i32, i32* %_hf_hashv, align 4
  %305 = load i32, i32* %_hj_i, align 4
  %306 = sub i32 %305, %304
  store i32 %306, i32* %_hj_i, align 4
  %307 = load i32, i32* %_hf_hashv, align 4
  %308 = lshr i32 %307, 13
  %309 = load i32, i32* %_hj_i, align 4
  %310 = xor i32 %309, %308
  store i32 %310, i32* %_hj_i, align 4
  %311 = load i32, i32* %_hf_hashv, align 4
  %312 = load i32, i32* %_hj_j, align 4
  %313 = sub i32 %312, %311
  store i32 %313, i32* %_hj_j, align 4
  %314 = load i32, i32* %_hj_i, align 4
  %315 = load i32, i32* %_hj_j, align 4
  %316 = sub i32 %315, %314
  store i32 %316, i32* %_hj_j, align 4
  %317 = load i32, i32* %_hj_i, align 4
  %318 = shl i32 %317, 8
  %319 = load i32, i32* %_hj_j, align 4
  %320 = xor i32 %319, %318
  store i32 %320, i32* %_hj_j, align 4
  %321 = load i32, i32* %_hj_i, align 4
  %322 = load i32, i32* %_hf_hashv, align 4
  %323 = sub i32 %322, %321
  store i32 %323, i32* %_hf_hashv, align 4
  %324 = load i32, i32* %_hj_j, align 4
  %325 = load i32, i32* %_hf_hashv, align 4
  %326 = sub i32 %325, %324
  store i32 %326, i32* %_hf_hashv, align 4
  %327 = load i32, i32* %_hj_j, align 4
  %328 = lshr i32 %327, 13
  %329 = load i32, i32* %_hf_hashv, align 4
  %330 = xor i32 %329, %328
  store i32 %330, i32* %_hf_hashv, align 4
  %331 = load i32, i32* %_hj_j, align 4
  %332 = load i32, i32* %_hj_i, align 4
  %333 = sub i32 %332, %331
  store i32 %333, i32* %_hj_i, align 4
  %334 = load i32, i32* %_hf_hashv, align 4
  %335 = load i32, i32* %_hj_i, align 4
  %336 = sub i32 %335, %334
  store i32 %336, i32* %_hj_i, align 4
  %337 = load i32, i32* %_hf_hashv, align 4
  %338 = lshr i32 %337, 12
  %339 = load i32, i32* %_hj_i, align 4
  %340 = xor i32 %339, %338
  store i32 %340, i32* %_hj_i, align 4
  %341 = load i32, i32* %_hf_hashv, align 4
  %342 = load i32, i32* %_hj_j, align 4
  %343 = sub i32 %342, %341
  store i32 %343, i32* %_hj_j, align 4
  %344 = load i32, i32* %_hj_i, align 4
  %345 = load i32, i32* %_hj_j, align 4
  %346 = sub i32 %345, %344
  store i32 %346, i32* %_hj_j, align 4
  %347 = load i32, i32* %_hj_i, align 4
  %348 = shl i32 %347, 16
  %349 = load i32, i32* %_hj_j, align 4
  %350 = xor i32 %349, %348
  store i32 %350, i32* %_hj_j, align 4
  %351 = load i32, i32* %_hj_i, align 4
  %352 = load i32, i32* %_hf_hashv, align 4
  %353 = sub i32 %352, %351
  store i32 %353, i32* %_hf_hashv, align 4
  %354 = load i32, i32* %_hj_j, align 4
  %355 = load i32, i32* %_hf_hashv, align 4
  %356 = sub i32 %355, %354
  store i32 %356, i32* %_hf_hashv, align 4
  %357 = load i32, i32* %_hj_j, align 4
  %358 = lshr i32 %357, 5
  %359 = load i32, i32* %_hf_hashv, align 4
  %360 = xor i32 %359, %358
  store i32 %360, i32* %_hf_hashv, align 4
  %361 = load i32, i32* %_hj_j, align 4
  %362 = load i32, i32* %_hj_i, align 4
  %363 = sub i32 %362, %361
  store i32 %363, i32* %_hj_i, align 4
  %364 = load i32, i32* %_hf_hashv, align 4
  %365 = load i32, i32* %_hj_i, align 4
  %366 = sub i32 %365, %364
  store i32 %366, i32* %_hj_i, align 4
  %367 = load i32, i32* %_hf_hashv, align 4
  %368 = lshr i32 %367, 3
  %369 = load i32, i32* %_hj_i, align 4
  %370 = xor i32 %369, %368
  store i32 %370, i32* %_hj_i, align 4
  %371 = load i32, i32* %_hf_hashv, align 4
  %372 = load i32, i32* %_hj_j, align 4
  %373 = sub i32 %372, %371
  store i32 %373, i32* %_hj_j, align 4
  %374 = load i32, i32* %_hj_i, align 4
  %375 = load i32, i32* %_hj_j, align 4
  %376 = sub i32 %375, %374
  store i32 %376, i32* %_hj_j, align 4
  %377 = load i32, i32* %_hj_i, align 4
  %378 = shl i32 %377, 10
  %379 = load i32, i32* %_hj_j, align 4
  %380 = xor i32 %379, %378
  store i32 %380, i32* %_hj_j, align 4
  %381 = load i32, i32* %_hj_i, align 4
  %382 = load i32, i32* %_hf_hashv, align 4
  %383 = sub i32 %382, %381
  store i32 %383, i32* %_hf_hashv, align 4
  %384 = load i32, i32* %_hj_j, align 4
  %385 = load i32, i32* %_hf_hashv, align 4
  %386 = sub i32 %385, %384
  store i32 %386, i32* %_hf_hashv, align 4
  %387 = load i32, i32* %_hj_j, align 4
  %388 = lshr i32 %387, 15
  %389 = load i32, i32* %_hf_hashv, align 4
  %390 = xor i32 %389, %388
  store i32 %390, i32* %_hf_hashv, align 4
  br label %391

; <label>:391                                     ; preds = %300
  br label %392

; <label>:392                                     ; preds = %391
  br label %393

; <label>:393                                     ; preds = %392
  br label %394

; <label>:394                                     ; preds = %393
  store %struct.hash_tbl* null, %struct.hash_tbl** %result, align 8
  %395 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %396 = icmp ne %struct.hash_tbl* %395, null
  br i1 %396, label %397, label %502

; <label>:397                                     ; preds = %394
  br label %398

; <label>:398                                     ; preds = %397
  %399 = load i32, i32* %_hf_hashv, align 4
  %400 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %401 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %400, i32 0, i32 2
  %402 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %401, i32 0, i32 0
  %403 = load %struct.UT_hash_table*, %struct.UT_hash_table** %402, align 8
  %404 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %403, i32 0, i32 1
  %405 = load i32, i32* %404, align 8
  %406 = sub i32 %405, 1
  %407 = and i32 %399, %406
  store i32 %407, i32* %_hf_bkt, align 4
  br label %408

; <label>:408                                     ; preds = %398
  br label %409

; <label>:409                                     ; preds = %408
  %410 = load i32, i32* %_hf_bkt, align 4
  %411 = zext i32 %410 to i64
  %412 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %413 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %412, i32 0, i32 2
  %414 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %413, i32 0, i32 0
  %415 = load %struct.UT_hash_table*, %struct.UT_hash_table** %414, align 8
  %416 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %415, i32 0, i32 0
  %417 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %416, align 8
  %418 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %417, i64 %411
  %419 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %418, i32 0, i32 0
  %420 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %419, align 8
  %421 = icmp ne %struct.UT_hash_handle* %420, null
  br i1 %421, label %422, label %446

; <label>:422                                     ; preds = %409
  br label %423

; <label>:423                                     ; preds = %422
  %424 = load i32, i32* %_hf_bkt, align 4
  %425 = zext i32 %424 to i64
  %426 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %427 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %426, i32 0, i32 2
  %428 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %427, i32 0, i32 0
  %429 = load %struct.UT_hash_table*, %struct.UT_hash_table** %428, align 8
  %430 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %429, i32 0, i32 0
  %431 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %430, align 8
  %432 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %431, i64 %425
  %433 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %432, i32 0, i32 0
  %434 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %433, align 8
  %435 = bitcast %struct.UT_hash_handle* %434 to i8*
  %436 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %437 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %436, i32 0, i32 2
  %438 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %437, i32 0, i32 0
  %439 = load %struct.UT_hash_table*, %struct.UT_hash_table** %438, align 8
  %440 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %439, i32 0, i32 5
  %441 = load i64, i64* %440, align 8
  %442 = sub i64 0, %441
  %443 = getelementptr inbounds i8, i8* %435, i64 %442
  %444 = bitcast i8* %443 to %struct.hash_tbl*
  store %struct.hash_tbl* %444, %struct.hash_tbl** %result, align 8
  br label %445

; <label>:445                                     ; preds = %423
  br label %447

; <label>:446                                     ; preds = %409
  store %struct.hash_tbl* null, %struct.hash_tbl** %result, align 8
  br label %447

; <label>:447                                     ; preds = %446, %445
  br label %448

; <label>:448                                     ; preds = %499, %447
  %449 = load %struct.hash_tbl*, %struct.hash_tbl** %result, align 8
  %450 = icmp ne %struct.hash_tbl* %449, null
  br i1 %450, label %451, label %500

; <label>:451                                     ; preds = %448
  %452 = load %struct.hash_tbl*, %struct.hash_tbl** %result, align 8
  %453 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %452, i32 0, i32 2
  %454 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %453, i32 0, i32 7
  %455 = load i32, i32* %454, align 4
  %456 = load i32, i32* %_hf_hashv, align 4
  %457 = icmp eq i32 %455, %456
  br i1 %457, label %458, label %475

; <label>:458                                     ; preds = %451
  %459 = load %struct.hash_tbl*, %struct.hash_tbl** %result, align 8
  %460 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %459, i32 0, i32 2
  %461 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %460, i32 0, i32 6
  %462 = load i32, i32* %461, align 8
  %463 = zext i32 %462 to i64
  %464 = icmp eq i64 %463, 8
  br i1 %464, label %465, label %475

; <label>:465                                     ; preds = %458
  %466 = load %struct.hash_tbl*, %struct.hash_tbl** %result, align 8
  %467 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %466, i32 0, i32 2
  %468 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %467, i32 0, i32 5
  %469 = load i8*, i8** %468, align 8
  %470 = bitcast i64* %key to i8*
  %471 = call i32 @memcmp(i8* %469, i8* %470, i64 8) #11
  %472 = icmp eq i32 %471, 0
  br i1 %472, label %473, label %474

; <label>:473                                     ; preds = %465
  br label %500

; <label>:474                                     ; preds = %465
  br label %475

; <label>:475                                     ; preds = %474, %458, %451
  %476 = load %struct.hash_tbl*, %struct.hash_tbl** %result, align 8
  %477 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %476, i32 0, i32 2
  %478 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %477, i32 0, i32 4
  %479 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %478, align 8
  %480 = icmp ne %struct.UT_hash_handle* %479, null
  br i1 %480, label %481, label %498

; <label>:481                                     ; preds = %475
  br label %482

; <label>:482                                     ; preds = %481
  %483 = load %struct.hash_tbl*, %struct.hash_tbl** %result, align 8
  %484 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %483, i32 0, i32 2
  %485 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %484, i32 0, i32 4
  %486 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %485, align 8
  %487 = bitcast %struct.UT_hash_handle* %486 to i8*
  %488 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %489 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %488, i32 0, i32 2
  %490 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %489, i32 0, i32 0
  %491 = load %struct.UT_hash_table*, %struct.UT_hash_table** %490, align 8
  %492 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %491, i32 0, i32 5
  %493 = load i64, i64* %492, align 8
  %494 = sub i64 0, %493
  %495 = getelementptr inbounds i8, i8* %487, i64 %494
  %496 = bitcast i8* %495 to %struct.hash_tbl*
  store %struct.hash_tbl* %496, %struct.hash_tbl** %result, align 8
  br label %497

; <label>:497                                     ; preds = %482
  br label %499

; <label>:498                                     ; preds = %475
  store %struct.hash_tbl* null, %struct.hash_tbl** %result, align 8
  br label %499

; <label>:499                                     ; preds = %498, %497
  br label %448

; <label>:500                                     ; preds = %473, %448
  br label %501

; <label>:501                                     ; preds = %500
  br label %502

; <label>:502                                     ; preds = %501, %394
  br label %503

; <label>:503                                     ; preds = %502
  br label %504

; <label>:504                                     ; preds = %503
  %505 = load %struct.hash_tbl*, %struct.hash_tbl** %result, align 8
  %506 = icmp ne %struct.hash_tbl* %505, null
  br i1 %506, label %507, label %511

; <label>:507                                     ; preds = %504
  %508 = load i64, i64* %pair, align 8
  %509 = load %struct.hash_tbl*, %struct.hash_tbl** %result, align 8
  %510 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %509, i32 0, i32 1
  store i64 %508, i64* %510, align 8
  br label %1354

; <label>:511                                     ; preds = %504
  br label %512

; <label>:512                                     ; preds = %511
  br label %513

; <label>:513                                     ; preds = %512
  br label %514

; <label>:514                                     ; preds = %513
  %515 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %516 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %515, i32 0, i32 0
  %517 = bitcast i64* %516 to i8*
  store i8* %517, i8** %_hj_key4, align 8
  store i32 -17973521, i32* %_ha_hashv, align 4
  store i32 -1640531527, i32* %_hj_j2, align 4
  store i32 -1640531527, i32* %_hj_i1, align 4
  store i32 8, i32* %_hj_k3, align 4
  br label %518

; <label>:518                                     ; preds = %685, %514
  %519 = load i32, i32* %_hj_k3, align 4
  %520 = icmp uge i32 %519, 12
  br i1 %520, label %521, label %690

; <label>:521                                     ; preds = %518
  %522 = load i8*, i8** %_hj_key4, align 8
  %523 = getelementptr inbounds i8, i8* %522, i64 0
  %524 = load i8, i8* %523, align 1
  %525 = zext i8 %524 to i32
  %526 = load i8*, i8** %_hj_key4, align 8
  %527 = getelementptr inbounds i8, i8* %526, i64 1
  %528 = load i8, i8* %527, align 1
  %529 = zext i8 %528 to i32
  %530 = shl i32 %529, 8
  %531 = add i32 %525, %530
  %532 = load i8*, i8** %_hj_key4, align 8
  %533 = getelementptr inbounds i8, i8* %532, i64 2
  %534 = load i8, i8* %533, align 1
  %535 = zext i8 %534 to i32
  %536 = shl i32 %535, 16
  %537 = add i32 %531, %536
  %538 = load i8*, i8** %_hj_key4, align 8
  %539 = getelementptr inbounds i8, i8* %538, i64 3
  %540 = load i8, i8* %539, align 1
  %541 = zext i8 %540 to i32
  %542 = shl i32 %541, 24
  %543 = add i32 %537, %542
  %544 = load i32, i32* %_hj_i1, align 4
  %545 = add i32 %544, %543
  store i32 %545, i32* %_hj_i1, align 4
  %546 = load i8*, i8** %_hj_key4, align 8
  %547 = getelementptr inbounds i8, i8* %546, i64 4
  %548 = load i8, i8* %547, align 1
  %549 = zext i8 %548 to i32
  %550 = load i8*, i8** %_hj_key4, align 8
  %551 = getelementptr inbounds i8, i8* %550, i64 5
  %552 = load i8, i8* %551, align 1
  %553 = zext i8 %552 to i32
  %554 = shl i32 %553, 8
  %555 = add i32 %549, %554
  %556 = load i8*, i8** %_hj_key4, align 8
  %557 = getelementptr inbounds i8, i8* %556, i64 6
  %558 = load i8, i8* %557, align 1
  %559 = zext i8 %558 to i32
  %560 = shl i32 %559, 16
  %561 = add i32 %555, %560
  %562 = load i8*, i8** %_hj_key4, align 8
  %563 = getelementptr inbounds i8, i8* %562, i64 7
  %564 = load i8, i8* %563, align 1
  %565 = zext i8 %564 to i32
  %566 = shl i32 %565, 24
  %567 = add i32 %561, %566
  %568 = load i32, i32* %_hj_j2, align 4
  %569 = add i32 %568, %567
  store i32 %569, i32* %_hj_j2, align 4
  %570 = load i8*, i8** %_hj_key4, align 8
  %571 = getelementptr inbounds i8, i8* %570, i64 8
  %572 = load i8, i8* %571, align 1
  %573 = zext i8 %572 to i32
  %574 = load i8*, i8** %_hj_key4, align 8
  %575 = getelementptr inbounds i8, i8* %574, i64 9
  %576 = load i8, i8* %575, align 1
  %577 = zext i8 %576 to i32
  %578 = shl i32 %577, 8
  %579 = add i32 %573, %578
  %580 = load i8*, i8** %_hj_key4, align 8
  %581 = getelementptr inbounds i8, i8* %580, i64 10
  %582 = load i8, i8* %581, align 1
  %583 = zext i8 %582 to i32
  %584 = shl i32 %583, 16
  %585 = add i32 %579, %584
  %586 = load i8*, i8** %_hj_key4, align 8
  %587 = getelementptr inbounds i8, i8* %586, i64 11
  %588 = load i8, i8* %587, align 1
  %589 = zext i8 %588 to i32
  %590 = shl i32 %589, 24
  %591 = add i32 %585, %590
  %592 = load i32, i32* %_ha_hashv, align 4
  %593 = add i32 %592, %591
  store i32 %593, i32* %_ha_hashv, align 4
  br label %594

; <label>:594                                     ; preds = %521
  %595 = load i32, i32* %_hj_j2, align 4
  %596 = load i32, i32* %_hj_i1, align 4
  %597 = sub i32 %596, %595
  store i32 %597, i32* %_hj_i1, align 4
  %598 = load i32, i32* %_ha_hashv, align 4
  %599 = load i32, i32* %_hj_i1, align 4
  %600 = sub i32 %599, %598
  store i32 %600, i32* %_hj_i1, align 4
  %601 = load i32, i32* %_ha_hashv, align 4
  %602 = lshr i32 %601, 13
  %603 = load i32, i32* %_hj_i1, align 4
  %604 = xor i32 %603, %602
  store i32 %604, i32* %_hj_i1, align 4
  %605 = load i32, i32* %_ha_hashv, align 4
  %606 = load i32, i32* %_hj_j2, align 4
  %607 = sub i32 %606, %605
  store i32 %607, i32* %_hj_j2, align 4
  %608 = load i32, i32* %_hj_i1, align 4
  %609 = load i32, i32* %_hj_j2, align 4
  %610 = sub i32 %609, %608
  store i32 %610, i32* %_hj_j2, align 4
  %611 = load i32, i32* %_hj_i1, align 4
  %612 = shl i32 %611, 8
  %613 = load i32, i32* %_hj_j2, align 4
  %614 = xor i32 %613, %612
  store i32 %614, i32* %_hj_j2, align 4
  %615 = load i32, i32* %_hj_i1, align 4
  %616 = load i32, i32* %_ha_hashv, align 4
  %617 = sub i32 %616, %615
  store i32 %617, i32* %_ha_hashv, align 4
  %618 = load i32, i32* %_hj_j2, align 4
  %619 = load i32, i32* %_ha_hashv, align 4
  %620 = sub i32 %619, %618
  store i32 %620, i32* %_ha_hashv, align 4
  %621 = load i32, i32* %_hj_j2, align 4
  %622 = lshr i32 %621, 13
  %623 = load i32, i32* %_ha_hashv, align 4
  %624 = xor i32 %623, %622
  store i32 %624, i32* %_ha_hashv, align 4
  %625 = load i32, i32* %_hj_j2, align 4
  %626 = load i32, i32* %_hj_i1, align 4
  %627 = sub i32 %626, %625
  store i32 %627, i32* %_hj_i1, align 4
  %628 = load i32, i32* %_ha_hashv, align 4
  %629 = load i32, i32* %_hj_i1, align 4
  %630 = sub i32 %629, %628
  store i32 %630, i32* %_hj_i1, align 4
  %631 = load i32, i32* %_ha_hashv, align 4
  %632 = lshr i32 %631, 12
  %633 = load i32, i32* %_hj_i1, align 4
  %634 = xor i32 %633, %632
  store i32 %634, i32* %_hj_i1, align 4
  %635 = load i32, i32* %_ha_hashv, align 4
  %636 = load i32, i32* %_hj_j2, align 4
  %637 = sub i32 %636, %635
  store i32 %637, i32* %_hj_j2, align 4
  %638 = load i32, i32* %_hj_i1, align 4
  %639 = load i32, i32* %_hj_j2, align 4
  %640 = sub i32 %639, %638
  store i32 %640, i32* %_hj_j2, align 4
  %641 = load i32, i32* %_hj_i1, align 4
  %642 = shl i32 %641, 16
  %643 = load i32, i32* %_hj_j2, align 4
  %644 = xor i32 %643, %642
  store i32 %644, i32* %_hj_j2, align 4
  %645 = load i32, i32* %_hj_i1, align 4
  %646 = load i32, i32* %_ha_hashv, align 4
  %647 = sub i32 %646, %645
  store i32 %647, i32* %_ha_hashv, align 4
  %648 = load i32, i32* %_hj_j2, align 4
  %649 = load i32, i32* %_ha_hashv, align 4
  %650 = sub i32 %649, %648
  store i32 %650, i32* %_ha_hashv, align 4
  %651 = load i32, i32* %_hj_j2, align 4
  %652 = lshr i32 %651, 5
  %653 = load i32, i32* %_ha_hashv, align 4
  %654 = xor i32 %653, %652
  store i32 %654, i32* %_ha_hashv, align 4
  %655 = load i32, i32* %_hj_j2, align 4
  %656 = load i32, i32* %_hj_i1, align 4
  %657 = sub i32 %656, %655
  store i32 %657, i32* %_hj_i1, align 4
  %658 = load i32, i32* %_ha_hashv, align 4
  %659 = load i32, i32* %_hj_i1, align 4
  %660 = sub i32 %659, %658
  store i32 %660, i32* %_hj_i1, align 4
  %661 = load i32, i32* %_ha_hashv, align 4
  %662 = lshr i32 %661, 3
  %663 = load i32, i32* %_hj_i1, align 4
  %664 = xor i32 %663, %662
  store i32 %664, i32* %_hj_i1, align 4
  %665 = load i32, i32* %_ha_hashv, align 4
  %666 = load i32, i32* %_hj_j2, align 4
  %667 = sub i32 %666, %665
  store i32 %667, i32* %_hj_j2, align 4
  %668 = load i32, i32* %_hj_i1, align 4
  %669 = load i32, i32* %_hj_j2, align 4
  %670 = sub i32 %669, %668
  store i32 %670, i32* %_hj_j2, align 4
  %671 = load i32, i32* %_hj_i1, align 4
  %672 = shl i32 %671, 10
  %673 = load i32, i32* %_hj_j2, align 4
  %674 = xor i32 %673, %672
  store i32 %674, i32* %_hj_j2, align 4
  %675 = load i32, i32* %_hj_i1, align 4
  %676 = load i32, i32* %_ha_hashv, align 4
  %677 = sub i32 %676, %675
  store i32 %677, i32* %_ha_hashv, align 4
  %678 = load i32, i32* %_hj_j2, align 4
  %679 = load i32, i32* %_ha_hashv, align 4
  %680 = sub i32 %679, %678
  store i32 %680, i32* %_ha_hashv, align 4
  %681 = load i32, i32* %_hj_j2, align 4
  %682 = lshr i32 %681, 15
  %683 = load i32, i32* %_ha_hashv, align 4
  %684 = xor i32 %683, %682
  store i32 %684, i32* %_ha_hashv, align 4
  br label %685

; <label>:685                                     ; preds = %594
  %686 = load i8*, i8** %_hj_key4, align 8
  %687 = getelementptr inbounds i8, i8* %686, i64 12
  store i8* %687, i8** %_hj_key4, align 8
  %688 = load i32, i32* %_hj_k3, align 4
  %689 = sub i32 %688, 12
  store i32 %689, i32* %_hj_k3, align 4
  br label %518

; <label>:690                                     ; preds = %518
  %691 = load i32, i32* %_ha_hashv, align 4
  %692 = add i32 %691, 8
  store i32 %692, i32* %_ha_hashv, align 4
  %693 = load i32, i32* %_hj_k3, align 4
  switch i32 %693, label %780 [
    i32 11, label %694
    i32 10, label %702
    i32 9, label %710
    i32 8, label %718
    i32 7, label %726
    i32 6, label %734
    i32 5, label %742
    i32 4, label %749
    i32 3, label %757
    i32 2, label %765
    i32 1, label %773
  ]

; <label>:694                                     ; preds = %690
  %695 = load i8*, i8** %_hj_key4, align 8
  %696 = getelementptr inbounds i8, i8* %695, i64 10
  %697 = load i8, i8* %696, align 1
  %698 = zext i8 %697 to i32
  %699 = shl i32 %698, 24
  %700 = load i32, i32* %_ha_hashv, align 4
  %701 = add i32 %700, %699
  store i32 %701, i32* %_ha_hashv, align 4
  br label %702

; <label>:702                                     ; preds = %690, %694
  %703 = load i8*, i8** %_hj_key4, align 8
  %704 = getelementptr inbounds i8, i8* %703, i64 9
  %705 = load i8, i8* %704, align 1
  %706 = zext i8 %705 to i32
  %707 = shl i32 %706, 16
  %708 = load i32, i32* %_ha_hashv, align 4
  %709 = add i32 %708, %707
  store i32 %709, i32* %_ha_hashv, align 4
  br label %710

; <label>:710                                     ; preds = %690, %702
  %711 = load i8*, i8** %_hj_key4, align 8
  %712 = getelementptr inbounds i8, i8* %711, i64 8
  %713 = load i8, i8* %712, align 1
  %714 = zext i8 %713 to i32
  %715 = shl i32 %714, 8
  %716 = load i32, i32* %_ha_hashv, align 4
  %717 = add i32 %716, %715
  store i32 %717, i32* %_ha_hashv, align 4
  br label %718

; <label>:718                                     ; preds = %690, %710
  %719 = load i8*, i8** %_hj_key4, align 8
  %720 = getelementptr inbounds i8, i8* %719, i64 7
  %721 = load i8, i8* %720, align 1
  %722 = zext i8 %721 to i32
  %723 = shl i32 %722, 24
  %724 = load i32, i32* %_hj_j2, align 4
  %725 = add i32 %724, %723
  store i32 %725, i32* %_hj_j2, align 4
  br label %726

; <label>:726                                     ; preds = %690, %718
  %727 = load i8*, i8** %_hj_key4, align 8
  %728 = getelementptr inbounds i8, i8* %727, i64 6
  %729 = load i8, i8* %728, align 1
  %730 = zext i8 %729 to i32
  %731 = shl i32 %730, 16
  %732 = load i32, i32* %_hj_j2, align 4
  %733 = add i32 %732, %731
  store i32 %733, i32* %_hj_j2, align 4
  br label %734

; <label>:734                                     ; preds = %690, %726
  %735 = load i8*, i8** %_hj_key4, align 8
  %736 = getelementptr inbounds i8, i8* %735, i64 5
  %737 = load i8, i8* %736, align 1
  %738 = zext i8 %737 to i32
  %739 = shl i32 %738, 8
  %740 = load i32, i32* %_hj_j2, align 4
  %741 = add i32 %740, %739
  store i32 %741, i32* %_hj_j2, align 4
  br label %742

; <label>:742                                     ; preds = %690, %734
  %743 = load i8*, i8** %_hj_key4, align 8
  %744 = getelementptr inbounds i8, i8* %743, i64 4
  %745 = load i8, i8* %744, align 1
  %746 = zext i8 %745 to i32
  %747 = load i32, i32* %_hj_j2, align 4
  %748 = add i32 %747, %746
  store i32 %748, i32* %_hj_j2, align 4
  br label %749

; <label>:749                                     ; preds = %690, %742
  %750 = load i8*, i8** %_hj_key4, align 8
  %751 = getelementptr inbounds i8, i8* %750, i64 3
  %752 = load i8, i8* %751, align 1
  %753 = zext i8 %752 to i32
  %754 = shl i32 %753, 24
  %755 = load i32, i32* %_hj_i1, align 4
  %756 = add i32 %755, %754
  store i32 %756, i32* %_hj_i1, align 4
  br label %757

; <label>:757                                     ; preds = %690, %749
  %758 = load i8*, i8** %_hj_key4, align 8
  %759 = getelementptr inbounds i8, i8* %758, i64 2
  %760 = load i8, i8* %759, align 1
  %761 = zext i8 %760 to i32
  %762 = shl i32 %761, 16
  %763 = load i32, i32* %_hj_i1, align 4
  %764 = add i32 %763, %762
  store i32 %764, i32* %_hj_i1, align 4
  br label %765

; <label>:765                                     ; preds = %690, %757
  %766 = load i8*, i8** %_hj_key4, align 8
  %767 = getelementptr inbounds i8, i8* %766, i64 1
  %768 = load i8, i8* %767, align 1
  %769 = zext i8 %768 to i32
  %770 = shl i32 %769, 8
  %771 = load i32, i32* %_hj_i1, align 4
  %772 = add i32 %771, %770
  store i32 %772, i32* %_hj_i1, align 4
  br label %773

; <label>:773                                     ; preds = %690, %765
  %774 = load i8*, i8** %_hj_key4, align 8
  %775 = getelementptr inbounds i8, i8* %774, i64 0
  %776 = load i8, i8* %775, align 1
  %777 = zext i8 %776 to i32
  %778 = load i32, i32* %_hj_i1, align 4
  %779 = add i32 %778, %777
  store i32 %779, i32* %_hj_i1, align 4
  br label %780

; <label>:780                                     ; preds = %773, %690
  br label %781

; <label>:781                                     ; preds = %780
  %782 = load i32, i32* %_hj_j2, align 4
  %783 = load i32, i32* %_hj_i1, align 4
  %784 = sub i32 %783, %782
  store i32 %784, i32* %_hj_i1, align 4
  %785 = load i32, i32* %_ha_hashv, align 4
  %786 = load i32, i32* %_hj_i1, align 4
  %787 = sub i32 %786, %785
  store i32 %787, i32* %_hj_i1, align 4
  %788 = load i32, i32* %_ha_hashv, align 4
  %789 = lshr i32 %788, 13
  %790 = load i32, i32* %_hj_i1, align 4
  %791 = xor i32 %790, %789
  store i32 %791, i32* %_hj_i1, align 4
  %792 = load i32, i32* %_ha_hashv, align 4
  %793 = load i32, i32* %_hj_j2, align 4
  %794 = sub i32 %793, %792
  store i32 %794, i32* %_hj_j2, align 4
  %795 = load i32, i32* %_hj_i1, align 4
  %796 = load i32, i32* %_hj_j2, align 4
  %797 = sub i32 %796, %795
  store i32 %797, i32* %_hj_j2, align 4
  %798 = load i32, i32* %_hj_i1, align 4
  %799 = shl i32 %798, 8
  %800 = load i32, i32* %_hj_j2, align 4
  %801 = xor i32 %800, %799
  store i32 %801, i32* %_hj_j2, align 4
  %802 = load i32, i32* %_hj_i1, align 4
  %803 = load i32, i32* %_ha_hashv, align 4
  %804 = sub i32 %803, %802
  store i32 %804, i32* %_ha_hashv, align 4
  %805 = load i32, i32* %_hj_j2, align 4
  %806 = load i32, i32* %_ha_hashv, align 4
  %807 = sub i32 %806, %805
  store i32 %807, i32* %_ha_hashv, align 4
  %808 = load i32, i32* %_hj_j2, align 4
  %809 = lshr i32 %808, 13
  %810 = load i32, i32* %_ha_hashv, align 4
  %811 = xor i32 %810, %809
  store i32 %811, i32* %_ha_hashv, align 4
  %812 = load i32, i32* %_hj_j2, align 4
  %813 = load i32, i32* %_hj_i1, align 4
  %814 = sub i32 %813, %812
  store i32 %814, i32* %_hj_i1, align 4
  %815 = load i32, i32* %_ha_hashv, align 4
  %816 = load i32, i32* %_hj_i1, align 4
  %817 = sub i32 %816, %815
  store i32 %817, i32* %_hj_i1, align 4
  %818 = load i32, i32* %_ha_hashv, align 4
  %819 = lshr i32 %818, 12
  %820 = load i32, i32* %_hj_i1, align 4
  %821 = xor i32 %820, %819
  store i32 %821, i32* %_hj_i1, align 4
  %822 = load i32, i32* %_ha_hashv, align 4
  %823 = load i32, i32* %_hj_j2, align 4
  %824 = sub i32 %823, %822
  store i32 %824, i32* %_hj_j2, align 4
  %825 = load i32, i32* %_hj_i1, align 4
  %826 = load i32, i32* %_hj_j2, align 4
  %827 = sub i32 %826, %825
  store i32 %827, i32* %_hj_j2, align 4
  %828 = load i32, i32* %_hj_i1, align 4
  %829 = shl i32 %828, 16
  %830 = load i32, i32* %_hj_j2, align 4
  %831 = xor i32 %830, %829
  store i32 %831, i32* %_hj_j2, align 4
  %832 = load i32, i32* %_hj_i1, align 4
  %833 = load i32, i32* %_ha_hashv, align 4
  %834 = sub i32 %833, %832
  store i32 %834, i32* %_ha_hashv, align 4
  %835 = load i32, i32* %_hj_j2, align 4
  %836 = load i32, i32* %_ha_hashv, align 4
  %837 = sub i32 %836, %835
  store i32 %837, i32* %_ha_hashv, align 4
  %838 = load i32, i32* %_hj_j2, align 4
  %839 = lshr i32 %838, 5
  %840 = load i32, i32* %_ha_hashv, align 4
  %841 = xor i32 %840, %839
  store i32 %841, i32* %_ha_hashv, align 4
  %842 = load i32, i32* %_hj_j2, align 4
  %843 = load i32, i32* %_hj_i1, align 4
  %844 = sub i32 %843, %842
  store i32 %844, i32* %_hj_i1, align 4
  %845 = load i32, i32* %_ha_hashv, align 4
  %846 = load i32, i32* %_hj_i1, align 4
  %847 = sub i32 %846, %845
  store i32 %847, i32* %_hj_i1, align 4
  %848 = load i32, i32* %_ha_hashv, align 4
  %849 = lshr i32 %848, 3
  %850 = load i32, i32* %_hj_i1, align 4
  %851 = xor i32 %850, %849
  store i32 %851, i32* %_hj_i1, align 4
  %852 = load i32, i32* %_ha_hashv, align 4
  %853 = load i32, i32* %_hj_j2, align 4
  %854 = sub i32 %853, %852
  store i32 %854, i32* %_hj_j2, align 4
  %855 = load i32, i32* %_hj_i1, align 4
  %856 = load i32, i32* %_hj_j2, align 4
  %857 = sub i32 %856, %855
  store i32 %857, i32* %_hj_j2, align 4
  %858 = load i32, i32* %_hj_i1, align 4
  %859 = shl i32 %858, 10
  %860 = load i32, i32* %_hj_j2, align 4
  %861 = xor i32 %860, %859
  store i32 %861, i32* %_hj_j2, align 4
  %862 = load i32, i32* %_hj_i1, align 4
  %863 = load i32, i32* %_ha_hashv, align 4
  %864 = sub i32 %863, %862
  store i32 %864, i32* %_ha_hashv, align 4
  %865 = load i32, i32* %_hj_j2, align 4
  %866 = load i32, i32* %_ha_hashv, align 4
  %867 = sub i32 %866, %865
  store i32 %867, i32* %_ha_hashv, align 4
  %868 = load i32, i32* %_hj_j2, align 4
  %869 = lshr i32 %868, 15
  %870 = load i32, i32* %_ha_hashv, align 4
  %871 = xor i32 %870, %869
  store i32 %871, i32* %_ha_hashv, align 4
  br label %872

; <label>:872                                     ; preds = %781
  br label %873

; <label>:873                                     ; preds = %872
  br label %874

; <label>:874                                     ; preds = %873
  br label %875

; <label>:875                                     ; preds = %874
  %876 = load i32, i32* %_ha_hashv, align 4
  %877 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %878 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %877, i32 0, i32 2
  %879 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %878, i32 0, i32 7
  store i32 %876, i32* %879, align 4
  %880 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %881 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %880, i32 0, i32 0
  %882 = bitcast i64* %881 to i8*
  %883 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %884 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %883, i32 0, i32 2
  %885 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %884, i32 0, i32 5
  store i8* %882, i8** %885, align 8
  %886 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %887 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %886, i32 0, i32 2
  %888 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %887, i32 0, i32 6
  store i32 8, i32* %888, align 8
  %889 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %890 = icmp ne %struct.hash_tbl* %889, null
  br i1 %890, label %978, label %891

; <label>:891                                     ; preds = %875
  %892 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %893 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %892, i32 0, i32 2
  %894 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %893, i32 0, i32 2
  store i8* null, i8** %894, align 8
  %895 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %896 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %895, i32 0, i32 2
  %897 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %896, i32 0, i32 1
  store i8* null, i8** %897, align 8
  br label %898

; <label>:898                                     ; preds = %891
  %899 = call noalias i8* @malloc(i64 64) #2
  %900 = bitcast i8* %899 to %struct.UT_hash_table*
  %901 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %902 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %901, i32 0, i32 2
  %903 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %902, i32 0, i32 0
  store %struct.UT_hash_table* %900, %struct.UT_hash_table** %903, align 8
  %904 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %905 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %904, i32 0, i32 2
  %906 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %905, i32 0, i32 0
  %907 = load %struct.UT_hash_table*, %struct.UT_hash_table** %906, align 8
  %908 = icmp ne %struct.UT_hash_table* %907, null
  br i1 %908, label %910, label %909

; <label>:909                                     ; preds = %898
  call void @exit(i32 -1) #9
  unreachable

; <label>:910                                     ; preds = %898
  %911 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %912 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %911, i32 0, i32 2
  %913 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %912, i32 0, i32 0
  %914 = load %struct.UT_hash_table*, %struct.UT_hash_table** %913, align 8
  %915 = bitcast %struct.UT_hash_table* %914 to i8*
  call void @llvm.memset.p0i8.i64(i8* %915, i8 0, i64 64, i32 8, i1 false)
  %916 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %917 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %916, i32 0, i32 2
  %918 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %919 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %918, i32 0, i32 2
  %920 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %919, i32 0, i32 0
  %921 = load %struct.UT_hash_table*, %struct.UT_hash_table** %920, align 8
  %922 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %921, i32 0, i32 4
  store %struct.UT_hash_handle* %917, %struct.UT_hash_handle** %922, align 8
  %923 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %924 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %923, i32 0, i32 2
  %925 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %924, i32 0, i32 0
  %926 = load %struct.UT_hash_table*, %struct.UT_hash_table** %925, align 8
  %927 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %926, i32 0, i32 1
  store i32 32, i32* %927, align 8
  %928 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %929 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %928, i32 0, i32 2
  %930 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %929, i32 0, i32 0
  %931 = load %struct.UT_hash_table*, %struct.UT_hash_table** %930, align 8
  %932 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %931, i32 0, i32 2
  store i32 5, i32* %932, align 4
  %933 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %934 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %933, i32 0, i32 2
  %935 = bitcast %struct.UT_hash_handle* %934 to i8*
  %936 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %937 = bitcast %struct.hash_tbl* %936 to i8*
  %938 = ptrtoint i8* %935 to i64
  %939 = ptrtoint i8* %937 to i64
  %940 = sub i64 %938, %939
  %941 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %942 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %941, i32 0, i32 2
  %943 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %942, i32 0, i32 0
  %944 = load %struct.UT_hash_table*, %struct.UT_hash_table** %943, align 8
  %945 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %944, i32 0, i32 5
  store i64 %940, i64* %945, align 8
  %946 = call noalias i8* @malloc(i64 512) #2
  %947 = bitcast i8* %946 to %struct.UT_hash_bucket*
  %948 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %949 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %948, i32 0, i32 2
  %950 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %949, i32 0, i32 0
  %951 = load %struct.UT_hash_table*, %struct.UT_hash_table** %950, align 8
  %952 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %951, i32 0, i32 0
  store %struct.UT_hash_bucket* %947, %struct.UT_hash_bucket** %952, align 8
  %953 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %954 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %953, i32 0, i32 2
  %955 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %954, i32 0, i32 0
  %956 = load %struct.UT_hash_table*, %struct.UT_hash_table** %955, align 8
  %957 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %956, i32 0, i32 10
  store i32 -1609490463, i32* %957, align 8
  %958 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %959 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %958, i32 0, i32 2
  %960 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %959, i32 0, i32 0
  %961 = load %struct.UT_hash_table*, %struct.UT_hash_table** %960, align 8
  %962 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %961, i32 0, i32 0
  %963 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %962, align 8
  %964 = icmp ne %struct.UT_hash_bucket* %963, null
  br i1 %964, label %966, label %965

; <label>:965                                     ; preds = %910
  call void @exit(i32 -1) #9
  unreachable

; <label>:966                                     ; preds = %910
  %967 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %968 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %967, i32 0, i32 2
  %969 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %968, i32 0, i32 0
  %970 = load %struct.UT_hash_table*, %struct.UT_hash_table** %969, align 8
  %971 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %970, i32 0, i32 0
  %972 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %971, align 8
  %973 = bitcast %struct.UT_hash_bucket* %972 to i8*
  call void @llvm.memset.p0i8.i64(i8* %973, i8 0, i64 512, i32 8, i1 false)
  br label %974

; <label>:974                                     ; preds = %966
  br label %975

; <label>:975                                     ; preds = %974
  br label %976

; <label>:976                                     ; preds = %975
  %977 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  store %struct.hash_tbl* %977, %struct.hash_tbl** %new_table, align 8
  br label %1025

; <label>:978                                     ; preds = %875
  %979 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %980 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %979, i32 0, i32 2
  %981 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %980, i32 0, i32 0
  %982 = load %struct.UT_hash_table*, %struct.UT_hash_table** %981, align 8
  %983 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %984 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %983, i32 0, i32 2
  %985 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %984, i32 0, i32 0
  store %struct.UT_hash_table* %982, %struct.UT_hash_table** %985, align 8
  br label %986

; <label>:986                                     ; preds = %978
  %987 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %988 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %987, i32 0, i32 2
  %989 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %988, i32 0, i32 2
  store i8* null, i8** %989, align 8
  %990 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %991 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %990, i32 0, i32 2
  %992 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %991, i32 0, i32 0
  %993 = load %struct.UT_hash_table*, %struct.UT_hash_table** %992, align 8
  %994 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %993, i32 0, i32 4
  %995 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %994, align 8
  %996 = bitcast %struct.UT_hash_handle* %995 to i8*
  %997 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %998 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %997, i32 0, i32 2
  %999 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %998, i32 0, i32 0
  %1000 = load %struct.UT_hash_table*, %struct.UT_hash_table** %999, align 8
  %1001 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1000, i32 0, i32 5
  %1002 = load i64, i64* %1001, align 8
  %1003 = sub i64 0, %1002
  %1004 = getelementptr inbounds i8, i8* %996, i64 %1003
  %1005 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1006 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1005, i32 0, i32 2
  %1007 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1006, i32 0, i32 1
  store i8* %1004, i8** %1007, align 8
  %1008 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1009 = bitcast %struct.hash_tbl* %1008 to i8*
  %1010 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %1011 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1010, i32 0, i32 2
  %1012 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1011, i32 0, i32 0
  %1013 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1012, align 8
  %1014 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1013, i32 0, i32 4
  %1015 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1014, align 8
  %1016 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1015, i32 0, i32 2
  store i8* %1009, i8** %1016, align 8
  %1017 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1018 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1017, i32 0, i32 2
  %1019 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %1020 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1019, i32 0, i32 2
  %1021 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1020, i32 0, i32 0
  %1022 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1021, align 8
  %1023 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1022, i32 0, i32 4
  store %struct.UT_hash_handle* %1018, %struct.UT_hash_handle** %1023, align 8
  br label %1024

; <label>:1024                                    ; preds = %986
  br label %1025

; <label>:1025                                    ; preds = %1024, %976
  br label %1026

; <label>:1026                                    ; preds = %1025
  %1027 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %1028 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1027, i32 0, i32 2
  %1029 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1028, i32 0, i32 0
  %1030 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1029, align 8
  %1031 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1030, i32 0, i32 3
  %1032 = load i32, i32* %1031, align 8
  %1033 = add i32 %1032, 1
  store i32 %1033, i32* %1031, align 8
  br label %1034

; <label>:1034                                    ; preds = %1026
  %1035 = load i32, i32* %_ha_hashv, align 4
  %1036 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %1037 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1036, i32 0, i32 2
  %1038 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1037, i32 0, i32 0
  %1039 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1038, align 8
  %1040 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1039, i32 0, i32 1
  %1041 = load i32, i32* %1040, align 8
  %1042 = sub i32 %1041, 1
  %1043 = and i32 %1035, %1042
  store i32 %1043, i32* %_ha_bkt, align 4
  br label %1044

; <label>:1044                                    ; preds = %1034
  br label %1045

; <label>:1045                                    ; preds = %1044
  %1046 = load i32, i32* %_ha_bkt, align 4
  %1047 = zext i32 %1046 to i64
  %1048 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %1049 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1048, i32 0, i32 2
  %1050 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1049, i32 0, i32 0
  %1051 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1050, align 8
  %1052 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1051, i32 0, i32 0
  %1053 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %1052, align 8
  %1054 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1053, i64 %1047
  store %struct.UT_hash_bucket* %1054, %struct.UT_hash_bucket** %_ha_head, align 8
  %1055 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1056 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1055, i32 0, i32 1
  %1057 = load i32, i32* %1056, align 8
  %1058 = add i32 %1057, 1
  store i32 %1058, i32* %1056, align 8
  %1059 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1060 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1059, i32 0, i32 0
  %1061 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1060, align 8
  %1062 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1063 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1062, i32 0, i32 2
  %1064 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1063, i32 0, i32 4
  store %struct.UT_hash_handle* %1061, %struct.UT_hash_handle** %1064, align 8
  %1065 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1066 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1065, i32 0, i32 2
  %1067 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1066, i32 0, i32 3
  store %struct.UT_hash_handle* null, %struct.UT_hash_handle** %1067, align 8
  %1068 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1069 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1068, i32 0, i32 0
  %1070 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1069, align 8
  %1071 = icmp ne %struct.UT_hash_handle* %1070, null
  br i1 %1071, label %1072, label %1079

; <label>:1072                                    ; preds = %1045
  %1073 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1074 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1073, i32 0, i32 2
  %1075 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1076 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1075, i32 0, i32 0
  %1077 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1076, align 8
  %1078 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1077, i32 0, i32 3
  store %struct.UT_hash_handle* %1074, %struct.UT_hash_handle** %1078, align 8
  br label %1079

; <label>:1079                                    ; preds = %1072, %1045
  %1080 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1081 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1080, i32 0, i32 2
  %1082 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1083 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1082, i32 0, i32 0
  store %struct.UT_hash_handle* %1081, %struct.UT_hash_handle** %1083, align 8
  %1084 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1085 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1084, i32 0, i32 1
  %1086 = load i32, i32* %1085, align 8
  %1087 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1088 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1087, i32 0, i32 2
  %1089 = load i32, i32* %1088, align 4
  %1090 = add i32 %1089, 1
  %1091 = mul i32 %1090, 10
  %1092 = icmp uge i32 %1086, %1091
  br i1 %1092, label %1093, label %1349

; <label>:1093                                    ; preds = %1079
  %1094 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1095 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1094, i32 0, i32 2
  %1096 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1095, i32 0, i32 0
  %1097 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1096, align 8
  %1098 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1097, i32 0, i32 9
  %1099 = load i32, i32* %1098, align 4
  %1100 = icmp ne i32 %1099, 0
  br i1 %1100, label %1349, label %1101

; <label>:1101                                    ; preds = %1093
  br label %1102

; <label>:1102                                    ; preds = %1101
  %1103 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1104 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1103, i32 0, i32 2
  %1105 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1104, i32 0, i32 0
  %1106 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1105, align 8
  %1107 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1106, i32 0, i32 1
  %1108 = load i32, i32* %1107, align 8
  %1109 = zext i32 %1108 to i64
  %1110 = mul i64 2, %1109
  %1111 = mul i64 %1110, 16
  %1112 = call noalias i8* @malloc(i64 %1111) #2
  %1113 = bitcast i8* %1112 to %struct.UT_hash_bucket*
  store %struct.UT_hash_bucket* %1113, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %1114 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %1115 = icmp ne %struct.UT_hash_bucket* %1114, null
  br i1 %1115, label %1117, label %1116

; <label>:1116                                    ; preds = %1102
  call void @exit(i32 -1) #9
  unreachable

; <label>:1117                                    ; preds = %1102
  %1118 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %1119 = bitcast %struct.UT_hash_bucket* %1118 to i8*
  %1120 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1121 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1120, i32 0, i32 2
  %1122 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1121, i32 0, i32 0
  %1123 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1122, align 8
  %1124 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1123, i32 0, i32 1
  %1125 = load i32, i32* %1124, align 8
  %1126 = zext i32 %1125 to i64
  %1127 = mul i64 2, %1126
  %1128 = mul i64 %1127, 16
  call void @llvm.memset.p0i8.i64(i8* %1119, i8 0, i64 %1128, i32 8, i1 false)
  %1129 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1130 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1129, i32 0, i32 2
  %1131 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1130, i32 0, i32 0
  %1132 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1131, align 8
  %1133 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1132, i32 0, i32 3
  %1134 = load i32, i32* %1133, align 8
  %1135 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1136 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1135, i32 0, i32 2
  %1137 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1136, i32 0, i32 0
  %1138 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1137, align 8
  %1139 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1138, i32 0, i32 2
  %1140 = load i32, i32* %1139, align 4
  %1141 = add i32 %1140, 1
  %1142 = lshr i32 %1134, %1141
  %1143 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1144 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1143, i32 0, i32 2
  %1145 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1144, i32 0, i32 0
  %1146 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1145, align 8
  %1147 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1146, i32 0, i32 3
  %1148 = load i32, i32* %1147, align 8
  %1149 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1150 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1149, i32 0, i32 2
  %1151 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1150, i32 0, i32 0
  %1152 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1151, align 8
  %1153 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1152, i32 0, i32 1
  %1154 = load i32, i32* %1153, align 8
  %1155 = mul i32 %1154, 2
  %1156 = sub i32 %1155, 1
  %1157 = and i32 %1148, %1156
  %1158 = icmp ne i32 %1157, 0
  %1159 = select i1 %1158, i32 1, i32 0
  %1160 = add i32 %1142, %1159
  %1161 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1162 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1161, i32 0, i32 2
  %1163 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1162, i32 0, i32 0
  %1164 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1163, align 8
  %1165 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1164, i32 0, i32 6
  store i32 %1160, i32* %1165, align 8
  %1166 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1167 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1166, i32 0, i32 2
  %1168 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1167, i32 0, i32 0
  %1169 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1168, align 8
  %1170 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1169, i32 0, i32 7
  store i32 0, i32* %1170, align 4
  store i32 0, i32* %_he_bkt_i, align 4
  br label %1171

; <label>:1171                                    ; preds = %1272, %1117
  %1172 = load i32, i32* %_he_bkt_i, align 4
  %1173 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1174 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1173, i32 0, i32 2
  %1175 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1174, i32 0, i32 0
  %1176 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1175, align 8
  %1177 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1176, i32 0, i32 1
  %1178 = load i32, i32* %1177, align 8
  %1179 = icmp ult i32 %1172, %1178
  br i1 %1179, label %1180, label %1275

; <label>:1180                                    ; preds = %1171
  %1181 = load i32, i32* %_he_bkt_i, align 4
  %1182 = zext i32 %1181 to i64
  %1183 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1184 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1183, i32 0, i32 2
  %1185 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1184, i32 0, i32 0
  %1186 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1185, align 8
  %1187 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1186, i32 0, i32 0
  %1188 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %1187, align 8
  %1189 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1188, i64 %1182
  %1190 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1189, i32 0, i32 0
  %1191 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1190, align 8
  store %struct.UT_hash_handle* %1191, %struct.UT_hash_handle** %_he_thh, align 8
  br label %1192

; <label>:1192                                    ; preds = %1266, %1180
  %1193 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1194 = icmp ne %struct.UT_hash_handle* %1193, null
  br i1 %1194, label %1195, label %1271

; <label>:1195                                    ; preds = %1192
  %1196 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1197 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1196, i32 0, i32 4
  %1198 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1197, align 8
  store %struct.UT_hash_handle* %1198, %struct.UT_hash_handle** %_he_hh_nxt, align 8
  br label %1199

; <label>:1199                                    ; preds = %1195
  %1200 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1201 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1200, i32 0, i32 7
  %1202 = load i32, i32* %1201, align 4
  %1203 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1204 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1203, i32 0, i32 2
  %1205 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1204, i32 0, i32 0
  %1206 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1205, align 8
  %1207 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1206, i32 0, i32 1
  %1208 = load i32, i32* %1207, align 8
  %1209 = mul i32 %1208, 2
  %1210 = sub i32 %1209, 1
  %1211 = and i32 %1202, %1210
  store i32 %1211, i32* %_he_bkt, align 4
  br label %1212

; <label>:1212                                    ; preds = %1199
  %1213 = load i32, i32* %_he_bkt, align 4
  %1214 = zext i32 %1213 to i64
  %1215 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %1216 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1215, i64 %1214
  store %struct.UT_hash_bucket* %1216, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1217 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1218 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1217, i32 0, i32 1
  %1219 = load i32, i32* %1218, align 8
  %1220 = add i32 %1219, 1
  store i32 %1220, i32* %1218, align 8
  %1221 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1222 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1221, i32 0, i32 2
  %1223 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1222, i32 0, i32 0
  %1224 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1223, align 8
  %1225 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1224, i32 0, i32 6
  %1226 = load i32, i32* %1225, align 8
  %1227 = icmp ugt i32 %1220, %1226
  br i1 %1227, label %1228, label %1248

; <label>:1228                                    ; preds = %1212
  %1229 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1230 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1229, i32 0, i32 2
  %1231 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1230, i32 0, i32 0
  %1232 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1231, align 8
  %1233 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1232, i32 0, i32 7
  %1234 = load i32, i32* %1233, align 4
  %1235 = add i32 %1234, 1
  store i32 %1235, i32* %1233, align 4
  %1236 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1237 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1236, i32 0, i32 1
  %1238 = load i32, i32* %1237, align 8
  %1239 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1240 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1239, i32 0, i32 2
  %1241 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1240, i32 0, i32 0
  %1242 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1241, align 8
  %1243 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1242, i32 0, i32 6
  %1244 = load i32, i32* %1243, align 8
  %1245 = udiv i32 %1238, %1244
  %1246 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1247 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1246, i32 0, i32 2
  store i32 %1245, i32* %1247, align 4
  br label %1248

; <label>:1248                                    ; preds = %1228, %1212
  %1249 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1250 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1249, i32 0, i32 3
  store %struct.UT_hash_handle* null, %struct.UT_hash_handle** %1250, align 8
  %1251 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1252 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1251, i32 0, i32 0
  %1253 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1252, align 8
  %1254 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1255 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1254, i32 0, i32 4
  store %struct.UT_hash_handle* %1253, %struct.UT_hash_handle** %1255, align 8
  %1256 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1257 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1256, i32 0, i32 0
  %1258 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1257, align 8
  %1259 = icmp ne %struct.UT_hash_handle* %1258, null
  br i1 %1259, label %1260, label %1266

; <label>:1260                                    ; preds = %1248
  %1261 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1262 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1263 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1262, i32 0, i32 0
  %1264 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1263, align 8
  %1265 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1264, i32 0, i32 3
  store %struct.UT_hash_handle* %1261, %struct.UT_hash_handle** %1265, align 8
  br label %1266

; <label>:1266                                    ; preds = %1260, %1248
  %1267 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1268 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1269 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1268, i32 0, i32 0
  store %struct.UT_hash_handle* %1267, %struct.UT_hash_handle** %1269, align 8
  %1270 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_hh_nxt, align 8
  store %struct.UT_hash_handle* %1270, %struct.UT_hash_handle** %_he_thh, align 8
  br label %1192

; <label>:1271                                    ; preds = %1192
  br label %1272

; <label>:1272                                    ; preds = %1271
  %1273 = load i32, i32* %_he_bkt_i, align 4
  %1274 = add i32 %1273, 1
  store i32 %1274, i32* %_he_bkt_i, align 4
  br label %1171

; <label>:1275                                    ; preds = %1171
  %1276 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1277 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1276, i32 0, i32 2
  %1278 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1277, i32 0, i32 0
  %1279 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1278, align 8
  %1280 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1279, i32 0, i32 0
  %1281 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %1280, align 8
  %1282 = bitcast %struct.UT_hash_bucket* %1281 to i8*
  call void @free(i8* %1282) #2
  %1283 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1284 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1283, i32 0, i32 2
  %1285 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1284, i32 0, i32 0
  %1286 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1285, align 8
  %1287 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1286, i32 0, i32 1
  %1288 = load i32, i32* %1287, align 8
  %1289 = mul i32 %1288, 2
  store i32 %1289, i32* %1287, align 8
  %1290 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1291 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1290, i32 0, i32 2
  %1292 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1291, i32 0, i32 0
  %1293 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1292, align 8
  %1294 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1293, i32 0, i32 2
  %1295 = load i32, i32* %1294, align 4
  %1296 = add i32 %1295, 1
  store i32 %1296, i32* %1294, align 4
  %1297 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %1298 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1299 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1298, i32 0, i32 2
  %1300 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1299, i32 0, i32 0
  %1301 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1300, align 8
  %1302 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1301, i32 0, i32 0
  store %struct.UT_hash_bucket* %1297, %struct.UT_hash_bucket** %1302, align 8
  %1303 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1304 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1303, i32 0, i32 2
  %1305 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1304, i32 0, i32 0
  %1306 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1305, align 8
  %1307 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1306, i32 0, i32 7
  %1308 = load i32, i32* %1307, align 4
  %1309 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1310 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1309, i32 0, i32 2
  %1311 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1310, i32 0, i32 0
  %1312 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1311, align 8
  %1313 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1312, i32 0, i32 3
  %1314 = load i32, i32* %1313, align 8
  %1315 = lshr i32 %1314, 1
  %1316 = icmp ugt i32 %1308, %1315
  br i1 %1316, label %1317, label %1325

; <label>:1317                                    ; preds = %1275
  %1318 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1319 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1318, i32 0, i32 2
  %1320 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1319, i32 0, i32 0
  %1321 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1320, align 8
  %1322 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1321, i32 0, i32 8
  %1323 = load i32, i32* %1322, align 8
  %1324 = add i32 %1323, 1
  br label %1326

; <label>:1325                                    ; preds = %1275
  br label %1326

; <label>:1326                                    ; preds = %1325, %1317
  %1327 = phi i32 [ %1324, %1317 ], [ 0, %1325 ]
  %1328 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1329 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1328, i32 0, i32 2
  %1330 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1329, i32 0, i32 0
  %1331 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1330, align 8
  %1332 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1331, i32 0, i32 8
  store i32 %1327, i32* %1332, align 8
  %1333 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1334 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1333, i32 0, i32 2
  %1335 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1334, i32 0, i32 0
  %1336 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1335, align 8
  %1337 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1336, i32 0, i32 8
  %1338 = load i32, i32* %1337, align 8
  %1339 = icmp ugt i32 %1338, 1
  br i1 %1339, label %1340, label %1346

; <label>:1340                                    ; preds = %1326
  %1341 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1342 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1341, i32 0, i32 2
  %1343 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1342, i32 0, i32 0
  %1344 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1343, align 8
  %1345 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1344, i32 0, i32 9
  store i32 1, i32* %1345, align 4
  br label %1346

; <label>:1346                                    ; preds = %1340, %1326
  br label %1347

; <label>:1347                                    ; preds = %1346
  br label %1348

; <label>:1348                                    ; preds = %1347
  br label %1349

; <label>:1349                                    ; preds = %1348, %1093, %1079
  br label %1350

; <label>:1350                                    ; preds = %1349
  br label %1351

; <label>:1351                                    ; preds = %1350
  br label %1352

; <label>:1352                                    ; preds = %1351
  br label %1353

; <label>:1353                                    ; preds = %1352
  br label %1354

; <label>:1354                                    ; preds = %1353, %507
  br label %16

; <label>:1355                                    ; preds = %16
  %1356 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %1357 = ptrtoint %struct.hash_tbl* %1356 to i64
  %1358 = or i64 %1357, 6
  %1359 = load i64*, i64** %vec, align 8
  %1360 = getelementptr inbounds i64, i64* %1359, i64 1
  store i64 %1358, i64* %1360, align 8
  %1361 = load i64*, i64** %vec, align 8
  %1362 = ptrtoint i64* %1361 to i64
  %1363 = or i64 %1362, 6
  ret i64 %1363
}

; Function Attrs: nounwind readonly
declare i32 @memcmp(i8*, i8*, i64) #7

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture, i8, i64, i32, i1) #8

; Function Attrs: nounwind
declare void @free(i8*) #4

; Function Attrs: uwtable
define i64 @applyprim_make_45hash(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %v0 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_args1(i64 %2)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %v0, align 8
  %5 = call i64 @prim_make_45hash(i64 %4)
  ret i64 %5
}

; Function Attrs: uwtable
define i64 @prim_hash_45ref(i64 %hash, i64 %key) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %vec = alloca i64*, align 8
  %ptr_to_hash = alloca i64*, align 8
  %tbls = alloca %struct.hash_tbl*, align 8
  %tbl = alloca %struct.hash_tbl*, align 8
  %_hf_hashv = alloca i32, align 4
  %_hj_i = alloca i32, align 4
  %_hj_j = alloca i32, align 4
  %_hj_k = alloca i32, align 4
  %_hj_key = alloca i8*, align 8
  %_hf_bkt = alloca i32, align 4
  store i64 %hash, i64* %2, align 8
  store i64 %key, i64* %3, align 8
  %4 = load i64, i64* %2, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 6
  br i1 %6, label %14, label %7

; <label>:7                                       ; preds = %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, -8
  %10 = inttoptr i64 %9 to i64*
  %11 = getelementptr inbounds i64, i64* %10, i64 0
  %12 = load i64, i64* %11, align 8
  %13 = icmp ne i64 2, %12
  br i1 %13, label %14, label %15

; <label>:14                                      ; preds = %7, %0
  call void @fatal_err(i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.52, i32 0, i32 0))
  br label %15

; <label>:15                                      ; preds = %14, %7
  %16 = load i64, i64* %2, align 8
  %17 = and i64 %16, -8
  %18 = inttoptr i64 %17 to i64*
  store i64* %18, i64** %vec, align 8
  %19 = load i64*, i64** %vec, align 8
  %20 = getelementptr inbounds i64, i64* %19, i64 1
  %21 = load i64, i64* %20, align 8
  %22 = and i64 %21, -8
  %23 = inttoptr i64 %22 to i64*
  store i64* %23, i64** %ptr_to_hash, align 8
  %24 = load i64*, i64** %ptr_to_hash, align 8
  %25 = bitcast i64* %24 to %struct.hash_tbl*
  store %struct.hash_tbl* %25, %struct.hash_tbl** %tbls, align 8
  store %struct.hash_tbl* null, %struct.hash_tbl** %tbl, align 8
  br label %26

; <label>:26                                      ; preds = %15
  br label %27

; <label>:27                                      ; preds = %26
  br label %28

; <label>:28                                      ; preds = %27
  %29 = bitcast i64* %3 to i8*
  store i8* %29, i8** %_hj_key, align 8
  store i32 -17973521, i32* %_hf_hashv, align 4
  store i32 -1640531527, i32* %_hj_j, align 4
  store i32 -1640531527, i32* %_hj_i, align 4
  store i32 8, i32* %_hj_k, align 4
  br label %30

; <label>:30                                      ; preds = %197, %28
  %31 = load i32, i32* %_hj_k, align 4
  %32 = icmp uge i32 %31, 12
  br i1 %32, label %33, label %202

; <label>:33                                      ; preds = %30
  %34 = load i8*, i8** %_hj_key, align 8
  %35 = getelementptr inbounds i8, i8* %34, i64 0
  %36 = load i8, i8* %35, align 1
  %37 = zext i8 %36 to i32
  %38 = load i8*, i8** %_hj_key, align 8
  %39 = getelementptr inbounds i8, i8* %38, i64 1
  %40 = load i8, i8* %39, align 1
  %41 = zext i8 %40 to i32
  %42 = shl i32 %41, 8
  %43 = add i32 %37, %42
  %44 = load i8*, i8** %_hj_key, align 8
  %45 = getelementptr inbounds i8, i8* %44, i64 2
  %46 = load i8, i8* %45, align 1
  %47 = zext i8 %46 to i32
  %48 = shl i32 %47, 16
  %49 = add i32 %43, %48
  %50 = load i8*, i8** %_hj_key, align 8
  %51 = getelementptr inbounds i8, i8* %50, i64 3
  %52 = load i8, i8* %51, align 1
  %53 = zext i8 %52 to i32
  %54 = shl i32 %53, 24
  %55 = add i32 %49, %54
  %56 = load i32, i32* %_hj_i, align 4
  %57 = add i32 %56, %55
  store i32 %57, i32* %_hj_i, align 4
  %58 = load i8*, i8** %_hj_key, align 8
  %59 = getelementptr inbounds i8, i8* %58, i64 4
  %60 = load i8, i8* %59, align 1
  %61 = zext i8 %60 to i32
  %62 = load i8*, i8** %_hj_key, align 8
  %63 = getelementptr inbounds i8, i8* %62, i64 5
  %64 = load i8, i8* %63, align 1
  %65 = zext i8 %64 to i32
  %66 = shl i32 %65, 8
  %67 = add i32 %61, %66
  %68 = load i8*, i8** %_hj_key, align 8
  %69 = getelementptr inbounds i8, i8* %68, i64 6
  %70 = load i8, i8* %69, align 1
  %71 = zext i8 %70 to i32
  %72 = shl i32 %71, 16
  %73 = add i32 %67, %72
  %74 = load i8*, i8** %_hj_key, align 8
  %75 = getelementptr inbounds i8, i8* %74, i64 7
  %76 = load i8, i8* %75, align 1
  %77 = zext i8 %76 to i32
  %78 = shl i32 %77, 24
  %79 = add i32 %73, %78
  %80 = load i32, i32* %_hj_j, align 4
  %81 = add i32 %80, %79
  store i32 %81, i32* %_hj_j, align 4
  %82 = load i8*, i8** %_hj_key, align 8
  %83 = getelementptr inbounds i8, i8* %82, i64 8
  %84 = load i8, i8* %83, align 1
  %85 = zext i8 %84 to i32
  %86 = load i8*, i8** %_hj_key, align 8
  %87 = getelementptr inbounds i8, i8* %86, i64 9
  %88 = load i8, i8* %87, align 1
  %89 = zext i8 %88 to i32
  %90 = shl i32 %89, 8
  %91 = add i32 %85, %90
  %92 = load i8*, i8** %_hj_key, align 8
  %93 = getelementptr inbounds i8, i8* %92, i64 10
  %94 = load i8, i8* %93, align 1
  %95 = zext i8 %94 to i32
  %96 = shl i32 %95, 16
  %97 = add i32 %91, %96
  %98 = load i8*, i8** %_hj_key, align 8
  %99 = getelementptr inbounds i8, i8* %98, i64 11
  %100 = load i8, i8* %99, align 1
  %101 = zext i8 %100 to i32
  %102 = shl i32 %101, 24
  %103 = add i32 %97, %102
  %104 = load i32, i32* %_hf_hashv, align 4
  %105 = add i32 %104, %103
  store i32 %105, i32* %_hf_hashv, align 4
  br label %106

; <label>:106                                     ; preds = %33
  %107 = load i32, i32* %_hj_j, align 4
  %108 = load i32, i32* %_hj_i, align 4
  %109 = sub i32 %108, %107
  store i32 %109, i32* %_hj_i, align 4
  %110 = load i32, i32* %_hf_hashv, align 4
  %111 = load i32, i32* %_hj_i, align 4
  %112 = sub i32 %111, %110
  store i32 %112, i32* %_hj_i, align 4
  %113 = load i32, i32* %_hf_hashv, align 4
  %114 = lshr i32 %113, 13
  %115 = load i32, i32* %_hj_i, align 4
  %116 = xor i32 %115, %114
  store i32 %116, i32* %_hj_i, align 4
  %117 = load i32, i32* %_hf_hashv, align 4
  %118 = load i32, i32* %_hj_j, align 4
  %119 = sub i32 %118, %117
  store i32 %119, i32* %_hj_j, align 4
  %120 = load i32, i32* %_hj_i, align 4
  %121 = load i32, i32* %_hj_j, align 4
  %122 = sub i32 %121, %120
  store i32 %122, i32* %_hj_j, align 4
  %123 = load i32, i32* %_hj_i, align 4
  %124 = shl i32 %123, 8
  %125 = load i32, i32* %_hj_j, align 4
  %126 = xor i32 %125, %124
  store i32 %126, i32* %_hj_j, align 4
  %127 = load i32, i32* %_hj_i, align 4
  %128 = load i32, i32* %_hf_hashv, align 4
  %129 = sub i32 %128, %127
  store i32 %129, i32* %_hf_hashv, align 4
  %130 = load i32, i32* %_hj_j, align 4
  %131 = load i32, i32* %_hf_hashv, align 4
  %132 = sub i32 %131, %130
  store i32 %132, i32* %_hf_hashv, align 4
  %133 = load i32, i32* %_hj_j, align 4
  %134 = lshr i32 %133, 13
  %135 = load i32, i32* %_hf_hashv, align 4
  %136 = xor i32 %135, %134
  store i32 %136, i32* %_hf_hashv, align 4
  %137 = load i32, i32* %_hj_j, align 4
  %138 = load i32, i32* %_hj_i, align 4
  %139 = sub i32 %138, %137
  store i32 %139, i32* %_hj_i, align 4
  %140 = load i32, i32* %_hf_hashv, align 4
  %141 = load i32, i32* %_hj_i, align 4
  %142 = sub i32 %141, %140
  store i32 %142, i32* %_hj_i, align 4
  %143 = load i32, i32* %_hf_hashv, align 4
  %144 = lshr i32 %143, 12
  %145 = load i32, i32* %_hj_i, align 4
  %146 = xor i32 %145, %144
  store i32 %146, i32* %_hj_i, align 4
  %147 = load i32, i32* %_hf_hashv, align 4
  %148 = load i32, i32* %_hj_j, align 4
  %149 = sub i32 %148, %147
  store i32 %149, i32* %_hj_j, align 4
  %150 = load i32, i32* %_hj_i, align 4
  %151 = load i32, i32* %_hj_j, align 4
  %152 = sub i32 %151, %150
  store i32 %152, i32* %_hj_j, align 4
  %153 = load i32, i32* %_hj_i, align 4
  %154 = shl i32 %153, 16
  %155 = load i32, i32* %_hj_j, align 4
  %156 = xor i32 %155, %154
  store i32 %156, i32* %_hj_j, align 4
  %157 = load i32, i32* %_hj_i, align 4
  %158 = load i32, i32* %_hf_hashv, align 4
  %159 = sub i32 %158, %157
  store i32 %159, i32* %_hf_hashv, align 4
  %160 = load i32, i32* %_hj_j, align 4
  %161 = load i32, i32* %_hf_hashv, align 4
  %162 = sub i32 %161, %160
  store i32 %162, i32* %_hf_hashv, align 4
  %163 = load i32, i32* %_hj_j, align 4
  %164 = lshr i32 %163, 5
  %165 = load i32, i32* %_hf_hashv, align 4
  %166 = xor i32 %165, %164
  store i32 %166, i32* %_hf_hashv, align 4
  %167 = load i32, i32* %_hj_j, align 4
  %168 = load i32, i32* %_hj_i, align 4
  %169 = sub i32 %168, %167
  store i32 %169, i32* %_hj_i, align 4
  %170 = load i32, i32* %_hf_hashv, align 4
  %171 = load i32, i32* %_hj_i, align 4
  %172 = sub i32 %171, %170
  store i32 %172, i32* %_hj_i, align 4
  %173 = load i32, i32* %_hf_hashv, align 4
  %174 = lshr i32 %173, 3
  %175 = load i32, i32* %_hj_i, align 4
  %176 = xor i32 %175, %174
  store i32 %176, i32* %_hj_i, align 4
  %177 = load i32, i32* %_hf_hashv, align 4
  %178 = load i32, i32* %_hj_j, align 4
  %179 = sub i32 %178, %177
  store i32 %179, i32* %_hj_j, align 4
  %180 = load i32, i32* %_hj_i, align 4
  %181 = load i32, i32* %_hj_j, align 4
  %182 = sub i32 %181, %180
  store i32 %182, i32* %_hj_j, align 4
  %183 = load i32, i32* %_hj_i, align 4
  %184 = shl i32 %183, 10
  %185 = load i32, i32* %_hj_j, align 4
  %186 = xor i32 %185, %184
  store i32 %186, i32* %_hj_j, align 4
  %187 = load i32, i32* %_hj_i, align 4
  %188 = load i32, i32* %_hf_hashv, align 4
  %189 = sub i32 %188, %187
  store i32 %189, i32* %_hf_hashv, align 4
  %190 = load i32, i32* %_hj_j, align 4
  %191 = load i32, i32* %_hf_hashv, align 4
  %192 = sub i32 %191, %190
  store i32 %192, i32* %_hf_hashv, align 4
  %193 = load i32, i32* %_hj_j, align 4
  %194 = lshr i32 %193, 15
  %195 = load i32, i32* %_hf_hashv, align 4
  %196 = xor i32 %195, %194
  store i32 %196, i32* %_hf_hashv, align 4
  br label %197

; <label>:197                                     ; preds = %106
  %198 = load i8*, i8** %_hj_key, align 8
  %199 = getelementptr inbounds i8, i8* %198, i64 12
  store i8* %199, i8** %_hj_key, align 8
  %200 = load i32, i32* %_hj_k, align 4
  %201 = sub i32 %200, 12
  store i32 %201, i32* %_hj_k, align 4
  br label %30

; <label>:202                                     ; preds = %30
  %203 = load i32, i32* %_hf_hashv, align 4
  %204 = add i32 %203, 8
  store i32 %204, i32* %_hf_hashv, align 4
  %205 = load i32, i32* %_hj_k, align 4
  switch i32 %205, label %292 [
    i32 11, label %206
    i32 10, label %214
    i32 9, label %222
    i32 8, label %230
    i32 7, label %238
    i32 6, label %246
    i32 5, label %254
    i32 4, label %261
    i32 3, label %269
    i32 2, label %277
    i32 1, label %285
  ]

; <label>:206                                     ; preds = %202
  %207 = load i8*, i8** %_hj_key, align 8
  %208 = getelementptr inbounds i8, i8* %207, i64 10
  %209 = load i8, i8* %208, align 1
  %210 = zext i8 %209 to i32
  %211 = shl i32 %210, 24
  %212 = load i32, i32* %_hf_hashv, align 4
  %213 = add i32 %212, %211
  store i32 %213, i32* %_hf_hashv, align 4
  br label %214

; <label>:214                                     ; preds = %202, %206
  %215 = load i8*, i8** %_hj_key, align 8
  %216 = getelementptr inbounds i8, i8* %215, i64 9
  %217 = load i8, i8* %216, align 1
  %218 = zext i8 %217 to i32
  %219 = shl i32 %218, 16
  %220 = load i32, i32* %_hf_hashv, align 4
  %221 = add i32 %220, %219
  store i32 %221, i32* %_hf_hashv, align 4
  br label %222

; <label>:222                                     ; preds = %202, %214
  %223 = load i8*, i8** %_hj_key, align 8
  %224 = getelementptr inbounds i8, i8* %223, i64 8
  %225 = load i8, i8* %224, align 1
  %226 = zext i8 %225 to i32
  %227 = shl i32 %226, 8
  %228 = load i32, i32* %_hf_hashv, align 4
  %229 = add i32 %228, %227
  store i32 %229, i32* %_hf_hashv, align 4
  br label %230

; <label>:230                                     ; preds = %202, %222
  %231 = load i8*, i8** %_hj_key, align 8
  %232 = getelementptr inbounds i8, i8* %231, i64 7
  %233 = load i8, i8* %232, align 1
  %234 = zext i8 %233 to i32
  %235 = shl i32 %234, 24
  %236 = load i32, i32* %_hj_j, align 4
  %237 = add i32 %236, %235
  store i32 %237, i32* %_hj_j, align 4
  br label %238

; <label>:238                                     ; preds = %202, %230
  %239 = load i8*, i8** %_hj_key, align 8
  %240 = getelementptr inbounds i8, i8* %239, i64 6
  %241 = load i8, i8* %240, align 1
  %242 = zext i8 %241 to i32
  %243 = shl i32 %242, 16
  %244 = load i32, i32* %_hj_j, align 4
  %245 = add i32 %244, %243
  store i32 %245, i32* %_hj_j, align 4
  br label %246

; <label>:246                                     ; preds = %202, %238
  %247 = load i8*, i8** %_hj_key, align 8
  %248 = getelementptr inbounds i8, i8* %247, i64 5
  %249 = load i8, i8* %248, align 1
  %250 = zext i8 %249 to i32
  %251 = shl i32 %250, 8
  %252 = load i32, i32* %_hj_j, align 4
  %253 = add i32 %252, %251
  store i32 %253, i32* %_hj_j, align 4
  br label %254

; <label>:254                                     ; preds = %202, %246
  %255 = load i8*, i8** %_hj_key, align 8
  %256 = getelementptr inbounds i8, i8* %255, i64 4
  %257 = load i8, i8* %256, align 1
  %258 = zext i8 %257 to i32
  %259 = load i32, i32* %_hj_j, align 4
  %260 = add i32 %259, %258
  store i32 %260, i32* %_hj_j, align 4
  br label %261

; <label>:261                                     ; preds = %202, %254
  %262 = load i8*, i8** %_hj_key, align 8
  %263 = getelementptr inbounds i8, i8* %262, i64 3
  %264 = load i8, i8* %263, align 1
  %265 = zext i8 %264 to i32
  %266 = shl i32 %265, 24
  %267 = load i32, i32* %_hj_i, align 4
  %268 = add i32 %267, %266
  store i32 %268, i32* %_hj_i, align 4
  br label %269

; <label>:269                                     ; preds = %202, %261
  %270 = load i8*, i8** %_hj_key, align 8
  %271 = getelementptr inbounds i8, i8* %270, i64 2
  %272 = load i8, i8* %271, align 1
  %273 = zext i8 %272 to i32
  %274 = shl i32 %273, 16
  %275 = load i32, i32* %_hj_i, align 4
  %276 = add i32 %275, %274
  store i32 %276, i32* %_hj_i, align 4
  br label %277

; <label>:277                                     ; preds = %202, %269
  %278 = load i8*, i8** %_hj_key, align 8
  %279 = getelementptr inbounds i8, i8* %278, i64 1
  %280 = load i8, i8* %279, align 1
  %281 = zext i8 %280 to i32
  %282 = shl i32 %281, 8
  %283 = load i32, i32* %_hj_i, align 4
  %284 = add i32 %283, %282
  store i32 %284, i32* %_hj_i, align 4
  br label %285

; <label>:285                                     ; preds = %202, %277
  %286 = load i8*, i8** %_hj_key, align 8
  %287 = getelementptr inbounds i8, i8* %286, i64 0
  %288 = load i8, i8* %287, align 1
  %289 = zext i8 %288 to i32
  %290 = load i32, i32* %_hj_i, align 4
  %291 = add i32 %290, %289
  store i32 %291, i32* %_hj_i, align 4
  br label %292

; <label>:292                                     ; preds = %285, %202
  br label %293

; <label>:293                                     ; preds = %292
  %294 = load i32, i32* %_hj_j, align 4
  %295 = load i32, i32* %_hj_i, align 4
  %296 = sub i32 %295, %294
  store i32 %296, i32* %_hj_i, align 4
  %297 = load i32, i32* %_hf_hashv, align 4
  %298 = load i32, i32* %_hj_i, align 4
  %299 = sub i32 %298, %297
  store i32 %299, i32* %_hj_i, align 4
  %300 = load i32, i32* %_hf_hashv, align 4
  %301 = lshr i32 %300, 13
  %302 = load i32, i32* %_hj_i, align 4
  %303 = xor i32 %302, %301
  store i32 %303, i32* %_hj_i, align 4
  %304 = load i32, i32* %_hf_hashv, align 4
  %305 = load i32, i32* %_hj_j, align 4
  %306 = sub i32 %305, %304
  store i32 %306, i32* %_hj_j, align 4
  %307 = load i32, i32* %_hj_i, align 4
  %308 = load i32, i32* %_hj_j, align 4
  %309 = sub i32 %308, %307
  store i32 %309, i32* %_hj_j, align 4
  %310 = load i32, i32* %_hj_i, align 4
  %311 = shl i32 %310, 8
  %312 = load i32, i32* %_hj_j, align 4
  %313 = xor i32 %312, %311
  store i32 %313, i32* %_hj_j, align 4
  %314 = load i32, i32* %_hj_i, align 4
  %315 = load i32, i32* %_hf_hashv, align 4
  %316 = sub i32 %315, %314
  store i32 %316, i32* %_hf_hashv, align 4
  %317 = load i32, i32* %_hj_j, align 4
  %318 = load i32, i32* %_hf_hashv, align 4
  %319 = sub i32 %318, %317
  store i32 %319, i32* %_hf_hashv, align 4
  %320 = load i32, i32* %_hj_j, align 4
  %321 = lshr i32 %320, 13
  %322 = load i32, i32* %_hf_hashv, align 4
  %323 = xor i32 %322, %321
  store i32 %323, i32* %_hf_hashv, align 4
  %324 = load i32, i32* %_hj_j, align 4
  %325 = load i32, i32* %_hj_i, align 4
  %326 = sub i32 %325, %324
  store i32 %326, i32* %_hj_i, align 4
  %327 = load i32, i32* %_hf_hashv, align 4
  %328 = load i32, i32* %_hj_i, align 4
  %329 = sub i32 %328, %327
  store i32 %329, i32* %_hj_i, align 4
  %330 = load i32, i32* %_hf_hashv, align 4
  %331 = lshr i32 %330, 12
  %332 = load i32, i32* %_hj_i, align 4
  %333 = xor i32 %332, %331
  store i32 %333, i32* %_hj_i, align 4
  %334 = load i32, i32* %_hf_hashv, align 4
  %335 = load i32, i32* %_hj_j, align 4
  %336 = sub i32 %335, %334
  store i32 %336, i32* %_hj_j, align 4
  %337 = load i32, i32* %_hj_i, align 4
  %338 = load i32, i32* %_hj_j, align 4
  %339 = sub i32 %338, %337
  store i32 %339, i32* %_hj_j, align 4
  %340 = load i32, i32* %_hj_i, align 4
  %341 = shl i32 %340, 16
  %342 = load i32, i32* %_hj_j, align 4
  %343 = xor i32 %342, %341
  store i32 %343, i32* %_hj_j, align 4
  %344 = load i32, i32* %_hj_i, align 4
  %345 = load i32, i32* %_hf_hashv, align 4
  %346 = sub i32 %345, %344
  store i32 %346, i32* %_hf_hashv, align 4
  %347 = load i32, i32* %_hj_j, align 4
  %348 = load i32, i32* %_hf_hashv, align 4
  %349 = sub i32 %348, %347
  store i32 %349, i32* %_hf_hashv, align 4
  %350 = load i32, i32* %_hj_j, align 4
  %351 = lshr i32 %350, 5
  %352 = load i32, i32* %_hf_hashv, align 4
  %353 = xor i32 %352, %351
  store i32 %353, i32* %_hf_hashv, align 4
  %354 = load i32, i32* %_hj_j, align 4
  %355 = load i32, i32* %_hj_i, align 4
  %356 = sub i32 %355, %354
  store i32 %356, i32* %_hj_i, align 4
  %357 = load i32, i32* %_hf_hashv, align 4
  %358 = load i32, i32* %_hj_i, align 4
  %359 = sub i32 %358, %357
  store i32 %359, i32* %_hj_i, align 4
  %360 = load i32, i32* %_hf_hashv, align 4
  %361 = lshr i32 %360, 3
  %362 = load i32, i32* %_hj_i, align 4
  %363 = xor i32 %362, %361
  store i32 %363, i32* %_hj_i, align 4
  %364 = load i32, i32* %_hf_hashv, align 4
  %365 = load i32, i32* %_hj_j, align 4
  %366 = sub i32 %365, %364
  store i32 %366, i32* %_hj_j, align 4
  %367 = load i32, i32* %_hj_i, align 4
  %368 = load i32, i32* %_hj_j, align 4
  %369 = sub i32 %368, %367
  store i32 %369, i32* %_hj_j, align 4
  %370 = load i32, i32* %_hj_i, align 4
  %371 = shl i32 %370, 10
  %372 = load i32, i32* %_hj_j, align 4
  %373 = xor i32 %372, %371
  store i32 %373, i32* %_hj_j, align 4
  %374 = load i32, i32* %_hj_i, align 4
  %375 = load i32, i32* %_hf_hashv, align 4
  %376 = sub i32 %375, %374
  store i32 %376, i32* %_hf_hashv, align 4
  %377 = load i32, i32* %_hj_j, align 4
  %378 = load i32, i32* %_hf_hashv, align 4
  %379 = sub i32 %378, %377
  store i32 %379, i32* %_hf_hashv, align 4
  %380 = load i32, i32* %_hj_j, align 4
  %381 = lshr i32 %380, 15
  %382 = load i32, i32* %_hf_hashv, align 4
  %383 = xor i32 %382, %381
  store i32 %383, i32* %_hf_hashv, align 4
  br label %384

; <label>:384                                     ; preds = %293
  br label %385

; <label>:385                                     ; preds = %384
  br label %386

; <label>:386                                     ; preds = %385
  br label %387

; <label>:387                                     ; preds = %386
  store %struct.hash_tbl* null, %struct.hash_tbl** %tbl, align 8
  %388 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %389 = icmp ne %struct.hash_tbl* %388, null
  br i1 %389, label %390, label %495

; <label>:390                                     ; preds = %387
  br label %391

; <label>:391                                     ; preds = %390
  %392 = load i32, i32* %_hf_hashv, align 4
  %393 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %394 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %393, i32 0, i32 2
  %395 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %394, i32 0, i32 0
  %396 = load %struct.UT_hash_table*, %struct.UT_hash_table** %395, align 8
  %397 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %396, i32 0, i32 1
  %398 = load i32, i32* %397, align 8
  %399 = sub i32 %398, 1
  %400 = and i32 %392, %399
  store i32 %400, i32* %_hf_bkt, align 4
  br label %401

; <label>:401                                     ; preds = %391
  br label %402

; <label>:402                                     ; preds = %401
  %403 = load i32, i32* %_hf_bkt, align 4
  %404 = zext i32 %403 to i64
  %405 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %406 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %405, i32 0, i32 2
  %407 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %406, i32 0, i32 0
  %408 = load %struct.UT_hash_table*, %struct.UT_hash_table** %407, align 8
  %409 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %408, i32 0, i32 0
  %410 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %409, align 8
  %411 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %410, i64 %404
  %412 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %411, i32 0, i32 0
  %413 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %412, align 8
  %414 = icmp ne %struct.UT_hash_handle* %413, null
  br i1 %414, label %415, label %439

; <label>:415                                     ; preds = %402
  br label %416

; <label>:416                                     ; preds = %415
  %417 = load i32, i32* %_hf_bkt, align 4
  %418 = zext i32 %417 to i64
  %419 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %420 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %419, i32 0, i32 2
  %421 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %420, i32 0, i32 0
  %422 = load %struct.UT_hash_table*, %struct.UT_hash_table** %421, align 8
  %423 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %422, i32 0, i32 0
  %424 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %423, align 8
  %425 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %424, i64 %418
  %426 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %425, i32 0, i32 0
  %427 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %426, align 8
  %428 = bitcast %struct.UT_hash_handle* %427 to i8*
  %429 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %430 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %429, i32 0, i32 2
  %431 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %430, i32 0, i32 0
  %432 = load %struct.UT_hash_table*, %struct.UT_hash_table** %431, align 8
  %433 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %432, i32 0, i32 5
  %434 = load i64, i64* %433, align 8
  %435 = sub i64 0, %434
  %436 = getelementptr inbounds i8, i8* %428, i64 %435
  %437 = bitcast i8* %436 to %struct.hash_tbl*
  store %struct.hash_tbl* %437, %struct.hash_tbl** %tbl, align 8
  br label %438

; <label>:438                                     ; preds = %416
  br label %440

; <label>:439                                     ; preds = %402
  store %struct.hash_tbl* null, %struct.hash_tbl** %tbl, align 8
  br label %440

; <label>:440                                     ; preds = %439, %438
  br label %441

; <label>:441                                     ; preds = %492, %440
  %442 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %443 = icmp ne %struct.hash_tbl* %442, null
  br i1 %443, label %444, label %493

; <label>:444                                     ; preds = %441
  %445 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %446 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %445, i32 0, i32 2
  %447 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %446, i32 0, i32 7
  %448 = load i32, i32* %447, align 4
  %449 = load i32, i32* %_hf_hashv, align 4
  %450 = icmp eq i32 %448, %449
  br i1 %450, label %451, label %468

; <label>:451                                     ; preds = %444
  %452 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %453 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %452, i32 0, i32 2
  %454 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %453, i32 0, i32 6
  %455 = load i32, i32* %454, align 8
  %456 = zext i32 %455 to i64
  %457 = icmp eq i64 %456, 8
  br i1 %457, label %458, label %468

; <label>:458                                     ; preds = %451
  %459 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %460 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %459, i32 0, i32 2
  %461 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %460, i32 0, i32 5
  %462 = load i8*, i8** %461, align 8
  %463 = bitcast i64* %3 to i8*
  %464 = call i32 @memcmp(i8* %462, i8* %463, i64 8) #11
  %465 = icmp eq i32 %464, 0
  br i1 %465, label %466, label %467

; <label>:466                                     ; preds = %458
  br label %493

; <label>:467                                     ; preds = %458
  br label %468

; <label>:468                                     ; preds = %467, %451, %444
  %469 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %470 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %469, i32 0, i32 2
  %471 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %470, i32 0, i32 4
  %472 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %471, align 8
  %473 = icmp ne %struct.UT_hash_handle* %472, null
  br i1 %473, label %474, label %491

; <label>:474                                     ; preds = %468
  br label %475

; <label>:475                                     ; preds = %474
  %476 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %477 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %476, i32 0, i32 2
  %478 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %477, i32 0, i32 4
  %479 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %478, align 8
  %480 = bitcast %struct.UT_hash_handle* %479 to i8*
  %481 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %482 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %481, i32 0, i32 2
  %483 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %482, i32 0, i32 0
  %484 = load %struct.UT_hash_table*, %struct.UT_hash_table** %483, align 8
  %485 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %484, i32 0, i32 5
  %486 = load i64, i64* %485, align 8
  %487 = sub i64 0, %486
  %488 = getelementptr inbounds i8, i8* %480, i64 %487
  %489 = bitcast i8* %488 to %struct.hash_tbl*
  store %struct.hash_tbl* %489, %struct.hash_tbl** %tbl, align 8
  br label %490

; <label>:490                                     ; preds = %475
  br label %492

; <label>:491                                     ; preds = %468
  store %struct.hash_tbl* null, %struct.hash_tbl** %tbl, align 8
  br label %492

; <label>:492                                     ; preds = %491, %490
  br label %441

; <label>:493                                     ; preds = %466, %441
  br label %494

; <label>:494                                     ; preds = %493
  br label %495

; <label>:495                                     ; preds = %494, %387
  br label %496

; <label>:496                                     ; preds = %495
  br label %497

; <label>:497                                     ; preds = %496
  %498 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %499 = icmp ne %struct.hash_tbl* %498, null
  br i1 %499, label %500, label %504

; <label>:500                                     ; preds = %497
  %501 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %502 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %501, i32 0, i32 1
  %503 = load i64, i64* %502, align 8
  store i64 %503, i64* %1, align 8
  br label %505

; <label>:504                                     ; preds = %497
  call void @fatal_err(i8* getelementptr inbounds ([47 x i8], [47 x i8]* @.str.53, i32 0, i32 0))
  store i64 39, i64* %1, align 8
  br label %505

; <label>:505                                     ; preds = %504, %500
  %506 = load i64, i64* %1, align 8
  ret i64 %506
}

; Function Attrs: uwtable
define i64 @applyprim_hash_45ref(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = icmp ne i64 %6, 0
  br i1 %7, label %8, label %9

; <label>:8                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.30, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_hash_45ref(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: uwtable
define i64 @prim_hash_45set_33(i64 %hash, i64 %key, i64 %value) #0 {
  %1 = alloca i64, align 8
  %2 = alloca i64, align 8
  %3 = alloca i64, align 8
  %vec = alloca i64*, align 8
  %ptr_to_hash = alloca i64*, align 8
  %tbls = alloca %struct.hash_tbl*, align 8
  %tbl = alloca %struct.hash_tbl*, align 8
  %_hf_hashv = alloca i32, align 4
  %_hj_i = alloca i32, align 4
  %_hj_j = alloca i32, align 4
  %_hj_k = alloca i32, align 4
  %_hj_key = alloca i8*, align 8
  %_hf_bkt = alloca i32, align 4
  %new_pair = alloca %struct.hash_tbl*, align 8
  %_ha_hashv = alloca i32, align 4
  %_hj_i1 = alloca i32, align 4
  %_hj_j2 = alloca i32, align 4
  %_hj_k3 = alloca i32, align 4
  %_hj_key4 = alloca i8*, align 8
  %_ha_bkt = alloca i32, align 4
  %_ha_head = alloca %struct.UT_hash_bucket*, align 8
  %_he_bkt = alloca i32, align 4
  %_he_bkt_i = alloca i32, align 4
  %_he_thh = alloca %struct.UT_hash_handle*, align 8
  %_he_hh_nxt = alloca %struct.UT_hash_handle*, align 8
  %_he_new_buckets = alloca %struct.UT_hash_bucket*, align 8
  %_he_newbkt = alloca %struct.UT_hash_bucket*, align 8
  store i64 %hash, i64* %1, align 8
  store i64 %key, i64* %2, align 8
  store i64 %value, i64* %3, align 8
  %4 = load i64, i64* %1, align 8
  %5 = and i64 %4, 7
  %6 = icmp ne i64 %5, 6
  br i1 %6, label %14, label %7

; <label>:7                                       ; preds = %0
  %8 = load i64, i64* %1, align 8
  %9 = and i64 %8, -8
  %10 = inttoptr i64 %9 to i64*
  %11 = getelementptr inbounds i64, i64* %10, i64 0
  %12 = load i64, i64* %11, align 8
  %13 = icmp ne i64 2, %12
  br i1 %13, label %14, label %15

; <label>:14                                      ; preds = %7, %0
  call void @fatal_err(i8* getelementptr inbounds ([18 x i8], [18 x i8]* @.str.52, i32 0, i32 0))
  br label %15

; <label>:15                                      ; preds = %14, %7
  %16 = load i64, i64* %1, align 8
  %17 = and i64 %16, -8
  %18 = inttoptr i64 %17 to i64*
  store i64* %18, i64** %vec, align 8
  %19 = load i64*, i64** %vec, align 8
  %20 = getelementptr inbounds i64, i64* %19, i64 1
  %21 = load i64, i64* %20, align 8
  %22 = and i64 %21, -8
  %23 = inttoptr i64 %22 to i64*
  store i64* %23, i64** %ptr_to_hash, align 8
  %24 = load i64*, i64** %ptr_to_hash, align 8
  %25 = bitcast i64* %24 to %struct.hash_tbl*
  store %struct.hash_tbl* %25, %struct.hash_tbl** %tbls, align 8
  store %struct.hash_tbl* null, %struct.hash_tbl** %tbl, align 8
  br label %26

; <label>:26                                      ; preds = %15
  br label %27

; <label>:27                                      ; preds = %26
  br label %28

; <label>:28                                      ; preds = %27
  %29 = bitcast i64* %2 to i8*
  store i8* %29, i8** %_hj_key, align 8
  store i32 -17973521, i32* %_hf_hashv, align 4
  store i32 -1640531527, i32* %_hj_j, align 4
  store i32 -1640531527, i32* %_hj_i, align 4
  store i32 8, i32* %_hj_k, align 4
  br label %30

; <label>:30                                      ; preds = %197, %28
  %31 = load i32, i32* %_hj_k, align 4
  %32 = icmp uge i32 %31, 12
  br i1 %32, label %33, label %202

; <label>:33                                      ; preds = %30
  %34 = load i8*, i8** %_hj_key, align 8
  %35 = getelementptr inbounds i8, i8* %34, i64 0
  %36 = load i8, i8* %35, align 1
  %37 = zext i8 %36 to i32
  %38 = load i8*, i8** %_hj_key, align 8
  %39 = getelementptr inbounds i8, i8* %38, i64 1
  %40 = load i8, i8* %39, align 1
  %41 = zext i8 %40 to i32
  %42 = shl i32 %41, 8
  %43 = add i32 %37, %42
  %44 = load i8*, i8** %_hj_key, align 8
  %45 = getelementptr inbounds i8, i8* %44, i64 2
  %46 = load i8, i8* %45, align 1
  %47 = zext i8 %46 to i32
  %48 = shl i32 %47, 16
  %49 = add i32 %43, %48
  %50 = load i8*, i8** %_hj_key, align 8
  %51 = getelementptr inbounds i8, i8* %50, i64 3
  %52 = load i8, i8* %51, align 1
  %53 = zext i8 %52 to i32
  %54 = shl i32 %53, 24
  %55 = add i32 %49, %54
  %56 = load i32, i32* %_hj_i, align 4
  %57 = add i32 %56, %55
  store i32 %57, i32* %_hj_i, align 4
  %58 = load i8*, i8** %_hj_key, align 8
  %59 = getelementptr inbounds i8, i8* %58, i64 4
  %60 = load i8, i8* %59, align 1
  %61 = zext i8 %60 to i32
  %62 = load i8*, i8** %_hj_key, align 8
  %63 = getelementptr inbounds i8, i8* %62, i64 5
  %64 = load i8, i8* %63, align 1
  %65 = zext i8 %64 to i32
  %66 = shl i32 %65, 8
  %67 = add i32 %61, %66
  %68 = load i8*, i8** %_hj_key, align 8
  %69 = getelementptr inbounds i8, i8* %68, i64 6
  %70 = load i8, i8* %69, align 1
  %71 = zext i8 %70 to i32
  %72 = shl i32 %71, 16
  %73 = add i32 %67, %72
  %74 = load i8*, i8** %_hj_key, align 8
  %75 = getelementptr inbounds i8, i8* %74, i64 7
  %76 = load i8, i8* %75, align 1
  %77 = zext i8 %76 to i32
  %78 = shl i32 %77, 24
  %79 = add i32 %73, %78
  %80 = load i32, i32* %_hj_j, align 4
  %81 = add i32 %80, %79
  store i32 %81, i32* %_hj_j, align 4
  %82 = load i8*, i8** %_hj_key, align 8
  %83 = getelementptr inbounds i8, i8* %82, i64 8
  %84 = load i8, i8* %83, align 1
  %85 = zext i8 %84 to i32
  %86 = load i8*, i8** %_hj_key, align 8
  %87 = getelementptr inbounds i8, i8* %86, i64 9
  %88 = load i8, i8* %87, align 1
  %89 = zext i8 %88 to i32
  %90 = shl i32 %89, 8
  %91 = add i32 %85, %90
  %92 = load i8*, i8** %_hj_key, align 8
  %93 = getelementptr inbounds i8, i8* %92, i64 10
  %94 = load i8, i8* %93, align 1
  %95 = zext i8 %94 to i32
  %96 = shl i32 %95, 16
  %97 = add i32 %91, %96
  %98 = load i8*, i8** %_hj_key, align 8
  %99 = getelementptr inbounds i8, i8* %98, i64 11
  %100 = load i8, i8* %99, align 1
  %101 = zext i8 %100 to i32
  %102 = shl i32 %101, 24
  %103 = add i32 %97, %102
  %104 = load i32, i32* %_hf_hashv, align 4
  %105 = add i32 %104, %103
  store i32 %105, i32* %_hf_hashv, align 4
  br label %106

; <label>:106                                     ; preds = %33
  %107 = load i32, i32* %_hj_j, align 4
  %108 = load i32, i32* %_hj_i, align 4
  %109 = sub i32 %108, %107
  store i32 %109, i32* %_hj_i, align 4
  %110 = load i32, i32* %_hf_hashv, align 4
  %111 = load i32, i32* %_hj_i, align 4
  %112 = sub i32 %111, %110
  store i32 %112, i32* %_hj_i, align 4
  %113 = load i32, i32* %_hf_hashv, align 4
  %114 = lshr i32 %113, 13
  %115 = load i32, i32* %_hj_i, align 4
  %116 = xor i32 %115, %114
  store i32 %116, i32* %_hj_i, align 4
  %117 = load i32, i32* %_hf_hashv, align 4
  %118 = load i32, i32* %_hj_j, align 4
  %119 = sub i32 %118, %117
  store i32 %119, i32* %_hj_j, align 4
  %120 = load i32, i32* %_hj_i, align 4
  %121 = load i32, i32* %_hj_j, align 4
  %122 = sub i32 %121, %120
  store i32 %122, i32* %_hj_j, align 4
  %123 = load i32, i32* %_hj_i, align 4
  %124 = shl i32 %123, 8
  %125 = load i32, i32* %_hj_j, align 4
  %126 = xor i32 %125, %124
  store i32 %126, i32* %_hj_j, align 4
  %127 = load i32, i32* %_hj_i, align 4
  %128 = load i32, i32* %_hf_hashv, align 4
  %129 = sub i32 %128, %127
  store i32 %129, i32* %_hf_hashv, align 4
  %130 = load i32, i32* %_hj_j, align 4
  %131 = load i32, i32* %_hf_hashv, align 4
  %132 = sub i32 %131, %130
  store i32 %132, i32* %_hf_hashv, align 4
  %133 = load i32, i32* %_hj_j, align 4
  %134 = lshr i32 %133, 13
  %135 = load i32, i32* %_hf_hashv, align 4
  %136 = xor i32 %135, %134
  store i32 %136, i32* %_hf_hashv, align 4
  %137 = load i32, i32* %_hj_j, align 4
  %138 = load i32, i32* %_hj_i, align 4
  %139 = sub i32 %138, %137
  store i32 %139, i32* %_hj_i, align 4
  %140 = load i32, i32* %_hf_hashv, align 4
  %141 = load i32, i32* %_hj_i, align 4
  %142 = sub i32 %141, %140
  store i32 %142, i32* %_hj_i, align 4
  %143 = load i32, i32* %_hf_hashv, align 4
  %144 = lshr i32 %143, 12
  %145 = load i32, i32* %_hj_i, align 4
  %146 = xor i32 %145, %144
  store i32 %146, i32* %_hj_i, align 4
  %147 = load i32, i32* %_hf_hashv, align 4
  %148 = load i32, i32* %_hj_j, align 4
  %149 = sub i32 %148, %147
  store i32 %149, i32* %_hj_j, align 4
  %150 = load i32, i32* %_hj_i, align 4
  %151 = load i32, i32* %_hj_j, align 4
  %152 = sub i32 %151, %150
  store i32 %152, i32* %_hj_j, align 4
  %153 = load i32, i32* %_hj_i, align 4
  %154 = shl i32 %153, 16
  %155 = load i32, i32* %_hj_j, align 4
  %156 = xor i32 %155, %154
  store i32 %156, i32* %_hj_j, align 4
  %157 = load i32, i32* %_hj_i, align 4
  %158 = load i32, i32* %_hf_hashv, align 4
  %159 = sub i32 %158, %157
  store i32 %159, i32* %_hf_hashv, align 4
  %160 = load i32, i32* %_hj_j, align 4
  %161 = load i32, i32* %_hf_hashv, align 4
  %162 = sub i32 %161, %160
  store i32 %162, i32* %_hf_hashv, align 4
  %163 = load i32, i32* %_hj_j, align 4
  %164 = lshr i32 %163, 5
  %165 = load i32, i32* %_hf_hashv, align 4
  %166 = xor i32 %165, %164
  store i32 %166, i32* %_hf_hashv, align 4
  %167 = load i32, i32* %_hj_j, align 4
  %168 = load i32, i32* %_hj_i, align 4
  %169 = sub i32 %168, %167
  store i32 %169, i32* %_hj_i, align 4
  %170 = load i32, i32* %_hf_hashv, align 4
  %171 = load i32, i32* %_hj_i, align 4
  %172 = sub i32 %171, %170
  store i32 %172, i32* %_hj_i, align 4
  %173 = load i32, i32* %_hf_hashv, align 4
  %174 = lshr i32 %173, 3
  %175 = load i32, i32* %_hj_i, align 4
  %176 = xor i32 %175, %174
  store i32 %176, i32* %_hj_i, align 4
  %177 = load i32, i32* %_hf_hashv, align 4
  %178 = load i32, i32* %_hj_j, align 4
  %179 = sub i32 %178, %177
  store i32 %179, i32* %_hj_j, align 4
  %180 = load i32, i32* %_hj_i, align 4
  %181 = load i32, i32* %_hj_j, align 4
  %182 = sub i32 %181, %180
  store i32 %182, i32* %_hj_j, align 4
  %183 = load i32, i32* %_hj_i, align 4
  %184 = shl i32 %183, 10
  %185 = load i32, i32* %_hj_j, align 4
  %186 = xor i32 %185, %184
  store i32 %186, i32* %_hj_j, align 4
  %187 = load i32, i32* %_hj_i, align 4
  %188 = load i32, i32* %_hf_hashv, align 4
  %189 = sub i32 %188, %187
  store i32 %189, i32* %_hf_hashv, align 4
  %190 = load i32, i32* %_hj_j, align 4
  %191 = load i32, i32* %_hf_hashv, align 4
  %192 = sub i32 %191, %190
  store i32 %192, i32* %_hf_hashv, align 4
  %193 = load i32, i32* %_hj_j, align 4
  %194 = lshr i32 %193, 15
  %195 = load i32, i32* %_hf_hashv, align 4
  %196 = xor i32 %195, %194
  store i32 %196, i32* %_hf_hashv, align 4
  br label %197

; <label>:197                                     ; preds = %106
  %198 = load i8*, i8** %_hj_key, align 8
  %199 = getelementptr inbounds i8, i8* %198, i64 12
  store i8* %199, i8** %_hj_key, align 8
  %200 = load i32, i32* %_hj_k, align 4
  %201 = sub i32 %200, 12
  store i32 %201, i32* %_hj_k, align 4
  br label %30

; <label>:202                                     ; preds = %30
  %203 = load i32, i32* %_hf_hashv, align 4
  %204 = add i32 %203, 8
  store i32 %204, i32* %_hf_hashv, align 4
  %205 = load i32, i32* %_hj_k, align 4
  switch i32 %205, label %292 [
    i32 11, label %206
    i32 10, label %214
    i32 9, label %222
    i32 8, label %230
    i32 7, label %238
    i32 6, label %246
    i32 5, label %254
    i32 4, label %261
    i32 3, label %269
    i32 2, label %277
    i32 1, label %285
  ]

; <label>:206                                     ; preds = %202
  %207 = load i8*, i8** %_hj_key, align 8
  %208 = getelementptr inbounds i8, i8* %207, i64 10
  %209 = load i8, i8* %208, align 1
  %210 = zext i8 %209 to i32
  %211 = shl i32 %210, 24
  %212 = load i32, i32* %_hf_hashv, align 4
  %213 = add i32 %212, %211
  store i32 %213, i32* %_hf_hashv, align 4
  br label %214

; <label>:214                                     ; preds = %202, %206
  %215 = load i8*, i8** %_hj_key, align 8
  %216 = getelementptr inbounds i8, i8* %215, i64 9
  %217 = load i8, i8* %216, align 1
  %218 = zext i8 %217 to i32
  %219 = shl i32 %218, 16
  %220 = load i32, i32* %_hf_hashv, align 4
  %221 = add i32 %220, %219
  store i32 %221, i32* %_hf_hashv, align 4
  br label %222

; <label>:222                                     ; preds = %202, %214
  %223 = load i8*, i8** %_hj_key, align 8
  %224 = getelementptr inbounds i8, i8* %223, i64 8
  %225 = load i8, i8* %224, align 1
  %226 = zext i8 %225 to i32
  %227 = shl i32 %226, 8
  %228 = load i32, i32* %_hf_hashv, align 4
  %229 = add i32 %228, %227
  store i32 %229, i32* %_hf_hashv, align 4
  br label %230

; <label>:230                                     ; preds = %202, %222
  %231 = load i8*, i8** %_hj_key, align 8
  %232 = getelementptr inbounds i8, i8* %231, i64 7
  %233 = load i8, i8* %232, align 1
  %234 = zext i8 %233 to i32
  %235 = shl i32 %234, 24
  %236 = load i32, i32* %_hj_j, align 4
  %237 = add i32 %236, %235
  store i32 %237, i32* %_hj_j, align 4
  br label %238

; <label>:238                                     ; preds = %202, %230
  %239 = load i8*, i8** %_hj_key, align 8
  %240 = getelementptr inbounds i8, i8* %239, i64 6
  %241 = load i8, i8* %240, align 1
  %242 = zext i8 %241 to i32
  %243 = shl i32 %242, 16
  %244 = load i32, i32* %_hj_j, align 4
  %245 = add i32 %244, %243
  store i32 %245, i32* %_hj_j, align 4
  br label %246

; <label>:246                                     ; preds = %202, %238
  %247 = load i8*, i8** %_hj_key, align 8
  %248 = getelementptr inbounds i8, i8* %247, i64 5
  %249 = load i8, i8* %248, align 1
  %250 = zext i8 %249 to i32
  %251 = shl i32 %250, 8
  %252 = load i32, i32* %_hj_j, align 4
  %253 = add i32 %252, %251
  store i32 %253, i32* %_hj_j, align 4
  br label %254

; <label>:254                                     ; preds = %202, %246
  %255 = load i8*, i8** %_hj_key, align 8
  %256 = getelementptr inbounds i8, i8* %255, i64 4
  %257 = load i8, i8* %256, align 1
  %258 = zext i8 %257 to i32
  %259 = load i32, i32* %_hj_j, align 4
  %260 = add i32 %259, %258
  store i32 %260, i32* %_hj_j, align 4
  br label %261

; <label>:261                                     ; preds = %202, %254
  %262 = load i8*, i8** %_hj_key, align 8
  %263 = getelementptr inbounds i8, i8* %262, i64 3
  %264 = load i8, i8* %263, align 1
  %265 = zext i8 %264 to i32
  %266 = shl i32 %265, 24
  %267 = load i32, i32* %_hj_i, align 4
  %268 = add i32 %267, %266
  store i32 %268, i32* %_hj_i, align 4
  br label %269

; <label>:269                                     ; preds = %202, %261
  %270 = load i8*, i8** %_hj_key, align 8
  %271 = getelementptr inbounds i8, i8* %270, i64 2
  %272 = load i8, i8* %271, align 1
  %273 = zext i8 %272 to i32
  %274 = shl i32 %273, 16
  %275 = load i32, i32* %_hj_i, align 4
  %276 = add i32 %275, %274
  store i32 %276, i32* %_hj_i, align 4
  br label %277

; <label>:277                                     ; preds = %202, %269
  %278 = load i8*, i8** %_hj_key, align 8
  %279 = getelementptr inbounds i8, i8* %278, i64 1
  %280 = load i8, i8* %279, align 1
  %281 = zext i8 %280 to i32
  %282 = shl i32 %281, 8
  %283 = load i32, i32* %_hj_i, align 4
  %284 = add i32 %283, %282
  store i32 %284, i32* %_hj_i, align 4
  br label %285

; <label>:285                                     ; preds = %202, %277
  %286 = load i8*, i8** %_hj_key, align 8
  %287 = getelementptr inbounds i8, i8* %286, i64 0
  %288 = load i8, i8* %287, align 1
  %289 = zext i8 %288 to i32
  %290 = load i32, i32* %_hj_i, align 4
  %291 = add i32 %290, %289
  store i32 %291, i32* %_hj_i, align 4
  br label %292

; <label>:292                                     ; preds = %285, %202
  br label %293

; <label>:293                                     ; preds = %292
  %294 = load i32, i32* %_hj_j, align 4
  %295 = load i32, i32* %_hj_i, align 4
  %296 = sub i32 %295, %294
  store i32 %296, i32* %_hj_i, align 4
  %297 = load i32, i32* %_hf_hashv, align 4
  %298 = load i32, i32* %_hj_i, align 4
  %299 = sub i32 %298, %297
  store i32 %299, i32* %_hj_i, align 4
  %300 = load i32, i32* %_hf_hashv, align 4
  %301 = lshr i32 %300, 13
  %302 = load i32, i32* %_hj_i, align 4
  %303 = xor i32 %302, %301
  store i32 %303, i32* %_hj_i, align 4
  %304 = load i32, i32* %_hf_hashv, align 4
  %305 = load i32, i32* %_hj_j, align 4
  %306 = sub i32 %305, %304
  store i32 %306, i32* %_hj_j, align 4
  %307 = load i32, i32* %_hj_i, align 4
  %308 = load i32, i32* %_hj_j, align 4
  %309 = sub i32 %308, %307
  store i32 %309, i32* %_hj_j, align 4
  %310 = load i32, i32* %_hj_i, align 4
  %311 = shl i32 %310, 8
  %312 = load i32, i32* %_hj_j, align 4
  %313 = xor i32 %312, %311
  store i32 %313, i32* %_hj_j, align 4
  %314 = load i32, i32* %_hj_i, align 4
  %315 = load i32, i32* %_hf_hashv, align 4
  %316 = sub i32 %315, %314
  store i32 %316, i32* %_hf_hashv, align 4
  %317 = load i32, i32* %_hj_j, align 4
  %318 = load i32, i32* %_hf_hashv, align 4
  %319 = sub i32 %318, %317
  store i32 %319, i32* %_hf_hashv, align 4
  %320 = load i32, i32* %_hj_j, align 4
  %321 = lshr i32 %320, 13
  %322 = load i32, i32* %_hf_hashv, align 4
  %323 = xor i32 %322, %321
  store i32 %323, i32* %_hf_hashv, align 4
  %324 = load i32, i32* %_hj_j, align 4
  %325 = load i32, i32* %_hj_i, align 4
  %326 = sub i32 %325, %324
  store i32 %326, i32* %_hj_i, align 4
  %327 = load i32, i32* %_hf_hashv, align 4
  %328 = load i32, i32* %_hj_i, align 4
  %329 = sub i32 %328, %327
  store i32 %329, i32* %_hj_i, align 4
  %330 = load i32, i32* %_hf_hashv, align 4
  %331 = lshr i32 %330, 12
  %332 = load i32, i32* %_hj_i, align 4
  %333 = xor i32 %332, %331
  store i32 %333, i32* %_hj_i, align 4
  %334 = load i32, i32* %_hf_hashv, align 4
  %335 = load i32, i32* %_hj_j, align 4
  %336 = sub i32 %335, %334
  store i32 %336, i32* %_hj_j, align 4
  %337 = load i32, i32* %_hj_i, align 4
  %338 = load i32, i32* %_hj_j, align 4
  %339 = sub i32 %338, %337
  store i32 %339, i32* %_hj_j, align 4
  %340 = load i32, i32* %_hj_i, align 4
  %341 = shl i32 %340, 16
  %342 = load i32, i32* %_hj_j, align 4
  %343 = xor i32 %342, %341
  store i32 %343, i32* %_hj_j, align 4
  %344 = load i32, i32* %_hj_i, align 4
  %345 = load i32, i32* %_hf_hashv, align 4
  %346 = sub i32 %345, %344
  store i32 %346, i32* %_hf_hashv, align 4
  %347 = load i32, i32* %_hj_j, align 4
  %348 = load i32, i32* %_hf_hashv, align 4
  %349 = sub i32 %348, %347
  store i32 %349, i32* %_hf_hashv, align 4
  %350 = load i32, i32* %_hj_j, align 4
  %351 = lshr i32 %350, 5
  %352 = load i32, i32* %_hf_hashv, align 4
  %353 = xor i32 %352, %351
  store i32 %353, i32* %_hf_hashv, align 4
  %354 = load i32, i32* %_hj_j, align 4
  %355 = load i32, i32* %_hj_i, align 4
  %356 = sub i32 %355, %354
  store i32 %356, i32* %_hj_i, align 4
  %357 = load i32, i32* %_hf_hashv, align 4
  %358 = load i32, i32* %_hj_i, align 4
  %359 = sub i32 %358, %357
  store i32 %359, i32* %_hj_i, align 4
  %360 = load i32, i32* %_hf_hashv, align 4
  %361 = lshr i32 %360, 3
  %362 = load i32, i32* %_hj_i, align 4
  %363 = xor i32 %362, %361
  store i32 %363, i32* %_hj_i, align 4
  %364 = load i32, i32* %_hf_hashv, align 4
  %365 = load i32, i32* %_hj_j, align 4
  %366 = sub i32 %365, %364
  store i32 %366, i32* %_hj_j, align 4
  %367 = load i32, i32* %_hj_i, align 4
  %368 = load i32, i32* %_hj_j, align 4
  %369 = sub i32 %368, %367
  store i32 %369, i32* %_hj_j, align 4
  %370 = load i32, i32* %_hj_i, align 4
  %371 = shl i32 %370, 10
  %372 = load i32, i32* %_hj_j, align 4
  %373 = xor i32 %372, %371
  store i32 %373, i32* %_hj_j, align 4
  %374 = load i32, i32* %_hj_i, align 4
  %375 = load i32, i32* %_hf_hashv, align 4
  %376 = sub i32 %375, %374
  store i32 %376, i32* %_hf_hashv, align 4
  %377 = load i32, i32* %_hj_j, align 4
  %378 = load i32, i32* %_hf_hashv, align 4
  %379 = sub i32 %378, %377
  store i32 %379, i32* %_hf_hashv, align 4
  %380 = load i32, i32* %_hj_j, align 4
  %381 = lshr i32 %380, 15
  %382 = load i32, i32* %_hf_hashv, align 4
  %383 = xor i32 %382, %381
  store i32 %383, i32* %_hf_hashv, align 4
  br label %384

; <label>:384                                     ; preds = %293
  br label %385

; <label>:385                                     ; preds = %384
  br label %386

; <label>:386                                     ; preds = %385
  br label %387

; <label>:387                                     ; preds = %386
  store %struct.hash_tbl* null, %struct.hash_tbl** %tbl, align 8
  %388 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %389 = icmp ne %struct.hash_tbl* %388, null
  br i1 %389, label %390, label %495

; <label>:390                                     ; preds = %387
  br label %391

; <label>:391                                     ; preds = %390
  %392 = load i32, i32* %_hf_hashv, align 4
  %393 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %394 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %393, i32 0, i32 2
  %395 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %394, i32 0, i32 0
  %396 = load %struct.UT_hash_table*, %struct.UT_hash_table** %395, align 8
  %397 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %396, i32 0, i32 1
  %398 = load i32, i32* %397, align 8
  %399 = sub i32 %398, 1
  %400 = and i32 %392, %399
  store i32 %400, i32* %_hf_bkt, align 4
  br label %401

; <label>:401                                     ; preds = %391
  br label %402

; <label>:402                                     ; preds = %401
  %403 = load i32, i32* %_hf_bkt, align 4
  %404 = zext i32 %403 to i64
  %405 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %406 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %405, i32 0, i32 2
  %407 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %406, i32 0, i32 0
  %408 = load %struct.UT_hash_table*, %struct.UT_hash_table** %407, align 8
  %409 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %408, i32 0, i32 0
  %410 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %409, align 8
  %411 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %410, i64 %404
  %412 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %411, i32 0, i32 0
  %413 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %412, align 8
  %414 = icmp ne %struct.UT_hash_handle* %413, null
  br i1 %414, label %415, label %439

; <label>:415                                     ; preds = %402
  br label %416

; <label>:416                                     ; preds = %415
  %417 = load i32, i32* %_hf_bkt, align 4
  %418 = zext i32 %417 to i64
  %419 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %420 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %419, i32 0, i32 2
  %421 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %420, i32 0, i32 0
  %422 = load %struct.UT_hash_table*, %struct.UT_hash_table** %421, align 8
  %423 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %422, i32 0, i32 0
  %424 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %423, align 8
  %425 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %424, i64 %418
  %426 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %425, i32 0, i32 0
  %427 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %426, align 8
  %428 = bitcast %struct.UT_hash_handle* %427 to i8*
  %429 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %430 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %429, i32 0, i32 2
  %431 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %430, i32 0, i32 0
  %432 = load %struct.UT_hash_table*, %struct.UT_hash_table** %431, align 8
  %433 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %432, i32 0, i32 5
  %434 = load i64, i64* %433, align 8
  %435 = sub i64 0, %434
  %436 = getelementptr inbounds i8, i8* %428, i64 %435
  %437 = bitcast i8* %436 to %struct.hash_tbl*
  store %struct.hash_tbl* %437, %struct.hash_tbl** %tbl, align 8
  br label %438

; <label>:438                                     ; preds = %416
  br label %440

; <label>:439                                     ; preds = %402
  store %struct.hash_tbl* null, %struct.hash_tbl** %tbl, align 8
  br label %440

; <label>:440                                     ; preds = %439, %438
  br label %441

; <label>:441                                     ; preds = %492, %440
  %442 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %443 = icmp ne %struct.hash_tbl* %442, null
  br i1 %443, label %444, label %493

; <label>:444                                     ; preds = %441
  %445 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %446 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %445, i32 0, i32 2
  %447 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %446, i32 0, i32 7
  %448 = load i32, i32* %447, align 4
  %449 = load i32, i32* %_hf_hashv, align 4
  %450 = icmp eq i32 %448, %449
  br i1 %450, label %451, label %468

; <label>:451                                     ; preds = %444
  %452 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %453 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %452, i32 0, i32 2
  %454 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %453, i32 0, i32 6
  %455 = load i32, i32* %454, align 8
  %456 = zext i32 %455 to i64
  %457 = icmp eq i64 %456, 8
  br i1 %457, label %458, label %468

; <label>:458                                     ; preds = %451
  %459 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %460 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %459, i32 0, i32 2
  %461 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %460, i32 0, i32 5
  %462 = load i8*, i8** %461, align 8
  %463 = bitcast i64* %2 to i8*
  %464 = call i32 @memcmp(i8* %462, i8* %463, i64 8) #11
  %465 = icmp eq i32 %464, 0
  br i1 %465, label %466, label %467

; <label>:466                                     ; preds = %458
  br label %493

; <label>:467                                     ; preds = %458
  br label %468

; <label>:468                                     ; preds = %467, %451, %444
  %469 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %470 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %469, i32 0, i32 2
  %471 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %470, i32 0, i32 4
  %472 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %471, align 8
  %473 = icmp ne %struct.UT_hash_handle* %472, null
  br i1 %473, label %474, label %491

; <label>:474                                     ; preds = %468
  br label %475

; <label>:475                                     ; preds = %474
  %476 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %477 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %476, i32 0, i32 2
  %478 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %477, i32 0, i32 4
  %479 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %478, align 8
  %480 = bitcast %struct.UT_hash_handle* %479 to i8*
  %481 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %482 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %481, i32 0, i32 2
  %483 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %482, i32 0, i32 0
  %484 = load %struct.UT_hash_table*, %struct.UT_hash_table** %483, align 8
  %485 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %484, i32 0, i32 5
  %486 = load i64, i64* %485, align 8
  %487 = sub i64 0, %486
  %488 = getelementptr inbounds i8, i8* %480, i64 %487
  %489 = bitcast i8* %488 to %struct.hash_tbl*
  store %struct.hash_tbl* %489, %struct.hash_tbl** %tbl, align 8
  br label %490

; <label>:490                                     ; preds = %475
  br label %492

; <label>:491                                     ; preds = %468
  store %struct.hash_tbl* null, %struct.hash_tbl** %tbl, align 8
  br label %492

; <label>:492                                     ; preds = %491, %490
  br label %441

; <label>:493                                     ; preds = %466, %441
  br label %494

; <label>:494                                     ; preds = %493
  br label %495

; <label>:495                                     ; preds = %494, %387
  br label %496

; <label>:496                                     ; preds = %495
  br label %497

; <label>:497                                     ; preds = %496
  %498 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %499 = icmp ne %struct.hash_tbl* %498, null
  br i1 %499, label %500, label %504

; <label>:500                                     ; preds = %497
  %501 = load i64, i64* %3, align 8
  %502 = load %struct.hash_tbl*, %struct.hash_tbl** %tbl, align 8
  %503 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %502, i32 0, i32 1
  store i64 %501, i64* %503, align 8
  br label %1355

; <label>:504                                     ; preds = %497
  store %struct.hash_tbl* null, %struct.hash_tbl** %new_pair, align 8
  %505 = call noalias i8* @malloc(i64 72) #2
  %506 = bitcast i8* %505 to %struct.hash_tbl*
  store %struct.hash_tbl* %506, %struct.hash_tbl** %new_pair, align 8
  %507 = load i64, i64* %2, align 8
  %508 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %509 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %508, i32 0, i32 0
  store i64 %507, i64* %509, align 8
  %510 = load i64, i64* %3, align 8
  %511 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %512 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %511, i32 0, i32 1
  store i64 %510, i64* %512, align 8
  br label %513

; <label>:513                                     ; preds = %504
  br label %514

; <label>:514                                     ; preds = %513
  br label %515

; <label>:515                                     ; preds = %514
  %516 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %517 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %516, i32 0, i32 0
  %518 = bitcast i64* %517 to i8*
  store i8* %518, i8** %_hj_key4, align 8
  store i32 -17973521, i32* %_ha_hashv, align 4
  store i32 -1640531527, i32* %_hj_j2, align 4
  store i32 -1640531527, i32* %_hj_i1, align 4
  store i32 8, i32* %_hj_k3, align 4
  br label %519

; <label>:519                                     ; preds = %686, %515
  %520 = load i32, i32* %_hj_k3, align 4
  %521 = icmp uge i32 %520, 12
  br i1 %521, label %522, label %691

; <label>:522                                     ; preds = %519
  %523 = load i8*, i8** %_hj_key4, align 8
  %524 = getelementptr inbounds i8, i8* %523, i64 0
  %525 = load i8, i8* %524, align 1
  %526 = zext i8 %525 to i32
  %527 = load i8*, i8** %_hj_key4, align 8
  %528 = getelementptr inbounds i8, i8* %527, i64 1
  %529 = load i8, i8* %528, align 1
  %530 = zext i8 %529 to i32
  %531 = shl i32 %530, 8
  %532 = add i32 %526, %531
  %533 = load i8*, i8** %_hj_key4, align 8
  %534 = getelementptr inbounds i8, i8* %533, i64 2
  %535 = load i8, i8* %534, align 1
  %536 = zext i8 %535 to i32
  %537 = shl i32 %536, 16
  %538 = add i32 %532, %537
  %539 = load i8*, i8** %_hj_key4, align 8
  %540 = getelementptr inbounds i8, i8* %539, i64 3
  %541 = load i8, i8* %540, align 1
  %542 = zext i8 %541 to i32
  %543 = shl i32 %542, 24
  %544 = add i32 %538, %543
  %545 = load i32, i32* %_hj_i1, align 4
  %546 = add i32 %545, %544
  store i32 %546, i32* %_hj_i1, align 4
  %547 = load i8*, i8** %_hj_key4, align 8
  %548 = getelementptr inbounds i8, i8* %547, i64 4
  %549 = load i8, i8* %548, align 1
  %550 = zext i8 %549 to i32
  %551 = load i8*, i8** %_hj_key4, align 8
  %552 = getelementptr inbounds i8, i8* %551, i64 5
  %553 = load i8, i8* %552, align 1
  %554 = zext i8 %553 to i32
  %555 = shl i32 %554, 8
  %556 = add i32 %550, %555
  %557 = load i8*, i8** %_hj_key4, align 8
  %558 = getelementptr inbounds i8, i8* %557, i64 6
  %559 = load i8, i8* %558, align 1
  %560 = zext i8 %559 to i32
  %561 = shl i32 %560, 16
  %562 = add i32 %556, %561
  %563 = load i8*, i8** %_hj_key4, align 8
  %564 = getelementptr inbounds i8, i8* %563, i64 7
  %565 = load i8, i8* %564, align 1
  %566 = zext i8 %565 to i32
  %567 = shl i32 %566, 24
  %568 = add i32 %562, %567
  %569 = load i32, i32* %_hj_j2, align 4
  %570 = add i32 %569, %568
  store i32 %570, i32* %_hj_j2, align 4
  %571 = load i8*, i8** %_hj_key4, align 8
  %572 = getelementptr inbounds i8, i8* %571, i64 8
  %573 = load i8, i8* %572, align 1
  %574 = zext i8 %573 to i32
  %575 = load i8*, i8** %_hj_key4, align 8
  %576 = getelementptr inbounds i8, i8* %575, i64 9
  %577 = load i8, i8* %576, align 1
  %578 = zext i8 %577 to i32
  %579 = shl i32 %578, 8
  %580 = add i32 %574, %579
  %581 = load i8*, i8** %_hj_key4, align 8
  %582 = getelementptr inbounds i8, i8* %581, i64 10
  %583 = load i8, i8* %582, align 1
  %584 = zext i8 %583 to i32
  %585 = shl i32 %584, 16
  %586 = add i32 %580, %585
  %587 = load i8*, i8** %_hj_key4, align 8
  %588 = getelementptr inbounds i8, i8* %587, i64 11
  %589 = load i8, i8* %588, align 1
  %590 = zext i8 %589 to i32
  %591 = shl i32 %590, 24
  %592 = add i32 %586, %591
  %593 = load i32, i32* %_ha_hashv, align 4
  %594 = add i32 %593, %592
  store i32 %594, i32* %_ha_hashv, align 4
  br label %595

; <label>:595                                     ; preds = %522
  %596 = load i32, i32* %_hj_j2, align 4
  %597 = load i32, i32* %_hj_i1, align 4
  %598 = sub i32 %597, %596
  store i32 %598, i32* %_hj_i1, align 4
  %599 = load i32, i32* %_ha_hashv, align 4
  %600 = load i32, i32* %_hj_i1, align 4
  %601 = sub i32 %600, %599
  store i32 %601, i32* %_hj_i1, align 4
  %602 = load i32, i32* %_ha_hashv, align 4
  %603 = lshr i32 %602, 13
  %604 = load i32, i32* %_hj_i1, align 4
  %605 = xor i32 %604, %603
  store i32 %605, i32* %_hj_i1, align 4
  %606 = load i32, i32* %_ha_hashv, align 4
  %607 = load i32, i32* %_hj_j2, align 4
  %608 = sub i32 %607, %606
  store i32 %608, i32* %_hj_j2, align 4
  %609 = load i32, i32* %_hj_i1, align 4
  %610 = load i32, i32* %_hj_j2, align 4
  %611 = sub i32 %610, %609
  store i32 %611, i32* %_hj_j2, align 4
  %612 = load i32, i32* %_hj_i1, align 4
  %613 = shl i32 %612, 8
  %614 = load i32, i32* %_hj_j2, align 4
  %615 = xor i32 %614, %613
  store i32 %615, i32* %_hj_j2, align 4
  %616 = load i32, i32* %_hj_i1, align 4
  %617 = load i32, i32* %_ha_hashv, align 4
  %618 = sub i32 %617, %616
  store i32 %618, i32* %_ha_hashv, align 4
  %619 = load i32, i32* %_hj_j2, align 4
  %620 = load i32, i32* %_ha_hashv, align 4
  %621 = sub i32 %620, %619
  store i32 %621, i32* %_ha_hashv, align 4
  %622 = load i32, i32* %_hj_j2, align 4
  %623 = lshr i32 %622, 13
  %624 = load i32, i32* %_ha_hashv, align 4
  %625 = xor i32 %624, %623
  store i32 %625, i32* %_ha_hashv, align 4
  %626 = load i32, i32* %_hj_j2, align 4
  %627 = load i32, i32* %_hj_i1, align 4
  %628 = sub i32 %627, %626
  store i32 %628, i32* %_hj_i1, align 4
  %629 = load i32, i32* %_ha_hashv, align 4
  %630 = load i32, i32* %_hj_i1, align 4
  %631 = sub i32 %630, %629
  store i32 %631, i32* %_hj_i1, align 4
  %632 = load i32, i32* %_ha_hashv, align 4
  %633 = lshr i32 %632, 12
  %634 = load i32, i32* %_hj_i1, align 4
  %635 = xor i32 %634, %633
  store i32 %635, i32* %_hj_i1, align 4
  %636 = load i32, i32* %_ha_hashv, align 4
  %637 = load i32, i32* %_hj_j2, align 4
  %638 = sub i32 %637, %636
  store i32 %638, i32* %_hj_j2, align 4
  %639 = load i32, i32* %_hj_i1, align 4
  %640 = load i32, i32* %_hj_j2, align 4
  %641 = sub i32 %640, %639
  store i32 %641, i32* %_hj_j2, align 4
  %642 = load i32, i32* %_hj_i1, align 4
  %643 = shl i32 %642, 16
  %644 = load i32, i32* %_hj_j2, align 4
  %645 = xor i32 %644, %643
  store i32 %645, i32* %_hj_j2, align 4
  %646 = load i32, i32* %_hj_i1, align 4
  %647 = load i32, i32* %_ha_hashv, align 4
  %648 = sub i32 %647, %646
  store i32 %648, i32* %_ha_hashv, align 4
  %649 = load i32, i32* %_hj_j2, align 4
  %650 = load i32, i32* %_ha_hashv, align 4
  %651 = sub i32 %650, %649
  store i32 %651, i32* %_ha_hashv, align 4
  %652 = load i32, i32* %_hj_j2, align 4
  %653 = lshr i32 %652, 5
  %654 = load i32, i32* %_ha_hashv, align 4
  %655 = xor i32 %654, %653
  store i32 %655, i32* %_ha_hashv, align 4
  %656 = load i32, i32* %_hj_j2, align 4
  %657 = load i32, i32* %_hj_i1, align 4
  %658 = sub i32 %657, %656
  store i32 %658, i32* %_hj_i1, align 4
  %659 = load i32, i32* %_ha_hashv, align 4
  %660 = load i32, i32* %_hj_i1, align 4
  %661 = sub i32 %660, %659
  store i32 %661, i32* %_hj_i1, align 4
  %662 = load i32, i32* %_ha_hashv, align 4
  %663 = lshr i32 %662, 3
  %664 = load i32, i32* %_hj_i1, align 4
  %665 = xor i32 %664, %663
  store i32 %665, i32* %_hj_i1, align 4
  %666 = load i32, i32* %_ha_hashv, align 4
  %667 = load i32, i32* %_hj_j2, align 4
  %668 = sub i32 %667, %666
  store i32 %668, i32* %_hj_j2, align 4
  %669 = load i32, i32* %_hj_i1, align 4
  %670 = load i32, i32* %_hj_j2, align 4
  %671 = sub i32 %670, %669
  store i32 %671, i32* %_hj_j2, align 4
  %672 = load i32, i32* %_hj_i1, align 4
  %673 = shl i32 %672, 10
  %674 = load i32, i32* %_hj_j2, align 4
  %675 = xor i32 %674, %673
  store i32 %675, i32* %_hj_j2, align 4
  %676 = load i32, i32* %_hj_i1, align 4
  %677 = load i32, i32* %_ha_hashv, align 4
  %678 = sub i32 %677, %676
  store i32 %678, i32* %_ha_hashv, align 4
  %679 = load i32, i32* %_hj_j2, align 4
  %680 = load i32, i32* %_ha_hashv, align 4
  %681 = sub i32 %680, %679
  store i32 %681, i32* %_ha_hashv, align 4
  %682 = load i32, i32* %_hj_j2, align 4
  %683 = lshr i32 %682, 15
  %684 = load i32, i32* %_ha_hashv, align 4
  %685 = xor i32 %684, %683
  store i32 %685, i32* %_ha_hashv, align 4
  br label %686

; <label>:686                                     ; preds = %595
  %687 = load i8*, i8** %_hj_key4, align 8
  %688 = getelementptr inbounds i8, i8* %687, i64 12
  store i8* %688, i8** %_hj_key4, align 8
  %689 = load i32, i32* %_hj_k3, align 4
  %690 = sub i32 %689, 12
  store i32 %690, i32* %_hj_k3, align 4
  br label %519

; <label>:691                                     ; preds = %519
  %692 = load i32, i32* %_ha_hashv, align 4
  %693 = add i32 %692, 8
  store i32 %693, i32* %_ha_hashv, align 4
  %694 = load i32, i32* %_hj_k3, align 4
  switch i32 %694, label %781 [
    i32 11, label %695
    i32 10, label %703
    i32 9, label %711
    i32 8, label %719
    i32 7, label %727
    i32 6, label %735
    i32 5, label %743
    i32 4, label %750
    i32 3, label %758
    i32 2, label %766
    i32 1, label %774
  ]

; <label>:695                                     ; preds = %691
  %696 = load i8*, i8** %_hj_key4, align 8
  %697 = getelementptr inbounds i8, i8* %696, i64 10
  %698 = load i8, i8* %697, align 1
  %699 = zext i8 %698 to i32
  %700 = shl i32 %699, 24
  %701 = load i32, i32* %_ha_hashv, align 4
  %702 = add i32 %701, %700
  store i32 %702, i32* %_ha_hashv, align 4
  br label %703

; <label>:703                                     ; preds = %691, %695
  %704 = load i8*, i8** %_hj_key4, align 8
  %705 = getelementptr inbounds i8, i8* %704, i64 9
  %706 = load i8, i8* %705, align 1
  %707 = zext i8 %706 to i32
  %708 = shl i32 %707, 16
  %709 = load i32, i32* %_ha_hashv, align 4
  %710 = add i32 %709, %708
  store i32 %710, i32* %_ha_hashv, align 4
  br label %711

; <label>:711                                     ; preds = %691, %703
  %712 = load i8*, i8** %_hj_key4, align 8
  %713 = getelementptr inbounds i8, i8* %712, i64 8
  %714 = load i8, i8* %713, align 1
  %715 = zext i8 %714 to i32
  %716 = shl i32 %715, 8
  %717 = load i32, i32* %_ha_hashv, align 4
  %718 = add i32 %717, %716
  store i32 %718, i32* %_ha_hashv, align 4
  br label %719

; <label>:719                                     ; preds = %691, %711
  %720 = load i8*, i8** %_hj_key4, align 8
  %721 = getelementptr inbounds i8, i8* %720, i64 7
  %722 = load i8, i8* %721, align 1
  %723 = zext i8 %722 to i32
  %724 = shl i32 %723, 24
  %725 = load i32, i32* %_hj_j2, align 4
  %726 = add i32 %725, %724
  store i32 %726, i32* %_hj_j2, align 4
  br label %727

; <label>:727                                     ; preds = %691, %719
  %728 = load i8*, i8** %_hj_key4, align 8
  %729 = getelementptr inbounds i8, i8* %728, i64 6
  %730 = load i8, i8* %729, align 1
  %731 = zext i8 %730 to i32
  %732 = shl i32 %731, 16
  %733 = load i32, i32* %_hj_j2, align 4
  %734 = add i32 %733, %732
  store i32 %734, i32* %_hj_j2, align 4
  br label %735

; <label>:735                                     ; preds = %691, %727
  %736 = load i8*, i8** %_hj_key4, align 8
  %737 = getelementptr inbounds i8, i8* %736, i64 5
  %738 = load i8, i8* %737, align 1
  %739 = zext i8 %738 to i32
  %740 = shl i32 %739, 8
  %741 = load i32, i32* %_hj_j2, align 4
  %742 = add i32 %741, %740
  store i32 %742, i32* %_hj_j2, align 4
  br label %743

; <label>:743                                     ; preds = %691, %735
  %744 = load i8*, i8** %_hj_key4, align 8
  %745 = getelementptr inbounds i8, i8* %744, i64 4
  %746 = load i8, i8* %745, align 1
  %747 = zext i8 %746 to i32
  %748 = load i32, i32* %_hj_j2, align 4
  %749 = add i32 %748, %747
  store i32 %749, i32* %_hj_j2, align 4
  br label %750

; <label>:750                                     ; preds = %691, %743
  %751 = load i8*, i8** %_hj_key4, align 8
  %752 = getelementptr inbounds i8, i8* %751, i64 3
  %753 = load i8, i8* %752, align 1
  %754 = zext i8 %753 to i32
  %755 = shl i32 %754, 24
  %756 = load i32, i32* %_hj_i1, align 4
  %757 = add i32 %756, %755
  store i32 %757, i32* %_hj_i1, align 4
  br label %758

; <label>:758                                     ; preds = %691, %750
  %759 = load i8*, i8** %_hj_key4, align 8
  %760 = getelementptr inbounds i8, i8* %759, i64 2
  %761 = load i8, i8* %760, align 1
  %762 = zext i8 %761 to i32
  %763 = shl i32 %762, 16
  %764 = load i32, i32* %_hj_i1, align 4
  %765 = add i32 %764, %763
  store i32 %765, i32* %_hj_i1, align 4
  br label %766

; <label>:766                                     ; preds = %691, %758
  %767 = load i8*, i8** %_hj_key4, align 8
  %768 = getelementptr inbounds i8, i8* %767, i64 1
  %769 = load i8, i8* %768, align 1
  %770 = zext i8 %769 to i32
  %771 = shl i32 %770, 8
  %772 = load i32, i32* %_hj_i1, align 4
  %773 = add i32 %772, %771
  store i32 %773, i32* %_hj_i1, align 4
  br label %774

; <label>:774                                     ; preds = %691, %766
  %775 = load i8*, i8** %_hj_key4, align 8
  %776 = getelementptr inbounds i8, i8* %775, i64 0
  %777 = load i8, i8* %776, align 1
  %778 = zext i8 %777 to i32
  %779 = load i32, i32* %_hj_i1, align 4
  %780 = add i32 %779, %778
  store i32 %780, i32* %_hj_i1, align 4
  br label %781

; <label>:781                                     ; preds = %774, %691
  br label %782

; <label>:782                                     ; preds = %781
  %783 = load i32, i32* %_hj_j2, align 4
  %784 = load i32, i32* %_hj_i1, align 4
  %785 = sub i32 %784, %783
  store i32 %785, i32* %_hj_i1, align 4
  %786 = load i32, i32* %_ha_hashv, align 4
  %787 = load i32, i32* %_hj_i1, align 4
  %788 = sub i32 %787, %786
  store i32 %788, i32* %_hj_i1, align 4
  %789 = load i32, i32* %_ha_hashv, align 4
  %790 = lshr i32 %789, 13
  %791 = load i32, i32* %_hj_i1, align 4
  %792 = xor i32 %791, %790
  store i32 %792, i32* %_hj_i1, align 4
  %793 = load i32, i32* %_ha_hashv, align 4
  %794 = load i32, i32* %_hj_j2, align 4
  %795 = sub i32 %794, %793
  store i32 %795, i32* %_hj_j2, align 4
  %796 = load i32, i32* %_hj_i1, align 4
  %797 = load i32, i32* %_hj_j2, align 4
  %798 = sub i32 %797, %796
  store i32 %798, i32* %_hj_j2, align 4
  %799 = load i32, i32* %_hj_i1, align 4
  %800 = shl i32 %799, 8
  %801 = load i32, i32* %_hj_j2, align 4
  %802 = xor i32 %801, %800
  store i32 %802, i32* %_hj_j2, align 4
  %803 = load i32, i32* %_hj_i1, align 4
  %804 = load i32, i32* %_ha_hashv, align 4
  %805 = sub i32 %804, %803
  store i32 %805, i32* %_ha_hashv, align 4
  %806 = load i32, i32* %_hj_j2, align 4
  %807 = load i32, i32* %_ha_hashv, align 4
  %808 = sub i32 %807, %806
  store i32 %808, i32* %_ha_hashv, align 4
  %809 = load i32, i32* %_hj_j2, align 4
  %810 = lshr i32 %809, 13
  %811 = load i32, i32* %_ha_hashv, align 4
  %812 = xor i32 %811, %810
  store i32 %812, i32* %_ha_hashv, align 4
  %813 = load i32, i32* %_hj_j2, align 4
  %814 = load i32, i32* %_hj_i1, align 4
  %815 = sub i32 %814, %813
  store i32 %815, i32* %_hj_i1, align 4
  %816 = load i32, i32* %_ha_hashv, align 4
  %817 = load i32, i32* %_hj_i1, align 4
  %818 = sub i32 %817, %816
  store i32 %818, i32* %_hj_i1, align 4
  %819 = load i32, i32* %_ha_hashv, align 4
  %820 = lshr i32 %819, 12
  %821 = load i32, i32* %_hj_i1, align 4
  %822 = xor i32 %821, %820
  store i32 %822, i32* %_hj_i1, align 4
  %823 = load i32, i32* %_ha_hashv, align 4
  %824 = load i32, i32* %_hj_j2, align 4
  %825 = sub i32 %824, %823
  store i32 %825, i32* %_hj_j2, align 4
  %826 = load i32, i32* %_hj_i1, align 4
  %827 = load i32, i32* %_hj_j2, align 4
  %828 = sub i32 %827, %826
  store i32 %828, i32* %_hj_j2, align 4
  %829 = load i32, i32* %_hj_i1, align 4
  %830 = shl i32 %829, 16
  %831 = load i32, i32* %_hj_j2, align 4
  %832 = xor i32 %831, %830
  store i32 %832, i32* %_hj_j2, align 4
  %833 = load i32, i32* %_hj_i1, align 4
  %834 = load i32, i32* %_ha_hashv, align 4
  %835 = sub i32 %834, %833
  store i32 %835, i32* %_ha_hashv, align 4
  %836 = load i32, i32* %_hj_j2, align 4
  %837 = load i32, i32* %_ha_hashv, align 4
  %838 = sub i32 %837, %836
  store i32 %838, i32* %_ha_hashv, align 4
  %839 = load i32, i32* %_hj_j2, align 4
  %840 = lshr i32 %839, 5
  %841 = load i32, i32* %_ha_hashv, align 4
  %842 = xor i32 %841, %840
  store i32 %842, i32* %_ha_hashv, align 4
  %843 = load i32, i32* %_hj_j2, align 4
  %844 = load i32, i32* %_hj_i1, align 4
  %845 = sub i32 %844, %843
  store i32 %845, i32* %_hj_i1, align 4
  %846 = load i32, i32* %_ha_hashv, align 4
  %847 = load i32, i32* %_hj_i1, align 4
  %848 = sub i32 %847, %846
  store i32 %848, i32* %_hj_i1, align 4
  %849 = load i32, i32* %_ha_hashv, align 4
  %850 = lshr i32 %849, 3
  %851 = load i32, i32* %_hj_i1, align 4
  %852 = xor i32 %851, %850
  store i32 %852, i32* %_hj_i1, align 4
  %853 = load i32, i32* %_ha_hashv, align 4
  %854 = load i32, i32* %_hj_j2, align 4
  %855 = sub i32 %854, %853
  store i32 %855, i32* %_hj_j2, align 4
  %856 = load i32, i32* %_hj_i1, align 4
  %857 = load i32, i32* %_hj_j2, align 4
  %858 = sub i32 %857, %856
  store i32 %858, i32* %_hj_j2, align 4
  %859 = load i32, i32* %_hj_i1, align 4
  %860 = shl i32 %859, 10
  %861 = load i32, i32* %_hj_j2, align 4
  %862 = xor i32 %861, %860
  store i32 %862, i32* %_hj_j2, align 4
  %863 = load i32, i32* %_hj_i1, align 4
  %864 = load i32, i32* %_ha_hashv, align 4
  %865 = sub i32 %864, %863
  store i32 %865, i32* %_ha_hashv, align 4
  %866 = load i32, i32* %_hj_j2, align 4
  %867 = load i32, i32* %_ha_hashv, align 4
  %868 = sub i32 %867, %866
  store i32 %868, i32* %_ha_hashv, align 4
  %869 = load i32, i32* %_hj_j2, align 4
  %870 = lshr i32 %869, 15
  %871 = load i32, i32* %_ha_hashv, align 4
  %872 = xor i32 %871, %870
  store i32 %872, i32* %_ha_hashv, align 4
  br label %873

; <label>:873                                     ; preds = %782
  br label %874

; <label>:874                                     ; preds = %873
  br label %875

; <label>:875                                     ; preds = %874
  br label %876

; <label>:876                                     ; preds = %875
  %877 = load i32, i32* %_ha_hashv, align 4
  %878 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %879 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %878, i32 0, i32 2
  %880 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %879, i32 0, i32 7
  store i32 %877, i32* %880, align 4
  %881 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %882 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %881, i32 0, i32 0
  %883 = bitcast i64* %882 to i8*
  %884 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %885 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %884, i32 0, i32 2
  %886 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %885, i32 0, i32 5
  store i8* %883, i8** %886, align 8
  %887 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %888 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %887, i32 0, i32 2
  %889 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %888, i32 0, i32 6
  store i32 8, i32* %889, align 8
  %890 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %891 = icmp ne %struct.hash_tbl* %890, null
  br i1 %891, label %979, label %892

; <label>:892                                     ; preds = %876
  %893 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %894 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %893, i32 0, i32 2
  %895 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %894, i32 0, i32 2
  store i8* null, i8** %895, align 8
  %896 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %897 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %896, i32 0, i32 2
  %898 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %897, i32 0, i32 1
  store i8* null, i8** %898, align 8
  br label %899

; <label>:899                                     ; preds = %892
  %900 = call noalias i8* @malloc(i64 64) #2
  %901 = bitcast i8* %900 to %struct.UT_hash_table*
  %902 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %903 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %902, i32 0, i32 2
  %904 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %903, i32 0, i32 0
  store %struct.UT_hash_table* %901, %struct.UT_hash_table** %904, align 8
  %905 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %906 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %905, i32 0, i32 2
  %907 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %906, i32 0, i32 0
  %908 = load %struct.UT_hash_table*, %struct.UT_hash_table** %907, align 8
  %909 = icmp ne %struct.UT_hash_table* %908, null
  br i1 %909, label %911, label %910

; <label>:910                                     ; preds = %899
  call void @exit(i32 -1) #9
  unreachable

; <label>:911                                     ; preds = %899
  %912 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %913 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %912, i32 0, i32 2
  %914 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %913, i32 0, i32 0
  %915 = load %struct.UT_hash_table*, %struct.UT_hash_table** %914, align 8
  %916 = bitcast %struct.UT_hash_table* %915 to i8*
  call void @llvm.memset.p0i8.i64(i8* %916, i8 0, i64 64, i32 8, i1 false)
  %917 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %918 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %917, i32 0, i32 2
  %919 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %920 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %919, i32 0, i32 2
  %921 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %920, i32 0, i32 0
  %922 = load %struct.UT_hash_table*, %struct.UT_hash_table** %921, align 8
  %923 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %922, i32 0, i32 4
  store %struct.UT_hash_handle* %918, %struct.UT_hash_handle** %923, align 8
  %924 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %925 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %924, i32 0, i32 2
  %926 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %925, i32 0, i32 0
  %927 = load %struct.UT_hash_table*, %struct.UT_hash_table** %926, align 8
  %928 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %927, i32 0, i32 1
  store i32 32, i32* %928, align 8
  %929 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %930 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %929, i32 0, i32 2
  %931 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %930, i32 0, i32 0
  %932 = load %struct.UT_hash_table*, %struct.UT_hash_table** %931, align 8
  %933 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %932, i32 0, i32 2
  store i32 5, i32* %933, align 4
  %934 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %935 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %934, i32 0, i32 2
  %936 = bitcast %struct.UT_hash_handle* %935 to i8*
  %937 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %938 = bitcast %struct.hash_tbl* %937 to i8*
  %939 = ptrtoint i8* %936 to i64
  %940 = ptrtoint i8* %938 to i64
  %941 = sub i64 %939, %940
  %942 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %943 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %942, i32 0, i32 2
  %944 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %943, i32 0, i32 0
  %945 = load %struct.UT_hash_table*, %struct.UT_hash_table** %944, align 8
  %946 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %945, i32 0, i32 5
  store i64 %941, i64* %946, align 8
  %947 = call noalias i8* @malloc(i64 512) #2
  %948 = bitcast i8* %947 to %struct.UT_hash_bucket*
  %949 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %950 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %949, i32 0, i32 2
  %951 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %950, i32 0, i32 0
  %952 = load %struct.UT_hash_table*, %struct.UT_hash_table** %951, align 8
  %953 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %952, i32 0, i32 0
  store %struct.UT_hash_bucket* %948, %struct.UT_hash_bucket** %953, align 8
  %954 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %955 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %954, i32 0, i32 2
  %956 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %955, i32 0, i32 0
  %957 = load %struct.UT_hash_table*, %struct.UT_hash_table** %956, align 8
  %958 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %957, i32 0, i32 10
  store i32 -1609490463, i32* %958, align 8
  %959 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %960 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %959, i32 0, i32 2
  %961 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %960, i32 0, i32 0
  %962 = load %struct.UT_hash_table*, %struct.UT_hash_table** %961, align 8
  %963 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %962, i32 0, i32 0
  %964 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %963, align 8
  %965 = icmp ne %struct.UT_hash_bucket* %964, null
  br i1 %965, label %967, label %966

; <label>:966                                     ; preds = %911
  call void @exit(i32 -1) #9
  unreachable

; <label>:967                                     ; preds = %911
  %968 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %969 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %968, i32 0, i32 2
  %970 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %969, i32 0, i32 0
  %971 = load %struct.UT_hash_table*, %struct.UT_hash_table** %970, align 8
  %972 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %971, i32 0, i32 0
  %973 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %972, align 8
  %974 = bitcast %struct.UT_hash_bucket* %973 to i8*
  call void @llvm.memset.p0i8.i64(i8* %974, i8 0, i64 512, i32 8, i1 false)
  br label %975

; <label>:975                                     ; preds = %967
  br label %976

; <label>:976                                     ; preds = %975
  br label %977

; <label>:977                                     ; preds = %976
  %978 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  store %struct.hash_tbl* %978, %struct.hash_tbl** %tbls, align 8
  br label %1026

; <label>:979                                     ; preds = %876
  %980 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %981 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %980, i32 0, i32 2
  %982 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %981, i32 0, i32 0
  %983 = load %struct.UT_hash_table*, %struct.UT_hash_table** %982, align 8
  %984 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %985 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %984, i32 0, i32 2
  %986 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %985, i32 0, i32 0
  store %struct.UT_hash_table* %983, %struct.UT_hash_table** %986, align 8
  br label %987

; <label>:987                                     ; preds = %979
  %988 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %989 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %988, i32 0, i32 2
  %990 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %989, i32 0, i32 2
  store i8* null, i8** %990, align 8
  %991 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %992 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %991, i32 0, i32 2
  %993 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %992, i32 0, i32 0
  %994 = load %struct.UT_hash_table*, %struct.UT_hash_table** %993, align 8
  %995 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %994, i32 0, i32 4
  %996 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %995, align 8
  %997 = bitcast %struct.UT_hash_handle* %996 to i8*
  %998 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %999 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %998, i32 0, i32 2
  %1000 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %999, i32 0, i32 0
  %1001 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1000, align 8
  %1002 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1001, i32 0, i32 5
  %1003 = load i64, i64* %1002, align 8
  %1004 = sub i64 0, %1003
  %1005 = getelementptr inbounds i8, i8* %997, i64 %1004
  %1006 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1007 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1006, i32 0, i32 2
  %1008 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1007, i32 0, i32 1
  store i8* %1005, i8** %1008, align 8
  %1009 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1010 = bitcast %struct.hash_tbl* %1009 to i8*
  %1011 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %1012 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1011, i32 0, i32 2
  %1013 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1012, i32 0, i32 0
  %1014 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1013, align 8
  %1015 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1014, i32 0, i32 4
  %1016 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1015, align 8
  %1017 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1016, i32 0, i32 2
  store i8* %1010, i8** %1017, align 8
  %1018 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1019 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1018, i32 0, i32 2
  %1020 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %1021 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1020, i32 0, i32 2
  %1022 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1021, i32 0, i32 0
  %1023 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1022, align 8
  %1024 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1023, i32 0, i32 4
  store %struct.UT_hash_handle* %1019, %struct.UT_hash_handle** %1024, align 8
  br label %1025

; <label>:1025                                    ; preds = %987
  br label %1026

; <label>:1026                                    ; preds = %1025, %977
  br label %1027

; <label>:1027                                    ; preds = %1026
  %1028 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %1029 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1028, i32 0, i32 2
  %1030 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1029, i32 0, i32 0
  %1031 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1030, align 8
  %1032 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1031, i32 0, i32 3
  %1033 = load i32, i32* %1032, align 8
  %1034 = add i32 %1033, 1
  store i32 %1034, i32* %1032, align 8
  br label %1035

; <label>:1035                                    ; preds = %1027
  %1036 = load i32, i32* %_ha_hashv, align 4
  %1037 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %1038 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1037, i32 0, i32 2
  %1039 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1038, i32 0, i32 0
  %1040 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1039, align 8
  %1041 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1040, i32 0, i32 1
  %1042 = load i32, i32* %1041, align 8
  %1043 = sub i32 %1042, 1
  %1044 = and i32 %1036, %1043
  store i32 %1044, i32* %_ha_bkt, align 4
  br label %1045

; <label>:1045                                    ; preds = %1035
  br label %1046

; <label>:1046                                    ; preds = %1045
  %1047 = load i32, i32* %_ha_bkt, align 4
  %1048 = zext i32 %1047 to i64
  %1049 = load %struct.hash_tbl*, %struct.hash_tbl** %tbls, align 8
  %1050 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1049, i32 0, i32 2
  %1051 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1050, i32 0, i32 0
  %1052 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1051, align 8
  %1053 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1052, i32 0, i32 0
  %1054 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %1053, align 8
  %1055 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1054, i64 %1048
  store %struct.UT_hash_bucket* %1055, %struct.UT_hash_bucket** %_ha_head, align 8
  %1056 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1057 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1056, i32 0, i32 1
  %1058 = load i32, i32* %1057, align 8
  %1059 = add i32 %1058, 1
  store i32 %1059, i32* %1057, align 8
  %1060 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1061 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1060, i32 0, i32 0
  %1062 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1061, align 8
  %1063 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1064 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1063, i32 0, i32 2
  %1065 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1064, i32 0, i32 4
  store %struct.UT_hash_handle* %1062, %struct.UT_hash_handle** %1065, align 8
  %1066 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1067 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1066, i32 0, i32 2
  %1068 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1067, i32 0, i32 3
  store %struct.UT_hash_handle* null, %struct.UT_hash_handle** %1068, align 8
  %1069 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1070 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1069, i32 0, i32 0
  %1071 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1070, align 8
  %1072 = icmp ne %struct.UT_hash_handle* %1071, null
  br i1 %1072, label %1073, label %1080

; <label>:1073                                    ; preds = %1046
  %1074 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1075 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1074, i32 0, i32 2
  %1076 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1077 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1076, i32 0, i32 0
  %1078 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1077, align 8
  %1079 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1078, i32 0, i32 3
  store %struct.UT_hash_handle* %1075, %struct.UT_hash_handle** %1079, align 8
  br label %1080

; <label>:1080                                    ; preds = %1073, %1046
  %1081 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1082 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1081, i32 0, i32 2
  %1083 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1084 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1083, i32 0, i32 0
  store %struct.UT_hash_handle* %1082, %struct.UT_hash_handle** %1084, align 8
  %1085 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1086 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1085, i32 0, i32 1
  %1087 = load i32, i32* %1086, align 8
  %1088 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %1089 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1088, i32 0, i32 2
  %1090 = load i32, i32* %1089, align 4
  %1091 = add i32 %1090, 1
  %1092 = mul i32 %1091, 10
  %1093 = icmp uge i32 %1087, %1092
  br i1 %1093, label %1094, label %1350

; <label>:1094                                    ; preds = %1080
  %1095 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1096 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1095, i32 0, i32 2
  %1097 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1096, i32 0, i32 0
  %1098 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1097, align 8
  %1099 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1098, i32 0, i32 9
  %1100 = load i32, i32* %1099, align 4
  %1101 = icmp ne i32 %1100, 0
  br i1 %1101, label %1350, label %1102

; <label>:1102                                    ; preds = %1094
  br label %1103

; <label>:1103                                    ; preds = %1102
  %1104 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1105 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1104, i32 0, i32 2
  %1106 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1105, i32 0, i32 0
  %1107 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1106, align 8
  %1108 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1107, i32 0, i32 1
  %1109 = load i32, i32* %1108, align 8
  %1110 = zext i32 %1109 to i64
  %1111 = mul i64 2, %1110
  %1112 = mul i64 %1111, 16
  %1113 = call noalias i8* @malloc(i64 %1112) #2
  %1114 = bitcast i8* %1113 to %struct.UT_hash_bucket*
  store %struct.UT_hash_bucket* %1114, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %1115 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %1116 = icmp ne %struct.UT_hash_bucket* %1115, null
  br i1 %1116, label %1118, label %1117

; <label>:1117                                    ; preds = %1103
  call void @exit(i32 -1) #9
  unreachable

; <label>:1118                                    ; preds = %1103
  %1119 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %1120 = bitcast %struct.UT_hash_bucket* %1119 to i8*
  %1121 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1122 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1121, i32 0, i32 2
  %1123 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1122, i32 0, i32 0
  %1124 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1123, align 8
  %1125 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1124, i32 0, i32 1
  %1126 = load i32, i32* %1125, align 8
  %1127 = zext i32 %1126 to i64
  %1128 = mul i64 2, %1127
  %1129 = mul i64 %1128, 16
  call void @llvm.memset.p0i8.i64(i8* %1120, i8 0, i64 %1129, i32 8, i1 false)
  %1130 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1131 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1130, i32 0, i32 2
  %1132 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1131, i32 0, i32 0
  %1133 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1132, align 8
  %1134 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1133, i32 0, i32 3
  %1135 = load i32, i32* %1134, align 8
  %1136 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1137 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1136, i32 0, i32 2
  %1138 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1137, i32 0, i32 0
  %1139 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1138, align 8
  %1140 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1139, i32 0, i32 2
  %1141 = load i32, i32* %1140, align 4
  %1142 = add i32 %1141, 1
  %1143 = lshr i32 %1135, %1142
  %1144 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1145 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1144, i32 0, i32 2
  %1146 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1145, i32 0, i32 0
  %1147 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1146, align 8
  %1148 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1147, i32 0, i32 3
  %1149 = load i32, i32* %1148, align 8
  %1150 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1151 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1150, i32 0, i32 2
  %1152 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1151, i32 0, i32 0
  %1153 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1152, align 8
  %1154 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1153, i32 0, i32 1
  %1155 = load i32, i32* %1154, align 8
  %1156 = mul i32 %1155, 2
  %1157 = sub i32 %1156, 1
  %1158 = and i32 %1149, %1157
  %1159 = icmp ne i32 %1158, 0
  %1160 = select i1 %1159, i32 1, i32 0
  %1161 = add i32 %1143, %1160
  %1162 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1163 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1162, i32 0, i32 2
  %1164 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1163, i32 0, i32 0
  %1165 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1164, align 8
  %1166 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1165, i32 0, i32 6
  store i32 %1161, i32* %1166, align 8
  %1167 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1168 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1167, i32 0, i32 2
  %1169 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1168, i32 0, i32 0
  %1170 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1169, align 8
  %1171 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1170, i32 0, i32 7
  store i32 0, i32* %1171, align 4
  store i32 0, i32* %_he_bkt_i, align 4
  br label %1172

; <label>:1172                                    ; preds = %1273, %1118
  %1173 = load i32, i32* %_he_bkt_i, align 4
  %1174 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1175 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1174, i32 0, i32 2
  %1176 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1175, i32 0, i32 0
  %1177 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1176, align 8
  %1178 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1177, i32 0, i32 1
  %1179 = load i32, i32* %1178, align 8
  %1180 = icmp ult i32 %1173, %1179
  br i1 %1180, label %1181, label %1276

; <label>:1181                                    ; preds = %1172
  %1182 = load i32, i32* %_he_bkt_i, align 4
  %1183 = zext i32 %1182 to i64
  %1184 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1185 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1184, i32 0, i32 2
  %1186 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1185, i32 0, i32 0
  %1187 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1186, align 8
  %1188 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1187, i32 0, i32 0
  %1189 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %1188, align 8
  %1190 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1189, i64 %1183
  %1191 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1190, i32 0, i32 0
  %1192 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1191, align 8
  store %struct.UT_hash_handle* %1192, %struct.UT_hash_handle** %_he_thh, align 8
  br label %1193

; <label>:1193                                    ; preds = %1267, %1181
  %1194 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1195 = icmp ne %struct.UT_hash_handle* %1194, null
  br i1 %1195, label %1196, label %1272

; <label>:1196                                    ; preds = %1193
  %1197 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1198 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1197, i32 0, i32 4
  %1199 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1198, align 8
  store %struct.UT_hash_handle* %1199, %struct.UT_hash_handle** %_he_hh_nxt, align 8
  br label %1200

; <label>:1200                                    ; preds = %1196
  %1201 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1202 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1201, i32 0, i32 7
  %1203 = load i32, i32* %1202, align 4
  %1204 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1205 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1204, i32 0, i32 2
  %1206 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1205, i32 0, i32 0
  %1207 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1206, align 8
  %1208 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1207, i32 0, i32 1
  %1209 = load i32, i32* %1208, align 8
  %1210 = mul i32 %1209, 2
  %1211 = sub i32 %1210, 1
  %1212 = and i32 %1203, %1211
  store i32 %1212, i32* %_he_bkt, align 4
  br label %1213

; <label>:1213                                    ; preds = %1200
  %1214 = load i32, i32* %_he_bkt, align 4
  %1215 = zext i32 %1214 to i64
  %1216 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %1217 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1216, i64 %1215
  store %struct.UT_hash_bucket* %1217, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1218 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1219 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1218, i32 0, i32 1
  %1220 = load i32, i32* %1219, align 8
  %1221 = add i32 %1220, 1
  store i32 %1221, i32* %1219, align 8
  %1222 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1223 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1222, i32 0, i32 2
  %1224 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1223, i32 0, i32 0
  %1225 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1224, align 8
  %1226 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1225, i32 0, i32 6
  %1227 = load i32, i32* %1226, align 8
  %1228 = icmp ugt i32 %1221, %1227
  br i1 %1228, label %1229, label %1249

; <label>:1229                                    ; preds = %1213
  %1230 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1231 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1230, i32 0, i32 2
  %1232 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1231, i32 0, i32 0
  %1233 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1232, align 8
  %1234 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1233, i32 0, i32 7
  %1235 = load i32, i32* %1234, align 4
  %1236 = add i32 %1235, 1
  store i32 %1236, i32* %1234, align 4
  %1237 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1238 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1237, i32 0, i32 1
  %1239 = load i32, i32* %1238, align 8
  %1240 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1241 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1240, i32 0, i32 2
  %1242 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1241, i32 0, i32 0
  %1243 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1242, align 8
  %1244 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1243, i32 0, i32 6
  %1245 = load i32, i32* %1244, align 8
  %1246 = udiv i32 %1239, %1245
  %1247 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1248 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1247, i32 0, i32 2
  store i32 %1246, i32* %1248, align 4
  br label %1249

; <label>:1249                                    ; preds = %1229, %1213
  %1250 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1251 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1250, i32 0, i32 3
  store %struct.UT_hash_handle* null, %struct.UT_hash_handle** %1251, align 8
  %1252 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1253 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1252, i32 0, i32 0
  %1254 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1253, align 8
  %1255 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1256 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1255, i32 0, i32 4
  store %struct.UT_hash_handle* %1254, %struct.UT_hash_handle** %1256, align 8
  %1257 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1258 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1257, i32 0, i32 0
  %1259 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1258, align 8
  %1260 = icmp ne %struct.UT_hash_handle* %1259, null
  br i1 %1260, label %1261, label %1267

; <label>:1261                                    ; preds = %1249
  %1262 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1263 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1264 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1263, i32 0, i32 0
  %1265 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %1264, align 8
  %1266 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1265, i32 0, i32 3
  store %struct.UT_hash_handle* %1262, %struct.UT_hash_handle** %1266, align 8
  br label %1267

; <label>:1267                                    ; preds = %1261, %1249
  %1268 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %1269 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %1270 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %1269, i32 0, i32 0
  store %struct.UT_hash_handle* %1268, %struct.UT_hash_handle** %1270, align 8
  %1271 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_hh_nxt, align 8
  store %struct.UT_hash_handle* %1271, %struct.UT_hash_handle** %_he_thh, align 8
  br label %1193

; <label>:1272                                    ; preds = %1193
  br label %1273

; <label>:1273                                    ; preds = %1272
  %1274 = load i32, i32* %_he_bkt_i, align 4
  %1275 = add i32 %1274, 1
  store i32 %1275, i32* %_he_bkt_i, align 4
  br label %1172

; <label>:1276                                    ; preds = %1172
  %1277 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1278 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1277, i32 0, i32 2
  %1279 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1278, i32 0, i32 0
  %1280 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1279, align 8
  %1281 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1280, i32 0, i32 0
  %1282 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %1281, align 8
  %1283 = bitcast %struct.UT_hash_bucket* %1282 to i8*
  call void @free(i8* %1283) #2
  %1284 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1285 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1284, i32 0, i32 2
  %1286 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1285, i32 0, i32 0
  %1287 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1286, align 8
  %1288 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1287, i32 0, i32 1
  %1289 = load i32, i32* %1288, align 8
  %1290 = mul i32 %1289, 2
  store i32 %1290, i32* %1288, align 8
  %1291 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1292 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1291, i32 0, i32 2
  %1293 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1292, i32 0, i32 0
  %1294 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1293, align 8
  %1295 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1294, i32 0, i32 2
  %1296 = load i32, i32* %1295, align 4
  %1297 = add i32 %1296, 1
  store i32 %1297, i32* %1295, align 4
  %1298 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %1299 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1300 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1299, i32 0, i32 2
  %1301 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1300, i32 0, i32 0
  %1302 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1301, align 8
  %1303 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1302, i32 0, i32 0
  store %struct.UT_hash_bucket* %1298, %struct.UT_hash_bucket** %1303, align 8
  %1304 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1305 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1304, i32 0, i32 2
  %1306 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1305, i32 0, i32 0
  %1307 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1306, align 8
  %1308 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1307, i32 0, i32 7
  %1309 = load i32, i32* %1308, align 4
  %1310 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1311 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1310, i32 0, i32 2
  %1312 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1311, i32 0, i32 0
  %1313 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1312, align 8
  %1314 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1313, i32 0, i32 3
  %1315 = load i32, i32* %1314, align 8
  %1316 = lshr i32 %1315, 1
  %1317 = icmp ugt i32 %1309, %1316
  br i1 %1317, label %1318, label %1326

; <label>:1318                                    ; preds = %1276
  %1319 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1320 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1319, i32 0, i32 2
  %1321 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1320, i32 0, i32 0
  %1322 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1321, align 8
  %1323 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1322, i32 0, i32 8
  %1324 = load i32, i32* %1323, align 8
  %1325 = add i32 %1324, 1
  br label %1327

; <label>:1326                                    ; preds = %1276
  br label %1327

; <label>:1327                                    ; preds = %1326, %1318
  %1328 = phi i32 [ %1325, %1318 ], [ 0, %1326 ]
  %1329 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1330 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1329, i32 0, i32 2
  %1331 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1330, i32 0, i32 0
  %1332 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1331, align 8
  %1333 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1332, i32 0, i32 8
  store i32 %1328, i32* %1333, align 8
  %1334 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1335 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1334, i32 0, i32 2
  %1336 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1335, i32 0, i32 0
  %1337 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1336, align 8
  %1338 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1337, i32 0, i32 8
  %1339 = load i32, i32* %1338, align 8
  %1340 = icmp ugt i32 %1339, 1
  br i1 %1340, label %1341, label %1347

; <label>:1341                                    ; preds = %1327
  %1342 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %1343 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %1342, i32 0, i32 2
  %1344 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %1343, i32 0, i32 0
  %1345 = load %struct.UT_hash_table*, %struct.UT_hash_table** %1344, align 8
  %1346 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %1345, i32 0, i32 9
  store i32 1, i32* %1346, align 4
  br label %1347

; <label>:1347                                    ; preds = %1341, %1327
  br label %1348

; <label>:1348                                    ; preds = %1347
  br label %1349

; <label>:1349                                    ; preds = %1348
  br label %1350

; <label>:1350                                    ; preds = %1349, %1094, %1080
  br label %1351

; <label>:1351                                    ; preds = %1350
  br label %1352

; <label>:1352                                    ; preds = %1351
  br label %1353

; <label>:1353                                    ; preds = %1352
  br label %1354

; <label>:1354                                    ; preds = %1353
  br label %1355

; <label>:1355                                    ; preds = %1354, %500
  ret i64 39
}

; Function Attrs: uwtable
define i64 @applyprim_hash_45set_33(i64 %lst) #0 {
  %1 = alloca i64, align 8
  %rest = alloca i64, align 8
  %v0 = alloca i64, align 8
  %v1 = alloca i64, align 8
  %v2 = alloca i64, align 8
  store i64 %lst, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i64 @expect_cons(i64 %2, i64* %rest)
  store i64 %3, i64* %v0, align 8
  %4 = load i64, i64* %rest, align 8
  %5 = call i64 @expect_cons(i64 %4, i64* %rest)
  store i64 %5, i64* %v1, align 8
  %6 = load i64, i64* %rest, align 8
  %7 = call i64 @expect_cons(i64 %6, i64* %rest)
  store i64 %7, i64* %v2, align 8
  %8 = load i64, i64* %rest, align 8
  %9 = icmp ne i64 %8, 0
  br i1 %9, label %10, label %11

; <label>:10                                      ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.30, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %0
  %12 = load i64, i64* %v0, align 8
  %13 = load i64, i64* %v1, align 8
  %14 = load i64, i64* %v2, align 8
  %15 = call i64 @prim_hash_45set_33(i64 %12, i64 %13, i64 %14)
  ret i64 %15
}

; Function Attrs: uwtable
define internal void @_GLOBAL__sub_I_header.cpp() #0 section ".text.startup" {
  call void @__cxx_global_var_init()
  ret void
}

attributes #0 = { uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }
attributes #3 = { noreturn nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nobuiltin nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { nounwind readonly "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #8 = { argmemonly nounwind }
attributes #9 = { noreturn nounwind }
attributes #10 = { builtin nounwind }
attributes #11 = { nounwind readonly }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0-2ubuntu4 (tags/RELEASE_380/final)"}


;;;;;;

define void @proc_main() {
  %cloptr10891 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10892 = getelementptr inbounds i64, i64* %cloptr10891, i64 0                  ; &cloptr10891[0]
  %f10893 = ptrtoint void(i64,i64,i64)* @lam10889 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10893, i64* %eptr10892                                                 ; store fptr
  %arg9515 = ptrtoint i64* %cloptr10891 to i64                                       ; closure cast; i64* -> i64
  %cloptr10894 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10895 = getelementptr inbounds i64, i64* %cloptr10894, i64 0                  ; &cloptr10894[0]
  %f10896 = ptrtoint void(i64,i64,i64)* @lam10887 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10896, i64* %eptr10895                                                 ; store fptr
  %arg9514 = ptrtoint i64* %cloptr10894 to i64                                       ; closure cast; i64* -> i64
  %cloptr10897 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10898 = getelementptr inbounds i64, i64* %cloptr10897, i64 0                  ; &cloptr10897[0]
  %f10899 = ptrtoint void(i64,i64,i64)* @lam10415 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10899, i64* %eptr10898                                                 ; store fptr
  %arg9513 = ptrtoint i64* %cloptr10897 to i64                                       ; closure cast; i64* -> i64
  %cloptr10900 = inttoptr i64 %arg9515 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10901 = getelementptr inbounds i64, i64* %cloptr10900, i64 0                 ; &cloptr10900[0]
  %f10903 = load i64, i64* %i0ptr10901, align 8                                      ; load; *i0ptr10901
  %fptr10902 = inttoptr i64 %f10903 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10902(i64 %arg9515, i64 %arg9514, i64 %arg9513)     ; tail call
  ret void
}


define i32 @main() {
  call fastcc void @proc_main()
  ret i32 0
}



define void @lam10889(i64 %env10890, i64 %cont9505, i64 %cjT$yu) {
  %cloptr10904 = inttoptr i64 %cjT$yu to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr10905 = getelementptr inbounds i64, i64* %cloptr10904, i64 0                 ; &cloptr10904[0]
  %f10907 = load i64, i64* %i0ptr10905, align 8                                      ; load; *i0ptr10905
  %fptr10906 = inttoptr i64 %f10907 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10906(i64 %cjT$yu, i64 %cont9505, i64 %cjT$yu)      ; tail call
  ret void
}


define void @lam10887(i64 %env10888, i64 %_959315, i64 %hHt$Ycmb) {
  %cloptr10908 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr10910 = getelementptr inbounds i64, i64* %cloptr10908, i64 1                  ; &eptr10910[1]
  store i64 %hHt$Ycmb, i64* %eptr10910                                               ; *eptr10910 = %hHt$Ycmb
  %eptr10909 = getelementptr inbounds i64, i64* %cloptr10908, i64 0                  ; &cloptr10908[0]
  %f10911 = ptrtoint void(i64,i64,i64)* @lam10885 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10911, i64* %eptr10909                                                 ; store fptr
  %arg9520 = ptrtoint i64* %cloptr10908 to i64                                       ; closure cast; i64* -> i64
  %cloptr10912 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10913 = getelementptr inbounds i64, i64* %cloptr10912, i64 0                  ; &cloptr10912[0]
  %f10914 = ptrtoint void(i64,i64,i64)* @lam10423 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10914, i64* %eptr10913                                                 ; store fptr
  %arg9519 = ptrtoint i64* %cloptr10912 to i64                                       ; closure cast; i64* -> i64
  %cloptr10915 = inttoptr i64 %hHt$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10916 = getelementptr inbounds i64, i64* %cloptr10915, i64 0                 ; &cloptr10915[0]
  %f10918 = load i64, i64* %i0ptr10916, align 8                                      ; load; *i0ptr10916
  %fptr10917 = inttoptr i64 %f10918 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10917(i64 %hHt$Ycmb, i64 %arg9520, i64 %arg9519)    ; tail call
  ret void
}


define void @lam10885(i64 %env10886, i64 %_959316, i64 %RQ7$_37foldr1) {
  %envptr10919 = inttoptr i64 %env10886 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10920 = getelementptr inbounds i64, i64* %envptr10919, i64 1                ; &envptr10919[1]
  %hHt$Ycmb = load i64, i64* %envptr10920, align 8                                   ; load; *envptr10920
  %cloptr10921 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr10923 = getelementptr inbounds i64, i64* %cloptr10921, i64 1                  ; &eptr10923[1]
  %eptr10924 = getelementptr inbounds i64, i64* %cloptr10921, i64 2                  ; &eptr10924[2]
  store i64 %RQ7$_37foldr1, i64* %eptr10923                                          ; *eptr10923 = %RQ7$_37foldr1
  store i64 %hHt$Ycmb, i64* %eptr10924                                               ; *eptr10924 = %hHt$Ycmb
  %eptr10922 = getelementptr inbounds i64, i64* %cloptr10921, i64 0                  ; &cloptr10921[0]
  %f10925 = ptrtoint void(i64,i64,i64)* @lam10883 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10925, i64* %eptr10922                                                 ; store fptr
  %arg9523 = ptrtoint i64* %cloptr10921 to i64                                       ; closure cast; i64* -> i64
  %cloptr10926 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10927 = getelementptr inbounds i64, i64* %cloptr10926, i64 0                  ; &cloptr10926[0]
  %f10928 = ptrtoint void(i64,i64,i64)* @lam10435 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10928, i64* %eptr10927                                                 ; store fptr
  %arg9522 = ptrtoint i64* %cloptr10926 to i64                                       ; closure cast; i64* -> i64
  %cloptr10929 = inttoptr i64 %hHt$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10930 = getelementptr inbounds i64, i64* %cloptr10929, i64 0                 ; &cloptr10929[0]
  %f10932 = load i64, i64* %i0ptr10930, align 8                                      ; load; *i0ptr10930
  %fptr10931 = inttoptr i64 %f10932 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10931(i64 %hHt$Ycmb, i64 %arg9523, i64 %arg9522)    ; tail call
  ret void
}


define void @lam10883(i64 %env10884, i64 %_959317, i64 %nad$_37map1) {
  %envptr10933 = inttoptr i64 %env10884 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10934 = getelementptr inbounds i64, i64* %envptr10933, i64 2                ; &envptr10933[2]
  %hHt$Ycmb = load i64, i64* %envptr10934, align 8                                   ; load; *envptr10934
  %envptr10935 = inttoptr i64 %env10884 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10936 = getelementptr inbounds i64, i64* %envptr10935, i64 1                ; &envptr10935[1]
  %RQ7$_37foldr1 = load i64, i64* %envptr10936, align 8                              ; load; *envptr10936
  %cloptr10937 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10939 = getelementptr inbounds i64, i64* %cloptr10937, i64 1                  ; &eptr10939[1]
  %eptr10940 = getelementptr inbounds i64, i64* %cloptr10937, i64 2                  ; &eptr10940[2]
  %eptr10941 = getelementptr inbounds i64, i64* %cloptr10937, i64 3                  ; &eptr10941[3]
  store i64 %RQ7$_37foldr1, i64* %eptr10939                                          ; *eptr10939 = %RQ7$_37foldr1
  store i64 %nad$_37map1, i64* %eptr10940                                            ; *eptr10940 = %nad$_37map1
  store i64 %hHt$Ycmb, i64* %eptr10941                                               ; *eptr10941 = %hHt$Ycmb
  %eptr10938 = getelementptr inbounds i64, i64* %cloptr10937, i64 0                  ; &cloptr10937[0]
  %f10942 = ptrtoint void(i64,i64,i64)* @lam10881 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10942, i64* %eptr10938                                                 ; store fptr
  %arg9526 = ptrtoint i64* %cloptr10937 to i64                                       ; closure cast; i64* -> i64
  %cloptr10943 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10944 = getelementptr inbounds i64, i64* %cloptr10943, i64 0                  ; &cloptr10943[0]
  %f10945 = ptrtoint void(i64,i64,i64)* @lam10449 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10945, i64* %eptr10944                                                 ; store fptr
  %arg9525 = ptrtoint i64* %cloptr10943 to i64                                       ; closure cast; i64* -> i64
  %cloptr10946 = inttoptr i64 %hHt$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10947 = getelementptr inbounds i64, i64* %cloptr10946, i64 0                 ; &cloptr10946[0]
  %f10949 = load i64, i64* %i0ptr10947, align 8                                      ; load; *i0ptr10947
  %fptr10948 = inttoptr i64 %f10949 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10948(i64 %hHt$Ycmb, i64 %arg9526, i64 %arg9525)    ; tail call
  ret void
}


define void @lam10881(i64 %env10882, i64 %_959318, i64 %ig8$_37take) {
  %envptr10950 = inttoptr i64 %env10882 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10951 = getelementptr inbounds i64, i64* %envptr10950, i64 3                ; &envptr10950[3]
  %hHt$Ycmb = load i64, i64* %envptr10951, align 8                                   ; load; *envptr10951
  %envptr10952 = inttoptr i64 %env10882 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10953 = getelementptr inbounds i64, i64* %envptr10952, i64 2                ; &envptr10952[2]
  %nad$_37map1 = load i64, i64* %envptr10953, align 8                                ; load; *envptr10953
  %envptr10954 = inttoptr i64 %env10882 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10955 = getelementptr inbounds i64, i64* %envptr10954, i64 1                ; &envptr10954[1]
  %RQ7$_37foldr1 = load i64, i64* %envptr10955, align 8                              ; load; *envptr10955
  %cloptr10956 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr10958 = getelementptr inbounds i64, i64* %cloptr10956, i64 1                  ; &eptr10958[1]
  %eptr10959 = getelementptr inbounds i64, i64* %cloptr10956, i64 2                  ; &eptr10959[2]
  %eptr10960 = getelementptr inbounds i64, i64* %cloptr10956, i64 3                  ; &eptr10960[3]
  %eptr10961 = getelementptr inbounds i64, i64* %cloptr10956, i64 4                  ; &eptr10961[4]
  store i64 %RQ7$_37foldr1, i64* %eptr10958                                          ; *eptr10958 = %RQ7$_37foldr1
  store i64 %nad$_37map1, i64* %eptr10959                                            ; *eptr10959 = %nad$_37map1
  store i64 %ig8$_37take, i64* %eptr10960                                            ; *eptr10960 = %ig8$_37take
  store i64 %hHt$Ycmb, i64* %eptr10961                                               ; *eptr10961 = %hHt$Ycmb
  %eptr10957 = getelementptr inbounds i64, i64* %cloptr10956, i64 0                  ; &cloptr10956[0]
  %f10962 = ptrtoint void(i64,i64,i64)* @lam10879 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10962, i64* %eptr10957                                                 ; store fptr
  %arg9529 = ptrtoint i64* %cloptr10956 to i64                                       ; closure cast; i64* -> i64
  %cloptr10963 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10964 = getelementptr inbounds i64, i64* %cloptr10963, i64 0                  ; &cloptr10963[0]
  %f10965 = ptrtoint void(i64,i64,i64)* @lam10460 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10965, i64* %eptr10964                                                 ; store fptr
  %arg9528 = ptrtoint i64* %cloptr10963 to i64                                       ; closure cast; i64* -> i64
  %cloptr10966 = inttoptr i64 %hHt$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10967 = getelementptr inbounds i64, i64* %cloptr10966, i64 0                 ; &cloptr10966[0]
  %f10969 = load i64, i64* %i0ptr10967, align 8                                      ; load; *i0ptr10967
  %fptr10968 = inttoptr i64 %f10969 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10968(i64 %hHt$Ycmb, i64 %arg9529, i64 %arg9528)    ; tail call
  ret void
}


define void @lam10879(i64 %env10880, i64 %_959319, i64 %Bv6$_37length) {
  %envptr10970 = inttoptr i64 %env10880 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10971 = getelementptr inbounds i64, i64* %envptr10970, i64 4                ; &envptr10970[4]
  %hHt$Ycmb = load i64, i64* %envptr10971, align 8                                   ; load; *envptr10971
  %envptr10972 = inttoptr i64 %env10880 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10973 = getelementptr inbounds i64, i64* %envptr10972, i64 3                ; &envptr10972[3]
  %ig8$_37take = load i64, i64* %envptr10973, align 8                                ; load; *envptr10973
  %envptr10974 = inttoptr i64 %env10880 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10975 = getelementptr inbounds i64, i64* %envptr10974, i64 2                ; &envptr10974[2]
  %nad$_37map1 = load i64, i64* %envptr10975, align 8                                ; load; *envptr10975
  %envptr10976 = inttoptr i64 %env10880 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10977 = getelementptr inbounds i64, i64* %envptr10976, i64 1                ; &envptr10976[1]
  %RQ7$_37foldr1 = load i64, i64* %envptr10977, align 8                              ; load; *envptr10977
  %cloptr10978 = call i64* @alloc(i64 48)                                            ; malloc
  %eptr10980 = getelementptr inbounds i64, i64* %cloptr10978, i64 1                  ; &eptr10980[1]
  %eptr10981 = getelementptr inbounds i64, i64* %cloptr10978, i64 2                  ; &eptr10981[2]
  %eptr10982 = getelementptr inbounds i64, i64* %cloptr10978, i64 3                  ; &eptr10982[3]
  %eptr10983 = getelementptr inbounds i64, i64* %cloptr10978, i64 4                  ; &eptr10983[4]
  %eptr10984 = getelementptr inbounds i64, i64* %cloptr10978, i64 5                  ; &eptr10984[5]
  store i64 %RQ7$_37foldr1, i64* %eptr10980                                          ; *eptr10980 = %RQ7$_37foldr1
  store i64 %nad$_37map1, i64* %eptr10981                                            ; *eptr10981 = %nad$_37map1
  store i64 %ig8$_37take, i64* %eptr10982                                            ; *eptr10982 = %ig8$_37take
  store i64 %Bv6$_37length, i64* %eptr10983                                          ; *eptr10983 = %Bv6$_37length
  store i64 %hHt$Ycmb, i64* %eptr10984                                               ; *eptr10984 = %hHt$Ycmb
  %eptr10979 = getelementptr inbounds i64, i64* %cloptr10978, i64 0                  ; &cloptr10978[0]
  %f10985 = ptrtoint void(i64,i64,i64)* @lam10877 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10985, i64* %eptr10979                                                 ; store fptr
  %arg9532 = ptrtoint i64* %cloptr10978 to i64                                       ; closure cast; i64* -> i64
  %cloptr10986 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10987 = getelementptr inbounds i64, i64* %cloptr10986, i64 0                  ; &cloptr10986[0]
  %f10988 = ptrtoint void(i64,i64,i64)* @lam10468 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f10988, i64* %eptr10987                                                 ; store fptr
  %arg9531 = ptrtoint i64* %cloptr10986 to i64                                       ; closure cast; i64* -> i64
  %cloptr10989 = inttoptr i64 %hHt$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10990 = getelementptr inbounds i64, i64* %cloptr10989, i64 0                 ; &cloptr10989[0]
  %f10992 = load i64, i64* %i0ptr10990, align 8                                      ; load; *i0ptr10990
  %fptr10991 = inttoptr i64 %f10992 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10991(i64 %hHt$Ycmb, i64 %arg9532, i64 %arg9531)    ; tail call
  ret void
}


define void @lam10877(i64 %env10878, i64 %_959320, i64 %uc0$_37foldl1) {
  %envptr10993 = inttoptr i64 %env10878 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10994 = getelementptr inbounds i64, i64* %envptr10993, i64 5                ; &envptr10993[5]
  %hHt$Ycmb = load i64, i64* %envptr10994, align 8                                   ; load; *envptr10994
  %envptr10995 = inttoptr i64 %env10878 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10996 = getelementptr inbounds i64, i64* %envptr10995, i64 4                ; &envptr10995[4]
  %Bv6$_37length = load i64, i64* %envptr10996, align 8                              ; load; *envptr10996
  %envptr10997 = inttoptr i64 %env10878 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr10998 = getelementptr inbounds i64, i64* %envptr10997, i64 3                ; &envptr10997[3]
  %ig8$_37take = load i64, i64* %envptr10998, align 8                                ; load; *envptr10998
  %envptr10999 = inttoptr i64 %env10878 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11000 = getelementptr inbounds i64, i64* %envptr10999, i64 2                ; &envptr10999[2]
  %nad$_37map1 = load i64, i64* %envptr11000, align 8                                ; load; *envptr11000
  %envptr11001 = inttoptr i64 %env10878 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11002 = getelementptr inbounds i64, i64* %envptr11001, i64 1                ; &envptr11001[1]
  %RQ7$_37foldr1 = load i64, i64* %envptr11002, align 8                              ; load; *envptr11002
  %cloptr11003 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11005 = getelementptr inbounds i64, i64* %cloptr11003, i64 1                  ; &eptr11005[1]
  store i64 %uc0$_37foldl1, i64* %eptr11005                                          ; *eptr11005 = %uc0$_37foldl1
  %eptr11004 = getelementptr inbounds i64, i64* %cloptr11003, i64 0                  ; &cloptr11003[0]
  %f11006 = ptrtoint void(i64,i64,i64)* @lam10875 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11006, i64* %eptr11004                                                 ; store fptr
  %PdG$_37last = ptrtoint i64* %cloptr11003 to i64                                   ; closure cast; i64* -> i64
  %cloptr11007 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11009 = getelementptr inbounds i64, i64* %cloptr11007, i64 1                  ; &eptr11009[1]
  %eptr11010 = getelementptr inbounds i64, i64* %cloptr11007, i64 2                  ; &eptr11010[2]
  store i64 %ig8$_37take, i64* %eptr11009                                            ; *eptr11009 = %ig8$_37take
  store i64 %Bv6$_37length, i64* %eptr11010                                          ; *eptr11010 = %Bv6$_37length
  %eptr11008 = getelementptr inbounds i64, i64* %cloptr11007, i64 0                  ; &cloptr11007[0]
  %f11011 = ptrtoint void(i64,i64,i64,i64)* @lam10869 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11011, i64* %eptr11008                                                 ; store fptr
  %fWC$_37drop_45right = ptrtoint i64* %cloptr11007 to i64                           ; closure cast; i64* -> i64
  %cloptr11012 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr11014 = getelementptr inbounds i64, i64* %cloptr11012, i64 1                  ; &eptr11014[1]
  %eptr11015 = getelementptr inbounds i64, i64* %cloptr11012, i64 2                  ; &eptr11015[2]
  %eptr11016 = getelementptr inbounds i64, i64* %cloptr11012, i64 3                  ; &eptr11016[3]
  %eptr11017 = getelementptr inbounds i64, i64* %cloptr11012, i64 4                  ; &eptr11017[4]
  %eptr11018 = getelementptr inbounds i64, i64* %cloptr11012, i64 5                  ; &eptr11018[5]
  %eptr11019 = getelementptr inbounds i64, i64* %cloptr11012, i64 6                  ; &eptr11019[6]
  store i64 %fWC$_37drop_45right, i64* %eptr11014                                    ; *eptr11014 = %fWC$_37drop_45right
  store i64 %RQ7$_37foldr1, i64* %eptr11015                                          ; *eptr11015 = %RQ7$_37foldr1
  store i64 %Bv6$_37length, i64* %eptr11016                                          ; *eptr11016 = %Bv6$_37length
  store i64 %hHt$Ycmb, i64* %eptr11017                                               ; *eptr11017 = %hHt$Ycmb
  store i64 %PdG$_37last, i64* %eptr11018                                            ; *eptr11018 = %PdG$_37last
  store i64 %uc0$_37foldl1, i64* %eptr11019                                          ; *eptr11019 = %uc0$_37foldl1
  %eptr11013 = getelementptr inbounds i64, i64* %cloptr11012, i64 0                  ; &cloptr11012[0]
  %f11020 = ptrtoint void(i64,i64,i64)* @lam10865 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11020, i64* %eptr11013                                                 ; store fptr
  %arg9552 = ptrtoint i64* %cloptr11012 to i64                                       ; closure cast; i64* -> i64
  %cloptr11021 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11023 = getelementptr inbounds i64, i64* %cloptr11021, i64 1                  ; &eptr11023[1]
  %eptr11024 = getelementptr inbounds i64, i64* %cloptr11021, i64 2                  ; &eptr11024[2]
  store i64 %RQ7$_37foldr1, i64* %eptr11023                                          ; *eptr11023 = %RQ7$_37foldr1
  store i64 %nad$_37map1, i64* %eptr11024                                            ; *eptr11024 = %nad$_37map1
  %eptr11022 = getelementptr inbounds i64, i64* %cloptr11021, i64 0                  ; &cloptr11021[0]
  %f11025 = ptrtoint void(i64,i64,i64)* @lam10505 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11025, i64* %eptr11022                                                 ; store fptr
  %arg9551 = ptrtoint i64* %cloptr11021 to i64                                       ; closure cast; i64* -> i64
  %cloptr11026 = inttoptr i64 %hHt$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11027 = getelementptr inbounds i64, i64* %cloptr11026, i64 0                 ; &cloptr11026[0]
  %f11029 = load i64, i64* %i0ptr11027, align 8                                      ; load; *i0ptr11027
  %fptr11028 = inttoptr i64 %f11029 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11028(i64 %hHt$Ycmb, i64 %arg9552, i64 %arg9551)    ; tail call
  ret void
}


define void @lam10875(i64 %env10876, i64 %cont9321, i64 %Hsl$lst) {
  %envptr11030 = inttoptr i64 %env10876 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11031 = getelementptr inbounds i64, i64* %envptr11030, i64 1                ; &envptr11030[1]
  %uc0$_37foldl1 = load i64, i64* %envptr11031, align 8                              ; load; *envptr11031
  %cloptr11032 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11033 = getelementptr inbounds i64, i64* %cloptr11032, i64 0                  ; &cloptr11032[0]
  %f11034 = ptrtoint void(i64,i64,i64,i64)* @lam10873 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11034, i64* %eptr11033                                                 ; store fptr
  %arg9536 = ptrtoint i64* %cloptr11032 to i64                                       ; closure cast; i64* -> i64
  %arg9535 = add i64 0, 0                                                            ; quoted ()
  %cloptr11035 = inttoptr i64 %uc0$_37foldl1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11036 = getelementptr inbounds i64, i64* %cloptr11035, i64 0                 ; &cloptr11035[0]
  %f11038 = load i64, i64* %i0ptr11036, align 8                                      ; load; *i0ptr11036
  %fptr11037 = inttoptr i64 %f11038 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11037(i64 %uc0$_37foldl1, i64 %cont9321, i64 %arg9536, i64 %arg9535, i64 %Hsl$lst); tail call
  ret void
}


define void @lam10873(i64 %env10874, i64 %cont9322, i64 %gEb$x, i64 %Kq1$y) {
  %arg9540 = add i64 0, 0                                                            ; quoted ()
  %cloptr11039 = inttoptr i64 %cont9322 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11040 = getelementptr inbounds i64, i64* %cloptr11039, i64 0                 ; &cloptr11039[0]
  %f11042 = load i64, i64* %i0ptr11040, align 8                                      ; load; *i0ptr11040
  %fptr11041 = inttoptr i64 %f11042 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11041(i64 %cont9322, i64 %arg9540, i64 %gEb$x)      ; tail call
  ret void
}


define void @lam10869(i64 %env10870, i64 %cont9323, i64 %whs$lst, i64 %QGH$n) {
  %envptr11043 = inttoptr i64 %env10870 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11044 = getelementptr inbounds i64, i64* %envptr11043, i64 2                ; &envptr11043[2]
  %Bv6$_37length = load i64, i64* %envptr11044, align 8                              ; load; *envptr11044
  %envptr11045 = inttoptr i64 %env10870 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11046 = getelementptr inbounds i64, i64* %envptr11045, i64 1                ; &envptr11045[1]
  %ig8$_37take = load i64, i64* %envptr11046, align 8                                ; load; *envptr11046
  %cloptr11047 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr11049 = getelementptr inbounds i64, i64* %cloptr11047, i64 1                  ; &eptr11049[1]
  %eptr11050 = getelementptr inbounds i64, i64* %cloptr11047, i64 2                  ; &eptr11050[2]
  %eptr11051 = getelementptr inbounds i64, i64* %cloptr11047, i64 3                  ; &eptr11051[3]
  %eptr11052 = getelementptr inbounds i64, i64* %cloptr11047, i64 4                  ; &eptr11052[4]
  store i64 %QGH$n, i64* %eptr11049                                                  ; *eptr11049 = %QGH$n
  store i64 %ig8$_37take, i64* %eptr11050                                            ; *eptr11050 = %ig8$_37take
  store i64 %cont9323, i64* %eptr11051                                               ; *eptr11051 = %cont9323
  store i64 %whs$lst, i64* %eptr11052                                                ; *eptr11052 = %whs$lst
  %eptr11048 = getelementptr inbounds i64, i64* %cloptr11047, i64 0                  ; &cloptr11047[0]
  %f11053 = ptrtoint void(i64,i64,i64)* @lam10867 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11053, i64* %eptr11048                                                 ; store fptr
  %arg9543 = ptrtoint i64* %cloptr11047 to i64                                       ; closure cast; i64* -> i64
  %cloptr11054 = inttoptr i64 %Bv6$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11055 = getelementptr inbounds i64, i64* %cloptr11054, i64 0                 ; &cloptr11054[0]
  %f11057 = load i64, i64* %i0ptr11055, align 8                                      ; load; *i0ptr11055
  %fptr11056 = inttoptr i64 %f11057 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11056(i64 %Bv6$_37length, i64 %arg9543, i64 %whs$lst); tail call
  ret void
}


define void @lam10867(i64 %env10868, i64 %_959324, i64 %a9214) {
  %envptr11058 = inttoptr i64 %env10868 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11059 = getelementptr inbounds i64, i64* %envptr11058, i64 4                ; &envptr11058[4]
  %whs$lst = load i64, i64* %envptr11059, align 8                                    ; load; *envptr11059
  %envptr11060 = inttoptr i64 %env10868 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11061 = getelementptr inbounds i64, i64* %envptr11060, i64 3                ; &envptr11060[3]
  %cont9323 = load i64, i64* %envptr11061, align 8                                   ; load; *envptr11061
  %envptr11062 = inttoptr i64 %env10868 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11063 = getelementptr inbounds i64, i64* %envptr11062, i64 2                ; &envptr11062[2]
  %ig8$_37take = load i64, i64* %envptr11063, align 8                                ; load; *envptr11063
  %envptr11064 = inttoptr i64 %env10868 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11065 = getelementptr inbounds i64, i64* %envptr11064, i64 1                ; &envptr11064[1]
  %QGH$n = load i64, i64* %envptr11065, align 8                                      ; load; *envptr11065
  %a9215 = call i64 @prim__45(i64 %a9214, i64 %QGH$n)                                ; call prim__45
  %cloptr11066 = inttoptr i64 %ig8$_37take to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr11067 = getelementptr inbounds i64, i64* %cloptr11066, i64 0                 ; &cloptr11066[0]
  %f11069 = load i64, i64* %i0ptr11067, align 8                                      ; load; *i0ptr11067
  %fptr11068 = inttoptr i64 %f11069 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11068(i64 %ig8$_37take, i64 %cont9323, i64 %whs$lst, i64 %a9215); tail call
  ret void
}


define void @lam10865(i64 %env10866, i64 %_959325, i64 %Aoo$_37foldr) {
  %envptr11070 = inttoptr i64 %env10866 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11071 = getelementptr inbounds i64, i64* %envptr11070, i64 6                ; &envptr11070[6]
  %uc0$_37foldl1 = load i64, i64* %envptr11071, align 8                              ; load; *envptr11071
  %envptr11072 = inttoptr i64 %env10866 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11073 = getelementptr inbounds i64, i64* %envptr11072, i64 5                ; &envptr11072[5]
  %PdG$_37last = load i64, i64* %envptr11073, align 8                                ; load; *envptr11073
  %envptr11074 = inttoptr i64 %env10866 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11075 = getelementptr inbounds i64, i64* %envptr11074, i64 4                ; &envptr11074[4]
  %hHt$Ycmb = load i64, i64* %envptr11075, align 8                                   ; load; *envptr11075
  %envptr11076 = inttoptr i64 %env10866 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11077 = getelementptr inbounds i64, i64* %envptr11076, i64 3                ; &envptr11076[3]
  %Bv6$_37length = load i64, i64* %envptr11077, align 8                              ; load; *envptr11077
  %envptr11078 = inttoptr i64 %env10866 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11079 = getelementptr inbounds i64, i64* %envptr11078, i64 2                ; &envptr11078[2]
  %RQ7$_37foldr1 = load i64, i64* %envptr11079, align 8                              ; load; *envptr11079
  %envptr11080 = inttoptr i64 %env10866 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11081 = getelementptr inbounds i64, i64* %envptr11080, i64 1                ; &envptr11080[1]
  %fWC$_37drop_45right = load i64, i64* %envptr11081, align 8                        ; load; *envptr11081
  %cloptr11082 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11084 = getelementptr inbounds i64, i64* %cloptr11082, i64 1                  ; &eptr11084[1]
  store i64 %RQ7$_37foldr1, i64* %eptr11084                                          ; *eptr11084 = %RQ7$_37foldr1
  %eptr11083 = getelementptr inbounds i64, i64* %cloptr11082, i64 0                  ; &cloptr11082[0]
  %f11085 = ptrtoint void(i64,i64,i64,i64)* @lam10863 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11085, i64* %eptr11083                                                 ; store fptr
  %yBl$_37map1 = ptrtoint i64* %cloptr11082 to i64                                   ; closure cast; i64* -> i64
  %cloptr11086 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11088 = getelementptr inbounds i64, i64* %cloptr11086, i64 1                  ; &eptr11088[1]
  %eptr11089 = getelementptr inbounds i64, i64* %cloptr11086, i64 2                  ; &eptr11089[2]
  %eptr11090 = getelementptr inbounds i64, i64* %cloptr11086, i64 3                  ; &eptr11090[3]
  store i64 %fWC$_37drop_45right, i64* %eptr11088                                    ; *eptr11088 = %fWC$_37drop_45right
  store i64 %Aoo$_37foldr, i64* %eptr11089                                           ; *eptr11089 = %Aoo$_37foldr
  store i64 %PdG$_37last, i64* %eptr11090                                            ; *eptr11090 = %PdG$_37last
  %eptr11087 = getelementptr inbounds i64, i64* %cloptr11086, i64 0                  ; &cloptr11086[0]
  %f11091 = ptrtoint void(i64,i64)* @lam10855 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f11091, i64* %eptr11087                                                 ; store fptr
  %Htz$_37map = ptrtoint i64* %cloptr11086 to i64                                    ; closure cast; i64* -> i64
  %cloptr11092 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11094 = getelementptr inbounds i64, i64* %cloptr11092, i64 1                  ; &eptr11094[1]
  %eptr11095 = getelementptr inbounds i64, i64* %cloptr11092, i64 2                  ; &eptr11095[2]
  store i64 %Bv6$_37length, i64* %eptr11094                                          ; *eptr11094 = %Bv6$_37length
  store i64 %uc0$_37foldl1, i64* %eptr11095                                          ; *eptr11095 = %uc0$_37foldl1
  %eptr11093 = getelementptr inbounds i64, i64* %cloptr11092, i64 0                  ; &cloptr11092[0]
  %f11096 = ptrtoint void(i64,i64,i64)* @lam10842 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11096, i64* %eptr11093                                                 ; store fptr
  %arg9594 = ptrtoint i64* %cloptr11092 to i64                                       ; closure cast; i64* -> i64
  %cloptr11097 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11099 = getelementptr inbounds i64, i64* %cloptr11097, i64 1                  ; &eptr11099[1]
  %eptr11100 = getelementptr inbounds i64, i64* %cloptr11097, i64 2                  ; &eptr11100[2]
  %eptr11101 = getelementptr inbounds i64, i64* %cloptr11097, i64 3                  ; &eptr11101[3]
  store i64 %RQ7$_37foldr1, i64* %eptr11099                                          ; *eptr11099 = %RQ7$_37foldr1
  store i64 %yBl$_37map1, i64* %eptr11100                                            ; *eptr11100 = %yBl$_37map1
  store i64 %Aoo$_37foldr, i64* %eptr11101                                           ; *eptr11101 = %Aoo$_37foldr
  %eptr11098 = getelementptr inbounds i64, i64* %cloptr11097, i64 0                  ; &cloptr11097[0]
  %f11102 = ptrtoint void(i64,i64,i64)* @lam10542 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11102, i64* %eptr11098                                                 ; store fptr
  %arg9593 = ptrtoint i64* %cloptr11097 to i64                                       ; closure cast; i64* -> i64
  %cloptr11103 = inttoptr i64 %hHt$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11104 = getelementptr inbounds i64, i64* %cloptr11103, i64 0                 ; &cloptr11103[0]
  %f11106 = load i64, i64* %i0ptr11104, align 8                                      ; load; *i0ptr11104
  %fptr11105 = inttoptr i64 %f11106 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11105(i64 %hHt$Ycmb, i64 %arg9594, i64 %arg9593)    ; tail call
  ret void
}


define void @lam10863(i64 %env10864, i64 %cont9326, i64 %N2n$f, i64 %nyJ$lst) {
  %envptr11107 = inttoptr i64 %env10864 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11108 = getelementptr inbounds i64, i64* %envptr11107, i64 1                ; &envptr11107[1]
  %RQ7$_37foldr1 = load i64, i64* %envptr11108, align 8                              ; load; *envptr11108
  %cloptr11109 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11111 = getelementptr inbounds i64, i64* %cloptr11109, i64 1                  ; &eptr11111[1]
  store i64 %N2n$f, i64* %eptr11111                                                  ; *eptr11111 = %N2n$f
  %eptr11110 = getelementptr inbounds i64, i64* %cloptr11109, i64 0                  ; &cloptr11109[0]
  %f11112 = ptrtoint void(i64,i64,i64,i64)* @lam10861 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11112, i64* %eptr11110                                                 ; store fptr
  %arg9556 = ptrtoint i64* %cloptr11109 to i64                                       ; closure cast; i64* -> i64
  %arg9555 = add i64 0, 0                                                            ; quoted ()
  %cloptr11113 = inttoptr i64 %RQ7$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11114 = getelementptr inbounds i64, i64* %cloptr11113, i64 0                 ; &cloptr11113[0]
  %f11116 = load i64, i64* %i0ptr11114, align 8                                      ; load; *i0ptr11114
  %fptr11115 = inttoptr i64 %f11116 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11115(i64 %RQ7$_37foldr1, i64 %cont9326, i64 %arg9556, i64 %arg9555, i64 %nyJ$lst); tail call
  ret void
}


define void @lam10861(i64 %env10862, i64 %cont9327, i64 %lqE$v, i64 %yed$r) {
  %envptr11117 = inttoptr i64 %env10862 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11118 = getelementptr inbounds i64, i64* %envptr11117, i64 1                ; &envptr11117[1]
  %N2n$f = load i64, i64* %envptr11118, align 8                                      ; load; *envptr11118
  %cloptr11119 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11121 = getelementptr inbounds i64, i64* %cloptr11119, i64 1                  ; &eptr11121[1]
  %eptr11122 = getelementptr inbounds i64, i64* %cloptr11119, i64 2                  ; &eptr11122[2]
  store i64 %yed$r, i64* %eptr11121                                                  ; *eptr11121 = %yed$r
  store i64 %cont9327, i64* %eptr11122                                               ; *eptr11122 = %cont9327
  %eptr11120 = getelementptr inbounds i64, i64* %cloptr11119, i64 0                  ; &cloptr11119[0]
  %f11123 = ptrtoint void(i64,i64,i64)* @lam10859 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11123, i64* %eptr11120                                                 ; store fptr
  %arg9560 = ptrtoint i64* %cloptr11119 to i64                                       ; closure cast; i64* -> i64
  %cloptr11124 = inttoptr i64 %N2n$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11125 = getelementptr inbounds i64, i64* %cloptr11124, i64 0                 ; &cloptr11124[0]
  %f11127 = load i64, i64* %i0ptr11125, align 8                                      ; load; *i0ptr11125
  %fptr11126 = inttoptr i64 %f11127 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11126(i64 %N2n$f, i64 %arg9560, i64 %lqE$v)         ; tail call
  ret void
}


define void @lam10859(i64 %env10860, i64 %_959328, i64 %a9224) {
  %envptr11128 = inttoptr i64 %env10860 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11129 = getelementptr inbounds i64, i64* %envptr11128, i64 2                ; &envptr11128[2]
  %cont9327 = load i64, i64* %envptr11129, align 8                                   ; load; *envptr11129
  %envptr11130 = inttoptr i64 %env10860 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11131 = getelementptr inbounds i64, i64* %envptr11130, i64 1                ; &envptr11130[1]
  %yed$r = load i64, i64* %envptr11131, align 8                                      ; load; *envptr11131
  %retprim9329 = call i64 @prim_cons(i64 %a9224, i64 %yed$r)                         ; call prim_cons
  %arg9565 = add i64 0, 0                                                            ; quoted ()
  %cloptr11132 = inttoptr i64 %cont9327 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11133 = getelementptr inbounds i64, i64* %cloptr11132, i64 0                 ; &cloptr11132[0]
  %f11135 = load i64, i64* %i0ptr11133, align 8                                      ; load; *i0ptr11133
  %fptr11134 = inttoptr i64 %f11135 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11134(i64 %cont9327, i64 %arg9565, i64 %retprim9329); tail call
  ret void
}


define void @lam10855(i64 %env10856, i64 %miC$args9331) {
  %envptr11136 = inttoptr i64 %env10856 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11137 = getelementptr inbounds i64, i64* %envptr11136, i64 3                ; &envptr11136[3]
  %PdG$_37last = load i64, i64* %envptr11137, align 8                                ; load; *envptr11137
  %envptr11138 = inttoptr i64 %env10856 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11139 = getelementptr inbounds i64, i64* %envptr11138, i64 2                ; &envptr11138[2]
  %Aoo$_37foldr = load i64, i64* %envptr11139, align 8                               ; load; *envptr11139
  %envptr11140 = inttoptr i64 %env10856 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11141 = getelementptr inbounds i64, i64* %envptr11140, i64 1                ; &envptr11140[1]
  %fWC$_37drop_45right = load i64, i64* %envptr11141, align 8                        ; load; *envptr11141
  %cont9330 = call i64 @prim_car(i64 %miC$args9331)                                  ; call prim_car
  %miC$args = call i64 @prim_cdr(i64 %miC$args9331)                                  ; call prim_cdr
  %Rcm$f = call i64 @prim_car(i64 %miC$args)                                         ; call prim_car
  %RCM$lsts = call i64 @prim_cdr(i64 %miC$args)                                      ; call prim_cdr
  %arg9572 = add i64 0, 0                                                            ; quoted ()
  %a9228 = call i64 @prim_cons(i64 %arg9572, i64 %RCM$lsts)                          ; call prim_cons
  %cloptr11142 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11144 = getelementptr inbounds i64, i64* %cloptr11142, i64 1                  ; &eptr11144[1]
  %eptr11145 = getelementptr inbounds i64, i64* %cloptr11142, i64 2                  ; &eptr11145[2]
  %eptr11146 = getelementptr inbounds i64, i64* %cloptr11142, i64 3                  ; &eptr11146[3]
  store i64 %Rcm$f, i64* %eptr11144                                                  ; *eptr11144 = %Rcm$f
  store i64 %fWC$_37drop_45right, i64* %eptr11145                                    ; *eptr11145 = %fWC$_37drop_45right
  store i64 %PdG$_37last, i64* %eptr11146                                            ; *eptr11146 = %PdG$_37last
  %eptr11143 = getelementptr inbounds i64, i64* %cloptr11142, i64 0                  ; &cloptr11142[0]
  %f11147 = ptrtoint void(i64,i64)* @lam10852 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f11147, i64* %eptr11143                                                 ; store fptr
  %arg9574 = ptrtoint i64* %cloptr11142 to i64                                       ; closure cast; i64* -> i64
  %a9229 = call i64 @prim_cons(i64 %arg9574, i64 %a9228)                             ; call prim_cons
  %cps_45lst9339 = call i64 @prim_cons(i64 %cont9330, i64 %a9229)                    ; call prim_cons
  %cloptr11148 = inttoptr i64 %Aoo$_37foldr to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr11149 = getelementptr inbounds i64, i64* %cloptr11148, i64 0                 ; &cloptr11148[0]
  %f11151 = load i64, i64* %i0ptr11149, align 8                                      ; load; *i0ptr11149
  %fptr11150 = inttoptr i64 %f11151 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11150(i64 %Aoo$_37foldr, i64 %cps_45lst9339)        ; tail call
  ret void
}


define void @lam10852(i64 %env10853, i64 %BGX$fargs9333) {
  %envptr11152 = inttoptr i64 %env10853 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11153 = getelementptr inbounds i64, i64* %envptr11152, i64 3                ; &envptr11152[3]
  %PdG$_37last = load i64, i64* %envptr11153, align 8                                ; load; *envptr11153
  %envptr11154 = inttoptr i64 %env10853 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11155 = getelementptr inbounds i64, i64* %envptr11154, i64 2                ; &envptr11154[2]
  %fWC$_37drop_45right = load i64, i64* %envptr11155, align 8                        ; load; *envptr11155
  %envptr11156 = inttoptr i64 %env10853 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11157 = getelementptr inbounds i64, i64* %envptr11156, i64 1                ; &envptr11156[1]
  %Rcm$f = load i64, i64* %envptr11157, align 8                                      ; load; *envptr11157
  %cont9332 = call i64 @prim_car(i64 %BGX$fargs9333)                                 ; call prim_car
  %BGX$fargs = call i64 @prim_cdr(i64 %BGX$fargs9333)                                ; call prim_cdr
  %cloptr11158 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr11160 = getelementptr inbounds i64, i64* %cloptr11158, i64 1                  ; &eptr11160[1]
  %eptr11161 = getelementptr inbounds i64, i64* %cloptr11158, i64 2                  ; &eptr11161[2]
  %eptr11162 = getelementptr inbounds i64, i64* %cloptr11158, i64 3                  ; &eptr11162[3]
  %eptr11163 = getelementptr inbounds i64, i64* %cloptr11158, i64 4                  ; &eptr11163[4]
  store i64 %Rcm$f, i64* %eptr11160                                                  ; *eptr11160 = %Rcm$f
  store i64 %PdG$_37last, i64* %eptr11161                                            ; *eptr11161 = %PdG$_37last
  store i64 %BGX$fargs, i64* %eptr11162                                              ; *eptr11162 = %BGX$fargs
  store i64 %cont9332, i64* %eptr11163                                               ; *eptr11163 = %cont9332
  %eptr11159 = getelementptr inbounds i64, i64* %cloptr11158, i64 0                  ; &cloptr11158[0]
  %f11164 = ptrtoint void(i64,i64,i64)* @lam10850 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11164, i64* %eptr11159                                                 ; store fptr
  %arg9579 = ptrtoint i64* %cloptr11158 to i64                                       ; closure cast; i64* -> i64
  %arg9577 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %cloptr11165 = inttoptr i64 %fWC$_37drop_45right to i64*                           ; closure/env cast; i64 -> i64*
  %i0ptr11166 = getelementptr inbounds i64, i64* %cloptr11165, i64 0                 ; &cloptr11165[0]
  %f11168 = load i64, i64* %i0ptr11166, align 8                                      ; load; *i0ptr11166
  %fptr11167 = inttoptr i64 %f11168 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11167(i64 %fWC$_37drop_45right, i64 %arg9579, i64 %BGX$fargs, i64 %arg9577); tail call
  ret void
}


define void @lam10850(i64 %env10851, i64 %_959334, i64 %a9225) {
  %envptr11169 = inttoptr i64 %env10851 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11170 = getelementptr inbounds i64, i64* %envptr11169, i64 4                ; &envptr11169[4]
  %cont9332 = load i64, i64* %envptr11170, align 8                                   ; load; *envptr11170
  %envptr11171 = inttoptr i64 %env10851 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11172 = getelementptr inbounds i64, i64* %envptr11171, i64 3                ; &envptr11171[3]
  %BGX$fargs = load i64, i64* %envptr11172, align 8                                  ; load; *envptr11172
  %envptr11173 = inttoptr i64 %env10851 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11174 = getelementptr inbounds i64, i64* %envptr11173, i64 2                ; &envptr11173[2]
  %PdG$_37last = load i64, i64* %envptr11174, align 8                                ; load; *envptr11174
  %envptr11175 = inttoptr i64 %env10851 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11176 = getelementptr inbounds i64, i64* %envptr11175, i64 1                ; &envptr11175[1]
  %Rcm$f = load i64, i64* %envptr11176, align 8                                      ; load; *envptr11176
  %cloptr11177 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11179 = getelementptr inbounds i64, i64* %cloptr11177, i64 1                  ; &eptr11179[1]
  %eptr11180 = getelementptr inbounds i64, i64* %cloptr11177, i64 2                  ; &eptr11180[2]
  %eptr11181 = getelementptr inbounds i64, i64* %cloptr11177, i64 3                  ; &eptr11181[3]
  store i64 %PdG$_37last, i64* %eptr11179                                            ; *eptr11179 = %PdG$_37last
  store i64 %BGX$fargs, i64* %eptr11180                                              ; *eptr11180 = %BGX$fargs
  store i64 %cont9332, i64* %eptr11181                                               ; *eptr11181 = %cont9332
  %eptr11178 = getelementptr inbounds i64, i64* %cloptr11177, i64 0                  ; &cloptr11177[0]
  %f11182 = ptrtoint void(i64,i64,i64)* @lam10848 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11182, i64* %eptr11178                                                 ; store fptr
  %arg9582 = ptrtoint i64* %cloptr11177 to i64                                       ; closure cast; i64* -> i64
  %cps_45lst9338 = call i64 @prim_cons(i64 %arg9582, i64 %a9225)                     ; call prim_cons
  %cloptr11183 = inttoptr i64 %Rcm$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11184 = getelementptr inbounds i64, i64* %cloptr11183, i64 0                 ; &cloptr11183[0]
  %f11186 = load i64, i64* %i0ptr11184, align 8                                      ; load; *i0ptr11184
  %fptr11185 = inttoptr i64 %f11186 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11185(i64 %Rcm$f, i64 %cps_45lst9338)               ; tail call
  ret void
}


define void @lam10848(i64 %env10849, i64 %_959335, i64 %a9226) {
  %envptr11187 = inttoptr i64 %env10849 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11188 = getelementptr inbounds i64, i64* %envptr11187, i64 3                ; &envptr11187[3]
  %cont9332 = load i64, i64* %envptr11188, align 8                                   ; load; *envptr11188
  %envptr11189 = inttoptr i64 %env10849 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11190 = getelementptr inbounds i64, i64* %envptr11189, i64 2                ; &envptr11189[2]
  %BGX$fargs = load i64, i64* %envptr11190, align 8                                  ; load; *envptr11190
  %envptr11191 = inttoptr i64 %env10849 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11192 = getelementptr inbounds i64, i64* %envptr11191, i64 1                ; &envptr11191[1]
  %PdG$_37last = load i64, i64* %envptr11192, align 8                                ; load; *envptr11192
  %cloptr11193 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11195 = getelementptr inbounds i64, i64* %cloptr11193, i64 1                  ; &eptr11195[1]
  %eptr11196 = getelementptr inbounds i64, i64* %cloptr11193, i64 2                  ; &eptr11196[2]
  store i64 %a9226, i64* %eptr11195                                                  ; *eptr11195 = %a9226
  store i64 %cont9332, i64* %eptr11196                                               ; *eptr11196 = %cont9332
  %eptr11194 = getelementptr inbounds i64, i64* %cloptr11193, i64 0                  ; &cloptr11193[0]
  %f11197 = ptrtoint void(i64,i64,i64)* @lam10846 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11197, i64* %eptr11194                                                 ; store fptr
  %arg9584 = ptrtoint i64* %cloptr11193 to i64                                       ; closure cast; i64* -> i64
  %cloptr11198 = inttoptr i64 %PdG$_37last to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr11199 = getelementptr inbounds i64, i64* %cloptr11198, i64 0                 ; &cloptr11198[0]
  %f11201 = load i64, i64* %i0ptr11199, align 8                                      ; load; *i0ptr11199
  %fptr11200 = inttoptr i64 %f11201 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11200(i64 %PdG$_37last, i64 %arg9584, i64 %BGX$fargs); tail call
  ret void
}


define void @lam10846(i64 %env10847, i64 %_959336, i64 %a9227) {
  %envptr11202 = inttoptr i64 %env10847 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11203 = getelementptr inbounds i64, i64* %envptr11202, i64 2                ; &envptr11202[2]
  %cont9332 = load i64, i64* %envptr11203, align 8                                   ; load; *envptr11203
  %envptr11204 = inttoptr i64 %env10847 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11205 = getelementptr inbounds i64, i64* %envptr11204, i64 1                ; &envptr11204[1]
  %a9226 = load i64, i64* %envptr11205, align 8                                      ; load; *envptr11205
  %retprim9337 = call i64 @prim_cons(i64 %a9226, i64 %a9227)                         ; call prim_cons
  %arg9589 = add i64 0, 0                                                            ; quoted ()
  %cloptr11206 = inttoptr i64 %cont9332 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11207 = getelementptr inbounds i64, i64* %cloptr11206, i64 0                 ; &cloptr11206[0]
  %f11209 = load i64, i64* %i0ptr11207, align 8                                      ; load; *i0ptr11207
  %fptr11208 = inttoptr i64 %f11209 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11208(i64 %cont9332, i64 %arg9589, i64 %retprim9337); tail call
  ret void
}


define void @lam10842(i64 %env10843, i64 %_959340, i64 %I3Q$_37foldl) {
  %envptr11210 = inttoptr i64 %env10843 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11211 = getelementptr inbounds i64, i64* %envptr11210, i64 2                ; &envptr11210[2]
  %uc0$_37foldl1 = load i64, i64* %envptr11211, align 8                              ; load; *envptr11211
  %envptr11212 = inttoptr i64 %env10843 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11213 = getelementptr inbounds i64, i64* %envptr11212, i64 1                ; &envptr11212[1]
  %Bv6$_37length = load i64, i64* %envptr11213, align 8                              ; load; *envptr11213
  %cloptr11214 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11215 = getelementptr inbounds i64, i64* %cloptr11214, i64 0                  ; &cloptr11214[0]
  %f11216 = ptrtoint void(i64,i64,i64,i64)* @lam10840 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11216, i64* %eptr11215                                                 ; store fptr
  %Q5z$_37_62 = ptrtoint i64* %cloptr11214 to i64                                    ; closure cast; i64* -> i64
  %cloptr11217 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11218 = getelementptr inbounds i64, i64* %cloptr11217, i64 0                  ; &cloptr11217[0]
  %f11219 = ptrtoint void(i64,i64,i64,i64)* @lam10837 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11219, i64* %eptr11218                                                 ; store fptr
  %dA5$_37_62_61 = ptrtoint i64* %cloptr11217 to i64                                 ; closure cast; i64* -> i64
  %arg9609 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %arg9608 = add i64 0, 0                                                            ; quoted ()
  %aCD$_37append = call i64 @prim_make_45vector(i64 %arg9609, i64 %arg9608)          ; call prim_make_45vector
  %arg9611 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %arg9610 = add i64 0, 0                                                            ; quoted ()
  %we3$_37append2 = call i64 @prim_make_45vector(i64 %arg9611, i64 %arg9610)         ; call prim_make_45vector
  %arg9613 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr11220 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11222 = getelementptr inbounds i64, i64* %cloptr11220, i64 1                  ; &eptr11222[1]
  store i64 %we3$_37append2, i64* %eptr11222                                         ; *eptr11222 = %we3$_37append2
  %eptr11221 = getelementptr inbounds i64, i64* %cloptr11220, i64 0                  ; &cloptr11220[0]
  %f11223 = ptrtoint void(i64,i64,i64,i64)* @lam10829 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11223, i64* %eptr11221                                                 ; store fptr
  %arg9612 = ptrtoint i64* %cloptr11220 to i64                                       ; closure cast; i64* -> i64
  %uwT$_950 = call i64 @prim_vector_45set_33(i64 %we3$_37append2, i64 %arg9613, i64 %arg9612); call prim_vector_45set_33
  %arg9633 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr11224 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11226 = getelementptr inbounds i64, i64* %cloptr11224, i64 1                  ; &eptr11226[1]
  %eptr11227 = getelementptr inbounds i64, i64* %cloptr11224, i64 2                  ; &eptr11227[2]
  store i64 %we3$_37append2, i64* %eptr11226                                         ; *eptr11226 = %we3$_37append2
  store i64 %aCD$_37append, i64* %eptr11227                                          ; *eptr11227 = %aCD$_37append
  %eptr11225 = getelementptr inbounds i64, i64* %cloptr11224, i64 0                  ; &cloptr11224[0]
  %f11228 = ptrtoint void(i64,i64)* @lam10821 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f11228, i64* %eptr11225                                                 ; store fptr
  %arg9632 = ptrtoint i64* %cloptr11224 to i64                                       ; closure cast; i64* -> i64
  %h5G$_951 = call i64 @prim_vector_45set_33(i64 %aCD$_37append, i64 %arg9633, i64 %arg9632); call prim_vector_45set_33
  %arg9653 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9441 = call i64 @prim_vector_45ref(i64 %aCD$_37append, i64 %arg9653)       ; call prim_vector_45ref
  %cloptr11229 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11231 = getelementptr inbounds i64, i64* %cloptr11229, i64 1                  ; &eptr11231[1]
  %eptr11232 = getelementptr inbounds i64, i64* %cloptr11229, i64 2                  ; &eptr11232[2]
  %eptr11233 = getelementptr inbounds i64, i64* %cloptr11229, i64 3                  ; &eptr11233[3]
  store i64 %Bv6$_37length, i64* %eptr11231                                          ; *eptr11231 = %Bv6$_37length
  store i64 %uc0$_37foldl1, i64* %eptr11232                                          ; *eptr11232 = %uc0$_37foldl1
  store i64 %Q5z$_37_62, i64* %eptr11233                                             ; *eptr11233 = %Q5z$_37_62
  %eptr11230 = getelementptr inbounds i64, i64* %cloptr11229, i64 0                  ; &cloptr11229[0]
  %f11234 = ptrtoint void(i64,i64,i64)* @lam10812 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11234, i64* %eptr11230                                                 ; store fptr
  %arg9657 = ptrtoint i64* %cloptr11229 to i64                                       ; closure cast; i64* -> i64
  %arg9656 = add i64 0, 0                                                            ; quoted ()
  %cloptr11235 = inttoptr i64 %arg9657 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11236 = getelementptr inbounds i64, i64* %cloptr11235, i64 0                 ; &cloptr11235[0]
  %f11238 = load i64, i64* %i0ptr11236, align 8                                      ; load; *i0ptr11236
  %fptr11237 = inttoptr i64 %f11238 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11237(i64 %arg9657, i64 %arg9656, i64 %retprim9441) ; tail call
  ret void
}


define void @lam10840(i64 %env10841, i64 %cont9341, i64 %b9O$a, i64 %EdM$b) {
  %a9237 = call i64 @prim__60_61(i64 %b9O$a, i64 %EdM$b)                             ; call prim__60_61
  %retprim9342 = call i64 @prim_not(i64 %a9237)                                      ; call prim_not
  %arg9600 = add i64 0, 0                                                            ; quoted ()
  %cloptr11239 = inttoptr i64 %cont9341 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11240 = getelementptr inbounds i64, i64* %cloptr11239, i64 0                 ; &cloptr11239[0]
  %f11242 = load i64, i64* %i0ptr11240, align 8                                      ; load; *i0ptr11240
  %fptr11241 = inttoptr i64 %f11242 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11241(i64 %cont9341, i64 %arg9600, i64 %retprim9342); tail call
  ret void
}


define void @lam10837(i64 %env10838, i64 %cont9343, i64 %dd6$a, i64 %sMh$b) {
  %a9238 = call i64 @prim__60(i64 %dd6$a, i64 %sMh$b)                                ; call prim__60
  %retprim9344 = call i64 @prim_not(i64 %a9238)                                      ; call prim_not
  %arg9606 = add i64 0, 0                                                            ; quoted ()
  %cloptr11243 = inttoptr i64 %cont9343 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11244 = getelementptr inbounds i64, i64* %cloptr11243, i64 0                 ; &cloptr11243[0]
  %f11246 = load i64, i64* %i0ptr11244, align 8                                      ; load; *i0ptr11244
  %fptr11245 = inttoptr i64 %f11246 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11245(i64 %cont9343, i64 %arg9606, i64 %retprim9344); tail call
  ret void
}


define void @lam10829(i64 %env10830, i64 %cont9434, i64 %C9V$ls0, i64 %g3E$ls1) {
  %envptr11247 = inttoptr i64 %env10830 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11248 = getelementptr inbounds i64, i64* %envptr11247, i64 1                ; &envptr11247[1]
  %we3$_37append2 = load i64, i64* %envptr11248, align 8                             ; load; *envptr11248
  %a9239 = call i64 @prim_null_63(i64 %C9V$ls0)                                      ; call prim_null_63
  %cmp11249 = icmp eq i64 %a9239, 15                                                 ; false?
  br i1 %cmp11249, label %else11251, label %then11250                                ; if

then11250:
  %arg9617 = add i64 0, 0                                                            ; quoted ()
  %cloptr11252 = inttoptr i64 %cont9434 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11253 = getelementptr inbounds i64, i64* %cloptr11252, i64 0                 ; &cloptr11252[0]
  %f11255 = load i64, i64* %i0ptr11253, align 8                                      ; load; *i0ptr11253
  %fptr11254 = inttoptr i64 %f11255 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11254(i64 %cont9434, i64 %arg9617, i64 %g3E$ls1)    ; tail call
  ret void

else11251:
  %a9240 = call i64 @prim_car(i64 %C9V$ls0)                                          ; call prim_car
  %arg9620 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9241 = call i64 @prim_vector_45ref(i64 %we3$_37append2, i64 %arg9620)            ; call prim_vector_45ref
  %a9242 = call i64 @prim_cdr(i64 %C9V$ls0)                                          ; call prim_cdr
  %cloptr11256 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11258 = getelementptr inbounds i64, i64* %cloptr11256, i64 1                  ; &eptr11258[1]
  %eptr11259 = getelementptr inbounds i64, i64* %cloptr11256, i64 2                  ; &eptr11259[2]
  store i64 %a9240, i64* %eptr11258                                                  ; *eptr11258 = %a9240
  store i64 %cont9434, i64* %eptr11259                                               ; *eptr11259 = %cont9434
  %eptr11257 = getelementptr inbounds i64, i64* %cloptr11256, i64 0                  ; &cloptr11256[0]
  %f11260 = ptrtoint void(i64,i64,i64)* @lam10826 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11260, i64* %eptr11257                                                 ; store fptr
  %arg9625 = ptrtoint i64* %cloptr11256 to i64                                       ; closure cast; i64* -> i64
  %cloptr11261 = inttoptr i64 %a9241 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11262 = getelementptr inbounds i64, i64* %cloptr11261, i64 0                 ; &cloptr11261[0]
  %f11264 = load i64, i64* %i0ptr11262, align 8                                      ; load; *i0ptr11262
  %fptr11263 = inttoptr i64 %f11264 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11263(i64 %a9241, i64 %arg9625, i64 %a9242, i64 %g3E$ls1); tail call
  ret void
}


define void @lam10826(i64 %env10827, i64 %_959435, i64 %a9243) {
  %envptr11265 = inttoptr i64 %env10827 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11266 = getelementptr inbounds i64, i64* %envptr11265, i64 2                ; &envptr11265[2]
  %cont9434 = load i64, i64* %envptr11266, align 8                                   ; load; *envptr11266
  %envptr11267 = inttoptr i64 %env10827 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11268 = getelementptr inbounds i64, i64* %envptr11267, i64 1                ; &envptr11267[1]
  %a9240 = load i64, i64* %envptr11268, align 8                                      ; load; *envptr11268
  %retprim9436 = call i64 @prim_cons(i64 %a9240, i64 %a9243)                         ; call prim_cons
  %arg9630 = add i64 0, 0                                                            ; quoted ()
  %cloptr11269 = inttoptr i64 %cont9434 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11270 = getelementptr inbounds i64, i64* %cloptr11269, i64 0                 ; &cloptr11269[0]
  %f11272 = load i64, i64* %i0ptr11270, align 8                                      ; load; *i0ptr11270
  %fptr11271 = inttoptr i64 %f11272 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11271(i64 %cont9434, i64 %arg9630, i64 %retprim9436); tail call
  ret void
}


define void @lam10821(i64 %env10822, i64 %viO$xs9438) {
  %envptr11273 = inttoptr i64 %env10822 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11274 = getelementptr inbounds i64, i64* %envptr11273, i64 2                ; &envptr11273[2]
  %aCD$_37append = load i64, i64* %envptr11274, align 8                              ; load; *envptr11274
  %envptr11275 = inttoptr i64 %env10822 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11276 = getelementptr inbounds i64, i64* %envptr11275, i64 1                ; &envptr11275[1]
  %we3$_37append2 = load i64, i64* %envptr11276, align 8                             ; load; *envptr11276
  %cont9437 = call i64 @prim_car(i64 %viO$xs9438)                                    ; call prim_car
  %viO$xs = call i64 @prim_cdr(i64 %viO$xs9438)                                      ; call prim_cdr
  %a9244 = call i64 @prim_null_63(i64 %viO$xs)                                       ; call prim_null_63
  %cmp11277 = icmp eq i64 %a9244, 15                                                 ; false?
  br i1 %cmp11277, label %else11279, label %then11278                                ; if

then11278:
  %arg9639 = add i64 0, 0                                                            ; quoted ()
  %arg9638 = add i64 0, 0                                                            ; quoted ()
  %cloptr11280 = inttoptr i64 %cont9437 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11281 = getelementptr inbounds i64, i64* %cloptr11280, i64 0                 ; &cloptr11280[0]
  %f11283 = load i64, i64* %i0ptr11281, align 8                                      ; load; *i0ptr11281
  %fptr11282 = inttoptr i64 %f11283 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11282(i64 %cont9437, i64 %arg9639, i64 %arg9638)    ; tail call
  ret void

else11279:
  %b17$hd = call i64 @prim_car(i64 %viO$xs)                                          ; call prim_car
  %xm3$tl = call i64 @prim_cdr(i64 %viO$xs)                                          ; call prim_cdr
  %arg9643 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9245 = call i64 @prim_vector_45ref(i64 %aCD$_37append, i64 %arg9643)             ; call prim_vector_45ref
  %cloptr11284 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11286 = getelementptr inbounds i64, i64* %cloptr11284, i64 1                  ; &eptr11286[1]
  %eptr11287 = getelementptr inbounds i64, i64* %cloptr11284, i64 2                  ; &eptr11287[2]
  %eptr11288 = getelementptr inbounds i64, i64* %cloptr11284, i64 3                  ; &eptr11288[3]
  store i64 %we3$_37append2, i64* %eptr11286                                         ; *eptr11286 = %we3$_37append2
  store i64 %cont9437, i64* %eptr11287                                               ; *eptr11287 = %cont9437
  store i64 %b17$hd, i64* %eptr11288                                                 ; *eptr11288 = %b17$hd
  %eptr11285 = getelementptr inbounds i64, i64* %cloptr11284, i64 0                  ; &cloptr11284[0]
  %f11289 = ptrtoint void(i64,i64,i64)* @lam10818 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11289, i64* %eptr11285                                                 ; store fptr
  %arg9646 = ptrtoint i64* %cloptr11284 to i64                                       ; closure cast; i64* -> i64
  %cps_45lst9440 = call i64 @prim_cons(i64 %arg9646, i64 %xm3$tl)                    ; call prim_cons
  %cloptr11290 = inttoptr i64 %a9245 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11291 = getelementptr inbounds i64, i64* %cloptr11290, i64 0                 ; &cloptr11290[0]
  %f11293 = load i64, i64* %i0ptr11291, align 8                                      ; load; *i0ptr11291
  %fptr11292 = inttoptr i64 %f11293 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11292(i64 %a9245, i64 %cps_45lst9440)               ; tail call
  ret void
}


define void @lam10818(i64 %env10819, i64 %_959439, i64 %Gjl$result1) {
  %envptr11294 = inttoptr i64 %env10819 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11295 = getelementptr inbounds i64, i64* %envptr11294, i64 3                ; &envptr11294[3]
  %b17$hd = load i64, i64* %envptr11295, align 8                                     ; load; *envptr11295
  %envptr11296 = inttoptr i64 %env10819 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11297 = getelementptr inbounds i64, i64* %envptr11296, i64 2                ; &envptr11296[2]
  %cont9437 = load i64, i64* %envptr11297, align 8                                   ; load; *envptr11297
  %envptr11298 = inttoptr i64 %env10819 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11299 = getelementptr inbounds i64, i64* %envptr11298, i64 1                ; &envptr11298[1]
  %we3$_37append2 = load i64, i64* %envptr11299, align 8                             ; load; *envptr11299
  %arg9647 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9246 = call i64 @prim_vector_45ref(i64 %we3$_37append2, i64 %arg9647)            ; call prim_vector_45ref
  %cloptr11300 = inttoptr i64 %a9246 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11301 = getelementptr inbounds i64, i64* %cloptr11300, i64 0                 ; &cloptr11300[0]
  %f11303 = load i64, i64* %i0ptr11301, align 8                                      ; load; *i0ptr11301
  %fptr11302 = inttoptr i64 %f11303 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11302(i64 %a9246, i64 %cont9437, i64 %b17$hd, i64 %Gjl$result1); tail call
  ret void
}


define void @lam10812(i64 %env10813, i64 %_959345, i64 %qlN$_37append) {
  %envptr11304 = inttoptr i64 %env10813 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11305 = getelementptr inbounds i64, i64* %envptr11304, i64 3                ; &envptr11304[3]
  %Q5z$_37_62 = load i64, i64* %envptr11305, align 8                                 ; load; *envptr11305
  %envptr11306 = inttoptr i64 %env10813 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11307 = getelementptr inbounds i64, i64* %envptr11306, i64 2                ; &envptr11306[2]
  %uc0$_37foldl1 = load i64, i64* %envptr11307, align 8                              ; load; *envptr11307
  %envptr11308 = inttoptr i64 %env10813 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11309 = getelementptr inbounds i64, i64* %envptr11308, i64 1                ; &envptr11308[1]
  %Bv6$_37length = load i64, i64* %envptr11309, align 8                              ; load; *envptr11309
  %cloptr11310 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11311 = getelementptr inbounds i64, i64* %cloptr11310, i64 0                  ; &cloptr11310[0]
  %f11312 = ptrtoint void(i64,i64,i64)* @lam10810 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11312, i64* %eptr11311                                                 ; store fptr
  %Vcp$_37list_63 = ptrtoint i64* %cloptr11310 to i64                                ; closure cast; i64* -> i64
  %cloptr11313 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11314 = getelementptr inbounds i64, i64* %cloptr11313, i64 0                  ; &cloptr11313[0]
  %f11315 = ptrtoint void(i64,i64,i64,i64)* @lam10770 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11315, i64* %eptr11314                                                 ; store fptr
  %I3I$_37drop = ptrtoint i64* %cloptr11313 to i64                                   ; closure cast; i64* -> i64
  %cloptr11316 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11317 = getelementptr inbounds i64, i64* %cloptr11316, i64 0                  ; &cloptr11316[0]
  %f11318 = ptrtoint void(i64,i64,i64,i64)* @lam10730 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11318, i64* %eptr11317                                                 ; store fptr
  %Cnd$_37memv = ptrtoint i64* %cloptr11316 to i64                                   ; closure cast; i64* -> i64
  %cloptr11319 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11321 = getelementptr inbounds i64, i64* %cloptr11319, i64 1                  ; &eptr11321[1]
  store i64 %uc0$_37foldl1, i64* %eptr11321                                          ; *eptr11321 = %uc0$_37foldl1
  %eptr11320 = getelementptr inbounds i64, i64* %cloptr11319, i64 0                  ; &cloptr11319[0]
  %f11322 = ptrtoint void(i64,i64)* @lam10699 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f11322, i64* %eptr11320                                                 ; store fptr
  %QKx$_37_47 = ptrtoint i64* %cloptr11319 to i64                                    ; closure cast; i64* -> i64
  %cloptr11323 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11324 = getelementptr inbounds i64, i64* %cloptr11323, i64 0                  ; &cloptr11323[0]
  %f11325 = ptrtoint void(i64,i64,i64)* @lam10691 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11325, i64* %eptr11324                                                 ; store fptr
  %GJK$_37first = ptrtoint i64* %cloptr11323 to i64                                  ; closure cast; i64* -> i64
  %cloptr11326 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11327 = getelementptr inbounds i64, i64* %cloptr11326, i64 0                  ; &cloptr11326[0]
  %f11328 = ptrtoint void(i64,i64,i64)* @lam10688 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11328, i64* %eptr11327                                                 ; store fptr
  %gTL$_37second = ptrtoint i64* %cloptr11326 to i64                                 ; closure cast; i64* -> i64
  %cloptr11329 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11330 = getelementptr inbounds i64, i64* %cloptr11329, i64 0                  ; &cloptr11329[0]
  %f11331 = ptrtoint void(i64,i64,i64)* @lam10685 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11331, i64* %eptr11330                                                 ; store fptr
  %sjh$_37third = ptrtoint i64* %cloptr11329 to i64                                  ; closure cast; i64* -> i64
  %cloptr11332 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11333 = getelementptr inbounds i64, i64* %cloptr11332, i64 0                  ; &cloptr11332[0]
  %f11334 = ptrtoint void(i64,i64,i64)* @lam10682 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11334, i64* %eptr11333                                                 ; store fptr
  %UbJ$_37fourth = ptrtoint i64* %cloptr11332 to i64                                 ; closure cast; i64* -> i64
  %cloptr11335 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11336 = getelementptr inbounds i64, i64* %cloptr11335, i64 0                  ; &cloptr11335[0]
  %f11337 = ptrtoint void(i64,i64,i64)* @lam10679 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11337, i64* %eptr11336                                                 ; store fptr
  %kS0$promise_63 = ptrtoint i64* %cloptr11335 to i64                                ; closure cast; i64* -> i64
  %cloptr11338 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11339 = getelementptr inbounds i64, i64* %cloptr11338, i64 0                  ; &cloptr11338[0]
  %f11340 = ptrtoint void(i64,i64)* @lam10673 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f11340, i64* %eptr11339                                                 ; store fptr
  %arg9919 = ptrtoint i64* %cloptr11338 to i64                                       ; closure cast; i64* -> i64
  %cloptr11341 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11343 = getelementptr inbounds i64, i64* %cloptr11341, i64 1                  ; &eptr11343[1]
  %eptr11344 = getelementptr inbounds i64, i64* %cloptr11341, i64 2                  ; &eptr11344[2]
  %eptr11345 = getelementptr inbounds i64, i64* %cloptr11341, i64 3                  ; &eptr11345[3]
  store i64 %I3I$_37drop, i64* %eptr11343                                            ; *eptr11343 = %I3I$_37drop
  store i64 %Bv6$_37length, i64* %eptr11344                                          ; *eptr11344 = %Bv6$_37length
  store i64 %Q5z$_37_62, i64* %eptr11345                                             ; *eptr11345 = %Q5z$_37_62
  %eptr11342 = getelementptr inbounds i64, i64* %cloptr11341, i64 0                  ; &cloptr11341[0]
  %f11346 = ptrtoint void(i64,i64,i64)* @lam10670 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11346, i64* %eptr11342                                                 ; store fptr
  %arg9918 = ptrtoint i64* %cloptr11341 to i64                                       ; closure cast; i64* -> i64
  %rva10405 = add i64 0, 0                                                           ; quoted ()
  %rva10404 = call i64 @prim_cons(i64 %arg9918, i64 %rva10405)                       ; call prim_cons
  %cloptr11347 = inttoptr i64 %arg9919 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11348 = getelementptr inbounds i64, i64* %cloptr11347, i64 0                 ; &cloptr11347[0]
  %f11350 = load i64, i64* %i0ptr11348, align 8                                      ; load; *i0ptr11348
  %fptr11349 = inttoptr i64 %f11350 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11349(i64 %arg9919, i64 %rva10404)                  ; tail call
  ret void
}


define void @lam10810(i64 %env10811, i64 %cont9346, i64 %RYI$a) {
  %arg9659 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %fux$a = call i64 @prim_make_45vector(i64 %arg9659, i64 %RYI$a)                    ; call prim_make_45vector
  %cloptr11351 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11352 = getelementptr inbounds i64, i64* %cloptr11351, i64 0                  ; &cloptr11351[0]
  %f11353 = ptrtoint void(i64,i64,i64)* @lam10807 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11353, i64* %eptr11352                                                 ; store fptr
  %arg9662 = ptrtoint i64* %cloptr11351 to i64                                       ; closure cast; i64* -> i64
  %cloptr11354 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11356 = getelementptr inbounds i64, i64* %cloptr11354, i64 1                  ; &eptr11356[1]
  %eptr11357 = getelementptr inbounds i64, i64* %cloptr11354, i64 2                  ; &eptr11357[2]
  store i64 %fux$a, i64* %eptr11356                                                  ; *eptr11356 = %fux$a
  store i64 %cont9346, i64* %eptr11357                                               ; *eptr11357 = %cont9346
  %eptr11355 = getelementptr inbounds i64, i64* %cloptr11354, i64 0                  ; &cloptr11354[0]
  %f11358 = ptrtoint void(i64,i64,i64)* @lam10804 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11358, i64* %eptr11355                                                 ; store fptr
  %arg9661 = ptrtoint i64* %cloptr11354 to i64                                       ; closure cast; i64* -> i64
  %cloptr11359 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11361 = getelementptr inbounds i64, i64* %cloptr11359, i64 1                  ; &eptr11361[1]
  %eptr11362 = getelementptr inbounds i64, i64* %cloptr11359, i64 2                  ; &eptr11362[2]
  store i64 %fux$a, i64* %eptr11361                                                  ; *eptr11361 = %fux$a
  store i64 %cont9346, i64* %eptr11362                                               ; *eptr11362 = %cont9346
  %eptr11360 = getelementptr inbounds i64, i64* %cloptr11359, i64 0                  ; &cloptr11359[0]
  %f11363 = ptrtoint void(i64,i64,i64)* @lam10787 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11363, i64* %eptr11360                                                 ; store fptr
  %arg9660 = ptrtoint i64* %cloptr11359 to i64                                       ; closure cast; i64* -> i64
  %cloptr11364 = inttoptr i64 %arg9662 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11365 = getelementptr inbounds i64, i64* %cloptr11364, i64 0                 ; &cloptr11364[0]
  %f11367 = load i64, i64* %i0ptr11365, align 8                                      ; load; *i0ptr11365
  %fptr11366 = inttoptr i64 %f11367 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11366(i64 %arg9662, i64 %arg9661, i64 %arg9660)     ; tail call
  ret void
}


define void @lam10807(i64 %env10808, i64 %cont9352, i64 %Sbs$k) {
  %arg9664 = add i64 0, 0                                                            ; quoted ()
  %cloptr11368 = inttoptr i64 %cont9352 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11369 = getelementptr inbounds i64, i64* %cloptr11368, i64 0                 ; &cloptr11368[0]
  %f11371 = load i64, i64* %i0ptr11369, align 8                                      ; load; *i0ptr11369
  %fptr11370 = inttoptr i64 %f11371 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11370(i64 %cont9352, i64 %arg9664, i64 %Sbs$k)      ; tail call
  ret void
}


define void @lam10804(i64 %env10805, i64 %_959347, i64 %pUb$cc) {
  %envptr11372 = inttoptr i64 %env10805 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11373 = getelementptr inbounds i64, i64* %envptr11372, i64 2                ; &envptr11372[2]
  %cont9346 = load i64, i64* %envptr11373, align 8                                   ; load; *envptr11373
  %envptr11374 = inttoptr i64 %env10805 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11375 = getelementptr inbounds i64, i64* %envptr11374, i64 1                ; &envptr11374[1]
  %fux$a = load i64, i64* %envptr11375, align 8                                      ; load; *envptr11375
  %arg9666 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9247 = call i64 @prim_vector_45ref(i64 %fux$a, i64 %arg9666)                     ; call prim_vector_45ref
  %a9248 = call i64 @prim_null_63(i64 %a9247)                                        ; call prim_null_63
  %cmp11376 = icmp eq i64 %a9248, 15                                                 ; false?
  br i1 %cmp11376, label %else11378, label %then11377                                ; if

then11377:
  %arg9670 = add i64 0, 0                                                            ; quoted ()
  %arg9669 = call i64 @const_init_true()                                             ; quoted #t
  %cloptr11379 = inttoptr i64 %cont9346 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11380 = getelementptr inbounds i64, i64* %cloptr11379, i64 0                 ; &cloptr11379[0]
  %f11382 = load i64, i64* %i0ptr11380, align 8                                      ; load; *i0ptr11380
  %fptr11381 = inttoptr i64 %f11382 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11381(i64 %cont9346, i64 %arg9670, i64 %arg9669)    ; tail call
  ret void

else11378:
  %arg9672 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9249 = call i64 @prim_vector_45ref(i64 %fux$a, i64 %arg9672)                     ; call prim_vector_45ref
  %a9250 = call i64 @prim_cons_63(i64 %a9249)                                        ; call prim_cons_63
  %cmp11383 = icmp eq i64 %a9250, 15                                                 ; false?
  br i1 %cmp11383, label %else11385, label %then11384                                ; if

then11384:
  %arg9675 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9251 = call i64 @prim_vector_45ref(i64 %fux$a, i64 %arg9675)                     ; call prim_vector_45ref
  %retprim9351 = call i64 @prim_cdr(i64 %a9251)                                      ; call prim_cdr
  %cloptr11386 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11388 = getelementptr inbounds i64, i64* %cloptr11386, i64 1                  ; &eptr11388[1]
  %eptr11389 = getelementptr inbounds i64, i64* %cloptr11386, i64 2                  ; &eptr11389[2]
  %eptr11390 = getelementptr inbounds i64, i64* %cloptr11386, i64 3                  ; &eptr11390[3]
  store i64 %pUb$cc, i64* %eptr11388                                                 ; *eptr11388 = %pUb$cc
  store i64 %fux$a, i64* %eptr11389                                                  ; *eptr11389 = %fux$a
  store i64 %cont9346, i64* %eptr11390                                               ; *eptr11390 = %cont9346
  %eptr11387 = getelementptr inbounds i64, i64* %cloptr11386, i64 0                  ; &cloptr11386[0]
  %f11391 = ptrtoint void(i64,i64,i64)* @lam10797 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11391, i64* %eptr11387                                                 ; store fptr
  %arg9680 = ptrtoint i64* %cloptr11386 to i64                                       ; closure cast; i64* -> i64
  %arg9679 = add i64 0, 0                                                            ; quoted ()
  %cloptr11392 = inttoptr i64 %arg9680 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11393 = getelementptr inbounds i64, i64* %cloptr11392, i64 0                 ; &cloptr11392[0]
  %f11395 = load i64, i64* %i0ptr11393, align 8                                      ; load; *i0ptr11393
  %fptr11394 = inttoptr i64 %f11395 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11394(i64 %arg9680, i64 %arg9679, i64 %retprim9351) ; tail call
  ret void

else11385:
  %arg9694 = add i64 0, 0                                                            ; quoted ()
  %arg9693 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr11396 = inttoptr i64 %cont9346 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11397 = getelementptr inbounds i64, i64* %cloptr11396, i64 0                 ; &cloptr11396[0]
  %f11399 = load i64, i64* %i0ptr11397, align 8                                      ; load; *i0ptr11397
  %fptr11398 = inttoptr i64 %f11399 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11398(i64 %cont9346, i64 %arg9694, i64 %arg9693)    ; tail call
  ret void
}


define void @lam10797(i64 %env10798, i64 %_959348, i64 %o4d$b) {
  %envptr11400 = inttoptr i64 %env10798 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11401 = getelementptr inbounds i64, i64* %envptr11400, i64 3                ; &envptr11400[3]
  %cont9346 = load i64, i64* %envptr11401, align 8                                   ; load; *envptr11401
  %envptr11402 = inttoptr i64 %env10798 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11403 = getelementptr inbounds i64, i64* %envptr11402, i64 2                ; &envptr11402[2]
  %fux$a = load i64, i64* %envptr11403, align 8                                      ; load; *envptr11403
  %envptr11404 = inttoptr i64 %env10798 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11405 = getelementptr inbounds i64, i64* %envptr11404, i64 1                ; &envptr11404[1]
  %pUb$cc = load i64, i64* %envptr11405, align 8                                     ; load; *envptr11405
  %arg9681 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9252 = call i64 @prim_vector_45ref(i64 %fux$a, i64 %arg9681)                     ; call prim_vector_45ref
  %a9253 = call i64 @prim_cdr(i64 %a9252)                                            ; call prim_cdr
  %arg9685 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9350 = call i64 @prim_vector_45set_33(i64 %fux$a, i64 %arg9685, i64 %a9253); call prim_vector_45set_33
  %cloptr11406 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11408 = getelementptr inbounds i64, i64* %cloptr11406, i64 1                  ; &eptr11408[1]
  %eptr11409 = getelementptr inbounds i64, i64* %cloptr11406, i64 2                  ; &eptr11409[2]
  store i64 %pUb$cc, i64* %eptr11408                                                 ; *eptr11408 = %pUb$cc
  store i64 %cont9346, i64* %eptr11409                                               ; *eptr11409 = %cont9346
  %eptr11407 = getelementptr inbounds i64, i64* %cloptr11406, i64 0                  ; &cloptr11406[0]
  %f11410 = ptrtoint void(i64,i64,i64)* @lam10793 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11410, i64* %eptr11407                                                 ; store fptr
  %arg9689 = ptrtoint i64* %cloptr11406 to i64                                       ; closure cast; i64* -> i64
  %arg9688 = add i64 0, 0                                                            ; quoted ()
  %cloptr11411 = inttoptr i64 %arg9689 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11412 = getelementptr inbounds i64, i64* %cloptr11411, i64 0                 ; &cloptr11411[0]
  %f11414 = load i64, i64* %i0ptr11412, align 8                                      ; load; *i0ptr11412
  %fptr11413 = inttoptr i64 %f11414 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11413(i64 %arg9689, i64 %arg9688, i64 %retprim9350) ; tail call
  ret void
}


define void @lam10793(i64 %env10794, i64 %_959349, i64 %XCK$_950) {
  %envptr11415 = inttoptr i64 %env10794 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11416 = getelementptr inbounds i64, i64* %envptr11415, i64 2                ; &envptr11415[2]
  %cont9346 = load i64, i64* %envptr11416, align 8                                   ; load; *envptr11416
  %envptr11417 = inttoptr i64 %env10794 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11418 = getelementptr inbounds i64, i64* %envptr11417, i64 1                ; &envptr11417[1]
  %pUb$cc = load i64, i64* %envptr11418, align 8                                     ; load; *envptr11418
  %cloptr11419 = inttoptr i64 %pUb$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr11420 = getelementptr inbounds i64, i64* %cloptr11419, i64 0                 ; &cloptr11419[0]
  %f11422 = load i64, i64* %i0ptr11420, align 8                                      ; load; *i0ptr11420
  %fptr11421 = inttoptr i64 %f11422 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11421(i64 %pUb$cc, i64 %cont9346, i64 %pUb$cc)      ; tail call
  ret void
}


define void @lam10787(i64 %env10788, i64 %_959347, i64 %pUb$cc) {
  %envptr11423 = inttoptr i64 %env10788 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11424 = getelementptr inbounds i64, i64* %envptr11423, i64 2                ; &envptr11423[2]
  %cont9346 = load i64, i64* %envptr11424, align 8                                   ; load; *envptr11424
  %envptr11425 = inttoptr i64 %env10788 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11426 = getelementptr inbounds i64, i64* %envptr11425, i64 1                ; &envptr11425[1]
  %fux$a = load i64, i64* %envptr11426, align 8                                      ; load; *envptr11426
  %arg9696 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9247 = call i64 @prim_vector_45ref(i64 %fux$a, i64 %arg9696)                     ; call prim_vector_45ref
  %a9248 = call i64 @prim_null_63(i64 %a9247)                                        ; call prim_null_63
  %cmp11427 = icmp eq i64 %a9248, 15                                                 ; false?
  br i1 %cmp11427, label %else11429, label %then11428                                ; if

then11428:
  %arg9700 = add i64 0, 0                                                            ; quoted ()
  %arg9699 = call i64 @const_init_true()                                             ; quoted #t
  %cloptr11430 = inttoptr i64 %cont9346 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11431 = getelementptr inbounds i64, i64* %cloptr11430, i64 0                 ; &cloptr11430[0]
  %f11433 = load i64, i64* %i0ptr11431, align 8                                      ; load; *i0ptr11431
  %fptr11432 = inttoptr i64 %f11433 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11432(i64 %cont9346, i64 %arg9700, i64 %arg9699)    ; tail call
  ret void

else11429:
  %arg9702 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9249 = call i64 @prim_vector_45ref(i64 %fux$a, i64 %arg9702)                     ; call prim_vector_45ref
  %a9250 = call i64 @prim_cons_63(i64 %a9249)                                        ; call prim_cons_63
  %cmp11434 = icmp eq i64 %a9250, 15                                                 ; false?
  br i1 %cmp11434, label %else11436, label %then11435                                ; if

then11435:
  %arg9705 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9251 = call i64 @prim_vector_45ref(i64 %fux$a, i64 %arg9705)                     ; call prim_vector_45ref
  %retprim9351 = call i64 @prim_cdr(i64 %a9251)                                      ; call prim_cdr
  %cloptr11437 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11439 = getelementptr inbounds i64, i64* %cloptr11437, i64 1                  ; &eptr11439[1]
  %eptr11440 = getelementptr inbounds i64, i64* %cloptr11437, i64 2                  ; &eptr11440[2]
  %eptr11441 = getelementptr inbounds i64, i64* %cloptr11437, i64 3                  ; &eptr11441[3]
  store i64 %pUb$cc, i64* %eptr11439                                                 ; *eptr11439 = %pUb$cc
  store i64 %fux$a, i64* %eptr11440                                                  ; *eptr11440 = %fux$a
  store i64 %cont9346, i64* %eptr11441                                               ; *eptr11441 = %cont9346
  %eptr11438 = getelementptr inbounds i64, i64* %cloptr11437, i64 0                  ; &cloptr11437[0]
  %f11442 = ptrtoint void(i64,i64,i64)* @lam10780 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11442, i64* %eptr11438                                                 ; store fptr
  %arg9710 = ptrtoint i64* %cloptr11437 to i64                                       ; closure cast; i64* -> i64
  %arg9709 = add i64 0, 0                                                            ; quoted ()
  %cloptr11443 = inttoptr i64 %arg9710 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11444 = getelementptr inbounds i64, i64* %cloptr11443, i64 0                 ; &cloptr11443[0]
  %f11446 = load i64, i64* %i0ptr11444, align 8                                      ; load; *i0ptr11444
  %fptr11445 = inttoptr i64 %f11446 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11445(i64 %arg9710, i64 %arg9709, i64 %retprim9351) ; tail call
  ret void

else11436:
  %arg9724 = add i64 0, 0                                                            ; quoted ()
  %arg9723 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr11447 = inttoptr i64 %cont9346 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11448 = getelementptr inbounds i64, i64* %cloptr11447, i64 0                 ; &cloptr11447[0]
  %f11450 = load i64, i64* %i0ptr11448, align 8                                      ; load; *i0ptr11448
  %fptr11449 = inttoptr i64 %f11450 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11449(i64 %cont9346, i64 %arg9724, i64 %arg9723)    ; tail call
  ret void
}


define void @lam10780(i64 %env10781, i64 %_959348, i64 %o4d$b) {
  %envptr11451 = inttoptr i64 %env10781 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11452 = getelementptr inbounds i64, i64* %envptr11451, i64 3                ; &envptr11451[3]
  %cont9346 = load i64, i64* %envptr11452, align 8                                   ; load; *envptr11452
  %envptr11453 = inttoptr i64 %env10781 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11454 = getelementptr inbounds i64, i64* %envptr11453, i64 2                ; &envptr11453[2]
  %fux$a = load i64, i64* %envptr11454, align 8                                      ; load; *envptr11454
  %envptr11455 = inttoptr i64 %env10781 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11456 = getelementptr inbounds i64, i64* %envptr11455, i64 1                ; &envptr11455[1]
  %pUb$cc = load i64, i64* %envptr11456, align 8                                     ; load; *envptr11456
  %arg9711 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9252 = call i64 @prim_vector_45ref(i64 %fux$a, i64 %arg9711)                     ; call prim_vector_45ref
  %a9253 = call i64 @prim_cdr(i64 %a9252)                                            ; call prim_cdr
  %arg9715 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9350 = call i64 @prim_vector_45set_33(i64 %fux$a, i64 %arg9715, i64 %a9253); call prim_vector_45set_33
  %cloptr11457 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11459 = getelementptr inbounds i64, i64* %cloptr11457, i64 1                  ; &eptr11459[1]
  %eptr11460 = getelementptr inbounds i64, i64* %cloptr11457, i64 2                  ; &eptr11460[2]
  store i64 %pUb$cc, i64* %eptr11459                                                 ; *eptr11459 = %pUb$cc
  store i64 %cont9346, i64* %eptr11460                                               ; *eptr11460 = %cont9346
  %eptr11458 = getelementptr inbounds i64, i64* %cloptr11457, i64 0                  ; &cloptr11457[0]
  %f11461 = ptrtoint void(i64,i64,i64)* @lam10776 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11461, i64* %eptr11458                                                 ; store fptr
  %arg9719 = ptrtoint i64* %cloptr11457 to i64                                       ; closure cast; i64* -> i64
  %arg9718 = add i64 0, 0                                                            ; quoted ()
  %cloptr11462 = inttoptr i64 %arg9719 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11463 = getelementptr inbounds i64, i64* %cloptr11462, i64 0                 ; &cloptr11462[0]
  %f11465 = load i64, i64* %i0ptr11463, align 8                                      ; load; *i0ptr11463
  %fptr11464 = inttoptr i64 %f11465 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11464(i64 %arg9719, i64 %arg9718, i64 %retprim9350) ; tail call
  ret void
}


define void @lam10776(i64 %env10777, i64 %_959349, i64 %XCK$_950) {
  %envptr11466 = inttoptr i64 %env10777 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11467 = getelementptr inbounds i64, i64* %envptr11466, i64 2                ; &envptr11466[2]
  %cont9346 = load i64, i64* %envptr11467, align 8                                   ; load; *envptr11467
  %envptr11468 = inttoptr i64 %env10777 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11469 = getelementptr inbounds i64, i64* %envptr11468, i64 1                ; &envptr11468[1]
  %pUb$cc = load i64, i64* %envptr11469, align 8                                     ; load; *envptr11469
  %cloptr11470 = inttoptr i64 %pUb$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr11471 = getelementptr inbounds i64, i64* %cloptr11470, i64 0                 ; &cloptr11470[0]
  %f11473 = load i64, i64* %i0ptr11471, align 8                                      ; load; *i0ptr11471
  %fptr11472 = inttoptr i64 %f11473 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11472(i64 %pUb$cc, i64 %cont9346, i64 %pUb$cc)      ; tail call
  ret void
}


define void @lam10770(i64 %env10771, i64 %cont9353, i64 %xi5$lst, i64 %NTB$n) {
  %arg9727 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %tTX$lst = call i64 @prim_make_45vector(i64 %arg9727, i64 %xi5$lst)                ; call prim_make_45vector
  %arg9729 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %ayU$n = call i64 @prim_make_45vector(i64 %arg9729, i64 %NTB$n)                    ; call prim_make_45vector
  %cloptr11474 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11475 = getelementptr inbounds i64, i64* %cloptr11474, i64 0                  ; &cloptr11474[0]
  %f11476 = ptrtoint void(i64,i64,i64)* @lam10766 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11476, i64* %eptr11475                                                 ; store fptr
  %arg9732 = ptrtoint i64* %cloptr11474 to i64                                       ; closure cast; i64* -> i64
  %cloptr11477 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11479 = getelementptr inbounds i64, i64* %cloptr11477, i64 1                  ; &eptr11479[1]
  %eptr11480 = getelementptr inbounds i64, i64* %cloptr11477, i64 2                  ; &eptr11480[2]
  %eptr11481 = getelementptr inbounds i64, i64* %cloptr11477, i64 3                  ; &eptr11481[3]
  store i64 %tTX$lst, i64* %eptr11479                                                ; *eptr11479 = %tTX$lst
  store i64 %cont9353, i64* %eptr11480                                               ; *eptr11480 = %cont9353
  store i64 %ayU$n, i64* %eptr11481                                                  ; *eptr11481 = %ayU$n
  %eptr11478 = getelementptr inbounds i64, i64* %cloptr11477, i64 0                  ; &cloptr11477[0]
  %f11482 = ptrtoint void(i64,i64,i64)* @lam10764 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11482, i64* %eptr11478                                                 ; store fptr
  %arg9731 = ptrtoint i64* %cloptr11477 to i64                                       ; closure cast; i64* -> i64
  %cloptr11483 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11485 = getelementptr inbounds i64, i64* %cloptr11483, i64 1                  ; &eptr11485[1]
  %eptr11486 = getelementptr inbounds i64, i64* %cloptr11483, i64 2                  ; &eptr11486[2]
  %eptr11487 = getelementptr inbounds i64, i64* %cloptr11483, i64 3                  ; &eptr11487[3]
  store i64 %tTX$lst, i64* %eptr11485                                                ; *eptr11485 = %tTX$lst
  store i64 %cont9353, i64* %eptr11486                                               ; *eptr11486 = %cont9353
  store i64 %ayU$n, i64* %eptr11487                                                  ; *eptr11487 = %ayU$n
  %eptr11484 = getelementptr inbounds i64, i64* %cloptr11483, i64 0                  ; &cloptr11483[0]
  %f11488 = ptrtoint void(i64,i64,i64)* @lam10747 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11488, i64* %eptr11484                                                 ; store fptr
  %arg9730 = ptrtoint i64* %cloptr11483 to i64                                       ; closure cast; i64* -> i64
  %cloptr11489 = inttoptr i64 %arg9732 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11490 = getelementptr inbounds i64, i64* %cloptr11489, i64 0                 ; &cloptr11489[0]
  %f11492 = load i64, i64* %i0ptr11490, align 8                                      ; load; *i0ptr11490
  %fptr11491 = inttoptr i64 %f11492 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11491(i64 %arg9732, i64 %arg9731, i64 %arg9730)     ; tail call
  ret void
}


define void @lam10766(i64 %env10767, i64 %cont9360, i64 %VOC$u) {
  %cloptr11493 = inttoptr i64 %VOC$u to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11494 = getelementptr inbounds i64, i64* %cloptr11493, i64 0                 ; &cloptr11493[0]
  %f11496 = load i64, i64* %i0ptr11494, align 8                                      ; load; *i0ptr11494
  %fptr11495 = inttoptr i64 %f11496 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11495(i64 %VOC$u, i64 %cont9360, i64 %VOC$u)        ; tail call
  ret void
}


define void @lam10764(i64 %env10765, i64 %_959354, i64 %rV9$cc) {
  %envptr11497 = inttoptr i64 %env10765 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11498 = getelementptr inbounds i64, i64* %envptr11497, i64 3                ; &envptr11497[3]
  %ayU$n = load i64, i64* %envptr11498, align 8                                      ; load; *envptr11498
  %envptr11499 = inttoptr i64 %env10765 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11500 = getelementptr inbounds i64, i64* %envptr11499, i64 2                ; &envptr11499[2]
  %cont9353 = load i64, i64* %envptr11500, align 8                                   ; load; *envptr11500
  %envptr11501 = inttoptr i64 %env10765 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11502 = getelementptr inbounds i64, i64* %envptr11501, i64 1                ; &envptr11501[1]
  %tTX$lst = load i64, i64* %envptr11502, align 8                                    ; load; *envptr11502
  %arg9736 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9254 = call i64 @prim_vector_45ref(i64 %ayU$n, i64 %arg9736)                     ; call prim_vector_45ref
  %arg9739 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9255 = call i64 @prim__61(i64 %arg9739, i64 %a9254)                              ; call prim__61
  %cmp11503 = icmp eq i64 %a9255, 15                                                 ; false?
  br i1 %cmp11503, label %else11505, label %then11504                                ; if

then11504:
  %arg9740 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9355 = call i64 @prim_vector_45ref(i64 %tTX$lst, i64 %arg9740)             ; call prim_vector_45ref
  %arg9743 = add i64 0, 0                                                            ; quoted ()
  %cloptr11506 = inttoptr i64 %cont9353 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11507 = getelementptr inbounds i64, i64* %cloptr11506, i64 0                 ; &cloptr11506[0]
  %f11509 = load i64, i64* %i0ptr11507, align 8                                      ; load; *i0ptr11507
  %fptr11508 = inttoptr i64 %f11509 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11508(i64 %cont9353, i64 %arg9743, i64 %retprim9355); tail call
  ret void

else11505:
  %arg9745 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9256 = call i64 @prim_vector_45ref(i64 %tTX$lst, i64 %arg9745)                   ; call prim_vector_45ref
  %a9257 = call i64 @prim_cdr(i64 %a9256)                                            ; call prim_cdr
  %arg9749 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9359 = call i64 @prim_vector_45set_33(i64 %tTX$lst, i64 %arg9749, i64 %a9257); call prim_vector_45set_33
  %cloptr11510 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11512 = getelementptr inbounds i64, i64* %cloptr11510, i64 1                  ; &eptr11512[1]
  %eptr11513 = getelementptr inbounds i64, i64* %cloptr11510, i64 2                  ; &eptr11513[2]
  %eptr11514 = getelementptr inbounds i64, i64* %cloptr11510, i64 3                  ; &eptr11514[3]
  store i64 %cont9353, i64* %eptr11512                                               ; *eptr11512 = %cont9353
  store i64 %rV9$cc, i64* %eptr11513                                                 ; *eptr11513 = %rV9$cc
  store i64 %ayU$n, i64* %eptr11514                                                  ; *eptr11514 = %ayU$n
  %eptr11511 = getelementptr inbounds i64, i64* %cloptr11510, i64 0                  ; &cloptr11510[0]
  %f11515 = ptrtoint void(i64,i64,i64)* @lam10758 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11515, i64* %eptr11511                                                 ; store fptr
  %arg9753 = ptrtoint i64* %cloptr11510 to i64                                       ; closure cast; i64* -> i64
  %arg9752 = add i64 0, 0                                                            ; quoted ()
  %cloptr11516 = inttoptr i64 %arg9753 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11517 = getelementptr inbounds i64, i64* %cloptr11516, i64 0                 ; &cloptr11516[0]
  %f11519 = load i64, i64* %i0ptr11517, align 8                                      ; load; *i0ptr11517
  %fptr11518 = inttoptr i64 %f11519 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11518(i64 %arg9753, i64 %arg9752, i64 %retprim9359) ; tail call
  ret void
}


define void @lam10758(i64 %env10759, i64 %_959356, i64 %sjo$_950) {
  %envptr11520 = inttoptr i64 %env10759 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11521 = getelementptr inbounds i64, i64* %envptr11520, i64 3                ; &envptr11520[3]
  %ayU$n = load i64, i64* %envptr11521, align 8                                      ; load; *envptr11521
  %envptr11522 = inttoptr i64 %env10759 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11523 = getelementptr inbounds i64, i64* %envptr11522, i64 2                ; &envptr11522[2]
  %rV9$cc = load i64, i64* %envptr11523, align 8                                     ; load; *envptr11523
  %envptr11524 = inttoptr i64 %env10759 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11525 = getelementptr inbounds i64, i64* %envptr11524, i64 1                ; &envptr11524[1]
  %cont9353 = load i64, i64* %envptr11525, align 8                                   ; load; *envptr11525
  %arg9754 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9258 = call i64 @prim_vector_45ref(i64 %ayU$n, i64 %arg9754)                     ; call prim_vector_45ref
  %arg9756 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %a9259 = call i64 @prim__45(i64 %a9258, i64 %arg9756)                              ; call prim__45
  %arg9759 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9358 = call i64 @prim_vector_45set_33(i64 %ayU$n, i64 %arg9759, i64 %a9259); call prim_vector_45set_33
  %cloptr11526 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11528 = getelementptr inbounds i64, i64* %cloptr11526, i64 1                  ; &eptr11528[1]
  %eptr11529 = getelementptr inbounds i64, i64* %cloptr11526, i64 2                  ; &eptr11529[2]
  store i64 %cont9353, i64* %eptr11528                                               ; *eptr11528 = %cont9353
  store i64 %rV9$cc, i64* %eptr11529                                                 ; *eptr11529 = %rV9$cc
  %eptr11527 = getelementptr inbounds i64, i64* %cloptr11526, i64 0                  ; &cloptr11526[0]
  %f11530 = ptrtoint void(i64,i64,i64)* @lam10753 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11530, i64* %eptr11527                                                 ; store fptr
  %arg9763 = ptrtoint i64* %cloptr11526 to i64                                       ; closure cast; i64* -> i64
  %arg9762 = add i64 0, 0                                                            ; quoted ()
  %cloptr11531 = inttoptr i64 %arg9763 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11532 = getelementptr inbounds i64, i64* %cloptr11531, i64 0                 ; &cloptr11531[0]
  %f11534 = load i64, i64* %i0ptr11532, align 8                                      ; load; *i0ptr11532
  %fptr11533 = inttoptr i64 %f11534 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11533(i64 %arg9763, i64 %arg9762, i64 %retprim9358) ; tail call
  ret void
}


define void @lam10753(i64 %env10754, i64 %_959357, i64 %kOH$_951) {
  %envptr11535 = inttoptr i64 %env10754 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11536 = getelementptr inbounds i64, i64* %envptr11535, i64 2                ; &envptr11535[2]
  %rV9$cc = load i64, i64* %envptr11536, align 8                                     ; load; *envptr11536
  %envptr11537 = inttoptr i64 %env10754 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11538 = getelementptr inbounds i64, i64* %envptr11537, i64 1                ; &envptr11537[1]
  %cont9353 = load i64, i64* %envptr11538, align 8                                   ; load; *envptr11538
  %cloptr11539 = inttoptr i64 %rV9$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr11540 = getelementptr inbounds i64, i64* %cloptr11539, i64 0                 ; &cloptr11539[0]
  %f11542 = load i64, i64* %i0ptr11540, align 8                                      ; load; *i0ptr11540
  %fptr11541 = inttoptr i64 %f11542 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11541(i64 %rV9$cc, i64 %cont9353, i64 %rV9$cc)      ; tail call
  ret void
}


define void @lam10747(i64 %env10748, i64 %_959354, i64 %rV9$cc) {
  %envptr11543 = inttoptr i64 %env10748 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11544 = getelementptr inbounds i64, i64* %envptr11543, i64 3                ; &envptr11543[3]
  %ayU$n = load i64, i64* %envptr11544, align 8                                      ; load; *envptr11544
  %envptr11545 = inttoptr i64 %env10748 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11546 = getelementptr inbounds i64, i64* %envptr11545, i64 2                ; &envptr11545[2]
  %cont9353 = load i64, i64* %envptr11546, align 8                                   ; load; *envptr11546
  %envptr11547 = inttoptr i64 %env10748 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11548 = getelementptr inbounds i64, i64* %envptr11547, i64 1                ; &envptr11547[1]
  %tTX$lst = load i64, i64* %envptr11548, align 8                                    ; load; *envptr11548
  %arg9767 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9254 = call i64 @prim_vector_45ref(i64 %ayU$n, i64 %arg9767)                     ; call prim_vector_45ref
  %arg9770 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9255 = call i64 @prim__61(i64 %arg9770, i64 %a9254)                              ; call prim__61
  %cmp11549 = icmp eq i64 %a9255, 15                                                 ; false?
  br i1 %cmp11549, label %else11551, label %then11550                                ; if

then11550:
  %arg9771 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9355 = call i64 @prim_vector_45ref(i64 %tTX$lst, i64 %arg9771)             ; call prim_vector_45ref
  %arg9774 = add i64 0, 0                                                            ; quoted ()
  %cloptr11552 = inttoptr i64 %cont9353 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11553 = getelementptr inbounds i64, i64* %cloptr11552, i64 0                 ; &cloptr11552[0]
  %f11555 = load i64, i64* %i0ptr11553, align 8                                      ; load; *i0ptr11553
  %fptr11554 = inttoptr i64 %f11555 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11554(i64 %cont9353, i64 %arg9774, i64 %retprim9355); tail call
  ret void

else11551:
  %arg9776 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9256 = call i64 @prim_vector_45ref(i64 %tTX$lst, i64 %arg9776)                   ; call prim_vector_45ref
  %a9257 = call i64 @prim_cdr(i64 %a9256)                                            ; call prim_cdr
  %arg9780 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9359 = call i64 @prim_vector_45set_33(i64 %tTX$lst, i64 %arg9780, i64 %a9257); call prim_vector_45set_33
  %cloptr11556 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11558 = getelementptr inbounds i64, i64* %cloptr11556, i64 1                  ; &eptr11558[1]
  %eptr11559 = getelementptr inbounds i64, i64* %cloptr11556, i64 2                  ; &eptr11559[2]
  %eptr11560 = getelementptr inbounds i64, i64* %cloptr11556, i64 3                  ; &eptr11560[3]
  store i64 %cont9353, i64* %eptr11558                                               ; *eptr11558 = %cont9353
  store i64 %rV9$cc, i64* %eptr11559                                                 ; *eptr11559 = %rV9$cc
  store i64 %ayU$n, i64* %eptr11560                                                  ; *eptr11560 = %ayU$n
  %eptr11557 = getelementptr inbounds i64, i64* %cloptr11556, i64 0                  ; &cloptr11556[0]
  %f11561 = ptrtoint void(i64,i64,i64)* @lam10741 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11561, i64* %eptr11557                                                 ; store fptr
  %arg9784 = ptrtoint i64* %cloptr11556 to i64                                       ; closure cast; i64* -> i64
  %arg9783 = add i64 0, 0                                                            ; quoted ()
  %cloptr11562 = inttoptr i64 %arg9784 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11563 = getelementptr inbounds i64, i64* %cloptr11562, i64 0                 ; &cloptr11562[0]
  %f11565 = load i64, i64* %i0ptr11563, align 8                                      ; load; *i0ptr11563
  %fptr11564 = inttoptr i64 %f11565 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11564(i64 %arg9784, i64 %arg9783, i64 %retprim9359) ; tail call
  ret void
}


define void @lam10741(i64 %env10742, i64 %_959356, i64 %sjo$_950) {
  %envptr11566 = inttoptr i64 %env10742 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11567 = getelementptr inbounds i64, i64* %envptr11566, i64 3                ; &envptr11566[3]
  %ayU$n = load i64, i64* %envptr11567, align 8                                      ; load; *envptr11567
  %envptr11568 = inttoptr i64 %env10742 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11569 = getelementptr inbounds i64, i64* %envptr11568, i64 2                ; &envptr11568[2]
  %rV9$cc = load i64, i64* %envptr11569, align 8                                     ; load; *envptr11569
  %envptr11570 = inttoptr i64 %env10742 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11571 = getelementptr inbounds i64, i64* %envptr11570, i64 1                ; &envptr11570[1]
  %cont9353 = load i64, i64* %envptr11571, align 8                                   ; load; *envptr11571
  %arg9785 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9258 = call i64 @prim_vector_45ref(i64 %ayU$n, i64 %arg9785)                     ; call prim_vector_45ref
  %arg9787 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %a9259 = call i64 @prim__45(i64 %a9258, i64 %arg9787)                              ; call prim__45
  %arg9790 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9358 = call i64 @prim_vector_45set_33(i64 %ayU$n, i64 %arg9790, i64 %a9259); call prim_vector_45set_33
  %cloptr11572 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11574 = getelementptr inbounds i64, i64* %cloptr11572, i64 1                  ; &eptr11574[1]
  %eptr11575 = getelementptr inbounds i64, i64* %cloptr11572, i64 2                  ; &eptr11575[2]
  store i64 %cont9353, i64* %eptr11574                                               ; *eptr11574 = %cont9353
  store i64 %rV9$cc, i64* %eptr11575                                                 ; *eptr11575 = %rV9$cc
  %eptr11573 = getelementptr inbounds i64, i64* %cloptr11572, i64 0                  ; &cloptr11572[0]
  %f11576 = ptrtoint void(i64,i64,i64)* @lam10736 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11576, i64* %eptr11573                                                 ; store fptr
  %arg9794 = ptrtoint i64* %cloptr11572 to i64                                       ; closure cast; i64* -> i64
  %arg9793 = add i64 0, 0                                                            ; quoted ()
  %cloptr11577 = inttoptr i64 %arg9794 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11578 = getelementptr inbounds i64, i64* %cloptr11577, i64 0                 ; &cloptr11577[0]
  %f11580 = load i64, i64* %i0ptr11578, align 8                                      ; load; *i0ptr11578
  %fptr11579 = inttoptr i64 %f11580 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11579(i64 %arg9794, i64 %arg9793, i64 %retprim9358) ; tail call
  ret void
}


define void @lam10736(i64 %env10737, i64 %_959357, i64 %kOH$_951) {
  %envptr11581 = inttoptr i64 %env10737 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11582 = getelementptr inbounds i64, i64* %envptr11581, i64 2                ; &envptr11581[2]
  %rV9$cc = load i64, i64* %envptr11582, align 8                                     ; load; *envptr11582
  %envptr11583 = inttoptr i64 %env10737 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11584 = getelementptr inbounds i64, i64* %envptr11583, i64 1                ; &envptr11583[1]
  %cont9353 = load i64, i64* %envptr11584, align 8                                   ; load; *envptr11584
  %cloptr11585 = inttoptr i64 %rV9$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr11586 = getelementptr inbounds i64, i64* %cloptr11585, i64 0                 ; &cloptr11585[0]
  %f11588 = load i64, i64* %i0ptr11586, align 8                                      ; load; *i0ptr11586
  %fptr11587 = inttoptr i64 %f11588 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11587(i64 %rV9$cc, i64 %cont9353, i64 %rV9$cc)      ; tail call
  ret void
}


define void @lam10730(i64 %env10731, i64 %cont9361, i64 %w7r$v, i64 %rxO$lst) {
  %arg9799 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %mPV$lst = call i64 @prim_make_45vector(i64 %arg9799, i64 %rxO$lst)                ; call prim_make_45vector
  %cloptr11589 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11590 = getelementptr inbounds i64, i64* %cloptr11589, i64 0                  ; &cloptr11589[0]
  %f11591 = ptrtoint void(i64,i64,i64)* @lam10727 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11591, i64* %eptr11590                                                 ; store fptr
  %arg9802 = ptrtoint i64* %cloptr11589 to i64                                       ; closure cast; i64* -> i64
  %cloptr11592 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11594 = getelementptr inbounds i64, i64* %cloptr11592, i64 1                  ; &eptr11594[1]
  %eptr11595 = getelementptr inbounds i64, i64* %cloptr11592, i64 2                  ; &eptr11595[2]
  %eptr11596 = getelementptr inbounds i64, i64* %cloptr11592, i64 3                  ; &eptr11596[3]
  store i64 %w7r$v, i64* %eptr11594                                                  ; *eptr11594 = %w7r$v
  store i64 %mPV$lst, i64* %eptr11595                                                ; *eptr11595 = %mPV$lst
  store i64 %cont9361, i64* %eptr11596                                               ; *eptr11596 = %cont9361
  %eptr11593 = getelementptr inbounds i64, i64* %cloptr11592, i64 0                  ; &cloptr11592[0]
  %f11597 = ptrtoint void(i64,i64,i64)* @lam10725 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11597, i64* %eptr11593                                                 ; store fptr
  %arg9801 = ptrtoint i64* %cloptr11592 to i64                                       ; closure cast; i64* -> i64
  %cloptr11598 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11600 = getelementptr inbounds i64, i64* %cloptr11598, i64 1                  ; &eptr11600[1]
  %eptr11601 = getelementptr inbounds i64, i64* %cloptr11598, i64 2                  ; &eptr11601[2]
  %eptr11602 = getelementptr inbounds i64, i64* %cloptr11598, i64 3                  ; &eptr11602[3]
  store i64 %w7r$v, i64* %eptr11600                                                  ; *eptr11600 = %w7r$v
  store i64 %mPV$lst, i64* %eptr11601                                                ; *eptr11601 = %mPV$lst
  store i64 %cont9361, i64* %eptr11602                                               ; *eptr11602 = %cont9361
  %eptr11599 = getelementptr inbounds i64, i64* %cloptr11598, i64 0                  ; &cloptr11598[0]
  %f11603 = ptrtoint void(i64,i64,i64)* @lam10712 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11603, i64* %eptr11599                                                 ; store fptr
  %arg9800 = ptrtoint i64* %cloptr11598 to i64                                       ; closure cast; i64* -> i64
  %cloptr11604 = inttoptr i64 %arg9802 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11605 = getelementptr inbounds i64, i64* %cloptr11604, i64 0                 ; &cloptr11604[0]
  %f11607 = load i64, i64* %i0ptr11605, align 8                                      ; load; *i0ptr11605
  %fptr11606 = inttoptr i64 %f11607 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11606(i64 %arg9802, i64 %arg9801, i64 %arg9800)     ; tail call
  ret void
}


define void @lam10727(i64 %env10728, i64 %cont9366, i64 %NKo$u) {
  %cloptr11608 = inttoptr i64 %NKo$u to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11609 = getelementptr inbounds i64, i64* %cloptr11608, i64 0                 ; &cloptr11608[0]
  %f11611 = load i64, i64* %i0ptr11609, align 8                                      ; load; *i0ptr11609
  %fptr11610 = inttoptr i64 %f11611 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11610(i64 %NKo$u, i64 %cont9366, i64 %NKo$u)        ; tail call
  ret void
}


define void @lam10725(i64 %env10726, i64 %_959362, i64 %ezS$cc) {
  %envptr11612 = inttoptr i64 %env10726 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11613 = getelementptr inbounds i64, i64* %envptr11612, i64 3                ; &envptr11612[3]
  %cont9361 = load i64, i64* %envptr11613, align 8                                   ; load; *envptr11613
  %envptr11614 = inttoptr i64 %env10726 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11615 = getelementptr inbounds i64, i64* %envptr11614, i64 2                ; &envptr11614[2]
  %mPV$lst = load i64, i64* %envptr11615, align 8                                    ; load; *envptr11615
  %envptr11616 = inttoptr i64 %env10726 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11617 = getelementptr inbounds i64, i64* %envptr11616, i64 1                ; &envptr11616[1]
  %w7r$v = load i64, i64* %envptr11617, align 8                                      ; load; *envptr11617
  %arg9806 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9260 = call i64 @prim_vector_45ref(i64 %mPV$lst, i64 %arg9806)                   ; call prim_vector_45ref
  %a9261 = call i64 @prim_null_63(i64 %a9260)                                        ; call prim_null_63
  %cmp11618 = icmp eq i64 %a9261, 15                                                 ; false?
  br i1 %cmp11618, label %else11620, label %then11619                                ; if

then11619:
  %arg9810 = add i64 0, 0                                                            ; quoted ()
  %arg9809 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr11621 = inttoptr i64 %cont9361 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11622 = getelementptr inbounds i64, i64* %cloptr11621, i64 0                 ; &cloptr11621[0]
  %f11624 = load i64, i64* %i0ptr11622, align 8                                      ; load; *i0ptr11622
  %fptr11623 = inttoptr i64 %f11624 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11623(i64 %cont9361, i64 %arg9810, i64 %arg9809)    ; tail call
  ret void

else11620:
  %arg9812 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9262 = call i64 @prim_vector_45ref(i64 %mPV$lst, i64 %arg9812)                   ; call prim_vector_45ref
  %a9263 = call i64 @prim_car(i64 %a9262)                                            ; call prim_car
  %a9264 = call i64 @prim_eqv_63(i64 %a9263, i64 %w7r$v)                             ; call prim_eqv_63
  %cmp11625 = icmp eq i64 %a9264, 15                                                 ; false?
  br i1 %cmp11625, label %else11627, label %then11626                                ; if

then11626:
  %arg9817 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9363 = call i64 @prim_vector_45ref(i64 %mPV$lst, i64 %arg9817)             ; call prim_vector_45ref
  %arg9820 = add i64 0, 0                                                            ; quoted ()
  %cloptr11628 = inttoptr i64 %cont9361 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11629 = getelementptr inbounds i64, i64* %cloptr11628, i64 0                 ; &cloptr11628[0]
  %f11631 = load i64, i64* %i0ptr11629, align 8                                      ; load; *i0ptr11629
  %fptr11630 = inttoptr i64 %f11631 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11630(i64 %cont9361, i64 %arg9820, i64 %retprim9363); tail call
  ret void

else11627:
  %arg9822 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9265 = call i64 @prim_vector_45ref(i64 %mPV$lst, i64 %arg9822)                   ; call prim_vector_45ref
  %a9266 = call i64 @prim_cdr(i64 %a9265)                                            ; call prim_cdr
  %arg9826 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9365 = call i64 @prim_vector_45set_33(i64 %mPV$lst, i64 %arg9826, i64 %a9266); call prim_vector_45set_33
  %cloptr11632 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11634 = getelementptr inbounds i64, i64* %cloptr11632, i64 1                  ; &eptr11634[1]
  %eptr11635 = getelementptr inbounds i64, i64* %cloptr11632, i64 2                  ; &eptr11635[2]
  store i64 %ezS$cc, i64* %eptr11634                                                 ; *eptr11634 = %ezS$cc
  store i64 %cont9361, i64* %eptr11635                                               ; *eptr11635 = %cont9361
  %eptr11633 = getelementptr inbounds i64, i64* %cloptr11632, i64 0                  ; &cloptr11632[0]
  %f11636 = ptrtoint void(i64,i64,i64)* @lam10719 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11636, i64* %eptr11633                                                 ; store fptr
  %arg9830 = ptrtoint i64* %cloptr11632 to i64                                       ; closure cast; i64* -> i64
  %arg9829 = add i64 0, 0                                                            ; quoted ()
  %cloptr11637 = inttoptr i64 %arg9830 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11638 = getelementptr inbounds i64, i64* %cloptr11637, i64 0                 ; &cloptr11637[0]
  %f11640 = load i64, i64* %i0ptr11638, align 8                                      ; load; *i0ptr11638
  %fptr11639 = inttoptr i64 %f11640 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11639(i64 %arg9830, i64 %arg9829, i64 %retprim9365) ; tail call
  ret void
}


define void @lam10719(i64 %env10720, i64 %_959364, i64 %W6i$_950) {
  %envptr11641 = inttoptr i64 %env10720 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11642 = getelementptr inbounds i64, i64* %envptr11641, i64 2                ; &envptr11641[2]
  %cont9361 = load i64, i64* %envptr11642, align 8                                   ; load; *envptr11642
  %envptr11643 = inttoptr i64 %env10720 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11644 = getelementptr inbounds i64, i64* %envptr11643, i64 1                ; &envptr11643[1]
  %ezS$cc = load i64, i64* %envptr11644, align 8                                     ; load; *envptr11644
  %cloptr11645 = inttoptr i64 %ezS$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr11646 = getelementptr inbounds i64, i64* %cloptr11645, i64 0                 ; &cloptr11645[0]
  %f11648 = load i64, i64* %i0ptr11646, align 8                                      ; load; *i0ptr11646
  %fptr11647 = inttoptr i64 %f11648 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11647(i64 %ezS$cc, i64 %cont9361, i64 %ezS$cc)      ; tail call
  ret void
}


define void @lam10712(i64 %env10713, i64 %_959362, i64 %ezS$cc) {
  %envptr11649 = inttoptr i64 %env10713 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11650 = getelementptr inbounds i64, i64* %envptr11649, i64 3                ; &envptr11649[3]
  %cont9361 = load i64, i64* %envptr11650, align 8                                   ; load; *envptr11650
  %envptr11651 = inttoptr i64 %env10713 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11652 = getelementptr inbounds i64, i64* %envptr11651, i64 2                ; &envptr11651[2]
  %mPV$lst = load i64, i64* %envptr11652, align 8                                    ; load; *envptr11652
  %envptr11653 = inttoptr i64 %env10713 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11654 = getelementptr inbounds i64, i64* %envptr11653, i64 1                ; &envptr11653[1]
  %w7r$v = load i64, i64* %envptr11654, align 8                                      ; load; *envptr11654
  %arg9834 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9260 = call i64 @prim_vector_45ref(i64 %mPV$lst, i64 %arg9834)                   ; call prim_vector_45ref
  %a9261 = call i64 @prim_null_63(i64 %a9260)                                        ; call prim_null_63
  %cmp11655 = icmp eq i64 %a9261, 15                                                 ; false?
  br i1 %cmp11655, label %else11657, label %then11656                                ; if

then11656:
  %arg9838 = add i64 0, 0                                                            ; quoted ()
  %arg9837 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr11658 = inttoptr i64 %cont9361 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11659 = getelementptr inbounds i64, i64* %cloptr11658, i64 0                 ; &cloptr11658[0]
  %f11661 = load i64, i64* %i0ptr11659, align 8                                      ; load; *i0ptr11659
  %fptr11660 = inttoptr i64 %f11661 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11660(i64 %cont9361, i64 %arg9838, i64 %arg9837)    ; tail call
  ret void

else11657:
  %arg9840 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9262 = call i64 @prim_vector_45ref(i64 %mPV$lst, i64 %arg9840)                   ; call prim_vector_45ref
  %a9263 = call i64 @prim_car(i64 %a9262)                                            ; call prim_car
  %a9264 = call i64 @prim_eqv_63(i64 %a9263, i64 %w7r$v)                             ; call prim_eqv_63
  %cmp11662 = icmp eq i64 %a9264, 15                                                 ; false?
  br i1 %cmp11662, label %else11664, label %then11663                                ; if

then11663:
  %arg9845 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9363 = call i64 @prim_vector_45ref(i64 %mPV$lst, i64 %arg9845)             ; call prim_vector_45ref
  %arg9848 = add i64 0, 0                                                            ; quoted ()
  %cloptr11665 = inttoptr i64 %cont9361 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11666 = getelementptr inbounds i64, i64* %cloptr11665, i64 0                 ; &cloptr11665[0]
  %f11668 = load i64, i64* %i0ptr11666, align 8                                      ; load; *i0ptr11666
  %fptr11667 = inttoptr i64 %f11668 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11667(i64 %cont9361, i64 %arg9848, i64 %retprim9363); tail call
  ret void

else11664:
  %arg9850 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9265 = call i64 @prim_vector_45ref(i64 %mPV$lst, i64 %arg9850)                   ; call prim_vector_45ref
  %a9266 = call i64 @prim_cdr(i64 %a9265)                                            ; call prim_cdr
  %arg9854 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9365 = call i64 @prim_vector_45set_33(i64 %mPV$lst, i64 %arg9854, i64 %a9266); call prim_vector_45set_33
  %cloptr11669 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11671 = getelementptr inbounds i64, i64* %cloptr11669, i64 1                  ; &eptr11671[1]
  %eptr11672 = getelementptr inbounds i64, i64* %cloptr11669, i64 2                  ; &eptr11672[2]
  store i64 %ezS$cc, i64* %eptr11671                                                 ; *eptr11671 = %ezS$cc
  store i64 %cont9361, i64* %eptr11672                                               ; *eptr11672 = %cont9361
  %eptr11670 = getelementptr inbounds i64, i64* %cloptr11669, i64 0                  ; &cloptr11669[0]
  %f11673 = ptrtoint void(i64,i64,i64)* @lam10706 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11673, i64* %eptr11670                                                 ; store fptr
  %arg9858 = ptrtoint i64* %cloptr11669 to i64                                       ; closure cast; i64* -> i64
  %arg9857 = add i64 0, 0                                                            ; quoted ()
  %cloptr11674 = inttoptr i64 %arg9858 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11675 = getelementptr inbounds i64, i64* %cloptr11674, i64 0                 ; &cloptr11674[0]
  %f11677 = load i64, i64* %i0ptr11675, align 8                                      ; load; *i0ptr11675
  %fptr11676 = inttoptr i64 %f11677 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11676(i64 %arg9858, i64 %arg9857, i64 %retprim9365) ; tail call
  ret void
}


define void @lam10706(i64 %env10707, i64 %_959364, i64 %W6i$_950) {
  %envptr11678 = inttoptr i64 %env10707 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11679 = getelementptr inbounds i64, i64* %envptr11678, i64 2                ; &envptr11678[2]
  %cont9361 = load i64, i64* %envptr11679, align 8                                   ; load; *envptr11679
  %envptr11680 = inttoptr i64 %env10707 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11681 = getelementptr inbounds i64, i64* %envptr11680, i64 1                ; &envptr11680[1]
  %ezS$cc = load i64, i64* %envptr11681, align 8                                     ; load; *envptr11681
  %cloptr11682 = inttoptr i64 %ezS$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr11683 = getelementptr inbounds i64, i64* %cloptr11682, i64 0                 ; &cloptr11682[0]
  %f11685 = load i64, i64* %i0ptr11683, align 8                                      ; load; *i0ptr11683
  %fptr11684 = inttoptr i64 %f11685 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11684(i64 %ezS$cc, i64 %cont9361, i64 %ezS$cc)      ; tail call
  ret void
}


define void @lam10699(i64 %env10700, i64 %Ebe$args9368) {
  %envptr11686 = inttoptr i64 %env10700 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11687 = getelementptr inbounds i64, i64* %envptr11686, i64 1                ; &envptr11686[1]
  %uc0$_37foldl1 = load i64, i64* %envptr11687, align 8                              ; load; *envptr11687
  %cont9367 = call i64 @prim_car(i64 %Ebe$args9368)                                  ; call prim_car
  %Ebe$args = call i64 @prim_cdr(i64 %Ebe$args9368)                                  ; call prim_cdr
  %a9267 = call i64 @prim_null_63(i64 %Ebe$args)                                     ; call prim_null_63
  %cmp11688 = icmp eq i64 %a9267, 15                                                 ; false?
  br i1 %cmp11688, label %else11690, label %then11689                                ; if

then11689:
  %arg9866 = add i64 0, 0                                                            ; quoted ()
  %arg9865 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %cloptr11691 = inttoptr i64 %cont9367 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11692 = getelementptr inbounds i64, i64* %cloptr11691, i64 0                 ; &cloptr11691[0]
  %f11694 = load i64, i64* %i0ptr11692, align 8                                      ; load; *i0ptr11692
  %fptr11693 = inttoptr i64 %f11694 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11693(i64 %cont9367, i64 %arg9866, i64 %arg9865)    ; tail call
  ret void

else11690:
  %a9268 = call i64 @prim_cdr(i64 %Ebe$args)                                         ; call prim_cdr
  %a9269 = call i64 @prim_null_63(i64 %a9268)                                        ; call prim_null_63
  %cmp11695 = icmp eq i64 %a9269, 15                                                 ; false?
  br i1 %cmp11695, label %else11697, label %then11696                                ; if

then11696:
  %retprim9369 = call i64 @prim_car(i64 %Ebe$args)                                   ; call prim_car
  %arg9872 = add i64 0, 0                                                            ; quoted ()
  %cloptr11698 = inttoptr i64 %cont9367 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11699 = getelementptr inbounds i64, i64* %cloptr11698, i64 0                 ; &cloptr11698[0]
  %f11701 = load i64, i64* %i0ptr11699, align 8                                      ; load; *i0ptr11699
  %fptr11700 = inttoptr i64 %f11701 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11700(i64 %cont9367, i64 %arg9872, i64 %retprim9369); tail call
  ret void

else11697:
  %a9270 = call i64 @prim_car(i64 %Ebe$args)                                         ; call prim_car
  %a9271 = call i64 @prim_cdr(i64 %Ebe$args)                                         ; call prim_cdr
  %cloptr11702 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11703 = getelementptr inbounds i64, i64* %cloptr11702, i64 0                  ; &cloptr11702[0]
  %f11704 = ptrtoint void(i64,i64,i64,i64)* @lam10697 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11704, i64* %eptr11703                                                 ; store fptr
  %arg9878 = ptrtoint i64* %cloptr11702 to i64                                       ; closure cast; i64* -> i64
  %cloptr11705 = inttoptr i64 %uc0$_37foldl1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11706 = getelementptr inbounds i64, i64* %cloptr11705, i64 0                 ; &cloptr11705[0]
  %f11708 = load i64, i64* %i0ptr11706, align 8                                      ; load; *i0ptr11706
  %fptr11707 = inttoptr i64 %f11708 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11707(i64 %uc0$_37foldl1, i64 %cont9367, i64 %arg9878, i64 %a9270, i64 %a9271); tail call
  ret void
}


define void @lam10697(i64 %env10698, i64 %cont9370, i64 %rRq$n, i64 %yEz$v) {
  %retprim9371 = call i64 @prim__47(i64 %yEz$v, i64 %rRq$n)                          ; call prim__47
  %arg9884 = add i64 0, 0                                                            ; quoted ()
  %cloptr11709 = inttoptr i64 %cont9370 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11710 = getelementptr inbounds i64, i64* %cloptr11709, i64 0                 ; &cloptr11709[0]
  %f11712 = load i64, i64* %i0ptr11710, align 8                                      ; load; *i0ptr11710
  %fptr11711 = inttoptr i64 %f11712 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11711(i64 %cont9370, i64 %arg9884, i64 %retprim9371); tail call
  ret void
}


define void @lam10691(i64 %env10692, i64 %cont9372, i64 %Aba$x) {
  %retprim9373 = call i64 @prim_car(i64 %Aba$x)                                      ; call prim_car
  %arg9888 = add i64 0, 0                                                            ; quoted ()
  %cloptr11713 = inttoptr i64 %cont9372 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11714 = getelementptr inbounds i64, i64* %cloptr11713, i64 0                 ; &cloptr11713[0]
  %f11716 = load i64, i64* %i0ptr11714, align 8                                      ; load; *i0ptr11714
  %fptr11715 = inttoptr i64 %f11716 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11715(i64 %cont9372, i64 %arg9888, i64 %retprim9373); tail call
  ret void
}


define void @lam10688(i64 %env10689, i64 %cont9374, i64 %E7v$x) {
  %a9272 = call i64 @prim_cdr(i64 %E7v$x)                                            ; call prim_cdr
  %retprim9375 = call i64 @prim_car(i64 %a9272)                                      ; call prim_car
  %arg9893 = add i64 0, 0                                                            ; quoted ()
  %cloptr11717 = inttoptr i64 %cont9374 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11718 = getelementptr inbounds i64, i64* %cloptr11717, i64 0                 ; &cloptr11717[0]
  %f11720 = load i64, i64* %i0ptr11718, align 8                                      ; load; *i0ptr11718
  %fptr11719 = inttoptr i64 %f11720 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11719(i64 %cont9374, i64 %arg9893, i64 %retprim9375); tail call
  ret void
}


define void @lam10685(i64 %env10686, i64 %cont9376, i64 %QJe$x) {
  %a9273 = call i64 @prim_cdr(i64 %QJe$x)                                            ; call prim_cdr
  %a9274 = call i64 @prim_cdr(i64 %a9273)                                            ; call prim_cdr
  %retprim9377 = call i64 @prim_car(i64 %a9274)                                      ; call prim_car
  %arg9899 = add i64 0, 0                                                            ; quoted ()
  %cloptr11721 = inttoptr i64 %cont9376 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11722 = getelementptr inbounds i64, i64* %cloptr11721, i64 0                 ; &cloptr11721[0]
  %f11724 = load i64, i64* %i0ptr11722, align 8                                      ; load; *i0ptr11722
  %fptr11723 = inttoptr i64 %f11724 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11723(i64 %cont9376, i64 %arg9899, i64 %retprim9377); tail call
  ret void
}


define void @lam10682(i64 %env10683, i64 %cont9378, i64 %a2k$x) {
  %a9275 = call i64 @prim_cdr(i64 %a2k$x)                                            ; call prim_cdr
  %a9276 = call i64 @prim_cdr(i64 %a9275)                                            ; call prim_cdr
  %a9277 = call i64 @prim_cdr(i64 %a9276)                                            ; call prim_cdr
  %retprim9379 = call i64 @prim_car(i64 %a9277)                                      ; call prim_car
  %arg9906 = add i64 0, 0                                                            ; quoted ()
  %cloptr11725 = inttoptr i64 %cont9378 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11726 = getelementptr inbounds i64, i64* %cloptr11725, i64 0                 ; &cloptr11725[0]
  %f11728 = load i64, i64* %i0ptr11726, align 8                                      ; load; *i0ptr11726
  %fptr11727 = inttoptr i64 %f11728 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11727(i64 %cont9378, i64 %arg9906, i64 %retprim9379); tail call
  ret void
}


define void @lam10679(i64 %env10680, i64 %cont9380, i64 %sqF$p) {
  %a9278 = call i64 @prim_cons_63(i64 %sqF$p)                                        ; call prim_cons_63
  %cmp11729 = icmp eq i64 %a9278, 15                                                 ; false?
  br i1 %cmp11729, label %else11731, label %then11730                                ; if

then11730:
  %a9279 = call i64 @prim_car(i64 %sqF$p)                                            ; call prim_car
  %arg9910 = call i64 @const_init_symbol(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @sym11732, i32 0, i32 0)); quoted string
  %retprim9381 = call i64 @prim_eq_63(i64 %a9279, i64 %arg9910)                      ; call prim_eq_63
  %arg9913 = add i64 0, 0                                                            ; quoted ()
  %cloptr11733 = inttoptr i64 %cont9380 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11734 = getelementptr inbounds i64, i64* %cloptr11733, i64 0                 ; &cloptr11733[0]
  %f11736 = load i64, i64* %i0ptr11734, align 8                                      ; load; *i0ptr11734
  %fptr11735 = inttoptr i64 %f11736 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11735(i64 %cont9380, i64 %arg9913, i64 %retprim9381); tail call
  ret void

else11731:
  %arg9916 = add i64 0, 0                                                            ; quoted ()
  %arg9915 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr11737 = inttoptr i64 %cont9380 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11738 = getelementptr inbounds i64, i64* %cloptr11737, i64 0                 ; &cloptr11737[0]
  %f11740 = load i64, i64* %i0ptr11738, align 8                                      ; load; *i0ptr11738
  %fptr11739 = inttoptr i64 %f11740 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11739(i64 %cont9380, i64 %arg9916, i64 %arg9915)    ; tail call
  ret void
}


define void @lam10673(i64 %env10674, i64 %PRF$lst9433) {
  %cont9432 = call i64 @prim_car(i64 %PRF$lst9433)                                   ; call prim_car
  %PRF$lst = call i64 @prim_cdr(i64 %PRF$lst9433)                                    ; call prim_cdr
  %arg9923 = add i64 0, 0                                                            ; quoted ()
  %cloptr11741 = inttoptr i64 %cont9432 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11742 = getelementptr inbounds i64, i64* %cloptr11741, i64 0                 ; &cloptr11741[0]
  %f11744 = load i64, i64* %i0ptr11742, align 8                                      ; load; *i0ptr11742
  %fptr11743 = inttoptr i64 %f11744 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11743(i64 %cont9432, i64 %arg9923, i64 %PRF$lst)    ; tail call
  ret void
}


define void @lam10670(i64 %env10671, i64 %_959382, i64 %S2D$_37raise_45handler) {
  %envptr11745 = inttoptr i64 %env10671 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11746 = getelementptr inbounds i64, i64* %envptr11745, i64 3                ; &envptr11745[3]
  %Q5z$_37_62 = load i64, i64* %envptr11746, align 8                                 ; load; *envptr11746
  %envptr11747 = inttoptr i64 %env10671 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11748 = getelementptr inbounds i64, i64* %envptr11747, i64 2                ; &envptr11747[2]
  %Bv6$_37length = load i64, i64* %envptr11748, align 8                              ; load; *envptr11748
  %envptr11749 = inttoptr i64 %env10671 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11750 = getelementptr inbounds i64, i64* %envptr11749, i64 1                ; &envptr11749[1]
  %I3I$_37drop = load i64, i64* %envptr11750, align 8                                ; load; *envptr11750
  %cloptr11751 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11752 = getelementptr inbounds i64, i64* %cloptr11751, i64 0                  ; &cloptr11751[0]
  %f11753 = ptrtoint void(i64,i64)* @lam10668 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f11753, i64* %eptr11752                                                 ; store fptr
  %arg9926 = ptrtoint i64* %cloptr11751 to i64                                       ; closure cast; i64* -> i64
  %cloptr11754 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11756 = getelementptr inbounds i64, i64* %cloptr11754, i64 1                  ; &eptr11756[1]
  %eptr11757 = getelementptr inbounds i64, i64* %cloptr11754, i64 2                  ; &eptr11757[2]
  %eptr11758 = getelementptr inbounds i64, i64* %cloptr11754, i64 3                  ; &eptr11758[3]
  store i64 %I3I$_37drop, i64* %eptr11756                                            ; *eptr11756 = %I3I$_37drop
  store i64 %Bv6$_37length, i64* %eptr11757                                          ; *eptr11757 = %Bv6$_37length
  store i64 %Q5z$_37_62, i64* %eptr11758                                             ; *eptr11758 = %Q5z$_37_62
  %eptr11755 = getelementptr inbounds i64, i64* %cloptr11754, i64 0                  ; &cloptr11754[0]
  %f11759 = ptrtoint void(i64,i64,i64)* @lam10665 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11759, i64* %eptr11755                                                 ; store fptr
  %arg9925 = ptrtoint i64* %cloptr11754 to i64                                       ; closure cast; i64* -> i64
  %rva10403 = add i64 0, 0                                                           ; quoted ()
  %rva10402 = call i64 @prim_cons(i64 %arg9925, i64 %rva10403)                       ; call prim_cons
  %cloptr11760 = inttoptr i64 %arg9926 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11761 = getelementptr inbounds i64, i64* %cloptr11760, i64 0                 ; &cloptr11760[0]
  %f11763 = load i64, i64* %i0ptr11761, align 8                                      ; load; *i0ptr11761
  %fptr11762 = inttoptr i64 %f11763 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11762(i64 %arg9926, i64 %rva10402)                  ; tail call
  ret void
}


define void @lam10668(i64 %env10669, i64 %XIA$lst9431) {
  %cont9430 = call i64 @prim_car(i64 %XIA$lst9431)                                   ; call prim_car
  %XIA$lst = call i64 @prim_cdr(i64 %XIA$lst9431)                                    ; call prim_cdr
  %arg9930 = add i64 0, 0                                                            ; quoted ()
  %cloptr11764 = inttoptr i64 %cont9430 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11765 = getelementptr inbounds i64, i64* %cloptr11764, i64 0                 ; &cloptr11764[0]
  %f11767 = load i64, i64* %i0ptr11765, align 8                                      ; load; *i0ptr11765
  %fptr11766 = inttoptr i64 %f11767 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11766(i64 %cont9430, i64 %arg9930, i64 %XIA$lst)    ; tail call
  ret void
}


define void @lam10665(i64 %env10666, i64 %_959428, i64 %a9280) {
  %envptr11768 = inttoptr i64 %env10666 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11769 = getelementptr inbounds i64, i64* %envptr11768, i64 3                ; &envptr11768[3]
  %Q5z$_37_62 = load i64, i64* %envptr11769, align 8                                 ; load; *envptr11769
  %envptr11770 = inttoptr i64 %env10666 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11771 = getelementptr inbounds i64, i64* %envptr11770, i64 2                ; &envptr11770[2]
  %Bv6$_37length = load i64, i64* %envptr11771, align 8                              ; load; *envptr11771
  %envptr11772 = inttoptr i64 %env10666 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11773 = getelementptr inbounds i64, i64* %envptr11772, i64 1                ; &envptr11772[1]
  %I3I$_37drop = load i64, i64* %envptr11773, align 8                                ; load; *envptr11773
  %arg9933 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %retprim9429 = call i64 @prim_make_45vector(i64 %arg9933, i64 %a9280)              ; call prim_make_45vector
  %cloptr11774 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11776 = getelementptr inbounds i64, i64* %cloptr11774, i64 1                  ; &eptr11776[1]
  %eptr11777 = getelementptr inbounds i64, i64* %cloptr11774, i64 2                  ; &eptr11777[2]
  %eptr11778 = getelementptr inbounds i64, i64* %cloptr11774, i64 3                  ; &eptr11778[3]
  store i64 %I3I$_37drop, i64* %eptr11776                                            ; *eptr11776 = %I3I$_37drop
  store i64 %Bv6$_37length, i64* %eptr11777                                          ; *eptr11777 = %Bv6$_37length
  store i64 %Q5z$_37_62, i64* %eptr11778                                             ; *eptr11778 = %Q5z$_37_62
  %eptr11775 = getelementptr inbounds i64, i64* %cloptr11774, i64 0                  ; &cloptr11774[0]
  %f11779 = ptrtoint void(i64,i64,i64)* @lam10662 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11779, i64* %eptr11775                                                 ; store fptr
  %arg9936 = ptrtoint i64* %cloptr11774 to i64                                       ; closure cast; i64* -> i64
  %arg9935 = add i64 0, 0                                                            ; quoted ()
  %cloptr11780 = inttoptr i64 %arg9936 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11781 = getelementptr inbounds i64, i64* %cloptr11780, i64 0                 ; &cloptr11780[0]
  %f11783 = load i64, i64* %i0ptr11781, align 8                                      ; load; *i0ptr11781
  %fptr11782 = inttoptr i64 %f11783 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11782(i64 %arg9936, i64 %arg9935, i64 %retprim9429) ; tail call
  ret void
}


define void @lam10662(i64 %env10663, i64 %_959383, i64 %L5S$_37wind_45stack) {
  %envptr11784 = inttoptr i64 %env10663 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11785 = getelementptr inbounds i64, i64* %envptr11784, i64 3                ; &envptr11784[3]
  %Q5z$_37_62 = load i64, i64* %envptr11785, align 8                                 ; load; *envptr11785
  %envptr11786 = inttoptr i64 %env10663 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11787 = getelementptr inbounds i64, i64* %envptr11786, i64 2                ; &envptr11786[2]
  %Bv6$_37length = load i64, i64* %envptr11787, align 8                              ; load; *envptr11787
  %envptr11788 = inttoptr i64 %env10663 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11789 = getelementptr inbounds i64, i64* %envptr11788, i64 1                ; &envptr11788[1]
  %I3I$_37drop = load i64, i64* %envptr11789, align 8                                ; load; *envptr11789
  %cloptr11790 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11792 = getelementptr inbounds i64, i64* %cloptr11790, i64 1                  ; &eptr11792[1]
  %eptr11793 = getelementptr inbounds i64, i64* %cloptr11790, i64 2                  ; &eptr11793[2]
  %eptr11794 = getelementptr inbounds i64, i64* %cloptr11790, i64 3                  ; &eptr11794[3]
  store i64 %I3I$_37drop, i64* %eptr11792                                            ; *eptr11792 = %I3I$_37drop
  store i64 %Bv6$_37length, i64* %eptr11793                                          ; *eptr11793 = %Bv6$_37length
  store i64 %Q5z$_37_62, i64* %eptr11794                                             ; *eptr11794 = %Q5z$_37_62
  %eptr11791 = getelementptr inbounds i64, i64* %cloptr11790, i64 0                  ; &cloptr11790[0]
  %f11795 = ptrtoint void(i64,i64,i64,i64)* @lam10660 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11795, i64* %eptr11791                                                 ; store fptr
  %RUC$_37common_45tail = ptrtoint i64* %cloptr11790 to i64                          ; closure cast; i64* -> i64
  %cloptr11796 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11798 = getelementptr inbounds i64, i64* %cloptr11796, i64 1                  ; &eptr11798[1]
  %eptr11799 = getelementptr inbounds i64, i64* %cloptr11796, i64 2                  ; &eptr11799[2]
  store i64 %L5S$_37wind_45stack, i64* %eptr11798                                    ; *eptr11798 = %L5S$_37wind_45stack
  store i64 %RUC$_37common_45tail, i64* %eptr11799                                   ; *eptr11799 = %RUC$_37common_45tail
  %eptr11797 = getelementptr inbounds i64, i64* %cloptr11796, i64 0                  ; &cloptr11796[0]
  %f11800 = ptrtoint void(i64,i64,i64)* @lam10618 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11800, i64* %eptr11797                                                 ; store fptr
  %TDP$_37do_45wind = ptrtoint i64* %cloptr11796 to i64                              ; closure cast; i64* -> i64
  %cloptr11801 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11802 = getelementptr inbounds i64, i64* %cloptr11801, i64 0                  ; &cloptr11801[0]
  %f11803 = ptrtoint void(i64,i64)* @lam10568 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f11803, i64* %eptr11802                                                 ; store fptr
  %arg10122 = ptrtoint i64* %cloptr11801 to i64                                      ; closure cast; i64* -> i64
  %cloptr11804 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11805 = getelementptr inbounds i64, i64* %cloptr11804, i64 0                  ; &cloptr11804[0]
  %f11806 = ptrtoint void(i64,i64,i64)* @lam10565 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11806, i64* %eptr11805                                                 ; store fptr
  %arg10121 = ptrtoint i64* %cloptr11804 to i64                                      ; closure cast; i64* -> i64
  %rva10401 = add i64 0, 0                                                           ; quoted ()
  %rva10400 = call i64 @prim_cons(i64 %arg10121, i64 %rva10401)                      ; call prim_cons
  %cloptr11807 = inttoptr i64 %arg10122 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11808 = getelementptr inbounds i64, i64* %cloptr11807, i64 0                 ; &cloptr11807[0]
  %f11810 = load i64, i64* %i0ptr11808, align 8                                      ; load; *i0ptr11808
  %fptr11809 = inttoptr i64 %f11810 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11809(i64 %arg10122, i64 %rva10400)                 ; tail call
  ret void
}


define void @lam10660(i64 %env10661, i64 %cont9384, i64 %rI4$x, i64 %KUA$y) {
  %envptr11811 = inttoptr i64 %env10661 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11812 = getelementptr inbounds i64, i64* %envptr11811, i64 3                ; &envptr11811[3]
  %Q5z$_37_62 = load i64, i64* %envptr11812, align 8                                 ; load; *envptr11812
  %envptr11813 = inttoptr i64 %env10661 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11814 = getelementptr inbounds i64, i64* %envptr11813, i64 2                ; &envptr11813[2]
  %Bv6$_37length = load i64, i64* %envptr11814, align 8                              ; load; *envptr11814
  %envptr11815 = inttoptr i64 %env10661 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11816 = getelementptr inbounds i64, i64* %envptr11815, i64 1                ; &envptr11815[1]
  %I3I$_37drop = load i64, i64* %envptr11816, align 8                                ; load; *envptr11816
  %cloptr11817 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr11819 = getelementptr inbounds i64, i64* %cloptr11817, i64 1                  ; &eptr11819[1]
  %eptr11820 = getelementptr inbounds i64, i64* %cloptr11817, i64 2                  ; &eptr11820[2]
  %eptr11821 = getelementptr inbounds i64, i64* %cloptr11817, i64 3                  ; &eptr11821[3]
  %eptr11822 = getelementptr inbounds i64, i64* %cloptr11817, i64 4                  ; &eptr11822[4]
  %eptr11823 = getelementptr inbounds i64, i64* %cloptr11817, i64 5                  ; &eptr11823[5]
  %eptr11824 = getelementptr inbounds i64, i64* %cloptr11817, i64 6                  ; &eptr11824[6]
  store i64 %rI4$x, i64* %eptr11819                                                  ; *eptr11819 = %rI4$x
  store i64 %I3I$_37drop, i64* %eptr11820                                            ; *eptr11820 = %I3I$_37drop
  store i64 %KUA$y, i64* %eptr11821                                                  ; *eptr11821 = %KUA$y
  store i64 %Bv6$_37length, i64* %eptr11822                                          ; *eptr11822 = %Bv6$_37length
  store i64 %cont9384, i64* %eptr11823                                               ; *eptr11823 = %cont9384
  store i64 %Q5z$_37_62, i64* %eptr11824                                             ; *eptr11824 = %Q5z$_37_62
  %eptr11818 = getelementptr inbounds i64, i64* %cloptr11817, i64 0                  ; &cloptr11817[0]
  %f11825 = ptrtoint void(i64,i64,i64)* @lam10658 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11825, i64* %eptr11818                                                 ; store fptr
  %arg9938 = ptrtoint i64* %cloptr11817 to i64                                       ; closure cast; i64* -> i64
  %cloptr11826 = inttoptr i64 %Bv6$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11827 = getelementptr inbounds i64, i64* %cloptr11826, i64 0                 ; &cloptr11826[0]
  %f11829 = load i64, i64* %i0ptr11827, align 8                                      ; load; *i0ptr11827
  %fptr11828 = inttoptr i64 %f11829 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11828(i64 %Bv6$_37length, i64 %arg9938, i64 %rI4$x) ; tail call
  ret void
}


define void @lam10658(i64 %env10659, i64 %_959385, i64 %Sjy$lx) {
  %envptr11830 = inttoptr i64 %env10659 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11831 = getelementptr inbounds i64, i64* %envptr11830, i64 6                ; &envptr11830[6]
  %Q5z$_37_62 = load i64, i64* %envptr11831, align 8                                 ; load; *envptr11831
  %envptr11832 = inttoptr i64 %env10659 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11833 = getelementptr inbounds i64, i64* %envptr11832, i64 5                ; &envptr11832[5]
  %cont9384 = load i64, i64* %envptr11833, align 8                                   ; load; *envptr11833
  %envptr11834 = inttoptr i64 %env10659 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11835 = getelementptr inbounds i64, i64* %envptr11834, i64 4                ; &envptr11834[4]
  %Bv6$_37length = load i64, i64* %envptr11835, align 8                              ; load; *envptr11835
  %envptr11836 = inttoptr i64 %env10659 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11837 = getelementptr inbounds i64, i64* %envptr11836, i64 3                ; &envptr11836[3]
  %KUA$y = load i64, i64* %envptr11837, align 8                                      ; load; *envptr11837
  %envptr11838 = inttoptr i64 %env10659 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11839 = getelementptr inbounds i64, i64* %envptr11838, i64 2                ; &envptr11838[2]
  %I3I$_37drop = load i64, i64* %envptr11839, align 8                                ; load; *envptr11839
  %envptr11840 = inttoptr i64 %env10659 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11841 = getelementptr inbounds i64, i64* %envptr11840, i64 1                ; &envptr11840[1]
  %rI4$x = load i64, i64* %envptr11841, align 8                                      ; load; *envptr11841
  %cloptr11842 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr11844 = getelementptr inbounds i64, i64* %cloptr11842, i64 1                  ; &eptr11844[1]
  %eptr11845 = getelementptr inbounds i64, i64* %cloptr11842, i64 2                  ; &eptr11845[2]
  %eptr11846 = getelementptr inbounds i64, i64* %cloptr11842, i64 3                  ; &eptr11846[3]
  %eptr11847 = getelementptr inbounds i64, i64* %cloptr11842, i64 4                  ; &eptr11847[4]
  %eptr11848 = getelementptr inbounds i64, i64* %cloptr11842, i64 5                  ; &eptr11848[5]
  %eptr11849 = getelementptr inbounds i64, i64* %cloptr11842, i64 6                  ; &eptr11849[6]
  store i64 %rI4$x, i64* %eptr11844                                                  ; *eptr11844 = %rI4$x
  store i64 %I3I$_37drop, i64* %eptr11845                                            ; *eptr11845 = %I3I$_37drop
  store i64 %KUA$y, i64* %eptr11846                                                  ; *eptr11846 = %KUA$y
  store i64 %cont9384, i64* %eptr11847                                               ; *eptr11847 = %cont9384
  store i64 %Sjy$lx, i64* %eptr11848                                                 ; *eptr11848 = %Sjy$lx
  store i64 %Q5z$_37_62, i64* %eptr11849                                             ; *eptr11849 = %Q5z$_37_62
  %eptr11843 = getelementptr inbounds i64, i64* %cloptr11842, i64 0                  ; &cloptr11842[0]
  %f11850 = ptrtoint void(i64,i64,i64)* @lam10656 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11850, i64* %eptr11843                                                 ; store fptr
  %arg9941 = ptrtoint i64* %cloptr11842 to i64                                       ; closure cast; i64* -> i64
  %cloptr11851 = inttoptr i64 %Bv6$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11852 = getelementptr inbounds i64, i64* %cloptr11851, i64 0                 ; &cloptr11851[0]
  %f11854 = load i64, i64* %i0ptr11852, align 8                                      ; load; *i0ptr11852
  %fptr11853 = inttoptr i64 %f11854 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11853(i64 %Bv6$_37length, i64 %arg9941, i64 %KUA$y) ; tail call
  ret void
}


define void @lam10656(i64 %env10657, i64 %_959386, i64 %jhm$ly) {
  %envptr11855 = inttoptr i64 %env10657 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11856 = getelementptr inbounds i64, i64* %envptr11855, i64 6                ; &envptr11855[6]
  %Q5z$_37_62 = load i64, i64* %envptr11856, align 8                                 ; load; *envptr11856
  %envptr11857 = inttoptr i64 %env10657 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11858 = getelementptr inbounds i64, i64* %envptr11857, i64 5                ; &envptr11857[5]
  %Sjy$lx = load i64, i64* %envptr11858, align 8                                     ; load; *envptr11858
  %envptr11859 = inttoptr i64 %env10657 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11860 = getelementptr inbounds i64, i64* %envptr11859, i64 4                ; &envptr11859[4]
  %cont9384 = load i64, i64* %envptr11860, align 8                                   ; load; *envptr11860
  %envptr11861 = inttoptr i64 %env10657 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11862 = getelementptr inbounds i64, i64* %envptr11861, i64 3                ; &envptr11861[3]
  %KUA$y = load i64, i64* %envptr11862, align 8                                      ; load; *envptr11862
  %envptr11863 = inttoptr i64 %env10657 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11864 = getelementptr inbounds i64, i64* %envptr11863, i64 2                ; &envptr11863[2]
  %I3I$_37drop = load i64, i64* %envptr11864, align 8                                ; load; *envptr11864
  %envptr11865 = inttoptr i64 %env10657 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11866 = getelementptr inbounds i64, i64* %envptr11865, i64 1                ; &envptr11865[1]
  %rI4$x = load i64, i64* %envptr11866, align 8                                      ; load; *envptr11866
  %cloptr11867 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11868 = getelementptr inbounds i64, i64* %cloptr11867, i64 0                  ; &cloptr11867[0]
  %f11869 = ptrtoint void(i64,i64)* @lam10654 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f11869, i64* %eptr11868                                                 ; store fptr
  %arg9944 = ptrtoint i64* %cloptr11867 to i64                                       ; closure cast; i64* -> i64
  %cloptr11870 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr11872 = getelementptr inbounds i64, i64* %cloptr11870, i64 1                  ; &eptr11872[1]
  %eptr11873 = getelementptr inbounds i64, i64* %cloptr11870, i64 2                  ; &eptr11873[2]
  %eptr11874 = getelementptr inbounds i64, i64* %cloptr11870, i64 3                  ; &eptr11874[3]
  %eptr11875 = getelementptr inbounds i64, i64* %cloptr11870, i64 4                  ; &eptr11875[4]
  %eptr11876 = getelementptr inbounds i64, i64* %cloptr11870, i64 5                  ; &eptr11876[5]
  %eptr11877 = getelementptr inbounds i64, i64* %cloptr11870, i64 6                  ; &eptr11877[6]
  %eptr11878 = getelementptr inbounds i64, i64* %cloptr11870, i64 7                  ; &eptr11878[7]
  store i64 %rI4$x, i64* %eptr11872                                                  ; *eptr11872 = %rI4$x
  store i64 %I3I$_37drop, i64* %eptr11873                                            ; *eptr11873 = %I3I$_37drop
  store i64 %KUA$y, i64* %eptr11874                                                  ; *eptr11874 = %KUA$y
  store i64 %jhm$ly, i64* %eptr11875                                                 ; *eptr11875 = %jhm$ly
  store i64 %cont9384, i64* %eptr11876                                               ; *eptr11876 = %cont9384
  store i64 %Sjy$lx, i64* %eptr11877                                                 ; *eptr11877 = %Sjy$lx
  store i64 %Q5z$_37_62, i64* %eptr11878                                             ; *eptr11878 = %Q5z$_37_62
  %eptr11871 = getelementptr inbounds i64, i64* %cloptr11870, i64 0                  ; &cloptr11870[0]
  %f11879 = ptrtoint void(i64,i64,i64)* @lam10651 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11879, i64* %eptr11871                                                 ; store fptr
  %arg9943 = ptrtoint i64* %cloptr11870 to i64                                       ; closure cast; i64* -> i64
  %cloptr11880 = inttoptr i64 %arg9944 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11881 = getelementptr inbounds i64, i64* %cloptr11880, i64 0                 ; &cloptr11880[0]
  %f11883 = load i64, i64* %i0ptr11881, align 8                                      ; load; *i0ptr11881
  %fptr11882 = inttoptr i64 %f11883 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11882(i64 %arg9944, i64 %arg9943)                   ; tail call
  ret void
}


define void @lam10654(i64 %env10655, i64 %cFv$lst9396) {
  %cont9395 = call i64 @prim_car(i64 %cFv$lst9396)                                   ; call prim_car
  %cFv$lst = call i64 @prim_cdr(i64 %cFv$lst9396)                                    ; call prim_cdr
  %arg9948 = add i64 0, 0                                                            ; quoted ()
  %cloptr11884 = inttoptr i64 %cont9395 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11885 = getelementptr inbounds i64, i64* %cloptr11884, i64 0                 ; &cloptr11884[0]
  %f11887 = load i64, i64* %i0ptr11885, align 8                                      ; load; *i0ptr11885
  %fptr11886 = inttoptr i64 %f11887 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11886(i64 %cont9395, i64 %arg9948, i64 %cFv$lst)    ; tail call
  ret void
}


define void @lam10651(i64 %env10652, i64 %_959393, i64 %a9281) {
  %envptr11888 = inttoptr i64 %env10652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11889 = getelementptr inbounds i64, i64* %envptr11888, i64 7                ; &envptr11888[7]
  %Q5z$_37_62 = load i64, i64* %envptr11889, align 8                                 ; load; *envptr11889
  %envptr11890 = inttoptr i64 %env10652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11891 = getelementptr inbounds i64, i64* %envptr11890, i64 6                ; &envptr11890[6]
  %Sjy$lx = load i64, i64* %envptr11891, align 8                                     ; load; *envptr11891
  %envptr11892 = inttoptr i64 %env10652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11893 = getelementptr inbounds i64, i64* %envptr11892, i64 5                ; &envptr11892[5]
  %cont9384 = load i64, i64* %envptr11893, align 8                                   ; load; *envptr11893
  %envptr11894 = inttoptr i64 %env10652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11895 = getelementptr inbounds i64, i64* %envptr11894, i64 4                ; &envptr11894[4]
  %jhm$ly = load i64, i64* %envptr11895, align 8                                     ; load; *envptr11895
  %envptr11896 = inttoptr i64 %env10652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11897 = getelementptr inbounds i64, i64* %envptr11896, i64 3                ; &envptr11896[3]
  %KUA$y = load i64, i64* %envptr11897, align 8                                      ; load; *envptr11897
  %envptr11898 = inttoptr i64 %env10652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11899 = getelementptr inbounds i64, i64* %envptr11898, i64 2                ; &envptr11898[2]
  %I3I$_37drop = load i64, i64* %envptr11899, align 8                                ; load; *envptr11899
  %envptr11900 = inttoptr i64 %env10652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11901 = getelementptr inbounds i64, i64* %envptr11900, i64 1                ; &envptr11900[1]
  %rI4$x = load i64, i64* %envptr11901, align 8                                      ; load; *envptr11901
  %arg9951 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %retprim9394 = call i64 @prim_make_45vector(i64 %arg9951, i64 %a9281)              ; call prim_make_45vector
  %cloptr11902 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr11904 = getelementptr inbounds i64, i64* %cloptr11902, i64 1                  ; &eptr11904[1]
  %eptr11905 = getelementptr inbounds i64, i64* %cloptr11902, i64 2                  ; &eptr11905[2]
  %eptr11906 = getelementptr inbounds i64, i64* %cloptr11902, i64 3                  ; &eptr11906[3]
  %eptr11907 = getelementptr inbounds i64, i64* %cloptr11902, i64 4                  ; &eptr11907[4]
  %eptr11908 = getelementptr inbounds i64, i64* %cloptr11902, i64 5                  ; &eptr11908[5]
  %eptr11909 = getelementptr inbounds i64, i64* %cloptr11902, i64 6                  ; &eptr11909[6]
  %eptr11910 = getelementptr inbounds i64, i64* %cloptr11902, i64 7                  ; &eptr11910[7]
  store i64 %rI4$x, i64* %eptr11904                                                  ; *eptr11904 = %rI4$x
  store i64 %I3I$_37drop, i64* %eptr11905                                            ; *eptr11905 = %I3I$_37drop
  store i64 %KUA$y, i64* %eptr11906                                                  ; *eptr11906 = %KUA$y
  store i64 %jhm$ly, i64* %eptr11907                                                 ; *eptr11907 = %jhm$ly
  store i64 %cont9384, i64* %eptr11908                                               ; *eptr11908 = %cont9384
  store i64 %Sjy$lx, i64* %eptr11909                                                 ; *eptr11909 = %Sjy$lx
  store i64 %Q5z$_37_62, i64* %eptr11910                                             ; *eptr11910 = %Q5z$_37_62
  %eptr11903 = getelementptr inbounds i64, i64* %cloptr11902, i64 0                  ; &cloptr11902[0]
  %f11911 = ptrtoint void(i64,i64,i64)* @lam10648 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11911, i64* %eptr11903                                                 ; store fptr
  %arg9954 = ptrtoint i64* %cloptr11902 to i64                                       ; closure cast; i64* -> i64
  %arg9953 = add i64 0, 0                                                            ; quoted ()
  %cloptr11912 = inttoptr i64 %arg9954 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11913 = getelementptr inbounds i64, i64* %cloptr11912, i64 0                 ; &cloptr11912[0]
  %f11915 = load i64, i64* %i0ptr11913, align 8                                      ; load; *i0ptr11913
  %fptr11914 = inttoptr i64 %f11915 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11914(i64 %arg9954, i64 %arg9953, i64 %retprim9394) ; tail call
  ret void
}


define void @lam10648(i64 %env10649, i64 %_959387, i64 %BYa$loop) {
  %envptr11916 = inttoptr i64 %env10649 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11917 = getelementptr inbounds i64, i64* %envptr11916, i64 7                ; &envptr11916[7]
  %Q5z$_37_62 = load i64, i64* %envptr11917, align 8                                 ; load; *envptr11917
  %envptr11918 = inttoptr i64 %env10649 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11919 = getelementptr inbounds i64, i64* %envptr11918, i64 6                ; &envptr11918[6]
  %Sjy$lx = load i64, i64* %envptr11919, align 8                                     ; load; *envptr11919
  %envptr11920 = inttoptr i64 %env10649 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11921 = getelementptr inbounds i64, i64* %envptr11920, i64 5                ; &envptr11920[5]
  %cont9384 = load i64, i64* %envptr11921, align 8                                   ; load; *envptr11921
  %envptr11922 = inttoptr i64 %env10649 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11923 = getelementptr inbounds i64, i64* %envptr11922, i64 4                ; &envptr11922[4]
  %jhm$ly = load i64, i64* %envptr11923, align 8                                     ; load; *envptr11923
  %envptr11924 = inttoptr i64 %env10649 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11925 = getelementptr inbounds i64, i64* %envptr11924, i64 3                ; &envptr11924[3]
  %KUA$y = load i64, i64* %envptr11925, align 8                                      ; load; *envptr11925
  %envptr11926 = inttoptr i64 %env10649 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11927 = getelementptr inbounds i64, i64* %envptr11926, i64 2                ; &envptr11926[2]
  %I3I$_37drop = load i64, i64* %envptr11927, align 8                                ; load; *envptr11927
  %envptr11928 = inttoptr i64 %env10649 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11929 = getelementptr inbounds i64, i64* %envptr11928, i64 1                ; &envptr11928[1]
  %rI4$x = load i64, i64* %envptr11929, align 8                                      ; load; *envptr11929
  %arg9956 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr11930 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11932 = getelementptr inbounds i64, i64* %cloptr11930, i64 1                  ; &eptr11932[1]
  store i64 %BYa$loop, i64* %eptr11932                                               ; *eptr11932 = %BYa$loop
  %eptr11931 = getelementptr inbounds i64, i64* %cloptr11930, i64 0                  ; &cloptr11930[0]
  %f11933 = ptrtoint void(i64,i64,i64,i64)* @lam10645 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f11933, i64* %eptr11931                                                 ; store fptr
  %arg9955 = ptrtoint i64* %cloptr11930 to i64                                       ; closure cast; i64* -> i64
  %lG7$_959181 = call i64 @prim_vector_45set_33(i64 %BYa$loop, i64 %arg9956, i64 %arg9955); call prim_vector_45set_33
  %arg9971 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9286 = call i64 @prim_vector_45ref(i64 %BYa$loop, i64 %arg9971)                  ; call prim_vector_45ref
  %cloptr11934 = call i64* @alloc(i64 72)                                            ; malloc
  %eptr11936 = getelementptr inbounds i64, i64* %cloptr11934, i64 1                  ; &eptr11936[1]
  %eptr11937 = getelementptr inbounds i64, i64* %cloptr11934, i64 2                  ; &eptr11937[2]
  %eptr11938 = getelementptr inbounds i64, i64* %cloptr11934, i64 3                  ; &eptr11938[3]
  %eptr11939 = getelementptr inbounds i64, i64* %cloptr11934, i64 4                  ; &eptr11939[4]
  %eptr11940 = getelementptr inbounds i64, i64* %cloptr11934, i64 5                  ; &eptr11940[5]
  %eptr11941 = getelementptr inbounds i64, i64* %cloptr11934, i64 6                  ; &eptr11941[6]
  %eptr11942 = getelementptr inbounds i64, i64* %cloptr11934, i64 7                  ; &eptr11942[7]
  %eptr11943 = getelementptr inbounds i64, i64* %cloptr11934, i64 8                  ; &eptr11943[8]
  store i64 %rI4$x, i64* %eptr11936                                                  ; *eptr11936 = %rI4$x
  store i64 %I3I$_37drop, i64* %eptr11937                                            ; *eptr11937 = %I3I$_37drop
  store i64 %KUA$y, i64* %eptr11938                                                  ; *eptr11938 = %KUA$y
  store i64 %a9286, i64* %eptr11939                                                  ; *eptr11939 = %a9286
  store i64 %jhm$ly, i64* %eptr11940                                                 ; *eptr11940 = %jhm$ly
  store i64 %cont9384, i64* %eptr11941                                               ; *eptr11941 = %cont9384
  store i64 %Sjy$lx, i64* %eptr11942                                                 ; *eptr11942 = %Sjy$lx
  store i64 %Q5z$_37_62, i64* %eptr11943                                             ; *eptr11943 = %Q5z$_37_62
  %eptr11935 = getelementptr inbounds i64, i64* %cloptr11934, i64 0                  ; &cloptr11934[0]
  %f11944 = ptrtoint void(i64,i64,i64)* @lam10640 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11944, i64* %eptr11935                                                 ; store fptr
  %arg9975 = ptrtoint i64* %cloptr11934 to i64                                       ; closure cast; i64* -> i64
  %cloptr11945 = inttoptr i64 %Q5z$_37_62 to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr11946 = getelementptr inbounds i64, i64* %cloptr11945, i64 0                 ; &cloptr11945[0]
  %f11948 = load i64, i64* %i0ptr11946, align 8                                      ; load; *i0ptr11946
  %fptr11947 = inttoptr i64 %f11948 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11947(i64 %Q5z$_37_62, i64 %arg9975, i64 %Sjy$lx, i64 %jhm$ly); tail call
  ret void
}


define void @lam10645(i64 %env10646, i64 %cont9388, i64 %isA$x, i64 %xY8$y) {
  %envptr11949 = inttoptr i64 %env10646 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11950 = getelementptr inbounds i64, i64* %envptr11949, i64 1                ; &envptr11949[1]
  %BYa$loop = load i64, i64* %envptr11950, align 8                                   ; load; *envptr11950
  %a9282 = call i64 @prim_eq_63(i64 %isA$x, i64 %xY8$y)                              ; call prim_eq_63
  %cmp11951 = icmp eq i64 %a9282, 15                                                 ; false?
  br i1 %cmp11951, label %else11953, label %then11952                                ; if

then11952:
  %arg9961 = add i64 0, 0                                                            ; quoted ()
  %cloptr11954 = inttoptr i64 %cont9388 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11955 = getelementptr inbounds i64, i64* %cloptr11954, i64 0                 ; &cloptr11954[0]
  %f11957 = load i64, i64* %i0ptr11955, align 8                                      ; load; *i0ptr11955
  %fptr11956 = inttoptr i64 %f11957 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11956(i64 %cont9388, i64 %arg9961, i64 %isA$x)      ; tail call
  ret void

else11953:
  %arg9963 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9283 = call i64 @prim_vector_45ref(i64 %BYa$loop, i64 %arg9963)                  ; call prim_vector_45ref
  %a9284 = call i64 @prim_cdr(i64 %isA$x)                                            ; call prim_cdr
  %a9285 = call i64 @prim_cdr(i64 %xY8$y)                                            ; call prim_cdr
  %cloptr11958 = inttoptr i64 %a9283 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11959 = getelementptr inbounds i64, i64* %cloptr11958, i64 0                 ; &cloptr11958[0]
  %f11961 = load i64, i64* %i0ptr11959, align 8                                      ; load; *i0ptr11959
  %fptr11960 = inttoptr i64 %f11961 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11960(i64 %a9283, i64 %cont9388, i64 %a9284, i64 %a9285); tail call
  ret void
}


define void @lam10640(i64 %env10641, i64 %_959389, i64 %a9287) {
  %envptr11962 = inttoptr i64 %env10641 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11963 = getelementptr inbounds i64, i64* %envptr11962, i64 8                ; &envptr11962[8]
  %Q5z$_37_62 = load i64, i64* %envptr11963, align 8                                 ; load; *envptr11963
  %envptr11964 = inttoptr i64 %env10641 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11965 = getelementptr inbounds i64, i64* %envptr11964, i64 7                ; &envptr11964[7]
  %Sjy$lx = load i64, i64* %envptr11965, align 8                                     ; load; *envptr11965
  %envptr11966 = inttoptr i64 %env10641 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11967 = getelementptr inbounds i64, i64* %envptr11966, i64 6                ; &envptr11966[6]
  %cont9384 = load i64, i64* %envptr11967, align 8                                   ; load; *envptr11967
  %envptr11968 = inttoptr i64 %env10641 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11969 = getelementptr inbounds i64, i64* %envptr11968, i64 5                ; &envptr11968[5]
  %jhm$ly = load i64, i64* %envptr11969, align 8                                     ; load; *envptr11969
  %envptr11970 = inttoptr i64 %env10641 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11971 = getelementptr inbounds i64, i64* %envptr11970, i64 4                ; &envptr11970[4]
  %a9286 = load i64, i64* %envptr11971, align 8                                      ; load; *envptr11971
  %envptr11972 = inttoptr i64 %env10641 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11973 = getelementptr inbounds i64, i64* %envptr11972, i64 3                ; &envptr11972[3]
  %KUA$y = load i64, i64* %envptr11973, align 8                                      ; load; *envptr11973
  %envptr11974 = inttoptr i64 %env10641 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11975 = getelementptr inbounds i64, i64* %envptr11974, i64 2                ; &envptr11974[2]
  %I3I$_37drop = load i64, i64* %envptr11975, align 8                                ; load; *envptr11975
  %envptr11976 = inttoptr i64 %env10641 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr11977 = getelementptr inbounds i64, i64* %envptr11976, i64 1                ; &envptr11976[1]
  %rI4$x = load i64, i64* %envptr11977, align 8                                      ; load; *envptr11977
  %cmp11978 = icmp eq i64 %a9287, 15                                                 ; false?
  br i1 %cmp11978, label %else11980, label %then11979                                ; if

then11979:
  %a9288 = call i64 @prim__45(i64 %Sjy$lx, i64 %jhm$ly)                              ; call prim__45
  %cloptr11981 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr11983 = getelementptr inbounds i64, i64* %cloptr11981, i64 1                  ; &eptr11983[1]
  %eptr11984 = getelementptr inbounds i64, i64* %cloptr11981, i64 2                  ; &eptr11984[2]
  %eptr11985 = getelementptr inbounds i64, i64* %cloptr11981, i64 3                  ; &eptr11985[3]
  %eptr11986 = getelementptr inbounds i64, i64* %cloptr11981, i64 4                  ; &eptr11986[4]
  %eptr11987 = getelementptr inbounds i64, i64* %cloptr11981, i64 5                  ; &eptr11987[5]
  %eptr11988 = getelementptr inbounds i64, i64* %cloptr11981, i64 6                  ; &eptr11988[6]
  %eptr11989 = getelementptr inbounds i64, i64* %cloptr11981, i64 7                  ; &eptr11989[7]
  store i64 %I3I$_37drop, i64* %eptr11983                                            ; *eptr11983 = %I3I$_37drop
  store i64 %KUA$y, i64* %eptr11984                                                  ; *eptr11984 = %KUA$y
  store i64 %a9286, i64* %eptr11985                                                  ; *eptr11985 = %a9286
  store i64 %jhm$ly, i64* %eptr11986                                                 ; *eptr11986 = %jhm$ly
  store i64 %cont9384, i64* %eptr11987                                               ; *eptr11987 = %cont9384
  store i64 %Sjy$lx, i64* %eptr11988                                                 ; *eptr11988 = %Sjy$lx
  store i64 %Q5z$_37_62, i64* %eptr11989                                             ; *eptr11989 = %Q5z$_37_62
  %eptr11982 = getelementptr inbounds i64, i64* %cloptr11981, i64 0                  ; &cloptr11981[0]
  %f11990 = ptrtoint void(i64,i64,i64)* @lam10628 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f11990, i64* %eptr11982                                                 ; store fptr
  %arg9981 = ptrtoint i64* %cloptr11981 to i64                                       ; closure cast; i64* -> i64
  %cloptr11991 = inttoptr i64 %I3I$_37drop to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr11992 = getelementptr inbounds i64, i64* %cloptr11991, i64 0                 ; &cloptr11991[0]
  %f11994 = load i64, i64* %i0ptr11992, align 8                                      ; load; *i0ptr11992
  %fptr11993 = inttoptr i64 %f11994 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11993(i64 %I3I$_37drop, i64 %arg9981, i64 %rI4$x, i64 %a9288); tail call
  ret void

else11980:
  %cloptr11995 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr11997 = getelementptr inbounds i64, i64* %cloptr11995, i64 1                  ; &eptr11997[1]
  %eptr11998 = getelementptr inbounds i64, i64* %cloptr11995, i64 2                  ; &eptr11998[2]
  %eptr11999 = getelementptr inbounds i64, i64* %cloptr11995, i64 3                  ; &eptr11999[3]
  %eptr12000 = getelementptr inbounds i64, i64* %cloptr11995, i64 4                  ; &eptr12000[4]
  %eptr12001 = getelementptr inbounds i64, i64* %cloptr11995, i64 5                  ; &eptr12001[5]
  %eptr12002 = getelementptr inbounds i64, i64* %cloptr11995, i64 6                  ; &eptr12002[6]
  %eptr12003 = getelementptr inbounds i64, i64* %cloptr11995, i64 7                  ; &eptr12003[7]
  store i64 %I3I$_37drop, i64* %eptr11997                                            ; *eptr11997 = %I3I$_37drop
  store i64 %KUA$y, i64* %eptr11998                                                  ; *eptr11998 = %KUA$y
  store i64 %a9286, i64* %eptr11999                                                  ; *eptr11999 = %a9286
  store i64 %jhm$ly, i64* %eptr12000                                                 ; *eptr12000 = %jhm$ly
  store i64 %cont9384, i64* %eptr12001                                               ; *eptr12001 = %cont9384
  store i64 %Sjy$lx, i64* %eptr12002                                                 ; *eptr12002 = %Sjy$lx
  store i64 %Q5z$_37_62, i64* %eptr12003                                             ; *eptr12003 = %Q5z$_37_62
  %eptr11996 = getelementptr inbounds i64, i64* %cloptr11995, i64 0                  ; &cloptr11995[0]
  %f12004 = ptrtoint void(i64,i64,i64)* @lam10638 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12004, i64* %eptr11996                                                 ; store fptr
  %arg10006 = ptrtoint i64* %cloptr11995 to i64                                      ; closure cast; i64* -> i64
  %arg10005 = add i64 0, 0                                                           ; quoted ()
  %cloptr12005 = inttoptr i64 %arg10006 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12006 = getelementptr inbounds i64, i64* %cloptr12005, i64 0                 ; &cloptr12005[0]
  %f12008 = load i64, i64* %i0ptr12006, align 8                                      ; load; *i0ptr12006
  %fptr12007 = inttoptr i64 %f12008 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12007(i64 %arg10006, i64 %arg10005, i64 %rI4$x)     ; tail call
  ret void
}


define void @lam10638(i64 %env10639, i64 %_959390, i64 %a9289) {
  %envptr12009 = inttoptr i64 %env10639 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12010 = getelementptr inbounds i64, i64* %envptr12009, i64 7                ; &envptr12009[7]
  %Q5z$_37_62 = load i64, i64* %envptr12010, align 8                                 ; load; *envptr12010
  %envptr12011 = inttoptr i64 %env10639 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12012 = getelementptr inbounds i64, i64* %envptr12011, i64 6                ; &envptr12011[6]
  %Sjy$lx = load i64, i64* %envptr12012, align 8                                     ; load; *envptr12012
  %envptr12013 = inttoptr i64 %env10639 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12014 = getelementptr inbounds i64, i64* %envptr12013, i64 5                ; &envptr12013[5]
  %cont9384 = load i64, i64* %envptr12014, align 8                                   ; load; *envptr12014
  %envptr12015 = inttoptr i64 %env10639 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12016 = getelementptr inbounds i64, i64* %envptr12015, i64 4                ; &envptr12015[4]
  %jhm$ly = load i64, i64* %envptr12016, align 8                                     ; load; *envptr12016
  %envptr12017 = inttoptr i64 %env10639 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12018 = getelementptr inbounds i64, i64* %envptr12017, i64 3                ; &envptr12017[3]
  %a9286 = load i64, i64* %envptr12018, align 8                                      ; load; *envptr12018
  %envptr12019 = inttoptr i64 %env10639 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12020 = getelementptr inbounds i64, i64* %envptr12019, i64 2                ; &envptr12019[2]
  %KUA$y = load i64, i64* %envptr12020, align 8                                      ; load; *envptr12020
  %envptr12021 = inttoptr i64 %env10639 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12022 = getelementptr inbounds i64, i64* %envptr12021, i64 1                ; &envptr12021[1]
  %I3I$_37drop = load i64, i64* %envptr12022, align 8                                ; load; *envptr12022
  %cloptr12023 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr12025 = getelementptr inbounds i64, i64* %cloptr12023, i64 1                  ; &eptr12025[1]
  %eptr12026 = getelementptr inbounds i64, i64* %cloptr12023, i64 2                  ; &eptr12026[2]
  %eptr12027 = getelementptr inbounds i64, i64* %cloptr12023, i64 3                  ; &eptr12027[3]
  %eptr12028 = getelementptr inbounds i64, i64* %cloptr12023, i64 4                  ; &eptr12028[4]
  %eptr12029 = getelementptr inbounds i64, i64* %cloptr12023, i64 5                  ; &eptr12029[5]
  %eptr12030 = getelementptr inbounds i64, i64* %cloptr12023, i64 6                  ; &eptr12030[6]
  %eptr12031 = getelementptr inbounds i64, i64* %cloptr12023, i64 7                  ; &eptr12031[7]
  store i64 %I3I$_37drop, i64* %eptr12025                                            ; *eptr12025 = %I3I$_37drop
  store i64 %KUA$y, i64* %eptr12026                                                  ; *eptr12026 = %KUA$y
  store i64 %a9286, i64* %eptr12027                                                  ; *eptr12027 = %a9286
  store i64 %jhm$ly, i64* %eptr12028                                                 ; *eptr12028 = %jhm$ly
  store i64 %cont9384, i64* %eptr12029                                               ; *eptr12029 = %cont9384
  store i64 %Sjy$lx, i64* %eptr12030                                                 ; *eptr12030 = %Sjy$lx
  store i64 %a9289, i64* %eptr12031                                                  ; *eptr12031 = %a9289
  %eptr12024 = getelementptr inbounds i64, i64* %cloptr12023, i64 0                  ; &cloptr12023[0]
  %f12032 = ptrtoint void(i64,i64,i64)* @lam10636 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12032, i64* %eptr12024                                                 ; store fptr
  %arg10009 = ptrtoint i64* %cloptr12023 to i64                                      ; closure cast; i64* -> i64
  %cloptr12033 = inttoptr i64 %Q5z$_37_62 to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr12034 = getelementptr inbounds i64, i64* %cloptr12033, i64 0                 ; &cloptr12033[0]
  %f12036 = load i64, i64* %i0ptr12034, align 8                                      ; load; *i0ptr12034
  %fptr12035 = inttoptr i64 %f12036 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12035(i64 %Q5z$_37_62, i64 %arg10009, i64 %jhm$ly, i64 %Sjy$lx); tail call
  ret void
}


define void @lam10636(i64 %env10637, i64 %_959391, i64 %a9290) {
  %envptr12037 = inttoptr i64 %env10637 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12038 = getelementptr inbounds i64, i64* %envptr12037, i64 7                ; &envptr12037[7]
  %a9289 = load i64, i64* %envptr12038, align 8                                      ; load; *envptr12038
  %envptr12039 = inttoptr i64 %env10637 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12040 = getelementptr inbounds i64, i64* %envptr12039, i64 6                ; &envptr12039[6]
  %Sjy$lx = load i64, i64* %envptr12040, align 8                                     ; load; *envptr12040
  %envptr12041 = inttoptr i64 %env10637 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12042 = getelementptr inbounds i64, i64* %envptr12041, i64 5                ; &envptr12041[5]
  %cont9384 = load i64, i64* %envptr12042, align 8                                   ; load; *envptr12042
  %envptr12043 = inttoptr i64 %env10637 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12044 = getelementptr inbounds i64, i64* %envptr12043, i64 4                ; &envptr12043[4]
  %jhm$ly = load i64, i64* %envptr12044, align 8                                     ; load; *envptr12044
  %envptr12045 = inttoptr i64 %env10637 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12046 = getelementptr inbounds i64, i64* %envptr12045, i64 3                ; &envptr12045[3]
  %a9286 = load i64, i64* %envptr12046, align 8                                      ; load; *envptr12046
  %envptr12047 = inttoptr i64 %env10637 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12048 = getelementptr inbounds i64, i64* %envptr12047, i64 2                ; &envptr12047[2]
  %KUA$y = load i64, i64* %envptr12048, align 8                                      ; load; *envptr12048
  %envptr12049 = inttoptr i64 %env10637 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12050 = getelementptr inbounds i64, i64* %envptr12049, i64 1                ; &envptr12049[1]
  %I3I$_37drop = load i64, i64* %envptr12050, align 8                                ; load; *envptr12050
  %cmp12051 = icmp eq i64 %a9290, 15                                                 ; false?
  br i1 %cmp12051, label %else12053, label %then12052                                ; if

then12052:
  %a9291 = call i64 @prim__45(i64 %jhm$ly, i64 %Sjy$lx)                              ; call prim__45
  %cloptr12054 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12056 = getelementptr inbounds i64, i64* %cloptr12054, i64 1                  ; &eptr12056[1]
  %eptr12057 = getelementptr inbounds i64, i64* %cloptr12054, i64 2                  ; &eptr12057[2]
  %eptr12058 = getelementptr inbounds i64, i64* %cloptr12054, i64 3                  ; &eptr12058[3]
  store i64 %a9286, i64* %eptr12056                                                  ; *eptr12056 = %a9286
  store i64 %cont9384, i64* %eptr12057                                               ; *eptr12057 = %cont9384
  store i64 %a9289, i64* %eptr12058                                                  ; *eptr12058 = %a9289
  %eptr12055 = getelementptr inbounds i64, i64* %cloptr12054, i64 0                  ; &cloptr12054[0]
  %f12059 = ptrtoint void(i64,i64,i64)* @lam10631 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12059, i64* %eptr12055                                                 ; store fptr
  %arg10015 = ptrtoint i64* %cloptr12054 to i64                                      ; closure cast; i64* -> i64
  %cloptr12060 = inttoptr i64 %I3I$_37drop to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr12061 = getelementptr inbounds i64, i64* %cloptr12060, i64 0                 ; &cloptr12060[0]
  %f12063 = load i64, i64* %i0ptr12061, align 8                                      ; load; *i0ptr12061
  %fptr12062 = inttoptr i64 %f12063 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12062(i64 %I3I$_37drop, i64 %arg10015, i64 %KUA$y, i64 %a9291); tail call
  ret void

else12053:
  %cloptr12064 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12066 = getelementptr inbounds i64, i64* %cloptr12064, i64 1                  ; &eptr12066[1]
  %eptr12067 = getelementptr inbounds i64, i64* %cloptr12064, i64 2                  ; &eptr12067[2]
  %eptr12068 = getelementptr inbounds i64, i64* %cloptr12064, i64 3                  ; &eptr12068[3]
  store i64 %a9286, i64* %eptr12066                                                  ; *eptr12066 = %a9286
  store i64 %cont9384, i64* %eptr12067                                               ; *eptr12067 = %cont9384
  store i64 %a9289, i64* %eptr12068                                                  ; *eptr12068 = %a9289
  %eptr12065 = getelementptr inbounds i64, i64* %cloptr12064, i64 0                  ; &cloptr12064[0]
  %f12069 = ptrtoint void(i64,i64,i64)* @lam10634 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12069, i64* %eptr12065                                                 ; store fptr
  %arg10023 = ptrtoint i64* %cloptr12064 to i64                                      ; closure cast; i64* -> i64
  %arg10022 = add i64 0, 0                                                           ; quoted ()
  %cloptr12070 = inttoptr i64 %arg10023 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12071 = getelementptr inbounds i64, i64* %cloptr12070, i64 0                 ; &cloptr12070[0]
  %f12073 = load i64, i64* %i0ptr12071, align 8                                      ; load; *i0ptr12071
  %fptr12072 = inttoptr i64 %f12073 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12072(i64 %arg10023, i64 %arg10022, i64 %KUA$y)     ; tail call
  ret void
}


define void @lam10634(i64 %env10635, i64 %_959392, i64 %a9292) {
  %envptr12074 = inttoptr i64 %env10635 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12075 = getelementptr inbounds i64, i64* %envptr12074, i64 3                ; &envptr12074[3]
  %a9289 = load i64, i64* %envptr12075, align 8                                      ; load; *envptr12075
  %envptr12076 = inttoptr i64 %env10635 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12077 = getelementptr inbounds i64, i64* %envptr12076, i64 2                ; &envptr12076[2]
  %cont9384 = load i64, i64* %envptr12077, align 8                                   ; load; *envptr12077
  %envptr12078 = inttoptr i64 %env10635 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12079 = getelementptr inbounds i64, i64* %envptr12078, i64 1                ; &envptr12078[1]
  %a9286 = load i64, i64* %envptr12079, align 8                                      ; load; *envptr12079
  %cloptr12080 = inttoptr i64 %a9286 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12081 = getelementptr inbounds i64, i64* %cloptr12080, i64 0                 ; &cloptr12080[0]
  %f12083 = load i64, i64* %i0ptr12081, align 8                                      ; load; *i0ptr12081
  %fptr12082 = inttoptr i64 %f12083 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12082(i64 %a9286, i64 %cont9384, i64 %a9289, i64 %a9292); tail call
  ret void
}


define void @lam10631(i64 %env10632, i64 %_959392, i64 %a9292) {
  %envptr12084 = inttoptr i64 %env10632 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12085 = getelementptr inbounds i64, i64* %envptr12084, i64 3                ; &envptr12084[3]
  %a9289 = load i64, i64* %envptr12085, align 8                                      ; load; *envptr12085
  %envptr12086 = inttoptr i64 %env10632 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12087 = getelementptr inbounds i64, i64* %envptr12086, i64 2                ; &envptr12086[2]
  %cont9384 = load i64, i64* %envptr12087, align 8                                   ; load; *envptr12087
  %envptr12088 = inttoptr i64 %env10632 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12089 = getelementptr inbounds i64, i64* %envptr12088, i64 1                ; &envptr12088[1]
  %a9286 = load i64, i64* %envptr12089, align 8                                      ; load; *envptr12089
  %cloptr12090 = inttoptr i64 %a9286 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12091 = getelementptr inbounds i64, i64* %cloptr12090, i64 0                 ; &cloptr12090[0]
  %f12093 = load i64, i64* %i0ptr12091, align 8                                      ; load; *i0ptr12091
  %fptr12092 = inttoptr i64 %f12093 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12092(i64 %a9286, i64 %cont9384, i64 %a9289, i64 %a9292); tail call
  ret void
}


define void @lam10628(i64 %env10629, i64 %_959390, i64 %a9289) {
  %envptr12094 = inttoptr i64 %env10629 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12095 = getelementptr inbounds i64, i64* %envptr12094, i64 7                ; &envptr12094[7]
  %Q5z$_37_62 = load i64, i64* %envptr12095, align 8                                 ; load; *envptr12095
  %envptr12096 = inttoptr i64 %env10629 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12097 = getelementptr inbounds i64, i64* %envptr12096, i64 6                ; &envptr12096[6]
  %Sjy$lx = load i64, i64* %envptr12097, align 8                                     ; load; *envptr12097
  %envptr12098 = inttoptr i64 %env10629 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12099 = getelementptr inbounds i64, i64* %envptr12098, i64 5                ; &envptr12098[5]
  %cont9384 = load i64, i64* %envptr12099, align 8                                   ; load; *envptr12099
  %envptr12100 = inttoptr i64 %env10629 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12101 = getelementptr inbounds i64, i64* %envptr12100, i64 4                ; &envptr12100[4]
  %jhm$ly = load i64, i64* %envptr12101, align 8                                     ; load; *envptr12101
  %envptr12102 = inttoptr i64 %env10629 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12103 = getelementptr inbounds i64, i64* %envptr12102, i64 3                ; &envptr12102[3]
  %a9286 = load i64, i64* %envptr12103, align 8                                      ; load; *envptr12103
  %envptr12104 = inttoptr i64 %env10629 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12105 = getelementptr inbounds i64, i64* %envptr12104, i64 2                ; &envptr12104[2]
  %KUA$y = load i64, i64* %envptr12105, align 8                                      ; load; *envptr12105
  %envptr12106 = inttoptr i64 %env10629 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12107 = getelementptr inbounds i64, i64* %envptr12106, i64 1                ; &envptr12106[1]
  %I3I$_37drop = load i64, i64* %envptr12107, align 8                                ; load; *envptr12107
  %cloptr12108 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr12110 = getelementptr inbounds i64, i64* %cloptr12108, i64 1                  ; &eptr12110[1]
  %eptr12111 = getelementptr inbounds i64, i64* %cloptr12108, i64 2                  ; &eptr12111[2]
  %eptr12112 = getelementptr inbounds i64, i64* %cloptr12108, i64 3                  ; &eptr12112[3]
  %eptr12113 = getelementptr inbounds i64, i64* %cloptr12108, i64 4                  ; &eptr12113[4]
  %eptr12114 = getelementptr inbounds i64, i64* %cloptr12108, i64 5                  ; &eptr12114[5]
  %eptr12115 = getelementptr inbounds i64, i64* %cloptr12108, i64 6                  ; &eptr12115[6]
  %eptr12116 = getelementptr inbounds i64, i64* %cloptr12108, i64 7                  ; &eptr12116[7]
  store i64 %I3I$_37drop, i64* %eptr12110                                            ; *eptr12110 = %I3I$_37drop
  store i64 %KUA$y, i64* %eptr12111                                                  ; *eptr12111 = %KUA$y
  store i64 %a9286, i64* %eptr12112                                                  ; *eptr12112 = %a9286
  store i64 %jhm$ly, i64* %eptr12113                                                 ; *eptr12113 = %jhm$ly
  store i64 %cont9384, i64* %eptr12114                                               ; *eptr12114 = %cont9384
  store i64 %Sjy$lx, i64* %eptr12115                                                 ; *eptr12115 = %Sjy$lx
  store i64 %a9289, i64* %eptr12116                                                  ; *eptr12116 = %a9289
  %eptr12109 = getelementptr inbounds i64, i64* %cloptr12108, i64 0                  ; &cloptr12108[0]
  %f12117 = ptrtoint void(i64,i64,i64)* @lam10626 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12117, i64* %eptr12109                                                 ; store fptr
  %arg9985 = ptrtoint i64* %cloptr12108 to i64                                       ; closure cast; i64* -> i64
  %cloptr12118 = inttoptr i64 %Q5z$_37_62 to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr12119 = getelementptr inbounds i64, i64* %cloptr12118, i64 0                 ; &cloptr12118[0]
  %f12121 = load i64, i64* %i0ptr12119, align 8                                      ; load; *i0ptr12119
  %fptr12120 = inttoptr i64 %f12121 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12120(i64 %Q5z$_37_62, i64 %arg9985, i64 %jhm$ly, i64 %Sjy$lx); tail call
  ret void
}


define void @lam10626(i64 %env10627, i64 %_959391, i64 %a9290) {
  %envptr12122 = inttoptr i64 %env10627 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12123 = getelementptr inbounds i64, i64* %envptr12122, i64 7                ; &envptr12122[7]
  %a9289 = load i64, i64* %envptr12123, align 8                                      ; load; *envptr12123
  %envptr12124 = inttoptr i64 %env10627 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12125 = getelementptr inbounds i64, i64* %envptr12124, i64 6                ; &envptr12124[6]
  %Sjy$lx = load i64, i64* %envptr12125, align 8                                     ; load; *envptr12125
  %envptr12126 = inttoptr i64 %env10627 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12127 = getelementptr inbounds i64, i64* %envptr12126, i64 5                ; &envptr12126[5]
  %cont9384 = load i64, i64* %envptr12127, align 8                                   ; load; *envptr12127
  %envptr12128 = inttoptr i64 %env10627 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12129 = getelementptr inbounds i64, i64* %envptr12128, i64 4                ; &envptr12128[4]
  %jhm$ly = load i64, i64* %envptr12129, align 8                                     ; load; *envptr12129
  %envptr12130 = inttoptr i64 %env10627 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12131 = getelementptr inbounds i64, i64* %envptr12130, i64 3                ; &envptr12130[3]
  %a9286 = load i64, i64* %envptr12131, align 8                                      ; load; *envptr12131
  %envptr12132 = inttoptr i64 %env10627 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12133 = getelementptr inbounds i64, i64* %envptr12132, i64 2                ; &envptr12132[2]
  %KUA$y = load i64, i64* %envptr12133, align 8                                      ; load; *envptr12133
  %envptr12134 = inttoptr i64 %env10627 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12135 = getelementptr inbounds i64, i64* %envptr12134, i64 1                ; &envptr12134[1]
  %I3I$_37drop = load i64, i64* %envptr12135, align 8                                ; load; *envptr12135
  %cmp12136 = icmp eq i64 %a9290, 15                                                 ; false?
  br i1 %cmp12136, label %else12138, label %then12137                                ; if

then12137:
  %a9291 = call i64 @prim__45(i64 %jhm$ly, i64 %Sjy$lx)                              ; call prim__45
  %cloptr12139 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12141 = getelementptr inbounds i64, i64* %cloptr12139, i64 1                  ; &eptr12141[1]
  %eptr12142 = getelementptr inbounds i64, i64* %cloptr12139, i64 2                  ; &eptr12142[2]
  %eptr12143 = getelementptr inbounds i64, i64* %cloptr12139, i64 3                  ; &eptr12143[3]
  store i64 %a9286, i64* %eptr12141                                                  ; *eptr12141 = %a9286
  store i64 %cont9384, i64* %eptr12142                                               ; *eptr12142 = %cont9384
  store i64 %a9289, i64* %eptr12143                                                  ; *eptr12143 = %a9289
  %eptr12140 = getelementptr inbounds i64, i64* %cloptr12139, i64 0                  ; &cloptr12139[0]
  %f12144 = ptrtoint void(i64,i64,i64)* @lam10621 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12144, i64* %eptr12140                                                 ; store fptr
  %arg9991 = ptrtoint i64* %cloptr12139 to i64                                       ; closure cast; i64* -> i64
  %cloptr12145 = inttoptr i64 %I3I$_37drop to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr12146 = getelementptr inbounds i64, i64* %cloptr12145, i64 0                 ; &cloptr12145[0]
  %f12148 = load i64, i64* %i0ptr12146, align 8                                      ; load; *i0ptr12146
  %fptr12147 = inttoptr i64 %f12148 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12147(i64 %I3I$_37drop, i64 %arg9991, i64 %KUA$y, i64 %a9291); tail call
  ret void

else12138:
  %cloptr12149 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12151 = getelementptr inbounds i64, i64* %cloptr12149, i64 1                  ; &eptr12151[1]
  %eptr12152 = getelementptr inbounds i64, i64* %cloptr12149, i64 2                  ; &eptr12152[2]
  %eptr12153 = getelementptr inbounds i64, i64* %cloptr12149, i64 3                  ; &eptr12153[3]
  store i64 %a9286, i64* %eptr12151                                                  ; *eptr12151 = %a9286
  store i64 %cont9384, i64* %eptr12152                                               ; *eptr12152 = %cont9384
  store i64 %a9289, i64* %eptr12153                                                  ; *eptr12153 = %a9289
  %eptr12150 = getelementptr inbounds i64, i64* %cloptr12149, i64 0                  ; &cloptr12149[0]
  %f12154 = ptrtoint void(i64,i64,i64)* @lam10624 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12154, i64* %eptr12150                                                 ; store fptr
  %arg9999 = ptrtoint i64* %cloptr12149 to i64                                       ; closure cast; i64* -> i64
  %arg9998 = add i64 0, 0                                                            ; quoted ()
  %cloptr12155 = inttoptr i64 %arg9999 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr12156 = getelementptr inbounds i64, i64* %cloptr12155, i64 0                 ; &cloptr12155[0]
  %f12158 = load i64, i64* %i0ptr12156, align 8                                      ; load; *i0ptr12156
  %fptr12157 = inttoptr i64 %f12158 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12157(i64 %arg9999, i64 %arg9998, i64 %KUA$y)       ; tail call
  ret void
}


define void @lam10624(i64 %env10625, i64 %_959392, i64 %a9292) {
  %envptr12159 = inttoptr i64 %env10625 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12160 = getelementptr inbounds i64, i64* %envptr12159, i64 3                ; &envptr12159[3]
  %a9289 = load i64, i64* %envptr12160, align 8                                      ; load; *envptr12160
  %envptr12161 = inttoptr i64 %env10625 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12162 = getelementptr inbounds i64, i64* %envptr12161, i64 2                ; &envptr12161[2]
  %cont9384 = load i64, i64* %envptr12162, align 8                                   ; load; *envptr12162
  %envptr12163 = inttoptr i64 %env10625 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12164 = getelementptr inbounds i64, i64* %envptr12163, i64 1                ; &envptr12163[1]
  %a9286 = load i64, i64* %envptr12164, align 8                                      ; load; *envptr12164
  %cloptr12165 = inttoptr i64 %a9286 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12166 = getelementptr inbounds i64, i64* %cloptr12165, i64 0                 ; &cloptr12165[0]
  %f12168 = load i64, i64* %i0ptr12166, align 8                                      ; load; *i0ptr12166
  %fptr12167 = inttoptr i64 %f12168 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12167(i64 %a9286, i64 %cont9384, i64 %a9289, i64 %a9292); tail call
  ret void
}


define void @lam10621(i64 %env10622, i64 %_959392, i64 %a9292) {
  %envptr12169 = inttoptr i64 %env10622 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12170 = getelementptr inbounds i64, i64* %envptr12169, i64 3                ; &envptr12169[3]
  %a9289 = load i64, i64* %envptr12170, align 8                                      ; load; *envptr12170
  %envptr12171 = inttoptr i64 %env10622 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12172 = getelementptr inbounds i64, i64* %envptr12171, i64 2                ; &envptr12171[2]
  %cont9384 = load i64, i64* %envptr12172, align 8                                   ; load; *envptr12172
  %envptr12173 = inttoptr i64 %env10622 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12174 = getelementptr inbounds i64, i64* %envptr12173, i64 1                ; &envptr12173[1]
  %a9286 = load i64, i64* %envptr12174, align 8                                      ; load; *envptr12174
  %cloptr12175 = inttoptr i64 %a9286 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12176 = getelementptr inbounds i64, i64* %cloptr12175, i64 0                 ; &cloptr12175[0]
  %f12178 = load i64, i64* %i0ptr12176, align 8                                      ; load; *i0ptr12176
  %fptr12177 = inttoptr i64 %f12178 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12177(i64 %a9286, i64 %cont9384, i64 %a9289, i64 %a9292); tail call
  ret void
}


define void @lam10618(i64 %env10619, i64 %cont9397, i64 %QQ3$new) {
  %envptr12179 = inttoptr i64 %env10619 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12180 = getelementptr inbounds i64, i64* %envptr12179, i64 2                ; &envptr12179[2]
  %RUC$_37common_45tail = load i64, i64* %envptr12180, align 8                       ; load; *envptr12180
  %envptr12181 = inttoptr i64 %env10619 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12182 = getelementptr inbounds i64, i64* %envptr12181, i64 1                ; &envptr12181[1]
  %L5S$_37wind_45stack = load i64, i64* %envptr12182, align 8                        ; load; *envptr12182
  %arg10028 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9293 = call i64 @prim_vector_45ref(i64 %L5S$_37wind_45stack, i64 %arg10028)      ; call prim_vector_45ref
  %cloptr12183 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12185 = getelementptr inbounds i64, i64* %cloptr12183, i64 1                  ; &eptr12185[1]
  %eptr12186 = getelementptr inbounds i64, i64* %cloptr12183, i64 2                  ; &eptr12186[2]
  %eptr12187 = getelementptr inbounds i64, i64* %cloptr12183, i64 3                  ; &eptr12187[3]
  store i64 %cont9397, i64* %eptr12185                                               ; *eptr12185 = %cont9397
  store i64 %QQ3$new, i64* %eptr12186                                                ; *eptr12186 = %QQ3$new
  store i64 %L5S$_37wind_45stack, i64* %eptr12187                                    ; *eptr12187 = %L5S$_37wind_45stack
  %eptr12184 = getelementptr inbounds i64, i64* %cloptr12183, i64 0                  ; &cloptr12183[0]
  %f12188 = ptrtoint void(i64,i64,i64)* @lam10615 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12188, i64* %eptr12184                                                 ; store fptr
  %arg10032 = ptrtoint i64* %cloptr12183 to i64                                      ; closure cast; i64* -> i64
  %cloptr12189 = inttoptr i64 %RUC$_37common_45tail to i64*                          ; closure/env cast; i64 -> i64*
  %i0ptr12190 = getelementptr inbounds i64, i64* %cloptr12189, i64 0                 ; &cloptr12189[0]
  %f12192 = load i64, i64* %i0ptr12190, align 8                                      ; load; *i0ptr12190
  %fptr12191 = inttoptr i64 %f12192 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12191(i64 %RUC$_37common_45tail, i64 %arg10032, i64 %QQ3$new, i64 %a9293); tail call
  ret void
}


define void @lam10615(i64 %env10616, i64 %_959398, i64 %YWM$tail) {
  %envptr12193 = inttoptr i64 %env10616 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12194 = getelementptr inbounds i64, i64* %envptr12193, i64 3                ; &envptr12193[3]
  %L5S$_37wind_45stack = load i64, i64* %envptr12194, align 8                        ; load; *envptr12194
  %envptr12195 = inttoptr i64 %env10616 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12196 = getelementptr inbounds i64, i64* %envptr12195, i64 2                ; &envptr12195[2]
  %QQ3$new = load i64, i64* %envptr12196, align 8                                    ; load; *envptr12196
  %envptr12197 = inttoptr i64 %env10616 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12198 = getelementptr inbounds i64, i64* %envptr12197, i64 1                ; &envptr12197[1]
  %cont9397 = load i64, i64* %envptr12198, align 8                                   ; load; *envptr12198
  %cloptr12199 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12200 = getelementptr inbounds i64, i64* %cloptr12199, i64 0                  ; &cloptr12199[0]
  %f12201 = ptrtoint void(i64,i64)* @lam10613 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f12201, i64* %eptr12200                                                 ; store fptr
  %arg10035 = ptrtoint i64* %cloptr12199 to i64                                      ; closure cast; i64* -> i64
  %cloptr12202 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr12204 = getelementptr inbounds i64, i64* %cloptr12202, i64 1                  ; &eptr12204[1]
  %eptr12205 = getelementptr inbounds i64, i64* %cloptr12202, i64 2                  ; &eptr12205[2]
  %eptr12206 = getelementptr inbounds i64, i64* %cloptr12202, i64 3                  ; &eptr12206[3]
  %eptr12207 = getelementptr inbounds i64, i64* %cloptr12202, i64 4                  ; &eptr12207[4]
  store i64 %YWM$tail, i64* %eptr12204                                               ; *eptr12204 = %YWM$tail
  store i64 %cont9397, i64* %eptr12205                                               ; *eptr12205 = %cont9397
  store i64 %QQ3$new, i64* %eptr12206                                                ; *eptr12206 = %QQ3$new
  store i64 %L5S$_37wind_45stack, i64* %eptr12207                                    ; *eptr12207 = %L5S$_37wind_45stack
  %eptr12203 = getelementptr inbounds i64, i64* %cloptr12202, i64 0                  ; &cloptr12202[0]
  %f12208 = ptrtoint void(i64,i64,i64)* @lam10610 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12208, i64* %eptr12203                                                 ; store fptr
  %arg10034 = ptrtoint i64* %cloptr12202 to i64                                      ; closure cast; i64* -> i64
  %cloptr12209 = inttoptr i64 %arg10035 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12210 = getelementptr inbounds i64, i64* %cloptr12209, i64 0                 ; &cloptr12209[0]
  %f12212 = load i64, i64* %i0ptr12210, align 8                                      ; load; *i0ptr12210
  %fptr12211 = inttoptr i64 %f12212 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12211(i64 %arg10035, i64 %arg10034)                 ; tail call
  ret void
}


define void @lam10613(i64 %env10614, i64 %tt4$lst9419) {
  %cont9418 = call i64 @prim_car(i64 %tt4$lst9419)                                   ; call prim_car
  %tt4$lst = call i64 @prim_cdr(i64 %tt4$lst9419)                                    ; call prim_cdr
  %arg10039 = add i64 0, 0                                                           ; quoted ()
  %cloptr12213 = inttoptr i64 %cont9418 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12214 = getelementptr inbounds i64, i64* %cloptr12213, i64 0                 ; &cloptr12213[0]
  %f12216 = load i64, i64* %i0ptr12214, align 8                                      ; load; *i0ptr12214
  %fptr12215 = inttoptr i64 %f12216 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12215(i64 %cont9418, i64 %arg10039, i64 %tt4$lst)   ; tail call
  ret void
}


define void @lam10610(i64 %env10611, i64 %_959416, i64 %a9294) {
  %envptr12217 = inttoptr i64 %env10611 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12218 = getelementptr inbounds i64, i64* %envptr12217, i64 4                ; &envptr12217[4]
  %L5S$_37wind_45stack = load i64, i64* %envptr12218, align 8                        ; load; *envptr12218
  %envptr12219 = inttoptr i64 %env10611 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12220 = getelementptr inbounds i64, i64* %envptr12219, i64 3                ; &envptr12219[3]
  %QQ3$new = load i64, i64* %envptr12220, align 8                                    ; load; *envptr12220
  %envptr12221 = inttoptr i64 %env10611 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12222 = getelementptr inbounds i64, i64* %envptr12221, i64 2                ; &envptr12221[2]
  %cont9397 = load i64, i64* %envptr12222, align 8                                   ; load; *envptr12222
  %envptr12223 = inttoptr i64 %env10611 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12224 = getelementptr inbounds i64, i64* %envptr12223, i64 1                ; &envptr12223[1]
  %YWM$tail = load i64, i64* %envptr12224, align 8                                   ; load; *envptr12224
  %arg10042 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9417 = call i64 @prim_make_45vector(i64 %arg10042, i64 %a9294)             ; call prim_make_45vector
  %cloptr12225 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr12227 = getelementptr inbounds i64, i64* %cloptr12225, i64 1                  ; &eptr12227[1]
  %eptr12228 = getelementptr inbounds i64, i64* %cloptr12225, i64 2                  ; &eptr12228[2]
  %eptr12229 = getelementptr inbounds i64, i64* %cloptr12225, i64 3                  ; &eptr12229[3]
  %eptr12230 = getelementptr inbounds i64, i64* %cloptr12225, i64 4                  ; &eptr12230[4]
  store i64 %YWM$tail, i64* %eptr12227                                               ; *eptr12227 = %YWM$tail
  store i64 %cont9397, i64* %eptr12228                                               ; *eptr12228 = %cont9397
  store i64 %QQ3$new, i64* %eptr12229                                                ; *eptr12229 = %QQ3$new
  store i64 %L5S$_37wind_45stack, i64* %eptr12230                                    ; *eptr12230 = %L5S$_37wind_45stack
  %eptr12226 = getelementptr inbounds i64, i64* %cloptr12225, i64 0                  ; &cloptr12225[0]
  %f12231 = ptrtoint void(i64,i64,i64)* @lam10607 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12231, i64* %eptr12226                                                 ; store fptr
  %arg10045 = ptrtoint i64* %cloptr12225 to i64                                      ; closure cast; i64* -> i64
  %arg10044 = add i64 0, 0                                                           ; quoted ()
  %cloptr12232 = inttoptr i64 %arg10045 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12233 = getelementptr inbounds i64, i64* %cloptr12232, i64 0                 ; &cloptr12232[0]
  %f12235 = load i64, i64* %i0ptr12233, align 8                                      ; load; *i0ptr12233
  %fptr12234 = inttoptr i64 %f12235 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12234(i64 %arg10045, i64 %arg10044, i64 %retprim9417); tail call
  ret void
}


define void @lam10607(i64 %env10608, i64 %_959410, i64 %v72$f) {
  %envptr12236 = inttoptr i64 %env10608 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12237 = getelementptr inbounds i64, i64* %envptr12236, i64 4                ; &envptr12236[4]
  %L5S$_37wind_45stack = load i64, i64* %envptr12237, align 8                        ; load; *envptr12237
  %envptr12238 = inttoptr i64 %env10608 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12239 = getelementptr inbounds i64, i64* %envptr12238, i64 3                ; &envptr12238[3]
  %QQ3$new = load i64, i64* %envptr12239, align 8                                    ; load; *envptr12239
  %envptr12240 = inttoptr i64 %env10608 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12241 = getelementptr inbounds i64, i64* %envptr12240, i64 2                ; &envptr12240[2]
  %cont9397 = load i64, i64* %envptr12241, align 8                                   ; load; *envptr12241
  %envptr12242 = inttoptr i64 %env10608 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12243 = getelementptr inbounds i64, i64* %envptr12242, i64 1                ; &envptr12242[1]
  %YWM$tail = load i64, i64* %envptr12243, align 8                                   ; load; *envptr12243
  %arg10047 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %cloptr12244 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12246 = getelementptr inbounds i64, i64* %cloptr12244, i64 1                  ; &eptr12246[1]
  %eptr12247 = getelementptr inbounds i64, i64* %cloptr12244, i64 2                  ; &eptr12247[2]
  %eptr12248 = getelementptr inbounds i64, i64* %cloptr12244, i64 3                  ; &eptr12248[3]
  store i64 %v72$f, i64* %eptr12246                                                  ; *eptr12246 = %v72$f
  store i64 %YWM$tail, i64* %eptr12247                                               ; *eptr12247 = %YWM$tail
  store i64 %L5S$_37wind_45stack, i64* %eptr12248                                    ; *eptr12248 = %L5S$_37wind_45stack
  %eptr12245 = getelementptr inbounds i64, i64* %cloptr12244, i64 0                  ; &cloptr12244[0]
  %f12249 = ptrtoint void(i64,i64,i64)* @lam10604 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12249, i64* %eptr12245                                                 ; store fptr
  %arg10046 = ptrtoint i64* %cloptr12244 to i64                                      ; closure cast; i64* -> i64
  %RwC$_959183 = call i64 @prim_vector_45set_33(i64 %v72$f, i64 %arg10047, i64 %arg10046); call prim_vector_45set_33
  %arg10072 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9302 = call i64 @prim_vector_45ref(i64 %v72$f, i64 %arg10072)                    ; call prim_vector_45ref
  %arg10074 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9303 = call i64 @prim_vector_45ref(i64 %L5S$_37wind_45stack, i64 %arg10074)      ; call prim_vector_45ref
  %cloptr12250 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr12252 = getelementptr inbounds i64, i64* %cloptr12250, i64 1                  ; &eptr12252[1]
  %eptr12253 = getelementptr inbounds i64, i64* %cloptr12250, i64 2                  ; &eptr12253[2]
  %eptr12254 = getelementptr inbounds i64, i64* %cloptr12250, i64 3                  ; &eptr12254[3]
  %eptr12255 = getelementptr inbounds i64, i64* %cloptr12250, i64 4                  ; &eptr12255[4]
  store i64 %YWM$tail, i64* %eptr12252                                               ; *eptr12252 = %YWM$tail
  store i64 %cont9397, i64* %eptr12253                                               ; *eptr12253 = %cont9397
  store i64 %QQ3$new, i64* %eptr12254                                                ; *eptr12254 = %QQ3$new
  store i64 %L5S$_37wind_45stack, i64* %eptr12255                                    ; *eptr12255 = %L5S$_37wind_45stack
  %eptr12251 = getelementptr inbounds i64, i64* %cloptr12250, i64 0                  ; &cloptr12250[0]
  %f12256 = ptrtoint void(i64,i64,i64)* @lam10592 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12256, i64* %eptr12251                                                 ; store fptr
  %arg10077 = ptrtoint i64* %cloptr12250 to i64                                      ; closure cast; i64* -> i64
  %cloptr12257 = inttoptr i64 %a9302 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12258 = getelementptr inbounds i64, i64* %cloptr12257, i64 0                 ; &cloptr12257[0]
  %f12260 = load i64, i64* %i0ptr12258, align 8                                      ; load; *i0ptr12258
  %fptr12259 = inttoptr i64 %f12260 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12259(i64 %a9302, i64 %arg10077, i64 %a9303)        ; tail call
  ret void
}


define void @lam10604(i64 %env10605, i64 %cont9411, i64 %ed1$l) {
  %envptr12261 = inttoptr i64 %env10605 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12262 = getelementptr inbounds i64, i64* %envptr12261, i64 3                ; &envptr12261[3]
  %L5S$_37wind_45stack = load i64, i64* %envptr12262, align 8                        ; load; *envptr12262
  %envptr12263 = inttoptr i64 %env10605 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12264 = getelementptr inbounds i64, i64* %envptr12263, i64 2                ; &envptr12263[2]
  %YWM$tail = load i64, i64* %envptr12264, align 8                                   ; load; *envptr12264
  %envptr12265 = inttoptr i64 %env10605 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12266 = getelementptr inbounds i64, i64* %envptr12265, i64 1                ; &envptr12265[1]
  %v72$f = load i64, i64* %envptr12266, align 8                                      ; load; *envptr12266
  %a9295 = call i64 @prim_eq_63(i64 %ed1$l, i64 %YWM$tail)                           ; call prim_eq_63
  %a9296 = call i64 @prim_not(i64 %a9295)                                            ; call prim_not
  %cmp12267 = icmp eq i64 %a9296, 15                                                 ; false?
  br i1 %cmp12267, label %else12269, label %then12268                                ; if

then12268:
  %a9297 = call i64 @prim_cdr(i64 %ed1$l)                                            ; call prim_cdr
  %arg10054 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9414 = call i64 @prim_vector_45set_33(i64 %L5S$_37wind_45stack, i64 %arg10054, i64 %a9297); call prim_vector_45set_33
  %cloptr12270 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12272 = getelementptr inbounds i64, i64* %cloptr12270, i64 1                  ; &eptr12272[1]
  %eptr12273 = getelementptr inbounds i64, i64* %cloptr12270, i64 2                  ; &eptr12273[2]
  %eptr12274 = getelementptr inbounds i64, i64* %cloptr12270, i64 3                  ; &eptr12274[3]
  store i64 %v72$f, i64* %eptr12272                                                  ; *eptr12272 = %v72$f
  store i64 %ed1$l, i64* %eptr12273                                                  ; *eptr12273 = %ed1$l
  store i64 %cont9411, i64* %eptr12274                                               ; *eptr12274 = %cont9411
  %eptr12271 = getelementptr inbounds i64, i64* %cloptr12270, i64 0                  ; &cloptr12270[0]
  %f12275 = ptrtoint void(i64,i64,i64)* @lam10600 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12275, i64* %eptr12271                                                 ; store fptr
  %arg10058 = ptrtoint i64* %cloptr12270 to i64                                      ; closure cast; i64* -> i64
  %arg10057 = add i64 0, 0                                                           ; quoted ()
  %cloptr12276 = inttoptr i64 %arg10058 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12277 = getelementptr inbounds i64, i64* %cloptr12276, i64 0                 ; &cloptr12276[0]
  %f12279 = load i64, i64* %i0ptr12277, align 8                                      ; load; *i0ptr12277
  %fptr12278 = inttoptr i64 %f12279 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12278(i64 %arg10058, i64 %arg10057, i64 %retprim9414); tail call
  ret void

else12269:
  %retprim9415 = call i64 @prim_void()                                               ; call prim_void
  %arg10070 = add i64 0, 0                                                           ; quoted ()
  %cloptr12280 = inttoptr i64 %cont9411 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12281 = getelementptr inbounds i64, i64* %cloptr12280, i64 0                 ; &cloptr12280[0]
  %f12283 = load i64, i64* %i0ptr12281, align 8                                      ; load; *i0ptr12281
  %fptr12282 = inttoptr i64 %f12283 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12282(i64 %cont9411, i64 %arg10070, i64 %retprim9415); tail call
  ret void
}


define void @lam10600(i64 %env10601, i64 %_959412, i64 %Ta6$_959184) {
  %envptr12284 = inttoptr i64 %env10601 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12285 = getelementptr inbounds i64, i64* %envptr12284, i64 3                ; &envptr12284[3]
  %cont9411 = load i64, i64* %envptr12285, align 8                                   ; load; *envptr12285
  %envptr12286 = inttoptr i64 %env10601 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12287 = getelementptr inbounds i64, i64* %envptr12286, i64 2                ; &envptr12286[2]
  %ed1$l = load i64, i64* %envptr12287, align 8                                      ; load; *envptr12287
  %envptr12288 = inttoptr i64 %env10601 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12289 = getelementptr inbounds i64, i64* %envptr12288, i64 1                ; &envptr12288[1]
  %v72$f = load i64, i64* %envptr12289, align 8                                      ; load; *envptr12289
  %a9298 = call i64 @prim_car(i64 %ed1$l)                                            ; call prim_car
  %a9299 = call i64 @prim_cdr(i64 %a9298)                                            ; call prim_cdr
  %cloptr12290 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12292 = getelementptr inbounds i64, i64* %cloptr12290, i64 1                  ; &eptr12292[1]
  %eptr12293 = getelementptr inbounds i64, i64* %cloptr12290, i64 2                  ; &eptr12293[2]
  %eptr12294 = getelementptr inbounds i64, i64* %cloptr12290, i64 3                  ; &eptr12294[3]
  store i64 %v72$f, i64* %eptr12292                                                  ; *eptr12292 = %v72$f
  store i64 %ed1$l, i64* %eptr12293                                                  ; *eptr12293 = %ed1$l
  store i64 %cont9411, i64* %eptr12294                                               ; *eptr12294 = %cont9411
  %eptr12291 = getelementptr inbounds i64, i64* %cloptr12290, i64 0                  ; &cloptr12290[0]
  %f12295 = ptrtoint void(i64,i64,i64)* @lam10598 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12295, i64* %eptr12291                                                 ; store fptr
  %arg10061 = ptrtoint i64* %cloptr12290 to i64                                      ; closure cast; i64* -> i64
  %cloptr12296 = inttoptr i64 %a9299 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12297 = getelementptr inbounds i64, i64* %cloptr12296, i64 0                 ; &cloptr12296[0]
  %f12299 = load i64, i64* %i0ptr12297, align 8                                      ; load; *i0ptr12297
  %fptr12298 = inttoptr i64 %f12299 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12298(i64 %a9299, i64 %arg10061)                    ; tail call
  ret void
}


define void @lam10598(i64 %env10599, i64 %_959413, i64 %wIL$_959185) {
  %envptr12300 = inttoptr i64 %env10599 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12301 = getelementptr inbounds i64, i64* %envptr12300, i64 3                ; &envptr12300[3]
  %cont9411 = load i64, i64* %envptr12301, align 8                                   ; load; *envptr12301
  %envptr12302 = inttoptr i64 %env10599 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12303 = getelementptr inbounds i64, i64* %envptr12302, i64 2                ; &envptr12302[2]
  %ed1$l = load i64, i64* %envptr12303, align 8                                      ; load; *envptr12303
  %envptr12304 = inttoptr i64 %env10599 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12305 = getelementptr inbounds i64, i64* %envptr12304, i64 1                ; &envptr12304[1]
  %v72$f = load i64, i64* %envptr12305, align 8                                      ; load; *envptr12305
  %arg10063 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9300 = call i64 @prim_vector_45ref(i64 %v72$f, i64 %arg10063)                    ; call prim_vector_45ref
  %a9301 = call i64 @prim_cdr(i64 %ed1$l)                                            ; call prim_cdr
  %cloptr12306 = inttoptr i64 %a9300 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12307 = getelementptr inbounds i64, i64* %cloptr12306, i64 0                 ; &cloptr12306[0]
  %f12309 = load i64, i64* %i0ptr12307, align 8                                      ; load; *i0ptr12307
  %fptr12308 = inttoptr i64 %f12309 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12308(i64 %a9300, i64 %cont9411, i64 %a9301)        ; tail call
  ret void
}


define void @lam10592(i64 %env10593, i64 %_959399, i64 %LbD$_959182) {
  %envptr12310 = inttoptr i64 %env10593 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12311 = getelementptr inbounds i64, i64* %envptr12310, i64 4                ; &envptr12310[4]
  %L5S$_37wind_45stack = load i64, i64* %envptr12311, align 8                        ; load; *envptr12311
  %envptr12312 = inttoptr i64 %env10593 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12313 = getelementptr inbounds i64, i64* %envptr12312, i64 3                ; &envptr12312[3]
  %QQ3$new = load i64, i64* %envptr12313, align 8                                    ; load; *envptr12313
  %envptr12314 = inttoptr i64 %env10593 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12315 = getelementptr inbounds i64, i64* %envptr12314, i64 2                ; &envptr12314[2]
  %cont9397 = load i64, i64* %envptr12315, align 8                                   ; load; *envptr12315
  %envptr12316 = inttoptr i64 %env10593 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12317 = getelementptr inbounds i64, i64* %envptr12316, i64 1                ; &envptr12316[1]
  %YWM$tail = load i64, i64* %envptr12317, align 8                                   ; load; *envptr12317
  %cloptr12318 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12319 = getelementptr inbounds i64, i64* %cloptr12318, i64 0                  ; &cloptr12318[0]
  %f12320 = ptrtoint void(i64,i64)* @lam10590 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f12320, i64* %eptr12319                                                 ; store fptr
  %arg10080 = ptrtoint i64* %cloptr12318 to i64                                      ; closure cast; i64* -> i64
  %cloptr12321 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr12323 = getelementptr inbounds i64, i64* %cloptr12321, i64 1                  ; &eptr12323[1]
  %eptr12324 = getelementptr inbounds i64, i64* %cloptr12321, i64 2                  ; &eptr12324[2]
  %eptr12325 = getelementptr inbounds i64, i64* %cloptr12321, i64 3                  ; &eptr12325[3]
  %eptr12326 = getelementptr inbounds i64, i64* %cloptr12321, i64 4                  ; &eptr12326[4]
  store i64 %YWM$tail, i64* %eptr12323                                               ; *eptr12323 = %YWM$tail
  store i64 %cont9397, i64* %eptr12324                                               ; *eptr12324 = %cont9397
  store i64 %QQ3$new, i64* %eptr12325                                                ; *eptr12325 = %QQ3$new
  store i64 %L5S$_37wind_45stack, i64* %eptr12326                                    ; *eptr12326 = %L5S$_37wind_45stack
  %eptr12322 = getelementptr inbounds i64, i64* %cloptr12321, i64 0                  ; &cloptr12321[0]
  %f12327 = ptrtoint void(i64,i64,i64)* @lam10587 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12327, i64* %eptr12322                                                 ; store fptr
  %arg10079 = ptrtoint i64* %cloptr12321 to i64                                      ; closure cast; i64* -> i64
  %cloptr12328 = inttoptr i64 %arg10080 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12329 = getelementptr inbounds i64, i64* %cloptr12328, i64 0                 ; &cloptr12328[0]
  %f12331 = load i64, i64* %i0ptr12329, align 8                                      ; load; *i0ptr12329
  %fptr12330 = inttoptr i64 %f12331 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12330(i64 %arg10080, i64 %arg10079)                 ; tail call
  ret void
}


define void @lam10590(i64 %env10591, i64 %I0K$lst9409) {
  %cont9408 = call i64 @prim_car(i64 %I0K$lst9409)                                   ; call prim_car
  %I0K$lst = call i64 @prim_cdr(i64 %I0K$lst9409)                                    ; call prim_cdr
  %arg10084 = add i64 0, 0                                                           ; quoted ()
  %cloptr12332 = inttoptr i64 %cont9408 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12333 = getelementptr inbounds i64, i64* %cloptr12332, i64 0                 ; &cloptr12332[0]
  %f12335 = load i64, i64* %i0ptr12333, align 8                                      ; load; *i0ptr12333
  %fptr12334 = inttoptr i64 %f12335 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12334(i64 %cont9408, i64 %arg10084, i64 %I0K$lst)   ; tail call
  ret void
}


define void @lam10587(i64 %env10588, i64 %_959406, i64 %a9304) {
  %envptr12336 = inttoptr i64 %env10588 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12337 = getelementptr inbounds i64, i64* %envptr12336, i64 4                ; &envptr12336[4]
  %L5S$_37wind_45stack = load i64, i64* %envptr12337, align 8                        ; load; *envptr12337
  %envptr12338 = inttoptr i64 %env10588 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12339 = getelementptr inbounds i64, i64* %envptr12338, i64 3                ; &envptr12338[3]
  %QQ3$new = load i64, i64* %envptr12339, align 8                                    ; load; *envptr12339
  %envptr12340 = inttoptr i64 %env10588 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12341 = getelementptr inbounds i64, i64* %envptr12340, i64 2                ; &envptr12340[2]
  %cont9397 = load i64, i64* %envptr12341, align 8                                   ; load; *envptr12341
  %envptr12342 = inttoptr i64 %env10588 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12343 = getelementptr inbounds i64, i64* %envptr12342, i64 1                ; &envptr12342[1]
  %YWM$tail = load i64, i64* %envptr12343, align 8                                   ; load; *envptr12343
  %arg10087 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9407 = call i64 @prim_make_45vector(i64 %arg10087, i64 %a9304)             ; call prim_make_45vector
  %cloptr12344 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr12346 = getelementptr inbounds i64, i64* %cloptr12344, i64 1                  ; &eptr12346[1]
  %eptr12347 = getelementptr inbounds i64, i64* %cloptr12344, i64 2                  ; &eptr12347[2]
  %eptr12348 = getelementptr inbounds i64, i64* %cloptr12344, i64 3                  ; &eptr12348[3]
  %eptr12349 = getelementptr inbounds i64, i64* %cloptr12344, i64 4                  ; &eptr12349[4]
  store i64 %YWM$tail, i64* %eptr12346                                               ; *eptr12346 = %YWM$tail
  store i64 %cont9397, i64* %eptr12347                                               ; *eptr12347 = %cont9397
  store i64 %QQ3$new, i64* %eptr12348                                                ; *eptr12348 = %QQ3$new
  store i64 %L5S$_37wind_45stack, i64* %eptr12349                                    ; *eptr12349 = %L5S$_37wind_45stack
  %eptr12345 = getelementptr inbounds i64, i64* %cloptr12344, i64 0                  ; &cloptr12344[0]
  %f12350 = ptrtoint void(i64,i64,i64)* @lam10584 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12350, i64* %eptr12345                                                 ; store fptr
  %arg10090 = ptrtoint i64* %cloptr12344 to i64                                      ; closure cast; i64* -> i64
  %arg10089 = add i64 0, 0                                                           ; quoted ()
  %cloptr12351 = inttoptr i64 %arg10090 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12352 = getelementptr inbounds i64, i64* %cloptr12351, i64 0                 ; &cloptr12351[0]
  %f12354 = load i64, i64* %i0ptr12352, align 8                                      ; load; *i0ptr12352
  %fptr12353 = inttoptr i64 %f12354 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12353(i64 %arg10090, i64 %arg10089, i64 %retprim9407); tail call
  ret void
}


define void @lam10584(i64 %env10585, i64 %_959400, i64 %tka$f) {
  %envptr12355 = inttoptr i64 %env10585 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12356 = getelementptr inbounds i64, i64* %envptr12355, i64 4                ; &envptr12355[4]
  %L5S$_37wind_45stack = load i64, i64* %envptr12356, align 8                        ; load; *envptr12356
  %envptr12357 = inttoptr i64 %env10585 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12358 = getelementptr inbounds i64, i64* %envptr12357, i64 3                ; &envptr12357[3]
  %QQ3$new = load i64, i64* %envptr12358, align 8                                    ; load; *envptr12358
  %envptr12359 = inttoptr i64 %env10585 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12360 = getelementptr inbounds i64, i64* %envptr12359, i64 2                ; &envptr12359[2]
  %cont9397 = load i64, i64* %envptr12360, align 8                                   ; load; *envptr12360
  %envptr12361 = inttoptr i64 %env10585 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12362 = getelementptr inbounds i64, i64* %envptr12361, i64 1                ; &envptr12361[1]
  %YWM$tail = load i64, i64* %envptr12362, align 8                                   ; load; *envptr12362
  %arg10092 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %cloptr12363 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12365 = getelementptr inbounds i64, i64* %cloptr12363, i64 1                  ; &eptr12365[1]
  %eptr12366 = getelementptr inbounds i64, i64* %cloptr12363, i64 2                  ; &eptr12366[2]
  %eptr12367 = getelementptr inbounds i64, i64* %cloptr12363, i64 3                  ; &eptr12367[3]
  store i64 %YWM$tail, i64* %eptr12365                                               ; *eptr12365 = %YWM$tail
  store i64 %tka$f, i64* %eptr12366                                                  ; *eptr12366 = %tka$f
  store i64 %L5S$_37wind_45stack, i64* %eptr12367                                    ; *eptr12367 = %L5S$_37wind_45stack
  %eptr12364 = getelementptr inbounds i64, i64* %cloptr12363, i64 0                  ; &cloptr12363[0]
  %f12368 = ptrtoint void(i64,i64,i64)* @lam10581 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12368, i64* %eptr12364                                                 ; store fptr
  %arg10091 = ptrtoint i64* %cloptr12363 to i64                                      ; closure cast; i64* -> i64
  %hXq$_959186 = call i64 @prim_vector_45set_33(i64 %tka$f, i64 %arg10092, i64 %arg10091); call prim_vector_45set_33
  %arg10116 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9311 = call i64 @prim_vector_45ref(i64 %tka$f, i64 %arg10116)                    ; call prim_vector_45ref
  %cloptr12369 = inttoptr i64 %a9311 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12370 = getelementptr inbounds i64, i64* %cloptr12369, i64 0                 ; &cloptr12369[0]
  %f12372 = load i64, i64* %i0ptr12370, align 8                                      ; load; *i0ptr12370
  %fptr12371 = inttoptr i64 %f12372 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12371(i64 %a9311, i64 %cont9397, i64 %QQ3$new)      ; tail call
  ret void
}


define void @lam10581(i64 %env10582, i64 %cont9401, i64 %zli$l) {
  %envptr12373 = inttoptr i64 %env10582 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12374 = getelementptr inbounds i64, i64* %envptr12373, i64 3                ; &envptr12373[3]
  %L5S$_37wind_45stack = load i64, i64* %envptr12374, align 8                        ; load; *envptr12374
  %envptr12375 = inttoptr i64 %env10582 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12376 = getelementptr inbounds i64, i64* %envptr12375, i64 2                ; &envptr12375[2]
  %tka$f = load i64, i64* %envptr12376, align 8                                      ; load; *envptr12376
  %envptr12377 = inttoptr i64 %env10582 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12378 = getelementptr inbounds i64, i64* %envptr12377, i64 1                ; &envptr12377[1]
  %YWM$tail = load i64, i64* %envptr12378, align 8                                   ; load; *envptr12378
  %a9305 = call i64 @prim_eq_63(i64 %zli$l, i64 %YWM$tail)                           ; call prim_eq_63
  %a9306 = call i64 @prim_not(i64 %a9305)                                            ; call prim_not
  %cmp12379 = icmp eq i64 %a9306, 15                                                 ; false?
  br i1 %cmp12379, label %else12381, label %then12380                                ; if

then12380:
  %arg10097 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9307 = call i64 @prim_vector_45ref(i64 %tka$f, i64 %arg10097)                    ; call prim_vector_45ref
  %a9308 = call i64 @prim_cdr(i64 %zli$l)                                            ; call prim_cdr
  %cloptr12382 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12384 = getelementptr inbounds i64, i64* %cloptr12382, i64 1                  ; &eptr12384[1]
  %eptr12385 = getelementptr inbounds i64, i64* %cloptr12382, i64 2                  ; &eptr12385[2]
  %eptr12386 = getelementptr inbounds i64, i64* %cloptr12382, i64 3                  ; &eptr12386[3]
  store i64 %zli$l, i64* %eptr12384                                                  ; *eptr12384 = %zli$l
  store i64 %cont9401, i64* %eptr12385                                               ; *eptr12385 = %cont9401
  store i64 %L5S$_37wind_45stack, i64* %eptr12386                                    ; *eptr12386 = %L5S$_37wind_45stack
  %eptr12383 = getelementptr inbounds i64, i64* %cloptr12382, i64 0                  ; &cloptr12382[0]
  %f12387 = ptrtoint void(i64,i64,i64)* @lam10577 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12387, i64* %eptr12383                                                 ; store fptr
  %arg10101 = ptrtoint i64* %cloptr12382 to i64                                      ; closure cast; i64* -> i64
  %cloptr12388 = inttoptr i64 %a9307 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12389 = getelementptr inbounds i64, i64* %cloptr12388, i64 0                 ; &cloptr12388[0]
  %f12391 = load i64, i64* %i0ptr12389, align 8                                      ; load; *i0ptr12389
  %fptr12390 = inttoptr i64 %f12391 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12390(i64 %a9307, i64 %arg10101, i64 %a9308)        ; tail call
  ret void

else12381:
  %retprim9405 = call i64 @prim_void()                                               ; call prim_void
  %arg10114 = add i64 0, 0                                                           ; quoted ()
  %cloptr12392 = inttoptr i64 %cont9401 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12393 = getelementptr inbounds i64, i64* %cloptr12392, i64 0                 ; &cloptr12392[0]
  %f12395 = load i64, i64* %i0ptr12393, align 8                                      ; load; *i0ptr12393
  %fptr12394 = inttoptr i64 %f12395 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12394(i64 %cont9401, i64 %arg10114, i64 %retprim9405); tail call
  ret void
}


define void @lam10577(i64 %env10578, i64 %_959402, i64 %vIP$_959187) {
  %envptr12396 = inttoptr i64 %env10578 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12397 = getelementptr inbounds i64, i64* %envptr12396, i64 3                ; &envptr12396[3]
  %L5S$_37wind_45stack = load i64, i64* %envptr12397, align 8                        ; load; *envptr12397
  %envptr12398 = inttoptr i64 %env10578 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12399 = getelementptr inbounds i64, i64* %envptr12398, i64 2                ; &envptr12398[2]
  %cont9401 = load i64, i64* %envptr12399, align 8                                   ; load; *envptr12399
  %envptr12400 = inttoptr i64 %env10578 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12401 = getelementptr inbounds i64, i64* %envptr12400, i64 1                ; &envptr12400[1]
  %zli$l = load i64, i64* %envptr12401, align 8                                      ; load; *envptr12401
  %a9309 = call i64 @prim_car(i64 %zli$l)                                            ; call prim_car
  %a9310 = call i64 @prim_car(i64 %a9309)                                            ; call prim_car
  %cloptr12402 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12404 = getelementptr inbounds i64, i64* %cloptr12402, i64 1                  ; &eptr12404[1]
  %eptr12405 = getelementptr inbounds i64, i64* %cloptr12402, i64 2                  ; &eptr12405[2]
  %eptr12406 = getelementptr inbounds i64, i64* %cloptr12402, i64 3                  ; &eptr12406[3]
  store i64 %zli$l, i64* %eptr12404                                                  ; *eptr12404 = %zli$l
  store i64 %cont9401, i64* %eptr12405                                               ; *eptr12405 = %cont9401
  store i64 %L5S$_37wind_45stack, i64* %eptr12406                                    ; *eptr12406 = %L5S$_37wind_45stack
  %eptr12403 = getelementptr inbounds i64, i64* %cloptr12402, i64 0                  ; &cloptr12402[0]
  %f12407 = ptrtoint void(i64,i64,i64)* @lam10575 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12407, i64* %eptr12403                                                 ; store fptr
  %arg10105 = ptrtoint i64* %cloptr12402 to i64                                      ; closure cast; i64* -> i64
  %cloptr12408 = inttoptr i64 %a9310 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12409 = getelementptr inbounds i64, i64* %cloptr12408, i64 0                 ; &cloptr12408[0]
  %f12411 = load i64, i64* %i0ptr12409, align 8                                      ; load; *i0ptr12409
  %fptr12410 = inttoptr i64 %f12411 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12410(i64 %a9310, i64 %arg10105)                    ; tail call
  ret void
}


define void @lam10575(i64 %env10576, i64 %_959403, i64 %ewr$_959188) {
  %envptr12412 = inttoptr i64 %env10576 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12413 = getelementptr inbounds i64, i64* %envptr12412, i64 3                ; &envptr12412[3]
  %L5S$_37wind_45stack = load i64, i64* %envptr12413, align 8                        ; load; *envptr12413
  %envptr12414 = inttoptr i64 %env10576 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12415 = getelementptr inbounds i64, i64* %envptr12414, i64 2                ; &envptr12414[2]
  %cont9401 = load i64, i64* %envptr12415, align 8                                   ; load; *envptr12415
  %envptr12416 = inttoptr i64 %env10576 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12417 = getelementptr inbounds i64, i64* %envptr12416, i64 1                ; &envptr12416[1]
  %zli$l = load i64, i64* %envptr12417, align 8                                      ; load; *envptr12417
  %arg10108 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9404 = call i64 @prim_vector_45set_33(i64 %L5S$_37wind_45stack, i64 %arg10108, i64 %zli$l); call prim_vector_45set_33
  %arg10111 = add i64 0, 0                                                           ; quoted ()
  %cloptr12418 = inttoptr i64 %cont9401 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12419 = getelementptr inbounds i64, i64* %cloptr12418, i64 0                 ; &cloptr12418[0]
  %f12421 = load i64, i64* %i0ptr12419, align 8                                      ; load; *i0ptr12419
  %fptr12420 = inttoptr i64 %f12421 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12420(i64 %cont9401, i64 %arg10111, i64 %retprim9404); tail call
  ret void
}


define void @lam10568(i64 %env10569, i64 %sc3$lst9427) {
  %cont9426 = call i64 @prim_car(i64 %sc3$lst9427)                                   ; call prim_car
  %sc3$lst = call i64 @prim_cdr(i64 %sc3$lst9427)                                    ; call prim_cdr
  %arg10126 = add i64 0, 0                                                           ; quoted ()
  %cloptr12422 = inttoptr i64 %cont9426 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12423 = getelementptr inbounds i64, i64* %cloptr12422, i64 0                 ; &cloptr12422[0]
  %f12425 = load i64, i64* %i0ptr12423, align 8                                      ; load; *i0ptr12423
  %fptr12424 = inttoptr i64 %f12425 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12424(i64 %cont9426, i64 %arg10126, i64 %sc3$lst)   ; tail call
  ret void
}


define void @lam10565(i64 %env10566, i64 %_959424, i64 %a9312) {
  %arg10129 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9425 = call i64 @prim_make_45vector(i64 %arg10129, i64 %a9312)             ; call prim_make_45vector
  %cloptr12426 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12427 = getelementptr inbounds i64, i64* %cloptr12426, i64 0                  ; &cloptr12426[0]
  %f12428 = ptrtoint void(i64,i64,i64)* @lam10562 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12428, i64* %eptr12427                                                 ; store fptr
  %arg10132 = ptrtoint i64* %cloptr12426 to i64                                      ; closure cast; i64* -> i64
  %arg10131 = add i64 0, 0                                                           ; quoted ()
  %cloptr12429 = inttoptr i64 %arg10132 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12430 = getelementptr inbounds i64, i64* %cloptr12429, i64 0                 ; &cloptr12429[0]
  %f12432 = load i64, i64* %i0ptr12430, align 8                                      ; load; *i0ptr12430
  %fptr12431 = inttoptr i64 %f12432 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12431(i64 %arg10132, i64 %arg10131, i64 %retprim9425); tail call
  ret void
}


define void @lam10562(i64 %env10563, i64 %_959420, i64 %CUG$v) {
  %arg10134 = call i64 @const_init_int(i64 5)                                        ; quoted int
  %arg10133 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9313 = call i64 @prim_make_45vector(i64 %arg10134, i64 %arg10133)                ; call prim_make_45vector
  %arg10136 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9423 = call i64 @prim_vector_45set_33(i64 %CUG$v, i64 %arg10136, i64 %a9313); call prim_vector_45set_33
  %cloptr12433 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr12435 = getelementptr inbounds i64, i64* %cloptr12433, i64 1                  ; &eptr12435[1]
  store i64 %CUG$v, i64* %eptr12435                                                  ; *eptr12435 = %CUG$v
  %eptr12434 = getelementptr inbounds i64, i64* %cloptr12433, i64 0                  ; &cloptr12433[0]
  %f12436 = ptrtoint void(i64,i64,i64)* @lam10557 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12436, i64* %eptr12434                                                 ; store fptr
  %arg10140 = ptrtoint i64* %cloptr12433 to i64                                      ; closure cast; i64* -> i64
  %arg10139 = add i64 0, 0                                                           ; quoted ()
  %cloptr12437 = inttoptr i64 %arg10140 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12438 = getelementptr inbounds i64, i64* %cloptr12437, i64 0                 ; &cloptr12437[0]
  %f12440 = load i64, i64* %i0ptr12438, align 8                                      ; load; *i0ptr12438
  %fptr12439 = inttoptr i64 %f12440 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12439(i64 %arg10140, i64 %arg10139, i64 %retprim9423); tail call
  ret void
}


define void @lam10557(i64 %env10558, i64 %_959421, i64 %NT0$_959189) {
  %envptr12441 = inttoptr i64 %env10558 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12442 = getelementptr inbounds i64, i64* %envptr12441, i64 1                ; &envptr12441[1]
  %CUG$v = load i64, i64* %envptr12442, align 8                                      ; load; *envptr12442
  %arg10141 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9314 = call i64 @prim_vector_45ref(i64 %CUG$v, i64 %arg10141)                    ; call prim_vector_45ref
  %arg10144 = call i64 @const_init_int(i64 -1)                                       ; quoted int
  %arg10143 = call i64 @const_init_int(i64 7)                                        ; quoted int
  %retprim9422 = call i64 @prim_vector_45set_33(i64 %a9314, i64 %arg10144, i64 %arg10143); call prim_vector_45set_33
  %cloptr12443 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12444 = getelementptr inbounds i64, i64* %cloptr12443, i64 0                  ; &cloptr12443[0]
  %f12445 = ptrtoint void(i64,i64,i64)* @lam10552 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12445, i64* %eptr12444                                                 ; store fptr
  %arg10148 = ptrtoint i64* %cloptr12443 to i64                                      ; closure cast; i64* -> i64
  %arg10147 = add i64 0, 0                                                           ; quoted ()
  %cloptr12446 = inttoptr i64 %arg10148 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12447 = getelementptr inbounds i64, i64* %cloptr12446, i64 0                 ; &cloptr12446[0]
  %f12449 = load i64, i64* %i0ptr12447, align 8                                      ; load; *i0ptr12447
  %fptr12448 = inttoptr i64 %f12449 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12448(i64 %arg10148, i64 %arg10147, i64 %retprim9422); tail call
  ret void
}


define void @lam10552(i64 %env10553, i64 %_950, i64 %x) {
  %_951 = call i64 @prim_halt(i64 %x)                                                ; call prim_halt
  %cloptr12450 = inttoptr i64 %_951 to i64*                                          ; closure/env cast; i64 -> i64*
  %i0ptr12451 = getelementptr inbounds i64, i64* %cloptr12450, i64 0                 ; &cloptr12450[0]
  %f12453 = load i64, i64* %i0ptr12451, align 8                                      ; load; *i0ptr12451
  %fptr12452 = inttoptr i64 %f12453 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12452(i64 %_951, i64 %_951)                         ; tail call
  ret void
}


define void @lam10542(i64 %env10543, i64 %cont9442, i64 %XiW$_37foldl) {
  %envptr12454 = inttoptr i64 %env10543 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12455 = getelementptr inbounds i64, i64* %envptr12454, i64 3                ; &envptr12454[3]
  %Aoo$_37foldr = load i64, i64* %envptr12455, align 8                               ; load; *envptr12455
  %envptr12456 = inttoptr i64 %env10543 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12457 = getelementptr inbounds i64, i64* %envptr12456, i64 2                ; &envptr12456[2]
  %yBl$_37map1 = load i64, i64* %envptr12457, align 8                                ; load; *envptr12457
  %envptr12458 = inttoptr i64 %env10543 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12459 = getelementptr inbounds i64, i64* %envptr12458, i64 1                ; &envptr12458[1]
  %RQ7$_37foldr1 = load i64, i64* %envptr12459, align 8                              ; load; *envptr12459
  %arg10153 = add i64 0, 0                                                           ; quoted ()
  %cloptr12460 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr12462 = getelementptr inbounds i64, i64* %cloptr12460, i64 1                  ; &eptr12462[1]
  %eptr12463 = getelementptr inbounds i64, i64* %cloptr12460, i64 2                  ; &eptr12463[2]
  %eptr12464 = getelementptr inbounds i64, i64* %cloptr12460, i64 3                  ; &eptr12464[3]
  %eptr12465 = getelementptr inbounds i64, i64* %cloptr12460, i64 4                  ; &eptr12465[4]
  store i64 %RQ7$_37foldr1, i64* %eptr12462                                          ; *eptr12462 = %RQ7$_37foldr1
  store i64 %yBl$_37map1, i64* %eptr12463                                            ; *eptr12463 = %yBl$_37map1
  store i64 %XiW$_37foldl, i64* %eptr12464                                           ; *eptr12464 = %XiW$_37foldl
  store i64 %Aoo$_37foldr, i64* %eptr12465                                           ; *eptr12465 = %Aoo$_37foldr
  %eptr12461 = getelementptr inbounds i64, i64* %cloptr12460, i64 0                  ; &cloptr12460[0]
  %f12466 = ptrtoint void(i64,i64)* @lam10539 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f12466, i64* %eptr12461                                                 ; store fptr
  %arg10152 = ptrtoint i64* %cloptr12460 to i64                                      ; closure cast; i64* -> i64
  %cloptr12467 = inttoptr i64 %cont9442 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12468 = getelementptr inbounds i64, i64* %cloptr12467, i64 0                 ; &cloptr12467[0]
  %f12470 = load i64, i64* %i0ptr12468, align 8                                      ; load; *i0ptr12468
  %fptr12469 = inttoptr i64 %f12470 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12469(i64 %cont9442, i64 %arg10153, i64 %arg10152)  ; tail call
  ret void
}


define void @lam10539(i64 %env10540, i64 %eal$args9444) {
  %envptr12471 = inttoptr i64 %env10540 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12472 = getelementptr inbounds i64, i64* %envptr12471, i64 4                ; &envptr12471[4]
  %Aoo$_37foldr = load i64, i64* %envptr12472, align 8                               ; load; *envptr12472
  %envptr12473 = inttoptr i64 %env10540 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12474 = getelementptr inbounds i64, i64* %envptr12473, i64 3                ; &envptr12473[3]
  %XiW$_37foldl = load i64, i64* %envptr12474, align 8                               ; load; *envptr12474
  %envptr12475 = inttoptr i64 %env10540 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12476 = getelementptr inbounds i64, i64* %envptr12475, i64 2                ; &envptr12475[2]
  %yBl$_37map1 = load i64, i64* %envptr12476, align 8                                ; load; *envptr12476
  %envptr12477 = inttoptr i64 %env10540 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12478 = getelementptr inbounds i64, i64* %envptr12477, i64 1                ; &envptr12477[1]
  %RQ7$_37foldr1 = load i64, i64* %envptr12478, align 8                              ; load; *envptr12478
  %cont9443 = call i64 @prim_car(i64 %eal$args9444)                                  ; call prim_car
  %eal$args = call i64 @prim_cdr(i64 %eal$args9444)                                  ; call prim_cdr
  %pIl$f = call i64 @prim_car(i64 %eal$args)                                         ; call prim_car
  %a9230 = call i64 @prim_cdr(i64 %eal$args)                                         ; call prim_cdr
  %retprim9463 = call i64 @prim_car(i64 %a9230)                                      ; call prim_car
  %cloptr12479 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr12481 = getelementptr inbounds i64, i64* %cloptr12479, i64 1                  ; &eptr12481[1]
  %eptr12482 = getelementptr inbounds i64, i64* %cloptr12479, i64 2                  ; &eptr12482[2]
  %eptr12483 = getelementptr inbounds i64, i64* %cloptr12479, i64 3                  ; &eptr12483[3]
  %eptr12484 = getelementptr inbounds i64, i64* %cloptr12479, i64 4                  ; &eptr12484[4]
  %eptr12485 = getelementptr inbounds i64, i64* %cloptr12479, i64 5                  ; &eptr12485[5]
  %eptr12486 = getelementptr inbounds i64, i64* %cloptr12479, i64 6                  ; &eptr12486[6]
  %eptr12487 = getelementptr inbounds i64, i64* %cloptr12479, i64 7                  ; &eptr12487[7]
  store i64 %RQ7$_37foldr1, i64* %eptr12481                                          ; *eptr12481 = %RQ7$_37foldr1
  store i64 %yBl$_37map1, i64* %eptr12482                                            ; *eptr12482 = %yBl$_37map1
  store i64 %XiW$_37foldl, i64* %eptr12483                                           ; *eptr12483 = %XiW$_37foldl
  store i64 %Aoo$_37foldr, i64* %eptr12484                                           ; *eptr12484 = %Aoo$_37foldr
  store i64 %eal$args, i64* %eptr12485                                               ; *eptr12485 = %eal$args
  store i64 %cont9443, i64* %eptr12486                                               ; *eptr12486 = %cont9443
  store i64 %pIl$f, i64* %eptr12487                                                  ; *eptr12487 = %pIl$f
  %eptr12480 = getelementptr inbounds i64, i64* %cloptr12479, i64 0                  ; &cloptr12479[0]
  %f12488 = ptrtoint void(i64,i64,i64)* @lam10537 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12488, i64* %eptr12480                                                 ; store fptr
  %arg10162 = ptrtoint i64* %cloptr12479 to i64                                      ; closure cast; i64* -> i64
  %arg10161 = add i64 0, 0                                                           ; quoted ()
  %cloptr12489 = inttoptr i64 %arg10162 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12490 = getelementptr inbounds i64, i64* %cloptr12489, i64 0                 ; &cloptr12489[0]
  %f12492 = load i64, i64* %i0ptr12490, align 8                                      ; load; *i0ptr12490
  %fptr12491 = inttoptr i64 %f12492 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12491(i64 %arg10162, i64 %arg10161, i64 %retprim9463); tail call
  ret void
}


define void @lam10537(i64 %env10538, i64 %_959445, i64 %Gkk$acc) {
  %envptr12493 = inttoptr i64 %env10538 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12494 = getelementptr inbounds i64, i64* %envptr12493, i64 7                ; &envptr12493[7]
  %pIl$f = load i64, i64* %envptr12494, align 8                                      ; load; *envptr12494
  %envptr12495 = inttoptr i64 %env10538 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12496 = getelementptr inbounds i64, i64* %envptr12495, i64 6                ; &envptr12495[6]
  %cont9443 = load i64, i64* %envptr12496, align 8                                   ; load; *envptr12496
  %envptr12497 = inttoptr i64 %env10538 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12498 = getelementptr inbounds i64, i64* %envptr12497, i64 5                ; &envptr12497[5]
  %eal$args = load i64, i64* %envptr12498, align 8                                   ; load; *envptr12498
  %envptr12499 = inttoptr i64 %env10538 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12500 = getelementptr inbounds i64, i64* %envptr12499, i64 4                ; &envptr12499[4]
  %Aoo$_37foldr = load i64, i64* %envptr12500, align 8                               ; load; *envptr12500
  %envptr12501 = inttoptr i64 %env10538 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12502 = getelementptr inbounds i64, i64* %envptr12501, i64 3                ; &envptr12501[3]
  %XiW$_37foldl = load i64, i64* %envptr12502, align 8                               ; load; *envptr12502
  %envptr12503 = inttoptr i64 %env10538 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12504 = getelementptr inbounds i64, i64* %envptr12503, i64 2                ; &envptr12503[2]
  %yBl$_37map1 = load i64, i64* %envptr12504, align 8                                ; load; *envptr12504
  %envptr12505 = inttoptr i64 %env10538 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12506 = getelementptr inbounds i64, i64* %envptr12505, i64 1                ; &envptr12505[1]
  %RQ7$_37foldr1 = load i64, i64* %envptr12506, align 8                              ; load; *envptr12506
  %a9231 = call i64 @prim_cdr(i64 %eal$args)                                         ; call prim_cdr
  %retprim9462 = call i64 @prim_cdr(i64 %a9231)                                      ; call prim_cdr
  %cloptr12507 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr12509 = getelementptr inbounds i64, i64* %cloptr12507, i64 1                  ; &eptr12509[1]
  %eptr12510 = getelementptr inbounds i64, i64* %cloptr12507, i64 2                  ; &eptr12510[2]
  %eptr12511 = getelementptr inbounds i64, i64* %cloptr12507, i64 3                  ; &eptr12511[3]
  %eptr12512 = getelementptr inbounds i64, i64* %cloptr12507, i64 4                  ; &eptr12512[4]
  %eptr12513 = getelementptr inbounds i64, i64* %cloptr12507, i64 5                  ; &eptr12513[5]
  %eptr12514 = getelementptr inbounds i64, i64* %cloptr12507, i64 6                  ; &eptr12514[6]
  %eptr12515 = getelementptr inbounds i64, i64* %cloptr12507, i64 7                  ; &eptr12515[7]
  store i64 %RQ7$_37foldr1, i64* %eptr12509                                          ; *eptr12509 = %RQ7$_37foldr1
  store i64 %yBl$_37map1, i64* %eptr12510                                            ; *eptr12510 = %yBl$_37map1
  store i64 %XiW$_37foldl, i64* %eptr12511                                           ; *eptr12511 = %XiW$_37foldl
  store i64 %Aoo$_37foldr, i64* %eptr12512                                           ; *eptr12512 = %Aoo$_37foldr
  store i64 %Gkk$acc, i64* %eptr12513                                                ; *eptr12513 = %Gkk$acc
  store i64 %cont9443, i64* %eptr12514                                               ; *eptr12514 = %cont9443
  store i64 %pIl$f, i64* %eptr12515                                                  ; *eptr12515 = %pIl$f
  %eptr12508 = getelementptr inbounds i64, i64* %cloptr12507, i64 0                  ; &cloptr12507[0]
  %f12516 = ptrtoint void(i64,i64,i64)* @lam10535 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12516, i64* %eptr12508                                                 ; store fptr
  %arg10167 = ptrtoint i64* %cloptr12507 to i64                                      ; closure cast; i64* -> i64
  %arg10166 = add i64 0, 0                                                           ; quoted ()
  %cloptr12517 = inttoptr i64 %arg10167 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12518 = getelementptr inbounds i64, i64* %cloptr12517, i64 0                 ; &cloptr12517[0]
  %f12520 = load i64, i64* %i0ptr12518, align 8                                      ; load; *i0ptr12518
  %fptr12519 = inttoptr i64 %f12520 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12519(i64 %arg10167, i64 %arg10166, i64 %retprim9462); tail call
  ret void
}


define void @lam10535(i64 %env10536, i64 %_959446, i64 %HQe$lsts) {
  %envptr12521 = inttoptr i64 %env10536 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12522 = getelementptr inbounds i64, i64* %envptr12521, i64 7                ; &envptr12521[7]
  %pIl$f = load i64, i64* %envptr12522, align 8                                      ; load; *envptr12522
  %envptr12523 = inttoptr i64 %env10536 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12524 = getelementptr inbounds i64, i64* %envptr12523, i64 6                ; &envptr12523[6]
  %cont9443 = load i64, i64* %envptr12524, align 8                                   ; load; *envptr12524
  %envptr12525 = inttoptr i64 %env10536 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12526 = getelementptr inbounds i64, i64* %envptr12525, i64 5                ; &envptr12525[5]
  %Gkk$acc = load i64, i64* %envptr12526, align 8                                    ; load; *envptr12526
  %envptr12527 = inttoptr i64 %env10536 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12528 = getelementptr inbounds i64, i64* %envptr12527, i64 4                ; &envptr12527[4]
  %Aoo$_37foldr = load i64, i64* %envptr12528, align 8                               ; load; *envptr12528
  %envptr12529 = inttoptr i64 %env10536 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12530 = getelementptr inbounds i64, i64* %envptr12529, i64 3                ; &envptr12529[3]
  %XiW$_37foldl = load i64, i64* %envptr12530, align 8                               ; load; *envptr12530
  %envptr12531 = inttoptr i64 %env10536 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12532 = getelementptr inbounds i64, i64* %envptr12531, i64 2                ; &envptr12531[2]
  %yBl$_37map1 = load i64, i64* %envptr12532, align 8                                ; load; *envptr12532
  %envptr12533 = inttoptr i64 %env10536 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12534 = getelementptr inbounds i64, i64* %envptr12533, i64 1                ; &envptr12533[1]
  %RQ7$_37foldr1 = load i64, i64* %envptr12534, align 8                              ; load; *envptr12534
  %cloptr12535 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr12537 = getelementptr inbounds i64, i64* %cloptr12535, i64 1                  ; &eptr12537[1]
  %eptr12538 = getelementptr inbounds i64, i64* %cloptr12535, i64 2                  ; &eptr12538[2]
  %eptr12539 = getelementptr inbounds i64, i64* %cloptr12535, i64 3                  ; &eptr12539[3]
  %eptr12540 = getelementptr inbounds i64, i64* %cloptr12535, i64 4                  ; &eptr12540[4]
  %eptr12541 = getelementptr inbounds i64, i64* %cloptr12535, i64 5                  ; &eptr12541[5]
  %eptr12542 = getelementptr inbounds i64, i64* %cloptr12535, i64 6                  ; &eptr12542[6]
  %eptr12543 = getelementptr inbounds i64, i64* %cloptr12535, i64 7                  ; &eptr12543[7]
  store i64 %HQe$lsts, i64* %eptr12537                                               ; *eptr12537 = %HQe$lsts
  store i64 %yBl$_37map1, i64* %eptr12538                                            ; *eptr12538 = %yBl$_37map1
  store i64 %XiW$_37foldl, i64* %eptr12539                                           ; *eptr12539 = %XiW$_37foldl
  store i64 %Aoo$_37foldr, i64* %eptr12540                                           ; *eptr12540 = %Aoo$_37foldr
  store i64 %Gkk$acc, i64* %eptr12541                                                ; *eptr12541 = %Gkk$acc
  store i64 %cont9443, i64* %eptr12542                                               ; *eptr12542 = %cont9443
  store i64 %pIl$f, i64* %eptr12543                                                  ; *eptr12543 = %pIl$f
  %eptr12536 = getelementptr inbounds i64, i64* %cloptr12535, i64 0                  ; &cloptr12535[0]
  %f12544 = ptrtoint void(i64,i64,i64)* @lam10533 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12544, i64* %eptr12536                                                 ; store fptr
  %arg10171 = ptrtoint i64* %cloptr12535 to i64                                      ; closure cast; i64* -> i64
  %cloptr12545 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12546 = getelementptr inbounds i64, i64* %cloptr12545, i64 0                  ; &cloptr12545[0]
  %f12547 = ptrtoint void(i64,i64,i64,i64)* @lam10512 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f12547, i64* %eptr12546                                                 ; store fptr
  %arg10170 = ptrtoint i64* %cloptr12545 to i64                                      ; closure cast; i64* -> i64
  %arg10169 = call i64 @const_init_false()                                           ; quoted #f
  %cloptr12548 = inttoptr i64 %RQ7$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr12549 = getelementptr inbounds i64, i64* %cloptr12548, i64 0                 ; &cloptr12548[0]
  %f12551 = load i64, i64* %i0ptr12549, align 8                                      ; load; *i0ptr12549
  %fptr12550 = inttoptr i64 %f12551 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12550(i64 %RQ7$_37foldr1, i64 %arg10171, i64 %arg10170, i64 %arg10169, i64 %HQe$lsts); tail call
  ret void
}


define void @lam10533(i64 %env10534, i64 %_959447, i64 %a9232) {
  %envptr12552 = inttoptr i64 %env10534 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12553 = getelementptr inbounds i64, i64* %envptr12552, i64 7                ; &envptr12552[7]
  %pIl$f = load i64, i64* %envptr12553, align 8                                      ; load; *envptr12553
  %envptr12554 = inttoptr i64 %env10534 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12555 = getelementptr inbounds i64, i64* %envptr12554, i64 6                ; &envptr12554[6]
  %cont9443 = load i64, i64* %envptr12555, align 8                                   ; load; *envptr12555
  %envptr12556 = inttoptr i64 %env10534 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12557 = getelementptr inbounds i64, i64* %envptr12556, i64 5                ; &envptr12556[5]
  %Gkk$acc = load i64, i64* %envptr12557, align 8                                    ; load; *envptr12557
  %envptr12558 = inttoptr i64 %env10534 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12559 = getelementptr inbounds i64, i64* %envptr12558, i64 4                ; &envptr12558[4]
  %Aoo$_37foldr = load i64, i64* %envptr12559, align 8                               ; load; *envptr12559
  %envptr12560 = inttoptr i64 %env10534 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12561 = getelementptr inbounds i64, i64* %envptr12560, i64 3                ; &envptr12560[3]
  %XiW$_37foldl = load i64, i64* %envptr12561, align 8                               ; load; *envptr12561
  %envptr12562 = inttoptr i64 %env10534 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12563 = getelementptr inbounds i64, i64* %envptr12562, i64 2                ; &envptr12562[2]
  %yBl$_37map1 = load i64, i64* %envptr12563, align 8                                ; load; *envptr12563
  %envptr12564 = inttoptr i64 %env10534 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12565 = getelementptr inbounds i64, i64* %envptr12564, i64 1                ; &envptr12564[1]
  %HQe$lsts = load i64, i64* %envptr12565, align 8                                   ; load; *envptr12565
  %cmp12566 = icmp eq i64 %a9232, 15                                                 ; false?
  br i1 %cmp12566, label %else12568, label %then12567                                ; if

then12567:
  %arg10174 = add i64 0, 0                                                           ; quoted ()
  %cloptr12569 = inttoptr i64 %cont9443 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12570 = getelementptr inbounds i64, i64* %cloptr12569, i64 0                 ; &cloptr12569[0]
  %f12572 = load i64, i64* %i0ptr12570, align 8                                      ; load; *i0ptr12570
  %fptr12571 = inttoptr i64 %f12572 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12571(i64 %cont9443, i64 %arg10174, i64 %Gkk$acc)   ; tail call
  ret void

else12568:
  %cloptr12573 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr12575 = getelementptr inbounds i64, i64* %cloptr12573, i64 1                  ; &eptr12575[1]
  %eptr12576 = getelementptr inbounds i64, i64* %cloptr12573, i64 2                  ; &eptr12576[2]
  %eptr12577 = getelementptr inbounds i64, i64* %cloptr12573, i64 3                  ; &eptr12577[3]
  %eptr12578 = getelementptr inbounds i64, i64* %cloptr12573, i64 4                  ; &eptr12578[4]
  %eptr12579 = getelementptr inbounds i64, i64* %cloptr12573, i64 5                  ; &eptr12579[5]
  %eptr12580 = getelementptr inbounds i64, i64* %cloptr12573, i64 6                  ; &eptr12580[6]
  %eptr12581 = getelementptr inbounds i64, i64* %cloptr12573, i64 7                  ; &eptr12581[7]
  store i64 %HQe$lsts, i64* %eptr12575                                               ; *eptr12575 = %HQe$lsts
  store i64 %yBl$_37map1, i64* %eptr12576                                            ; *eptr12576 = %yBl$_37map1
  store i64 %XiW$_37foldl, i64* %eptr12577                                           ; *eptr12577 = %XiW$_37foldl
  store i64 %Aoo$_37foldr, i64* %eptr12578                                           ; *eptr12578 = %Aoo$_37foldr
  store i64 %Gkk$acc, i64* %eptr12579                                                ; *eptr12579 = %Gkk$acc
  store i64 %cont9443, i64* %eptr12580                                               ; *eptr12580 = %cont9443
  store i64 %pIl$f, i64* %eptr12581                                                  ; *eptr12581 = %pIl$f
  %eptr12574 = getelementptr inbounds i64, i64* %cloptr12573, i64 0                  ; &cloptr12573[0]
  %f12582 = ptrtoint void(i64,i64,i64)* @lam10531 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12582, i64* %eptr12574                                                 ; store fptr
  %arg10178 = ptrtoint i64* %cloptr12573 to i64                                      ; closure cast; i64* -> i64
  %cloptr12583 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12584 = getelementptr inbounds i64, i64* %cloptr12583, i64 0                  ; &cloptr12583[0]
  %f12585 = ptrtoint void(i64,i64,i64)* @lam10516 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12585, i64* %eptr12584                                                 ; store fptr
  %arg10177 = ptrtoint i64* %cloptr12583 to i64                                      ; closure cast; i64* -> i64
  %cloptr12586 = inttoptr i64 %yBl$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr12587 = getelementptr inbounds i64, i64* %cloptr12586, i64 0                 ; &cloptr12586[0]
  %f12589 = load i64, i64* %i0ptr12587, align 8                                      ; load; *i0ptr12587
  %fptr12588 = inttoptr i64 %f12589 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12588(i64 %yBl$_37map1, i64 %arg10178, i64 %arg10177, i64 %HQe$lsts); tail call
  ret void
}


define void @lam10531(i64 %env10532, i64 %_959448, i64 %oeO$lsts_43) {
  %envptr12590 = inttoptr i64 %env10532 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12591 = getelementptr inbounds i64, i64* %envptr12590, i64 7                ; &envptr12590[7]
  %pIl$f = load i64, i64* %envptr12591, align 8                                      ; load; *envptr12591
  %envptr12592 = inttoptr i64 %env10532 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12593 = getelementptr inbounds i64, i64* %envptr12592, i64 6                ; &envptr12592[6]
  %cont9443 = load i64, i64* %envptr12593, align 8                                   ; load; *envptr12593
  %envptr12594 = inttoptr i64 %env10532 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12595 = getelementptr inbounds i64, i64* %envptr12594, i64 5                ; &envptr12594[5]
  %Gkk$acc = load i64, i64* %envptr12595, align 8                                    ; load; *envptr12595
  %envptr12596 = inttoptr i64 %env10532 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12597 = getelementptr inbounds i64, i64* %envptr12596, i64 4                ; &envptr12596[4]
  %Aoo$_37foldr = load i64, i64* %envptr12597, align 8                               ; load; *envptr12597
  %envptr12598 = inttoptr i64 %env10532 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12599 = getelementptr inbounds i64, i64* %envptr12598, i64 3                ; &envptr12598[3]
  %XiW$_37foldl = load i64, i64* %envptr12599, align 8                               ; load; *envptr12599
  %envptr12600 = inttoptr i64 %env10532 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12601 = getelementptr inbounds i64, i64* %envptr12600, i64 2                ; &envptr12600[2]
  %yBl$_37map1 = load i64, i64* %envptr12601, align 8                                ; load; *envptr12601
  %envptr12602 = inttoptr i64 %env10532 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12603 = getelementptr inbounds i64, i64* %envptr12602, i64 1                ; &envptr12602[1]
  %HQe$lsts = load i64, i64* %envptr12603, align 8                                   ; load; *envptr12603
  %cloptr12604 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr12606 = getelementptr inbounds i64, i64* %cloptr12604, i64 1                  ; &eptr12606[1]
  %eptr12607 = getelementptr inbounds i64, i64* %cloptr12604, i64 2                  ; &eptr12607[2]
  %eptr12608 = getelementptr inbounds i64, i64* %cloptr12604, i64 3                  ; &eptr12608[3]
  %eptr12609 = getelementptr inbounds i64, i64* %cloptr12604, i64 4                  ; &eptr12609[4]
  %eptr12610 = getelementptr inbounds i64, i64* %cloptr12604, i64 5                  ; &eptr12610[5]
  %eptr12611 = getelementptr inbounds i64, i64* %cloptr12604, i64 6                  ; &eptr12611[6]
  store i64 %oeO$lsts_43, i64* %eptr12606                                            ; *eptr12606 = %oeO$lsts_43
  store i64 %XiW$_37foldl, i64* %eptr12607                                           ; *eptr12607 = %XiW$_37foldl
  store i64 %Aoo$_37foldr, i64* %eptr12608                                           ; *eptr12608 = %Aoo$_37foldr
  store i64 %Gkk$acc, i64* %eptr12609                                                ; *eptr12609 = %Gkk$acc
  store i64 %cont9443, i64* %eptr12610                                               ; *eptr12610 = %cont9443
  store i64 %pIl$f, i64* %eptr12611                                                  ; *eptr12611 = %pIl$f
  %eptr12605 = getelementptr inbounds i64, i64* %cloptr12604, i64 0                  ; &cloptr12604[0]
  %f12612 = ptrtoint void(i64,i64,i64)* @lam10529 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12612, i64* %eptr12605                                                 ; store fptr
  %arg10182 = ptrtoint i64* %cloptr12604 to i64                                      ; closure cast; i64* -> i64
  %cloptr12613 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12614 = getelementptr inbounds i64, i64* %cloptr12613, i64 0                  ; &cloptr12613[0]
  %f12615 = ptrtoint void(i64,i64,i64)* @lam10519 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12615, i64* %eptr12614                                                 ; store fptr
  %arg10181 = ptrtoint i64* %cloptr12613 to i64                                      ; closure cast; i64* -> i64
  %cloptr12616 = inttoptr i64 %yBl$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr12617 = getelementptr inbounds i64, i64* %cloptr12616, i64 0                 ; &cloptr12616[0]
  %f12619 = load i64, i64* %i0ptr12617, align 8                                      ; load; *i0ptr12617
  %fptr12618 = inttoptr i64 %f12619 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12618(i64 %yBl$_37map1, i64 %arg10182, i64 %arg10181, i64 %HQe$lsts); tail call
  ret void
}


define void @lam10529(i64 %env10530, i64 %_959449, i64 %nx4$vs) {
  %envptr12620 = inttoptr i64 %env10530 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12621 = getelementptr inbounds i64, i64* %envptr12620, i64 6                ; &envptr12620[6]
  %pIl$f = load i64, i64* %envptr12621, align 8                                      ; load; *envptr12621
  %envptr12622 = inttoptr i64 %env10530 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12623 = getelementptr inbounds i64, i64* %envptr12622, i64 5                ; &envptr12622[5]
  %cont9443 = load i64, i64* %envptr12623, align 8                                   ; load; *envptr12623
  %envptr12624 = inttoptr i64 %env10530 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12625 = getelementptr inbounds i64, i64* %envptr12624, i64 4                ; &envptr12624[4]
  %Gkk$acc = load i64, i64* %envptr12625, align 8                                    ; load; *envptr12625
  %envptr12626 = inttoptr i64 %env10530 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12627 = getelementptr inbounds i64, i64* %envptr12626, i64 3                ; &envptr12626[3]
  %Aoo$_37foldr = load i64, i64* %envptr12627, align 8                               ; load; *envptr12627
  %envptr12628 = inttoptr i64 %env10530 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12629 = getelementptr inbounds i64, i64* %envptr12628, i64 2                ; &envptr12628[2]
  %XiW$_37foldl = load i64, i64* %envptr12629, align 8                               ; load; *envptr12629
  %envptr12630 = inttoptr i64 %env10530 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12631 = getelementptr inbounds i64, i64* %envptr12630, i64 1                ; &envptr12630[1]
  %oeO$lsts_43 = load i64, i64* %envptr12631, align 8                                ; load; *envptr12631
  %arg10184 = add i64 0, 0                                                           ; quoted ()
  %a9233 = call i64 @prim_cons(i64 %Gkk$acc, i64 %arg10184)                          ; call prim_cons
  %cloptr12632 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr12634 = getelementptr inbounds i64, i64* %cloptr12632, i64 1                  ; &eptr12634[1]
  %eptr12635 = getelementptr inbounds i64, i64* %cloptr12632, i64 2                  ; &eptr12635[2]
  %eptr12636 = getelementptr inbounds i64, i64* %cloptr12632, i64 3                  ; &eptr12636[3]
  %eptr12637 = getelementptr inbounds i64, i64* %cloptr12632, i64 4                  ; &eptr12637[4]
  store i64 %oeO$lsts_43, i64* %eptr12634                                            ; *eptr12634 = %oeO$lsts_43
  store i64 %XiW$_37foldl, i64* %eptr12635                                           ; *eptr12635 = %XiW$_37foldl
  store i64 %cont9443, i64* %eptr12636                                               ; *eptr12636 = %cont9443
  store i64 %pIl$f, i64* %eptr12637                                                  ; *eptr12637 = %pIl$f
  %eptr12633 = getelementptr inbounds i64, i64* %cloptr12632, i64 0                  ; &cloptr12632[0]
  %f12638 = ptrtoint void(i64,i64,i64)* @lam10526 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12638, i64* %eptr12633                                                 ; store fptr
  %arg10189 = ptrtoint i64* %cloptr12632 to i64                                      ; closure cast; i64* -> i64
  %cloptr12639 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12640 = getelementptr inbounds i64, i64* %cloptr12639, i64 0                  ; &cloptr12639[0]
  %f12641 = ptrtoint void(i64,i64,i64,i64)* @lam10522 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f12641, i64* %eptr12640                                                 ; store fptr
  %arg10188 = ptrtoint i64* %cloptr12639 to i64                                      ; closure cast; i64* -> i64
  %cloptr12642 = inttoptr i64 %Aoo$_37foldr to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr12643 = getelementptr inbounds i64, i64* %cloptr12642, i64 0                 ; &cloptr12642[0]
  %f12645 = load i64, i64* %i0ptr12643, align 8                                      ; load; *i0ptr12643
  %fptr12644 = inttoptr i64 %f12645 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12644(i64 %Aoo$_37foldr, i64 %arg10189, i64 %arg10188, i64 %a9233, i64 %nx4$vs); tail call
  ret void
}


define void @lam10526(i64 %env10527, i64 %_959452, i64 %a9234) {
  %envptr12646 = inttoptr i64 %env10527 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12647 = getelementptr inbounds i64, i64* %envptr12646, i64 4                ; &envptr12646[4]
  %pIl$f = load i64, i64* %envptr12647, align 8                                      ; load; *envptr12647
  %envptr12648 = inttoptr i64 %env10527 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12649 = getelementptr inbounds i64, i64* %envptr12648, i64 3                ; &envptr12648[3]
  %cont9443 = load i64, i64* %envptr12649, align 8                                   ; load; *envptr12649
  %envptr12650 = inttoptr i64 %env10527 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12651 = getelementptr inbounds i64, i64* %envptr12650, i64 2                ; &envptr12650[2]
  %XiW$_37foldl = load i64, i64* %envptr12651, align 8                               ; load; *envptr12651
  %envptr12652 = inttoptr i64 %env10527 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12653 = getelementptr inbounds i64, i64* %envptr12652, i64 1                ; &envptr12652[1]
  %oeO$lsts_43 = load i64, i64* %envptr12653, align 8                                ; load; *envptr12653
  %cloptr12654 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr12656 = getelementptr inbounds i64, i64* %cloptr12654, i64 1                  ; &eptr12656[1]
  %eptr12657 = getelementptr inbounds i64, i64* %cloptr12654, i64 2                  ; &eptr12657[2]
  %eptr12658 = getelementptr inbounds i64, i64* %cloptr12654, i64 3                  ; &eptr12658[3]
  %eptr12659 = getelementptr inbounds i64, i64* %cloptr12654, i64 4                  ; &eptr12659[4]
  store i64 %oeO$lsts_43, i64* %eptr12656                                            ; *eptr12656 = %oeO$lsts_43
  store i64 %XiW$_37foldl, i64* %eptr12657                                           ; *eptr12657 = %XiW$_37foldl
  store i64 %cont9443, i64* %eptr12658                                               ; *eptr12658 = %cont9443
  store i64 %pIl$f, i64* %eptr12659                                                  ; *eptr12659 = %pIl$f
  %eptr12655 = getelementptr inbounds i64, i64* %cloptr12654, i64 0                  ; &cloptr12654[0]
  %f12660 = ptrtoint void(i64,i64,i64)* @lam10524 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12660, i64* %eptr12655                                                 ; store fptr
  %arg10192 = ptrtoint i64* %cloptr12654 to i64                                      ; closure cast; i64* -> i64
  %cps_45lst9453 = call i64 @prim_cons(i64 %arg10192, i64 %a9234)                    ; call prim_cons
  %cloptr12661 = inttoptr i64 %pIl$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12662 = getelementptr inbounds i64, i64* %cloptr12661, i64 0                 ; &cloptr12661[0]
  %f12664 = load i64, i64* %i0ptr12662, align 8                                      ; load; *i0ptr12662
  %fptr12663 = inttoptr i64 %f12664 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12663(i64 %pIl$f, i64 %cps_45lst9453)               ; tail call
  ret void
}


define void @lam10524(i64 %env10525, i64 %_959450, i64 %HY0$acc_43) {
  %envptr12665 = inttoptr i64 %env10525 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12666 = getelementptr inbounds i64, i64* %envptr12665, i64 4                ; &envptr12665[4]
  %pIl$f = load i64, i64* %envptr12666, align 8                                      ; load; *envptr12666
  %envptr12667 = inttoptr i64 %env10525 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12668 = getelementptr inbounds i64, i64* %envptr12667, i64 3                ; &envptr12667[3]
  %cont9443 = load i64, i64* %envptr12668, align 8                                   ; load; *envptr12668
  %envptr12669 = inttoptr i64 %env10525 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12670 = getelementptr inbounds i64, i64* %envptr12669, i64 2                ; &envptr12669[2]
  %XiW$_37foldl = load i64, i64* %envptr12670, align 8                               ; load; *envptr12670
  %envptr12671 = inttoptr i64 %env10525 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12672 = getelementptr inbounds i64, i64* %envptr12671, i64 1                ; &envptr12671[1]
  %oeO$lsts_43 = load i64, i64* %envptr12672, align 8                                ; load; *envptr12672
  %a9235 = call i64 @prim_cons(i64 %HY0$acc_43, i64 %oeO$lsts_43)                    ; call prim_cons
  %a9236 = call i64 @prim_cons(i64 %pIl$f, i64 %a9235)                               ; call prim_cons
  %cps_45lst9451 = call i64 @prim_cons(i64 %cont9443, i64 %a9236)                    ; call prim_cons
  %cloptr12673 = inttoptr i64 %XiW$_37foldl to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr12674 = getelementptr inbounds i64, i64* %cloptr12673, i64 0                 ; &cloptr12673[0]
  %f12676 = load i64, i64* %i0ptr12674, align 8                                      ; load; *i0ptr12674
  %fptr12675 = inttoptr i64 %f12676 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12675(i64 %XiW$_37foldl, i64 %cps_45lst9451)        ; tail call
  ret void
}


define void @lam10522(i64 %env10523, i64 %cont9454, i64 %Paq$a, i64 %to9$b) {
  %retprim9455 = call i64 @prim_cons(i64 %Paq$a, i64 %to9$b)                         ; call prim_cons
  %arg10202 = add i64 0, 0                                                           ; quoted ()
  %cloptr12677 = inttoptr i64 %cont9454 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12678 = getelementptr inbounds i64, i64* %cloptr12677, i64 0                 ; &cloptr12677[0]
  %f12680 = load i64, i64* %i0ptr12678, align 8                                      ; load; *i0ptr12678
  %fptr12679 = inttoptr i64 %f12680 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12679(i64 %cont9454, i64 %arg10202, i64 %retprim9455); tail call
  ret void
}


define void @lam10519(i64 %env10520, i64 %cont9456, i64 %WnD$x) {
  %retprim9457 = call i64 @prim_car(i64 %WnD$x)                                      ; call prim_car
  %arg10206 = add i64 0, 0                                                           ; quoted ()
  %cloptr12681 = inttoptr i64 %cont9456 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12682 = getelementptr inbounds i64, i64* %cloptr12681, i64 0                 ; &cloptr12681[0]
  %f12684 = load i64, i64* %i0ptr12682, align 8                                      ; load; *i0ptr12682
  %fptr12683 = inttoptr i64 %f12684 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12683(i64 %cont9456, i64 %arg10206, i64 %retprim9457); tail call
  ret void
}


define void @lam10516(i64 %env10517, i64 %cont9458, i64 %cDK$x) {
  %retprim9459 = call i64 @prim_cdr(i64 %cDK$x)                                      ; call prim_cdr
  %arg10210 = add i64 0, 0                                                           ; quoted ()
  %cloptr12685 = inttoptr i64 %cont9458 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12686 = getelementptr inbounds i64, i64* %cloptr12685, i64 0                 ; &cloptr12685[0]
  %f12688 = load i64, i64* %i0ptr12686, align 8                                      ; load; *i0ptr12686
  %fptr12687 = inttoptr i64 %f12688 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12687(i64 %cont9458, i64 %arg10210, i64 %retprim9459); tail call
  ret void
}


define void @lam10512(i64 %env10513, i64 %cont9460, i64 %Zwh$lst, i64 %tQp$b) {
  %cmp12689 = icmp eq i64 %tQp$b, 15                                                 ; false?
  br i1 %cmp12689, label %else12691, label %then12690                                ; if

then12690:
  %arg10213 = add i64 0, 0                                                           ; quoted ()
  %cloptr12692 = inttoptr i64 %cont9460 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12693 = getelementptr inbounds i64, i64* %cloptr12692, i64 0                 ; &cloptr12692[0]
  %f12695 = load i64, i64* %i0ptr12693, align 8                                      ; load; *i0ptr12693
  %fptr12694 = inttoptr i64 %f12695 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12694(i64 %cont9460, i64 %arg10213, i64 %tQp$b)     ; tail call
  ret void

else12691:
  %retprim9461 = call i64 @prim_null_63(i64 %Zwh$lst)                                ; call prim_null_63
  %arg10217 = add i64 0, 0                                                           ; quoted ()
  %cloptr12696 = inttoptr i64 %cont9460 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12697 = getelementptr inbounds i64, i64* %cloptr12696, i64 0                 ; &cloptr12696[0]
  %f12699 = load i64, i64* %i0ptr12697, align 8                                      ; load; *i0ptr12697
  %fptr12698 = inttoptr i64 %f12699 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12698(i64 %cont9460, i64 %arg10217, i64 %retprim9461); tail call
  ret void
}


define void @lam10505(i64 %env10506, i64 %cont9464, i64 %yPf$_37foldr) {
  %envptr12700 = inttoptr i64 %env10506 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12701 = getelementptr inbounds i64, i64* %envptr12700, i64 2                ; &envptr12700[2]
  %nad$_37map1 = load i64, i64* %envptr12701, align 8                                ; load; *envptr12701
  %envptr12702 = inttoptr i64 %env10506 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12703 = getelementptr inbounds i64, i64* %envptr12702, i64 1                ; &envptr12702[1]
  %RQ7$_37foldr1 = load i64, i64* %envptr12703, align 8                              ; load; *envptr12703
  %arg10220 = add i64 0, 0                                                           ; quoted ()
  %cloptr12704 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr12706 = getelementptr inbounds i64, i64* %cloptr12704, i64 1                  ; &eptr12706[1]
  %eptr12707 = getelementptr inbounds i64, i64* %cloptr12704, i64 2                  ; &eptr12707[2]
  %eptr12708 = getelementptr inbounds i64, i64* %cloptr12704, i64 3                  ; &eptr12708[3]
  store i64 %yPf$_37foldr, i64* %eptr12706                                           ; *eptr12706 = %yPf$_37foldr
  store i64 %RQ7$_37foldr1, i64* %eptr12707                                          ; *eptr12707 = %RQ7$_37foldr1
  store i64 %nad$_37map1, i64* %eptr12708                                            ; *eptr12708 = %nad$_37map1
  %eptr12705 = getelementptr inbounds i64, i64* %cloptr12704, i64 0                  ; &cloptr12704[0]
  %f12709 = ptrtoint void(i64,i64)* @lam10502 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f12709, i64* %eptr12705                                                 ; store fptr
  %arg10219 = ptrtoint i64* %cloptr12704 to i64                                      ; closure cast; i64* -> i64
  %cloptr12710 = inttoptr i64 %cont9464 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12711 = getelementptr inbounds i64, i64* %cloptr12710, i64 0                 ; &cloptr12710[0]
  %f12713 = load i64, i64* %i0ptr12711, align 8                                      ; load; *i0ptr12711
  %fptr12712 = inttoptr i64 %f12713 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12712(i64 %cont9464, i64 %arg10220, i64 %arg10219)  ; tail call
  ret void
}


define void @lam10502(i64 %env10503, i64 %BSj$args9466) {
  %envptr12714 = inttoptr i64 %env10503 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12715 = getelementptr inbounds i64, i64* %envptr12714, i64 3                ; &envptr12714[3]
  %nad$_37map1 = load i64, i64* %envptr12715, align 8                                ; load; *envptr12715
  %envptr12716 = inttoptr i64 %env10503 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12717 = getelementptr inbounds i64, i64* %envptr12716, i64 2                ; &envptr12716[2]
  %RQ7$_37foldr1 = load i64, i64* %envptr12717, align 8                              ; load; *envptr12717
  %envptr12718 = inttoptr i64 %env10503 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12719 = getelementptr inbounds i64, i64* %envptr12718, i64 1                ; &envptr12718[1]
  %yPf$_37foldr = load i64, i64* %envptr12719, align 8                               ; load; *envptr12719
  %cont9465 = call i64 @prim_car(i64 %BSj$args9466)                                  ; call prim_car
  %BSj$args = call i64 @prim_cdr(i64 %BSj$args9466)                                  ; call prim_cdr
  %tNI$f = call i64 @prim_car(i64 %BSj$args)                                         ; call prim_car
  %a9216 = call i64 @prim_cdr(i64 %BSj$args)                                         ; call prim_cdr
  %retprim9485 = call i64 @prim_car(i64 %a9216)                                      ; call prim_car
  %cloptr12720 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr12722 = getelementptr inbounds i64, i64* %cloptr12720, i64 1                  ; &eptr12722[1]
  %eptr12723 = getelementptr inbounds i64, i64* %cloptr12720, i64 2                  ; &eptr12723[2]
  %eptr12724 = getelementptr inbounds i64, i64* %cloptr12720, i64 3                  ; &eptr12724[3]
  %eptr12725 = getelementptr inbounds i64, i64* %cloptr12720, i64 4                  ; &eptr12725[4]
  %eptr12726 = getelementptr inbounds i64, i64* %cloptr12720, i64 5                  ; &eptr12726[5]
  %eptr12727 = getelementptr inbounds i64, i64* %cloptr12720, i64 6                  ; &eptr12727[6]
  store i64 %BSj$args, i64* %eptr12722                                               ; *eptr12722 = %BSj$args
  store i64 %yPf$_37foldr, i64* %eptr12723                                           ; *eptr12723 = %yPf$_37foldr
  store i64 %RQ7$_37foldr1, i64* %eptr12724                                          ; *eptr12724 = %RQ7$_37foldr1
  store i64 %nad$_37map1, i64* %eptr12725                                            ; *eptr12725 = %nad$_37map1
  store i64 %cont9465, i64* %eptr12726                                               ; *eptr12726 = %cont9465
  store i64 %tNI$f, i64* %eptr12727                                                  ; *eptr12727 = %tNI$f
  %eptr12721 = getelementptr inbounds i64, i64* %cloptr12720, i64 0                  ; &cloptr12720[0]
  %f12728 = ptrtoint void(i64,i64,i64)* @lam10500 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12728, i64* %eptr12721                                                 ; store fptr
  %arg10229 = ptrtoint i64* %cloptr12720 to i64                                      ; closure cast; i64* -> i64
  %arg10228 = add i64 0, 0                                                           ; quoted ()
  %cloptr12729 = inttoptr i64 %arg10229 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12730 = getelementptr inbounds i64, i64* %cloptr12729, i64 0                 ; &cloptr12729[0]
  %f12732 = load i64, i64* %i0ptr12730, align 8                                      ; load; *i0ptr12730
  %fptr12731 = inttoptr i64 %f12732 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12731(i64 %arg10229, i64 %arg10228, i64 %retprim9485); tail call
  ret void
}


define void @lam10500(i64 %env10501, i64 %_959467, i64 %Udy$acc) {
  %envptr12733 = inttoptr i64 %env10501 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12734 = getelementptr inbounds i64, i64* %envptr12733, i64 6                ; &envptr12733[6]
  %tNI$f = load i64, i64* %envptr12734, align 8                                      ; load; *envptr12734
  %envptr12735 = inttoptr i64 %env10501 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12736 = getelementptr inbounds i64, i64* %envptr12735, i64 5                ; &envptr12735[5]
  %cont9465 = load i64, i64* %envptr12736, align 8                                   ; load; *envptr12736
  %envptr12737 = inttoptr i64 %env10501 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12738 = getelementptr inbounds i64, i64* %envptr12737, i64 4                ; &envptr12737[4]
  %nad$_37map1 = load i64, i64* %envptr12738, align 8                                ; load; *envptr12738
  %envptr12739 = inttoptr i64 %env10501 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12740 = getelementptr inbounds i64, i64* %envptr12739, i64 3                ; &envptr12739[3]
  %RQ7$_37foldr1 = load i64, i64* %envptr12740, align 8                              ; load; *envptr12740
  %envptr12741 = inttoptr i64 %env10501 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12742 = getelementptr inbounds i64, i64* %envptr12741, i64 2                ; &envptr12741[2]
  %yPf$_37foldr = load i64, i64* %envptr12742, align 8                               ; load; *envptr12742
  %envptr12743 = inttoptr i64 %env10501 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12744 = getelementptr inbounds i64, i64* %envptr12743, i64 1                ; &envptr12743[1]
  %BSj$args = load i64, i64* %envptr12744, align 8                                   ; load; *envptr12744
  %a9217 = call i64 @prim_cdr(i64 %BSj$args)                                         ; call prim_cdr
  %retprim9484 = call i64 @prim_cdr(i64 %a9217)                                      ; call prim_cdr
  %cloptr12745 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr12747 = getelementptr inbounds i64, i64* %cloptr12745, i64 1                  ; &eptr12747[1]
  %eptr12748 = getelementptr inbounds i64, i64* %cloptr12745, i64 2                  ; &eptr12748[2]
  %eptr12749 = getelementptr inbounds i64, i64* %cloptr12745, i64 3                  ; &eptr12749[3]
  %eptr12750 = getelementptr inbounds i64, i64* %cloptr12745, i64 4                  ; &eptr12750[4]
  %eptr12751 = getelementptr inbounds i64, i64* %cloptr12745, i64 5                  ; &eptr12751[5]
  %eptr12752 = getelementptr inbounds i64, i64* %cloptr12745, i64 6                  ; &eptr12752[6]
  store i64 %yPf$_37foldr, i64* %eptr12747                                           ; *eptr12747 = %yPf$_37foldr
  store i64 %Udy$acc, i64* %eptr12748                                                ; *eptr12748 = %Udy$acc
  store i64 %RQ7$_37foldr1, i64* %eptr12749                                          ; *eptr12749 = %RQ7$_37foldr1
  store i64 %nad$_37map1, i64* %eptr12750                                            ; *eptr12750 = %nad$_37map1
  store i64 %cont9465, i64* %eptr12751                                               ; *eptr12751 = %cont9465
  store i64 %tNI$f, i64* %eptr12752                                                  ; *eptr12752 = %tNI$f
  %eptr12746 = getelementptr inbounds i64, i64* %cloptr12745, i64 0                  ; &cloptr12745[0]
  %f12753 = ptrtoint void(i64,i64,i64)* @lam10498 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12753, i64* %eptr12746                                                 ; store fptr
  %arg10234 = ptrtoint i64* %cloptr12745 to i64                                      ; closure cast; i64* -> i64
  %arg10233 = add i64 0, 0                                                           ; quoted ()
  %cloptr12754 = inttoptr i64 %arg10234 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12755 = getelementptr inbounds i64, i64* %cloptr12754, i64 0                 ; &cloptr12754[0]
  %f12757 = load i64, i64* %i0ptr12755, align 8                                      ; load; *i0ptr12755
  %fptr12756 = inttoptr i64 %f12757 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12756(i64 %arg10234, i64 %arg10233, i64 %retprim9484); tail call
  ret void
}


define void @lam10498(i64 %env10499, i64 %_959468, i64 %Do7$lsts) {
  %envptr12758 = inttoptr i64 %env10499 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12759 = getelementptr inbounds i64, i64* %envptr12758, i64 6                ; &envptr12758[6]
  %tNI$f = load i64, i64* %envptr12759, align 8                                      ; load; *envptr12759
  %envptr12760 = inttoptr i64 %env10499 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12761 = getelementptr inbounds i64, i64* %envptr12760, i64 5                ; &envptr12760[5]
  %cont9465 = load i64, i64* %envptr12761, align 8                                   ; load; *envptr12761
  %envptr12762 = inttoptr i64 %env10499 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12763 = getelementptr inbounds i64, i64* %envptr12762, i64 4                ; &envptr12762[4]
  %nad$_37map1 = load i64, i64* %envptr12763, align 8                                ; load; *envptr12763
  %envptr12764 = inttoptr i64 %env10499 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12765 = getelementptr inbounds i64, i64* %envptr12764, i64 3                ; &envptr12764[3]
  %RQ7$_37foldr1 = load i64, i64* %envptr12765, align 8                              ; load; *envptr12765
  %envptr12766 = inttoptr i64 %env10499 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12767 = getelementptr inbounds i64, i64* %envptr12766, i64 2                ; &envptr12766[2]
  %Udy$acc = load i64, i64* %envptr12767, align 8                                    ; load; *envptr12767
  %envptr12768 = inttoptr i64 %env10499 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12769 = getelementptr inbounds i64, i64* %envptr12768, i64 1                ; &envptr12768[1]
  %yPf$_37foldr = load i64, i64* %envptr12769, align 8                               ; load; *envptr12769
  %cloptr12770 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr12772 = getelementptr inbounds i64, i64* %cloptr12770, i64 1                  ; &eptr12772[1]
  %eptr12773 = getelementptr inbounds i64, i64* %cloptr12770, i64 2                  ; &eptr12773[2]
  %eptr12774 = getelementptr inbounds i64, i64* %cloptr12770, i64 3                  ; &eptr12774[3]
  %eptr12775 = getelementptr inbounds i64, i64* %cloptr12770, i64 4                  ; &eptr12775[4]
  %eptr12776 = getelementptr inbounds i64, i64* %cloptr12770, i64 5                  ; &eptr12776[5]
  %eptr12777 = getelementptr inbounds i64, i64* %cloptr12770, i64 6                  ; &eptr12777[6]
  %eptr12778 = getelementptr inbounds i64, i64* %cloptr12770, i64 7                  ; &eptr12778[7]
  store i64 %yPf$_37foldr, i64* %eptr12772                                           ; *eptr12772 = %yPf$_37foldr
  store i64 %Udy$acc, i64* %eptr12773                                                ; *eptr12773 = %Udy$acc
  store i64 %Do7$lsts, i64* %eptr12774                                               ; *eptr12774 = %Do7$lsts
  store i64 %RQ7$_37foldr1, i64* %eptr12775                                          ; *eptr12775 = %RQ7$_37foldr1
  store i64 %nad$_37map1, i64* %eptr12776                                            ; *eptr12776 = %nad$_37map1
  store i64 %cont9465, i64* %eptr12777                                               ; *eptr12777 = %cont9465
  store i64 %tNI$f, i64* %eptr12778                                                  ; *eptr12778 = %tNI$f
  %eptr12771 = getelementptr inbounds i64, i64* %cloptr12770, i64 0                  ; &cloptr12770[0]
  %f12779 = ptrtoint void(i64,i64,i64)* @lam10496 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12779, i64* %eptr12771                                                 ; store fptr
  %arg10238 = ptrtoint i64* %cloptr12770 to i64                                      ; closure cast; i64* -> i64
  %cloptr12780 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12781 = getelementptr inbounds i64, i64* %cloptr12780, i64 0                  ; &cloptr12780[0]
  %f12782 = ptrtoint void(i64,i64,i64,i64)* @lam10475 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f12782, i64* %eptr12781                                                 ; store fptr
  %arg10237 = ptrtoint i64* %cloptr12780 to i64                                      ; closure cast; i64* -> i64
  %arg10236 = call i64 @const_init_false()                                           ; quoted #f
  %cloptr12783 = inttoptr i64 %RQ7$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr12784 = getelementptr inbounds i64, i64* %cloptr12783, i64 0                 ; &cloptr12783[0]
  %f12786 = load i64, i64* %i0ptr12784, align 8                                      ; load; *i0ptr12784
  %fptr12785 = inttoptr i64 %f12786 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12785(i64 %RQ7$_37foldr1, i64 %arg10238, i64 %arg10237, i64 %arg10236, i64 %Do7$lsts); tail call
  ret void
}


define void @lam10496(i64 %env10497, i64 %_959469, i64 %a9218) {
  %envptr12787 = inttoptr i64 %env10497 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12788 = getelementptr inbounds i64, i64* %envptr12787, i64 7                ; &envptr12787[7]
  %tNI$f = load i64, i64* %envptr12788, align 8                                      ; load; *envptr12788
  %envptr12789 = inttoptr i64 %env10497 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12790 = getelementptr inbounds i64, i64* %envptr12789, i64 6                ; &envptr12789[6]
  %cont9465 = load i64, i64* %envptr12790, align 8                                   ; load; *envptr12790
  %envptr12791 = inttoptr i64 %env10497 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12792 = getelementptr inbounds i64, i64* %envptr12791, i64 5                ; &envptr12791[5]
  %nad$_37map1 = load i64, i64* %envptr12792, align 8                                ; load; *envptr12792
  %envptr12793 = inttoptr i64 %env10497 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12794 = getelementptr inbounds i64, i64* %envptr12793, i64 4                ; &envptr12793[4]
  %RQ7$_37foldr1 = load i64, i64* %envptr12794, align 8                              ; load; *envptr12794
  %envptr12795 = inttoptr i64 %env10497 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12796 = getelementptr inbounds i64, i64* %envptr12795, i64 3                ; &envptr12795[3]
  %Do7$lsts = load i64, i64* %envptr12796, align 8                                   ; load; *envptr12796
  %envptr12797 = inttoptr i64 %env10497 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12798 = getelementptr inbounds i64, i64* %envptr12797, i64 2                ; &envptr12797[2]
  %Udy$acc = load i64, i64* %envptr12798, align 8                                    ; load; *envptr12798
  %envptr12799 = inttoptr i64 %env10497 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12800 = getelementptr inbounds i64, i64* %envptr12799, i64 1                ; &envptr12799[1]
  %yPf$_37foldr = load i64, i64* %envptr12800, align 8                               ; load; *envptr12800
  %cmp12801 = icmp eq i64 %a9218, 15                                                 ; false?
  br i1 %cmp12801, label %else12803, label %then12802                                ; if

then12802:
  %arg10241 = add i64 0, 0                                                           ; quoted ()
  %cloptr12804 = inttoptr i64 %cont9465 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12805 = getelementptr inbounds i64, i64* %cloptr12804, i64 0                 ; &cloptr12804[0]
  %f12807 = load i64, i64* %i0ptr12805, align 8                                      ; load; *i0ptr12805
  %fptr12806 = inttoptr i64 %f12807 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12806(i64 %cont9465, i64 %arg10241, i64 %Udy$acc)   ; tail call
  ret void

else12803:
  %cloptr12808 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr12810 = getelementptr inbounds i64, i64* %cloptr12808, i64 1                  ; &eptr12810[1]
  %eptr12811 = getelementptr inbounds i64, i64* %cloptr12808, i64 2                  ; &eptr12811[2]
  %eptr12812 = getelementptr inbounds i64, i64* %cloptr12808, i64 3                  ; &eptr12812[3]
  %eptr12813 = getelementptr inbounds i64, i64* %cloptr12808, i64 4                  ; &eptr12813[4]
  %eptr12814 = getelementptr inbounds i64, i64* %cloptr12808, i64 5                  ; &eptr12814[5]
  %eptr12815 = getelementptr inbounds i64, i64* %cloptr12808, i64 6                  ; &eptr12815[6]
  %eptr12816 = getelementptr inbounds i64, i64* %cloptr12808, i64 7                  ; &eptr12816[7]
  store i64 %yPf$_37foldr, i64* %eptr12810                                           ; *eptr12810 = %yPf$_37foldr
  store i64 %Udy$acc, i64* %eptr12811                                                ; *eptr12811 = %Udy$acc
  store i64 %Do7$lsts, i64* %eptr12812                                               ; *eptr12812 = %Do7$lsts
  store i64 %RQ7$_37foldr1, i64* %eptr12813                                          ; *eptr12813 = %RQ7$_37foldr1
  store i64 %nad$_37map1, i64* %eptr12814                                            ; *eptr12814 = %nad$_37map1
  store i64 %cont9465, i64* %eptr12815                                               ; *eptr12815 = %cont9465
  store i64 %tNI$f, i64* %eptr12816                                                  ; *eptr12816 = %tNI$f
  %eptr12809 = getelementptr inbounds i64, i64* %cloptr12808, i64 0                  ; &cloptr12808[0]
  %f12817 = ptrtoint void(i64,i64,i64)* @lam10494 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12817, i64* %eptr12809                                                 ; store fptr
  %arg10245 = ptrtoint i64* %cloptr12808 to i64                                      ; closure cast; i64* -> i64
  %cloptr12818 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12819 = getelementptr inbounds i64, i64* %cloptr12818, i64 0                  ; &cloptr12818[0]
  %f12820 = ptrtoint void(i64,i64,i64)* @lam10479 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12820, i64* %eptr12819                                                 ; store fptr
  %arg10244 = ptrtoint i64* %cloptr12818 to i64                                      ; closure cast; i64* -> i64
  %cloptr12821 = inttoptr i64 %nad$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr12822 = getelementptr inbounds i64, i64* %cloptr12821, i64 0                 ; &cloptr12821[0]
  %f12824 = load i64, i64* %i0ptr12822, align 8                                      ; load; *i0ptr12822
  %fptr12823 = inttoptr i64 %f12824 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12823(i64 %nad$_37map1, i64 %arg10245, i64 %arg10244, i64 %Do7$lsts); tail call
  ret void
}


define void @lam10494(i64 %env10495, i64 %_959470, i64 %jHt$lsts_43) {
  %envptr12825 = inttoptr i64 %env10495 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12826 = getelementptr inbounds i64, i64* %envptr12825, i64 7                ; &envptr12825[7]
  %tNI$f = load i64, i64* %envptr12826, align 8                                      ; load; *envptr12826
  %envptr12827 = inttoptr i64 %env10495 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12828 = getelementptr inbounds i64, i64* %envptr12827, i64 6                ; &envptr12827[6]
  %cont9465 = load i64, i64* %envptr12828, align 8                                   ; load; *envptr12828
  %envptr12829 = inttoptr i64 %env10495 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12830 = getelementptr inbounds i64, i64* %envptr12829, i64 5                ; &envptr12829[5]
  %nad$_37map1 = load i64, i64* %envptr12830, align 8                                ; load; *envptr12830
  %envptr12831 = inttoptr i64 %env10495 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12832 = getelementptr inbounds i64, i64* %envptr12831, i64 4                ; &envptr12831[4]
  %RQ7$_37foldr1 = load i64, i64* %envptr12832, align 8                              ; load; *envptr12832
  %envptr12833 = inttoptr i64 %env10495 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12834 = getelementptr inbounds i64, i64* %envptr12833, i64 3                ; &envptr12833[3]
  %Do7$lsts = load i64, i64* %envptr12834, align 8                                   ; load; *envptr12834
  %envptr12835 = inttoptr i64 %env10495 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12836 = getelementptr inbounds i64, i64* %envptr12835, i64 2                ; &envptr12835[2]
  %Udy$acc = load i64, i64* %envptr12836, align 8                                    ; load; *envptr12836
  %envptr12837 = inttoptr i64 %env10495 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12838 = getelementptr inbounds i64, i64* %envptr12837, i64 1                ; &envptr12837[1]
  %yPf$_37foldr = load i64, i64* %envptr12838, align 8                               ; load; *envptr12838
  %cloptr12839 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr12841 = getelementptr inbounds i64, i64* %cloptr12839, i64 1                  ; &eptr12841[1]
  %eptr12842 = getelementptr inbounds i64, i64* %cloptr12839, i64 2                  ; &eptr12842[2]
  %eptr12843 = getelementptr inbounds i64, i64* %cloptr12839, i64 3                  ; &eptr12843[3]
  %eptr12844 = getelementptr inbounds i64, i64* %cloptr12839, i64 4                  ; &eptr12844[4]
  %eptr12845 = getelementptr inbounds i64, i64* %cloptr12839, i64 5                  ; &eptr12845[5]
  %eptr12846 = getelementptr inbounds i64, i64* %cloptr12839, i64 6                  ; &eptr12846[6]
  store i64 %jHt$lsts_43, i64* %eptr12841                                            ; *eptr12841 = %jHt$lsts_43
  store i64 %yPf$_37foldr, i64* %eptr12842                                           ; *eptr12842 = %yPf$_37foldr
  store i64 %Udy$acc, i64* %eptr12843                                                ; *eptr12843 = %Udy$acc
  store i64 %RQ7$_37foldr1, i64* %eptr12844                                          ; *eptr12844 = %RQ7$_37foldr1
  store i64 %cont9465, i64* %eptr12845                                               ; *eptr12845 = %cont9465
  store i64 %tNI$f, i64* %eptr12846                                                  ; *eptr12846 = %tNI$f
  %eptr12840 = getelementptr inbounds i64, i64* %cloptr12839, i64 0                  ; &cloptr12839[0]
  %f12847 = ptrtoint void(i64,i64,i64)* @lam10492 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12847, i64* %eptr12840                                                 ; store fptr
  %arg10249 = ptrtoint i64* %cloptr12839 to i64                                      ; closure cast; i64* -> i64
  %cloptr12848 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12849 = getelementptr inbounds i64, i64* %cloptr12848, i64 0                  ; &cloptr12848[0]
  %f12850 = ptrtoint void(i64,i64,i64)* @lam10482 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12850, i64* %eptr12849                                                 ; store fptr
  %arg10248 = ptrtoint i64* %cloptr12848 to i64                                      ; closure cast; i64* -> i64
  %cloptr12851 = inttoptr i64 %nad$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr12852 = getelementptr inbounds i64, i64* %cloptr12851, i64 0                 ; &cloptr12851[0]
  %f12854 = load i64, i64* %i0ptr12852, align 8                                      ; load; *i0ptr12852
  %fptr12853 = inttoptr i64 %f12854 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12853(i64 %nad$_37map1, i64 %arg10249, i64 %arg10248, i64 %Do7$lsts); tail call
  ret void
}


define void @lam10492(i64 %env10493, i64 %_959471, i64 %AT5$vs) {
  %envptr12855 = inttoptr i64 %env10493 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12856 = getelementptr inbounds i64, i64* %envptr12855, i64 6                ; &envptr12855[6]
  %tNI$f = load i64, i64* %envptr12856, align 8                                      ; load; *envptr12856
  %envptr12857 = inttoptr i64 %env10493 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12858 = getelementptr inbounds i64, i64* %envptr12857, i64 5                ; &envptr12857[5]
  %cont9465 = load i64, i64* %envptr12858, align 8                                   ; load; *envptr12858
  %envptr12859 = inttoptr i64 %env10493 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12860 = getelementptr inbounds i64, i64* %envptr12859, i64 4                ; &envptr12859[4]
  %RQ7$_37foldr1 = load i64, i64* %envptr12860, align 8                              ; load; *envptr12860
  %envptr12861 = inttoptr i64 %env10493 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12862 = getelementptr inbounds i64, i64* %envptr12861, i64 3                ; &envptr12861[3]
  %Udy$acc = load i64, i64* %envptr12862, align 8                                    ; load; *envptr12862
  %envptr12863 = inttoptr i64 %env10493 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12864 = getelementptr inbounds i64, i64* %envptr12863, i64 2                ; &envptr12863[2]
  %yPf$_37foldr = load i64, i64* %envptr12864, align 8                               ; load; *envptr12864
  %envptr12865 = inttoptr i64 %env10493 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12866 = getelementptr inbounds i64, i64* %envptr12865, i64 1                ; &envptr12865[1]
  %jHt$lsts_43 = load i64, i64* %envptr12866, align 8                                ; load; *envptr12866
  %a9219 = call i64 @prim_cons(i64 %Udy$acc, i64 %jHt$lsts_43)                       ; call prim_cons
  %a9220 = call i64 @prim_cons(i64 %tNI$f, i64 %a9219)                               ; call prim_cons
  %cloptr12867 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr12869 = getelementptr inbounds i64, i64* %cloptr12867, i64 1                  ; &eptr12869[1]
  %eptr12870 = getelementptr inbounds i64, i64* %cloptr12867, i64 2                  ; &eptr12870[2]
  %eptr12871 = getelementptr inbounds i64, i64* %cloptr12867, i64 3                  ; &eptr12871[3]
  %eptr12872 = getelementptr inbounds i64, i64* %cloptr12867, i64 4                  ; &eptr12872[4]
  store i64 %AT5$vs, i64* %eptr12869                                                 ; *eptr12869 = %AT5$vs
  store i64 %RQ7$_37foldr1, i64* %eptr12870                                          ; *eptr12870 = %RQ7$_37foldr1
  store i64 %cont9465, i64* %eptr12871                                               ; *eptr12871 = %cont9465
  store i64 %tNI$f, i64* %eptr12872                                                  ; *eptr12872 = %tNI$f
  %eptr12868 = getelementptr inbounds i64, i64* %cloptr12867, i64 0                  ; &cloptr12867[0]
  %f12873 = ptrtoint void(i64,i64,i64)* @lam10490 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12873, i64* %eptr12868                                                 ; store fptr
  %arg10256 = ptrtoint i64* %cloptr12867 to i64                                      ; closure cast; i64* -> i64
  %cps_45lst9477 = call i64 @prim_cons(i64 %arg10256, i64 %a9220)                    ; call prim_cons
  %cloptr12874 = inttoptr i64 %yPf$_37foldr to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr12875 = getelementptr inbounds i64, i64* %cloptr12874, i64 0                 ; &cloptr12874[0]
  %f12877 = load i64, i64* %i0ptr12875, align 8                                      ; load; *i0ptr12875
  %fptr12876 = inttoptr i64 %f12877 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12876(i64 %yPf$_37foldr, i64 %cps_45lst9477)        ; tail call
  ret void
}


define void @lam10490(i64 %env10491, i64 %_959472, i64 %a9221) {
  %envptr12878 = inttoptr i64 %env10491 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12879 = getelementptr inbounds i64, i64* %envptr12878, i64 4                ; &envptr12878[4]
  %tNI$f = load i64, i64* %envptr12879, align 8                                      ; load; *envptr12879
  %envptr12880 = inttoptr i64 %env10491 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12881 = getelementptr inbounds i64, i64* %envptr12880, i64 3                ; &envptr12880[3]
  %cont9465 = load i64, i64* %envptr12881, align 8                                   ; load; *envptr12881
  %envptr12882 = inttoptr i64 %env10491 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12883 = getelementptr inbounds i64, i64* %envptr12882, i64 2                ; &envptr12882[2]
  %RQ7$_37foldr1 = load i64, i64* %envptr12883, align 8                              ; load; *envptr12883
  %envptr12884 = inttoptr i64 %env10491 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12885 = getelementptr inbounds i64, i64* %envptr12884, i64 1                ; &envptr12884[1]
  %AT5$vs = load i64, i64* %envptr12885, align 8                                     ; load; *envptr12885
  %arg10257 = add i64 0, 0                                                           ; quoted ()
  %a9222 = call i64 @prim_cons(i64 %a9221, i64 %arg10257)                            ; call prim_cons
  %cloptr12886 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr12888 = getelementptr inbounds i64, i64* %cloptr12886, i64 1                  ; &eptr12888[1]
  %eptr12889 = getelementptr inbounds i64, i64* %cloptr12886, i64 2                  ; &eptr12889[2]
  store i64 %cont9465, i64* %eptr12888                                               ; *eptr12888 = %cont9465
  store i64 %tNI$f, i64* %eptr12889                                                  ; *eptr12889 = %tNI$f
  %eptr12887 = getelementptr inbounds i64, i64* %cloptr12886, i64 0                  ; &cloptr12886[0]
  %f12890 = ptrtoint void(i64,i64,i64)* @lam10487 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12890, i64* %eptr12887                                                 ; store fptr
  %arg10262 = ptrtoint i64* %cloptr12886 to i64                                      ; closure cast; i64* -> i64
  %cloptr12891 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr12892 = getelementptr inbounds i64, i64* %cloptr12891, i64 0                  ; &cloptr12891[0]
  %f12893 = ptrtoint void(i64,i64,i64,i64)* @lam10485 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f12893, i64* %eptr12892                                                 ; store fptr
  %arg10261 = ptrtoint i64* %cloptr12891 to i64                                      ; closure cast; i64* -> i64
  %cloptr12894 = inttoptr i64 %RQ7$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr12895 = getelementptr inbounds i64, i64* %cloptr12894, i64 0                 ; &cloptr12894[0]
  %f12897 = load i64, i64* %i0ptr12895, align 8                                      ; load; *i0ptr12895
  %fptr12896 = inttoptr i64 %f12897 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12896(i64 %RQ7$_37foldr1, i64 %arg10262, i64 %arg10261, i64 %a9222, i64 %AT5$vs); tail call
  ret void
}


define void @lam10487(i64 %env10488, i64 %_959473, i64 %a9223) {
  %envptr12898 = inttoptr i64 %env10488 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12899 = getelementptr inbounds i64, i64* %envptr12898, i64 2                ; &envptr12898[2]
  %tNI$f = load i64, i64* %envptr12899, align 8                                      ; load; *envptr12899
  %envptr12900 = inttoptr i64 %env10488 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12901 = getelementptr inbounds i64, i64* %envptr12900, i64 1                ; &envptr12900[1]
  %cont9465 = load i64, i64* %envptr12901, align 8                                   ; load; *envptr12901
  %cps_45lst9474 = call i64 @prim_cons(i64 %cont9465, i64 %a9223)                    ; call prim_cons
  %cloptr12902 = inttoptr i64 %tNI$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12903 = getelementptr inbounds i64, i64* %cloptr12902, i64 0                 ; &cloptr12902[0]
  %f12905 = load i64, i64* %i0ptr12903, align 8                                      ; load; *i0ptr12903
  %fptr12904 = inttoptr i64 %f12905 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12904(i64 %tNI$f, i64 %cps_45lst9474)               ; tail call
  ret void
}


define void @lam10485(i64 %env10486, i64 %cont9475, i64 %H1r$a, i64 %nYK$b) {
  %retprim9476 = call i64 @prim_cons(i64 %H1r$a, i64 %nYK$b)                         ; call prim_cons
  %arg10269 = add i64 0, 0                                                           ; quoted ()
  %cloptr12906 = inttoptr i64 %cont9475 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12907 = getelementptr inbounds i64, i64* %cloptr12906, i64 0                 ; &cloptr12906[0]
  %f12909 = load i64, i64* %i0ptr12907, align 8                                      ; load; *i0ptr12907
  %fptr12908 = inttoptr i64 %f12909 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12908(i64 %cont9475, i64 %arg10269, i64 %retprim9476); tail call
  ret void
}


define void @lam10482(i64 %env10483, i64 %cont9478, i64 %UX7$x) {
  %retprim9479 = call i64 @prim_car(i64 %UX7$x)                                      ; call prim_car
  %arg10273 = add i64 0, 0                                                           ; quoted ()
  %cloptr12910 = inttoptr i64 %cont9478 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12911 = getelementptr inbounds i64, i64* %cloptr12910, i64 0                 ; &cloptr12910[0]
  %f12913 = load i64, i64* %i0ptr12911, align 8                                      ; load; *i0ptr12911
  %fptr12912 = inttoptr i64 %f12913 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12912(i64 %cont9478, i64 %arg10273, i64 %retprim9479); tail call
  ret void
}


define void @lam10479(i64 %env10480, i64 %cont9480, i64 %CNf$x) {
  %retprim9481 = call i64 @prim_cdr(i64 %CNf$x)                                      ; call prim_cdr
  %arg10277 = add i64 0, 0                                                           ; quoted ()
  %cloptr12914 = inttoptr i64 %cont9480 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12915 = getelementptr inbounds i64, i64* %cloptr12914, i64 0                 ; &cloptr12914[0]
  %f12917 = load i64, i64* %i0ptr12915, align 8                                      ; load; *i0ptr12915
  %fptr12916 = inttoptr i64 %f12917 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12916(i64 %cont9480, i64 %arg10277, i64 %retprim9481); tail call
  ret void
}


define void @lam10475(i64 %env10476, i64 %cont9482, i64 %pNQ$lst, i64 %vN3$b) {
  %cmp12918 = icmp eq i64 %vN3$b, 15                                                 ; false?
  br i1 %cmp12918, label %else12920, label %then12919                                ; if

then12919:
  %arg10280 = add i64 0, 0                                                           ; quoted ()
  %cloptr12921 = inttoptr i64 %cont9482 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12922 = getelementptr inbounds i64, i64* %cloptr12921, i64 0                 ; &cloptr12921[0]
  %f12924 = load i64, i64* %i0ptr12922, align 8                                      ; load; *i0ptr12922
  %fptr12923 = inttoptr i64 %f12924 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12923(i64 %cont9482, i64 %arg10280, i64 %vN3$b)     ; tail call
  ret void

else12920:
  %retprim9483 = call i64 @prim_null_63(i64 %pNQ$lst)                                ; call prim_null_63
  %arg10284 = add i64 0, 0                                                           ; quoted ()
  %cloptr12925 = inttoptr i64 %cont9482 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12926 = getelementptr inbounds i64, i64* %cloptr12925, i64 0                 ; &cloptr12925[0]
  %f12928 = load i64, i64* %i0ptr12926, align 8                                      ; load; *i0ptr12926
  %fptr12927 = inttoptr i64 %f12928 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12927(i64 %cont9482, i64 %arg10284, i64 %retprim9483); tail call
  ret void
}


define void @lam10468(i64 %env10469, i64 %cont9486, i64 %Bcd$_37foldl1) {
  %arg10287 = add i64 0, 0                                                           ; quoted ()
  %cloptr12929 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr12931 = getelementptr inbounds i64, i64* %cloptr12929, i64 1                  ; &eptr12931[1]
  store i64 %Bcd$_37foldl1, i64* %eptr12931                                          ; *eptr12931 = %Bcd$_37foldl1
  %eptr12930 = getelementptr inbounds i64, i64* %cloptr12929, i64 0                  ; &cloptr12929[0]
  %f12932 = ptrtoint void(i64,i64,i64,i64,i64)* @lam10465 to i64                     ; fptr cast; i64(...)* -> i64
  store i64 %f12932, i64* %eptr12930                                                 ; store fptr
  %arg10286 = ptrtoint i64* %cloptr12929 to i64                                      ; closure cast; i64* -> i64
  %cloptr12933 = inttoptr i64 %cont9486 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12934 = getelementptr inbounds i64, i64* %cloptr12933, i64 0                 ; &cloptr12933[0]
  %f12936 = load i64, i64* %i0ptr12934, align 8                                      ; load; *i0ptr12934
  %fptr12935 = inttoptr i64 %f12936 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12935(i64 %cont9486, i64 %arg10287, i64 %arg10286)  ; tail call
  ret void
}


define void @lam10465(i64 %env10466, i64 %cont9487, i64 %B51$f, i64 %o98$acc, i64 %NlG$lst) {
  %envptr12937 = inttoptr i64 %env10466 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12938 = getelementptr inbounds i64, i64* %envptr12937, i64 1                ; &envptr12937[1]
  %Bcd$_37foldl1 = load i64, i64* %envptr12938, align 8                              ; load; *envptr12938
  %a9210 = call i64 @prim_null_63(i64 %NlG$lst)                                      ; call prim_null_63
  %cmp12939 = icmp eq i64 %a9210, 15                                                 ; false?
  br i1 %cmp12939, label %else12941, label %then12940                                ; if

then12940:
  %arg10291 = add i64 0, 0                                                           ; quoted ()
  %cloptr12942 = inttoptr i64 %cont9487 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12943 = getelementptr inbounds i64, i64* %cloptr12942, i64 0                 ; &cloptr12942[0]
  %f12945 = load i64, i64* %i0ptr12943, align 8                                      ; load; *i0ptr12943
  %fptr12944 = inttoptr i64 %f12945 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12944(i64 %cont9487, i64 %arg10291, i64 %o98$acc)   ; tail call
  ret void

else12941:
  %a9211 = call i64 @prim_car(i64 %NlG$lst)                                          ; call prim_car
  %cloptr12946 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr12948 = getelementptr inbounds i64, i64* %cloptr12946, i64 1                  ; &eptr12948[1]
  %eptr12949 = getelementptr inbounds i64, i64* %cloptr12946, i64 2                  ; &eptr12949[2]
  %eptr12950 = getelementptr inbounds i64, i64* %cloptr12946, i64 3                  ; &eptr12950[3]
  %eptr12951 = getelementptr inbounds i64, i64* %cloptr12946, i64 4                  ; &eptr12951[4]
  store i64 %cont9487, i64* %eptr12948                                               ; *eptr12948 = %cont9487
  store i64 %B51$f, i64* %eptr12949                                                  ; *eptr12949 = %B51$f
  store i64 %Bcd$_37foldl1, i64* %eptr12950                                          ; *eptr12950 = %Bcd$_37foldl1
  store i64 %NlG$lst, i64* %eptr12951                                                ; *eptr12951 = %NlG$lst
  %eptr12947 = getelementptr inbounds i64, i64* %cloptr12946, i64 0                  ; &cloptr12946[0]
  %f12952 = ptrtoint void(i64,i64,i64)* @lam10463 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12952, i64* %eptr12947                                                 ; store fptr
  %arg10296 = ptrtoint i64* %cloptr12946 to i64                                      ; closure cast; i64* -> i64
  %cloptr12953 = inttoptr i64 %B51$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr12954 = getelementptr inbounds i64, i64* %cloptr12953, i64 0                 ; &cloptr12953[0]
  %f12956 = load i64, i64* %i0ptr12954, align 8                                      ; load; *i0ptr12954
  %fptr12955 = inttoptr i64 %f12956 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12955(i64 %B51$f, i64 %arg10296, i64 %a9211, i64 %o98$acc); tail call
  ret void
}


define void @lam10463(i64 %env10464, i64 %_959488, i64 %a9212) {
  %envptr12957 = inttoptr i64 %env10464 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12958 = getelementptr inbounds i64, i64* %envptr12957, i64 4                ; &envptr12957[4]
  %NlG$lst = load i64, i64* %envptr12958, align 8                                    ; load; *envptr12958
  %envptr12959 = inttoptr i64 %env10464 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12960 = getelementptr inbounds i64, i64* %envptr12959, i64 3                ; &envptr12959[3]
  %Bcd$_37foldl1 = load i64, i64* %envptr12960, align 8                              ; load; *envptr12960
  %envptr12961 = inttoptr i64 %env10464 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12962 = getelementptr inbounds i64, i64* %envptr12961, i64 2                ; &envptr12961[2]
  %B51$f = load i64, i64* %envptr12962, align 8                                      ; load; *envptr12962
  %envptr12963 = inttoptr i64 %env10464 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12964 = getelementptr inbounds i64, i64* %envptr12963, i64 1                ; &envptr12963[1]
  %cont9487 = load i64, i64* %envptr12964, align 8                                   ; load; *envptr12964
  %a9213 = call i64 @prim_cdr(i64 %NlG$lst)                                          ; call prim_cdr
  %cloptr12965 = inttoptr i64 %Bcd$_37foldl1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr12966 = getelementptr inbounds i64, i64* %cloptr12965, i64 0                 ; &cloptr12965[0]
  %f12968 = load i64, i64* %i0ptr12966, align 8                                      ; load; *i0ptr12966
  %fptr12967 = inttoptr i64 %f12968 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12967(i64 %Bcd$_37foldl1, i64 %cont9487, i64 %B51$f, i64 %a9212, i64 %a9213); tail call
  ret void
}


define void @lam10460(i64 %env10461, i64 %cont9489, i64 %Ay1$_37length) {
  %arg10305 = add i64 0, 0                                                           ; quoted ()
  %cloptr12969 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr12971 = getelementptr inbounds i64, i64* %cloptr12969, i64 1                  ; &eptr12971[1]
  store i64 %Ay1$_37length, i64* %eptr12971                                          ; *eptr12971 = %Ay1$_37length
  %eptr12970 = getelementptr inbounds i64, i64* %cloptr12969, i64 0                  ; &cloptr12969[0]
  %f12972 = ptrtoint void(i64,i64,i64)* @lam10457 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12972, i64* %eptr12970                                                 ; store fptr
  %arg10304 = ptrtoint i64* %cloptr12969 to i64                                      ; closure cast; i64* -> i64
  %cloptr12973 = inttoptr i64 %cont9489 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12974 = getelementptr inbounds i64, i64* %cloptr12973, i64 0                 ; &cloptr12973[0]
  %f12976 = load i64, i64* %i0ptr12974, align 8                                      ; load; *i0ptr12974
  %fptr12975 = inttoptr i64 %f12976 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12975(i64 %cont9489, i64 %arg10305, i64 %arg10304)  ; tail call
  ret void
}


define void @lam10457(i64 %env10458, i64 %cont9490, i64 %biP$lst) {
  %envptr12977 = inttoptr i64 %env10458 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12978 = getelementptr inbounds i64, i64* %envptr12977, i64 1                ; &envptr12977[1]
  %Ay1$_37length = load i64, i64* %envptr12978, align 8                              ; load; *envptr12978
  %a9207 = call i64 @prim_null_63(i64 %biP$lst)                                      ; call prim_null_63
  %cmp12979 = icmp eq i64 %a9207, 15                                                 ; false?
  br i1 %cmp12979, label %else12981, label %then12980                                ; if

then12980:
  %arg10309 = add i64 0, 0                                                           ; quoted ()
  %arg10308 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %cloptr12982 = inttoptr i64 %cont9490 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12983 = getelementptr inbounds i64, i64* %cloptr12982, i64 0                 ; &cloptr12982[0]
  %f12985 = load i64, i64* %i0ptr12983, align 8                                      ; load; *i0ptr12983
  %fptr12984 = inttoptr i64 %f12985 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12984(i64 %cont9490, i64 %arg10309, i64 %arg10308)  ; tail call
  ret void

else12981:
  %a9208 = call i64 @prim_cdr(i64 %biP$lst)                                          ; call prim_cdr
  %cloptr12986 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr12988 = getelementptr inbounds i64, i64* %cloptr12986, i64 1                  ; &eptr12988[1]
  store i64 %cont9490, i64* %eptr12988                                               ; *eptr12988 = %cont9490
  %eptr12987 = getelementptr inbounds i64, i64* %cloptr12986, i64 0                  ; &cloptr12986[0]
  %f12989 = ptrtoint void(i64,i64,i64)* @lam10455 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f12989, i64* %eptr12987                                                 ; store fptr
  %arg10313 = ptrtoint i64* %cloptr12986 to i64                                      ; closure cast; i64* -> i64
  %cloptr12990 = inttoptr i64 %Ay1$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr12991 = getelementptr inbounds i64, i64* %cloptr12990, i64 0                 ; &cloptr12990[0]
  %f12993 = load i64, i64* %i0ptr12991, align 8                                      ; load; *i0ptr12991
  %fptr12992 = inttoptr i64 %f12993 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12992(i64 %Ay1$_37length, i64 %arg10313, i64 %a9208); tail call
  ret void
}


define void @lam10455(i64 %env10456, i64 %_959491, i64 %a9209) {
  %envptr12994 = inttoptr i64 %env10456 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr12995 = getelementptr inbounds i64, i64* %envptr12994, i64 1                ; &envptr12994[1]
  %cont9490 = load i64, i64* %envptr12995, align 8                                   ; load; *envptr12995
  %arg10316 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9492 = call i64 @prim__43(i64 %arg10316, i64 %a9209)                       ; call prim__43
  %arg10318 = add i64 0, 0                                                           ; quoted ()
  %cloptr12996 = inttoptr i64 %cont9490 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr12997 = getelementptr inbounds i64, i64* %cloptr12996, i64 0                 ; &cloptr12996[0]
  %f12999 = load i64, i64* %i0ptr12997, align 8                                      ; load; *i0ptr12997
  %fptr12998 = inttoptr i64 %f12999 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr12998(i64 %cont9490, i64 %arg10318, i64 %retprim9492); tail call
  ret void
}


define void @lam10449(i64 %env10450, i64 %cont9493, i64 %Uay$_37take) {
  %arg10321 = add i64 0, 0                                                           ; quoted ()
  %cloptr13000 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr13002 = getelementptr inbounds i64, i64* %cloptr13000, i64 1                  ; &eptr13002[1]
  store i64 %Uay$_37take, i64* %eptr13002                                            ; *eptr13002 = %Uay$_37take
  %eptr13001 = getelementptr inbounds i64, i64* %cloptr13000, i64 0                  ; &cloptr13000[0]
  %f13003 = ptrtoint void(i64,i64,i64,i64)* @lam10446 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f13003, i64* %eptr13001                                                 ; store fptr
  %arg10320 = ptrtoint i64* %cloptr13000 to i64                                      ; closure cast; i64* -> i64
  %cloptr13004 = inttoptr i64 %cont9493 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13005 = getelementptr inbounds i64, i64* %cloptr13004, i64 0                 ; &cloptr13004[0]
  %f13007 = load i64, i64* %i0ptr13005, align 8                                      ; load; *i0ptr13005
  %fptr13006 = inttoptr i64 %f13007 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13006(i64 %cont9493, i64 %arg10321, i64 %arg10320)  ; tail call
  ret void
}


define void @lam10446(i64 %env10447, i64 %cont9494, i64 %yIW$lst, i64 %Nhv$n) {
  %envptr13008 = inttoptr i64 %env10447 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13009 = getelementptr inbounds i64, i64* %envptr13008, i64 1                ; &envptr13008[1]
  %Uay$_37take = load i64, i64* %envptr13009, align 8                                ; load; *envptr13009
  %arg10323 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9201 = call i64 @prim__61(i64 %Nhv$n, i64 %arg10323)                             ; call prim__61
  %cmp13010 = icmp eq i64 %a9201, 15                                                 ; false?
  br i1 %cmp13010, label %else13012, label %then13011                                ; if

then13011:
  %arg10326 = add i64 0, 0                                                           ; quoted ()
  %arg10325 = add i64 0, 0                                                           ; quoted ()
  %cloptr13013 = inttoptr i64 %cont9494 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13014 = getelementptr inbounds i64, i64* %cloptr13013, i64 0                 ; &cloptr13013[0]
  %f13016 = load i64, i64* %i0ptr13014, align 8                                      ; load; *i0ptr13014
  %fptr13015 = inttoptr i64 %f13016 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13015(i64 %cont9494, i64 %arg10326, i64 %arg10325)  ; tail call
  ret void

else13012:
  %a9202 = call i64 @prim_null_63(i64 %yIW$lst)                                      ; call prim_null_63
  %cmp13017 = icmp eq i64 %a9202, 15                                                 ; false?
  br i1 %cmp13017, label %else13019, label %then13018                                ; if

then13018:
  %arg10330 = add i64 0, 0                                                           ; quoted ()
  %arg10329 = add i64 0, 0                                                           ; quoted ()
  %cloptr13020 = inttoptr i64 %cont9494 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13021 = getelementptr inbounds i64, i64* %cloptr13020, i64 0                 ; &cloptr13020[0]
  %f13023 = load i64, i64* %i0ptr13021, align 8                                      ; load; *i0ptr13021
  %fptr13022 = inttoptr i64 %f13023 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13022(i64 %cont9494, i64 %arg10330, i64 %arg10329)  ; tail call
  ret void

else13019:
  %a9203 = call i64 @prim_car(i64 %yIW$lst)                                          ; call prim_car
  %a9204 = call i64 @prim_cdr(i64 %yIW$lst)                                          ; call prim_cdr
  %arg10334 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %a9205 = call i64 @prim__45(i64 %Nhv$n, i64 %arg10334)                             ; call prim__45
  %cloptr13024 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13026 = getelementptr inbounds i64, i64* %cloptr13024, i64 1                  ; &eptr13026[1]
  %eptr13027 = getelementptr inbounds i64, i64* %cloptr13024, i64 2                  ; &eptr13027[2]
  store i64 %a9203, i64* %eptr13026                                                  ; *eptr13026 = %a9203
  store i64 %cont9494, i64* %eptr13027                                               ; *eptr13027 = %cont9494
  %eptr13025 = getelementptr inbounds i64, i64* %cloptr13024, i64 0                  ; &cloptr13024[0]
  %f13028 = ptrtoint void(i64,i64,i64)* @lam10442 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f13028, i64* %eptr13025                                                 ; store fptr
  %arg10338 = ptrtoint i64* %cloptr13024 to i64                                      ; closure cast; i64* -> i64
  %cloptr13029 = inttoptr i64 %Uay$_37take to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr13030 = getelementptr inbounds i64, i64* %cloptr13029, i64 0                 ; &cloptr13029[0]
  %f13032 = load i64, i64* %i0ptr13030, align 8                                      ; load; *i0ptr13030
  %fptr13031 = inttoptr i64 %f13032 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13031(i64 %Uay$_37take, i64 %arg10338, i64 %a9204, i64 %a9205); tail call
  ret void
}


define void @lam10442(i64 %env10443, i64 %_959495, i64 %a9206) {
  %envptr13033 = inttoptr i64 %env10443 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13034 = getelementptr inbounds i64, i64* %envptr13033, i64 2                ; &envptr13033[2]
  %cont9494 = load i64, i64* %envptr13034, align 8                                   ; load; *envptr13034
  %envptr13035 = inttoptr i64 %env10443 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13036 = getelementptr inbounds i64, i64* %envptr13035, i64 1                ; &envptr13035[1]
  %a9203 = load i64, i64* %envptr13036, align 8                                      ; load; *envptr13036
  %retprim9496 = call i64 @prim_cons(i64 %a9203, i64 %a9206)                         ; call prim_cons
  %arg10343 = add i64 0, 0                                                           ; quoted ()
  %cloptr13037 = inttoptr i64 %cont9494 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13038 = getelementptr inbounds i64, i64* %cloptr13037, i64 0                 ; &cloptr13037[0]
  %f13040 = load i64, i64* %i0ptr13038, align 8                                      ; load; *i0ptr13038
  %fptr13039 = inttoptr i64 %f13040 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13039(i64 %cont9494, i64 %arg10343, i64 %retprim9496); tail call
  ret void
}


define void @lam10435(i64 %env10436, i64 %cont9497, i64 %d26$_37map) {
  %arg10346 = add i64 0, 0                                                           ; quoted ()
  %cloptr13041 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr13043 = getelementptr inbounds i64, i64* %cloptr13041, i64 1                  ; &eptr13043[1]
  store i64 %d26$_37map, i64* %eptr13043                                             ; *eptr13043 = %d26$_37map
  %eptr13042 = getelementptr inbounds i64, i64* %cloptr13041, i64 0                  ; &cloptr13041[0]
  %f13044 = ptrtoint void(i64,i64,i64,i64)* @lam10432 to i64                         ; fptr cast; i64(...)* -> i64
  store i64 %f13044, i64* %eptr13042                                                 ; store fptr
  %arg10345 = ptrtoint i64* %cloptr13041 to i64                                      ; closure cast; i64* -> i64
  %cloptr13045 = inttoptr i64 %cont9497 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13046 = getelementptr inbounds i64, i64* %cloptr13045, i64 0                 ; &cloptr13045[0]
  %f13048 = load i64, i64* %i0ptr13046, align 8                                      ; load; *i0ptr13046
  %fptr13047 = inttoptr i64 %f13048 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13047(i64 %cont9497, i64 %arg10346, i64 %arg10345)  ; tail call
  ret void
}


define void @lam10432(i64 %env10433, i64 %cont9498, i64 %T9r$f, i64 %qVC$lst) {
  %envptr13049 = inttoptr i64 %env10433 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13050 = getelementptr inbounds i64, i64* %envptr13049, i64 1                ; &envptr13049[1]
  %d26$_37map = load i64, i64* %envptr13050, align 8                                 ; load; *envptr13050
  %a9196 = call i64 @prim_null_63(i64 %qVC$lst)                                      ; call prim_null_63
  %cmp13051 = icmp eq i64 %a9196, 15                                                 ; false?
  br i1 %cmp13051, label %else13053, label %then13052                                ; if

then13052:
  %arg10350 = add i64 0, 0                                                           ; quoted ()
  %arg10349 = add i64 0, 0                                                           ; quoted ()
  %cloptr13054 = inttoptr i64 %cont9498 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13055 = getelementptr inbounds i64, i64* %cloptr13054, i64 0                 ; &cloptr13054[0]
  %f13057 = load i64, i64* %i0ptr13055, align 8                                      ; load; *i0ptr13055
  %fptr13056 = inttoptr i64 %f13057 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13056(i64 %cont9498, i64 %arg10350, i64 %arg10349)  ; tail call
  ret void

else13053:
  %a9197 = call i64 @prim_car(i64 %qVC$lst)                                          ; call prim_car
  %cloptr13058 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr13060 = getelementptr inbounds i64, i64* %cloptr13058, i64 1                  ; &eptr13060[1]
  %eptr13061 = getelementptr inbounds i64, i64* %cloptr13058, i64 2                  ; &eptr13061[2]
  %eptr13062 = getelementptr inbounds i64, i64* %cloptr13058, i64 3                  ; &eptr13062[3]
  %eptr13063 = getelementptr inbounds i64, i64* %cloptr13058, i64 4                  ; &eptr13063[4]
  store i64 %T9r$f, i64* %eptr13060                                                  ; *eptr13060 = %T9r$f
  store i64 %d26$_37map, i64* %eptr13061                                             ; *eptr13061 = %d26$_37map
  store i64 %cont9498, i64* %eptr13062                                               ; *eptr13062 = %cont9498
  store i64 %qVC$lst, i64* %eptr13063                                                ; *eptr13063 = %qVC$lst
  %eptr13059 = getelementptr inbounds i64, i64* %cloptr13058, i64 0                  ; &cloptr13058[0]
  %f13064 = ptrtoint void(i64,i64,i64)* @lam10430 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f13064, i64* %eptr13059                                                 ; store fptr
  %arg10354 = ptrtoint i64* %cloptr13058 to i64                                      ; closure cast; i64* -> i64
  %cloptr13065 = inttoptr i64 %T9r$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13066 = getelementptr inbounds i64, i64* %cloptr13065, i64 0                 ; &cloptr13065[0]
  %f13068 = load i64, i64* %i0ptr13066, align 8                                      ; load; *i0ptr13066
  %fptr13067 = inttoptr i64 %f13068 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13067(i64 %T9r$f, i64 %arg10354, i64 %a9197)        ; tail call
  ret void
}


define void @lam10430(i64 %env10431, i64 %_959499, i64 %a9198) {
  %envptr13069 = inttoptr i64 %env10431 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13070 = getelementptr inbounds i64, i64* %envptr13069, i64 4                ; &envptr13069[4]
  %qVC$lst = load i64, i64* %envptr13070, align 8                                    ; load; *envptr13070
  %envptr13071 = inttoptr i64 %env10431 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13072 = getelementptr inbounds i64, i64* %envptr13071, i64 3                ; &envptr13071[3]
  %cont9498 = load i64, i64* %envptr13072, align 8                                   ; load; *envptr13072
  %envptr13073 = inttoptr i64 %env10431 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13074 = getelementptr inbounds i64, i64* %envptr13073, i64 2                ; &envptr13073[2]
  %d26$_37map = load i64, i64* %envptr13074, align 8                                 ; load; *envptr13074
  %envptr13075 = inttoptr i64 %env10431 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13076 = getelementptr inbounds i64, i64* %envptr13075, i64 1                ; &envptr13075[1]
  %T9r$f = load i64, i64* %envptr13076, align 8                                      ; load; *envptr13076
  %a9199 = call i64 @prim_cdr(i64 %qVC$lst)                                          ; call prim_cdr
  %cloptr13077 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13079 = getelementptr inbounds i64, i64* %cloptr13077, i64 1                  ; &eptr13079[1]
  %eptr13080 = getelementptr inbounds i64, i64* %cloptr13077, i64 2                  ; &eptr13080[2]
  store i64 %a9198, i64* %eptr13079                                                  ; *eptr13079 = %a9198
  store i64 %cont9498, i64* %eptr13080                                               ; *eptr13080 = %cont9498
  %eptr13078 = getelementptr inbounds i64, i64* %cloptr13077, i64 0                  ; &cloptr13077[0]
  %f13081 = ptrtoint void(i64,i64,i64)* @lam10428 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f13081, i64* %eptr13078                                                 ; store fptr
  %arg10359 = ptrtoint i64* %cloptr13077 to i64                                      ; closure cast; i64* -> i64
  %cloptr13082 = inttoptr i64 %d26$_37map to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr13083 = getelementptr inbounds i64, i64* %cloptr13082, i64 0                 ; &cloptr13082[0]
  %f13085 = load i64, i64* %i0ptr13083, align 8                                      ; load; *i0ptr13083
  %fptr13084 = inttoptr i64 %f13085 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13084(i64 %d26$_37map, i64 %arg10359, i64 %T9r$f, i64 %a9199); tail call
  ret void
}


define void @lam10428(i64 %env10429, i64 %_959500, i64 %a9200) {
  %envptr13086 = inttoptr i64 %env10429 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13087 = getelementptr inbounds i64, i64* %envptr13086, i64 2                ; &envptr13086[2]
  %cont9498 = load i64, i64* %envptr13087, align 8                                   ; load; *envptr13087
  %envptr13088 = inttoptr i64 %env10429 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13089 = getelementptr inbounds i64, i64* %envptr13088, i64 1                ; &envptr13088[1]
  %a9198 = load i64, i64* %envptr13089, align 8                                      ; load; *envptr13089
  %retprim9501 = call i64 @prim_cons(i64 %a9198, i64 %a9200)                         ; call prim_cons
  %arg10364 = add i64 0, 0                                                           ; quoted ()
  %cloptr13090 = inttoptr i64 %cont9498 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13091 = getelementptr inbounds i64, i64* %cloptr13090, i64 0                 ; &cloptr13090[0]
  %f13093 = load i64, i64* %i0ptr13091, align 8                                      ; load; *i0ptr13091
  %fptr13092 = inttoptr i64 %f13093 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13092(i64 %cont9498, i64 %arg10364, i64 %retprim9501); tail call
  ret void
}


define void @lam10423(i64 %env10424, i64 %cont9502, i64 %cVQ$_37foldr1) {
  %arg10367 = add i64 0, 0                                                           ; quoted ()
  %cloptr13094 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr13096 = getelementptr inbounds i64, i64* %cloptr13094, i64 1                  ; &eptr13096[1]
  store i64 %cVQ$_37foldr1, i64* %eptr13096                                          ; *eptr13096 = %cVQ$_37foldr1
  %eptr13095 = getelementptr inbounds i64, i64* %cloptr13094, i64 0                  ; &cloptr13094[0]
  %f13097 = ptrtoint void(i64,i64,i64,i64,i64)* @lam10420 to i64                     ; fptr cast; i64(...)* -> i64
  store i64 %f13097, i64* %eptr13095                                                 ; store fptr
  %arg10366 = ptrtoint i64* %cloptr13094 to i64                                      ; closure cast; i64* -> i64
  %cloptr13098 = inttoptr i64 %cont9502 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13099 = getelementptr inbounds i64, i64* %cloptr13098, i64 0                 ; &cloptr13098[0]
  %f13101 = load i64, i64* %i0ptr13099, align 8                                      ; load; *i0ptr13099
  %fptr13100 = inttoptr i64 %f13101 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13100(i64 %cont9502, i64 %arg10367, i64 %arg10366)  ; tail call
  ret void
}


define void @lam10420(i64 %env10421, i64 %cont9503, i64 %Gx7$f, i64 %BRG$acc, i64 %qV6$lst) {
  %envptr13102 = inttoptr i64 %env10421 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13103 = getelementptr inbounds i64, i64* %envptr13102, i64 1                ; &envptr13102[1]
  %cVQ$_37foldr1 = load i64, i64* %envptr13103, align 8                              ; load; *envptr13103
  %a9192 = call i64 @prim_null_63(i64 %qV6$lst)                                      ; call prim_null_63
  %cmp13104 = icmp eq i64 %a9192, 15                                                 ; false?
  br i1 %cmp13104, label %else13106, label %then13105                                ; if

then13105:
  %arg10371 = add i64 0, 0                                                           ; quoted ()
  %cloptr13107 = inttoptr i64 %cont9503 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13108 = getelementptr inbounds i64, i64* %cloptr13107, i64 0                 ; &cloptr13107[0]
  %f13110 = load i64, i64* %i0ptr13108, align 8                                      ; load; *i0ptr13108
  %fptr13109 = inttoptr i64 %f13110 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13109(i64 %cont9503, i64 %arg10371, i64 %BRG$acc)   ; tail call
  ret void

else13106:
  %a9193 = call i64 @prim_car(i64 %qV6$lst)                                          ; call prim_car
  %a9194 = call i64 @prim_cdr(i64 %qV6$lst)                                          ; call prim_cdr
  %cloptr13111 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13113 = getelementptr inbounds i64, i64* %cloptr13111, i64 1                  ; &eptr13113[1]
  %eptr13114 = getelementptr inbounds i64, i64* %cloptr13111, i64 2                  ; &eptr13114[2]
  %eptr13115 = getelementptr inbounds i64, i64* %cloptr13111, i64 3                  ; &eptr13115[3]
  store i64 %cont9503, i64* %eptr13113                                               ; *eptr13113 = %cont9503
  store i64 %a9193, i64* %eptr13114                                                  ; *eptr13114 = %a9193
  store i64 %Gx7$f, i64* %eptr13115                                                  ; *eptr13115 = %Gx7$f
  %eptr13112 = getelementptr inbounds i64, i64* %cloptr13111, i64 0                  ; &cloptr13111[0]
  %f13116 = ptrtoint void(i64,i64,i64)* @lam10418 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f13116, i64* %eptr13112                                                 ; store fptr
  %arg10378 = ptrtoint i64* %cloptr13111 to i64                                      ; closure cast; i64* -> i64
  %cloptr13117 = inttoptr i64 %cVQ$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr13118 = getelementptr inbounds i64, i64* %cloptr13117, i64 0                 ; &cloptr13117[0]
  %f13120 = load i64, i64* %i0ptr13118, align 8                                      ; load; *i0ptr13118
  %fptr13119 = inttoptr i64 %f13120 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13119(i64 %cVQ$_37foldr1, i64 %arg10378, i64 %Gx7$f, i64 %BRG$acc, i64 %a9194); tail call
  ret void
}


define void @lam10418(i64 %env10419, i64 %_959504, i64 %a9195) {
  %envptr13121 = inttoptr i64 %env10419 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13122 = getelementptr inbounds i64, i64* %envptr13121, i64 3                ; &envptr13121[3]
  %Gx7$f = load i64, i64* %envptr13122, align 8                                      ; load; *envptr13122
  %envptr13123 = inttoptr i64 %env10419 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13124 = getelementptr inbounds i64, i64* %envptr13123, i64 2                ; &envptr13123[2]
  %a9193 = load i64, i64* %envptr13124, align 8                                      ; load; *envptr13124
  %envptr13125 = inttoptr i64 %env10419 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13126 = getelementptr inbounds i64, i64* %envptr13125, i64 1                ; &envptr13125[1]
  %cont9503 = load i64, i64* %envptr13126, align 8                                   ; load; *envptr13126
  %cloptr13127 = inttoptr i64 %Gx7$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13128 = getelementptr inbounds i64, i64* %cloptr13127, i64 0                 ; &cloptr13127[0]
  %f13130 = load i64, i64* %i0ptr13128, align 8                                      ; load; *i0ptr13128
  %fptr13129 = inttoptr i64 %f13130 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13129(i64 %Gx7$f, i64 %cont9503, i64 %a9193, i64 %a9195); tail call
  ret void
}


define void @lam10415(i64 %env10416, i64 %cont9506, i64 %gie$y) {
  %arg10385 = add i64 0, 0                                                           ; quoted ()
  %cloptr13131 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr13133 = getelementptr inbounds i64, i64* %cloptr13131, i64 1                  ; &eptr13133[1]
  store i64 %gie$y, i64* %eptr13133                                                  ; *eptr13133 = %gie$y
  %eptr13132 = getelementptr inbounds i64, i64* %cloptr13131, i64 0                  ; &cloptr13131[0]
  %f13134 = ptrtoint void(i64,i64,i64)* @lam10412 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f13134, i64* %eptr13132                                                 ; store fptr
  %arg10384 = ptrtoint i64* %cloptr13131 to i64                                      ; closure cast; i64* -> i64
  %cloptr13135 = inttoptr i64 %cont9506 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13136 = getelementptr inbounds i64, i64* %cloptr13135, i64 0                 ; &cloptr13135[0]
  %f13138 = load i64, i64* %i0ptr13136, align 8                                      ; load; *i0ptr13136
  %fptr13137 = inttoptr i64 %f13138 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13137(i64 %cont9506, i64 %arg10385, i64 %arg10384)  ; tail call
  ret void
}


define void @lam10412(i64 %env10413, i64 %cont9507, i64 %Eh3$f) {
  %envptr13139 = inttoptr i64 %env10413 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13140 = getelementptr inbounds i64, i64* %envptr13139, i64 1                ; &envptr13139[1]
  %gie$y = load i64, i64* %envptr13140, align 8                                      ; load; *envptr13140
  %cloptr13141 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13143 = getelementptr inbounds i64, i64* %cloptr13141, i64 1                  ; &eptr13143[1]
  %eptr13144 = getelementptr inbounds i64, i64* %cloptr13141, i64 2                  ; &eptr13144[2]
  store i64 %Eh3$f, i64* %eptr13143                                                  ; *eptr13143 = %Eh3$f
  store i64 %gie$y, i64* %eptr13144                                                  ; *eptr13144 = %gie$y
  %eptr13142 = getelementptr inbounds i64, i64* %cloptr13141, i64 0                  ; &cloptr13141[0]
  %f13145 = ptrtoint void(i64,i64)* @lam10410 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13145, i64* %eptr13142                                                 ; store fptr
  %arg10387 = ptrtoint i64* %cloptr13141 to i64                                      ; closure cast; i64* -> i64
  %cloptr13146 = inttoptr i64 %Eh3$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13147 = getelementptr inbounds i64, i64* %cloptr13146, i64 0                 ; &cloptr13146[0]
  %f13149 = load i64, i64* %i0ptr13147, align 8                                      ; load; *i0ptr13147
  %fptr13148 = inttoptr i64 %f13149 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13148(i64 %Eh3$f, i64 %cont9507, i64 %arg10387)     ; tail call
  ret void
}


define void @lam10410(i64 %env10411, i64 %B6H$args9509) {
  %envptr13150 = inttoptr i64 %env10411 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13151 = getelementptr inbounds i64, i64* %envptr13150, i64 2                ; &envptr13150[2]
  %gie$y = load i64, i64* %envptr13151, align 8                                      ; load; *envptr13151
  %envptr13152 = inttoptr i64 %env10411 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13153 = getelementptr inbounds i64, i64* %envptr13152, i64 1                ; &envptr13152[1]
  %Eh3$f = load i64, i64* %envptr13153, align 8                                      ; load; *envptr13153
  %cont9508 = call i64 @prim_car(i64 %B6H$args9509)                                  ; call prim_car
  %B6H$args = call i64 @prim_cdr(i64 %B6H$args9509)                                  ; call prim_cdr
  %cloptr13154 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13156 = getelementptr inbounds i64, i64* %cloptr13154, i64 1                  ; &eptr13156[1]
  %eptr13157 = getelementptr inbounds i64, i64* %cloptr13154, i64 2                  ; &eptr13157[2]
  %eptr13158 = getelementptr inbounds i64, i64* %cloptr13154, i64 3                  ; &eptr13158[3]
  store i64 %Eh3$f, i64* %eptr13156                                                  ; *eptr13156 = %Eh3$f
  store i64 %B6H$args, i64* %eptr13157                                               ; *eptr13157 = %B6H$args
  store i64 %cont9508, i64* %eptr13158                                               ; *eptr13158 = %cont9508
  %eptr13155 = getelementptr inbounds i64, i64* %cloptr13154, i64 0                  ; &cloptr13154[0]
  %f13159 = ptrtoint void(i64,i64,i64)* @lam10408 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f13159, i64* %eptr13155                                                 ; store fptr
  %arg10393 = ptrtoint i64* %cloptr13154 to i64                                      ; closure cast; i64* -> i64
  %cloptr13160 = inttoptr i64 %gie$y to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13161 = getelementptr inbounds i64, i64* %cloptr13160, i64 0                 ; &cloptr13160[0]
  %f13163 = load i64, i64* %i0ptr13161, align 8                                      ; load; *i0ptr13161
  %fptr13162 = inttoptr i64 %f13163 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13162(i64 %gie$y, i64 %arg10393, i64 %gie$y)        ; tail call
  ret void
}


define void @lam10408(i64 %env10409, i64 %_959510, i64 %a9190) {
  %envptr13164 = inttoptr i64 %env10409 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13165 = getelementptr inbounds i64, i64* %envptr13164, i64 3                ; &envptr13164[3]
  %cont9508 = load i64, i64* %envptr13165, align 8                                   ; load; *envptr13165
  %envptr13166 = inttoptr i64 %env10409 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13167 = getelementptr inbounds i64, i64* %envptr13166, i64 2                ; &envptr13166[2]
  %B6H$args = load i64, i64* %envptr13167, align 8                                   ; load; *envptr13167
  %envptr13168 = inttoptr i64 %env10409 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13169 = getelementptr inbounds i64, i64* %envptr13168, i64 1                ; &envptr13168[1]
  %Eh3$f = load i64, i64* %envptr13169, align 8                                      ; load; *envptr13169
  %cloptr13170 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13172 = getelementptr inbounds i64, i64* %cloptr13170, i64 1                  ; &eptr13172[1]
  %eptr13173 = getelementptr inbounds i64, i64* %cloptr13170, i64 2                  ; &eptr13173[2]
  store i64 %B6H$args, i64* %eptr13172                                               ; *eptr13172 = %B6H$args
  store i64 %cont9508, i64* %eptr13173                                               ; *eptr13173 = %cont9508
  %eptr13171 = getelementptr inbounds i64, i64* %cloptr13170, i64 0                  ; &cloptr13170[0]
  %f13174 = ptrtoint void(i64,i64,i64)* @lam10406 to i64                             ; fptr cast; i64(...)* -> i64
  store i64 %f13174, i64* %eptr13171                                                 ; store fptr
  %arg10396 = ptrtoint i64* %cloptr13170 to i64                                      ; closure cast; i64* -> i64
  %cloptr13175 = inttoptr i64 %a9190 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13176 = getelementptr inbounds i64, i64* %cloptr13175, i64 0                 ; &cloptr13175[0]
  %f13178 = load i64, i64* %i0ptr13176, align 8                                      ; load; *i0ptr13176
  %fptr13177 = inttoptr i64 %f13178 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13177(i64 %a9190, i64 %arg10396, i64 %Eh3$f)        ; tail call
  ret void
}


define void @lam10406(i64 %env10407, i64 %_959511, i64 %a9191) {
  %envptr13179 = inttoptr i64 %env10407 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13180 = getelementptr inbounds i64, i64* %envptr13179, i64 2                ; &envptr13179[2]
  %cont9508 = load i64, i64* %envptr13180, align 8                                   ; load; *envptr13180
  %envptr13181 = inttoptr i64 %env10407 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13182 = getelementptr inbounds i64, i64* %envptr13181, i64 1                ; &envptr13181[1]
  %B6H$args = load i64, i64* %envptr13182, align 8                                   ; load; *envptr13182
  %cps_45lst9512 = call i64 @prim_cons(i64 %cont9508, i64 %B6H$args)                 ; call prim_cons
  %cloptr13183 = inttoptr i64 %a9191 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13184 = getelementptr inbounds i64, i64* %cloptr13183, i64 0                 ; &cloptr13183[0]
  %f13186 = load i64, i64* %i0ptr13184, align 8                                      ; load; *i0ptr13184
  %fptr13185 = inttoptr i64 %f13186 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13185(i64 %a9191, i64 %cps_45lst9512)               ; tail call
  ret void
}





@sym11732 = private unnamed_addr constant [10 x i8] c"%%promise\00", align 8
