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
