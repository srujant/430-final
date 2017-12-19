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
@.str.3 = private unnamed_addr constant [117 x i8] c"One of the variables was given a value that was larger than the maximum value of an int causing an integer overflow.\00", align 1
@.str.4 = private unnamed_addr constant [5 x i8] c"%lu\0A\00", align 1
@.str.5 = private unnamed_addr constant [68 x i8] c"Expected value: null (in expect_args0). Prim cannot take arguments.\00", align 1
@.str.6 = private unnamed_addr constant [79 x i8] c"Expected cons value (in expect_args1). Prim applied on an empty argument list.\00", align 1
@.str.7 = private unnamed_addr constant [70 x i8] c"Expected null value (in expect_args1). Prim can only take 1 argument.\00", align 1
@.str.8 = private unnamed_addr constant [37 x i8] c"Expected a cons value. (expect_cons)\00", align 1
@.str.9 = private unnamed_addr constant [51 x i8] c"Expected a vector or special value. (expect_other)\00", align 1
@.str.10 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.11 = private unnamed_addr constant [13 x i8] c"#<procedure>\00", align 1
@.str.12 = private unnamed_addr constant [2 x i8] c"(\00", align 1
@.str.13 = private unnamed_addr constant [4 x i8] c" . \00", align 1
@.str.14 = private unnamed_addr constant [2 x i8] c")\00", align 1
@.str.15 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.16 = private unnamed_addr constant [5 x i8] c"\22%s\22\00", align 1
@.str.17 = private unnamed_addr constant [3 x i8] c"#(\00", align 1
@.str.18 = private unnamed_addr constant [2 x i8] c",\00", align 1
@.str.19 = private unnamed_addr constant [5 x i8] c"HERE\00", align 1
@.str.20 = private unnamed_addr constant [36 x i8] c"(print.. v); unrecognized value %lu\00", align 1
@.str.21 = private unnamed_addr constant [4 x i8] c"'()\00", align 1
@.str.22 = private unnamed_addr constant [3 x i8] c"'(\00", align 1
@.str.23 = private unnamed_addr constant [4 x i8] c"'%s\00", align 1
@.str.24 = private unnamed_addr constant [7 x i8] c"#hash(\00", align 1
@.str.25 = private unnamed_addr constant [10 x i8] c"(%d . %d)\00", align 1
@.str.26 = private unnamed_addr constant [11 x i8] c"(%d . %d) \00", align 1
@.str.27 = private unnamed_addr constant [34 x i8] c"(print v); unrecognized value %lu\00", align 1
@.str.28 = private unnamed_addr constant [28 x i8] c"Memory cap exceeded 256 mb.\00", align 1
@.str.29 = private unnamed_addr constant [49 x i8] c"first argument to make-vector must be an integer\00", align 1
@.str.30 = private unnamed_addr constant [39 x i8] c"prim applied on more than 2 arguments.\00", align 1
@.str.31 = private unnamed_addr constant [49 x i8] c"second argument to vector-ref must be an integer\00", align 1
@.str.32 = private unnamed_addr constant [46 x i8] c"first argument to vector-ref must be a vector\00", align 1
@.str.33 = private unnamed_addr constant [46 x i8] c"vector-ref not given a properly formed vector\00", align 1
@.str.34 = private unnamed_addr constant [48 x i8] c"first argument to vector-ref must be an integer\00", align 1
@.str.35 = private unnamed_addr constant [34 x i8] c"(prim + a b); a is not an integer\00", align 1
@.str.36 = private unnamed_addr constant [34 x i8] c"(prim + a b); b is not an integer\00", align 1
@.str.37 = private unnamed_addr constant [36 x i8] c"Tried to apply + on non list value.\00", align 1
@.str.38 = private unnamed_addr constant [34 x i8] c"(prim - a b); b is not an integer\00", align 1
@.str.39 = private unnamed_addr constant [34 x i8] c"(prim * a b); a is not an integer\00", align 1
@.str.40 = private unnamed_addr constant [34 x i8] c"(prim * a b); b is not an integer\00", align 1
@.str.41 = private unnamed_addr constant [34 x i8] c"(prim / a b); a is not an integer\00", align 1
@.str.42 = private unnamed_addr constant [34 x i8] c"(prim / a b); b is not an integer\00", align 1
@.str.43 = private unnamed_addr constant [41 x i8] c"Division by 0 error. Cannot divide by 0.\00", align 1
@.str.44 = private unnamed_addr constant [34 x i8] c"(prim = a b); a is not an integer\00", align 1
@.str.45 = private unnamed_addr constant [34 x i8] c"(prim = a b); b is not an integer\00", align 1
@.str.46 = private unnamed_addr constant [34 x i8] c"(prim < a b); a is not an integer\00", align 1
@.str.47 = private unnamed_addr constant [34 x i8] c"(prim < a b); b is not an integer\00", align 1
@.str.48 = private unnamed_addr constant [35 x i8] c"(prim <= a b); a is not an integer\00", align 1
@.str.49 = private unnamed_addr constant [35 x i8] c"(prim <= a b); b is not an integer\00", align 1
@.str.50 = private unnamed_addr constant [17 x i8] c"not given a list\00", align 1
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
  call void @exit(i32 1) #8
  unreachable
                                                  ; No predecessors!
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noreturn nounwind
declare void @exit(i32) #3

; Function Attrs: nounwind uwtable
define i64* @alloc(i64 %m) #4 {
  %1 = alloca i64, align 8
  store i64 %m, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call noalias i8* @malloc(i64 %2) #2
  %4 = bitcast i8* %3 to i64*
  ret i64* %4
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #5

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
  call void @fatal_err(i8* getelementptr inbounds ([117 x i8], [117 x i8]* @.str.3, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  ret void
}

; Function Attrs: uwtable
define void @print_u64(i64 %i) #0 {
  %1 = alloca i64, align 8
  store i64 %i, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.4, i32 0, i32 0), i64 %2)
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
  call void @fatal_err(i8* getelementptr inbounds ([68 x i8], [68 x i8]* @.str.5, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([79 x i8], [79 x i8]* @.str.6, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([70 x i8], [70 x i8]* @.str.7, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.8, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([51 x i8], [51 x i8]* @.str.9, i32 0, i32 0))
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
define i64 @const_init_int(i64 %i) #4 {
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
define i64 @const_init_void() #4 {
  ret i64 39
}

; Function Attrs: nounwind uwtable
define i64 @const_init_null() #4 {
  ret i64 0
}

; Function Attrs: nounwind uwtable
define i64 @const_init_true() #4 {
  ret i64 31
}

; Function Attrs: nounwind uwtable
define i64 @const_init_false() #4 {
  ret i64 15
}

; Function Attrs: nounwind uwtable
define i64 @const_init_string(i8* %s) #4 {
  %1 = alloca i8*, align 8
  store i8* %s, i8** %1, align 8
  %2 = load i8*, i8** %1, align 8
  %3 = ptrtoint i8* %2 to i64
  %4 = or i64 %3, 3
  ret i64 %4
}

; Function Attrs: nounwind uwtable
define i64 @const_init_symbol(i8* %s) #4 {
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
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.10, i32 0, i32 0))
  br label %122

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 7
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %10, label %12

; <label>:10                                      ; preds = %6
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.11, i32 0, i32 0))
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
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.12, i32 0, i32 0))
  %21 = load i64*, i64** %p, align 8
  %22 = getelementptr inbounds i64, i64* %21, i64 0
  %23 = load i64, i64* %22, align 8
  %24 = call i64 @prim_print_aux(i64 %23)
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.13, i32 0, i32 0))
  %26 = load i64*, i64** %p, align 8
  %27 = getelementptr inbounds i64, i64* %26, i64 1
  %28 = load i64, i64* %27, align 8
  %29 = call i64 @prim_print_aux(i64 %28)
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i32 0, i32 0))
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
  %39 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.15, i32 0, i32 0), i32 %38)
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
  %48 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.16, i32 0, i32 0), i8* %47)
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
  %71 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.17, i32 0, i32 0))
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
  %88 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.18, i32 0, i32 0))
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
  %98 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i32 0, i32 0))
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
  %111 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.19, i32 0, i32 0))
  br label %115

; <label>:112                                     ; preds = %103, %99
  %113 = load i64, i64* %1, align 8
  %114 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.20, i32 0, i32 0), i64 %113)
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
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.21, i32 0, i32 0))
  br label %179

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 7
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %10, label %12

; <label>:10                                      ; preds = %6
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.11, i32 0, i32 0))
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
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.22, i32 0, i32 0))
  %21 = load i64*, i64** %p, align 8
  %22 = getelementptr inbounds i64, i64* %21, i64 0
  %23 = load i64, i64* %22, align 8
  %24 = call i64 @prim_print_aux(i64 %23)
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.13, i32 0, i32 0))
  %26 = load i64*, i64** %p, align 8
  %27 = getelementptr inbounds i64, i64* %26, i64 1
  %28 = load i64, i64* %27, align 8
  %29 = call i64 @prim_print_aux(i64 %28)
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i32 0, i32 0))
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
  %39 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.15, i32 0, i32 0), i32 %38)
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
  %48 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.16, i32 0, i32 0), i8* %47)
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
  %57 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.23, i32 0, i32 0), i8* %56)
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
  %71 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.17, i32 0, i32 0))
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
  %88 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.18, i32 0, i32 0))
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
  %98 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i32 0, i32 0))
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
  %111 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([7 x i8], [7 x i8]* @.str.24, i32 0, i32 0))
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
  %145 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.25, i32 0, i32 0), i32 %138, i32 %144)
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
  %159 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([11 x i8], [11 x i8]* @.str.26, i32 0, i32 0), i32 %152, i32 %158)
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
  %168 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.14, i32 0, i32 0))
  br label %172

; <label>:169                                     ; preds = %103, %99
  %170 = load i64, i64* %1, align 8
  %171 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.27, i32 0, i32 0), i64 %170)
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
  call void @exit(i32 0) #8
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
  %2 = call noalias i8* @malloc(i64 4096) #2
  %3 = bitcast i8* %2 to i64*
  store i64* %3, i64** %buffer, align 8
  %4 = load i64, i64* @memory_cap, align 8
  %5 = and i64 %4, -8
  %6 = lshr i64 %5, 32
  %7 = trunc i64 %6 to i32
  %8 = sext i32 %7 to i64
  %9 = add i64 %8, 4096
  %10 = trunc i64 %9 to i32
  %11 = zext i32 %10 to i64
  %12 = shl i64 %11, 32
  %13 = or i64 %12, 2
  store i64 %13, i64* @memory_cap, align 8
  %14 = load i64, i64* @memory_cap, align 8
  %15 = and i64 %14, -8
  %16 = lshr i64 %15, 32
  %17 = trunc i64 %16 to i32
  %18 = icmp sgt i32 %17, 268435456
  br i1 %18, label %19, label %20

; <label>:19                                      ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([28 x i8], [28 x i8]* @.str.28, i32 0, i32 0))
  br label %20

; <label>:20                                      ; preds = %19, %0
  store i64 0, i64* %l, align 8
  br label %21

; <label>:21                                      ; preds = %30, %20
  %22 = load i64, i64* %1, align 8
  %23 = and i64 %22, 7
  %24 = icmp eq i64 %23, 1
  br i1 %24, label %25, label %28

; <label>:25                                      ; preds = %21
  %26 = load i64, i64* %l, align 8
  %27 = icmp ult i64 %26, 512
  br label %28

; <label>:28                                      ; preds = %25, %21
  %29 = phi i1 [ false, %21 ], [ %27, %25 ]
  br i1 %29, label %30, label %37

; <label>:30                                      ; preds = %28
  %31 = load i64, i64* %1, align 8
  %32 = call i64 @expect_cons(i64 %31, i64* %1)
  %33 = load i64, i64* %l, align 8
  %34 = add i64 %33, 1
  store i64 %34, i64* %l, align 8
  %35 = load i64*, i64** %buffer, align 8
  %36 = getelementptr inbounds i64, i64* %35, i64 %33
  store i64 %32, i64* %36, align 8
  br label %21

; <label>:37                                      ; preds = %28
  %38 = load i64, i64* %l, align 8
  %39 = add i64 %38, 1
  %40 = mul i64 %39, 8
  %41 = call i64* @alloc(i64 %40)
  store i64* %41, i64** %mem, align 8
  %42 = load i64, i64* @memory_cap, align 8
  %43 = and i64 %42, -8
  %44 = lshr i64 %43, 32
  %45 = trunc i64 %44 to i32
  %46 = sext i32 %45 to i64
  %47 = load i64, i64* %l, align 8
  %48 = add i64 %47, 1
  %49 = mul i64 %48, 8
  %50 = add i64 %46, %49
  %51 = trunc i64 %50 to i32
  %52 = zext i32 %51 to i64
  %53 = shl i64 %52, 32
  %54 = or i64 %53, 2
  store i64 %54, i64* @memory_cap, align 8
  %55 = load i64, i64* @memory_cap, align 8
  %56 = and i64 %55, -8
  %57 = lshr i64 %56, 32
  %58 = trunc i64 %57 to i32
  %59 = icmp sgt i32 %58, 268435456
  br i1 %59, label %60, label %61

; <label>:60                                      ; preds = %37
  call void @fatal_err(i8* getelementptr inbounds ([28 x i8], [28 x i8]* @.str.28, i32 0, i32 0))
  br label %61

; <label>:61                                      ; preds = %60, %37
  %62 = load i64, i64* %l, align 8
  %63 = shl i64 %62, 3
  %64 = or i64 %63, 1
  %65 = load i64*, i64** %mem, align 8
  %66 = getelementptr inbounds i64, i64* %65, i64 0
  store i64 %64, i64* %66, align 8
  store i64 0, i64* %i, align 8
  br label %67

; <label>:67                                      ; preds = %80, %61
  %68 = load i64, i64* %i, align 8
  %69 = load i64, i64* %l, align 8
  %70 = icmp ult i64 %68, %69
  br i1 %70, label %71, label %83

; <label>:71                                      ; preds = %67
  %72 = load i64, i64* %i, align 8
  %73 = load i64*, i64** %buffer, align 8
  %74 = getelementptr inbounds i64, i64* %73, i64 %72
  %75 = load i64, i64* %74, align 8
  %76 = load i64, i64* %i, align 8
  %77 = add i64 %76, 1
  %78 = load i64*, i64** %mem, align 8
  %79 = getelementptr inbounds i64, i64* %78, i64 %77
  store i64 %75, i64* %79, align 8
  br label %80

; <label>:80                                      ; preds = %71
  %81 = load i64, i64* %i, align 8
  %82 = add i64 %81, 1
  store i64 %82, i64* %i, align 8
  br label %67

; <label>:83                                      ; preds = %67
  %84 = load i64*, i64** %buffer, align 8
  %85 = icmp eq i64* %84, null
  br i1 %85, label %88, label %86

; <label>:86                                      ; preds = %83
  %87 = bitcast i64* %84 to i8*
  call void @_ZdaPv(i8* %87) #9
  br label %88

; <label>:88                                      ; preds = %86, %83
  %89 = load i64*, i64** %mem, align 8
  %90 = ptrtoint i64* %89 to i64
  %91 = or i64 %90, 6
  ret i64 %91
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
  %17 = load i64, i64* @memory_cap, align 8
  %18 = and i64 %17, -8
  %19 = lshr i64 %18, 32
  %20 = trunc i64 %19 to i32
  %21 = sext i32 %20 to i64
  %22 = load i64, i64* %l, align 8
  %23 = add i64 %22, 1
  %24 = mul i64 %23, 8
  %25 = add i64 %21, %24
  %26 = trunc i64 %25 to i32
  %27 = zext i32 %26 to i64
  %28 = shl i64 %27, 32
  %29 = or i64 %28, 2
  store i64 %29, i64* @memory_cap, align 8
  %30 = load i64, i64* @memory_cap, align 8
  %31 = and i64 %30, -8
  %32 = lshr i64 %31, 32
  %33 = trunc i64 %32 to i32
  %34 = icmp sgt i32 %33, 268435456
  br i1 %34, label %35, label %36

; <label>:35                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([28 x i8], [28 x i8]* @.str.28, i32 0, i32 0))
  br label %36

; <label>:36                                      ; preds = %35, %7
  %37 = load i64, i64* %l, align 8
  %38 = shl i64 %37, 3
  %39 = or i64 %38, 1
  %40 = load i64*, i64** %vec, align 8
  %41 = getelementptr inbounds i64, i64* %40, i64 0
  store i64 %39, i64* %41, align 8
  store i64 1, i64* %i, align 8
  br label %42

; <label>:42                                      ; preds = %51, %36
  %43 = load i64, i64* %i, align 8
  %44 = load i64, i64* %l, align 8
  %45 = icmp ule i64 %43, %44
  br i1 %45, label %46, label %54

; <label>:46                                      ; preds = %42
  %47 = load i64, i64* %2, align 8
  %48 = load i64, i64* %i, align 8
  %49 = load i64*, i64** %vec, align 8
  %50 = getelementptr inbounds i64, i64* %49, i64 %48
  store i64 %47, i64* %50, align 8
  br label %51

; <label>:51                                      ; preds = %46
  %52 = load i64, i64* %i, align 8
  %53 = add i64 %52, 1
  store i64 %53, i64* %i, align 8
  br label %42

; <label>:54                                      ; preds = %42
  %55 = load i64*, i64** %vec, align 8
  %56 = ptrtoint i64* %55 to i64
  %57 = or i64 %56, 6
  ret i64 %57
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
  %25 = load i64, i64* %2, align 8
  %26 = and i64 %25, -8
  %27 = lshr i64 %26, 32
  %28 = trunc i64 %27 to i32
  %29 = add nsw i32 1, %28
  %30 = sext i32 %29 to i64
  %31 = load i64, i64* %1, align 8
  %32 = and i64 %31, -8
  %33 = inttoptr i64 %32 to i64*
  %34 = getelementptr inbounds i64, i64* %33, i64 %30
  %35 = load i64, i64* %34, align 8
  ret i64 %35
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
  call void @fatal_err(i8* getelementptr inbounds ([48 x i8], [48 x i8]* @.str.34, i32 0, i32 0))
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
  %23 = load i64, i64* %3, align 8
  %24 = load i64, i64* %2, align 8
  %25 = and i64 %24, -8
  %26 = lshr i64 %25, 32
  %27 = trunc i64 %26 to i32
  %28 = add nsw i32 1, %27
  %29 = sext i32 %28 to i64
  %30 = load i64, i64* %1, align 8
  %31 = and i64 %30, -8
  %32 = inttoptr i64 %31 to i64*
  %33 = getelementptr inbounds i64, i64* %32, i64 %29
  store i64 %23, i64* %33, align 8
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
define i64 @prim_void() #4 {
  ret i64 39
}

; Function Attrs: nounwind uwtable
define i64 @prim_eq_63(i64 %a, i64 %b) #4 {
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
define i64 @prim_eqv_63(i64 %a, i64 %b) #4 {
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
define i64 @prim_number_63(i64 %a) #4 {
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
define i64 @prim_integer_63(i64 %a) #4 {
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
define i64 @prim_void_63(i64 %a) #4 {
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
define i64 @prim_procedure_63(i64 %a) #4 {
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
define i64 @prim_null_63(i64 %p) #4 {
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
define i64 @prim_cons_63(i64 %p) #4 {
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
  %4 = load i64, i64* @memory_cap, align 8
  %5 = and i64 %4, -8
  %6 = lshr i64 %5, 32
  %7 = trunc i64 %6 to i32
  %8 = sext i32 %7 to i64
  %9 = add i64 %8, 16
  %10 = trunc i64 %9 to i32
  %11 = zext i32 %10 to i64
  %12 = shl i64 %11, 32
  %13 = or i64 %12, 2
  store i64 %13, i64* @memory_cap, align 8
  %14 = load i64, i64* @memory_cap, align 8
  %15 = and i64 %14, -8
  %16 = lshr i64 %15, 32
  %17 = trunc i64 %16 to i32
  %18 = icmp sgt i32 %17, 268435456
  br i1 %18, label %19, label %20

; <label>:19                                      ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([28 x i8], [28 x i8]* @.str.28, i32 0, i32 0))
  br label %20

; <label>:20                                      ; preds = %19, %0
  %21 = load i64, i64* %1, align 8
  %22 = load i64*, i64** %p, align 8
  %23 = getelementptr inbounds i64, i64* %22, i64 0
  store i64 %21, i64* %23, align 8
  %24 = load i64, i64* %2, align 8
  %25 = load i64*, i64** %p, align 8
  %26 = getelementptr inbounds i64, i64* %25, i64 1
  store i64 %24, i64* %26, align 8
  %27 = load i64*, i64** %p, align 8
  %28 = ptrtoint i64* %27 to i64
  %29 = or i64 %28, 1
  ret i64 %29
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.35, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.36, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.37, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.35, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.38, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.37, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.39, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.40, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.37, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.41, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.42, i32 0, i32 0))
  br label %12

; <label>:12                                      ; preds = %11, %7
  %13 = load i64, i64* %2, align 8
  %14 = icmp eq i64 %13, 2
  br i1 %14, label %15, label %16

; <label>:15                                      ; preds = %12
  call void @fatal_err(i8* getelementptr inbounds ([41 x i8], [41 x i8]* @.str.43, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.44, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.45, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.46, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.47, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.48, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.49, i32 0, i32 0))
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
define i64 @prim_not(i64 %a) #4 {
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
  %keylen = alloca i64, align 8
  %key = alloca i64, align 8
  %value = alloca i64, align 8
  %_ha_hashv = alloca i32, align 4
  %_hj_i = alloca i32, align 4
  %_hj_j = alloca i32, align 4
  %_hj_k = alloca i32, align 4
  %_hj_key = alloca i8*, align 8
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
  call void @fatal_err(i8* getelementptr inbounds ([17 x i8], [17 x i8]* @.str.50, i32 0, i32 0))
  br label %6

; <label>:6                                       ; preds = %5, %0
  store %struct.hash_tbl* null, %struct.hash_tbl** %new_pair, align 8
  store %struct.hash_tbl* null, %struct.hash_tbl** %new_table, align 8
  %7 = call i64* @alloc(i64 144)
  store i64* %7, i64** %vec, align 8
  %8 = load i64*, i64** %vec, align 8
  %9 = getelementptr inbounds i64, i64* %8, i64 0
  store i64 2, i64* %9, align 8
  br label %10

; <label>:10                                      ; preds = %886, %6
  %11 = load i64, i64* %1, align 8
  %12 = and i64 %11, 7
  %13 = icmp eq i64 %12, 1
  br i1 %13, label %14, label %887

; <label>:14                                      ; preds = %10
  %15 = call noalias i8* @malloc(i64 72) #2
  %16 = bitcast i8* %15 to %struct.hash_tbl*
  store %struct.hash_tbl* %16, %struct.hash_tbl** %new_pair, align 8
  store i64 8, i64* %keylen, align 8
  %17 = load i64, i64* %1, align 8
  %18 = call i64 @prim_car(i64 %17)
  store i64 %18, i64* %pair, align 8
  %19 = load i64, i64* %pair, align 8
  %20 = call i64 @prim_car(i64 %19)
  %21 = or i64 %20, 6
  store i64 %21, i64* %key, align 8
  %22 = load i64, i64* %pair, align 8
  %23 = call i64 @prim_cdr(i64 %22)
  %24 = or i64 %23, 6
  store i64 %24, i64* %value, align 8
  %25 = load i64, i64* %key, align 8
  %26 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %27 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %26, i32 0, i32 0
  store i64 %25, i64* %27, align 8
  %28 = load i64, i64* %value, align 8
  %29 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %30 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %29, i32 0, i32 1
  store i64 %28, i64* %30, align 8
  %31 = load i64, i64* %1, align 8
  %32 = call i64 @prim_cdr(i64 %31)
  store i64 %32, i64* %1, align 8
  br label %33

; <label>:33                                      ; preds = %14
  br label %34

; <label>:34                                      ; preds = %33
  br label %35

; <label>:35                                      ; preds = %34
  %36 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %37 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %36, i32 0, i32 0
  %38 = bitcast i64* %37 to i8*
  store i8* %38, i8** %_hj_key, align 8
  store i32 -17973521, i32* %_ha_hashv, align 4
  store i32 -1640531527, i32* %_hj_j, align 4
  store i32 -1640531527, i32* %_hj_i, align 4
  %39 = load i64, i64* %keylen, align 8
  %40 = and i64 %39, -8
  %41 = lshr i64 %40, 32
  %42 = trunc i64 %41 to i32
  store i32 %42, i32* %_hj_k, align 4
  br label %43

; <label>:43                                      ; preds = %210, %35
  %44 = load i32, i32* %_hj_k, align 4
  %45 = icmp uge i32 %44, 12
  br i1 %45, label %46, label %215

; <label>:46                                      ; preds = %43
  %47 = load i8*, i8** %_hj_key, align 8
  %48 = getelementptr inbounds i8, i8* %47, i64 0
  %49 = load i8, i8* %48, align 1
  %50 = zext i8 %49 to i32
  %51 = load i8*, i8** %_hj_key, align 8
  %52 = getelementptr inbounds i8, i8* %51, i64 1
  %53 = load i8, i8* %52, align 1
  %54 = zext i8 %53 to i32
  %55 = shl i32 %54, 8
  %56 = add i32 %50, %55
  %57 = load i8*, i8** %_hj_key, align 8
  %58 = getelementptr inbounds i8, i8* %57, i64 2
  %59 = load i8, i8* %58, align 1
  %60 = zext i8 %59 to i32
  %61 = shl i32 %60, 16
  %62 = add i32 %56, %61
  %63 = load i8*, i8** %_hj_key, align 8
  %64 = getelementptr inbounds i8, i8* %63, i64 3
  %65 = load i8, i8* %64, align 1
  %66 = zext i8 %65 to i32
  %67 = shl i32 %66, 24
  %68 = add i32 %62, %67
  %69 = load i32, i32* %_hj_i, align 4
  %70 = add i32 %69, %68
  store i32 %70, i32* %_hj_i, align 4
  %71 = load i8*, i8** %_hj_key, align 8
  %72 = getelementptr inbounds i8, i8* %71, i64 4
  %73 = load i8, i8* %72, align 1
  %74 = zext i8 %73 to i32
  %75 = load i8*, i8** %_hj_key, align 8
  %76 = getelementptr inbounds i8, i8* %75, i64 5
  %77 = load i8, i8* %76, align 1
  %78 = zext i8 %77 to i32
  %79 = shl i32 %78, 8
  %80 = add i32 %74, %79
  %81 = load i8*, i8** %_hj_key, align 8
  %82 = getelementptr inbounds i8, i8* %81, i64 6
  %83 = load i8, i8* %82, align 1
  %84 = zext i8 %83 to i32
  %85 = shl i32 %84, 16
  %86 = add i32 %80, %85
  %87 = load i8*, i8** %_hj_key, align 8
  %88 = getelementptr inbounds i8, i8* %87, i64 7
  %89 = load i8, i8* %88, align 1
  %90 = zext i8 %89 to i32
  %91 = shl i32 %90, 24
  %92 = add i32 %86, %91
  %93 = load i32, i32* %_hj_j, align 4
  %94 = add i32 %93, %92
  store i32 %94, i32* %_hj_j, align 4
  %95 = load i8*, i8** %_hj_key, align 8
  %96 = getelementptr inbounds i8, i8* %95, i64 8
  %97 = load i8, i8* %96, align 1
  %98 = zext i8 %97 to i32
  %99 = load i8*, i8** %_hj_key, align 8
  %100 = getelementptr inbounds i8, i8* %99, i64 9
  %101 = load i8, i8* %100, align 1
  %102 = zext i8 %101 to i32
  %103 = shl i32 %102, 8
  %104 = add i32 %98, %103
  %105 = load i8*, i8** %_hj_key, align 8
  %106 = getelementptr inbounds i8, i8* %105, i64 10
  %107 = load i8, i8* %106, align 1
  %108 = zext i8 %107 to i32
  %109 = shl i32 %108, 16
  %110 = add i32 %104, %109
  %111 = load i8*, i8** %_hj_key, align 8
  %112 = getelementptr inbounds i8, i8* %111, i64 11
  %113 = load i8, i8* %112, align 1
  %114 = zext i8 %113 to i32
  %115 = shl i32 %114, 24
  %116 = add i32 %110, %115
  %117 = load i32, i32* %_ha_hashv, align 4
  %118 = add i32 %117, %116
  store i32 %118, i32* %_ha_hashv, align 4
  br label %119

; <label>:119                                     ; preds = %46
  %120 = load i32, i32* %_hj_j, align 4
  %121 = load i32, i32* %_hj_i, align 4
  %122 = sub i32 %121, %120
  store i32 %122, i32* %_hj_i, align 4
  %123 = load i32, i32* %_ha_hashv, align 4
  %124 = load i32, i32* %_hj_i, align 4
  %125 = sub i32 %124, %123
  store i32 %125, i32* %_hj_i, align 4
  %126 = load i32, i32* %_ha_hashv, align 4
  %127 = lshr i32 %126, 13
  %128 = load i32, i32* %_hj_i, align 4
  %129 = xor i32 %128, %127
  store i32 %129, i32* %_hj_i, align 4
  %130 = load i32, i32* %_ha_hashv, align 4
  %131 = load i32, i32* %_hj_j, align 4
  %132 = sub i32 %131, %130
  store i32 %132, i32* %_hj_j, align 4
  %133 = load i32, i32* %_hj_i, align 4
  %134 = load i32, i32* %_hj_j, align 4
  %135 = sub i32 %134, %133
  store i32 %135, i32* %_hj_j, align 4
  %136 = load i32, i32* %_hj_i, align 4
  %137 = shl i32 %136, 8
  %138 = load i32, i32* %_hj_j, align 4
  %139 = xor i32 %138, %137
  store i32 %139, i32* %_hj_j, align 4
  %140 = load i32, i32* %_hj_i, align 4
  %141 = load i32, i32* %_ha_hashv, align 4
  %142 = sub i32 %141, %140
  store i32 %142, i32* %_ha_hashv, align 4
  %143 = load i32, i32* %_hj_j, align 4
  %144 = load i32, i32* %_ha_hashv, align 4
  %145 = sub i32 %144, %143
  store i32 %145, i32* %_ha_hashv, align 4
  %146 = load i32, i32* %_hj_j, align 4
  %147 = lshr i32 %146, 13
  %148 = load i32, i32* %_ha_hashv, align 4
  %149 = xor i32 %148, %147
  store i32 %149, i32* %_ha_hashv, align 4
  %150 = load i32, i32* %_hj_j, align 4
  %151 = load i32, i32* %_hj_i, align 4
  %152 = sub i32 %151, %150
  store i32 %152, i32* %_hj_i, align 4
  %153 = load i32, i32* %_ha_hashv, align 4
  %154 = load i32, i32* %_hj_i, align 4
  %155 = sub i32 %154, %153
  store i32 %155, i32* %_hj_i, align 4
  %156 = load i32, i32* %_ha_hashv, align 4
  %157 = lshr i32 %156, 12
  %158 = load i32, i32* %_hj_i, align 4
  %159 = xor i32 %158, %157
  store i32 %159, i32* %_hj_i, align 4
  %160 = load i32, i32* %_ha_hashv, align 4
  %161 = load i32, i32* %_hj_j, align 4
  %162 = sub i32 %161, %160
  store i32 %162, i32* %_hj_j, align 4
  %163 = load i32, i32* %_hj_i, align 4
  %164 = load i32, i32* %_hj_j, align 4
  %165 = sub i32 %164, %163
  store i32 %165, i32* %_hj_j, align 4
  %166 = load i32, i32* %_hj_i, align 4
  %167 = shl i32 %166, 16
  %168 = load i32, i32* %_hj_j, align 4
  %169 = xor i32 %168, %167
  store i32 %169, i32* %_hj_j, align 4
  %170 = load i32, i32* %_hj_i, align 4
  %171 = load i32, i32* %_ha_hashv, align 4
  %172 = sub i32 %171, %170
  store i32 %172, i32* %_ha_hashv, align 4
  %173 = load i32, i32* %_hj_j, align 4
  %174 = load i32, i32* %_ha_hashv, align 4
  %175 = sub i32 %174, %173
  store i32 %175, i32* %_ha_hashv, align 4
  %176 = load i32, i32* %_hj_j, align 4
  %177 = lshr i32 %176, 5
  %178 = load i32, i32* %_ha_hashv, align 4
  %179 = xor i32 %178, %177
  store i32 %179, i32* %_ha_hashv, align 4
  %180 = load i32, i32* %_hj_j, align 4
  %181 = load i32, i32* %_hj_i, align 4
  %182 = sub i32 %181, %180
  store i32 %182, i32* %_hj_i, align 4
  %183 = load i32, i32* %_ha_hashv, align 4
  %184 = load i32, i32* %_hj_i, align 4
  %185 = sub i32 %184, %183
  store i32 %185, i32* %_hj_i, align 4
  %186 = load i32, i32* %_ha_hashv, align 4
  %187 = lshr i32 %186, 3
  %188 = load i32, i32* %_hj_i, align 4
  %189 = xor i32 %188, %187
  store i32 %189, i32* %_hj_i, align 4
  %190 = load i32, i32* %_ha_hashv, align 4
  %191 = load i32, i32* %_hj_j, align 4
  %192 = sub i32 %191, %190
  store i32 %192, i32* %_hj_j, align 4
  %193 = load i32, i32* %_hj_i, align 4
  %194 = load i32, i32* %_hj_j, align 4
  %195 = sub i32 %194, %193
  store i32 %195, i32* %_hj_j, align 4
  %196 = load i32, i32* %_hj_i, align 4
  %197 = shl i32 %196, 10
  %198 = load i32, i32* %_hj_j, align 4
  %199 = xor i32 %198, %197
  store i32 %199, i32* %_hj_j, align 4
  %200 = load i32, i32* %_hj_i, align 4
  %201 = load i32, i32* %_ha_hashv, align 4
  %202 = sub i32 %201, %200
  store i32 %202, i32* %_ha_hashv, align 4
  %203 = load i32, i32* %_hj_j, align 4
  %204 = load i32, i32* %_ha_hashv, align 4
  %205 = sub i32 %204, %203
  store i32 %205, i32* %_ha_hashv, align 4
  %206 = load i32, i32* %_hj_j, align 4
  %207 = lshr i32 %206, 15
  %208 = load i32, i32* %_ha_hashv, align 4
  %209 = xor i32 %208, %207
  store i32 %209, i32* %_ha_hashv, align 4
  br label %210

; <label>:210                                     ; preds = %119
  %211 = load i8*, i8** %_hj_key, align 8
  %212 = getelementptr inbounds i8, i8* %211, i64 12
  store i8* %212, i8** %_hj_key, align 8
  %213 = load i32, i32* %_hj_k, align 4
  %214 = sub i32 %213, 12
  store i32 %214, i32* %_hj_k, align 4
  br label %43

; <label>:215                                     ; preds = %43
  %216 = load i64, i64* %keylen, align 8
  %217 = and i64 %216, -8
  %218 = lshr i64 %217, 32
  %219 = trunc i64 %218 to i32
  %220 = load i32, i32* %_ha_hashv, align 4
  %221 = add i32 %220, %219
  store i32 %221, i32* %_ha_hashv, align 4
  %222 = load i32, i32* %_hj_k, align 4
  switch i32 %222, label %309 [
    i32 11, label %223
    i32 10, label %231
    i32 9, label %239
    i32 8, label %247
    i32 7, label %255
    i32 6, label %263
    i32 5, label %271
    i32 4, label %278
    i32 3, label %286
    i32 2, label %294
    i32 1, label %302
  ]

; <label>:223                                     ; preds = %215
  %224 = load i8*, i8** %_hj_key, align 8
  %225 = getelementptr inbounds i8, i8* %224, i64 10
  %226 = load i8, i8* %225, align 1
  %227 = zext i8 %226 to i32
  %228 = shl i32 %227, 24
  %229 = load i32, i32* %_ha_hashv, align 4
  %230 = add i32 %229, %228
  store i32 %230, i32* %_ha_hashv, align 4
  br label %231

; <label>:231                                     ; preds = %215, %223
  %232 = load i8*, i8** %_hj_key, align 8
  %233 = getelementptr inbounds i8, i8* %232, i64 9
  %234 = load i8, i8* %233, align 1
  %235 = zext i8 %234 to i32
  %236 = shl i32 %235, 16
  %237 = load i32, i32* %_ha_hashv, align 4
  %238 = add i32 %237, %236
  store i32 %238, i32* %_ha_hashv, align 4
  br label %239

; <label>:239                                     ; preds = %215, %231
  %240 = load i8*, i8** %_hj_key, align 8
  %241 = getelementptr inbounds i8, i8* %240, i64 8
  %242 = load i8, i8* %241, align 1
  %243 = zext i8 %242 to i32
  %244 = shl i32 %243, 8
  %245 = load i32, i32* %_ha_hashv, align 4
  %246 = add i32 %245, %244
  store i32 %246, i32* %_ha_hashv, align 4
  br label %247

; <label>:247                                     ; preds = %215, %239
  %248 = load i8*, i8** %_hj_key, align 8
  %249 = getelementptr inbounds i8, i8* %248, i64 7
  %250 = load i8, i8* %249, align 1
  %251 = zext i8 %250 to i32
  %252 = shl i32 %251, 24
  %253 = load i32, i32* %_hj_j, align 4
  %254 = add i32 %253, %252
  store i32 %254, i32* %_hj_j, align 4
  br label %255

; <label>:255                                     ; preds = %215, %247
  %256 = load i8*, i8** %_hj_key, align 8
  %257 = getelementptr inbounds i8, i8* %256, i64 6
  %258 = load i8, i8* %257, align 1
  %259 = zext i8 %258 to i32
  %260 = shl i32 %259, 16
  %261 = load i32, i32* %_hj_j, align 4
  %262 = add i32 %261, %260
  store i32 %262, i32* %_hj_j, align 4
  br label %263

; <label>:263                                     ; preds = %215, %255
  %264 = load i8*, i8** %_hj_key, align 8
  %265 = getelementptr inbounds i8, i8* %264, i64 5
  %266 = load i8, i8* %265, align 1
  %267 = zext i8 %266 to i32
  %268 = shl i32 %267, 8
  %269 = load i32, i32* %_hj_j, align 4
  %270 = add i32 %269, %268
  store i32 %270, i32* %_hj_j, align 4
  br label %271

; <label>:271                                     ; preds = %215, %263
  %272 = load i8*, i8** %_hj_key, align 8
  %273 = getelementptr inbounds i8, i8* %272, i64 4
  %274 = load i8, i8* %273, align 1
  %275 = zext i8 %274 to i32
  %276 = load i32, i32* %_hj_j, align 4
  %277 = add i32 %276, %275
  store i32 %277, i32* %_hj_j, align 4
  br label %278

; <label>:278                                     ; preds = %215, %271
  %279 = load i8*, i8** %_hj_key, align 8
  %280 = getelementptr inbounds i8, i8* %279, i64 3
  %281 = load i8, i8* %280, align 1
  %282 = zext i8 %281 to i32
  %283 = shl i32 %282, 24
  %284 = load i32, i32* %_hj_i, align 4
  %285 = add i32 %284, %283
  store i32 %285, i32* %_hj_i, align 4
  br label %286

; <label>:286                                     ; preds = %215, %278
  %287 = load i8*, i8** %_hj_key, align 8
  %288 = getelementptr inbounds i8, i8* %287, i64 2
  %289 = load i8, i8* %288, align 1
  %290 = zext i8 %289 to i32
  %291 = shl i32 %290, 16
  %292 = load i32, i32* %_hj_i, align 4
  %293 = add i32 %292, %291
  store i32 %293, i32* %_hj_i, align 4
  br label %294

; <label>:294                                     ; preds = %215, %286
  %295 = load i8*, i8** %_hj_key, align 8
  %296 = getelementptr inbounds i8, i8* %295, i64 1
  %297 = load i8, i8* %296, align 1
  %298 = zext i8 %297 to i32
  %299 = shl i32 %298, 8
  %300 = load i32, i32* %_hj_i, align 4
  %301 = add i32 %300, %299
  store i32 %301, i32* %_hj_i, align 4
  br label %302

; <label>:302                                     ; preds = %215, %294
  %303 = load i8*, i8** %_hj_key, align 8
  %304 = getelementptr inbounds i8, i8* %303, i64 0
  %305 = load i8, i8* %304, align 1
  %306 = zext i8 %305 to i32
  %307 = load i32, i32* %_hj_i, align 4
  %308 = add i32 %307, %306
  store i32 %308, i32* %_hj_i, align 4
  br label %309

; <label>:309                                     ; preds = %302, %215
  br label %310

; <label>:310                                     ; preds = %309
  %311 = load i32, i32* %_hj_j, align 4
  %312 = load i32, i32* %_hj_i, align 4
  %313 = sub i32 %312, %311
  store i32 %313, i32* %_hj_i, align 4
  %314 = load i32, i32* %_ha_hashv, align 4
  %315 = load i32, i32* %_hj_i, align 4
  %316 = sub i32 %315, %314
  store i32 %316, i32* %_hj_i, align 4
  %317 = load i32, i32* %_ha_hashv, align 4
  %318 = lshr i32 %317, 13
  %319 = load i32, i32* %_hj_i, align 4
  %320 = xor i32 %319, %318
  store i32 %320, i32* %_hj_i, align 4
  %321 = load i32, i32* %_ha_hashv, align 4
  %322 = load i32, i32* %_hj_j, align 4
  %323 = sub i32 %322, %321
  store i32 %323, i32* %_hj_j, align 4
  %324 = load i32, i32* %_hj_i, align 4
  %325 = load i32, i32* %_hj_j, align 4
  %326 = sub i32 %325, %324
  store i32 %326, i32* %_hj_j, align 4
  %327 = load i32, i32* %_hj_i, align 4
  %328 = shl i32 %327, 8
  %329 = load i32, i32* %_hj_j, align 4
  %330 = xor i32 %329, %328
  store i32 %330, i32* %_hj_j, align 4
  %331 = load i32, i32* %_hj_i, align 4
  %332 = load i32, i32* %_ha_hashv, align 4
  %333 = sub i32 %332, %331
  store i32 %333, i32* %_ha_hashv, align 4
  %334 = load i32, i32* %_hj_j, align 4
  %335 = load i32, i32* %_ha_hashv, align 4
  %336 = sub i32 %335, %334
  store i32 %336, i32* %_ha_hashv, align 4
  %337 = load i32, i32* %_hj_j, align 4
  %338 = lshr i32 %337, 13
  %339 = load i32, i32* %_ha_hashv, align 4
  %340 = xor i32 %339, %338
  store i32 %340, i32* %_ha_hashv, align 4
  %341 = load i32, i32* %_hj_j, align 4
  %342 = load i32, i32* %_hj_i, align 4
  %343 = sub i32 %342, %341
  store i32 %343, i32* %_hj_i, align 4
  %344 = load i32, i32* %_ha_hashv, align 4
  %345 = load i32, i32* %_hj_i, align 4
  %346 = sub i32 %345, %344
  store i32 %346, i32* %_hj_i, align 4
  %347 = load i32, i32* %_ha_hashv, align 4
  %348 = lshr i32 %347, 12
  %349 = load i32, i32* %_hj_i, align 4
  %350 = xor i32 %349, %348
  store i32 %350, i32* %_hj_i, align 4
  %351 = load i32, i32* %_ha_hashv, align 4
  %352 = load i32, i32* %_hj_j, align 4
  %353 = sub i32 %352, %351
  store i32 %353, i32* %_hj_j, align 4
  %354 = load i32, i32* %_hj_i, align 4
  %355 = load i32, i32* %_hj_j, align 4
  %356 = sub i32 %355, %354
  store i32 %356, i32* %_hj_j, align 4
  %357 = load i32, i32* %_hj_i, align 4
  %358 = shl i32 %357, 16
  %359 = load i32, i32* %_hj_j, align 4
  %360 = xor i32 %359, %358
  store i32 %360, i32* %_hj_j, align 4
  %361 = load i32, i32* %_hj_i, align 4
  %362 = load i32, i32* %_ha_hashv, align 4
  %363 = sub i32 %362, %361
  store i32 %363, i32* %_ha_hashv, align 4
  %364 = load i32, i32* %_hj_j, align 4
  %365 = load i32, i32* %_ha_hashv, align 4
  %366 = sub i32 %365, %364
  store i32 %366, i32* %_ha_hashv, align 4
  %367 = load i32, i32* %_hj_j, align 4
  %368 = lshr i32 %367, 5
  %369 = load i32, i32* %_ha_hashv, align 4
  %370 = xor i32 %369, %368
  store i32 %370, i32* %_ha_hashv, align 4
  %371 = load i32, i32* %_hj_j, align 4
  %372 = load i32, i32* %_hj_i, align 4
  %373 = sub i32 %372, %371
  store i32 %373, i32* %_hj_i, align 4
  %374 = load i32, i32* %_ha_hashv, align 4
  %375 = load i32, i32* %_hj_i, align 4
  %376 = sub i32 %375, %374
  store i32 %376, i32* %_hj_i, align 4
  %377 = load i32, i32* %_ha_hashv, align 4
  %378 = lshr i32 %377, 3
  %379 = load i32, i32* %_hj_i, align 4
  %380 = xor i32 %379, %378
  store i32 %380, i32* %_hj_i, align 4
  %381 = load i32, i32* %_ha_hashv, align 4
  %382 = load i32, i32* %_hj_j, align 4
  %383 = sub i32 %382, %381
  store i32 %383, i32* %_hj_j, align 4
  %384 = load i32, i32* %_hj_i, align 4
  %385 = load i32, i32* %_hj_j, align 4
  %386 = sub i32 %385, %384
  store i32 %386, i32* %_hj_j, align 4
  %387 = load i32, i32* %_hj_i, align 4
  %388 = shl i32 %387, 10
  %389 = load i32, i32* %_hj_j, align 4
  %390 = xor i32 %389, %388
  store i32 %390, i32* %_hj_j, align 4
  %391 = load i32, i32* %_hj_i, align 4
  %392 = load i32, i32* %_ha_hashv, align 4
  %393 = sub i32 %392, %391
  store i32 %393, i32* %_ha_hashv, align 4
  %394 = load i32, i32* %_hj_j, align 4
  %395 = load i32, i32* %_ha_hashv, align 4
  %396 = sub i32 %395, %394
  store i32 %396, i32* %_ha_hashv, align 4
  %397 = load i32, i32* %_hj_j, align 4
  %398 = lshr i32 %397, 15
  %399 = load i32, i32* %_ha_hashv, align 4
  %400 = xor i32 %399, %398
  store i32 %400, i32* %_ha_hashv, align 4
  br label %401

; <label>:401                                     ; preds = %310
  br label %402

; <label>:402                                     ; preds = %401
  br label %403

; <label>:403                                     ; preds = %402
  br label %404

; <label>:404                                     ; preds = %403
  %405 = load i32, i32* %_ha_hashv, align 4
  %406 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %407 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %406, i32 0, i32 2
  %408 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %407, i32 0, i32 7
  store i32 %405, i32* %408, align 4
  %409 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %410 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %409, i32 0, i32 0
  %411 = bitcast i64* %410 to i8*
  %412 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %413 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %412, i32 0, i32 2
  %414 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %413, i32 0, i32 5
  store i8* %411, i8** %414, align 8
  %415 = load i64, i64* %keylen, align 8
  %416 = and i64 %415, -8
  %417 = lshr i64 %416, 32
  %418 = trunc i64 %417 to i32
  %419 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %420 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %419, i32 0, i32 2
  %421 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %420, i32 0, i32 6
  store i32 %418, i32* %421, align 8
  %422 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %423 = icmp ne %struct.hash_tbl* %422, null
  br i1 %423, label %511, label %424

; <label>:424                                     ; preds = %404
  %425 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %426 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %425, i32 0, i32 2
  %427 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %426, i32 0, i32 2
  store i8* null, i8** %427, align 8
  %428 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %429 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %428, i32 0, i32 2
  %430 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %429, i32 0, i32 1
  store i8* null, i8** %430, align 8
  br label %431

; <label>:431                                     ; preds = %424
  %432 = call noalias i8* @malloc(i64 64) #2
  %433 = bitcast i8* %432 to %struct.UT_hash_table*
  %434 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %435 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %434, i32 0, i32 2
  %436 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %435, i32 0, i32 0
  store %struct.UT_hash_table* %433, %struct.UT_hash_table** %436, align 8
  %437 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %438 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %437, i32 0, i32 2
  %439 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %438, i32 0, i32 0
  %440 = load %struct.UT_hash_table*, %struct.UT_hash_table** %439, align 8
  %441 = icmp ne %struct.UT_hash_table* %440, null
  br i1 %441, label %443, label %442

; <label>:442                                     ; preds = %431
  call void @exit(i32 -1) #8
  unreachable

; <label>:443                                     ; preds = %431
  %444 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %445 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %444, i32 0, i32 2
  %446 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %445, i32 0, i32 0
  %447 = load %struct.UT_hash_table*, %struct.UT_hash_table** %446, align 8
  %448 = bitcast %struct.UT_hash_table* %447 to i8*
  call void @llvm.memset.p0i8.i64(i8* %448, i8 0, i64 64, i32 8, i1 false)
  %449 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %450 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %449, i32 0, i32 2
  %451 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %452 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %451, i32 0, i32 2
  %453 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %452, i32 0, i32 0
  %454 = load %struct.UT_hash_table*, %struct.UT_hash_table** %453, align 8
  %455 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %454, i32 0, i32 4
  store %struct.UT_hash_handle* %450, %struct.UT_hash_handle** %455, align 8
  %456 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %457 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %456, i32 0, i32 2
  %458 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %457, i32 0, i32 0
  %459 = load %struct.UT_hash_table*, %struct.UT_hash_table** %458, align 8
  %460 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %459, i32 0, i32 1
  store i32 32, i32* %460, align 8
  %461 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %462 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %461, i32 0, i32 2
  %463 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %462, i32 0, i32 0
  %464 = load %struct.UT_hash_table*, %struct.UT_hash_table** %463, align 8
  %465 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %464, i32 0, i32 2
  store i32 5, i32* %465, align 4
  %466 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %467 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %466, i32 0, i32 2
  %468 = bitcast %struct.UT_hash_handle* %467 to i8*
  %469 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %470 = bitcast %struct.hash_tbl* %469 to i8*
  %471 = ptrtoint i8* %468 to i64
  %472 = ptrtoint i8* %470 to i64
  %473 = sub i64 %471, %472
  %474 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %475 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %474, i32 0, i32 2
  %476 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %475, i32 0, i32 0
  %477 = load %struct.UT_hash_table*, %struct.UT_hash_table** %476, align 8
  %478 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %477, i32 0, i32 5
  store i64 %473, i64* %478, align 8
  %479 = call noalias i8* @malloc(i64 512) #2
  %480 = bitcast i8* %479 to %struct.UT_hash_bucket*
  %481 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %482 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %481, i32 0, i32 2
  %483 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %482, i32 0, i32 0
  %484 = load %struct.UT_hash_table*, %struct.UT_hash_table** %483, align 8
  %485 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %484, i32 0, i32 0
  store %struct.UT_hash_bucket* %480, %struct.UT_hash_bucket** %485, align 8
  %486 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %487 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %486, i32 0, i32 2
  %488 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %487, i32 0, i32 0
  %489 = load %struct.UT_hash_table*, %struct.UT_hash_table** %488, align 8
  %490 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %489, i32 0, i32 10
  store i32 -1609490463, i32* %490, align 8
  %491 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %492 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %491, i32 0, i32 2
  %493 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %492, i32 0, i32 0
  %494 = load %struct.UT_hash_table*, %struct.UT_hash_table** %493, align 8
  %495 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %494, i32 0, i32 0
  %496 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %495, align 8
  %497 = icmp ne %struct.UT_hash_bucket* %496, null
  br i1 %497, label %499, label %498

; <label>:498                                     ; preds = %443
  call void @exit(i32 -1) #8
  unreachable

; <label>:499                                     ; preds = %443
  %500 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %501 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %500, i32 0, i32 2
  %502 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %501, i32 0, i32 0
  %503 = load %struct.UT_hash_table*, %struct.UT_hash_table** %502, align 8
  %504 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %503, i32 0, i32 0
  %505 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %504, align 8
  %506 = bitcast %struct.UT_hash_bucket* %505 to i8*
  call void @llvm.memset.p0i8.i64(i8* %506, i8 0, i64 512, i32 8, i1 false)
  br label %507

; <label>:507                                     ; preds = %499
  br label %508

; <label>:508                                     ; preds = %507
  br label %509

; <label>:509                                     ; preds = %508
  %510 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  store %struct.hash_tbl* %510, %struct.hash_tbl** %new_table, align 8
  br label %558

; <label>:511                                     ; preds = %404
  %512 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %513 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %512, i32 0, i32 2
  %514 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %513, i32 0, i32 0
  %515 = load %struct.UT_hash_table*, %struct.UT_hash_table** %514, align 8
  %516 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %517 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %516, i32 0, i32 2
  %518 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %517, i32 0, i32 0
  store %struct.UT_hash_table* %515, %struct.UT_hash_table** %518, align 8
  br label %519

; <label>:519                                     ; preds = %511
  %520 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %521 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %520, i32 0, i32 2
  %522 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %521, i32 0, i32 2
  store i8* null, i8** %522, align 8
  %523 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %524 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %523, i32 0, i32 2
  %525 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %524, i32 0, i32 0
  %526 = load %struct.UT_hash_table*, %struct.UT_hash_table** %525, align 8
  %527 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %526, i32 0, i32 4
  %528 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %527, align 8
  %529 = bitcast %struct.UT_hash_handle* %528 to i8*
  %530 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %531 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %530, i32 0, i32 2
  %532 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %531, i32 0, i32 0
  %533 = load %struct.UT_hash_table*, %struct.UT_hash_table** %532, align 8
  %534 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %533, i32 0, i32 5
  %535 = load i64, i64* %534, align 8
  %536 = sub i64 0, %535
  %537 = getelementptr inbounds i8, i8* %529, i64 %536
  %538 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %539 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %538, i32 0, i32 2
  %540 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %539, i32 0, i32 1
  store i8* %537, i8** %540, align 8
  %541 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %542 = bitcast %struct.hash_tbl* %541 to i8*
  %543 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %544 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %543, i32 0, i32 2
  %545 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %544, i32 0, i32 0
  %546 = load %struct.UT_hash_table*, %struct.UT_hash_table** %545, align 8
  %547 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %546, i32 0, i32 4
  %548 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %547, align 8
  %549 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %548, i32 0, i32 2
  store i8* %542, i8** %549, align 8
  %550 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %551 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %550, i32 0, i32 2
  %552 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %553 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %552, i32 0, i32 2
  %554 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %553, i32 0, i32 0
  %555 = load %struct.UT_hash_table*, %struct.UT_hash_table** %554, align 8
  %556 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %555, i32 0, i32 4
  store %struct.UT_hash_handle* %551, %struct.UT_hash_handle** %556, align 8
  br label %557

; <label>:557                                     ; preds = %519
  br label %558

; <label>:558                                     ; preds = %557, %509
  br label %559

; <label>:559                                     ; preds = %558
  %560 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %561 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %560, i32 0, i32 2
  %562 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %561, i32 0, i32 0
  %563 = load %struct.UT_hash_table*, %struct.UT_hash_table** %562, align 8
  %564 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %563, i32 0, i32 3
  %565 = load i32, i32* %564, align 8
  %566 = add i32 %565, 1
  store i32 %566, i32* %564, align 8
  br label %567

; <label>:567                                     ; preds = %559
  %568 = load i32, i32* %_ha_hashv, align 4
  %569 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %570 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %569, i32 0, i32 2
  %571 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %570, i32 0, i32 0
  %572 = load %struct.UT_hash_table*, %struct.UT_hash_table** %571, align 8
  %573 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %572, i32 0, i32 1
  %574 = load i32, i32* %573, align 8
  %575 = sub i32 %574, 1
  %576 = and i32 %568, %575
  store i32 %576, i32* %_ha_bkt, align 4
  br label %577

; <label>:577                                     ; preds = %567
  br label %578

; <label>:578                                     ; preds = %577
  %579 = load i32, i32* %_ha_bkt, align 4
  %580 = zext i32 %579 to i64
  %581 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %582 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %581, i32 0, i32 2
  %583 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %582, i32 0, i32 0
  %584 = load %struct.UT_hash_table*, %struct.UT_hash_table** %583, align 8
  %585 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %584, i32 0, i32 0
  %586 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %585, align 8
  %587 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %586, i64 %580
  store %struct.UT_hash_bucket* %587, %struct.UT_hash_bucket** %_ha_head, align 8
  %588 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %589 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %588, i32 0, i32 1
  %590 = load i32, i32* %589, align 8
  %591 = add i32 %590, 1
  store i32 %591, i32* %589, align 8
  %592 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %593 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %592, i32 0, i32 0
  %594 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %593, align 8
  %595 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %596 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %595, i32 0, i32 2
  %597 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %596, i32 0, i32 4
  store %struct.UT_hash_handle* %594, %struct.UT_hash_handle** %597, align 8
  %598 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %599 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %598, i32 0, i32 2
  %600 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %599, i32 0, i32 3
  store %struct.UT_hash_handle* null, %struct.UT_hash_handle** %600, align 8
  %601 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %602 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %601, i32 0, i32 0
  %603 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %602, align 8
  %604 = icmp ne %struct.UT_hash_handle* %603, null
  br i1 %604, label %605, label %612

; <label>:605                                     ; preds = %578
  %606 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %607 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %606, i32 0, i32 2
  %608 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %609 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %608, i32 0, i32 0
  %610 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %609, align 8
  %611 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %610, i32 0, i32 3
  store %struct.UT_hash_handle* %607, %struct.UT_hash_handle** %611, align 8
  br label %612

; <label>:612                                     ; preds = %605, %578
  %613 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %614 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %613, i32 0, i32 2
  %615 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %616 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %615, i32 0, i32 0
  store %struct.UT_hash_handle* %614, %struct.UT_hash_handle** %616, align 8
  %617 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %618 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %617, i32 0, i32 1
  %619 = load i32, i32* %618, align 8
  %620 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_ha_head, align 8
  %621 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %620, i32 0, i32 2
  %622 = load i32, i32* %621, align 4
  %623 = add i32 %622, 1
  %624 = mul i32 %623, 10
  %625 = icmp uge i32 %619, %624
  br i1 %625, label %626, label %882

; <label>:626                                     ; preds = %612
  %627 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %628 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %627, i32 0, i32 2
  %629 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %628, i32 0, i32 0
  %630 = load %struct.UT_hash_table*, %struct.UT_hash_table** %629, align 8
  %631 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %630, i32 0, i32 9
  %632 = load i32, i32* %631, align 4
  %633 = icmp ne i32 %632, 0
  br i1 %633, label %882, label %634

; <label>:634                                     ; preds = %626
  br label %635

; <label>:635                                     ; preds = %634
  %636 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %637 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %636, i32 0, i32 2
  %638 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %637, i32 0, i32 0
  %639 = load %struct.UT_hash_table*, %struct.UT_hash_table** %638, align 8
  %640 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %639, i32 0, i32 1
  %641 = load i32, i32* %640, align 8
  %642 = zext i32 %641 to i64
  %643 = mul i64 2, %642
  %644 = mul i64 %643, 16
  %645 = call noalias i8* @malloc(i64 %644) #2
  %646 = bitcast i8* %645 to %struct.UT_hash_bucket*
  store %struct.UT_hash_bucket* %646, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %647 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %648 = icmp ne %struct.UT_hash_bucket* %647, null
  br i1 %648, label %650, label %649

; <label>:649                                     ; preds = %635
  call void @exit(i32 -1) #8
  unreachable

; <label>:650                                     ; preds = %635
  %651 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %652 = bitcast %struct.UT_hash_bucket* %651 to i8*
  %653 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %654 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %653, i32 0, i32 2
  %655 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %654, i32 0, i32 0
  %656 = load %struct.UT_hash_table*, %struct.UT_hash_table** %655, align 8
  %657 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %656, i32 0, i32 1
  %658 = load i32, i32* %657, align 8
  %659 = zext i32 %658 to i64
  %660 = mul i64 2, %659
  %661 = mul i64 %660, 16
  call void @llvm.memset.p0i8.i64(i8* %652, i8 0, i64 %661, i32 8, i1 false)
  %662 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %663 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %662, i32 0, i32 2
  %664 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %663, i32 0, i32 0
  %665 = load %struct.UT_hash_table*, %struct.UT_hash_table** %664, align 8
  %666 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %665, i32 0, i32 3
  %667 = load i32, i32* %666, align 8
  %668 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %669 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %668, i32 0, i32 2
  %670 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %669, i32 0, i32 0
  %671 = load %struct.UT_hash_table*, %struct.UT_hash_table** %670, align 8
  %672 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %671, i32 0, i32 2
  %673 = load i32, i32* %672, align 4
  %674 = add i32 %673, 1
  %675 = lshr i32 %667, %674
  %676 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %677 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %676, i32 0, i32 2
  %678 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %677, i32 0, i32 0
  %679 = load %struct.UT_hash_table*, %struct.UT_hash_table** %678, align 8
  %680 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %679, i32 0, i32 3
  %681 = load i32, i32* %680, align 8
  %682 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %683 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %682, i32 0, i32 2
  %684 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %683, i32 0, i32 0
  %685 = load %struct.UT_hash_table*, %struct.UT_hash_table** %684, align 8
  %686 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %685, i32 0, i32 1
  %687 = load i32, i32* %686, align 8
  %688 = mul i32 %687, 2
  %689 = sub i32 %688, 1
  %690 = and i32 %681, %689
  %691 = icmp ne i32 %690, 0
  %692 = select i1 %691, i32 1, i32 0
  %693 = add i32 %675, %692
  %694 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %695 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %694, i32 0, i32 2
  %696 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %695, i32 0, i32 0
  %697 = load %struct.UT_hash_table*, %struct.UT_hash_table** %696, align 8
  %698 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %697, i32 0, i32 6
  store i32 %693, i32* %698, align 8
  %699 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %700 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %699, i32 0, i32 2
  %701 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %700, i32 0, i32 0
  %702 = load %struct.UT_hash_table*, %struct.UT_hash_table** %701, align 8
  %703 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %702, i32 0, i32 7
  store i32 0, i32* %703, align 4
  store i32 0, i32* %_he_bkt_i, align 4
  br label %704

; <label>:704                                     ; preds = %805, %650
  %705 = load i32, i32* %_he_bkt_i, align 4
  %706 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %707 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %706, i32 0, i32 2
  %708 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %707, i32 0, i32 0
  %709 = load %struct.UT_hash_table*, %struct.UT_hash_table** %708, align 8
  %710 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %709, i32 0, i32 1
  %711 = load i32, i32* %710, align 8
  %712 = icmp ult i32 %705, %711
  br i1 %712, label %713, label %808

; <label>:713                                     ; preds = %704
  %714 = load i32, i32* %_he_bkt_i, align 4
  %715 = zext i32 %714 to i64
  %716 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %717 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %716, i32 0, i32 2
  %718 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %717, i32 0, i32 0
  %719 = load %struct.UT_hash_table*, %struct.UT_hash_table** %718, align 8
  %720 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %719, i32 0, i32 0
  %721 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %720, align 8
  %722 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %721, i64 %715
  %723 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %722, i32 0, i32 0
  %724 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %723, align 8
  store %struct.UT_hash_handle* %724, %struct.UT_hash_handle** %_he_thh, align 8
  br label %725

; <label>:725                                     ; preds = %799, %713
  %726 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %727 = icmp ne %struct.UT_hash_handle* %726, null
  br i1 %727, label %728, label %804

; <label>:728                                     ; preds = %725
  %729 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %730 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %729, i32 0, i32 4
  %731 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %730, align 8
  store %struct.UT_hash_handle* %731, %struct.UT_hash_handle** %_he_hh_nxt, align 8
  br label %732

; <label>:732                                     ; preds = %728
  %733 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %734 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %733, i32 0, i32 7
  %735 = load i32, i32* %734, align 4
  %736 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %737 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %736, i32 0, i32 2
  %738 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %737, i32 0, i32 0
  %739 = load %struct.UT_hash_table*, %struct.UT_hash_table** %738, align 8
  %740 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %739, i32 0, i32 1
  %741 = load i32, i32* %740, align 8
  %742 = mul i32 %741, 2
  %743 = sub i32 %742, 1
  %744 = and i32 %735, %743
  store i32 %744, i32* %_he_bkt, align 4
  br label %745

; <label>:745                                     ; preds = %732
  %746 = load i32, i32* %_he_bkt, align 4
  %747 = zext i32 %746 to i64
  %748 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %749 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %748, i64 %747
  store %struct.UT_hash_bucket* %749, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %750 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %751 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %750, i32 0, i32 1
  %752 = load i32, i32* %751, align 8
  %753 = add i32 %752, 1
  store i32 %753, i32* %751, align 8
  %754 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %755 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %754, i32 0, i32 2
  %756 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %755, i32 0, i32 0
  %757 = load %struct.UT_hash_table*, %struct.UT_hash_table** %756, align 8
  %758 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %757, i32 0, i32 6
  %759 = load i32, i32* %758, align 8
  %760 = icmp ugt i32 %753, %759
  br i1 %760, label %761, label %781

; <label>:761                                     ; preds = %745
  %762 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %763 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %762, i32 0, i32 2
  %764 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %763, i32 0, i32 0
  %765 = load %struct.UT_hash_table*, %struct.UT_hash_table** %764, align 8
  %766 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %765, i32 0, i32 7
  %767 = load i32, i32* %766, align 4
  %768 = add i32 %767, 1
  store i32 %768, i32* %766, align 4
  %769 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %770 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %769, i32 0, i32 1
  %771 = load i32, i32* %770, align 8
  %772 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %773 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %772, i32 0, i32 2
  %774 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %773, i32 0, i32 0
  %775 = load %struct.UT_hash_table*, %struct.UT_hash_table** %774, align 8
  %776 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %775, i32 0, i32 6
  %777 = load i32, i32* %776, align 8
  %778 = udiv i32 %771, %777
  %779 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %780 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %779, i32 0, i32 2
  store i32 %778, i32* %780, align 4
  br label %781

; <label>:781                                     ; preds = %761, %745
  %782 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %783 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %782, i32 0, i32 3
  store %struct.UT_hash_handle* null, %struct.UT_hash_handle** %783, align 8
  %784 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %785 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %784, i32 0, i32 0
  %786 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %785, align 8
  %787 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %788 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %787, i32 0, i32 4
  store %struct.UT_hash_handle* %786, %struct.UT_hash_handle** %788, align 8
  %789 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %790 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %789, i32 0, i32 0
  %791 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %790, align 8
  %792 = icmp ne %struct.UT_hash_handle* %791, null
  br i1 %792, label %793, label %799

; <label>:793                                     ; preds = %781
  %794 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %795 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %796 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %795, i32 0, i32 0
  %797 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %796, align 8
  %798 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %797, i32 0, i32 3
  store %struct.UT_hash_handle* %794, %struct.UT_hash_handle** %798, align 8
  br label %799

; <label>:799                                     ; preds = %793, %781
  %800 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_thh, align 8
  %801 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_newbkt, align 8
  %802 = getelementptr inbounds %struct.UT_hash_bucket, %struct.UT_hash_bucket* %801, i32 0, i32 0
  store %struct.UT_hash_handle* %800, %struct.UT_hash_handle** %802, align 8
  %803 = load %struct.UT_hash_handle*, %struct.UT_hash_handle** %_he_hh_nxt, align 8
  store %struct.UT_hash_handle* %803, %struct.UT_hash_handle** %_he_thh, align 8
  br label %725

; <label>:804                                     ; preds = %725
  br label %805

; <label>:805                                     ; preds = %804
  %806 = load i32, i32* %_he_bkt_i, align 4
  %807 = add i32 %806, 1
  store i32 %807, i32* %_he_bkt_i, align 4
  br label %704

; <label>:808                                     ; preds = %704
  %809 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %810 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %809, i32 0, i32 2
  %811 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %810, i32 0, i32 0
  %812 = load %struct.UT_hash_table*, %struct.UT_hash_table** %811, align 8
  %813 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %812, i32 0, i32 0
  %814 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %813, align 8
  %815 = bitcast %struct.UT_hash_bucket* %814 to i8*
  call void @free(i8* %815) #2
  %816 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %817 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %816, i32 0, i32 2
  %818 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %817, i32 0, i32 0
  %819 = load %struct.UT_hash_table*, %struct.UT_hash_table** %818, align 8
  %820 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %819, i32 0, i32 1
  %821 = load i32, i32* %820, align 8
  %822 = mul i32 %821, 2
  store i32 %822, i32* %820, align 8
  %823 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %824 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %823, i32 0, i32 2
  %825 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %824, i32 0, i32 0
  %826 = load %struct.UT_hash_table*, %struct.UT_hash_table** %825, align 8
  %827 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %826, i32 0, i32 2
  %828 = load i32, i32* %827, align 4
  %829 = add i32 %828, 1
  store i32 %829, i32* %827, align 4
  %830 = load %struct.UT_hash_bucket*, %struct.UT_hash_bucket** %_he_new_buckets, align 8
  %831 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %832 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %831, i32 0, i32 2
  %833 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %832, i32 0, i32 0
  %834 = load %struct.UT_hash_table*, %struct.UT_hash_table** %833, align 8
  %835 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %834, i32 0, i32 0
  store %struct.UT_hash_bucket* %830, %struct.UT_hash_bucket** %835, align 8
  %836 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %837 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %836, i32 0, i32 2
  %838 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %837, i32 0, i32 0
  %839 = load %struct.UT_hash_table*, %struct.UT_hash_table** %838, align 8
  %840 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %839, i32 0, i32 7
  %841 = load i32, i32* %840, align 4
  %842 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %843 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %842, i32 0, i32 2
  %844 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %843, i32 0, i32 0
  %845 = load %struct.UT_hash_table*, %struct.UT_hash_table** %844, align 8
  %846 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %845, i32 0, i32 3
  %847 = load i32, i32* %846, align 8
  %848 = lshr i32 %847, 1
  %849 = icmp ugt i32 %841, %848
  br i1 %849, label %850, label %858

; <label>:850                                     ; preds = %808
  %851 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %852 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %851, i32 0, i32 2
  %853 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %852, i32 0, i32 0
  %854 = load %struct.UT_hash_table*, %struct.UT_hash_table** %853, align 8
  %855 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %854, i32 0, i32 8
  %856 = load i32, i32* %855, align 8
  %857 = add i32 %856, 1
  br label %859

; <label>:858                                     ; preds = %808
  br label %859

; <label>:859                                     ; preds = %858, %850
  %860 = phi i32 [ %857, %850 ], [ 0, %858 ]
  %861 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %862 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %861, i32 0, i32 2
  %863 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %862, i32 0, i32 0
  %864 = load %struct.UT_hash_table*, %struct.UT_hash_table** %863, align 8
  %865 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %864, i32 0, i32 8
  store i32 %860, i32* %865, align 8
  %866 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %867 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %866, i32 0, i32 2
  %868 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %867, i32 0, i32 0
  %869 = load %struct.UT_hash_table*, %struct.UT_hash_table** %868, align 8
  %870 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %869, i32 0, i32 8
  %871 = load i32, i32* %870, align 8
  %872 = icmp ugt i32 %871, 1
  br i1 %872, label %873, label %879

; <label>:873                                     ; preds = %859
  %874 = load %struct.hash_tbl*, %struct.hash_tbl** %new_pair, align 8
  %875 = getelementptr inbounds %struct.hash_tbl, %struct.hash_tbl* %874, i32 0, i32 2
  %876 = getelementptr inbounds %struct.UT_hash_handle, %struct.UT_hash_handle* %875, i32 0, i32 0
  %877 = load %struct.UT_hash_table*, %struct.UT_hash_table** %876, align 8
  %878 = getelementptr inbounds %struct.UT_hash_table, %struct.UT_hash_table* %877, i32 0, i32 9
  store i32 1, i32* %878, align 4
  br label %879

; <label>:879                                     ; preds = %873, %859
  br label %880

; <label>:880                                     ; preds = %879
  br label %881

; <label>:881                                     ; preds = %880
  br label %882

; <label>:882                                     ; preds = %881, %626, %612
  br label %883

; <label>:883                                     ; preds = %882
  br label %884

; <label>:884                                     ; preds = %883
  br label %885

; <label>:885                                     ; preds = %884
  br label %886

; <label>:886                                     ; preds = %885
  br label %10

; <label>:887                                     ; preds = %10
  %888 = load %struct.hash_tbl*, %struct.hash_tbl** %new_table, align 8
  %889 = ptrtoint %struct.hash_tbl* %888 to i64
  %890 = or i64 %889, 6
  %891 = load i64*, i64** %vec, align 8
  %892 = getelementptr inbounds i64, i64* %891, i64 1
  store i64 %890, i64* %892, align 8
  %893 = load i64*, i64** %vec, align 8
  %894 = ptrtoint i64* %893 to i64
  %895 = or i64 %894, 6
  ret i64 %895
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture, i8, i64, i32, i1) #7

; Function Attrs: nounwind
declare void @free(i8*) #5

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
define internal void @_GLOBAL__sub_I_header.cpp() #0 section ".text.startup" {
  call void @__cxx_global_var_init()
  ret void
}

attributes #0 = { uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }
attributes #3 = { noreturn nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nobuiltin nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { argmemonly nounwind }
attributes #8 = { noreturn nounwind }
attributes #9 = { builtin nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0-2ubuntu4 (tags/RELEASE_380/final)"}


;;;;;;

define void @proc_main() {
  %cloptr13301 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13302 = getelementptr inbounds i64, i64* %cloptr13301, i64 0                  ; &cloptr13301[0]
  %f13303 = ptrtoint void(i64,i64)* @lam13299 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13303, i64* %eptr13302                                                 ; store fptr
  %arg9678 = ptrtoint i64* %cloptr13301 to i64                                       ; closure cast; i64* -> i64
  %cloptr13304 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13305 = getelementptr inbounds i64, i64* %cloptr13304, i64 0                  ; &cloptr13304[0]
  %f13306 = ptrtoint void(i64,i64)* @lam13296 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13306, i64* %eptr13305                                                 ; store fptr
  %arg9677 = ptrtoint i64* %cloptr13304 to i64                                       ; closure cast; i64* -> i64
  %cloptr13307 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13308 = getelementptr inbounds i64, i64* %cloptr13307, i64 0                  ; &cloptr13307[0]
  %f13309 = ptrtoint void(i64,i64)* @lam12388 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13309, i64* %eptr13308                                                 ; store fptr
  %arg9676 = ptrtoint i64* %cloptr13307 to i64                                       ; closure cast; i64* -> i64
  %rva12373 = add i64 0, 0                                                           ; quoted ()
  %rva12372 = call i64 @prim_cons(i64 %arg9676, i64 %rva12373)                       ; call prim_cons
  %rva12371 = call i64 @prim_cons(i64 %arg9677, i64 %rva12372)                       ; call prim_cons
  %cloptr13310 = inttoptr i64 %arg9678 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13311 = getelementptr inbounds i64, i64* %cloptr13310, i64 0                 ; &cloptr13310[0]
  %f13313 = load i64, i64* %i0ptr13311, align 8                                      ; load; *i0ptr13311
  %fptr13312 = inttoptr i64 %f13313 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13312(i64 %arg9678, i64 %rva12371)                  ; tail call
  ret void
}


define i32 @main() {
  call fastcc void @proc_main()
  ret i32 0
}



define void @lam13299(i64 %env13300, i64 %rvp10929) {
  %cont9668 = call i64 @prim_car(i64 %rvp10929)                                      ; call prim_car
  %rvp10928 = call i64 @prim_cdr(i64 %rvp10929)                                      ; call prim_cdr
  %zlx$yu = call i64 @prim_car(i64 %rvp10928)                                        ; call prim_car
  %na10924 = call i64 @prim_cdr(i64 %rvp10928)                                       ; call prim_cdr
  %rva10927 = add i64 0, 0                                                           ; quoted ()
  %rva10926 = call i64 @prim_cons(i64 %zlx$yu, i64 %rva10927)                        ; call prim_cons
  %rva10925 = call i64 @prim_cons(i64 %cont9668, i64 %rva10926)                      ; call prim_cons
  %cloptr13314 = inttoptr i64 %zlx$yu to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr13315 = getelementptr inbounds i64, i64* %cloptr13314, i64 0                 ; &cloptr13314[0]
  %f13317 = load i64, i64* %i0ptr13315, align 8                                      ; load; *i0ptr13315
  %fptr13316 = inttoptr i64 %f13317 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13316(i64 %zlx$yu, i64 %rva10925)                   ; tail call
  ret void
}


define void @lam13296(i64 %env13297, i64 %rvp12342) {
  %_959404 = call i64 @prim_car(i64 %rvp12342)                                       ; call prim_car
  %rvp12341 = call i64 @prim_cdr(i64 %rvp12342)                                      ; call prim_cdr
  %L1Q$Ycmb = call i64 @prim_car(i64 %rvp12341)                                      ; call prim_car
  %na10931 = call i64 @prim_cdr(i64 %rvp12341)                                       ; call prim_cdr
  %cloptr13318 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr13320 = getelementptr inbounds i64, i64* %cloptr13318, i64 1                  ; &eptr13320[1]
  store i64 %L1Q$Ycmb, i64* %eptr13320                                               ; *eptr13320 = %L1Q$Ycmb
  %eptr13319 = getelementptr inbounds i64, i64* %cloptr13318, i64 0                  ; &cloptr13318[0]
  %f13321 = ptrtoint void(i64,i64)* @lam13294 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13321, i64* %eptr13319                                                 ; store fptr
  %arg9683 = ptrtoint i64* %cloptr13318 to i64                                       ; closure cast; i64* -> i64
  %cloptr13322 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13323 = getelementptr inbounds i64, i64* %cloptr13322, i64 0                  ; &cloptr13322[0]
  %f13324 = ptrtoint void(i64,i64)* @lam12401 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13324, i64* %eptr13323                                                 ; store fptr
  %arg9682 = ptrtoint i64* %cloptr13322 to i64                                       ; closure cast; i64* -> i64
  %rva12340 = add i64 0, 0                                                           ; quoted ()
  %rva12339 = call i64 @prim_cons(i64 %arg9682, i64 %rva12340)                       ; call prim_cons
  %rva12338 = call i64 @prim_cons(i64 %arg9683, i64 %rva12339)                       ; call prim_cons
  %cloptr13325 = inttoptr i64 %L1Q$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13326 = getelementptr inbounds i64, i64* %cloptr13325, i64 0                 ; &cloptr13325[0]
  %f13328 = load i64, i64* %i0ptr13326, align 8                                      ; load; *i0ptr13326
  %fptr13327 = inttoptr i64 %f13328 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13327(i64 %L1Q$Ycmb, i64 %rva12338)                 ; tail call
  ret void
}


define void @lam13294(i64 %env13295, i64 %rvp12308) {
  %envptr13329 = inttoptr i64 %env13295 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13330 = getelementptr inbounds i64, i64* %envptr13329, i64 1                ; &envptr13329[1]
  %L1Q$Ycmb = load i64, i64* %envptr13330, align 8                                   ; load; *envptr13330
  %_959405 = call i64 @prim_car(i64 %rvp12308)                                       ; call prim_car
  %rvp12307 = call i64 @prim_cdr(i64 %rvp12308)                                      ; call prim_cdr
  %LWd$_37foldr1 = call i64 @prim_car(i64 %rvp12307)                                 ; call prim_car
  %na10933 = call i64 @prim_cdr(i64 %rvp12307)                                       ; call prim_cdr
  %cloptr13331 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13333 = getelementptr inbounds i64, i64* %cloptr13331, i64 1                  ; &eptr13333[1]
  %eptr13334 = getelementptr inbounds i64, i64* %cloptr13331, i64 2                  ; &eptr13334[2]
  store i64 %LWd$_37foldr1, i64* %eptr13333                                          ; *eptr13333 = %LWd$_37foldr1
  store i64 %L1Q$Ycmb, i64* %eptr13334                                               ; *eptr13334 = %L1Q$Ycmb
  %eptr13332 = getelementptr inbounds i64, i64* %cloptr13331, i64 0                  ; &cloptr13331[0]
  %f13335 = ptrtoint void(i64,i64)* @lam13292 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13335, i64* %eptr13332                                                 ; store fptr
  %arg9686 = ptrtoint i64* %cloptr13331 to i64                                       ; closure cast; i64* -> i64
  %cloptr13336 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13337 = getelementptr inbounds i64, i64* %cloptr13336, i64 0                  ; &cloptr13336[0]
  %f13338 = ptrtoint void(i64,i64)* @lam12419 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13338, i64* %eptr13337                                                 ; store fptr
  %arg9685 = ptrtoint i64* %cloptr13336 to i64                                       ; closure cast; i64* -> i64
  %rva12306 = add i64 0, 0                                                           ; quoted ()
  %rva12305 = call i64 @prim_cons(i64 %arg9685, i64 %rva12306)                       ; call prim_cons
  %rva12304 = call i64 @prim_cons(i64 %arg9686, i64 %rva12305)                       ; call prim_cons
  %cloptr13339 = inttoptr i64 %L1Q$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13340 = getelementptr inbounds i64, i64* %cloptr13339, i64 0                 ; &cloptr13339[0]
  %f13342 = load i64, i64* %i0ptr13340, align 8                                      ; load; *i0ptr13340
  %fptr13341 = inttoptr i64 %f13342 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13341(i64 %L1Q$Ycmb, i64 %rva12304)                 ; tail call
  ret void
}


define void @lam13292(i64 %env13293, i64 %rvp12270) {
  %envptr13343 = inttoptr i64 %env13293 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13344 = getelementptr inbounds i64, i64* %envptr13343, i64 2                ; &envptr13343[2]
  %L1Q$Ycmb = load i64, i64* %envptr13344, align 8                                   ; load; *envptr13344
  %envptr13345 = inttoptr i64 %env13293 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13346 = getelementptr inbounds i64, i64* %envptr13345, i64 1                ; &envptr13345[1]
  %LWd$_37foldr1 = load i64, i64* %envptr13346, align 8                              ; load; *envptr13346
  %_959406 = call i64 @prim_car(i64 %rvp12270)                                       ; call prim_car
  %rvp12269 = call i64 @prim_cdr(i64 %rvp12270)                                      ; call prim_cdr
  %SVw$_37map1 = call i64 @prim_car(i64 %rvp12269)                                   ; call prim_car
  %na10935 = call i64 @prim_cdr(i64 %rvp12269)                                       ; call prim_cdr
  %cloptr13347 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13349 = getelementptr inbounds i64, i64* %cloptr13347, i64 1                  ; &eptr13349[1]
  %eptr13350 = getelementptr inbounds i64, i64* %cloptr13347, i64 2                  ; &eptr13350[2]
  %eptr13351 = getelementptr inbounds i64, i64* %cloptr13347, i64 3                  ; &eptr13351[3]
  store i64 %LWd$_37foldr1, i64* %eptr13349                                          ; *eptr13349 = %LWd$_37foldr1
  store i64 %SVw$_37map1, i64* %eptr13350                                            ; *eptr13350 = %SVw$_37map1
  store i64 %L1Q$Ycmb, i64* %eptr13351                                               ; *eptr13351 = %L1Q$Ycmb
  %eptr13348 = getelementptr inbounds i64, i64* %cloptr13347, i64 0                  ; &cloptr13347[0]
  %f13352 = ptrtoint void(i64,i64)* @lam13290 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13352, i64* %eptr13348                                                 ; store fptr
  %arg9689 = ptrtoint i64* %cloptr13347 to i64                                       ; closure cast; i64* -> i64
  %cloptr13353 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13354 = getelementptr inbounds i64, i64* %cloptr13353, i64 0                  ; &cloptr13353[0]
  %f13355 = ptrtoint void(i64,i64)* @lam12439 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13355, i64* %eptr13354                                                 ; store fptr
  %arg9688 = ptrtoint i64* %cloptr13353 to i64                                       ; closure cast; i64* -> i64
  %rva12268 = add i64 0, 0                                                           ; quoted ()
  %rva12267 = call i64 @prim_cons(i64 %arg9688, i64 %rva12268)                       ; call prim_cons
  %rva12266 = call i64 @prim_cons(i64 %arg9689, i64 %rva12267)                       ; call prim_cons
  %cloptr13356 = inttoptr i64 %L1Q$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13357 = getelementptr inbounds i64, i64* %cloptr13356, i64 0                 ; &cloptr13356[0]
  %f13359 = load i64, i64* %i0ptr13357, align 8                                      ; load; *i0ptr13357
  %fptr13358 = inttoptr i64 %f13359 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13358(i64 %L1Q$Ycmb, i64 %rva12266)                 ; tail call
  ret void
}


define void @lam13290(i64 %env13291, i64 %rvp12236) {
  %envptr13360 = inttoptr i64 %env13291 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13361 = getelementptr inbounds i64, i64* %envptr13360, i64 3                ; &envptr13360[3]
  %L1Q$Ycmb = load i64, i64* %envptr13361, align 8                                   ; load; *envptr13361
  %envptr13362 = inttoptr i64 %env13291 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13363 = getelementptr inbounds i64, i64* %envptr13362, i64 2                ; &envptr13362[2]
  %SVw$_37map1 = load i64, i64* %envptr13363, align 8                                ; load; *envptr13363
  %envptr13364 = inttoptr i64 %env13291 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13365 = getelementptr inbounds i64, i64* %envptr13364, i64 1                ; &envptr13364[1]
  %LWd$_37foldr1 = load i64, i64* %envptr13365, align 8                              ; load; *envptr13365
  %_959407 = call i64 @prim_car(i64 %rvp12236)                                       ; call prim_car
  %rvp12235 = call i64 @prim_cdr(i64 %rvp12236)                                      ; call prim_cdr
  %PdF$_37take = call i64 @prim_car(i64 %rvp12235)                                   ; call prim_car
  %na10937 = call i64 @prim_cdr(i64 %rvp12235)                                       ; call prim_cdr
  %cloptr13366 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr13368 = getelementptr inbounds i64, i64* %cloptr13366, i64 1                  ; &eptr13368[1]
  %eptr13369 = getelementptr inbounds i64, i64* %cloptr13366, i64 2                  ; &eptr13369[2]
  %eptr13370 = getelementptr inbounds i64, i64* %cloptr13366, i64 3                  ; &eptr13370[3]
  %eptr13371 = getelementptr inbounds i64, i64* %cloptr13366, i64 4                  ; &eptr13371[4]
  store i64 %LWd$_37foldr1, i64* %eptr13368                                          ; *eptr13368 = %LWd$_37foldr1
  store i64 %SVw$_37map1, i64* %eptr13369                                            ; *eptr13369 = %SVw$_37map1
  store i64 %L1Q$Ycmb, i64* %eptr13370                                               ; *eptr13370 = %L1Q$Ycmb
  store i64 %PdF$_37take, i64* %eptr13371                                            ; *eptr13371 = %PdF$_37take
  %eptr13367 = getelementptr inbounds i64, i64* %cloptr13366, i64 0                  ; &cloptr13366[0]
  %f13372 = ptrtoint void(i64,i64)* @lam13288 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13372, i64* %eptr13367                                                 ; store fptr
  %arg9692 = ptrtoint i64* %cloptr13366 to i64                                       ; closure cast; i64* -> i64
  %cloptr13373 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13374 = getelementptr inbounds i64, i64* %cloptr13373, i64 0                  ; &cloptr13373[0]
  %f13375 = ptrtoint void(i64,i64)* @lam12455 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13375, i64* %eptr13374                                                 ; store fptr
  %arg9691 = ptrtoint i64* %cloptr13373 to i64                                       ; closure cast; i64* -> i64
  %rva12234 = add i64 0, 0                                                           ; quoted ()
  %rva12233 = call i64 @prim_cons(i64 %arg9691, i64 %rva12234)                       ; call prim_cons
  %rva12232 = call i64 @prim_cons(i64 %arg9692, i64 %rva12233)                       ; call prim_cons
  %cloptr13376 = inttoptr i64 %L1Q$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13377 = getelementptr inbounds i64, i64* %cloptr13376, i64 0                 ; &cloptr13376[0]
  %f13379 = load i64, i64* %i0ptr13377, align 8                                      ; load; *i0ptr13377
  %fptr13378 = inttoptr i64 %f13379 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13378(i64 %L1Q$Ycmb, i64 %rva12232)                 ; tail call
  ret void
}


define void @lam13288(i64 %env13289, i64 %rvp12207) {
  %envptr13380 = inttoptr i64 %env13289 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13381 = getelementptr inbounds i64, i64* %envptr13380, i64 4                ; &envptr13380[4]
  %PdF$_37take = load i64, i64* %envptr13381, align 8                                ; load; *envptr13381
  %envptr13382 = inttoptr i64 %env13289 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13383 = getelementptr inbounds i64, i64* %envptr13382, i64 3                ; &envptr13382[3]
  %L1Q$Ycmb = load i64, i64* %envptr13383, align 8                                   ; load; *envptr13383
  %envptr13384 = inttoptr i64 %env13289 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13385 = getelementptr inbounds i64, i64* %envptr13384, i64 2                ; &envptr13384[2]
  %SVw$_37map1 = load i64, i64* %envptr13385, align 8                                ; load; *envptr13385
  %envptr13386 = inttoptr i64 %env13289 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13387 = getelementptr inbounds i64, i64* %envptr13386, i64 1                ; &envptr13386[1]
  %LWd$_37foldr1 = load i64, i64* %envptr13387, align 8                              ; load; *envptr13387
  %_959408 = call i64 @prim_car(i64 %rvp12207)                                       ; call prim_car
  %rvp12206 = call i64 @prim_cdr(i64 %rvp12207)                                      ; call prim_cdr
  %ecz$_37length = call i64 @prim_car(i64 %rvp12206)                                 ; call prim_car
  %na10939 = call i64 @prim_cdr(i64 %rvp12206)                                       ; call prim_cdr
  %cloptr13388 = call i64* @alloc(i64 48)                                            ; malloc
  %eptr13390 = getelementptr inbounds i64, i64* %cloptr13388, i64 1                  ; &eptr13390[1]
  %eptr13391 = getelementptr inbounds i64, i64* %cloptr13388, i64 2                  ; &eptr13391[2]
  %eptr13392 = getelementptr inbounds i64, i64* %cloptr13388, i64 3                  ; &eptr13392[3]
  %eptr13393 = getelementptr inbounds i64, i64* %cloptr13388, i64 4                  ; &eptr13393[4]
  %eptr13394 = getelementptr inbounds i64, i64* %cloptr13388, i64 5                  ; &eptr13394[5]
  store i64 %ecz$_37length, i64* %eptr13390                                          ; *eptr13390 = %ecz$_37length
  store i64 %LWd$_37foldr1, i64* %eptr13391                                          ; *eptr13391 = %LWd$_37foldr1
  store i64 %SVw$_37map1, i64* %eptr13392                                            ; *eptr13392 = %SVw$_37map1
  store i64 %L1Q$Ycmb, i64* %eptr13393                                               ; *eptr13393 = %L1Q$Ycmb
  store i64 %PdF$_37take, i64* %eptr13394                                            ; *eptr13394 = %PdF$_37take
  %eptr13389 = getelementptr inbounds i64, i64* %cloptr13388, i64 0                  ; &cloptr13388[0]
  %f13395 = ptrtoint void(i64,i64)* @lam13286 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13395, i64* %eptr13389                                                 ; store fptr
  %arg9695 = ptrtoint i64* %cloptr13388 to i64                                       ; closure cast; i64* -> i64
  %cloptr13396 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13397 = getelementptr inbounds i64, i64* %cloptr13396, i64 0                  ; &cloptr13396[0]
  %f13398 = ptrtoint void(i64,i64)* @lam12468 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13398, i64* %eptr13397                                                 ; store fptr
  %arg9694 = ptrtoint i64* %cloptr13396 to i64                                       ; closure cast; i64* -> i64
  %rva12205 = add i64 0, 0                                                           ; quoted ()
  %rva12204 = call i64 @prim_cons(i64 %arg9694, i64 %rva12205)                       ; call prim_cons
  %rva12203 = call i64 @prim_cons(i64 %arg9695, i64 %rva12204)                       ; call prim_cons
  %cloptr13399 = inttoptr i64 %L1Q$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13400 = getelementptr inbounds i64, i64* %cloptr13399, i64 0                 ; &cloptr13399[0]
  %f13402 = load i64, i64* %i0ptr13400, align 8                                      ; load; *i0ptr13400
  %fptr13401 = inttoptr i64 %f13402 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13401(i64 %L1Q$Ycmb, i64 %rva12203)                 ; tail call
  ret void
}


define void @lam13286(i64 %env13287, i64 %rvp12173) {
  %envptr13403 = inttoptr i64 %env13287 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13404 = getelementptr inbounds i64, i64* %envptr13403, i64 5                ; &envptr13403[5]
  %PdF$_37take = load i64, i64* %envptr13404, align 8                                ; load; *envptr13404
  %envptr13405 = inttoptr i64 %env13287 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13406 = getelementptr inbounds i64, i64* %envptr13405, i64 4                ; &envptr13405[4]
  %L1Q$Ycmb = load i64, i64* %envptr13406, align 8                                   ; load; *envptr13406
  %envptr13407 = inttoptr i64 %env13287 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13408 = getelementptr inbounds i64, i64* %envptr13407, i64 3                ; &envptr13407[3]
  %SVw$_37map1 = load i64, i64* %envptr13408, align 8                                ; load; *envptr13408
  %envptr13409 = inttoptr i64 %env13287 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13410 = getelementptr inbounds i64, i64* %envptr13409, i64 2                ; &envptr13409[2]
  %LWd$_37foldr1 = load i64, i64* %envptr13410, align 8                              ; load; *envptr13410
  %envptr13411 = inttoptr i64 %env13287 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13412 = getelementptr inbounds i64, i64* %envptr13411, i64 1                ; &envptr13411[1]
  %ecz$_37length = load i64, i64* %envptr13412, align 8                              ; load; *envptr13412
  %_959409 = call i64 @prim_car(i64 %rvp12173)                                       ; call prim_car
  %rvp12172 = call i64 @prim_cdr(i64 %rvp12173)                                      ; call prim_cdr
  %X5k$_37foldl1 = call i64 @prim_car(i64 %rvp12172)                                 ; call prim_car
  %na10941 = call i64 @prim_cdr(i64 %rvp12172)                                       ; call prim_cdr
  %cloptr13413 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr13415 = getelementptr inbounds i64, i64* %cloptr13413, i64 1                  ; &eptr13415[1]
  store i64 %X5k$_37foldl1, i64* %eptr13415                                          ; *eptr13415 = %X5k$_37foldl1
  %eptr13414 = getelementptr inbounds i64, i64* %cloptr13413, i64 0                  ; &cloptr13413[0]
  %f13416 = ptrtoint void(i64,i64)* @lam13284 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13416, i64* %eptr13414                                                 ; store fptr
  %obG$_37last = ptrtoint i64* %cloptr13413 to i64                                   ; closure cast; i64* -> i64
  %cloptr13417 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13419 = getelementptr inbounds i64, i64* %cloptr13417, i64 1                  ; &eptr13419[1]
  %eptr13420 = getelementptr inbounds i64, i64* %cloptr13417, i64 2                  ; &eptr13420[2]
  store i64 %ecz$_37length, i64* %eptr13419                                          ; *eptr13419 = %ecz$_37length
  store i64 %PdF$_37take, i64* %eptr13420                                            ; *eptr13420 = %PdF$_37take
  %eptr13418 = getelementptr inbounds i64, i64* %cloptr13417, i64 0                  ; &cloptr13417[0]
  %f13421 = ptrtoint void(i64,i64)* @lam13276 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13421, i64* %eptr13418                                                 ; store fptr
  %Mo9$_37drop_45right = ptrtoint i64* %cloptr13417 to i64                           ; closure cast; i64* -> i64
  %cloptr13422 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr13424 = getelementptr inbounds i64, i64* %cloptr13422, i64 1                  ; &eptr13424[1]
  %eptr13425 = getelementptr inbounds i64, i64* %cloptr13422, i64 2                  ; &eptr13425[2]
  %eptr13426 = getelementptr inbounds i64, i64* %cloptr13422, i64 3                  ; &eptr13426[3]
  %eptr13427 = getelementptr inbounds i64, i64* %cloptr13422, i64 4                  ; &eptr13427[4]
  %eptr13428 = getelementptr inbounds i64, i64* %cloptr13422, i64 5                  ; &eptr13428[5]
  %eptr13429 = getelementptr inbounds i64, i64* %cloptr13422, i64 6                  ; &eptr13429[6]
  store i64 %ecz$_37length, i64* %eptr13424                                          ; *eptr13424 = %ecz$_37length
  store i64 %LWd$_37foldr1, i64* %eptr13425                                          ; *eptr13425 = %LWd$_37foldr1
  store i64 %Mo9$_37drop_45right, i64* %eptr13426                                    ; *eptr13426 = %Mo9$_37drop_45right
  store i64 %X5k$_37foldl1, i64* %eptr13427                                          ; *eptr13427 = %X5k$_37foldl1
  store i64 %obG$_37last, i64* %eptr13428                                            ; *eptr13428 = %obG$_37last
  store i64 %L1Q$Ycmb, i64* %eptr13429                                               ; *eptr13429 = %L1Q$Ycmb
  %eptr13423 = getelementptr inbounds i64, i64* %cloptr13422, i64 0                  ; &cloptr13422[0]
  %f13430 = ptrtoint void(i64,i64)* @lam13270 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13430, i64* %eptr13423                                                 ; store fptr
  %arg9715 = ptrtoint i64* %cloptr13422 to i64                                       ; closure cast; i64* -> i64
  %cloptr13431 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13433 = getelementptr inbounds i64, i64* %cloptr13431, i64 1                  ; &eptr13433[1]
  %eptr13434 = getelementptr inbounds i64, i64* %cloptr13431, i64 2                  ; &eptr13434[2]
  store i64 %LWd$_37foldr1, i64* %eptr13433                                          ; *eptr13433 = %LWd$_37foldr1
  store i64 %SVw$_37map1, i64* %eptr13434                                            ; *eptr13434 = %SVw$_37map1
  %eptr13432 = getelementptr inbounds i64, i64* %cloptr13431, i64 0                  ; &cloptr13431[0]
  %f13435 = ptrtoint void(i64,i64)* @lam12519 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13435, i64* %eptr13432                                                 ; store fptr
  %arg9714 = ptrtoint i64* %cloptr13431 to i64                                       ; closure cast; i64* -> i64
  %rva12171 = add i64 0, 0                                                           ; quoted ()
  %rva12170 = call i64 @prim_cons(i64 %arg9714, i64 %rva12171)                       ; call prim_cons
  %rva12169 = call i64 @prim_cons(i64 %arg9715, i64 %rva12170)                       ; call prim_cons
  %cloptr13436 = inttoptr i64 %L1Q$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13437 = getelementptr inbounds i64, i64* %cloptr13436, i64 0                 ; &cloptr13436[0]
  %f13439 = load i64, i64* %i0ptr13437, align 8                                      ; load; *i0ptr13437
  %fptr13438 = inttoptr i64 %f13439 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13438(i64 %L1Q$Ycmb, i64 %rva12169)                 ; tail call
  ret void
}


define void @lam13284(i64 %env13285, i64 %rvp10958) {
  %envptr13440 = inttoptr i64 %env13285 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13441 = getelementptr inbounds i64, i64* %envptr13440, i64 1                ; &envptr13440[1]
  %X5k$_37foldl1 = load i64, i64* %envptr13441, align 8                              ; load; *envptr13441
  %cont9410 = call i64 @prim_car(i64 %rvp10958)                                      ; call prim_car
  %rvp10957 = call i64 @prim_cdr(i64 %rvp10958)                                      ; call prim_cdr
  %gZY$lst = call i64 @prim_car(i64 %rvp10957)                                       ; call prim_car
  %na10943 = call i64 @prim_cdr(i64 %rvp10957)                                       ; call prim_cdr
  %cloptr13442 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13443 = getelementptr inbounds i64, i64* %cloptr13442, i64 0                  ; &cloptr13442[0]
  %f13444 = ptrtoint void(i64,i64)* @lam13282 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13444, i64* %eptr13443                                                 ; store fptr
  %arg9699 = ptrtoint i64* %cloptr13442 to i64                                       ; closure cast; i64* -> i64
  %arg9698 = add i64 0, 0                                                            ; quoted ()
  %rva10956 = add i64 0, 0                                                           ; quoted ()
  %rva10955 = call i64 @prim_cons(i64 %gZY$lst, i64 %rva10956)                       ; call prim_cons
  %rva10954 = call i64 @prim_cons(i64 %arg9698, i64 %rva10955)                       ; call prim_cons
  %rva10953 = call i64 @prim_cons(i64 %arg9699, i64 %rva10954)                       ; call prim_cons
  %rva10952 = call i64 @prim_cons(i64 %cont9410, i64 %rva10953)                      ; call prim_cons
  %cloptr13445 = inttoptr i64 %X5k$_37foldl1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr13446 = getelementptr inbounds i64, i64* %cloptr13445, i64 0                 ; &cloptr13445[0]
  %f13448 = load i64, i64* %i0ptr13446, align 8                                      ; load; *i0ptr13446
  %fptr13447 = inttoptr i64 %f13448 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13447(i64 %X5k$_37foldl1, i64 %rva10952)            ; tail call
  ret void
}


define void @lam13282(i64 %env13283, i64 %rvp10951) {
  %cont9411 = call i64 @prim_car(i64 %rvp10951)                                      ; call prim_car
  %rvp10950 = call i64 @prim_cdr(i64 %rvp10951)                                      ; call prim_cdr
  %naA$x = call i64 @prim_car(i64 %rvp10950)                                         ; call prim_car
  %rvp10949 = call i64 @prim_cdr(i64 %rvp10950)                                      ; call prim_cdr
  %FrQ$y = call i64 @prim_car(i64 %rvp10949)                                         ; call prim_car
  %na10945 = call i64 @prim_cdr(i64 %rvp10949)                                       ; call prim_cdr
  %arg9703 = add i64 0, 0                                                            ; quoted ()
  %rva10948 = add i64 0, 0                                                           ; quoted ()
  %rva10947 = call i64 @prim_cons(i64 %naA$x, i64 %rva10948)                         ; call prim_cons
  %rva10946 = call i64 @prim_cons(i64 %arg9703, i64 %rva10947)                       ; call prim_cons
  %cloptr13449 = inttoptr i64 %cont9411 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13450 = getelementptr inbounds i64, i64* %cloptr13449, i64 0                 ; &cloptr13449[0]
  %f13452 = load i64, i64* %i0ptr13450, align 8                                      ; load; *i0ptr13450
  %fptr13451 = inttoptr i64 %f13452 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13451(i64 %cont9411, i64 %rva10946)                 ; tail call
  ret void
}


define void @lam13276(i64 %env13277, i64 %rvp10974) {
  %envptr13453 = inttoptr i64 %env13277 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13454 = getelementptr inbounds i64, i64* %envptr13453, i64 2                ; &envptr13453[2]
  %PdF$_37take = load i64, i64* %envptr13454, align 8                                ; load; *envptr13454
  %envptr13455 = inttoptr i64 %env13277 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13456 = getelementptr inbounds i64, i64* %envptr13455, i64 1                ; &envptr13455[1]
  %ecz$_37length = load i64, i64* %envptr13456, align 8                              ; load; *envptr13456
  %cont9412 = call i64 @prim_car(i64 %rvp10974)                                      ; call prim_car
  %rvp10973 = call i64 @prim_cdr(i64 %rvp10974)                                      ; call prim_cdr
  %d5K$lst = call i64 @prim_car(i64 %rvp10973)                                       ; call prim_car
  %rvp10972 = call i64 @prim_cdr(i64 %rvp10973)                                      ; call prim_cdr
  %lKZ$n = call i64 @prim_car(i64 %rvp10972)                                         ; call prim_car
  %na10960 = call i64 @prim_cdr(i64 %rvp10972)                                       ; call prim_cdr
  %cloptr13457 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr13459 = getelementptr inbounds i64, i64* %cloptr13457, i64 1                  ; &eptr13459[1]
  %eptr13460 = getelementptr inbounds i64, i64* %cloptr13457, i64 2                  ; &eptr13460[2]
  %eptr13461 = getelementptr inbounds i64, i64* %cloptr13457, i64 3                  ; &eptr13461[3]
  %eptr13462 = getelementptr inbounds i64, i64* %cloptr13457, i64 4                  ; &eptr13462[4]
  store i64 %cont9412, i64* %eptr13459                                               ; *eptr13459 = %cont9412
  store i64 %lKZ$n, i64* %eptr13460                                                  ; *eptr13460 = %lKZ$n
  store i64 %d5K$lst, i64* %eptr13461                                                ; *eptr13461 = %d5K$lst
  store i64 %PdF$_37take, i64* %eptr13462                                            ; *eptr13462 = %PdF$_37take
  %eptr13458 = getelementptr inbounds i64, i64* %cloptr13457, i64 0                  ; &cloptr13457[0]
  %f13463 = ptrtoint void(i64,i64)* @lam13274 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13463, i64* %eptr13458                                                 ; store fptr
  %arg9706 = ptrtoint i64* %cloptr13457 to i64                                       ; closure cast; i64* -> i64
  %rva10971 = add i64 0, 0                                                           ; quoted ()
  %rva10970 = call i64 @prim_cons(i64 %d5K$lst, i64 %rva10971)                       ; call prim_cons
  %rva10969 = call i64 @prim_cons(i64 %arg9706, i64 %rva10970)                       ; call prim_cons
  %cloptr13464 = inttoptr i64 %ecz$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr13465 = getelementptr inbounds i64, i64* %cloptr13464, i64 0                 ; &cloptr13464[0]
  %f13467 = load i64, i64* %i0ptr13465, align 8                                      ; load; *i0ptr13465
  %fptr13466 = inttoptr i64 %f13467 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13466(i64 %ecz$_37length, i64 %rva10969)            ; tail call
  ret void
}


define void @lam13274(i64 %env13275, i64 %rvp10968) {
  %envptr13468 = inttoptr i64 %env13275 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13469 = getelementptr inbounds i64, i64* %envptr13468, i64 4                ; &envptr13468[4]
  %PdF$_37take = load i64, i64* %envptr13469, align 8                                ; load; *envptr13469
  %envptr13470 = inttoptr i64 %env13275 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13471 = getelementptr inbounds i64, i64* %envptr13470, i64 3                ; &envptr13470[3]
  %d5K$lst = load i64, i64* %envptr13471, align 8                                    ; load; *envptr13471
  %envptr13472 = inttoptr i64 %env13275 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13473 = getelementptr inbounds i64, i64* %envptr13472, i64 2                ; &envptr13472[2]
  %lKZ$n = load i64, i64* %envptr13473, align 8                                      ; load; *envptr13473
  %envptr13474 = inttoptr i64 %env13275 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13475 = getelementptr inbounds i64, i64* %envptr13474, i64 1                ; &envptr13474[1]
  %cont9412 = load i64, i64* %envptr13475, align 8                                   ; load; *envptr13475
  %_959413 = call i64 @prim_car(i64 %rvp10968)                                       ; call prim_car
  %rvp10967 = call i64 @prim_cdr(i64 %rvp10968)                                      ; call prim_cdr
  %a9248 = call i64 @prim_car(i64 %rvp10967)                                         ; call prim_car
  %na10962 = call i64 @prim_cdr(i64 %rvp10967)                                       ; call prim_cdr
  %a9249 = call i64 @prim__45(i64 %a9248, i64 %lKZ$n)                                ; call prim__45
  %rva10966 = add i64 0, 0                                                           ; quoted ()
  %rva10965 = call i64 @prim_cons(i64 %a9249, i64 %rva10966)                         ; call prim_cons
  %rva10964 = call i64 @prim_cons(i64 %d5K$lst, i64 %rva10965)                       ; call prim_cons
  %rva10963 = call i64 @prim_cons(i64 %cont9412, i64 %rva10964)                      ; call prim_cons
  %cloptr13476 = inttoptr i64 %PdF$_37take to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr13477 = getelementptr inbounds i64, i64* %cloptr13476, i64 0                 ; &cloptr13476[0]
  %f13479 = load i64, i64* %i0ptr13477, align 8                                      ; load; *i0ptr13477
  %fptr13478 = inttoptr i64 %f13479 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13478(i64 %PdF$_37take, i64 %rva10963)              ; tail call
  ret void
}


define void @lam13270(i64 %env13271, i64 %rvp12073) {
  %envptr13480 = inttoptr i64 %env13271 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13481 = getelementptr inbounds i64, i64* %envptr13480, i64 6                ; &envptr13480[6]
  %L1Q$Ycmb = load i64, i64* %envptr13481, align 8                                   ; load; *envptr13481
  %envptr13482 = inttoptr i64 %env13271 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13483 = getelementptr inbounds i64, i64* %envptr13482, i64 5                ; &envptr13482[5]
  %obG$_37last = load i64, i64* %envptr13483, align 8                                ; load; *envptr13483
  %envptr13484 = inttoptr i64 %env13271 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13485 = getelementptr inbounds i64, i64* %envptr13484, i64 4                ; &envptr13484[4]
  %X5k$_37foldl1 = load i64, i64* %envptr13485, align 8                              ; load; *envptr13485
  %envptr13486 = inttoptr i64 %env13271 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13487 = getelementptr inbounds i64, i64* %envptr13486, i64 3                ; &envptr13486[3]
  %Mo9$_37drop_45right = load i64, i64* %envptr13487, align 8                        ; load; *envptr13487
  %envptr13488 = inttoptr i64 %env13271 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13489 = getelementptr inbounds i64, i64* %envptr13488, i64 2                ; &envptr13488[2]
  %LWd$_37foldr1 = load i64, i64* %envptr13489, align 8                              ; load; *envptr13489
  %envptr13490 = inttoptr i64 %env13271 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13491 = getelementptr inbounds i64, i64* %envptr13490, i64 1                ; &envptr13490[1]
  %ecz$_37length = load i64, i64* %envptr13491, align 8                              ; load; *envptr13491
  %_959414 = call i64 @prim_car(i64 %rvp12073)                                       ; call prim_car
  %rvp12072 = call i64 @prim_cdr(i64 %rvp12073)                                      ; call prim_cdr
  %ZKo$_37foldr = call i64 @prim_car(i64 %rvp12072)                                  ; call prim_car
  %na10976 = call i64 @prim_cdr(i64 %rvp12072)                                       ; call prim_cdr
  %cloptr13492 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr13494 = getelementptr inbounds i64, i64* %cloptr13492, i64 1                  ; &eptr13494[1]
  store i64 %LWd$_37foldr1, i64* %eptr13494                                          ; *eptr13494 = %LWd$_37foldr1
  %eptr13493 = getelementptr inbounds i64, i64* %cloptr13492, i64 0                  ; &cloptr13492[0]
  %f13495 = ptrtoint void(i64,i64)* @lam13268 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13495, i64* %eptr13493                                                 ; store fptr
  %ujo$_37map1 = ptrtoint i64* %cloptr13492 to i64                                   ; closure cast; i64* -> i64
  %cloptr13496 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13498 = getelementptr inbounds i64, i64* %cloptr13496, i64 1                  ; &eptr13498[1]
  %eptr13499 = getelementptr inbounds i64, i64* %cloptr13496, i64 2                  ; &eptr13499[2]
  %eptr13500 = getelementptr inbounds i64, i64* %cloptr13496, i64 3                  ; &eptr13500[3]
  store i64 %ZKo$_37foldr, i64* %eptr13498                                           ; *eptr13498 = %ZKo$_37foldr
  store i64 %Mo9$_37drop_45right, i64* %eptr13499                                    ; *eptr13499 = %Mo9$_37drop_45right
  store i64 %obG$_37last, i64* %eptr13500                                            ; *eptr13500 = %obG$_37last
  %eptr13497 = getelementptr inbounds i64, i64* %cloptr13496, i64 0                  ; &cloptr13496[0]
  %f13501 = ptrtoint void(i64,i64)* @lam13257 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13501, i64* %eptr13497                                                 ; store fptr
  %smp$_37map = ptrtoint i64* %cloptr13496 to i64                                    ; closure cast; i64* -> i64
  %cloptr13502 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13504 = getelementptr inbounds i64, i64* %cloptr13502, i64 1                  ; &eptr13504[1]
  %eptr13505 = getelementptr inbounds i64, i64* %cloptr13502, i64 2                  ; &eptr13505[2]
  store i64 %ecz$_37length, i64* %eptr13504                                          ; *eptr13504 = %ecz$_37length
  store i64 %X5k$_37foldl1, i64* %eptr13505                                          ; *eptr13505 = %X5k$_37foldl1
  %eptr13503 = getelementptr inbounds i64, i64* %cloptr13502, i64 0                  ; &cloptr13502[0]
  %f13506 = ptrtoint void(i64,i64)* @lam13241 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13506, i64* %eptr13503                                                 ; store fptr
  %arg9757 = ptrtoint i64* %cloptr13502 to i64                                       ; closure cast; i64* -> i64
  %cloptr13507 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13509 = getelementptr inbounds i64, i64* %cloptr13507, i64 1                  ; &eptr13509[1]
  %eptr13510 = getelementptr inbounds i64, i64* %cloptr13507, i64 2                  ; &eptr13510[2]
  %eptr13511 = getelementptr inbounds i64, i64* %cloptr13507, i64 3                  ; &eptr13511[3]
  store i64 %ZKo$_37foldr, i64* %eptr13509                                           ; *eptr13509 = %ZKo$_37foldr
  store i64 %LWd$_37foldr1, i64* %eptr13510                                          ; *eptr13510 = %LWd$_37foldr1
  store i64 %ujo$_37map1, i64* %eptr13511                                            ; *eptr13511 = %ujo$_37map1
  %eptr13508 = getelementptr inbounds i64, i64* %cloptr13507, i64 0                  ; &cloptr13507[0]
  %f13512 = ptrtoint void(i64,i64)* @lam12570 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13512, i64* %eptr13508                                                 ; store fptr
  %arg9756 = ptrtoint i64* %cloptr13507 to i64                                       ; closure cast; i64* -> i64
  %rva12071 = add i64 0, 0                                                           ; quoted ()
  %rva12070 = call i64 @prim_cons(i64 %arg9756, i64 %rva12071)                       ; call prim_cons
  %rva12069 = call i64 @prim_cons(i64 %arg9757, i64 %rva12070)                       ; call prim_cons
  %cloptr13513 = inttoptr i64 %L1Q$Ycmb to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13514 = getelementptr inbounds i64, i64* %cloptr13513, i64 0                 ; &cloptr13513[0]
  %f13516 = load i64, i64* %i0ptr13514, align 8                                      ; load; *i0ptr13514
  %fptr13515 = inttoptr i64 %f13516 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13515(i64 %L1Q$Ycmb, i64 %rva12069)                 ; tail call
  ret void
}


define void @lam13268(i64 %env13269, i64 %rvp11001) {
  %envptr13517 = inttoptr i64 %env13269 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13518 = getelementptr inbounds i64, i64* %envptr13517, i64 1                ; &envptr13517[1]
  %LWd$_37foldr1 = load i64, i64* %envptr13518, align 8                              ; load; *envptr13518
  %cont9415 = call i64 @prim_car(i64 %rvp11001)                                      ; call prim_car
  %rvp11000 = call i64 @prim_cdr(i64 %rvp11001)                                      ; call prim_cdr
  %Y6B$f = call i64 @prim_car(i64 %rvp11000)                                         ; call prim_car
  %rvp10999 = call i64 @prim_cdr(i64 %rvp11000)                                      ; call prim_cdr
  %qNK$lst = call i64 @prim_car(i64 %rvp10999)                                       ; call prim_car
  %na10978 = call i64 @prim_cdr(i64 %rvp10999)                                       ; call prim_cdr
  %cloptr13519 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr13521 = getelementptr inbounds i64, i64* %cloptr13519, i64 1                  ; &eptr13521[1]
  store i64 %Y6B$f, i64* %eptr13521                                                  ; *eptr13521 = %Y6B$f
  %eptr13520 = getelementptr inbounds i64, i64* %cloptr13519, i64 0                  ; &cloptr13519[0]
  %f13522 = ptrtoint void(i64,i64)* @lam13266 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13522, i64* %eptr13520                                                 ; store fptr
  %arg9719 = ptrtoint i64* %cloptr13519 to i64                                       ; closure cast; i64* -> i64
  %arg9718 = add i64 0, 0                                                            ; quoted ()
  %rva10998 = add i64 0, 0                                                           ; quoted ()
  %rva10997 = call i64 @prim_cons(i64 %qNK$lst, i64 %rva10998)                       ; call prim_cons
  %rva10996 = call i64 @prim_cons(i64 %arg9718, i64 %rva10997)                       ; call prim_cons
  %rva10995 = call i64 @prim_cons(i64 %arg9719, i64 %rva10996)                       ; call prim_cons
  %rva10994 = call i64 @prim_cons(i64 %cont9415, i64 %rva10995)                      ; call prim_cons
  %cloptr13523 = inttoptr i64 %LWd$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr13524 = getelementptr inbounds i64, i64* %cloptr13523, i64 0                 ; &cloptr13523[0]
  %f13526 = load i64, i64* %i0ptr13524, align 8                                      ; load; *i0ptr13524
  %fptr13525 = inttoptr i64 %f13526 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13525(i64 %LWd$_37foldr1, i64 %rva10994)            ; tail call
  ret void
}


define void @lam13266(i64 %env13267, i64 %rvp10993) {
  %envptr13527 = inttoptr i64 %env13267 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13528 = getelementptr inbounds i64, i64* %envptr13527, i64 1                ; &envptr13527[1]
  %Y6B$f = load i64, i64* %envptr13528, align 8                                      ; load; *envptr13528
  %cont9416 = call i64 @prim_car(i64 %rvp10993)                                      ; call prim_car
  %rvp10992 = call i64 @prim_cdr(i64 %rvp10993)                                      ; call prim_cdr
  %ssh$v = call i64 @prim_car(i64 %rvp10992)                                         ; call prim_car
  %rvp10991 = call i64 @prim_cdr(i64 %rvp10992)                                      ; call prim_cdr
  %U44$r = call i64 @prim_car(i64 %rvp10991)                                         ; call prim_car
  %na10980 = call i64 @prim_cdr(i64 %rvp10991)                                       ; call prim_cdr
  %cloptr13529 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13531 = getelementptr inbounds i64, i64* %cloptr13529, i64 1                  ; &eptr13531[1]
  %eptr13532 = getelementptr inbounds i64, i64* %cloptr13529, i64 2                  ; &eptr13532[2]
  store i64 %cont9416, i64* %eptr13531                                               ; *eptr13531 = %cont9416
  store i64 %U44$r, i64* %eptr13532                                                  ; *eptr13532 = %U44$r
  %eptr13530 = getelementptr inbounds i64, i64* %cloptr13529, i64 0                  ; &cloptr13529[0]
  %f13533 = ptrtoint void(i64,i64)* @lam13264 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13533, i64* %eptr13530                                                 ; store fptr
  %arg9723 = ptrtoint i64* %cloptr13529 to i64                                       ; closure cast; i64* -> i64
  %rva10990 = add i64 0, 0                                                           ; quoted ()
  %rva10989 = call i64 @prim_cons(i64 %ssh$v, i64 %rva10990)                         ; call prim_cons
  %rva10988 = call i64 @prim_cons(i64 %arg9723, i64 %rva10989)                       ; call prim_cons
  %cloptr13534 = inttoptr i64 %Y6B$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13535 = getelementptr inbounds i64, i64* %cloptr13534, i64 0                 ; &cloptr13534[0]
  %f13537 = load i64, i64* %i0ptr13535, align 8                                      ; load; *i0ptr13535
  %fptr13536 = inttoptr i64 %f13537 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13536(i64 %Y6B$f, i64 %rva10988)                    ; tail call
  ret void
}


define void @lam13264(i64 %env13265, i64 %rvp10987) {
  %envptr13538 = inttoptr i64 %env13265 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13539 = getelementptr inbounds i64, i64* %envptr13538, i64 2                ; &envptr13538[2]
  %U44$r = load i64, i64* %envptr13539, align 8                                      ; load; *envptr13539
  %envptr13540 = inttoptr i64 %env13265 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13541 = getelementptr inbounds i64, i64* %envptr13540, i64 1                ; &envptr13540[1]
  %cont9416 = load i64, i64* %envptr13541, align 8                                   ; load; *envptr13541
  %_959417 = call i64 @prim_car(i64 %rvp10987)                                       ; call prim_car
  %rvp10986 = call i64 @prim_cdr(i64 %rvp10987)                                      ; call prim_cdr
  %a9258 = call i64 @prim_car(i64 %rvp10986)                                         ; call prim_car
  %na10982 = call i64 @prim_cdr(i64 %rvp10986)                                       ; call prim_cdr
  %retprim9418 = call i64 @prim_cons(i64 %a9258, i64 %U44$r)                         ; call prim_cons
  %arg9728 = add i64 0, 0                                                            ; quoted ()
  %rva10985 = add i64 0, 0                                                           ; quoted ()
  %rva10984 = call i64 @prim_cons(i64 %retprim9418, i64 %rva10985)                   ; call prim_cons
  %rva10983 = call i64 @prim_cons(i64 %arg9728, i64 %rva10984)                       ; call prim_cons
  %cloptr13542 = inttoptr i64 %cont9416 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13543 = getelementptr inbounds i64, i64* %cloptr13542, i64 0                 ; &cloptr13542[0]
  %f13545 = load i64, i64* %i0ptr13543, align 8                                      ; load; *i0ptr13543
  %fptr13544 = inttoptr i64 %f13545 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13544(i64 %cont9416, i64 %rva10983)                 ; tail call
  ret void
}


define void @lam13257(i64 %env13258, i64 %ADS$args9420) {
  %envptr13546 = inttoptr i64 %env13258 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13547 = getelementptr inbounds i64, i64* %envptr13546, i64 3                ; &envptr13546[3]
  %obG$_37last = load i64, i64* %envptr13547, align 8                                ; load; *envptr13547
  %envptr13548 = inttoptr i64 %env13258 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13549 = getelementptr inbounds i64, i64* %envptr13548, i64 2                ; &envptr13548[2]
  %Mo9$_37drop_45right = load i64, i64* %envptr13549, align 8                        ; load; *envptr13549
  %envptr13550 = inttoptr i64 %env13258 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13551 = getelementptr inbounds i64, i64* %envptr13550, i64 1                ; &envptr13550[1]
  %ZKo$_37foldr = load i64, i64* %envptr13551, align 8                               ; load; *envptr13551
  %cont9419 = call i64 @prim_car(i64 %ADS$args9420)                                  ; call prim_car
  %ADS$args = call i64 @prim_cdr(i64 %ADS$args9420)                                  ; call prim_cdr
  %Sie$f = call i64 @prim_car(i64 %ADS$args)                                         ; call prim_car
  %G7X$lsts = call i64 @prim_cdr(i64 %ADS$args)                                      ; call prim_cdr
  %arg9735 = add i64 0, 0                                                            ; quoted ()
  %a9262 = call i64 @prim_cons(i64 %arg9735, i64 %G7X$lsts)                          ; call prim_cons
  %cloptr13552 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13554 = getelementptr inbounds i64, i64* %cloptr13552, i64 1                  ; &eptr13554[1]
  %eptr13555 = getelementptr inbounds i64, i64* %cloptr13552, i64 2                  ; &eptr13555[2]
  %eptr13556 = getelementptr inbounds i64, i64* %cloptr13552, i64 3                  ; &eptr13556[3]
  store i64 %Sie$f, i64* %eptr13554                                                  ; *eptr13554 = %Sie$f
  store i64 %Mo9$_37drop_45right, i64* %eptr13555                                    ; *eptr13555 = %Mo9$_37drop_45right
  store i64 %obG$_37last, i64* %eptr13556                                            ; *eptr13556 = %obG$_37last
  %eptr13553 = getelementptr inbounds i64, i64* %cloptr13552, i64 0                  ; &cloptr13552[0]
  %f13557 = ptrtoint void(i64,i64)* @lam13254 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13557, i64* %eptr13553                                                 ; store fptr
  %arg9737 = ptrtoint i64* %cloptr13552 to i64                                       ; closure cast; i64* -> i64
  %a9263 = call i64 @prim_cons(i64 %arg9737, i64 %a9262)                             ; call prim_cons
  %cps_45lst9428 = call i64 @prim_cons(i64 %cont9419, i64 %a9263)                    ; call prim_cons
  %cloptr13558 = inttoptr i64 %ZKo$_37foldr to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr13559 = getelementptr inbounds i64, i64* %cloptr13558, i64 0                 ; &cloptr13558[0]
  %f13561 = load i64, i64* %i0ptr13559, align 8                                      ; load; *i0ptr13559
  %fptr13560 = inttoptr i64 %f13561 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13560(i64 %ZKo$_37foldr, i64 %cps_45lst9428)        ; tail call
  ret void
}


define void @lam13254(i64 %env13255, i64 %exX$fargs9422) {
  %envptr13562 = inttoptr i64 %env13255 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13563 = getelementptr inbounds i64, i64* %envptr13562, i64 3                ; &envptr13562[3]
  %obG$_37last = load i64, i64* %envptr13563, align 8                                ; load; *envptr13563
  %envptr13564 = inttoptr i64 %env13255 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13565 = getelementptr inbounds i64, i64* %envptr13564, i64 2                ; &envptr13564[2]
  %Mo9$_37drop_45right = load i64, i64* %envptr13565, align 8                        ; load; *envptr13565
  %envptr13566 = inttoptr i64 %env13255 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13567 = getelementptr inbounds i64, i64* %envptr13566, i64 1                ; &envptr13566[1]
  %Sie$f = load i64, i64* %envptr13567, align 8                                      ; load; *envptr13567
  %cont9421 = call i64 @prim_car(i64 %exX$fargs9422)                                 ; call prim_car
  %exX$fargs = call i64 @prim_cdr(i64 %exX$fargs9422)                                ; call prim_cdr
  %cloptr13568 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr13570 = getelementptr inbounds i64, i64* %cloptr13568, i64 1                  ; &eptr13570[1]
  %eptr13571 = getelementptr inbounds i64, i64* %cloptr13568, i64 2                  ; &eptr13571[2]
  %eptr13572 = getelementptr inbounds i64, i64* %cloptr13568, i64 3                  ; &eptr13572[3]
  %eptr13573 = getelementptr inbounds i64, i64* %cloptr13568, i64 4                  ; &eptr13573[4]
  store i64 %Sie$f, i64* %eptr13570                                                  ; *eptr13570 = %Sie$f
  store i64 %obG$_37last, i64* %eptr13571                                            ; *eptr13571 = %obG$_37last
  store i64 %exX$fargs, i64* %eptr13572                                              ; *eptr13572 = %exX$fargs
  store i64 %cont9421, i64* %eptr13573                                               ; *eptr13573 = %cont9421
  %eptr13569 = getelementptr inbounds i64, i64* %cloptr13568, i64 0                  ; &cloptr13568[0]
  %f13574 = ptrtoint void(i64,i64)* @lam13252 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13574, i64* %eptr13569                                                 ; store fptr
  %arg9742 = ptrtoint i64* %cloptr13568 to i64                                       ; closure cast; i64* -> i64
  %arg9740 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %rva11023 = add i64 0, 0                                                           ; quoted ()
  %rva11022 = call i64 @prim_cons(i64 %arg9740, i64 %rva11023)                       ; call prim_cons
  %rva11021 = call i64 @prim_cons(i64 %exX$fargs, i64 %rva11022)                     ; call prim_cons
  %rva11020 = call i64 @prim_cons(i64 %arg9742, i64 %rva11021)                       ; call prim_cons
  %cloptr13575 = inttoptr i64 %Mo9$_37drop_45right to i64*                           ; closure/env cast; i64 -> i64*
  %i0ptr13576 = getelementptr inbounds i64, i64* %cloptr13575, i64 0                 ; &cloptr13575[0]
  %f13578 = load i64, i64* %i0ptr13576, align 8                                      ; load; *i0ptr13576
  %fptr13577 = inttoptr i64 %f13578 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13577(i64 %Mo9$_37drop_45right, i64 %rva11020)      ; tail call
  ret void
}


define void @lam13252(i64 %env13253, i64 %rvp11019) {
  %envptr13579 = inttoptr i64 %env13253 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13580 = getelementptr inbounds i64, i64* %envptr13579, i64 4                ; &envptr13579[4]
  %cont9421 = load i64, i64* %envptr13580, align 8                                   ; load; *envptr13580
  %envptr13581 = inttoptr i64 %env13253 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13582 = getelementptr inbounds i64, i64* %envptr13581, i64 3                ; &envptr13581[3]
  %exX$fargs = load i64, i64* %envptr13582, align 8                                  ; load; *envptr13582
  %envptr13583 = inttoptr i64 %env13253 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13584 = getelementptr inbounds i64, i64* %envptr13583, i64 2                ; &envptr13583[2]
  %obG$_37last = load i64, i64* %envptr13584, align 8                                ; load; *envptr13584
  %envptr13585 = inttoptr i64 %env13253 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13586 = getelementptr inbounds i64, i64* %envptr13585, i64 1                ; &envptr13585[1]
  %Sie$f = load i64, i64* %envptr13586, align 8                                      ; load; *envptr13586
  %_959423 = call i64 @prim_car(i64 %rvp11019)                                       ; call prim_car
  %rvp11018 = call i64 @prim_cdr(i64 %rvp11019)                                      ; call prim_cdr
  %a9259 = call i64 @prim_car(i64 %rvp11018)                                         ; call prim_car
  %na11003 = call i64 @prim_cdr(i64 %rvp11018)                                       ; call prim_cdr
  %cloptr13587 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13589 = getelementptr inbounds i64, i64* %cloptr13587, i64 1                  ; &eptr13589[1]
  %eptr13590 = getelementptr inbounds i64, i64* %cloptr13587, i64 2                  ; &eptr13590[2]
  %eptr13591 = getelementptr inbounds i64, i64* %cloptr13587, i64 3                  ; &eptr13591[3]
  store i64 %obG$_37last, i64* %eptr13589                                            ; *eptr13589 = %obG$_37last
  store i64 %exX$fargs, i64* %eptr13590                                              ; *eptr13590 = %exX$fargs
  store i64 %cont9421, i64* %eptr13591                                               ; *eptr13591 = %cont9421
  %eptr13588 = getelementptr inbounds i64, i64* %cloptr13587, i64 0                  ; &cloptr13587[0]
  %f13592 = ptrtoint void(i64,i64)* @lam13250 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13592, i64* %eptr13588                                                 ; store fptr
  %arg9745 = ptrtoint i64* %cloptr13587 to i64                                       ; closure cast; i64* -> i64
  %cps_45lst9427 = call i64 @prim_cons(i64 %arg9745, i64 %a9259)                     ; call prim_cons
  %cloptr13593 = inttoptr i64 %Sie$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13594 = getelementptr inbounds i64, i64* %cloptr13593, i64 0                 ; &cloptr13593[0]
  %f13596 = load i64, i64* %i0ptr13594, align 8                                      ; load; *i0ptr13594
  %fptr13595 = inttoptr i64 %f13596 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13595(i64 %Sie$f, i64 %cps_45lst9427)               ; tail call
  ret void
}


define void @lam13250(i64 %env13251, i64 %rvp11017) {
  %envptr13597 = inttoptr i64 %env13251 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13598 = getelementptr inbounds i64, i64* %envptr13597, i64 3                ; &envptr13597[3]
  %cont9421 = load i64, i64* %envptr13598, align 8                                   ; load; *envptr13598
  %envptr13599 = inttoptr i64 %env13251 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13600 = getelementptr inbounds i64, i64* %envptr13599, i64 2                ; &envptr13599[2]
  %exX$fargs = load i64, i64* %envptr13600, align 8                                  ; load; *envptr13600
  %envptr13601 = inttoptr i64 %env13251 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13602 = getelementptr inbounds i64, i64* %envptr13601, i64 1                ; &envptr13601[1]
  %obG$_37last = load i64, i64* %envptr13602, align 8                                ; load; *envptr13602
  %_959424 = call i64 @prim_car(i64 %rvp11017)                                       ; call prim_car
  %rvp11016 = call i64 @prim_cdr(i64 %rvp11017)                                      ; call prim_cdr
  %a9260 = call i64 @prim_car(i64 %rvp11016)                                         ; call prim_car
  %na11005 = call i64 @prim_cdr(i64 %rvp11016)                                       ; call prim_cdr
  %cloptr13603 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13605 = getelementptr inbounds i64, i64* %cloptr13603, i64 1                  ; &eptr13605[1]
  %eptr13606 = getelementptr inbounds i64, i64* %cloptr13603, i64 2                  ; &eptr13606[2]
  store i64 %cont9421, i64* %eptr13605                                               ; *eptr13605 = %cont9421
  store i64 %a9260, i64* %eptr13606                                                  ; *eptr13606 = %a9260
  %eptr13604 = getelementptr inbounds i64, i64* %cloptr13603, i64 0                  ; &cloptr13603[0]
  %f13607 = ptrtoint void(i64,i64)* @lam13248 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13607, i64* %eptr13604                                                 ; store fptr
  %arg9747 = ptrtoint i64* %cloptr13603 to i64                                       ; closure cast; i64* -> i64
  %rva11015 = add i64 0, 0                                                           ; quoted ()
  %rva11014 = call i64 @prim_cons(i64 %exX$fargs, i64 %rva11015)                     ; call prim_cons
  %rva11013 = call i64 @prim_cons(i64 %arg9747, i64 %rva11014)                       ; call prim_cons
  %cloptr13608 = inttoptr i64 %obG$_37last to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr13609 = getelementptr inbounds i64, i64* %cloptr13608, i64 0                 ; &cloptr13608[0]
  %f13611 = load i64, i64* %i0ptr13609, align 8                                      ; load; *i0ptr13609
  %fptr13610 = inttoptr i64 %f13611 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13610(i64 %obG$_37last, i64 %rva11013)              ; tail call
  ret void
}


define void @lam13248(i64 %env13249, i64 %rvp11012) {
  %envptr13612 = inttoptr i64 %env13249 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13613 = getelementptr inbounds i64, i64* %envptr13612, i64 2                ; &envptr13612[2]
  %a9260 = load i64, i64* %envptr13613, align 8                                      ; load; *envptr13613
  %envptr13614 = inttoptr i64 %env13249 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13615 = getelementptr inbounds i64, i64* %envptr13614, i64 1                ; &envptr13614[1]
  %cont9421 = load i64, i64* %envptr13615, align 8                                   ; load; *envptr13615
  %_959425 = call i64 @prim_car(i64 %rvp11012)                                       ; call prim_car
  %rvp11011 = call i64 @prim_cdr(i64 %rvp11012)                                      ; call prim_cdr
  %a9261 = call i64 @prim_car(i64 %rvp11011)                                         ; call prim_car
  %na11007 = call i64 @prim_cdr(i64 %rvp11011)                                       ; call prim_cdr
  %retprim9426 = call i64 @prim_cons(i64 %a9260, i64 %a9261)                         ; call prim_cons
  %arg9752 = add i64 0, 0                                                            ; quoted ()
  %rva11010 = add i64 0, 0                                                           ; quoted ()
  %rva11009 = call i64 @prim_cons(i64 %retprim9426, i64 %rva11010)                   ; call prim_cons
  %rva11008 = call i64 @prim_cons(i64 %arg9752, i64 %rva11009)                       ; call prim_cons
  %cloptr13616 = inttoptr i64 %cont9421 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13617 = getelementptr inbounds i64, i64* %cloptr13616, i64 0                 ; &cloptr13616[0]
  %f13619 = load i64, i64* %i0ptr13617, align 8                                      ; load; *i0ptr13617
  %fptr13618 = inttoptr i64 %f13619 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13618(i64 %cont9421, i64 %rva11008)                 ; tail call
  ret void
}


define void @lam13241(i64 %env13242, i64 %rvp11973) {
  %envptr13620 = inttoptr i64 %env13242 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13621 = getelementptr inbounds i64, i64* %envptr13620, i64 2                ; &envptr13620[2]
  %X5k$_37foldl1 = load i64, i64* %envptr13621, align 8                              ; load; *envptr13621
  %envptr13622 = inttoptr i64 %env13242 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13623 = getelementptr inbounds i64, i64* %envptr13622, i64 1                ; &envptr13622[1]
  %ecz$_37length = load i64, i64* %envptr13623, align 8                              ; load; *envptr13623
  %_959429 = call i64 @prim_car(i64 %rvp11973)                                       ; call prim_car
  %rvp11972 = call i64 @prim_cdr(i64 %rvp11973)                                      ; call prim_cdr
  %wGw$_37foldl = call i64 @prim_car(i64 %rvp11972)                                  ; call prim_car
  %na11025 = call i64 @prim_cdr(i64 %rvp11972)                                       ; call prim_cdr
  %cloptr13624 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13625 = getelementptr inbounds i64, i64* %cloptr13624, i64 0                  ; &cloptr13624[0]
  %f13626 = ptrtoint void(i64,i64)* @lam13239 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13626, i64* %eptr13625                                                 ; store fptr
  %IA9$_37_62 = ptrtoint i64* %cloptr13624 to i64                                    ; closure cast; i64* -> i64
  %cloptr13627 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13628 = getelementptr inbounds i64, i64* %cloptr13627, i64 0                  ; &cloptr13627[0]
  %f13629 = ptrtoint void(i64,i64)* @lam13235 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13629, i64* %eptr13628                                                 ; store fptr
  %uc0$_37_62_61 = ptrtoint i64* %cloptr13627 to i64                                 ; closure cast; i64* -> i64
  %arg9772 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %arg9771 = add i64 0, 0                                                            ; quoted ()
  %iT3$_37append = call i64 @prim_make_45vector(i64 %arg9772, i64 %arg9771)          ; call prim_make_45vector
  %arg9774 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %arg9773 = add i64 0, 0                                                            ; quoted ()
  %Ioh$_37append2 = call i64 @prim_make_45vector(i64 %arg9774, i64 %arg9773)         ; call prim_make_45vector
  %arg9776 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr13630 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr13632 = getelementptr inbounds i64, i64* %cloptr13630, i64 1                  ; &eptr13632[1]
  store i64 %Ioh$_37append2, i64* %eptr13632                                         ; *eptr13632 = %Ioh$_37append2
  %eptr13631 = getelementptr inbounds i64, i64* %cloptr13630, i64 0                  ; &cloptr13630[0]
  %f13633 = ptrtoint void(i64,i64)* @lam13226 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13633, i64* %eptr13631                                                 ; store fptr
  %arg9775 = ptrtoint i64* %cloptr13630 to i64                                       ; closure cast; i64* -> i64
  %Rvx$_950 = call i64 @prim_vector_45set_33(i64 %Ioh$_37append2, i64 %arg9776, i64 %arg9775); call prim_vector_45set_33
  %arg9796 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr13634 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13636 = getelementptr inbounds i64, i64* %cloptr13634, i64 1                  ; &eptr13636[1]
  %eptr13637 = getelementptr inbounds i64, i64* %cloptr13634, i64 2                  ; &eptr13637[2]
  store i64 %Ioh$_37append2, i64* %eptr13636                                         ; *eptr13636 = %Ioh$_37append2
  store i64 %iT3$_37append, i64* %eptr13637                                          ; *eptr13637 = %iT3$_37append
  %eptr13635 = getelementptr inbounds i64, i64* %cloptr13634, i64 0                  ; &cloptr13634[0]
  %f13638 = ptrtoint void(i64,i64)* @lam13215 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13638, i64* %eptr13635                                                 ; store fptr
  %arg9795 = ptrtoint i64* %cloptr13634 to i64                                       ; closure cast; i64* -> i64
  %a4w$_951 = call i64 @prim_vector_45set_33(i64 %iT3$_37append, i64 %arg9796, i64 %arg9795); call prim_vector_45set_33
  %arg9816 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9604 = call i64 @prim_vector_45ref(i64 %iT3$_37append, i64 %arg9816)       ; call prim_vector_45ref
  %cloptr13639 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13641 = getelementptr inbounds i64, i64* %cloptr13639, i64 1                  ; &eptr13641[1]
  %eptr13642 = getelementptr inbounds i64, i64* %cloptr13639, i64 2                  ; &eptr13642[2]
  %eptr13643 = getelementptr inbounds i64, i64* %cloptr13639, i64 3                  ; &eptr13643[3]
  store i64 %ecz$_37length, i64* %eptr13641                                          ; *eptr13641 = %ecz$_37length
  store i64 %IA9$_37_62, i64* %eptr13642                                             ; *eptr13642 = %IA9$_37_62
  store i64 %X5k$_37foldl1, i64* %eptr13643                                          ; *eptr13643 = %X5k$_37foldl1
  %eptr13640 = getelementptr inbounds i64, i64* %cloptr13639, i64 0                  ; &cloptr13639[0]
  %f13644 = ptrtoint void(i64,i64)* @lam13204 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13644, i64* %eptr13640                                                 ; store fptr
  %arg9820 = ptrtoint i64* %cloptr13639 to i64                                       ; closure cast; i64* -> i64
  %arg9819 = add i64 0, 0                                                            ; quoted ()
  %rva11971 = add i64 0, 0                                                           ; quoted ()
  %rva11970 = call i64 @prim_cons(i64 %retprim9604, i64 %rva11971)                   ; call prim_cons
  %rva11969 = call i64 @prim_cons(i64 %arg9819, i64 %rva11970)                       ; call prim_cons
  %cloptr13645 = inttoptr i64 %arg9820 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13646 = getelementptr inbounds i64, i64* %cloptr13645, i64 0                 ; &cloptr13645[0]
  %f13648 = load i64, i64* %i0ptr13646, align 8                                      ; load; *i0ptr13646
  %fptr13647 = inttoptr i64 %f13648 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13647(i64 %arg9820, i64 %rva11969)                  ; tail call
  ret void
}


define void @lam13239(i64 %env13240, i64 %rvp11033) {
  %cont9430 = call i64 @prim_car(i64 %rvp11033)                                      ; call prim_car
  %rvp11032 = call i64 @prim_cdr(i64 %rvp11033)                                      ; call prim_cdr
  %SRt$a = call i64 @prim_car(i64 %rvp11032)                                         ; call prim_car
  %rvp11031 = call i64 @prim_cdr(i64 %rvp11032)                                      ; call prim_cdr
  %rUt$b = call i64 @prim_car(i64 %rvp11031)                                         ; call prim_car
  %na11027 = call i64 @prim_cdr(i64 %rvp11031)                                       ; call prim_cdr
  %a9271 = call i64 @prim__60_61(i64 %SRt$a, i64 %rUt$b)                             ; call prim__60_61
  %retprim9431 = call i64 @prim_not(i64 %a9271)                                      ; call prim_not
  %arg9763 = add i64 0, 0                                                            ; quoted ()
  %rva11030 = add i64 0, 0                                                           ; quoted ()
  %rva11029 = call i64 @prim_cons(i64 %retprim9431, i64 %rva11030)                   ; call prim_cons
  %rva11028 = call i64 @prim_cons(i64 %arg9763, i64 %rva11029)                       ; call prim_cons
  %cloptr13649 = inttoptr i64 %cont9430 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13650 = getelementptr inbounds i64, i64* %cloptr13649, i64 0                 ; &cloptr13649[0]
  %f13652 = load i64, i64* %i0ptr13650, align 8                                      ; load; *i0ptr13650
  %fptr13651 = inttoptr i64 %f13652 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13651(i64 %cont9430, i64 %rva11028)                 ; tail call
  ret void
}


define void @lam13235(i64 %env13236, i64 %rvp11041) {
  %cont9432 = call i64 @prim_car(i64 %rvp11041)                                      ; call prim_car
  %rvp11040 = call i64 @prim_cdr(i64 %rvp11041)                                      ; call prim_cdr
  %ga0$a = call i64 @prim_car(i64 %rvp11040)                                         ; call prim_car
  %rvp11039 = call i64 @prim_cdr(i64 %rvp11040)                                      ; call prim_cdr
  %w4Q$b = call i64 @prim_car(i64 %rvp11039)                                         ; call prim_car
  %na11035 = call i64 @prim_cdr(i64 %rvp11039)                                       ; call prim_cdr
  %a9272 = call i64 @prim__60(i64 %ga0$a, i64 %w4Q$b)                                ; call prim__60
  %retprim9433 = call i64 @prim_not(i64 %a9272)                                      ; call prim_not
  %arg9769 = add i64 0, 0                                                            ; quoted ()
  %rva11038 = add i64 0, 0                                                           ; quoted ()
  %rva11037 = call i64 @prim_cons(i64 %retprim9433, i64 %rva11038)                   ; call prim_cons
  %rva11036 = call i64 @prim_cons(i64 %arg9769, i64 %rva11037)                       ; call prim_cons
  %cloptr13653 = inttoptr i64 %cont9432 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13654 = getelementptr inbounds i64, i64* %cloptr13653, i64 0                 ; &cloptr13653[0]
  %f13656 = load i64, i64* %i0ptr13654, align 8                                      ; load; *i0ptr13654
  %fptr13655 = inttoptr i64 %f13656 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13655(i64 %cont9432, i64 %rva11036)                 ; tail call
  ret void
}


define void @lam13226(i64 %env13227, i64 %rvp11060) {
  %envptr13657 = inttoptr i64 %env13227 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13658 = getelementptr inbounds i64, i64* %envptr13657, i64 1                ; &envptr13657[1]
  %Ioh$_37append2 = load i64, i64* %envptr13658, align 8                             ; load; *envptr13658
  %cont9597 = call i64 @prim_car(i64 %rvp11060)                                      ; call prim_car
  %rvp11059 = call i64 @prim_cdr(i64 %rvp11060)                                      ; call prim_cdr
  %nqP$ls0 = call i64 @prim_car(i64 %rvp11059)                                       ; call prim_car
  %rvp11058 = call i64 @prim_cdr(i64 %rvp11059)                                      ; call prim_cdr
  %C1C$ls1 = call i64 @prim_car(i64 %rvp11058)                                       ; call prim_car
  %na11043 = call i64 @prim_cdr(i64 %rvp11058)                                       ; call prim_cdr
  %a9273 = call i64 @prim_null_63(i64 %nqP$ls0)                                      ; call prim_null_63
  %cmp13659 = icmp eq i64 %a9273, 15                                                 ; false?
  br i1 %cmp13659, label %else13661, label %then13660                                ; if

then13660:
  %arg9780 = add i64 0, 0                                                            ; quoted ()
  %rva11046 = add i64 0, 0                                                           ; quoted ()
  %rva11045 = call i64 @prim_cons(i64 %C1C$ls1, i64 %rva11046)                       ; call prim_cons
  %rva11044 = call i64 @prim_cons(i64 %arg9780, i64 %rva11045)                       ; call prim_cons
  %cloptr13662 = inttoptr i64 %cont9597 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13663 = getelementptr inbounds i64, i64* %cloptr13662, i64 0                 ; &cloptr13662[0]
  %f13665 = load i64, i64* %i0ptr13663, align 8                                      ; load; *i0ptr13663
  %fptr13664 = inttoptr i64 %f13665 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13664(i64 %cont9597, i64 %rva11044)                 ; tail call
  ret void

else13661:
  %a9274 = call i64 @prim_car(i64 %nqP$ls0)                                          ; call prim_car
  %arg9783 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9275 = call i64 @prim_vector_45ref(i64 %Ioh$_37append2, i64 %arg9783)            ; call prim_vector_45ref
  %a9276 = call i64 @prim_cdr(i64 %nqP$ls0)                                          ; call prim_cdr
  %cloptr13666 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13668 = getelementptr inbounds i64, i64* %cloptr13666, i64 1                  ; &eptr13668[1]
  %eptr13669 = getelementptr inbounds i64, i64* %cloptr13666, i64 2                  ; &eptr13669[2]
  store i64 %cont9597, i64* %eptr13668                                               ; *eptr13668 = %cont9597
  store i64 %a9274, i64* %eptr13669                                                  ; *eptr13669 = %a9274
  %eptr13667 = getelementptr inbounds i64, i64* %cloptr13666, i64 0                  ; &cloptr13666[0]
  %f13670 = ptrtoint void(i64,i64)* @lam13223 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13670, i64* %eptr13667                                                 ; store fptr
  %arg9788 = ptrtoint i64* %cloptr13666 to i64                                       ; closure cast; i64* -> i64
  %rva11057 = add i64 0, 0                                                           ; quoted ()
  %rva11056 = call i64 @prim_cons(i64 %C1C$ls1, i64 %rva11057)                       ; call prim_cons
  %rva11055 = call i64 @prim_cons(i64 %a9276, i64 %rva11056)                         ; call prim_cons
  %rva11054 = call i64 @prim_cons(i64 %arg9788, i64 %rva11055)                       ; call prim_cons
  %cloptr13671 = inttoptr i64 %a9275 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13672 = getelementptr inbounds i64, i64* %cloptr13671, i64 0                 ; &cloptr13671[0]
  %f13674 = load i64, i64* %i0ptr13672, align 8                                      ; load; *i0ptr13672
  %fptr13673 = inttoptr i64 %f13674 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13673(i64 %a9275, i64 %rva11054)                    ; tail call
  ret void
}


define void @lam13223(i64 %env13224, i64 %rvp11053) {
  %envptr13675 = inttoptr i64 %env13224 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13676 = getelementptr inbounds i64, i64* %envptr13675, i64 2                ; &envptr13675[2]
  %a9274 = load i64, i64* %envptr13676, align 8                                      ; load; *envptr13676
  %envptr13677 = inttoptr i64 %env13224 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13678 = getelementptr inbounds i64, i64* %envptr13677, i64 1                ; &envptr13677[1]
  %cont9597 = load i64, i64* %envptr13678, align 8                                   ; load; *envptr13678
  %_959598 = call i64 @prim_car(i64 %rvp11053)                                       ; call prim_car
  %rvp11052 = call i64 @prim_cdr(i64 %rvp11053)                                      ; call prim_cdr
  %a9277 = call i64 @prim_car(i64 %rvp11052)                                         ; call prim_car
  %na11048 = call i64 @prim_cdr(i64 %rvp11052)                                       ; call prim_cdr
  %retprim9599 = call i64 @prim_cons(i64 %a9274, i64 %a9277)                         ; call prim_cons
  %arg9793 = add i64 0, 0                                                            ; quoted ()
  %rva11051 = add i64 0, 0                                                           ; quoted ()
  %rva11050 = call i64 @prim_cons(i64 %retprim9599, i64 %rva11051)                   ; call prim_cons
  %rva11049 = call i64 @prim_cons(i64 %arg9793, i64 %rva11050)                       ; call prim_cons
  %cloptr13679 = inttoptr i64 %cont9597 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13680 = getelementptr inbounds i64, i64* %cloptr13679, i64 0                 ; &cloptr13679[0]
  %f13682 = load i64, i64* %i0ptr13680, align 8                                      ; load; *i0ptr13680
  %fptr13681 = inttoptr i64 %f13682 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13681(i64 %cont9597, i64 %rva11049)                 ; tail call
  ret void
}


define void @lam13215(i64 %env13216, i64 %Prp$xs9601) {
  %envptr13683 = inttoptr i64 %env13216 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13684 = getelementptr inbounds i64, i64* %envptr13683, i64 2                ; &envptr13683[2]
  %iT3$_37append = load i64, i64* %envptr13684, align 8                              ; load; *envptr13684
  %envptr13685 = inttoptr i64 %env13216 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13686 = getelementptr inbounds i64, i64* %envptr13685, i64 1                ; &envptr13685[1]
  %Ioh$_37append2 = load i64, i64* %envptr13686, align 8                             ; load; *envptr13686
  %cont9600 = call i64 @prim_car(i64 %Prp$xs9601)                                    ; call prim_car
  %Prp$xs = call i64 @prim_cdr(i64 %Prp$xs9601)                                      ; call prim_cdr
  %a9278 = call i64 @prim_null_63(i64 %Prp$xs)                                       ; call prim_null_63
  %cmp13687 = icmp eq i64 %a9278, 15                                                 ; false?
  br i1 %cmp13687, label %else13689, label %then13688                                ; if

then13688:
  %arg9802 = add i64 0, 0                                                            ; quoted ()
  %arg9801 = add i64 0, 0                                                            ; quoted ()
  %rva11063 = add i64 0, 0                                                           ; quoted ()
  %rva11062 = call i64 @prim_cons(i64 %arg9801, i64 %rva11063)                       ; call prim_cons
  %rva11061 = call i64 @prim_cons(i64 %arg9802, i64 %rva11062)                       ; call prim_cons
  %cloptr13690 = inttoptr i64 %cont9600 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13691 = getelementptr inbounds i64, i64* %cloptr13690, i64 0                 ; &cloptr13690[0]
  %f13693 = load i64, i64* %i0ptr13691, align 8                                      ; load; *i0ptr13691
  %fptr13692 = inttoptr i64 %f13693 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13692(i64 %cont9600, i64 %rva11061)                 ; tail call
  ret void

else13689:
  %vve$hd = call i64 @prim_car(i64 %Prp$xs)                                          ; call prim_car
  %fM9$tl = call i64 @prim_cdr(i64 %Prp$xs)                                          ; call prim_cdr
  %arg9806 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9279 = call i64 @prim_vector_45ref(i64 %iT3$_37append, i64 %arg9806)             ; call prim_vector_45ref
  %cloptr13694 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13696 = getelementptr inbounds i64, i64* %cloptr13694, i64 1                  ; &eptr13696[1]
  %eptr13697 = getelementptr inbounds i64, i64* %cloptr13694, i64 2                  ; &eptr13697[2]
  %eptr13698 = getelementptr inbounds i64, i64* %cloptr13694, i64 3                  ; &eptr13698[3]
  store i64 %Ioh$_37append2, i64* %eptr13696                                         ; *eptr13696 = %Ioh$_37append2
  store i64 %cont9600, i64* %eptr13697                                               ; *eptr13697 = %cont9600
  store i64 %vve$hd, i64* %eptr13698                                                 ; *eptr13698 = %vve$hd
  %eptr13695 = getelementptr inbounds i64, i64* %cloptr13694, i64 0                  ; &cloptr13694[0]
  %f13699 = ptrtoint void(i64,i64)* @lam13212 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13699, i64* %eptr13695                                                 ; store fptr
  %arg9809 = ptrtoint i64* %cloptr13694 to i64                                       ; closure cast; i64* -> i64
  %cps_45lst9603 = call i64 @prim_cons(i64 %arg9809, i64 %fM9$tl)                    ; call prim_cons
  %cloptr13700 = inttoptr i64 %a9279 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13701 = getelementptr inbounds i64, i64* %cloptr13700, i64 0                 ; &cloptr13700[0]
  %f13703 = load i64, i64* %i0ptr13701, align 8                                      ; load; *i0ptr13701
  %fptr13702 = inttoptr i64 %f13703 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13702(i64 %a9279, i64 %cps_45lst9603)               ; tail call
  ret void
}


define void @lam13212(i64 %env13213, i64 %rvp11071) {
  %envptr13704 = inttoptr i64 %env13213 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13705 = getelementptr inbounds i64, i64* %envptr13704, i64 3                ; &envptr13704[3]
  %vve$hd = load i64, i64* %envptr13705, align 8                                     ; load; *envptr13705
  %envptr13706 = inttoptr i64 %env13213 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13707 = getelementptr inbounds i64, i64* %envptr13706, i64 2                ; &envptr13706[2]
  %cont9600 = load i64, i64* %envptr13707, align 8                                   ; load; *envptr13707
  %envptr13708 = inttoptr i64 %env13213 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13709 = getelementptr inbounds i64, i64* %envptr13708, i64 1                ; &envptr13708[1]
  %Ioh$_37append2 = load i64, i64* %envptr13709, align 8                             ; load; *envptr13709
  %_959602 = call i64 @prim_car(i64 %rvp11071)                                       ; call prim_car
  %rvp11070 = call i64 @prim_cdr(i64 %rvp11071)                                      ; call prim_cdr
  %AXW$result1 = call i64 @prim_car(i64 %rvp11070)                                   ; call prim_car
  %na11065 = call i64 @prim_cdr(i64 %rvp11070)                                       ; call prim_cdr
  %arg9810 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9280 = call i64 @prim_vector_45ref(i64 %Ioh$_37append2, i64 %arg9810)            ; call prim_vector_45ref
  %rva11069 = add i64 0, 0                                                           ; quoted ()
  %rva11068 = call i64 @prim_cons(i64 %AXW$result1, i64 %rva11069)                   ; call prim_cons
  %rva11067 = call i64 @prim_cons(i64 %vve$hd, i64 %rva11068)                        ; call prim_cons
  %rva11066 = call i64 @prim_cons(i64 %cont9600, i64 %rva11067)                      ; call prim_cons
  %cloptr13710 = inttoptr i64 %a9280 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13711 = getelementptr inbounds i64, i64* %cloptr13710, i64 0                 ; &cloptr13710[0]
  %f13713 = load i64, i64* %i0ptr13711, align 8                                      ; load; *i0ptr13711
  %fptr13712 = inttoptr i64 %f13713 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13712(i64 %a9280, i64 %rva11066)                    ; tail call
  ret void
}


define void @lam13204(i64 %env13205, i64 %rvp11968) {
  %envptr13714 = inttoptr i64 %env13205 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13715 = getelementptr inbounds i64, i64* %envptr13714, i64 3                ; &envptr13714[3]
  %X5k$_37foldl1 = load i64, i64* %envptr13715, align 8                              ; load; *envptr13715
  %envptr13716 = inttoptr i64 %env13205 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13717 = getelementptr inbounds i64, i64* %envptr13716, i64 2                ; &envptr13716[2]
  %IA9$_37_62 = load i64, i64* %envptr13717, align 8                                 ; load; *envptr13717
  %envptr13718 = inttoptr i64 %env13205 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13719 = getelementptr inbounds i64, i64* %envptr13718, i64 1                ; &envptr13718[1]
  %ecz$_37length = load i64, i64* %envptr13719, align 8                              ; load; *envptr13719
  %_959434 = call i64 @prim_car(i64 %rvp11968)                                       ; call prim_car
  %rvp11967 = call i64 @prim_cdr(i64 %rvp11968)                                      ; call prim_cdr
  %RMn$_37append = call i64 @prim_car(i64 %rvp11967)                                 ; call prim_car
  %na11073 = call i64 @prim_cdr(i64 %rvp11967)                                       ; call prim_cdr
  %cloptr13720 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13721 = getelementptr inbounds i64, i64* %cloptr13720, i64 0                  ; &cloptr13720[0]
  %f13722 = ptrtoint void(i64,i64)* @lam13202 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13722, i64* %eptr13721                                                 ; store fptr
  %kTA$_37list_63 = ptrtoint i64* %cloptr13720 to i64                                ; closure cast; i64* -> i64
  %cloptr13723 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13724 = getelementptr inbounds i64, i64* %cloptr13723, i64 0                  ; &cloptr13723[0]
  %f13725 = ptrtoint void(i64,i64)* @lam13150 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13725, i64* %eptr13724                                                 ; store fptr
  %ILf$_37drop = ptrtoint i64* %cloptr13723 to i64                                   ; closure cast; i64* -> i64
  %cloptr13726 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13727 = getelementptr inbounds i64, i64* %cloptr13726, i64 0                  ; &cloptr13726[0]
  %f13728 = ptrtoint void(i64,i64)* @lam13100 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13728, i64* %eptr13727                                                 ; store fptr
  %h0n$_37memv = ptrtoint i64* %cloptr13726 to i64                                   ; closure cast; i64* -> i64
  %cloptr13729 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr13731 = getelementptr inbounds i64, i64* %cloptr13729, i64 1                  ; &eptr13731[1]
  store i64 %X5k$_37foldl1, i64* %eptr13731                                          ; *eptr13731 = %X5k$_37foldl1
  %eptr13730 = getelementptr inbounds i64, i64* %cloptr13729, i64 0                  ; &cloptr13729[0]
  %f13732 = ptrtoint void(i64,i64)* @lam13059 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13732, i64* %eptr13730                                                 ; store fptr
  %Nne$_37_47 = ptrtoint i64* %cloptr13729 to i64                                    ; closure cast; i64* -> i64
  %cloptr13733 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13734 = getelementptr inbounds i64, i64* %cloptr13733, i64 0                  ; &cloptr13733[0]
  %f13735 = ptrtoint void(i64,i64)* @lam13047 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13735, i64* %eptr13734                                                 ; store fptr
  %fuW$_37first = ptrtoint i64* %cloptr13733 to i64                                  ; closure cast; i64* -> i64
  %cloptr13736 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13737 = getelementptr inbounds i64, i64* %cloptr13736, i64 0                  ; &cloptr13736[0]
  %f13738 = ptrtoint void(i64,i64)* @lam13043 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13738, i64* %eptr13737                                                 ; store fptr
  %TSh$_37second = ptrtoint i64* %cloptr13736 to i64                                 ; closure cast; i64* -> i64
  %cloptr13739 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13740 = getelementptr inbounds i64, i64* %cloptr13739, i64 0                  ; &cloptr13739[0]
  %f13741 = ptrtoint void(i64,i64)* @lam13039 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13741, i64* %eptr13740                                                 ; store fptr
  %toh$_37third = ptrtoint i64* %cloptr13739 to i64                                  ; closure cast; i64* -> i64
  %cloptr13742 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13743 = getelementptr inbounds i64, i64* %cloptr13742, i64 0                  ; &cloptr13742[0]
  %f13744 = ptrtoint void(i64,i64)* @lam13035 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13744, i64* %eptr13743                                                 ; store fptr
  %ZXQ$_37fourth = ptrtoint i64* %cloptr13742 to i64                                 ; closure cast; i64* -> i64
  %cloptr13745 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13746 = getelementptr inbounds i64, i64* %cloptr13745, i64 0                  ; &cloptr13745[0]
  %f13747 = ptrtoint void(i64,i64)* @lam13031 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13747, i64* %eptr13746                                                 ; store fptr
  %QJS$promise_63 = ptrtoint i64* %cloptr13745 to i64                                ; closure cast; i64* -> i64
  %cloptr13748 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13749 = getelementptr inbounds i64, i64* %cloptr13748, i64 0                  ; &cloptr13748[0]
  %f13750 = ptrtoint void(i64,i64)* @lam13023 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13750, i64* %eptr13749                                                 ; store fptr
  %arg10082 = ptrtoint i64* %cloptr13748 to i64                                      ; closure cast; i64* -> i64
  %cloptr13751 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13753 = getelementptr inbounds i64, i64* %cloptr13751, i64 1                  ; &eptr13753[1]
  %eptr13754 = getelementptr inbounds i64, i64* %cloptr13751, i64 2                  ; &eptr13754[2]
  %eptr13755 = getelementptr inbounds i64, i64* %cloptr13751, i64 3                  ; &eptr13755[3]
  store i64 %ecz$_37length, i64* %eptr13753                                          ; *eptr13753 = %ecz$_37length
  store i64 %IA9$_37_62, i64* %eptr13754                                             ; *eptr13754 = %IA9$_37_62
  store i64 %ILf$_37drop, i64* %eptr13755                                            ; *eptr13755 = %ILf$_37drop
  %eptr13752 = getelementptr inbounds i64, i64* %cloptr13751, i64 0                  ; &cloptr13751[0]
  %f13756 = ptrtoint void(i64,i64)* @lam13019 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13756, i64* %eptr13752                                                 ; store fptr
  %arg10081 = ptrtoint i64* %cloptr13751 to i64                                      ; closure cast; i64* -> i64
  %rva11966 = add i64 0, 0                                                           ; quoted ()
  %rva11965 = call i64 @prim_cons(i64 %arg10081, i64 %rva11966)                      ; call prim_cons
  %cloptr13757 = inttoptr i64 %arg10082 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13758 = getelementptr inbounds i64, i64* %cloptr13757, i64 0                 ; &cloptr13757[0]
  %f13760 = load i64, i64* %i0ptr13758, align 8                                      ; load; *i0ptr13758
  %fptr13759 = inttoptr i64 %f13760 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13759(i64 %arg10082, i64 %rva11965)                 ; tail call
  ret void
}


define void @lam13202(i64 %env13203, i64 %rvp11141) {
  %cont9435 = call i64 @prim_car(i64 %rvp11141)                                      ; call prim_car
  %rvp11140 = call i64 @prim_cdr(i64 %rvp11141)                                      ; call prim_cdr
  %voz$a = call i64 @prim_car(i64 %rvp11140)                                         ; call prim_car
  %na11075 = call i64 @prim_cdr(i64 %rvp11140)                                       ; call prim_cdr
  %arg9822 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %sGp$a = call i64 @prim_make_45vector(i64 %arg9822, i64 %voz$a)                    ; call prim_make_45vector
  %cloptr13761 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13762 = getelementptr inbounds i64, i64* %cloptr13761, i64 0                  ; &cloptr13761[0]
  %f13763 = ptrtoint void(i64,i64)* @lam13199 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13763, i64* %eptr13762                                                 ; store fptr
  %arg9825 = ptrtoint i64* %cloptr13761 to i64                                       ; closure cast; i64* -> i64
  %cloptr13764 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13766 = getelementptr inbounds i64, i64* %cloptr13764, i64 1                  ; &eptr13766[1]
  %eptr13767 = getelementptr inbounds i64, i64* %cloptr13764, i64 2                  ; &eptr13767[2]
  store i64 %sGp$a, i64* %eptr13766                                                  ; *eptr13766 = %sGp$a
  store i64 %cont9435, i64* %eptr13767                                               ; *eptr13767 = %cont9435
  %eptr13765 = getelementptr inbounds i64, i64* %cloptr13764, i64 0                  ; &cloptr13764[0]
  %f13768 = ptrtoint void(i64,i64)* @lam13195 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13768, i64* %eptr13765                                                 ; store fptr
  %arg9824 = ptrtoint i64* %cloptr13764 to i64                                       ; closure cast; i64* -> i64
  %cloptr13769 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13771 = getelementptr inbounds i64, i64* %cloptr13769, i64 1                  ; &eptr13771[1]
  %eptr13772 = getelementptr inbounds i64, i64* %cloptr13769, i64 2                  ; &eptr13772[2]
  store i64 %sGp$a, i64* %eptr13771                                                  ; *eptr13771 = %sGp$a
  store i64 %cont9435, i64* %eptr13772                                               ; *eptr13772 = %cont9435
  %eptr13770 = getelementptr inbounds i64, i64* %cloptr13769, i64 0                  ; &cloptr13769[0]
  %f13773 = ptrtoint void(i64,i64)* @lam13173 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13773, i64* %eptr13770                                                 ; store fptr
  %arg9823 = ptrtoint i64* %cloptr13769 to i64                                       ; closure cast; i64* -> i64
  %rva11139 = add i64 0, 0                                                           ; quoted ()
  %rva11138 = call i64 @prim_cons(i64 %arg9823, i64 %rva11139)                       ; call prim_cons
  %rva11137 = call i64 @prim_cons(i64 %arg9824, i64 %rva11138)                       ; call prim_cons
  %cloptr13774 = inttoptr i64 %arg9825 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13775 = getelementptr inbounds i64, i64* %cloptr13774, i64 0                 ; &cloptr13774[0]
  %f13777 = load i64, i64* %i0ptr13775, align 8                                      ; load; *i0ptr13775
  %fptr13776 = inttoptr i64 %f13777 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13776(i64 %arg9825, i64 %rva11137)                  ; tail call
  ret void
}


define void @lam13199(i64 %env13200, i64 %rvp11082) {
  %cont9441 = call i64 @prim_car(i64 %rvp11082)                                      ; call prim_car
  %rvp11081 = call i64 @prim_cdr(i64 %rvp11082)                                      ; call prim_cdr
  %tTr$k = call i64 @prim_car(i64 %rvp11081)                                         ; call prim_car
  %na11077 = call i64 @prim_cdr(i64 %rvp11081)                                       ; call prim_cdr
  %arg9827 = add i64 0, 0                                                            ; quoted ()
  %rva11080 = add i64 0, 0                                                           ; quoted ()
  %rva11079 = call i64 @prim_cons(i64 %tTr$k, i64 %rva11080)                         ; call prim_cons
  %rva11078 = call i64 @prim_cons(i64 %arg9827, i64 %rva11079)                       ; call prim_cons
  %cloptr13778 = inttoptr i64 %cont9441 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13779 = getelementptr inbounds i64, i64* %cloptr13778, i64 0                 ; &cloptr13778[0]
  %f13781 = load i64, i64* %i0ptr13779, align 8                                      ; load; *i0ptr13779
  %fptr13780 = inttoptr i64 %f13781 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13780(i64 %cont9441, i64 %rva11078)                 ; tail call
  ret void
}


define void @lam13195(i64 %env13196, i64 %rvp11109) {
  %envptr13782 = inttoptr i64 %env13196 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13783 = getelementptr inbounds i64, i64* %envptr13782, i64 2                ; &envptr13782[2]
  %cont9435 = load i64, i64* %envptr13783, align 8                                   ; load; *envptr13783
  %envptr13784 = inttoptr i64 %env13196 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13785 = getelementptr inbounds i64, i64* %envptr13784, i64 1                ; &envptr13784[1]
  %sGp$a = load i64, i64* %envptr13785, align 8                                      ; load; *envptr13785
  %_959436 = call i64 @prim_car(i64 %rvp11109)                                       ; call prim_car
  %rvp11108 = call i64 @prim_cdr(i64 %rvp11109)                                      ; call prim_cdr
  %dJc$cc = call i64 @prim_car(i64 %rvp11108)                                        ; call prim_car
  %na11084 = call i64 @prim_cdr(i64 %rvp11108)                                       ; call prim_cdr
  %arg9829 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9281 = call i64 @prim_vector_45ref(i64 %sGp$a, i64 %arg9829)                     ; call prim_vector_45ref
  %a9282 = call i64 @prim_null_63(i64 %a9281)                                        ; call prim_null_63
  %cmp13786 = icmp eq i64 %a9282, 15                                                 ; false?
  br i1 %cmp13786, label %else13788, label %then13787                                ; if

then13787:
  %arg9833 = add i64 0, 0                                                            ; quoted ()
  %arg9832 = call i64 @const_init_true()                                             ; quoted #t
  %rva11087 = add i64 0, 0                                                           ; quoted ()
  %rva11086 = call i64 @prim_cons(i64 %arg9832, i64 %rva11087)                       ; call prim_cons
  %rva11085 = call i64 @prim_cons(i64 %arg9833, i64 %rva11086)                       ; call prim_cons
  %cloptr13789 = inttoptr i64 %cont9435 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13790 = getelementptr inbounds i64, i64* %cloptr13789, i64 0                 ; &cloptr13789[0]
  %f13792 = load i64, i64* %i0ptr13790, align 8                                      ; load; *i0ptr13790
  %fptr13791 = inttoptr i64 %f13792 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13791(i64 %cont9435, i64 %rva11085)                 ; tail call
  ret void

else13788:
  %arg9835 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9283 = call i64 @prim_vector_45ref(i64 %sGp$a, i64 %arg9835)                     ; call prim_vector_45ref
  %a9284 = call i64 @prim_cons_63(i64 %a9283)                                        ; call prim_cons_63
  %cmp13793 = icmp eq i64 %a9284, 15                                                 ; false?
  br i1 %cmp13793, label %else13795, label %then13794                                ; if

then13794:
  %arg9838 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9285 = call i64 @prim_vector_45ref(i64 %sGp$a, i64 %arg9838)                     ; call prim_vector_45ref
  %retprim9440 = call i64 @prim_cdr(i64 %a9285)                                      ; call prim_cdr
  %cloptr13796 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13798 = getelementptr inbounds i64, i64* %cloptr13796, i64 1                  ; &eptr13798[1]
  %eptr13799 = getelementptr inbounds i64, i64* %cloptr13796, i64 2                  ; &eptr13799[2]
  %eptr13800 = getelementptr inbounds i64, i64* %cloptr13796, i64 3                  ; &eptr13800[3]
  store i64 %sGp$a, i64* %eptr13798                                                  ; *eptr13798 = %sGp$a
  store i64 %dJc$cc, i64* %eptr13799                                                 ; *eptr13799 = %dJc$cc
  store i64 %cont9435, i64* %eptr13800                                               ; *eptr13800 = %cont9435
  %eptr13797 = getelementptr inbounds i64, i64* %cloptr13796, i64 0                  ; &cloptr13796[0]
  %f13801 = ptrtoint void(i64,i64)* @lam13187 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13801, i64* %eptr13797                                                 ; store fptr
  %arg9843 = ptrtoint i64* %cloptr13796 to i64                                       ; closure cast; i64* -> i64
  %arg9842 = add i64 0, 0                                                            ; quoted ()
  %rva11104 = add i64 0, 0                                                           ; quoted ()
  %rva11103 = call i64 @prim_cons(i64 %retprim9440, i64 %rva11104)                   ; call prim_cons
  %rva11102 = call i64 @prim_cons(i64 %arg9842, i64 %rva11103)                       ; call prim_cons
  %cloptr13802 = inttoptr i64 %arg9843 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13803 = getelementptr inbounds i64, i64* %cloptr13802, i64 0                 ; &cloptr13802[0]
  %f13805 = load i64, i64* %i0ptr13803, align 8                                      ; load; *i0ptr13803
  %fptr13804 = inttoptr i64 %f13805 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13804(i64 %arg9843, i64 %rva11102)                  ; tail call
  ret void

else13795:
  %arg9857 = add i64 0, 0                                                            ; quoted ()
  %arg9856 = call i64 @const_init_false()                                            ; quoted #f
  %rva11107 = add i64 0, 0                                                           ; quoted ()
  %rva11106 = call i64 @prim_cons(i64 %arg9856, i64 %rva11107)                       ; call prim_cons
  %rva11105 = call i64 @prim_cons(i64 %arg9857, i64 %rva11106)                       ; call prim_cons
  %cloptr13806 = inttoptr i64 %cont9435 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13807 = getelementptr inbounds i64, i64* %cloptr13806, i64 0                 ; &cloptr13806[0]
  %f13809 = load i64, i64* %i0ptr13807, align 8                                      ; load; *i0ptr13807
  %fptr13808 = inttoptr i64 %f13809 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13808(i64 %cont9435, i64 %rva11105)                 ; tail call
  ret void
}


define void @lam13187(i64 %env13188, i64 %rvp11101) {
  %envptr13810 = inttoptr i64 %env13188 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13811 = getelementptr inbounds i64, i64* %envptr13810, i64 3                ; &envptr13810[3]
  %cont9435 = load i64, i64* %envptr13811, align 8                                   ; load; *envptr13811
  %envptr13812 = inttoptr i64 %env13188 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13813 = getelementptr inbounds i64, i64* %envptr13812, i64 2                ; &envptr13812[2]
  %dJc$cc = load i64, i64* %envptr13813, align 8                                     ; load; *envptr13813
  %envptr13814 = inttoptr i64 %env13188 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13815 = getelementptr inbounds i64, i64* %envptr13814, i64 1                ; &envptr13814[1]
  %sGp$a = load i64, i64* %envptr13815, align 8                                      ; load; *envptr13815
  %_959437 = call i64 @prim_car(i64 %rvp11101)                                       ; call prim_car
  %rvp11100 = call i64 @prim_cdr(i64 %rvp11101)                                      ; call prim_cdr
  %DUd$b = call i64 @prim_car(i64 %rvp11100)                                         ; call prim_car
  %na11089 = call i64 @prim_cdr(i64 %rvp11100)                                       ; call prim_cdr
  %arg9844 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9286 = call i64 @prim_vector_45ref(i64 %sGp$a, i64 %arg9844)                     ; call prim_vector_45ref
  %a9287 = call i64 @prim_cdr(i64 %a9286)                                            ; call prim_cdr
  %arg9848 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9439 = call i64 @prim_vector_45set_33(i64 %sGp$a, i64 %arg9848, i64 %a9287); call prim_vector_45set_33
  %cloptr13816 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13818 = getelementptr inbounds i64, i64* %cloptr13816, i64 1                  ; &eptr13818[1]
  %eptr13819 = getelementptr inbounds i64, i64* %cloptr13816, i64 2                  ; &eptr13819[2]
  store i64 %dJc$cc, i64* %eptr13818                                                 ; *eptr13818 = %dJc$cc
  store i64 %cont9435, i64* %eptr13819                                               ; *eptr13819 = %cont9435
  %eptr13817 = getelementptr inbounds i64, i64* %cloptr13816, i64 0                  ; &cloptr13816[0]
  %f13820 = ptrtoint void(i64,i64)* @lam13183 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13820, i64* %eptr13817                                                 ; store fptr
  %arg9852 = ptrtoint i64* %cloptr13816 to i64                                       ; closure cast; i64* -> i64
  %arg9851 = add i64 0, 0                                                            ; quoted ()
  %rva11099 = add i64 0, 0                                                           ; quoted ()
  %rva11098 = call i64 @prim_cons(i64 %retprim9439, i64 %rva11099)                   ; call prim_cons
  %rva11097 = call i64 @prim_cons(i64 %arg9851, i64 %rva11098)                       ; call prim_cons
  %cloptr13821 = inttoptr i64 %arg9852 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13822 = getelementptr inbounds i64, i64* %cloptr13821, i64 0                 ; &cloptr13821[0]
  %f13824 = load i64, i64* %i0ptr13822, align 8                                      ; load; *i0ptr13822
  %fptr13823 = inttoptr i64 %f13824 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13823(i64 %arg9852, i64 %rva11097)                  ; tail call
  ret void
}


define void @lam13183(i64 %env13184, i64 %rvp11096) {
  %envptr13825 = inttoptr i64 %env13184 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13826 = getelementptr inbounds i64, i64* %envptr13825, i64 2                ; &envptr13825[2]
  %cont9435 = load i64, i64* %envptr13826, align 8                                   ; load; *envptr13826
  %envptr13827 = inttoptr i64 %env13184 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13828 = getelementptr inbounds i64, i64* %envptr13827, i64 1                ; &envptr13827[1]
  %dJc$cc = load i64, i64* %envptr13828, align 8                                     ; load; *envptr13828
  %_959438 = call i64 @prim_car(i64 %rvp11096)                                       ; call prim_car
  %rvp11095 = call i64 @prim_cdr(i64 %rvp11096)                                      ; call prim_cdr
  %gpP$_950 = call i64 @prim_car(i64 %rvp11095)                                      ; call prim_car
  %na11091 = call i64 @prim_cdr(i64 %rvp11095)                                       ; call prim_cdr
  %rva11094 = add i64 0, 0                                                           ; quoted ()
  %rva11093 = call i64 @prim_cons(i64 %dJc$cc, i64 %rva11094)                        ; call prim_cons
  %rva11092 = call i64 @prim_cons(i64 %cont9435, i64 %rva11093)                      ; call prim_cons
  %cloptr13829 = inttoptr i64 %dJc$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr13830 = getelementptr inbounds i64, i64* %cloptr13829, i64 0                 ; &cloptr13829[0]
  %f13832 = load i64, i64* %i0ptr13830, align 8                                      ; load; *i0ptr13830
  %fptr13831 = inttoptr i64 %f13832 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13831(i64 %dJc$cc, i64 %rva11092)                   ; tail call
  ret void
}


define void @lam13173(i64 %env13174, i64 %rvp11136) {
  %envptr13833 = inttoptr i64 %env13174 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13834 = getelementptr inbounds i64, i64* %envptr13833, i64 2                ; &envptr13833[2]
  %cont9435 = load i64, i64* %envptr13834, align 8                                   ; load; *envptr13834
  %envptr13835 = inttoptr i64 %env13174 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13836 = getelementptr inbounds i64, i64* %envptr13835, i64 1                ; &envptr13835[1]
  %sGp$a = load i64, i64* %envptr13836, align 8                                      ; load; *envptr13836
  %_959436 = call i64 @prim_car(i64 %rvp11136)                                       ; call prim_car
  %rvp11135 = call i64 @prim_cdr(i64 %rvp11136)                                      ; call prim_cdr
  %dJc$cc = call i64 @prim_car(i64 %rvp11135)                                        ; call prim_car
  %na11111 = call i64 @prim_cdr(i64 %rvp11135)                                       ; call prim_cdr
  %arg9859 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9281 = call i64 @prim_vector_45ref(i64 %sGp$a, i64 %arg9859)                     ; call prim_vector_45ref
  %a9282 = call i64 @prim_null_63(i64 %a9281)                                        ; call prim_null_63
  %cmp13837 = icmp eq i64 %a9282, 15                                                 ; false?
  br i1 %cmp13837, label %else13839, label %then13838                                ; if

then13838:
  %arg9863 = add i64 0, 0                                                            ; quoted ()
  %arg9862 = call i64 @const_init_true()                                             ; quoted #t
  %rva11114 = add i64 0, 0                                                           ; quoted ()
  %rva11113 = call i64 @prim_cons(i64 %arg9862, i64 %rva11114)                       ; call prim_cons
  %rva11112 = call i64 @prim_cons(i64 %arg9863, i64 %rva11113)                       ; call prim_cons
  %cloptr13840 = inttoptr i64 %cont9435 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13841 = getelementptr inbounds i64, i64* %cloptr13840, i64 0                 ; &cloptr13840[0]
  %f13843 = load i64, i64* %i0ptr13841, align 8                                      ; load; *i0ptr13841
  %fptr13842 = inttoptr i64 %f13843 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13842(i64 %cont9435, i64 %rva11112)                 ; tail call
  ret void

else13839:
  %arg9865 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9283 = call i64 @prim_vector_45ref(i64 %sGp$a, i64 %arg9865)                     ; call prim_vector_45ref
  %a9284 = call i64 @prim_cons_63(i64 %a9283)                                        ; call prim_cons_63
  %cmp13844 = icmp eq i64 %a9284, 15                                                 ; false?
  br i1 %cmp13844, label %else13846, label %then13845                                ; if

then13845:
  %arg9868 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9285 = call i64 @prim_vector_45ref(i64 %sGp$a, i64 %arg9868)                     ; call prim_vector_45ref
  %retprim9440 = call i64 @prim_cdr(i64 %a9285)                                      ; call prim_cdr
  %cloptr13847 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13849 = getelementptr inbounds i64, i64* %cloptr13847, i64 1                  ; &eptr13849[1]
  %eptr13850 = getelementptr inbounds i64, i64* %cloptr13847, i64 2                  ; &eptr13850[2]
  %eptr13851 = getelementptr inbounds i64, i64* %cloptr13847, i64 3                  ; &eptr13851[3]
  store i64 %sGp$a, i64* %eptr13849                                                  ; *eptr13849 = %sGp$a
  store i64 %dJc$cc, i64* %eptr13850                                                 ; *eptr13850 = %dJc$cc
  store i64 %cont9435, i64* %eptr13851                                               ; *eptr13851 = %cont9435
  %eptr13848 = getelementptr inbounds i64, i64* %cloptr13847, i64 0                  ; &cloptr13847[0]
  %f13852 = ptrtoint void(i64,i64)* @lam13165 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13852, i64* %eptr13848                                                 ; store fptr
  %arg9873 = ptrtoint i64* %cloptr13847 to i64                                       ; closure cast; i64* -> i64
  %arg9872 = add i64 0, 0                                                            ; quoted ()
  %rva11131 = add i64 0, 0                                                           ; quoted ()
  %rva11130 = call i64 @prim_cons(i64 %retprim9440, i64 %rva11131)                   ; call prim_cons
  %rva11129 = call i64 @prim_cons(i64 %arg9872, i64 %rva11130)                       ; call prim_cons
  %cloptr13853 = inttoptr i64 %arg9873 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13854 = getelementptr inbounds i64, i64* %cloptr13853, i64 0                 ; &cloptr13853[0]
  %f13856 = load i64, i64* %i0ptr13854, align 8                                      ; load; *i0ptr13854
  %fptr13855 = inttoptr i64 %f13856 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13855(i64 %arg9873, i64 %rva11129)                  ; tail call
  ret void

else13846:
  %arg9887 = add i64 0, 0                                                            ; quoted ()
  %arg9886 = call i64 @const_init_false()                                            ; quoted #f
  %rva11134 = add i64 0, 0                                                           ; quoted ()
  %rva11133 = call i64 @prim_cons(i64 %arg9886, i64 %rva11134)                       ; call prim_cons
  %rva11132 = call i64 @prim_cons(i64 %arg9887, i64 %rva11133)                       ; call prim_cons
  %cloptr13857 = inttoptr i64 %cont9435 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13858 = getelementptr inbounds i64, i64* %cloptr13857, i64 0                 ; &cloptr13857[0]
  %f13860 = load i64, i64* %i0ptr13858, align 8                                      ; load; *i0ptr13858
  %fptr13859 = inttoptr i64 %f13860 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13859(i64 %cont9435, i64 %rva11132)                 ; tail call
  ret void
}


define void @lam13165(i64 %env13166, i64 %rvp11128) {
  %envptr13861 = inttoptr i64 %env13166 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13862 = getelementptr inbounds i64, i64* %envptr13861, i64 3                ; &envptr13861[3]
  %cont9435 = load i64, i64* %envptr13862, align 8                                   ; load; *envptr13862
  %envptr13863 = inttoptr i64 %env13166 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13864 = getelementptr inbounds i64, i64* %envptr13863, i64 2                ; &envptr13863[2]
  %dJc$cc = load i64, i64* %envptr13864, align 8                                     ; load; *envptr13864
  %envptr13865 = inttoptr i64 %env13166 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13866 = getelementptr inbounds i64, i64* %envptr13865, i64 1                ; &envptr13865[1]
  %sGp$a = load i64, i64* %envptr13866, align 8                                      ; load; *envptr13866
  %_959437 = call i64 @prim_car(i64 %rvp11128)                                       ; call prim_car
  %rvp11127 = call i64 @prim_cdr(i64 %rvp11128)                                      ; call prim_cdr
  %DUd$b = call i64 @prim_car(i64 %rvp11127)                                         ; call prim_car
  %na11116 = call i64 @prim_cdr(i64 %rvp11127)                                       ; call prim_cdr
  %arg9874 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9286 = call i64 @prim_vector_45ref(i64 %sGp$a, i64 %arg9874)                     ; call prim_vector_45ref
  %a9287 = call i64 @prim_cdr(i64 %a9286)                                            ; call prim_cdr
  %arg9878 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9439 = call i64 @prim_vector_45set_33(i64 %sGp$a, i64 %arg9878, i64 %a9287); call prim_vector_45set_33
  %cloptr13867 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13869 = getelementptr inbounds i64, i64* %cloptr13867, i64 1                  ; &eptr13869[1]
  %eptr13870 = getelementptr inbounds i64, i64* %cloptr13867, i64 2                  ; &eptr13870[2]
  store i64 %dJc$cc, i64* %eptr13869                                                 ; *eptr13869 = %dJc$cc
  store i64 %cont9435, i64* %eptr13870                                               ; *eptr13870 = %cont9435
  %eptr13868 = getelementptr inbounds i64, i64* %cloptr13867, i64 0                  ; &cloptr13867[0]
  %f13871 = ptrtoint void(i64,i64)* @lam13161 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13871, i64* %eptr13868                                                 ; store fptr
  %arg9882 = ptrtoint i64* %cloptr13867 to i64                                       ; closure cast; i64* -> i64
  %arg9881 = add i64 0, 0                                                            ; quoted ()
  %rva11126 = add i64 0, 0                                                           ; quoted ()
  %rva11125 = call i64 @prim_cons(i64 %retprim9439, i64 %rva11126)                   ; call prim_cons
  %rva11124 = call i64 @prim_cons(i64 %arg9881, i64 %rva11125)                       ; call prim_cons
  %cloptr13872 = inttoptr i64 %arg9882 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13873 = getelementptr inbounds i64, i64* %cloptr13872, i64 0                 ; &cloptr13872[0]
  %f13875 = load i64, i64* %i0ptr13873, align 8                                      ; load; *i0ptr13873
  %fptr13874 = inttoptr i64 %f13875 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13874(i64 %arg9882, i64 %rva11124)                  ; tail call
  ret void
}


define void @lam13161(i64 %env13162, i64 %rvp11123) {
  %envptr13876 = inttoptr i64 %env13162 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13877 = getelementptr inbounds i64, i64* %envptr13876, i64 2                ; &envptr13876[2]
  %cont9435 = load i64, i64* %envptr13877, align 8                                   ; load; *envptr13877
  %envptr13878 = inttoptr i64 %env13162 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13879 = getelementptr inbounds i64, i64* %envptr13878, i64 1                ; &envptr13878[1]
  %dJc$cc = load i64, i64* %envptr13879, align 8                                     ; load; *envptr13879
  %_959438 = call i64 @prim_car(i64 %rvp11123)                                       ; call prim_car
  %rvp11122 = call i64 @prim_cdr(i64 %rvp11123)                                      ; call prim_cdr
  %gpP$_950 = call i64 @prim_car(i64 %rvp11122)                                      ; call prim_car
  %na11118 = call i64 @prim_cdr(i64 %rvp11122)                                       ; call prim_cdr
  %rva11121 = add i64 0, 0                                                           ; quoted ()
  %rva11120 = call i64 @prim_cons(i64 %dJc$cc, i64 %rva11121)                        ; call prim_cons
  %rva11119 = call i64 @prim_cons(i64 %cont9435, i64 %rva11120)                      ; call prim_cons
  %cloptr13880 = inttoptr i64 %dJc$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr13881 = getelementptr inbounds i64, i64* %cloptr13880, i64 0                 ; &cloptr13880[0]
  %f13883 = load i64, i64* %i0ptr13881, align 8                                      ; load; *i0ptr13881
  %fptr13882 = inttoptr i64 %f13883 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13882(i64 %dJc$cc, i64 %rva11119)                   ; tail call
  ret void
}


define void @lam13150(i64 %env13151, i64 %rvp11204) {
  %cont9442 = call i64 @prim_car(i64 %rvp11204)                                      ; call prim_car
  %rvp11203 = call i64 @prim_cdr(i64 %rvp11204)                                      ; call prim_cdr
  %TY2$lst = call i64 @prim_car(i64 %rvp11203)                                       ; call prim_car
  %rvp11202 = call i64 @prim_cdr(i64 %rvp11203)                                      ; call prim_cdr
  %A7o$n = call i64 @prim_car(i64 %rvp11202)                                         ; call prim_car
  %na11143 = call i64 @prim_cdr(i64 %rvp11202)                                       ; call prim_cdr
  %arg9890 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %k96$lst = call i64 @prim_make_45vector(i64 %arg9890, i64 %TY2$lst)                ; call prim_make_45vector
  %arg9892 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %nsw$n = call i64 @prim_make_45vector(i64 %arg9892, i64 %A7o$n)                    ; call prim_make_45vector
  %cloptr13884 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr13885 = getelementptr inbounds i64, i64* %cloptr13884, i64 0                  ; &cloptr13884[0]
  %f13886 = ptrtoint void(i64,i64)* @lam13146 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13886, i64* %eptr13885                                                 ; store fptr
  %arg9895 = ptrtoint i64* %cloptr13884 to i64                                       ; closure cast; i64* -> i64
  %cloptr13887 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13889 = getelementptr inbounds i64, i64* %cloptr13887, i64 1                  ; &eptr13889[1]
  %eptr13890 = getelementptr inbounds i64, i64* %cloptr13887, i64 2                  ; &eptr13890[2]
  %eptr13891 = getelementptr inbounds i64, i64* %cloptr13887, i64 3                  ; &eptr13891[3]
  store i64 %nsw$n, i64* %eptr13889                                                  ; *eptr13889 = %nsw$n
  store i64 %cont9442, i64* %eptr13890                                               ; *eptr13890 = %cont9442
  store i64 %k96$lst, i64* %eptr13891                                                ; *eptr13891 = %k96$lst
  %eptr13888 = getelementptr inbounds i64, i64* %cloptr13887, i64 0                  ; &cloptr13887[0]
  %f13892 = ptrtoint void(i64,i64)* @lam13143 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13892, i64* %eptr13888                                                 ; store fptr
  %arg9894 = ptrtoint i64* %cloptr13887 to i64                                       ; closure cast; i64* -> i64
  %cloptr13893 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13895 = getelementptr inbounds i64, i64* %cloptr13893, i64 1                  ; &eptr13895[1]
  %eptr13896 = getelementptr inbounds i64, i64* %cloptr13893, i64 2                  ; &eptr13896[2]
  %eptr13897 = getelementptr inbounds i64, i64* %cloptr13893, i64 3                  ; &eptr13897[3]
  store i64 %nsw$n, i64* %eptr13895                                                  ; *eptr13895 = %nsw$n
  store i64 %cont9442, i64* %eptr13896                                               ; *eptr13896 = %cont9442
  store i64 %k96$lst, i64* %eptr13897                                                ; *eptr13897 = %k96$lst
  %eptr13894 = getelementptr inbounds i64, i64* %cloptr13893, i64 0                  ; &cloptr13893[0]
  %f13898 = ptrtoint void(i64,i64)* @lam13122 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13898, i64* %eptr13894                                                 ; store fptr
  %arg9893 = ptrtoint i64* %cloptr13893 to i64                                       ; closure cast; i64* -> i64
  %rva11201 = add i64 0, 0                                                           ; quoted ()
  %rva11200 = call i64 @prim_cons(i64 %arg9893, i64 %rva11201)                       ; call prim_cons
  %rva11199 = call i64 @prim_cons(i64 %arg9894, i64 %rva11200)                       ; call prim_cons
  %cloptr13899 = inttoptr i64 %arg9895 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13900 = getelementptr inbounds i64, i64* %cloptr13899, i64 0                 ; &cloptr13899[0]
  %f13902 = load i64, i64* %i0ptr13900, align 8                                      ; load; *i0ptr13900
  %fptr13901 = inttoptr i64 %f13902 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13901(i64 %arg9895, i64 %rva11199)                  ; tail call
  ret void
}


define void @lam13146(i64 %env13147, i64 %rvp11150) {
  %cont9449 = call i64 @prim_car(i64 %rvp11150)                                      ; call prim_car
  %rvp11149 = call i64 @prim_cdr(i64 %rvp11150)                                      ; call prim_cdr
  %P4O$u = call i64 @prim_car(i64 %rvp11149)                                         ; call prim_car
  %na11145 = call i64 @prim_cdr(i64 %rvp11149)                                       ; call prim_cdr
  %rva11148 = add i64 0, 0                                                           ; quoted ()
  %rva11147 = call i64 @prim_cons(i64 %P4O$u, i64 %rva11148)                         ; call prim_cons
  %rva11146 = call i64 @prim_cons(i64 %cont9449, i64 %rva11147)                      ; call prim_cons
  %cloptr13903 = inttoptr i64 %P4O$u to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr13904 = getelementptr inbounds i64, i64* %cloptr13903, i64 0                 ; &cloptr13903[0]
  %f13906 = load i64, i64* %i0ptr13904, align 8                                      ; load; *i0ptr13904
  %fptr13905 = inttoptr i64 %f13906 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13905(i64 %P4O$u, i64 %rva11146)                    ; tail call
  ret void
}


define void @lam13143(i64 %env13144, i64 %rvp11174) {
  %envptr13907 = inttoptr i64 %env13144 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13908 = getelementptr inbounds i64, i64* %envptr13907, i64 3                ; &envptr13907[3]
  %k96$lst = load i64, i64* %envptr13908, align 8                                    ; load; *envptr13908
  %envptr13909 = inttoptr i64 %env13144 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13910 = getelementptr inbounds i64, i64* %envptr13909, i64 2                ; &envptr13909[2]
  %cont9442 = load i64, i64* %envptr13910, align 8                                   ; load; *envptr13910
  %envptr13911 = inttoptr i64 %env13144 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13912 = getelementptr inbounds i64, i64* %envptr13911, i64 1                ; &envptr13911[1]
  %nsw$n = load i64, i64* %envptr13912, align 8                                      ; load; *envptr13912
  %_959443 = call i64 @prim_car(i64 %rvp11174)                                       ; call prim_car
  %rvp11173 = call i64 @prim_cdr(i64 %rvp11174)                                      ; call prim_cdr
  %bHM$cc = call i64 @prim_car(i64 %rvp11173)                                        ; call prim_car
  %na11152 = call i64 @prim_cdr(i64 %rvp11173)                                       ; call prim_cdr
  %arg9899 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9288 = call i64 @prim_vector_45ref(i64 %nsw$n, i64 %arg9899)                     ; call prim_vector_45ref
  %arg9902 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9289 = call i64 @prim__61(i64 %arg9902, i64 %a9288)                              ; call prim__61
  %cmp13913 = icmp eq i64 %a9289, 15                                                 ; false?
  br i1 %cmp13913, label %else13915, label %then13914                                ; if

then13914:
  %arg9903 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9444 = call i64 @prim_vector_45ref(i64 %k96$lst, i64 %arg9903)             ; call prim_vector_45ref
  %arg9906 = add i64 0, 0                                                            ; quoted ()
  %rva11155 = add i64 0, 0                                                           ; quoted ()
  %rva11154 = call i64 @prim_cons(i64 %retprim9444, i64 %rva11155)                   ; call prim_cons
  %rva11153 = call i64 @prim_cons(i64 %arg9906, i64 %rva11154)                       ; call prim_cons
  %cloptr13916 = inttoptr i64 %cont9442 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13917 = getelementptr inbounds i64, i64* %cloptr13916, i64 0                 ; &cloptr13916[0]
  %f13919 = load i64, i64* %i0ptr13917, align 8                                      ; load; *i0ptr13917
  %fptr13918 = inttoptr i64 %f13919 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13918(i64 %cont9442, i64 %rva11153)                 ; tail call
  ret void

else13915:
  %arg9908 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9290 = call i64 @prim_vector_45ref(i64 %k96$lst, i64 %arg9908)                   ; call prim_vector_45ref
  %a9291 = call i64 @prim_cdr(i64 %a9290)                                            ; call prim_cdr
  %arg9912 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9448 = call i64 @prim_vector_45set_33(i64 %k96$lst, i64 %arg9912, i64 %a9291); call prim_vector_45set_33
  %cloptr13920 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13922 = getelementptr inbounds i64, i64* %cloptr13920, i64 1                  ; &eptr13922[1]
  %eptr13923 = getelementptr inbounds i64, i64* %cloptr13920, i64 2                  ; &eptr13923[2]
  %eptr13924 = getelementptr inbounds i64, i64* %cloptr13920, i64 3                  ; &eptr13924[3]
  store i64 %nsw$n, i64* %eptr13922                                                  ; *eptr13922 = %nsw$n
  store i64 %cont9442, i64* %eptr13923                                               ; *eptr13923 = %cont9442
  store i64 %bHM$cc, i64* %eptr13924                                                 ; *eptr13924 = %bHM$cc
  %eptr13921 = getelementptr inbounds i64, i64* %cloptr13920, i64 0                  ; &cloptr13920[0]
  %f13925 = ptrtoint void(i64,i64)* @lam13137 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13925, i64* %eptr13921                                                 ; store fptr
  %arg9916 = ptrtoint i64* %cloptr13920 to i64                                       ; closure cast; i64* -> i64
  %arg9915 = add i64 0, 0                                                            ; quoted ()
  %rva11172 = add i64 0, 0                                                           ; quoted ()
  %rva11171 = call i64 @prim_cons(i64 %retprim9448, i64 %rva11172)                   ; call prim_cons
  %rva11170 = call i64 @prim_cons(i64 %arg9915, i64 %rva11171)                       ; call prim_cons
  %cloptr13926 = inttoptr i64 %arg9916 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13927 = getelementptr inbounds i64, i64* %cloptr13926, i64 0                 ; &cloptr13926[0]
  %f13929 = load i64, i64* %i0ptr13927, align 8                                      ; load; *i0ptr13927
  %fptr13928 = inttoptr i64 %f13929 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13928(i64 %arg9916, i64 %rva11170)                  ; tail call
  ret void
}


define void @lam13137(i64 %env13138, i64 %rvp11169) {
  %envptr13930 = inttoptr i64 %env13138 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13931 = getelementptr inbounds i64, i64* %envptr13930, i64 3                ; &envptr13930[3]
  %bHM$cc = load i64, i64* %envptr13931, align 8                                     ; load; *envptr13931
  %envptr13932 = inttoptr i64 %env13138 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13933 = getelementptr inbounds i64, i64* %envptr13932, i64 2                ; &envptr13932[2]
  %cont9442 = load i64, i64* %envptr13933, align 8                                   ; load; *envptr13933
  %envptr13934 = inttoptr i64 %env13138 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13935 = getelementptr inbounds i64, i64* %envptr13934, i64 1                ; &envptr13934[1]
  %nsw$n = load i64, i64* %envptr13935, align 8                                      ; load; *envptr13935
  %_959445 = call i64 @prim_car(i64 %rvp11169)                                       ; call prim_car
  %rvp11168 = call i64 @prim_cdr(i64 %rvp11169)                                      ; call prim_cdr
  %zNC$_950 = call i64 @prim_car(i64 %rvp11168)                                      ; call prim_car
  %na11157 = call i64 @prim_cdr(i64 %rvp11168)                                       ; call prim_cdr
  %arg9917 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9292 = call i64 @prim_vector_45ref(i64 %nsw$n, i64 %arg9917)                     ; call prim_vector_45ref
  %arg9919 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %a9293 = call i64 @prim__45(i64 %a9292, i64 %arg9919)                              ; call prim__45
  %arg9922 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9447 = call i64 @prim_vector_45set_33(i64 %nsw$n, i64 %arg9922, i64 %a9293); call prim_vector_45set_33
  %cloptr13936 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13938 = getelementptr inbounds i64, i64* %cloptr13936, i64 1                  ; &eptr13938[1]
  %eptr13939 = getelementptr inbounds i64, i64* %cloptr13936, i64 2                  ; &eptr13939[2]
  store i64 %cont9442, i64* %eptr13938                                               ; *eptr13938 = %cont9442
  store i64 %bHM$cc, i64* %eptr13939                                                 ; *eptr13939 = %bHM$cc
  %eptr13937 = getelementptr inbounds i64, i64* %cloptr13936, i64 0                  ; &cloptr13936[0]
  %f13940 = ptrtoint void(i64,i64)* @lam13132 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13940, i64* %eptr13937                                                 ; store fptr
  %arg9926 = ptrtoint i64* %cloptr13936 to i64                                       ; closure cast; i64* -> i64
  %arg9925 = add i64 0, 0                                                            ; quoted ()
  %rva11167 = add i64 0, 0                                                           ; quoted ()
  %rva11166 = call i64 @prim_cons(i64 %retprim9447, i64 %rva11167)                   ; call prim_cons
  %rva11165 = call i64 @prim_cons(i64 %arg9925, i64 %rva11166)                       ; call prim_cons
  %cloptr13941 = inttoptr i64 %arg9926 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13942 = getelementptr inbounds i64, i64* %cloptr13941, i64 0                 ; &cloptr13941[0]
  %f13944 = load i64, i64* %i0ptr13942, align 8                                      ; load; *i0ptr13942
  %fptr13943 = inttoptr i64 %f13944 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13943(i64 %arg9926, i64 %rva11165)                  ; tail call
  ret void
}


define void @lam13132(i64 %env13133, i64 %rvp11164) {
  %envptr13945 = inttoptr i64 %env13133 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13946 = getelementptr inbounds i64, i64* %envptr13945, i64 2                ; &envptr13945[2]
  %bHM$cc = load i64, i64* %envptr13946, align 8                                     ; load; *envptr13946
  %envptr13947 = inttoptr i64 %env13133 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13948 = getelementptr inbounds i64, i64* %envptr13947, i64 1                ; &envptr13947[1]
  %cont9442 = load i64, i64* %envptr13948, align 8                                   ; load; *envptr13948
  %_959446 = call i64 @prim_car(i64 %rvp11164)                                       ; call prim_car
  %rvp11163 = call i64 @prim_cdr(i64 %rvp11164)                                      ; call prim_cdr
  %Ko1$_951 = call i64 @prim_car(i64 %rvp11163)                                      ; call prim_car
  %na11159 = call i64 @prim_cdr(i64 %rvp11163)                                       ; call prim_cdr
  %rva11162 = add i64 0, 0                                                           ; quoted ()
  %rva11161 = call i64 @prim_cons(i64 %bHM$cc, i64 %rva11162)                        ; call prim_cons
  %rva11160 = call i64 @prim_cons(i64 %cont9442, i64 %rva11161)                      ; call prim_cons
  %cloptr13949 = inttoptr i64 %bHM$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr13950 = getelementptr inbounds i64, i64* %cloptr13949, i64 0                 ; &cloptr13949[0]
  %f13952 = load i64, i64* %i0ptr13950, align 8                                      ; load; *i0ptr13950
  %fptr13951 = inttoptr i64 %f13952 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13951(i64 %bHM$cc, i64 %rva11160)                   ; tail call
  ret void
}


define void @lam13122(i64 %env13123, i64 %rvp11198) {
  %envptr13953 = inttoptr i64 %env13123 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13954 = getelementptr inbounds i64, i64* %envptr13953, i64 3                ; &envptr13953[3]
  %k96$lst = load i64, i64* %envptr13954, align 8                                    ; load; *envptr13954
  %envptr13955 = inttoptr i64 %env13123 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13956 = getelementptr inbounds i64, i64* %envptr13955, i64 2                ; &envptr13955[2]
  %cont9442 = load i64, i64* %envptr13956, align 8                                   ; load; *envptr13956
  %envptr13957 = inttoptr i64 %env13123 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13958 = getelementptr inbounds i64, i64* %envptr13957, i64 1                ; &envptr13957[1]
  %nsw$n = load i64, i64* %envptr13958, align 8                                      ; load; *envptr13958
  %_959443 = call i64 @prim_car(i64 %rvp11198)                                       ; call prim_car
  %rvp11197 = call i64 @prim_cdr(i64 %rvp11198)                                      ; call prim_cdr
  %bHM$cc = call i64 @prim_car(i64 %rvp11197)                                        ; call prim_car
  %na11176 = call i64 @prim_cdr(i64 %rvp11197)                                       ; call prim_cdr
  %arg9930 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9288 = call i64 @prim_vector_45ref(i64 %nsw$n, i64 %arg9930)                     ; call prim_vector_45ref
  %arg9933 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9289 = call i64 @prim__61(i64 %arg9933, i64 %a9288)                              ; call prim__61
  %cmp13959 = icmp eq i64 %a9289, 15                                                 ; false?
  br i1 %cmp13959, label %else13961, label %then13960                                ; if

then13960:
  %arg9934 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9444 = call i64 @prim_vector_45ref(i64 %k96$lst, i64 %arg9934)             ; call prim_vector_45ref
  %arg9937 = add i64 0, 0                                                            ; quoted ()
  %rva11179 = add i64 0, 0                                                           ; quoted ()
  %rva11178 = call i64 @prim_cons(i64 %retprim9444, i64 %rva11179)                   ; call prim_cons
  %rva11177 = call i64 @prim_cons(i64 %arg9937, i64 %rva11178)                       ; call prim_cons
  %cloptr13962 = inttoptr i64 %cont9442 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr13963 = getelementptr inbounds i64, i64* %cloptr13962, i64 0                 ; &cloptr13962[0]
  %f13965 = load i64, i64* %i0ptr13963, align 8                                      ; load; *i0ptr13963
  %fptr13964 = inttoptr i64 %f13965 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13964(i64 %cont9442, i64 %rva11177)                 ; tail call
  ret void

else13961:
  %arg9939 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9290 = call i64 @prim_vector_45ref(i64 %k96$lst, i64 %arg9939)                   ; call prim_vector_45ref
  %a9291 = call i64 @prim_cdr(i64 %a9290)                                            ; call prim_cdr
  %arg9943 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9448 = call i64 @prim_vector_45set_33(i64 %k96$lst, i64 %arg9943, i64 %a9291); call prim_vector_45set_33
  %cloptr13966 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr13968 = getelementptr inbounds i64, i64* %cloptr13966, i64 1                  ; &eptr13968[1]
  %eptr13969 = getelementptr inbounds i64, i64* %cloptr13966, i64 2                  ; &eptr13969[2]
  %eptr13970 = getelementptr inbounds i64, i64* %cloptr13966, i64 3                  ; &eptr13970[3]
  store i64 %nsw$n, i64* %eptr13968                                                  ; *eptr13968 = %nsw$n
  store i64 %cont9442, i64* %eptr13969                                               ; *eptr13969 = %cont9442
  store i64 %bHM$cc, i64* %eptr13970                                                 ; *eptr13970 = %bHM$cc
  %eptr13967 = getelementptr inbounds i64, i64* %cloptr13966, i64 0                  ; &cloptr13966[0]
  %f13971 = ptrtoint void(i64,i64)* @lam13116 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13971, i64* %eptr13967                                                 ; store fptr
  %arg9947 = ptrtoint i64* %cloptr13966 to i64                                       ; closure cast; i64* -> i64
  %arg9946 = add i64 0, 0                                                            ; quoted ()
  %rva11196 = add i64 0, 0                                                           ; quoted ()
  %rva11195 = call i64 @prim_cons(i64 %retprim9448, i64 %rva11196)                   ; call prim_cons
  %rva11194 = call i64 @prim_cons(i64 %arg9946, i64 %rva11195)                       ; call prim_cons
  %cloptr13972 = inttoptr i64 %arg9947 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13973 = getelementptr inbounds i64, i64* %cloptr13972, i64 0                 ; &cloptr13972[0]
  %f13975 = load i64, i64* %i0ptr13973, align 8                                      ; load; *i0ptr13973
  %fptr13974 = inttoptr i64 %f13975 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13974(i64 %arg9947, i64 %rva11194)                  ; tail call
  ret void
}


define void @lam13116(i64 %env13117, i64 %rvp11193) {
  %envptr13976 = inttoptr i64 %env13117 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13977 = getelementptr inbounds i64, i64* %envptr13976, i64 3                ; &envptr13976[3]
  %bHM$cc = load i64, i64* %envptr13977, align 8                                     ; load; *envptr13977
  %envptr13978 = inttoptr i64 %env13117 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13979 = getelementptr inbounds i64, i64* %envptr13978, i64 2                ; &envptr13978[2]
  %cont9442 = load i64, i64* %envptr13979, align 8                                   ; load; *envptr13979
  %envptr13980 = inttoptr i64 %env13117 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13981 = getelementptr inbounds i64, i64* %envptr13980, i64 1                ; &envptr13980[1]
  %nsw$n = load i64, i64* %envptr13981, align 8                                      ; load; *envptr13981
  %_959445 = call i64 @prim_car(i64 %rvp11193)                                       ; call prim_car
  %rvp11192 = call i64 @prim_cdr(i64 %rvp11193)                                      ; call prim_cdr
  %zNC$_950 = call i64 @prim_car(i64 %rvp11192)                                      ; call prim_car
  %na11181 = call i64 @prim_cdr(i64 %rvp11192)                                       ; call prim_cdr
  %arg9948 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9292 = call i64 @prim_vector_45ref(i64 %nsw$n, i64 %arg9948)                     ; call prim_vector_45ref
  %arg9950 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %a9293 = call i64 @prim__45(i64 %a9292, i64 %arg9950)                              ; call prim__45
  %arg9953 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9447 = call i64 @prim_vector_45set_33(i64 %nsw$n, i64 %arg9953, i64 %a9293); call prim_vector_45set_33
  %cloptr13982 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr13984 = getelementptr inbounds i64, i64* %cloptr13982, i64 1                  ; &eptr13984[1]
  %eptr13985 = getelementptr inbounds i64, i64* %cloptr13982, i64 2                  ; &eptr13985[2]
  store i64 %cont9442, i64* %eptr13984                                               ; *eptr13984 = %cont9442
  store i64 %bHM$cc, i64* %eptr13985                                                 ; *eptr13985 = %bHM$cc
  %eptr13983 = getelementptr inbounds i64, i64* %cloptr13982, i64 0                  ; &cloptr13982[0]
  %f13986 = ptrtoint void(i64,i64)* @lam13111 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f13986, i64* %eptr13983                                                 ; store fptr
  %arg9957 = ptrtoint i64* %cloptr13982 to i64                                       ; closure cast; i64* -> i64
  %arg9956 = add i64 0, 0                                                            ; quoted ()
  %rva11191 = add i64 0, 0                                                           ; quoted ()
  %rva11190 = call i64 @prim_cons(i64 %retprim9447, i64 %rva11191)                   ; call prim_cons
  %rva11189 = call i64 @prim_cons(i64 %arg9956, i64 %rva11190)                       ; call prim_cons
  %cloptr13987 = inttoptr i64 %arg9957 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr13988 = getelementptr inbounds i64, i64* %cloptr13987, i64 0                 ; &cloptr13987[0]
  %f13990 = load i64, i64* %i0ptr13988, align 8                                      ; load; *i0ptr13988
  %fptr13989 = inttoptr i64 %f13990 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13989(i64 %arg9957, i64 %rva11189)                  ; tail call
  ret void
}


define void @lam13111(i64 %env13112, i64 %rvp11188) {
  %envptr13991 = inttoptr i64 %env13112 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13992 = getelementptr inbounds i64, i64* %envptr13991, i64 2                ; &envptr13991[2]
  %bHM$cc = load i64, i64* %envptr13992, align 8                                     ; load; *envptr13992
  %envptr13993 = inttoptr i64 %env13112 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr13994 = getelementptr inbounds i64, i64* %envptr13993, i64 1                ; &envptr13993[1]
  %cont9442 = load i64, i64* %envptr13994, align 8                                   ; load; *envptr13994
  %_959446 = call i64 @prim_car(i64 %rvp11188)                                       ; call prim_car
  %rvp11187 = call i64 @prim_cdr(i64 %rvp11188)                                      ; call prim_cdr
  %Ko1$_951 = call i64 @prim_car(i64 %rvp11187)                                      ; call prim_car
  %na11183 = call i64 @prim_cdr(i64 %rvp11187)                                       ; call prim_cdr
  %rva11186 = add i64 0, 0                                                           ; quoted ()
  %rva11185 = call i64 @prim_cons(i64 %bHM$cc, i64 %rva11186)                        ; call prim_cons
  %rva11184 = call i64 @prim_cons(i64 %cont9442, i64 %rva11185)                      ; call prim_cons
  %cloptr13995 = inttoptr i64 %bHM$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr13996 = getelementptr inbounds i64, i64* %cloptr13995, i64 0                 ; &cloptr13995[0]
  %f13998 = load i64, i64* %i0ptr13996, align 8                                      ; load; *i0ptr13996
  %fptr13997 = inttoptr i64 %f13998 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr13997(i64 %bHM$cc, i64 %rva11184)                   ; tail call
  ret void
}


define void @lam13100(i64 %env13101, i64 %rvp11259) {
  %cont9450 = call i64 @prim_car(i64 %rvp11259)                                      ; call prim_car
  %rvp11258 = call i64 @prim_cdr(i64 %rvp11259)                                      ; call prim_cdr
  %Wlq$v = call i64 @prim_car(i64 %rvp11258)                                         ; call prim_car
  %rvp11257 = call i64 @prim_cdr(i64 %rvp11258)                                      ; call prim_cdr
  %uNb$lst = call i64 @prim_car(i64 %rvp11257)                                       ; call prim_car
  %na11206 = call i64 @prim_cdr(i64 %rvp11257)                                       ; call prim_cdr
  %arg9962 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %RGh$lst = call i64 @prim_make_45vector(i64 %arg9962, i64 %uNb$lst)                ; call prim_make_45vector
  %cloptr13999 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr14000 = getelementptr inbounds i64, i64* %cloptr13999, i64 0                  ; &cloptr13999[0]
  %f14001 = ptrtoint void(i64,i64)* @lam13097 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14001, i64* %eptr14000                                                 ; store fptr
  %arg9965 = ptrtoint i64* %cloptr13999 to i64                                       ; closure cast; i64* -> i64
  %cloptr14002 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14004 = getelementptr inbounds i64, i64* %cloptr14002, i64 1                  ; &eptr14004[1]
  %eptr14005 = getelementptr inbounds i64, i64* %cloptr14002, i64 2                  ; &eptr14005[2]
  %eptr14006 = getelementptr inbounds i64, i64* %cloptr14002, i64 3                  ; &eptr14006[3]
  store i64 %Wlq$v, i64* %eptr14004                                                  ; *eptr14004 = %Wlq$v
  store i64 %cont9450, i64* %eptr14005                                               ; *eptr14005 = %cont9450
  store i64 %RGh$lst, i64* %eptr14006                                                ; *eptr14006 = %RGh$lst
  %eptr14003 = getelementptr inbounds i64, i64* %cloptr14002, i64 0                  ; &cloptr14002[0]
  %f14007 = ptrtoint void(i64,i64)* @lam13094 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14007, i64* %eptr14003                                                 ; store fptr
  %arg9964 = ptrtoint i64* %cloptr14002 to i64                                       ; closure cast; i64* -> i64
  %cloptr14008 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14010 = getelementptr inbounds i64, i64* %cloptr14008, i64 1                  ; &eptr14010[1]
  %eptr14011 = getelementptr inbounds i64, i64* %cloptr14008, i64 2                  ; &eptr14011[2]
  %eptr14012 = getelementptr inbounds i64, i64* %cloptr14008, i64 3                  ; &eptr14012[3]
  store i64 %Wlq$v, i64* %eptr14010                                                  ; *eptr14010 = %Wlq$v
  store i64 %cont9450, i64* %eptr14011                                               ; *eptr14011 = %cont9450
  store i64 %RGh$lst, i64* %eptr14012                                                ; *eptr14012 = %RGh$lst
  %eptr14009 = getelementptr inbounds i64, i64* %cloptr14008, i64 0                  ; &cloptr14008[0]
  %f14013 = ptrtoint void(i64,i64)* @lam13077 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14013, i64* %eptr14009                                                 ; store fptr
  %arg9963 = ptrtoint i64* %cloptr14008 to i64                                       ; closure cast; i64* -> i64
  %rva11256 = add i64 0, 0                                                           ; quoted ()
  %rva11255 = call i64 @prim_cons(i64 %arg9963, i64 %rva11256)                       ; call prim_cons
  %rva11254 = call i64 @prim_cons(i64 %arg9964, i64 %rva11255)                       ; call prim_cons
  %cloptr14014 = inttoptr i64 %arg9965 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr14015 = getelementptr inbounds i64, i64* %cloptr14014, i64 0                 ; &cloptr14014[0]
  %f14017 = load i64, i64* %i0ptr14015, align 8                                      ; load; *i0ptr14015
  %fptr14016 = inttoptr i64 %f14017 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14016(i64 %arg9965, i64 %rva11254)                  ; tail call
  ret void
}


define void @lam13097(i64 %env13098, i64 %rvp11213) {
  %cont9455 = call i64 @prim_car(i64 %rvp11213)                                      ; call prim_car
  %rvp11212 = call i64 @prim_cdr(i64 %rvp11213)                                      ; call prim_cdr
  %DH5$u = call i64 @prim_car(i64 %rvp11212)                                         ; call prim_car
  %na11208 = call i64 @prim_cdr(i64 %rvp11212)                                       ; call prim_cdr
  %rva11211 = add i64 0, 0                                                           ; quoted ()
  %rva11210 = call i64 @prim_cons(i64 %DH5$u, i64 %rva11211)                         ; call prim_cons
  %rva11209 = call i64 @prim_cons(i64 %cont9455, i64 %rva11210)                      ; call prim_cons
  %cloptr14018 = inttoptr i64 %DH5$u to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14019 = getelementptr inbounds i64, i64* %cloptr14018, i64 0                 ; &cloptr14018[0]
  %f14021 = load i64, i64* %i0ptr14019, align 8                                      ; load; *i0ptr14019
  %fptr14020 = inttoptr i64 %f14021 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14020(i64 %DH5$u, i64 %rva11209)                    ; tail call
  ret void
}


define void @lam13094(i64 %env13095, i64 %rvp11233) {
  %envptr14022 = inttoptr i64 %env13095 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14023 = getelementptr inbounds i64, i64* %envptr14022, i64 3                ; &envptr14022[3]
  %RGh$lst = load i64, i64* %envptr14023, align 8                                    ; load; *envptr14023
  %envptr14024 = inttoptr i64 %env13095 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14025 = getelementptr inbounds i64, i64* %envptr14024, i64 2                ; &envptr14024[2]
  %cont9450 = load i64, i64* %envptr14025, align 8                                   ; load; *envptr14025
  %envptr14026 = inttoptr i64 %env13095 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14027 = getelementptr inbounds i64, i64* %envptr14026, i64 1                ; &envptr14026[1]
  %Wlq$v = load i64, i64* %envptr14027, align 8                                      ; load; *envptr14027
  %_959451 = call i64 @prim_car(i64 %rvp11233)                                       ; call prim_car
  %rvp11232 = call i64 @prim_cdr(i64 %rvp11233)                                      ; call prim_cdr
  %taA$cc = call i64 @prim_car(i64 %rvp11232)                                        ; call prim_car
  %na11215 = call i64 @prim_cdr(i64 %rvp11232)                                       ; call prim_cdr
  %arg9969 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9294 = call i64 @prim_vector_45ref(i64 %RGh$lst, i64 %arg9969)                   ; call prim_vector_45ref
  %a9295 = call i64 @prim_null_63(i64 %a9294)                                        ; call prim_null_63
  %cmp14028 = icmp eq i64 %a9295, 15                                                 ; false?
  br i1 %cmp14028, label %else14030, label %then14029                                ; if

then14029:
  %arg9973 = add i64 0, 0                                                            ; quoted ()
  %arg9972 = call i64 @const_init_false()                                            ; quoted #f
  %rva11218 = add i64 0, 0                                                           ; quoted ()
  %rva11217 = call i64 @prim_cons(i64 %arg9972, i64 %rva11218)                       ; call prim_cons
  %rva11216 = call i64 @prim_cons(i64 %arg9973, i64 %rva11217)                       ; call prim_cons
  %cloptr14031 = inttoptr i64 %cont9450 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14032 = getelementptr inbounds i64, i64* %cloptr14031, i64 0                 ; &cloptr14031[0]
  %f14034 = load i64, i64* %i0ptr14032, align 8                                      ; load; *i0ptr14032
  %fptr14033 = inttoptr i64 %f14034 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14033(i64 %cont9450, i64 %rva11216)                 ; tail call
  ret void

else14030:
  %arg9975 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9296 = call i64 @prim_vector_45ref(i64 %RGh$lst, i64 %arg9975)                   ; call prim_vector_45ref
  %a9297 = call i64 @prim_car(i64 %a9296)                                            ; call prim_car
  %a9298 = call i64 @prim_eqv_63(i64 %a9297, i64 %Wlq$v)                             ; call prim_eqv_63
  %cmp14035 = icmp eq i64 %a9298, 15                                                 ; false?
  br i1 %cmp14035, label %else14037, label %then14036                                ; if

then14036:
  %arg9980 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9452 = call i64 @prim_vector_45ref(i64 %RGh$lst, i64 %arg9980)             ; call prim_vector_45ref
  %arg9983 = add i64 0, 0                                                            ; quoted ()
  %rva11221 = add i64 0, 0                                                           ; quoted ()
  %rva11220 = call i64 @prim_cons(i64 %retprim9452, i64 %rva11221)                   ; call prim_cons
  %rva11219 = call i64 @prim_cons(i64 %arg9983, i64 %rva11220)                       ; call prim_cons
  %cloptr14038 = inttoptr i64 %cont9450 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14039 = getelementptr inbounds i64, i64* %cloptr14038, i64 0                 ; &cloptr14038[0]
  %f14041 = load i64, i64* %i0ptr14039, align 8                                      ; load; *i0ptr14039
  %fptr14040 = inttoptr i64 %f14041 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14040(i64 %cont9450, i64 %rva11219)                 ; tail call
  ret void

else14037:
  %arg9985 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9299 = call i64 @prim_vector_45ref(i64 %RGh$lst, i64 %arg9985)                   ; call prim_vector_45ref
  %a9300 = call i64 @prim_cdr(i64 %a9299)                                            ; call prim_cdr
  %arg9989 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim9454 = call i64 @prim_vector_45set_33(i64 %RGh$lst, i64 %arg9989, i64 %a9300); call prim_vector_45set_33
  %cloptr14042 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr14044 = getelementptr inbounds i64, i64* %cloptr14042, i64 1                  ; &eptr14044[1]
  %eptr14045 = getelementptr inbounds i64, i64* %cloptr14042, i64 2                  ; &eptr14045[2]
  store i64 %cont9450, i64* %eptr14044                                               ; *eptr14044 = %cont9450
  store i64 %taA$cc, i64* %eptr14045                                                 ; *eptr14045 = %taA$cc
  %eptr14043 = getelementptr inbounds i64, i64* %cloptr14042, i64 0                  ; &cloptr14042[0]
  %f14046 = ptrtoint void(i64,i64)* @lam13088 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14046, i64* %eptr14043                                                 ; store fptr
  %arg9993 = ptrtoint i64* %cloptr14042 to i64                                       ; closure cast; i64* -> i64
  %arg9992 = add i64 0, 0                                                            ; quoted ()
  %rva11231 = add i64 0, 0                                                           ; quoted ()
  %rva11230 = call i64 @prim_cons(i64 %retprim9454, i64 %rva11231)                   ; call prim_cons
  %rva11229 = call i64 @prim_cons(i64 %arg9992, i64 %rva11230)                       ; call prim_cons
  %cloptr14047 = inttoptr i64 %arg9993 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr14048 = getelementptr inbounds i64, i64* %cloptr14047, i64 0                 ; &cloptr14047[0]
  %f14050 = load i64, i64* %i0ptr14048, align 8                                      ; load; *i0ptr14048
  %fptr14049 = inttoptr i64 %f14050 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14049(i64 %arg9993, i64 %rva11229)                  ; tail call
  ret void
}


define void @lam13088(i64 %env13089, i64 %rvp11228) {
  %envptr14051 = inttoptr i64 %env13089 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14052 = getelementptr inbounds i64, i64* %envptr14051, i64 2                ; &envptr14051[2]
  %taA$cc = load i64, i64* %envptr14052, align 8                                     ; load; *envptr14052
  %envptr14053 = inttoptr i64 %env13089 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14054 = getelementptr inbounds i64, i64* %envptr14053, i64 1                ; &envptr14053[1]
  %cont9450 = load i64, i64* %envptr14054, align 8                                   ; load; *envptr14054
  %_959453 = call i64 @prim_car(i64 %rvp11228)                                       ; call prim_car
  %rvp11227 = call i64 @prim_cdr(i64 %rvp11228)                                      ; call prim_cdr
  %SLe$_950 = call i64 @prim_car(i64 %rvp11227)                                      ; call prim_car
  %na11223 = call i64 @prim_cdr(i64 %rvp11227)                                       ; call prim_cdr
  %rva11226 = add i64 0, 0                                                           ; quoted ()
  %rva11225 = call i64 @prim_cons(i64 %taA$cc, i64 %rva11226)                        ; call prim_cons
  %rva11224 = call i64 @prim_cons(i64 %cont9450, i64 %rva11225)                      ; call prim_cons
  %cloptr14055 = inttoptr i64 %taA$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr14056 = getelementptr inbounds i64, i64* %cloptr14055, i64 0                 ; &cloptr14055[0]
  %f14058 = load i64, i64* %i0ptr14056, align 8                                      ; load; *i0ptr14056
  %fptr14057 = inttoptr i64 %f14058 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14057(i64 %taA$cc, i64 %rva11224)                   ; tail call
  ret void
}


define void @lam13077(i64 %env13078, i64 %rvp11253) {
  %envptr14059 = inttoptr i64 %env13078 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14060 = getelementptr inbounds i64, i64* %envptr14059, i64 3                ; &envptr14059[3]
  %RGh$lst = load i64, i64* %envptr14060, align 8                                    ; load; *envptr14060
  %envptr14061 = inttoptr i64 %env13078 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14062 = getelementptr inbounds i64, i64* %envptr14061, i64 2                ; &envptr14061[2]
  %cont9450 = load i64, i64* %envptr14062, align 8                                   ; load; *envptr14062
  %envptr14063 = inttoptr i64 %env13078 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14064 = getelementptr inbounds i64, i64* %envptr14063, i64 1                ; &envptr14063[1]
  %Wlq$v = load i64, i64* %envptr14064, align 8                                      ; load; *envptr14064
  %_959451 = call i64 @prim_car(i64 %rvp11253)                                       ; call prim_car
  %rvp11252 = call i64 @prim_cdr(i64 %rvp11253)                                      ; call prim_cdr
  %taA$cc = call i64 @prim_car(i64 %rvp11252)                                        ; call prim_car
  %na11235 = call i64 @prim_cdr(i64 %rvp11252)                                       ; call prim_cdr
  %arg9997 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a9294 = call i64 @prim_vector_45ref(i64 %RGh$lst, i64 %arg9997)                   ; call prim_vector_45ref
  %a9295 = call i64 @prim_null_63(i64 %a9294)                                        ; call prim_null_63
  %cmp14065 = icmp eq i64 %a9295, 15                                                 ; false?
  br i1 %cmp14065, label %else14067, label %then14066                                ; if

then14066:
  %arg10001 = add i64 0, 0                                                           ; quoted ()
  %arg10000 = call i64 @const_init_false()                                           ; quoted #f
  %rva11238 = add i64 0, 0                                                           ; quoted ()
  %rva11237 = call i64 @prim_cons(i64 %arg10000, i64 %rva11238)                      ; call prim_cons
  %rva11236 = call i64 @prim_cons(i64 %arg10001, i64 %rva11237)                      ; call prim_cons
  %cloptr14068 = inttoptr i64 %cont9450 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14069 = getelementptr inbounds i64, i64* %cloptr14068, i64 0                 ; &cloptr14068[0]
  %f14071 = load i64, i64* %i0ptr14069, align 8                                      ; load; *i0ptr14069
  %fptr14070 = inttoptr i64 %f14071 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14070(i64 %cont9450, i64 %rva11236)                 ; tail call
  ret void

else14067:
  %arg10003 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9296 = call i64 @prim_vector_45ref(i64 %RGh$lst, i64 %arg10003)                  ; call prim_vector_45ref
  %a9297 = call i64 @prim_car(i64 %a9296)                                            ; call prim_car
  %a9298 = call i64 @prim_eqv_63(i64 %a9297, i64 %Wlq$v)                             ; call prim_eqv_63
  %cmp14072 = icmp eq i64 %a9298, 15                                                 ; false?
  br i1 %cmp14072, label %else14074, label %then14073                                ; if

then14073:
  %arg10008 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9452 = call i64 @prim_vector_45ref(i64 %RGh$lst, i64 %arg10008)            ; call prim_vector_45ref
  %arg10011 = add i64 0, 0                                                           ; quoted ()
  %rva11241 = add i64 0, 0                                                           ; quoted ()
  %rva11240 = call i64 @prim_cons(i64 %retprim9452, i64 %rva11241)                   ; call prim_cons
  %rva11239 = call i64 @prim_cons(i64 %arg10011, i64 %rva11240)                      ; call prim_cons
  %cloptr14075 = inttoptr i64 %cont9450 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14076 = getelementptr inbounds i64, i64* %cloptr14075, i64 0                 ; &cloptr14075[0]
  %f14078 = load i64, i64* %i0ptr14076, align 8                                      ; load; *i0ptr14076
  %fptr14077 = inttoptr i64 %f14078 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14077(i64 %cont9450, i64 %rva11239)                 ; tail call
  ret void

else14074:
  %arg10013 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9299 = call i64 @prim_vector_45ref(i64 %RGh$lst, i64 %arg10013)                  ; call prim_vector_45ref
  %a9300 = call i64 @prim_cdr(i64 %a9299)                                            ; call prim_cdr
  %arg10017 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9454 = call i64 @prim_vector_45set_33(i64 %RGh$lst, i64 %arg10017, i64 %a9300); call prim_vector_45set_33
  %cloptr14079 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr14081 = getelementptr inbounds i64, i64* %cloptr14079, i64 1                  ; &eptr14081[1]
  %eptr14082 = getelementptr inbounds i64, i64* %cloptr14079, i64 2                  ; &eptr14082[2]
  store i64 %cont9450, i64* %eptr14081                                               ; *eptr14081 = %cont9450
  store i64 %taA$cc, i64* %eptr14082                                                 ; *eptr14082 = %taA$cc
  %eptr14080 = getelementptr inbounds i64, i64* %cloptr14079, i64 0                  ; &cloptr14079[0]
  %f14083 = ptrtoint void(i64,i64)* @lam13071 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14083, i64* %eptr14080                                                 ; store fptr
  %arg10021 = ptrtoint i64* %cloptr14079 to i64                                      ; closure cast; i64* -> i64
  %arg10020 = add i64 0, 0                                                           ; quoted ()
  %rva11251 = add i64 0, 0                                                           ; quoted ()
  %rva11250 = call i64 @prim_cons(i64 %retprim9454, i64 %rva11251)                   ; call prim_cons
  %rva11249 = call i64 @prim_cons(i64 %arg10020, i64 %rva11250)                      ; call prim_cons
  %cloptr14084 = inttoptr i64 %arg10021 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14085 = getelementptr inbounds i64, i64* %cloptr14084, i64 0                 ; &cloptr14084[0]
  %f14087 = load i64, i64* %i0ptr14085, align 8                                      ; load; *i0ptr14085
  %fptr14086 = inttoptr i64 %f14087 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14086(i64 %arg10021, i64 %rva11249)                 ; tail call
  ret void
}


define void @lam13071(i64 %env13072, i64 %rvp11248) {
  %envptr14088 = inttoptr i64 %env13072 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14089 = getelementptr inbounds i64, i64* %envptr14088, i64 2                ; &envptr14088[2]
  %taA$cc = load i64, i64* %envptr14089, align 8                                     ; load; *envptr14089
  %envptr14090 = inttoptr i64 %env13072 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14091 = getelementptr inbounds i64, i64* %envptr14090, i64 1                ; &envptr14090[1]
  %cont9450 = load i64, i64* %envptr14091, align 8                                   ; load; *envptr14091
  %_959453 = call i64 @prim_car(i64 %rvp11248)                                       ; call prim_car
  %rvp11247 = call i64 @prim_cdr(i64 %rvp11248)                                      ; call prim_cdr
  %SLe$_950 = call i64 @prim_car(i64 %rvp11247)                                      ; call prim_car
  %na11243 = call i64 @prim_cdr(i64 %rvp11247)                                       ; call prim_cdr
  %rva11246 = add i64 0, 0                                                           ; quoted ()
  %rva11245 = call i64 @prim_cons(i64 %taA$cc, i64 %rva11246)                        ; call prim_cons
  %rva11244 = call i64 @prim_cons(i64 %cont9450, i64 %rva11245)                      ; call prim_cons
  %cloptr14092 = inttoptr i64 %taA$cc to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr14093 = getelementptr inbounds i64, i64* %cloptr14092, i64 0                 ; &cloptr14092[0]
  %f14095 = load i64, i64* %i0ptr14093, align 8                                      ; load; *i0ptr14093
  %fptr14094 = inttoptr i64 %f14095 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14094(i64 %taA$cc, i64 %rva11244)                   ; tail call
  ret void
}


define void @lam13059(i64 %env13060, i64 %mI0$args9457) {
  %envptr14096 = inttoptr i64 %env13060 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14097 = getelementptr inbounds i64, i64* %envptr14096, i64 1                ; &envptr14096[1]
  %X5k$_37foldl1 = load i64, i64* %envptr14097, align 8                              ; load; *envptr14097
  %cont9456 = call i64 @prim_car(i64 %mI0$args9457)                                  ; call prim_car
  %mI0$args = call i64 @prim_cdr(i64 %mI0$args9457)                                  ; call prim_cdr
  %a9301 = call i64 @prim_null_63(i64 %mI0$args)                                     ; call prim_null_63
  %cmp14098 = icmp eq i64 %a9301, 15                                                 ; false?
  br i1 %cmp14098, label %else14100, label %then14099                                ; if

then14099:
  %arg10029 = add i64 0, 0                                                           ; quoted ()
  %arg10028 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %rva11262 = add i64 0, 0                                                           ; quoted ()
  %rva11261 = call i64 @prim_cons(i64 %arg10028, i64 %rva11262)                      ; call prim_cons
  %rva11260 = call i64 @prim_cons(i64 %arg10029, i64 %rva11261)                      ; call prim_cons
  %cloptr14101 = inttoptr i64 %cont9456 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14102 = getelementptr inbounds i64, i64* %cloptr14101, i64 0                 ; &cloptr14101[0]
  %f14104 = load i64, i64* %i0ptr14102, align 8                                      ; load; *i0ptr14102
  %fptr14103 = inttoptr i64 %f14104 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14103(i64 %cont9456, i64 %rva11260)                 ; tail call
  ret void

else14100:
  %a9302 = call i64 @prim_cdr(i64 %mI0$args)                                         ; call prim_cdr
  %a9303 = call i64 @prim_null_63(i64 %a9302)                                        ; call prim_null_63
  %cmp14105 = icmp eq i64 %a9303, 15                                                 ; false?
  br i1 %cmp14105, label %else14107, label %then14106                                ; if

then14106:
  %retprim9458 = call i64 @prim_car(i64 %mI0$args)                                   ; call prim_car
  %arg10035 = add i64 0, 0                                                           ; quoted ()
  %rva11265 = add i64 0, 0                                                           ; quoted ()
  %rva11264 = call i64 @prim_cons(i64 %retprim9458, i64 %rva11265)                   ; call prim_cons
  %rva11263 = call i64 @prim_cons(i64 %arg10035, i64 %rva11264)                      ; call prim_cons
  %cloptr14108 = inttoptr i64 %cont9456 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14109 = getelementptr inbounds i64, i64* %cloptr14108, i64 0                 ; &cloptr14108[0]
  %f14111 = load i64, i64* %i0ptr14109, align 8                                      ; load; *i0ptr14109
  %fptr14110 = inttoptr i64 %f14111 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14110(i64 %cont9456, i64 %rva11263)                 ; tail call
  ret void

else14107:
  %a9304 = call i64 @prim_car(i64 %mI0$args)                                         ; call prim_car
  %a9305 = call i64 @prim_cdr(i64 %mI0$args)                                         ; call prim_cdr
  %cloptr14112 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr14113 = getelementptr inbounds i64, i64* %cloptr14112, i64 0                  ; &cloptr14112[0]
  %f14114 = ptrtoint void(i64,i64)* @lam13057 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14114, i64* %eptr14113                                                 ; store fptr
  %arg10041 = ptrtoint i64* %cloptr14112 to i64                                      ; closure cast; i64* -> i64
  %rva11278 = add i64 0, 0                                                           ; quoted ()
  %rva11277 = call i64 @prim_cons(i64 %a9305, i64 %rva11278)                         ; call prim_cons
  %rva11276 = call i64 @prim_cons(i64 %a9304, i64 %rva11277)                         ; call prim_cons
  %rva11275 = call i64 @prim_cons(i64 %arg10041, i64 %rva11276)                      ; call prim_cons
  %rva11274 = call i64 @prim_cons(i64 %cont9456, i64 %rva11275)                      ; call prim_cons
  %cloptr14115 = inttoptr i64 %X5k$_37foldl1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr14116 = getelementptr inbounds i64, i64* %cloptr14115, i64 0                 ; &cloptr14115[0]
  %f14118 = load i64, i64* %i0ptr14116, align 8                                      ; load; *i0ptr14116
  %fptr14117 = inttoptr i64 %f14118 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14117(i64 %X5k$_37foldl1, i64 %rva11274)            ; tail call
  ret void
}


define void @lam13057(i64 %env13058, i64 %rvp11273) {
  %cont9459 = call i64 @prim_car(i64 %rvp11273)                                      ; call prim_car
  %rvp11272 = call i64 @prim_cdr(i64 %rvp11273)                                      ; call prim_cdr
  %DSJ$n = call i64 @prim_car(i64 %rvp11272)                                         ; call prim_car
  %rvp11271 = call i64 @prim_cdr(i64 %rvp11272)                                      ; call prim_cdr
  %GbW$v = call i64 @prim_car(i64 %rvp11271)                                         ; call prim_car
  %na11267 = call i64 @prim_cdr(i64 %rvp11271)                                       ; call prim_cdr
  %retprim9460 = call i64 @prim__47(i64 %GbW$v, i64 %DSJ$n)                          ; call prim__47
  %arg10047 = add i64 0, 0                                                           ; quoted ()
  %rva11270 = add i64 0, 0                                                           ; quoted ()
  %rva11269 = call i64 @prim_cons(i64 %retprim9460, i64 %rva11270)                   ; call prim_cons
  %rva11268 = call i64 @prim_cons(i64 %arg10047, i64 %rva11269)                      ; call prim_cons
  %cloptr14119 = inttoptr i64 %cont9459 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14120 = getelementptr inbounds i64, i64* %cloptr14119, i64 0                 ; &cloptr14119[0]
  %f14122 = load i64, i64* %i0ptr14120, align 8                                      ; load; *i0ptr14120
  %fptr14121 = inttoptr i64 %f14122 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14121(i64 %cont9459, i64 %rva11268)                 ; tail call
  ret void
}


define void @lam13047(i64 %env13048, i64 %rvp11285) {
  %cont9461 = call i64 @prim_car(i64 %rvp11285)                                      ; call prim_car
  %rvp11284 = call i64 @prim_cdr(i64 %rvp11285)                                      ; call prim_cdr
  %GTB$x = call i64 @prim_car(i64 %rvp11284)                                         ; call prim_car
  %na11280 = call i64 @prim_cdr(i64 %rvp11284)                                       ; call prim_cdr
  %retprim9462 = call i64 @prim_car(i64 %GTB$x)                                      ; call prim_car
  %arg10051 = add i64 0, 0                                                           ; quoted ()
  %rva11283 = add i64 0, 0                                                           ; quoted ()
  %rva11282 = call i64 @prim_cons(i64 %retprim9462, i64 %rva11283)                   ; call prim_cons
  %rva11281 = call i64 @prim_cons(i64 %arg10051, i64 %rva11282)                      ; call prim_cons
  %cloptr14123 = inttoptr i64 %cont9461 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14124 = getelementptr inbounds i64, i64* %cloptr14123, i64 0                 ; &cloptr14123[0]
  %f14126 = load i64, i64* %i0ptr14124, align 8                                      ; load; *i0ptr14124
  %fptr14125 = inttoptr i64 %f14126 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14125(i64 %cont9461, i64 %rva11281)                 ; tail call
  ret void
}


define void @lam13043(i64 %env13044, i64 %rvp11292) {
  %cont9463 = call i64 @prim_car(i64 %rvp11292)                                      ; call prim_car
  %rvp11291 = call i64 @prim_cdr(i64 %rvp11292)                                      ; call prim_cdr
  %rwJ$x = call i64 @prim_car(i64 %rvp11291)                                         ; call prim_car
  %na11287 = call i64 @prim_cdr(i64 %rvp11291)                                       ; call prim_cdr
  %a9306 = call i64 @prim_cdr(i64 %rwJ$x)                                            ; call prim_cdr
  %retprim9464 = call i64 @prim_car(i64 %a9306)                                      ; call prim_car
  %arg10056 = add i64 0, 0                                                           ; quoted ()
  %rva11290 = add i64 0, 0                                                           ; quoted ()
  %rva11289 = call i64 @prim_cons(i64 %retprim9464, i64 %rva11290)                   ; call prim_cons
  %rva11288 = call i64 @prim_cons(i64 %arg10056, i64 %rva11289)                      ; call prim_cons
  %cloptr14127 = inttoptr i64 %cont9463 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14128 = getelementptr inbounds i64, i64* %cloptr14127, i64 0                 ; &cloptr14127[0]
  %f14130 = load i64, i64* %i0ptr14128, align 8                                      ; load; *i0ptr14128
  %fptr14129 = inttoptr i64 %f14130 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14129(i64 %cont9463, i64 %rva11288)                 ; tail call
  ret void
}


define void @lam13039(i64 %env13040, i64 %rvp11299) {
  %cont9465 = call i64 @prim_car(i64 %rvp11299)                                      ; call prim_car
  %rvp11298 = call i64 @prim_cdr(i64 %rvp11299)                                      ; call prim_cdr
  %xkv$x = call i64 @prim_car(i64 %rvp11298)                                         ; call prim_car
  %na11294 = call i64 @prim_cdr(i64 %rvp11298)                                       ; call prim_cdr
  %a9307 = call i64 @prim_cdr(i64 %xkv$x)                                            ; call prim_cdr
  %a9308 = call i64 @prim_cdr(i64 %a9307)                                            ; call prim_cdr
  %retprim9466 = call i64 @prim_car(i64 %a9308)                                      ; call prim_car
  %arg10062 = add i64 0, 0                                                           ; quoted ()
  %rva11297 = add i64 0, 0                                                           ; quoted ()
  %rva11296 = call i64 @prim_cons(i64 %retprim9466, i64 %rva11297)                   ; call prim_cons
  %rva11295 = call i64 @prim_cons(i64 %arg10062, i64 %rva11296)                      ; call prim_cons
  %cloptr14131 = inttoptr i64 %cont9465 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14132 = getelementptr inbounds i64, i64* %cloptr14131, i64 0                 ; &cloptr14131[0]
  %f14134 = load i64, i64* %i0ptr14132, align 8                                      ; load; *i0ptr14132
  %fptr14133 = inttoptr i64 %f14134 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14133(i64 %cont9465, i64 %rva11295)                 ; tail call
  ret void
}


define void @lam13035(i64 %env13036, i64 %rvp11306) {
  %cont9467 = call i64 @prim_car(i64 %rvp11306)                                      ; call prim_car
  %rvp11305 = call i64 @prim_cdr(i64 %rvp11306)                                      ; call prim_cdr
  %mq0$x = call i64 @prim_car(i64 %rvp11305)                                         ; call prim_car
  %na11301 = call i64 @prim_cdr(i64 %rvp11305)                                       ; call prim_cdr
  %a9309 = call i64 @prim_cdr(i64 %mq0$x)                                            ; call prim_cdr
  %a9310 = call i64 @prim_cdr(i64 %a9309)                                            ; call prim_cdr
  %a9311 = call i64 @prim_cdr(i64 %a9310)                                            ; call prim_cdr
  %retprim9468 = call i64 @prim_car(i64 %a9311)                                      ; call prim_car
  %arg10069 = add i64 0, 0                                                           ; quoted ()
  %rva11304 = add i64 0, 0                                                           ; quoted ()
  %rva11303 = call i64 @prim_cons(i64 %retprim9468, i64 %rva11304)                   ; call prim_cons
  %rva11302 = call i64 @prim_cons(i64 %arg10069, i64 %rva11303)                      ; call prim_cons
  %cloptr14135 = inttoptr i64 %cont9467 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14136 = getelementptr inbounds i64, i64* %cloptr14135, i64 0                 ; &cloptr14135[0]
  %f14138 = load i64, i64* %i0ptr14136, align 8                                      ; load; *i0ptr14136
  %fptr14137 = inttoptr i64 %f14138 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14137(i64 %cont9467, i64 %rva11302)                 ; tail call
  ret void
}


define void @lam13031(i64 %env13032, i64 %rvp11316) {
  %cont9469 = call i64 @prim_car(i64 %rvp11316)                                      ; call prim_car
  %rvp11315 = call i64 @prim_cdr(i64 %rvp11316)                                      ; call prim_cdr
  %YN2$p = call i64 @prim_car(i64 %rvp11315)                                         ; call prim_car
  %na11308 = call i64 @prim_cdr(i64 %rvp11315)                                       ; call prim_cdr
  %a9312 = call i64 @prim_cons_63(i64 %YN2$p)                                        ; call prim_cons_63
  %cmp14139 = icmp eq i64 %a9312, 15                                                 ; false?
  br i1 %cmp14139, label %else14141, label %then14140                                ; if

then14140:
  %a9313 = call i64 @prim_car(i64 %YN2$p)                                            ; call prim_car
  %arg10073 = call i64 @const_init_symbol(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @sym14142, i32 0, i32 0)); quoted string
  %retprim9470 = call i64 @prim_eq_63(i64 %a9313, i64 %arg10073)                     ; call prim_eq_63
  %arg10076 = add i64 0, 0                                                           ; quoted ()
  %rva11311 = add i64 0, 0                                                           ; quoted ()
  %rva11310 = call i64 @prim_cons(i64 %retprim9470, i64 %rva11311)                   ; call prim_cons
  %rva11309 = call i64 @prim_cons(i64 %arg10076, i64 %rva11310)                      ; call prim_cons
  %cloptr14143 = inttoptr i64 %cont9469 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14144 = getelementptr inbounds i64, i64* %cloptr14143, i64 0                 ; &cloptr14143[0]
  %f14146 = load i64, i64* %i0ptr14144, align 8                                      ; load; *i0ptr14144
  %fptr14145 = inttoptr i64 %f14146 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14145(i64 %cont9469, i64 %rva11309)                 ; tail call
  ret void

else14141:
  %arg10079 = add i64 0, 0                                                           ; quoted ()
  %arg10078 = call i64 @const_init_false()                                           ; quoted #f
  %rva11314 = add i64 0, 0                                                           ; quoted ()
  %rva11313 = call i64 @prim_cons(i64 %arg10078, i64 %rva11314)                      ; call prim_cons
  %rva11312 = call i64 @prim_cons(i64 %arg10079, i64 %rva11313)                      ; call prim_cons
  %cloptr14147 = inttoptr i64 %cont9469 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14148 = getelementptr inbounds i64, i64* %cloptr14147, i64 0                 ; &cloptr14147[0]
  %f14150 = load i64, i64* %i0ptr14148, align 8                                      ; load; *i0ptr14148
  %fptr14149 = inttoptr i64 %f14150 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14149(i64 %cont9469, i64 %rva11312)                 ; tail call
  ret void
}


define void @lam13023(i64 %env13024, i64 %Pry$lst9596) {
  %cont9595 = call i64 @prim_car(i64 %Pry$lst9596)                                   ; call prim_car
  %Pry$lst = call i64 @prim_cdr(i64 %Pry$lst9596)                                    ; call prim_cdr
  %arg10086 = add i64 0, 0                                                           ; quoted ()
  %rva11319 = add i64 0, 0                                                           ; quoted ()
  %rva11318 = call i64 @prim_cons(i64 %Pry$lst, i64 %rva11319)                       ; call prim_cons
  %rva11317 = call i64 @prim_cons(i64 %arg10086, i64 %rva11318)                      ; call prim_cons
  %cloptr14151 = inttoptr i64 %cont9595 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14152 = getelementptr inbounds i64, i64* %cloptr14151, i64 0                 ; &cloptr14151[0]
  %f14154 = load i64, i64* %i0ptr14152, align 8                                      ; load; *i0ptr14152
  %fptr14153 = inttoptr i64 %f14154 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14153(i64 %cont9595, i64 %rva11317)                 ; tail call
  ret void
}


define void @lam13019(i64 %env13020, i64 %rvp11964) {
  %envptr14155 = inttoptr i64 %env13020 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14156 = getelementptr inbounds i64, i64* %envptr14155, i64 3                ; &envptr14155[3]
  %ILf$_37drop = load i64, i64* %envptr14156, align 8                                ; load; *envptr14156
  %envptr14157 = inttoptr i64 %env13020 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14158 = getelementptr inbounds i64, i64* %envptr14157, i64 2                ; &envptr14157[2]
  %IA9$_37_62 = load i64, i64* %envptr14158, align 8                                 ; load; *envptr14158
  %envptr14159 = inttoptr i64 %env13020 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14160 = getelementptr inbounds i64, i64* %envptr14159, i64 1                ; &envptr14159[1]
  %ecz$_37length = load i64, i64* %envptr14160, align 8                              ; load; *envptr14160
  %_959471 = call i64 @prim_car(i64 %rvp11964)                                       ; call prim_car
  %rvp11963 = call i64 @prim_cdr(i64 %rvp11964)                                      ; call prim_cdr
  %hzL$_37raise_45handler = call i64 @prim_car(i64 %rvp11963)                        ; call prim_car
  %na11321 = call i64 @prim_cdr(i64 %rvp11963)                                       ; call prim_cdr
  %cloptr14161 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr14162 = getelementptr inbounds i64, i64* %cloptr14161, i64 0                  ; &cloptr14161[0]
  %f14163 = ptrtoint void(i64,i64)* @lam13017 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14163, i64* %eptr14162                                                 ; store fptr
  %arg10089 = ptrtoint i64* %cloptr14161 to i64                                      ; closure cast; i64* -> i64
  %cloptr14164 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr14166 = getelementptr inbounds i64, i64* %cloptr14164, i64 1                  ; &eptr14166[1]
  %eptr14167 = getelementptr inbounds i64, i64* %cloptr14164, i64 2                  ; &eptr14167[2]
  %eptr14168 = getelementptr inbounds i64, i64* %cloptr14164, i64 3                  ; &eptr14168[3]
  %eptr14169 = getelementptr inbounds i64, i64* %cloptr14164, i64 4                  ; &eptr14169[4]
  store i64 %hzL$_37raise_45handler, i64* %eptr14166                                 ; *eptr14166 = %hzL$_37raise_45handler
  store i64 %ecz$_37length, i64* %eptr14167                                          ; *eptr14167 = %ecz$_37length
  store i64 %IA9$_37_62, i64* %eptr14168                                             ; *eptr14168 = %IA9$_37_62
  store i64 %ILf$_37drop, i64* %eptr14169                                            ; *eptr14169 = %ILf$_37drop
  %eptr14165 = getelementptr inbounds i64, i64* %cloptr14164, i64 0                  ; &cloptr14164[0]
  %f14170 = ptrtoint void(i64,i64)* @lam13013 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14170, i64* %eptr14165                                                 ; store fptr
  %arg10088 = ptrtoint i64* %cloptr14164 to i64                                      ; closure cast; i64* -> i64
  %rva11962 = add i64 0, 0                                                           ; quoted ()
  %rva11961 = call i64 @prim_cons(i64 %arg10088, i64 %rva11962)                      ; call prim_cons
  %cloptr14171 = inttoptr i64 %arg10089 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14172 = getelementptr inbounds i64, i64* %cloptr14171, i64 0                 ; &cloptr14171[0]
  %f14174 = load i64, i64* %i0ptr14172, align 8                                      ; load; *i0ptr14172
  %fptr14173 = inttoptr i64 %f14174 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14173(i64 %arg10089, i64 %rva11961)                 ; tail call
  ret void
}


define void @lam13017(i64 %env13018, i64 %oEv$lst9594) {
  %cont9593 = call i64 @prim_car(i64 %oEv$lst9594)                                   ; call prim_car
  %oEv$lst = call i64 @prim_cdr(i64 %oEv$lst9594)                                    ; call prim_cdr
  %arg10093 = add i64 0, 0                                                           ; quoted ()
  %rva11324 = add i64 0, 0                                                           ; quoted ()
  %rva11323 = call i64 @prim_cons(i64 %oEv$lst, i64 %rva11324)                       ; call prim_cons
  %rva11322 = call i64 @prim_cons(i64 %arg10093, i64 %rva11323)                      ; call prim_cons
  %cloptr14175 = inttoptr i64 %cont9593 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14176 = getelementptr inbounds i64, i64* %cloptr14175, i64 0                 ; &cloptr14175[0]
  %f14178 = load i64, i64* %i0ptr14176, align 8                                      ; load; *i0ptr14176
  %fptr14177 = inttoptr i64 %f14178 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14177(i64 %cont9593, i64 %rva11322)                 ; tail call
  ret void
}


define void @lam13013(i64 %env13014, i64 %rvp11960) {
  %envptr14179 = inttoptr i64 %env13014 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14180 = getelementptr inbounds i64, i64* %envptr14179, i64 4                ; &envptr14179[4]
  %ILf$_37drop = load i64, i64* %envptr14180, align 8                                ; load; *envptr14180
  %envptr14181 = inttoptr i64 %env13014 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14182 = getelementptr inbounds i64, i64* %envptr14181, i64 3                ; &envptr14181[3]
  %IA9$_37_62 = load i64, i64* %envptr14182, align 8                                 ; load; *envptr14182
  %envptr14183 = inttoptr i64 %env13014 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14184 = getelementptr inbounds i64, i64* %envptr14183, i64 2                ; &envptr14183[2]
  %ecz$_37length = load i64, i64* %envptr14184, align 8                              ; load; *envptr14184
  %envptr14185 = inttoptr i64 %env13014 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14186 = getelementptr inbounds i64, i64* %envptr14185, i64 1                ; &envptr14185[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr14186, align 8                     ; load; *envptr14186
  %_959591 = call i64 @prim_car(i64 %rvp11960)                                       ; call prim_car
  %rvp11959 = call i64 @prim_cdr(i64 %rvp11960)                                      ; call prim_cdr
  %a9314 = call i64 @prim_car(i64 %rvp11959)                                         ; call prim_car
  %na11326 = call i64 @prim_cdr(i64 %rvp11959)                                       ; call prim_cdr
  %arg10096 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9592 = call i64 @prim_make_45vector(i64 %arg10096, i64 %a9314)             ; call prim_make_45vector
  %cloptr14187 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr14189 = getelementptr inbounds i64, i64* %cloptr14187, i64 1                  ; &eptr14189[1]
  %eptr14190 = getelementptr inbounds i64, i64* %cloptr14187, i64 2                  ; &eptr14190[2]
  %eptr14191 = getelementptr inbounds i64, i64* %cloptr14187, i64 3                  ; &eptr14191[3]
  %eptr14192 = getelementptr inbounds i64, i64* %cloptr14187, i64 4                  ; &eptr14192[4]
  store i64 %hzL$_37raise_45handler, i64* %eptr14189                                 ; *eptr14189 = %hzL$_37raise_45handler
  store i64 %ecz$_37length, i64* %eptr14190                                          ; *eptr14190 = %ecz$_37length
  store i64 %IA9$_37_62, i64* %eptr14191                                             ; *eptr14191 = %IA9$_37_62
  store i64 %ILf$_37drop, i64* %eptr14192                                            ; *eptr14192 = %ILf$_37drop
  %eptr14188 = getelementptr inbounds i64, i64* %cloptr14187, i64 0                  ; &cloptr14187[0]
  %f14193 = ptrtoint void(i64,i64)* @lam13010 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14193, i64* %eptr14188                                                 ; store fptr
  %arg10099 = ptrtoint i64* %cloptr14187 to i64                                      ; closure cast; i64* -> i64
  %arg10098 = add i64 0, 0                                                           ; quoted ()
  %rva11958 = add i64 0, 0                                                           ; quoted ()
  %rva11957 = call i64 @prim_cons(i64 %retprim9592, i64 %rva11958)                   ; call prim_cons
  %rva11956 = call i64 @prim_cons(i64 %arg10098, i64 %rva11957)                      ; call prim_cons
  %cloptr14194 = inttoptr i64 %arg10099 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14195 = getelementptr inbounds i64, i64* %cloptr14194, i64 0                 ; &cloptr14194[0]
  %f14197 = load i64, i64* %i0ptr14195, align 8                                      ; load; *i0ptr14195
  %fptr14196 = inttoptr i64 %f14197 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14196(i64 %arg10099, i64 %rva11956)                 ; tail call
  ret void
}


define void @lam13010(i64 %env13011, i64 %rvp11955) {
  %envptr14198 = inttoptr i64 %env13011 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14199 = getelementptr inbounds i64, i64* %envptr14198, i64 4                ; &envptr14198[4]
  %ILf$_37drop = load i64, i64* %envptr14199, align 8                                ; load; *envptr14199
  %envptr14200 = inttoptr i64 %env13011 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14201 = getelementptr inbounds i64, i64* %envptr14200, i64 3                ; &envptr14200[3]
  %IA9$_37_62 = load i64, i64* %envptr14201, align 8                                 ; load; *envptr14201
  %envptr14202 = inttoptr i64 %env13011 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14203 = getelementptr inbounds i64, i64* %envptr14202, i64 2                ; &envptr14202[2]
  %ecz$_37length = load i64, i64* %envptr14203, align 8                              ; load; *envptr14203
  %envptr14204 = inttoptr i64 %env13011 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14205 = getelementptr inbounds i64, i64* %envptr14204, i64 1                ; &envptr14204[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr14205, align 8                     ; load; *envptr14205
  %_959472 = call i64 @prim_car(i64 %rvp11955)                                       ; call prim_car
  %rvp11954 = call i64 @prim_cdr(i64 %rvp11955)                                      ; call prim_cdr
  %Sea$_37wind_45stack = call i64 @prim_car(i64 %rvp11954)                           ; call prim_car
  %na11328 = call i64 @prim_cdr(i64 %rvp11954)                                       ; call prim_cdr
  %cloptr14206 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14208 = getelementptr inbounds i64, i64* %cloptr14206, i64 1                  ; &eptr14208[1]
  %eptr14209 = getelementptr inbounds i64, i64* %cloptr14206, i64 2                  ; &eptr14209[2]
  %eptr14210 = getelementptr inbounds i64, i64* %cloptr14206, i64 3                  ; &eptr14210[3]
  store i64 %ecz$_37length, i64* %eptr14208                                          ; *eptr14208 = %ecz$_37length
  store i64 %IA9$_37_62, i64* %eptr14209                                             ; *eptr14209 = %IA9$_37_62
  store i64 %ILf$_37drop, i64* %eptr14210                                            ; *eptr14210 = %ILf$_37drop
  %eptr14207 = getelementptr inbounds i64, i64* %cloptr14206, i64 0                  ; &cloptr14206[0]
  %f14211 = ptrtoint void(i64,i64)* @lam13008 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14211, i64* %eptr14207                                                 ; store fptr
  %kcI$_37common_45tail = ptrtoint i64* %cloptr14206 to i64                          ; closure cast; i64* -> i64
  %cloptr14212 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr14214 = getelementptr inbounds i64, i64* %cloptr14212, i64 1                  ; &eptr14214[1]
  %eptr14215 = getelementptr inbounds i64, i64* %cloptr14212, i64 2                  ; &eptr14215[2]
  store i64 %Sea$_37wind_45stack, i64* %eptr14214                                    ; *eptr14214 = %Sea$_37wind_45stack
  store i64 %kcI$_37common_45tail, i64* %eptr14215                                   ; *eptr14215 = %kcI$_37common_45tail
  %eptr14213 = getelementptr inbounds i64, i64* %cloptr14212, i64 0                  ; &cloptr14212[0]
  %f14216 = ptrtoint void(i64,i64)* @lam12946 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14216, i64* %eptr14213                                                 ; store fptr
  %w8C$_37do_45wind = ptrtoint i64* %cloptr14212 to i64                              ; closure cast; i64* -> i64
  %cloptr14217 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr14218 = getelementptr inbounds i64, i64* %cloptr14217, i64 0                  ; &cloptr14217[0]
  %f14219 = ptrtoint void(i64,i64)* @lam12879 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14219, i64* %eptr14218                                                 ; store fptr
  %arg10285 = ptrtoint i64* %cloptr14217 to i64                                      ; closure cast; i64* -> i64
  %cloptr14220 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14222 = getelementptr inbounds i64, i64* %cloptr14220, i64 1                  ; &eptr14222[1]
  %eptr14223 = getelementptr inbounds i64, i64* %cloptr14220, i64 2                  ; &eptr14223[2]
  %eptr14224 = getelementptr inbounds i64, i64* %cloptr14220, i64 3                  ; &eptr14224[3]
  store i64 %hzL$_37raise_45handler, i64* %eptr14222                                 ; *eptr14222 = %hzL$_37raise_45handler
  store i64 %w8C$_37do_45wind, i64* %eptr14223                                       ; *eptr14223 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr14224                                    ; *eptr14224 = %Sea$_37wind_45stack
  %eptr14221 = getelementptr inbounds i64, i64* %cloptr14220, i64 0                  ; &cloptr14220[0]
  %f14225 = ptrtoint void(i64,i64)* @lam12875 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14225, i64* %eptr14221                                                 ; store fptr
  %arg10284 = ptrtoint i64* %cloptr14220 to i64                                      ; closure cast; i64* -> i64
  %rva11953 = add i64 0, 0                                                           ; quoted ()
  %rva11952 = call i64 @prim_cons(i64 %arg10284, i64 %rva11953)                      ; call prim_cons
  %cloptr14226 = inttoptr i64 %arg10285 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14227 = getelementptr inbounds i64, i64* %cloptr14226, i64 0                 ; &cloptr14226[0]
  %f14229 = load i64, i64* %i0ptr14227, align 8                                      ; load; *i0ptr14227
  %fptr14228 = inttoptr i64 %f14229 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14228(i64 %arg10285, i64 %rva11952)                 ; tail call
  ret void
}


define void @lam13008(i64 %env13009, i64 %rvp11460) {
  %envptr14230 = inttoptr i64 %env13009 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14231 = getelementptr inbounds i64, i64* %envptr14230, i64 3                ; &envptr14230[3]
  %ILf$_37drop = load i64, i64* %envptr14231, align 8                                ; load; *envptr14231
  %envptr14232 = inttoptr i64 %env13009 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14233 = getelementptr inbounds i64, i64* %envptr14232, i64 2                ; &envptr14232[2]
  %IA9$_37_62 = load i64, i64* %envptr14233, align 8                                 ; load; *envptr14233
  %envptr14234 = inttoptr i64 %env13009 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14235 = getelementptr inbounds i64, i64* %envptr14234, i64 1                ; &envptr14234[1]
  %ecz$_37length = load i64, i64* %envptr14235, align 8                              ; load; *envptr14235
  %cont9473 = call i64 @prim_car(i64 %rvp11460)                                      ; call prim_car
  %rvp11459 = call i64 @prim_cdr(i64 %rvp11460)                                      ; call prim_cdr
  %RzU$x = call i64 @prim_car(i64 %rvp11459)                                         ; call prim_car
  %rvp11458 = call i64 @prim_cdr(i64 %rvp11459)                                      ; call prim_cdr
  %YWM$y = call i64 @prim_car(i64 %rvp11458)                                         ; call prim_car
  %na11330 = call i64 @prim_cdr(i64 %rvp11458)                                       ; call prim_cdr
  %cloptr14236 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr14238 = getelementptr inbounds i64, i64* %cloptr14236, i64 1                  ; &eptr14238[1]
  %eptr14239 = getelementptr inbounds i64, i64* %cloptr14236, i64 2                  ; &eptr14239[2]
  %eptr14240 = getelementptr inbounds i64, i64* %cloptr14236, i64 3                  ; &eptr14240[3]
  %eptr14241 = getelementptr inbounds i64, i64* %cloptr14236, i64 4                  ; &eptr14241[4]
  %eptr14242 = getelementptr inbounds i64, i64* %cloptr14236, i64 5                  ; &eptr14242[5]
  %eptr14243 = getelementptr inbounds i64, i64* %cloptr14236, i64 6                  ; &eptr14243[6]
  store i64 %ecz$_37length, i64* %eptr14238                                          ; *eptr14238 = %ecz$_37length
  store i64 %YWM$y, i64* %eptr14239                                                  ; *eptr14239 = %YWM$y
  store i64 %IA9$_37_62, i64* %eptr14240                                             ; *eptr14240 = %IA9$_37_62
  store i64 %cont9473, i64* %eptr14241                                               ; *eptr14241 = %cont9473
  store i64 %ILf$_37drop, i64* %eptr14242                                            ; *eptr14242 = %ILf$_37drop
  store i64 %RzU$x, i64* %eptr14243                                                  ; *eptr14243 = %RzU$x
  %eptr14237 = getelementptr inbounds i64, i64* %cloptr14236, i64 0                  ; &cloptr14236[0]
  %f14244 = ptrtoint void(i64,i64)* @lam13006 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14244, i64* %eptr14237                                                 ; store fptr
  %arg10101 = ptrtoint i64* %cloptr14236 to i64                                      ; closure cast; i64* -> i64
  %rva11457 = add i64 0, 0                                                           ; quoted ()
  %rva11456 = call i64 @prim_cons(i64 %RzU$x, i64 %rva11457)                         ; call prim_cons
  %rva11455 = call i64 @prim_cons(i64 %arg10101, i64 %rva11456)                      ; call prim_cons
  %cloptr14245 = inttoptr i64 %ecz$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr14246 = getelementptr inbounds i64, i64* %cloptr14245, i64 0                 ; &cloptr14245[0]
  %f14248 = load i64, i64* %i0ptr14246, align 8                                      ; load; *i0ptr14246
  %fptr14247 = inttoptr i64 %f14248 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14247(i64 %ecz$_37length, i64 %rva11455)            ; tail call
  ret void
}


define void @lam13006(i64 %env13007, i64 %rvp11454) {
  %envptr14249 = inttoptr i64 %env13007 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14250 = getelementptr inbounds i64, i64* %envptr14249, i64 6                ; &envptr14249[6]
  %RzU$x = load i64, i64* %envptr14250, align 8                                      ; load; *envptr14250
  %envptr14251 = inttoptr i64 %env13007 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14252 = getelementptr inbounds i64, i64* %envptr14251, i64 5                ; &envptr14251[5]
  %ILf$_37drop = load i64, i64* %envptr14252, align 8                                ; load; *envptr14252
  %envptr14253 = inttoptr i64 %env13007 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14254 = getelementptr inbounds i64, i64* %envptr14253, i64 4                ; &envptr14253[4]
  %cont9473 = load i64, i64* %envptr14254, align 8                                   ; load; *envptr14254
  %envptr14255 = inttoptr i64 %env13007 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14256 = getelementptr inbounds i64, i64* %envptr14255, i64 3                ; &envptr14255[3]
  %IA9$_37_62 = load i64, i64* %envptr14256, align 8                                 ; load; *envptr14256
  %envptr14257 = inttoptr i64 %env13007 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14258 = getelementptr inbounds i64, i64* %envptr14257, i64 2                ; &envptr14257[2]
  %YWM$y = load i64, i64* %envptr14258, align 8                                      ; load; *envptr14258
  %envptr14259 = inttoptr i64 %env13007 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14260 = getelementptr inbounds i64, i64* %envptr14259, i64 1                ; &envptr14259[1]
  %ecz$_37length = load i64, i64* %envptr14260, align 8                              ; load; *envptr14260
  %_959474 = call i64 @prim_car(i64 %rvp11454)                                       ; call prim_car
  %rvp11453 = call i64 @prim_cdr(i64 %rvp11454)                                      ; call prim_cdr
  %y7h$lx = call i64 @prim_car(i64 %rvp11453)                                        ; call prim_car
  %na11332 = call i64 @prim_cdr(i64 %rvp11453)                                       ; call prim_cdr
  %cloptr14261 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr14263 = getelementptr inbounds i64, i64* %cloptr14261, i64 1                  ; &eptr14263[1]
  %eptr14264 = getelementptr inbounds i64, i64* %cloptr14261, i64 2                  ; &eptr14264[2]
  %eptr14265 = getelementptr inbounds i64, i64* %cloptr14261, i64 3                  ; &eptr14265[3]
  %eptr14266 = getelementptr inbounds i64, i64* %cloptr14261, i64 4                  ; &eptr14266[4]
  %eptr14267 = getelementptr inbounds i64, i64* %cloptr14261, i64 5                  ; &eptr14267[5]
  %eptr14268 = getelementptr inbounds i64, i64* %cloptr14261, i64 6                  ; &eptr14268[6]
  store i64 %YWM$y, i64* %eptr14263                                                  ; *eptr14263 = %YWM$y
  store i64 %IA9$_37_62, i64* %eptr14264                                             ; *eptr14264 = %IA9$_37_62
  store i64 %cont9473, i64* %eptr14265                                               ; *eptr14265 = %cont9473
  store i64 %y7h$lx, i64* %eptr14266                                                 ; *eptr14266 = %y7h$lx
  store i64 %ILf$_37drop, i64* %eptr14267                                            ; *eptr14267 = %ILf$_37drop
  store i64 %RzU$x, i64* %eptr14268                                                  ; *eptr14268 = %RzU$x
  %eptr14262 = getelementptr inbounds i64, i64* %cloptr14261, i64 0                  ; &cloptr14261[0]
  %f14269 = ptrtoint void(i64,i64)* @lam13004 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14269, i64* %eptr14262                                                 ; store fptr
  %arg10104 = ptrtoint i64* %cloptr14261 to i64                                      ; closure cast; i64* -> i64
  %rva11452 = add i64 0, 0                                                           ; quoted ()
  %rva11451 = call i64 @prim_cons(i64 %YWM$y, i64 %rva11452)                         ; call prim_cons
  %rva11450 = call i64 @prim_cons(i64 %arg10104, i64 %rva11451)                      ; call prim_cons
  %cloptr14270 = inttoptr i64 %ecz$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr14271 = getelementptr inbounds i64, i64* %cloptr14270, i64 0                 ; &cloptr14270[0]
  %f14273 = load i64, i64* %i0ptr14271, align 8                                      ; load; *i0ptr14271
  %fptr14272 = inttoptr i64 %f14273 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14272(i64 %ecz$_37length, i64 %rva11450)            ; tail call
  ret void
}


define void @lam13004(i64 %env13005, i64 %rvp11449) {
  %envptr14274 = inttoptr i64 %env13005 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14275 = getelementptr inbounds i64, i64* %envptr14274, i64 6                ; &envptr14274[6]
  %RzU$x = load i64, i64* %envptr14275, align 8                                      ; load; *envptr14275
  %envptr14276 = inttoptr i64 %env13005 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14277 = getelementptr inbounds i64, i64* %envptr14276, i64 5                ; &envptr14276[5]
  %ILf$_37drop = load i64, i64* %envptr14277, align 8                                ; load; *envptr14277
  %envptr14278 = inttoptr i64 %env13005 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14279 = getelementptr inbounds i64, i64* %envptr14278, i64 4                ; &envptr14278[4]
  %y7h$lx = load i64, i64* %envptr14279, align 8                                     ; load; *envptr14279
  %envptr14280 = inttoptr i64 %env13005 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14281 = getelementptr inbounds i64, i64* %envptr14280, i64 3                ; &envptr14280[3]
  %cont9473 = load i64, i64* %envptr14281, align 8                                   ; load; *envptr14281
  %envptr14282 = inttoptr i64 %env13005 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14283 = getelementptr inbounds i64, i64* %envptr14282, i64 2                ; &envptr14282[2]
  %IA9$_37_62 = load i64, i64* %envptr14283, align 8                                 ; load; *envptr14283
  %envptr14284 = inttoptr i64 %env13005 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14285 = getelementptr inbounds i64, i64* %envptr14284, i64 1                ; &envptr14284[1]
  %YWM$y = load i64, i64* %envptr14285, align 8                                      ; load; *envptr14285
  %_959475 = call i64 @prim_car(i64 %rvp11449)                                       ; call prim_car
  %rvp11448 = call i64 @prim_cdr(i64 %rvp11449)                                      ; call prim_cdr
  %YxT$ly = call i64 @prim_car(i64 %rvp11448)                                        ; call prim_car
  %na11334 = call i64 @prim_cdr(i64 %rvp11448)                                       ; call prim_cdr
  %cloptr14286 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr14287 = getelementptr inbounds i64, i64* %cloptr14286, i64 0                  ; &cloptr14286[0]
  %f14288 = ptrtoint void(i64,i64)* @lam13002 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14288, i64* %eptr14287                                                 ; store fptr
  %arg10107 = ptrtoint i64* %cloptr14286 to i64                                      ; closure cast; i64* -> i64
  %cloptr14289 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr14291 = getelementptr inbounds i64, i64* %cloptr14289, i64 1                  ; &eptr14291[1]
  %eptr14292 = getelementptr inbounds i64, i64* %cloptr14289, i64 2                  ; &eptr14292[2]
  %eptr14293 = getelementptr inbounds i64, i64* %cloptr14289, i64 3                  ; &eptr14293[3]
  %eptr14294 = getelementptr inbounds i64, i64* %cloptr14289, i64 4                  ; &eptr14294[4]
  %eptr14295 = getelementptr inbounds i64, i64* %cloptr14289, i64 5                  ; &eptr14295[5]
  %eptr14296 = getelementptr inbounds i64, i64* %cloptr14289, i64 6                  ; &eptr14296[6]
  %eptr14297 = getelementptr inbounds i64, i64* %cloptr14289, i64 7                  ; &eptr14297[7]
  store i64 %YWM$y, i64* %eptr14291                                                  ; *eptr14291 = %YWM$y
  store i64 %IA9$_37_62, i64* %eptr14292                                             ; *eptr14292 = %IA9$_37_62
  store i64 %cont9473, i64* %eptr14293                                               ; *eptr14293 = %cont9473
  store i64 %YxT$ly, i64* %eptr14294                                                 ; *eptr14294 = %YxT$ly
  store i64 %y7h$lx, i64* %eptr14295                                                 ; *eptr14295 = %y7h$lx
  store i64 %ILf$_37drop, i64* %eptr14296                                            ; *eptr14296 = %ILf$_37drop
  store i64 %RzU$x, i64* %eptr14297                                                  ; *eptr14297 = %RzU$x
  %eptr14290 = getelementptr inbounds i64, i64* %cloptr14289, i64 0                  ; &cloptr14289[0]
  %f14298 = ptrtoint void(i64,i64)* @lam12998 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14298, i64* %eptr14290                                                 ; store fptr
  %arg10106 = ptrtoint i64* %cloptr14289 to i64                                      ; closure cast; i64* -> i64
  %rva11447 = add i64 0, 0                                                           ; quoted ()
  %rva11446 = call i64 @prim_cons(i64 %arg10106, i64 %rva11447)                      ; call prim_cons
  %cloptr14299 = inttoptr i64 %arg10107 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14300 = getelementptr inbounds i64, i64* %cloptr14299, i64 0                 ; &cloptr14299[0]
  %f14302 = load i64, i64* %i0ptr14300, align 8                                      ; load; *i0ptr14300
  %fptr14301 = inttoptr i64 %f14302 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14301(i64 %arg10107, i64 %rva11446)                 ; tail call
  ret void
}


define void @lam13002(i64 %env13003, i64 %IOG$lst9485) {
  %cont9484 = call i64 @prim_car(i64 %IOG$lst9485)                                   ; call prim_car
  %IOG$lst = call i64 @prim_cdr(i64 %IOG$lst9485)                                    ; call prim_cdr
  %arg10111 = add i64 0, 0                                                           ; quoted ()
  %rva11337 = add i64 0, 0                                                           ; quoted ()
  %rva11336 = call i64 @prim_cons(i64 %IOG$lst, i64 %rva11337)                       ; call prim_cons
  %rva11335 = call i64 @prim_cons(i64 %arg10111, i64 %rva11336)                      ; call prim_cons
  %cloptr14303 = inttoptr i64 %cont9484 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14304 = getelementptr inbounds i64, i64* %cloptr14303, i64 0                 ; &cloptr14303[0]
  %f14306 = load i64, i64* %i0ptr14304, align 8                                      ; load; *i0ptr14304
  %fptr14305 = inttoptr i64 %f14306 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14305(i64 %cont9484, i64 %rva11335)                 ; tail call
  ret void
}


define void @lam12998(i64 %env12999, i64 %rvp11445) {
  %envptr14307 = inttoptr i64 %env12999 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14308 = getelementptr inbounds i64, i64* %envptr14307, i64 7                ; &envptr14307[7]
  %RzU$x = load i64, i64* %envptr14308, align 8                                      ; load; *envptr14308
  %envptr14309 = inttoptr i64 %env12999 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14310 = getelementptr inbounds i64, i64* %envptr14309, i64 6                ; &envptr14309[6]
  %ILf$_37drop = load i64, i64* %envptr14310, align 8                                ; load; *envptr14310
  %envptr14311 = inttoptr i64 %env12999 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14312 = getelementptr inbounds i64, i64* %envptr14311, i64 5                ; &envptr14311[5]
  %y7h$lx = load i64, i64* %envptr14312, align 8                                     ; load; *envptr14312
  %envptr14313 = inttoptr i64 %env12999 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14314 = getelementptr inbounds i64, i64* %envptr14313, i64 4                ; &envptr14313[4]
  %YxT$ly = load i64, i64* %envptr14314, align 8                                     ; load; *envptr14314
  %envptr14315 = inttoptr i64 %env12999 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14316 = getelementptr inbounds i64, i64* %envptr14315, i64 3                ; &envptr14315[3]
  %cont9473 = load i64, i64* %envptr14316, align 8                                   ; load; *envptr14316
  %envptr14317 = inttoptr i64 %env12999 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14318 = getelementptr inbounds i64, i64* %envptr14317, i64 2                ; &envptr14317[2]
  %IA9$_37_62 = load i64, i64* %envptr14318, align 8                                 ; load; *envptr14318
  %envptr14319 = inttoptr i64 %env12999 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14320 = getelementptr inbounds i64, i64* %envptr14319, i64 1                ; &envptr14319[1]
  %YWM$y = load i64, i64* %envptr14320, align 8                                      ; load; *envptr14320
  %_959482 = call i64 @prim_car(i64 %rvp11445)                                       ; call prim_car
  %rvp11444 = call i64 @prim_cdr(i64 %rvp11445)                                      ; call prim_cdr
  %a9315 = call i64 @prim_car(i64 %rvp11444)                                         ; call prim_car
  %na11339 = call i64 @prim_cdr(i64 %rvp11444)                                       ; call prim_cdr
  %arg10114 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9483 = call i64 @prim_make_45vector(i64 %arg10114, i64 %a9315)             ; call prim_make_45vector
  %cloptr14321 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr14323 = getelementptr inbounds i64, i64* %cloptr14321, i64 1                  ; &eptr14323[1]
  %eptr14324 = getelementptr inbounds i64, i64* %cloptr14321, i64 2                  ; &eptr14324[2]
  %eptr14325 = getelementptr inbounds i64, i64* %cloptr14321, i64 3                  ; &eptr14325[3]
  %eptr14326 = getelementptr inbounds i64, i64* %cloptr14321, i64 4                  ; &eptr14326[4]
  %eptr14327 = getelementptr inbounds i64, i64* %cloptr14321, i64 5                  ; &eptr14327[5]
  %eptr14328 = getelementptr inbounds i64, i64* %cloptr14321, i64 6                  ; &eptr14328[6]
  %eptr14329 = getelementptr inbounds i64, i64* %cloptr14321, i64 7                  ; &eptr14329[7]
  store i64 %YWM$y, i64* %eptr14323                                                  ; *eptr14323 = %YWM$y
  store i64 %IA9$_37_62, i64* %eptr14324                                             ; *eptr14324 = %IA9$_37_62
  store i64 %cont9473, i64* %eptr14325                                               ; *eptr14325 = %cont9473
  store i64 %YxT$ly, i64* %eptr14326                                                 ; *eptr14326 = %YxT$ly
  store i64 %y7h$lx, i64* %eptr14327                                                 ; *eptr14327 = %y7h$lx
  store i64 %ILf$_37drop, i64* %eptr14328                                            ; *eptr14328 = %ILf$_37drop
  store i64 %RzU$x, i64* %eptr14329                                                  ; *eptr14329 = %RzU$x
  %eptr14322 = getelementptr inbounds i64, i64* %cloptr14321, i64 0                  ; &cloptr14321[0]
  %f14330 = ptrtoint void(i64,i64)* @lam12995 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14330, i64* %eptr14322                                                 ; store fptr
  %arg10117 = ptrtoint i64* %cloptr14321 to i64                                      ; closure cast; i64* -> i64
  %arg10116 = add i64 0, 0                                                           ; quoted ()
  %rva11443 = add i64 0, 0                                                           ; quoted ()
  %rva11442 = call i64 @prim_cons(i64 %retprim9483, i64 %rva11443)                   ; call prim_cons
  %rva11441 = call i64 @prim_cons(i64 %arg10116, i64 %rva11442)                      ; call prim_cons
  %cloptr14331 = inttoptr i64 %arg10117 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14332 = getelementptr inbounds i64, i64* %cloptr14331, i64 0                 ; &cloptr14331[0]
  %f14334 = load i64, i64* %i0ptr14332, align 8                                      ; load; *i0ptr14332
  %fptr14333 = inttoptr i64 %f14334 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14333(i64 %arg10117, i64 %rva11441)                 ; tail call
  ret void
}


define void @lam12995(i64 %env12996, i64 %rvp11440) {
  %envptr14335 = inttoptr i64 %env12996 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14336 = getelementptr inbounds i64, i64* %envptr14335, i64 7                ; &envptr14335[7]
  %RzU$x = load i64, i64* %envptr14336, align 8                                      ; load; *envptr14336
  %envptr14337 = inttoptr i64 %env12996 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14338 = getelementptr inbounds i64, i64* %envptr14337, i64 6                ; &envptr14337[6]
  %ILf$_37drop = load i64, i64* %envptr14338, align 8                                ; load; *envptr14338
  %envptr14339 = inttoptr i64 %env12996 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14340 = getelementptr inbounds i64, i64* %envptr14339, i64 5                ; &envptr14339[5]
  %y7h$lx = load i64, i64* %envptr14340, align 8                                     ; load; *envptr14340
  %envptr14341 = inttoptr i64 %env12996 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14342 = getelementptr inbounds i64, i64* %envptr14341, i64 4                ; &envptr14341[4]
  %YxT$ly = load i64, i64* %envptr14342, align 8                                     ; load; *envptr14342
  %envptr14343 = inttoptr i64 %env12996 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14344 = getelementptr inbounds i64, i64* %envptr14343, i64 3                ; &envptr14343[3]
  %cont9473 = load i64, i64* %envptr14344, align 8                                   ; load; *envptr14344
  %envptr14345 = inttoptr i64 %env12996 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14346 = getelementptr inbounds i64, i64* %envptr14345, i64 2                ; &envptr14345[2]
  %IA9$_37_62 = load i64, i64* %envptr14346, align 8                                 ; load; *envptr14346
  %envptr14347 = inttoptr i64 %env12996 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14348 = getelementptr inbounds i64, i64* %envptr14347, i64 1                ; &envptr14347[1]
  %YWM$y = load i64, i64* %envptr14348, align 8                                      ; load; *envptr14348
  %_959476 = call i64 @prim_car(i64 %rvp11440)                                       ; call prim_car
  %rvp11439 = call i64 @prim_cdr(i64 %rvp11440)                                      ; call prim_cdr
  %LCV$loop = call i64 @prim_car(i64 %rvp11439)                                      ; call prim_car
  %na11341 = call i64 @prim_cdr(i64 %rvp11439)                                       ; call prim_cdr
  %arg10119 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %cloptr14349 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr14351 = getelementptr inbounds i64, i64* %cloptr14349, i64 1                  ; &eptr14351[1]
  store i64 %LCV$loop, i64* %eptr14351                                               ; *eptr14351 = %LCV$loop
  %eptr14350 = getelementptr inbounds i64, i64* %cloptr14349, i64 0                  ; &cloptr14349[0]
  %f14352 = ptrtoint void(i64,i64)* @lam12992 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14352, i64* %eptr14350                                                 ; store fptr
  %arg10118 = ptrtoint i64* %cloptr14349 to i64                                      ; closure cast; i64* -> i64
  %OfU$_959203 = call i64 @prim_vector_45set_33(i64 %LCV$loop, i64 %arg10119, i64 %arg10118); call prim_vector_45set_33
  %arg10134 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9320 = call i64 @prim_vector_45ref(i64 %LCV$loop, i64 %arg10134)                 ; call prim_vector_45ref
  %cloptr14353 = call i64* @alloc(i64 72)                                            ; malloc
  %eptr14355 = getelementptr inbounds i64, i64* %cloptr14353, i64 1                  ; &eptr14355[1]
  %eptr14356 = getelementptr inbounds i64, i64* %cloptr14353, i64 2                  ; &eptr14356[2]
  %eptr14357 = getelementptr inbounds i64, i64* %cloptr14353, i64 3                  ; &eptr14357[3]
  %eptr14358 = getelementptr inbounds i64, i64* %cloptr14353, i64 4                  ; &eptr14358[4]
  %eptr14359 = getelementptr inbounds i64, i64* %cloptr14353, i64 5                  ; &eptr14359[5]
  %eptr14360 = getelementptr inbounds i64, i64* %cloptr14353, i64 6                  ; &eptr14360[6]
  %eptr14361 = getelementptr inbounds i64, i64* %cloptr14353, i64 7                  ; &eptr14361[7]
  %eptr14362 = getelementptr inbounds i64, i64* %cloptr14353, i64 8                  ; &eptr14362[8]
  store i64 %YWM$y, i64* %eptr14355                                                  ; *eptr14355 = %YWM$y
  store i64 %IA9$_37_62, i64* %eptr14356                                             ; *eptr14356 = %IA9$_37_62
  store i64 %cont9473, i64* %eptr14357                                               ; *eptr14357 = %cont9473
  store i64 %a9320, i64* %eptr14358                                                  ; *eptr14358 = %a9320
  store i64 %YxT$ly, i64* %eptr14359                                                 ; *eptr14359 = %YxT$ly
  store i64 %y7h$lx, i64* %eptr14360                                                 ; *eptr14360 = %y7h$lx
  store i64 %ILf$_37drop, i64* %eptr14361                                            ; *eptr14361 = %ILf$_37drop
  store i64 %RzU$x, i64* %eptr14362                                                  ; *eptr14362 = %RzU$x
  %eptr14354 = getelementptr inbounds i64, i64* %cloptr14353, i64 0                  ; &cloptr14353[0]
  %f14363 = ptrtoint void(i64,i64)* @lam12985 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14363, i64* %eptr14354                                                 ; store fptr
  %arg10138 = ptrtoint i64* %cloptr14353 to i64                                      ; closure cast; i64* -> i64
  %rva11438 = add i64 0, 0                                                           ; quoted ()
  %rva11437 = call i64 @prim_cons(i64 %YxT$ly, i64 %rva11438)                        ; call prim_cons
  %rva11436 = call i64 @prim_cons(i64 %y7h$lx, i64 %rva11437)                        ; call prim_cons
  %rva11435 = call i64 @prim_cons(i64 %arg10138, i64 %rva11436)                      ; call prim_cons
  %cloptr14364 = inttoptr i64 %IA9$_37_62 to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr14365 = getelementptr inbounds i64, i64* %cloptr14364, i64 0                 ; &cloptr14364[0]
  %f14367 = load i64, i64* %i0ptr14365, align 8                                      ; load; *i0ptr14365
  %fptr14366 = inttoptr i64 %f14367 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14366(i64 %IA9$_37_62, i64 %rva11435)               ; tail call
  ret void
}


define void @lam12992(i64 %env12993, i64 %rvp11353) {
  %envptr14368 = inttoptr i64 %env12993 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14369 = getelementptr inbounds i64, i64* %envptr14368, i64 1                ; &envptr14368[1]
  %LCV$loop = load i64, i64* %envptr14369, align 8                                   ; load; *envptr14369
  %cont9477 = call i64 @prim_car(i64 %rvp11353)                                      ; call prim_car
  %rvp11352 = call i64 @prim_cdr(i64 %rvp11353)                                      ; call prim_cdr
  %ZyQ$x = call i64 @prim_car(i64 %rvp11352)                                         ; call prim_car
  %rvp11351 = call i64 @prim_cdr(i64 %rvp11352)                                      ; call prim_cdr
  %ujP$y = call i64 @prim_car(i64 %rvp11351)                                         ; call prim_car
  %na11343 = call i64 @prim_cdr(i64 %rvp11351)                                       ; call prim_cdr
  %a9316 = call i64 @prim_eq_63(i64 %ZyQ$x, i64 %ujP$y)                              ; call prim_eq_63
  %cmp14370 = icmp eq i64 %a9316, 15                                                 ; false?
  br i1 %cmp14370, label %else14372, label %then14371                                ; if

then14371:
  %arg10124 = add i64 0, 0                                                           ; quoted ()
  %rva11346 = add i64 0, 0                                                           ; quoted ()
  %rva11345 = call i64 @prim_cons(i64 %ZyQ$x, i64 %rva11346)                         ; call prim_cons
  %rva11344 = call i64 @prim_cons(i64 %arg10124, i64 %rva11345)                      ; call prim_cons
  %cloptr14373 = inttoptr i64 %cont9477 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14374 = getelementptr inbounds i64, i64* %cloptr14373, i64 0                 ; &cloptr14373[0]
  %f14376 = load i64, i64* %i0ptr14374, align 8                                      ; load; *i0ptr14374
  %fptr14375 = inttoptr i64 %f14376 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14375(i64 %cont9477, i64 %rva11344)                 ; tail call
  ret void

else14372:
  %arg10126 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9317 = call i64 @prim_vector_45ref(i64 %LCV$loop, i64 %arg10126)                 ; call prim_vector_45ref
  %a9318 = call i64 @prim_cdr(i64 %ZyQ$x)                                            ; call prim_cdr
  %a9319 = call i64 @prim_cdr(i64 %ujP$y)                                            ; call prim_cdr
  %rva11350 = add i64 0, 0                                                           ; quoted ()
  %rva11349 = call i64 @prim_cons(i64 %a9319, i64 %rva11350)                         ; call prim_cons
  %rva11348 = call i64 @prim_cons(i64 %a9318, i64 %rva11349)                         ; call prim_cons
  %rva11347 = call i64 @prim_cons(i64 %cont9477, i64 %rva11348)                      ; call prim_cons
  %cloptr14377 = inttoptr i64 %a9317 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14378 = getelementptr inbounds i64, i64* %cloptr14377, i64 0                 ; &cloptr14377[0]
  %f14380 = load i64, i64* %i0ptr14378, align 8                                      ; load; *i0ptr14378
  %fptr14379 = inttoptr i64 %f14380 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14379(i64 %a9317, i64 %rva11347)                    ; tail call
  ret void
}


define void @lam12985(i64 %env12986, i64 %rvp11434) {
  %envptr14381 = inttoptr i64 %env12986 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14382 = getelementptr inbounds i64, i64* %envptr14381, i64 8                ; &envptr14381[8]
  %RzU$x = load i64, i64* %envptr14382, align 8                                      ; load; *envptr14382
  %envptr14383 = inttoptr i64 %env12986 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14384 = getelementptr inbounds i64, i64* %envptr14383, i64 7                ; &envptr14383[7]
  %ILf$_37drop = load i64, i64* %envptr14384, align 8                                ; load; *envptr14384
  %envptr14385 = inttoptr i64 %env12986 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14386 = getelementptr inbounds i64, i64* %envptr14385, i64 6                ; &envptr14385[6]
  %y7h$lx = load i64, i64* %envptr14386, align 8                                     ; load; *envptr14386
  %envptr14387 = inttoptr i64 %env12986 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14388 = getelementptr inbounds i64, i64* %envptr14387, i64 5                ; &envptr14387[5]
  %YxT$ly = load i64, i64* %envptr14388, align 8                                     ; load; *envptr14388
  %envptr14389 = inttoptr i64 %env12986 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14390 = getelementptr inbounds i64, i64* %envptr14389, i64 4                ; &envptr14389[4]
  %a9320 = load i64, i64* %envptr14390, align 8                                      ; load; *envptr14390
  %envptr14391 = inttoptr i64 %env12986 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14392 = getelementptr inbounds i64, i64* %envptr14391, i64 3                ; &envptr14391[3]
  %cont9473 = load i64, i64* %envptr14392, align 8                                   ; load; *envptr14392
  %envptr14393 = inttoptr i64 %env12986 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14394 = getelementptr inbounds i64, i64* %envptr14393, i64 2                ; &envptr14393[2]
  %IA9$_37_62 = load i64, i64* %envptr14394, align 8                                 ; load; *envptr14394
  %envptr14395 = inttoptr i64 %env12986 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14396 = getelementptr inbounds i64, i64* %envptr14395, i64 1                ; &envptr14395[1]
  %YWM$y = load i64, i64* %envptr14396, align 8                                      ; load; *envptr14396
  %_959478 = call i64 @prim_car(i64 %rvp11434)                                       ; call prim_car
  %rvp11433 = call i64 @prim_cdr(i64 %rvp11434)                                      ; call prim_cdr
  %a9321 = call i64 @prim_car(i64 %rvp11433)                                         ; call prim_car
  %na11355 = call i64 @prim_cdr(i64 %rvp11433)                                       ; call prim_cdr
  %cmp14397 = icmp eq i64 %a9321, 15                                                 ; false?
  br i1 %cmp14397, label %else14399, label %then14398                                ; if

then14398:
  %a9322 = call i64 @prim__45(i64 %y7h$lx, i64 %YxT$ly)                              ; call prim__45
  %cloptr14400 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr14402 = getelementptr inbounds i64, i64* %cloptr14400, i64 1                  ; &eptr14402[1]
  %eptr14403 = getelementptr inbounds i64, i64* %cloptr14400, i64 2                  ; &eptr14403[2]
  %eptr14404 = getelementptr inbounds i64, i64* %cloptr14400, i64 3                  ; &eptr14404[3]
  %eptr14405 = getelementptr inbounds i64, i64* %cloptr14400, i64 4                  ; &eptr14405[4]
  %eptr14406 = getelementptr inbounds i64, i64* %cloptr14400, i64 5                  ; &eptr14406[5]
  %eptr14407 = getelementptr inbounds i64, i64* %cloptr14400, i64 6                  ; &eptr14407[6]
  %eptr14408 = getelementptr inbounds i64, i64* %cloptr14400, i64 7                  ; &eptr14408[7]
  store i64 %YWM$y, i64* %eptr14402                                                  ; *eptr14402 = %YWM$y
  store i64 %IA9$_37_62, i64* %eptr14403                                             ; *eptr14403 = %IA9$_37_62
  store i64 %cont9473, i64* %eptr14404                                               ; *eptr14404 = %cont9473
  store i64 %a9320, i64* %eptr14405                                                  ; *eptr14405 = %a9320
  store i64 %YxT$ly, i64* %eptr14406                                                 ; *eptr14406 = %YxT$ly
  store i64 %y7h$lx, i64* %eptr14407                                                 ; *eptr14407 = %y7h$lx
  store i64 %ILf$_37drop, i64* %eptr14408                                            ; *eptr14408 = %ILf$_37drop
  %eptr14401 = getelementptr inbounds i64, i64* %cloptr14400, i64 0                  ; &cloptr14400[0]
  %f14409 = ptrtoint void(i64,i64)* @lam12967 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14409, i64* %eptr14401                                                 ; store fptr
  %arg10144 = ptrtoint i64* %cloptr14400 to i64                                      ; closure cast; i64* -> i64
  %rva11394 = add i64 0, 0                                                           ; quoted ()
  %rva11393 = call i64 @prim_cons(i64 %a9322, i64 %rva11394)                         ; call prim_cons
  %rva11392 = call i64 @prim_cons(i64 %RzU$x, i64 %rva11393)                         ; call prim_cons
  %rva11391 = call i64 @prim_cons(i64 %arg10144, i64 %rva11392)                      ; call prim_cons
  %cloptr14410 = inttoptr i64 %ILf$_37drop to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr14411 = getelementptr inbounds i64, i64* %cloptr14410, i64 0                 ; &cloptr14410[0]
  %f14413 = load i64, i64* %i0ptr14411, align 8                                      ; load; *i0ptr14411
  %fptr14412 = inttoptr i64 %f14413 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14412(i64 %ILf$_37drop, i64 %rva11391)              ; tail call
  ret void

else14399:
  %cloptr14414 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr14416 = getelementptr inbounds i64, i64* %cloptr14414, i64 1                  ; &eptr14416[1]
  %eptr14417 = getelementptr inbounds i64, i64* %cloptr14414, i64 2                  ; &eptr14417[2]
  %eptr14418 = getelementptr inbounds i64, i64* %cloptr14414, i64 3                  ; &eptr14418[3]
  %eptr14419 = getelementptr inbounds i64, i64* %cloptr14414, i64 4                  ; &eptr14419[4]
  %eptr14420 = getelementptr inbounds i64, i64* %cloptr14414, i64 5                  ; &eptr14420[5]
  %eptr14421 = getelementptr inbounds i64, i64* %cloptr14414, i64 6                  ; &eptr14421[6]
  %eptr14422 = getelementptr inbounds i64, i64* %cloptr14414, i64 7                  ; &eptr14422[7]
  store i64 %YWM$y, i64* %eptr14416                                                  ; *eptr14416 = %YWM$y
  store i64 %IA9$_37_62, i64* %eptr14417                                             ; *eptr14417 = %IA9$_37_62
  store i64 %cont9473, i64* %eptr14418                                               ; *eptr14418 = %cont9473
  store i64 %a9320, i64* %eptr14419                                                  ; *eptr14419 = %a9320
  store i64 %YxT$ly, i64* %eptr14420                                                 ; *eptr14420 = %YxT$ly
  store i64 %y7h$lx, i64* %eptr14421                                                 ; *eptr14421 = %y7h$lx
  store i64 %ILf$_37drop, i64* %eptr14422                                            ; *eptr14422 = %ILf$_37drop
  %eptr14415 = getelementptr inbounds i64, i64* %cloptr14414, i64 0                  ; &cloptr14414[0]
  %f14423 = ptrtoint void(i64,i64)* @lam12983 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14423, i64* %eptr14415                                                 ; store fptr
  %arg10169 = ptrtoint i64* %cloptr14414 to i64                                      ; closure cast; i64* -> i64
  %arg10168 = add i64 0, 0                                                           ; quoted ()
  %rva11432 = add i64 0, 0                                                           ; quoted ()
  %rva11431 = call i64 @prim_cons(i64 %RzU$x, i64 %rva11432)                         ; call prim_cons
  %rva11430 = call i64 @prim_cons(i64 %arg10168, i64 %rva11431)                      ; call prim_cons
  %cloptr14424 = inttoptr i64 %arg10169 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14425 = getelementptr inbounds i64, i64* %cloptr14424, i64 0                 ; &cloptr14424[0]
  %f14427 = load i64, i64* %i0ptr14425, align 8                                      ; load; *i0ptr14425
  %fptr14426 = inttoptr i64 %f14427 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14426(i64 %arg10169, i64 %rva11430)                 ; tail call
  ret void
}


define void @lam12983(i64 %env12984, i64 %rvp11429) {
  %envptr14428 = inttoptr i64 %env12984 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14429 = getelementptr inbounds i64, i64* %envptr14428, i64 7                ; &envptr14428[7]
  %ILf$_37drop = load i64, i64* %envptr14429, align 8                                ; load; *envptr14429
  %envptr14430 = inttoptr i64 %env12984 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14431 = getelementptr inbounds i64, i64* %envptr14430, i64 6                ; &envptr14430[6]
  %y7h$lx = load i64, i64* %envptr14431, align 8                                     ; load; *envptr14431
  %envptr14432 = inttoptr i64 %env12984 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14433 = getelementptr inbounds i64, i64* %envptr14432, i64 5                ; &envptr14432[5]
  %YxT$ly = load i64, i64* %envptr14433, align 8                                     ; load; *envptr14433
  %envptr14434 = inttoptr i64 %env12984 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14435 = getelementptr inbounds i64, i64* %envptr14434, i64 4                ; &envptr14434[4]
  %a9320 = load i64, i64* %envptr14435, align 8                                      ; load; *envptr14435
  %envptr14436 = inttoptr i64 %env12984 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14437 = getelementptr inbounds i64, i64* %envptr14436, i64 3                ; &envptr14436[3]
  %cont9473 = load i64, i64* %envptr14437, align 8                                   ; load; *envptr14437
  %envptr14438 = inttoptr i64 %env12984 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14439 = getelementptr inbounds i64, i64* %envptr14438, i64 2                ; &envptr14438[2]
  %IA9$_37_62 = load i64, i64* %envptr14439, align 8                                 ; load; *envptr14439
  %envptr14440 = inttoptr i64 %env12984 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14441 = getelementptr inbounds i64, i64* %envptr14440, i64 1                ; &envptr14440[1]
  %YWM$y = load i64, i64* %envptr14441, align 8                                      ; load; *envptr14441
  %_959479 = call i64 @prim_car(i64 %rvp11429)                                       ; call prim_car
  %rvp11428 = call i64 @prim_cdr(i64 %rvp11429)                                      ; call prim_cdr
  %a9323 = call i64 @prim_car(i64 %rvp11428)                                         ; call prim_car
  %na11396 = call i64 @prim_cdr(i64 %rvp11428)                                       ; call prim_cdr
  %cloptr14442 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr14444 = getelementptr inbounds i64, i64* %cloptr14442, i64 1                  ; &eptr14444[1]
  %eptr14445 = getelementptr inbounds i64, i64* %cloptr14442, i64 2                  ; &eptr14445[2]
  %eptr14446 = getelementptr inbounds i64, i64* %cloptr14442, i64 3                  ; &eptr14446[3]
  %eptr14447 = getelementptr inbounds i64, i64* %cloptr14442, i64 4                  ; &eptr14447[4]
  %eptr14448 = getelementptr inbounds i64, i64* %cloptr14442, i64 5                  ; &eptr14448[5]
  %eptr14449 = getelementptr inbounds i64, i64* %cloptr14442, i64 6                  ; &eptr14449[6]
  %eptr14450 = getelementptr inbounds i64, i64* %cloptr14442, i64 7                  ; &eptr14450[7]
  store i64 %YWM$y, i64* %eptr14444                                                  ; *eptr14444 = %YWM$y
  store i64 %a9323, i64* %eptr14445                                                  ; *eptr14445 = %a9323
  store i64 %cont9473, i64* %eptr14446                                               ; *eptr14446 = %cont9473
  store i64 %a9320, i64* %eptr14447                                                  ; *eptr14447 = %a9320
  store i64 %YxT$ly, i64* %eptr14448                                                 ; *eptr14448 = %YxT$ly
  store i64 %y7h$lx, i64* %eptr14449                                                 ; *eptr14449 = %y7h$lx
  store i64 %ILf$_37drop, i64* %eptr14450                                            ; *eptr14450 = %ILf$_37drop
  %eptr14443 = getelementptr inbounds i64, i64* %cloptr14442, i64 0                  ; &cloptr14442[0]
  %f14451 = ptrtoint void(i64,i64)* @lam12981 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14451, i64* %eptr14443                                                 ; store fptr
  %arg10172 = ptrtoint i64* %cloptr14442 to i64                                      ; closure cast; i64* -> i64
  %rva11427 = add i64 0, 0                                                           ; quoted ()
  %rva11426 = call i64 @prim_cons(i64 %y7h$lx, i64 %rva11427)                        ; call prim_cons
  %rva11425 = call i64 @prim_cons(i64 %YxT$ly, i64 %rva11426)                        ; call prim_cons
  %rva11424 = call i64 @prim_cons(i64 %arg10172, i64 %rva11425)                      ; call prim_cons
  %cloptr14452 = inttoptr i64 %IA9$_37_62 to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr14453 = getelementptr inbounds i64, i64* %cloptr14452, i64 0                 ; &cloptr14452[0]
  %f14455 = load i64, i64* %i0ptr14453, align 8                                      ; load; *i0ptr14453
  %fptr14454 = inttoptr i64 %f14455 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14454(i64 %IA9$_37_62, i64 %rva11424)               ; tail call
  ret void
}


define void @lam12981(i64 %env12982, i64 %rvp11423) {
  %envptr14456 = inttoptr i64 %env12982 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14457 = getelementptr inbounds i64, i64* %envptr14456, i64 7                ; &envptr14456[7]
  %ILf$_37drop = load i64, i64* %envptr14457, align 8                                ; load; *envptr14457
  %envptr14458 = inttoptr i64 %env12982 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14459 = getelementptr inbounds i64, i64* %envptr14458, i64 6                ; &envptr14458[6]
  %y7h$lx = load i64, i64* %envptr14459, align 8                                     ; load; *envptr14459
  %envptr14460 = inttoptr i64 %env12982 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14461 = getelementptr inbounds i64, i64* %envptr14460, i64 5                ; &envptr14460[5]
  %YxT$ly = load i64, i64* %envptr14461, align 8                                     ; load; *envptr14461
  %envptr14462 = inttoptr i64 %env12982 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14463 = getelementptr inbounds i64, i64* %envptr14462, i64 4                ; &envptr14462[4]
  %a9320 = load i64, i64* %envptr14463, align 8                                      ; load; *envptr14463
  %envptr14464 = inttoptr i64 %env12982 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14465 = getelementptr inbounds i64, i64* %envptr14464, i64 3                ; &envptr14464[3]
  %cont9473 = load i64, i64* %envptr14465, align 8                                   ; load; *envptr14465
  %envptr14466 = inttoptr i64 %env12982 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14467 = getelementptr inbounds i64, i64* %envptr14466, i64 2                ; &envptr14466[2]
  %a9323 = load i64, i64* %envptr14467, align 8                                      ; load; *envptr14467
  %envptr14468 = inttoptr i64 %env12982 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14469 = getelementptr inbounds i64, i64* %envptr14468, i64 1                ; &envptr14468[1]
  %YWM$y = load i64, i64* %envptr14469, align 8                                      ; load; *envptr14469
  %_959480 = call i64 @prim_car(i64 %rvp11423)                                       ; call prim_car
  %rvp11422 = call i64 @prim_cdr(i64 %rvp11423)                                      ; call prim_cdr
  %a9324 = call i64 @prim_car(i64 %rvp11422)                                         ; call prim_car
  %na11398 = call i64 @prim_cdr(i64 %rvp11422)                                       ; call prim_cdr
  %cmp14470 = icmp eq i64 %a9324, 15                                                 ; false?
  br i1 %cmp14470, label %else14472, label %then14471                                ; if

then14471:
  %a9325 = call i64 @prim__45(i64 %YxT$ly, i64 %y7h$lx)                              ; call prim__45
  %cloptr14473 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14475 = getelementptr inbounds i64, i64* %cloptr14473, i64 1                  ; &eptr14475[1]
  %eptr14476 = getelementptr inbounds i64, i64* %cloptr14473, i64 2                  ; &eptr14476[2]
  %eptr14477 = getelementptr inbounds i64, i64* %cloptr14473, i64 3                  ; &eptr14477[3]
  store i64 %a9323, i64* %eptr14475                                                  ; *eptr14475 = %a9323
  store i64 %cont9473, i64* %eptr14476                                               ; *eptr14476 = %cont9473
  store i64 %a9320, i64* %eptr14477                                                  ; *eptr14477 = %a9320
  %eptr14474 = getelementptr inbounds i64, i64* %cloptr14473, i64 0                  ; &cloptr14473[0]
  %f14478 = ptrtoint void(i64,i64)* @lam12974 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14478, i64* %eptr14474                                                 ; store fptr
  %arg10178 = ptrtoint i64* %cloptr14473 to i64                                      ; closure cast; i64* -> i64
  %rva11410 = add i64 0, 0                                                           ; quoted ()
  %rva11409 = call i64 @prim_cons(i64 %a9325, i64 %rva11410)                         ; call prim_cons
  %rva11408 = call i64 @prim_cons(i64 %YWM$y, i64 %rva11409)                         ; call prim_cons
  %rva11407 = call i64 @prim_cons(i64 %arg10178, i64 %rva11408)                      ; call prim_cons
  %cloptr14479 = inttoptr i64 %ILf$_37drop to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr14480 = getelementptr inbounds i64, i64* %cloptr14479, i64 0                 ; &cloptr14479[0]
  %f14482 = load i64, i64* %i0ptr14480, align 8                                      ; load; *i0ptr14480
  %fptr14481 = inttoptr i64 %f14482 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14481(i64 %ILf$_37drop, i64 %rva11407)              ; tail call
  ret void

else14472:
  %cloptr14483 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14485 = getelementptr inbounds i64, i64* %cloptr14483, i64 1                  ; &eptr14485[1]
  %eptr14486 = getelementptr inbounds i64, i64* %cloptr14483, i64 2                  ; &eptr14486[2]
  %eptr14487 = getelementptr inbounds i64, i64* %cloptr14483, i64 3                  ; &eptr14487[3]
  store i64 %a9323, i64* %eptr14485                                                  ; *eptr14485 = %a9323
  store i64 %cont9473, i64* %eptr14486                                               ; *eptr14486 = %cont9473
  store i64 %a9320, i64* %eptr14487                                                  ; *eptr14487 = %a9320
  %eptr14484 = getelementptr inbounds i64, i64* %cloptr14483, i64 0                  ; &cloptr14483[0]
  %f14488 = ptrtoint void(i64,i64)* @lam12979 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14488, i64* %eptr14484                                                 ; store fptr
  %arg10186 = ptrtoint i64* %cloptr14483 to i64                                      ; closure cast; i64* -> i64
  %arg10185 = add i64 0, 0                                                           ; quoted ()
  %rva11421 = add i64 0, 0                                                           ; quoted ()
  %rva11420 = call i64 @prim_cons(i64 %YWM$y, i64 %rva11421)                         ; call prim_cons
  %rva11419 = call i64 @prim_cons(i64 %arg10185, i64 %rva11420)                      ; call prim_cons
  %cloptr14489 = inttoptr i64 %arg10186 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14490 = getelementptr inbounds i64, i64* %cloptr14489, i64 0                 ; &cloptr14489[0]
  %f14492 = load i64, i64* %i0ptr14490, align 8                                      ; load; *i0ptr14490
  %fptr14491 = inttoptr i64 %f14492 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14491(i64 %arg10186, i64 %rva11419)                 ; tail call
  ret void
}


define void @lam12979(i64 %env12980, i64 %rvp11418) {
  %envptr14493 = inttoptr i64 %env12980 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14494 = getelementptr inbounds i64, i64* %envptr14493, i64 3                ; &envptr14493[3]
  %a9320 = load i64, i64* %envptr14494, align 8                                      ; load; *envptr14494
  %envptr14495 = inttoptr i64 %env12980 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14496 = getelementptr inbounds i64, i64* %envptr14495, i64 2                ; &envptr14495[2]
  %cont9473 = load i64, i64* %envptr14496, align 8                                   ; load; *envptr14496
  %envptr14497 = inttoptr i64 %env12980 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14498 = getelementptr inbounds i64, i64* %envptr14497, i64 1                ; &envptr14497[1]
  %a9323 = load i64, i64* %envptr14498, align 8                                      ; load; *envptr14498
  %_959481 = call i64 @prim_car(i64 %rvp11418)                                       ; call prim_car
  %rvp11417 = call i64 @prim_cdr(i64 %rvp11418)                                      ; call prim_cdr
  %a9326 = call i64 @prim_car(i64 %rvp11417)                                         ; call prim_car
  %na11412 = call i64 @prim_cdr(i64 %rvp11417)                                       ; call prim_cdr
  %rva11416 = add i64 0, 0                                                           ; quoted ()
  %rva11415 = call i64 @prim_cons(i64 %a9326, i64 %rva11416)                         ; call prim_cons
  %rva11414 = call i64 @prim_cons(i64 %a9323, i64 %rva11415)                         ; call prim_cons
  %rva11413 = call i64 @prim_cons(i64 %cont9473, i64 %rva11414)                      ; call prim_cons
  %cloptr14499 = inttoptr i64 %a9320 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14500 = getelementptr inbounds i64, i64* %cloptr14499, i64 0                 ; &cloptr14499[0]
  %f14502 = load i64, i64* %i0ptr14500, align 8                                      ; load; *i0ptr14500
  %fptr14501 = inttoptr i64 %f14502 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14501(i64 %a9320, i64 %rva11413)                    ; tail call
  ret void
}


define void @lam12974(i64 %env12975, i64 %rvp11406) {
  %envptr14503 = inttoptr i64 %env12975 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14504 = getelementptr inbounds i64, i64* %envptr14503, i64 3                ; &envptr14503[3]
  %a9320 = load i64, i64* %envptr14504, align 8                                      ; load; *envptr14504
  %envptr14505 = inttoptr i64 %env12975 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14506 = getelementptr inbounds i64, i64* %envptr14505, i64 2                ; &envptr14505[2]
  %cont9473 = load i64, i64* %envptr14506, align 8                                   ; load; *envptr14506
  %envptr14507 = inttoptr i64 %env12975 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14508 = getelementptr inbounds i64, i64* %envptr14507, i64 1                ; &envptr14507[1]
  %a9323 = load i64, i64* %envptr14508, align 8                                      ; load; *envptr14508
  %_959481 = call i64 @prim_car(i64 %rvp11406)                                       ; call prim_car
  %rvp11405 = call i64 @prim_cdr(i64 %rvp11406)                                      ; call prim_cdr
  %a9326 = call i64 @prim_car(i64 %rvp11405)                                         ; call prim_car
  %na11400 = call i64 @prim_cdr(i64 %rvp11405)                                       ; call prim_cdr
  %rva11404 = add i64 0, 0                                                           ; quoted ()
  %rva11403 = call i64 @prim_cons(i64 %a9326, i64 %rva11404)                         ; call prim_cons
  %rva11402 = call i64 @prim_cons(i64 %a9323, i64 %rva11403)                         ; call prim_cons
  %rva11401 = call i64 @prim_cons(i64 %cont9473, i64 %rva11402)                      ; call prim_cons
  %cloptr14509 = inttoptr i64 %a9320 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14510 = getelementptr inbounds i64, i64* %cloptr14509, i64 0                 ; &cloptr14509[0]
  %f14512 = load i64, i64* %i0ptr14510, align 8                                      ; load; *i0ptr14510
  %fptr14511 = inttoptr i64 %f14512 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14511(i64 %a9320, i64 %rva11401)                    ; tail call
  ret void
}


define void @lam12967(i64 %env12968, i64 %rvp11390) {
  %envptr14513 = inttoptr i64 %env12968 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14514 = getelementptr inbounds i64, i64* %envptr14513, i64 7                ; &envptr14513[7]
  %ILf$_37drop = load i64, i64* %envptr14514, align 8                                ; load; *envptr14514
  %envptr14515 = inttoptr i64 %env12968 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14516 = getelementptr inbounds i64, i64* %envptr14515, i64 6                ; &envptr14515[6]
  %y7h$lx = load i64, i64* %envptr14516, align 8                                     ; load; *envptr14516
  %envptr14517 = inttoptr i64 %env12968 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14518 = getelementptr inbounds i64, i64* %envptr14517, i64 5                ; &envptr14517[5]
  %YxT$ly = load i64, i64* %envptr14518, align 8                                     ; load; *envptr14518
  %envptr14519 = inttoptr i64 %env12968 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14520 = getelementptr inbounds i64, i64* %envptr14519, i64 4                ; &envptr14519[4]
  %a9320 = load i64, i64* %envptr14520, align 8                                      ; load; *envptr14520
  %envptr14521 = inttoptr i64 %env12968 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14522 = getelementptr inbounds i64, i64* %envptr14521, i64 3                ; &envptr14521[3]
  %cont9473 = load i64, i64* %envptr14522, align 8                                   ; load; *envptr14522
  %envptr14523 = inttoptr i64 %env12968 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14524 = getelementptr inbounds i64, i64* %envptr14523, i64 2                ; &envptr14523[2]
  %IA9$_37_62 = load i64, i64* %envptr14524, align 8                                 ; load; *envptr14524
  %envptr14525 = inttoptr i64 %env12968 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14526 = getelementptr inbounds i64, i64* %envptr14525, i64 1                ; &envptr14525[1]
  %YWM$y = load i64, i64* %envptr14526, align 8                                      ; load; *envptr14526
  %_959479 = call i64 @prim_car(i64 %rvp11390)                                       ; call prim_car
  %rvp11389 = call i64 @prim_cdr(i64 %rvp11390)                                      ; call prim_cdr
  %a9323 = call i64 @prim_car(i64 %rvp11389)                                         ; call prim_car
  %na11357 = call i64 @prim_cdr(i64 %rvp11389)                                       ; call prim_cdr
  %cloptr14527 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr14529 = getelementptr inbounds i64, i64* %cloptr14527, i64 1                  ; &eptr14529[1]
  %eptr14530 = getelementptr inbounds i64, i64* %cloptr14527, i64 2                  ; &eptr14530[2]
  %eptr14531 = getelementptr inbounds i64, i64* %cloptr14527, i64 3                  ; &eptr14531[3]
  %eptr14532 = getelementptr inbounds i64, i64* %cloptr14527, i64 4                  ; &eptr14532[4]
  %eptr14533 = getelementptr inbounds i64, i64* %cloptr14527, i64 5                  ; &eptr14533[5]
  %eptr14534 = getelementptr inbounds i64, i64* %cloptr14527, i64 6                  ; &eptr14534[6]
  %eptr14535 = getelementptr inbounds i64, i64* %cloptr14527, i64 7                  ; &eptr14535[7]
  store i64 %YWM$y, i64* %eptr14529                                                  ; *eptr14529 = %YWM$y
  store i64 %a9323, i64* %eptr14530                                                  ; *eptr14530 = %a9323
  store i64 %cont9473, i64* %eptr14531                                               ; *eptr14531 = %cont9473
  store i64 %a9320, i64* %eptr14532                                                  ; *eptr14532 = %a9320
  store i64 %YxT$ly, i64* %eptr14533                                                 ; *eptr14533 = %YxT$ly
  store i64 %y7h$lx, i64* %eptr14534                                                 ; *eptr14534 = %y7h$lx
  store i64 %ILf$_37drop, i64* %eptr14535                                            ; *eptr14535 = %ILf$_37drop
  %eptr14528 = getelementptr inbounds i64, i64* %cloptr14527, i64 0                  ; &cloptr14527[0]
  %f14536 = ptrtoint void(i64,i64)* @lam12965 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14536, i64* %eptr14528                                                 ; store fptr
  %arg10148 = ptrtoint i64* %cloptr14527 to i64                                      ; closure cast; i64* -> i64
  %rva11388 = add i64 0, 0                                                           ; quoted ()
  %rva11387 = call i64 @prim_cons(i64 %y7h$lx, i64 %rva11388)                        ; call prim_cons
  %rva11386 = call i64 @prim_cons(i64 %YxT$ly, i64 %rva11387)                        ; call prim_cons
  %rva11385 = call i64 @prim_cons(i64 %arg10148, i64 %rva11386)                      ; call prim_cons
  %cloptr14537 = inttoptr i64 %IA9$_37_62 to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr14538 = getelementptr inbounds i64, i64* %cloptr14537, i64 0                 ; &cloptr14537[0]
  %f14540 = load i64, i64* %i0ptr14538, align 8                                      ; load; *i0ptr14538
  %fptr14539 = inttoptr i64 %f14540 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14539(i64 %IA9$_37_62, i64 %rva11385)               ; tail call
  ret void
}


define void @lam12965(i64 %env12966, i64 %rvp11384) {
  %envptr14541 = inttoptr i64 %env12966 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14542 = getelementptr inbounds i64, i64* %envptr14541, i64 7                ; &envptr14541[7]
  %ILf$_37drop = load i64, i64* %envptr14542, align 8                                ; load; *envptr14542
  %envptr14543 = inttoptr i64 %env12966 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14544 = getelementptr inbounds i64, i64* %envptr14543, i64 6                ; &envptr14543[6]
  %y7h$lx = load i64, i64* %envptr14544, align 8                                     ; load; *envptr14544
  %envptr14545 = inttoptr i64 %env12966 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14546 = getelementptr inbounds i64, i64* %envptr14545, i64 5                ; &envptr14545[5]
  %YxT$ly = load i64, i64* %envptr14546, align 8                                     ; load; *envptr14546
  %envptr14547 = inttoptr i64 %env12966 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14548 = getelementptr inbounds i64, i64* %envptr14547, i64 4                ; &envptr14547[4]
  %a9320 = load i64, i64* %envptr14548, align 8                                      ; load; *envptr14548
  %envptr14549 = inttoptr i64 %env12966 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14550 = getelementptr inbounds i64, i64* %envptr14549, i64 3                ; &envptr14549[3]
  %cont9473 = load i64, i64* %envptr14550, align 8                                   ; load; *envptr14550
  %envptr14551 = inttoptr i64 %env12966 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14552 = getelementptr inbounds i64, i64* %envptr14551, i64 2                ; &envptr14551[2]
  %a9323 = load i64, i64* %envptr14552, align 8                                      ; load; *envptr14552
  %envptr14553 = inttoptr i64 %env12966 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14554 = getelementptr inbounds i64, i64* %envptr14553, i64 1                ; &envptr14553[1]
  %YWM$y = load i64, i64* %envptr14554, align 8                                      ; load; *envptr14554
  %_959480 = call i64 @prim_car(i64 %rvp11384)                                       ; call prim_car
  %rvp11383 = call i64 @prim_cdr(i64 %rvp11384)                                      ; call prim_cdr
  %a9324 = call i64 @prim_car(i64 %rvp11383)                                         ; call prim_car
  %na11359 = call i64 @prim_cdr(i64 %rvp11383)                                       ; call prim_cdr
  %cmp14555 = icmp eq i64 %a9324, 15                                                 ; false?
  br i1 %cmp14555, label %else14557, label %then14556                                ; if

then14556:
  %a9325 = call i64 @prim__45(i64 %YxT$ly, i64 %y7h$lx)                              ; call prim__45
  %cloptr14558 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14560 = getelementptr inbounds i64, i64* %cloptr14558, i64 1                  ; &eptr14560[1]
  %eptr14561 = getelementptr inbounds i64, i64* %cloptr14558, i64 2                  ; &eptr14561[2]
  %eptr14562 = getelementptr inbounds i64, i64* %cloptr14558, i64 3                  ; &eptr14562[3]
  store i64 %a9323, i64* %eptr14560                                                  ; *eptr14560 = %a9323
  store i64 %cont9473, i64* %eptr14561                                               ; *eptr14561 = %cont9473
  store i64 %a9320, i64* %eptr14562                                                  ; *eptr14562 = %a9320
  %eptr14559 = getelementptr inbounds i64, i64* %cloptr14558, i64 0                  ; &cloptr14558[0]
  %f14563 = ptrtoint void(i64,i64)* @lam12958 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14563, i64* %eptr14559                                                 ; store fptr
  %arg10154 = ptrtoint i64* %cloptr14558 to i64                                      ; closure cast; i64* -> i64
  %rva11371 = add i64 0, 0                                                           ; quoted ()
  %rva11370 = call i64 @prim_cons(i64 %a9325, i64 %rva11371)                         ; call prim_cons
  %rva11369 = call i64 @prim_cons(i64 %YWM$y, i64 %rva11370)                         ; call prim_cons
  %rva11368 = call i64 @prim_cons(i64 %arg10154, i64 %rva11369)                      ; call prim_cons
  %cloptr14564 = inttoptr i64 %ILf$_37drop to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr14565 = getelementptr inbounds i64, i64* %cloptr14564, i64 0                 ; &cloptr14564[0]
  %f14567 = load i64, i64* %i0ptr14565, align 8                                      ; load; *i0ptr14565
  %fptr14566 = inttoptr i64 %f14567 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14566(i64 %ILf$_37drop, i64 %rva11368)              ; tail call
  ret void

else14557:
  %cloptr14568 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14570 = getelementptr inbounds i64, i64* %cloptr14568, i64 1                  ; &eptr14570[1]
  %eptr14571 = getelementptr inbounds i64, i64* %cloptr14568, i64 2                  ; &eptr14571[2]
  %eptr14572 = getelementptr inbounds i64, i64* %cloptr14568, i64 3                  ; &eptr14572[3]
  store i64 %a9323, i64* %eptr14570                                                  ; *eptr14570 = %a9323
  store i64 %cont9473, i64* %eptr14571                                               ; *eptr14571 = %cont9473
  store i64 %a9320, i64* %eptr14572                                                  ; *eptr14572 = %a9320
  %eptr14569 = getelementptr inbounds i64, i64* %cloptr14568, i64 0                  ; &cloptr14568[0]
  %f14573 = ptrtoint void(i64,i64)* @lam12963 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14573, i64* %eptr14569                                                 ; store fptr
  %arg10162 = ptrtoint i64* %cloptr14568 to i64                                      ; closure cast; i64* -> i64
  %arg10161 = add i64 0, 0                                                           ; quoted ()
  %rva11382 = add i64 0, 0                                                           ; quoted ()
  %rva11381 = call i64 @prim_cons(i64 %YWM$y, i64 %rva11382)                         ; call prim_cons
  %rva11380 = call i64 @prim_cons(i64 %arg10161, i64 %rva11381)                      ; call prim_cons
  %cloptr14574 = inttoptr i64 %arg10162 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14575 = getelementptr inbounds i64, i64* %cloptr14574, i64 0                 ; &cloptr14574[0]
  %f14577 = load i64, i64* %i0ptr14575, align 8                                      ; load; *i0ptr14575
  %fptr14576 = inttoptr i64 %f14577 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14576(i64 %arg10162, i64 %rva11380)                 ; tail call
  ret void
}


define void @lam12963(i64 %env12964, i64 %rvp11379) {
  %envptr14578 = inttoptr i64 %env12964 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14579 = getelementptr inbounds i64, i64* %envptr14578, i64 3                ; &envptr14578[3]
  %a9320 = load i64, i64* %envptr14579, align 8                                      ; load; *envptr14579
  %envptr14580 = inttoptr i64 %env12964 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14581 = getelementptr inbounds i64, i64* %envptr14580, i64 2                ; &envptr14580[2]
  %cont9473 = load i64, i64* %envptr14581, align 8                                   ; load; *envptr14581
  %envptr14582 = inttoptr i64 %env12964 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14583 = getelementptr inbounds i64, i64* %envptr14582, i64 1                ; &envptr14582[1]
  %a9323 = load i64, i64* %envptr14583, align 8                                      ; load; *envptr14583
  %_959481 = call i64 @prim_car(i64 %rvp11379)                                       ; call prim_car
  %rvp11378 = call i64 @prim_cdr(i64 %rvp11379)                                      ; call prim_cdr
  %a9326 = call i64 @prim_car(i64 %rvp11378)                                         ; call prim_car
  %na11373 = call i64 @prim_cdr(i64 %rvp11378)                                       ; call prim_cdr
  %rva11377 = add i64 0, 0                                                           ; quoted ()
  %rva11376 = call i64 @prim_cons(i64 %a9326, i64 %rva11377)                         ; call prim_cons
  %rva11375 = call i64 @prim_cons(i64 %a9323, i64 %rva11376)                         ; call prim_cons
  %rva11374 = call i64 @prim_cons(i64 %cont9473, i64 %rva11375)                      ; call prim_cons
  %cloptr14584 = inttoptr i64 %a9320 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14585 = getelementptr inbounds i64, i64* %cloptr14584, i64 0                 ; &cloptr14584[0]
  %f14587 = load i64, i64* %i0ptr14585, align 8                                      ; load; *i0ptr14585
  %fptr14586 = inttoptr i64 %f14587 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14586(i64 %a9320, i64 %rva11374)                    ; tail call
  ret void
}


define void @lam12958(i64 %env12959, i64 %rvp11367) {
  %envptr14588 = inttoptr i64 %env12959 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14589 = getelementptr inbounds i64, i64* %envptr14588, i64 3                ; &envptr14588[3]
  %a9320 = load i64, i64* %envptr14589, align 8                                      ; load; *envptr14589
  %envptr14590 = inttoptr i64 %env12959 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14591 = getelementptr inbounds i64, i64* %envptr14590, i64 2                ; &envptr14590[2]
  %cont9473 = load i64, i64* %envptr14591, align 8                                   ; load; *envptr14591
  %envptr14592 = inttoptr i64 %env12959 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14593 = getelementptr inbounds i64, i64* %envptr14592, i64 1                ; &envptr14592[1]
  %a9323 = load i64, i64* %envptr14593, align 8                                      ; load; *envptr14593
  %_959481 = call i64 @prim_car(i64 %rvp11367)                                       ; call prim_car
  %rvp11366 = call i64 @prim_cdr(i64 %rvp11367)                                      ; call prim_cdr
  %a9326 = call i64 @prim_car(i64 %rvp11366)                                         ; call prim_car
  %na11361 = call i64 @prim_cdr(i64 %rvp11366)                                       ; call prim_cdr
  %rva11365 = add i64 0, 0                                                           ; quoted ()
  %rva11364 = call i64 @prim_cons(i64 %a9326, i64 %rva11365)                         ; call prim_cons
  %rva11363 = call i64 @prim_cons(i64 %a9323, i64 %rva11364)                         ; call prim_cons
  %rva11362 = call i64 @prim_cons(i64 %cont9473, i64 %rva11363)                      ; call prim_cons
  %cloptr14594 = inttoptr i64 %a9320 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14595 = getelementptr inbounds i64, i64* %cloptr14594, i64 0                 ; &cloptr14594[0]
  %f14597 = load i64, i64* %i0ptr14595, align 8                                      ; load; *i0ptr14595
  %fptr14596 = inttoptr i64 %f14597 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14596(i64 %a9320, i64 %rva11362)                    ; tail call
  ret void
}


define void @lam12946(i64 %env12947, i64 %rvp11560) {
  %envptr14598 = inttoptr i64 %env12947 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14599 = getelementptr inbounds i64, i64* %envptr14598, i64 2                ; &envptr14598[2]
  %kcI$_37common_45tail = load i64, i64* %envptr14599, align 8                       ; load; *envptr14599
  %envptr14600 = inttoptr i64 %env12947 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14601 = getelementptr inbounds i64, i64* %envptr14600, i64 1                ; &envptr14600[1]
  %Sea$_37wind_45stack = load i64, i64* %envptr14601, align 8                        ; load; *envptr14601
  %cont9486 = call i64 @prim_car(i64 %rvp11560)                                      ; call prim_car
  %rvp11559 = call i64 @prim_cdr(i64 %rvp11560)                                      ; call prim_cdr
  %j24$new = call i64 @prim_car(i64 %rvp11559)                                       ; call prim_car
  %na11462 = call i64 @prim_cdr(i64 %rvp11559)                                       ; call prim_cdr
  %arg10191 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9327 = call i64 @prim_vector_45ref(i64 %Sea$_37wind_45stack, i64 %arg10191)      ; call prim_vector_45ref
  %cloptr14602 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14604 = getelementptr inbounds i64, i64* %cloptr14602, i64 1                  ; &eptr14604[1]
  %eptr14605 = getelementptr inbounds i64, i64* %cloptr14602, i64 2                  ; &eptr14605[2]
  %eptr14606 = getelementptr inbounds i64, i64* %cloptr14602, i64 3                  ; &eptr14606[3]
  store i64 %j24$new, i64* %eptr14604                                                ; *eptr14604 = %j24$new
  store i64 %Sea$_37wind_45stack, i64* %eptr14605                                    ; *eptr14605 = %Sea$_37wind_45stack
  store i64 %cont9486, i64* %eptr14606                                               ; *eptr14606 = %cont9486
  %eptr14603 = getelementptr inbounds i64, i64* %cloptr14602, i64 0                  ; &cloptr14602[0]
  %f14607 = ptrtoint void(i64,i64)* @lam12943 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14607, i64* %eptr14603                                                 ; store fptr
  %arg10195 = ptrtoint i64* %cloptr14602 to i64                                      ; closure cast; i64* -> i64
  %rva11558 = add i64 0, 0                                                           ; quoted ()
  %rva11557 = call i64 @prim_cons(i64 %a9327, i64 %rva11558)                         ; call prim_cons
  %rva11556 = call i64 @prim_cons(i64 %j24$new, i64 %rva11557)                       ; call prim_cons
  %rva11555 = call i64 @prim_cons(i64 %arg10195, i64 %rva11556)                      ; call prim_cons
  %cloptr14608 = inttoptr i64 %kcI$_37common_45tail to i64*                          ; closure/env cast; i64 -> i64*
  %i0ptr14609 = getelementptr inbounds i64, i64* %cloptr14608, i64 0                 ; &cloptr14608[0]
  %f14611 = load i64, i64* %i0ptr14609, align 8                                      ; load; *i0ptr14609
  %fptr14610 = inttoptr i64 %f14611 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14610(i64 %kcI$_37common_45tail, i64 %rva11555)     ; tail call
  ret void
}


define void @lam12943(i64 %env12944, i64 %rvp11554) {
  %envptr14612 = inttoptr i64 %env12944 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14613 = getelementptr inbounds i64, i64* %envptr14612, i64 3                ; &envptr14612[3]
  %cont9486 = load i64, i64* %envptr14613, align 8                                   ; load; *envptr14613
  %envptr14614 = inttoptr i64 %env12944 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14615 = getelementptr inbounds i64, i64* %envptr14614, i64 2                ; &envptr14614[2]
  %Sea$_37wind_45stack = load i64, i64* %envptr14615, align 8                        ; load; *envptr14615
  %envptr14616 = inttoptr i64 %env12944 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14617 = getelementptr inbounds i64, i64* %envptr14616, i64 1                ; &envptr14616[1]
  %j24$new = load i64, i64* %envptr14617, align 8                                    ; load; *envptr14617
  %_959487 = call i64 @prim_car(i64 %rvp11554)                                       ; call prim_car
  %rvp11553 = call i64 @prim_cdr(i64 %rvp11554)                                      ; call prim_cdr
  %hTj$tail = call i64 @prim_car(i64 %rvp11553)                                      ; call prim_car
  %na11464 = call i64 @prim_cdr(i64 %rvp11553)                                       ; call prim_cdr
  %cloptr14618 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr14619 = getelementptr inbounds i64, i64* %cloptr14618, i64 0                  ; &cloptr14618[0]
  %f14620 = ptrtoint void(i64,i64)* @lam12941 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14620, i64* %eptr14619                                                 ; store fptr
  %arg10198 = ptrtoint i64* %cloptr14618 to i64                                      ; closure cast; i64* -> i64
  %cloptr14621 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr14623 = getelementptr inbounds i64, i64* %cloptr14621, i64 1                  ; &eptr14623[1]
  %eptr14624 = getelementptr inbounds i64, i64* %cloptr14621, i64 2                  ; &eptr14624[2]
  %eptr14625 = getelementptr inbounds i64, i64* %cloptr14621, i64 3                  ; &eptr14625[3]
  %eptr14626 = getelementptr inbounds i64, i64* %cloptr14621, i64 4                  ; &eptr14626[4]
  store i64 %hTj$tail, i64* %eptr14623                                               ; *eptr14623 = %hTj$tail
  store i64 %j24$new, i64* %eptr14624                                                ; *eptr14624 = %j24$new
  store i64 %Sea$_37wind_45stack, i64* %eptr14625                                    ; *eptr14625 = %Sea$_37wind_45stack
  store i64 %cont9486, i64* %eptr14626                                               ; *eptr14626 = %cont9486
  %eptr14622 = getelementptr inbounds i64, i64* %cloptr14621, i64 0                  ; &cloptr14621[0]
  %f14627 = ptrtoint void(i64,i64)* @lam12937 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14627, i64* %eptr14622                                                 ; store fptr
  %arg10197 = ptrtoint i64* %cloptr14621 to i64                                      ; closure cast; i64* -> i64
  %rva11552 = add i64 0, 0                                                           ; quoted ()
  %rva11551 = call i64 @prim_cons(i64 %arg10197, i64 %rva11552)                      ; call prim_cons
  %cloptr14628 = inttoptr i64 %arg10198 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14629 = getelementptr inbounds i64, i64* %cloptr14628, i64 0                 ; &cloptr14628[0]
  %f14631 = load i64, i64* %i0ptr14629, align 8                                      ; load; *i0ptr14629
  %fptr14630 = inttoptr i64 %f14631 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14630(i64 %arg10198, i64 %rva11551)                 ; tail call
  ret void
}


define void @lam12941(i64 %env12942, i64 %L8D$lst9508) {
  %cont9507 = call i64 @prim_car(i64 %L8D$lst9508)                                   ; call prim_car
  %L8D$lst = call i64 @prim_cdr(i64 %L8D$lst9508)                                    ; call prim_cdr
  %arg10202 = add i64 0, 0                                                           ; quoted ()
  %rva11467 = add i64 0, 0                                                           ; quoted ()
  %rva11466 = call i64 @prim_cons(i64 %L8D$lst, i64 %rva11467)                       ; call prim_cons
  %rva11465 = call i64 @prim_cons(i64 %arg10202, i64 %rva11466)                      ; call prim_cons
  %cloptr14632 = inttoptr i64 %cont9507 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14633 = getelementptr inbounds i64, i64* %cloptr14632, i64 0                 ; &cloptr14632[0]
  %f14635 = load i64, i64* %i0ptr14633, align 8                                      ; load; *i0ptr14633
  %fptr14634 = inttoptr i64 %f14635 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14634(i64 %cont9507, i64 %rva11465)                 ; tail call
  ret void
}


define void @lam12937(i64 %env12938, i64 %rvp11550) {
  %envptr14636 = inttoptr i64 %env12938 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14637 = getelementptr inbounds i64, i64* %envptr14636, i64 4                ; &envptr14636[4]
  %cont9486 = load i64, i64* %envptr14637, align 8                                   ; load; *envptr14637
  %envptr14638 = inttoptr i64 %env12938 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14639 = getelementptr inbounds i64, i64* %envptr14638, i64 3                ; &envptr14638[3]
  %Sea$_37wind_45stack = load i64, i64* %envptr14639, align 8                        ; load; *envptr14639
  %envptr14640 = inttoptr i64 %env12938 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14641 = getelementptr inbounds i64, i64* %envptr14640, i64 2                ; &envptr14640[2]
  %j24$new = load i64, i64* %envptr14641, align 8                                    ; load; *envptr14641
  %envptr14642 = inttoptr i64 %env12938 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14643 = getelementptr inbounds i64, i64* %envptr14642, i64 1                ; &envptr14642[1]
  %hTj$tail = load i64, i64* %envptr14643, align 8                                   ; load; *envptr14643
  %_959505 = call i64 @prim_car(i64 %rvp11550)                                       ; call prim_car
  %rvp11549 = call i64 @prim_cdr(i64 %rvp11550)                                      ; call prim_cdr
  %a9328 = call i64 @prim_car(i64 %rvp11549)                                         ; call prim_car
  %na11469 = call i64 @prim_cdr(i64 %rvp11549)                                       ; call prim_cdr
  %arg10205 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9506 = call i64 @prim_make_45vector(i64 %arg10205, i64 %a9328)             ; call prim_make_45vector
  %cloptr14644 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr14646 = getelementptr inbounds i64, i64* %cloptr14644, i64 1                  ; &eptr14646[1]
  %eptr14647 = getelementptr inbounds i64, i64* %cloptr14644, i64 2                  ; &eptr14647[2]
  %eptr14648 = getelementptr inbounds i64, i64* %cloptr14644, i64 3                  ; &eptr14648[3]
  %eptr14649 = getelementptr inbounds i64, i64* %cloptr14644, i64 4                  ; &eptr14649[4]
  store i64 %hTj$tail, i64* %eptr14646                                               ; *eptr14646 = %hTj$tail
  store i64 %j24$new, i64* %eptr14647                                                ; *eptr14647 = %j24$new
  store i64 %Sea$_37wind_45stack, i64* %eptr14648                                    ; *eptr14648 = %Sea$_37wind_45stack
  store i64 %cont9486, i64* %eptr14649                                               ; *eptr14649 = %cont9486
  %eptr14645 = getelementptr inbounds i64, i64* %cloptr14644, i64 0                  ; &cloptr14644[0]
  %f14650 = ptrtoint void(i64,i64)* @lam12934 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14650, i64* %eptr14645                                                 ; store fptr
  %arg10208 = ptrtoint i64* %cloptr14644 to i64                                      ; closure cast; i64* -> i64
  %arg10207 = add i64 0, 0                                                           ; quoted ()
  %rva11548 = add i64 0, 0                                                           ; quoted ()
  %rva11547 = call i64 @prim_cons(i64 %retprim9506, i64 %rva11548)                   ; call prim_cons
  %rva11546 = call i64 @prim_cons(i64 %arg10207, i64 %rva11547)                      ; call prim_cons
  %cloptr14651 = inttoptr i64 %arg10208 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14652 = getelementptr inbounds i64, i64* %cloptr14651, i64 0                 ; &cloptr14651[0]
  %f14654 = load i64, i64* %i0ptr14652, align 8                                      ; load; *i0ptr14652
  %fptr14653 = inttoptr i64 %f14654 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14653(i64 %arg10208, i64 %rva11546)                 ; tail call
  ret void
}


define void @lam12934(i64 %env12935, i64 %rvp11545) {
  %envptr14655 = inttoptr i64 %env12935 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14656 = getelementptr inbounds i64, i64* %envptr14655, i64 4                ; &envptr14655[4]
  %cont9486 = load i64, i64* %envptr14656, align 8                                   ; load; *envptr14656
  %envptr14657 = inttoptr i64 %env12935 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14658 = getelementptr inbounds i64, i64* %envptr14657, i64 3                ; &envptr14657[3]
  %Sea$_37wind_45stack = load i64, i64* %envptr14658, align 8                        ; load; *envptr14658
  %envptr14659 = inttoptr i64 %env12935 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14660 = getelementptr inbounds i64, i64* %envptr14659, i64 2                ; &envptr14659[2]
  %j24$new = load i64, i64* %envptr14660, align 8                                    ; load; *envptr14660
  %envptr14661 = inttoptr i64 %env12935 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14662 = getelementptr inbounds i64, i64* %envptr14661, i64 1                ; &envptr14661[1]
  %hTj$tail = load i64, i64* %envptr14662, align 8                                   ; load; *envptr14662
  %_959499 = call i64 @prim_car(i64 %rvp11545)                                       ; call prim_car
  %rvp11544 = call i64 @prim_cdr(i64 %rvp11545)                                      ; call prim_cdr
  %sYS$f = call i64 @prim_car(i64 %rvp11544)                                         ; call prim_car
  %na11471 = call i64 @prim_cdr(i64 %rvp11544)                                       ; call prim_cdr
  %arg10210 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %cloptr14663 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14665 = getelementptr inbounds i64, i64* %cloptr14663, i64 1                  ; &eptr14665[1]
  %eptr14666 = getelementptr inbounds i64, i64* %cloptr14663, i64 2                  ; &eptr14666[2]
  %eptr14667 = getelementptr inbounds i64, i64* %cloptr14663, i64 3                  ; &eptr14667[3]
  store i64 %hTj$tail, i64* %eptr14665                                               ; *eptr14665 = %hTj$tail
  store i64 %sYS$f, i64* %eptr14666                                                  ; *eptr14666 = %sYS$f
  store i64 %Sea$_37wind_45stack, i64* %eptr14667                                    ; *eptr14667 = %Sea$_37wind_45stack
  %eptr14664 = getelementptr inbounds i64, i64* %cloptr14663, i64 0                  ; &cloptr14663[0]
  %f14668 = ptrtoint void(i64,i64)* @lam12931 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14668, i64* %eptr14664                                                 ; store fptr
  %arg10209 = ptrtoint i64* %cloptr14663 to i64                                      ; closure cast; i64* -> i64
  %uJ5$_959205 = call i64 @prim_vector_45set_33(i64 %sYS$f, i64 %arg10210, i64 %arg10209); call prim_vector_45set_33
  %arg10235 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9336 = call i64 @prim_vector_45ref(i64 %sYS$f, i64 %arg10235)                    ; call prim_vector_45ref
  %arg10237 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9337 = call i64 @prim_vector_45ref(i64 %Sea$_37wind_45stack, i64 %arg10237)      ; call prim_vector_45ref
  %cloptr14669 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr14671 = getelementptr inbounds i64, i64* %cloptr14669, i64 1                  ; &eptr14671[1]
  %eptr14672 = getelementptr inbounds i64, i64* %cloptr14669, i64 2                  ; &eptr14672[2]
  %eptr14673 = getelementptr inbounds i64, i64* %cloptr14669, i64 3                  ; &eptr14673[3]
  %eptr14674 = getelementptr inbounds i64, i64* %cloptr14669, i64 4                  ; &eptr14674[4]
  store i64 %hTj$tail, i64* %eptr14671                                               ; *eptr14671 = %hTj$tail
  store i64 %j24$new, i64* %eptr14672                                                ; *eptr14672 = %j24$new
  store i64 %Sea$_37wind_45stack, i64* %eptr14673                                    ; *eptr14673 = %Sea$_37wind_45stack
  store i64 %cont9486, i64* %eptr14674                                               ; *eptr14674 = %cont9486
  %eptr14670 = getelementptr inbounds i64, i64* %cloptr14669, i64 0                  ; &cloptr14669[0]
  %f14675 = ptrtoint void(i64,i64)* @lam12915 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14675, i64* %eptr14670                                                 ; store fptr
  %arg10240 = ptrtoint i64* %cloptr14669 to i64                                      ; closure cast; i64* -> i64
  %rva11543 = add i64 0, 0                                                           ; quoted ()
  %rva11542 = call i64 @prim_cons(i64 %a9337, i64 %rva11543)                         ; call prim_cons
  %rva11541 = call i64 @prim_cons(i64 %arg10240, i64 %rva11542)                      ; call prim_cons
  %cloptr14676 = inttoptr i64 %a9336 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14677 = getelementptr inbounds i64, i64* %cloptr14676, i64 0                 ; &cloptr14676[0]
  %f14679 = load i64, i64* %i0ptr14677, align 8                                      ; load; *i0ptr14677
  %fptr14678 = inttoptr i64 %f14679 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14678(i64 %a9336, i64 %rva11541)                    ; tail call
  ret void
}


define void @lam12931(i64 %env12932, i64 %rvp11494) {
  %envptr14680 = inttoptr i64 %env12932 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14681 = getelementptr inbounds i64, i64* %envptr14680, i64 3                ; &envptr14680[3]
  %Sea$_37wind_45stack = load i64, i64* %envptr14681, align 8                        ; load; *envptr14681
  %envptr14682 = inttoptr i64 %env12932 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14683 = getelementptr inbounds i64, i64* %envptr14682, i64 2                ; &envptr14682[2]
  %sYS$f = load i64, i64* %envptr14683, align 8                                      ; load; *envptr14683
  %envptr14684 = inttoptr i64 %env12932 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14685 = getelementptr inbounds i64, i64* %envptr14684, i64 1                ; &envptr14684[1]
  %hTj$tail = load i64, i64* %envptr14685, align 8                                   ; load; *envptr14685
  %cont9500 = call i64 @prim_car(i64 %rvp11494)                                      ; call prim_car
  %rvp11493 = call i64 @prim_cdr(i64 %rvp11494)                                      ; call prim_cdr
  %rNw$l = call i64 @prim_car(i64 %rvp11493)                                         ; call prim_car
  %na11473 = call i64 @prim_cdr(i64 %rvp11493)                                       ; call prim_cdr
  %a9329 = call i64 @prim_eq_63(i64 %rNw$l, i64 %hTj$tail)                           ; call prim_eq_63
  %a9330 = call i64 @prim_not(i64 %a9329)                                            ; call prim_not
  %cmp14686 = icmp eq i64 %a9330, 15                                                 ; false?
  br i1 %cmp14686, label %else14688, label %then14687                                ; if

then14687:
  %a9331 = call i64 @prim_cdr(i64 %rNw$l)                                            ; call prim_cdr
  %arg10217 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9503 = call i64 @prim_vector_45set_33(i64 %Sea$_37wind_45stack, i64 %arg10217, i64 %a9331); call prim_vector_45set_33
  %cloptr14689 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14691 = getelementptr inbounds i64, i64* %cloptr14689, i64 1                  ; &eptr14691[1]
  %eptr14692 = getelementptr inbounds i64, i64* %cloptr14689, i64 2                  ; &eptr14692[2]
  %eptr14693 = getelementptr inbounds i64, i64* %cloptr14689, i64 3                  ; &eptr14693[3]
  store i64 %rNw$l, i64* %eptr14691                                                  ; *eptr14691 = %rNw$l
  store i64 %sYS$f, i64* %eptr14692                                                  ; *eptr14692 = %sYS$f
  store i64 %cont9500, i64* %eptr14693                                               ; *eptr14693 = %cont9500
  %eptr14690 = getelementptr inbounds i64, i64* %cloptr14689, i64 0                  ; &cloptr14689[0]
  %f14694 = ptrtoint void(i64,i64)* @lam12926 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14694, i64* %eptr14690                                                 ; store fptr
  %arg10221 = ptrtoint i64* %cloptr14689 to i64                                      ; closure cast; i64* -> i64
  %arg10220 = add i64 0, 0                                                           ; quoted ()
  %rva11489 = add i64 0, 0                                                           ; quoted ()
  %rva11488 = call i64 @prim_cons(i64 %retprim9503, i64 %rva11489)                   ; call prim_cons
  %rva11487 = call i64 @prim_cons(i64 %arg10220, i64 %rva11488)                      ; call prim_cons
  %cloptr14695 = inttoptr i64 %arg10221 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14696 = getelementptr inbounds i64, i64* %cloptr14695, i64 0                 ; &cloptr14695[0]
  %f14698 = load i64, i64* %i0ptr14696, align 8                                      ; load; *i0ptr14696
  %fptr14697 = inttoptr i64 %f14698 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14697(i64 %arg10221, i64 %rva11487)                 ; tail call
  ret void

else14688:
  %retprim9504 = call i64 @prim_void()                                               ; call prim_void
  %arg10233 = add i64 0, 0                                                           ; quoted ()
  %rva11492 = add i64 0, 0                                                           ; quoted ()
  %rva11491 = call i64 @prim_cons(i64 %retprim9504, i64 %rva11492)                   ; call prim_cons
  %rva11490 = call i64 @prim_cons(i64 %arg10233, i64 %rva11491)                      ; call prim_cons
  %cloptr14699 = inttoptr i64 %cont9500 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14700 = getelementptr inbounds i64, i64* %cloptr14699, i64 0                 ; &cloptr14699[0]
  %f14702 = load i64, i64* %i0ptr14700, align 8                                      ; load; *i0ptr14700
  %fptr14701 = inttoptr i64 %f14702 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14701(i64 %cont9500, i64 %rva11490)                 ; tail call
  ret void
}


define void @lam12926(i64 %env12927, i64 %rvp11486) {
  %envptr14703 = inttoptr i64 %env12927 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14704 = getelementptr inbounds i64, i64* %envptr14703, i64 3                ; &envptr14703[3]
  %cont9500 = load i64, i64* %envptr14704, align 8                                   ; load; *envptr14704
  %envptr14705 = inttoptr i64 %env12927 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14706 = getelementptr inbounds i64, i64* %envptr14705, i64 2                ; &envptr14705[2]
  %sYS$f = load i64, i64* %envptr14706, align 8                                      ; load; *envptr14706
  %envptr14707 = inttoptr i64 %env12927 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14708 = getelementptr inbounds i64, i64* %envptr14707, i64 1                ; &envptr14707[1]
  %rNw$l = load i64, i64* %envptr14708, align 8                                      ; load; *envptr14708
  %_959501 = call i64 @prim_car(i64 %rvp11486)                                       ; call prim_car
  %rvp11485 = call i64 @prim_cdr(i64 %rvp11486)                                      ; call prim_cdr
  %nmB$_959206 = call i64 @prim_car(i64 %rvp11485)                                   ; call prim_car
  %na11475 = call i64 @prim_cdr(i64 %rvp11485)                                       ; call prim_cdr
  %a9332 = call i64 @prim_car(i64 %rNw$l)                                            ; call prim_car
  %a9333 = call i64 @prim_cdr(i64 %a9332)                                            ; call prim_cdr
  %cloptr14709 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14711 = getelementptr inbounds i64, i64* %cloptr14709, i64 1                  ; &eptr14711[1]
  %eptr14712 = getelementptr inbounds i64, i64* %cloptr14709, i64 2                  ; &eptr14712[2]
  %eptr14713 = getelementptr inbounds i64, i64* %cloptr14709, i64 3                  ; &eptr14713[3]
  store i64 %rNw$l, i64* %eptr14711                                                  ; *eptr14711 = %rNw$l
  store i64 %sYS$f, i64* %eptr14712                                                  ; *eptr14712 = %sYS$f
  store i64 %cont9500, i64* %eptr14713                                               ; *eptr14713 = %cont9500
  %eptr14710 = getelementptr inbounds i64, i64* %cloptr14709, i64 0                  ; &cloptr14709[0]
  %f14714 = ptrtoint void(i64,i64)* @lam12924 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14714, i64* %eptr14710                                                 ; store fptr
  %arg10224 = ptrtoint i64* %cloptr14709 to i64                                      ; closure cast; i64* -> i64
  %rva11484 = add i64 0, 0                                                           ; quoted ()
  %rva11483 = call i64 @prim_cons(i64 %arg10224, i64 %rva11484)                      ; call prim_cons
  %cloptr14715 = inttoptr i64 %a9333 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14716 = getelementptr inbounds i64, i64* %cloptr14715, i64 0                 ; &cloptr14715[0]
  %f14718 = load i64, i64* %i0ptr14716, align 8                                      ; load; *i0ptr14716
  %fptr14717 = inttoptr i64 %f14718 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14717(i64 %a9333, i64 %rva11483)                    ; tail call
  ret void
}


define void @lam12924(i64 %env12925, i64 %rvp11482) {
  %envptr14719 = inttoptr i64 %env12925 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14720 = getelementptr inbounds i64, i64* %envptr14719, i64 3                ; &envptr14719[3]
  %cont9500 = load i64, i64* %envptr14720, align 8                                   ; load; *envptr14720
  %envptr14721 = inttoptr i64 %env12925 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14722 = getelementptr inbounds i64, i64* %envptr14721, i64 2                ; &envptr14721[2]
  %sYS$f = load i64, i64* %envptr14722, align 8                                      ; load; *envptr14722
  %envptr14723 = inttoptr i64 %env12925 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14724 = getelementptr inbounds i64, i64* %envptr14723, i64 1                ; &envptr14723[1]
  %rNw$l = load i64, i64* %envptr14724, align 8                                      ; load; *envptr14724
  %_959502 = call i64 @prim_car(i64 %rvp11482)                                       ; call prim_car
  %rvp11481 = call i64 @prim_cdr(i64 %rvp11482)                                      ; call prim_cdr
  %WAr$_959207 = call i64 @prim_car(i64 %rvp11481)                                   ; call prim_car
  %na11477 = call i64 @prim_cdr(i64 %rvp11481)                                       ; call prim_cdr
  %arg10226 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9334 = call i64 @prim_vector_45ref(i64 %sYS$f, i64 %arg10226)                    ; call prim_vector_45ref
  %a9335 = call i64 @prim_cdr(i64 %rNw$l)                                            ; call prim_cdr
  %rva11480 = add i64 0, 0                                                           ; quoted ()
  %rva11479 = call i64 @prim_cons(i64 %a9335, i64 %rva11480)                         ; call prim_cons
  %rva11478 = call i64 @prim_cons(i64 %cont9500, i64 %rva11479)                      ; call prim_cons
  %cloptr14725 = inttoptr i64 %a9334 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14726 = getelementptr inbounds i64, i64* %cloptr14725, i64 0                 ; &cloptr14725[0]
  %f14728 = load i64, i64* %i0ptr14726, align 8                                      ; load; *i0ptr14726
  %fptr14727 = inttoptr i64 %f14728 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14727(i64 %a9334, i64 %rva11478)                    ; tail call
  ret void
}


define void @lam12915(i64 %env12916, i64 %rvp11540) {
  %envptr14729 = inttoptr i64 %env12916 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14730 = getelementptr inbounds i64, i64* %envptr14729, i64 4                ; &envptr14729[4]
  %cont9486 = load i64, i64* %envptr14730, align 8                                   ; load; *envptr14730
  %envptr14731 = inttoptr i64 %env12916 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14732 = getelementptr inbounds i64, i64* %envptr14731, i64 3                ; &envptr14731[3]
  %Sea$_37wind_45stack = load i64, i64* %envptr14732, align 8                        ; load; *envptr14732
  %envptr14733 = inttoptr i64 %env12916 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14734 = getelementptr inbounds i64, i64* %envptr14733, i64 2                ; &envptr14733[2]
  %j24$new = load i64, i64* %envptr14734, align 8                                    ; load; *envptr14734
  %envptr14735 = inttoptr i64 %env12916 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14736 = getelementptr inbounds i64, i64* %envptr14735, i64 1                ; &envptr14735[1]
  %hTj$tail = load i64, i64* %envptr14736, align 8                                   ; load; *envptr14736
  %_959488 = call i64 @prim_car(i64 %rvp11540)                                       ; call prim_car
  %rvp11539 = call i64 @prim_cdr(i64 %rvp11540)                                      ; call prim_cdr
  %hE7$_959204 = call i64 @prim_car(i64 %rvp11539)                                   ; call prim_car
  %na11496 = call i64 @prim_cdr(i64 %rvp11539)                                       ; call prim_cdr
  %cloptr14737 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr14738 = getelementptr inbounds i64, i64* %cloptr14737, i64 0                  ; &cloptr14737[0]
  %f14739 = ptrtoint void(i64,i64)* @lam12913 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14739, i64* %eptr14738                                                 ; store fptr
  %arg10243 = ptrtoint i64* %cloptr14737 to i64                                      ; closure cast; i64* -> i64
  %cloptr14740 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr14742 = getelementptr inbounds i64, i64* %cloptr14740, i64 1                  ; &eptr14742[1]
  %eptr14743 = getelementptr inbounds i64, i64* %cloptr14740, i64 2                  ; &eptr14743[2]
  %eptr14744 = getelementptr inbounds i64, i64* %cloptr14740, i64 3                  ; &eptr14744[3]
  %eptr14745 = getelementptr inbounds i64, i64* %cloptr14740, i64 4                  ; &eptr14745[4]
  store i64 %hTj$tail, i64* %eptr14742                                               ; *eptr14742 = %hTj$tail
  store i64 %j24$new, i64* %eptr14743                                                ; *eptr14743 = %j24$new
  store i64 %Sea$_37wind_45stack, i64* %eptr14744                                    ; *eptr14744 = %Sea$_37wind_45stack
  store i64 %cont9486, i64* %eptr14745                                               ; *eptr14745 = %cont9486
  %eptr14741 = getelementptr inbounds i64, i64* %cloptr14740, i64 0                  ; &cloptr14740[0]
  %f14746 = ptrtoint void(i64,i64)* @lam12909 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14746, i64* %eptr14741                                                 ; store fptr
  %arg10242 = ptrtoint i64* %cloptr14740 to i64                                      ; closure cast; i64* -> i64
  %rva11538 = add i64 0, 0                                                           ; quoted ()
  %rva11537 = call i64 @prim_cons(i64 %arg10242, i64 %rva11538)                      ; call prim_cons
  %cloptr14747 = inttoptr i64 %arg10243 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14748 = getelementptr inbounds i64, i64* %cloptr14747, i64 0                 ; &cloptr14747[0]
  %f14750 = load i64, i64* %i0ptr14748, align 8                                      ; load; *i0ptr14748
  %fptr14749 = inttoptr i64 %f14750 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14749(i64 %arg10243, i64 %rva11537)                 ; tail call
  ret void
}


define void @lam12913(i64 %env12914, i64 %biM$lst9498) {
  %cont9497 = call i64 @prim_car(i64 %biM$lst9498)                                   ; call prim_car
  %biM$lst = call i64 @prim_cdr(i64 %biM$lst9498)                                    ; call prim_cdr
  %arg10247 = add i64 0, 0                                                           ; quoted ()
  %rva11499 = add i64 0, 0                                                           ; quoted ()
  %rva11498 = call i64 @prim_cons(i64 %biM$lst, i64 %rva11499)                       ; call prim_cons
  %rva11497 = call i64 @prim_cons(i64 %arg10247, i64 %rva11498)                      ; call prim_cons
  %cloptr14751 = inttoptr i64 %cont9497 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14752 = getelementptr inbounds i64, i64* %cloptr14751, i64 0                 ; &cloptr14751[0]
  %f14754 = load i64, i64* %i0ptr14752, align 8                                      ; load; *i0ptr14752
  %fptr14753 = inttoptr i64 %f14754 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14753(i64 %cont9497, i64 %rva11497)                 ; tail call
  ret void
}


define void @lam12909(i64 %env12910, i64 %rvp11536) {
  %envptr14755 = inttoptr i64 %env12910 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14756 = getelementptr inbounds i64, i64* %envptr14755, i64 4                ; &envptr14755[4]
  %cont9486 = load i64, i64* %envptr14756, align 8                                   ; load; *envptr14756
  %envptr14757 = inttoptr i64 %env12910 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14758 = getelementptr inbounds i64, i64* %envptr14757, i64 3                ; &envptr14757[3]
  %Sea$_37wind_45stack = load i64, i64* %envptr14758, align 8                        ; load; *envptr14758
  %envptr14759 = inttoptr i64 %env12910 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14760 = getelementptr inbounds i64, i64* %envptr14759, i64 2                ; &envptr14759[2]
  %j24$new = load i64, i64* %envptr14760, align 8                                    ; load; *envptr14760
  %envptr14761 = inttoptr i64 %env12910 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14762 = getelementptr inbounds i64, i64* %envptr14761, i64 1                ; &envptr14761[1]
  %hTj$tail = load i64, i64* %envptr14762, align 8                                   ; load; *envptr14762
  %_959495 = call i64 @prim_car(i64 %rvp11536)                                       ; call prim_car
  %rvp11535 = call i64 @prim_cdr(i64 %rvp11536)                                      ; call prim_cdr
  %a9338 = call i64 @prim_car(i64 %rvp11535)                                         ; call prim_car
  %na11501 = call i64 @prim_cdr(i64 %rvp11535)                                       ; call prim_cdr
  %arg10250 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9496 = call i64 @prim_make_45vector(i64 %arg10250, i64 %a9338)             ; call prim_make_45vector
  %cloptr14763 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr14765 = getelementptr inbounds i64, i64* %cloptr14763, i64 1                  ; &eptr14765[1]
  %eptr14766 = getelementptr inbounds i64, i64* %cloptr14763, i64 2                  ; &eptr14766[2]
  %eptr14767 = getelementptr inbounds i64, i64* %cloptr14763, i64 3                  ; &eptr14767[3]
  %eptr14768 = getelementptr inbounds i64, i64* %cloptr14763, i64 4                  ; &eptr14768[4]
  store i64 %hTj$tail, i64* %eptr14765                                               ; *eptr14765 = %hTj$tail
  store i64 %j24$new, i64* %eptr14766                                                ; *eptr14766 = %j24$new
  store i64 %Sea$_37wind_45stack, i64* %eptr14767                                    ; *eptr14767 = %Sea$_37wind_45stack
  store i64 %cont9486, i64* %eptr14768                                               ; *eptr14768 = %cont9486
  %eptr14764 = getelementptr inbounds i64, i64* %cloptr14763, i64 0                  ; &cloptr14763[0]
  %f14769 = ptrtoint void(i64,i64)* @lam12906 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14769, i64* %eptr14764                                                 ; store fptr
  %arg10253 = ptrtoint i64* %cloptr14763 to i64                                      ; closure cast; i64* -> i64
  %arg10252 = add i64 0, 0                                                           ; quoted ()
  %rva11534 = add i64 0, 0                                                           ; quoted ()
  %rva11533 = call i64 @prim_cons(i64 %retprim9496, i64 %rva11534)                   ; call prim_cons
  %rva11532 = call i64 @prim_cons(i64 %arg10252, i64 %rva11533)                      ; call prim_cons
  %cloptr14770 = inttoptr i64 %arg10253 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14771 = getelementptr inbounds i64, i64* %cloptr14770, i64 0                 ; &cloptr14770[0]
  %f14773 = load i64, i64* %i0ptr14771, align 8                                      ; load; *i0ptr14771
  %fptr14772 = inttoptr i64 %f14773 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14772(i64 %arg10253, i64 %rva11532)                 ; tail call
  ret void
}


define void @lam12906(i64 %env12907, i64 %rvp11531) {
  %envptr14774 = inttoptr i64 %env12907 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14775 = getelementptr inbounds i64, i64* %envptr14774, i64 4                ; &envptr14774[4]
  %cont9486 = load i64, i64* %envptr14775, align 8                                   ; load; *envptr14775
  %envptr14776 = inttoptr i64 %env12907 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14777 = getelementptr inbounds i64, i64* %envptr14776, i64 3                ; &envptr14776[3]
  %Sea$_37wind_45stack = load i64, i64* %envptr14777, align 8                        ; load; *envptr14777
  %envptr14778 = inttoptr i64 %env12907 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14779 = getelementptr inbounds i64, i64* %envptr14778, i64 2                ; &envptr14778[2]
  %j24$new = load i64, i64* %envptr14779, align 8                                    ; load; *envptr14779
  %envptr14780 = inttoptr i64 %env12907 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14781 = getelementptr inbounds i64, i64* %envptr14780, i64 1                ; &envptr14780[1]
  %hTj$tail = load i64, i64* %envptr14781, align 8                                   ; load; *envptr14781
  %_959489 = call i64 @prim_car(i64 %rvp11531)                                       ; call prim_car
  %rvp11530 = call i64 @prim_cdr(i64 %rvp11531)                                      ; call prim_cdr
  %x5I$f = call i64 @prim_car(i64 %rvp11530)                                         ; call prim_car
  %na11503 = call i64 @prim_cdr(i64 %rvp11530)                                       ; call prim_cdr
  %arg10255 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %cloptr14782 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14784 = getelementptr inbounds i64, i64* %cloptr14782, i64 1                  ; &eptr14784[1]
  %eptr14785 = getelementptr inbounds i64, i64* %cloptr14782, i64 2                  ; &eptr14785[2]
  %eptr14786 = getelementptr inbounds i64, i64* %cloptr14782, i64 3                  ; &eptr14786[3]
  store i64 %hTj$tail, i64* %eptr14784                                               ; *eptr14784 = %hTj$tail
  store i64 %x5I$f, i64* %eptr14785                                                  ; *eptr14785 = %x5I$f
  store i64 %Sea$_37wind_45stack, i64* %eptr14786                                    ; *eptr14786 = %Sea$_37wind_45stack
  %eptr14783 = getelementptr inbounds i64, i64* %cloptr14782, i64 0                  ; &cloptr14782[0]
  %f14787 = ptrtoint void(i64,i64)* @lam12903 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14787, i64* %eptr14783                                                 ; store fptr
  %arg10254 = ptrtoint i64* %cloptr14782 to i64                                      ; closure cast; i64* -> i64
  %URO$_959208 = call i64 @prim_vector_45set_33(i64 %x5I$f, i64 %arg10255, i64 %arg10254); call prim_vector_45set_33
  %arg10279 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9345 = call i64 @prim_vector_45ref(i64 %x5I$f, i64 %arg10279)                    ; call prim_vector_45ref
  %rva11529 = add i64 0, 0                                                           ; quoted ()
  %rva11528 = call i64 @prim_cons(i64 %j24$new, i64 %rva11529)                       ; call prim_cons
  %rva11527 = call i64 @prim_cons(i64 %cont9486, i64 %rva11528)                      ; call prim_cons
  %cloptr14788 = inttoptr i64 %a9345 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14789 = getelementptr inbounds i64, i64* %cloptr14788, i64 0                 ; &cloptr14788[0]
  %f14791 = load i64, i64* %i0ptr14789, align 8                                      ; load; *i0ptr14789
  %fptr14790 = inttoptr i64 %f14791 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14790(i64 %a9345, i64 %rva11527)                    ; tail call
  ret void
}


define void @lam12903(i64 %env12904, i64 %rvp11526) {
  %envptr14792 = inttoptr i64 %env12904 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14793 = getelementptr inbounds i64, i64* %envptr14792, i64 3                ; &envptr14792[3]
  %Sea$_37wind_45stack = load i64, i64* %envptr14793, align 8                        ; load; *envptr14793
  %envptr14794 = inttoptr i64 %env12904 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14795 = getelementptr inbounds i64, i64* %envptr14794, i64 2                ; &envptr14794[2]
  %x5I$f = load i64, i64* %envptr14795, align 8                                      ; load; *envptr14795
  %envptr14796 = inttoptr i64 %env12904 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14797 = getelementptr inbounds i64, i64* %envptr14796, i64 1                ; &envptr14796[1]
  %hTj$tail = load i64, i64* %envptr14797, align 8                                   ; load; *envptr14797
  %cont9490 = call i64 @prim_car(i64 %rvp11526)                                      ; call prim_car
  %rvp11525 = call i64 @prim_cdr(i64 %rvp11526)                                      ; call prim_cdr
  %l1t$l = call i64 @prim_car(i64 %rvp11525)                                         ; call prim_car
  %na11505 = call i64 @prim_cdr(i64 %rvp11525)                                       ; call prim_cdr
  %a9339 = call i64 @prim_eq_63(i64 %l1t$l, i64 %hTj$tail)                           ; call prim_eq_63
  %a9340 = call i64 @prim_not(i64 %a9339)                                            ; call prim_not
  %cmp14798 = icmp eq i64 %a9340, 15                                                 ; false?
  br i1 %cmp14798, label %else14800, label %then14799                                ; if

then14799:
  %arg10260 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9341 = call i64 @prim_vector_45ref(i64 %x5I$f, i64 %arg10260)                    ; call prim_vector_45ref
  %a9342 = call i64 @prim_cdr(i64 %l1t$l)                                            ; call prim_cdr
  %cloptr14801 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14803 = getelementptr inbounds i64, i64* %cloptr14801, i64 1                  ; &eptr14803[1]
  %eptr14804 = getelementptr inbounds i64, i64* %cloptr14801, i64 2                  ; &eptr14804[2]
  %eptr14805 = getelementptr inbounds i64, i64* %cloptr14801, i64 3                  ; &eptr14805[3]
  store i64 %l1t$l, i64* %eptr14803                                                  ; *eptr14803 = %l1t$l
  store i64 %Sea$_37wind_45stack, i64* %eptr14804                                    ; *eptr14804 = %Sea$_37wind_45stack
  store i64 %cont9490, i64* %eptr14805                                               ; *eptr14805 = %cont9490
  %eptr14802 = getelementptr inbounds i64, i64* %cloptr14801, i64 0                  ; &cloptr14801[0]
  %f14806 = ptrtoint void(i64,i64)* @lam12898 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14806, i64* %eptr14802                                                 ; store fptr
  %arg10264 = ptrtoint i64* %cloptr14801 to i64                                      ; closure cast; i64* -> i64
  %rva11521 = add i64 0, 0                                                           ; quoted ()
  %rva11520 = call i64 @prim_cons(i64 %a9342, i64 %rva11521)                         ; call prim_cons
  %rva11519 = call i64 @prim_cons(i64 %arg10264, i64 %rva11520)                      ; call prim_cons
  %cloptr14807 = inttoptr i64 %a9341 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14808 = getelementptr inbounds i64, i64* %cloptr14807, i64 0                 ; &cloptr14807[0]
  %f14810 = load i64, i64* %i0ptr14808, align 8                                      ; load; *i0ptr14808
  %fptr14809 = inttoptr i64 %f14810 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14809(i64 %a9341, i64 %rva11519)                    ; tail call
  ret void

else14800:
  %retprim9494 = call i64 @prim_void()                                               ; call prim_void
  %arg10277 = add i64 0, 0                                                           ; quoted ()
  %rva11524 = add i64 0, 0                                                           ; quoted ()
  %rva11523 = call i64 @prim_cons(i64 %retprim9494, i64 %rva11524)                   ; call prim_cons
  %rva11522 = call i64 @prim_cons(i64 %arg10277, i64 %rva11523)                      ; call prim_cons
  %cloptr14811 = inttoptr i64 %cont9490 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14812 = getelementptr inbounds i64, i64* %cloptr14811, i64 0                 ; &cloptr14811[0]
  %f14814 = load i64, i64* %i0ptr14812, align 8                                      ; load; *i0ptr14812
  %fptr14813 = inttoptr i64 %f14814 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14813(i64 %cont9490, i64 %rva11522)                 ; tail call
  ret void
}


define void @lam12898(i64 %env12899, i64 %rvp11518) {
  %envptr14815 = inttoptr i64 %env12899 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14816 = getelementptr inbounds i64, i64* %envptr14815, i64 3                ; &envptr14815[3]
  %cont9490 = load i64, i64* %envptr14816, align 8                                   ; load; *envptr14816
  %envptr14817 = inttoptr i64 %env12899 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14818 = getelementptr inbounds i64, i64* %envptr14817, i64 2                ; &envptr14817[2]
  %Sea$_37wind_45stack = load i64, i64* %envptr14818, align 8                        ; load; *envptr14818
  %envptr14819 = inttoptr i64 %env12899 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14820 = getelementptr inbounds i64, i64* %envptr14819, i64 1                ; &envptr14819[1]
  %l1t$l = load i64, i64* %envptr14820, align 8                                      ; load; *envptr14820
  %_959491 = call i64 @prim_car(i64 %rvp11518)                                       ; call prim_car
  %rvp11517 = call i64 @prim_cdr(i64 %rvp11518)                                      ; call prim_cdr
  %bW3$_959209 = call i64 @prim_car(i64 %rvp11517)                                   ; call prim_car
  %na11507 = call i64 @prim_cdr(i64 %rvp11517)                                       ; call prim_cdr
  %a9343 = call i64 @prim_car(i64 %l1t$l)                                            ; call prim_car
  %a9344 = call i64 @prim_car(i64 %a9343)                                            ; call prim_car
  %cloptr14821 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14823 = getelementptr inbounds i64, i64* %cloptr14821, i64 1                  ; &eptr14823[1]
  %eptr14824 = getelementptr inbounds i64, i64* %cloptr14821, i64 2                  ; &eptr14824[2]
  %eptr14825 = getelementptr inbounds i64, i64* %cloptr14821, i64 3                  ; &eptr14825[3]
  store i64 %l1t$l, i64* %eptr14823                                                  ; *eptr14823 = %l1t$l
  store i64 %Sea$_37wind_45stack, i64* %eptr14824                                    ; *eptr14824 = %Sea$_37wind_45stack
  store i64 %cont9490, i64* %eptr14825                                               ; *eptr14825 = %cont9490
  %eptr14822 = getelementptr inbounds i64, i64* %cloptr14821, i64 0                  ; &cloptr14821[0]
  %f14826 = ptrtoint void(i64,i64)* @lam12896 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14826, i64* %eptr14822                                                 ; store fptr
  %arg10268 = ptrtoint i64* %cloptr14821 to i64                                      ; closure cast; i64* -> i64
  %rva11516 = add i64 0, 0                                                           ; quoted ()
  %rva11515 = call i64 @prim_cons(i64 %arg10268, i64 %rva11516)                      ; call prim_cons
  %cloptr14827 = inttoptr i64 %a9344 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr14828 = getelementptr inbounds i64, i64* %cloptr14827, i64 0                 ; &cloptr14827[0]
  %f14830 = load i64, i64* %i0ptr14828, align 8                                      ; load; *i0ptr14828
  %fptr14829 = inttoptr i64 %f14830 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14829(i64 %a9344, i64 %rva11515)                    ; tail call
  ret void
}


define void @lam12896(i64 %env12897, i64 %rvp11514) {
  %envptr14831 = inttoptr i64 %env12897 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14832 = getelementptr inbounds i64, i64* %envptr14831, i64 3                ; &envptr14831[3]
  %cont9490 = load i64, i64* %envptr14832, align 8                                   ; load; *envptr14832
  %envptr14833 = inttoptr i64 %env12897 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14834 = getelementptr inbounds i64, i64* %envptr14833, i64 2                ; &envptr14833[2]
  %Sea$_37wind_45stack = load i64, i64* %envptr14834, align 8                        ; load; *envptr14834
  %envptr14835 = inttoptr i64 %env12897 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14836 = getelementptr inbounds i64, i64* %envptr14835, i64 1                ; &envptr14835[1]
  %l1t$l = load i64, i64* %envptr14836, align 8                                      ; load; *envptr14836
  %_959492 = call i64 @prim_car(i64 %rvp11514)                                       ; call prim_car
  %rvp11513 = call i64 @prim_cdr(i64 %rvp11514)                                      ; call prim_cdr
  %XsA$_959210 = call i64 @prim_car(i64 %rvp11513)                                   ; call prim_car
  %na11509 = call i64 @prim_cdr(i64 %rvp11513)                                       ; call prim_cdr
  %arg10271 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9493 = call i64 @prim_vector_45set_33(i64 %Sea$_37wind_45stack, i64 %arg10271, i64 %l1t$l); call prim_vector_45set_33
  %arg10274 = add i64 0, 0                                                           ; quoted ()
  %rva11512 = add i64 0, 0                                                           ; quoted ()
  %rva11511 = call i64 @prim_cons(i64 %retprim9493, i64 %rva11512)                   ; call prim_cons
  %rva11510 = call i64 @prim_cons(i64 %arg10274, i64 %rva11511)                      ; call prim_cons
  %cloptr14837 = inttoptr i64 %cont9490 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14838 = getelementptr inbounds i64, i64* %cloptr14837, i64 0                 ; &cloptr14837[0]
  %f14840 = load i64, i64* %i0ptr14838, align 8                                      ; load; *i0ptr14838
  %fptr14839 = inttoptr i64 %f14840 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14839(i64 %cont9490, i64 %rva11510)                 ; tail call
  ret void
}


define void @lam12879(i64 %env12880, i64 %wd4$lst9590) {
  %cont9589 = call i64 @prim_car(i64 %wd4$lst9590)                                   ; call prim_car
  %wd4$lst = call i64 @prim_cdr(i64 %wd4$lst9590)                                    ; call prim_cdr
  %arg10289 = add i64 0, 0                                                           ; quoted ()
  %rva11563 = add i64 0, 0                                                           ; quoted ()
  %rva11562 = call i64 @prim_cons(i64 %wd4$lst, i64 %rva11563)                       ; call prim_cons
  %rva11561 = call i64 @prim_cons(i64 %arg10289, i64 %rva11562)                      ; call prim_cons
  %cloptr14841 = inttoptr i64 %cont9589 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14842 = getelementptr inbounds i64, i64* %cloptr14841, i64 0                 ; &cloptr14841[0]
  %f14844 = load i64, i64* %i0ptr14842, align 8                                      ; load; *i0ptr14842
  %fptr14843 = inttoptr i64 %f14844 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14843(i64 %cont9589, i64 %rva11561)                 ; tail call
  ret void
}


define void @lam12875(i64 %env12876, i64 %rvp11951) {
  %envptr14845 = inttoptr i64 %env12876 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14846 = getelementptr inbounds i64, i64* %envptr14845, i64 3                ; &envptr14845[3]
  %Sea$_37wind_45stack = load i64, i64* %envptr14846, align 8                        ; load; *envptr14846
  %envptr14847 = inttoptr i64 %env12876 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14848 = getelementptr inbounds i64, i64* %envptr14847, i64 2                ; &envptr14847[2]
  %w8C$_37do_45wind = load i64, i64* %envptr14848, align 8                           ; load; *envptr14848
  %envptr14849 = inttoptr i64 %env12876 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14850 = getelementptr inbounds i64, i64* %envptr14849, i64 1                ; &envptr14849[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr14850, align 8                     ; load; *envptr14850
  %_959587 = call i64 @prim_car(i64 %rvp11951)                                       ; call prim_car
  %rvp11950 = call i64 @prim_cdr(i64 %rvp11951)                                      ; call prim_cdr
  %a9346 = call i64 @prim_car(i64 %rvp11950)                                         ; call prim_car
  %na11565 = call i64 @prim_cdr(i64 %rvp11950)                                       ; call prim_cdr
  %arg10292 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9588 = call i64 @prim_make_45vector(i64 %arg10292, i64 %a9346)             ; call prim_make_45vector
  %cloptr14851 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr14853 = getelementptr inbounds i64, i64* %cloptr14851, i64 1                  ; &eptr14853[1]
  %eptr14854 = getelementptr inbounds i64, i64* %cloptr14851, i64 2                  ; &eptr14854[2]
  %eptr14855 = getelementptr inbounds i64, i64* %cloptr14851, i64 3                  ; &eptr14855[3]
  store i64 %hzL$_37raise_45handler, i64* %eptr14853                                 ; *eptr14853 = %hzL$_37raise_45handler
  store i64 %w8C$_37do_45wind, i64* %eptr14854                                       ; *eptr14854 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr14855                                    ; *eptr14855 = %Sea$_37wind_45stack
  %eptr14852 = getelementptr inbounds i64, i64* %cloptr14851, i64 0                  ; &cloptr14851[0]
  %f14856 = ptrtoint void(i64,i64)* @lam12872 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14856, i64* %eptr14852                                                 ; store fptr
  %arg10295 = ptrtoint i64* %cloptr14851 to i64                                      ; closure cast; i64* -> i64
  %arg10294 = add i64 0, 0                                                           ; quoted ()
  %rva11949 = add i64 0, 0                                                           ; quoted ()
  %rva11948 = call i64 @prim_cons(i64 %retprim9588, i64 %rva11949)                   ; call prim_cons
  %rva11947 = call i64 @prim_cons(i64 %arg10294, i64 %rva11948)                      ; call prim_cons
  %cloptr14857 = inttoptr i64 %arg10295 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14858 = getelementptr inbounds i64, i64* %cloptr14857, i64 0                 ; &cloptr14857[0]
  %f14860 = load i64, i64* %i0ptr14858, align 8                                      ; load; *i0ptr14858
  %fptr14859 = inttoptr i64 %f14860 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14859(i64 %arg10295, i64 %rva11947)                 ; tail call
  ret void
}


define void @lam12872(i64 %env12873, i64 %rvp11946) {
  %envptr14861 = inttoptr i64 %env12873 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14862 = getelementptr inbounds i64, i64* %envptr14861, i64 3                ; &envptr14861[3]
  %Sea$_37wind_45stack = load i64, i64* %envptr14862, align 8                        ; load; *envptr14862
  %envptr14863 = inttoptr i64 %env12873 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14864 = getelementptr inbounds i64, i64* %envptr14863, i64 2                ; &envptr14863[2]
  %w8C$_37do_45wind = load i64, i64* %envptr14864, align 8                           ; load; *envptr14864
  %envptr14865 = inttoptr i64 %env12873 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14866 = getelementptr inbounds i64, i64* %envptr14865, i64 1                ; &envptr14865[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr14866, align 8                     ; load; *envptr14866
  %_959509 = call i64 @prim_car(i64 %rvp11946)                                       ; call prim_car
  %rvp11945 = call i64 @prim_cdr(i64 %rvp11946)                                      ; call prim_cdr
  %ZER$ccstack = call i64 @prim_car(i64 %rvp11945)                                   ; call prim_car
  %na11567 = call i64 @prim_cdr(i64 %rvp11945)                                       ; call prim_cdr
  %cloptr14867 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr14868 = getelementptr inbounds i64, i64* %cloptr14867, i64 0                  ; &cloptr14867[0]
  %f14869 = ptrtoint void(i64,i64)* @lam12870 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14869, i64* %eptr14868                                                 ; store fptr
  %arg10297 = ptrtoint i64* %cloptr14867 to i64                                      ; closure cast; i64* -> i64
  %cloptr14870 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr14872 = getelementptr inbounds i64, i64* %cloptr14870, i64 1                  ; &eptr14872[1]
  %eptr14873 = getelementptr inbounds i64, i64* %cloptr14870, i64 2                  ; &eptr14873[2]
  %eptr14874 = getelementptr inbounds i64, i64* %cloptr14870, i64 3                  ; &eptr14874[3]
  %eptr14875 = getelementptr inbounds i64, i64* %cloptr14870, i64 4                  ; &eptr14875[4]
  store i64 %hzL$_37raise_45handler, i64* %eptr14872                                 ; *eptr14872 = %hzL$_37raise_45handler
  store i64 %ZER$ccstack, i64* %eptr14873                                            ; *eptr14873 = %ZER$ccstack
  store i64 %w8C$_37do_45wind, i64* %eptr14874                                       ; *eptr14874 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr14875                                    ; *eptr14875 = %Sea$_37wind_45stack
  %eptr14871 = getelementptr inbounds i64, i64* %cloptr14870, i64 0                  ; &cloptr14870[0]
  %f14876 = ptrtoint void(i64,i64)* @lam12866 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14876, i64* %eptr14871                                                 ; store fptr
  %arg10296 = ptrtoint i64* %cloptr14870 to i64                                      ; closure cast; i64* -> i64
  %rva11944 = add i64 0, 0                                                           ; quoted ()
  %rva11943 = call i64 @prim_cons(i64 %arg10296, i64 %rva11944)                      ; call prim_cons
  %cloptr14877 = inttoptr i64 %arg10297 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14878 = getelementptr inbounds i64, i64* %cloptr14877, i64 0                 ; &cloptr14877[0]
  %f14880 = load i64, i64* %i0ptr14878, align 8                                      ; load; *i0ptr14878
  %fptr14879 = inttoptr i64 %f14880 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14879(i64 %arg10297, i64 %rva11943)                 ; tail call
  ret void
}


define void @lam12870(i64 %env12871, i64 %MWQ$lst9586) {
  %cont9585 = call i64 @prim_car(i64 %MWQ$lst9586)                                   ; call prim_car
  %MWQ$lst = call i64 @prim_cdr(i64 %MWQ$lst9586)                                    ; call prim_cdr
  %arg10301 = add i64 0, 0                                                           ; quoted ()
  %rva11570 = add i64 0, 0                                                           ; quoted ()
  %rva11569 = call i64 @prim_cons(i64 %MWQ$lst, i64 %rva11570)                       ; call prim_cons
  %rva11568 = call i64 @prim_cons(i64 %arg10301, i64 %rva11569)                      ; call prim_cons
  %cloptr14881 = inttoptr i64 %cont9585 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14882 = getelementptr inbounds i64, i64* %cloptr14881, i64 0                 ; &cloptr14881[0]
  %f14884 = load i64, i64* %i0ptr14882, align 8                                      ; load; *i0ptr14882
  %fptr14883 = inttoptr i64 %f14884 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14883(i64 %cont9585, i64 %rva11568)                 ; tail call
  ret void
}


define void @lam12866(i64 %env12867, i64 %rvp11942) {
  %envptr14885 = inttoptr i64 %env12867 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14886 = getelementptr inbounds i64, i64* %envptr14885, i64 4                ; &envptr14885[4]
  %Sea$_37wind_45stack = load i64, i64* %envptr14886, align 8                        ; load; *envptr14886
  %envptr14887 = inttoptr i64 %env12867 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14888 = getelementptr inbounds i64, i64* %envptr14887, i64 3                ; &envptr14887[3]
  %w8C$_37do_45wind = load i64, i64* %envptr14888, align 8                           ; load; *envptr14888
  %envptr14889 = inttoptr i64 %env12867 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14890 = getelementptr inbounds i64, i64* %envptr14889, i64 2                ; &envptr14889[2]
  %ZER$ccstack = load i64, i64* %envptr14890, align 8                                ; load; *envptr14890
  %envptr14891 = inttoptr i64 %env12867 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14892 = getelementptr inbounds i64, i64* %envptr14891, i64 1                ; &envptr14891[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr14892, align 8                     ; load; *envptr14892
  %_959583 = call i64 @prim_car(i64 %rvp11942)                                       ; call prim_car
  %rvp11941 = call i64 @prim_cdr(i64 %rvp11942)                                      ; call prim_cdr
  %a9347 = call i64 @prim_car(i64 %rvp11941)                                         ; call prim_car
  %na11572 = call i64 @prim_cdr(i64 %rvp11941)                                       ; call prim_cdr
  %arg10304 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9584 = call i64 @prim_make_45vector(i64 %arg10304, i64 %a9347)             ; call prim_make_45vector
  %cloptr14893 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr14895 = getelementptr inbounds i64, i64* %cloptr14893, i64 1                  ; &eptr14895[1]
  %eptr14896 = getelementptr inbounds i64, i64* %cloptr14893, i64 2                  ; &eptr14896[2]
  %eptr14897 = getelementptr inbounds i64, i64* %cloptr14893, i64 3                  ; &eptr14897[3]
  %eptr14898 = getelementptr inbounds i64, i64* %cloptr14893, i64 4                  ; &eptr14898[4]
  store i64 %hzL$_37raise_45handler, i64* %eptr14895                                 ; *eptr14895 = %hzL$_37raise_45handler
  store i64 %ZER$ccstack, i64* %eptr14896                                            ; *eptr14896 = %ZER$ccstack
  store i64 %w8C$_37do_45wind, i64* %eptr14897                                       ; *eptr14897 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr14898                                    ; *eptr14898 = %Sea$_37wind_45stack
  %eptr14894 = getelementptr inbounds i64, i64* %cloptr14893, i64 0                  ; &cloptr14893[0]
  %f14899 = ptrtoint void(i64,i64)* @lam12863 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14899, i64* %eptr14894                                                 ; store fptr
  %arg10307 = ptrtoint i64* %cloptr14893 to i64                                      ; closure cast; i64* -> i64
  %arg10306 = add i64 0, 0                                                           ; quoted ()
  %rva11940 = add i64 0, 0                                                           ; quoted ()
  %rva11939 = call i64 @prim_cons(i64 %retprim9584, i64 %rva11940)                   ; call prim_cons
  %rva11938 = call i64 @prim_cons(i64 %arg10306, i64 %rva11939)                      ; call prim_cons
  %cloptr14900 = inttoptr i64 %arg10307 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14901 = getelementptr inbounds i64, i64* %cloptr14900, i64 0                 ; &cloptr14900[0]
  %f14903 = load i64, i64* %i0ptr14901, align 8                                      ; load; *i0ptr14901
  %fptr14902 = inttoptr i64 %f14903 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14902(i64 %arg10307, i64 %rva11938)                 ; tail call
  ret void
}


define void @lam12863(i64 %env12864, i64 %rvp11937) {
  %envptr14904 = inttoptr i64 %env12864 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14905 = getelementptr inbounds i64, i64* %envptr14904, i64 4                ; &envptr14904[4]
  %Sea$_37wind_45stack = load i64, i64* %envptr14905, align 8                        ; load; *envptr14905
  %envptr14906 = inttoptr i64 %env12864 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14907 = getelementptr inbounds i64, i64* %envptr14906, i64 3                ; &envptr14906[3]
  %w8C$_37do_45wind = load i64, i64* %envptr14907, align 8                           ; load; *envptr14907
  %envptr14908 = inttoptr i64 %env12864 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14909 = getelementptr inbounds i64, i64* %envptr14908, i64 2                ; &envptr14908[2]
  %ZER$ccstack = load i64, i64* %envptr14909, align 8                                ; load; *envptr14909
  %envptr14910 = inttoptr i64 %env12864 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14911 = getelementptr inbounds i64, i64* %envptr14910, i64 1                ; &envptr14910[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr14911, align 8                     ; load; *envptr14911
  %_959510 = call i64 @prim_car(i64 %rvp11937)                                       ; call prim_car
  %rvp11936 = call i64 @prim_cdr(i64 %rvp11937)                                      ; call prim_cdr
  %OB6$fail = call i64 @prim_car(i64 %rvp11936)                                      ; call prim_car
  %na11574 = call i64 @prim_cdr(i64 %rvp11936)                                       ; call prim_cdr
  %cloptr14912 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr14913 = getelementptr inbounds i64, i64* %cloptr14912, i64 0                  ; &cloptr14912[0]
  %f14914 = ptrtoint void(i64,i64)* @lam12861 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14914, i64* %eptr14913                                                 ; store fptr
  %arg10309 = ptrtoint i64* %cloptr14912 to i64                                      ; closure cast; i64* -> i64
  %cloptr14915 = call i64* @alloc(i64 48)                                            ; malloc
  %eptr14917 = getelementptr inbounds i64, i64* %cloptr14915, i64 1                  ; &eptr14917[1]
  %eptr14918 = getelementptr inbounds i64, i64* %cloptr14915, i64 2                  ; &eptr14918[2]
  %eptr14919 = getelementptr inbounds i64, i64* %cloptr14915, i64 3                  ; &eptr14919[3]
  %eptr14920 = getelementptr inbounds i64, i64* %cloptr14915, i64 4                  ; &eptr14920[4]
  %eptr14921 = getelementptr inbounds i64, i64* %cloptr14915, i64 5                  ; &eptr14921[5]
  store i64 %hzL$_37raise_45handler, i64* %eptr14917                                 ; *eptr14917 = %hzL$_37raise_45handler
  store i64 %ZER$ccstack, i64* %eptr14918                                            ; *eptr14918 = %ZER$ccstack
  store i64 %OB6$fail, i64* %eptr14919                                               ; *eptr14919 = %OB6$fail
  store i64 %w8C$_37do_45wind, i64* %eptr14920                                       ; *eptr14920 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr14921                                    ; *eptr14921 = %Sea$_37wind_45stack
  %eptr14916 = getelementptr inbounds i64, i64* %cloptr14915, i64 0                  ; &cloptr14915[0]
  %f14922 = ptrtoint void(i64,i64)* @lam12857 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14922, i64* %eptr14916                                                 ; store fptr
  %arg10308 = ptrtoint i64* %cloptr14915 to i64                                      ; closure cast; i64* -> i64
  %rva11935 = add i64 0, 0                                                           ; quoted ()
  %rva11934 = call i64 @prim_cons(i64 %arg10308, i64 %rva11935)                      ; call prim_cons
  %cloptr14923 = inttoptr i64 %arg10309 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14924 = getelementptr inbounds i64, i64* %cloptr14923, i64 0                 ; &cloptr14923[0]
  %f14926 = load i64, i64* %i0ptr14924, align 8                                      ; load; *i0ptr14924
  %fptr14925 = inttoptr i64 %f14926 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14925(i64 %arg10309, i64 %rva11934)                 ; tail call
  ret void
}


define void @lam12861(i64 %env12862, i64 %H2U$lst9582) {
  %cont9581 = call i64 @prim_car(i64 %H2U$lst9582)                                   ; call prim_car
  %H2U$lst = call i64 @prim_cdr(i64 %H2U$lst9582)                                    ; call prim_cdr
  %arg10313 = add i64 0, 0                                                           ; quoted ()
  %rva11577 = add i64 0, 0                                                           ; quoted ()
  %rva11576 = call i64 @prim_cons(i64 %H2U$lst, i64 %rva11577)                       ; call prim_cons
  %rva11575 = call i64 @prim_cons(i64 %arg10313, i64 %rva11576)                      ; call prim_cons
  %cloptr14927 = inttoptr i64 %cont9581 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14928 = getelementptr inbounds i64, i64* %cloptr14927, i64 0                 ; &cloptr14927[0]
  %f14930 = load i64, i64* %i0ptr14928, align 8                                      ; load; *i0ptr14928
  %fptr14929 = inttoptr i64 %f14930 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14929(i64 %cont9581, i64 %rva11575)                 ; tail call
  ret void
}


define void @lam12857(i64 %env12858, i64 %rvp11933) {
  %envptr14931 = inttoptr i64 %env12858 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14932 = getelementptr inbounds i64, i64* %envptr14931, i64 5                ; &envptr14931[5]
  %Sea$_37wind_45stack = load i64, i64* %envptr14932, align 8                        ; load; *envptr14932
  %envptr14933 = inttoptr i64 %env12858 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14934 = getelementptr inbounds i64, i64* %envptr14933, i64 4                ; &envptr14933[4]
  %w8C$_37do_45wind = load i64, i64* %envptr14934, align 8                           ; load; *envptr14934
  %envptr14935 = inttoptr i64 %env12858 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14936 = getelementptr inbounds i64, i64* %envptr14935, i64 3                ; &envptr14935[3]
  %OB6$fail = load i64, i64* %envptr14936, align 8                                   ; load; *envptr14936
  %envptr14937 = inttoptr i64 %env12858 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14938 = getelementptr inbounds i64, i64* %envptr14937, i64 2                ; &envptr14937[2]
  %ZER$ccstack = load i64, i64* %envptr14938, align 8                                ; load; *envptr14938
  %envptr14939 = inttoptr i64 %env12858 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14940 = getelementptr inbounds i64, i64* %envptr14939, i64 1                ; &envptr14939[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr14940, align 8                     ; load; *envptr14940
  %_959579 = call i64 @prim_car(i64 %rvp11933)                                       ; call prim_car
  %rvp11932 = call i64 @prim_cdr(i64 %rvp11933)                                      ; call prim_cdr
  %a9348 = call i64 @prim_car(i64 %rvp11932)                                         ; call prim_car
  %na11579 = call i64 @prim_cdr(i64 %rvp11932)                                       ; call prim_cdr
  %arg10316 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9580 = call i64 @prim_make_45vector(i64 %arg10316, i64 %a9348)             ; call prim_make_45vector
  %cloptr14941 = call i64* @alloc(i64 48)                                            ; malloc
  %eptr14943 = getelementptr inbounds i64, i64* %cloptr14941, i64 1                  ; &eptr14943[1]
  %eptr14944 = getelementptr inbounds i64, i64* %cloptr14941, i64 2                  ; &eptr14944[2]
  %eptr14945 = getelementptr inbounds i64, i64* %cloptr14941, i64 3                  ; &eptr14945[3]
  %eptr14946 = getelementptr inbounds i64, i64* %cloptr14941, i64 4                  ; &eptr14946[4]
  %eptr14947 = getelementptr inbounds i64, i64* %cloptr14941, i64 5                  ; &eptr14947[5]
  store i64 %hzL$_37raise_45handler, i64* %eptr14943                                 ; *eptr14943 = %hzL$_37raise_45handler
  store i64 %ZER$ccstack, i64* %eptr14944                                            ; *eptr14944 = %ZER$ccstack
  store i64 %OB6$fail, i64* %eptr14945                                               ; *eptr14945 = %OB6$fail
  store i64 %w8C$_37do_45wind, i64* %eptr14946                                       ; *eptr14946 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr14947                                    ; *eptr14947 = %Sea$_37wind_45stack
  %eptr14942 = getelementptr inbounds i64, i64* %cloptr14941, i64 0                  ; &cloptr14941[0]
  %f14948 = ptrtoint void(i64,i64)* @lam12854 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14948, i64* %eptr14942                                                 ; store fptr
  %arg10319 = ptrtoint i64* %cloptr14941 to i64                                      ; closure cast; i64* -> i64
  %arg10318 = add i64 0, 0                                                           ; quoted ()
  %rva11931 = add i64 0, 0                                                           ; quoted ()
  %rva11930 = call i64 @prim_cons(i64 %retprim9580, i64 %rva11931)                   ; call prim_cons
  %rva11929 = call i64 @prim_cons(i64 %arg10318, i64 %rva11930)                      ; call prim_cons
  %cloptr14949 = inttoptr i64 %arg10319 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14950 = getelementptr inbounds i64, i64* %cloptr14949, i64 0                 ; &cloptr14949[0]
  %f14952 = load i64, i64* %i0ptr14950, align 8                                      ; load; *i0ptr14950
  %fptr14951 = inttoptr i64 %f14952 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14951(i64 %arg10319, i64 %rva11929)                 ; tail call
  ret void
}


define void @lam12854(i64 %env12855, i64 %rvp11928) {
  %envptr14953 = inttoptr i64 %env12855 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14954 = getelementptr inbounds i64, i64* %envptr14953, i64 5                ; &envptr14953[5]
  %Sea$_37wind_45stack = load i64, i64* %envptr14954, align 8                        ; load; *envptr14954
  %envptr14955 = inttoptr i64 %env12855 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14956 = getelementptr inbounds i64, i64* %envptr14955, i64 4                ; &envptr14955[4]
  %w8C$_37do_45wind = load i64, i64* %envptr14956, align 8                           ; load; *envptr14956
  %envptr14957 = inttoptr i64 %env12855 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14958 = getelementptr inbounds i64, i64* %envptr14957, i64 3                ; &envptr14957[3]
  %OB6$fail = load i64, i64* %envptr14958, align 8                                   ; load; *envptr14958
  %envptr14959 = inttoptr i64 %env12855 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14960 = getelementptr inbounds i64, i64* %envptr14959, i64 2                ; &envptr14959[2]
  %ZER$ccstack = load i64, i64* %envptr14960, align 8                                ; load; *envptr14960
  %envptr14961 = inttoptr i64 %env12855 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14962 = getelementptr inbounds i64, i64* %envptr14961, i64 1                ; &envptr14961[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr14962, align 8                     ; load; *envptr14962
  %_959511 = call i64 @prim_car(i64 %rvp11928)                                       ; call prim_car
  %rvp11927 = call i64 @prim_cdr(i64 %rvp11928)                                      ; call prim_cdr
  %UAc$assert = call i64 @prim_car(i64 %rvp11927)                                    ; call prim_car
  %na11581 = call i64 @prim_cdr(i64 %rvp11927)                                       ; call prim_cdr
  %cloptr14963 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr14964 = getelementptr inbounds i64, i64* %cloptr14963, i64 0                  ; &cloptr14963[0]
  %f14965 = ptrtoint void(i64,i64)* @lam12852 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14965, i64* %eptr14964                                                 ; store fptr
  %arg10321 = ptrtoint i64* %cloptr14963 to i64                                      ; closure cast; i64* -> i64
  %cloptr14966 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr14968 = getelementptr inbounds i64, i64* %cloptr14966, i64 1                  ; &eptr14968[1]
  %eptr14969 = getelementptr inbounds i64, i64* %cloptr14966, i64 2                  ; &eptr14969[2]
  %eptr14970 = getelementptr inbounds i64, i64* %cloptr14966, i64 3                  ; &eptr14970[3]
  %eptr14971 = getelementptr inbounds i64, i64* %cloptr14966, i64 4                  ; &eptr14971[4]
  %eptr14972 = getelementptr inbounds i64, i64* %cloptr14966, i64 5                  ; &eptr14972[5]
  %eptr14973 = getelementptr inbounds i64, i64* %cloptr14966, i64 6                  ; &eptr14973[6]
  store i64 %hzL$_37raise_45handler, i64* %eptr14968                                 ; *eptr14968 = %hzL$_37raise_45handler
  store i64 %UAc$assert, i64* %eptr14969                                             ; *eptr14969 = %UAc$assert
  store i64 %ZER$ccstack, i64* %eptr14970                                            ; *eptr14970 = %ZER$ccstack
  store i64 %OB6$fail, i64* %eptr14971                                               ; *eptr14971 = %OB6$fail
  store i64 %w8C$_37do_45wind, i64* %eptr14972                                       ; *eptr14972 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr14973                                    ; *eptr14973 = %Sea$_37wind_45stack
  %eptr14967 = getelementptr inbounds i64, i64* %cloptr14966, i64 0                  ; &cloptr14966[0]
  %f14974 = ptrtoint void(i64,i64)* @lam12848 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f14974, i64* %eptr14967                                                 ; store fptr
  %arg10320 = ptrtoint i64* %cloptr14966 to i64                                      ; closure cast; i64* -> i64
  %rva11926 = add i64 0, 0                                                           ; quoted ()
  %rva11925 = call i64 @prim_cons(i64 %arg10320, i64 %rva11926)                      ; call prim_cons
  %cloptr14975 = inttoptr i64 %arg10321 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14976 = getelementptr inbounds i64, i64* %cloptr14975, i64 0                 ; &cloptr14975[0]
  %f14978 = load i64, i64* %i0ptr14976, align 8                                      ; load; *i0ptr14976
  %fptr14977 = inttoptr i64 %f14978 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14977(i64 %arg10321, i64 %rva11925)                 ; tail call
  ret void
}


define void @lam12852(i64 %env12853, i64 %ukH$lst9578) {
  %cont9577 = call i64 @prim_car(i64 %ukH$lst9578)                                   ; call prim_car
  %ukH$lst = call i64 @prim_cdr(i64 %ukH$lst9578)                                    ; call prim_cdr
  %arg10325 = add i64 0, 0                                                           ; quoted ()
  %rva11584 = add i64 0, 0                                                           ; quoted ()
  %rva11583 = call i64 @prim_cons(i64 %ukH$lst, i64 %rva11584)                       ; call prim_cons
  %rva11582 = call i64 @prim_cons(i64 %arg10325, i64 %rva11583)                      ; call prim_cons
  %cloptr14979 = inttoptr i64 %cont9577 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr14980 = getelementptr inbounds i64, i64* %cloptr14979, i64 0                 ; &cloptr14979[0]
  %f14982 = load i64, i64* %i0ptr14980, align 8                                      ; load; *i0ptr14980
  %fptr14981 = inttoptr i64 %f14982 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr14981(i64 %cont9577, i64 %rva11582)                 ; tail call
  ret void
}


define void @lam12848(i64 %env12849, i64 %rvp11924) {
  %envptr14983 = inttoptr i64 %env12849 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14984 = getelementptr inbounds i64, i64* %envptr14983, i64 6                ; &envptr14983[6]
  %Sea$_37wind_45stack = load i64, i64* %envptr14984, align 8                        ; load; *envptr14984
  %envptr14985 = inttoptr i64 %env12849 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14986 = getelementptr inbounds i64, i64* %envptr14985, i64 5                ; &envptr14985[5]
  %w8C$_37do_45wind = load i64, i64* %envptr14986, align 8                           ; load; *envptr14986
  %envptr14987 = inttoptr i64 %env12849 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14988 = getelementptr inbounds i64, i64* %envptr14987, i64 4                ; &envptr14987[4]
  %OB6$fail = load i64, i64* %envptr14988, align 8                                   ; load; *envptr14988
  %envptr14989 = inttoptr i64 %env12849 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14990 = getelementptr inbounds i64, i64* %envptr14989, i64 3                ; &envptr14989[3]
  %ZER$ccstack = load i64, i64* %envptr14990, align 8                                ; load; *envptr14990
  %envptr14991 = inttoptr i64 %env12849 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14992 = getelementptr inbounds i64, i64* %envptr14991, i64 2                ; &envptr14991[2]
  %UAc$assert = load i64, i64* %envptr14992, align 8                                 ; load; *envptr14992
  %envptr14993 = inttoptr i64 %env12849 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr14994 = getelementptr inbounds i64, i64* %envptr14993, i64 1                ; &envptr14993[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr14994, align 8                     ; load; *envptr14994
  %_959575 = call i64 @prim_car(i64 %rvp11924)                                       ; call prim_car
  %rvp11923 = call i64 @prim_cdr(i64 %rvp11924)                                      ; call prim_cdr
  %a9349 = call i64 @prim_car(i64 %rvp11923)                                         ; call prim_car
  %na11586 = call i64 @prim_cdr(i64 %rvp11923)                                       ; call prim_cdr
  %arg10328 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9576 = call i64 @prim_make_45vector(i64 %arg10328, i64 %a9349)             ; call prim_make_45vector
  %cloptr14995 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr14997 = getelementptr inbounds i64, i64* %cloptr14995, i64 1                  ; &eptr14997[1]
  %eptr14998 = getelementptr inbounds i64, i64* %cloptr14995, i64 2                  ; &eptr14998[2]
  %eptr14999 = getelementptr inbounds i64, i64* %cloptr14995, i64 3                  ; &eptr14999[3]
  %eptr15000 = getelementptr inbounds i64, i64* %cloptr14995, i64 4                  ; &eptr15000[4]
  %eptr15001 = getelementptr inbounds i64, i64* %cloptr14995, i64 5                  ; &eptr15001[5]
  %eptr15002 = getelementptr inbounds i64, i64* %cloptr14995, i64 6                  ; &eptr15002[6]
  store i64 %hzL$_37raise_45handler, i64* %eptr14997                                 ; *eptr14997 = %hzL$_37raise_45handler
  store i64 %UAc$assert, i64* %eptr14998                                             ; *eptr14998 = %UAc$assert
  store i64 %ZER$ccstack, i64* %eptr14999                                            ; *eptr14999 = %ZER$ccstack
  store i64 %OB6$fail, i64* %eptr15000                                               ; *eptr15000 = %OB6$fail
  store i64 %w8C$_37do_45wind, i64* %eptr15001                                       ; *eptr15001 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr15002                                    ; *eptr15002 = %Sea$_37wind_45stack
  %eptr14996 = getelementptr inbounds i64, i64* %cloptr14995, i64 0                  ; &cloptr14995[0]
  %f15003 = ptrtoint void(i64,i64)* @lam12845 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15003, i64* %eptr14996                                                 ; store fptr
  %arg10331 = ptrtoint i64* %cloptr14995 to i64                                      ; closure cast; i64* -> i64
  %arg10330 = add i64 0, 0                                                           ; quoted ()
  %rva11922 = add i64 0, 0                                                           ; quoted ()
  %rva11921 = call i64 @prim_cons(i64 %retprim9576, i64 %rva11922)                   ; call prim_cons
  %rva11920 = call i64 @prim_cons(i64 %arg10330, i64 %rva11921)                      ; call prim_cons
  %cloptr15004 = inttoptr i64 %arg10331 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15005 = getelementptr inbounds i64, i64* %cloptr15004, i64 0                 ; &cloptr15004[0]
  %f15007 = load i64, i64* %i0ptr15005, align 8                                      ; load; *i0ptr15005
  %fptr15006 = inttoptr i64 %f15007 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15006(i64 %arg10331, i64 %rva11920)                 ; tail call
  ret void
}


define void @lam12845(i64 %env12846, i64 %rvp11919) {
  %envptr15008 = inttoptr i64 %env12846 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15009 = getelementptr inbounds i64, i64* %envptr15008, i64 6                ; &envptr15008[6]
  %Sea$_37wind_45stack = load i64, i64* %envptr15009, align 8                        ; load; *envptr15009
  %envptr15010 = inttoptr i64 %env12846 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15011 = getelementptr inbounds i64, i64* %envptr15010, i64 5                ; &envptr15010[5]
  %w8C$_37do_45wind = load i64, i64* %envptr15011, align 8                           ; load; *envptr15011
  %envptr15012 = inttoptr i64 %env12846 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15013 = getelementptr inbounds i64, i64* %envptr15012, i64 4                ; &envptr15012[4]
  %OB6$fail = load i64, i64* %envptr15013, align 8                                   ; load; *envptr15013
  %envptr15014 = inttoptr i64 %env12846 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15015 = getelementptr inbounds i64, i64* %envptr15014, i64 3                ; &envptr15014[3]
  %ZER$ccstack = load i64, i64* %envptr15015, align 8                                ; load; *envptr15015
  %envptr15016 = inttoptr i64 %env12846 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15017 = getelementptr inbounds i64, i64* %envptr15016, i64 2                ; &envptr15016[2]
  %UAc$assert = load i64, i64* %envptr15017, align 8                                 ; load; *envptr15017
  %envptr15018 = inttoptr i64 %env12846 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15019 = getelementptr inbounds i64, i64* %envptr15018, i64 1                ; &envptr15018[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr15019, align 8                     ; load; *envptr15019
  %_959512 = call i64 @prim_car(i64 %rvp11919)                                       ; call prim_car
  %rvp11918 = call i64 @prim_cdr(i64 %rvp11919)                                      ; call prim_cdr
  %h1l$amb = call i64 @prim_car(i64 %rvp11918)                                       ; call prim_car
  %na11588 = call i64 @prim_cdr(i64 %rvp11918)                                       ; call prim_cdr
  %cloptr15020 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15021 = getelementptr inbounds i64, i64* %cloptr15020, i64 0                  ; &cloptr15020[0]
  %f15022 = ptrtoint void(i64,i64)* @lam12843 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15022, i64* %eptr15021                                                 ; store fptr
  %arg10333 = ptrtoint i64* %cloptr15020 to i64                                      ; closure cast; i64* -> i64
  %cloptr15023 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr15025 = getelementptr inbounds i64, i64* %cloptr15023, i64 1                  ; &eptr15025[1]
  %eptr15026 = getelementptr inbounds i64, i64* %cloptr15023, i64 2                  ; &eptr15026[2]
  %eptr15027 = getelementptr inbounds i64, i64* %cloptr15023, i64 3                  ; &eptr15027[3]
  %eptr15028 = getelementptr inbounds i64, i64* %cloptr15023, i64 4                  ; &eptr15028[4]
  %eptr15029 = getelementptr inbounds i64, i64* %cloptr15023, i64 5                  ; &eptr15029[5]
  %eptr15030 = getelementptr inbounds i64, i64* %cloptr15023, i64 6                  ; &eptr15030[6]
  %eptr15031 = getelementptr inbounds i64, i64* %cloptr15023, i64 7                  ; &eptr15031[7]
  store i64 %hzL$_37raise_45handler, i64* %eptr15025                                 ; *eptr15025 = %hzL$_37raise_45handler
  store i64 %UAc$assert, i64* %eptr15026                                             ; *eptr15026 = %UAc$assert
  store i64 %h1l$amb, i64* %eptr15027                                                ; *eptr15027 = %h1l$amb
  store i64 %ZER$ccstack, i64* %eptr15028                                            ; *eptr15028 = %ZER$ccstack
  store i64 %OB6$fail, i64* %eptr15029                                               ; *eptr15029 = %OB6$fail
  store i64 %w8C$_37do_45wind, i64* %eptr15030                                       ; *eptr15030 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr15031                                    ; *eptr15031 = %Sea$_37wind_45stack
  %eptr15024 = getelementptr inbounds i64, i64* %cloptr15023, i64 0                  ; &cloptr15023[0]
  %f15032 = ptrtoint void(i64,i64)* @lam12839 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15032, i64* %eptr15024                                                 ; store fptr
  %arg10332 = ptrtoint i64* %cloptr15023 to i64                                      ; closure cast; i64* -> i64
  %rva11917 = add i64 0, 0                                                           ; quoted ()
  %rva11916 = call i64 @prim_cons(i64 %arg10332, i64 %rva11917)                      ; call prim_cons
  %cloptr15033 = inttoptr i64 %arg10333 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15034 = getelementptr inbounds i64, i64* %cloptr15033, i64 0                 ; &cloptr15033[0]
  %f15036 = load i64, i64* %i0ptr15034, align 8                                      ; load; *i0ptr15034
  %fptr15035 = inttoptr i64 %f15036 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15035(i64 %arg10333, i64 %rva11916)                 ; tail call
  ret void
}


define void @lam12843(i64 %env12844, i64 %XSO$lst9574) {
  %cont9573 = call i64 @prim_car(i64 %XSO$lst9574)                                   ; call prim_car
  %XSO$lst = call i64 @prim_cdr(i64 %XSO$lst9574)                                    ; call prim_cdr
  %arg10337 = add i64 0, 0                                                           ; quoted ()
  %rva11591 = add i64 0, 0                                                           ; quoted ()
  %rva11590 = call i64 @prim_cons(i64 %XSO$lst, i64 %rva11591)                       ; call prim_cons
  %rva11589 = call i64 @prim_cons(i64 %arg10337, i64 %rva11590)                      ; call prim_cons
  %cloptr15037 = inttoptr i64 %cont9573 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15038 = getelementptr inbounds i64, i64* %cloptr15037, i64 0                 ; &cloptr15037[0]
  %f15040 = load i64, i64* %i0ptr15038, align 8                                      ; load; *i0ptr15038
  %fptr15039 = inttoptr i64 %f15040 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15039(i64 %cont9573, i64 %rva11589)                 ; tail call
  ret void
}


define void @lam12839(i64 %env12840, i64 %rvp11915) {
  %envptr15041 = inttoptr i64 %env12840 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15042 = getelementptr inbounds i64, i64* %envptr15041, i64 7                ; &envptr15041[7]
  %Sea$_37wind_45stack = load i64, i64* %envptr15042, align 8                        ; load; *envptr15042
  %envptr15043 = inttoptr i64 %env12840 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15044 = getelementptr inbounds i64, i64* %envptr15043, i64 6                ; &envptr15043[6]
  %w8C$_37do_45wind = load i64, i64* %envptr15044, align 8                           ; load; *envptr15044
  %envptr15045 = inttoptr i64 %env12840 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15046 = getelementptr inbounds i64, i64* %envptr15045, i64 5                ; &envptr15045[5]
  %OB6$fail = load i64, i64* %envptr15046, align 8                                   ; load; *envptr15046
  %envptr15047 = inttoptr i64 %env12840 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15048 = getelementptr inbounds i64, i64* %envptr15047, i64 4                ; &envptr15047[4]
  %ZER$ccstack = load i64, i64* %envptr15048, align 8                                ; load; *envptr15048
  %envptr15049 = inttoptr i64 %env12840 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15050 = getelementptr inbounds i64, i64* %envptr15049, i64 3                ; &envptr15049[3]
  %h1l$amb = load i64, i64* %envptr15050, align 8                                    ; load; *envptr15050
  %envptr15051 = inttoptr i64 %env12840 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15052 = getelementptr inbounds i64, i64* %envptr15051, i64 2                ; &envptr15051[2]
  %UAc$assert = load i64, i64* %envptr15052, align 8                                 ; load; *envptr15052
  %envptr15053 = inttoptr i64 %env12840 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15054 = getelementptr inbounds i64, i64* %envptr15053, i64 1                ; &envptr15053[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr15054, align 8                     ; load; *envptr15054
  %_959513 = call i64 @prim_car(i64 %rvp11915)                                       ; call prim_car
  %rvp11914 = call i64 @prim_cdr(i64 %rvp11915)                                      ; call prim_cdr
  %agQ$letrec9211 = call i64 @prim_car(i64 %rvp11914)                                ; call prim_car
  %na11593 = call i64 @prim_cdr(i64 %rvp11914)                                       ; call prim_cdr
  %cloptr15055 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr15057 = getelementptr inbounds i64, i64* %cloptr15055, i64 1                  ; &eptr15057[1]
  %eptr15058 = getelementptr inbounds i64, i64* %cloptr15055, i64 2                  ; &eptr15058[2]
  store i64 %hzL$_37raise_45handler, i64* %eptr15057                                 ; *eptr15057 = %hzL$_37raise_45handler
  store i64 %ZER$ccstack, i64* %eptr15058                                            ; *eptr15058 = %ZER$ccstack
  %eptr15056 = getelementptr inbounds i64, i64* %cloptr15055, i64 0                  ; &cloptr15055[0]
  %f15059 = ptrtoint void(i64,i64)* @lam12837 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15059, i64* %eptr15056                                                 ; store fptr
  %KTX$letrec9212 = ptrtoint i64* %cloptr15055 to i64                                ; closure cast; i64* -> i64
  %cloptr15060 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr15062 = getelementptr inbounds i64, i64* %cloptr15060, i64 1                  ; &eptr15062[1]
  store i64 %OB6$fail, i64* %eptr15062                                               ; *eptr15062 = %OB6$fail
  %eptr15061 = getelementptr inbounds i64, i64* %cloptr15060, i64 0                  ; &cloptr15060[0]
  %f15063 = ptrtoint void(i64,i64)* @lam12807 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15063, i64* %eptr15061                                                 ; store fptr
  %ECc$letrec9213 = ptrtoint i64* %cloptr15060 to i64                                ; closure cast; i64* -> i64
  %cloptr15064 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15066 = getelementptr inbounds i64, i64* %cloptr15064, i64 1                  ; &eptr15066[1]
  %eptr15067 = getelementptr inbounds i64, i64* %cloptr15064, i64 2                  ; &eptr15067[2]
  %eptr15068 = getelementptr inbounds i64, i64* %cloptr15064, i64 3                  ; &eptr15068[3]
  %eptr15069 = getelementptr inbounds i64, i64* %cloptr15064, i64 4                  ; &eptr15069[4]
  store i64 %ZER$ccstack, i64* %eptr15066                                            ; *eptr15066 = %ZER$ccstack
  store i64 %OB6$fail, i64* %eptr15067                                               ; *eptr15067 = %OB6$fail
  store i64 %w8C$_37do_45wind, i64* %eptr15068                                       ; *eptr15068 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr15069                                    ; *eptr15069 = %Sea$_37wind_45stack
  %eptr15065 = getelementptr inbounds i64, i64* %cloptr15064, i64 0                  ; &cloptr15064[0]
  %f15070 = ptrtoint void(i64,i64)* @lam12800 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15070, i64* %eptr15065                                                 ; store fptr
  %mvo$letrec9214 = ptrtoint i64* %cloptr15064 to i64                                ; closure cast; i64* -> i64
  %arg10548 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %EBs$_959219 = call i64 @prim_vector_45set_33(i64 %ZER$ccstack, i64 %arg10548, i64 %agQ$letrec9211); call prim_vector_45set_33
  %arg10551 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %Zyq$_959220 = call i64 @prim_vector_45set_33(i64 %OB6$fail, i64 %arg10551, i64 %KTX$letrec9212); call prim_vector_45set_33
  %arg10554 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %CEy$_959221 = call i64 @prim_vector_45set_33(i64 %UAc$assert, i64 %arg10554, i64 %ECc$letrec9213); call prim_vector_45set_33
  %arg10557 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %T3O$_959222 = call i64 @prim_vector_45set_33(i64 %h1l$amb, i64 %arg10557, i64 %mvo$letrec9214); call prim_vector_45set_33
  %arg10559 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9373 = call i64 @prim_vector_45ref(i64 %h1l$amb, i64 %arg10559)                  ; call prim_vector_45ref
  %cloptr15071 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15072 = getelementptr inbounds i64, i64* %cloptr15071, i64 0                  ; &cloptr15071[0]
  %f15073 = ptrtoint void(i64,i64)* @lam12668 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15073, i64* %eptr15072                                                 ; store fptr
  %arg10562 = ptrtoint i64* %cloptr15071 to i64                                      ; closure cast; i64* -> i64
  %cloptr15074 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr15076 = getelementptr inbounds i64, i64* %cloptr15074, i64 1                  ; &eptr15076[1]
  %eptr15077 = getelementptr inbounds i64, i64* %cloptr15074, i64 2                  ; &eptr15077[2]
  %eptr15078 = getelementptr inbounds i64, i64* %cloptr15074, i64 3                  ; &eptr15078[3]
  store i64 %UAc$assert, i64* %eptr15076                                             ; *eptr15076 = %UAc$assert
  store i64 %h1l$amb, i64* %eptr15077                                                ; *eptr15077 = %h1l$amb
  store i64 %a9373, i64* %eptr15078                                                  ; *eptr15078 = %a9373
  %eptr15075 = getelementptr inbounds i64, i64* %cloptr15074, i64 0                  ; &cloptr15074[0]
  %f15079 = ptrtoint void(i64,i64)* @lam12664 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15079, i64* %eptr15075                                                 ; store fptr
  %arg10561 = ptrtoint i64* %cloptr15074 to i64                                      ; closure cast; i64* -> i64
  %rva11913 = add i64 0, 0                                                           ; quoted ()
  %rva11912 = call i64 @prim_cons(i64 %arg10561, i64 %rva11913)                      ; call prim_cons
  %cloptr15080 = inttoptr i64 %arg10562 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15081 = getelementptr inbounds i64, i64* %cloptr15080, i64 0                 ; &cloptr15080[0]
  %f15083 = load i64, i64* %i0ptr15081, align 8                                      ; load; *i0ptr15081
  %fptr15082 = inttoptr i64 %f15083 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15082(i64 %arg10562, i64 %rva11912)                 ; tail call
  ret void
}


define void @lam12837(i64 %env12838, i64 %rvp11632) {
  %envptr15084 = inttoptr i64 %env12838 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15085 = getelementptr inbounds i64, i64* %envptr15084, i64 2                ; &envptr15084[2]
  %ZER$ccstack = load i64, i64* %envptr15085, align 8                                ; load; *envptr15085
  %envptr15086 = inttoptr i64 %env12838 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15087 = getelementptr inbounds i64, i64* %envptr15086, i64 1                ; &envptr15086[1]
  %hzL$_37raise_45handler = load i64, i64* %envptr15087, align 8                     ; load; *envptr15087
  %cont9514 = call i64 @prim_car(i64 %rvp11632)                                      ; call prim_car
  %na11595 = call i64 @prim_cdr(i64 %rvp11632)                                       ; call prim_cdr
  %arg10339 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9350 = call i64 @prim_vector_45ref(i64 %ZER$ccstack, i64 %arg10339)              ; call prim_vector_45ref
  %a9351 = call i64 @prim_null_63(i64 %a9350)                                        ; call prim_null_63
  %cmp15088 = icmp eq i64 %a9351, 15                                                 ; false?
  br i1 %cmp15088, label %else15090, label %then15089                                ; if

then15089:
  %arg10342 = call i64 @const_init_symbol(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @sym15091, i32 0, i32 0)); quoted string
  %rva11598 = add i64 0, 0                                                           ; quoted ()
  %rva11597 = call i64 @prim_cons(i64 %arg10342, i64 %rva11598)                      ; call prim_cons
  %rva11596 = call i64 @prim_cons(i64 %cont9514, i64 %rva11597)                      ; call prim_cons
  %cloptr15092 = inttoptr i64 %hzL$_37raise_45handler to i64*                        ; closure/env cast; i64 -> i64*
  %i0ptr15093 = getelementptr inbounds i64, i64* %cloptr15092, i64 0                 ; &cloptr15092[0]
  %f15095 = load i64, i64* %i0ptr15093, align 8                                      ; load; *i0ptr15093
  %fptr15094 = inttoptr i64 %f15095 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15094(i64 %hzL$_37raise_45handler, i64 %rva11596)   ; tail call
  ret void

else15090:
  %arg10345 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9352 = call i64 @prim_vector_45ref(i64 %ZER$ccstack, i64 %arg10345)              ; call prim_vector_45ref
  %retprim9523 = call i64 @prim_car(i64 %a9352)                                      ; call prim_car
  %cloptr15096 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr15098 = getelementptr inbounds i64, i64* %cloptr15096, i64 1                  ; &eptr15098[1]
  %eptr15099 = getelementptr inbounds i64, i64* %cloptr15096, i64 2                  ; &eptr15099[2]
  store i64 %ZER$ccstack, i64* %eptr15098                                            ; *eptr15098 = %ZER$ccstack
  store i64 %cont9514, i64* %eptr15099                                               ; *eptr15099 = %cont9514
  %eptr15097 = getelementptr inbounds i64, i64* %cloptr15096, i64 0                  ; &cloptr15096[0]
  %f15100 = ptrtoint void(i64,i64)* @lam12833 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15100, i64* %eptr15097                                                 ; store fptr
  %arg10350 = ptrtoint i64* %cloptr15096 to i64                                      ; closure cast; i64* -> i64
  %arg10349 = add i64 0, 0                                                           ; quoted ()
  %rva11631 = add i64 0, 0                                                           ; quoted ()
  %rva11630 = call i64 @prim_cons(i64 %retprim9523, i64 %rva11631)                   ; call prim_cons
  %rva11629 = call i64 @prim_cons(i64 %arg10349, i64 %rva11630)                      ; call prim_cons
  %cloptr15101 = inttoptr i64 %arg10350 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15102 = getelementptr inbounds i64, i64* %cloptr15101, i64 0                 ; &cloptr15101[0]
  %f15104 = load i64, i64* %i0ptr15102, align 8                                      ; load; *i0ptr15102
  %fptr15103 = inttoptr i64 %f15104 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15103(i64 %arg10350, i64 %rva11629)                 ; tail call
  ret void
}


define void @lam12833(i64 %env12834, i64 %rvp11628) {
  %envptr15105 = inttoptr i64 %env12834 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15106 = getelementptr inbounds i64, i64* %envptr15105, i64 2                ; &envptr15105[2]
  %cont9514 = load i64, i64* %envptr15106, align 8                                   ; load; *envptr15106
  %envptr15107 = inttoptr i64 %env12834 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15108 = getelementptr inbounds i64, i64* %envptr15107, i64 1                ; &envptr15107[1]
  %ZER$ccstack = load i64, i64* %envptr15108, align 8                                ; load; *envptr15108
  %_959515 = call i64 @prim_car(i64 %rvp11628)                                       ; call prim_car
  %rvp11627 = call i64 @prim_cdr(i64 %rvp11628)                                      ; call prim_cdr
  %Tkh$next_45cc = call i64 @prim_car(i64 %rvp11627)                                 ; call prim_car
  %na11600 = call i64 @prim_cdr(i64 %rvp11627)                                       ; call prim_cdr
  %cloptr15109 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15110 = getelementptr inbounds i64, i64* %cloptr15109, i64 0                  ; &cloptr15109[0]
  %f15111 = ptrtoint void(i64,i64)* @lam12831 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15111, i64* %eptr15110                                                 ; store fptr
  %arg10352 = ptrtoint i64* %cloptr15109 to i64                                      ; closure cast; i64* -> i64
  %cloptr15112 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr15114 = getelementptr inbounds i64, i64* %cloptr15112, i64 1                  ; &eptr15114[1]
  %eptr15115 = getelementptr inbounds i64, i64* %cloptr15112, i64 2                  ; &eptr15115[2]
  %eptr15116 = getelementptr inbounds i64, i64* %cloptr15112, i64 3                  ; &eptr15116[3]
  store i64 %ZER$ccstack, i64* %eptr15114                                            ; *eptr15114 = %ZER$ccstack
  store i64 %cont9514, i64* %eptr15115                                               ; *eptr15115 = %cont9514
  store i64 %Tkh$next_45cc, i64* %eptr15116                                          ; *eptr15116 = %Tkh$next_45cc
  %eptr15113 = getelementptr inbounds i64, i64* %cloptr15112, i64 0                  ; &cloptr15112[0]
  %f15117 = ptrtoint void(i64,i64)* @lam12827 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15117, i64* %eptr15113                                                 ; store fptr
  %arg10351 = ptrtoint i64* %cloptr15112 to i64                                      ; closure cast; i64* -> i64
  %rva11626 = add i64 0, 0                                                           ; quoted ()
  %rva11625 = call i64 @prim_cons(i64 %arg10351, i64 %rva11626)                      ; call prim_cons
  %cloptr15118 = inttoptr i64 %arg10352 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15119 = getelementptr inbounds i64, i64* %cloptr15118, i64 0                 ; &cloptr15118[0]
  %f15121 = load i64, i64* %i0ptr15119, align 8                                      ; load; *i0ptr15119
  %fptr15120 = inttoptr i64 %f15121 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15120(i64 %arg10352, i64 %rva11625)                 ; tail call
  ret void
}


define void @lam12831(i64 %env12832, i64 %WC0$lst9522) {
  %cont9521 = call i64 @prim_car(i64 %WC0$lst9522)                                   ; call prim_car
  %WC0$lst = call i64 @prim_cdr(i64 %WC0$lst9522)                                    ; call prim_cdr
  %arg10356 = add i64 0, 0                                                           ; quoted ()
  %rva11603 = add i64 0, 0                                                           ; quoted ()
  %rva11602 = call i64 @prim_cons(i64 %WC0$lst, i64 %rva11603)                       ; call prim_cons
  %rva11601 = call i64 @prim_cons(i64 %arg10356, i64 %rva11602)                      ; call prim_cons
  %cloptr15122 = inttoptr i64 %cont9521 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15123 = getelementptr inbounds i64, i64* %cloptr15122, i64 0                 ; &cloptr15122[0]
  %f15125 = load i64, i64* %i0ptr15123, align 8                                      ; load; *i0ptr15123
  %fptr15124 = inttoptr i64 %f15125 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15124(i64 %cont9521, i64 %rva11601)                 ; tail call
  ret void
}


define void @lam12827(i64 %env12828, i64 %rvp11624) {
  %envptr15126 = inttoptr i64 %env12828 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15127 = getelementptr inbounds i64, i64* %envptr15126, i64 3                ; &envptr15126[3]
  %Tkh$next_45cc = load i64, i64* %envptr15127, align 8                              ; load; *envptr15127
  %envptr15128 = inttoptr i64 %env12828 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15129 = getelementptr inbounds i64, i64* %envptr15128, i64 2                ; &envptr15128[2]
  %cont9514 = load i64, i64* %envptr15129, align 8                                   ; load; *envptr15129
  %envptr15130 = inttoptr i64 %env12828 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15131 = getelementptr inbounds i64, i64* %envptr15130, i64 1                ; &envptr15130[1]
  %ZER$ccstack = load i64, i64* %envptr15131, align 8                                ; load; *envptr15131
  %_959519 = call i64 @prim_car(i64 %rvp11624)                                       ; call prim_car
  %rvp11623 = call i64 @prim_cdr(i64 %rvp11624)                                      ; call prim_cdr
  %a9353 = call i64 @prim_car(i64 %rvp11623)                                         ; call prim_car
  %na11605 = call i64 @prim_cdr(i64 %rvp11623)                                       ; call prim_cdr
  %arg10359 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9520 = call i64 @prim_make_45vector(i64 %arg10359, i64 %a9353)             ; call prim_make_45vector
  %cloptr15132 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr15134 = getelementptr inbounds i64, i64* %cloptr15132, i64 1                  ; &eptr15134[1]
  %eptr15135 = getelementptr inbounds i64, i64* %cloptr15132, i64 2                  ; &eptr15135[2]
  %eptr15136 = getelementptr inbounds i64, i64* %cloptr15132, i64 3                  ; &eptr15136[3]
  store i64 %ZER$ccstack, i64* %eptr15134                                            ; *eptr15134 = %ZER$ccstack
  store i64 %cont9514, i64* %eptr15135                                               ; *eptr15135 = %cont9514
  store i64 %Tkh$next_45cc, i64* %eptr15136                                          ; *eptr15136 = %Tkh$next_45cc
  %eptr15133 = getelementptr inbounds i64, i64* %cloptr15132, i64 0                  ; &cloptr15132[0]
  %f15137 = ptrtoint void(i64,i64)* @lam12824 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15137, i64* %eptr15133                                                 ; store fptr
  %arg10362 = ptrtoint i64* %cloptr15132 to i64                                      ; closure cast; i64* -> i64
  %arg10361 = add i64 0, 0                                                           ; quoted ()
  %rva11622 = add i64 0, 0                                                           ; quoted ()
  %rva11621 = call i64 @prim_cons(i64 %retprim9520, i64 %rva11622)                   ; call prim_cons
  %rva11620 = call i64 @prim_cons(i64 %arg10361, i64 %rva11621)                      ; call prim_cons
  %cloptr15138 = inttoptr i64 %arg10362 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15139 = getelementptr inbounds i64, i64* %cloptr15138, i64 0                 ; &cloptr15138[0]
  %f15141 = load i64, i64* %i0ptr15139, align 8                                      ; load; *i0ptr15139
  %fptr15140 = inttoptr i64 %f15141 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15140(i64 %arg10362, i64 %rva11620)                 ; tail call
  ret void
}


define void @lam12824(i64 %env12825, i64 %rvp11619) {
  %envptr15142 = inttoptr i64 %env12825 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15143 = getelementptr inbounds i64, i64* %envptr15142, i64 3                ; &envptr15142[3]
  %Tkh$next_45cc = load i64, i64* %envptr15143, align 8                              ; load; *envptr15143
  %envptr15144 = inttoptr i64 %env12825 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15145 = getelementptr inbounds i64, i64* %envptr15144, i64 2                ; &envptr15144[2]
  %cont9514 = load i64, i64* %envptr15145, align 8                                   ; load; *envptr15145
  %envptr15146 = inttoptr i64 %env12825 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15147 = getelementptr inbounds i64, i64* %envptr15146, i64 1                ; &envptr15146[1]
  %ZER$ccstack = load i64, i64* %envptr15147, align 8                                ; load; *envptr15147
  %_959516 = call i64 @prim_car(i64 %rvp11619)                                       ; call prim_car
  %rvp11618 = call i64 @prim_cdr(i64 %rvp11619)                                      ; call prim_cdr
  %AVD$tmp9187 = call i64 @prim_car(i64 %rvp11618)                                   ; call prim_car
  %na11607 = call i64 @prim_cdr(i64 %rvp11618)                                       ; call prim_cdr
  %arg10363 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9354 = call i64 @prim_vector_45ref(i64 %ZER$ccstack, i64 %arg10363)              ; call prim_vector_45ref
  %a9355 = call i64 @prim_cdr(i64 %a9354)                                            ; call prim_cdr
  %arg10367 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9356 = call i64 @prim_vector_45set_33(i64 %ZER$ccstack, i64 %arg10367, i64 %a9355); call prim_vector_45set_33
  %arg10370 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9518 = call i64 @prim_vector_45set_33(i64 %AVD$tmp9187, i64 %arg10370, i64 %a9356); call prim_vector_45set_33
  %cloptr15148 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr15150 = getelementptr inbounds i64, i64* %cloptr15148, i64 1                  ; &eptr15150[1]
  %eptr15151 = getelementptr inbounds i64, i64* %cloptr15148, i64 2                  ; &eptr15151[2]
  store i64 %cont9514, i64* %eptr15150                                               ; *eptr15150 = %cont9514
  store i64 %Tkh$next_45cc, i64* %eptr15151                                          ; *eptr15151 = %Tkh$next_45cc
  %eptr15149 = getelementptr inbounds i64, i64* %cloptr15148, i64 0                  ; &cloptr15148[0]
  %f15152 = ptrtoint void(i64,i64)* @lam12819 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15152, i64* %eptr15149                                                 ; store fptr
  %arg10374 = ptrtoint i64* %cloptr15148 to i64                                      ; closure cast; i64* -> i64
  %arg10373 = add i64 0, 0                                                           ; quoted ()
  %rva11617 = add i64 0, 0                                                           ; quoted ()
  %rva11616 = call i64 @prim_cons(i64 %retprim9518, i64 %rva11617)                   ; call prim_cons
  %rva11615 = call i64 @prim_cons(i64 %arg10373, i64 %rva11616)                      ; call prim_cons
  %cloptr15153 = inttoptr i64 %arg10374 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15154 = getelementptr inbounds i64, i64* %cloptr15153, i64 0                 ; &cloptr15153[0]
  %f15156 = load i64, i64* %i0ptr15154, align 8                                      ; load; *i0ptr15154
  %fptr15155 = inttoptr i64 %f15156 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15155(i64 %arg10374, i64 %rva11615)                 ; tail call
  ret void
}


define void @lam12819(i64 %env12820, i64 %rvp11614) {
  %envptr15157 = inttoptr i64 %env12820 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15158 = getelementptr inbounds i64, i64* %envptr15157, i64 2                ; &envptr15157[2]
  %Tkh$next_45cc = load i64, i64* %envptr15158, align 8                              ; load; *envptr15158
  %envptr15159 = inttoptr i64 %env12820 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15160 = getelementptr inbounds i64, i64* %envptr15159, i64 1                ; &envptr15159[1]
  %cont9514 = load i64, i64* %envptr15160, align 8                                   ; load; *envptr15160
  %_959517 = call i64 @prim_car(i64 %rvp11614)                                       ; call prim_car
  %rvp11613 = call i64 @prim_cdr(i64 %rvp11614)                                      ; call prim_cdr
  %TlZ$_959215 = call i64 @prim_car(i64 %rvp11613)                                   ; call prim_car
  %na11609 = call i64 @prim_cdr(i64 %rvp11613)                                       ; call prim_cdr
  %rva11612 = add i64 0, 0                                                           ; quoted ()
  %rva11611 = call i64 @prim_cons(i64 %Tkh$next_45cc, i64 %rva11612)                 ; call prim_cons
  %rva11610 = call i64 @prim_cons(i64 %cont9514, i64 %rva11611)                      ; call prim_cons
  %cloptr15161 = inttoptr i64 %Tkh$next_45cc to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr15162 = getelementptr inbounds i64, i64* %cloptr15161, i64 0                 ; &cloptr15161[0]
  %f15164 = load i64, i64* %i0ptr15162, align 8                                      ; load; *i0ptr15162
  %fptr15163 = inttoptr i64 %f15164 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15163(i64 %Tkh$next_45cc, i64 %rva11610)            ; tail call
  ret void
}


define void @lam12807(i64 %env12808, i64 %rvp11641) {
  %envptr15165 = inttoptr i64 %env12808 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15166 = getelementptr inbounds i64, i64* %envptr15165, i64 1                ; &envptr15165[1]
  %OB6$fail = load i64, i64* %envptr15166, align 8                                   ; load; *envptr15166
  %cont9524 = call i64 @prim_car(i64 %rvp11641)                                      ; call prim_car
  %rvp11640 = call i64 @prim_cdr(i64 %rvp11641)                                      ; call prim_cdr
  %TUj$b = call i64 @prim_car(i64 %rvp11640)                                         ; call prim_car
  %na11634 = call i64 @prim_cdr(i64 %rvp11640)                                       ; call prim_cdr
  %cmp15167 = icmp eq i64 %TUj$b, 15                                                 ; false?
  br i1 %cmp15167, label %else15169, label %then15168                                ; if

then15168:
  %arg10379 = add i64 0, 0                                                           ; quoted ()
  %arg10378 = call i64 @const_init_true()                                            ; quoted #t
  %rva11637 = add i64 0, 0                                                           ; quoted ()
  %rva11636 = call i64 @prim_cons(i64 %arg10378, i64 %rva11637)                      ; call prim_cons
  %rva11635 = call i64 @prim_cons(i64 %arg10379, i64 %rva11636)                      ; call prim_cons
  %cloptr15170 = inttoptr i64 %cont9524 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15171 = getelementptr inbounds i64, i64* %cloptr15170, i64 0                 ; &cloptr15170[0]
  %f15173 = load i64, i64* %i0ptr15171, align 8                                      ; load; *i0ptr15171
  %fptr15172 = inttoptr i64 %f15173 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15172(i64 %cont9524, i64 %rva11635)                 ; tail call
  ret void

else15169:
  %arg10381 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9357 = call i64 @prim_vector_45ref(i64 %OB6$fail, i64 %arg10381)                 ; call prim_vector_45ref
  %rva11639 = add i64 0, 0                                                           ; quoted ()
  %rva11638 = call i64 @prim_cons(i64 %cont9524, i64 %rva11639)                      ; call prim_cons
  %cloptr15174 = inttoptr i64 %a9357 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr15175 = getelementptr inbounds i64, i64* %cloptr15174, i64 0                 ; &cloptr15174[0]
  %f15177 = load i64, i64* %i0ptr15175, align 8                                      ; load; *i0ptr15175
  %fptr15176 = inttoptr i64 %f15177 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15176(i64 %a9357, i64 %rva11638)                    ; tail call
  ret void
}


define void @lam12800(i64 %env12801, i64 %rvp11817) {
  %envptr15178 = inttoptr i64 %env12801 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15179 = getelementptr inbounds i64, i64* %envptr15178, i64 4                ; &envptr15178[4]
  %Sea$_37wind_45stack = load i64, i64* %envptr15179, align 8                        ; load; *envptr15179
  %envptr15180 = inttoptr i64 %env12801 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15181 = getelementptr inbounds i64, i64* %envptr15180, i64 3                ; &envptr15180[3]
  %w8C$_37do_45wind = load i64, i64* %envptr15181, align 8                           ; load; *envptr15181
  %envptr15182 = inttoptr i64 %env12801 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15183 = getelementptr inbounds i64, i64* %envptr15182, i64 2                ; &envptr15182[2]
  %OB6$fail = load i64, i64* %envptr15183, align 8                                   ; load; *envptr15183
  %envptr15184 = inttoptr i64 %env12801 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15185 = getelementptr inbounds i64, i64* %envptr15184, i64 1                ; &envptr15184[1]
  %ZER$ccstack = load i64, i64* %envptr15185, align 8                                ; load; *envptr15185
  %cont9525 = call i64 @prim_car(i64 %rvp11817)                                      ; call prim_car
  %rvp11816 = call i64 @prim_cdr(i64 %rvp11817)                                      ; call prim_cdr
  %bYt$lst = call i64 @prim_car(i64 %rvp11816)                                       ; call prim_car
  %na11643 = call i64 @prim_cdr(i64 %rvp11816)                                       ; call prim_cdr
  %arg10386 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %RnY$lst = call i64 @prim_make_45vector(i64 %arg10386, i64 %bYt$lst)               ; call prim_make_45vector
  %cloptr15186 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr15188 = getelementptr inbounds i64, i64* %cloptr15186, i64 1                  ; &eptr15188[1]
  %eptr15189 = getelementptr inbounds i64, i64* %cloptr15186, i64 2                  ; &eptr15189[2]
  store i64 %w8C$_37do_45wind, i64* %eptr15188                                       ; *eptr15188 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr15189                                    ; *eptr15189 = %Sea$_37wind_45stack
  %eptr15187 = getelementptr inbounds i64, i64* %cloptr15186, i64 0                  ; &cloptr15186[0]
  %f15190 = ptrtoint void(i64,i64)* @lam12797 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15190, i64* %eptr15187                                                 ; store fptr
  %arg10389 = ptrtoint i64* %cloptr15186 to i64                                      ; closure cast; i64* -> i64
  %cloptr15191 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15193 = getelementptr inbounds i64, i64* %cloptr15191, i64 1                  ; &eptr15193[1]
  %eptr15194 = getelementptr inbounds i64, i64* %cloptr15191, i64 2                  ; &eptr15194[2]
  %eptr15195 = getelementptr inbounds i64, i64* %cloptr15191, i64 3                  ; &eptr15195[3]
  %eptr15196 = getelementptr inbounds i64, i64* %cloptr15191, i64 4                  ; &eptr15196[4]
  store i64 %RnY$lst, i64* %eptr15193                                                ; *eptr15193 = %RnY$lst
  store i64 %ZER$ccstack, i64* %eptr15194                                            ; *eptr15194 = %ZER$ccstack
  store i64 %cont9525, i64* %eptr15195                                               ; *eptr15195 = %cont9525
  store i64 %OB6$fail, i64* %eptr15196                                               ; *eptr15196 = %OB6$fail
  %eptr15192 = getelementptr inbounds i64, i64* %cloptr15191, i64 0                  ; &cloptr15191[0]
  %f15197 = ptrtoint void(i64,i64)* @lam12774 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15197, i64* %eptr15192                                                 ; store fptr
  %arg10388 = ptrtoint i64* %cloptr15191 to i64                                      ; closure cast; i64* -> i64
  %cloptr15198 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15200 = getelementptr inbounds i64, i64* %cloptr15198, i64 1                  ; &eptr15200[1]
  %eptr15201 = getelementptr inbounds i64, i64* %cloptr15198, i64 2                  ; &eptr15201[2]
  %eptr15202 = getelementptr inbounds i64, i64* %cloptr15198, i64 3                  ; &eptr15202[3]
  %eptr15203 = getelementptr inbounds i64, i64* %cloptr15198, i64 4                  ; &eptr15203[4]
  store i64 %RnY$lst, i64* %eptr15200                                                ; *eptr15200 = %RnY$lst
  store i64 %ZER$ccstack, i64* %eptr15201                                            ; *eptr15201 = %ZER$ccstack
  store i64 %cont9525, i64* %eptr15202                                               ; *eptr15202 = %cont9525
  store i64 %OB6$fail, i64* %eptr15203                                               ; *eptr15203 = %OB6$fail
  %eptr15199 = getelementptr inbounds i64, i64* %cloptr15198, i64 0                  ; &cloptr15198[0]
  %f15204 = ptrtoint void(i64,i64)* @lam12724 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15204, i64* %eptr15199                                                 ; store fptr
  %arg10387 = ptrtoint i64* %cloptr15198 to i64                                      ; closure cast; i64* -> i64
  %rva11815 = add i64 0, 0                                                           ; quoted ()
  %rva11814 = call i64 @prim_cons(i64 %arg10387, i64 %rva11815)                      ; call prim_cons
  %rva11813 = call i64 @prim_cons(i64 %arg10388, i64 %rva11814)                      ; call prim_cons
  %cloptr15205 = inttoptr i64 %arg10389 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15206 = getelementptr inbounds i64, i64* %cloptr15205, i64 0                 ; &cloptr15205[0]
  %f15208 = load i64, i64* %i0ptr15206, align 8                                      ; load; *i0ptr15206
  %fptr15207 = inttoptr i64 %f15208 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15207(i64 %arg10389, i64 %rva11813)                 ; tail call
  ret void
}


define void @lam12797(i64 %env12798, i64 %rvp11688) {
  %envptr15209 = inttoptr i64 %env12798 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15210 = getelementptr inbounds i64, i64* %envptr15209, i64 2                ; &envptr15209[2]
  %Sea$_37wind_45stack = load i64, i64* %envptr15210, align 8                        ; load; *envptr15210
  %envptr15211 = inttoptr i64 %env12798 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15212 = getelementptr inbounds i64, i64* %envptr15211, i64 1                ; &envptr15211[1]
  %w8C$_37do_45wind = load i64, i64* %envptr15212, align 8                           ; load; *envptr15212
  %cont9543 = call i64 @prim_car(i64 %rvp11688)                                      ; call prim_car
  %rvp11687 = call i64 @prim_cdr(i64 %rvp11688)                                      ; call prim_cdr
  %CN0$_37k = call i64 @prim_car(i64 %rvp11687)                                      ; call prim_car
  %na11645 = call i64 @prim_cdr(i64 %rvp11687)                                       ; call prim_cdr
  %arg10390 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %Lcl$_37saved_45stack = call i64 @prim_vector_45ref(i64 %Sea$_37wind_45stack, i64 %arg10390); call prim_vector_45ref
  %cloptr15213 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr15215 = getelementptr inbounds i64, i64* %cloptr15213, i64 1                  ; &eptr15215[1]
  store i64 %cont9543, i64* %eptr15215                                               ; *eptr15215 = %cont9543
  %eptr15214 = getelementptr inbounds i64, i64* %cloptr15213, i64 0                  ; &cloptr15213[0]
  %f15216 = ptrtoint void(i64,i64)* @lam12794 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15216, i64* %eptr15214                                                 ; store fptr
  %arg10394 = ptrtoint i64* %cloptr15213 to i64                                      ; closure cast; i64* -> i64
  %arg10393 = add i64 0, 0                                                           ; quoted ()
  %cloptr15217 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15219 = getelementptr inbounds i64, i64* %cloptr15217, i64 1                  ; &eptr15219[1]
  %eptr15220 = getelementptr inbounds i64, i64* %cloptr15217, i64 2                  ; &eptr15220[2]
  %eptr15221 = getelementptr inbounds i64, i64* %cloptr15217, i64 3                  ; &eptr15221[3]
  %eptr15222 = getelementptr inbounds i64, i64* %cloptr15217, i64 4                  ; &eptr15222[4]
  store i64 %Lcl$_37saved_45stack, i64* %eptr15219                                   ; *eptr15219 = %Lcl$_37saved_45stack
  store i64 %w8C$_37do_45wind, i64* %eptr15220                                       ; *eptr15220 = %w8C$_37do_45wind
  store i64 %Sea$_37wind_45stack, i64* %eptr15221                                    ; *eptr15221 = %Sea$_37wind_45stack
  store i64 %CN0$_37k, i64* %eptr15222                                               ; *eptr15222 = %CN0$_37k
  %eptr15218 = getelementptr inbounds i64, i64* %cloptr15217, i64 0                  ; &cloptr15217[0]
  %f15223 = ptrtoint void(i64,i64)* @lam12787 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15223, i64* %eptr15218                                                 ; store fptr
  %arg10392 = ptrtoint i64* %cloptr15217 to i64                                      ; closure cast; i64* -> i64
  %rva11686 = add i64 0, 0                                                           ; quoted ()
  %rva11685 = call i64 @prim_cons(i64 %arg10392, i64 %rva11686)                      ; call prim_cons
  %rva11684 = call i64 @prim_cons(i64 %arg10393, i64 %rva11685)                      ; call prim_cons
  %cloptr15224 = inttoptr i64 %arg10394 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15225 = getelementptr inbounds i64, i64* %cloptr15224, i64 0                 ; &cloptr15224[0]
  %f15227 = load i64, i64* %i0ptr15225, align 8                                      ; load; *i0ptr15225
  %fptr15226 = inttoptr i64 %f15227 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15226(i64 %arg10394, i64 %rva11684)                 ; tail call
  ret void
}


define void @lam12794(i64 %env12795, i64 %rvp11659) {
  %envptr15228 = inttoptr i64 %env12795 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15229 = getelementptr inbounds i64, i64* %envptr15228, i64 1                ; &envptr15228[1]
  %cont9543 = load i64, i64* %envptr15229, align 8                                   ; load; *envptr15229
  %_959544 = call i64 @prim_car(i64 %rvp11659)                                       ; call prim_car
  %rvp11658 = call i64 @prim_cdr(i64 %rvp11659)                                      ; call prim_cdr
  %a9360 = call i64 @prim_car(i64 %rvp11658)                                         ; call prim_car
  %na11647 = call i64 @prim_cdr(i64 %rvp11658)                                       ; call prim_cdr
  %cloptr15230 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15231 = getelementptr inbounds i64, i64* %cloptr15230, i64 0                  ; &cloptr15230[0]
  %f15232 = ptrtoint void(i64,i64)* @lam12792 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15232, i64* %eptr15231                                                 ; store fptr
  %arg10397 = ptrtoint i64* %cloptr15230 to i64                                      ; closure cast; i64* -> i64
  %rva11657 = add i64 0, 0                                                           ; quoted ()
  %rva11656 = call i64 @prim_cons(i64 %a9360, i64 %rva11657)                         ; call prim_cons
  %rva11655 = call i64 @prim_cons(i64 %cont9543, i64 %rva11656)                      ; call prim_cons
  %cloptr15233 = inttoptr i64 %arg10397 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15234 = getelementptr inbounds i64, i64* %cloptr15233, i64 0                 ; &cloptr15233[0]
  %f15236 = load i64, i64* %i0ptr15234, align 8                                      ; load; *i0ptr15234
  %fptr15235 = inttoptr i64 %f15236 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15235(i64 %arg10397, i64 %rva11655)                 ; tail call
  ret void
}


define void @lam12792(i64 %env12793, i64 %rvp11654) {
  %cont9545 = call i64 @prim_car(i64 %rvp11654)                                      ; call prim_car
  %rvp11653 = call i64 @prim_cdr(i64 %rvp11654)                                      ; call prim_cdr
  %Vlj$u = call i64 @prim_car(i64 %rvp11653)                                         ; call prim_car
  %na11649 = call i64 @prim_cdr(i64 %rvp11653)                                       ; call prim_cdr
  %rva11652 = add i64 0, 0                                                           ; quoted ()
  %rva11651 = call i64 @prim_cons(i64 %Vlj$u, i64 %rva11652)                         ; call prim_cons
  %rva11650 = call i64 @prim_cons(i64 %cont9545, i64 %rva11651)                      ; call prim_cons
  %cloptr15237 = inttoptr i64 %Vlj$u to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr15238 = getelementptr inbounds i64, i64* %cloptr15237, i64 0                 ; &cloptr15237[0]
  %f15240 = load i64, i64* %i0ptr15238, align 8                                      ; load; *i0ptr15238
  %fptr15239 = inttoptr i64 %f15240 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15239(i64 %Vlj$u, i64 %rva11650)                    ; tail call
  ret void
}


define void @lam12787(i64 %env12788, i64 %rvp11683) {
  %envptr15241 = inttoptr i64 %env12788 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15242 = getelementptr inbounds i64, i64* %envptr15241, i64 4                ; &envptr15241[4]
  %CN0$_37k = load i64, i64* %envptr15242, align 8                                   ; load; *envptr15242
  %envptr15243 = inttoptr i64 %env12788 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15244 = getelementptr inbounds i64, i64* %envptr15243, i64 3                ; &envptr15243[3]
  %Sea$_37wind_45stack = load i64, i64* %envptr15244, align 8                        ; load; *envptr15244
  %envptr15245 = inttoptr i64 %env12788 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15246 = getelementptr inbounds i64, i64* %envptr15245, i64 2                ; &envptr15245[2]
  %w8C$_37do_45wind = load i64, i64* %envptr15246, align 8                           ; load; *envptr15246
  %envptr15247 = inttoptr i64 %env12788 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15248 = getelementptr inbounds i64, i64* %envptr15247, i64 1                ; &envptr15247[1]
  %Lcl$_37saved_45stack = load i64, i64* %envptr15248, align 8                       ; load; *envptr15248
  %cont9546 = call i64 @prim_car(i64 %rvp11683)                                      ; call prim_car
  %rvp11682 = call i64 @prim_cdr(i64 %rvp11683)                                      ; call prim_cdr
  %okk$_37x = call i64 @prim_car(i64 %rvp11682)                                      ; call prim_car
  %na11661 = call i64 @prim_cdr(i64 %rvp11682)                                       ; call prim_cdr
  %arg10401 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9358 = call i64 @prim_vector_45ref(i64 %Sea$_37wind_45stack, i64 %arg10401)      ; call prim_vector_45ref
  %a9359 = call i64 @prim_eq_63(i64 %Lcl$_37saved_45stack, i64 %a9358)               ; call prim_eq_63
  %cmp15249 = icmp eq i64 %a9359, 15                                                 ; false?
  br i1 %cmp15249, label %else15251, label %then15250                                ; if

then15250:
  %retprim9548 = call i64 @prim_void()                                               ; call prim_void
  %cloptr15252 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr15254 = getelementptr inbounds i64, i64* %cloptr15252, i64 1                  ; &eptr15254[1]
  %eptr15255 = getelementptr inbounds i64, i64* %cloptr15252, i64 2                  ; &eptr15255[2]
  %eptr15256 = getelementptr inbounds i64, i64* %cloptr15252, i64 3                  ; &eptr15256[3]
  store i64 %okk$_37x, i64* %eptr15254                                               ; *eptr15254 = %okk$_37x
  store i64 %CN0$_37k, i64* %eptr15255                                               ; *eptr15255 = %CN0$_37k
  store i64 %cont9546, i64* %eptr15256                                               ; *eptr15256 = %cont9546
  %eptr15253 = getelementptr inbounds i64, i64* %cloptr15252, i64 0                  ; &cloptr15252[0]
  %f15257 = ptrtoint void(i64,i64)* @lam12780 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15257, i64* %eptr15253                                                 ; store fptr
  %arg10407 = ptrtoint i64* %cloptr15252 to i64                                      ; closure cast; i64* -> i64
  %arg10406 = add i64 0, 0                                                           ; quoted ()
  %rva11671 = add i64 0, 0                                                           ; quoted ()
  %rva11670 = call i64 @prim_cons(i64 %retprim9548, i64 %rva11671)                   ; call prim_cons
  %rva11669 = call i64 @prim_cons(i64 %arg10406, i64 %rva11670)                      ; call prim_cons
  %cloptr15258 = inttoptr i64 %arg10407 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15259 = getelementptr inbounds i64, i64* %cloptr15258, i64 0                 ; &cloptr15258[0]
  %f15261 = load i64, i64* %i0ptr15259, align 8                                      ; load; *i0ptr15259
  %fptr15260 = inttoptr i64 %f15261 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15260(i64 %arg10407, i64 %rva11669)                 ; tail call
  ret void

else15251:
  %cloptr15262 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr15264 = getelementptr inbounds i64, i64* %cloptr15262, i64 1                  ; &eptr15264[1]
  %eptr15265 = getelementptr inbounds i64, i64* %cloptr15262, i64 2                  ; &eptr15265[2]
  %eptr15266 = getelementptr inbounds i64, i64* %cloptr15262, i64 3                  ; &eptr15266[3]
  store i64 %okk$_37x, i64* %eptr15264                                               ; *eptr15264 = %okk$_37x
  store i64 %CN0$_37k, i64* %eptr15265                                               ; *eptr15265 = %CN0$_37k
  store i64 %cont9546, i64* %eptr15266                                               ; *eptr15266 = %cont9546
  %eptr15263 = getelementptr inbounds i64, i64* %cloptr15262, i64 0                  ; &cloptr15262[0]
  %f15267 = ptrtoint void(i64,i64)* @lam12784 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15267, i64* %eptr15263                                                 ; store fptr
  %arg10412 = ptrtoint i64* %cloptr15262 to i64                                      ; closure cast; i64* -> i64
  %rva11681 = add i64 0, 0                                                           ; quoted ()
  %rva11680 = call i64 @prim_cons(i64 %Lcl$_37saved_45stack, i64 %rva11681)          ; call prim_cons
  %rva11679 = call i64 @prim_cons(i64 %arg10412, i64 %rva11680)                      ; call prim_cons
  %cloptr15268 = inttoptr i64 %w8C$_37do_45wind to i64*                              ; closure/env cast; i64 -> i64*
  %i0ptr15269 = getelementptr inbounds i64, i64* %cloptr15268, i64 0                 ; &cloptr15268[0]
  %f15271 = load i64, i64* %i0ptr15269, align 8                                      ; load; *i0ptr15269
  %fptr15270 = inttoptr i64 %f15271 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15270(i64 %w8C$_37do_45wind, i64 %rva11679)         ; tail call
  ret void
}


define void @lam12784(i64 %env12785, i64 %rvp11678) {
  %envptr15272 = inttoptr i64 %env12785 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15273 = getelementptr inbounds i64, i64* %envptr15272, i64 3                ; &envptr15272[3]
  %cont9546 = load i64, i64* %envptr15273, align 8                                   ; load; *envptr15273
  %envptr15274 = inttoptr i64 %env12785 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15275 = getelementptr inbounds i64, i64* %envptr15274, i64 2                ; &envptr15274[2]
  %CN0$_37k = load i64, i64* %envptr15275, align 8                                   ; load; *envptr15275
  %envptr15276 = inttoptr i64 %env12785 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15277 = getelementptr inbounds i64, i64* %envptr15276, i64 1                ; &envptr15276[1]
  %okk$_37x = load i64, i64* %envptr15277, align 8                                   ; load; *envptr15277
  %_959547 = call i64 @prim_car(i64 %rvp11678)                                       ; call prim_car
  %rvp11677 = call i64 @prim_cdr(i64 %rvp11678)                                      ; call prim_cdr
  %zqA$_959216 = call i64 @prim_car(i64 %rvp11677)                                   ; call prim_car
  %na11673 = call i64 @prim_cdr(i64 %rvp11677)                                       ; call prim_cdr
  %rva11676 = add i64 0, 0                                                           ; quoted ()
  %rva11675 = call i64 @prim_cons(i64 %okk$_37x, i64 %rva11676)                      ; call prim_cons
  %rva11674 = call i64 @prim_cons(i64 %cont9546, i64 %rva11675)                      ; call prim_cons
  %cloptr15278 = inttoptr i64 %CN0$_37k to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15279 = getelementptr inbounds i64, i64* %cloptr15278, i64 0                 ; &cloptr15278[0]
  %f15281 = load i64, i64* %i0ptr15279, align 8                                      ; load; *i0ptr15279
  %fptr15280 = inttoptr i64 %f15281 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15280(i64 %CN0$_37k, i64 %rva11674)                 ; tail call
  ret void
}


define void @lam12780(i64 %env12781, i64 %rvp11668) {
  %envptr15282 = inttoptr i64 %env12781 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15283 = getelementptr inbounds i64, i64* %envptr15282, i64 3                ; &envptr15282[3]
  %cont9546 = load i64, i64* %envptr15283, align 8                                   ; load; *envptr15283
  %envptr15284 = inttoptr i64 %env12781 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15285 = getelementptr inbounds i64, i64* %envptr15284, i64 2                ; &envptr15284[2]
  %CN0$_37k = load i64, i64* %envptr15285, align 8                                   ; load; *envptr15285
  %envptr15286 = inttoptr i64 %env12781 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15287 = getelementptr inbounds i64, i64* %envptr15286, i64 1                ; &envptr15286[1]
  %okk$_37x = load i64, i64* %envptr15287, align 8                                   ; load; *envptr15287
  %_959547 = call i64 @prim_car(i64 %rvp11668)                                       ; call prim_car
  %rvp11667 = call i64 @prim_cdr(i64 %rvp11668)                                      ; call prim_cdr
  %zqA$_959216 = call i64 @prim_car(i64 %rvp11667)                                   ; call prim_car
  %na11663 = call i64 @prim_cdr(i64 %rvp11667)                                       ; call prim_cdr
  %rva11666 = add i64 0, 0                                                           ; quoted ()
  %rva11665 = call i64 @prim_cons(i64 %okk$_37x, i64 %rva11666)                      ; call prim_cons
  %rva11664 = call i64 @prim_cons(i64 %cont9546, i64 %rva11665)                      ; call prim_cons
  %cloptr15288 = inttoptr i64 %CN0$_37k to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15289 = getelementptr inbounds i64, i64* %cloptr15288, i64 0                 ; &cloptr15288[0]
  %f15291 = load i64, i64* %i0ptr15289, align 8                                      ; load; *i0ptr15289
  %fptr15290 = inttoptr i64 %f15291 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15290(i64 %CN0$_37k, i64 %rva11664)                 ; tail call
  ret void
}


define void @lam12774(i64 %env12775, i64 %rvp11750) {
  %envptr15292 = inttoptr i64 %env12775 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15293 = getelementptr inbounds i64, i64* %envptr15292, i64 4                ; &envptr15292[4]
  %OB6$fail = load i64, i64* %envptr15293, align 8                                   ; load; *envptr15293
  %envptr15294 = inttoptr i64 %env12775 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15295 = getelementptr inbounds i64, i64* %envptr15294, i64 3                ; &envptr15294[3]
  %cont9525 = load i64, i64* %envptr15295, align 8                                   ; load; *envptr15295
  %envptr15296 = inttoptr i64 %env12775 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15297 = getelementptr inbounds i64, i64* %envptr15296, i64 2                ; &envptr15296[2]
  %ZER$ccstack = load i64, i64* %envptr15297, align 8                                ; load; *envptr15297
  %envptr15298 = inttoptr i64 %env12775 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15299 = getelementptr inbounds i64, i64* %envptr15298, i64 1                ; &envptr15298[1]
  %RnY$lst = load i64, i64* %envptr15299, align 8                                    ; load; *envptr15299
  %_959526 = call i64 @prim_car(i64 %rvp11750)                                       ; call prim_car
  %rvp11749 = call i64 @prim_cdr(i64 %rvp11750)                                      ; call prim_cdr
  %LeX$cc = call i64 @prim_car(i64 %rvp11749)                                        ; call prim_car
  %na11690 = call i64 @prim_cdr(i64 %rvp11749)                                       ; call prim_cdr
  %arg10417 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9361 = call i64 @prim_vector_45ref(i64 %RnY$lst, i64 %arg10417)                  ; call prim_vector_45ref
  %a9362 = call i64 @prim_null_63(i64 %a9361)                                        ; call prim_null_63
  %cmp15300 = icmp eq i64 %a9362, 15                                                 ; false?
  br i1 %cmp15300, label %else15302, label %then15301                                ; if

then15301:
  %arg10420 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9363 = call i64 @prim_vector_45ref(i64 %OB6$fail, i64 %arg10420)                 ; call prim_vector_45ref
  %rva11692 = add i64 0, 0                                                           ; quoted ()
  %rva11691 = call i64 @prim_cons(i64 %cont9525, i64 %rva11692)                      ; call prim_cons
  %cloptr15303 = inttoptr i64 %a9363 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr15304 = getelementptr inbounds i64, i64* %cloptr15303, i64 0                 ; &cloptr15303[0]
  %f15306 = load i64, i64* %i0ptr15304, align 8                                      ; load; *i0ptr15304
  %fptr15305 = inttoptr i64 %f15306 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15305(i64 %a9363, i64 %rva11691)                    ; tail call
  ret void

else15302:
  %arg10424 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9364 = call i64 @prim_vector_45ref(i64 %RnY$lst, i64 %arg10424)                  ; call prim_vector_45ref
  %retprim9542 = call i64 @prim_car(i64 %a9364)                                      ; call prim_car
  %cloptr15307 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15309 = getelementptr inbounds i64, i64* %cloptr15307, i64 1                  ; &eptr15309[1]
  %eptr15310 = getelementptr inbounds i64, i64* %cloptr15307, i64 2                  ; &eptr15310[2]
  %eptr15311 = getelementptr inbounds i64, i64* %cloptr15307, i64 3                  ; &eptr15311[3]
  %eptr15312 = getelementptr inbounds i64, i64* %cloptr15307, i64 4                  ; &eptr15312[4]
  store i64 %RnY$lst, i64* %eptr15309                                                ; *eptr15309 = %RnY$lst
  store i64 %LeX$cc, i64* %eptr15310                                                 ; *eptr15310 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15311                                            ; *eptr15311 = %ZER$ccstack
  store i64 %cont9525, i64* %eptr15312                                               ; *eptr15312 = %cont9525
  %eptr15308 = getelementptr inbounds i64, i64* %cloptr15307, i64 0                  ; &cloptr15307[0]
  %f15313 = ptrtoint void(i64,i64)* @lam12770 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15313, i64* %eptr15308                                                 ; store fptr
  %arg10429 = ptrtoint i64* %cloptr15307 to i64                                      ; closure cast; i64* -> i64
  %arg10428 = add i64 0, 0                                                           ; quoted ()
  %rva11748 = add i64 0, 0                                                           ; quoted ()
  %rva11747 = call i64 @prim_cons(i64 %retprim9542, i64 %rva11748)                   ; call prim_cons
  %rva11746 = call i64 @prim_cons(i64 %arg10428, i64 %rva11747)                      ; call prim_cons
  %cloptr15314 = inttoptr i64 %arg10429 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15315 = getelementptr inbounds i64, i64* %cloptr15314, i64 0                 ; &cloptr15314[0]
  %f15317 = load i64, i64* %i0ptr15315, align 8                                      ; load; *i0ptr15315
  %fptr15316 = inttoptr i64 %f15317 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15316(i64 %arg10429, i64 %rva11746)                 ; tail call
  ret void
}


define void @lam12770(i64 %env12771, i64 %rvp11745) {
  %envptr15318 = inttoptr i64 %env12771 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15319 = getelementptr inbounds i64, i64* %envptr15318, i64 4                ; &envptr15318[4]
  %cont9525 = load i64, i64* %envptr15319, align 8                                   ; load; *envptr15319
  %envptr15320 = inttoptr i64 %env12771 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15321 = getelementptr inbounds i64, i64* %envptr15320, i64 3                ; &envptr15320[3]
  %ZER$ccstack = load i64, i64* %envptr15321, align 8                                ; load; *envptr15321
  %envptr15322 = inttoptr i64 %env12771 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15323 = getelementptr inbounds i64, i64* %envptr15322, i64 2                ; &envptr15322[2]
  %LeX$cc = load i64, i64* %envptr15323, align 8                                     ; load; *envptr15323
  %envptr15324 = inttoptr i64 %env12771 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15325 = getelementptr inbounds i64, i64* %envptr15324, i64 1                ; &envptr15324[1]
  %RnY$lst = load i64, i64* %envptr15325, align 8                                    ; load; *envptr15325
  %_959527 = call i64 @prim_car(i64 %rvp11745)                                       ; call prim_car
  %rvp11744 = call i64 @prim_cdr(i64 %rvp11745)                                      ; call prim_cdr
  %mnc$head = call i64 @prim_car(i64 %rvp11744)                                      ; call prim_car
  %na11694 = call i64 @prim_cdr(i64 %rvp11744)                                       ; call prim_cdr
  %cloptr15326 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15327 = getelementptr inbounds i64, i64* %cloptr15326, i64 0                  ; &cloptr15326[0]
  %f15328 = ptrtoint void(i64,i64)* @lam12768 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15328, i64* %eptr15327                                                 ; store fptr
  %arg10431 = ptrtoint i64* %cloptr15326 to i64                                      ; closure cast; i64* -> i64
  %cloptr15329 = call i64* @alloc(i64 48)                                            ; malloc
  %eptr15331 = getelementptr inbounds i64, i64* %cloptr15329, i64 1                  ; &eptr15331[1]
  %eptr15332 = getelementptr inbounds i64, i64* %cloptr15329, i64 2                  ; &eptr15332[2]
  %eptr15333 = getelementptr inbounds i64, i64* %cloptr15329, i64 3                  ; &eptr15333[3]
  %eptr15334 = getelementptr inbounds i64, i64* %cloptr15329, i64 4                  ; &eptr15334[4]
  %eptr15335 = getelementptr inbounds i64, i64* %cloptr15329, i64 5                  ; &eptr15335[5]
  store i64 %mnc$head, i64* %eptr15331                                               ; *eptr15331 = %mnc$head
  store i64 %RnY$lst, i64* %eptr15332                                                ; *eptr15332 = %RnY$lst
  store i64 %LeX$cc, i64* %eptr15333                                                 ; *eptr15333 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15334                                            ; *eptr15334 = %ZER$ccstack
  store i64 %cont9525, i64* %eptr15335                                               ; *eptr15335 = %cont9525
  %eptr15330 = getelementptr inbounds i64, i64* %cloptr15329, i64 0                  ; &cloptr15329[0]
  %f15336 = ptrtoint void(i64,i64)* @lam12764 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15336, i64* %eptr15330                                                 ; store fptr
  %arg10430 = ptrtoint i64* %cloptr15329 to i64                                      ; closure cast; i64* -> i64
  %rva11743 = add i64 0, 0                                                           ; quoted ()
  %rva11742 = call i64 @prim_cons(i64 %arg10430, i64 %rva11743)                      ; call prim_cons
  %cloptr15337 = inttoptr i64 %arg10431 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15338 = getelementptr inbounds i64, i64* %cloptr15337, i64 0                 ; &cloptr15337[0]
  %f15340 = load i64, i64* %i0ptr15338, align 8                                      ; load; *i0ptr15338
  %fptr15339 = inttoptr i64 %f15340 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15339(i64 %arg10431, i64 %rva11742)                 ; tail call
  ret void
}


define void @lam12768(i64 %env12769, i64 %rnt$lst9541) {
  %cont9540 = call i64 @prim_car(i64 %rnt$lst9541)                                   ; call prim_car
  %rnt$lst = call i64 @prim_cdr(i64 %rnt$lst9541)                                    ; call prim_cdr
  %arg10435 = add i64 0, 0                                                           ; quoted ()
  %rva11697 = add i64 0, 0                                                           ; quoted ()
  %rva11696 = call i64 @prim_cons(i64 %rnt$lst, i64 %rva11697)                       ; call prim_cons
  %rva11695 = call i64 @prim_cons(i64 %arg10435, i64 %rva11696)                      ; call prim_cons
  %cloptr15341 = inttoptr i64 %cont9540 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15342 = getelementptr inbounds i64, i64* %cloptr15341, i64 0                 ; &cloptr15341[0]
  %f15344 = load i64, i64* %i0ptr15342, align 8                                      ; load; *i0ptr15342
  %fptr15343 = inttoptr i64 %f15344 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15343(i64 %cont9540, i64 %rva11695)                 ; tail call
  ret void
}


define void @lam12764(i64 %env12765, i64 %rvp11741) {
  %envptr15345 = inttoptr i64 %env12765 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15346 = getelementptr inbounds i64, i64* %envptr15345, i64 5                ; &envptr15345[5]
  %cont9525 = load i64, i64* %envptr15346, align 8                                   ; load; *envptr15346
  %envptr15347 = inttoptr i64 %env12765 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15348 = getelementptr inbounds i64, i64* %envptr15347, i64 4                ; &envptr15347[4]
  %ZER$ccstack = load i64, i64* %envptr15348, align 8                                ; load; *envptr15348
  %envptr15349 = inttoptr i64 %env12765 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15350 = getelementptr inbounds i64, i64* %envptr15349, i64 3                ; &envptr15349[3]
  %LeX$cc = load i64, i64* %envptr15350, align 8                                     ; load; *envptr15350
  %envptr15351 = inttoptr i64 %env12765 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15352 = getelementptr inbounds i64, i64* %envptr15351, i64 2                ; &envptr15351[2]
  %RnY$lst = load i64, i64* %envptr15352, align 8                                    ; load; *envptr15352
  %envptr15353 = inttoptr i64 %env12765 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15354 = getelementptr inbounds i64, i64* %envptr15353, i64 1                ; &envptr15353[1]
  %mnc$head = load i64, i64* %envptr15354, align 8                                   ; load; *envptr15354
  %_959538 = call i64 @prim_car(i64 %rvp11741)                                       ; call prim_car
  %rvp11740 = call i64 @prim_cdr(i64 %rvp11741)                                      ; call prim_cdr
  %a9365 = call i64 @prim_car(i64 %rvp11740)                                         ; call prim_car
  %na11699 = call i64 @prim_cdr(i64 %rvp11740)                                       ; call prim_cdr
  %arg10438 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9539 = call i64 @prim_make_45vector(i64 %arg10438, i64 %a9365)             ; call prim_make_45vector
  %cloptr15355 = call i64* @alloc(i64 48)                                            ; malloc
  %eptr15357 = getelementptr inbounds i64, i64* %cloptr15355, i64 1                  ; &eptr15357[1]
  %eptr15358 = getelementptr inbounds i64, i64* %cloptr15355, i64 2                  ; &eptr15358[2]
  %eptr15359 = getelementptr inbounds i64, i64* %cloptr15355, i64 3                  ; &eptr15359[3]
  %eptr15360 = getelementptr inbounds i64, i64* %cloptr15355, i64 4                  ; &eptr15360[4]
  %eptr15361 = getelementptr inbounds i64, i64* %cloptr15355, i64 5                  ; &eptr15361[5]
  store i64 %mnc$head, i64* %eptr15357                                               ; *eptr15357 = %mnc$head
  store i64 %RnY$lst, i64* %eptr15358                                                ; *eptr15358 = %RnY$lst
  store i64 %LeX$cc, i64* %eptr15359                                                 ; *eptr15359 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15360                                            ; *eptr15360 = %ZER$ccstack
  store i64 %cont9525, i64* %eptr15361                                               ; *eptr15361 = %cont9525
  %eptr15356 = getelementptr inbounds i64, i64* %cloptr15355, i64 0                  ; &cloptr15355[0]
  %f15362 = ptrtoint void(i64,i64)* @lam12761 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15362, i64* %eptr15356                                                 ; store fptr
  %arg10441 = ptrtoint i64* %cloptr15355 to i64                                      ; closure cast; i64* -> i64
  %arg10440 = add i64 0, 0                                                           ; quoted ()
  %rva11739 = add i64 0, 0                                                           ; quoted ()
  %rva11738 = call i64 @prim_cons(i64 %retprim9539, i64 %rva11739)                   ; call prim_cons
  %rva11737 = call i64 @prim_cons(i64 %arg10440, i64 %rva11738)                      ; call prim_cons
  %cloptr15363 = inttoptr i64 %arg10441 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15364 = getelementptr inbounds i64, i64* %cloptr15363, i64 0                 ; &cloptr15363[0]
  %f15366 = load i64, i64* %i0ptr15364, align 8                                      ; load; *i0ptr15364
  %fptr15365 = inttoptr i64 %f15366 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15365(i64 %arg10441, i64 %rva11737)                 ; tail call
  ret void
}


define void @lam12761(i64 %env12762, i64 %rvp11736) {
  %envptr15367 = inttoptr i64 %env12762 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15368 = getelementptr inbounds i64, i64* %envptr15367, i64 5                ; &envptr15367[5]
  %cont9525 = load i64, i64* %envptr15368, align 8                                   ; load; *envptr15368
  %envptr15369 = inttoptr i64 %env12762 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15370 = getelementptr inbounds i64, i64* %envptr15369, i64 4                ; &envptr15369[4]
  %ZER$ccstack = load i64, i64* %envptr15370, align 8                                ; load; *envptr15370
  %envptr15371 = inttoptr i64 %env12762 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15372 = getelementptr inbounds i64, i64* %envptr15371, i64 3                ; &envptr15371[3]
  %LeX$cc = load i64, i64* %envptr15372, align 8                                     ; load; *envptr15372
  %envptr15373 = inttoptr i64 %env12762 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15374 = getelementptr inbounds i64, i64* %envptr15373, i64 2                ; &envptr15373[2]
  %RnY$lst = load i64, i64* %envptr15374, align 8                                    ; load; *envptr15374
  %envptr15375 = inttoptr i64 %env12762 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15376 = getelementptr inbounds i64, i64* %envptr15375, i64 1                ; &envptr15375[1]
  %mnc$head = load i64, i64* %envptr15376, align 8                                   ; load; *envptr15376
  %_959528 = call i64 @prim_car(i64 %rvp11736)                                       ; call prim_car
  %rvp11735 = call i64 @prim_cdr(i64 %rvp11736)                                      ; call prim_cdr
  %G80$tmp9199 = call i64 @prim_car(i64 %rvp11735)                                   ; call prim_car
  %na11701 = call i64 @prim_cdr(i64 %rvp11735)                                       ; call prim_cdr
  %cloptr15377 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15378 = getelementptr inbounds i64, i64* %cloptr15377, i64 0                  ; &cloptr15377[0]
  %f15379 = ptrtoint void(i64,i64)* @lam12759 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15379, i64* %eptr15378                                                 ; store fptr
  %arg10443 = ptrtoint i64* %cloptr15377 to i64                                      ; closure cast; i64* -> i64
  %cloptr15380 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr15382 = getelementptr inbounds i64, i64* %cloptr15380, i64 1                  ; &eptr15382[1]
  %eptr15383 = getelementptr inbounds i64, i64* %cloptr15380, i64 2                  ; &eptr15383[2]
  %eptr15384 = getelementptr inbounds i64, i64* %cloptr15380, i64 3                  ; &eptr15384[3]
  %eptr15385 = getelementptr inbounds i64, i64* %cloptr15380, i64 4                  ; &eptr15385[4]
  %eptr15386 = getelementptr inbounds i64, i64* %cloptr15380, i64 5                  ; &eptr15386[5]
  %eptr15387 = getelementptr inbounds i64, i64* %cloptr15380, i64 6                  ; &eptr15387[6]
  store i64 %mnc$head, i64* %eptr15382                                               ; *eptr15382 = %mnc$head
  store i64 %RnY$lst, i64* %eptr15383                                                ; *eptr15383 = %RnY$lst
  store i64 %LeX$cc, i64* %eptr15384                                                 ; *eptr15384 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15385                                            ; *eptr15385 = %ZER$ccstack
  store i64 %G80$tmp9199, i64* %eptr15386                                            ; *eptr15386 = %G80$tmp9199
  store i64 %cont9525, i64* %eptr15387                                               ; *eptr15387 = %cont9525
  %eptr15381 = getelementptr inbounds i64, i64* %cloptr15380, i64 0                  ; &cloptr15380[0]
  %f15388 = ptrtoint void(i64,i64)* @lam12755 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15388, i64* %eptr15381                                                 ; store fptr
  %arg10442 = ptrtoint i64* %cloptr15380 to i64                                      ; closure cast; i64* -> i64
  %rva11734 = add i64 0, 0                                                           ; quoted ()
  %rva11733 = call i64 @prim_cons(i64 %arg10442, i64 %rva11734)                      ; call prim_cons
  %cloptr15389 = inttoptr i64 %arg10443 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15390 = getelementptr inbounds i64, i64* %cloptr15389, i64 0                 ; &cloptr15389[0]
  %f15392 = load i64, i64* %i0ptr15390, align 8                                      ; load; *i0ptr15390
  %fptr15391 = inttoptr i64 %f15392 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15391(i64 %arg10443, i64 %rva11733)                 ; tail call
  ret void
}


define void @lam12759(i64 %env12760, i64 %rix$lst9537) {
  %cont9536 = call i64 @prim_car(i64 %rix$lst9537)                                   ; call prim_car
  %rix$lst = call i64 @prim_cdr(i64 %rix$lst9537)                                    ; call prim_cdr
  %arg10447 = add i64 0, 0                                                           ; quoted ()
  %rva11704 = add i64 0, 0                                                           ; quoted ()
  %rva11703 = call i64 @prim_cons(i64 %rix$lst, i64 %rva11704)                       ; call prim_cons
  %rva11702 = call i64 @prim_cons(i64 %arg10447, i64 %rva11703)                      ; call prim_cons
  %cloptr15393 = inttoptr i64 %cont9536 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15394 = getelementptr inbounds i64, i64* %cloptr15393, i64 0                 ; &cloptr15393[0]
  %f15396 = load i64, i64* %i0ptr15394, align 8                                      ; load; *i0ptr15394
  %fptr15395 = inttoptr i64 %f15396 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15395(i64 %cont9536, i64 %rva11702)                 ; tail call
  ret void
}


define void @lam12755(i64 %env12756, i64 %rvp11732) {
  %envptr15397 = inttoptr i64 %env12756 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15398 = getelementptr inbounds i64, i64* %envptr15397, i64 6                ; &envptr15397[6]
  %cont9525 = load i64, i64* %envptr15398, align 8                                   ; load; *envptr15398
  %envptr15399 = inttoptr i64 %env12756 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15400 = getelementptr inbounds i64, i64* %envptr15399, i64 5                ; &envptr15399[5]
  %G80$tmp9199 = load i64, i64* %envptr15400, align 8                                ; load; *envptr15400
  %envptr15401 = inttoptr i64 %env12756 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15402 = getelementptr inbounds i64, i64* %envptr15401, i64 4                ; &envptr15401[4]
  %ZER$ccstack = load i64, i64* %envptr15402, align 8                                ; load; *envptr15402
  %envptr15403 = inttoptr i64 %env12756 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15404 = getelementptr inbounds i64, i64* %envptr15403, i64 3                ; &envptr15403[3]
  %LeX$cc = load i64, i64* %envptr15404, align 8                                     ; load; *envptr15404
  %envptr15405 = inttoptr i64 %env12756 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15406 = getelementptr inbounds i64, i64* %envptr15405, i64 2                ; &envptr15405[2]
  %RnY$lst = load i64, i64* %envptr15406, align 8                                    ; load; *envptr15406
  %envptr15407 = inttoptr i64 %env12756 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15408 = getelementptr inbounds i64, i64* %envptr15407, i64 1                ; &envptr15407[1]
  %mnc$head = load i64, i64* %envptr15408, align 8                                   ; load; *envptr15408
  %_959534 = call i64 @prim_car(i64 %rvp11732)                                       ; call prim_car
  %rvp11731 = call i64 @prim_cdr(i64 %rvp11732)                                      ; call prim_cdr
  %a9366 = call i64 @prim_car(i64 %rvp11731)                                         ; call prim_car
  %na11706 = call i64 @prim_cdr(i64 %rvp11731)                                       ; call prim_cdr
  %arg10450 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9535 = call i64 @prim_make_45vector(i64 %arg10450, i64 %a9366)             ; call prim_make_45vector
  %cloptr15409 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr15411 = getelementptr inbounds i64, i64* %cloptr15409, i64 1                  ; &eptr15411[1]
  %eptr15412 = getelementptr inbounds i64, i64* %cloptr15409, i64 2                  ; &eptr15412[2]
  %eptr15413 = getelementptr inbounds i64, i64* %cloptr15409, i64 3                  ; &eptr15413[3]
  %eptr15414 = getelementptr inbounds i64, i64* %cloptr15409, i64 4                  ; &eptr15414[4]
  %eptr15415 = getelementptr inbounds i64, i64* %cloptr15409, i64 5                  ; &eptr15415[5]
  %eptr15416 = getelementptr inbounds i64, i64* %cloptr15409, i64 6                  ; &eptr15416[6]
  store i64 %mnc$head, i64* %eptr15411                                               ; *eptr15411 = %mnc$head
  store i64 %RnY$lst, i64* %eptr15412                                                ; *eptr15412 = %RnY$lst
  store i64 %LeX$cc, i64* %eptr15413                                                 ; *eptr15413 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15414                                            ; *eptr15414 = %ZER$ccstack
  store i64 %G80$tmp9199, i64* %eptr15415                                            ; *eptr15415 = %G80$tmp9199
  store i64 %cont9525, i64* %eptr15416                                               ; *eptr15416 = %cont9525
  %eptr15410 = getelementptr inbounds i64, i64* %cloptr15409, i64 0                  ; &cloptr15409[0]
  %f15417 = ptrtoint void(i64,i64)* @lam12752 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15417, i64* %eptr15410                                                 ; store fptr
  %arg10453 = ptrtoint i64* %cloptr15409 to i64                                      ; closure cast; i64* -> i64
  %arg10452 = add i64 0, 0                                                           ; quoted ()
  %rva11730 = add i64 0, 0                                                           ; quoted ()
  %rva11729 = call i64 @prim_cons(i64 %retprim9535, i64 %rva11730)                   ; call prim_cons
  %rva11728 = call i64 @prim_cons(i64 %arg10452, i64 %rva11729)                      ; call prim_cons
  %cloptr15418 = inttoptr i64 %arg10453 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15419 = getelementptr inbounds i64, i64* %cloptr15418, i64 0                 ; &cloptr15418[0]
  %f15421 = load i64, i64* %i0ptr15419, align 8                                      ; load; *i0ptr15419
  %fptr15420 = inttoptr i64 %f15421 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15420(i64 %arg10453, i64 %rva11728)                 ; tail call
  ret void
}


define void @lam12752(i64 %env12753, i64 %rvp11727) {
  %envptr15422 = inttoptr i64 %env12753 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15423 = getelementptr inbounds i64, i64* %envptr15422, i64 6                ; &envptr15422[6]
  %cont9525 = load i64, i64* %envptr15423, align 8                                   ; load; *envptr15423
  %envptr15424 = inttoptr i64 %env12753 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15425 = getelementptr inbounds i64, i64* %envptr15424, i64 5                ; &envptr15424[5]
  %G80$tmp9199 = load i64, i64* %envptr15425, align 8                                ; load; *envptr15425
  %envptr15426 = inttoptr i64 %env12753 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15427 = getelementptr inbounds i64, i64* %envptr15426, i64 4                ; &envptr15426[4]
  %ZER$ccstack = load i64, i64* %envptr15427, align 8                                ; load; *envptr15427
  %envptr15428 = inttoptr i64 %env12753 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15429 = getelementptr inbounds i64, i64* %envptr15428, i64 3                ; &envptr15428[3]
  %LeX$cc = load i64, i64* %envptr15429, align 8                                     ; load; *envptr15429
  %envptr15430 = inttoptr i64 %env12753 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15431 = getelementptr inbounds i64, i64* %envptr15430, i64 2                ; &envptr15430[2]
  %RnY$lst = load i64, i64* %envptr15431, align 8                                    ; load; *envptr15431
  %envptr15432 = inttoptr i64 %env12753 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15433 = getelementptr inbounds i64, i64* %envptr15432, i64 1                ; &envptr15432[1]
  %mnc$head = load i64, i64* %envptr15433, align 8                                   ; load; *envptr15433
  %_959529 = call i64 @prim_car(i64 %rvp11727)                                       ; call prim_car
  %rvp11726 = call i64 @prim_cdr(i64 %rvp11727)                                      ; call prim_cdr
  %vBx$tmp9198 = call i64 @prim_car(i64 %rvp11726)                                   ; call prim_car
  %na11708 = call i64 @prim_cdr(i64 %rvp11726)                                       ; call prim_cdr
  %arg10454 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9367 = call i64 @prim_vector_45ref(i64 %RnY$lst, i64 %arg10454)                  ; call prim_vector_45ref
  %a9368 = call i64 @prim_cdr(i64 %a9367)                                            ; call prim_cdr
  %arg10458 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9369 = call i64 @prim_vector_45set_33(i64 %RnY$lst, i64 %arg10458, i64 %a9368)   ; call prim_vector_45set_33
  %arg10461 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9533 = call i64 @prim_vector_45set_33(i64 %G80$tmp9199, i64 %arg10461, i64 %a9369); call prim_vector_45set_33
  %cloptr15434 = call i64* @alloc(i64 48)                                            ; malloc
  %eptr15436 = getelementptr inbounds i64, i64* %cloptr15434, i64 1                  ; &eptr15436[1]
  %eptr15437 = getelementptr inbounds i64, i64* %cloptr15434, i64 2                  ; &eptr15437[2]
  %eptr15438 = getelementptr inbounds i64, i64* %cloptr15434, i64 3                  ; &eptr15438[3]
  %eptr15439 = getelementptr inbounds i64, i64* %cloptr15434, i64 4                  ; &eptr15439[4]
  %eptr15440 = getelementptr inbounds i64, i64* %cloptr15434, i64 5                  ; &eptr15440[5]
  store i64 %mnc$head, i64* %eptr15436                                               ; *eptr15436 = %mnc$head
  store i64 %LeX$cc, i64* %eptr15437                                                 ; *eptr15437 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15438                                            ; *eptr15438 = %ZER$ccstack
  store i64 %cont9525, i64* %eptr15439                                               ; *eptr15439 = %cont9525
  store i64 %vBx$tmp9198, i64* %eptr15440                                            ; *eptr15440 = %vBx$tmp9198
  %eptr15435 = getelementptr inbounds i64, i64* %cloptr15434, i64 0                  ; &cloptr15434[0]
  %f15441 = ptrtoint void(i64,i64)* @lam12747 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15441, i64* %eptr15435                                                 ; store fptr
  %arg10465 = ptrtoint i64* %cloptr15434 to i64                                      ; closure cast; i64* -> i64
  %arg10464 = add i64 0, 0                                                           ; quoted ()
  %rva11725 = add i64 0, 0                                                           ; quoted ()
  %rva11724 = call i64 @prim_cons(i64 %retprim9533, i64 %rva11725)                   ; call prim_cons
  %rva11723 = call i64 @prim_cons(i64 %arg10464, i64 %rva11724)                      ; call prim_cons
  %cloptr15442 = inttoptr i64 %arg10465 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15443 = getelementptr inbounds i64, i64* %cloptr15442, i64 0                 ; &cloptr15442[0]
  %f15445 = load i64, i64* %i0ptr15443, align 8                                      ; load; *i0ptr15443
  %fptr15444 = inttoptr i64 %f15445 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15444(i64 %arg10465, i64 %rva11723)                 ; tail call
  ret void
}


define void @lam12747(i64 %env12748, i64 %rvp11722) {
  %envptr15446 = inttoptr i64 %env12748 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15447 = getelementptr inbounds i64, i64* %envptr15446, i64 5                ; &envptr15446[5]
  %vBx$tmp9198 = load i64, i64* %envptr15447, align 8                                ; load; *envptr15447
  %envptr15448 = inttoptr i64 %env12748 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15449 = getelementptr inbounds i64, i64* %envptr15448, i64 4                ; &envptr15448[4]
  %cont9525 = load i64, i64* %envptr15449, align 8                                   ; load; *envptr15449
  %envptr15450 = inttoptr i64 %env12748 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15451 = getelementptr inbounds i64, i64* %envptr15450, i64 3                ; &envptr15450[3]
  %ZER$ccstack = load i64, i64* %envptr15451, align 8                                ; load; *envptr15451
  %envptr15452 = inttoptr i64 %env12748 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15453 = getelementptr inbounds i64, i64* %envptr15452, i64 2                ; &envptr15452[2]
  %LeX$cc = load i64, i64* %envptr15453, align 8                                     ; load; *envptr15453
  %envptr15454 = inttoptr i64 %env12748 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15455 = getelementptr inbounds i64, i64* %envptr15454, i64 1                ; &envptr15454[1]
  %mnc$head = load i64, i64* %envptr15455, align 8                                   ; load; *envptr15455
  %_959530 = call i64 @prim_car(i64 %rvp11722)                                       ; call prim_car
  %rvp11721 = call i64 @prim_cdr(i64 %rvp11722)                                      ; call prim_cdr
  %ID1$_959217 = call i64 @prim_car(i64 %rvp11721)                                   ; call prim_car
  %na11710 = call i64 @prim_cdr(i64 %rvp11721)                                       ; call prim_cdr
  %arg10466 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9370 = call i64 @prim_vector_45ref(i64 %ZER$ccstack, i64 %arg10466)              ; call prim_vector_45ref
  %a9371 = call i64 @prim_cons(i64 %LeX$cc, i64 %a9370)                              ; call prim_cons
  %arg10471 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9372 = call i64 @prim_vector_45set_33(i64 %ZER$ccstack, i64 %arg10471, i64 %a9371); call prim_vector_45set_33
  %arg10474 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9532 = call i64 @prim_vector_45set_33(i64 %vBx$tmp9198, i64 %arg10474, i64 %a9372); call prim_vector_45set_33
  %cloptr15456 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr15458 = getelementptr inbounds i64, i64* %cloptr15456, i64 1                  ; &eptr15458[1]
  %eptr15459 = getelementptr inbounds i64, i64* %cloptr15456, i64 2                  ; &eptr15459[2]
  store i64 %mnc$head, i64* %eptr15458                                               ; *eptr15458 = %mnc$head
  store i64 %cont9525, i64* %eptr15459                                               ; *eptr15459 = %cont9525
  %eptr15457 = getelementptr inbounds i64, i64* %cloptr15456, i64 0                  ; &cloptr15456[0]
  %f15460 = ptrtoint void(i64,i64)* @lam12742 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15460, i64* %eptr15457                                                 ; store fptr
  %arg10478 = ptrtoint i64* %cloptr15456 to i64                                      ; closure cast; i64* -> i64
  %arg10477 = add i64 0, 0                                                           ; quoted ()
  %rva11720 = add i64 0, 0                                                           ; quoted ()
  %rva11719 = call i64 @prim_cons(i64 %retprim9532, i64 %rva11720)                   ; call prim_cons
  %rva11718 = call i64 @prim_cons(i64 %arg10477, i64 %rva11719)                      ; call prim_cons
  %cloptr15461 = inttoptr i64 %arg10478 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15462 = getelementptr inbounds i64, i64* %cloptr15461, i64 0                 ; &cloptr15461[0]
  %f15464 = load i64, i64* %i0ptr15462, align 8                                      ; load; *i0ptr15462
  %fptr15463 = inttoptr i64 %f15464 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15463(i64 %arg10478, i64 %rva11718)                 ; tail call
  ret void
}


define void @lam12742(i64 %env12743, i64 %rvp11717) {
  %envptr15465 = inttoptr i64 %env12743 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15466 = getelementptr inbounds i64, i64* %envptr15465, i64 2                ; &envptr15465[2]
  %cont9525 = load i64, i64* %envptr15466, align 8                                   ; load; *envptr15466
  %envptr15467 = inttoptr i64 %env12743 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15468 = getelementptr inbounds i64, i64* %envptr15467, i64 1                ; &envptr15467[1]
  %mnc$head = load i64, i64* %envptr15468, align 8                                   ; load; *envptr15468
  %_959531 = call i64 @prim_car(i64 %rvp11717)                                       ; call prim_car
  %rvp11716 = call i64 @prim_cdr(i64 %rvp11717)                                      ; call prim_cdr
  %pA4$_959218 = call i64 @prim_car(i64 %rvp11716)                                   ; call prim_car
  %na11712 = call i64 @prim_cdr(i64 %rvp11716)                                       ; call prim_cdr
  %arg10480 = add i64 0, 0                                                           ; quoted ()
  %rva11715 = add i64 0, 0                                                           ; quoted ()
  %rva11714 = call i64 @prim_cons(i64 %mnc$head, i64 %rva11715)                      ; call prim_cons
  %rva11713 = call i64 @prim_cons(i64 %arg10480, i64 %rva11714)                      ; call prim_cons
  %cloptr15469 = inttoptr i64 %cont9525 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15470 = getelementptr inbounds i64, i64* %cloptr15469, i64 0                 ; &cloptr15469[0]
  %f15472 = load i64, i64* %i0ptr15470, align 8                                      ; load; *i0ptr15470
  %fptr15471 = inttoptr i64 %f15472 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15471(i64 %cont9525, i64 %rva11713)                 ; tail call
  ret void
}


define void @lam12724(i64 %env12725, i64 %rvp11812) {
  %envptr15473 = inttoptr i64 %env12725 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15474 = getelementptr inbounds i64, i64* %envptr15473, i64 4                ; &envptr15473[4]
  %OB6$fail = load i64, i64* %envptr15474, align 8                                   ; load; *envptr15474
  %envptr15475 = inttoptr i64 %env12725 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15476 = getelementptr inbounds i64, i64* %envptr15475, i64 3                ; &envptr15475[3]
  %cont9525 = load i64, i64* %envptr15476, align 8                                   ; load; *envptr15476
  %envptr15477 = inttoptr i64 %env12725 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15478 = getelementptr inbounds i64, i64* %envptr15477, i64 2                ; &envptr15477[2]
  %ZER$ccstack = load i64, i64* %envptr15478, align 8                                ; load; *envptr15478
  %envptr15479 = inttoptr i64 %env12725 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15480 = getelementptr inbounds i64, i64* %envptr15479, i64 1                ; &envptr15479[1]
  %RnY$lst = load i64, i64* %envptr15480, align 8                                    ; load; *envptr15480
  %_959526 = call i64 @prim_car(i64 %rvp11812)                                       ; call prim_car
  %rvp11811 = call i64 @prim_cdr(i64 %rvp11812)                                      ; call prim_cdr
  %LeX$cc = call i64 @prim_car(i64 %rvp11811)                                        ; call prim_car
  %na11752 = call i64 @prim_cdr(i64 %rvp11811)                                       ; call prim_cdr
  %arg10482 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9361 = call i64 @prim_vector_45ref(i64 %RnY$lst, i64 %arg10482)                  ; call prim_vector_45ref
  %a9362 = call i64 @prim_null_63(i64 %a9361)                                        ; call prim_null_63
  %cmp15481 = icmp eq i64 %a9362, 15                                                 ; false?
  br i1 %cmp15481, label %else15483, label %then15482                                ; if

then15482:
  %arg10485 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9363 = call i64 @prim_vector_45ref(i64 %OB6$fail, i64 %arg10485)                 ; call prim_vector_45ref
  %rva11754 = add i64 0, 0                                                           ; quoted ()
  %rva11753 = call i64 @prim_cons(i64 %cont9525, i64 %rva11754)                      ; call prim_cons
  %cloptr15484 = inttoptr i64 %a9363 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr15485 = getelementptr inbounds i64, i64* %cloptr15484, i64 0                 ; &cloptr15484[0]
  %f15487 = load i64, i64* %i0ptr15485, align 8                                      ; load; *i0ptr15485
  %fptr15486 = inttoptr i64 %f15487 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15486(i64 %a9363, i64 %rva11753)                    ; tail call
  ret void

else15483:
  %arg10489 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9364 = call i64 @prim_vector_45ref(i64 %RnY$lst, i64 %arg10489)                  ; call prim_vector_45ref
  %retprim9542 = call i64 @prim_car(i64 %a9364)                                      ; call prim_car
  %cloptr15488 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15490 = getelementptr inbounds i64, i64* %cloptr15488, i64 1                  ; &eptr15490[1]
  %eptr15491 = getelementptr inbounds i64, i64* %cloptr15488, i64 2                  ; &eptr15491[2]
  %eptr15492 = getelementptr inbounds i64, i64* %cloptr15488, i64 3                  ; &eptr15492[3]
  %eptr15493 = getelementptr inbounds i64, i64* %cloptr15488, i64 4                  ; &eptr15493[4]
  store i64 %RnY$lst, i64* %eptr15490                                                ; *eptr15490 = %RnY$lst
  store i64 %LeX$cc, i64* %eptr15491                                                 ; *eptr15491 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15492                                            ; *eptr15492 = %ZER$ccstack
  store i64 %cont9525, i64* %eptr15493                                               ; *eptr15493 = %cont9525
  %eptr15489 = getelementptr inbounds i64, i64* %cloptr15488, i64 0                  ; &cloptr15488[0]
  %f15494 = ptrtoint void(i64,i64)* @lam12720 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15494, i64* %eptr15489                                                 ; store fptr
  %arg10494 = ptrtoint i64* %cloptr15488 to i64                                      ; closure cast; i64* -> i64
  %arg10493 = add i64 0, 0                                                           ; quoted ()
  %rva11810 = add i64 0, 0                                                           ; quoted ()
  %rva11809 = call i64 @prim_cons(i64 %retprim9542, i64 %rva11810)                   ; call prim_cons
  %rva11808 = call i64 @prim_cons(i64 %arg10493, i64 %rva11809)                      ; call prim_cons
  %cloptr15495 = inttoptr i64 %arg10494 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15496 = getelementptr inbounds i64, i64* %cloptr15495, i64 0                 ; &cloptr15495[0]
  %f15498 = load i64, i64* %i0ptr15496, align 8                                      ; load; *i0ptr15496
  %fptr15497 = inttoptr i64 %f15498 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15497(i64 %arg10494, i64 %rva11808)                 ; tail call
  ret void
}


define void @lam12720(i64 %env12721, i64 %rvp11807) {
  %envptr15499 = inttoptr i64 %env12721 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15500 = getelementptr inbounds i64, i64* %envptr15499, i64 4                ; &envptr15499[4]
  %cont9525 = load i64, i64* %envptr15500, align 8                                   ; load; *envptr15500
  %envptr15501 = inttoptr i64 %env12721 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15502 = getelementptr inbounds i64, i64* %envptr15501, i64 3                ; &envptr15501[3]
  %ZER$ccstack = load i64, i64* %envptr15502, align 8                                ; load; *envptr15502
  %envptr15503 = inttoptr i64 %env12721 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15504 = getelementptr inbounds i64, i64* %envptr15503, i64 2                ; &envptr15503[2]
  %LeX$cc = load i64, i64* %envptr15504, align 8                                     ; load; *envptr15504
  %envptr15505 = inttoptr i64 %env12721 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15506 = getelementptr inbounds i64, i64* %envptr15505, i64 1                ; &envptr15505[1]
  %RnY$lst = load i64, i64* %envptr15506, align 8                                    ; load; *envptr15506
  %_959527 = call i64 @prim_car(i64 %rvp11807)                                       ; call prim_car
  %rvp11806 = call i64 @prim_cdr(i64 %rvp11807)                                      ; call prim_cdr
  %mnc$head = call i64 @prim_car(i64 %rvp11806)                                      ; call prim_car
  %na11756 = call i64 @prim_cdr(i64 %rvp11806)                                       ; call prim_cdr
  %cloptr15507 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15508 = getelementptr inbounds i64, i64* %cloptr15507, i64 0                  ; &cloptr15507[0]
  %f15509 = ptrtoint void(i64,i64)* @lam12718 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15509, i64* %eptr15508                                                 ; store fptr
  %arg10496 = ptrtoint i64* %cloptr15507 to i64                                      ; closure cast; i64* -> i64
  %cloptr15510 = call i64* @alloc(i64 48)                                            ; malloc
  %eptr15512 = getelementptr inbounds i64, i64* %cloptr15510, i64 1                  ; &eptr15512[1]
  %eptr15513 = getelementptr inbounds i64, i64* %cloptr15510, i64 2                  ; &eptr15513[2]
  %eptr15514 = getelementptr inbounds i64, i64* %cloptr15510, i64 3                  ; &eptr15514[3]
  %eptr15515 = getelementptr inbounds i64, i64* %cloptr15510, i64 4                  ; &eptr15515[4]
  %eptr15516 = getelementptr inbounds i64, i64* %cloptr15510, i64 5                  ; &eptr15516[5]
  store i64 %mnc$head, i64* %eptr15512                                               ; *eptr15512 = %mnc$head
  store i64 %RnY$lst, i64* %eptr15513                                                ; *eptr15513 = %RnY$lst
  store i64 %LeX$cc, i64* %eptr15514                                                 ; *eptr15514 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15515                                            ; *eptr15515 = %ZER$ccstack
  store i64 %cont9525, i64* %eptr15516                                               ; *eptr15516 = %cont9525
  %eptr15511 = getelementptr inbounds i64, i64* %cloptr15510, i64 0                  ; &cloptr15510[0]
  %f15517 = ptrtoint void(i64,i64)* @lam12714 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15517, i64* %eptr15511                                                 ; store fptr
  %arg10495 = ptrtoint i64* %cloptr15510 to i64                                      ; closure cast; i64* -> i64
  %rva11805 = add i64 0, 0                                                           ; quoted ()
  %rva11804 = call i64 @prim_cons(i64 %arg10495, i64 %rva11805)                      ; call prim_cons
  %cloptr15518 = inttoptr i64 %arg10496 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15519 = getelementptr inbounds i64, i64* %cloptr15518, i64 0                 ; &cloptr15518[0]
  %f15521 = load i64, i64* %i0ptr15519, align 8                                      ; load; *i0ptr15519
  %fptr15520 = inttoptr i64 %f15521 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15520(i64 %arg10496, i64 %rva11804)                 ; tail call
  ret void
}


define void @lam12718(i64 %env12719, i64 %rnt$lst9541) {
  %cont9540 = call i64 @prim_car(i64 %rnt$lst9541)                                   ; call prim_car
  %rnt$lst = call i64 @prim_cdr(i64 %rnt$lst9541)                                    ; call prim_cdr
  %arg10500 = add i64 0, 0                                                           ; quoted ()
  %rva11759 = add i64 0, 0                                                           ; quoted ()
  %rva11758 = call i64 @prim_cons(i64 %rnt$lst, i64 %rva11759)                       ; call prim_cons
  %rva11757 = call i64 @prim_cons(i64 %arg10500, i64 %rva11758)                      ; call prim_cons
  %cloptr15522 = inttoptr i64 %cont9540 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15523 = getelementptr inbounds i64, i64* %cloptr15522, i64 0                 ; &cloptr15522[0]
  %f15525 = load i64, i64* %i0ptr15523, align 8                                      ; load; *i0ptr15523
  %fptr15524 = inttoptr i64 %f15525 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15524(i64 %cont9540, i64 %rva11757)                 ; tail call
  ret void
}


define void @lam12714(i64 %env12715, i64 %rvp11803) {
  %envptr15526 = inttoptr i64 %env12715 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15527 = getelementptr inbounds i64, i64* %envptr15526, i64 5                ; &envptr15526[5]
  %cont9525 = load i64, i64* %envptr15527, align 8                                   ; load; *envptr15527
  %envptr15528 = inttoptr i64 %env12715 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15529 = getelementptr inbounds i64, i64* %envptr15528, i64 4                ; &envptr15528[4]
  %ZER$ccstack = load i64, i64* %envptr15529, align 8                                ; load; *envptr15529
  %envptr15530 = inttoptr i64 %env12715 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15531 = getelementptr inbounds i64, i64* %envptr15530, i64 3                ; &envptr15530[3]
  %LeX$cc = load i64, i64* %envptr15531, align 8                                     ; load; *envptr15531
  %envptr15532 = inttoptr i64 %env12715 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15533 = getelementptr inbounds i64, i64* %envptr15532, i64 2                ; &envptr15532[2]
  %RnY$lst = load i64, i64* %envptr15533, align 8                                    ; load; *envptr15533
  %envptr15534 = inttoptr i64 %env12715 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15535 = getelementptr inbounds i64, i64* %envptr15534, i64 1                ; &envptr15534[1]
  %mnc$head = load i64, i64* %envptr15535, align 8                                   ; load; *envptr15535
  %_959538 = call i64 @prim_car(i64 %rvp11803)                                       ; call prim_car
  %rvp11802 = call i64 @prim_cdr(i64 %rvp11803)                                      ; call prim_cdr
  %a9365 = call i64 @prim_car(i64 %rvp11802)                                         ; call prim_car
  %na11761 = call i64 @prim_cdr(i64 %rvp11802)                                       ; call prim_cdr
  %arg10503 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9539 = call i64 @prim_make_45vector(i64 %arg10503, i64 %a9365)             ; call prim_make_45vector
  %cloptr15536 = call i64* @alloc(i64 48)                                            ; malloc
  %eptr15538 = getelementptr inbounds i64, i64* %cloptr15536, i64 1                  ; &eptr15538[1]
  %eptr15539 = getelementptr inbounds i64, i64* %cloptr15536, i64 2                  ; &eptr15539[2]
  %eptr15540 = getelementptr inbounds i64, i64* %cloptr15536, i64 3                  ; &eptr15540[3]
  %eptr15541 = getelementptr inbounds i64, i64* %cloptr15536, i64 4                  ; &eptr15541[4]
  %eptr15542 = getelementptr inbounds i64, i64* %cloptr15536, i64 5                  ; &eptr15542[5]
  store i64 %mnc$head, i64* %eptr15538                                               ; *eptr15538 = %mnc$head
  store i64 %RnY$lst, i64* %eptr15539                                                ; *eptr15539 = %RnY$lst
  store i64 %LeX$cc, i64* %eptr15540                                                 ; *eptr15540 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15541                                            ; *eptr15541 = %ZER$ccstack
  store i64 %cont9525, i64* %eptr15542                                               ; *eptr15542 = %cont9525
  %eptr15537 = getelementptr inbounds i64, i64* %cloptr15536, i64 0                  ; &cloptr15536[0]
  %f15543 = ptrtoint void(i64,i64)* @lam12711 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15543, i64* %eptr15537                                                 ; store fptr
  %arg10506 = ptrtoint i64* %cloptr15536 to i64                                      ; closure cast; i64* -> i64
  %arg10505 = add i64 0, 0                                                           ; quoted ()
  %rva11801 = add i64 0, 0                                                           ; quoted ()
  %rva11800 = call i64 @prim_cons(i64 %retprim9539, i64 %rva11801)                   ; call prim_cons
  %rva11799 = call i64 @prim_cons(i64 %arg10505, i64 %rva11800)                      ; call prim_cons
  %cloptr15544 = inttoptr i64 %arg10506 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15545 = getelementptr inbounds i64, i64* %cloptr15544, i64 0                 ; &cloptr15544[0]
  %f15547 = load i64, i64* %i0ptr15545, align 8                                      ; load; *i0ptr15545
  %fptr15546 = inttoptr i64 %f15547 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15546(i64 %arg10506, i64 %rva11799)                 ; tail call
  ret void
}


define void @lam12711(i64 %env12712, i64 %rvp11798) {
  %envptr15548 = inttoptr i64 %env12712 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15549 = getelementptr inbounds i64, i64* %envptr15548, i64 5                ; &envptr15548[5]
  %cont9525 = load i64, i64* %envptr15549, align 8                                   ; load; *envptr15549
  %envptr15550 = inttoptr i64 %env12712 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15551 = getelementptr inbounds i64, i64* %envptr15550, i64 4                ; &envptr15550[4]
  %ZER$ccstack = load i64, i64* %envptr15551, align 8                                ; load; *envptr15551
  %envptr15552 = inttoptr i64 %env12712 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15553 = getelementptr inbounds i64, i64* %envptr15552, i64 3                ; &envptr15552[3]
  %LeX$cc = load i64, i64* %envptr15553, align 8                                     ; load; *envptr15553
  %envptr15554 = inttoptr i64 %env12712 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15555 = getelementptr inbounds i64, i64* %envptr15554, i64 2                ; &envptr15554[2]
  %RnY$lst = load i64, i64* %envptr15555, align 8                                    ; load; *envptr15555
  %envptr15556 = inttoptr i64 %env12712 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15557 = getelementptr inbounds i64, i64* %envptr15556, i64 1                ; &envptr15556[1]
  %mnc$head = load i64, i64* %envptr15557, align 8                                   ; load; *envptr15557
  %_959528 = call i64 @prim_car(i64 %rvp11798)                                       ; call prim_car
  %rvp11797 = call i64 @prim_cdr(i64 %rvp11798)                                      ; call prim_cdr
  %G80$tmp9199 = call i64 @prim_car(i64 %rvp11797)                                   ; call prim_car
  %na11763 = call i64 @prim_cdr(i64 %rvp11797)                                       ; call prim_cdr
  %cloptr15558 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15559 = getelementptr inbounds i64, i64* %cloptr15558, i64 0                  ; &cloptr15558[0]
  %f15560 = ptrtoint void(i64,i64)* @lam12709 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15560, i64* %eptr15559                                                 ; store fptr
  %arg10508 = ptrtoint i64* %cloptr15558 to i64                                      ; closure cast; i64* -> i64
  %cloptr15561 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr15563 = getelementptr inbounds i64, i64* %cloptr15561, i64 1                  ; &eptr15563[1]
  %eptr15564 = getelementptr inbounds i64, i64* %cloptr15561, i64 2                  ; &eptr15564[2]
  %eptr15565 = getelementptr inbounds i64, i64* %cloptr15561, i64 3                  ; &eptr15565[3]
  %eptr15566 = getelementptr inbounds i64, i64* %cloptr15561, i64 4                  ; &eptr15566[4]
  %eptr15567 = getelementptr inbounds i64, i64* %cloptr15561, i64 5                  ; &eptr15567[5]
  %eptr15568 = getelementptr inbounds i64, i64* %cloptr15561, i64 6                  ; &eptr15568[6]
  store i64 %mnc$head, i64* %eptr15563                                               ; *eptr15563 = %mnc$head
  store i64 %RnY$lst, i64* %eptr15564                                                ; *eptr15564 = %RnY$lst
  store i64 %LeX$cc, i64* %eptr15565                                                 ; *eptr15565 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15566                                            ; *eptr15566 = %ZER$ccstack
  store i64 %G80$tmp9199, i64* %eptr15567                                            ; *eptr15567 = %G80$tmp9199
  store i64 %cont9525, i64* %eptr15568                                               ; *eptr15568 = %cont9525
  %eptr15562 = getelementptr inbounds i64, i64* %cloptr15561, i64 0                  ; &cloptr15561[0]
  %f15569 = ptrtoint void(i64,i64)* @lam12705 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15569, i64* %eptr15562                                                 ; store fptr
  %arg10507 = ptrtoint i64* %cloptr15561 to i64                                      ; closure cast; i64* -> i64
  %rva11796 = add i64 0, 0                                                           ; quoted ()
  %rva11795 = call i64 @prim_cons(i64 %arg10507, i64 %rva11796)                      ; call prim_cons
  %cloptr15570 = inttoptr i64 %arg10508 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15571 = getelementptr inbounds i64, i64* %cloptr15570, i64 0                 ; &cloptr15570[0]
  %f15573 = load i64, i64* %i0ptr15571, align 8                                      ; load; *i0ptr15571
  %fptr15572 = inttoptr i64 %f15573 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15572(i64 %arg10508, i64 %rva11795)                 ; tail call
  ret void
}


define void @lam12709(i64 %env12710, i64 %rix$lst9537) {
  %cont9536 = call i64 @prim_car(i64 %rix$lst9537)                                   ; call prim_car
  %rix$lst = call i64 @prim_cdr(i64 %rix$lst9537)                                    ; call prim_cdr
  %arg10512 = add i64 0, 0                                                           ; quoted ()
  %rva11766 = add i64 0, 0                                                           ; quoted ()
  %rva11765 = call i64 @prim_cons(i64 %rix$lst, i64 %rva11766)                       ; call prim_cons
  %rva11764 = call i64 @prim_cons(i64 %arg10512, i64 %rva11765)                      ; call prim_cons
  %cloptr15574 = inttoptr i64 %cont9536 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15575 = getelementptr inbounds i64, i64* %cloptr15574, i64 0                 ; &cloptr15574[0]
  %f15577 = load i64, i64* %i0ptr15575, align 8                                      ; load; *i0ptr15575
  %fptr15576 = inttoptr i64 %f15577 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15576(i64 %cont9536, i64 %rva11764)                 ; tail call
  ret void
}


define void @lam12705(i64 %env12706, i64 %rvp11794) {
  %envptr15578 = inttoptr i64 %env12706 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15579 = getelementptr inbounds i64, i64* %envptr15578, i64 6                ; &envptr15578[6]
  %cont9525 = load i64, i64* %envptr15579, align 8                                   ; load; *envptr15579
  %envptr15580 = inttoptr i64 %env12706 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15581 = getelementptr inbounds i64, i64* %envptr15580, i64 5                ; &envptr15580[5]
  %G80$tmp9199 = load i64, i64* %envptr15581, align 8                                ; load; *envptr15581
  %envptr15582 = inttoptr i64 %env12706 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15583 = getelementptr inbounds i64, i64* %envptr15582, i64 4                ; &envptr15582[4]
  %ZER$ccstack = load i64, i64* %envptr15583, align 8                                ; load; *envptr15583
  %envptr15584 = inttoptr i64 %env12706 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15585 = getelementptr inbounds i64, i64* %envptr15584, i64 3                ; &envptr15584[3]
  %LeX$cc = load i64, i64* %envptr15585, align 8                                     ; load; *envptr15585
  %envptr15586 = inttoptr i64 %env12706 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15587 = getelementptr inbounds i64, i64* %envptr15586, i64 2                ; &envptr15586[2]
  %RnY$lst = load i64, i64* %envptr15587, align 8                                    ; load; *envptr15587
  %envptr15588 = inttoptr i64 %env12706 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15589 = getelementptr inbounds i64, i64* %envptr15588, i64 1                ; &envptr15588[1]
  %mnc$head = load i64, i64* %envptr15589, align 8                                   ; load; *envptr15589
  %_959534 = call i64 @prim_car(i64 %rvp11794)                                       ; call prim_car
  %rvp11793 = call i64 @prim_cdr(i64 %rvp11794)                                      ; call prim_cdr
  %a9366 = call i64 @prim_car(i64 %rvp11793)                                         ; call prim_car
  %na11768 = call i64 @prim_cdr(i64 %rvp11793)                                       ; call prim_cdr
  %arg10515 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9535 = call i64 @prim_make_45vector(i64 %arg10515, i64 %a9366)             ; call prim_make_45vector
  %cloptr15590 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr15592 = getelementptr inbounds i64, i64* %cloptr15590, i64 1                  ; &eptr15592[1]
  %eptr15593 = getelementptr inbounds i64, i64* %cloptr15590, i64 2                  ; &eptr15593[2]
  %eptr15594 = getelementptr inbounds i64, i64* %cloptr15590, i64 3                  ; &eptr15594[3]
  %eptr15595 = getelementptr inbounds i64, i64* %cloptr15590, i64 4                  ; &eptr15595[4]
  %eptr15596 = getelementptr inbounds i64, i64* %cloptr15590, i64 5                  ; &eptr15596[5]
  %eptr15597 = getelementptr inbounds i64, i64* %cloptr15590, i64 6                  ; &eptr15597[6]
  store i64 %mnc$head, i64* %eptr15592                                               ; *eptr15592 = %mnc$head
  store i64 %RnY$lst, i64* %eptr15593                                                ; *eptr15593 = %RnY$lst
  store i64 %LeX$cc, i64* %eptr15594                                                 ; *eptr15594 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15595                                            ; *eptr15595 = %ZER$ccstack
  store i64 %G80$tmp9199, i64* %eptr15596                                            ; *eptr15596 = %G80$tmp9199
  store i64 %cont9525, i64* %eptr15597                                               ; *eptr15597 = %cont9525
  %eptr15591 = getelementptr inbounds i64, i64* %cloptr15590, i64 0                  ; &cloptr15590[0]
  %f15598 = ptrtoint void(i64,i64)* @lam12702 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15598, i64* %eptr15591                                                 ; store fptr
  %arg10518 = ptrtoint i64* %cloptr15590 to i64                                      ; closure cast; i64* -> i64
  %arg10517 = add i64 0, 0                                                           ; quoted ()
  %rva11792 = add i64 0, 0                                                           ; quoted ()
  %rva11791 = call i64 @prim_cons(i64 %retprim9535, i64 %rva11792)                   ; call prim_cons
  %rva11790 = call i64 @prim_cons(i64 %arg10517, i64 %rva11791)                      ; call prim_cons
  %cloptr15599 = inttoptr i64 %arg10518 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15600 = getelementptr inbounds i64, i64* %cloptr15599, i64 0                 ; &cloptr15599[0]
  %f15602 = load i64, i64* %i0ptr15600, align 8                                      ; load; *i0ptr15600
  %fptr15601 = inttoptr i64 %f15602 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15601(i64 %arg10518, i64 %rva11790)                 ; tail call
  ret void
}


define void @lam12702(i64 %env12703, i64 %rvp11789) {
  %envptr15603 = inttoptr i64 %env12703 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15604 = getelementptr inbounds i64, i64* %envptr15603, i64 6                ; &envptr15603[6]
  %cont9525 = load i64, i64* %envptr15604, align 8                                   ; load; *envptr15604
  %envptr15605 = inttoptr i64 %env12703 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15606 = getelementptr inbounds i64, i64* %envptr15605, i64 5                ; &envptr15605[5]
  %G80$tmp9199 = load i64, i64* %envptr15606, align 8                                ; load; *envptr15606
  %envptr15607 = inttoptr i64 %env12703 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15608 = getelementptr inbounds i64, i64* %envptr15607, i64 4                ; &envptr15607[4]
  %ZER$ccstack = load i64, i64* %envptr15608, align 8                                ; load; *envptr15608
  %envptr15609 = inttoptr i64 %env12703 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15610 = getelementptr inbounds i64, i64* %envptr15609, i64 3                ; &envptr15609[3]
  %LeX$cc = load i64, i64* %envptr15610, align 8                                     ; load; *envptr15610
  %envptr15611 = inttoptr i64 %env12703 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15612 = getelementptr inbounds i64, i64* %envptr15611, i64 2                ; &envptr15611[2]
  %RnY$lst = load i64, i64* %envptr15612, align 8                                    ; load; *envptr15612
  %envptr15613 = inttoptr i64 %env12703 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15614 = getelementptr inbounds i64, i64* %envptr15613, i64 1                ; &envptr15613[1]
  %mnc$head = load i64, i64* %envptr15614, align 8                                   ; load; *envptr15614
  %_959529 = call i64 @prim_car(i64 %rvp11789)                                       ; call prim_car
  %rvp11788 = call i64 @prim_cdr(i64 %rvp11789)                                      ; call prim_cdr
  %vBx$tmp9198 = call i64 @prim_car(i64 %rvp11788)                                   ; call prim_car
  %na11770 = call i64 @prim_cdr(i64 %rvp11788)                                       ; call prim_cdr
  %arg10519 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9367 = call i64 @prim_vector_45ref(i64 %RnY$lst, i64 %arg10519)                  ; call prim_vector_45ref
  %a9368 = call i64 @prim_cdr(i64 %a9367)                                            ; call prim_cdr
  %arg10523 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9369 = call i64 @prim_vector_45set_33(i64 %RnY$lst, i64 %arg10523, i64 %a9368)   ; call prim_vector_45set_33
  %arg10526 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9533 = call i64 @prim_vector_45set_33(i64 %G80$tmp9199, i64 %arg10526, i64 %a9369); call prim_vector_45set_33
  %cloptr15615 = call i64* @alloc(i64 48)                                            ; malloc
  %eptr15617 = getelementptr inbounds i64, i64* %cloptr15615, i64 1                  ; &eptr15617[1]
  %eptr15618 = getelementptr inbounds i64, i64* %cloptr15615, i64 2                  ; &eptr15618[2]
  %eptr15619 = getelementptr inbounds i64, i64* %cloptr15615, i64 3                  ; &eptr15619[3]
  %eptr15620 = getelementptr inbounds i64, i64* %cloptr15615, i64 4                  ; &eptr15620[4]
  %eptr15621 = getelementptr inbounds i64, i64* %cloptr15615, i64 5                  ; &eptr15621[5]
  store i64 %mnc$head, i64* %eptr15617                                               ; *eptr15617 = %mnc$head
  store i64 %LeX$cc, i64* %eptr15618                                                 ; *eptr15618 = %LeX$cc
  store i64 %ZER$ccstack, i64* %eptr15619                                            ; *eptr15619 = %ZER$ccstack
  store i64 %cont9525, i64* %eptr15620                                               ; *eptr15620 = %cont9525
  store i64 %vBx$tmp9198, i64* %eptr15621                                            ; *eptr15621 = %vBx$tmp9198
  %eptr15616 = getelementptr inbounds i64, i64* %cloptr15615, i64 0                  ; &cloptr15615[0]
  %f15622 = ptrtoint void(i64,i64)* @lam12697 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15622, i64* %eptr15616                                                 ; store fptr
  %arg10530 = ptrtoint i64* %cloptr15615 to i64                                      ; closure cast; i64* -> i64
  %arg10529 = add i64 0, 0                                                           ; quoted ()
  %rva11787 = add i64 0, 0                                                           ; quoted ()
  %rva11786 = call i64 @prim_cons(i64 %retprim9533, i64 %rva11787)                   ; call prim_cons
  %rva11785 = call i64 @prim_cons(i64 %arg10529, i64 %rva11786)                      ; call prim_cons
  %cloptr15623 = inttoptr i64 %arg10530 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15624 = getelementptr inbounds i64, i64* %cloptr15623, i64 0                 ; &cloptr15623[0]
  %f15626 = load i64, i64* %i0ptr15624, align 8                                      ; load; *i0ptr15624
  %fptr15625 = inttoptr i64 %f15626 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15625(i64 %arg10530, i64 %rva11785)                 ; tail call
  ret void
}


define void @lam12697(i64 %env12698, i64 %rvp11784) {
  %envptr15627 = inttoptr i64 %env12698 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15628 = getelementptr inbounds i64, i64* %envptr15627, i64 5                ; &envptr15627[5]
  %vBx$tmp9198 = load i64, i64* %envptr15628, align 8                                ; load; *envptr15628
  %envptr15629 = inttoptr i64 %env12698 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15630 = getelementptr inbounds i64, i64* %envptr15629, i64 4                ; &envptr15629[4]
  %cont9525 = load i64, i64* %envptr15630, align 8                                   ; load; *envptr15630
  %envptr15631 = inttoptr i64 %env12698 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15632 = getelementptr inbounds i64, i64* %envptr15631, i64 3                ; &envptr15631[3]
  %ZER$ccstack = load i64, i64* %envptr15632, align 8                                ; load; *envptr15632
  %envptr15633 = inttoptr i64 %env12698 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15634 = getelementptr inbounds i64, i64* %envptr15633, i64 2                ; &envptr15633[2]
  %LeX$cc = load i64, i64* %envptr15634, align 8                                     ; load; *envptr15634
  %envptr15635 = inttoptr i64 %env12698 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15636 = getelementptr inbounds i64, i64* %envptr15635, i64 1                ; &envptr15635[1]
  %mnc$head = load i64, i64* %envptr15636, align 8                                   ; load; *envptr15636
  %_959530 = call i64 @prim_car(i64 %rvp11784)                                       ; call prim_car
  %rvp11783 = call i64 @prim_cdr(i64 %rvp11784)                                      ; call prim_cdr
  %ID1$_959217 = call i64 @prim_car(i64 %rvp11783)                                   ; call prim_car
  %na11772 = call i64 @prim_cdr(i64 %rvp11783)                                       ; call prim_cdr
  %arg10531 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9370 = call i64 @prim_vector_45ref(i64 %ZER$ccstack, i64 %arg10531)              ; call prim_vector_45ref
  %a9371 = call i64 @prim_cons(i64 %LeX$cc, i64 %a9370)                              ; call prim_cons
  %arg10536 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9372 = call i64 @prim_vector_45set_33(i64 %ZER$ccstack, i64 %arg10536, i64 %a9371); call prim_vector_45set_33
  %arg10539 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9532 = call i64 @prim_vector_45set_33(i64 %vBx$tmp9198, i64 %arg10539, i64 %a9372); call prim_vector_45set_33
  %cloptr15637 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr15639 = getelementptr inbounds i64, i64* %cloptr15637, i64 1                  ; &eptr15639[1]
  %eptr15640 = getelementptr inbounds i64, i64* %cloptr15637, i64 2                  ; &eptr15640[2]
  store i64 %mnc$head, i64* %eptr15639                                               ; *eptr15639 = %mnc$head
  store i64 %cont9525, i64* %eptr15640                                               ; *eptr15640 = %cont9525
  %eptr15638 = getelementptr inbounds i64, i64* %cloptr15637, i64 0                  ; &cloptr15637[0]
  %f15641 = ptrtoint void(i64,i64)* @lam12692 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15641, i64* %eptr15638                                                 ; store fptr
  %arg10543 = ptrtoint i64* %cloptr15637 to i64                                      ; closure cast; i64* -> i64
  %arg10542 = add i64 0, 0                                                           ; quoted ()
  %rva11782 = add i64 0, 0                                                           ; quoted ()
  %rva11781 = call i64 @prim_cons(i64 %retprim9532, i64 %rva11782)                   ; call prim_cons
  %rva11780 = call i64 @prim_cons(i64 %arg10542, i64 %rva11781)                      ; call prim_cons
  %cloptr15642 = inttoptr i64 %arg10543 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15643 = getelementptr inbounds i64, i64* %cloptr15642, i64 0                 ; &cloptr15642[0]
  %f15645 = load i64, i64* %i0ptr15643, align 8                                      ; load; *i0ptr15643
  %fptr15644 = inttoptr i64 %f15645 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15644(i64 %arg10543, i64 %rva11780)                 ; tail call
  ret void
}


define void @lam12692(i64 %env12693, i64 %rvp11779) {
  %envptr15646 = inttoptr i64 %env12693 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15647 = getelementptr inbounds i64, i64* %envptr15646, i64 2                ; &envptr15646[2]
  %cont9525 = load i64, i64* %envptr15647, align 8                                   ; load; *envptr15647
  %envptr15648 = inttoptr i64 %env12693 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15649 = getelementptr inbounds i64, i64* %envptr15648, i64 1                ; &envptr15648[1]
  %mnc$head = load i64, i64* %envptr15649, align 8                                   ; load; *envptr15649
  %_959531 = call i64 @prim_car(i64 %rvp11779)                                       ; call prim_car
  %rvp11778 = call i64 @prim_cdr(i64 %rvp11779)                                      ; call prim_cdr
  %pA4$_959218 = call i64 @prim_car(i64 %rvp11778)                                   ; call prim_car
  %na11774 = call i64 @prim_cdr(i64 %rvp11778)                                       ; call prim_cdr
  %arg10545 = add i64 0, 0                                                           ; quoted ()
  %rva11777 = add i64 0, 0                                                           ; quoted ()
  %rva11776 = call i64 @prim_cons(i64 %mnc$head, i64 %rva11777)                      ; call prim_cons
  %rva11775 = call i64 @prim_cons(i64 %arg10545, i64 %rva11776)                      ; call prim_cons
  %cloptr15650 = inttoptr i64 %cont9525 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15651 = getelementptr inbounds i64, i64* %cloptr15650, i64 0                 ; &cloptr15650[0]
  %f15653 = load i64, i64* %i0ptr15651, align 8                                      ; load; *i0ptr15651
  %fptr15652 = inttoptr i64 %f15653 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15652(i64 %cont9525, i64 %rva11775)                 ; tail call
  ret void
}


define void @lam12668(i64 %env12669, i64 %iKh$lst9572) {
  %cont9571 = call i64 @prim_car(i64 %iKh$lst9572)                                   ; call prim_car
  %iKh$lst = call i64 @prim_cdr(i64 %iKh$lst9572)                                    ; call prim_cdr
  %arg10566 = add i64 0, 0                                                           ; quoted ()
  %rva11820 = add i64 0, 0                                                           ; quoted ()
  %rva11819 = call i64 @prim_cons(i64 %iKh$lst, i64 %rva11820)                       ; call prim_cons
  %rva11818 = call i64 @prim_cons(i64 %arg10566, i64 %rva11819)                      ; call prim_cons
  %cloptr15654 = inttoptr i64 %cont9571 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15655 = getelementptr inbounds i64, i64* %cloptr15654, i64 0                 ; &cloptr15654[0]
  %f15657 = load i64, i64* %i0ptr15655, align 8                                      ; load; *i0ptr15655
  %fptr15656 = inttoptr i64 %f15657 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15656(i64 %cont9571, i64 %rva11818)                 ; tail call
  ret void
}


define void @lam12664(i64 %env12665, i64 %rvp11911) {
  %envptr15658 = inttoptr i64 %env12665 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15659 = getelementptr inbounds i64, i64* %envptr15658, i64 3                ; &envptr15658[3]
  %a9373 = load i64, i64* %envptr15659, align 8                                      ; load; *envptr15659
  %envptr15660 = inttoptr i64 %env12665 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15661 = getelementptr inbounds i64, i64* %envptr15660, i64 2                ; &envptr15660[2]
  %h1l$amb = load i64, i64* %envptr15661, align 8                                    ; load; *envptr15661
  %envptr15662 = inttoptr i64 %env12665 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15663 = getelementptr inbounds i64, i64* %envptr15662, i64 1                ; &envptr15662[1]
  %UAc$assert = load i64, i64* %envptr15663, align 8                                 ; load; *envptr15663
  %_959570 = call i64 @prim_car(i64 %rvp11911)                                       ; call prim_car
  %rvp11910 = call i64 @prim_cdr(i64 %rvp11911)                                      ; call prim_cdr
  %a9374 = call i64 @prim_car(i64 %rvp11910)                                         ; call prim_car
  %na11822 = call i64 @prim_cdr(i64 %rvp11910)                                       ; call prim_cdr
  %arg10569 = call i64 @const_init_int(i64 5)                                        ; quoted int
  %a9375 = call i64 @prim_cons(i64 %arg10569, i64 %a9374)                            ; call prim_cons
  %arg10571 = call i64 @const_init_int(i64 4)                                        ; quoted int
  %a9376 = call i64 @prim_cons(i64 %arg10571, i64 %a9375)                            ; call prim_cons
  %arg10573 = call i64 @const_init_int(i64 3)                                        ; quoted int
  %a9377 = call i64 @prim_cons(i64 %arg10573, i64 %a9376)                            ; call prim_cons
  %arg10575 = call i64 @const_init_int(i64 2)                                        ; quoted int
  %a9378 = call i64 @prim_cons(i64 %arg10575, i64 %a9377)                            ; call prim_cons
  %cloptr15664 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr15666 = getelementptr inbounds i64, i64* %cloptr15664, i64 1                  ; &eptr15666[1]
  %eptr15667 = getelementptr inbounds i64, i64* %cloptr15664, i64 2                  ; &eptr15667[2]
  store i64 %UAc$assert, i64* %eptr15666                                             ; *eptr15666 = %UAc$assert
  store i64 %h1l$amb, i64* %eptr15667                                                ; *eptr15667 = %h1l$amb
  %eptr15665 = getelementptr inbounds i64, i64* %cloptr15664, i64 0                  ; &cloptr15664[0]
  %f15668 = ptrtoint void(i64,i64)* @lam12658 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15668, i64* %eptr15665                                                 ; store fptr
  %arg10577 = ptrtoint i64* %cloptr15664 to i64                                      ; closure cast; i64* -> i64
  %rva11909 = add i64 0, 0                                                           ; quoted ()
  %rva11908 = call i64 @prim_cons(i64 %a9378, i64 %rva11909)                         ; call prim_cons
  %rva11907 = call i64 @prim_cons(i64 %arg10577, i64 %rva11908)                      ; call prim_cons
  %cloptr15669 = inttoptr i64 %a9373 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr15670 = getelementptr inbounds i64, i64* %cloptr15669, i64 0                 ; &cloptr15669[0]
  %f15672 = load i64, i64* %i0ptr15670, align 8                                      ; load; *i0ptr15670
  %fptr15671 = inttoptr i64 %f15672 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15671(i64 %a9373, i64 %rva11907)                    ; tail call
  ret void
}


define void @lam12658(i64 %env12659, i64 %rvp11906) {
  %envptr15673 = inttoptr i64 %env12659 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15674 = getelementptr inbounds i64, i64* %envptr15673, i64 2                ; &envptr15673[2]
  %h1l$amb = load i64, i64* %envptr15674, align 8                                    ; load; *envptr15674
  %envptr15675 = inttoptr i64 %env12659 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15676 = getelementptr inbounds i64, i64* %envptr15675, i64 1                ; &envptr15675[1]
  %UAc$assert = load i64, i64* %envptr15676, align 8                                 ; load; *envptr15676
  %_959549 = call i64 @prim_car(i64 %rvp11906)                                       ; call prim_car
  %rvp11905 = call i64 @prim_cdr(i64 %rvp11906)                                      ; call prim_cdr
  %IVV$a = call i64 @prim_car(i64 %rvp11905)                                         ; call prim_car
  %na11824 = call i64 @prim_cdr(i64 %rvp11905)                                       ; call prim_cdr
  %arg10579 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9379 = call i64 @prim_vector_45ref(i64 %h1l$amb, i64 %arg10579)                  ; call prim_vector_45ref
  %cloptr15677 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15678 = getelementptr inbounds i64, i64* %cloptr15677, i64 0                  ; &cloptr15677[0]
  %f15679 = ptrtoint void(i64,i64)* @lam12655 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15679, i64* %eptr15678                                                 ; store fptr
  %arg10582 = ptrtoint i64* %cloptr15677 to i64                                      ; closure cast; i64* -> i64
  %cloptr15680 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15682 = getelementptr inbounds i64, i64* %cloptr15680, i64 1                  ; &eptr15682[1]
  %eptr15683 = getelementptr inbounds i64, i64* %cloptr15680, i64 2                  ; &eptr15683[2]
  %eptr15684 = getelementptr inbounds i64, i64* %cloptr15680, i64 3                  ; &eptr15684[3]
  %eptr15685 = getelementptr inbounds i64, i64* %cloptr15680, i64 4                  ; &eptr15685[4]
  store i64 %UAc$assert, i64* %eptr15682                                             ; *eptr15682 = %UAc$assert
  store i64 %h1l$amb, i64* %eptr15683                                                ; *eptr15683 = %h1l$amb
  store i64 %a9379, i64* %eptr15684                                                  ; *eptr15684 = %a9379
  store i64 %IVV$a, i64* %eptr15685                                                  ; *eptr15685 = %IVV$a
  %eptr15681 = getelementptr inbounds i64, i64* %cloptr15680, i64 0                  ; &cloptr15680[0]
  %f15686 = ptrtoint void(i64,i64)* @lam12651 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15686, i64* %eptr15681                                                 ; store fptr
  %arg10581 = ptrtoint i64* %cloptr15680 to i64                                      ; closure cast; i64* -> i64
  %rva11904 = add i64 0, 0                                                           ; quoted ()
  %rva11903 = call i64 @prim_cons(i64 %arg10581, i64 %rva11904)                      ; call prim_cons
  %cloptr15687 = inttoptr i64 %arg10582 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15688 = getelementptr inbounds i64, i64* %cloptr15687, i64 0                 ; &cloptr15687[0]
  %f15690 = load i64, i64* %i0ptr15688, align 8                                      ; load; *i0ptr15688
  %fptr15689 = inttoptr i64 %f15690 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15689(i64 %arg10582, i64 %rva11903)                 ; tail call
  ret void
}


define void @lam12655(i64 %env12656, i64 %dEy$lst9569) {
  %cont9568 = call i64 @prim_car(i64 %dEy$lst9569)                                   ; call prim_car
  %dEy$lst = call i64 @prim_cdr(i64 %dEy$lst9569)                                    ; call prim_cdr
  %arg10586 = add i64 0, 0                                                           ; quoted ()
  %rva11827 = add i64 0, 0                                                           ; quoted ()
  %rva11826 = call i64 @prim_cons(i64 %dEy$lst, i64 %rva11827)                       ; call prim_cons
  %rva11825 = call i64 @prim_cons(i64 %arg10586, i64 %rva11826)                      ; call prim_cons
  %cloptr15691 = inttoptr i64 %cont9568 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15692 = getelementptr inbounds i64, i64* %cloptr15691, i64 0                 ; &cloptr15691[0]
  %f15694 = load i64, i64* %i0ptr15692, align 8                                      ; load; *i0ptr15692
  %fptr15693 = inttoptr i64 %f15694 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15693(i64 %cont9568, i64 %rva11825)                 ; tail call
  ret void
}


define void @lam12651(i64 %env12652, i64 %rvp11902) {
  %envptr15695 = inttoptr i64 %env12652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15696 = getelementptr inbounds i64, i64* %envptr15695, i64 4                ; &envptr15695[4]
  %IVV$a = load i64, i64* %envptr15696, align 8                                      ; load; *envptr15696
  %envptr15697 = inttoptr i64 %env12652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15698 = getelementptr inbounds i64, i64* %envptr15697, i64 3                ; &envptr15697[3]
  %a9379 = load i64, i64* %envptr15698, align 8                                      ; load; *envptr15698
  %envptr15699 = inttoptr i64 %env12652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15700 = getelementptr inbounds i64, i64* %envptr15699, i64 2                ; &envptr15699[2]
  %h1l$amb = load i64, i64* %envptr15700, align 8                                    ; load; *envptr15700
  %envptr15701 = inttoptr i64 %env12652 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15702 = getelementptr inbounds i64, i64* %envptr15701, i64 1                ; &envptr15701[1]
  %UAc$assert = load i64, i64* %envptr15702, align 8                                 ; load; *envptr15702
  %_959567 = call i64 @prim_car(i64 %rvp11902)                                       ; call prim_car
  %rvp11901 = call i64 @prim_cdr(i64 %rvp11902)                                      ; call prim_cdr
  %a9380 = call i64 @prim_car(i64 %rvp11901)                                         ; call prim_car
  %na11829 = call i64 @prim_cdr(i64 %rvp11901)                                       ; call prim_cdr
  %arg10589 = call i64 @const_init_int(i64 6)                                        ; quoted int
  %a9381 = call i64 @prim_cons(i64 %arg10589, i64 %a9380)                            ; call prim_cons
  %arg10591 = call i64 @const_init_int(i64 5)                                        ; quoted int
  %a9382 = call i64 @prim_cons(i64 %arg10591, i64 %a9381)                            ; call prim_cons
  %arg10593 = call i64 @const_init_int(i64 4)                                        ; quoted int
  %a9383 = call i64 @prim_cons(i64 %arg10593, i64 %a9382)                            ; call prim_cons
  %arg10595 = call i64 @const_init_int(i64 3)                                        ; quoted int
  %a9384 = call i64 @prim_cons(i64 %arg10595, i64 %a9383)                            ; call prim_cons
  %arg10597 = call i64 @const_init_int(i64 2)                                        ; quoted int
  %a9385 = call i64 @prim_cons(i64 %arg10597, i64 %a9384)                            ; call prim_cons
  %cloptr15703 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr15705 = getelementptr inbounds i64, i64* %cloptr15703, i64 1                  ; &eptr15705[1]
  %eptr15706 = getelementptr inbounds i64, i64* %cloptr15703, i64 2                  ; &eptr15706[2]
  %eptr15707 = getelementptr inbounds i64, i64* %cloptr15703, i64 3                  ; &eptr15707[3]
  store i64 %UAc$assert, i64* %eptr15705                                             ; *eptr15705 = %UAc$assert
  store i64 %h1l$amb, i64* %eptr15706                                                ; *eptr15706 = %h1l$amb
  store i64 %IVV$a, i64* %eptr15707                                                  ; *eptr15707 = %IVV$a
  %eptr15704 = getelementptr inbounds i64, i64* %cloptr15703, i64 0                  ; &cloptr15703[0]
  %f15708 = ptrtoint void(i64,i64)* @lam12644 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15708, i64* %eptr15704                                                 ; store fptr
  %arg10599 = ptrtoint i64* %cloptr15703 to i64                                      ; closure cast; i64* -> i64
  %rva11900 = add i64 0, 0                                                           ; quoted ()
  %rva11899 = call i64 @prim_cons(i64 %a9385, i64 %rva11900)                         ; call prim_cons
  %rva11898 = call i64 @prim_cons(i64 %arg10599, i64 %rva11899)                      ; call prim_cons
  %cloptr15709 = inttoptr i64 %a9379 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr15710 = getelementptr inbounds i64, i64* %cloptr15709, i64 0                 ; &cloptr15709[0]
  %f15712 = load i64, i64* %i0ptr15710, align 8                                      ; load; *i0ptr15710
  %fptr15711 = inttoptr i64 %f15712 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15711(i64 %a9379, i64 %rva11898)                    ; tail call
  ret void
}


define void @lam12644(i64 %env12645, i64 %rvp11897) {
  %envptr15713 = inttoptr i64 %env12645 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15714 = getelementptr inbounds i64, i64* %envptr15713, i64 3                ; &envptr15713[3]
  %IVV$a = load i64, i64* %envptr15714, align 8                                      ; load; *envptr15714
  %envptr15715 = inttoptr i64 %env12645 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15716 = getelementptr inbounds i64, i64* %envptr15715, i64 2                ; &envptr15715[2]
  %h1l$amb = load i64, i64* %envptr15716, align 8                                    ; load; *envptr15716
  %envptr15717 = inttoptr i64 %env12645 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15718 = getelementptr inbounds i64, i64* %envptr15717, i64 1                ; &envptr15717[1]
  %UAc$assert = load i64, i64* %envptr15718, align 8                                 ; load; *envptr15718
  %_959550 = call i64 @prim_car(i64 %rvp11897)                                       ; call prim_car
  %rvp11896 = call i64 @prim_cdr(i64 %rvp11897)                                      ; call prim_cdr
  %yPs$b = call i64 @prim_car(i64 %rvp11896)                                         ; call prim_car
  %na11831 = call i64 @prim_cdr(i64 %rvp11896)                                       ; call prim_cdr
  %arg10601 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9386 = call i64 @prim_vector_45ref(i64 %h1l$amb, i64 %arg10601)                  ; call prim_vector_45ref
  %cloptr15719 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15720 = getelementptr inbounds i64, i64* %cloptr15719, i64 0                  ; &cloptr15719[0]
  %f15721 = ptrtoint void(i64,i64)* @lam12641 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15721, i64* %eptr15720                                                 ; store fptr
  %arg10604 = ptrtoint i64* %cloptr15719 to i64                                      ; closure cast; i64* -> i64
  %cloptr15722 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15724 = getelementptr inbounds i64, i64* %cloptr15722, i64 1                  ; &eptr15724[1]
  %eptr15725 = getelementptr inbounds i64, i64* %cloptr15722, i64 2                  ; &eptr15725[2]
  %eptr15726 = getelementptr inbounds i64, i64* %cloptr15722, i64 3                  ; &eptr15726[3]
  %eptr15727 = getelementptr inbounds i64, i64* %cloptr15722, i64 4                  ; &eptr15727[4]
  store i64 %UAc$assert, i64* %eptr15724                                             ; *eptr15724 = %UAc$assert
  store i64 %a9386, i64* %eptr15725                                                  ; *eptr15725 = %a9386
  store i64 %IVV$a, i64* %eptr15726                                                  ; *eptr15726 = %IVV$a
  store i64 %yPs$b, i64* %eptr15727                                                  ; *eptr15727 = %yPs$b
  %eptr15723 = getelementptr inbounds i64, i64* %cloptr15722, i64 0                  ; &cloptr15722[0]
  %f15728 = ptrtoint void(i64,i64)* @lam12637 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15728, i64* %eptr15723                                                 ; store fptr
  %arg10603 = ptrtoint i64* %cloptr15722 to i64                                      ; closure cast; i64* -> i64
  %rva11895 = add i64 0, 0                                                           ; quoted ()
  %rva11894 = call i64 @prim_cons(i64 %arg10603, i64 %rva11895)                      ; call prim_cons
  %cloptr15729 = inttoptr i64 %arg10604 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15730 = getelementptr inbounds i64, i64* %cloptr15729, i64 0                 ; &cloptr15729[0]
  %f15732 = load i64, i64* %i0ptr15730, align 8                                      ; load; *i0ptr15730
  %fptr15731 = inttoptr i64 %f15732 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15731(i64 %arg10604, i64 %rva11894)                 ; tail call
  ret void
}


define void @lam12641(i64 %env12642, i64 %Ril$lst9566) {
  %cont9565 = call i64 @prim_car(i64 %Ril$lst9566)                                   ; call prim_car
  %Ril$lst = call i64 @prim_cdr(i64 %Ril$lst9566)                                    ; call prim_cdr
  %arg10608 = add i64 0, 0                                                           ; quoted ()
  %rva11834 = add i64 0, 0                                                           ; quoted ()
  %rva11833 = call i64 @prim_cons(i64 %Ril$lst, i64 %rva11834)                       ; call prim_cons
  %rva11832 = call i64 @prim_cons(i64 %arg10608, i64 %rva11833)                      ; call prim_cons
  %cloptr15733 = inttoptr i64 %cont9565 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15734 = getelementptr inbounds i64, i64* %cloptr15733, i64 0                 ; &cloptr15733[0]
  %f15736 = load i64, i64* %i0ptr15734, align 8                                      ; load; *i0ptr15734
  %fptr15735 = inttoptr i64 %f15736 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15735(i64 %cont9565, i64 %rva11832)                 ; tail call
  ret void
}


define void @lam12637(i64 %env12638, i64 %rvp11893) {
  %envptr15737 = inttoptr i64 %env12638 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15738 = getelementptr inbounds i64, i64* %envptr15737, i64 4                ; &envptr15737[4]
  %yPs$b = load i64, i64* %envptr15738, align 8                                      ; load; *envptr15738
  %envptr15739 = inttoptr i64 %env12638 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15740 = getelementptr inbounds i64, i64* %envptr15739, i64 3                ; &envptr15739[3]
  %IVV$a = load i64, i64* %envptr15740, align 8                                      ; load; *envptr15740
  %envptr15741 = inttoptr i64 %env12638 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15742 = getelementptr inbounds i64, i64* %envptr15741, i64 2                ; &envptr15741[2]
  %a9386 = load i64, i64* %envptr15742, align 8                                      ; load; *envptr15742
  %envptr15743 = inttoptr i64 %env12638 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15744 = getelementptr inbounds i64, i64* %envptr15743, i64 1                ; &envptr15743[1]
  %UAc$assert = load i64, i64* %envptr15744, align 8                                 ; load; *envptr15744
  %_959564 = call i64 @prim_car(i64 %rvp11893)                                       ; call prim_car
  %rvp11892 = call i64 @prim_cdr(i64 %rvp11893)                                      ; call prim_cdr
  %a9387 = call i64 @prim_car(i64 %rvp11892)                                         ; call prim_car
  %na11836 = call i64 @prim_cdr(i64 %rvp11892)                                       ; call prim_cdr
  %arg10611 = call i64 @const_init_int(i64 5)                                        ; quoted int
  %a9388 = call i64 @prim_cons(i64 %arg10611, i64 %a9387)                            ; call prim_cons
  %arg10613 = call i64 @const_init_int(i64 4)                                        ; quoted int
  %a9389 = call i64 @prim_cons(i64 %arg10613, i64 %a9388)                            ; call prim_cons
  %arg10615 = call i64 @const_init_int(i64 3)                                        ; quoted int
  %a9390 = call i64 @prim_cons(i64 %arg10615, i64 %a9389)                            ; call prim_cons
  %arg10617 = call i64 @const_init_int(i64 2)                                        ; quoted int
  %a9391 = call i64 @prim_cons(i64 %arg10617, i64 %a9390)                            ; call prim_cons
  %cloptr15745 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr15747 = getelementptr inbounds i64, i64* %cloptr15745, i64 1                  ; &eptr15747[1]
  %eptr15748 = getelementptr inbounds i64, i64* %cloptr15745, i64 2                  ; &eptr15748[2]
  %eptr15749 = getelementptr inbounds i64, i64* %cloptr15745, i64 3                  ; &eptr15749[3]
  store i64 %UAc$assert, i64* %eptr15747                                             ; *eptr15747 = %UAc$assert
  store i64 %IVV$a, i64* %eptr15748                                                  ; *eptr15748 = %IVV$a
  store i64 %yPs$b, i64* %eptr15749                                                  ; *eptr15749 = %yPs$b
  %eptr15746 = getelementptr inbounds i64, i64* %cloptr15745, i64 0                  ; &cloptr15745[0]
  %f15750 = ptrtoint void(i64,i64)* @lam12631 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15750, i64* %eptr15746                                                 ; store fptr
  %arg10619 = ptrtoint i64* %cloptr15745 to i64                                      ; closure cast; i64* -> i64
  %rva11891 = add i64 0, 0                                                           ; quoted ()
  %rva11890 = call i64 @prim_cons(i64 %a9391, i64 %rva11891)                         ; call prim_cons
  %rva11889 = call i64 @prim_cons(i64 %arg10619, i64 %rva11890)                      ; call prim_cons
  %cloptr15751 = inttoptr i64 %a9386 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr15752 = getelementptr inbounds i64, i64* %cloptr15751, i64 0                 ; &cloptr15751[0]
  %f15754 = load i64, i64* %i0ptr15752, align 8                                      ; load; *i0ptr15752
  %fptr15753 = inttoptr i64 %f15754 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15753(i64 %a9386, i64 %rva11889)                    ; tail call
  ret void
}


define void @lam12631(i64 %env12632, i64 %rvp11888) {
  %envptr15755 = inttoptr i64 %env12632 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15756 = getelementptr inbounds i64, i64* %envptr15755, i64 3                ; &envptr15755[3]
  %yPs$b = load i64, i64* %envptr15756, align 8                                      ; load; *envptr15756
  %envptr15757 = inttoptr i64 %env12632 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15758 = getelementptr inbounds i64, i64* %envptr15757, i64 2                ; &envptr15757[2]
  %IVV$a = load i64, i64* %envptr15758, align 8                                      ; load; *envptr15758
  %envptr15759 = inttoptr i64 %env12632 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15760 = getelementptr inbounds i64, i64* %envptr15759, i64 1                ; &envptr15759[1]
  %UAc$assert = load i64, i64* %envptr15760, align 8                                 ; load; *envptr15760
  %_959551 = call i64 @prim_car(i64 %rvp11888)                                       ; call prim_car
  %rvp11887 = call i64 @prim_cdr(i64 %rvp11888)                                      ; call prim_cdr
  %sGx$c = call i64 @prim_car(i64 %rvp11887)                                         ; call prim_car
  %na11838 = call i64 @prim_cdr(i64 %rvp11887)                                       ; call prim_cdr
  %cloptr15761 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15762 = getelementptr inbounds i64, i64* %cloptr15761, i64 0                  ; &cloptr15761[0]
  %f15763 = ptrtoint void(i64,i64)* @lam12629 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15763, i64* %eptr15762                                                 ; store fptr
  %arg10622 = ptrtoint i64* %cloptr15761 to i64                                      ; closure cast; i64* -> i64
  %cloptr15764 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15766 = getelementptr inbounds i64, i64* %cloptr15764, i64 1                  ; &eptr15766[1]
  %eptr15767 = getelementptr inbounds i64, i64* %cloptr15764, i64 2                  ; &eptr15767[2]
  %eptr15768 = getelementptr inbounds i64, i64* %cloptr15764, i64 3                  ; &eptr15768[3]
  %eptr15769 = getelementptr inbounds i64, i64* %cloptr15764, i64 4                  ; &eptr15769[4]
  store i64 %UAc$assert, i64* %eptr15766                                             ; *eptr15766 = %UAc$assert
  store i64 %IVV$a, i64* %eptr15767                                                  ; *eptr15767 = %IVV$a
  store i64 %yPs$b, i64* %eptr15768                                                  ; *eptr15768 = %yPs$b
  store i64 %sGx$c, i64* %eptr15769                                                  ; *eptr15769 = %sGx$c
  %eptr15765 = getelementptr inbounds i64, i64* %cloptr15764, i64 0                  ; &cloptr15764[0]
  %f15770 = ptrtoint void(i64,i64)* @lam12625 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15770, i64* %eptr15765                                                 ; store fptr
  %arg10621 = ptrtoint i64* %cloptr15764 to i64                                      ; closure cast; i64* -> i64
  %rva11886 = add i64 0, 0                                                           ; quoted ()
  %rva11885 = call i64 @prim_cons(i64 %arg10621, i64 %rva11886)                      ; call prim_cons
  %cloptr15771 = inttoptr i64 %arg10622 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15772 = getelementptr inbounds i64, i64* %cloptr15771, i64 0                 ; &cloptr15771[0]
  %f15774 = load i64, i64* %i0ptr15772, align 8                                      ; load; *i0ptr15772
  %fptr15773 = inttoptr i64 %f15774 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15773(i64 %arg10622, i64 %rva11885)                 ; tail call
  ret void
}


define void @lam12629(i64 %env12630, i64 %SyL$lst9563) {
  %cont9562 = call i64 @prim_car(i64 %SyL$lst9563)                                   ; call prim_car
  %SyL$lst = call i64 @prim_cdr(i64 %SyL$lst9563)                                    ; call prim_cdr
  %arg10626 = add i64 0, 0                                                           ; quoted ()
  %rva11841 = add i64 0, 0                                                           ; quoted ()
  %rva11840 = call i64 @prim_cons(i64 %SyL$lst, i64 %rva11841)                       ; call prim_cons
  %rva11839 = call i64 @prim_cons(i64 %arg10626, i64 %rva11840)                      ; call prim_cons
  %cloptr15775 = inttoptr i64 %cont9562 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15776 = getelementptr inbounds i64, i64* %cloptr15775, i64 0                 ; &cloptr15775[0]
  %f15778 = load i64, i64* %i0ptr15776, align 8                                      ; load; *i0ptr15776
  %fptr15777 = inttoptr i64 %f15778 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15777(i64 %cont9562, i64 %rva11839)                 ; tail call
  ret void
}


define void @lam12625(i64 %env12626, i64 %rvp11884) {
  %envptr15779 = inttoptr i64 %env12626 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15780 = getelementptr inbounds i64, i64* %envptr15779, i64 4                ; &envptr15779[4]
  %sGx$c = load i64, i64* %envptr15780, align 8                                      ; load; *envptr15780
  %envptr15781 = inttoptr i64 %env12626 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15782 = getelementptr inbounds i64, i64* %envptr15781, i64 3                ; &envptr15781[3]
  %yPs$b = load i64, i64* %envptr15782, align 8                                      ; load; *envptr15782
  %envptr15783 = inttoptr i64 %env12626 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15784 = getelementptr inbounds i64, i64* %envptr15783, i64 2                ; &envptr15783[2]
  %IVV$a = load i64, i64* %envptr15784, align 8                                      ; load; *envptr15784
  %envptr15785 = inttoptr i64 %env12626 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15786 = getelementptr inbounds i64, i64* %envptr15785, i64 1                ; &envptr15785[1]
  %UAc$assert = load i64, i64* %envptr15786, align 8                                 ; load; *envptr15786
  %_959560 = call i64 @prim_car(i64 %rvp11884)                                       ; call prim_car
  %rvp11883 = call i64 @prim_cdr(i64 %rvp11884)                                      ; call prim_cdr
  %a9392 = call i64 @prim_car(i64 %rvp11883)                                         ; call prim_car
  %na11843 = call i64 @prim_cdr(i64 %rvp11883)                                       ; call prim_cdr
  %arg10629 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9561 = call i64 @prim_make_45vector(i64 %arg10629, i64 %a9392)             ; call prim_make_45vector
  %cloptr15787 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15789 = getelementptr inbounds i64, i64* %cloptr15787, i64 1                  ; &eptr15789[1]
  %eptr15790 = getelementptr inbounds i64, i64* %cloptr15787, i64 2                  ; &eptr15790[2]
  %eptr15791 = getelementptr inbounds i64, i64* %cloptr15787, i64 3                  ; &eptr15791[3]
  %eptr15792 = getelementptr inbounds i64, i64* %cloptr15787, i64 4                  ; &eptr15792[4]
  store i64 %UAc$assert, i64* %eptr15789                                             ; *eptr15789 = %UAc$assert
  store i64 %IVV$a, i64* %eptr15790                                                  ; *eptr15790 = %IVV$a
  store i64 %yPs$b, i64* %eptr15791                                                  ; *eptr15791 = %yPs$b
  store i64 %sGx$c, i64* %eptr15792                                                  ; *eptr15792 = %sGx$c
  %eptr15788 = getelementptr inbounds i64, i64* %cloptr15787, i64 0                  ; &cloptr15787[0]
  %f15793 = ptrtoint void(i64,i64)* @lam12622 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15793, i64* %eptr15788                                                 ; store fptr
  %arg10632 = ptrtoint i64* %cloptr15787 to i64                                      ; closure cast; i64* -> i64
  %arg10631 = add i64 0, 0                                                           ; quoted ()
  %rva11882 = add i64 0, 0                                                           ; quoted ()
  %rva11881 = call i64 @prim_cons(i64 %retprim9561, i64 %rva11882)                   ; call prim_cons
  %rva11880 = call i64 @prim_cons(i64 %arg10631, i64 %rva11881)                      ; call prim_cons
  %cloptr15794 = inttoptr i64 %arg10632 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15795 = getelementptr inbounds i64, i64* %cloptr15794, i64 0                 ; &cloptr15794[0]
  %f15797 = load i64, i64* %i0ptr15795, align 8                                      ; load; *i0ptr15795
  %fptr15796 = inttoptr i64 %f15797 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15796(i64 %arg10632, i64 %rva11880)                 ; tail call
  ret void
}


define void @lam12622(i64 %env12623, i64 %rvp11879) {
  %envptr15798 = inttoptr i64 %env12623 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15799 = getelementptr inbounds i64, i64* %envptr15798, i64 4                ; &envptr15798[4]
  %sGx$c = load i64, i64* %envptr15799, align 8                                      ; load; *envptr15799
  %envptr15800 = inttoptr i64 %env12623 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15801 = getelementptr inbounds i64, i64* %envptr15800, i64 3                ; &envptr15800[3]
  %yPs$b = load i64, i64* %envptr15801, align 8                                      ; load; *envptr15801
  %envptr15802 = inttoptr i64 %env12623 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15803 = getelementptr inbounds i64, i64* %envptr15802, i64 2                ; &envptr15802[2]
  %IVV$a = load i64, i64* %envptr15803, align 8                                      ; load; *envptr15803
  %envptr15804 = inttoptr i64 %env12623 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15805 = getelementptr inbounds i64, i64* %envptr15804, i64 1                ; &envptr15804[1]
  %UAc$assert = load i64, i64* %envptr15805, align 8                                 ; load; *envptr15805
  %_959552 = call i64 @prim_car(i64 %rvp11879)                                       ; call prim_car
  %rvp11878 = call i64 @prim_cdr(i64 %rvp11879)                                      ; call prim_cdr
  %jWY$tmp9202 = call i64 @prim_car(i64 %rvp11878)                                   ; call prim_car
  %na11845 = call i64 @prim_cdr(i64 %rvp11878)                                       ; call prim_cdr
  %arg10633 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9393 = call i64 @prim_vector_45ref(i64 %UAc$assert, i64 %arg10633)               ; call prim_vector_45ref
  %a9394 = call i64 @prim__42(i64 %IVV$a, i64 %IVV$a)                                ; call prim__42
  %a9395 = call i64 @prim__42(i64 %yPs$b, i64 %yPs$b)                                ; call prim__42
  %a9396 = call i64 @prim__43(i64 %a9394, i64 %a9395)                                ; call prim__43
  %a9397 = call i64 @prim__42(i64 %sGx$c, i64 %sGx$c)                                ; call prim__42
  %a9398 = call i64 @prim__61(i64 %a9396, i64 %a9397)                                ; call prim__61
  %cloptr15806 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15808 = getelementptr inbounds i64, i64* %cloptr15806, i64 1                  ; &eptr15808[1]
  %eptr15809 = getelementptr inbounds i64, i64* %cloptr15806, i64 2                  ; &eptr15809[2]
  %eptr15810 = getelementptr inbounds i64, i64* %cloptr15806, i64 3                  ; &eptr15810[3]
  %eptr15811 = getelementptr inbounds i64, i64* %cloptr15806, i64 4                  ; &eptr15811[4]
  store i64 %jWY$tmp9202, i64* %eptr15808                                            ; *eptr15808 = %jWY$tmp9202
  store i64 %IVV$a, i64* %eptr15809                                                  ; *eptr15809 = %IVV$a
  store i64 %yPs$b, i64* %eptr15810                                                  ; *eptr15810 = %yPs$b
  store i64 %sGx$c, i64* %eptr15811                                                  ; *eptr15811 = %sGx$c
  %eptr15807 = getelementptr inbounds i64, i64* %cloptr15806, i64 0                  ; &cloptr15806[0]
  %f15812 = ptrtoint void(i64,i64)* @lam12619 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15812, i64* %eptr15807                                                 ; store fptr
  %arg10646 = ptrtoint i64* %cloptr15806 to i64                                      ; closure cast; i64* -> i64
  %rva11877 = add i64 0, 0                                                           ; quoted ()
  %rva11876 = call i64 @prim_cons(i64 %a9398, i64 %rva11877)                         ; call prim_cons
  %rva11875 = call i64 @prim_cons(i64 %arg10646, i64 %rva11876)                      ; call prim_cons
  %cloptr15813 = inttoptr i64 %a9393 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr15814 = getelementptr inbounds i64, i64* %cloptr15813, i64 0                 ; &cloptr15813[0]
  %f15816 = load i64, i64* %i0ptr15814, align 8                                      ; load; *i0ptr15814
  %fptr15815 = inttoptr i64 %f15816 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15815(i64 %a9393, i64 %rva11875)                    ; tail call
  ret void
}


define void @lam12619(i64 %env12620, i64 %rvp11874) {
  %envptr15817 = inttoptr i64 %env12620 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15818 = getelementptr inbounds i64, i64* %envptr15817, i64 4                ; &envptr15817[4]
  %sGx$c = load i64, i64* %envptr15818, align 8                                      ; load; *envptr15818
  %envptr15819 = inttoptr i64 %env12620 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15820 = getelementptr inbounds i64, i64* %envptr15819, i64 3                ; &envptr15819[3]
  %yPs$b = load i64, i64* %envptr15820, align 8                                      ; load; *envptr15820
  %envptr15821 = inttoptr i64 %env12620 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15822 = getelementptr inbounds i64, i64* %envptr15821, i64 2                ; &envptr15821[2]
  %IVV$a = load i64, i64* %envptr15822, align 8                                      ; load; *envptr15822
  %envptr15823 = inttoptr i64 %env12620 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15824 = getelementptr inbounds i64, i64* %envptr15823, i64 1                ; &envptr15823[1]
  %jWY$tmp9202 = load i64, i64* %envptr15824, align 8                                ; load; *envptr15824
  %_959558 = call i64 @prim_car(i64 %rvp11874)                                       ; call prim_car
  %rvp11873 = call i64 @prim_cdr(i64 %rvp11874)                                      ; call prim_cdr
  %a9399 = call i64 @prim_car(i64 %rvp11873)                                         ; call prim_car
  %na11847 = call i64 @prim_cdr(i64 %rvp11873)                                       ; call prim_cdr
  %arg10649 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %retprim9559 = call i64 @prim_vector_45set_33(i64 %jWY$tmp9202, i64 %arg10649, i64 %a9399); call prim_vector_45set_33
  %cloptr15825 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr15827 = getelementptr inbounds i64, i64* %cloptr15825, i64 1                  ; &eptr15827[1]
  %eptr15828 = getelementptr inbounds i64, i64* %cloptr15825, i64 2                  ; &eptr15828[2]
  %eptr15829 = getelementptr inbounds i64, i64* %cloptr15825, i64 3                  ; &eptr15829[3]
  store i64 %IVV$a, i64* %eptr15827                                                  ; *eptr15827 = %IVV$a
  store i64 %yPs$b, i64* %eptr15828                                                  ; *eptr15828 = %yPs$b
  store i64 %sGx$c, i64* %eptr15829                                                  ; *eptr15829 = %sGx$c
  %eptr15826 = getelementptr inbounds i64, i64* %cloptr15825, i64 0                  ; &cloptr15825[0]
  %f15830 = ptrtoint void(i64,i64)* @lam12616 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15830, i64* %eptr15826                                                 ; store fptr
  %arg10653 = ptrtoint i64* %cloptr15825 to i64                                      ; closure cast; i64* -> i64
  %arg10652 = add i64 0, 0                                                           ; quoted ()
  %rva11872 = add i64 0, 0                                                           ; quoted ()
  %rva11871 = call i64 @prim_cons(i64 %retprim9559, i64 %rva11872)                   ; call prim_cons
  %rva11870 = call i64 @prim_cons(i64 %arg10652, i64 %rva11871)                      ; call prim_cons
  %cloptr15831 = inttoptr i64 %arg10653 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15832 = getelementptr inbounds i64, i64* %cloptr15831, i64 0                 ; &cloptr15831[0]
  %f15834 = load i64, i64* %i0ptr15832, align 8                                      ; load; *i0ptr15832
  %fptr15833 = inttoptr i64 %f15834 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15833(i64 %arg10653, i64 %rva11870)                 ; tail call
  ret void
}


define void @lam12616(i64 %env12617, i64 %rvp11869) {
  %envptr15835 = inttoptr i64 %env12617 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15836 = getelementptr inbounds i64, i64* %envptr15835, i64 3                ; &envptr15835[3]
  %sGx$c = load i64, i64* %envptr15836, align 8                                      ; load; *envptr15836
  %envptr15837 = inttoptr i64 %env12617 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15838 = getelementptr inbounds i64, i64* %envptr15837, i64 2                ; &envptr15837[2]
  %yPs$b = load i64, i64* %envptr15838, align 8                                      ; load; *envptr15838
  %envptr15839 = inttoptr i64 %env12617 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15840 = getelementptr inbounds i64, i64* %envptr15839, i64 1                ; &envptr15839[1]
  %IVV$a = load i64, i64* %envptr15840, align 8                                      ; load; *envptr15840
  %_959553 = call i64 @prim_car(i64 %rvp11869)                                       ; call prim_car
  %rvp11868 = call i64 @prim_cdr(i64 %rvp11869)                                      ; call prim_cdr
  %eK7$_959223 = call i64 @prim_car(i64 %rvp11868)                                   ; call prim_car
  %na11849 = call i64 @prim_cdr(i64 %rvp11868)                                       ; call prim_cdr
  %cloptr15841 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15842 = getelementptr inbounds i64, i64* %cloptr15841, i64 0                  ; &cloptr15841[0]
  %f15843 = ptrtoint void(i64,i64)* @lam12614 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15843, i64* %eptr15842                                                 ; store fptr
  %arg10655 = ptrtoint i64* %cloptr15841 to i64                                      ; closure cast; i64* -> i64
  %cloptr15844 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr15846 = getelementptr inbounds i64, i64* %cloptr15844, i64 1                  ; &eptr15846[1]
  %eptr15847 = getelementptr inbounds i64, i64* %cloptr15844, i64 2                  ; &eptr15847[2]
  %eptr15848 = getelementptr inbounds i64, i64* %cloptr15844, i64 3                  ; &eptr15848[3]
  store i64 %IVV$a, i64* %eptr15846                                                  ; *eptr15846 = %IVV$a
  store i64 %yPs$b, i64* %eptr15847                                                  ; *eptr15847 = %yPs$b
  store i64 %sGx$c, i64* %eptr15848                                                  ; *eptr15848 = %sGx$c
  %eptr15845 = getelementptr inbounds i64, i64* %cloptr15844, i64 0                  ; &cloptr15844[0]
  %f15849 = ptrtoint void(i64,i64)* @lam12610 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15849, i64* %eptr15845                                                 ; store fptr
  %arg10654 = ptrtoint i64* %cloptr15844 to i64                                      ; closure cast; i64* -> i64
  %rva11867 = add i64 0, 0                                                           ; quoted ()
  %rva11866 = call i64 @prim_cons(i64 %arg10654, i64 %rva11867)                      ; call prim_cons
  %cloptr15850 = inttoptr i64 %arg10655 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15851 = getelementptr inbounds i64, i64* %cloptr15850, i64 0                 ; &cloptr15850[0]
  %f15853 = load i64, i64* %i0ptr15851, align 8                                      ; load; *i0ptr15851
  %fptr15852 = inttoptr i64 %f15853 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15852(i64 %arg10655, i64 %rva11866)                 ; tail call
  ret void
}


define void @lam12614(i64 %env12615, i64 %C49$lst9557) {
  %cont9556 = call i64 @prim_car(i64 %C49$lst9557)                                   ; call prim_car
  %C49$lst = call i64 @prim_cdr(i64 %C49$lst9557)                                    ; call prim_cdr
  %arg10659 = add i64 0, 0                                                           ; quoted ()
  %rva11852 = add i64 0, 0                                                           ; quoted ()
  %rva11851 = call i64 @prim_cons(i64 %C49$lst, i64 %rva11852)                       ; call prim_cons
  %rva11850 = call i64 @prim_cons(i64 %arg10659, i64 %rva11851)                      ; call prim_cons
  %cloptr15854 = inttoptr i64 %cont9556 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15855 = getelementptr inbounds i64, i64* %cloptr15854, i64 0                 ; &cloptr15854[0]
  %f15857 = load i64, i64* %i0ptr15855, align 8                                      ; load; *i0ptr15855
  %fptr15856 = inttoptr i64 %f15857 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15856(i64 %cont9556, i64 %rva11850)                 ; tail call
  ret void
}


define void @lam12610(i64 %env12611, i64 %rvp11865) {
  %envptr15858 = inttoptr i64 %env12611 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15859 = getelementptr inbounds i64, i64* %envptr15858, i64 3                ; &envptr15858[3]
  %sGx$c = load i64, i64* %envptr15859, align 8                                      ; load; *envptr15859
  %envptr15860 = inttoptr i64 %env12611 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15861 = getelementptr inbounds i64, i64* %envptr15860, i64 2                ; &envptr15860[2]
  %yPs$b = load i64, i64* %envptr15861, align 8                                      ; load; *envptr15861
  %envptr15862 = inttoptr i64 %env12611 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15863 = getelementptr inbounds i64, i64* %envptr15862, i64 1                ; &envptr15862[1]
  %IVV$a = load i64, i64* %envptr15863, align 8                                      ; load; *envptr15863
  %_959554 = call i64 @prim_car(i64 %rvp11865)                                       ; call prim_car
  %rvp11864 = call i64 @prim_cdr(i64 %rvp11865)                                      ; call prim_cdr
  %a9400 = call i64 @prim_car(i64 %rvp11864)                                         ; call prim_car
  %na11854 = call i64 @prim_cdr(i64 %rvp11864)                                       ; call prim_cdr
  %a9401 = call i64 @prim_cons(i64 %sGx$c, i64 %a9400)                               ; call prim_cons
  %a9402 = call i64 @prim_cons(i64 %yPs$b, i64 %a9401)                               ; call prim_cons
  %a9403 = call i64 @prim_cons(i64 %IVV$a, i64 %a9402)                               ; call prim_cons
  %arg10668 = call i64 @const_init_symbol(i8* getelementptr inbounds ([9 x i8], [9 x i8]* @sym15864, i32 0, i32 0)); quoted string
  %retprim9555 = call i64 @prim_cons(i64 %arg10668, i64 %a9403)                      ; call prim_cons
  %cloptr15865 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15866 = getelementptr inbounds i64, i64* %cloptr15865, i64 0                  ; &cloptr15865[0]
  %f15867 = ptrtoint void(i64,i64)* @lam12607 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15867, i64* %eptr15866                                                 ; store fptr
  %arg10671 = ptrtoint i64* %cloptr15865 to i64                                      ; closure cast; i64* -> i64
  %arg10670 = add i64 0, 0                                                           ; quoted ()
  %rva11863 = add i64 0, 0                                                           ; quoted ()
  %rva11862 = call i64 @prim_cons(i64 %retprim9555, i64 %rva11863)                   ; call prim_cons
  %rva11861 = call i64 @prim_cons(i64 %arg10670, i64 %rva11862)                      ; call prim_cons
  %cloptr15868 = inttoptr i64 %arg10671 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15869 = getelementptr inbounds i64, i64* %cloptr15868, i64 0                 ; &cloptr15868[0]
  %f15871 = load i64, i64* %i0ptr15869, align 8                                      ; load; *i0ptr15869
  %fptr15870 = inttoptr i64 %f15871 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15870(i64 %arg10671, i64 %rva11861)                 ; tail call
  ret void
}


define void @lam12607(i64 %env12608, i64 %rvp11860) {
  %_950 = call i64 @prim_car(i64 %rvp11860)                                          ; call prim_car
  %rvp11859 = call i64 @prim_cdr(i64 %rvp11860)                                      ; call prim_cdr
  %x = call i64 @prim_car(i64 %rvp11859)                                             ; call prim_car
  %na11856 = call i64 @prim_cdr(i64 %rvp11859)                                       ; call prim_cdr
  %_951 = call i64 @prim_halt(i64 %x)                                                ; call prim_halt
  %rva11858 = add i64 0, 0                                                           ; quoted ()
  %rva11857 = call i64 @prim_cons(i64 %_951, i64 %rva11858)                          ; call prim_cons
  %cloptr15872 = inttoptr i64 %_951 to i64*                                          ; closure/env cast; i64 -> i64*
  %i0ptr15873 = getelementptr inbounds i64, i64* %cloptr15872, i64 0                 ; &cloptr15872[0]
  %f15875 = load i64, i64* %i0ptr15873, align 8                                      ; load; *i0ptr15873
  %fptr15874 = inttoptr i64 %f15875 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15874(i64 %_951, i64 %rva11857)                     ; tail call
  ret void
}


define void @lam12570(i64 %env12571, i64 %rvp12068) {
  %envptr15876 = inttoptr i64 %env12571 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15877 = getelementptr inbounds i64, i64* %envptr15876, i64 3                ; &envptr15876[3]
  %ujo$_37map1 = load i64, i64* %envptr15877, align 8                                ; load; *envptr15877
  %envptr15878 = inttoptr i64 %env12571 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15879 = getelementptr inbounds i64, i64* %envptr15878, i64 2                ; &envptr15878[2]
  %LWd$_37foldr1 = load i64, i64* %envptr15879, align 8                              ; load; *envptr15879
  %envptr15880 = inttoptr i64 %env12571 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15881 = getelementptr inbounds i64, i64* %envptr15880, i64 1                ; &envptr15880[1]
  %ZKo$_37foldr = load i64, i64* %envptr15881, align 8                               ; load; *envptr15881
  %cont9605 = call i64 @prim_car(i64 %rvp12068)                                      ; call prim_car
  %rvp12067 = call i64 @prim_cdr(i64 %rvp12068)                                      ; call prim_cdr
  %gT1$_37foldl = call i64 @prim_car(i64 %rvp12067)                                  ; call prim_car
  %na11975 = call i64 @prim_cdr(i64 %rvp12067)                                       ; call prim_cdr
  %arg10676 = add i64 0, 0                                                           ; quoted ()
  %cloptr15882 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr15884 = getelementptr inbounds i64, i64* %cloptr15882, i64 1                  ; &eptr15884[1]
  %eptr15885 = getelementptr inbounds i64, i64* %cloptr15882, i64 2                  ; &eptr15885[2]
  %eptr15886 = getelementptr inbounds i64, i64* %cloptr15882, i64 3                  ; &eptr15886[3]
  %eptr15887 = getelementptr inbounds i64, i64* %cloptr15882, i64 4                  ; &eptr15887[4]
  store i64 %ZKo$_37foldr, i64* %eptr15884                                           ; *eptr15884 = %ZKo$_37foldr
  store i64 %LWd$_37foldr1, i64* %eptr15885                                          ; *eptr15885 = %LWd$_37foldr1
  store i64 %ujo$_37map1, i64* %eptr15886                                            ; *eptr15886 = %ujo$_37map1
  store i64 %gT1$_37foldl, i64* %eptr15887                                           ; *eptr15887 = %gT1$_37foldl
  %eptr15883 = getelementptr inbounds i64, i64* %cloptr15882, i64 0                  ; &cloptr15882[0]
  %f15888 = ptrtoint void(i64,i64)* @lam12567 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15888, i64* %eptr15883                                                 ; store fptr
  %arg10675 = ptrtoint i64* %cloptr15882 to i64                                      ; closure cast; i64* -> i64
  %rva12066 = add i64 0, 0                                                           ; quoted ()
  %rva12065 = call i64 @prim_cons(i64 %arg10675, i64 %rva12066)                      ; call prim_cons
  %rva12064 = call i64 @prim_cons(i64 %arg10676, i64 %rva12065)                      ; call prim_cons
  %cloptr15889 = inttoptr i64 %cont9605 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15890 = getelementptr inbounds i64, i64* %cloptr15889, i64 0                 ; &cloptr15889[0]
  %f15892 = load i64, i64* %i0ptr15890, align 8                                      ; load; *i0ptr15890
  %fptr15891 = inttoptr i64 %f15892 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15891(i64 %cont9605, i64 %rva12064)                 ; tail call
  ret void
}


define void @lam12567(i64 %env12568, i64 %onO$args9607) {
  %envptr15893 = inttoptr i64 %env12568 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15894 = getelementptr inbounds i64, i64* %envptr15893, i64 4                ; &envptr15893[4]
  %gT1$_37foldl = load i64, i64* %envptr15894, align 8                               ; load; *envptr15894
  %envptr15895 = inttoptr i64 %env12568 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15896 = getelementptr inbounds i64, i64* %envptr15895, i64 3                ; &envptr15895[3]
  %ujo$_37map1 = load i64, i64* %envptr15896, align 8                                ; load; *envptr15896
  %envptr15897 = inttoptr i64 %env12568 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15898 = getelementptr inbounds i64, i64* %envptr15897, i64 2                ; &envptr15897[2]
  %LWd$_37foldr1 = load i64, i64* %envptr15898, align 8                              ; load; *envptr15898
  %envptr15899 = inttoptr i64 %env12568 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15900 = getelementptr inbounds i64, i64* %envptr15899, i64 1                ; &envptr15899[1]
  %ZKo$_37foldr = load i64, i64* %envptr15900, align 8                               ; load; *envptr15900
  %cont9606 = call i64 @prim_car(i64 %onO$args9607)                                  ; call prim_car
  %onO$args = call i64 @prim_cdr(i64 %onO$args9607)                                  ; call prim_cdr
  %Z69$f = call i64 @prim_car(i64 %onO$args)                                         ; call prim_car
  %a9264 = call i64 @prim_cdr(i64 %onO$args)                                         ; call prim_cdr
  %retprim9626 = call i64 @prim_car(i64 %a9264)                                      ; call prim_car
  %cloptr15901 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr15903 = getelementptr inbounds i64, i64* %cloptr15901, i64 1                  ; &eptr15903[1]
  %eptr15904 = getelementptr inbounds i64, i64* %cloptr15901, i64 2                  ; &eptr15904[2]
  %eptr15905 = getelementptr inbounds i64, i64* %cloptr15901, i64 3                  ; &eptr15905[3]
  %eptr15906 = getelementptr inbounds i64, i64* %cloptr15901, i64 4                  ; &eptr15906[4]
  %eptr15907 = getelementptr inbounds i64, i64* %cloptr15901, i64 5                  ; &eptr15907[5]
  %eptr15908 = getelementptr inbounds i64, i64* %cloptr15901, i64 6                  ; &eptr15908[6]
  %eptr15909 = getelementptr inbounds i64, i64* %cloptr15901, i64 7                  ; &eptr15909[7]
  store i64 %ZKo$_37foldr, i64* %eptr15903                                           ; *eptr15903 = %ZKo$_37foldr
  store i64 %LWd$_37foldr1, i64* %eptr15904                                          ; *eptr15904 = %LWd$_37foldr1
  store i64 %cont9606, i64* %eptr15905                                               ; *eptr15905 = %cont9606
  store i64 %ujo$_37map1, i64* %eptr15906                                            ; *eptr15906 = %ujo$_37map1
  store i64 %gT1$_37foldl, i64* %eptr15907                                           ; *eptr15907 = %gT1$_37foldl
  store i64 %onO$args, i64* %eptr15908                                               ; *eptr15908 = %onO$args
  store i64 %Z69$f, i64* %eptr15909                                                  ; *eptr15909 = %Z69$f
  %eptr15902 = getelementptr inbounds i64, i64* %cloptr15901, i64 0                  ; &cloptr15901[0]
  %f15910 = ptrtoint void(i64,i64)* @lam12565 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15910, i64* %eptr15902                                                 ; store fptr
  %arg10685 = ptrtoint i64* %cloptr15901 to i64                                      ; closure cast; i64* -> i64
  %arg10684 = add i64 0, 0                                                           ; quoted ()
  %rva12063 = add i64 0, 0                                                           ; quoted ()
  %rva12062 = call i64 @prim_cons(i64 %retprim9626, i64 %rva12063)                   ; call prim_cons
  %rva12061 = call i64 @prim_cons(i64 %arg10684, i64 %rva12062)                      ; call prim_cons
  %cloptr15911 = inttoptr i64 %arg10685 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15912 = getelementptr inbounds i64, i64* %cloptr15911, i64 0                 ; &cloptr15911[0]
  %f15914 = load i64, i64* %i0ptr15912, align 8                                      ; load; *i0ptr15912
  %fptr15913 = inttoptr i64 %f15914 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15913(i64 %arg10685, i64 %rva12061)                 ; tail call
  ret void
}


define void @lam12565(i64 %env12566, i64 %rvp12060) {
  %envptr15915 = inttoptr i64 %env12566 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15916 = getelementptr inbounds i64, i64* %envptr15915, i64 7                ; &envptr15915[7]
  %Z69$f = load i64, i64* %envptr15916, align 8                                      ; load; *envptr15916
  %envptr15917 = inttoptr i64 %env12566 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15918 = getelementptr inbounds i64, i64* %envptr15917, i64 6                ; &envptr15917[6]
  %onO$args = load i64, i64* %envptr15918, align 8                                   ; load; *envptr15918
  %envptr15919 = inttoptr i64 %env12566 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15920 = getelementptr inbounds i64, i64* %envptr15919, i64 5                ; &envptr15919[5]
  %gT1$_37foldl = load i64, i64* %envptr15920, align 8                               ; load; *envptr15920
  %envptr15921 = inttoptr i64 %env12566 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15922 = getelementptr inbounds i64, i64* %envptr15921, i64 4                ; &envptr15921[4]
  %ujo$_37map1 = load i64, i64* %envptr15922, align 8                                ; load; *envptr15922
  %envptr15923 = inttoptr i64 %env12566 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15924 = getelementptr inbounds i64, i64* %envptr15923, i64 3                ; &envptr15923[3]
  %cont9606 = load i64, i64* %envptr15924, align 8                                   ; load; *envptr15924
  %envptr15925 = inttoptr i64 %env12566 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15926 = getelementptr inbounds i64, i64* %envptr15925, i64 2                ; &envptr15925[2]
  %LWd$_37foldr1 = load i64, i64* %envptr15926, align 8                              ; load; *envptr15926
  %envptr15927 = inttoptr i64 %env12566 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15928 = getelementptr inbounds i64, i64* %envptr15927, i64 1                ; &envptr15927[1]
  %ZKo$_37foldr = load i64, i64* %envptr15928, align 8                               ; load; *envptr15928
  %_959608 = call i64 @prim_car(i64 %rvp12060)                                       ; call prim_car
  %rvp12059 = call i64 @prim_cdr(i64 %rvp12060)                                      ; call prim_cdr
  %B4I$acc = call i64 @prim_car(i64 %rvp12059)                                       ; call prim_car
  %na11977 = call i64 @prim_cdr(i64 %rvp12059)                                       ; call prim_cdr
  %a9265 = call i64 @prim_cdr(i64 %onO$args)                                         ; call prim_cdr
  %retprim9625 = call i64 @prim_cdr(i64 %a9265)                                      ; call prim_cdr
  %cloptr15929 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr15931 = getelementptr inbounds i64, i64* %cloptr15929, i64 1                  ; &eptr15931[1]
  %eptr15932 = getelementptr inbounds i64, i64* %cloptr15929, i64 2                  ; &eptr15932[2]
  %eptr15933 = getelementptr inbounds i64, i64* %cloptr15929, i64 3                  ; &eptr15933[3]
  %eptr15934 = getelementptr inbounds i64, i64* %cloptr15929, i64 4                  ; &eptr15934[4]
  %eptr15935 = getelementptr inbounds i64, i64* %cloptr15929, i64 5                  ; &eptr15935[5]
  %eptr15936 = getelementptr inbounds i64, i64* %cloptr15929, i64 6                  ; &eptr15936[6]
  %eptr15937 = getelementptr inbounds i64, i64* %cloptr15929, i64 7                  ; &eptr15937[7]
  store i64 %ZKo$_37foldr, i64* %eptr15931                                           ; *eptr15931 = %ZKo$_37foldr
  store i64 %LWd$_37foldr1, i64* %eptr15932                                          ; *eptr15932 = %LWd$_37foldr1
  store i64 %B4I$acc, i64* %eptr15933                                                ; *eptr15933 = %B4I$acc
  store i64 %cont9606, i64* %eptr15934                                               ; *eptr15934 = %cont9606
  store i64 %ujo$_37map1, i64* %eptr15935                                            ; *eptr15935 = %ujo$_37map1
  store i64 %gT1$_37foldl, i64* %eptr15936                                           ; *eptr15936 = %gT1$_37foldl
  store i64 %Z69$f, i64* %eptr15937                                                  ; *eptr15937 = %Z69$f
  %eptr15930 = getelementptr inbounds i64, i64* %cloptr15929, i64 0                  ; &cloptr15929[0]
  %f15938 = ptrtoint void(i64,i64)* @lam12563 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15938, i64* %eptr15930                                                 ; store fptr
  %arg10690 = ptrtoint i64* %cloptr15929 to i64                                      ; closure cast; i64* -> i64
  %arg10689 = add i64 0, 0                                                           ; quoted ()
  %rva12058 = add i64 0, 0                                                           ; quoted ()
  %rva12057 = call i64 @prim_cons(i64 %retprim9625, i64 %rva12058)                   ; call prim_cons
  %rva12056 = call i64 @prim_cons(i64 %arg10689, i64 %rva12057)                      ; call prim_cons
  %cloptr15939 = inttoptr i64 %arg10690 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15940 = getelementptr inbounds i64, i64* %cloptr15939, i64 0                 ; &cloptr15939[0]
  %f15942 = load i64, i64* %i0ptr15940, align 8                                      ; load; *i0ptr15940
  %fptr15941 = inttoptr i64 %f15942 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15941(i64 %arg10690, i64 %rva12056)                 ; tail call
  ret void
}


define void @lam12563(i64 %env12564, i64 %rvp12055) {
  %envptr15943 = inttoptr i64 %env12564 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15944 = getelementptr inbounds i64, i64* %envptr15943, i64 7                ; &envptr15943[7]
  %Z69$f = load i64, i64* %envptr15944, align 8                                      ; load; *envptr15944
  %envptr15945 = inttoptr i64 %env12564 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15946 = getelementptr inbounds i64, i64* %envptr15945, i64 6                ; &envptr15945[6]
  %gT1$_37foldl = load i64, i64* %envptr15946, align 8                               ; load; *envptr15946
  %envptr15947 = inttoptr i64 %env12564 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15948 = getelementptr inbounds i64, i64* %envptr15947, i64 5                ; &envptr15947[5]
  %ujo$_37map1 = load i64, i64* %envptr15948, align 8                                ; load; *envptr15948
  %envptr15949 = inttoptr i64 %env12564 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15950 = getelementptr inbounds i64, i64* %envptr15949, i64 4                ; &envptr15949[4]
  %cont9606 = load i64, i64* %envptr15950, align 8                                   ; load; *envptr15950
  %envptr15951 = inttoptr i64 %env12564 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15952 = getelementptr inbounds i64, i64* %envptr15951, i64 3                ; &envptr15951[3]
  %B4I$acc = load i64, i64* %envptr15952, align 8                                    ; load; *envptr15952
  %envptr15953 = inttoptr i64 %env12564 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15954 = getelementptr inbounds i64, i64* %envptr15953, i64 2                ; &envptr15953[2]
  %LWd$_37foldr1 = load i64, i64* %envptr15954, align 8                              ; load; *envptr15954
  %envptr15955 = inttoptr i64 %env12564 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15956 = getelementptr inbounds i64, i64* %envptr15955, i64 1                ; &envptr15955[1]
  %ZKo$_37foldr = load i64, i64* %envptr15956, align 8                               ; load; *envptr15956
  %_959609 = call i64 @prim_car(i64 %rvp12055)                                       ; call prim_car
  %rvp12054 = call i64 @prim_cdr(i64 %rvp12055)                                      ; call prim_cdr
  %l94$lsts = call i64 @prim_car(i64 %rvp12054)                                      ; call prim_car
  %na11979 = call i64 @prim_cdr(i64 %rvp12054)                                       ; call prim_cdr
  %cloptr15957 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr15959 = getelementptr inbounds i64, i64* %cloptr15957, i64 1                  ; &eptr15959[1]
  %eptr15960 = getelementptr inbounds i64, i64* %cloptr15957, i64 2                  ; &eptr15960[2]
  %eptr15961 = getelementptr inbounds i64, i64* %cloptr15957, i64 3                  ; &eptr15961[3]
  %eptr15962 = getelementptr inbounds i64, i64* %cloptr15957, i64 4                  ; &eptr15962[4]
  %eptr15963 = getelementptr inbounds i64, i64* %cloptr15957, i64 5                  ; &eptr15963[5]
  %eptr15964 = getelementptr inbounds i64, i64* %cloptr15957, i64 6                  ; &eptr15964[6]
  %eptr15965 = getelementptr inbounds i64, i64* %cloptr15957, i64 7                  ; &eptr15965[7]
  store i64 %ZKo$_37foldr, i64* %eptr15959                                           ; *eptr15959 = %ZKo$_37foldr
  store i64 %l94$lsts, i64* %eptr15960                                               ; *eptr15960 = %l94$lsts
  store i64 %B4I$acc, i64* %eptr15961                                                ; *eptr15961 = %B4I$acc
  store i64 %cont9606, i64* %eptr15962                                               ; *eptr15962 = %cont9606
  store i64 %ujo$_37map1, i64* %eptr15963                                            ; *eptr15963 = %ujo$_37map1
  store i64 %gT1$_37foldl, i64* %eptr15964                                           ; *eptr15964 = %gT1$_37foldl
  store i64 %Z69$f, i64* %eptr15965                                                  ; *eptr15965 = %Z69$f
  %eptr15958 = getelementptr inbounds i64, i64* %cloptr15957, i64 0                  ; &cloptr15957[0]
  %f15966 = ptrtoint void(i64,i64)* @lam12561 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15966, i64* %eptr15958                                                 ; store fptr
  %arg10694 = ptrtoint i64* %cloptr15957 to i64                                      ; closure cast; i64* -> i64
  %cloptr15967 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr15968 = getelementptr inbounds i64, i64* %cloptr15967, i64 0                  ; &cloptr15967[0]
  %f15969 = ptrtoint void(i64,i64)* @lam12533 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f15969, i64* %eptr15968                                                 ; store fptr
  %arg10693 = ptrtoint i64* %cloptr15967 to i64                                      ; closure cast; i64* -> i64
  %arg10692 = call i64 @const_init_false()                                           ; quoted #f
  %rva12053 = add i64 0, 0                                                           ; quoted ()
  %rva12052 = call i64 @prim_cons(i64 %l94$lsts, i64 %rva12053)                      ; call prim_cons
  %rva12051 = call i64 @prim_cons(i64 %arg10692, i64 %rva12052)                      ; call prim_cons
  %rva12050 = call i64 @prim_cons(i64 %arg10693, i64 %rva12051)                      ; call prim_cons
  %rva12049 = call i64 @prim_cons(i64 %arg10694, i64 %rva12050)                      ; call prim_cons
  %cloptr15970 = inttoptr i64 %LWd$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr15971 = getelementptr inbounds i64, i64* %cloptr15970, i64 0                 ; &cloptr15970[0]
  %f15973 = load i64, i64* %i0ptr15971, align 8                                      ; load; *i0ptr15971
  %fptr15972 = inttoptr i64 %f15973 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15972(i64 %LWd$_37foldr1, i64 %rva12049)            ; tail call
  ret void
}


define void @lam12561(i64 %env12562, i64 %rvp12037) {
  %envptr15974 = inttoptr i64 %env12562 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15975 = getelementptr inbounds i64, i64* %envptr15974, i64 7                ; &envptr15974[7]
  %Z69$f = load i64, i64* %envptr15975, align 8                                      ; load; *envptr15975
  %envptr15976 = inttoptr i64 %env12562 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15977 = getelementptr inbounds i64, i64* %envptr15976, i64 6                ; &envptr15976[6]
  %gT1$_37foldl = load i64, i64* %envptr15977, align 8                               ; load; *envptr15977
  %envptr15978 = inttoptr i64 %env12562 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15979 = getelementptr inbounds i64, i64* %envptr15978, i64 5                ; &envptr15978[5]
  %ujo$_37map1 = load i64, i64* %envptr15979, align 8                                ; load; *envptr15979
  %envptr15980 = inttoptr i64 %env12562 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15981 = getelementptr inbounds i64, i64* %envptr15980, i64 4                ; &envptr15980[4]
  %cont9606 = load i64, i64* %envptr15981, align 8                                   ; load; *envptr15981
  %envptr15982 = inttoptr i64 %env12562 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15983 = getelementptr inbounds i64, i64* %envptr15982, i64 3                ; &envptr15982[3]
  %B4I$acc = load i64, i64* %envptr15983, align 8                                    ; load; *envptr15983
  %envptr15984 = inttoptr i64 %env12562 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15985 = getelementptr inbounds i64, i64* %envptr15984, i64 2                ; &envptr15984[2]
  %l94$lsts = load i64, i64* %envptr15985, align 8                                   ; load; *envptr15985
  %envptr15986 = inttoptr i64 %env12562 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr15987 = getelementptr inbounds i64, i64* %envptr15986, i64 1                ; &envptr15986[1]
  %ZKo$_37foldr = load i64, i64* %envptr15987, align 8                               ; load; *envptr15987
  %_959610 = call i64 @prim_car(i64 %rvp12037)                                       ; call prim_car
  %rvp12036 = call i64 @prim_cdr(i64 %rvp12037)                                      ; call prim_cdr
  %a9266 = call i64 @prim_car(i64 %rvp12036)                                         ; call prim_car
  %na11981 = call i64 @prim_cdr(i64 %rvp12036)                                       ; call prim_cdr
  %cmp15988 = icmp eq i64 %a9266, 15                                                 ; false?
  br i1 %cmp15988, label %else15990, label %then15989                                ; if

then15989:
  %arg10697 = add i64 0, 0                                                           ; quoted ()
  %rva11984 = add i64 0, 0                                                           ; quoted ()
  %rva11983 = call i64 @prim_cons(i64 %B4I$acc, i64 %rva11984)                       ; call prim_cons
  %rva11982 = call i64 @prim_cons(i64 %arg10697, i64 %rva11983)                      ; call prim_cons
  %cloptr15991 = inttoptr i64 %cont9606 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr15992 = getelementptr inbounds i64, i64* %cloptr15991, i64 0                 ; &cloptr15991[0]
  %f15994 = load i64, i64* %i0ptr15992, align 8                                      ; load; *i0ptr15992
  %fptr15993 = inttoptr i64 %f15994 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr15993(i64 %cont9606, i64 %rva11982)                 ; tail call
  ret void

else15990:
  %cloptr15995 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr15997 = getelementptr inbounds i64, i64* %cloptr15995, i64 1                  ; &eptr15997[1]
  %eptr15998 = getelementptr inbounds i64, i64* %cloptr15995, i64 2                  ; &eptr15998[2]
  %eptr15999 = getelementptr inbounds i64, i64* %cloptr15995, i64 3                  ; &eptr15999[3]
  %eptr16000 = getelementptr inbounds i64, i64* %cloptr15995, i64 4                  ; &eptr16000[4]
  %eptr16001 = getelementptr inbounds i64, i64* %cloptr15995, i64 5                  ; &eptr16001[5]
  %eptr16002 = getelementptr inbounds i64, i64* %cloptr15995, i64 6                  ; &eptr16002[6]
  %eptr16003 = getelementptr inbounds i64, i64* %cloptr15995, i64 7                  ; &eptr16003[7]
  store i64 %ZKo$_37foldr, i64* %eptr15997                                           ; *eptr15997 = %ZKo$_37foldr
  store i64 %l94$lsts, i64* %eptr15998                                               ; *eptr15998 = %l94$lsts
  store i64 %B4I$acc, i64* %eptr15999                                                ; *eptr15999 = %B4I$acc
  store i64 %cont9606, i64* %eptr16000                                               ; *eptr16000 = %cont9606
  store i64 %ujo$_37map1, i64* %eptr16001                                            ; *eptr16001 = %ujo$_37map1
  store i64 %gT1$_37foldl, i64* %eptr16002                                           ; *eptr16002 = %gT1$_37foldl
  store i64 %Z69$f, i64* %eptr16003                                                  ; *eptr16003 = %Z69$f
  %eptr15996 = getelementptr inbounds i64, i64* %cloptr15995, i64 0                  ; &cloptr15995[0]
  %f16004 = ptrtoint void(i64,i64)* @lam12559 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16004, i64* %eptr15996                                                 ; store fptr
  %arg10701 = ptrtoint i64* %cloptr15995 to i64                                      ; closure cast; i64* -> i64
  %cloptr16005 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr16006 = getelementptr inbounds i64, i64* %cloptr16005, i64 0                  ; &cloptr16005[0]
  %f16007 = ptrtoint void(i64,i64)* @lam12540 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16007, i64* %eptr16006                                                 ; store fptr
  %arg10700 = ptrtoint i64* %cloptr16005 to i64                                      ; closure cast; i64* -> i64
  %rva12035 = add i64 0, 0                                                           ; quoted ()
  %rva12034 = call i64 @prim_cons(i64 %l94$lsts, i64 %rva12035)                      ; call prim_cons
  %rva12033 = call i64 @prim_cons(i64 %arg10700, i64 %rva12034)                      ; call prim_cons
  %rva12032 = call i64 @prim_cons(i64 %arg10701, i64 %rva12033)                      ; call prim_cons
  %cloptr16008 = inttoptr i64 %ujo$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr16009 = getelementptr inbounds i64, i64* %cloptr16008, i64 0                 ; &cloptr16008[0]
  %f16011 = load i64, i64* %i0ptr16009, align 8                                      ; load; *i0ptr16009
  %fptr16010 = inttoptr i64 %f16011 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16010(i64 %ujo$_37map1, i64 %rva12032)              ; tail call
  ret void
}


define void @lam12559(i64 %env12560, i64 %rvp12024) {
  %envptr16012 = inttoptr i64 %env12560 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16013 = getelementptr inbounds i64, i64* %envptr16012, i64 7                ; &envptr16012[7]
  %Z69$f = load i64, i64* %envptr16013, align 8                                      ; load; *envptr16013
  %envptr16014 = inttoptr i64 %env12560 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16015 = getelementptr inbounds i64, i64* %envptr16014, i64 6                ; &envptr16014[6]
  %gT1$_37foldl = load i64, i64* %envptr16015, align 8                               ; load; *envptr16015
  %envptr16016 = inttoptr i64 %env12560 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16017 = getelementptr inbounds i64, i64* %envptr16016, i64 5                ; &envptr16016[5]
  %ujo$_37map1 = load i64, i64* %envptr16017, align 8                                ; load; *envptr16017
  %envptr16018 = inttoptr i64 %env12560 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16019 = getelementptr inbounds i64, i64* %envptr16018, i64 4                ; &envptr16018[4]
  %cont9606 = load i64, i64* %envptr16019, align 8                                   ; load; *envptr16019
  %envptr16020 = inttoptr i64 %env12560 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16021 = getelementptr inbounds i64, i64* %envptr16020, i64 3                ; &envptr16020[3]
  %B4I$acc = load i64, i64* %envptr16021, align 8                                    ; load; *envptr16021
  %envptr16022 = inttoptr i64 %env12560 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16023 = getelementptr inbounds i64, i64* %envptr16022, i64 2                ; &envptr16022[2]
  %l94$lsts = load i64, i64* %envptr16023, align 8                                   ; load; *envptr16023
  %envptr16024 = inttoptr i64 %env12560 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16025 = getelementptr inbounds i64, i64* %envptr16024, i64 1                ; &envptr16024[1]
  %ZKo$_37foldr = load i64, i64* %envptr16025, align 8                               ; load; *envptr16025
  %_959611 = call i64 @prim_car(i64 %rvp12024)                                       ; call prim_car
  %rvp12023 = call i64 @prim_cdr(i64 %rvp12024)                                      ; call prim_cdr
  %eYn$lsts_43 = call i64 @prim_car(i64 %rvp12023)                                   ; call prim_car
  %na11986 = call i64 @prim_cdr(i64 %rvp12023)                                       ; call prim_cdr
  %cloptr16026 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr16028 = getelementptr inbounds i64, i64* %cloptr16026, i64 1                  ; &eptr16028[1]
  %eptr16029 = getelementptr inbounds i64, i64* %cloptr16026, i64 2                  ; &eptr16029[2]
  %eptr16030 = getelementptr inbounds i64, i64* %cloptr16026, i64 3                  ; &eptr16030[3]
  %eptr16031 = getelementptr inbounds i64, i64* %cloptr16026, i64 4                  ; &eptr16031[4]
  %eptr16032 = getelementptr inbounds i64, i64* %cloptr16026, i64 5                  ; &eptr16032[5]
  %eptr16033 = getelementptr inbounds i64, i64* %cloptr16026, i64 6                  ; &eptr16033[6]
  store i64 %ZKo$_37foldr, i64* %eptr16028                                           ; *eptr16028 = %ZKo$_37foldr
  store i64 %eYn$lsts_43, i64* %eptr16029                                            ; *eptr16029 = %eYn$lsts_43
  store i64 %B4I$acc, i64* %eptr16030                                                ; *eptr16030 = %B4I$acc
  store i64 %cont9606, i64* %eptr16031                                               ; *eptr16031 = %cont9606
  store i64 %gT1$_37foldl, i64* %eptr16032                                           ; *eptr16032 = %gT1$_37foldl
  store i64 %Z69$f, i64* %eptr16033                                                  ; *eptr16033 = %Z69$f
  %eptr16027 = getelementptr inbounds i64, i64* %cloptr16026, i64 0                  ; &cloptr16026[0]
  %f16034 = ptrtoint void(i64,i64)* @lam12557 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16034, i64* %eptr16027                                                 ; store fptr
  %arg10705 = ptrtoint i64* %cloptr16026 to i64                                      ; closure cast; i64* -> i64
  %cloptr16035 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr16036 = getelementptr inbounds i64, i64* %cloptr16035, i64 0                  ; &cloptr16035[0]
  %f16037 = ptrtoint void(i64,i64)* @lam12545 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16037, i64* %eptr16036                                                 ; store fptr
  %arg10704 = ptrtoint i64* %cloptr16035 to i64                                      ; closure cast; i64* -> i64
  %rva12022 = add i64 0, 0                                                           ; quoted ()
  %rva12021 = call i64 @prim_cons(i64 %l94$lsts, i64 %rva12022)                      ; call prim_cons
  %rva12020 = call i64 @prim_cons(i64 %arg10704, i64 %rva12021)                      ; call prim_cons
  %rva12019 = call i64 @prim_cons(i64 %arg10705, i64 %rva12020)                      ; call prim_cons
  %cloptr16038 = inttoptr i64 %ujo$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr16039 = getelementptr inbounds i64, i64* %cloptr16038, i64 0                 ; &cloptr16038[0]
  %f16041 = load i64, i64* %i0ptr16039, align 8                                      ; load; *i0ptr16039
  %fptr16040 = inttoptr i64 %f16041 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16040(i64 %ujo$_37map1, i64 %rva12019)              ; tail call
  ret void
}


define void @lam12557(i64 %env12558, i64 %rvp12011) {
  %envptr16042 = inttoptr i64 %env12558 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16043 = getelementptr inbounds i64, i64* %envptr16042, i64 6                ; &envptr16042[6]
  %Z69$f = load i64, i64* %envptr16043, align 8                                      ; load; *envptr16043
  %envptr16044 = inttoptr i64 %env12558 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16045 = getelementptr inbounds i64, i64* %envptr16044, i64 5                ; &envptr16044[5]
  %gT1$_37foldl = load i64, i64* %envptr16045, align 8                               ; load; *envptr16045
  %envptr16046 = inttoptr i64 %env12558 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16047 = getelementptr inbounds i64, i64* %envptr16046, i64 4                ; &envptr16046[4]
  %cont9606 = load i64, i64* %envptr16047, align 8                                   ; load; *envptr16047
  %envptr16048 = inttoptr i64 %env12558 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16049 = getelementptr inbounds i64, i64* %envptr16048, i64 3                ; &envptr16048[3]
  %B4I$acc = load i64, i64* %envptr16049, align 8                                    ; load; *envptr16049
  %envptr16050 = inttoptr i64 %env12558 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16051 = getelementptr inbounds i64, i64* %envptr16050, i64 2                ; &envptr16050[2]
  %eYn$lsts_43 = load i64, i64* %envptr16051, align 8                                ; load; *envptr16051
  %envptr16052 = inttoptr i64 %env12558 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16053 = getelementptr inbounds i64, i64* %envptr16052, i64 1                ; &envptr16052[1]
  %ZKo$_37foldr = load i64, i64* %envptr16053, align 8                               ; load; *envptr16053
  %_959612 = call i64 @prim_car(i64 %rvp12011)                                       ; call prim_car
  %rvp12010 = call i64 @prim_cdr(i64 %rvp12011)                                      ; call prim_cdr
  %GIa$vs = call i64 @prim_car(i64 %rvp12010)                                        ; call prim_car
  %na11988 = call i64 @prim_cdr(i64 %rvp12010)                                       ; call prim_cdr
  %arg10707 = add i64 0, 0                                                           ; quoted ()
  %a9267 = call i64 @prim_cons(i64 %B4I$acc, i64 %arg10707)                          ; call prim_cons
  %cloptr16054 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr16056 = getelementptr inbounds i64, i64* %cloptr16054, i64 1                  ; &eptr16056[1]
  %eptr16057 = getelementptr inbounds i64, i64* %cloptr16054, i64 2                  ; &eptr16057[2]
  %eptr16058 = getelementptr inbounds i64, i64* %cloptr16054, i64 3                  ; &eptr16058[3]
  %eptr16059 = getelementptr inbounds i64, i64* %cloptr16054, i64 4                  ; &eptr16059[4]
  store i64 %eYn$lsts_43, i64* %eptr16056                                            ; *eptr16056 = %eYn$lsts_43
  store i64 %cont9606, i64* %eptr16057                                               ; *eptr16057 = %cont9606
  store i64 %gT1$_37foldl, i64* %eptr16058                                           ; *eptr16058 = %gT1$_37foldl
  store i64 %Z69$f, i64* %eptr16059                                                  ; *eptr16059 = %Z69$f
  %eptr16055 = getelementptr inbounds i64, i64* %cloptr16054, i64 0                  ; &cloptr16054[0]
  %f16060 = ptrtoint void(i64,i64)* @lam12554 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16060, i64* %eptr16055                                                 ; store fptr
  %arg10712 = ptrtoint i64* %cloptr16054 to i64                                      ; closure cast; i64* -> i64
  %cloptr16061 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr16062 = getelementptr inbounds i64, i64* %cloptr16061, i64 0                  ; &cloptr16061[0]
  %f16063 = ptrtoint void(i64,i64)* @lam12550 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16063, i64* %eptr16062                                                 ; store fptr
  %arg10711 = ptrtoint i64* %cloptr16061 to i64                                      ; closure cast; i64* -> i64
  %rva12009 = add i64 0, 0                                                           ; quoted ()
  %rva12008 = call i64 @prim_cons(i64 %GIa$vs, i64 %rva12009)                        ; call prim_cons
  %rva12007 = call i64 @prim_cons(i64 %a9267, i64 %rva12008)                         ; call prim_cons
  %rva12006 = call i64 @prim_cons(i64 %arg10711, i64 %rva12007)                      ; call prim_cons
  %rva12005 = call i64 @prim_cons(i64 %arg10712, i64 %rva12006)                      ; call prim_cons
  %cloptr16064 = inttoptr i64 %ZKo$_37foldr to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr16065 = getelementptr inbounds i64, i64* %cloptr16064, i64 0                 ; &cloptr16064[0]
  %f16067 = load i64, i64* %i0ptr16065, align 8                                      ; load; *i0ptr16065
  %fptr16066 = inttoptr i64 %f16067 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16066(i64 %ZKo$_37foldr, i64 %rva12005)             ; tail call
  ret void
}


define void @lam12554(i64 %env12555, i64 %rvp11996) {
  %envptr16068 = inttoptr i64 %env12555 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16069 = getelementptr inbounds i64, i64* %envptr16068, i64 4                ; &envptr16068[4]
  %Z69$f = load i64, i64* %envptr16069, align 8                                      ; load; *envptr16069
  %envptr16070 = inttoptr i64 %env12555 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16071 = getelementptr inbounds i64, i64* %envptr16070, i64 3                ; &envptr16070[3]
  %gT1$_37foldl = load i64, i64* %envptr16071, align 8                               ; load; *envptr16071
  %envptr16072 = inttoptr i64 %env12555 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16073 = getelementptr inbounds i64, i64* %envptr16072, i64 2                ; &envptr16072[2]
  %cont9606 = load i64, i64* %envptr16073, align 8                                   ; load; *envptr16073
  %envptr16074 = inttoptr i64 %env12555 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16075 = getelementptr inbounds i64, i64* %envptr16074, i64 1                ; &envptr16074[1]
  %eYn$lsts_43 = load i64, i64* %envptr16075, align 8                                ; load; *envptr16075
  %_959615 = call i64 @prim_car(i64 %rvp11996)                                       ; call prim_car
  %rvp11995 = call i64 @prim_cdr(i64 %rvp11996)                                      ; call prim_cdr
  %a9268 = call i64 @prim_car(i64 %rvp11995)                                         ; call prim_car
  %na11990 = call i64 @prim_cdr(i64 %rvp11995)                                       ; call prim_cdr
  %cloptr16076 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr16078 = getelementptr inbounds i64, i64* %cloptr16076, i64 1                  ; &eptr16078[1]
  %eptr16079 = getelementptr inbounds i64, i64* %cloptr16076, i64 2                  ; &eptr16079[2]
  %eptr16080 = getelementptr inbounds i64, i64* %cloptr16076, i64 3                  ; &eptr16080[3]
  %eptr16081 = getelementptr inbounds i64, i64* %cloptr16076, i64 4                  ; &eptr16081[4]
  store i64 %eYn$lsts_43, i64* %eptr16078                                            ; *eptr16078 = %eYn$lsts_43
  store i64 %cont9606, i64* %eptr16079                                               ; *eptr16079 = %cont9606
  store i64 %gT1$_37foldl, i64* %eptr16080                                           ; *eptr16080 = %gT1$_37foldl
  store i64 %Z69$f, i64* %eptr16081                                                  ; *eptr16081 = %Z69$f
  %eptr16077 = getelementptr inbounds i64, i64* %cloptr16076, i64 0                  ; &cloptr16076[0]
  %f16082 = ptrtoint void(i64,i64)* @lam12552 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16082, i64* %eptr16077                                                 ; store fptr
  %arg10715 = ptrtoint i64* %cloptr16076 to i64                                      ; closure cast; i64* -> i64
  %cps_45lst9616 = call i64 @prim_cons(i64 %arg10715, i64 %a9268)                    ; call prim_cons
  %cloptr16083 = inttoptr i64 %Z69$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr16084 = getelementptr inbounds i64, i64* %cloptr16083, i64 0                 ; &cloptr16083[0]
  %f16086 = load i64, i64* %i0ptr16084, align 8                                      ; load; *i0ptr16084
  %fptr16085 = inttoptr i64 %f16086 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16085(i64 %Z69$f, i64 %cps_45lst9616)               ; tail call
  ret void
}


define void @lam12552(i64 %env12553, i64 %rvp11994) {
  %envptr16087 = inttoptr i64 %env12553 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16088 = getelementptr inbounds i64, i64* %envptr16087, i64 4                ; &envptr16087[4]
  %Z69$f = load i64, i64* %envptr16088, align 8                                      ; load; *envptr16088
  %envptr16089 = inttoptr i64 %env12553 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16090 = getelementptr inbounds i64, i64* %envptr16089, i64 3                ; &envptr16089[3]
  %gT1$_37foldl = load i64, i64* %envptr16090, align 8                               ; load; *envptr16090
  %envptr16091 = inttoptr i64 %env12553 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16092 = getelementptr inbounds i64, i64* %envptr16091, i64 2                ; &envptr16091[2]
  %cont9606 = load i64, i64* %envptr16092, align 8                                   ; load; *envptr16092
  %envptr16093 = inttoptr i64 %env12553 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16094 = getelementptr inbounds i64, i64* %envptr16093, i64 1                ; &envptr16093[1]
  %eYn$lsts_43 = load i64, i64* %envptr16094, align 8                                ; load; *envptr16094
  %_959613 = call i64 @prim_car(i64 %rvp11994)                                       ; call prim_car
  %rvp11993 = call i64 @prim_cdr(i64 %rvp11994)                                      ; call prim_cdr
  %psl$acc_43 = call i64 @prim_car(i64 %rvp11993)                                    ; call prim_car
  %na11992 = call i64 @prim_cdr(i64 %rvp11993)                                       ; call prim_cdr
  %a9269 = call i64 @prim_cons(i64 %psl$acc_43, i64 %eYn$lsts_43)                    ; call prim_cons
  %a9270 = call i64 @prim_cons(i64 %Z69$f, i64 %a9269)                               ; call prim_cons
  %cps_45lst9614 = call i64 @prim_cons(i64 %cont9606, i64 %a9270)                    ; call prim_cons
  %cloptr16095 = inttoptr i64 %gT1$_37foldl to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr16096 = getelementptr inbounds i64, i64* %cloptr16095, i64 0                 ; &cloptr16095[0]
  %f16098 = load i64, i64* %i0ptr16096, align 8                                      ; load; *i0ptr16096
  %fptr16097 = inttoptr i64 %f16098 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16097(i64 %gT1$_37foldl, i64 %cps_45lst9614)        ; tail call
  ret void
}


define void @lam12550(i64 %env12551, i64 %rvp12004) {
  %cont9617 = call i64 @prim_car(i64 %rvp12004)                                      ; call prim_car
  %rvp12003 = call i64 @prim_cdr(i64 %rvp12004)                                      ; call prim_cdr
  %wwM$a = call i64 @prim_car(i64 %rvp12003)                                         ; call prim_car
  %rvp12002 = call i64 @prim_cdr(i64 %rvp12003)                                      ; call prim_cdr
  %OJN$b = call i64 @prim_car(i64 %rvp12002)                                         ; call prim_car
  %na11998 = call i64 @prim_cdr(i64 %rvp12002)                                       ; call prim_cdr
  %retprim9618 = call i64 @prim_cons(i64 %wwM$a, i64 %OJN$b)                         ; call prim_cons
  %arg10725 = add i64 0, 0                                                           ; quoted ()
  %rva12001 = add i64 0, 0                                                           ; quoted ()
  %rva12000 = call i64 @prim_cons(i64 %retprim9618, i64 %rva12001)                   ; call prim_cons
  %rva11999 = call i64 @prim_cons(i64 %arg10725, i64 %rva12000)                      ; call prim_cons
  %cloptr16099 = inttoptr i64 %cont9617 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16100 = getelementptr inbounds i64, i64* %cloptr16099, i64 0                 ; &cloptr16099[0]
  %f16102 = load i64, i64* %i0ptr16100, align 8                                      ; load; *i0ptr16100
  %fptr16101 = inttoptr i64 %f16102 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16101(i64 %cont9617, i64 %rva11999)                 ; tail call
  ret void
}


define void @lam12545(i64 %env12546, i64 %rvp12018) {
  %cont9619 = call i64 @prim_car(i64 %rvp12018)                                      ; call prim_car
  %rvp12017 = call i64 @prim_cdr(i64 %rvp12018)                                      ; call prim_cdr
  %pfc$x = call i64 @prim_car(i64 %rvp12017)                                         ; call prim_car
  %na12013 = call i64 @prim_cdr(i64 %rvp12017)                                       ; call prim_cdr
  %retprim9620 = call i64 @prim_car(i64 %pfc$x)                                      ; call prim_car
  %arg10729 = add i64 0, 0                                                           ; quoted ()
  %rva12016 = add i64 0, 0                                                           ; quoted ()
  %rva12015 = call i64 @prim_cons(i64 %retprim9620, i64 %rva12016)                   ; call prim_cons
  %rva12014 = call i64 @prim_cons(i64 %arg10729, i64 %rva12015)                      ; call prim_cons
  %cloptr16103 = inttoptr i64 %cont9619 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16104 = getelementptr inbounds i64, i64* %cloptr16103, i64 0                 ; &cloptr16103[0]
  %f16106 = load i64, i64* %i0ptr16104, align 8                                      ; load; *i0ptr16104
  %fptr16105 = inttoptr i64 %f16106 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16105(i64 %cont9619, i64 %rva12014)                 ; tail call
  ret void
}


define void @lam12540(i64 %env12541, i64 %rvp12031) {
  %cont9621 = call i64 @prim_car(i64 %rvp12031)                                      ; call prim_car
  %rvp12030 = call i64 @prim_cdr(i64 %rvp12031)                                      ; call prim_cdr
  %hbW$x = call i64 @prim_car(i64 %rvp12030)                                         ; call prim_car
  %na12026 = call i64 @prim_cdr(i64 %rvp12030)                                       ; call prim_cdr
  %retprim9622 = call i64 @prim_cdr(i64 %hbW$x)                                      ; call prim_cdr
  %arg10733 = add i64 0, 0                                                           ; quoted ()
  %rva12029 = add i64 0, 0                                                           ; quoted ()
  %rva12028 = call i64 @prim_cons(i64 %retprim9622, i64 %rva12029)                   ; call prim_cons
  %rva12027 = call i64 @prim_cons(i64 %arg10733, i64 %rva12028)                      ; call prim_cons
  %cloptr16107 = inttoptr i64 %cont9621 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16108 = getelementptr inbounds i64, i64* %cloptr16107, i64 0                 ; &cloptr16107[0]
  %f16110 = load i64, i64* %i0ptr16108, align 8                                      ; load; *i0ptr16108
  %fptr16109 = inttoptr i64 %f16110 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16109(i64 %cont9621, i64 %rva12027)                 ; tail call
  ret void
}


define void @lam12533(i64 %env12534, i64 %rvp12048) {
  %cont9623 = call i64 @prim_car(i64 %rvp12048)                                      ; call prim_car
  %rvp12047 = call i64 @prim_cdr(i64 %rvp12048)                                      ; call prim_cdr
  %mPv$lst = call i64 @prim_car(i64 %rvp12047)                                       ; call prim_car
  %rvp12046 = call i64 @prim_cdr(i64 %rvp12047)                                      ; call prim_cdr
  %iAW$b = call i64 @prim_car(i64 %rvp12046)                                         ; call prim_car
  %na12039 = call i64 @prim_cdr(i64 %rvp12046)                                       ; call prim_cdr
  %cmp16111 = icmp eq i64 %iAW$b, 15                                                 ; false?
  br i1 %cmp16111, label %else16113, label %then16112                                ; if

then16112:
  %arg10736 = add i64 0, 0                                                           ; quoted ()
  %rva12042 = add i64 0, 0                                                           ; quoted ()
  %rva12041 = call i64 @prim_cons(i64 %iAW$b, i64 %rva12042)                         ; call prim_cons
  %rva12040 = call i64 @prim_cons(i64 %arg10736, i64 %rva12041)                      ; call prim_cons
  %cloptr16114 = inttoptr i64 %cont9623 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16115 = getelementptr inbounds i64, i64* %cloptr16114, i64 0                 ; &cloptr16114[0]
  %f16117 = load i64, i64* %i0ptr16115, align 8                                      ; load; *i0ptr16115
  %fptr16116 = inttoptr i64 %f16117 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16116(i64 %cont9623, i64 %rva12040)                 ; tail call
  ret void

else16113:
  %retprim9624 = call i64 @prim_null_63(i64 %mPv$lst)                                ; call prim_null_63
  %arg10740 = add i64 0, 0                                                           ; quoted ()
  %rva12045 = add i64 0, 0                                                           ; quoted ()
  %rva12044 = call i64 @prim_cons(i64 %retprim9624, i64 %rva12045)                   ; call prim_cons
  %rva12043 = call i64 @prim_cons(i64 %arg10740, i64 %rva12044)                      ; call prim_cons
  %cloptr16118 = inttoptr i64 %cont9623 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16119 = getelementptr inbounds i64, i64* %cloptr16118, i64 0                 ; &cloptr16118[0]
  %f16121 = load i64, i64* %i0ptr16119, align 8                                      ; load; *i0ptr16119
  %fptr16120 = inttoptr i64 %f16121 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16120(i64 %cont9623, i64 %rva12043)                 ; tail call
  ret void
}


define void @lam12519(i64 %env12520, i64 %rvp12168) {
  %envptr16122 = inttoptr i64 %env12520 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16123 = getelementptr inbounds i64, i64* %envptr16122, i64 2                ; &envptr16122[2]
  %SVw$_37map1 = load i64, i64* %envptr16123, align 8                                ; load; *envptr16123
  %envptr16124 = inttoptr i64 %env12520 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16125 = getelementptr inbounds i64, i64* %envptr16124, i64 1                ; &envptr16124[1]
  %LWd$_37foldr1 = load i64, i64* %envptr16125, align 8                              ; load; *envptr16125
  %cont9627 = call i64 @prim_car(i64 %rvp12168)                                      ; call prim_car
  %rvp12167 = call i64 @prim_cdr(i64 %rvp12168)                                      ; call prim_cdr
  %b4r$_37foldr = call i64 @prim_car(i64 %rvp12167)                                  ; call prim_car
  %na12075 = call i64 @prim_cdr(i64 %rvp12167)                                       ; call prim_cdr
  %arg10743 = add i64 0, 0                                                           ; quoted ()
  %cloptr16126 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr16128 = getelementptr inbounds i64, i64* %cloptr16126, i64 1                  ; &eptr16128[1]
  %eptr16129 = getelementptr inbounds i64, i64* %cloptr16126, i64 2                  ; &eptr16129[2]
  %eptr16130 = getelementptr inbounds i64, i64* %cloptr16126, i64 3                  ; &eptr16130[3]
  store i64 %LWd$_37foldr1, i64* %eptr16128                                          ; *eptr16128 = %LWd$_37foldr1
  store i64 %SVw$_37map1, i64* %eptr16129                                            ; *eptr16129 = %SVw$_37map1
  store i64 %b4r$_37foldr, i64* %eptr16130                                           ; *eptr16130 = %b4r$_37foldr
  %eptr16127 = getelementptr inbounds i64, i64* %cloptr16126, i64 0                  ; &cloptr16126[0]
  %f16131 = ptrtoint void(i64,i64)* @lam12516 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16131, i64* %eptr16127                                                 ; store fptr
  %arg10742 = ptrtoint i64* %cloptr16126 to i64                                      ; closure cast; i64* -> i64
  %rva12166 = add i64 0, 0                                                           ; quoted ()
  %rva12165 = call i64 @prim_cons(i64 %arg10742, i64 %rva12166)                      ; call prim_cons
  %rva12164 = call i64 @prim_cons(i64 %arg10743, i64 %rva12165)                      ; call prim_cons
  %cloptr16132 = inttoptr i64 %cont9627 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16133 = getelementptr inbounds i64, i64* %cloptr16132, i64 0                 ; &cloptr16132[0]
  %f16135 = load i64, i64* %i0ptr16133, align 8                                      ; load; *i0ptr16133
  %fptr16134 = inttoptr i64 %f16135 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16134(i64 %cont9627, i64 %rva12164)                 ; tail call
  ret void
}


define void @lam12516(i64 %env12517, i64 %pJ3$args9629) {
  %envptr16136 = inttoptr i64 %env12517 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16137 = getelementptr inbounds i64, i64* %envptr16136, i64 3                ; &envptr16136[3]
  %b4r$_37foldr = load i64, i64* %envptr16137, align 8                               ; load; *envptr16137
  %envptr16138 = inttoptr i64 %env12517 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16139 = getelementptr inbounds i64, i64* %envptr16138, i64 2                ; &envptr16138[2]
  %SVw$_37map1 = load i64, i64* %envptr16139, align 8                                ; load; *envptr16139
  %envptr16140 = inttoptr i64 %env12517 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16141 = getelementptr inbounds i64, i64* %envptr16140, i64 1                ; &envptr16140[1]
  %LWd$_37foldr1 = load i64, i64* %envptr16141, align 8                              ; load; *envptr16141
  %cont9628 = call i64 @prim_car(i64 %pJ3$args9629)                                  ; call prim_car
  %pJ3$args = call i64 @prim_cdr(i64 %pJ3$args9629)                                  ; call prim_cdr
  %m3U$f = call i64 @prim_car(i64 %pJ3$args)                                         ; call prim_car
  %a9250 = call i64 @prim_cdr(i64 %pJ3$args)                                         ; call prim_cdr
  %retprim9648 = call i64 @prim_car(i64 %a9250)                                      ; call prim_car
  %cloptr16142 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr16144 = getelementptr inbounds i64, i64* %cloptr16142, i64 1                  ; &eptr16144[1]
  %eptr16145 = getelementptr inbounds i64, i64* %cloptr16142, i64 2                  ; &eptr16145[2]
  %eptr16146 = getelementptr inbounds i64, i64* %cloptr16142, i64 3                  ; &eptr16146[3]
  %eptr16147 = getelementptr inbounds i64, i64* %cloptr16142, i64 4                  ; &eptr16147[4]
  %eptr16148 = getelementptr inbounds i64, i64* %cloptr16142, i64 5                  ; &eptr16148[5]
  %eptr16149 = getelementptr inbounds i64, i64* %cloptr16142, i64 6                  ; &eptr16149[6]
  store i64 %LWd$_37foldr1, i64* %eptr16144                                          ; *eptr16144 = %LWd$_37foldr1
  store i64 %pJ3$args, i64* %eptr16145                                               ; *eptr16145 = %pJ3$args
  store i64 %SVw$_37map1, i64* %eptr16146                                            ; *eptr16146 = %SVw$_37map1
  store i64 %b4r$_37foldr, i64* %eptr16147                                           ; *eptr16147 = %b4r$_37foldr
  store i64 %cont9628, i64* %eptr16148                                               ; *eptr16148 = %cont9628
  store i64 %m3U$f, i64* %eptr16149                                                  ; *eptr16149 = %m3U$f
  %eptr16143 = getelementptr inbounds i64, i64* %cloptr16142, i64 0                  ; &cloptr16142[0]
  %f16150 = ptrtoint void(i64,i64)* @lam12514 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16150, i64* %eptr16143                                                 ; store fptr
  %arg10752 = ptrtoint i64* %cloptr16142 to i64                                      ; closure cast; i64* -> i64
  %arg10751 = add i64 0, 0                                                           ; quoted ()
  %rva12163 = add i64 0, 0                                                           ; quoted ()
  %rva12162 = call i64 @prim_cons(i64 %retprim9648, i64 %rva12163)                   ; call prim_cons
  %rva12161 = call i64 @prim_cons(i64 %arg10751, i64 %rva12162)                      ; call prim_cons
  %cloptr16151 = inttoptr i64 %arg10752 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16152 = getelementptr inbounds i64, i64* %cloptr16151, i64 0                 ; &cloptr16151[0]
  %f16154 = load i64, i64* %i0ptr16152, align 8                                      ; load; *i0ptr16152
  %fptr16153 = inttoptr i64 %f16154 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16153(i64 %arg10752, i64 %rva12161)                 ; tail call
  ret void
}


define void @lam12514(i64 %env12515, i64 %rvp12160) {
  %envptr16155 = inttoptr i64 %env12515 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16156 = getelementptr inbounds i64, i64* %envptr16155, i64 6                ; &envptr16155[6]
  %m3U$f = load i64, i64* %envptr16156, align 8                                      ; load; *envptr16156
  %envptr16157 = inttoptr i64 %env12515 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16158 = getelementptr inbounds i64, i64* %envptr16157, i64 5                ; &envptr16157[5]
  %cont9628 = load i64, i64* %envptr16158, align 8                                   ; load; *envptr16158
  %envptr16159 = inttoptr i64 %env12515 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16160 = getelementptr inbounds i64, i64* %envptr16159, i64 4                ; &envptr16159[4]
  %b4r$_37foldr = load i64, i64* %envptr16160, align 8                               ; load; *envptr16160
  %envptr16161 = inttoptr i64 %env12515 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16162 = getelementptr inbounds i64, i64* %envptr16161, i64 3                ; &envptr16161[3]
  %SVw$_37map1 = load i64, i64* %envptr16162, align 8                                ; load; *envptr16162
  %envptr16163 = inttoptr i64 %env12515 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16164 = getelementptr inbounds i64, i64* %envptr16163, i64 2                ; &envptr16163[2]
  %pJ3$args = load i64, i64* %envptr16164, align 8                                   ; load; *envptr16164
  %envptr16165 = inttoptr i64 %env12515 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16166 = getelementptr inbounds i64, i64* %envptr16165, i64 1                ; &envptr16165[1]
  %LWd$_37foldr1 = load i64, i64* %envptr16166, align 8                              ; load; *envptr16166
  %_959630 = call i64 @prim_car(i64 %rvp12160)                                       ; call prim_car
  %rvp12159 = call i64 @prim_cdr(i64 %rvp12160)                                      ; call prim_cdr
  %VAu$acc = call i64 @prim_car(i64 %rvp12159)                                       ; call prim_car
  %na12077 = call i64 @prim_cdr(i64 %rvp12159)                                       ; call prim_cdr
  %a9251 = call i64 @prim_cdr(i64 %pJ3$args)                                         ; call prim_cdr
  %retprim9647 = call i64 @prim_cdr(i64 %a9251)                                      ; call prim_cdr
  %cloptr16167 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr16169 = getelementptr inbounds i64, i64* %cloptr16167, i64 1                  ; &eptr16169[1]
  %eptr16170 = getelementptr inbounds i64, i64* %cloptr16167, i64 2                  ; &eptr16170[2]
  %eptr16171 = getelementptr inbounds i64, i64* %cloptr16167, i64 3                  ; &eptr16171[3]
  %eptr16172 = getelementptr inbounds i64, i64* %cloptr16167, i64 4                  ; &eptr16172[4]
  %eptr16173 = getelementptr inbounds i64, i64* %cloptr16167, i64 5                  ; &eptr16173[5]
  %eptr16174 = getelementptr inbounds i64, i64* %cloptr16167, i64 6                  ; &eptr16174[6]
  store i64 %LWd$_37foldr1, i64* %eptr16169                                          ; *eptr16169 = %LWd$_37foldr1
  store i64 %SVw$_37map1, i64* %eptr16170                                            ; *eptr16170 = %SVw$_37map1
  store i64 %VAu$acc, i64* %eptr16171                                                ; *eptr16171 = %VAu$acc
  store i64 %b4r$_37foldr, i64* %eptr16172                                           ; *eptr16172 = %b4r$_37foldr
  store i64 %cont9628, i64* %eptr16173                                               ; *eptr16173 = %cont9628
  store i64 %m3U$f, i64* %eptr16174                                                  ; *eptr16174 = %m3U$f
  %eptr16168 = getelementptr inbounds i64, i64* %cloptr16167, i64 0                  ; &cloptr16167[0]
  %f16175 = ptrtoint void(i64,i64)* @lam12512 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16175, i64* %eptr16168                                                 ; store fptr
  %arg10757 = ptrtoint i64* %cloptr16167 to i64                                      ; closure cast; i64* -> i64
  %arg10756 = add i64 0, 0                                                           ; quoted ()
  %rva12158 = add i64 0, 0                                                           ; quoted ()
  %rva12157 = call i64 @prim_cons(i64 %retprim9647, i64 %rva12158)                   ; call prim_cons
  %rva12156 = call i64 @prim_cons(i64 %arg10756, i64 %rva12157)                      ; call prim_cons
  %cloptr16176 = inttoptr i64 %arg10757 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16177 = getelementptr inbounds i64, i64* %cloptr16176, i64 0                 ; &cloptr16176[0]
  %f16179 = load i64, i64* %i0ptr16177, align 8                                      ; load; *i0ptr16177
  %fptr16178 = inttoptr i64 %f16179 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16178(i64 %arg10757, i64 %rva12156)                 ; tail call
  ret void
}


define void @lam12512(i64 %env12513, i64 %rvp12155) {
  %envptr16180 = inttoptr i64 %env12513 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16181 = getelementptr inbounds i64, i64* %envptr16180, i64 6                ; &envptr16180[6]
  %m3U$f = load i64, i64* %envptr16181, align 8                                      ; load; *envptr16181
  %envptr16182 = inttoptr i64 %env12513 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16183 = getelementptr inbounds i64, i64* %envptr16182, i64 5                ; &envptr16182[5]
  %cont9628 = load i64, i64* %envptr16183, align 8                                   ; load; *envptr16183
  %envptr16184 = inttoptr i64 %env12513 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16185 = getelementptr inbounds i64, i64* %envptr16184, i64 4                ; &envptr16184[4]
  %b4r$_37foldr = load i64, i64* %envptr16185, align 8                               ; load; *envptr16185
  %envptr16186 = inttoptr i64 %env12513 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16187 = getelementptr inbounds i64, i64* %envptr16186, i64 3                ; &envptr16186[3]
  %VAu$acc = load i64, i64* %envptr16187, align 8                                    ; load; *envptr16187
  %envptr16188 = inttoptr i64 %env12513 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16189 = getelementptr inbounds i64, i64* %envptr16188, i64 2                ; &envptr16188[2]
  %SVw$_37map1 = load i64, i64* %envptr16189, align 8                                ; load; *envptr16189
  %envptr16190 = inttoptr i64 %env12513 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16191 = getelementptr inbounds i64, i64* %envptr16190, i64 1                ; &envptr16190[1]
  %LWd$_37foldr1 = load i64, i64* %envptr16191, align 8                              ; load; *envptr16191
  %_959631 = call i64 @prim_car(i64 %rvp12155)                                       ; call prim_car
  %rvp12154 = call i64 @prim_cdr(i64 %rvp12155)                                      ; call prim_cdr
  %h68$lsts = call i64 @prim_car(i64 %rvp12154)                                      ; call prim_car
  %na12079 = call i64 @prim_cdr(i64 %rvp12154)                                       ; call prim_cdr
  %cloptr16192 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr16194 = getelementptr inbounds i64, i64* %cloptr16192, i64 1                  ; &eptr16194[1]
  %eptr16195 = getelementptr inbounds i64, i64* %cloptr16192, i64 2                  ; &eptr16195[2]
  %eptr16196 = getelementptr inbounds i64, i64* %cloptr16192, i64 3                  ; &eptr16196[3]
  %eptr16197 = getelementptr inbounds i64, i64* %cloptr16192, i64 4                  ; &eptr16197[4]
  %eptr16198 = getelementptr inbounds i64, i64* %cloptr16192, i64 5                  ; &eptr16198[5]
  %eptr16199 = getelementptr inbounds i64, i64* %cloptr16192, i64 6                  ; &eptr16199[6]
  %eptr16200 = getelementptr inbounds i64, i64* %cloptr16192, i64 7                  ; &eptr16200[7]
  store i64 %h68$lsts, i64* %eptr16194                                               ; *eptr16194 = %h68$lsts
  store i64 %LWd$_37foldr1, i64* %eptr16195                                          ; *eptr16195 = %LWd$_37foldr1
  store i64 %SVw$_37map1, i64* %eptr16196                                            ; *eptr16196 = %SVw$_37map1
  store i64 %VAu$acc, i64* %eptr16197                                                ; *eptr16197 = %VAu$acc
  store i64 %b4r$_37foldr, i64* %eptr16198                                           ; *eptr16198 = %b4r$_37foldr
  store i64 %cont9628, i64* %eptr16199                                               ; *eptr16199 = %cont9628
  store i64 %m3U$f, i64* %eptr16200                                                  ; *eptr16200 = %m3U$f
  %eptr16193 = getelementptr inbounds i64, i64* %cloptr16192, i64 0                  ; &cloptr16192[0]
  %f16201 = ptrtoint void(i64,i64)* @lam12510 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16201, i64* %eptr16193                                                 ; store fptr
  %arg10761 = ptrtoint i64* %cloptr16192 to i64                                      ; closure cast; i64* -> i64
  %cloptr16202 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr16203 = getelementptr inbounds i64, i64* %cloptr16202, i64 0                  ; &cloptr16202[0]
  %f16204 = ptrtoint void(i64,i64)* @lam12482 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16204, i64* %eptr16203                                                 ; store fptr
  %arg10760 = ptrtoint i64* %cloptr16202 to i64                                      ; closure cast; i64* -> i64
  %arg10759 = call i64 @const_init_false()                                           ; quoted #f
  %rva12153 = add i64 0, 0                                                           ; quoted ()
  %rva12152 = call i64 @prim_cons(i64 %h68$lsts, i64 %rva12153)                      ; call prim_cons
  %rva12151 = call i64 @prim_cons(i64 %arg10759, i64 %rva12152)                      ; call prim_cons
  %rva12150 = call i64 @prim_cons(i64 %arg10760, i64 %rva12151)                      ; call prim_cons
  %rva12149 = call i64 @prim_cons(i64 %arg10761, i64 %rva12150)                      ; call prim_cons
  %cloptr16205 = inttoptr i64 %LWd$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr16206 = getelementptr inbounds i64, i64* %cloptr16205, i64 0                 ; &cloptr16205[0]
  %f16208 = load i64, i64* %i0ptr16206, align 8                                      ; load; *i0ptr16206
  %fptr16207 = inttoptr i64 %f16208 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16207(i64 %LWd$_37foldr1, i64 %rva12149)            ; tail call
  ret void
}


define void @lam12510(i64 %env12511, i64 %rvp12137) {
  %envptr16209 = inttoptr i64 %env12511 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16210 = getelementptr inbounds i64, i64* %envptr16209, i64 7                ; &envptr16209[7]
  %m3U$f = load i64, i64* %envptr16210, align 8                                      ; load; *envptr16210
  %envptr16211 = inttoptr i64 %env12511 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16212 = getelementptr inbounds i64, i64* %envptr16211, i64 6                ; &envptr16211[6]
  %cont9628 = load i64, i64* %envptr16212, align 8                                   ; load; *envptr16212
  %envptr16213 = inttoptr i64 %env12511 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16214 = getelementptr inbounds i64, i64* %envptr16213, i64 5                ; &envptr16213[5]
  %b4r$_37foldr = load i64, i64* %envptr16214, align 8                               ; load; *envptr16214
  %envptr16215 = inttoptr i64 %env12511 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16216 = getelementptr inbounds i64, i64* %envptr16215, i64 4                ; &envptr16215[4]
  %VAu$acc = load i64, i64* %envptr16216, align 8                                    ; load; *envptr16216
  %envptr16217 = inttoptr i64 %env12511 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16218 = getelementptr inbounds i64, i64* %envptr16217, i64 3                ; &envptr16217[3]
  %SVw$_37map1 = load i64, i64* %envptr16218, align 8                                ; load; *envptr16218
  %envptr16219 = inttoptr i64 %env12511 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16220 = getelementptr inbounds i64, i64* %envptr16219, i64 2                ; &envptr16219[2]
  %LWd$_37foldr1 = load i64, i64* %envptr16220, align 8                              ; load; *envptr16220
  %envptr16221 = inttoptr i64 %env12511 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16222 = getelementptr inbounds i64, i64* %envptr16221, i64 1                ; &envptr16221[1]
  %h68$lsts = load i64, i64* %envptr16222, align 8                                   ; load; *envptr16222
  %_959632 = call i64 @prim_car(i64 %rvp12137)                                       ; call prim_car
  %rvp12136 = call i64 @prim_cdr(i64 %rvp12137)                                      ; call prim_cdr
  %a9252 = call i64 @prim_car(i64 %rvp12136)                                         ; call prim_car
  %na12081 = call i64 @prim_cdr(i64 %rvp12136)                                       ; call prim_cdr
  %cmp16223 = icmp eq i64 %a9252, 15                                                 ; false?
  br i1 %cmp16223, label %else16225, label %then16224                                ; if

then16224:
  %arg10764 = add i64 0, 0                                                           ; quoted ()
  %rva12084 = add i64 0, 0                                                           ; quoted ()
  %rva12083 = call i64 @prim_cons(i64 %VAu$acc, i64 %rva12084)                       ; call prim_cons
  %rva12082 = call i64 @prim_cons(i64 %arg10764, i64 %rva12083)                      ; call prim_cons
  %cloptr16226 = inttoptr i64 %cont9628 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16227 = getelementptr inbounds i64, i64* %cloptr16226, i64 0                 ; &cloptr16226[0]
  %f16229 = load i64, i64* %i0ptr16227, align 8                                      ; load; *i0ptr16227
  %fptr16228 = inttoptr i64 %f16229 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16228(i64 %cont9628, i64 %rva12082)                 ; tail call
  ret void

else16225:
  %cloptr16230 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr16232 = getelementptr inbounds i64, i64* %cloptr16230, i64 1                  ; &eptr16232[1]
  %eptr16233 = getelementptr inbounds i64, i64* %cloptr16230, i64 2                  ; &eptr16233[2]
  %eptr16234 = getelementptr inbounds i64, i64* %cloptr16230, i64 3                  ; &eptr16234[3]
  %eptr16235 = getelementptr inbounds i64, i64* %cloptr16230, i64 4                  ; &eptr16235[4]
  %eptr16236 = getelementptr inbounds i64, i64* %cloptr16230, i64 5                  ; &eptr16236[5]
  %eptr16237 = getelementptr inbounds i64, i64* %cloptr16230, i64 6                  ; &eptr16237[6]
  %eptr16238 = getelementptr inbounds i64, i64* %cloptr16230, i64 7                  ; &eptr16238[7]
  store i64 %h68$lsts, i64* %eptr16232                                               ; *eptr16232 = %h68$lsts
  store i64 %LWd$_37foldr1, i64* %eptr16233                                          ; *eptr16233 = %LWd$_37foldr1
  store i64 %SVw$_37map1, i64* %eptr16234                                            ; *eptr16234 = %SVw$_37map1
  store i64 %VAu$acc, i64* %eptr16235                                                ; *eptr16235 = %VAu$acc
  store i64 %b4r$_37foldr, i64* %eptr16236                                           ; *eptr16236 = %b4r$_37foldr
  store i64 %cont9628, i64* %eptr16237                                               ; *eptr16237 = %cont9628
  store i64 %m3U$f, i64* %eptr16238                                                  ; *eptr16238 = %m3U$f
  %eptr16231 = getelementptr inbounds i64, i64* %cloptr16230, i64 0                  ; &cloptr16230[0]
  %f16239 = ptrtoint void(i64,i64)* @lam12508 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16239, i64* %eptr16231                                                 ; store fptr
  %arg10768 = ptrtoint i64* %cloptr16230 to i64                                      ; closure cast; i64* -> i64
  %cloptr16240 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr16241 = getelementptr inbounds i64, i64* %cloptr16240, i64 0                  ; &cloptr16240[0]
  %f16242 = ptrtoint void(i64,i64)* @lam12489 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16242, i64* %eptr16241                                                 ; store fptr
  %arg10767 = ptrtoint i64* %cloptr16240 to i64                                      ; closure cast; i64* -> i64
  %rva12135 = add i64 0, 0                                                           ; quoted ()
  %rva12134 = call i64 @prim_cons(i64 %h68$lsts, i64 %rva12135)                      ; call prim_cons
  %rva12133 = call i64 @prim_cons(i64 %arg10767, i64 %rva12134)                      ; call prim_cons
  %rva12132 = call i64 @prim_cons(i64 %arg10768, i64 %rva12133)                      ; call prim_cons
  %cloptr16243 = inttoptr i64 %SVw$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr16244 = getelementptr inbounds i64, i64* %cloptr16243, i64 0                 ; &cloptr16243[0]
  %f16246 = load i64, i64* %i0ptr16244, align 8                                      ; load; *i0ptr16244
  %fptr16245 = inttoptr i64 %f16246 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16245(i64 %SVw$_37map1, i64 %rva12132)              ; tail call
  ret void
}


define void @lam12508(i64 %env12509, i64 %rvp12124) {
  %envptr16247 = inttoptr i64 %env12509 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16248 = getelementptr inbounds i64, i64* %envptr16247, i64 7                ; &envptr16247[7]
  %m3U$f = load i64, i64* %envptr16248, align 8                                      ; load; *envptr16248
  %envptr16249 = inttoptr i64 %env12509 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16250 = getelementptr inbounds i64, i64* %envptr16249, i64 6                ; &envptr16249[6]
  %cont9628 = load i64, i64* %envptr16250, align 8                                   ; load; *envptr16250
  %envptr16251 = inttoptr i64 %env12509 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16252 = getelementptr inbounds i64, i64* %envptr16251, i64 5                ; &envptr16251[5]
  %b4r$_37foldr = load i64, i64* %envptr16252, align 8                               ; load; *envptr16252
  %envptr16253 = inttoptr i64 %env12509 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16254 = getelementptr inbounds i64, i64* %envptr16253, i64 4                ; &envptr16253[4]
  %VAu$acc = load i64, i64* %envptr16254, align 8                                    ; load; *envptr16254
  %envptr16255 = inttoptr i64 %env12509 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16256 = getelementptr inbounds i64, i64* %envptr16255, i64 3                ; &envptr16255[3]
  %SVw$_37map1 = load i64, i64* %envptr16256, align 8                                ; load; *envptr16256
  %envptr16257 = inttoptr i64 %env12509 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16258 = getelementptr inbounds i64, i64* %envptr16257, i64 2                ; &envptr16257[2]
  %LWd$_37foldr1 = load i64, i64* %envptr16258, align 8                              ; load; *envptr16258
  %envptr16259 = inttoptr i64 %env12509 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16260 = getelementptr inbounds i64, i64* %envptr16259, i64 1                ; &envptr16259[1]
  %h68$lsts = load i64, i64* %envptr16260, align 8                                   ; load; *envptr16260
  %_959633 = call i64 @prim_car(i64 %rvp12124)                                       ; call prim_car
  %rvp12123 = call i64 @prim_cdr(i64 %rvp12124)                                      ; call prim_cdr
  %Wnz$lsts_43 = call i64 @prim_car(i64 %rvp12123)                                   ; call prim_car
  %na12086 = call i64 @prim_cdr(i64 %rvp12123)                                       ; call prim_cdr
  %cloptr16261 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr16263 = getelementptr inbounds i64, i64* %cloptr16261, i64 1                  ; &eptr16263[1]
  %eptr16264 = getelementptr inbounds i64, i64* %cloptr16261, i64 2                  ; &eptr16264[2]
  %eptr16265 = getelementptr inbounds i64, i64* %cloptr16261, i64 3                  ; &eptr16265[3]
  %eptr16266 = getelementptr inbounds i64, i64* %cloptr16261, i64 4                  ; &eptr16266[4]
  %eptr16267 = getelementptr inbounds i64, i64* %cloptr16261, i64 5                  ; &eptr16267[5]
  %eptr16268 = getelementptr inbounds i64, i64* %cloptr16261, i64 6                  ; &eptr16268[6]
  store i64 %LWd$_37foldr1, i64* %eptr16263                                          ; *eptr16263 = %LWd$_37foldr1
  store i64 %Wnz$lsts_43, i64* %eptr16264                                            ; *eptr16264 = %Wnz$lsts_43
  store i64 %VAu$acc, i64* %eptr16265                                                ; *eptr16265 = %VAu$acc
  store i64 %b4r$_37foldr, i64* %eptr16266                                           ; *eptr16266 = %b4r$_37foldr
  store i64 %cont9628, i64* %eptr16267                                               ; *eptr16267 = %cont9628
  store i64 %m3U$f, i64* %eptr16268                                                  ; *eptr16268 = %m3U$f
  %eptr16262 = getelementptr inbounds i64, i64* %cloptr16261, i64 0                  ; &cloptr16261[0]
  %f16269 = ptrtoint void(i64,i64)* @lam12506 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16269, i64* %eptr16262                                                 ; store fptr
  %arg10772 = ptrtoint i64* %cloptr16261 to i64                                      ; closure cast; i64* -> i64
  %cloptr16270 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr16271 = getelementptr inbounds i64, i64* %cloptr16270, i64 0                  ; &cloptr16270[0]
  %f16272 = ptrtoint void(i64,i64)* @lam12494 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16272, i64* %eptr16271                                                 ; store fptr
  %arg10771 = ptrtoint i64* %cloptr16270 to i64                                      ; closure cast; i64* -> i64
  %rva12122 = add i64 0, 0                                                           ; quoted ()
  %rva12121 = call i64 @prim_cons(i64 %h68$lsts, i64 %rva12122)                      ; call prim_cons
  %rva12120 = call i64 @prim_cons(i64 %arg10771, i64 %rva12121)                      ; call prim_cons
  %rva12119 = call i64 @prim_cons(i64 %arg10772, i64 %rva12120)                      ; call prim_cons
  %cloptr16273 = inttoptr i64 %SVw$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr16274 = getelementptr inbounds i64, i64* %cloptr16273, i64 0                 ; &cloptr16273[0]
  %f16276 = load i64, i64* %i0ptr16274, align 8                                      ; load; *i0ptr16274
  %fptr16275 = inttoptr i64 %f16276 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16275(i64 %SVw$_37map1, i64 %rva12119)              ; tail call
  ret void
}


define void @lam12506(i64 %env12507, i64 %rvp12111) {
  %envptr16277 = inttoptr i64 %env12507 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16278 = getelementptr inbounds i64, i64* %envptr16277, i64 6                ; &envptr16277[6]
  %m3U$f = load i64, i64* %envptr16278, align 8                                      ; load; *envptr16278
  %envptr16279 = inttoptr i64 %env12507 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16280 = getelementptr inbounds i64, i64* %envptr16279, i64 5                ; &envptr16279[5]
  %cont9628 = load i64, i64* %envptr16280, align 8                                   ; load; *envptr16280
  %envptr16281 = inttoptr i64 %env12507 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16282 = getelementptr inbounds i64, i64* %envptr16281, i64 4                ; &envptr16281[4]
  %b4r$_37foldr = load i64, i64* %envptr16282, align 8                               ; load; *envptr16282
  %envptr16283 = inttoptr i64 %env12507 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16284 = getelementptr inbounds i64, i64* %envptr16283, i64 3                ; &envptr16283[3]
  %VAu$acc = load i64, i64* %envptr16284, align 8                                    ; load; *envptr16284
  %envptr16285 = inttoptr i64 %env12507 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16286 = getelementptr inbounds i64, i64* %envptr16285, i64 2                ; &envptr16285[2]
  %Wnz$lsts_43 = load i64, i64* %envptr16286, align 8                                ; load; *envptr16286
  %envptr16287 = inttoptr i64 %env12507 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16288 = getelementptr inbounds i64, i64* %envptr16287, i64 1                ; &envptr16287[1]
  %LWd$_37foldr1 = load i64, i64* %envptr16288, align 8                              ; load; *envptr16288
  %_959634 = call i64 @prim_car(i64 %rvp12111)                                       ; call prim_car
  %rvp12110 = call i64 @prim_cdr(i64 %rvp12111)                                      ; call prim_cdr
  %NSu$vs = call i64 @prim_car(i64 %rvp12110)                                        ; call prim_car
  %na12088 = call i64 @prim_cdr(i64 %rvp12110)                                       ; call prim_cdr
  %a9253 = call i64 @prim_cons(i64 %VAu$acc, i64 %Wnz$lsts_43)                       ; call prim_cons
  %a9254 = call i64 @prim_cons(i64 %m3U$f, i64 %a9253)                               ; call prim_cons
  %cloptr16289 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr16291 = getelementptr inbounds i64, i64* %cloptr16289, i64 1                  ; &eptr16291[1]
  %eptr16292 = getelementptr inbounds i64, i64* %cloptr16289, i64 2                  ; &eptr16292[2]
  %eptr16293 = getelementptr inbounds i64, i64* %cloptr16289, i64 3                  ; &eptr16293[3]
  %eptr16294 = getelementptr inbounds i64, i64* %cloptr16289, i64 4                  ; &eptr16294[4]
  store i64 %LWd$_37foldr1, i64* %eptr16291                                          ; *eptr16291 = %LWd$_37foldr1
  store i64 %NSu$vs, i64* %eptr16292                                                 ; *eptr16292 = %NSu$vs
  store i64 %cont9628, i64* %eptr16293                                               ; *eptr16293 = %cont9628
  store i64 %m3U$f, i64* %eptr16294                                                  ; *eptr16294 = %m3U$f
  %eptr16290 = getelementptr inbounds i64, i64* %cloptr16289, i64 0                  ; &cloptr16289[0]
  %f16295 = ptrtoint void(i64,i64)* @lam12504 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16295, i64* %eptr16290                                                 ; store fptr
  %arg10779 = ptrtoint i64* %cloptr16289 to i64                                      ; closure cast; i64* -> i64
  %cps_45lst9640 = call i64 @prim_cons(i64 %arg10779, i64 %a9254)                    ; call prim_cons
  %cloptr16296 = inttoptr i64 %b4r$_37foldr to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr16297 = getelementptr inbounds i64, i64* %cloptr16296, i64 0                 ; &cloptr16296[0]
  %f16299 = load i64, i64* %i0ptr16297, align 8                                      ; load; *i0ptr16297
  %fptr16298 = inttoptr i64 %f16299 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16298(i64 %b4r$_37foldr, i64 %cps_45lst9640)        ; tail call
  ret void
}


define void @lam12504(i64 %env12505, i64 %rvp12109) {
  %envptr16300 = inttoptr i64 %env12505 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16301 = getelementptr inbounds i64, i64* %envptr16300, i64 4                ; &envptr16300[4]
  %m3U$f = load i64, i64* %envptr16301, align 8                                      ; load; *envptr16301
  %envptr16302 = inttoptr i64 %env12505 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16303 = getelementptr inbounds i64, i64* %envptr16302, i64 3                ; &envptr16302[3]
  %cont9628 = load i64, i64* %envptr16303, align 8                                   ; load; *envptr16303
  %envptr16304 = inttoptr i64 %env12505 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16305 = getelementptr inbounds i64, i64* %envptr16304, i64 2                ; &envptr16304[2]
  %NSu$vs = load i64, i64* %envptr16305, align 8                                     ; load; *envptr16305
  %envptr16306 = inttoptr i64 %env12505 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16307 = getelementptr inbounds i64, i64* %envptr16306, i64 1                ; &envptr16306[1]
  %LWd$_37foldr1 = load i64, i64* %envptr16307, align 8                              ; load; *envptr16307
  %_959635 = call i64 @prim_car(i64 %rvp12109)                                       ; call prim_car
  %rvp12108 = call i64 @prim_cdr(i64 %rvp12109)                                      ; call prim_cdr
  %a9255 = call i64 @prim_car(i64 %rvp12108)                                         ; call prim_car
  %na12090 = call i64 @prim_cdr(i64 %rvp12108)                                       ; call prim_cdr
  %arg10780 = add i64 0, 0                                                           ; quoted ()
  %a9256 = call i64 @prim_cons(i64 %a9255, i64 %arg10780)                            ; call prim_cons
  %cloptr16308 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr16310 = getelementptr inbounds i64, i64* %cloptr16308, i64 1                  ; &eptr16310[1]
  %eptr16311 = getelementptr inbounds i64, i64* %cloptr16308, i64 2                  ; &eptr16311[2]
  store i64 %cont9628, i64* %eptr16310                                               ; *eptr16310 = %cont9628
  store i64 %m3U$f, i64* %eptr16311                                                  ; *eptr16311 = %m3U$f
  %eptr16309 = getelementptr inbounds i64, i64* %cloptr16308, i64 0                  ; &cloptr16308[0]
  %f16312 = ptrtoint void(i64,i64)* @lam12501 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16312, i64* %eptr16309                                                 ; store fptr
  %arg10785 = ptrtoint i64* %cloptr16308 to i64                                      ; closure cast; i64* -> i64
  %cloptr16313 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr16314 = getelementptr inbounds i64, i64* %cloptr16313, i64 0                  ; &cloptr16313[0]
  %f16315 = ptrtoint void(i64,i64)* @lam12499 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16315, i64* %eptr16314                                                 ; store fptr
  %arg10784 = ptrtoint i64* %cloptr16313 to i64                                      ; closure cast; i64* -> i64
  %rva12107 = add i64 0, 0                                                           ; quoted ()
  %rva12106 = call i64 @prim_cons(i64 %NSu$vs, i64 %rva12107)                        ; call prim_cons
  %rva12105 = call i64 @prim_cons(i64 %a9256, i64 %rva12106)                         ; call prim_cons
  %rva12104 = call i64 @prim_cons(i64 %arg10784, i64 %rva12105)                      ; call prim_cons
  %rva12103 = call i64 @prim_cons(i64 %arg10785, i64 %rva12104)                      ; call prim_cons
  %cloptr16316 = inttoptr i64 %LWd$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr16317 = getelementptr inbounds i64, i64* %cloptr16316, i64 0                 ; &cloptr16316[0]
  %f16319 = load i64, i64* %i0ptr16317, align 8                                      ; load; *i0ptr16317
  %fptr16318 = inttoptr i64 %f16319 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16318(i64 %LWd$_37foldr1, i64 %rva12103)            ; tail call
  ret void
}


define void @lam12501(i64 %env12502, i64 %rvp12094) {
  %envptr16320 = inttoptr i64 %env12502 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16321 = getelementptr inbounds i64, i64* %envptr16320, i64 2                ; &envptr16320[2]
  %m3U$f = load i64, i64* %envptr16321, align 8                                      ; load; *envptr16321
  %envptr16322 = inttoptr i64 %env12502 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16323 = getelementptr inbounds i64, i64* %envptr16322, i64 1                ; &envptr16322[1]
  %cont9628 = load i64, i64* %envptr16323, align 8                                   ; load; *envptr16323
  %_959636 = call i64 @prim_car(i64 %rvp12094)                                       ; call prim_car
  %rvp12093 = call i64 @prim_cdr(i64 %rvp12094)                                      ; call prim_cdr
  %a9257 = call i64 @prim_car(i64 %rvp12093)                                         ; call prim_car
  %na12092 = call i64 @prim_cdr(i64 %rvp12093)                                       ; call prim_cdr
  %cps_45lst9637 = call i64 @prim_cons(i64 %cont9628, i64 %a9257)                    ; call prim_cons
  %cloptr16324 = inttoptr i64 %m3U$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr16325 = getelementptr inbounds i64, i64* %cloptr16324, i64 0                 ; &cloptr16324[0]
  %f16327 = load i64, i64* %i0ptr16325, align 8                                      ; load; *i0ptr16325
  %fptr16326 = inttoptr i64 %f16327 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16326(i64 %m3U$f, i64 %cps_45lst9637)               ; tail call
  ret void
}


define void @lam12499(i64 %env12500, i64 %rvp12102) {
  %cont9638 = call i64 @prim_car(i64 %rvp12102)                                      ; call prim_car
  %rvp12101 = call i64 @prim_cdr(i64 %rvp12102)                                      ; call prim_cdr
  %CGt$a = call i64 @prim_car(i64 %rvp12101)                                         ; call prim_car
  %rvp12100 = call i64 @prim_cdr(i64 %rvp12101)                                      ; call prim_cdr
  %hgJ$b = call i64 @prim_car(i64 %rvp12100)                                         ; call prim_car
  %na12096 = call i64 @prim_cdr(i64 %rvp12100)                                       ; call prim_cdr
  %retprim9639 = call i64 @prim_cons(i64 %CGt$a, i64 %hgJ$b)                         ; call prim_cons
  %arg10792 = add i64 0, 0                                                           ; quoted ()
  %rva12099 = add i64 0, 0                                                           ; quoted ()
  %rva12098 = call i64 @prim_cons(i64 %retprim9639, i64 %rva12099)                   ; call prim_cons
  %rva12097 = call i64 @prim_cons(i64 %arg10792, i64 %rva12098)                      ; call prim_cons
  %cloptr16328 = inttoptr i64 %cont9638 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16329 = getelementptr inbounds i64, i64* %cloptr16328, i64 0                 ; &cloptr16328[0]
  %f16331 = load i64, i64* %i0ptr16329, align 8                                      ; load; *i0ptr16329
  %fptr16330 = inttoptr i64 %f16331 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16330(i64 %cont9638, i64 %rva12097)                 ; tail call
  ret void
}


define void @lam12494(i64 %env12495, i64 %rvp12118) {
  %cont9641 = call i64 @prim_car(i64 %rvp12118)                                      ; call prim_car
  %rvp12117 = call i64 @prim_cdr(i64 %rvp12118)                                      ; call prim_cdr
  %foU$x = call i64 @prim_car(i64 %rvp12117)                                         ; call prim_car
  %na12113 = call i64 @prim_cdr(i64 %rvp12117)                                       ; call prim_cdr
  %retprim9642 = call i64 @prim_car(i64 %foU$x)                                      ; call prim_car
  %arg10796 = add i64 0, 0                                                           ; quoted ()
  %rva12116 = add i64 0, 0                                                           ; quoted ()
  %rva12115 = call i64 @prim_cons(i64 %retprim9642, i64 %rva12116)                   ; call prim_cons
  %rva12114 = call i64 @prim_cons(i64 %arg10796, i64 %rva12115)                      ; call prim_cons
  %cloptr16332 = inttoptr i64 %cont9641 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16333 = getelementptr inbounds i64, i64* %cloptr16332, i64 0                 ; &cloptr16332[0]
  %f16335 = load i64, i64* %i0ptr16333, align 8                                      ; load; *i0ptr16333
  %fptr16334 = inttoptr i64 %f16335 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16334(i64 %cont9641, i64 %rva12114)                 ; tail call
  ret void
}


define void @lam12489(i64 %env12490, i64 %rvp12131) {
  %cont9643 = call i64 @prim_car(i64 %rvp12131)                                      ; call prim_car
  %rvp12130 = call i64 @prim_cdr(i64 %rvp12131)                                      ; call prim_cdr
  %NuM$x = call i64 @prim_car(i64 %rvp12130)                                         ; call prim_car
  %na12126 = call i64 @prim_cdr(i64 %rvp12130)                                       ; call prim_cdr
  %retprim9644 = call i64 @prim_cdr(i64 %NuM$x)                                      ; call prim_cdr
  %arg10800 = add i64 0, 0                                                           ; quoted ()
  %rva12129 = add i64 0, 0                                                           ; quoted ()
  %rva12128 = call i64 @prim_cons(i64 %retprim9644, i64 %rva12129)                   ; call prim_cons
  %rva12127 = call i64 @prim_cons(i64 %arg10800, i64 %rva12128)                      ; call prim_cons
  %cloptr16336 = inttoptr i64 %cont9643 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16337 = getelementptr inbounds i64, i64* %cloptr16336, i64 0                 ; &cloptr16336[0]
  %f16339 = load i64, i64* %i0ptr16337, align 8                                      ; load; *i0ptr16337
  %fptr16338 = inttoptr i64 %f16339 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16338(i64 %cont9643, i64 %rva12127)                 ; tail call
  ret void
}


define void @lam12482(i64 %env12483, i64 %rvp12148) {
  %cont9645 = call i64 @prim_car(i64 %rvp12148)                                      ; call prim_car
  %rvp12147 = call i64 @prim_cdr(i64 %rvp12148)                                      ; call prim_cdr
  %r29$lst = call i64 @prim_car(i64 %rvp12147)                                       ; call prim_car
  %rvp12146 = call i64 @prim_cdr(i64 %rvp12147)                                      ; call prim_cdr
  %MCq$b = call i64 @prim_car(i64 %rvp12146)                                         ; call prim_car
  %na12139 = call i64 @prim_cdr(i64 %rvp12146)                                       ; call prim_cdr
  %cmp16340 = icmp eq i64 %MCq$b, 15                                                 ; false?
  br i1 %cmp16340, label %else16342, label %then16341                                ; if

then16341:
  %arg10803 = add i64 0, 0                                                           ; quoted ()
  %rva12142 = add i64 0, 0                                                           ; quoted ()
  %rva12141 = call i64 @prim_cons(i64 %MCq$b, i64 %rva12142)                         ; call prim_cons
  %rva12140 = call i64 @prim_cons(i64 %arg10803, i64 %rva12141)                      ; call prim_cons
  %cloptr16343 = inttoptr i64 %cont9645 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16344 = getelementptr inbounds i64, i64* %cloptr16343, i64 0                 ; &cloptr16343[0]
  %f16346 = load i64, i64* %i0ptr16344, align 8                                      ; load; *i0ptr16344
  %fptr16345 = inttoptr i64 %f16346 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16345(i64 %cont9645, i64 %rva12140)                 ; tail call
  ret void

else16342:
  %retprim9646 = call i64 @prim_null_63(i64 %r29$lst)                                ; call prim_null_63
  %arg10807 = add i64 0, 0                                                           ; quoted ()
  %rva12145 = add i64 0, 0                                                           ; quoted ()
  %rva12144 = call i64 @prim_cons(i64 %retprim9646, i64 %rva12145)                   ; call prim_cons
  %rva12143 = call i64 @prim_cons(i64 %arg10807, i64 %rva12144)                      ; call prim_cons
  %cloptr16347 = inttoptr i64 %cont9645 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16348 = getelementptr inbounds i64, i64* %cloptr16347, i64 0                 ; &cloptr16347[0]
  %f16350 = load i64, i64* %i0ptr16348, align 8                                      ; load; *i0ptr16348
  %fptr16349 = inttoptr i64 %f16350 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16349(i64 %cont9645, i64 %rva12143)                 ; tail call
  ret void
}


define void @lam12468(i64 %env12469, i64 %rvp12202) {
  %cont9649 = call i64 @prim_car(i64 %rvp12202)                                      ; call prim_car
  %rvp12201 = call i64 @prim_cdr(i64 %rvp12202)                                      ; call prim_cdr
  %EBu$_37foldl1 = call i64 @prim_car(i64 %rvp12201)                                 ; call prim_car
  %na12175 = call i64 @prim_cdr(i64 %rvp12201)                                       ; call prim_cdr
  %arg10810 = add i64 0, 0                                                           ; quoted ()
  %cloptr16351 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr16353 = getelementptr inbounds i64, i64* %cloptr16351, i64 1                  ; &eptr16353[1]
  store i64 %EBu$_37foldl1, i64* %eptr16353                                          ; *eptr16353 = %EBu$_37foldl1
  %eptr16352 = getelementptr inbounds i64, i64* %cloptr16351, i64 0                  ; &cloptr16351[0]
  %f16354 = ptrtoint void(i64,i64)* @lam12465 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16354, i64* %eptr16352                                                 ; store fptr
  %arg10809 = ptrtoint i64* %cloptr16351 to i64                                      ; closure cast; i64* -> i64
  %rva12200 = add i64 0, 0                                                           ; quoted ()
  %rva12199 = call i64 @prim_cons(i64 %arg10809, i64 %rva12200)                      ; call prim_cons
  %rva12198 = call i64 @prim_cons(i64 %arg10810, i64 %rva12199)                      ; call prim_cons
  %cloptr16355 = inttoptr i64 %cont9649 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16356 = getelementptr inbounds i64, i64* %cloptr16355, i64 0                 ; &cloptr16355[0]
  %f16358 = load i64, i64* %i0ptr16356, align 8                                      ; load; *i0ptr16356
  %fptr16357 = inttoptr i64 %f16358 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16357(i64 %cont9649, i64 %rva12198)                 ; tail call
  ret void
}


define void @lam12465(i64 %env12466, i64 %rvp12197) {
  %envptr16359 = inttoptr i64 %env12466 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16360 = getelementptr inbounds i64, i64* %envptr16359, i64 1                ; &envptr16359[1]
  %EBu$_37foldl1 = load i64, i64* %envptr16360, align 8                              ; load; *envptr16360
  %cont9650 = call i64 @prim_car(i64 %rvp12197)                                      ; call prim_car
  %rvp12196 = call i64 @prim_cdr(i64 %rvp12197)                                      ; call prim_cdr
  %QLo$f = call i64 @prim_car(i64 %rvp12196)                                         ; call prim_car
  %rvp12195 = call i64 @prim_cdr(i64 %rvp12196)                                      ; call prim_cdr
  %X8O$acc = call i64 @prim_car(i64 %rvp12195)                                       ; call prim_car
  %rvp12194 = call i64 @prim_cdr(i64 %rvp12195)                                      ; call prim_cdr
  %Rs0$lst = call i64 @prim_car(i64 %rvp12194)                                       ; call prim_car
  %na12177 = call i64 @prim_cdr(i64 %rvp12194)                                       ; call prim_cdr
  %a9244 = call i64 @prim_null_63(i64 %Rs0$lst)                                      ; call prim_null_63
  %cmp16361 = icmp eq i64 %a9244, 15                                                 ; false?
  br i1 %cmp16361, label %else16363, label %then16362                                ; if

then16362:
  %arg10814 = add i64 0, 0                                                           ; quoted ()
  %rva12180 = add i64 0, 0                                                           ; quoted ()
  %rva12179 = call i64 @prim_cons(i64 %X8O$acc, i64 %rva12180)                       ; call prim_cons
  %rva12178 = call i64 @prim_cons(i64 %arg10814, i64 %rva12179)                      ; call prim_cons
  %cloptr16364 = inttoptr i64 %cont9650 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16365 = getelementptr inbounds i64, i64* %cloptr16364, i64 0                 ; &cloptr16364[0]
  %f16367 = load i64, i64* %i0ptr16365, align 8                                      ; load; *i0ptr16365
  %fptr16366 = inttoptr i64 %f16367 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16366(i64 %cont9650, i64 %rva12178)                 ; tail call
  ret void

else16363:
  %a9245 = call i64 @prim_car(i64 %Rs0$lst)                                          ; call prim_car
  %cloptr16368 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr16370 = getelementptr inbounds i64, i64* %cloptr16368, i64 1                  ; &eptr16370[1]
  %eptr16371 = getelementptr inbounds i64, i64* %cloptr16368, i64 2                  ; &eptr16371[2]
  %eptr16372 = getelementptr inbounds i64, i64* %cloptr16368, i64 3                  ; &eptr16372[3]
  %eptr16373 = getelementptr inbounds i64, i64* %cloptr16368, i64 4                  ; &eptr16373[4]
  store i64 %cont9650, i64* %eptr16370                                               ; *eptr16370 = %cont9650
  store i64 %QLo$f, i64* %eptr16371                                                  ; *eptr16371 = %QLo$f
  store i64 %EBu$_37foldl1, i64* %eptr16372                                          ; *eptr16372 = %EBu$_37foldl1
  store i64 %Rs0$lst, i64* %eptr16373                                                ; *eptr16373 = %Rs0$lst
  %eptr16369 = getelementptr inbounds i64, i64* %cloptr16368, i64 0                  ; &cloptr16368[0]
  %f16374 = ptrtoint void(i64,i64)* @lam12463 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16374, i64* %eptr16369                                                 ; store fptr
  %arg10819 = ptrtoint i64* %cloptr16368 to i64                                      ; closure cast; i64* -> i64
  %rva12193 = add i64 0, 0                                                           ; quoted ()
  %rva12192 = call i64 @prim_cons(i64 %X8O$acc, i64 %rva12193)                       ; call prim_cons
  %rva12191 = call i64 @prim_cons(i64 %a9245, i64 %rva12192)                         ; call prim_cons
  %rva12190 = call i64 @prim_cons(i64 %arg10819, i64 %rva12191)                      ; call prim_cons
  %cloptr16375 = inttoptr i64 %QLo$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr16376 = getelementptr inbounds i64, i64* %cloptr16375, i64 0                 ; &cloptr16375[0]
  %f16378 = load i64, i64* %i0ptr16376, align 8                                      ; load; *i0ptr16376
  %fptr16377 = inttoptr i64 %f16378 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16377(i64 %QLo$f, i64 %rva12190)                    ; tail call
  ret void
}


define void @lam12463(i64 %env12464, i64 %rvp12189) {
  %envptr16379 = inttoptr i64 %env12464 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16380 = getelementptr inbounds i64, i64* %envptr16379, i64 4                ; &envptr16379[4]
  %Rs0$lst = load i64, i64* %envptr16380, align 8                                    ; load; *envptr16380
  %envptr16381 = inttoptr i64 %env12464 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16382 = getelementptr inbounds i64, i64* %envptr16381, i64 3                ; &envptr16381[3]
  %EBu$_37foldl1 = load i64, i64* %envptr16382, align 8                              ; load; *envptr16382
  %envptr16383 = inttoptr i64 %env12464 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16384 = getelementptr inbounds i64, i64* %envptr16383, i64 2                ; &envptr16383[2]
  %QLo$f = load i64, i64* %envptr16384, align 8                                      ; load; *envptr16384
  %envptr16385 = inttoptr i64 %env12464 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16386 = getelementptr inbounds i64, i64* %envptr16385, i64 1                ; &envptr16385[1]
  %cont9650 = load i64, i64* %envptr16386, align 8                                   ; load; *envptr16386
  %_959651 = call i64 @prim_car(i64 %rvp12189)                                       ; call prim_car
  %rvp12188 = call i64 @prim_cdr(i64 %rvp12189)                                      ; call prim_cdr
  %a9246 = call i64 @prim_car(i64 %rvp12188)                                         ; call prim_car
  %na12182 = call i64 @prim_cdr(i64 %rvp12188)                                       ; call prim_cdr
  %a9247 = call i64 @prim_cdr(i64 %Rs0$lst)                                          ; call prim_cdr
  %rva12187 = add i64 0, 0                                                           ; quoted ()
  %rva12186 = call i64 @prim_cons(i64 %a9247, i64 %rva12187)                         ; call prim_cons
  %rva12185 = call i64 @prim_cons(i64 %a9246, i64 %rva12186)                         ; call prim_cons
  %rva12184 = call i64 @prim_cons(i64 %QLo$f, i64 %rva12185)                         ; call prim_cons
  %rva12183 = call i64 @prim_cons(i64 %cont9650, i64 %rva12184)                      ; call prim_cons
  %cloptr16387 = inttoptr i64 %EBu$_37foldl1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr16388 = getelementptr inbounds i64, i64* %cloptr16387, i64 0                 ; &cloptr16387[0]
  %f16390 = load i64, i64* %i0ptr16388, align 8                                      ; load; *i0ptr16388
  %fptr16389 = inttoptr i64 %f16390 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16389(i64 %EBu$_37foldl1, i64 %rva12183)            ; tail call
  ret void
}


define void @lam12455(i64 %env12456, i64 %rvp12231) {
  %cont9652 = call i64 @prim_car(i64 %rvp12231)                                      ; call prim_car
  %rvp12230 = call i64 @prim_cdr(i64 %rvp12231)                                      ; call prim_cdr
  %D0C$_37length = call i64 @prim_car(i64 %rvp12230)                                 ; call prim_car
  %na12209 = call i64 @prim_cdr(i64 %rvp12230)                                       ; call prim_cdr
  %arg10828 = add i64 0, 0                                                           ; quoted ()
  %cloptr16391 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr16393 = getelementptr inbounds i64, i64* %cloptr16391, i64 1                  ; &eptr16393[1]
  store i64 %D0C$_37length, i64* %eptr16393                                          ; *eptr16393 = %D0C$_37length
  %eptr16392 = getelementptr inbounds i64, i64* %cloptr16391, i64 0                  ; &cloptr16391[0]
  %f16394 = ptrtoint void(i64,i64)* @lam12452 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16394, i64* %eptr16392                                                 ; store fptr
  %arg10827 = ptrtoint i64* %cloptr16391 to i64                                      ; closure cast; i64* -> i64
  %rva12229 = add i64 0, 0                                                           ; quoted ()
  %rva12228 = call i64 @prim_cons(i64 %arg10827, i64 %rva12229)                      ; call prim_cons
  %rva12227 = call i64 @prim_cons(i64 %arg10828, i64 %rva12228)                      ; call prim_cons
  %cloptr16395 = inttoptr i64 %cont9652 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16396 = getelementptr inbounds i64, i64* %cloptr16395, i64 0                 ; &cloptr16395[0]
  %f16398 = load i64, i64* %i0ptr16396, align 8                                      ; load; *i0ptr16396
  %fptr16397 = inttoptr i64 %f16398 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16397(i64 %cont9652, i64 %rva12227)                 ; tail call
  ret void
}


define void @lam12452(i64 %env12453, i64 %rvp12226) {
  %envptr16399 = inttoptr i64 %env12453 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16400 = getelementptr inbounds i64, i64* %envptr16399, i64 1                ; &envptr16399[1]
  %D0C$_37length = load i64, i64* %envptr16400, align 8                              ; load; *envptr16400
  %cont9653 = call i64 @prim_car(i64 %rvp12226)                                      ; call prim_car
  %rvp12225 = call i64 @prim_cdr(i64 %rvp12226)                                      ; call prim_cdr
  %VkT$lst = call i64 @prim_car(i64 %rvp12225)                                       ; call prim_car
  %na12211 = call i64 @prim_cdr(i64 %rvp12225)                                       ; call prim_cdr
  %a9241 = call i64 @prim_null_63(i64 %VkT$lst)                                      ; call prim_null_63
  %cmp16401 = icmp eq i64 %a9241, 15                                                 ; false?
  br i1 %cmp16401, label %else16403, label %then16402                                ; if

then16402:
  %arg10832 = add i64 0, 0                                                           ; quoted ()
  %arg10831 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %rva12214 = add i64 0, 0                                                           ; quoted ()
  %rva12213 = call i64 @prim_cons(i64 %arg10831, i64 %rva12214)                      ; call prim_cons
  %rva12212 = call i64 @prim_cons(i64 %arg10832, i64 %rva12213)                      ; call prim_cons
  %cloptr16404 = inttoptr i64 %cont9653 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16405 = getelementptr inbounds i64, i64* %cloptr16404, i64 0                 ; &cloptr16404[0]
  %f16407 = load i64, i64* %i0ptr16405, align 8                                      ; load; *i0ptr16405
  %fptr16406 = inttoptr i64 %f16407 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16406(i64 %cont9653, i64 %rva12212)                 ; tail call
  ret void

else16403:
  %a9242 = call i64 @prim_cdr(i64 %VkT$lst)                                          ; call prim_cdr
  %cloptr16408 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr16410 = getelementptr inbounds i64, i64* %cloptr16408, i64 1                  ; &eptr16410[1]
  store i64 %cont9653, i64* %eptr16410                                               ; *eptr16410 = %cont9653
  %eptr16409 = getelementptr inbounds i64, i64* %cloptr16408, i64 0                  ; &cloptr16408[0]
  %f16411 = ptrtoint void(i64,i64)* @lam12450 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16411, i64* %eptr16409                                                 ; store fptr
  %arg10836 = ptrtoint i64* %cloptr16408 to i64                                      ; closure cast; i64* -> i64
  %rva12224 = add i64 0, 0                                                           ; quoted ()
  %rva12223 = call i64 @prim_cons(i64 %a9242, i64 %rva12224)                         ; call prim_cons
  %rva12222 = call i64 @prim_cons(i64 %arg10836, i64 %rva12223)                      ; call prim_cons
  %cloptr16412 = inttoptr i64 %D0C$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr16413 = getelementptr inbounds i64, i64* %cloptr16412, i64 0                 ; &cloptr16412[0]
  %f16415 = load i64, i64* %i0ptr16413, align 8                                      ; load; *i0ptr16413
  %fptr16414 = inttoptr i64 %f16415 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16414(i64 %D0C$_37length, i64 %rva12222)            ; tail call
  ret void
}


define void @lam12450(i64 %env12451, i64 %rvp12221) {
  %envptr16416 = inttoptr i64 %env12451 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16417 = getelementptr inbounds i64, i64* %envptr16416, i64 1                ; &envptr16416[1]
  %cont9653 = load i64, i64* %envptr16417, align 8                                   ; load; *envptr16417
  %_959654 = call i64 @prim_car(i64 %rvp12221)                                       ; call prim_car
  %rvp12220 = call i64 @prim_cdr(i64 %rvp12221)                                      ; call prim_cdr
  %a9243 = call i64 @prim_car(i64 %rvp12220)                                         ; call prim_car
  %na12216 = call i64 @prim_cdr(i64 %rvp12220)                                       ; call prim_cdr
  %arg10839 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %retprim9655 = call i64 @prim__43(i64 %arg10839, i64 %a9243)                       ; call prim__43
  %arg10841 = add i64 0, 0                                                           ; quoted ()
  %rva12219 = add i64 0, 0                                                           ; quoted ()
  %rva12218 = call i64 @prim_cons(i64 %retprim9655, i64 %rva12219)                   ; call prim_cons
  %rva12217 = call i64 @prim_cons(i64 %arg10841, i64 %rva12218)                      ; call prim_cons
  %cloptr16418 = inttoptr i64 %cont9653 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16419 = getelementptr inbounds i64, i64* %cloptr16418, i64 0                 ; &cloptr16418[0]
  %f16421 = load i64, i64* %i0ptr16419, align 8                                      ; load; *i0ptr16419
  %fptr16420 = inttoptr i64 %f16421 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16420(i64 %cont9653, i64 %rva12217)                 ; tail call
  ret void
}


define void @lam12439(i64 %env12440, i64 %rvp12265) {
  %cont9656 = call i64 @prim_car(i64 %rvp12265)                                      ; call prim_car
  %rvp12264 = call i64 @prim_cdr(i64 %rvp12265)                                      ; call prim_cdr
  %A9C$_37take = call i64 @prim_car(i64 %rvp12264)                                   ; call prim_car
  %na12238 = call i64 @prim_cdr(i64 %rvp12264)                                       ; call prim_cdr
  %arg10844 = add i64 0, 0                                                           ; quoted ()
  %cloptr16422 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr16424 = getelementptr inbounds i64, i64* %cloptr16422, i64 1                  ; &eptr16424[1]
  store i64 %A9C$_37take, i64* %eptr16424                                            ; *eptr16424 = %A9C$_37take
  %eptr16423 = getelementptr inbounds i64, i64* %cloptr16422, i64 0                  ; &cloptr16422[0]
  %f16425 = ptrtoint void(i64,i64)* @lam12436 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16425, i64* %eptr16423                                                 ; store fptr
  %arg10843 = ptrtoint i64* %cloptr16422 to i64                                      ; closure cast; i64* -> i64
  %rva12263 = add i64 0, 0                                                           ; quoted ()
  %rva12262 = call i64 @prim_cons(i64 %arg10843, i64 %rva12263)                      ; call prim_cons
  %rva12261 = call i64 @prim_cons(i64 %arg10844, i64 %rva12262)                      ; call prim_cons
  %cloptr16426 = inttoptr i64 %cont9656 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16427 = getelementptr inbounds i64, i64* %cloptr16426, i64 0                 ; &cloptr16426[0]
  %f16429 = load i64, i64* %i0ptr16427, align 8                                      ; load; *i0ptr16427
  %fptr16428 = inttoptr i64 %f16429 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16428(i64 %cont9656, i64 %rva12261)                 ; tail call
  ret void
}


define void @lam12436(i64 %env12437, i64 %rvp12260) {
  %envptr16430 = inttoptr i64 %env12437 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16431 = getelementptr inbounds i64, i64* %envptr16430, i64 1                ; &envptr16430[1]
  %A9C$_37take = load i64, i64* %envptr16431, align 8                                ; load; *envptr16431
  %cont9657 = call i64 @prim_car(i64 %rvp12260)                                      ; call prim_car
  %rvp12259 = call i64 @prim_cdr(i64 %rvp12260)                                      ; call prim_cdr
  %BVx$lst = call i64 @prim_car(i64 %rvp12259)                                       ; call prim_car
  %rvp12258 = call i64 @prim_cdr(i64 %rvp12259)                                      ; call prim_cdr
  %O3s$n = call i64 @prim_car(i64 %rvp12258)                                         ; call prim_car
  %na12240 = call i64 @prim_cdr(i64 %rvp12258)                                       ; call prim_cdr
  %arg10846 = call i64 @const_init_int(i64 0)                                        ; quoted int
  %a9235 = call i64 @prim__61(i64 %O3s$n, i64 %arg10846)                             ; call prim__61
  %cmp16432 = icmp eq i64 %a9235, 15                                                 ; false?
  br i1 %cmp16432, label %else16434, label %then16433                                ; if

then16433:
  %arg10849 = add i64 0, 0                                                           ; quoted ()
  %arg10848 = add i64 0, 0                                                           ; quoted ()
  %rva12243 = add i64 0, 0                                                           ; quoted ()
  %rva12242 = call i64 @prim_cons(i64 %arg10848, i64 %rva12243)                      ; call prim_cons
  %rva12241 = call i64 @prim_cons(i64 %arg10849, i64 %rva12242)                      ; call prim_cons
  %cloptr16435 = inttoptr i64 %cont9657 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16436 = getelementptr inbounds i64, i64* %cloptr16435, i64 0                 ; &cloptr16435[0]
  %f16438 = load i64, i64* %i0ptr16436, align 8                                      ; load; *i0ptr16436
  %fptr16437 = inttoptr i64 %f16438 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16437(i64 %cont9657, i64 %rva12241)                 ; tail call
  ret void

else16434:
  %a9236 = call i64 @prim_null_63(i64 %BVx$lst)                                      ; call prim_null_63
  %cmp16439 = icmp eq i64 %a9236, 15                                                 ; false?
  br i1 %cmp16439, label %else16441, label %then16440                                ; if

then16440:
  %arg10853 = add i64 0, 0                                                           ; quoted ()
  %arg10852 = add i64 0, 0                                                           ; quoted ()
  %rva12246 = add i64 0, 0                                                           ; quoted ()
  %rva12245 = call i64 @prim_cons(i64 %arg10852, i64 %rva12246)                      ; call prim_cons
  %rva12244 = call i64 @prim_cons(i64 %arg10853, i64 %rva12245)                      ; call prim_cons
  %cloptr16442 = inttoptr i64 %cont9657 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16443 = getelementptr inbounds i64, i64* %cloptr16442, i64 0                 ; &cloptr16442[0]
  %f16445 = load i64, i64* %i0ptr16443, align 8                                      ; load; *i0ptr16443
  %fptr16444 = inttoptr i64 %f16445 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16444(i64 %cont9657, i64 %rva12244)                 ; tail call
  ret void

else16441:
  %a9237 = call i64 @prim_car(i64 %BVx$lst)                                          ; call prim_car
  %a9238 = call i64 @prim_cdr(i64 %BVx$lst)                                          ; call prim_cdr
  %arg10857 = call i64 @const_init_int(i64 1)                                        ; quoted int
  %a9239 = call i64 @prim__45(i64 %O3s$n, i64 %arg10857)                             ; call prim__45
  %cloptr16446 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr16448 = getelementptr inbounds i64, i64* %cloptr16446, i64 1                  ; &eptr16448[1]
  %eptr16449 = getelementptr inbounds i64, i64* %cloptr16446, i64 2                  ; &eptr16449[2]
  store i64 %cont9657, i64* %eptr16448                                               ; *eptr16448 = %cont9657
  store i64 %a9237, i64* %eptr16449                                                  ; *eptr16449 = %a9237
  %eptr16447 = getelementptr inbounds i64, i64* %cloptr16446, i64 0                  ; &cloptr16446[0]
  %f16450 = ptrtoint void(i64,i64)* @lam12432 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16450, i64* %eptr16447                                                 ; store fptr
  %arg10861 = ptrtoint i64* %cloptr16446 to i64                                      ; closure cast; i64* -> i64
  %rva12257 = add i64 0, 0                                                           ; quoted ()
  %rva12256 = call i64 @prim_cons(i64 %a9239, i64 %rva12257)                         ; call prim_cons
  %rva12255 = call i64 @prim_cons(i64 %a9238, i64 %rva12256)                         ; call prim_cons
  %rva12254 = call i64 @prim_cons(i64 %arg10861, i64 %rva12255)                      ; call prim_cons
  %cloptr16451 = inttoptr i64 %A9C$_37take to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr16452 = getelementptr inbounds i64, i64* %cloptr16451, i64 0                 ; &cloptr16451[0]
  %f16454 = load i64, i64* %i0ptr16452, align 8                                      ; load; *i0ptr16452
  %fptr16453 = inttoptr i64 %f16454 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16453(i64 %A9C$_37take, i64 %rva12254)              ; tail call
  ret void
}


define void @lam12432(i64 %env12433, i64 %rvp12253) {
  %envptr16455 = inttoptr i64 %env12433 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16456 = getelementptr inbounds i64, i64* %envptr16455, i64 2                ; &envptr16455[2]
  %a9237 = load i64, i64* %envptr16456, align 8                                      ; load; *envptr16456
  %envptr16457 = inttoptr i64 %env12433 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16458 = getelementptr inbounds i64, i64* %envptr16457, i64 1                ; &envptr16457[1]
  %cont9657 = load i64, i64* %envptr16458, align 8                                   ; load; *envptr16458
  %_959658 = call i64 @prim_car(i64 %rvp12253)                                       ; call prim_car
  %rvp12252 = call i64 @prim_cdr(i64 %rvp12253)                                      ; call prim_cdr
  %a9240 = call i64 @prim_car(i64 %rvp12252)                                         ; call prim_car
  %na12248 = call i64 @prim_cdr(i64 %rvp12252)                                       ; call prim_cdr
  %retprim9659 = call i64 @prim_cons(i64 %a9237, i64 %a9240)                         ; call prim_cons
  %arg10866 = add i64 0, 0                                                           ; quoted ()
  %rva12251 = add i64 0, 0                                                           ; quoted ()
  %rva12250 = call i64 @prim_cons(i64 %retprim9659, i64 %rva12251)                   ; call prim_cons
  %rva12249 = call i64 @prim_cons(i64 %arg10866, i64 %rva12250)                      ; call prim_cons
  %cloptr16459 = inttoptr i64 %cont9657 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16460 = getelementptr inbounds i64, i64* %cloptr16459, i64 0                 ; &cloptr16459[0]
  %f16462 = load i64, i64* %i0ptr16460, align 8                                      ; load; *i0ptr16460
  %fptr16461 = inttoptr i64 %f16462 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16461(i64 %cont9657, i64 %rva12249)                 ; tail call
  ret void
}


define void @lam12419(i64 %env12420, i64 %rvp12303) {
  %cont9660 = call i64 @prim_car(i64 %rvp12303)                                      ; call prim_car
  %rvp12302 = call i64 @prim_cdr(i64 %rvp12303)                                      ; call prim_cdr
  %XuE$_37map = call i64 @prim_car(i64 %rvp12302)                                    ; call prim_car
  %na12272 = call i64 @prim_cdr(i64 %rvp12302)                                       ; call prim_cdr
  %arg10869 = add i64 0, 0                                                           ; quoted ()
  %cloptr16463 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr16465 = getelementptr inbounds i64, i64* %cloptr16463, i64 1                  ; &eptr16465[1]
  store i64 %XuE$_37map, i64* %eptr16465                                             ; *eptr16465 = %XuE$_37map
  %eptr16464 = getelementptr inbounds i64, i64* %cloptr16463, i64 0                  ; &cloptr16463[0]
  %f16466 = ptrtoint void(i64,i64)* @lam12416 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16466, i64* %eptr16464                                                 ; store fptr
  %arg10868 = ptrtoint i64* %cloptr16463 to i64                                      ; closure cast; i64* -> i64
  %rva12301 = add i64 0, 0                                                           ; quoted ()
  %rva12300 = call i64 @prim_cons(i64 %arg10868, i64 %rva12301)                      ; call prim_cons
  %rva12299 = call i64 @prim_cons(i64 %arg10869, i64 %rva12300)                      ; call prim_cons
  %cloptr16467 = inttoptr i64 %cont9660 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16468 = getelementptr inbounds i64, i64* %cloptr16467, i64 0                 ; &cloptr16467[0]
  %f16470 = load i64, i64* %i0ptr16468, align 8                                      ; load; *i0ptr16468
  %fptr16469 = inttoptr i64 %f16470 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16469(i64 %cont9660, i64 %rva12299)                 ; tail call
  ret void
}


define void @lam12416(i64 %env12417, i64 %rvp12298) {
  %envptr16471 = inttoptr i64 %env12417 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16472 = getelementptr inbounds i64, i64* %envptr16471, i64 1                ; &envptr16471[1]
  %XuE$_37map = load i64, i64* %envptr16472, align 8                                 ; load; *envptr16472
  %cont9661 = call i64 @prim_car(i64 %rvp12298)                                      ; call prim_car
  %rvp12297 = call i64 @prim_cdr(i64 %rvp12298)                                      ; call prim_cdr
  %phx$f = call i64 @prim_car(i64 %rvp12297)                                         ; call prim_car
  %rvp12296 = call i64 @prim_cdr(i64 %rvp12297)                                      ; call prim_cdr
  %Fds$lst = call i64 @prim_car(i64 %rvp12296)                                       ; call prim_car
  %na12274 = call i64 @prim_cdr(i64 %rvp12296)                                       ; call prim_cdr
  %a9230 = call i64 @prim_null_63(i64 %Fds$lst)                                      ; call prim_null_63
  %cmp16473 = icmp eq i64 %a9230, 15                                                 ; false?
  br i1 %cmp16473, label %else16475, label %then16474                                ; if

then16474:
  %arg10873 = add i64 0, 0                                                           ; quoted ()
  %arg10872 = add i64 0, 0                                                           ; quoted ()
  %rva12277 = add i64 0, 0                                                           ; quoted ()
  %rva12276 = call i64 @prim_cons(i64 %arg10872, i64 %rva12277)                      ; call prim_cons
  %rva12275 = call i64 @prim_cons(i64 %arg10873, i64 %rva12276)                      ; call prim_cons
  %cloptr16476 = inttoptr i64 %cont9661 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16477 = getelementptr inbounds i64, i64* %cloptr16476, i64 0                 ; &cloptr16476[0]
  %f16479 = load i64, i64* %i0ptr16477, align 8                                      ; load; *i0ptr16477
  %fptr16478 = inttoptr i64 %f16479 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16478(i64 %cont9661, i64 %rva12275)                 ; tail call
  ret void

else16475:
  %a9231 = call i64 @prim_car(i64 %Fds$lst)                                          ; call prim_car
  %cloptr16480 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr16482 = getelementptr inbounds i64, i64* %cloptr16480, i64 1                  ; &eptr16482[1]
  %eptr16483 = getelementptr inbounds i64, i64* %cloptr16480, i64 2                  ; &eptr16483[2]
  %eptr16484 = getelementptr inbounds i64, i64* %cloptr16480, i64 3                  ; &eptr16484[3]
  %eptr16485 = getelementptr inbounds i64, i64* %cloptr16480, i64 4                  ; &eptr16485[4]
  store i64 %Fds$lst, i64* %eptr16482                                                ; *eptr16482 = %Fds$lst
  store i64 %phx$f, i64* %eptr16483                                                  ; *eptr16483 = %phx$f
  store i64 %cont9661, i64* %eptr16484                                               ; *eptr16484 = %cont9661
  store i64 %XuE$_37map, i64* %eptr16485                                             ; *eptr16485 = %XuE$_37map
  %eptr16481 = getelementptr inbounds i64, i64* %cloptr16480, i64 0                  ; &cloptr16480[0]
  %f16486 = ptrtoint void(i64,i64)* @lam12414 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16486, i64* %eptr16481                                                 ; store fptr
  %arg10877 = ptrtoint i64* %cloptr16480 to i64                                      ; closure cast; i64* -> i64
  %rva12295 = add i64 0, 0                                                           ; quoted ()
  %rva12294 = call i64 @prim_cons(i64 %a9231, i64 %rva12295)                         ; call prim_cons
  %rva12293 = call i64 @prim_cons(i64 %arg10877, i64 %rva12294)                      ; call prim_cons
  %cloptr16487 = inttoptr i64 %phx$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr16488 = getelementptr inbounds i64, i64* %cloptr16487, i64 0                 ; &cloptr16487[0]
  %f16490 = load i64, i64* %i0ptr16488, align 8                                      ; load; *i0ptr16488
  %fptr16489 = inttoptr i64 %f16490 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16489(i64 %phx$f, i64 %rva12293)                    ; tail call
  ret void
}


define void @lam12414(i64 %env12415, i64 %rvp12292) {
  %envptr16491 = inttoptr i64 %env12415 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16492 = getelementptr inbounds i64, i64* %envptr16491, i64 4                ; &envptr16491[4]
  %XuE$_37map = load i64, i64* %envptr16492, align 8                                 ; load; *envptr16492
  %envptr16493 = inttoptr i64 %env12415 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16494 = getelementptr inbounds i64, i64* %envptr16493, i64 3                ; &envptr16493[3]
  %cont9661 = load i64, i64* %envptr16494, align 8                                   ; load; *envptr16494
  %envptr16495 = inttoptr i64 %env12415 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16496 = getelementptr inbounds i64, i64* %envptr16495, i64 2                ; &envptr16495[2]
  %phx$f = load i64, i64* %envptr16496, align 8                                      ; load; *envptr16496
  %envptr16497 = inttoptr i64 %env12415 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16498 = getelementptr inbounds i64, i64* %envptr16497, i64 1                ; &envptr16497[1]
  %Fds$lst = load i64, i64* %envptr16498, align 8                                    ; load; *envptr16498
  %_959662 = call i64 @prim_car(i64 %rvp12292)                                       ; call prim_car
  %rvp12291 = call i64 @prim_cdr(i64 %rvp12292)                                      ; call prim_cdr
  %a9232 = call i64 @prim_car(i64 %rvp12291)                                         ; call prim_car
  %na12279 = call i64 @prim_cdr(i64 %rvp12291)                                       ; call prim_cdr
  %a9233 = call i64 @prim_cdr(i64 %Fds$lst)                                          ; call prim_cdr
  %cloptr16499 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr16501 = getelementptr inbounds i64, i64* %cloptr16499, i64 1                  ; &eptr16501[1]
  %eptr16502 = getelementptr inbounds i64, i64* %cloptr16499, i64 2                  ; &eptr16502[2]
  store i64 %a9232, i64* %eptr16501                                                  ; *eptr16501 = %a9232
  store i64 %cont9661, i64* %eptr16502                                               ; *eptr16502 = %cont9661
  %eptr16500 = getelementptr inbounds i64, i64* %cloptr16499, i64 0                  ; &cloptr16499[0]
  %f16503 = ptrtoint void(i64,i64)* @lam12412 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16503, i64* %eptr16500                                                 ; store fptr
  %arg10882 = ptrtoint i64* %cloptr16499 to i64                                      ; closure cast; i64* -> i64
  %rva12290 = add i64 0, 0                                                           ; quoted ()
  %rva12289 = call i64 @prim_cons(i64 %a9233, i64 %rva12290)                         ; call prim_cons
  %rva12288 = call i64 @prim_cons(i64 %phx$f, i64 %rva12289)                         ; call prim_cons
  %rva12287 = call i64 @prim_cons(i64 %arg10882, i64 %rva12288)                      ; call prim_cons
  %cloptr16504 = inttoptr i64 %XuE$_37map to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr16505 = getelementptr inbounds i64, i64* %cloptr16504, i64 0                 ; &cloptr16504[0]
  %f16507 = load i64, i64* %i0ptr16505, align 8                                      ; load; *i0ptr16505
  %fptr16506 = inttoptr i64 %f16507 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16506(i64 %XuE$_37map, i64 %rva12287)               ; tail call
  ret void
}


define void @lam12412(i64 %env12413, i64 %rvp12286) {
  %envptr16508 = inttoptr i64 %env12413 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16509 = getelementptr inbounds i64, i64* %envptr16508, i64 2                ; &envptr16508[2]
  %cont9661 = load i64, i64* %envptr16509, align 8                                   ; load; *envptr16509
  %envptr16510 = inttoptr i64 %env12413 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16511 = getelementptr inbounds i64, i64* %envptr16510, i64 1                ; &envptr16510[1]
  %a9232 = load i64, i64* %envptr16511, align 8                                      ; load; *envptr16511
  %_959663 = call i64 @prim_car(i64 %rvp12286)                                       ; call prim_car
  %rvp12285 = call i64 @prim_cdr(i64 %rvp12286)                                      ; call prim_cdr
  %a9234 = call i64 @prim_car(i64 %rvp12285)                                         ; call prim_car
  %na12281 = call i64 @prim_cdr(i64 %rvp12285)                                       ; call prim_cdr
  %retprim9664 = call i64 @prim_cons(i64 %a9232, i64 %a9234)                         ; call prim_cons
  %arg10887 = add i64 0, 0                                                           ; quoted ()
  %rva12284 = add i64 0, 0                                                           ; quoted ()
  %rva12283 = call i64 @prim_cons(i64 %retprim9664, i64 %rva12284)                   ; call prim_cons
  %rva12282 = call i64 @prim_cons(i64 %arg10887, i64 %rva12283)                      ; call prim_cons
  %cloptr16512 = inttoptr i64 %cont9661 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16513 = getelementptr inbounds i64, i64* %cloptr16512, i64 0                 ; &cloptr16512[0]
  %f16515 = load i64, i64* %i0ptr16513, align 8                                      ; load; *i0ptr16513
  %fptr16514 = inttoptr i64 %f16515 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16514(i64 %cont9661, i64 %rva12282)                 ; tail call
  ret void
}


define void @lam12401(i64 %env12402, i64 %rvp12337) {
  %cont9665 = call i64 @prim_car(i64 %rvp12337)                                      ; call prim_car
  %rvp12336 = call i64 @prim_cdr(i64 %rvp12337)                                      ; call prim_cdr
  %ChW$_37foldr1 = call i64 @prim_car(i64 %rvp12336)                                 ; call prim_car
  %na12310 = call i64 @prim_cdr(i64 %rvp12336)                                       ; call prim_cdr
  %arg10890 = add i64 0, 0                                                           ; quoted ()
  %cloptr16516 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr16518 = getelementptr inbounds i64, i64* %cloptr16516, i64 1                  ; &eptr16518[1]
  store i64 %ChW$_37foldr1, i64* %eptr16518                                          ; *eptr16518 = %ChW$_37foldr1
  %eptr16517 = getelementptr inbounds i64, i64* %cloptr16516, i64 0                  ; &cloptr16516[0]
  %f16519 = ptrtoint void(i64,i64)* @lam12398 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16519, i64* %eptr16517                                                 ; store fptr
  %arg10889 = ptrtoint i64* %cloptr16516 to i64                                      ; closure cast; i64* -> i64
  %rva12335 = add i64 0, 0                                                           ; quoted ()
  %rva12334 = call i64 @prim_cons(i64 %arg10889, i64 %rva12335)                      ; call prim_cons
  %rva12333 = call i64 @prim_cons(i64 %arg10890, i64 %rva12334)                      ; call prim_cons
  %cloptr16520 = inttoptr i64 %cont9665 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16521 = getelementptr inbounds i64, i64* %cloptr16520, i64 0                 ; &cloptr16520[0]
  %f16523 = load i64, i64* %i0ptr16521, align 8                                      ; load; *i0ptr16521
  %fptr16522 = inttoptr i64 %f16523 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16522(i64 %cont9665, i64 %rva12333)                 ; tail call
  ret void
}


define void @lam12398(i64 %env12399, i64 %rvp12332) {
  %envptr16524 = inttoptr i64 %env12399 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16525 = getelementptr inbounds i64, i64* %envptr16524, i64 1                ; &envptr16524[1]
  %ChW$_37foldr1 = load i64, i64* %envptr16525, align 8                              ; load; *envptr16525
  %cont9666 = call i64 @prim_car(i64 %rvp12332)                                      ; call prim_car
  %rvp12331 = call i64 @prim_cdr(i64 %rvp12332)                                      ; call prim_cdr
  %E5o$f = call i64 @prim_car(i64 %rvp12331)                                         ; call prim_car
  %rvp12330 = call i64 @prim_cdr(i64 %rvp12331)                                      ; call prim_cdr
  %Pl7$acc = call i64 @prim_car(i64 %rvp12330)                                       ; call prim_car
  %rvp12329 = call i64 @prim_cdr(i64 %rvp12330)                                      ; call prim_cdr
  %MAj$lst = call i64 @prim_car(i64 %rvp12329)                                       ; call prim_car
  %na12312 = call i64 @prim_cdr(i64 %rvp12329)                                       ; call prim_cdr
  %a9226 = call i64 @prim_null_63(i64 %MAj$lst)                                      ; call prim_null_63
  %cmp16526 = icmp eq i64 %a9226, 15                                                 ; false?
  br i1 %cmp16526, label %else16528, label %then16527                                ; if

then16527:
  %arg10894 = add i64 0, 0                                                           ; quoted ()
  %rva12315 = add i64 0, 0                                                           ; quoted ()
  %rva12314 = call i64 @prim_cons(i64 %Pl7$acc, i64 %rva12315)                       ; call prim_cons
  %rva12313 = call i64 @prim_cons(i64 %arg10894, i64 %rva12314)                      ; call prim_cons
  %cloptr16529 = inttoptr i64 %cont9666 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16530 = getelementptr inbounds i64, i64* %cloptr16529, i64 0                 ; &cloptr16529[0]
  %f16532 = load i64, i64* %i0ptr16530, align 8                                      ; load; *i0ptr16530
  %fptr16531 = inttoptr i64 %f16532 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16531(i64 %cont9666, i64 %rva12313)                 ; tail call
  ret void

else16528:
  %a9227 = call i64 @prim_car(i64 %MAj$lst)                                          ; call prim_car
  %a9228 = call i64 @prim_cdr(i64 %MAj$lst)                                          ; call prim_cdr
  %cloptr16533 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr16535 = getelementptr inbounds i64, i64* %cloptr16533, i64 1                  ; &eptr16535[1]
  %eptr16536 = getelementptr inbounds i64, i64* %cloptr16533, i64 2                  ; &eptr16536[2]
  %eptr16537 = getelementptr inbounds i64, i64* %cloptr16533, i64 3                  ; &eptr16537[3]
  store i64 %E5o$f, i64* %eptr16535                                                  ; *eptr16535 = %E5o$f
  store i64 %cont9666, i64* %eptr16536                                               ; *eptr16536 = %cont9666
  store i64 %a9227, i64* %eptr16537                                                  ; *eptr16537 = %a9227
  %eptr16534 = getelementptr inbounds i64, i64* %cloptr16533, i64 0                  ; &cloptr16533[0]
  %f16538 = ptrtoint void(i64,i64)* @lam12396 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16538, i64* %eptr16534                                                 ; store fptr
  %arg10901 = ptrtoint i64* %cloptr16533 to i64                                      ; closure cast; i64* -> i64
  %rva12328 = add i64 0, 0                                                           ; quoted ()
  %rva12327 = call i64 @prim_cons(i64 %a9228, i64 %rva12328)                         ; call prim_cons
  %rva12326 = call i64 @prim_cons(i64 %Pl7$acc, i64 %rva12327)                       ; call prim_cons
  %rva12325 = call i64 @prim_cons(i64 %E5o$f, i64 %rva12326)                         ; call prim_cons
  %rva12324 = call i64 @prim_cons(i64 %arg10901, i64 %rva12325)                      ; call prim_cons
  %cloptr16539 = inttoptr i64 %ChW$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr16540 = getelementptr inbounds i64, i64* %cloptr16539, i64 0                 ; &cloptr16539[0]
  %f16542 = load i64, i64* %i0ptr16540, align 8                                      ; load; *i0ptr16540
  %fptr16541 = inttoptr i64 %f16542 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16541(i64 %ChW$_37foldr1, i64 %rva12324)            ; tail call
  ret void
}


define void @lam12396(i64 %env12397, i64 %rvp12323) {
  %envptr16543 = inttoptr i64 %env12397 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16544 = getelementptr inbounds i64, i64* %envptr16543, i64 3                ; &envptr16543[3]
  %a9227 = load i64, i64* %envptr16544, align 8                                      ; load; *envptr16544
  %envptr16545 = inttoptr i64 %env12397 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16546 = getelementptr inbounds i64, i64* %envptr16545, i64 2                ; &envptr16545[2]
  %cont9666 = load i64, i64* %envptr16546, align 8                                   ; load; *envptr16546
  %envptr16547 = inttoptr i64 %env12397 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16548 = getelementptr inbounds i64, i64* %envptr16547, i64 1                ; &envptr16547[1]
  %E5o$f = load i64, i64* %envptr16548, align 8                                      ; load; *envptr16548
  %_959667 = call i64 @prim_car(i64 %rvp12323)                                       ; call prim_car
  %rvp12322 = call i64 @prim_cdr(i64 %rvp12323)                                      ; call prim_cdr
  %a9229 = call i64 @prim_car(i64 %rvp12322)                                         ; call prim_car
  %na12317 = call i64 @prim_cdr(i64 %rvp12322)                                       ; call prim_cdr
  %rva12321 = add i64 0, 0                                                           ; quoted ()
  %rva12320 = call i64 @prim_cons(i64 %a9229, i64 %rva12321)                         ; call prim_cons
  %rva12319 = call i64 @prim_cons(i64 %a9227, i64 %rva12320)                         ; call prim_cons
  %rva12318 = call i64 @prim_cons(i64 %cont9666, i64 %rva12319)                      ; call prim_cons
  %cloptr16549 = inttoptr i64 %E5o$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr16550 = getelementptr inbounds i64, i64* %cloptr16549, i64 0                 ; &cloptr16549[0]
  %f16552 = load i64, i64* %i0ptr16550, align 8                                      ; load; *i0ptr16550
  %fptr16551 = inttoptr i64 %f16552 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16551(i64 %E5o$f, i64 %rva12318)                    ; tail call
  ret void
}


define void @lam12388(i64 %env12389, i64 %rvp12370) {
  %cont9669 = call i64 @prim_car(i64 %rvp12370)                                      ; call prim_car
  %rvp12369 = call i64 @prim_cdr(i64 %rvp12370)                                      ; call prim_cdr
  %XbB$y = call i64 @prim_car(i64 %rvp12369)                                         ; call prim_car
  %na12344 = call i64 @prim_cdr(i64 %rvp12369)                                       ; call prim_cdr
  %arg10908 = add i64 0, 0                                                           ; quoted ()
  %cloptr16553 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr16555 = getelementptr inbounds i64, i64* %cloptr16553, i64 1                  ; &eptr16555[1]
  store i64 %XbB$y, i64* %eptr16555                                                  ; *eptr16555 = %XbB$y
  %eptr16554 = getelementptr inbounds i64, i64* %cloptr16553, i64 0                  ; &cloptr16553[0]
  %f16556 = ptrtoint void(i64,i64)* @lam12385 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16556, i64* %eptr16554                                                 ; store fptr
  %arg10907 = ptrtoint i64* %cloptr16553 to i64                                      ; closure cast; i64* -> i64
  %rva12368 = add i64 0, 0                                                           ; quoted ()
  %rva12367 = call i64 @prim_cons(i64 %arg10907, i64 %rva12368)                      ; call prim_cons
  %rva12366 = call i64 @prim_cons(i64 %arg10908, i64 %rva12367)                      ; call prim_cons
  %cloptr16557 = inttoptr i64 %cont9669 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr16558 = getelementptr inbounds i64, i64* %cloptr16557, i64 0                 ; &cloptr16557[0]
  %f16560 = load i64, i64* %i0ptr16558, align 8                                      ; load; *i0ptr16558
  %fptr16559 = inttoptr i64 %f16560 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16559(i64 %cont9669, i64 %rva12366)                 ; tail call
  ret void
}


define void @lam12385(i64 %env12386, i64 %rvp12365) {
  %envptr16561 = inttoptr i64 %env12386 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16562 = getelementptr inbounds i64, i64* %envptr16561, i64 1                ; &envptr16561[1]
  %XbB$y = load i64, i64* %envptr16562, align 8                                      ; load; *envptr16562
  %cont9670 = call i64 @prim_car(i64 %rvp12365)                                      ; call prim_car
  %rvp12364 = call i64 @prim_cdr(i64 %rvp12365)                                      ; call prim_cdr
  %ae4$f = call i64 @prim_car(i64 %rvp12364)                                         ; call prim_car
  %na12346 = call i64 @prim_cdr(i64 %rvp12364)                                       ; call prim_cdr
  %cloptr16563 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr16565 = getelementptr inbounds i64, i64* %cloptr16563, i64 1                  ; &eptr16565[1]
  %eptr16566 = getelementptr inbounds i64, i64* %cloptr16563, i64 2                  ; &eptr16566[2]
  store i64 %XbB$y, i64* %eptr16565                                                  ; *eptr16565 = %XbB$y
  store i64 %ae4$f, i64* %eptr16566                                                  ; *eptr16566 = %ae4$f
  %eptr16564 = getelementptr inbounds i64, i64* %cloptr16563, i64 0                  ; &cloptr16563[0]
  %f16567 = ptrtoint void(i64,i64)* @lam12383 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16567, i64* %eptr16564                                                 ; store fptr
  %arg10910 = ptrtoint i64* %cloptr16563 to i64                                      ; closure cast; i64* -> i64
  %rva12363 = add i64 0, 0                                                           ; quoted ()
  %rva12362 = call i64 @prim_cons(i64 %arg10910, i64 %rva12363)                      ; call prim_cons
  %rva12361 = call i64 @prim_cons(i64 %cont9670, i64 %rva12362)                      ; call prim_cons
  %cloptr16568 = inttoptr i64 %ae4$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr16569 = getelementptr inbounds i64, i64* %cloptr16568, i64 0                 ; &cloptr16568[0]
  %f16571 = load i64, i64* %i0ptr16569, align 8                                      ; load; *i0ptr16569
  %fptr16570 = inttoptr i64 %f16571 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16570(i64 %ae4$f, i64 %rva12361)                    ; tail call
  ret void
}


define void @lam12383(i64 %env12384, i64 %W9n$args9672) {
  %envptr16572 = inttoptr i64 %env12384 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16573 = getelementptr inbounds i64, i64* %envptr16572, i64 2                ; &envptr16572[2]
  %ae4$f = load i64, i64* %envptr16573, align 8                                      ; load; *envptr16573
  %envptr16574 = inttoptr i64 %env12384 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16575 = getelementptr inbounds i64, i64* %envptr16574, i64 1                ; &envptr16574[1]
  %XbB$y = load i64, i64* %envptr16575, align 8                                      ; load; *envptr16575
  %cont9671 = call i64 @prim_car(i64 %W9n$args9672)                                  ; call prim_car
  %W9n$args = call i64 @prim_cdr(i64 %W9n$args9672)                                  ; call prim_cdr
  %cloptr16576 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr16578 = getelementptr inbounds i64, i64* %cloptr16576, i64 1                  ; &eptr16578[1]
  %eptr16579 = getelementptr inbounds i64, i64* %cloptr16576, i64 2                  ; &eptr16579[2]
  %eptr16580 = getelementptr inbounds i64, i64* %cloptr16576, i64 3                  ; &eptr16580[3]
  store i64 %cont9671, i64* %eptr16578                                               ; *eptr16578 = %cont9671
  store i64 %ae4$f, i64* %eptr16579                                                  ; *eptr16579 = %ae4$f
  store i64 %W9n$args, i64* %eptr16580                                               ; *eptr16580 = %W9n$args
  %eptr16577 = getelementptr inbounds i64, i64* %cloptr16576, i64 0                  ; &cloptr16576[0]
  %f16581 = ptrtoint void(i64,i64)* @lam12381 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16581, i64* %eptr16577                                                 ; store fptr
  %arg10916 = ptrtoint i64* %cloptr16576 to i64                                      ; closure cast; i64* -> i64
  %rva12360 = add i64 0, 0                                                           ; quoted ()
  %rva12359 = call i64 @prim_cons(i64 %XbB$y, i64 %rva12360)                         ; call prim_cons
  %rva12358 = call i64 @prim_cons(i64 %arg10916, i64 %rva12359)                      ; call prim_cons
  %cloptr16582 = inttoptr i64 %XbB$y to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr16583 = getelementptr inbounds i64, i64* %cloptr16582, i64 0                 ; &cloptr16582[0]
  %f16585 = load i64, i64* %i0ptr16583, align 8                                      ; load; *i0ptr16583
  %fptr16584 = inttoptr i64 %f16585 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16584(i64 %XbB$y, i64 %rva12358)                    ; tail call
  ret void
}


define void @lam12381(i64 %env12382, i64 %rvp12357) {
  %envptr16586 = inttoptr i64 %env12382 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16587 = getelementptr inbounds i64, i64* %envptr16586, i64 3                ; &envptr16586[3]
  %W9n$args = load i64, i64* %envptr16587, align 8                                   ; load; *envptr16587
  %envptr16588 = inttoptr i64 %env12382 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16589 = getelementptr inbounds i64, i64* %envptr16588, i64 2                ; &envptr16588[2]
  %ae4$f = load i64, i64* %envptr16589, align 8                                      ; load; *envptr16589
  %envptr16590 = inttoptr i64 %env12382 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16591 = getelementptr inbounds i64, i64* %envptr16590, i64 1                ; &envptr16590[1]
  %cont9671 = load i64, i64* %envptr16591, align 8                                   ; load; *envptr16591
  %_959673 = call i64 @prim_car(i64 %rvp12357)                                       ; call prim_car
  %rvp12356 = call i64 @prim_cdr(i64 %rvp12357)                                      ; call prim_cdr
  %a9224 = call i64 @prim_car(i64 %rvp12356)                                         ; call prim_car
  %na12348 = call i64 @prim_cdr(i64 %rvp12356)                                       ; call prim_cdr
  %cloptr16592 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr16594 = getelementptr inbounds i64, i64* %cloptr16592, i64 1                  ; &eptr16594[1]
  %eptr16595 = getelementptr inbounds i64, i64* %cloptr16592, i64 2                  ; &eptr16595[2]
  store i64 %cont9671, i64* %eptr16594                                               ; *eptr16594 = %cont9671
  store i64 %W9n$args, i64* %eptr16595                                               ; *eptr16595 = %W9n$args
  %eptr16593 = getelementptr inbounds i64, i64* %cloptr16592, i64 0                  ; &cloptr16592[0]
  %f16596 = ptrtoint void(i64,i64)* @lam12379 to i64                                 ; fptr cast; i64(...)* -> i64
  store i64 %f16596, i64* %eptr16593                                                 ; store fptr
  %arg10919 = ptrtoint i64* %cloptr16592 to i64                                      ; closure cast; i64* -> i64
  %rva12355 = add i64 0, 0                                                           ; quoted ()
  %rva12354 = call i64 @prim_cons(i64 %ae4$f, i64 %rva12355)                         ; call prim_cons
  %rva12353 = call i64 @prim_cons(i64 %arg10919, i64 %rva12354)                      ; call prim_cons
  %cloptr16597 = inttoptr i64 %a9224 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr16598 = getelementptr inbounds i64, i64* %cloptr16597, i64 0                 ; &cloptr16597[0]
  %f16600 = load i64, i64* %i0ptr16598, align 8                                      ; load; *i0ptr16598
  %fptr16599 = inttoptr i64 %f16600 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16599(i64 %a9224, i64 %rva12353)                    ; tail call
  ret void
}


define void @lam12379(i64 %env12380, i64 %rvp12352) {
  %envptr16601 = inttoptr i64 %env12380 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16602 = getelementptr inbounds i64, i64* %envptr16601, i64 2                ; &envptr16601[2]
  %W9n$args = load i64, i64* %envptr16602, align 8                                   ; load; *envptr16602
  %envptr16603 = inttoptr i64 %env12380 to i64*                                      ; closure/env cast; i64 -> i64*
  %envptr16604 = getelementptr inbounds i64, i64* %envptr16603, i64 1                ; &envptr16603[1]
  %cont9671 = load i64, i64* %envptr16604, align 8                                   ; load; *envptr16604
  %_959674 = call i64 @prim_car(i64 %rvp12352)                                       ; call prim_car
  %rvp12351 = call i64 @prim_cdr(i64 %rvp12352)                                      ; call prim_cdr
  %a9225 = call i64 @prim_car(i64 %rvp12351)                                         ; call prim_car
  %na12350 = call i64 @prim_cdr(i64 %rvp12351)                                       ; call prim_cdr
  %cps_45lst9675 = call i64 @prim_cons(i64 %cont9671, i64 %W9n$args)                 ; call prim_cons
  %cloptr16605 = inttoptr i64 %a9225 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr16606 = getelementptr inbounds i64, i64* %cloptr16605, i64 0                 ; &cloptr16605[0]
  %f16608 = load i64, i64* %i0ptr16606, align 8                                      ; load; *i0ptr16606
  %fptr16607 = inttoptr i64 %f16608 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr16607(i64 %a9225, i64 %cps_45lst9675)               ; tail call
  ret void
}





@sym14142 = private unnamed_addr constant [10 x i8] c"%%promise\00", align 8
@sym15091 = private unnamed_addr constant [12 x i8] c"no-solution\00", align 8
@sym15864 = private unnamed_addr constant [9 x i8] c"solution\00", align 8
