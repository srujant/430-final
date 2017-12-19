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
