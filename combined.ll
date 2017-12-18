; ModuleID = 'header.cpp'
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%"class.std::ios_base::Init" = type { i8 }

@_ZStL8__ioinit = internal global %"class.std::ios_base::Init" zeroinitializer, align 1
@__dso_handle = external global i8
@.str = private unnamed_addr constant [25 x i8] c"library run-time error: \00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"%s\00", align 1
@.str.2 = private unnamed_addr constant [2 x i8] c"\0A\00", align 1
@.str.3 = private unnamed_addr constant [5 x i8] c"%lu\0A\00", align 1
@.str.4 = private unnamed_addr constant [68 x i8] c"Expected value: null (in expect_args0). Prim cannot take arguments.\00", align 1
@.str.5 = private unnamed_addr constant [79 x i8] c"Expected cons value (in expect_args1). Prim applied on an empty argument list.\00", align 1
@.str.6 = private unnamed_addr constant [70 x i8] c"Expected null value (in expect_args1). Prim can only take 1 argument.\00", align 1
@.str.7 = private unnamed_addr constant [37 x i8] c"Expected a cons value. (expect_cons)\00", align 1
@.str.8 = private unnamed_addr constant [51 x i8] c"Expected a vector or special value. (expect_other)\00", align 1
@.str.9 = private unnamed_addr constant [3 x i8] c"()\00", align 1
@.str.10 = private unnamed_addr constant [13 x i8] c"#<procedure>\00", align 1
@.str.11 = private unnamed_addr constant [2 x i8] c"(\00", align 1
@.str.12 = private unnamed_addr constant [4 x i8] c" . \00", align 1
@.str.13 = private unnamed_addr constant [2 x i8] c")\00", align 1
@.str.14 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@.str.15 = private unnamed_addr constant [5 x i8] c"\22%s\22\00", align 1
@.str.16 = private unnamed_addr constant [3 x i8] c"#(\00", align 1
@.str.17 = private unnamed_addr constant [2 x i8] c",\00", align 1
@.str.18 = private unnamed_addr constant [36 x i8] c"(print.. v); unrecognized value %lu\00", align 1
@.str.19 = private unnamed_addr constant [4 x i8] c"'()\00", align 1
@.str.20 = private unnamed_addr constant [3 x i8] c"'(\00", align 1
@.str.21 = private unnamed_addr constant [4 x i8] c"'%s\00", align 1
@.str.22 = private unnamed_addr constant [34 x i8] c"(print v); unrecognized value %lu\00", align 1
@.str.23 = private unnamed_addr constant [49 x i8] c"first argument to make-vector must be an integer\00", align 1
@.str.24 = private unnamed_addr constant [39 x i8] c"prim applied on more than 2 arguments.\00", align 1
@.str.25 = private unnamed_addr constant [49 x i8] c"second argument to vector-ref must be an integer\00", align 1
@.str.26 = private unnamed_addr constant [46 x i8] c"first argument to vector-ref must be a vector\00", align 1
@.str.27 = private unnamed_addr constant [46 x i8] c"vector-ref not given a properly formed vector\00", align 1
@.str.28 = private unnamed_addr constant [48 x i8] c"first argument to vector-ref must be an integer\00", align 1
@.str.29 = private unnamed_addr constant [34 x i8] c"(prim + a b); a is not an integer\00", align 1
@.str.30 = private unnamed_addr constant [34 x i8] c"(prim + a b); b is not an integer\00", align 1
@.str.31 = private unnamed_addr constant [36 x i8] c"Tried to apply + on non list value.\00", align 1
@.str.32 = private unnamed_addr constant [34 x i8] c"(prim - a b); b is not an integer\00", align 1
@.str.33 = private unnamed_addr constant [34 x i8] c"(prim * a b); a is not an integer\00", align 1
@.str.34 = private unnamed_addr constant [34 x i8] c"(prim * a b); b is not an integer\00", align 1
@.str.35 = private unnamed_addr constant [34 x i8] c"(prim / a b); a is not an integer\00", align 1
@.str.36 = private unnamed_addr constant [34 x i8] c"(prim / a b); b is not an integer\00", align 1
@.str.37 = private unnamed_addr constant [41 x i8] c"Division by 0 error. Cannot divide by 0.\00", align 1
@.str.38 = private unnamed_addr constant [34 x i8] c"(prim = a b); a is not an integer\00", align 1
@.str.39 = private unnamed_addr constant [34 x i8] c"(prim = a b); b is not an integer\00", align 1
@.str.40 = private unnamed_addr constant [34 x i8] c"(prim < a b); a is not an integer\00", align 1
@.str.41 = private unnamed_addr constant [34 x i8] c"(prim < a b); b is not an integer\00", align 1
@.str.42 = private unnamed_addr constant [35 x i8] c"(prim <= a b); a is not an integer\00", align 1
@.str.43 = private unnamed_addr constant [35 x i8] c"(prim <= a b); b is not an integer\00", align 1
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

; Function Attrs: nounwind uwtable
define i64* @alloc(i64 %m) #3 {
  %1 = alloca i64, align 8
  store i64 %m, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call noalias i8* @malloc(i64 %2) #2
  %4 = bitcast i8* %3 to i64*
  ret i64* %4
}

; Function Attrs: nounwind
declare noalias i8* @malloc(i64) #4

; Function Attrs: uwtable
define void @fatal_err(i8* %msg) #0 {
  %1 = alloca i8*, align 8
  store i8* %msg, i8** %1, align 8
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([25 x i8], [25 x i8]* @.str, i32 0, i32 0))
  %3 = load i8*, i8** %1, align 8
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.1, i32 0, i32 0), i8* %3)
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.2, i32 0, i32 0))
  call void @exit(i32 1) #7
  unreachable
                                                  ; No predecessors!
  ret void
}

declare i32 @printf(i8*, ...) #1

; Function Attrs: noreturn nounwind
declare void @exit(i32) #5

; Function Attrs: uwtable
define void @print_u64(i64 %i) #0 {
  %1 = alloca i64, align 8
  store i64 %i, i64* %1, align 8
  %2 = load i64, i64* %1, align 8
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.3, i32 0, i32 0), i64 %2)
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
  call void @fatal_err(i8* getelementptr inbounds ([68 x i8], [68 x i8]* @.str.4, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([79 x i8], [79 x i8]* @.str.5, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([70 x i8], [70 x i8]* @.str.6, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([37 x i8], [37 x i8]* @.str.7, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([51 x i8], [51 x i8]* @.str.8, i32 0, i32 0))
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
define i64 @const_init_int(i64 %i) #3 {
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
define i64 @const_init_void() #3 {
  ret i64 39
}

; Function Attrs: nounwind uwtable
define i64 @const_init_null() #3 {
  ret i64 0
}

; Function Attrs: nounwind uwtable
define i64 @const_init_true() #3 {
  ret i64 31
}

; Function Attrs: nounwind uwtable
define i64 @const_init_false() #3 {
  ret i64 15
}

; Function Attrs: nounwind uwtable
define i64 @const_init_string(i8* %s) #3 {
  %1 = alloca i8*, align 8
  store i8* %s, i8** %1, align 8
  %2 = load i8*, i8** %1, align 8
  %3 = ptrtoint i8* %2 to i64
  %4 = or i64 %3, 3
  ret i64 %4
}

; Function Attrs: nounwind uwtable
define i64 @const_init_symbol(i8* %s) #3 {
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
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.9, i32 0, i32 0))
  br label %108

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 7
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %10, label %12

; <label>:10                                      ; preds = %6
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.10, i32 0, i32 0))
  br label %107

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
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.11, i32 0, i32 0))
  %21 = load i64*, i64** %p, align 8
  %22 = getelementptr inbounds i64, i64* %21, i64 0
  %23 = load i64, i64* %22, align 8
  %24 = call i64 @prim_print_aux(i64 %23)
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.12, i32 0, i32 0))
  %26 = load i64*, i64** %p, align 8
  %27 = getelementptr inbounds i64, i64* %26, i64 1
  %28 = load i64, i64* %27, align 8
  %29 = call i64 @prim_print_aux(i64 %28)
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.13, i32 0, i32 0))
  br label %106

; <label>:31                                      ; preds = %12
  %32 = load i64, i64* %1, align 8
  %33 = and i64 %32, 7
  %34 = icmp eq i64 %33, 2
  br i1 %34, label %35, label %40

; <label>:35                                      ; preds = %31
  %36 = load i64, i64* %1, align 8
  %37 = lshr i64 %36, 32
  %38 = trunc i64 %37 to i32
  %39 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.14, i32 0, i32 0), i32 %38)
  br label %105

; <label>:40                                      ; preds = %31
  %41 = load i64, i64* %1, align 8
  %42 = and i64 %41, 7
  %43 = icmp eq i64 %42, 3
  br i1 %43, label %44, label %49

; <label>:44                                      ; preds = %40
  %45 = load i64, i64* %1, align 8
  %46 = and i64 %45, -8
  %47 = inttoptr i64 %46 to i8*
  %48 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.15, i32 0, i32 0), i8* %47)
  br label %104

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
  br label %103

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
  %71 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.16, i32 0, i32 0))
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
  %88 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.17, i32 0, i32 0))
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
  %98 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.13, i32 0, i32 0))
  br label %102

; <label>:99                                      ; preds = %62, %58
  %100 = load i64, i64* %1, align 8
  %101 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.18, i32 0, i32 0), i64 %100)
  br label %102

; <label>:102                                     ; preds = %99, %97
  br label %103

; <label>:103                                     ; preds = %102, %53
  br label %104

; <label>:104                                     ; preds = %103, %44
  br label %105

; <label>:105                                     ; preds = %104, %35
  br label %106

; <label>:106                                     ; preds = %105, %16
  br label %107

; <label>:107                                     ; preds = %106, %10
  br label %108

; <label>:108                                     ; preds = %107, %4
  ret i64 39
}

; Function Attrs: uwtable
define i64 @prim_print(i64 %v) #0 {
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
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.19, i32 0, i32 0))
  br label %108

; <label>:6                                       ; preds = %0
  %7 = load i64, i64* %1, align 8
  %8 = and i64 %7, 7
  %9 = icmp eq i64 %8, 0
  br i1 %9, label %10, label %12

; <label>:10                                      ; preds = %6
  %11 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([13 x i8], [13 x i8]* @.str.10, i32 0, i32 0))
  br label %107

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
  %20 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.20, i32 0, i32 0))
  %21 = load i64*, i64** %p, align 8
  %22 = getelementptr inbounds i64, i64* %21, i64 0
  %23 = load i64, i64* %22, align 8
  %24 = call i64 @prim_print_aux(i64 %23)
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.12, i32 0, i32 0))
  %26 = load i64*, i64** %p, align 8
  %27 = getelementptr inbounds i64, i64* %26, i64 1
  %28 = load i64, i64* %27, align 8
  %29 = call i64 @prim_print_aux(i64 %28)
  %30 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.13, i32 0, i32 0))
  br label %106

; <label>:31                                      ; preds = %12
  %32 = load i64, i64* %1, align 8
  %33 = and i64 %32, 7
  %34 = icmp eq i64 %33, 2
  br i1 %34, label %35, label %40

; <label>:35                                      ; preds = %31
  %36 = load i64, i64* %1, align 8
  %37 = lshr i64 %36, 32
  %38 = trunc i64 %37 to i32
  %39 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.14, i32 0, i32 0), i32 %38)
  br label %105

; <label>:40                                      ; preds = %31
  %41 = load i64, i64* %1, align 8
  %42 = and i64 %41, 7
  %43 = icmp eq i64 %42, 3
  br i1 %43, label %44, label %49

; <label>:44                                      ; preds = %40
  %45 = load i64, i64* %1, align 8
  %46 = and i64 %45, -8
  %47 = inttoptr i64 %46 to i8*
  %48 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str.15, i32 0, i32 0), i8* %47)
  br label %104

; <label>:49                                      ; preds = %40
  %50 = load i64, i64* %1, align 8
  %51 = and i64 %50, 7
  %52 = icmp eq i64 %51, 4
  br i1 %52, label %53, label %58

; <label>:53                                      ; preds = %49
  %54 = load i64, i64* %1, align 8
  %55 = and i64 %54, -8
  %56 = inttoptr i64 %55 to i8*
  %57 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.21, i32 0, i32 0), i8* %56)
  br label %103

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
  %71 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.16, i32 0, i32 0))
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
  %88 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.17, i32 0, i32 0))
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
  %98 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.13, i32 0, i32 0))
  br label %102

; <label>:99                                      ; preds = %62, %58
  %100 = load i64, i64* %1, align 8
  %101 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.22, i32 0, i32 0), i64 %100)
  br label %102

; <label>:102                                     ; preds = %99, %97
  br label %103

; <label>:103                                     ; preds = %102, %53
  br label %104

; <label>:104                                     ; preds = %103, %44
  br label %105

; <label>:105                                     ; preds = %104, %35
  br label %106

; <label>:106                                     ; preds = %105, %16
  br label %107

; <label>:107                                     ; preds = %106, %10
  br label %108

; <label>:108                                     ; preds = %107, %4
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
  call void @exit(i32 0) #7
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
  store i64 0, i64* %l, align 8
  br label %4

; <label>:4                                       ; preds = %13, %0
  %5 = load i64, i64* %1, align 8
  %6 = and i64 %5, 7
  %7 = icmp eq i64 %6, 1
  br i1 %7, label %8, label %11

; <label>:8                                       ; preds = %4
  %9 = load i64, i64* %l, align 8
  %10 = icmp ult i64 %9, 512
  br label %11

; <label>:11                                      ; preds = %8, %4
  %12 = phi i1 [ false, %4 ], [ %10, %8 ]
  br i1 %12, label %13, label %20

; <label>:13                                      ; preds = %11
  %14 = load i64, i64* %1, align 8
  %15 = call i64 @expect_cons(i64 %14, i64* %1)
  %16 = load i64, i64* %l, align 8
  %17 = add i64 %16, 1
  store i64 %17, i64* %l, align 8
  %18 = load i64*, i64** %buffer, align 8
  %19 = getelementptr inbounds i64, i64* %18, i64 %16
  store i64 %15, i64* %19, align 8
  br label %4

; <label>:20                                      ; preds = %11
  %21 = load i64, i64* %l, align 8
  %22 = add i64 %21, 1
  %23 = mul i64 %22, 8
  %24 = call i64* @alloc(i64 %23)
  store i64* %24, i64** %mem, align 8
  %25 = load i64, i64* %l, align 8
  %26 = shl i64 %25, 3
  %27 = or i64 %26, 1
  %28 = load i64*, i64** %mem, align 8
  %29 = getelementptr inbounds i64, i64* %28, i64 0
  store i64 %27, i64* %29, align 8
  store i64 0, i64* %i, align 8
  br label %30

; <label>:30                                      ; preds = %43, %20
  %31 = load i64, i64* %i, align 8
  %32 = load i64, i64* %l, align 8
  %33 = icmp ult i64 %31, %32
  br i1 %33, label %34, label %46

; <label>:34                                      ; preds = %30
  %35 = load i64, i64* %i, align 8
  %36 = load i64*, i64** %buffer, align 8
  %37 = getelementptr inbounds i64, i64* %36, i64 %35
  %38 = load i64, i64* %37, align 8
  %39 = load i64, i64* %i, align 8
  %40 = add i64 %39, 1
  %41 = load i64*, i64** %mem, align 8
  %42 = getelementptr inbounds i64, i64* %41, i64 %40
  store i64 %38, i64* %42, align 8
  br label %43

; <label>:43                                      ; preds = %34
  %44 = load i64, i64* %i, align 8
  %45 = add i64 %44, 1
  store i64 %45, i64* %i, align 8
  br label %30

; <label>:46                                      ; preds = %30
  %47 = load i64*, i64** %buffer, align 8
  %48 = icmp eq i64* %47, null
  br i1 %48, label %51, label %49

; <label>:49                                      ; preds = %46
  %50 = bitcast i64* %47 to i8*
  call void @_ZdaPv(i8* %50) #8
  br label %51

; <label>:51                                      ; preds = %49, %46
  %52 = load i64*, i64** %mem, align 8
  %53 = ptrtoint i64* %52 to i64
  %54 = or i64 %53, 6
  ret i64 %54
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
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.23, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.24, i32 0, i32 0))
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
  store i64 %v, i64* %1, align 8
  store i64 %i, i64* %2, align 8
  %3 = load i64, i64* %2, align 8
  %4 = and i64 %3, 7
  %5 = icmp ne i64 %4, 2
  br i1 %5, label %6, label %7

; <label>:6                                       ; preds = %0
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.25, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %1, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 6
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.26, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.27, i32 0, i32 0))
  br label %21

; <label>:21                                      ; preds = %20, %12
  %22 = load i64, i64* %2, align 8
  %23 = and i64 %22, -8
  %24 = lshr i64 %23, 32
  %25 = trunc i64 %24 to i32
  %26 = add nsw i32 1, %25
  %27 = sext i32 %26 to i64
  %28 = load i64, i64* %1, align 8
  %29 = and i64 %28, -8
  %30 = inttoptr i64 %29 to i64*
  %31 = getelementptr inbounds i64, i64* %30, i64 %27
  %32 = load i64, i64* %31, align 8
  ret i64 %32
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
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.24, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([49 x i8], [49 x i8]* @.str.25, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %1, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 6
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([48 x i8], [48 x i8]* @.str.28, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([46 x i8], [46 x i8]* @.str.27, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.24, i32 0, i32 0))
  br label %11

; <label>:11                                      ; preds = %10, %0
  %12 = load i64, i64* %v0, align 8
  %13 = load i64, i64* %v1, align 8
  %14 = load i64, i64* %v2, align 8
  %15 = call i64 @prim_vector_45set_33(i64 %12, i64 %13, i64 %14)
  ret i64 %15
}

; Function Attrs: nounwind uwtable
define i64 @prim_void() #3 {
  ret i64 39
}

; Function Attrs: nounwind uwtable
define i64 @prim_eq_63(i64 %a, i64 %b) #3 {
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
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.24, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_eq_63(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: nounwind uwtable
define i64 @prim_eqv_63(i64 %a, i64 %b) #3 {
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
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.24, i32 0, i32 0))
  br label %9

; <label>:9                                       ; preds = %8, %0
  %10 = load i64, i64* %v0, align 8
  %11 = load i64, i64* %v1, align 8
  %12 = call i64 @prim_eqv_63(i64 %10, i64 %11)
  ret i64 %12
}

; Function Attrs: nounwind uwtable
define i64 @prim_number_63(i64 %a) #3 {
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
define i64 @prim_integer_63(i64 %a) #3 {
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
define i64 @prim_void_63(i64 %a) #3 {
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
define i64 @prim_procedure_63(i64 %a) #3 {
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
define i64 @prim_null_63(i64 %p) #3 {
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
define i64 @prim_cons_63(i64 %p) #3 {
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

; Function Attrs: nounwind uwtable
define i64 @prim_cons(i64 %a, i64 %b) #3 {
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
  call void @fatal_err(i8* getelementptr inbounds ([39 x i8], [39 x i8]* @.str.24, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.29, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.30, i32 0, i32 0))
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
  %21 = add nsw i32 %16, %20
  %22 = zext i32 %21 to i64
  %23 = shl i64 %22, 32
  %24 = or i64 %23, 2
  ret i64 %24
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
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.31, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.29, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.32, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.31, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.33, i32 0, i32 0))
  br label %7

; <label>:7                                       ; preds = %6, %0
  %8 = load i64, i64* %2, align 8
  %9 = and i64 %8, 7
  %10 = icmp ne i64 %9, 2
  br i1 %10, label %11, label %12

; <label>:11                                      ; preds = %7
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.34, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([36 x i8], [36 x i8]* @.str.31, i32 0, i32 0))
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
  %13 = load i64, i64* %2, align 8
  %14 = icmp eq i64 %13, 2
  br i1 %14, label %15, label %16

; <label>:15                                      ; preds = %12
  call void @fatal_err(i8* getelementptr inbounds ([41 x i8], [41 x i8]* @.str.37, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.38, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.39, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.40, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([34 x i8], [34 x i8]* @.str.41, i32 0, i32 0))
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
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.42, i32 0, i32 0))
  br label %8

; <label>:8                                       ; preds = %7, %0
  %9 = load i64, i64* %3, align 8
  %10 = and i64 %9, 7
  %11 = icmp ne i64 %10, 2
  br i1 %11, label %12, label %13

; <label>:12                                      ; preds = %8
  call void @fatal_err(i8* getelementptr inbounds ([35 x i8], [35 x i8]* @.str.43, i32 0, i32 0))
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
define i64 @prim_not(i64 %a) #3 {
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
define internal void @_GLOBAL__sub_I_header.cpp() #0 section ".text.startup" {
  call void @__cxx_global_var_init()
  ret void
}

attributes #0 = { uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { nounwind }
attributes #3 = { nounwind uwtable "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { noreturn nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { nobuiltin nounwind "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { noreturn nounwind }
attributes #8 = { builtin nounwind }

!llvm.ident = !{!0}

!0 = !{!"clang version 3.8.0-2ubuntu4 (tags/RELEASE_380/final)"}


;;;;;;

define void @proc_main() {
  %cloptr9199 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9200 = getelementptr inbounds i64, i64* %cloptr9199, i64 0                    ; &cloptr9199[0]
  %f9201 = ptrtoint void(i64,i64,i64)* @lam9197 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9201, i64* %eptr9200                                                   ; store fptr
  %arg7784 = ptrtoint i64* %cloptr9199 to i64                                        ; closure cast; i64* -> i64
  %cloptr9202 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9203 = getelementptr inbounds i64, i64* %cloptr9202, i64 0                    ; &cloptr9202[0]
  %f9204 = ptrtoint void(i64,i64,i64)* @lam9195 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9204, i64* %eptr9203                                                   ; store fptr
  %arg7783 = ptrtoint i64* %cloptr9202 to i64                                        ; closure cast; i64* -> i64
  %cloptr9205 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9206 = getelementptr inbounds i64, i64* %cloptr9205, i64 0                    ; &cloptr9205[0]
  %f9207 = ptrtoint void(i64,i64,i64)* @lam8713 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9207, i64* %eptr9206                                                   ; store fptr
  %arg7782 = ptrtoint i64* %cloptr9205 to i64                                        ; closure cast; i64* -> i64
  %cloptr9208 = inttoptr i64 %arg7784 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9209 = getelementptr inbounds i64, i64* %cloptr9208, i64 0                   ; &cloptr9208[0]
  %f9211 = load i64, i64* %i0ptr9209, align 8                                        ; load; *i0ptr9209
  %fptr9210 = inttoptr i64 %f9211 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9210(i64 %arg7784, i64 %arg7783, i64 %arg7782)      ; tail call
  ret void
}


define i32 @main() {
  call fastcc void @proc_main()
  ret i32 0
}



define void @lam9197(i64 %env9198, i64 %cont7774, i64 %RxT$yu) {
  %cloptr9212 = inttoptr i64 %RxT$yu to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr9213 = getelementptr inbounds i64, i64* %cloptr9212, i64 0                   ; &cloptr9212[0]
  %f9215 = load i64, i64* %i0ptr9213, align 8                                        ; load; *i0ptr9213
  %fptr9214 = inttoptr i64 %f9215 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9214(i64 %RxT$yu, i64 %cont7774, i64 %RxT$yu)       ; tail call
  ret void
}


define void @lam9195(i64 %env9196, i64 %_957581, i64 %PM2$Ycmb) {
  %cloptr9216 = call i64* @alloc(i64 16)                                             ; malloc
  %eptr9218 = getelementptr inbounds i64, i64* %cloptr9216, i64 1                    ; &eptr9218[1]
  store i64 %PM2$Ycmb, i64* %eptr9218                                                ; *eptr9218 = %PM2$Ycmb
  %eptr9217 = getelementptr inbounds i64, i64* %cloptr9216, i64 0                    ; &cloptr9216[0]
  %f9219 = ptrtoint void(i64,i64,i64)* @lam9193 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9219, i64* %eptr9217                                                   ; store fptr
  %arg7789 = ptrtoint i64* %cloptr9216 to i64                                        ; closure cast; i64* -> i64
  %cloptr9220 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9221 = getelementptr inbounds i64, i64* %cloptr9220, i64 0                    ; &cloptr9220[0]
  %f9222 = ptrtoint void(i64,i64,i64)* @lam8721 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9222, i64* %eptr9221                                                   ; store fptr
  %arg7788 = ptrtoint i64* %cloptr9220 to i64                                        ; closure cast; i64* -> i64
  %cloptr9223 = inttoptr i64 %PM2$Ycmb to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9224 = getelementptr inbounds i64, i64* %cloptr9223, i64 0                   ; &cloptr9223[0]
  %f9226 = load i64, i64* %i0ptr9224, align 8                                        ; load; *i0ptr9224
  %fptr9225 = inttoptr i64 %f9226 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9225(i64 %PM2$Ycmb, i64 %arg7789, i64 %arg7788)     ; tail call
  ret void
}


define void @lam9193(i64 %env9194, i64 %_957582, i64 %rqD$_37foldr1) {
  %envptr9227 = inttoptr i64 %env9194 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9228 = getelementptr inbounds i64, i64* %envptr9227, i64 1                  ; &envptr9227[1]
  %PM2$Ycmb = load i64, i64* %envptr9228, align 8                                    ; load; *envptr9228
  %cloptr9229 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9231 = getelementptr inbounds i64, i64* %cloptr9229, i64 1                    ; &eptr9231[1]
  %eptr9232 = getelementptr inbounds i64, i64* %cloptr9229, i64 2                    ; &eptr9232[2]
  store i64 %PM2$Ycmb, i64* %eptr9231                                                ; *eptr9231 = %PM2$Ycmb
  store i64 %rqD$_37foldr1, i64* %eptr9232                                           ; *eptr9232 = %rqD$_37foldr1
  %eptr9230 = getelementptr inbounds i64, i64* %cloptr9229, i64 0                    ; &cloptr9229[0]
  %f9233 = ptrtoint void(i64,i64,i64)* @lam9191 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9233, i64* %eptr9230                                                   ; store fptr
  %arg7792 = ptrtoint i64* %cloptr9229 to i64                                        ; closure cast; i64* -> i64
  %cloptr9234 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9235 = getelementptr inbounds i64, i64* %cloptr9234, i64 0                    ; &cloptr9234[0]
  %f9236 = ptrtoint void(i64,i64,i64)* @lam8733 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9236, i64* %eptr9235                                                   ; store fptr
  %arg7791 = ptrtoint i64* %cloptr9234 to i64                                        ; closure cast; i64* -> i64
  %cloptr9237 = inttoptr i64 %PM2$Ycmb to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9238 = getelementptr inbounds i64, i64* %cloptr9237, i64 0                   ; &cloptr9237[0]
  %f9240 = load i64, i64* %i0ptr9238, align 8                                        ; load; *i0ptr9238
  %fptr9239 = inttoptr i64 %f9240 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9239(i64 %PM2$Ycmb, i64 %arg7792, i64 %arg7791)     ; tail call
  ret void
}


define void @lam9191(i64 %env9192, i64 %_957583, i64 %J7R$_37map1) {
  %envptr9241 = inttoptr i64 %env9192 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9242 = getelementptr inbounds i64, i64* %envptr9241, i64 2                  ; &envptr9241[2]
  %rqD$_37foldr1 = load i64, i64* %envptr9242, align 8                               ; load; *envptr9242
  %envptr9243 = inttoptr i64 %env9192 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9244 = getelementptr inbounds i64, i64* %envptr9243, i64 1                  ; &envptr9243[1]
  %PM2$Ycmb = load i64, i64* %envptr9244, align 8                                    ; load; *envptr9244
  %cloptr9245 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9247 = getelementptr inbounds i64, i64* %cloptr9245, i64 1                    ; &eptr9247[1]
  %eptr9248 = getelementptr inbounds i64, i64* %cloptr9245, i64 2                    ; &eptr9248[2]
  %eptr9249 = getelementptr inbounds i64, i64* %cloptr9245, i64 3                    ; &eptr9249[3]
  store i64 %J7R$_37map1, i64* %eptr9247                                             ; *eptr9247 = %J7R$_37map1
  store i64 %PM2$Ycmb, i64* %eptr9248                                                ; *eptr9248 = %PM2$Ycmb
  store i64 %rqD$_37foldr1, i64* %eptr9249                                           ; *eptr9249 = %rqD$_37foldr1
  %eptr9246 = getelementptr inbounds i64, i64* %cloptr9245, i64 0                    ; &cloptr9245[0]
  %f9250 = ptrtoint void(i64,i64,i64)* @lam9189 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9250, i64* %eptr9246                                                   ; store fptr
  %arg7795 = ptrtoint i64* %cloptr9245 to i64                                        ; closure cast; i64* -> i64
  %cloptr9251 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9252 = getelementptr inbounds i64, i64* %cloptr9251, i64 0                    ; &cloptr9251[0]
  %f9253 = ptrtoint void(i64,i64,i64)* @lam8747 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9253, i64* %eptr9252                                                   ; store fptr
  %arg7794 = ptrtoint i64* %cloptr9251 to i64                                        ; closure cast; i64* -> i64
  %cloptr9254 = inttoptr i64 %PM2$Ycmb to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9255 = getelementptr inbounds i64, i64* %cloptr9254, i64 0                   ; &cloptr9254[0]
  %f9257 = load i64, i64* %i0ptr9255, align 8                                        ; load; *i0ptr9255
  %fptr9256 = inttoptr i64 %f9257 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9256(i64 %PM2$Ycmb, i64 %arg7795, i64 %arg7794)     ; tail call
  ret void
}


define void @lam9189(i64 %env9190, i64 %_957584, i64 %N7V$_37take) {
  %envptr9258 = inttoptr i64 %env9190 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9259 = getelementptr inbounds i64, i64* %envptr9258, i64 3                  ; &envptr9258[3]
  %rqD$_37foldr1 = load i64, i64* %envptr9259, align 8                               ; load; *envptr9259
  %envptr9260 = inttoptr i64 %env9190 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9261 = getelementptr inbounds i64, i64* %envptr9260, i64 2                  ; &envptr9260[2]
  %PM2$Ycmb = load i64, i64* %envptr9261, align 8                                    ; load; *envptr9261
  %envptr9262 = inttoptr i64 %env9190 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9263 = getelementptr inbounds i64, i64* %envptr9262, i64 1                  ; &envptr9262[1]
  %J7R$_37map1 = load i64, i64* %envptr9263, align 8                                 ; load; *envptr9263
  %cloptr9264 = call i64* @alloc(i64 40)                                             ; malloc
  %eptr9266 = getelementptr inbounds i64, i64* %cloptr9264, i64 1                    ; &eptr9266[1]
  %eptr9267 = getelementptr inbounds i64, i64* %cloptr9264, i64 2                    ; &eptr9267[2]
  %eptr9268 = getelementptr inbounds i64, i64* %cloptr9264, i64 3                    ; &eptr9268[3]
  %eptr9269 = getelementptr inbounds i64, i64* %cloptr9264, i64 4                    ; &eptr9269[4]
  store i64 %J7R$_37map1, i64* %eptr9266                                             ; *eptr9266 = %J7R$_37map1
  store i64 %PM2$Ycmb, i64* %eptr9267                                                ; *eptr9267 = %PM2$Ycmb
  store i64 %rqD$_37foldr1, i64* %eptr9268                                           ; *eptr9268 = %rqD$_37foldr1
  store i64 %N7V$_37take, i64* %eptr9269                                             ; *eptr9269 = %N7V$_37take
  %eptr9265 = getelementptr inbounds i64, i64* %cloptr9264, i64 0                    ; &cloptr9264[0]
  %f9270 = ptrtoint void(i64,i64,i64)* @lam9187 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9270, i64* %eptr9265                                                   ; store fptr
  %arg7798 = ptrtoint i64* %cloptr9264 to i64                                        ; closure cast; i64* -> i64
  %cloptr9271 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9272 = getelementptr inbounds i64, i64* %cloptr9271, i64 0                    ; &cloptr9271[0]
  %f9273 = ptrtoint void(i64,i64,i64)* @lam8758 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9273, i64* %eptr9272                                                   ; store fptr
  %arg7797 = ptrtoint i64* %cloptr9271 to i64                                        ; closure cast; i64* -> i64
  %cloptr9274 = inttoptr i64 %PM2$Ycmb to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9275 = getelementptr inbounds i64, i64* %cloptr9274, i64 0                   ; &cloptr9274[0]
  %f9277 = load i64, i64* %i0ptr9275, align 8                                        ; load; *i0ptr9275
  %fptr9276 = inttoptr i64 %f9277 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9276(i64 %PM2$Ycmb, i64 %arg7798, i64 %arg7797)     ; tail call
  ret void
}


define void @lam9187(i64 %env9188, i64 %_957585, i64 %R74$_37length) {
  %envptr9278 = inttoptr i64 %env9188 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9279 = getelementptr inbounds i64, i64* %envptr9278, i64 4                  ; &envptr9278[4]
  %N7V$_37take = load i64, i64* %envptr9279, align 8                                 ; load; *envptr9279
  %envptr9280 = inttoptr i64 %env9188 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9281 = getelementptr inbounds i64, i64* %envptr9280, i64 3                  ; &envptr9280[3]
  %rqD$_37foldr1 = load i64, i64* %envptr9281, align 8                               ; load; *envptr9281
  %envptr9282 = inttoptr i64 %env9188 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9283 = getelementptr inbounds i64, i64* %envptr9282, i64 2                  ; &envptr9282[2]
  %PM2$Ycmb = load i64, i64* %envptr9283, align 8                                    ; load; *envptr9283
  %envptr9284 = inttoptr i64 %env9188 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9285 = getelementptr inbounds i64, i64* %envptr9284, i64 1                  ; &envptr9284[1]
  %J7R$_37map1 = load i64, i64* %envptr9285, align 8                                 ; load; *envptr9285
  %cloptr9286 = call i64* @alloc(i64 48)                                             ; malloc
  %eptr9288 = getelementptr inbounds i64, i64* %cloptr9286, i64 1                    ; &eptr9288[1]
  %eptr9289 = getelementptr inbounds i64, i64* %cloptr9286, i64 2                    ; &eptr9289[2]
  %eptr9290 = getelementptr inbounds i64, i64* %cloptr9286, i64 3                    ; &eptr9290[3]
  %eptr9291 = getelementptr inbounds i64, i64* %cloptr9286, i64 4                    ; &eptr9291[4]
  %eptr9292 = getelementptr inbounds i64, i64* %cloptr9286, i64 5                    ; &eptr9292[5]
  store i64 %J7R$_37map1, i64* %eptr9288                                             ; *eptr9288 = %J7R$_37map1
  store i64 %PM2$Ycmb, i64* %eptr9289                                                ; *eptr9289 = %PM2$Ycmb
  store i64 %R74$_37length, i64* %eptr9290                                           ; *eptr9290 = %R74$_37length
  store i64 %rqD$_37foldr1, i64* %eptr9291                                           ; *eptr9291 = %rqD$_37foldr1
  store i64 %N7V$_37take, i64* %eptr9292                                             ; *eptr9292 = %N7V$_37take
  %eptr9287 = getelementptr inbounds i64, i64* %cloptr9286, i64 0                    ; &cloptr9286[0]
  %f9293 = ptrtoint void(i64,i64,i64)* @lam9185 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9293, i64* %eptr9287                                                   ; store fptr
  %arg7801 = ptrtoint i64* %cloptr9286 to i64                                        ; closure cast; i64* -> i64
  %cloptr9294 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9295 = getelementptr inbounds i64, i64* %cloptr9294, i64 0                    ; &cloptr9294[0]
  %f9296 = ptrtoint void(i64,i64,i64)* @lam8766 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9296, i64* %eptr9295                                                   ; store fptr
  %arg7800 = ptrtoint i64* %cloptr9294 to i64                                        ; closure cast; i64* -> i64
  %cloptr9297 = inttoptr i64 %PM2$Ycmb to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9298 = getelementptr inbounds i64, i64* %cloptr9297, i64 0                   ; &cloptr9297[0]
  %f9300 = load i64, i64* %i0ptr9298, align 8                                        ; load; *i0ptr9298
  %fptr9299 = inttoptr i64 %f9300 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9299(i64 %PM2$Ycmb, i64 %arg7801, i64 %arg7800)     ; tail call
  ret void
}


define void @lam9185(i64 %env9186, i64 %_957586, i64 %dbd$_37foldl1) {
  %envptr9301 = inttoptr i64 %env9186 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9302 = getelementptr inbounds i64, i64* %envptr9301, i64 5                  ; &envptr9301[5]
  %N7V$_37take = load i64, i64* %envptr9302, align 8                                 ; load; *envptr9302
  %envptr9303 = inttoptr i64 %env9186 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9304 = getelementptr inbounds i64, i64* %envptr9303, i64 4                  ; &envptr9303[4]
  %rqD$_37foldr1 = load i64, i64* %envptr9304, align 8                               ; load; *envptr9304
  %envptr9305 = inttoptr i64 %env9186 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9306 = getelementptr inbounds i64, i64* %envptr9305, i64 3                  ; &envptr9305[3]
  %R74$_37length = load i64, i64* %envptr9306, align 8                               ; load; *envptr9306
  %envptr9307 = inttoptr i64 %env9186 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9308 = getelementptr inbounds i64, i64* %envptr9307, i64 2                  ; &envptr9307[2]
  %PM2$Ycmb = load i64, i64* %envptr9308, align 8                                    ; load; *envptr9308
  %envptr9309 = inttoptr i64 %env9186 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9310 = getelementptr inbounds i64, i64* %envptr9309, i64 1                  ; &envptr9309[1]
  %J7R$_37map1 = load i64, i64* %envptr9310, align 8                                 ; load; *envptr9310
  %cloptr9311 = call i64* @alloc(i64 16)                                             ; malloc
  %eptr9313 = getelementptr inbounds i64, i64* %cloptr9311, i64 1                    ; &eptr9313[1]
  store i64 %dbd$_37foldl1, i64* %eptr9313                                           ; *eptr9313 = %dbd$_37foldl1
  %eptr9312 = getelementptr inbounds i64, i64* %cloptr9311, i64 0                    ; &cloptr9311[0]
  %f9314 = ptrtoint void(i64,i64,i64)* @lam9183 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9314, i64* %eptr9312                                                   ; store fptr
  %HhX$_37last = ptrtoint i64* %cloptr9311 to i64                                    ; closure cast; i64* -> i64
  %cloptr9315 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9317 = getelementptr inbounds i64, i64* %cloptr9315, i64 1                    ; &eptr9317[1]
  %eptr9318 = getelementptr inbounds i64, i64* %cloptr9315, i64 2                    ; &eptr9318[2]
  store i64 %R74$_37length, i64* %eptr9317                                           ; *eptr9317 = %R74$_37length
  store i64 %N7V$_37take, i64* %eptr9318                                             ; *eptr9318 = %N7V$_37take
  %eptr9316 = getelementptr inbounds i64, i64* %cloptr9315, i64 0                    ; &cloptr9315[0]
  %f9319 = ptrtoint void(i64,i64,i64,i64)* @lam9177 to i64                           ; fptr cast; i64(...)* -> i64
  store i64 %f9319, i64* %eptr9316                                                   ; store fptr
  %TKl$_37drop_45right = ptrtoint i64* %cloptr9315 to i64                            ; closure cast; i64* -> i64
  %cloptr9320 = call i64* @alloc(i64 56)                                             ; malloc
  %eptr9322 = getelementptr inbounds i64, i64* %cloptr9320, i64 1                    ; &eptr9322[1]
  %eptr9323 = getelementptr inbounds i64, i64* %cloptr9320, i64 2                    ; &eptr9323[2]
  %eptr9324 = getelementptr inbounds i64, i64* %cloptr9320, i64 3                    ; &eptr9324[3]
  %eptr9325 = getelementptr inbounds i64, i64* %cloptr9320, i64 4                    ; &eptr9325[4]
  %eptr9326 = getelementptr inbounds i64, i64* %cloptr9320, i64 5                    ; &eptr9326[5]
  %eptr9327 = getelementptr inbounds i64, i64* %cloptr9320, i64 6                    ; &eptr9327[6]
  store i64 %PM2$Ycmb, i64* %eptr9322                                                ; *eptr9322 = %PM2$Ycmb
  store i64 %HhX$_37last, i64* %eptr9323                                             ; *eptr9323 = %HhX$_37last
  store i64 %TKl$_37drop_45right, i64* %eptr9324                                     ; *eptr9324 = %TKl$_37drop_45right
  store i64 %dbd$_37foldl1, i64* %eptr9325                                           ; *eptr9325 = %dbd$_37foldl1
  store i64 %R74$_37length, i64* %eptr9326                                           ; *eptr9326 = %R74$_37length
  store i64 %rqD$_37foldr1, i64* %eptr9327                                           ; *eptr9327 = %rqD$_37foldr1
  %eptr9321 = getelementptr inbounds i64, i64* %cloptr9320, i64 0                    ; &cloptr9320[0]
  %f9328 = ptrtoint void(i64,i64,i64)* @lam9173 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9328, i64* %eptr9321                                                   ; store fptr
  %arg7821 = ptrtoint i64* %cloptr9320 to i64                                        ; closure cast; i64* -> i64
  %cloptr9329 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9331 = getelementptr inbounds i64, i64* %cloptr9329, i64 1                    ; &eptr9331[1]
  %eptr9332 = getelementptr inbounds i64, i64* %cloptr9329, i64 2                    ; &eptr9332[2]
  store i64 %J7R$_37map1, i64* %eptr9331                                             ; *eptr9331 = %J7R$_37map1
  store i64 %rqD$_37foldr1, i64* %eptr9332                                           ; *eptr9332 = %rqD$_37foldr1
  %eptr9330 = getelementptr inbounds i64, i64* %cloptr9329, i64 0                    ; &cloptr9329[0]
  %f9333 = ptrtoint void(i64,i64,i64)* @lam8803 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9333, i64* %eptr9330                                                   ; store fptr
  %arg7820 = ptrtoint i64* %cloptr9329 to i64                                        ; closure cast; i64* -> i64
  %cloptr9334 = inttoptr i64 %PM2$Ycmb to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9335 = getelementptr inbounds i64, i64* %cloptr9334, i64 0                   ; &cloptr9334[0]
  %f9337 = load i64, i64* %i0ptr9335, align 8                                        ; load; *i0ptr9335
  %fptr9336 = inttoptr i64 %f9337 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9336(i64 %PM2$Ycmb, i64 %arg7821, i64 %arg7820)     ; tail call
  ret void
}


define void @lam9183(i64 %env9184, i64 %cont7587, i64 %qQA$lst) {
  %envptr9338 = inttoptr i64 %env9184 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9339 = getelementptr inbounds i64, i64* %envptr9338, i64 1                  ; &envptr9338[1]
  %dbd$_37foldl1 = load i64, i64* %envptr9339, align 8                               ; load; *envptr9339
  %cloptr9340 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9341 = getelementptr inbounds i64, i64* %cloptr9340, i64 0                    ; &cloptr9340[0]
  %f9342 = ptrtoint void(i64,i64,i64,i64)* @lam9181 to i64                           ; fptr cast; i64(...)* -> i64
  store i64 %f9342, i64* %eptr9341                                                   ; store fptr
  %arg7805 = ptrtoint i64* %cloptr9340 to i64                                        ; closure cast; i64* -> i64
  %arg7804 = add i64 0, 0                                                            ; quoted ()
  %cloptr9343 = inttoptr i64 %dbd$_37foldl1 to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr9344 = getelementptr inbounds i64, i64* %cloptr9343, i64 0                   ; &cloptr9343[0]
  %f9346 = load i64, i64* %i0ptr9344, align 8                                        ; load; *i0ptr9344
  %fptr9345 = inttoptr i64 %f9346 to void (i64,i64,i64,i64,i64)*                     ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9345(i64 %dbd$_37foldl1, i64 %cont7587, i64 %arg7805, i64 %arg7804, i64 %qQA$lst); tail call
  ret void
}


define void @lam9181(i64 %env9182, i64 %cont7588, i64 %I1k$x, i64 %RVr$y) {
  %arg7809 = add i64 0, 0                                                            ; quoted ()
  %cloptr9347 = inttoptr i64 %cont7588 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9348 = getelementptr inbounds i64, i64* %cloptr9347, i64 0                   ; &cloptr9347[0]
  %f9350 = load i64, i64* %i0ptr9348, align 8                                        ; load; *i0ptr9348
  %fptr9349 = inttoptr i64 %f9350 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9349(i64 %cont7588, i64 %arg7809, i64 %I1k$x)       ; tail call
  ret void
}


define void @lam9177(i64 %env9178, i64 %cont7589, i64 %txY$lst, i64 %EFp$n) {
  %envptr9351 = inttoptr i64 %env9178 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9352 = getelementptr inbounds i64, i64* %envptr9351, i64 2                  ; &envptr9351[2]
  %N7V$_37take = load i64, i64* %envptr9352, align 8                                 ; load; *envptr9352
  %envptr9353 = inttoptr i64 %env9178 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9354 = getelementptr inbounds i64, i64* %envptr9353, i64 1                  ; &envptr9353[1]
  %R74$_37length = load i64, i64* %envptr9354, align 8                               ; load; *envptr9354
  %cloptr9355 = call i64* @alloc(i64 40)                                             ; malloc
  %eptr9357 = getelementptr inbounds i64, i64* %cloptr9355, i64 1                    ; &eptr9357[1]
  %eptr9358 = getelementptr inbounds i64, i64* %cloptr9355, i64 2                    ; &eptr9358[2]
  %eptr9359 = getelementptr inbounds i64, i64* %cloptr9355, i64 3                    ; &eptr9359[3]
  %eptr9360 = getelementptr inbounds i64, i64* %cloptr9355, i64 4                    ; &eptr9360[4]
  store i64 %EFp$n, i64* %eptr9357                                                   ; *eptr9357 = %EFp$n
  store i64 %txY$lst, i64* %eptr9358                                                 ; *eptr9358 = %txY$lst
  store i64 %N7V$_37take, i64* %eptr9359                                             ; *eptr9359 = %N7V$_37take
  store i64 %cont7589, i64* %eptr9360                                                ; *eptr9360 = %cont7589
  %eptr9356 = getelementptr inbounds i64, i64* %cloptr9355, i64 0                    ; &cloptr9355[0]
  %f9361 = ptrtoint void(i64,i64,i64)* @lam9175 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9361, i64* %eptr9356                                                   ; store fptr
  %arg7812 = ptrtoint i64* %cloptr9355 to i64                                        ; closure cast; i64* -> i64
  %cloptr9362 = inttoptr i64 %R74$_37length to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr9363 = getelementptr inbounds i64, i64* %cloptr9362, i64 0                   ; &cloptr9362[0]
  %f9365 = load i64, i64* %i0ptr9363, align 8                                        ; load; *i0ptr9363
  %fptr9364 = inttoptr i64 %f9365 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9364(i64 %R74$_37length, i64 %arg7812, i64 %txY$lst); tail call
  ret void
}


define void @lam9175(i64 %env9176, i64 %_957590, i64 %a7479) {
  %envptr9366 = inttoptr i64 %env9176 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9367 = getelementptr inbounds i64, i64* %envptr9366, i64 4                  ; &envptr9366[4]
  %cont7589 = load i64, i64* %envptr9367, align 8                                    ; load; *envptr9367
  %envptr9368 = inttoptr i64 %env9176 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9369 = getelementptr inbounds i64, i64* %envptr9368, i64 3                  ; &envptr9368[3]
  %N7V$_37take = load i64, i64* %envptr9369, align 8                                 ; load; *envptr9369
  %envptr9370 = inttoptr i64 %env9176 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9371 = getelementptr inbounds i64, i64* %envptr9370, i64 2                  ; &envptr9370[2]
  %txY$lst = load i64, i64* %envptr9371, align 8                                     ; load; *envptr9371
  %envptr9372 = inttoptr i64 %env9176 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9373 = getelementptr inbounds i64, i64* %envptr9372, i64 1                  ; &envptr9372[1]
  %EFp$n = load i64, i64* %envptr9373, align 8                                       ; load; *envptr9373
  %a7480 = call i64 @prim__45(i64 %a7479, i64 %EFp$n)                                ; call prim__45
  %cloptr9374 = inttoptr i64 %N7V$_37take to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr9375 = getelementptr inbounds i64, i64* %cloptr9374, i64 0                   ; &cloptr9374[0]
  %f9377 = load i64, i64* %i0ptr9375, align 8                                        ; load; *i0ptr9375
  %fptr9376 = inttoptr i64 %f9377 to void (i64,i64,i64,i64)*                         ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9376(i64 %N7V$_37take, i64 %cont7589, i64 %txY$lst, i64 %a7480); tail call
  ret void
}


define void @lam9173(i64 %env9174, i64 %_957591, i64 %POA$_37foldr) {
  %envptr9378 = inttoptr i64 %env9174 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9379 = getelementptr inbounds i64, i64* %envptr9378, i64 6                  ; &envptr9378[6]
  %rqD$_37foldr1 = load i64, i64* %envptr9379, align 8                               ; load; *envptr9379
  %envptr9380 = inttoptr i64 %env9174 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9381 = getelementptr inbounds i64, i64* %envptr9380, i64 5                  ; &envptr9380[5]
  %R74$_37length = load i64, i64* %envptr9381, align 8                               ; load; *envptr9381
  %envptr9382 = inttoptr i64 %env9174 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9383 = getelementptr inbounds i64, i64* %envptr9382, i64 4                  ; &envptr9382[4]
  %dbd$_37foldl1 = load i64, i64* %envptr9383, align 8                               ; load; *envptr9383
  %envptr9384 = inttoptr i64 %env9174 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9385 = getelementptr inbounds i64, i64* %envptr9384, i64 3                  ; &envptr9384[3]
  %TKl$_37drop_45right = load i64, i64* %envptr9385, align 8                         ; load; *envptr9385
  %envptr9386 = inttoptr i64 %env9174 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9387 = getelementptr inbounds i64, i64* %envptr9386, i64 2                  ; &envptr9386[2]
  %HhX$_37last = load i64, i64* %envptr9387, align 8                                 ; load; *envptr9387
  %envptr9388 = inttoptr i64 %env9174 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9389 = getelementptr inbounds i64, i64* %envptr9388, i64 1                  ; &envptr9388[1]
  %PM2$Ycmb = load i64, i64* %envptr9389, align 8                                    ; load; *envptr9389
  %cloptr9390 = call i64* @alloc(i64 16)                                             ; malloc
  %eptr9392 = getelementptr inbounds i64, i64* %cloptr9390, i64 1                    ; &eptr9392[1]
  store i64 %rqD$_37foldr1, i64* %eptr9392                                           ; *eptr9392 = %rqD$_37foldr1
  %eptr9391 = getelementptr inbounds i64, i64* %cloptr9390, i64 0                    ; &cloptr9390[0]
  %f9393 = ptrtoint void(i64,i64,i64,i64)* @lam9171 to i64                           ; fptr cast; i64(...)* -> i64
  store i64 %f9393, i64* %eptr9391                                                   ; store fptr
  %umA$_37map1 = ptrtoint i64* %cloptr9390 to i64                                    ; closure cast; i64* -> i64
  %cloptr9394 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9396 = getelementptr inbounds i64, i64* %cloptr9394, i64 1                    ; &eptr9396[1]
  %eptr9397 = getelementptr inbounds i64, i64* %cloptr9394, i64 2                    ; &eptr9397[2]
  %eptr9398 = getelementptr inbounds i64, i64* %cloptr9394, i64 3                    ; &eptr9398[3]
  store i64 %HhX$_37last, i64* %eptr9396                                             ; *eptr9396 = %HhX$_37last
  store i64 %TKl$_37drop_45right, i64* %eptr9397                                     ; *eptr9397 = %TKl$_37drop_45right
  store i64 %POA$_37foldr, i64* %eptr9398                                            ; *eptr9398 = %POA$_37foldr
  %eptr9395 = getelementptr inbounds i64, i64* %cloptr9394, i64 0                    ; &cloptr9394[0]
  %f9399 = ptrtoint void(i64,i64)* @lam9163 to i64                                   ; fptr cast; i64(...)* -> i64
  store i64 %f9399, i64* %eptr9395                                                   ; store fptr
  %NHs$_37map = ptrtoint i64* %cloptr9394 to i64                                     ; closure cast; i64* -> i64
  %cloptr9400 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9402 = getelementptr inbounds i64, i64* %cloptr9400, i64 1                    ; &eptr9402[1]
  %eptr9403 = getelementptr inbounds i64, i64* %cloptr9400, i64 2                    ; &eptr9403[2]
  store i64 %dbd$_37foldl1, i64* %eptr9402                                           ; *eptr9402 = %dbd$_37foldl1
  store i64 %R74$_37length, i64* %eptr9403                                           ; *eptr9403 = %R74$_37length
  %eptr9401 = getelementptr inbounds i64, i64* %cloptr9400, i64 0                    ; &cloptr9400[0]
  %f9404 = ptrtoint void(i64,i64,i64)* @lam9150 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9404, i64* %eptr9401                                                   ; store fptr
  %arg7863 = ptrtoint i64* %cloptr9400 to i64                                        ; closure cast; i64* -> i64
  %cloptr9405 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9407 = getelementptr inbounds i64, i64* %cloptr9405, i64 1                    ; &eptr9407[1]
  %eptr9408 = getelementptr inbounds i64, i64* %cloptr9405, i64 2                    ; &eptr9408[2]
  %eptr9409 = getelementptr inbounds i64, i64* %cloptr9405, i64 3                    ; &eptr9409[3]
  store i64 %umA$_37map1, i64* %eptr9407                                             ; *eptr9407 = %umA$_37map1
  store i64 %POA$_37foldr, i64* %eptr9408                                            ; *eptr9408 = %POA$_37foldr
  store i64 %rqD$_37foldr1, i64* %eptr9409                                           ; *eptr9409 = %rqD$_37foldr1
  %eptr9406 = getelementptr inbounds i64, i64* %cloptr9405, i64 0                    ; &cloptr9405[0]
  %f9410 = ptrtoint void(i64,i64,i64)* @lam8840 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9410, i64* %eptr9406                                                   ; store fptr
  %arg7862 = ptrtoint i64* %cloptr9405 to i64                                        ; closure cast; i64* -> i64
  %cloptr9411 = inttoptr i64 %PM2$Ycmb to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9412 = getelementptr inbounds i64, i64* %cloptr9411, i64 0                   ; &cloptr9411[0]
  %f9414 = load i64, i64* %i0ptr9412, align 8                                        ; load; *i0ptr9412
  %fptr9413 = inttoptr i64 %f9414 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9413(i64 %PM2$Ycmb, i64 %arg7863, i64 %arg7862)     ; tail call
  ret void
}


define void @lam9171(i64 %env9172, i64 %cont7592, i64 %s63$f, i64 %aCB$lst) {
  %envptr9415 = inttoptr i64 %env9172 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9416 = getelementptr inbounds i64, i64* %envptr9415, i64 1                  ; &envptr9415[1]
  %rqD$_37foldr1 = load i64, i64* %envptr9416, align 8                               ; load; *envptr9416
  %cloptr9417 = call i64* @alloc(i64 16)                                             ; malloc
  %eptr9419 = getelementptr inbounds i64, i64* %cloptr9417, i64 1                    ; &eptr9419[1]
  store i64 %s63$f, i64* %eptr9419                                                   ; *eptr9419 = %s63$f
  %eptr9418 = getelementptr inbounds i64, i64* %cloptr9417, i64 0                    ; &cloptr9417[0]
  %f9420 = ptrtoint void(i64,i64,i64,i64)* @lam9169 to i64                           ; fptr cast; i64(...)* -> i64
  store i64 %f9420, i64* %eptr9418                                                   ; store fptr
  %arg7825 = ptrtoint i64* %cloptr9417 to i64                                        ; closure cast; i64* -> i64
  %arg7824 = add i64 0, 0                                                            ; quoted ()
  %cloptr9421 = inttoptr i64 %rqD$_37foldr1 to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr9422 = getelementptr inbounds i64, i64* %cloptr9421, i64 0                   ; &cloptr9421[0]
  %f9424 = load i64, i64* %i0ptr9422, align 8                                        ; load; *i0ptr9422
  %fptr9423 = inttoptr i64 %f9424 to void (i64,i64,i64,i64,i64)*                     ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9423(i64 %rqD$_37foldr1, i64 %cont7592, i64 %arg7825, i64 %arg7824, i64 %aCB$lst); tail call
  ret void
}


define void @lam9169(i64 %env9170, i64 %cont7593, i64 %B1M$v, i64 %T20$r) {
  %envptr9425 = inttoptr i64 %env9170 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9426 = getelementptr inbounds i64, i64* %envptr9425, i64 1                  ; &envptr9425[1]
  %s63$f = load i64, i64* %envptr9426, align 8                                       ; load; *envptr9426
  %cloptr9427 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9429 = getelementptr inbounds i64, i64* %cloptr9427, i64 1                    ; &eptr9429[1]
  %eptr9430 = getelementptr inbounds i64, i64* %cloptr9427, i64 2                    ; &eptr9430[2]
  store i64 %T20$r, i64* %eptr9429                                                   ; *eptr9429 = %T20$r
  store i64 %cont7593, i64* %eptr9430                                                ; *eptr9430 = %cont7593
  %eptr9428 = getelementptr inbounds i64, i64* %cloptr9427, i64 0                    ; &cloptr9427[0]
  %f9431 = ptrtoint void(i64,i64,i64)* @lam9167 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9431, i64* %eptr9428                                                   ; store fptr
  %arg7829 = ptrtoint i64* %cloptr9427 to i64                                        ; closure cast; i64* -> i64
  %cloptr9432 = inttoptr i64 %s63$f to i64*                                          ; closure/env cast; i64 -> i64*
  %i0ptr9433 = getelementptr inbounds i64, i64* %cloptr9432, i64 0                   ; &cloptr9432[0]
  %f9435 = load i64, i64* %i0ptr9433, align 8                                        ; load; *i0ptr9433
  %fptr9434 = inttoptr i64 %f9435 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9434(i64 %s63$f, i64 %arg7829, i64 %B1M$v)          ; tail call
  ret void
}


define void @lam9167(i64 %env9168, i64 %_957594, i64 %a7489) {
  %envptr9436 = inttoptr i64 %env9168 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9437 = getelementptr inbounds i64, i64* %envptr9436, i64 2                  ; &envptr9436[2]
  %cont7593 = load i64, i64* %envptr9437, align 8                                    ; load; *envptr9437
  %envptr9438 = inttoptr i64 %env9168 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9439 = getelementptr inbounds i64, i64* %envptr9438, i64 1                  ; &envptr9438[1]
  %T20$r = load i64, i64* %envptr9439, align 8                                       ; load; *envptr9439
  %retprim7595 = call i64 @prim_cons(i64 %a7489, i64 %T20$r)                         ; call prim_cons
  %arg7834 = add i64 0, 0                                                            ; quoted ()
  %cloptr9440 = inttoptr i64 %cont7593 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9441 = getelementptr inbounds i64, i64* %cloptr9440, i64 0                   ; &cloptr9440[0]
  %f9443 = load i64, i64* %i0ptr9441, align 8                                        ; load; *i0ptr9441
  %fptr9442 = inttoptr i64 %f9443 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9442(i64 %cont7593, i64 %arg7834, i64 %retprim7595) ; tail call
  ret void
}


define void @lam9163(i64 %env9164, i64 %INo$args7597) {
  %envptr9444 = inttoptr i64 %env9164 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9445 = getelementptr inbounds i64, i64* %envptr9444, i64 3                  ; &envptr9444[3]
  %POA$_37foldr = load i64, i64* %envptr9445, align 8                                ; load; *envptr9445
  %envptr9446 = inttoptr i64 %env9164 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9447 = getelementptr inbounds i64, i64* %envptr9446, i64 2                  ; &envptr9446[2]
  %TKl$_37drop_45right = load i64, i64* %envptr9447, align 8                         ; load; *envptr9447
  %envptr9448 = inttoptr i64 %env9164 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9449 = getelementptr inbounds i64, i64* %envptr9448, i64 1                  ; &envptr9448[1]
  %HhX$_37last = load i64, i64* %envptr9449, align 8                                 ; load; *envptr9449
  %cont7596 = call i64 @prim_car(i64 %INo$args7597)                                  ; call prim_car
  %INo$args = call i64 @prim_cdr(i64 %INo$args7597)                                  ; call prim_cdr
  %EAS$f = call i64 @prim_car(i64 %INo$args)                                         ; call prim_car
  %kEY$lsts = call i64 @prim_cdr(i64 %INo$args)                                      ; call prim_cdr
  %arg7841 = add i64 0, 0                                                            ; quoted ()
  %a7493 = call i64 @prim_cons(i64 %arg7841, i64 %kEY$lsts)                          ; call prim_cons
  %cloptr9450 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9452 = getelementptr inbounds i64, i64* %cloptr9450, i64 1                    ; &eptr9452[1]
  %eptr9453 = getelementptr inbounds i64, i64* %cloptr9450, i64 2                    ; &eptr9453[2]
  %eptr9454 = getelementptr inbounds i64, i64* %cloptr9450, i64 3                    ; &eptr9454[3]
  store i64 %HhX$_37last, i64* %eptr9452                                             ; *eptr9452 = %HhX$_37last
  store i64 %TKl$_37drop_45right, i64* %eptr9453                                     ; *eptr9453 = %TKl$_37drop_45right
  store i64 %EAS$f, i64* %eptr9454                                                   ; *eptr9454 = %EAS$f
  %eptr9451 = getelementptr inbounds i64, i64* %cloptr9450, i64 0                    ; &cloptr9450[0]
  %f9455 = ptrtoint void(i64,i64)* @lam9160 to i64                                   ; fptr cast; i64(...)* -> i64
  store i64 %f9455, i64* %eptr9451                                                   ; store fptr
  %arg7843 = ptrtoint i64* %cloptr9450 to i64                                        ; closure cast; i64* -> i64
  %a7494 = call i64 @prim_cons(i64 %arg7843, i64 %a7493)                             ; call prim_cons
  %cps_45lst7605 = call i64 @prim_cons(i64 %cont7596, i64 %a7494)                    ; call prim_cons
  %cloptr9456 = inttoptr i64 %POA$_37foldr to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr9457 = getelementptr inbounds i64, i64* %cloptr9456, i64 0                   ; &cloptr9456[0]
  %f9459 = load i64, i64* %i0ptr9457, align 8                                        ; load; *i0ptr9457
  %fptr9458 = inttoptr i64 %f9459 to void (i64,i64)*                                 ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9458(i64 %POA$_37foldr, i64 %cps_45lst7605)         ; tail call
  ret void
}


define void @lam9160(i64 %env9161, i64 %RRR$fargs7599) {
  %envptr9460 = inttoptr i64 %env9161 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9461 = getelementptr inbounds i64, i64* %envptr9460, i64 3                  ; &envptr9460[3]
  %EAS$f = load i64, i64* %envptr9461, align 8                                       ; load; *envptr9461
  %envptr9462 = inttoptr i64 %env9161 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9463 = getelementptr inbounds i64, i64* %envptr9462, i64 2                  ; &envptr9462[2]
  %TKl$_37drop_45right = load i64, i64* %envptr9463, align 8                         ; load; *envptr9463
  %envptr9464 = inttoptr i64 %env9161 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9465 = getelementptr inbounds i64, i64* %envptr9464, i64 1                  ; &envptr9464[1]
  %HhX$_37last = load i64, i64* %envptr9465, align 8                                 ; load; *envptr9465
  %cont7598 = call i64 @prim_car(i64 %RRR$fargs7599)                                 ; call prim_car
  %RRR$fargs = call i64 @prim_cdr(i64 %RRR$fargs7599)                                ; call prim_cdr
  %cloptr9466 = call i64* @alloc(i64 40)                                             ; malloc
  %eptr9468 = getelementptr inbounds i64, i64* %cloptr9466, i64 1                    ; &eptr9468[1]
  %eptr9469 = getelementptr inbounds i64, i64* %cloptr9466, i64 2                    ; &eptr9469[2]
  %eptr9470 = getelementptr inbounds i64, i64* %cloptr9466, i64 3                    ; &eptr9470[3]
  %eptr9471 = getelementptr inbounds i64, i64* %cloptr9466, i64 4                    ; &eptr9471[4]
  store i64 %RRR$fargs, i64* %eptr9468                                               ; *eptr9468 = %RRR$fargs
  store i64 %HhX$_37last, i64* %eptr9469                                             ; *eptr9469 = %HhX$_37last
  store i64 %cont7598, i64* %eptr9470                                                ; *eptr9470 = %cont7598
  store i64 %EAS$f, i64* %eptr9471                                                   ; *eptr9471 = %EAS$f
  %eptr9467 = getelementptr inbounds i64, i64* %cloptr9466, i64 0                    ; &cloptr9466[0]
  %f9472 = ptrtoint void(i64,i64,i64)* @lam9158 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9472, i64* %eptr9467                                                   ; store fptr
  %arg7848 = ptrtoint i64* %cloptr9466 to i64                                        ; closure cast; i64* -> i64
  %arg7846 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %cloptr9473 = inttoptr i64 %TKl$_37drop_45right to i64*                            ; closure/env cast; i64 -> i64*
  %i0ptr9474 = getelementptr inbounds i64, i64* %cloptr9473, i64 0                   ; &cloptr9473[0]
  %f9476 = load i64, i64* %i0ptr9474, align 8                                        ; load; *i0ptr9474
  %fptr9475 = inttoptr i64 %f9476 to void (i64,i64,i64,i64)*                         ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9475(i64 %TKl$_37drop_45right, i64 %arg7848, i64 %RRR$fargs, i64 %arg7846); tail call
  ret void
}


define void @lam9158(i64 %env9159, i64 %_957600, i64 %a7490) {
  %envptr9477 = inttoptr i64 %env9159 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9478 = getelementptr inbounds i64, i64* %envptr9477, i64 4                  ; &envptr9477[4]
  %EAS$f = load i64, i64* %envptr9478, align 8                                       ; load; *envptr9478
  %envptr9479 = inttoptr i64 %env9159 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9480 = getelementptr inbounds i64, i64* %envptr9479, i64 3                  ; &envptr9479[3]
  %cont7598 = load i64, i64* %envptr9480, align 8                                    ; load; *envptr9480
  %envptr9481 = inttoptr i64 %env9159 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9482 = getelementptr inbounds i64, i64* %envptr9481, i64 2                  ; &envptr9481[2]
  %HhX$_37last = load i64, i64* %envptr9482, align 8                                 ; load; *envptr9482
  %envptr9483 = inttoptr i64 %env9159 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9484 = getelementptr inbounds i64, i64* %envptr9483, i64 1                  ; &envptr9483[1]
  %RRR$fargs = load i64, i64* %envptr9484, align 8                                   ; load; *envptr9484
  %cloptr9485 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9487 = getelementptr inbounds i64, i64* %cloptr9485, i64 1                    ; &eptr9487[1]
  %eptr9488 = getelementptr inbounds i64, i64* %cloptr9485, i64 2                    ; &eptr9488[2]
  %eptr9489 = getelementptr inbounds i64, i64* %cloptr9485, i64 3                    ; &eptr9489[3]
  store i64 %RRR$fargs, i64* %eptr9487                                               ; *eptr9487 = %RRR$fargs
  store i64 %HhX$_37last, i64* %eptr9488                                             ; *eptr9488 = %HhX$_37last
  store i64 %cont7598, i64* %eptr9489                                                ; *eptr9489 = %cont7598
  %eptr9486 = getelementptr inbounds i64, i64* %cloptr9485, i64 0                    ; &cloptr9485[0]
  %f9490 = ptrtoint void(i64,i64,i64)* @lam9156 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9490, i64* %eptr9486                                                   ; store fptr
  %arg7851 = ptrtoint i64* %cloptr9485 to i64                                        ; closure cast; i64* -> i64
  %cps_45lst7604 = call i64 @prim_cons(i64 %arg7851, i64 %a7490)                     ; call prim_cons
  %cloptr9491 = inttoptr i64 %EAS$f to i64*                                          ; closure/env cast; i64 -> i64*
  %i0ptr9492 = getelementptr inbounds i64, i64* %cloptr9491, i64 0                   ; &cloptr9491[0]
  %f9494 = load i64, i64* %i0ptr9492, align 8                                        ; load; *i0ptr9492
  %fptr9493 = inttoptr i64 %f9494 to void (i64,i64)*                                 ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9493(i64 %EAS$f, i64 %cps_45lst7604)                ; tail call
  ret void
}


define void @lam9156(i64 %env9157, i64 %_957601, i64 %a7491) {
  %envptr9495 = inttoptr i64 %env9157 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9496 = getelementptr inbounds i64, i64* %envptr9495, i64 3                  ; &envptr9495[3]
  %cont7598 = load i64, i64* %envptr9496, align 8                                    ; load; *envptr9496
  %envptr9497 = inttoptr i64 %env9157 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9498 = getelementptr inbounds i64, i64* %envptr9497, i64 2                  ; &envptr9497[2]
  %HhX$_37last = load i64, i64* %envptr9498, align 8                                 ; load; *envptr9498
  %envptr9499 = inttoptr i64 %env9157 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9500 = getelementptr inbounds i64, i64* %envptr9499, i64 1                  ; &envptr9499[1]
  %RRR$fargs = load i64, i64* %envptr9500, align 8                                   ; load; *envptr9500
  %cloptr9501 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9503 = getelementptr inbounds i64, i64* %cloptr9501, i64 1                    ; &eptr9503[1]
  %eptr9504 = getelementptr inbounds i64, i64* %cloptr9501, i64 2                    ; &eptr9504[2]
  store i64 %cont7598, i64* %eptr9503                                                ; *eptr9503 = %cont7598
  store i64 %a7491, i64* %eptr9504                                                   ; *eptr9504 = %a7491
  %eptr9502 = getelementptr inbounds i64, i64* %cloptr9501, i64 0                    ; &cloptr9501[0]
  %f9505 = ptrtoint void(i64,i64,i64)* @lam9154 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9505, i64* %eptr9502                                                   ; store fptr
  %arg7853 = ptrtoint i64* %cloptr9501 to i64                                        ; closure cast; i64* -> i64
  %cloptr9506 = inttoptr i64 %HhX$_37last to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr9507 = getelementptr inbounds i64, i64* %cloptr9506, i64 0                   ; &cloptr9506[0]
  %f9509 = load i64, i64* %i0ptr9507, align 8                                        ; load; *i0ptr9507
  %fptr9508 = inttoptr i64 %f9509 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9508(i64 %HhX$_37last, i64 %arg7853, i64 %RRR$fargs); tail call
  ret void
}


define void @lam9154(i64 %env9155, i64 %_957602, i64 %a7492) {
  %envptr9510 = inttoptr i64 %env9155 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9511 = getelementptr inbounds i64, i64* %envptr9510, i64 2                  ; &envptr9510[2]
  %a7491 = load i64, i64* %envptr9511, align 8                                       ; load; *envptr9511
  %envptr9512 = inttoptr i64 %env9155 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9513 = getelementptr inbounds i64, i64* %envptr9512, i64 1                  ; &envptr9512[1]
  %cont7598 = load i64, i64* %envptr9513, align 8                                    ; load; *envptr9513
  %retprim7603 = call i64 @prim_cons(i64 %a7491, i64 %a7492)                         ; call prim_cons
  %arg7858 = add i64 0, 0                                                            ; quoted ()
  %cloptr9514 = inttoptr i64 %cont7598 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9515 = getelementptr inbounds i64, i64* %cloptr9514, i64 0                   ; &cloptr9514[0]
  %f9517 = load i64, i64* %i0ptr9515, align 8                                        ; load; *i0ptr9515
  %fptr9516 = inttoptr i64 %f9517 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9516(i64 %cont7598, i64 %arg7858, i64 %retprim7603) ; tail call
  ret void
}


define void @lam9150(i64 %env9151, i64 %_957606, i64 %kYR$_37foldl) {
  %envptr9518 = inttoptr i64 %env9151 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9519 = getelementptr inbounds i64, i64* %envptr9518, i64 2                  ; &envptr9518[2]
  %R74$_37length = load i64, i64* %envptr9519, align 8                               ; load; *envptr9519
  %envptr9520 = inttoptr i64 %env9151 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9521 = getelementptr inbounds i64, i64* %envptr9520, i64 1                  ; &envptr9520[1]
  %dbd$_37foldl1 = load i64, i64* %envptr9521, align 8                               ; load; *envptr9521
  %cloptr9522 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9523 = getelementptr inbounds i64, i64* %cloptr9522, i64 0                    ; &cloptr9522[0]
  %f9524 = ptrtoint void(i64,i64,i64,i64)* @lam9148 to i64                           ; fptr cast; i64(...)* -> i64
  store i64 %f9524, i64* %eptr9523                                                   ; store fptr
  %LdG$_37_62 = ptrtoint i64* %cloptr9522 to i64                                     ; closure cast; i64* -> i64
  %cloptr9525 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9526 = getelementptr inbounds i64, i64* %cloptr9525, i64 0                    ; &cloptr9525[0]
  %f9527 = ptrtoint void(i64,i64,i64,i64)* @lam9145 to i64                           ; fptr cast; i64(...)* -> i64
  store i64 %f9527, i64* %eptr9526                                                   ; store fptr
  %HZe$_37_62_61 = ptrtoint i64* %cloptr9525 to i64                                  ; closure cast; i64* -> i64
  %arg7878 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %arg7877 = add i64 0, 0                                                            ; quoted ()
  %rLp$_37append = call i64 @prim_make_45vector(i64 %arg7878, i64 %arg7877)          ; call prim_make_45vector
  %arg7880 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %arg7879 = add i64 0, 0                                                            ; quoted ()
  %buh$_37append2 = call i64 @prim_make_45vector(i64 %arg7880, i64 %arg7879)         ; call prim_make_45vector
  %arg7882 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr9528 = call i64* @alloc(i64 16)                                             ; malloc
  %eptr9530 = getelementptr inbounds i64, i64* %cloptr9528, i64 1                    ; &eptr9530[1]
  store i64 %buh$_37append2, i64* %eptr9530                                          ; *eptr9530 = %buh$_37append2
  %eptr9529 = getelementptr inbounds i64, i64* %cloptr9528, i64 0                    ; &cloptr9528[0]
  %f9531 = ptrtoint void(i64,i64,i64,i64)* @lam9137 to i64                           ; fptr cast; i64(...)* -> i64
  store i64 %f9531, i64* %eptr9529                                                   ; store fptr
  %arg7881 = ptrtoint i64* %cloptr9528 to i64                                        ; closure cast; i64* -> i64
  %EvR$_950 = call i64 @prim_vector_45set_33(i64 %buh$_37append2, i64 %arg7882, i64 %arg7881); call prim_vector_45set_33
  %arg7902 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr9532 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9534 = getelementptr inbounds i64, i64* %cloptr9532, i64 1                    ; &eptr9534[1]
  %eptr9535 = getelementptr inbounds i64, i64* %cloptr9532, i64 2                    ; &eptr9535[2]
  store i64 %rLp$_37append, i64* %eptr9534                                           ; *eptr9534 = %rLp$_37append
  store i64 %buh$_37append2, i64* %eptr9535                                          ; *eptr9535 = %buh$_37append2
  %eptr9533 = getelementptr inbounds i64, i64* %cloptr9532, i64 0                    ; &cloptr9532[0]
  %f9536 = ptrtoint void(i64,i64)* @lam9129 to i64                                   ; fptr cast; i64(...)* -> i64
  store i64 %f9536, i64* %eptr9533                                                   ; store fptr
  %arg7901 = ptrtoint i64* %cloptr9532 to i64                                        ; closure cast; i64* -> i64
  %BIo$_951 = call i64 @prim_vector_45set_33(i64 %rLp$_37append, i64 %arg7902, i64 %arg7901); call prim_vector_45set_33
  %arg7922 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7710 = call i64 @prim_vector_45ref(i64 %rLp$_37append, i64 %arg7922)       ; call prim_vector_45ref
  %cloptr9537 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9539 = getelementptr inbounds i64, i64* %cloptr9537, i64 1                    ; &eptr9539[1]
  %eptr9540 = getelementptr inbounds i64, i64* %cloptr9537, i64 2                    ; &eptr9540[2]
  %eptr9541 = getelementptr inbounds i64, i64* %cloptr9537, i64 3                    ; &eptr9541[3]
  store i64 %LdG$_37_62, i64* %eptr9539                                              ; *eptr9539 = %LdG$_37_62
  store i64 %dbd$_37foldl1, i64* %eptr9540                                           ; *eptr9540 = %dbd$_37foldl1
  store i64 %R74$_37length, i64* %eptr9541                                           ; *eptr9541 = %R74$_37length
  %eptr9538 = getelementptr inbounds i64, i64* %cloptr9537, i64 0                    ; &cloptr9537[0]
  %f9542 = ptrtoint void(i64,i64,i64)* @lam9120 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9542, i64* %eptr9538                                                   ; store fptr
  %arg7926 = ptrtoint i64* %cloptr9537 to i64                                        ; closure cast; i64* -> i64
  %arg7925 = add i64 0, 0                                                            ; quoted ()
  %cloptr9543 = inttoptr i64 %arg7926 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9544 = getelementptr inbounds i64, i64* %cloptr9543, i64 0                   ; &cloptr9543[0]
  %f9546 = load i64, i64* %i0ptr9544, align 8                                        ; load; *i0ptr9544
  %fptr9545 = inttoptr i64 %f9546 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9545(i64 %arg7926, i64 %arg7925, i64 %retprim7710)  ; tail call
  ret void
}


define void @lam9148(i64 %env9149, i64 %cont7607, i64 %EzE$a, i64 %Qjm$b) {
  %a7502 = call i64 @prim__60_61(i64 %EzE$a, i64 %Qjm$b)                             ; call prim__60_61
  %retprim7608 = call i64 @prim_not(i64 %a7502)                                      ; call prim_not
  %arg7869 = add i64 0, 0                                                            ; quoted ()
  %cloptr9547 = inttoptr i64 %cont7607 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9548 = getelementptr inbounds i64, i64* %cloptr9547, i64 0                   ; &cloptr9547[0]
  %f9550 = load i64, i64* %i0ptr9548, align 8                                        ; load; *i0ptr9548
  %fptr9549 = inttoptr i64 %f9550 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9549(i64 %cont7607, i64 %arg7869, i64 %retprim7608) ; tail call
  ret void
}


define void @lam9145(i64 %env9146, i64 %cont7609, i64 %jud$a, i64 %d5z$b) {
  %a7503 = call i64 @prim__60(i64 %jud$a, i64 %d5z$b)                                ; call prim__60
  %retprim7610 = call i64 @prim_not(i64 %a7503)                                      ; call prim_not
  %arg7875 = add i64 0, 0                                                            ; quoted ()
  %cloptr9551 = inttoptr i64 %cont7609 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9552 = getelementptr inbounds i64, i64* %cloptr9551, i64 0                   ; &cloptr9551[0]
  %f9554 = load i64, i64* %i0ptr9552, align 8                                        ; load; *i0ptr9552
  %fptr9553 = inttoptr i64 %f9554 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9553(i64 %cont7609, i64 %arg7875, i64 %retprim7610) ; tail call
  ret void
}


define void @lam9137(i64 %env9138, i64 %cont7703, i64 %OTH$ls0, i64 %tGZ$ls1) {
  %envptr9555 = inttoptr i64 %env9138 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9556 = getelementptr inbounds i64, i64* %envptr9555, i64 1                  ; &envptr9555[1]
  %buh$_37append2 = load i64, i64* %envptr9556, align 8                              ; load; *envptr9556
  %a7504 = call i64 @prim_null_63(i64 %OTH$ls0)                                      ; call prim_null_63
  %cmp9557 = icmp eq i64 %a7504, 15                                                  ; false?
  br i1 %cmp9557, label %else9559, label %then9558                                   ; if

then9558:
  %arg7886 = add i64 0, 0                                                            ; quoted ()
  %cloptr9560 = inttoptr i64 %cont7703 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9561 = getelementptr inbounds i64, i64* %cloptr9560, i64 0                   ; &cloptr9560[0]
  %f9563 = load i64, i64* %i0ptr9561, align 8                                        ; load; *i0ptr9561
  %fptr9562 = inttoptr i64 %f9563 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9562(i64 %cont7703, i64 %arg7886, i64 %tGZ$ls1)     ; tail call
  ret void

else9559:
  %a7505 = call i64 @prim_car(i64 %OTH$ls0)                                          ; call prim_car
  %arg7889 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7506 = call i64 @prim_vector_45ref(i64 %buh$_37append2, i64 %arg7889)            ; call prim_vector_45ref
  %a7507 = call i64 @prim_cdr(i64 %OTH$ls0)                                          ; call prim_cdr
  %cloptr9564 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9566 = getelementptr inbounds i64, i64* %cloptr9564, i64 1                    ; &eptr9566[1]
  %eptr9567 = getelementptr inbounds i64, i64* %cloptr9564, i64 2                    ; &eptr9567[2]
  store i64 %cont7703, i64* %eptr9566                                                ; *eptr9566 = %cont7703
  store i64 %a7505, i64* %eptr9567                                                   ; *eptr9567 = %a7505
  %eptr9565 = getelementptr inbounds i64, i64* %cloptr9564, i64 0                    ; &cloptr9564[0]
  %f9568 = ptrtoint void(i64,i64,i64)* @lam9134 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9568, i64* %eptr9565                                                   ; store fptr
  %arg7894 = ptrtoint i64* %cloptr9564 to i64                                        ; closure cast; i64* -> i64
  %cloptr9569 = inttoptr i64 %a7506 to i64*                                          ; closure/env cast; i64 -> i64*
  %i0ptr9570 = getelementptr inbounds i64, i64* %cloptr9569, i64 0                   ; &cloptr9569[0]
  %f9572 = load i64, i64* %i0ptr9570, align 8                                        ; load; *i0ptr9570
  %fptr9571 = inttoptr i64 %f9572 to void (i64,i64,i64,i64)*                         ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9571(i64 %a7506, i64 %arg7894, i64 %a7507, i64 %tGZ$ls1); tail call
  ret void
}


define void @lam9134(i64 %env9135, i64 %_957704, i64 %a7508) {
  %envptr9573 = inttoptr i64 %env9135 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9574 = getelementptr inbounds i64, i64* %envptr9573, i64 2                  ; &envptr9573[2]
  %a7505 = load i64, i64* %envptr9574, align 8                                       ; load; *envptr9574
  %envptr9575 = inttoptr i64 %env9135 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9576 = getelementptr inbounds i64, i64* %envptr9575, i64 1                  ; &envptr9575[1]
  %cont7703 = load i64, i64* %envptr9576, align 8                                    ; load; *envptr9576
  %retprim7705 = call i64 @prim_cons(i64 %a7505, i64 %a7508)                         ; call prim_cons
  %arg7899 = add i64 0, 0                                                            ; quoted ()
  %cloptr9577 = inttoptr i64 %cont7703 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9578 = getelementptr inbounds i64, i64* %cloptr9577, i64 0                   ; &cloptr9577[0]
  %f9580 = load i64, i64* %i0ptr9578, align 8                                        ; load; *i0ptr9578
  %fptr9579 = inttoptr i64 %f9580 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9579(i64 %cont7703, i64 %arg7899, i64 %retprim7705) ; tail call
  ret void
}


define void @lam9129(i64 %env9130, i64 %H9u$xs7707) {
  %envptr9581 = inttoptr i64 %env9130 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9582 = getelementptr inbounds i64, i64* %envptr9581, i64 2                  ; &envptr9581[2]
  %buh$_37append2 = load i64, i64* %envptr9582, align 8                              ; load; *envptr9582
  %envptr9583 = inttoptr i64 %env9130 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9584 = getelementptr inbounds i64, i64* %envptr9583, i64 1                  ; &envptr9583[1]
  %rLp$_37append = load i64, i64* %envptr9584, align 8                               ; load; *envptr9584
  %cont7706 = call i64 @prim_car(i64 %H9u$xs7707)                                    ; call prim_car
  %H9u$xs = call i64 @prim_cdr(i64 %H9u$xs7707)                                      ; call prim_cdr
  %a7509 = call i64 @prim_null_63(i64 %H9u$xs)                                       ; call prim_null_63
  %cmp9585 = icmp eq i64 %a7509, 15                                                  ; false?
  br i1 %cmp9585, label %else9587, label %then9586                                   ; if

then9586:
  %arg7908 = add i64 0, 0                                                            ; quoted ()
  %arg7907 = add i64 0, 0                                                            ; quoted ()
  %cloptr9588 = inttoptr i64 %cont7706 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9589 = getelementptr inbounds i64, i64* %cloptr9588, i64 0                   ; &cloptr9588[0]
  %f9591 = load i64, i64* %i0ptr9589, align 8                                        ; load; *i0ptr9589
  %fptr9590 = inttoptr i64 %f9591 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9590(i64 %cont7706, i64 %arg7908, i64 %arg7907)     ; tail call
  ret void

else9587:
  %SAz$hd = call i64 @prim_car(i64 %H9u$xs)                                          ; call prim_car
  %Hyu$tl = call i64 @prim_cdr(i64 %H9u$xs)                                          ; call prim_cdr
  %arg7912 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7510 = call i64 @prim_vector_45ref(i64 %rLp$_37append, i64 %arg7912)             ; call prim_vector_45ref
  %cloptr9592 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9594 = getelementptr inbounds i64, i64* %cloptr9592, i64 1                    ; &eptr9594[1]
  %eptr9595 = getelementptr inbounds i64, i64* %cloptr9592, i64 2                    ; &eptr9595[2]
  %eptr9596 = getelementptr inbounds i64, i64* %cloptr9592, i64 3                    ; &eptr9596[3]
  store i64 %SAz$hd, i64* %eptr9594                                                  ; *eptr9594 = %SAz$hd
  store i64 %buh$_37append2, i64* %eptr9595                                          ; *eptr9595 = %buh$_37append2
  store i64 %cont7706, i64* %eptr9596                                                ; *eptr9596 = %cont7706
  %eptr9593 = getelementptr inbounds i64, i64* %cloptr9592, i64 0                    ; &cloptr9592[0]
  %f9597 = ptrtoint void(i64,i64,i64)* @lam9126 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9597, i64* %eptr9593                                                   ; store fptr
  %arg7915 = ptrtoint i64* %cloptr9592 to i64                                        ; closure cast; i64* -> i64
  %cps_45lst7709 = call i64 @prim_cons(i64 %arg7915, i64 %Hyu$tl)                    ; call prim_cons
  %cloptr9598 = inttoptr i64 %a7510 to i64*                                          ; closure/env cast; i64 -> i64*
  %i0ptr9599 = getelementptr inbounds i64, i64* %cloptr9598, i64 0                   ; &cloptr9598[0]
  %f9601 = load i64, i64* %i0ptr9599, align 8                                        ; load; *i0ptr9599
  %fptr9600 = inttoptr i64 %f9601 to void (i64,i64)*                                 ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9600(i64 %a7510, i64 %cps_45lst7709)                ; tail call
  ret void
}


define void @lam9126(i64 %env9127, i64 %_957708, i64 %qCr$result1) {
  %envptr9602 = inttoptr i64 %env9127 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9603 = getelementptr inbounds i64, i64* %envptr9602, i64 3                  ; &envptr9602[3]
  %cont7706 = load i64, i64* %envptr9603, align 8                                    ; load; *envptr9603
  %envptr9604 = inttoptr i64 %env9127 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9605 = getelementptr inbounds i64, i64* %envptr9604, i64 2                  ; &envptr9604[2]
  %buh$_37append2 = load i64, i64* %envptr9605, align 8                              ; load; *envptr9605
  %envptr9606 = inttoptr i64 %env9127 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9607 = getelementptr inbounds i64, i64* %envptr9606, i64 1                  ; &envptr9606[1]
  %SAz$hd = load i64, i64* %envptr9607, align 8                                      ; load; *envptr9607
  %arg7916 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7511 = call i64 @prim_vector_45ref(i64 %buh$_37append2, i64 %arg7916)            ; call prim_vector_45ref
  %cloptr9608 = inttoptr i64 %a7511 to i64*                                          ; closure/env cast; i64 -> i64*
  %i0ptr9609 = getelementptr inbounds i64, i64* %cloptr9608, i64 0                   ; &cloptr9608[0]
  %f9611 = load i64, i64* %i0ptr9609, align 8                                        ; load; *i0ptr9609
  %fptr9610 = inttoptr i64 %f9611 to void (i64,i64,i64,i64)*                         ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9610(i64 %a7511, i64 %cont7706, i64 %SAz$hd, i64 %qCr$result1); tail call
  ret void
}


define void @lam9120(i64 %env9121, i64 %_957611, i64 %RXY$_37append) {
  %envptr9612 = inttoptr i64 %env9121 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9613 = getelementptr inbounds i64, i64* %envptr9612, i64 3                  ; &envptr9612[3]
  %R74$_37length = load i64, i64* %envptr9613, align 8                               ; load; *envptr9613
  %envptr9614 = inttoptr i64 %env9121 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9615 = getelementptr inbounds i64, i64* %envptr9614, i64 2                  ; &envptr9614[2]
  %dbd$_37foldl1 = load i64, i64* %envptr9615, align 8                               ; load; *envptr9615
  %envptr9616 = inttoptr i64 %env9121 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9617 = getelementptr inbounds i64, i64* %envptr9616, i64 1                  ; &envptr9616[1]
  %LdG$_37_62 = load i64, i64* %envptr9617, align 8                                  ; load; *envptr9617
  %cloptr9618 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9619 = getelementptr inbounds i64, i64* %cloptr9618, i64 0                    ; &cloptr9618[0]
  %f9620 = ptrtoint void(i64,i64,i64)* @lam9118 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9620, i64* %eptr9619                                                   ; store fptr
  %QIQ$_37list_63 = ptrtoint i64* %cloptr9618 to i64                                 ; closure cast; i64* -> i64
  %cloptr9621 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9622 = getelementptr inbounds i64, i64* %cloptr9621, i64 0                    ; &cloptr9621[0]
  %f9623 = ptrtoint void(i64,i64,i64,i64)* @lam9078 to i64                           ; fptr cast; i64(...)* -> i64
  store i64 %f9623, i64* %eptr9622                                                   ; store fptr
  %ajc$_37drop = ptrtoint i64* %cloptr9621 to i64                                    ; closure cast; i64* -> i64
  %cloptr9624 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9625 = getelementptr inbounds i64, i64* %cloptr9624, i64 0                    ; &cloptr9624[0]
  %f9626 = ptrtoint void(i64,i64,i64,i64)* @lam9038 to i64                           ; fptr cast; i64(...)* -> i64
  store i64 %f9626, i64* %eptr9625                                                   ; store fptr
  %KV3$_37memv = ptrtoint i64* %cloptr9624 to i64                                    ; closure cast; i64* -> i64
  %cloptr9627 = call i64* @alloc(i64 16)                                             ; malloc
  %eptr9629 = getelementptr inbounds i64, i64* %cloptr9627, i64 1                    ; &eptr9629[1]
  store i64 %dbd$_37foldl1, i64* %eptr9629                                           ; *eptr9629 = %dbd$_37foldl1
  %eptr9628 = getelementptr inbounds i64, i64* %cloptr9627, i64 0                    ; &cloptr9627[0]
  %f9630 = ptrtoint void(i64,i64)* @lam9007 to i64                                   ; fptr cast; i64(...)* -> i64
  store i64 %f9630, i64* %eptr9628                                                   ; store fptr
  %M5R$_37_47 = ptrtoint i64* %cloptr9627 to i64                                     ; closure cast; i64* -> i64
  %cloptr9631 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9632 = getelementptr inbounds i64, i64* %cloptr9631, i64 0                    ; &cloptr9631[0]
  %f9633 = ptrtoint void(i64,i64,i64)* @lam8999 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9633, i64* %eptr9632                                                   ; store fptr
  %CE8$_37first = ptrtoint i64* %cloptr9631 to i64                                   ; closure cast; i64* -> i64
  %cloptr9634 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9635 = getelementptr inbounds i64, i64* %cloptr9634, i64 0                    ; &cloptr9634[0]
  %f9636 = ptrtoint void(i64,i64,i64)* @lam8996 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9636, i64* %eptr9635                                                   ; store fptr
  %iHN$_37second = ptrtoint i64* %cloptr9634 to i64                                  ; closure cast; i64* -> i64
  %cloptr9637 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9638 = getelementptr inbounds i64, i64* %cloptr9637, i64 0                    ; &cloptr9637[0]
  %f9639 = ptrtoint void(i64,i64,i64)* @lam8993 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9639, i64* %eptr9638                                                   ; store fptr
  %yiM$_37third = ptrtoint i64* %cloptr9637 to i64                                   ; closure cast; i64* -> i64
  %cloptr9640 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9641 = getelementptr inbounds i64, i64* %cloptr9640, i64 0                    ; &cloptr9640[0]
  %f9642 = ptrtoint void(i64,i64,i64)* @lam8990 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9642, i64* %eptr9641                                                   ; store fptr
  %w0F$_37fourth = ptrtoint i64* %cloptr9640 to i64                                  ; closure cast; i64* -> i64
  %cloptr9643 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9644 = getelementptr inbounds i64, i64* %cloptr9643, i64 0                    ; &cloptr9643[0]
  %f9645 = ptrtoint void(i64,i64,i64)* @lam8987 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9645, i64* %eptr9644                                                   ; store fptr
  %QOG$promise_63 = ptrtoint i64* %cloptr9643 to i64                                 ; closure cast; i64* -> i64
  %cloptr9646 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9647 = getelementptr inbounds i64, i64* %cloptr9646, i64 0                    ; &cloptr9646[0]
  %f9648 = ptrtoint void(i64,i64)* @lam8981 to i64                                   ; fptr cast; i64(...)* -> i64
  store i64 %f9648, i64* %eptr9647                                                   ; store fptr
  %arg8188 = ptrtoint i64* %cloptr9646 to i64                                        ; closure cast; i64* -> i64
  %cloptr9649 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9651 = getelementptr inbounds i64, i64* %cloptr9649, i64 1                    ; &eptr9651[1]
  %eptr9652 = getelementptr inbounds i64, i64* %cloptr9649, i64 2                    ; &eptr9652[2]
  %eptr9653 = getelementptr inbounds i64, i64* %cloptr9649, i64 3                    ; &eptr9653[3]
  store i64 %LdG$_37_62, i64* %eptr9651                                              ; *eptr9651 = %LdG$_37_62
  store i64 %ajc$_37drop, i64* %eptr9652                                             ; *eptr9652 = %ajc$_37drop
  store i64 %R74$_37length, i64* %eptr9653                                           ; *eptr9653 = %R74$_37length
  %eptr9650 = getelementptr inbounds i64, i64* %cloptr9649, i64 0                    ; &cloptr9649[0]
  %f9654 = ptrtoint void(i64,i64,i64)* @lam8978 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9654, i64* %eptr9650                                                   ; store fptr
  %arg8187 = ptrtoint i64* %cloptr9649 to i64                                        ; closure cast; i64* -> i64
  %rva8703 = add i64 0, 0                                                            ; quoted ()
  %rva8702 = call i64 @prim_cons(i64 %arg8187, i64 %rva8703)                         ; call prim_cons
  %cloptr9655 = inttoptr i64 %arg8188 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9656 = getelementptr inbounds i64, i64* %cloptr9655, i64 0                   ; &cloptr9655[0]
  %f9658 = load i64, i64* %i0ptr9656, align 8                                        ; load; *i0ptr9656
  %fptr9657 = inttoptr i64 %f9658 to void (i64,i64)*                                 ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9657(i64 %arg8188, i64 %rva8702)                    ; tail call
  ret void
}


define void @lam9118(i64 %env9119, i64 %cont7612, i64 %clX$a) {
  %arg7928 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %HZw$a = call i64 @prim_make_45vector(i64 %arg7928, i64 %clX$a)                    ; call prim_make_45vector
  %cloptr9659 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9660 = getelementptr inbounds i64, i64* %cloptr9659, i64 0                    ; &cloptr9659[0]
  %f9661 = ptrtoint void(i64,i64,i64)* @lam9115 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9661, i64* %eptr9660                                                   ; store fptr
  %arg7931 = ptrtoint i64* %cloptr9659 to i64                                        ; closure cast; i64* -> i64
  %cloptr9662 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9664 = getelementptr inbounds i64, i64* %cloptr9662, i64 1                    ; &eptr9664[1]
  %eptr9665 = getelementptr inbounds i64, i64* %cloptr9662, i64 2                    ; &eptr9665[2]
  store i64 %HZw$a, i64* %eptr9664                                                   ; *eptr9664 = %HZw$a
  store i64 %cont7612, i64* %eptr9665                                                ; *eptr9665 = %cont7612
  %eptr9663 = getelementptr inbounds i64, i64* %cloptr9662, i64 0                    ; &cloptr9662[0]
  %f9666 = ptrtoint void(i64,i64,i64)* @lam9112 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9666, i64* %eptr9663                                                   ; store fptr
  %arg7930 = ptrtoint i64* %cloptr9662 to i64                                        ; closure cast; i64* -> i64
  %cloptr9667 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9669 = getelementptr inbounds i64, i64* %cloptr9667, i64 1                    ; &eptr9669[1]
  %eptr9670 = getelementptr inbounds i64, i64* %cloptr9667, i64 2                    ; &eptr9670[2]
  store i64 %HZw$a, i64* %eptr9669                                                   ; *eptr9669 = %HZw$a
  store i64 %cont7612, i64* %eptr9670                                                ; *eptr9670 = %cont7612
  %eptr9668 = getelementptr inbounds i64, i64* %cloptr9667, i64 0                    ; &cloptr9667[0]
  %f9671 = ptrtoint void(i64,i64,i64)* @lam9095 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9671, i64* %eptr9668                                                   ; store fptr
  %arg7929 = ptrtoint i64* %cloptr9667 to i64                                        ; closure cast; i64* -> i64
  %cloptr9672 = inttoptr i64 %arg7931 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9673 = getelementptr inbounds i64, i64* %cloptr9672, i64 0                   ; &cloptr9672[0]
  %f9675 = load i64, i64* %i0ptr9673, align 8                                        ; load; *i0ptr9673
  %fptr9674 = inttoptr i64 %f9675 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9674(i64 %arg7931, i64 %arg7930, i64 %arg7929)      ; tail call
  ret void
}


define void @lam9115(i64 %env9116, i64 %cont7618, i64 %O6y$k) {
  %arg7933 = add i64 0, 0                                                            ; quoted ()
  %cloptr9676 = inttoptr i64 %cont7618 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9677 = getelementptr inbounds i64, i64* %cloptr9676, i64 0                   ; &cloptr9676[0]
  %f9679 = load i64, i64* %i0ptr9677, align 8                                        ; load; *i0ptr9677
  %fptr9678 = inttoptr i64 %f9679 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9678(i64 %cont7618, i64 %arg7933, i64 %O6y$k)       ; tail call
  ret void
}


define void @lam9112(i64 %env9113, i64 %_957613, i64 %AzI$cc) {
  %envptr9680 = inttoptr i64 %env9113 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9681 = getelementptr inbounds i64, i64* %envptr9680, i64 2                  ; &envptr9680[2]
  %cont7612 = load i64, i64* %envptr9681, align 8                                    ; load; *envptr9681
  %envptr9682 = inttoptr i64 %env9113 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9683 = getelementptr inbounds i64, i64* %envptr9682, i64 1                  ; &envptr9682[1]
  %HZw$a = load i64, i64* %envptr9683, align 8                                       ; load; *envptr9683
  %arg7935 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7512 = call i64 @prim_vector_45ref(i64 %HZw$a, i64 %arg7935)                     ; call prim_vector_45ref
  %a7513 = call i64 @prim_null_63(i64 %a7512)                                        ; call prim_null_63
  %cmp9684 = icmp eq i64 %a7513, 15                                                  ; false?
  br i1 %cmp9684, label %else9686, label %then9685                                   ; if

then9685:
  %arg7939 = add i64 0, 0                                                            ; quoted ()
  %arg7938 = call i64 @const_init_true()                                             ; quoted #t
  %cloptr9687 = inttoptr i64 %cont7612 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9688 = getelementptr inbounds i64, i64* %cloptr9687, i64 0                   ; &cloptr9687[0]
  %f9690 = load i64, i64* %i0ptr9688, align 8                                        ; load; *i0ptr9688
  %fptr9689 = inttoptr i64 %f9690 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9689(i64 %cont7612, i64 %arg7939, i64 %arg7938)     ; tail call
  ret void

else9686:
  %arg7941 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7514 = call i64 @prim_vector_45ref(i64 %HZw$a, i64 %arg7941)                     ; call prim_vector_45ref
  %a7515 = call i64 @prim_cons_63(i64 %a7514)                                        ; call prim_cons_63
  %cmp9691 = icmp eq i64 %a7515, 15                                                  ; false?
  br i1 %cmp9691, label %else9693, label %then9692                                   ; if

then9692:
  %arg7944 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7516 = call i64 @prim_vector_45ref(i64 %HZw$a, i64 %arg7944)                     ; call prim_vector_45ref
  %retprim7617 = call i64 @prim_cdr(i64 %a7516)                                      ; call prim_cdr
  %cloptr9694 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9696 = getelementptr inbounds i64, i64* %cloptr9694, i64 1                    ; &eptr9696[1]
  %eptr9697 = getelementptr inbounds i64, i64* %cloptr9694, i64 2                    ; &eptr9697[2]
  %eptr9698 = getelementptr inbounds i64, i64* %cloptr9694, i64 3                    ; &eptr9698[3]
  store i64 %HZw$a, i64* %eptr9696                                                   ; *eptr9696 = %HZw$a
  store i64 %AzI$cc, i64* %eptr9697                                                  ; *eptr9697 = %AzI$cc
  store i64 %cont7612, i64* %eptr9698                                                ; *eptr9698 = %cont7612
  %eptr9695 = getelementptr inbounds i64, i64* %cloptr9694, i64 0                    ; &cloptr9694[0]
  %f9699 = ptrtoint void(i64,i64,i64)* @lam9105 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9699, i64* %eptr9695                                                   ; store fptr
  %arg7949 = ptrtoint i64* %cloptr9694 to i64                                        ; closure cast; i64* -> i64
  %arg7948 = add i64 0, 0                                                            ; quoted ()
  %cloptr9700 = inttoptr i64 %arg7949 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9701 = getelementptr inbounds i64, i64* %cloptr9700, i64 0                   ; &cloptr9700[0]
  %f9703 = load i64, i64* %i0ptr9701, align 8                                        ; load; *i0ptr9701
  %fptr9702 = inttoptr i64 %f9703 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9702(i64 %arg7949, i64 %arg7948, i64 %retprim7617)  ; tail call
  ret void

else9693:
  %arg7963 = add i64 0, 0                                                            ; quoted ()
  %arg7962 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr9704 = inttoptr i64 %cont7612 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9705 = getelementptr inbounds i64, i64* %cloptr9704, i64 0                   ; &cloptr9704[0]
  %f9707 = load i64, i64* %i0ptr9705, align 8                                        ; load; *i0ptr9705
  %fptr9706 = inttoptr i64 %f9707 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9706(i64 %cont7612, i64 %arg7963, i64 %arg7962)     ; tail call
  ret void
}


define void @lam9105(i64 %env9106, i64 %_957614, i64 %OoV$b) {
  %envptr9708 = inttoptr i64 %env9106 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9709 = getelementptr inbounds i64, i64* %envptr9708, i64 3                  ; &envptr9708[3]
  %cont7612 = load i64, i64* %envptr9709, align 8                                    ; load; *envptr9709
  %envptr9710 = inttoptr i64 %env9106 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9711 = getelementptr inbounds i64, i64* %envptr9710, i64 2                  ; &envptr9710[2]
  %AzI$cc = load i64, i64* %envptr9711, align 8                                      ; load; *envptr9711
  %envptr9712 = inttoptr i64 %env9106 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9713 = getelementptr inbounds i64, i64* %envptr9712, i64 1                  ; &envptr9712[1]
  %HZw$a = load i64, i64* %envptr9713, align 8                                       ; load; *envptr9713
  %arg7950 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7517 = call i64 @prim_vector_45ref(i64 %HZw$a, i64 %arg7950)                     ; call prim_vector_45ref
  %a7518 = call i64 @prim_cdr(i64 %a7517)                                            ; call prim_cdr
  %arg7954 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7616 = call i64 @prim_vector_45set_33(i64 %HZw$a, i64 %arg7954, i64 %a7518); call prim_vector_45set_33
  %cloptr9714 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9716 = getelementptr inbounds i64, i64* %cloptr9714, i64 1                    ; &eptr9716[1]
  %eptr9717 = getelementptr inbounds i64, i64* %cloptr9714, i64 2                    ; &eptr9717[2]
  store i64 %AzI$cc, i64* %eptr9716                                                  ; *eptr9716 = %AzI$cc
  store i64 %cont7612, i64* %eptr9717                                                ; *eptr9717 = %cont7612
  %eptr9715 = getelementptr inbounds i64, i64* %cloptr9714, i64 0                    ; &cloptr9714[0]
  %f9718 = ptrtoint void(i64,i64,i64)* @lam9101 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9718, i64* %eptr9715                                                   ; store fptr
  %arg7958 = ptrtoint i64* %cloptr9714 to i64                                        ; closure cast; i64* -> i64
  %arg7957 = add i64 0, 0                                                            ; quoted ()
  %cloptr9719 = inttoptr i64 %arg7958 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9720 = getelementptr inbounds i64, i64* %cloptr9719, i64 0                   ; &cloptr9719[0]
  %f9722 = load i64, i64* %i0ptr9720, align 8                                        ; load; *i0ptr9720
  %fptr9721 = inttoptr i64 %f9722 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9721(i64 %arg7958, i64 %arg7957, i64 %retprim7616)  ; tail call
  ret void
}


define void @lam9101(i64 %env9102, i64 %_957615, i64 %gtT$_950) {
  %envptr9723 = inttoptr i64 %env9102 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9724 = getelementptr inbounds i64, i64* %envptr9723, i64 2                  ; &envptr9723[2]
  %cont7612 = load i64, i64* %envptr9724, align 8                                    ; load; *envptr9724
  %envptr9725 = inttoptr i64 %env9102 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9726 = getelementptr inbounds i64, i64* %envptr9725, i64 1                  ; &envptr9725[1]
  %AzI$cc = load i64, i64* %envptr9726, align 8                                      ; load; *envptr9726
  %cloptr9727 = inttoptr i64 %AzI$cc to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr9728 = getelementptr inbounds i64, i64* %cloptr9727, i64 0                   ; &cloptr9727[0]
  %f9730 = load i64, i64* %i0ptr9728, align 8                                        ; load; *i0ptr9728
  %fptr9729 = inttoptr i64 %f9730 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9729(i64 %AzI$cc, i64 %cont7612, i64 %AzI$cc)       ; tail call
  ret void
}


define void @lam9095(i64 %env9096, i64 %_957613, i64 %AzI$cc) {
  %envptr9731 = inttoptr i64 %env9096 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9732 = getelementptr inbounds i64, i64* %envptr9731, i64 2                  ; &envptr9731[2]
  %cont7612 = load i64, i64* %envptr9732, align 8                                    ; load; *envptr9732
  %envptr9733 = inttoptr i64 %env9096 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9734 = getelementptr inbounds i64, i64* %envptr9733, i64 1                  ; &envptr9733[1]
  %HZw$a = load i64, i64* %envptr9734, align 8                                       ; load; *envptr9734
  %arg7965 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7512 = call i64 @prim_vector_45ref(i64 %HZw$a, i64 %arg7965)                     ; call prim_vector_45ref
  %a7513 = call i64 @prim_null_63(i64 %a7512)                                        ; call prim_null_63
  %cmp9735 = icmp eq i64 %a7513, 15                                                  ; false?
  br i1 %cmp9735, label %else9737, label %then9736                                   ; if

then9736:
  %arg7969 = add i64 0, 0                                                            ; quoted ()
  %arg7968 = call i64 @const_init_true()                                             ; quoted #t
  %cloptr9738 = inttoptr i64 %cont7612 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9739 = getelementptr inbounds i64, i64* %cloptr9738, i64 0                   ; &cloptr9738[0]
  %f9741 = load i64, i64* %i0ptr9739, align 8                                        ; load; *i0ptr9739
  %fptr9740 = inttoptr i64 %f9741 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9740(i64 %cont7612, i64 %arg7969, i64 %arg7968)     ; tail call
  ret void

else9737:
  %arg7971 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7514 = call i64 @prim_vector_45ref(i64 %HZw$a, i64 %arg7971)                     ; call prim_vector_45ref
  %a7515 = call i64 @prim_cons_63(i64 %a7514)                                        ; call prim_cons_63
  %cmp9742 = icmp eq i64 %a7515, 15                                                  ; false?
  br i1 %cmp9742, label %else9744, label %then9743                                   ; if

then9743:
  %arg7974 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7516 = call i64 @prim_vector_45ref(i64 %HZw$a, i64 %arg7974)                     ; call prim_vector_45ref
  %retprim7617 = call i64 @prim_cdr(i64 %a7516)                                      ; call prim_cdr
  %cloptr9745 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9747 = getelementptr inbounds i64, i64* %cloptr9745, i64 1                    ; &eptr9747[1]
  %eptr9748 = getelementptr inbounds i64, i64* %cloptr9745, i64 2                    ; &eptr9748[2]
  %eptr9749 = getelementptr inbounds i64, i64* %cloptr9745, i64 3                    ; &eptr9749[3]
  store i64 %HZw$a, i64* %eptr9747                                                   ; *eptr9747 = %HZw$a
  store i64 %AzI$cc, i64* %eptr9748                                                  ; *eptr9748 = %AzI$cc
  store i64 %cont7612, i64* %eptr9749                                                ; *eptr9749 = %cont7612
  %eptr9746 = getelementptr inbounds i64, i64* %cloptr9745, i64 0                    ; &cloptr9745[0]
  %f9750 = ptrtoint void(i64,i64,i64)* @lam9088 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9750, i64* %eptr9746                                                   ; store fptr
  %arg7979 = ptrtoint i64* %cloptr9745 to i64                                        ; closure cast; i64* -> i64
  %arg7978 = add i64 0, 0                                                            ; quoted ()
  %cloptr9751 = inttoptr i64 %arg7979 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9752 = getelementptr inbounds i64, i64* %cloptr9751, i64 0                   ; &cloptr9751[0]
  %f9754 = load i64, i64* %i0ptr9752, align 8                                        ; load; *i0ptr9752
  %fptr9753 = inttoptr i64 %f9754 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9753(i64 %arg7979, i64 %arg7978, i64 %retprim7617)  ; tail call
  ret void

else9744:
  %arg7993 = add i64 0, 0                                                            ; quoted ()
  %arg7992 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr9755 = inttoptr i64 %cont7612 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9756 = getelementptr inbounds i64, i64* %cloptr9755, i64 0                   ; &cloptr9755[0]
  %f9758 = load i64, i64* %i0ptr9756, align 8                                        ; load; *i0ptr9756
  %fptr9757 = inttoptr i64 %f9758 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9757(i64 %cont7612, i64 %arg7993, i64 %arg7992)     ; tail call
  ret void
}


define void @lam9088(i64 %env9089, i64 %_957614, i64 %OoV$b) {
  %envptr9759 = inttoptr i64 %env9089 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9760 = getelementptr inbounds i64, i64* %envptr9759, i64 3                  ; &envptr9759[3]
  %cont7612 = load i64, i64* %envptr9760, align 8                                    ; load; *envptr9760
  %envptr9761 = inttoptr i64 %env9089 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9762 = getelementptr inbounds i64, i64* %envptr9761, i64 2                  ; &envptr9761[2]
  %AzI$cc = load i64, i64* %envptr9762, align 8                                      ; load; *envptr9762
  %envptr9763 = inttoptr i64 %env9089 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9764 = getelementptr inbounds i64, i64* %envptr9763, i64 1                  ; &envptr9763[1]
  %HZw$a = load i64, i64* %envptr9764, align 8                                       ; load; *envptr9764
  %arg7980 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7517 = call i64 @prim_vector_45ref(i64 %HZw$a, i64 %arg7980)                     ; call prim_vector_45ref
  %a7518 = call i64 @prim_cdr(i64 %a7517)                                            ; call prim_cdr
  %arg7984 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7616 = call i64 @prim_vector_45set_33(i64 %HZw$a, i64 %arg7984, i64 %a7518); call prim_vector_45set_33
  %cloptr9765 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9767 = getelementptr inbounds i64, i64* %cloptr9765, i64 1                    ; &eptr9767[1]
  %eptr9768 = getelementptr inbounds i64, i64* %cloptr9765, i64 2                    ; &eptr9768[2]
  store i64 %AzI$cc, i64* %eptr9767                                                  ; *eptr9767 = %AzI$cc
  store i64 %cont7612, i64* %eptr9768                                                ; *eptr9768 = %cont7612
  %eptr9766 = getelementptr inbounds i64, i64* %cloptr9765, i64 0                    ; &cloptr9765[0]
  %f9769 = ptrtoint void(i64,i64,i64)* @lam9084 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9769, i64* %eptr9766                                                   ; store fptr
  %arg7988 = ptrtoint i64* %cloptr9765 to i64                                        ; closure cast; i64* -> i64
  %arg7987 = add i64 0, 0                                                            ; quoted ()
  %cloptr9770 = inttoptr i64 %arg7988 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9771 = getelementptr inbounds i64, i64* %cloptr9770, i64 0                   ; &cloptr9770[0]
  %f9773 = load i64, i64* %i0ptr9771, align 8                                        ; load; *i0ptr9771
  %fptr9772 = inttoptr i64 %f9773 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9772(i64 %arg7988, i64 %arg7987, i64 %retprim7616)  ; tail call
  ret void
}


define void @lam9084(i64 %env9085, i64 %_957615, i64 %gtT$_950) {
  %envptr9774 = inttoptr i64 %env9085 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9775 = getelementptr inbounds i64, i64* %envptr9774, i64 2                  ; &envptr9774[2]
  %cont7612 = load i64, i64* %envptr9775, align 8                                    ; load; *envptr9775
  %envptr9776 = inttoptr i64 %env9085 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9777 = getelementptr inbounds i64, i64* %envptr9776, i64 1                  ; &envptr9776[1]
  %AzI$cc = load i64, i64* %envptr9777, align 8                                      ; load; *envptr9777
  %cloptr9778 = inttoptr i64 %AzI$cc to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr9779 = getelementptr inbounds i64, i64* %cloptr9778, i64 0                   ; &cloptr9778[0]
  %f9781 = load i64, i64* %i0ptr9779, align 8                                        ; load; *i0ptr9779
  %fptr9780 = inttoptr i64 %f9781 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9780(i64 %AzI$cc, i64 %cont7612, i64 %AzI$cc)       ; tail call
  ret void
}


define void @lam9078(i64 %env9079, i64 %cont7619, i64 %O6i$lst, i64 %kBM$n) {
  %arg7996 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %Cg5$lst = call i64 @prim_make_45vector(i64 %arg7996, i64 %O6i$lst)                ; call prim_make_45vector
  %arg7998 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %uX1$n = call i64 @prim_make_45vector(i64 %arg7998, i64 %kBM$n)                    ; call prim_make_45vector
  %cloptr9782 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9783 = getelementptr inbounds i64, i64* %cloptr9782, i64 0                    ; &cloptr9782[0]
  %f9784 = ptrtoint void(i64,i64,i64)* @lam9074 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9784, i64* %eptr9783                                                   ; store fptr
  %arg8001 = ptrtoint i64* %cloptr9782 to i64                                        ; closure cast; i64* -> i64
  %cloptr9785 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9787 = getelementptr inbounds i64, i64* %cloptr9785, i64 1                    ; &eptr9787[1]
  %eptr9788 = getelementptr inbounds i64, i64* %cloptr9785, i64 2                    ; &eptr9788[2]
  %eptr9789 = getelementptr inbounds i64, i64* %cloptr9785, i64 3                    ; &eptr9789[3]
  store i64 %uX1$n, i64* %eptr9787                                                   ; *eptr9787 = %uX1$n
  store i64 %Cg5$lst, i64* %eptr9788                                                 ; *eptr9788 = %Cg5$lst
  store i64 %cont7619, i64* %eptr9789                                                ; *eptr9789 = %cont7619
  %eptr9786 = getelementptr inbounds i64, i64* %cloptr9785, i64 0                    ; &cloptr9785[0]
  %f9790 = ptrtoint void(i64,i64,i64)* @lam9072 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9790, i64* %eptr9786                                                   ; store fptr
  %arg8000 = ptrtoint i64* %cloptr9785 to i64                                        ; closure cast; i64* -> i64
  %cloptr9791 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9793 = getelementptr inbounds i64, i64* %cloptr9791, i64 1                    ; &eptr9793[1]
  %eptr9794 = getelementptr inbounds i64, i64* %cloptr9791, i64 2                    ; &eptr9794[2]
  %eptr9795 = getelementptr inbounds i64, i64* %cloptr9791, i64 3                    ; &eptr9795[3]
  store i64 %uX1$n, i64* %eptr9793                                                   ; *eptr9793 = %uX1$n
  store i64 %Cg5$lst, i64* %eptr9794                                                 ; *eptr9794 = %Cg5$lst
  store i64 %cont7619, i64* %eptr9795                                                ; *eptr9795 = %cont7619
  %eptr9792 = getelementptr inbounds i64, i64* %cloptr9791, i64 0                    ; &cloptr9791[0]
  %f9796 = ptrtoint void(i64,i64,i64)* @lam9055 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9796, i64* %eptr9792                                                   ; store fptr
  %arg7999 = ptrtoint i64* %cloptr9791 to i64                                        ; closure cast; i64* -> i64
  %cloptr9797 = inttoptr i64 %arg8001 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9798 = getelementptr inbounds i64, i64* %cloptr9797, i64 0                   ; &cloptr9797[0]
  %f9800 = load i64, i64* %i0ptr9798, align 8                                        ; load; *i0ptr9798
  %fptr9799 = inttoptr i64 %f9800 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9799(i64 %arg8001, i64 %arg8000, i64 %arg7999)      ; tail call
  ret void
}


define void @lam9074(i64 %env9075, i64 %cont7626, i64 %Xnd$u) {
  %cloptr9801 = inttoptr i64 %Xnd$u to i64*                                          ; closure/env cast; i64 -> i64*
  %i0ptr9802 = getelementptr inbounds i64, i64* %cloptr9801, i64 0                   ; &cloptr9801[0]
  %f9804 = load i64, i64* %i0ptr9802, align 8                                        ; load; *i0ptr9802
  %fptr9803 = inttoptr i64 %f9804 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9803(i64 %Xnd$u, i64 %cont7626, i64 %Xnd$u)         ; tail call
  ret void
}


define void @lam9072(i64 %env9073, i64 %_957620, i64 %GHO$cc) {
  %envptr9805 = inttoptr i64 %env9073 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9806 = getelementptr inbounds i64, i64* %envptr9805, i64 3                  ; &envptr9805[3]
  %cont7619 = load i64, i64* %envptr9806, align 8                                    ; load; *envptr9806
  %envptr9807 = inttoptr i64 %env9073 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9808 = getelementptr inbounds i64, i64* %envptr9807, i64 2                  ; &envptr9807[2]
  %Cg5$lst = load i64, i64* %envptr9808, align 8                                     ; load; *envptr9808
  %envptr9809 = inttoptr i64 %env9073 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9810 = getelementptr inbounds i64, i64* %envptr9809, i64 1                  ; &envptr9809[1]
  %uX1$n = load i64, i64* %envptr9810, align 8                                       ; load; *envptr9810
  %arg8005 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7519 = call i64 @prim_vector_45ref(i64 %uX1$n, i64 %arg8005)                     ; call prim_vector_45ref
  %arg8008 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7520 = call i64 @prim__61(i64 %arg8008, i64 %a7519)                              ; call prim__61
  %cmp9811 = icmp eq i64 %a7520, 15                                                  ; false?
  br i1 %cmp9811, label %else9813, label %then9812                                   ; if

then9812:
  %arg8009 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7621 = call i64 @prim_vector_45ref(i64 %Cg5$lst, i64 %arg8009)             ; call prim_vector_45ref
  %arg8012 = add i64 0, 0                                                            ; quoted ()
  %cloptr9814 = inttoptr i64 %cont7619 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9815 = getelementptr inbounds i64, i64* %cloptr9814, i64 0                   ; &cloptr9814[0]
  %f9817 = load i64, i64* %i0ptr9815, align 8                                        ; load; *i0ptr9815
  %fptr9816 = inttoptr i64 %f9817 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9816(i64 %cont7619, i64 %arg8012, i64 %retprim7621) ; tail call
  ret void

else9813:
  %arg8014 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7521 = call i64 @prim_vector_45ref(i64 %Cg5$lst, i64 %arg8014)                   ; call prim_vector_45ref
  %a7522 = call i64 @prim_cdr(i64 %a7521)                                            ; call prim_cdr
  %arg8018 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7625 = call i64 @prim_vector_45set_33(i64 %Cg5$lst, i64 %arg8018, i64 %a7522); call prim_vector_45set_33
  %cloptr9818 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9820 = getelementptr inbounds i64, i64* %cloptr9818, i64 1                    ; &eptr9820[1]
  %eptr9821 = getelementptr inbounds i64, i64* %cloptr9818, i64 2                    ; &eptr9821[2]
  %eptr9822 = getelementptr inbounds i64, i64* %cloptr9818, i64 3                    ; &eptr9822[3]
  store i64 %uX1$n, i64* %eptr9820                                                   ; *eptr9820 = %uX1$n
  store i64 %GHO$cc, i64* %eptr9821                                                  ; *eptr9821 = %GHO$cc
  store i64 %cont7619, i64* %eptr9822                                                ; *eptr9822 = %cont7619
  %eptr9819 = getelementptr inbounds i64, i64* %cloptr9818, i64 0                    ; &cloptr9818[0]
  %f9823 = ptrtoint void(i64,i64,i64)* @lam9066 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9823, i64* %eptr9819                                                   ; store fptr
  %arg8022 = ptrtoint i64* %cloptr9818 to i64                                        ; closure cast; i64* -> i64
  %arg8021 = add i64 0, 0                                                            ; quoted ()
  %cloptr9824 = inttoptr i64 %arg8022 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9825 = getelementptr inbounds i64, i64* %cloptr9824, i64 0                   ; &cloptr9824[0]
  %f9827 = load i64, i64* %i0ptr9825, align 8                                        ; load; *i0ptr9825
  %fptr9826 = inttoptr i64 %f9827 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9826(i64 %arg8022, i64 %arg8021, i64 %retprim7625)  ; tail call
  ret void
}


define void @lam9066(i64 %env9067, i64 %_957622, i64 %cIm$_950) {
  %envptr9828 = inttoptr i64 %env9067 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9829 = getelementptr inbounds i64, i64* %envptr9828, i64 3                  ; &envptr9828[3]
  %cont7619 = load i64, i64* %envptr9829, align 8                                    ; load; *envptr9829
  %envptr9830 = inttoptr i64 %env9067 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9831 = getelementptr inbounds i64, i64* %envptr9830, i64 2                  ; &envptr9830[2]
  %GHO$cc = load i64, i64* %envptr9831, align 8                                      ; load; *envptr9831
  %envptr9832 = inttoptr i64 %env9067 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9833 = getelementptr inbounds i64, i64* %envptr9832, i64 1                  ; &envptr9832[1]
  %uX1$n = load i64, i64* %envptr9833, align 8                                       ; load; *envptr9833
  %arg8023 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7523 = call i64 @prim_vector_45ref(i64 %uX1$n, i64 %arg8023)                     ; call prim_vector_45ref
  %arg8025 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %a7524 = call i64 @prim__45(i64 %a7523, i64 %arg8025)                              ; call prim__45
  %arg8028 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7624 = call i64 @prim_vector_45set_33(i64 %uX1$n, i64 %arg8028, i64 %a7524); call prim_vector_45set_33
  %cloptr9834 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9836 = getelementptr inbounds i64, i64* %cloptr9834, i64 1                    ; &eptr9836[1]
  %eptr9837 = getelementptr inbounds i64, i64* %cloptr9834, i64 2                    ; &eptr9837[2]
  store i64 %GHO$cc, i64* %eptr9836                                                  ; *eptr9836 = %GHO$cc
  store i64 %cont7619, i64* %eptr9837                                                ; *eptr9837 = %cont7619
  %eptr9835 = getelementptr inbounds i64, i64* %cloptr9834, i64 0                    ; &cloptr9834[0]
  %f9838 = ptrtoint void(i64,i64,i64)* @lam9061 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9838, i64* %eptr9835                                                   ; store fptr
  %arg8032 = ptrtoint i64* %cloptr9834 to i64                                        ; closure cast; i64* -> i64
  %arg8031 = add i64 0, 0                                                            ; quoted ()
  %cloptr9839 = inttoptr i64 %arg8032 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9840 = getelementptr inbounds i64, i64* %cloptr9839, i64 0                   ; &cloptr9839[0]
  %f9842 = load i64, i64* %i0ptr9840, align 8                                        ; load; *i0ptr9840
  %fptr9841 = inttoptr i64 %f9842 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9841(i64 %arg8032, i64 %arg8031, i64 %retprim7624)  ; tail call
  ret void
}


define void @lam9061(i64 %env9062, i64 %_957623, i64 %mkB$_951) {
  %envptr9843 = inttoptr i64 %env9062 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9844 = getelementptr inbounds i64, i64* %envptr9843, i64 2                  ; &envptr9843[2]
  %cont7619 = load i64, i64* %envptr9844, align 8                                    ; load; *envptr9844
  %envptr9845 = inttoptr i64 %env9062 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9846 = getelementptr inbounds i64, i64* %envptr9845, i64 1                  ; &envptr9845[1]
  %GHO$cc = load i64, i64* %envptr9846, align 8                                      ; load; *envptr9846
  %cloptr9847 = inttoptr i64 %GHO$cc to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr9848 = getelementptr inbounds i64, i64* %cloptr9847, i64 0                   ; &cloptr9847[0]
  %f9850 = load i64, i64* %i0ptr9848, align 8                                        ; load; *i0ptr9848
  %fptr9849 = inttoptr i64 %f9850 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9849(i64 %GHO$cc, i64 %cont7619, i64 %GHO$cc)       ; tail call
  ret void
}


define void @lam9055(i64 %env9056, i64 %_957620, i64 %GHO$cc) {
  %envptr9851 = inttoptr i64 %env9056 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9852 = getelementptr inbounds i64, i64* %envptr9851, i64 3                  ; &envptr9851[3]
  %cont7619 = load i64, i64* %envptr9852, align 8                                    ; load; *envptr9852
  %envptr9853 = inttoptr i64 %env9056 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9854 = getelementptr inbounds i64, i64* %envptr9853, i64 2                  ; &envptr9853[2]
  %Cg5$lst = load i64, i64* %envptr9854, align 8                                     ; load; *envptr9854
  %envptr9855 = inttoptr i64 %env9056 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9856 = getelementptr inbounds i64, i64* %envptr9855, i64 1                  ; &envptr9855[1]
  %uX1$n = load i64, i64* %envptr9856, align 8                                       ; load; *envptr9856
  %arg8036 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7519 = call i64 @prim_vector_45ref(i64 %uX1$n, i64 %arg8036)                     ; call prim_vector_45ref
  %arg8039 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7520 = call i64 @prim__61(i64 %arg8039, i64 %a7519)                              ; call prim__61
  %cmp9857 = icmp eq i64 %a7520, 15                                                  ; false?
  br i1 %cmp9857, label %else9859, label %then9858                                   ; if

then9858:
  %arg8040 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7621 = call i64 @prim_vector_45ref(i64 %Cg5$lst, i64 %arg8040)             ; call prim_vector_45ref
  %arg8043 = add i64 0, 0                                                            ; quoted ()
  %cloptr9860 = inttoptr i64 %cont7619 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9861 = getelementptr inbounds i64, i64* %cloptr9860, i64 0                   ; &cloptr9860[0]
  %f9863 = load i64, i64* %i0ptr9861, align 8                                        ; load; *i0ptr9861
  %fptr9862 = inttoptr i64 %f9863 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9862(i64 %cont7619, i64 %arg8043, i64 %retprim7621) ; tail call
  ret void

else9859:
  %arg8045 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7521 = call i64 @prim_vector_45ref(i64 %Cg5$lst, i64 %arg8045)                   ; call prim_vector_45ref
  %a7522 = call i64 @prim_cdr(i64 %a7521)                                            ; call prim_cdr
  %arg8049 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7625 = call i64 @prim_vector_45set_33(i64 %Cg5$lst, i64 %arg8049, i64 %a7522); call prim_vector_45set_33
  %cloptr9864 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9866 = getelementptr inbounds i64, i64* %cloptr9864, i64 1                    ; &eptr9866[1]
  %eptr9867 = getelementptr inbounds i64, i64* %cloptr9864, i64 2                    ; &eptr9867[2]
  %eptr9868 = getelementptr inbounds i64, i64* %cloptr9864, i64 3                    ; &eptr9868[3]
  store i64 %uX1$n, i64* %eptr9866                                                   ; *eptr9866 = %uX1$n
  store i64 %GHO$cc, i64* %eptr9867                                                  ; *eptr9867 = %GHO$cc
  store i64 %cont7619, i64* %eptr9868                                                ; *eptr9868 = %cont7619
  %eptr9865 = getelementptr inbounds i64, i64* %cloptr9864, i64 0                    ; &cloptr9864[0]
  %f9869 = ptrtoint void(i64,i64,i64)* @lam9049 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9869, i64* %eptr9865                                                   ; store fptr
  %arg8053 = ptrtoint i64* %cloptr9864 to i64                                        ; closure cast; i64* -> i64
  %arg8052 = add i64 0, 0                                                            ; quoted ()
  %cloptr9870 = inttoptr i64 %arg8053 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9871 = getelementptr inbounds i64, i64* %cloptr9870, i64 0                   ; &cloptr9870[0]
  %f9873 = load i64, i64* %i0ptr9871, align 8                                        ; load; *i0ptr9871
  %fptr9872 = inttoptr i64 %f9873 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9872(i64 %arg8053, i64 %arg8052, i64 %retprim7625)  ; tail call
  ret void
}


define void @lam9049(i64 %env9050, i64 %_957622, i64 %cIm$_950) {
  %envptr9874 = inttoptr i64 %env9050 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9875 = getelementptr inbounds i64, i64* %envptr9874, i64 3                  ; &envptr9874[3]
  %cont7619 = load i64, i64* %envptr9875, align 8                                    ; load; *envptr9875
  %envptr9876 = inttoptr i64 %env9050 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9877 = getelementptr inbounds i64, i64* %envptr9876, i64 2                  ; &envptr9876[2]
  %GHO$cc = load i64, i64* %envptr9877, align 8                                      ; load; *envptr9877
  %envptr9878 = inttoptr i64 %env9050 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9879 = getelementptr inbounds i64, i64* %envptr9878, i64 1                  ; &envptr9878[1]
  %uX1$n = load i64, i64* %envptr9879, align 8                                       ; load; *envptr9879
  %arg8054 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7523 = call i64 @prim_vector_45ref(i64 %uX1$n, i64 %arg8054)                     ; call prim_vector_45ref
  %arg8056 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %a7524 = call i64 @prim__45(i64 %a7523, i64 %arg8056)                              ; call prim__45
  %arg8059 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7624 = call i64 @prim_vector_45set_33(i64 %uX1$n, i64 %arg8059, i64 %a7524); call prim_vector_45set_33
  %cloptr9880 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9882 = getelementptr inbounds i64, i64* %cloptr9880, i64 1                    ; &eptr9882[1]
  %eptr9883 = getelementptr inbounds i64, i64* %cloptr9880, i64 2                    ; &eptr9883[2]
  store i64 %GHO$cc, i64* %eptr9882                                                  ; *eptr9882 = %GHO$cc
  store i64 %cont7619, i64* %eptr9883                                                ; *eptr9883 = %cont7619
  %eptr9881 = getelementptr inbounds i64, i64* %cloptr9880, i64 0                    ; &cloptr9880[0]
  %f9884 = ptrtoint void(i64,i64,i64)* @lam9044 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9884, i64* %eptr9881                                                   ; store fptr
  %arg8063 = ptrtoint i64* %cloptr9880 to i64                                        ; closure cast; i64* -> i64
  %arg8062 = add i64 0, 0                                                            ; quoted ()
  %cloptr9885 = inttoptr i64 %arg8063 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9886 = getelementptr inbounds i64, i64* %cloptr9885, i64 0                   ; &cloptr9885[0]
  %f9888 = load i64, i64* %i0ptr9886, align 8                                        ; load; *i0ptr9886
  %fptr9887 = inttoptr i64 %f9888 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9887(i64 %arg8063, i64 %arg8062, i64 %retprim7624)  ; tail call
  ret void
}


define void @lam9044(i64 %env9045, i64 %_957623, i64 %mkB$_951) {
  %envptr9889 = inttoptr i64 %env9045 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9890 = getelementptr inbounds i64, i64* %envptr9889, i64 2                  ; &envptr9889[2]
  %cont7619 = load i64, i64* %envptr9890, align 8                                    ; load; *envptr9890
  %envptr9891 = inttoptr i64 %env9045 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9892 = getelementptr inbounds i64, i64* %envptr9891, i64 1                  ; &envptr9891[1]
  %GHO$cc = load i64, i64* %envptr9892, align 8                                      ; load; *envptr9892
  %cloptr9893 = inttoptr i64 %GHO$cc to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr9894 = getelementptr inbounds i64, i64* %cloptr9893, i64 0                   ; &cloptr9893[0]
  %f9896 = load i64, i64* %i0ptr9894, align 8                                        ; load; *i0ptr9894
  %fptr9895 = inttoptr i64 %f9896 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9895(i64 %GHO$cc, i64 %cont7619, i64 %GHO$cc)       ; tail call
  ret void
}


define void @lam9038(i64 %env9039, i64 %cont7627, i64 %Ygv$v, i64 %qSw$lst) {
  %arg8068 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %rHk$lst = call i64 @prim_make_45vector(i64 %arg8068, i64 %qSw$lst)                ; call prim_make_45vector
  %cloptr9897 = call i64* @alloc(i64 8)                                              ; malloc
  %eptr9898 = getelementptr inbounds i64, i64* %cloptr9897, i64 0                    ; &cloptr9897[0]
  %f9899 = ptrtoint void(i64,i64,i64)* @lam9035 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9899, i64* %eptr9898                                                   ; store fptr
  %arg8071 = ptrtoint i64* %cloptr9897 to i64                                        ; closure cast; i64* -> i64
  %cloptr9900 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9902 = getelementptr inbounds i64, i64* %cloptr9900, i64 1                    ; &eptr9902[1]
  %eptr9903 = getelementptr inbounds i64, i64* %cloptr9900, i64 2                    ; &eptr9903[2]
  %eptr9904 = getelementptr inbounds i64, i64* %cloptr9900, i64 3                    ; &eptr9904[3]
  store i64 %Ygv$v, i64* %eptr9902                                                   ; *eptr9902 = %Ygv$v
  store i64 %rHk$lst, i64* %eptr9903                                                 ; *eptr9903 = %rHk$lst
  store i64 %cont7627, i64* %eptr9904                                                ; *eptr9904 = %cont7627
  %eptr9901 = getelementptr inbounds i64, i64* %cloptr9900, i64 0                    ; &cloptr9900[0]
  %f9905 = ptrtoint void(i64,i64,i64)* @lam9033 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9905, i64* %eptr9901                                                   ; store fptr
  %arg8070 = ptrtoint i64* %cloptr9900 to i64                                        ; closure cast; i64* -> i64
  %cloptr9906 = call i64* @alloc(i64 32)                                             ; malloc
  %eptr9908 = getelementptr inbounds i64, i64* %cloptr9906, i64 1                    ; &eptr9908[1]
  %eptr9909 = getelementptr inbounds i64, i64* %cloptr9906, i64 2                    ; &eptr9909[2]
  %eptr9910 = getelementptr inbounds i64, i64* %cloptr9906, i64 3                    ; &eptr9910[3]
  store i64 %Ygv$v, i64* %eptr9908                                                   ; *eptr9908 = %Ygv$v
  store i64 %rHk$lst, i64* %eptr9909                                                 ; *eptr9909 = %rHk$lst
  store i64 %cont7627, i64* %eptr9910                                                ; *eptr9910 = %cont7627
  %eptr9907 = getelementptr inbounds i64, i64* %cloptr9906, i64 0                    ; &cloptr9906[0]
  %f9911 = ptrtoint void(i64,i64,i64)* @lam9020 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9911, i64* %eptr9907                                                   ; store fptr
  %arg8069 = ptrtoint i64* %cloptr9906 to i64                                        ; closure cast; i64* -> i64
  %cloptr9912 = inttoptr i64 %arg8071 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9913 = getelementptr inbounds i64, i64* %cloptr9912, i64 0                   ; &cloptr9912[0]
  %f9915 = load i64, i64* %i0ptr9913, align 8                                        ; load; *i0ptr9913
  %fptr9914 = inttoptr i64 %f9915 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9914(i64 %arg8071, i64 %arg8070, i64 %arg8069)      ; tail call
  ret void
}


define void @lam9035(i64 %env9036, i64 %cont7632, i64 %ubW$u) {
  %cloptr9916 = inttoptr i64 %ubW$u to i64*                                          ; closure/env cast; i64 -> i64*
  %i0ptr9917 = getelementptr inbounds i64, i64* %cloptr9916, i64 0                   ; &cloptr9916[0]
  %f9919 = load i64, i64* %i0ptr9917, align 8                                        ; load; *i0ptr9917
  %fptr9918 = inttoptr i64 %f9919 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9918(i64 %ubW$u, i64 %cont7632, i64 %ubW$u)         ; tail call
  ret void
}


define void @lam9033(i64 %env9034, i64 %_957628, i64 %FBl$cc) {
  %envptr9920 = inttoptr i64 %env9034 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9921 = getelementptr inbounds i64, i64* %envptr9920, i64 3                  ; &envptr9920[3]
  %cont7627 = load i64, i64* %envptr9921, align 8                                    ; load; *envptr9921
  %envptr9922 = inttoptr i64 %env9034 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9923 = getelementptr inbounds i64, i64* %envptr9922, i64 2                  ; &envptr9922[2]
  %rHk$lst = load i64, i64* %envptr9923, align 8                                     ; load; *envptr9923
  %envptr9924 = inttoptr i64 %env9034 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9925 = getelementptr inbounds i64, i64* %envptr9924, i64 1                  ; &envptr9924[1]
  %Ygv$v = load i64, i64* %envptr9925, align 8                                       ; load; *envptr9925
  %arg8075 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7525 = call i64 @prim_vector_45ref(i64 %rHk$lst, i64 %arg8075)                   ; call prim_vector_45ref
  %a7526 = call i64 @prim_null_63(i64 %a7525)                                        ; call prim_null_63
  %cmp9926 = icmp eq i64 %a7526, 15                                                  ; false?
  br i1 %cmp9926, label %else9928, label %then9927                                   ; if

then9927:
  %arg8079 = add i64 0, 0                                                            ; quoted ()
  %arg8078 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr9929 = inttoptr i64 %cont7627 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9930 = getelementptr inbounds i64, i64* %cloptr9929, i64 0                   ; &cloptr9929[0]
  %f9932 = load i64, i64* %i0ptr9930, align 8                                        ; load; *i0ptr9930
  %fptr9931 = inttoptr i64 %f9932 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9931(i64 %cont7627, i64 %arg8079, i64 %arg8078)     ; tail call
  ret void

else9928:
  %arg8081 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7527 = call i64 @prim_vector_45ref(i64 %rHk$lst, i64 %arg8081)                   ; call prim_vector_45ref
  %a7528 = call i64 @prim_car(i64 %a7527)                                            ; call prim_car
  %a7529 = call i64 @prim_eqv_63(i64 %a7528, i64 %Ygv$v)                             ; call prim_eqv_63
  %cmp9933 = icmp eq i64 %a7529, 15                                                  ; false?
  br i1 %cmp9933, label %else9935, label %then9934                                   ; if

then9934:
  %arg8086 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7629 = call i64 @prim_vector_45ref(i64 %rHk$lst, i64 %arg8086)             ; call prim_vector_45ref
  %arg8089 = add i64 0, 0                                                            ; quoted ()
  %cloptr9936 = inttoptr i64 %cont7627 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9937 = getelementptr inbounds i64, i64* %cloptr9936, i64 0                   ; &cloptr9936[0]
  %f9939 = load i64, i64* %i0ptr9937, align 8                                        ; load; *i0ptr9937
  %fptr9938 = inttoptr i64 %f9939 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9938(i64 %cont7627, i64 %arg8089, i64 %retprim7629) ; tail call
  ret void

else9935:
  %arg8091 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7530 = call i64 @prim_vector_45ref(i64 %rHk$lst, i64 %arg8091)                   ; call prim_vector_45ref
  %a7531 = call i64 @prim_cdr(i64 %a7530)                                            ; call prim_cdr
  %arg8095 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7631 = call i64 @prim_vector_45set_33(i64 %rHk$lst, i64 %arg8095, i64 %a7531); call prim_vector_45set_33
  %cloptr9940 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9942 = getelementptr inbounds i64, i64* %cloptr9940, i64 1                    ; &eptr9942[1]
  %eptr9943 = getelementptr inbounds i64, i64* %cloptr9940, i64 2                    ; &eptr9943[2]
  store i64 %FBl$cc, i64* %eptr9942                                                  ; *eptr9942 = %FBl$cc
  store i64 %cont7627, i64* %eptr9943                                                ; *eptr9943 = %cont7627
  %eptr9941 = getelementptr inbounds i64, i64* %cloptr9940, i64 0                    ; &cloptr9940[0]
  %f9944 = ptrtoint void(i64,i64,i64)* @lam9027 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9944, i64* %eptr9941                                                   ; store fptr
  %arg8099 = ptrtoint i64* %cloptr9940 to i64                                        ; closure cast; i64* -> i64
  %arg8098 = add i64 0, 0                                                            ; quoted ()
  %cloptr9945 = inttoptr i64 %arg8099 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9946 = getelementptr inbounds i64, i64* %cloptr9945, i64 0                   ; &cloptr9945[0]
  %f9948 = load i64, i64* %i0ptr9946, align 8                                        ; load; *i0ptr9946
  %fptr9947 = inttoptr i64 %f9948 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9947(i64 %arg8099, i64 %arg8098, i64 %retprim7631)  ; tail call
  ret void
}


define void @lam9027(i64 %env9028, i64 %_957630, i64 %sOB$_950) {
  %envptr9949 = inttoptr i64 %env9028 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9950 = getelementptr inbounds i64, i64* %envptr9949, i64 2                  ; &envptr9949[2]
  %cont7627 = load i64, i64* %envptr9950, align 8                                    ; load; *envptr9950
  %envptr9951 = inttoptr i64 %env9028 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9952 = getelementptr inbounds i64, i64* %envptr9951, i64 1                  ; &envptr9951[1]
  %FBl$cc = load i64, i64* %envptr9952, align 8                                      ; load; *envptr9952
  %cloptr9953 = inttoptr i64 %FBl$cc to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr9954 = getelementptr inbounds i64, i64* %cloptr9953, i64 0                   ; &cloptr9953[0]
  %f9956 = load i64, i64* %i0ptr9954, align 8                                        ; load; *i0ptr9954
  %fptr9955 = inttoptr i64 %f9956 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9955(i64 %FBl$cc, i64 %cont7627, i64 %FBl$cc)       ; tail call
  ret void
}


define void @lam9020(i64 %env9021, i64 %_957628, i64 %FBl$cc) {
  %envptr9957 = inttoptr i64 %env9021 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9958 = getelementptr inbounds i64, i64* %envptr9957, i64 3                  ; &envptr9957[3]
  %cont7627 = load i64, i64* %envptr9958, align 8                                    ; load; *envptr9958
  %envptr9959 = inttoptr i64 %env9021 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9960 = getelementptr inbounds i64, i64* %envptr9959, i64 2                  ; &envptr9959[2]
  %rHk$lst = load i64, i64* %envptr9960, align 8                                     ; load; *envptr9960
  %envptr9961 = inttoptr i64 %env9021 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9962 = getelementptr inbounds i64, i64* %envptr9961, i64 1                  ; &envptr9961[1]
  %Ygv$v = load i64, i64* %envptr9962, align 8                                       ; load; *envptr9962
  %arg8103 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7525 = call i64 @prim_vector_45ref(i64 %rHk$lst, i64 %arg8103)                   ; call prim_vector_45ref
  %a7526 = call i64 @prim_null_63(i64 %a7525)                                        ; call prim_null_63
  %cmp9963 = icmp eq i64 %a7526, 15                                                  ; false?
  br i1 %cmp9963, label %else9965, label %then9964                                   ; if

then9964:
  %arg8107 = add i64 0, 0                                                            ; quoted ()
  %arg8106 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr9966 = inttoptr i64 %cont7627 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9967 = getelementptr inbounds i64, i64* %cloptr9966, i64 0                   ; &cloptr9966[0]
  %f9969 = load i64, i64* %i0ptr9967, align 8                                        ; load; *i0ptr9967
  %fptr9968 = inttoptr i64 %f9969 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9968(i64 %cont7627, i64 %arg8107, i64 %arg8106)     ; tail call
  ret void

else9965:
  %arg8109 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7527 = call i64 @prim_vector_45ref(i64 %rHk$lst, i64 %arg8109)                   ; call prim_vector_45ref
  %a7528 = call i64 @prim_car(i64 %a7527)                                            ; call prim_car
  %a7529 = call i64 @prim_eqv_63(i64 %a7528, i64 %Ygv$v)                             ; call prim_eqv_63
  %cmp9970 = icmp eq i64 %a7529, 15                                                  ; false?
  br i1 %cmp9970, label %else9972, label %then9971                                   ; if

then9971:
  %arg8114 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7629 = call i64 @prim_vector_45ref(i64 %rHk$lst, i64 %arg8114)             ; call prim_vector_45ref
  %arg8117 = add i64 0, 0                                                            ; quoted ()
  %cloptr9973 = inttoptr i64 %cont7627 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr9974 = getelementptr inbounds i64, i64* %cloptr9973, i64 0                   ; &cloptr9973[0]
  %f9976 = load i64, i64* %i0ptr9974, align 8                                        ; load; *i0ptr9974
  %fptr9975 = inttoptr i64 %f9976 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9975(i64 %cont7627, i64 %arg8117, i64 %retprim7629) ; tail call
  ret void

else9972:
  %arg8119 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7530 = call i64 @prim_vector_45ref(i64 %rHk$lst, i64 %arg8119)                   ; call prim_vector_45ref
  %a7531 = call i64 @prim_cdr(i64 %a7530)                                            ; call prim_cdr
  %arg8123 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7631 = call i64 @prim_vector_45set_33(i64 %rHk$lst, i64 %arg8123, i64 %a7531); call prim_vector_45set_33
  %cloptr9977 = call i64* @alloc(i64 24)                                             ; malloc
  %eptr9979 = getelementptr inbounds i64, i64* %cloptr9977, i64 1                    ; &eptr9979[1]
  %eptr9980 = getelementptr inbounds i64, i64* %cloptr9977, i64 2                    ; &eptr9980[2]
  store i64 %FBl$cc, i64* %eptr9979                                                  ; *eptr9979 = %FBl$cc
  store i64 %cont7627, i64* %eptr9980                                                ; *eptr9980 = %cont7627
  %eptr9978 = getelementptr inbounds i64, i64* %cloptr9977, i64 0                    ; &cloptr9977[0]
  %f9981 = ptrtoint void(i64,i64,i64)* @lam9014 to i64                               ; fptr cast; i64(...)* -> i64
  store i64 %f9981, i64* %eptr9978                                                   ; store fptr
  %arg8127 = ptrtoint i64* %cloptr9977 to i64                                        ; closure cast; i64* -> i64
  %arg8126 = add i64 0, 0                                                            ; quoted ()
  %cloptr9982 = inttoptr i64 %arg8127 to i64*                                        ; closure/env cast; i64 -> i64*
  %i0ptr9983 = getelementptr inbounds i64, i64* %cloptr9982, i64 0                   ; &cloptr9982[0]
  %f9985 = load i64, i64* %i0ptr9983, align 8                                        ; load; *i0ptr9983
  %fptr9984 = inttoptr i64 %f9985 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9984(i64 %arg8127, i64 %arg8126, i64 %retprim7631)  ; tail call
  ret void
}


define void @lam9014(i64 %env9015, i64 %_957630, i64 %sOB$_950) {
  %envptr9986 = inttoptr i64 %env9015 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9987 = getelementptr inbounds i64, i64* %envptr9986, i64 2                  ; &envptr9986[2]
  %cont7627 = load i64, i64* %envptr9987, align 8                                    ; load; *envptr9987
  %envptr9988 = inttoptr i64 %env9015 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9989 = getelementptr inbounds i64, i64* %envptr9988, i64 1                  ; &envptr9988[1]
  %FBl$cc = load i64, i64* %envptr9989, align 8                                      ; load; *envptr9989
  %cloptr9990 = inttoptr i64 %FBl$cc to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr9991 = getelementptr inbounds i64, i64* %cloptr9990, i64 0                   ; &cloptr9990[0]
  %f9993 = load i64, i64* %i0ptr9991, align 8                                        ; load; *i0ptr9991
  %fptr9992 = inttoptr i64 %f9993 to void (i64,i64,i64)*                             ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr9992(i64 %FBl$cc, i64 %cont7627, i64 %FBl$cc)       ; tail call
  ret void
}


define void @lam9007(i64 %env9008, i64 %qtP$args7634) {
  %envptr9994 = inttoptr i64 %env9008 to i64*                                        ; closure/env cast; i64 -> i64*
  %envptr9995 = getelementptr inbounds i64, i64* %envptr9994, i64 1                  ; &envptr9994[1]
  %dbd$_37foldl1 = load i64, i64* %envptr9995, align 8                               ; load; *envptr9995
  %cont7633 = call i64 @prim_car(i64 %qtP$args7634)                                  ; call prim_car
  %qtP$args = call i64 @prim_cdr(i64 %qtP$args7634)                                  ; call prim_cdr
  %a7532 = call i64 @prim_null_63(i64 %qtP$args)                                     ; call prim_null_63
  %cmp9996 = icmp eq i64 %a7532, 15                                                  ; false?
  br i1 %cmp9996, label %else9998, label %then9997                                   ; if

then9997:
  %arg8135 = add i64 0, 0                                                            ; quoted ()
  %arg8134 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %cloptr9999 = inttoptr i64 %cont7633 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10000 = getelementptr inbounds i64, i64* %cloptr9999, i64 0                  ; &cloptr9999[0]
  %f10002 = load i64, i64* %i0ptr10000, align 8                                      ; load; *i0ptr10000
  %fptr10001 = inttoptr i64 %f10002 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10001(i64 %cont7633, i64 %arg8135, i64 %arg8134)    ; tail call
  ret void

else9998:
  %a7533 = call i64 @prim_cdr(i64 %qtP$args)                                         ; call prim_cdr
  %a7534 = call i64 @prim_null_63(i64 %a7533)                                        ; call prim_null_63
  %cmp10003 = icmp eq i64 %a7534, 15                                                 ; false?
  br i1 %cmp10003, label %else10005, label %then10004                                ; if

then10004:
  %retprim7635 = call i64 @prim_car(i64 %qtP$args)                                   ; call prim_car
  %arg8141 = add i64 0, 0                                                            ; quoted ()
  %cloptr10006 = inttoptr i64 %cont7633 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10007 = getelementptr inbounds i64, i64* %cloptr10006, i64 0                 ; &cloptr10006[0]
  %f10009 = load i64, i64* %i0ptr10007, align 8                                      ; load; *i0ptr10007
  %fptr10008 = inttoptr i64 %f10009 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10008(i64 %cont7633, i64 %arg8141, i64 %retprim7635); tail call
  ret void

else10005:
  %a7535 = call i64 @prim_car(i64 %qtP$args)                                         ; call prim_car
  %a7536 = call i64 @prim_cdr(i64 %qtP$args)                                         ; call prim_cdr
  %cloptr10010 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10011 = getelementptr inbounds i64, i64* %cloptr10010, i64 0                  ; &cloptr10010[0]
  %f10012 = ptrtoint void(i64,i64,i64,i64)* @lam9005 to i64                          ; fptr cast; i64(...)* -> i64
  store i64 %f10012, i64* %eptr10011                                                 ; store fptr
  %arg8147 = ptrtoint i64* %cloptr10010 to i64                                       ; closure cast; i64* -> i64
  %cloptr10013 = inttoptr i64 %dbd$_37foldl1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr10014 = getelementptr inbounds i64, i64* %cloptr10013, i64 0                 ; &cloptr10013[0]
  %f10016 = load i64, i64* %i0ptr10014, align 8                                      ; load; *i0ptr10014
  %fptr10015 = inttoptr i64 %f10016 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10015(i64 %dbd$_37foldl1, i64 %cont7633, i64 %arg8147, i64 %a7535, i64 %a7536); tail call
  ret void
}


define void @lam9005(i64 %env9006, i64 %cont7636, i64 %eJB$n, i64 %zRH$v) {
  %retprim7637 = call i64 @prim__47(i64 %zRH$v, i64 %eJB$n)                          ; call prim__47
  %arg8153 = add i64 0, 0                                                            ; quoted ()
  %cloptr10017 = inttoptr i64 %cont7636 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10018 = getelementptr inbounds i64, i64* %cloptr10017, i64 0                 ; &cloptr10017[0]
  %f10020 = load i64, i64* %i0ptr10018, align 8                                      ; load; *i0ptr10018
  %fptr10019 = inttoptr i64 %f10020 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10019(i64 %cont7636, i64 %arg8153, i64 %retprim7637); tail call
  ret void
}


define void @lam8999(i64 %env9000, i64 %cont7638, i64 %v43$x) {
  %retprim7639 = call i64 @prim_car(i64 %v43$x)                                      ; call prim_car
  %arg8157 = add i64 0, 0                                                            ; quoted ()
  %cloptr10021 = inttoptr i64 %cont7638 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10022 = getelementptr inbounds i64, i64* %cloptr10021, i64 0                 ; &cloptr10021[0]
  %f10024 = load i64, i64* %i0ptr10022, align 8                                      ; load; *i0ptr10022
  %fptr10023 = inttoptr i64 %f10024 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10023(i64 %cont7638, i64 %arg8157, i64 %retprim7639); tail call
  ret void
}


define void @lam8996(i64 %env8997, i64 %cont7640, i64 %vWg$x) {
  %a7537 = call i64 @prim_cdr(i64 %vWg$x)                                            ; call prim_cdr
  %retprim7641 = call i64 @prim_car(i64 %a7537)                                      ; call prim_car
  %arg8162 = add i64 0, 0                                                            ; quoted ()
  %cloptr10025 = inttoptr i64 %cont7640 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10026 = getelementptr inbounds i64, i64* %cloptr10025, i64 0                 ; &cloptr10025[0]
  %f10028 = load i64, i64* %i0ptr10026, align 8                                      ; load; *i0ptr10026
  %fptr10027 = inttoptr i64 %f10028 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10027(i64 %cont7640, i64 %arg8162, i64 %retprim7641); tail call
  ret void
}


define void @lam8993(i64 %env8994, i64 %cont7642, i64 %dLt$x) {
  %a7538 = call i64 @prim_cdr(i64 %dLt$x)                                            ; call prim_cdr
  %a7539 = call i64 @prim_cdr(i64 %a7538)                                            ; call prim_cdr
  %retprim7643 = call i64 @prim_car(i64 %a7539)                                      ; call prim_car
  %arg8168 = add i64 0, 0                                                            ; quoted ()
  %cloptr10029 = inttoptr i64 %cont7642 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10030 = getelementptr inbounds i64, i64* %cloptr10029, i64 0                 ; &cloptr10029[0]
  %f10032 = load i64, i64* %i0ptr10030, align 8                                      ; load; *i0ptr10030
  %fptr10031 = inttoptr i64 %f10032 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10031(i64 %cont7642, i64 %arg8168, i64 %retprim7643); tail call
  ret void
}


define void @lam8990(i64 %env8991, i64 %cont7644, i64 %tHx$x) {
  %a7540 = call i64 @prim_cdr(i64 %tHx$x)                                            ; call prim_cdr
  %a7541 = call i64 @prim_cdr(i64 %a7540)                                            ; call prim_cdr
  %a7542 = call i64 @prim_cdr(i64 %a7541)                                            ; call prim_cdr
  %retprim7645 = call i64 @prim_car(i64 %a7542)                                      ; call prim_car
  %arg8175 = add i64 0, 0                                                            ; quoted ()
  %cloptr10033 = inttoptr i64 %cont7644 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10034 = getelementptr inbounds i64, i64* %cloptr10033, i64 0                 ; &cloptr10033[0]
  %f10036 = load i64, i64* %i0ptr10034, align 8                                      ; load; *i0ptr10034
  %fptr10035 = inttoptr i64 %f10036 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10035(i64 %cont7644, i64 %arg8175, i64 %retprim7645); tail call
  ret void
}


define void @lam8987(i64 %env8988, i64 %cont7646, i64 %N1X$p) {
  %a7543 = call i64 @prim_cons_63(i64 %N1X$p)                                        ; call prim_cons_63
  %cmp10037 = icmp eq i64 %a7543, 15                                                 ; false?
  br i1 %cmp10037, label %else10039, label %then10038                                ; if

then10038:
  %a7544 = call i64 @prim_car(i64 %N1X$p)                                            ; call prim_car
  %arg8179 = call i64 @const_init_symbol(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @sym10040, i32 0, i32 0)); quoted string
  %retprim7647 = call i64 @prim_eq_63(i64 %a7544, i64 %arg8179)                      ; call prim_eq_63
  %arg8182 = add i64 0, 0                                                            ; quoted ()
  %cloptr10041 = inttoptr i64 %cont7646 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10042 = getelementptr inbounds i64, i64* %cloptr10041, i64 0                 ; &cloptr10041[0]
  %f10044 = load i64, i64* %i0ptr10042, align 8                                      ; load; *i0ptr10042
  %fptr10043 = inttoptr i64 %f10044 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10043(i64 %cont7646, i64 %arg8182, i64 %retprim7647); tail call
  ret void

else10039:
  %arg8185 = add i64 0, 0                                                            ; quoted ()
  %arg8184 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr10045 = inttoptr i64 %cont7646 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10046 = getelementptr inbounds i64, i64* %cloptr10045, i64 0                 ; &cloptr10045[0]
  %f10048 = load i64, i64* %i0ptr10046, align 8                                      ; load; *i0ptr10046
  %fptr10047 = inttoptr i64 %f10048 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10047(i64 %cont7646, i64 %arg8185, i64 %arg8184)    ; tail call
  ret void
}


define void @lam8981(i64 %env8982, i64 %Wl5$lst7702) {
  %cont7701 = call i64 @prim_car(i64 %Wl5$lst7702)                                   ; call prim_car
  %Wl5$lst = call i64 @prim_cdr(i64 %Wl5$lst7702)                                    ; call prim_cdr
  %arg8192 = add i64 0, 0                                                            ; quoted ()
  %cloptr10049 = inttoptr i64 %cont7701 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10050 = getelementptr inbounds i64, i64* %cloptr10049, i64 0                 ; &cloptr10049[0]
  %f10052 = load i64, i64* %i0ptr10050, align 8                                      ; load; *i0ptr10050
  %fptr10051 = inttoptr i64 %f10052 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10051(i64 %cont7701, i64 %arg8192, i64 %Wl5$lst)    ; tail call
  ret void
}


define void @lam8978(i64 %env8979, i64 %_957648, i64 %I89$_37raise_45handler) {
  %envptr10053 = inttoptr i64 %env8979 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10054 = getelementptr inbounds i64, i64* %envptr10053, i64 3                ; &envptr10053[3]
  %R74$_37length = load i64, i64* %envptr10054, align 8                              ; load; *envptr10054
  %envptr10055 = inttoptr i64 %env8979 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10056 = getelementptr inbounds i64, i64* %envptr10055, i64 2                ; &envptr10055[2]
  %ajc$_37drop = load i64, i64* %envptr10056, align 8                                ; load; *envptr10056
  %envptr10057 = inttoptr i64 %env8979 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10058 = getelementptr inbounds i64, i64* %envptr10057, i64 1                ; &envptr10057[1]
  %LdG$_37_62 = load i64, i64* %envptr10058, align 8                                 ; load; *envptr10058
  %cloptr10059 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10060 = getelementptr inbounds i64, i64* %cloptr10059, i64 0                  ; &cloptr10059[0]
  %f10061 = ptrtoint void(i64,i64)* @lam8976 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f10061, i64* %eptr10060                                                 ; store fptr
  %arg8195 = ptrtoint i64* %cloptr10059 to i64                                       ; closure cast; i64* -> i64
  %cloptr10062 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10064 = getelementptr inbounds i64, i64* %cloptr10062, i64 1                  ; &eptr10064[1]
  %eptr10065 = getelementptr inbounds i64, i64* %cloptr10062, i64 2                  ; &eptr10065[2]
  %eptr10066 = getelementptr inbounds i64, i64* %cloptr10062, i64 3                  ; &eptr10066[3]
  store i64 %LdG$_37_62, i64* %eptr10064                                             ; *eptr10064 = %LdG$_37_62
  store i64 %ajc$_37drop, i64* %eptr10065                                            ; *eptr10065 = %ajc$_37drop
  store i64 %R74$_37length, i64* %eptr10066                                          ; *eptr10066 = %R74$_37length
  %eptr10063 = getelementptr inbounds i64, i64* %cloptr10062, i64 0                  ; &cloptr10062[0]
  %f10067 = ptrtoint void(i64,i64,i64)* @lam8973 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10067, i64* %eptr10063                                                 ; store fptr
  %arg8194 = ptrtoint i64* %cloptr10062 to i64                                       ; closure cast; i64* -> i64
  %rva8701 = add i64 0, 0                                                            ; quoted ()
  %rva8700 = call i64 @prim_cons(i64 %arg8194, i64 %rva8701)                         ; call prim_cons
  %cloptr10068 = inttoptr i64 %arg8195 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10069 = getelementptr inbounds i64, i64* %cloptr10068, i64 0                 ; &cloptr10068[0]
  %f10071 = load i64, i64* %i0ptr10069, align 8                                      ; load; *i0ptr10069
  %fptr10070 = inttoptr i64 %f10071 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10070(i64 %arg8195, i64 %rva8700)                   ; tail call
  ret void
}


define void @lam8976(i64 %env8977, i64 %fzw$lst7700) {
  %cont7699 = call i64 @prim_car(i64 %fzw$lst7700)                                   ; call prim_car
  %fzw$lst = call i64 @prim_cdr(i64 %fzw$lst7700)                                    ; call prim_cdr
  %arg8199 = add i64 0, 0                                                            ; quoted ()
  %cloptr10072 = inttoptr i64 %cont7699 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10073 = getelementptr inbounds i64, i64* %cloptr10072, i64 0                 ; &cloptr10072[0]
  %f10075 = load i64, i64* %i0ptr10073, align 8                                      ; load; *i0ptr10073
  %fptr10074 = inttoptr i64 %f10075 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10074(i64 %cont7699, i64 %arg8199, i64 %fzw$lst)    ; tail call
  ret void
}


define void @lam8973(i64 %env8974, i64 %_957697, i64 %a7545) {
  %envptr10076 = inttoptr i64 %env8974 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10077 = getelementptr inbounds i64, i64* %envptr10076, i64 3                ; &envptr10076[3]
  %R74$_37length = load i64, i64* %envptr10077, align 8                              ; load; *envptr10077
  %envptr10078 = inttoptr i64 %env8974 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10079 = getelementptr inbounds i64, i64* %envptr10078, i64 2                ; &envptr10078[2]
  %ajc$_37drop = load i64, i64* %envptr10079, align 8                                ; load; *envptr10079
  %envptr10080 = inttoptr i64 %env8974 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10081 = getelementptr inbounds i64, i64* %envptr10080, i64 1                ; &envptr10080[1]
  %LdG$_37_62 = load i64, i64* %envptr10081, align 8                                 ; load; *envptr10081
  %arg8202 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %retprim7698 = call i64 @prim_make_45vector(i64 %arg8202, i64 %a7545)              ; call prim_make_45vector
  %cloptr10082 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10084 = getelementptr inbounds i64, i64* %cloptr10082, i64 1                  ; &eptr10084[1]
  %eptr10085 = getelementptr inbounds i64, i64* %cloptr10082, i64 2                  ; &eptr10085[2]
  %eptr10086 = getelementptr inbounds i64, i64* %cloptr10082, i64 3                  ; &eptr10086[3]
  store i64 %LdG$_37_62, i64* %eptr10084                                             ; *eptr10084 = %LdG$_37_62
  store i64 %ajc$_37drop, i64* %eptr10085                                            ; *eptr10085 = %ajc$_37drop
  store i64 %R74$_37length, i64* %eptr10086                                          ; *eptr10086 = %R74$_37length
  %eptr10083 = getelementptr inbounds i64, i64* %cloptr10082, i64 0                  ; &cloptr10082[0]
  %f10087 = ptrtoint void(i64,i64,i64)* @lam8970 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10087, i64* %eptr10083                                                 ; store fptr
  %arg8205 = ptrtoint i64* %cloptr10082 to i64                                       ; closure cast; i64* -> i64
  %arg8204 = add i64 0, 0                                                            ; quoted ()
  %cloptr10088 = inttoptr i64 %arg8205 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10089 = getelementptr inbounds i64, i64* %cloptr10088, i64 0                 ; &cloptr10088[0]
  %f10091 = load i64, i64* %i0ptr10089, align 8                                      ; load; *i0ptr10089
  %fptr10090 = inttoptr i64 %f10091 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10090(i64 %arg8205, i64 %arg8204, i64 %retprim7698) ; tail call
  ret void
}


define void @lam8970(i64 %env8971, i64 %_957649, i64 %v9o$_37wind_45stack) {
  %envptr10092 = inttoptr i64 %env8971 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10093 = getelementptr inbounds i64, i64* %envptr10092, i64 3                ; &envptr10092[3]
  %R74$_37length = load i64, i64* %envptr10093, align 8                              ; load; *envptr10093
  %envptr10094 = inttoptr i64 %env8971 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10095 = getelementptr inbounds i64, i64* %envptr10094, i64 2                ; &envptr10094[2]
  %ajc$_37drop = load i64, i64* %envptr10095, align 8                                ; load; *envptr10095
  %envptr10096 = inttoptr i64 %env8971 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10097 = getelementptr inbounds i64, i64* %envptr10096, i64 1                ; &envptr10096[1]
  %LdG$_37_62 = load i64, i64* %envptr10097, align 8                                 ; load; *envptr10097
  %cloptr10098 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10100 = getelementptr inbounds i64, i64* %cloptr10098, i64 1                  ; &eptr10100[1]
  %eptr10101 = getelementptr inbounds i64, i64* %cloptr10098, i64 2                  ; &eptr10101[2]
  %eptr10102 = getelementptr inbounds i64, i64* %cloptr10098, i64 3                  ; &eptr10102[3]
  store i64 %LdG$_37_62, i64* %eptr10100                                             ; *eptr10100 = %LdG$_37_62
  store i64 %ajc$_37drop, i64* %eptr10101                                            ; *eptr10101 = %ajc$_37drop
  store i64 %R74$_37length, i64* %eptr10102                                          ; *eptr10102 = %R74$_37length
  %eptr10099 = getelementptr inbounds i64, i64* %cloptr10098, i64 0                  ; &cloptr10098[0]
  %f10103 = ptrtoint void(i64,i64,i64,i64)* @lam8968 to i64                          ; fptr cast; i64(...)* -> i64
  store i64 %f10103, i64* %eptr10099                                                 ; store fptr
  %nTN$_37common_45tail = ptrtoint i64* %cloptr10098 to i64                          ; closure cast; i64* -> i64
  %cloptr10104 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr10106 = getelementptr inbounds i64, i64* %cloptr10104, i64 1                  ; &eptr10106[1]
  %eptr10107 = getelementptr inbounds i64, i64* %cloptr10104, i64 2                  ; &eptr10107[2]
  store i64 %nTN$_37common_45tail, i64* %eptr10106                                   ; *eptr10106 = %nTN$_37common_45tail
  store i64 %v9o$_37wind_45stack, i64* %eptr10107                                    ; *eptr10107 = %v9o$_37wind_45stack
  %eptr10105 = getelementptr inbounds i64, i64* %cloptr10104, i64 0                  ; &cloptr10104[0]
  %f10108 = ptrtoint void(i64,i64,i64)* @lam8926 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10108, i64* %eptr10105                                                 ; store fptr
  %EtN$_37do_45wind = ptrtoint i64* %cloptr10104 to i64                              ; closure cast; i64* -> i64
  %cloptr10109 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10110 = getelementptr inbounds i64, i64* %cloptr10109, i64 0                  ; &cloptr10109[0]
  %f10111 = ptrtoint void(i64,i64)* @lam8876 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f10111, i64* %eptr10110                                                 ; store fptr
  %arg8391 = ptrtoint i64* %cloptr10109 to i64                                       ; closure cast; i64* -> i64
  %cloptr10112 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10113 = getelementptr inbounds i64, i64* %cloptr10112, i64 0                  ; &cloptr10112[0]
  %f10114 = ptrtoint void(i64,i64)* @lam8872 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f10114, i64* %eptr10113                                                 ; store fptr
  %arg8390 = ptrtoint i64* %cloptr10112 to i64                                       ; closure cast; i64* -> i64
  %rva8699 = add i64 0, 0                                                            ; quoted ()
  %rva8698 = call i64 @prim_cons(i64 %arg8390, i64 %rva8699)                         ; call prim_cons
  %cloptr10115 = inttoptr i64 %arg8391 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10116 = getelementptr inbounds i64, i64* %cloptr10115, i64 0                 ; &cloptr10115[0]
  %f10118 = load i64, i64* %i0ptr10116, align 8                                      ; load; *i0ptr10116
  %fptr10117 = inttoptr i64 %f10118 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10117(i64 %arg8391, i64 %rva8698)                   ; tail call
  ret void
}


define void @lam8968(i64 %env8969, i64 %cont7650, i64 %POV$x, i64 %FaV$y) {
  %envptr10119 = inttoptr i64 %env8969 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10120 = getelementptr inbounds i64, i64* %envptr10119, i64 3                ; &envptr10119[3]
  %R74$_37length = load i64, i64* %envptr10120, align 8                              ; load; *envptr10120
  %envptr10121 = inttoptr i64 %env8969 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10122 = getelementptr inbounds i64, i64* %envptr10121, i64 2                ; &envptr10121[2]
  %ajc$_37drop = load i64, i64* %envptr10122, align 8                                ; load; *envptr10122
  %envptr10123 = inttoptr i64 %env8969 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10124 = getelementptr inbounds i64, i64* %envptr10123, i64 1                ; &envptr10123[1]
  %LdG$_37_62 = load i64, i64* %envptr10124, align 8                                 ; load; *envptr10124
  %cloptr10125 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr10127 = getelementptr inbounds i64, i64* %cloptr10125, i64 1                  ; &eptr10127[1]
  %eptr10128 = getelementptr inbounds i64, i64* %cloptr10125, i64 2                  ; &eptr10128[2]
  %eptr10129 = getelementptr inbounds i64, i64* %cloptr10125, i64 3                  ; &eptr10129[3]
  %eptr10130 = getelementptr inbounds i64, i64* %cloptr10125, i64 4                  ; &eptr10130[4]
  %eptr10131 = getelementptr inbounds i64, i64* %cloptr10125, i64 5                  ; &eptr10131[5]
  %eptr10132 = getelementptr inbounds i64, i64* %cloptr10125, i64 6                  ; &eptr10132[6]
  store i64 %LdG$_37_62, i64* %eptr10127                                             ; *eptr10127 = %LdG$_37_62
  store i64 %FaV$y, i64* %eptr10128                                                  ; *eptr10128 = %FaV$y
  store i64 %ajc$_37drop, i64* %eptr10129                                            ; *eptr10129 = %ajc$_37drop
  store i64 %R74$_37length, i64* %eptr10130                                          ; *eptr10130 = %R74$_37length
  store i64 %POV$x, i64* %eptr10131                                                  ; *eptr10131 = %POV$x
  store i64 %cont7650, i64* %eptr10132                                               ; *eptr10132 = %cont7650
  %eptr10126 = getelementptr inbounds i64, i64* %cloptr10125, i64 0                  ; &cloptr10125[0]
  %f10133 = ptrtoint void(i64,i64,i64)* @lam8966 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10133, i64* %eptr10126                                                 ; store fptr
  %arg8207 = ptrtoint i64* %cloptr10125 to i64                                       ; closure cast; i64* -> i64
  %cloptr10134 = inttoptr i64 %R74$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr10135 = getelementptr inbounds i64, i64* %cloptr10134, i64 0                 ; &cloptr10134[0]
  %f10137 = load i64, i64* %i0ptr10135, align 8                                      ; load; *i0ptr10135
  %fptr10136 = inttoptr i64 %f10137 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10136(i64 %R74$_37length, i64 %arg8207, i64 %POV$x) ; tail call
  ret void
}


define void @lam8966(i64 %env8967, i64 %_957651, i64 %ZSw$lx) {
  %envptr10138 = inttoptr i64 %env8967 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10139 = getelementptr inbounds i64, i64* %envptr10138, i64 6                ; &envptr10138[6]
  %cont7650 = load i64, i64* %envptr10139, align 8                                   ; load; *envptr10139
  %envptr10140 = inttoptr i64 %env8967 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10141 = getelementptr inbounds i64, i64* %envptr10140, i64 5                ; &envptr10140[5]
  %POV$x = load i64, i64* %envptr10141, align 8                                      ; load; *envptr10141
  %envptr10142 = inttoptr i64 %env8967 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10143 = getelementptr inbounds i64, i64* %envptr10142, i64 4                ; &envptr10142[4]
  %R74$_37length = load i64, i64* %envptr10143, align 8                              ; load; *envptr10143
  %envptr10144 = inttoptr i64 %env8967 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10145 = getelementptr inbounds i64, i64* %envptr10144, i64 3                ; &envptr10144[3]
  %ajc$_37drop = load i64, i64* %envptr10145, align 8                                ; load; *envptr10145
  %envptr10146 = inttoptr i64 %env8967 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10147 = getelementptr inbounds i64, i64* %envptr10146, i64 2                ; &envptr10146[2]
  %FaV$y = load i64, i64* %envptr10147, align 8                                      ; load; *envptr10147
  %envptr10148 = inttoptr i64 %env8967 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10149 = getelementptr inbounds i64, i64* %envptr10148, i64 1                ; &envptr10148[1]
  %LdG$_37_62 = load i64, i64* %envptr10149, align 8                                 ; load; *envptr10149
  %cloptr10150 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr10152 = getelementptr inbounds i64, i64* %cloptr10150, i64 1                  ; &eptr10152[1]
  %eptr10153 = getelementptr inbounds i64, i64* %cloptr10150, i64 2                  ; &eptr10153[2]
  %eptr10154 = getelementptr inbounds i64, i64* %cloptr10150, i64 3                  ; &eptr10154[3]
  %eptr10155 = getelementptr inbounds i64, i64* %cloptr10150, i64 4                  ; &eptr10155[4]
  %eptr10156 = getelementptr inbounds i64, i64* %cloptr10150, i64 5                  ; &eptr10156[5]
  %eptr10157 = getelementptr inbounds i64, i64* %cloptr10150, i64 6                  ; &eptr10157[6]
  store i64 %LdG$_37_62, i64* %eptr10152                                             ; *eptr10152 = %LdG$_37_62
  store i64 %FaV$y, i64* %eptr10153                                                  ; *eptr10153 = %FaV$y
  store i64 %ZSw$lx, i64* %eptr10154                                                 ; *eptr10154 = %ZSw$lx
  store i64 %ajc$_37drop, i64* %eptr10155                                            ; *eptr10155 = %ajc$_37drop
  store i64 %POV$x, i64* %eptr10156                                                  ; *eptr10156 = %POV$x
  store i64 %cont7650, i64* %eptr10157                                               ; *eptr10157 = %cont7650
  %eptr10151 = getelementptr inbounds i64, i64* %cloptr10150, i64 0                  ; &cloptr10150[0]
  %f10158 = ptrtoint void(i64,i64,i64)* @lam8964 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10158, i64* %eptr10151                                                 ; store fptr
  %arg8210 = ptrtoint i64* %cloptr10150 to i64                                       ; closure cast; i64* -> i64
  %cloptr10159 = inttoptr i64 %R74$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr10160 = getelementptr inbounds i64, i64* %cloptr10159, i64 0                 ; &cloptr10159[0]
  %f10162 = load i64, i64* %i0ptr10160, align 8                                      ; load; *i0ptr10160
  %fptr10161 = inttoptr i64 %f10162 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10161(i64 %R74$_37length, i64 %arg8210, i64 %FaV$y) ; tail call
  ret void
}


define void @lam8964(i64 %env8965, i64 %_957652, i64 %Sqj$ly) {
  %envptr10163 = inttoptr i64 %env8965 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10164 = getelementptr inbounds i64, i64* %envptr10163, i64 6                ; &envptr10163[6]
  %cont7650 = load i64, i64* %envptr10164, align 8                                   ; load; *envptr10164
  %envptr10165 = inttoptr i64 %env8965 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10166 = getelementptr inbounds i64, i64* %envptr10165, i64 5                ; &envptr10165[5]
  %POV$x = load i64, i64* %envptr10166, align 8                                      ; load; *envptr10166
  %envptr10167 = inttoptr i64 %env8965 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10168 = getelementptr inbounds i64, i64* %envptr10167, i64 4                ; &envptr10167[4]
  %ajc$_37drop = load i64, i64* %envptr10168, align 8                                ; load; *envptr10168
  %envptr10169 = inttoptr i64 %env8965 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10170 = getelementptr inbounds i64, i64* %envptr10169, i64 3                ; &envptr10169[3]
  %ZSw$lx = load i64, i64* %envptr10170, align 8                                     ; load; *envptr10170
  %envptr10171 = inttoptr i64 %env8965 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10172 = getelementptr inbounds i64, i64* %envptr10171, i64 2                ; &envptr10171[2]
  %FaV$y = load i64, i64* %envptr10172, align 8                                      ; load; *envptr10172
  %envptr10173 = inttoptr i64 %env8965 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10174 = getelementptr inbounds i64, i64* %envptr10173, i64 1                ; &envptr10173[1]
  %LdG$_37_62 = load i64, i64* %envptr10174, align 8                                 ; load; *envptr10174
  %cloptr10175 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10176 = getelementptr inbounds i64, i64* %cloptr10175, i64 0                  ; &cloptr10175[0]
  %f10177 = ptrtoint void(i64,i64)* @lam8962 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f10177, i64* %eptr10176                                                 ; store fptr
  %arg8213 = ptrtoint i64* %cloptr10175 to i64                                       ; closure cast; i64* -> i64
  %cloptr10178 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr10180 = getelementptr inbounds i64, i64* %cloptr10178, i64 1                  ; &eptr10180[1]
  %eptr10181 = getelementptr inbounds i64, i64* %cloptr10178, i64 2                  ; &eptr10181[2]
  %eptr10182 = getelementptr inbounds i64, i64* %cloptr10178, i64 3                  ; &eptr10182[3]
  %eptr10183 = getelementptr inbounds i64, i64* %cloptr10178, i64 4                  ; &eptr10183[4]
  %eptr10184 = getelementptr inbounds i64, i64* %cloptr10178, i64 5                  ; &eptr10184[5]
  %eptr10185 = getelementptr inbounds i64, i64* %cloptr10178, i64 6                  ; &eptr10185[6]
  %eptr10186 = getelementptr inbounds i64, i64* %cloptr10178, i64 7                  ; &eptr10186[7]
  store i64 %LdG$_37_62, i64* %eptr10180                                             ; *eptr10180 = %LdG$_37_62
  store i64 %Sqj$ly, i64* %eptr10181                                                 ; *eptr10181 = %Sqj$ly
  store i64 %FaV$y, i64* %eptr10182                                                  ; *eptr10182 = %FaV$y
  store i64 %ZSw$lx, i64* %eptr10183                                                 ; *eptr10183 = %ZSw$lx
  store i64 %ajc$_37drop, i64* %eptr10184                                            ; *eptr10184 = %ajc$_37drop
  store i64 %POV$x, i64* %eptr10185                                                  ; *eptr10185 = %POV$x
  store i64 %cont7650, i64* %eptr10186                                               ; *eptr10186 = %cont7650
  %eptr10179 = getelementptr inbounds i64, i64* %cloptr10178, i64 0                  ; &cloptr10178[0]
  %f10187 = ptrtoint void(i64,i64,i64)* @lam8959 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10187, i64* %eptr10179                                                 ; store fptr
  %arg8212 = ptrtoint i64* %cloptr10178 to i64                                       ; closure cast; i64* -> i64
  %cloptr10188 = inttoptr i64 %arg8213 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10189 = getelementptr inbounds i64, i64* %cloptr10188, i64 0                 ; &cloptr10188[0]
  %f10191 = load i64, i64* %i0ptr10189, align 8                                      ; load; *i0ptr10189
  %fptr10190 = inttoptr i64 %f10191 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10190(i64 %arg8213, i64 %arg8212)                   ; tail call
  ret void
}


define void @lam8962(i64 %env8963, i64 %mf8$lst7662) {
  %cont7661 = call i64 @prim_car(i64 %mf8$lst7662)                                   ; call prim_car
  %mf8$lst = call i64 @prim_cdr(i64 %mf8$lst7662)                                    ; call prim_cdr
  %arg8217 = add i64 0, 0                                                            ; quoted ()
  %cloptr10192 = inttoptr i64 %cont7661 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10193 = getelementptr inbounds i64, i64* %cloptr10192, i64 0                 ; &cloptr10192[0]
  %f10195 = load i64, i64* %i0ptr10193, align 8                                      ; load; *i0ptr10193
  %fptr10194 = inttoptr i64 %f10195 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10194(i64 %cont7661, i64 %arg8217, i64 %mf8$lst)    ; tail call
  ret void
}


define void @lam8959(i64 %env8960, i64 %_957659, i64 %a7546) {
  %envptr10196 = inttoptr i64 %env8960 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10197 = getelementptr inbounds i64, i64* %envptr10196, i64 7                ; &envptr10196[7]
  %cont7650 = load i64, i64* %envptr10197, align 8                                   ; load; *envptr10197
  %envptr10198 = inttoptr i64 %env8960 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10199 = getelementptr inbounds i64, i64* %envptr10198, i64 6                ; &envptr10198[6]
  %POV$x = load i64, i64* %envptr10199, align 8                                      ; load; *envptr10199
  %envptr10200 = inttoptr i64 %env8960 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10201 = getelementptr inbounds i64, i64* %envptr10200, i64 5                ; &envptr10200[5]
  %ajc$_37drop = load i64, i64* %envptr10201, align 8                                ; load; *envptr10201
  %envptr10202 = inttoptr i64 %env8960 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10203 = getelementptr inbounds i64, i64* %envptr10202, i64 4                ; &envptr10202[4]
  %ZSw$lx = load i64, i64* %envptr10203, align 8                                     ; load; *envptr10203
  %envptr10204 = inttoptr i64 %env8960 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10205 = getelementptr inbounds i64, i64* %envptr10204, i64 3                ; &envptr10204[3]
  %FaV$y = load i64, i64* %envptr10205, align 8                                      ; load; *envptr10205
  %envptr10206 = inttoptr i64 %env8960 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10207 = getelementptr inbounds i64, i64* %envptr10206, i64 2                ; &envptr10206[2]
  %Sqj$ly = load i64, i64* %envptr10207, align 8                                     ; load; *envptr10207
  %envptr10208 = inttoptr i64 %env8960 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10209 = getelementptr inbounds i64, i64* %envptr10208, i64 1                ; &envptr10208[1]
  %LdG$_37_62 = load i64, i64* %envptr10209, align 8                                 ; load; *envptr10209
  %arg8220 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %retprim7660 = call i64 @prim_make_45vector(i64 %arg8220, i64 %a7546)              ; call prim_make_45vector
  %cloptr10210 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr10212 = getelementptr inbounds i64, i64* %cloptr10210, i64 1                  ; &eptr10212[1]
  %eptr10213 = getelementptr inbounds i64, i64* %cloptr10210, i64 2                  ; &eptr10213[2]
  %eptr10214 = getelementptr inbounds i64, i64* %cloptr10210, i64 3                  ; &eptr10214[3]
  %eptr10215 = getelementptr inbounds i64, i64* %cloptr10210, i64 4                  ; &eptr10215[4]
  %eptr10216 = getelementptr inbounds i64, i64* %cloptr10210, i64 5                  ; &eptr10216[5]
  %eptr10217 = getelementptr inbounds i64, i64* %cloptr10210, i64 6                  ; &eptr10217[6]
  %eptr10218 = getelementptr inbounds i64, i64* %cloptr10210, i64 7                  ; &eptr10218[7]
  store i64 %LdG$_37_62, i64* %eptr10212                                             ; *eptr10212 = %LdG$_37_62
  store i64 %Sqj$ly, i64* %eptr10213                                                 ; *eptr10213 = %Sqj$ly
  store i64 %FaV$y, i64* %eptr10214                                                  ; *eptr10214 = %FaV$y
  store i64 %ZSw$lx, i64* %eptr10215                                                 ; *eptr10215 = %ZSw$lx
  store i64 %ajc$_37drop, i64* %eptr10216                                            ; *eptr10216 = %ajc$_37drop
  store i64 %POV$x, i64* %eptr10217                                                  ; *eptr10217 = %POV$x
  store i64 %cont7650, i64* %eptr10218                                               ; *eptr10218 = %cont7650
  %eptr10211 = getelementptr inbounds i64, i64* %cloptr10210, i64 0                  ; &cloptr10210[0]
  %f10219 = ptrtoint void(i64,i64,i64)* @lam8956 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10219, i64* %eptr10211                                                 ; store fptr
  %arg8223 = ptrtoint i64* %cloptr10210 to i64                                       ; closure cast; i64* -> i64
  %arg8222 = add i64 0, 0                                                            ; quoted ()
  %cloptr10220 = inttoptr i64 %arg8223 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10221 = getelementptr inbounds i64, i64* %cloptr10220, i64 0                 ; &cloptr10220[0]
  %f10223 = load i64, i64* %i0ptr10221, align 8                                      ; load; *i0ptr10221
  %fptr10222 = inttoptr i64 %f10223 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10222(i64 %arg8223, i64 %arg8222, i64 %retprim7660) ; tail call
  ret void
}


define void @lam8956(i64 %env8957, i64 %_957653, i64 %b1U$loop) {
  %envptr10224 = inttoptr i64 %env8957 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10225 = getelementptr inbounds i64, i64* %envptr10224, i64 7                ; &envptr10224[7]
  %cont7650 = load i64, i64* %envptr10225, align 8                                   ; load; *envptr10225
  %envptr10226 = inttoptr i64 %env8957 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10227 = getelementptr inbounds i64, i64* %envptr10226, i64 6                ; &envptr10226[6]
  %POV$x = load i64, i64* %envptr10227, align 8                                      ; load; *envptr10227
  %envptr10228 = inttoptr i64 %env8957 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10229 = getelementptr inbounds i64, i64* %envptr10228, i64 5                ; &envptr10228[5]
  %ajc$_37drop = load i64, i64* %envptr10229, align 8                                ; load; *envptr10229
  %envptr10230 = inttoptr i64 %env8957 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10231 = getelementptr inbounds i64, i64* %envptr10230, i64 4                ; &envptr10230[4]
  %ZSw$lx = load i64, i64* %envptr10231, align 8                                     ; load; *envptr10231
  %envptr10232 = inttoptr i64 %env8957 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10233 = getelementptr inbounds i64, i64* %envptr10232, i64 3                ; &envptr10232[3]
  %FaV$y = load i64, i64* %envptr10233, align 8                                      ; load; *envptr10233
  %envptr10234 = inttoptr i64 %env8957 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10235 = getelementptr inbounds i64, i64* %envptr10234, i64 2                ; &envptr10234[2]
  %Sqj$ly = load i64, i64* %envptr10235, align 8                                     ; load; *envptr10235
  %envptr10236 = inttoptr i64 %env8957 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10237 = getelementptr inbounds i64, i64* %envptr10236, i64 1                ; &envptr10236[1]
  %LdG$_37_62 = load i64, i64* %envptr10237, align 8                                 ; load; *envptr10237
  %arg8225 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr10238 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr10240 = getelementptr inbounds i64, i64* %cloptr10238, i64 1                  ; &eptr10240[1]
  store i64 %b1U$loop, i64* %eptr10240                                               ; *eptr10240 = %b1U$loop
  %eptr10239 = getelementptr inbounds i64, i64* %cloptr10238, i64 0                  ; &cloptr10238[0]
  %f10241 = ptrtoint void(i64,i64,i64,i64)* @lam8953 to i64                          ; fptr cast; i64(...)* -> i64
  store i64 %f10241, i64* %eptr10239                                                 ; store fptr
  %arg8224 = ptrtoint i64* %cloptr10238 to i64                                       ; closure cast; i64* -> i64
  %h2H$_957445 = call i64 @prim_vector_45set_33(i64 %b1U$loop, i64 %arg8225, i64 %arg8224); call prim_vector_45set_33
  %arg8240 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7551 = call i64 @prim_vector_45ref(i64 %b1U$loop, i64 %arg8240)                  ; call prim_vector_45ref
  %cloptr10242 = call i64* @alloc(i64 72)                                            ; malloc
  %eptr10244 = getelementptr inbounds i64, i64* %cloptr10242, i64 1                  ; &eptr10244[1]
  %eptr10245 = getelementptr inbounds i64, i64* %cloptr10242, i64 2                  ; &eptr10245[2]
  %eptr10246 = getelementptr inbounds i64, i64* %cloptr10242, i64 3                  ; &eptr10246[3]
  %eptr10247 = getelementptr inbounds i64, i64* %cloptr10242, i64 4                  ; &eptr10247[4]
  %eptr10248 = getelementptr inbounds i64, i64* %cloptr10242, i64 5                  ; &eptr10248[5]
  %eptr10249 = getelementptr inbounds i64, i64* %cloptr10242, i64 6                  ; &eptr10249[6]
  %eptr10250 = getelementptr inbounds i64, i64* %cloptr10242, i64 7                  ; &eptr10250[7]
  %eptr10251 = getelementptr inbounds i64, i64* %cloptr10242, i64 8                  ; &eptr10251[8]
  store i64 %LdG$_37_62, i64* %eptr10244                                             ; *eptr10244 = %LdG$_37_62
  store i64 %Sqj$ly, i64* %eptr10245                                                 ; *eptr10245 = %Sqj$ly
  store i64 %FaV$y, i64* %eptr10246                                                  ; *eptr10246 = %FaV$y
  store i64 %ZSw$lx, i64* %eptr10247                                                 ; *eptr10247 = %ZSw$lx
  store i64 %ajc$_37drop, i64* %eptr10248                                            ; *eptr10248 = %ajc$_37drop
  store i64 %POV$x, i64* %eptr10249                                                  ; *eptr10249 = %POV$x
  store i64 %a7551, i64* %eptr10250                                                  ; *eptr10250 = %a7551
  store i64 %cont7650, i64* %eptr10251                                               ; *eptr10251 = %cont7650
  %eptr10243 = getelementptr inbounds i64, i64* %cloptr10242, i64 0                  ; &cloptr10242[0]
  %f10252 = ptrtoint void(i64,i64,i64)* @lam8948 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10252, i64* %eptr10243                                                 ; store fptr
  %arg8244 = ptrtoint i64* %cloptr10242 to i64                                       ; closure cast; i64* -> i64
  %cloptr10253 = inttoptr i64 %LdG$_37_62 to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr10254 = getelementptr inbounds i64, i64* %cloptr10253, i64 0                 ; &cloptr10253[0]
  %f10256 = load i64, i64* %i0ptr10254, align 8                                      ; load; *i0ptr10254
  %fptr10255 = inttoptr i64 %f10256 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10255(i64 %LdG$_37_62, i64 %arg8244, i64 %ZSw$lx, i64 %Sqj$ly); tail call
  ret void
}


define void @lam8953(i64 %env8954, i64 %cont7654, i64 %DOC$x, i64 %pWx$y) {
  %envptr10257 = inttoptr i64 %env8954 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10258 = getelementptr inbounds i64, i64* %envptr10257, i64 1                ; &envptr10257[1]
  %b1U$loop = load i64, i64* %envptr10258, align 8                                   ; load; *envptr10258
  %a7547 = call i64 @prim_eq_63(i64 %DOC$x, i64 %pWx$y)                              ; call prim_eq_63
  %cmp10259 = icmp eq i64 %a7547, 15                                                 ; false?
  br i1 %cmp10259, label %else10261, label %then10260                                ; if

then10260:
  %arg8230 = add i64 0, 0                                                            ; quoted ()
  %cloptr10262 = inttoptr i64 %cont7654 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10263 = getelementptr inbounds i64, i64* %cloptr10262, i64 0                 ; &cloptr10262[0]
  %f10265 = load i64, i64* %i0ptr10263, align 8                                      ; load; *i0ptr10263
  %fptr10264 = inttoptr i64 %f10265 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10264(i64 %cont7654, i64 %arg8230, i64 %DOC$x)      ; tail call
  ret void

else10261:
  %arg8232 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7548 = call i64 @prim_vector_45ref(i64 %b1U$loop, i64 %arg8232)                  ; call prim_vector_45ref
  %a7549 = call i64 @prim_cdr(i64 %DOC$x)                                            ; call prim_cdr
  %a7550 = call i64 @prim_cdr(i64 %pWx$y)                                            ; call prim_cdr
  %cloptr10266 = inttoptr i64 %a7548 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10267 = getelementptr inbounds i64, i64* %cloptr10266, i64 0                 ; &cloptr10266[0]
  %f10269 = load i64, i64* %i0ptr10267, align 8                                      ; load; *i0ptr10267
  %fptr10268 = inttoptr i64 %f10269 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10268(i64 %a7548, i64 %cont7654, i64 %a7549, i64 %a7550); tail call
  ret void
}


define void @lam8948(i64 %env8949, i64 %_957655, i64 %a7552) {
  %envptr10270 = inttoptr i64 %env8949 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10271 = getelementptr inbounds i64, i64* %envptr10270, i64 8                ; &envptr10270[8]
  %cont7650 = load i64, i64* %envptr10271, align 8                                   ; load; *envptr10271
  %envptr10272 = inttoptr i64 %env8949 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10273 = getelementptr inbounds i64, i64* %envptr10272, i64 7                ; &envptr10272[7]
  %a7551 = load i64, i64* %envptr10273, align 8                                      ; load; *envptr10273
  %envptr10274 = inttoptr i64 %env8949 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10275 = getelementptr inbounds i64, i64* %envptr10274, i64 6                ; &envptr10274[6]
  %POV$x = load i64, i64* %envptr10275, align 8                                      ; load; *envptr10275
  %envptr10276 = inttoptr i64 %env8949 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10277 = getelementptr inbounds i64, i64* %envptr10276, i64 5                ; &envptr10276[5]
  %ajc$_37drop = load i64, i64* %envptr10277, align 8                                ; load; *envptr10277
  %envptr10278 = inttoptr i64 %env8949 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10279 = getelementptr inbounds i64, i64* %envptr10278, i64 4                ; &envptr10278[4]
  %ZSw$lx = load i64, i64* %envptr10279, align 8                                     ; load; *envptr10279
  %envptr10280 = inttoptr i64 %env8949 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10281 = getelementptr inbounds i64, i64* %envptr10280, i64 3                ; &envptr10280[3]
  %FaV$y = load i64, i64* %envptr10281, align 8                                      ; load; *envptr10281
  %envptr10282 = inttoptr i64 %env8949 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10283 = getelementptr inbounds i64, i64* %envptr10282, i64 2                ; &envptr10282[2]
  %Sqj$ly = load i64, i64* %envptr10283, align 8                                     ; load; *envptr10283
  %envptr10284 = inttoptr i64 %env8949 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10285 = getelementptr inbounds i64, i64* %envptr10284, i64 1                ; &envptr10284[1]
  %LdG$_37_62 = load i64, i64* %envptr10285, align 8                                 ; load; *envptr10285
  %cmp10286 = icmp eq i64 %a7552, 15                                                 ; false?
  br i1 %cmp10286, label %else10288, label %then10287                                ; if

then10287:
  %a7553 = call i64 @prim__45(i64 %ZSw$lx, i64 %Sqj$ly)                              ; call prim__45
  %cloptr10289 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr10291 = getelementptr inbounds i64, i64* %cloptr10289, i64 1                  ; &eptr10291[1]
  %eptr10292 = getelementptr inbounds i64, i64* %cloptr10289, i64 2                  ; &eptr10292[2]
  %eptr10293 = getelementptr inbounds i64, i64* %cloptr10289, i64 3                  ; &eptr10293[3]
  %eptr10294 = getelementptr inbounds i64, i64* %cloptr10289, i64 4                  ; &eptr10294[4]
  %eptr10295 = getelementptr inbounds i64, i64* %cloptr10289, i64 5                  ; &eptr10295[5]
  %eptr10296 = getelementptr inbounds i64, i64* %cloptr10289, i64 6                  ; &eptr10296[6]
  %eptr10297 = getelementptr inbounds i64, i64* %cloptr10289, i64 7                  ; &eptr10297[7]
  store i64 %LdG$_37_62, i64* %eptr10291                                             ; *eptr10291 = %LdG$_37_62
  store i64 %Sqj$ly, i64* %eptr10292                                                 ; *eptr10292 = %Sqj$ly
  store i64 %FaV$y, i64* %eptr10293                                                  ; *eptr10293 = %FaV$y
  store i64 %ZSw$lx, i64* %eptr10294                                                 ; *eptr10294 = %ZSw$lx
  store i64 %ajc$_37drop, i64* %eptr10295                                            ; *eptr10295 = %ajc$_37drop
  store i64 %a7551, i64* %eptr10296                                                  ; *eptr10296 = %a7551
  store i64 %cont7650, i64* %eptr10297                                               ; *eptr10297 = %cont7650
  %eptr10290 = getelementptr inbounds i64, i64* %cloptr10289, i64 0                  ; &cloptr10289[0]
  %f10298 = ptrtoint void(i64,i64,i64)* @lam8936 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10298, i64* %eptr10290                                                 ; store fptr
  %arg8250 = ptrtoint i64* %cloptr10289 to i64                                       ; closure cast; i64* -> i64
  %cloptr10299 = inttoptr i64 %ajc$_37drop to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr10300 = getelementptr inbounds i64, i64* %cloptr10299, i64 0                 ; &cloptr10299[0]
  %f10302 = load i64, i64* %i0ptr10300, align 8                                      ; load; *i0ptr10300
  %fptr10301 = inttoptr i64 %f10302 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10301(i64 %ajc$_37drop, i64 %arg8250, i64 %POV$x, i64 %a7553); tail call
  ret void

else10288:
  %cloptr10303 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr10305 = getelementptr inbounds i64, i64* %cloptr10303, i64 1                  ; &eptr10305[1]
  %eptr10306 = getelementptr inbounds i64, i64* %cloptr10303, i64 2                  ; &eptr10306[2]
  %eptr10307 = getelementptr inbounds i64, i64* %cloptr10303, i64 3                  ; &eptr10307[3]
  %eptr10308 = getelementptr inbounds i64, i64* %cloptr10303, i64 4                  ; &eptr10308[4]
  %eptr10309 = getelementptr inbounds i64, i64* %cloptr10303, i64 5                  ; &eptr10309[5]
  %eptr10310 = getelementptr inbounds i64, i64* %cloptr10303, i64 6                  ; &eptr10310[6]
  %eptr10311 = getelementptr inbounds i64, i64* %cloptr10303, i64 7                  ; &eptr10311[7]
  store i64 %LdG$_37_62, i64* %eptr10305                                             ; *eptr10305 = %LdG$_37_62
  store i64 %Sqj$ly, i64* %eptr10306                                                 ; *eptr10306 = %Sqj$ly
  store i64 %FaV$y, i64* %eptr10307                                                  ; *eptr10307 = %FaV$y
  store i64 %ZSw$lx, i64* %eptr10308                                                 ; *eptr10308 = %ZSw$lx
  store i64 %ajc$_37drop, i64* %eptr10309                                            ; *eptr10309 = %ajc$_37drop
  store i64 %a7551, i64* %eptr10310                                                  ; *eptr10310 = %a7551
  store i64 %cont7650, i64* %eptr10311                                               ; *eptr10311 = %cont7650
  %eptr10304 = getelementptr inbounds i64, i64* %cloptr10303, i64 0                  ; &cloptr10303[0]
  %f10312 = ptrtoint void(i64,i64,i64)* @lam8946 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10312, i64* %eptr10304                                                 ; store fptr
  %arg8275 = ptrtoint i64* %cloptr10303 to i64                                       ; closure cast; i64* -> i64
  %arg8274 = add i64 0, 0                                                            ; quoted ()
  %cloptr10313 = inttoptr i64 %arg8275 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10314 = getelementptr inbounds i64, i64* %cloptr10313, i64 0                 ; &cloptr10313[0]
  %f10316 = load i64, i64* %i0ptr10314, align 8                                      ; load; *i0ptr10314
  %fptr10315 = inttoptr i64 %f10316 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10315(i64 %arg8275, i64 %arg8274, i64 %POV$x)       ; tail call
  ret void
}


define void @lam8946(i64 %env8947, i64 %_957656, i64 %a7554) {
  %envptr10317 = inttoptr i64 %env8947 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10318 = getelementptr inbounds i64, i64* %envptr10317, i64 7                ; &envptr10317[7]
  %cont7650 = load i64, i64* %envptr10318, align 8                                   ; load; *envptr10318
  %envptr10319 = inttoptr i64 %env8947 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10320 = getelementptr inbounds i64, i64* %envptr10319, i64 6                ; &envptr10319[6]
  %a7551 = load i64, i64* %envptr10320, align 8                                      ; load; *envptr10320
  %envptr10321 = inttoptr i64 %env8947 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10322 = getelementptr inbounds i64, i64* %envptr10321, i64 5                ; &envptr10321[5]
  %ajc$_37drop = load i64, i64* %envptr10322, align 8                                ; load; *envptr10322
  %envptr10323 = inttoptr i64 %env8947 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10324 = getelementptr inbounds i64, i64* %envptr10323, i64 4                ; &envptr10323[4]
  %ZSw$lx = load i64, i64* %envptr10324, align 8                                     ; load; *envptr10324
  %envptr10325 = inttoptr i64 %env8947 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10326 = getelementptr inbounds i64, i64* %envptr10325, i64 3                ; &envptr10325[3]
  %FaV$y = load i64, i64* %envptr10326, align 8                                      ; load; *envptr10326
  %envptr10327 = inttoptr i64 %env8947 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10328 = getelementptr inbounds i64, i64* %envptr10327, i64 2                ; &envptr10327[2]
  %Sqj$ly = load i64, i64* %envptr10328, align 8                                     ; load; *envptr10328
  %envptr10329 = inttoptr i64 %env8947 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10330 = getelementptr inbounds i64, i64* %envptr10329, i64 1                ; &envptr10329[1]
  %LdG$_37_62 = load i64, i64* %envptr10330, align 8                                 ; load; *envptr10330
  %cloptr10331 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr10333 = getelementptr inbounds i64, i64* %cloptr10331, i64 1                  ; &eptr10333[1]
  %eptr10334 = getelementptr inbounds i64, i64* %cloptr10331, i64 2                  ; &eptr10334[2]
  %eptr10335 = getelementptr inbounds i64, i64* %cloptr10331, i64 3                  ; &eptr10335[3]
  %eptr10336 = getelementptr inbounds i64, i64* %cloptr10331, i64 4                  ; &eptr10336[4]
  %eptr10337 = getelementptr inbounds i64, i64* %cloptr10331, i64 5                  ; &eptr10337[5]
  %eptr10338 = getelementptr inbounds i64, i64* %cloptr10331, i64 6                  ; &eptr10338[6]
  %eptr10339 = getelementptr inbounds i64, i64* %cloptr10331, i64 7                  ; &eptr10339[7]
  store i64 %Sqj$ly, i64* %eptr10333                                                 ; *eptr10333 = %Sqj$ly
  store i64 %FaV$y, i64* %eptr10334                                                  ; *eptr10334 = %FaV$y
  store i64 %ZSw$lx, i64* %eptr10335                                                 ; *eptr10335 = %ZSw$lx
  store i64 %ajc$_37drop, i64* %eptr10336                                            ; *eptr10336 = %ajc$_37drop
  store i64 %a7551, i64* %eptr10337                                                  ; *eptr10337 = %a7551
  store i64 %a7554, i64* %eptr10338                                                  ; *eptr10338 = %a7554
  store i64 %cont7650, i64* %eptr10339                                               ; *eptr10339 = %cont7650
  %eptr10332 = getelementptr inbounds i64, i64* %cloptr10331, i64 0                  ; &cloptr10331[0]
  %f10340 = ptrtoint void(i64,i64,i64)* @lam8944 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10340, i64* %eptr10332                                                 ; store fptr
  %arg8278 = ptrtoint i64* %cloptr10331 to i64                                       ; closure cast; i64* -> i64
  %cloptr10341 = inttoptr i64 %LdG$_37_62 to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr10342 = getelementptr inbounds i64, i64* %cloptr10341, i64 0                 ; &cloptr10341[0]
  %f10344 = load i64, i64* %i0ptr10342, align 8                                      ; load; *i0ptr10342
  %fptr10343 = inttoptr i64 %f10344 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10343(i64 %LdG$_37_62, i64 %arg8278, i64 %Sqj$ly, i64 %ZSw$lx); tail call
  ret void
}


define void @lam8944(i64 %env8945, i64 %_957657, i64 %a7555) {
  %envptr10345 = inttoptr i64 %env8945 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10346 = getelementptr inbounds i64, i64* %envptr10345, i64 7                ; &envptr10345[7]
  %cont7650 = load i64, i64* %envptr10346, align 8                                   ; load; *envptr10346
  %envptr10347 = inttoptr i64 %env8945 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10348 = getelementptr inbounds i64, i64* %envptr10347, i64 6                ; &envptr10347[6]
  %a7554 = load i64, i64* %envptr10348, align 8                                      ; load; *envptr10348
  %envptr10349 = inttoptr i64 %env8945 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10350 = getelementptr inbounds i64, i64* %envptr10349, i64 5                ; &envptr10349[5]
  %a7551 = load i64, i64* %envptr10350, align 8                                      ; load; *envptr10350
  %envptr10351 = inttoptr i64 %env8945 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10352 = getelementptr inbounds i64, i64* %envptr10351, i64 4                ; &envptr10351[4]
  %ajc$_37drop = load i64, i64* %envptr10352, align 8                                ; load; *envptr10352
  %envptr10353 = inttoptr i64 %env8945 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10354 = getelementptr inbounds i64, i64* %envptr10353, i64 3                ; &envptr10353[3]
  %ZSw$lx = load i64, i64* %envptr10354, align 8                                     ; load; *envptr10354
  %envptr10355 = inttoptr i64 %env8945 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10356 = getelementptr inbounds i64, i64* %envptr10355, i64 2                ; &envptr10355[2]
  %FaV$y = load i64, i64* %envptr10356, align 8                                      ; load; *envptr10356
  %envptr10357 = inttoptr i64 %env8945 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10358 = getelementptr inbounds i64, i64* %envptr10357, i64 1                ; &envptr10357[1]
  %Sqj$ly = load i64, i64* %envptr10358, align 8                                     ; load; *envptr10358
  %cmp10359 = icmp eq i64 %a7555, 15                                                 ; false?
  br i1 %cmp10359, label %else10361, label %then10360                                ; if

then10360:
  %a7556 = call i64 @prim__45(i64 %Sqj$ly, i64 %ZSw$lx)                              ; call prim__45
  %cloptr10362 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10364 = getelementptr inbounds i64, i64* %cloptr10362, i64 1                  ; &eptr10364[1]
  %eptr10365 = getelementptr inbounds i64, i64* %cloptr10362, i64 2                  ; &eptr10365[2]
  %eptr10366 = getelementptr inbounds i64, i64* %cloptr10362, i64 3                  ; &eptr10366[3]
  store i64 %a7551, i64* %eptr10364                                                  ; *eptr10364 = %a7551
  store i64 %a7554, i64* %eptr10365                                                  ; *eptr10365 = %a7554
  store i64 %cont7650, i64* %eptr10366                                               ; *eptr10366 = %cont7650
  %eptr10363 = getelementptr inbounds i64, i64* %cloptr10362, i64 0                  ; &cloptr10362[0]
  %f10367 = ptrtoint void(i64,i64,i64)* @lam8939 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10367, i64* %eptr10363                                                 ; store fptr
  %arg8284 = ptrtoint i64* %cloptr10362 to i64                                       ; closure cast; i64* -> i64
  %cloptr10368 = inttoptr i64 %ajc$_37drop to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr10369 = getelementptr inbounds i64, i64* %cloptr10368, i64 0                 ; &cloptr10368[0]
  %f10371 = load i64, i64* %i0ptr10369, align 8                                      ; load; *i0ptr10369
  %fptr10370 = inttoptr i64 %f10371 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10370(i64 %ajc$_37drop, i64 %arg8284, i64 %FaV$y, i64 %a7556); tail call
  ret void

else10361:
  %cloptr10372 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10374 = getelementptr inbounds i64, i64* %cloptr10372, i64 1                  ; &eptr10374[1]
  %eptr10375 = getelementptr inbounds i64, i64* %cloptr10372, i64 2                  ; &eptr10375[2]
  %eptr10376 = getelementptr inbounds i64, i64* %cloptr10372, i64 3                  ; &eptr10376[3]
  store i64 %a7551, i64* %eptr10374                                                  ; *eptr10374 = %a7551
  store i64 %a7554, i64* %eptr10375                                                  ; *eptr10375 = %a7554
  store i64 %cont7650, i64* %eptr10376                                               ; *eptr10376 = %cont7650
  %eptr10373 = getelementptr inbounds i64, i64* %cloptr10372, i64 0                  ; &cloptr10372[0]
  %f10377 = ptrtoint void(i64,i64,i64)* @lam8942 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10377, i64* %eptr10373                                                 ; store fptr
  %arg8292 = ptrtoint i64* %cloptr10372 to i64                                       ; closure cast; i64* -> i64
  %arg8291 = add i64 0, 0                                                            ; quoted ()
  %cloptr10378 = inttoptr i64 %arg8292 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10379 = getelementptr inbounds i64, i64* %cloptr10378, i64 0                 ; &cloptr10378[0]
  %f10381 = load i64, i64* %i0ptr10379, align 8                                      ; load; *i0ptr10379
  %fptr10380 = inttoptr i64 %f10381 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10380(i64 %arg8292, i64 %arg8291, i64 %FaV$y)       ; tail call
  ret void
}


define void @lam8942(i64 %env8943, i64 %_957658, i64 %a7557) {
  %envptr10382 = inttoptr i64 %env8943 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10383 = getelementptr inbounds i64, i64* %envptr10382, i64 3                ; &envptr10382[3]
  %cont7650 = load i64, i64* %envptr10383, align 8                                   ; load; *envptr10383
  %envptr10384 = inttoptr i64 %env8943 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10385 = getelementptr inbounds i64, i64* %envptr10384, i64 2                ; &envptr10384[2]
  %a7554 = load i64, i64* %envptr10385, align 8                                      ; load; *envptr10385
  %envptr10386 = inttoptr i64 %env8943 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10387 = getelementptr inbounds i64, i64* %envptr10386, i64 1                ; &envptr10386[1]
  %a7551 = load i64, i64* %envptr10387, align 8                                      ; load; *envptr10387
  %cloptr10388 = inttoptr i64 %a7551 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10389 = getelementptr inbounds i64, i64* %cloptr10388, i64 0                 ; &cloptr10388[0]
  %f10391 = load i64, i64* %i0ptr10389, align 8                                      ; load; *i0ptr10389
  %fptr10390 = inttoptr i64 %f10391 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10390(i64 %a7551, i64 %cont7650, i64 %a7554, i64 %a7557); tail call
  ret void
}


define void @lam8939(i64 %env8940, i64 %_957658, i64 %a7557) {
  %envptr10392 = inttoptr i64 %env8940 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10393 = getelementptr inbounds i64, i64* %envptr10392, i64 3                ; &envptr10392[3]
  %cont7650 = load i64, i64* %envptr10393, align 8                                   ; load; *envptr10393
  %envptr10394 = inttoptr i64 %env8940 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10395 = getelementptr inbounds i64, i64* %envptr10394, i64 2                ; &envptr10394[2]
  %a7554 = load i64, i64* %envptr10395, align 8                                      ; load; *envptr10395
  %envptr10396 = inttoptr i64 %env8940 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10397 = getelementptr inbounds i64, i64* %envptr10396, i64 1                ; &envptr10396[1]
  %a7551 = load i64, i64* %envptr10397, align 8                                      ; load; *envptr10397
  %cloptr10398 = inttoptr i64 %a7551 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10399 = getelementptr inbounds i64, i64* %cloptr10398, i64 0                 ; &cloptr10398[0]
  %f10401 = load i64, i64* %i0ptr10399, align 8                                      ; load; *i0ptr10399
  %fptr10400 = inttoptr i64 %f10401 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10400(i64 %a7551, i64 %cont7650, i64 %a7554, i64 %a7557); tail call
  ret void
}


define void @lam8936(i64 %env8937, i64 %_957656, i64 %a7554) {
  %envptr10402 = inttoptr i64 %env8937 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10403 = getelementptr inbounds i64, i64* %envptr10402, i64 7                ; &envptr10402[7]
  %cont7650 = load i64, i64* %envptr10403, align 8                                   ; load; *envptr10403
  %envptr10404 = inttoptr i64 %env8937 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10405 = getelementptr inbounds i64, i64* %envptr10404, i64 6                ; &envptr10404[6]
  %a7551 = load i64, i64* %envptr10405, align 8                                      ; load; *envptr10405
  %envptr10406 = inttoptr i64 %env8937 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10407 = getelementptr inbounds i64, i64* %envptr10406, i64 5                ; &envptr10406[5]
  %ajc$_37drop = load i64, i64* %envptr10407, align 8                                ; load; *envptr10407
  %envptr10408 = inttoptr i64 %env8937 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10409 = getelementptr inbounds i64, i64* %envptr10408, i64 4                ; &envptr10408[4]
  %ZSw$lx = load i64, i64* %envptr10409, align 8                                     ; load; *envptr10409
  %envptr10410 = inttoptr i64 %env8937 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10411 = getelementptr inbounds i64, i64* %envptr10410, i64 3                ; &envptr10410[3]
  %FaV$y = load i64, i64* %envptr10411, align 8                                      ; load; *envptr10411
  %envptr10412 = inttoptr i64 %env8937 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10413 = getelementptr inbounds i64, i64* %envptr10412, i64 2                ; &envptr10412[2]
  %Sqj$ly = load i64, i64* %envptr10413, align 8                                     ; load; *envptr10413
  %envptr10414 = inttoptr i64 %env8937 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10415 = getelementptr inbounds i64, i64* %envptr10414, i64 1                ; &envptr10414[1]
  %LdG$_37_62 = load i64, i64* %envptr10415, align 8                                 ; load; *envptr10415
  %cloptr10416 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr10418 = getelementptr inbounds i64, i64* %cloptr10416, i64 1                  ; &eptr10418[1]
  %eptr10419 = getelementptr inbounds i64, i64* %cloptr10416, i64 2                  ; &eptr10419[2]
  %eptr10420 = getelementptr inbounds i64, i64* %cloptr10416, i64 3                  ; &eptr10420[3]
  %eptr10421 = getelementptr inbounds i64, i64* %cloptr10416, i64 4                  ; &eptr10421[4]
  %eptr10422 = getelementptr inbounds i64, i64* %cloptr10416, i64 5                  ; &eptr10422[5]
  %eptr10423 = getelementptr inbounds i64, i64* %cloptr10416, i64 6                  ; &eptr10423[6]
  %eptr10424 = getelementptr inbounds i64, i64* %cloptr10416, i64 7                  ; &eptr10424[7]
  store i64 %Sqj$ly, i64* %eptr10418                                                 ; *eptr10418 = %Sqj$ly
  store i64 %FaV$y, i64* %eptr10419                                                  ; *eptr10419 = %FaV$y
  store i64 %ZSw$lx, i64* %eptr10420                                                 ; *eptr10420 = %ZSw$lx
  store i64 %ajc$_37drop, i64* %eptr10421                                            ; *eptr10421 = %ajc$_37drop
  store i64 %a7551, i64* %eptr10422                                                  ; *eptr10422 = %a7551
  store i64 %a7554, i64* %eptr10423                                                  ; *eptr10423 = %a7554
  store i64 %cont7650, i64* %eptr10424                                               ; *eptr10424 = %cont7650
  %eptr10417 = getelementptr inbounds i64, i64* %cloptr10416, i64 0                  ; &cloptr10416[0]
  %f10425 = ptrtoint void(i64,i64,i64)* @lam8934 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10425, i64* %eptr10417                                                 ; store fptr
  %arg8254 = ptrtoint i64* %cloptr10416 to i64                                       ; closure cast; i64* -> i64
  %cloptr10426 = inttoptr i64 %LdG$_37_62 to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr10427 = getelementptr inbounds i64, i64* %cloptr10426, i64 0                 ; &cloptr10426[0]
  %f10429 = load i64, i64* %i0ptr10427, align 8                                      ; load; *i0ptr10427
  %fptr10428 = inttoptr i64 %f10429 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10428(i64 %LdG$_37_62, i64 %arg8254, i64 %Sqj$ly, i64 %ZSw$lx); tail call
  ret void
}


define void @lam8934(i64 %env8935, i64 %_957657, i64 %a7555) {
  %envptr10430 = inttoptr i64 %env8935 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10431 = getelementptr inbounds i64, i64* %envptr10430, i64 7                ; &envptr10430[7]
  %cont7650 = load i64, i64* %envptr10431, align 8                                   ; load; *envptr10431
  %envptr10432 = inttoptr i64 %env8935 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10433 = getelementptr inbounds i64, i64* %envptr10432, i64 6                ; &envptr10432[6]
  %a7554 = load i64, i64* %envptr10433, align 8                                      ; load; *envptr10433
  %envptr10434 = inttoptr i64 %env8935 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10435 = getelementptr inbounds i64, i64* %envptr10434, i64 5                ; &envptr10434[5]
  %a7551 = load i64, i64* %envptr10435, align 8                                      ; load; *envptr10435
  %envptr10436 = inttoptr i64 %env8935 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10437 = getelementptr inbounds i64, i64* %envptr10436, i64 4                ; &envptr10436[4]
  %ajc$_37drop = load i64, i64* %envptr10437, align 8                                ; load; *envptr10437
  %envptr10438 = inttoptr i64 %env8935 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10439 = getelementptr inbounds i64, i64* %envptr10438, i64 3                ; &envptr10438[3]
  %ZSw$lx = load i64, i64* %envptr10439, align 8                                     ; load; *envptr10439
  %envptr10440 = inttoptr i64 %env8935 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10441 = getelementptr inbounds i64, i64* %envptr10440, i64 2                ; &envptr10440[2]
  %FaV$y = load i64, i64* %envptr10441, align 8                                      ; load; *envptr10441
  %envptr10442 = inttoptr i64 %env8935 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10443 = getelementptr inbounds i64, i64* %envptr10442, i64 1                ; &envptr10442[1]
  %Sqj$ly = load i64, i64* %envptr10443, align 8                                     ; load; *envptr10443
  %cmp10444 = icmp eq i64 %a7555, 15                                                 ; false?
  br i1 %cmp10444, label %else10446, label %then10445                                ; if

then10445:
  %a7556 = call i64 @prim__45(i64 %Sqj$ly, i64 %ZSw$lx)                              ; call prim__45
  %cloptr10447 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10449 = getelementptr inbounds i64, i64* %cloptr10447, i64 1                  ; &eptr10449[1]
  %eptr10450 = getelementptr inbounds i64, i64* %cloptr10447, i64 2                  ; &eptr10450[2]
  %eptr10451 = getelementptr inbounds i64, i64* %cloptr10447, i64 3                  ; &eptr10451[3]
  store i64 %a7551, i64* %eptr10449                                                  ; *eptr10449 = %a7551
  store i64 %a7554, i64* %eptr10450                                                  ; *eptr10450 = %a7554
  store i64 %cont7650, i64* %eptr10451                                               ; *eptr10451 = %cont7650
  %eptr10448 = getelementptr inbounds i64, i64* %cloptr10447, i64 0                  ; &cloptr10447[0]
  %f10452 = ptrtoint void(i64,i64,i64)* @lam8929 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10452, i64* %eptr10448                                                 ; store fptr
  %arg8260 = ptrtoint i64* %cloptr10447 to i64                                       ; closure cast; i64* -> i64
  %cloptr10453 = inttoptr i64 %ajc$_37drop to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr10454 = getelementptr inbounds i64, i64* %cloptr10453, i64 0                 ; &cloptr10453[0]
  %f10456 = load i64, i64* %i0ptr10454, align 8                                      ; load; *i0ptr10454
  %fptr10455 = inttoptr i64 %f10456 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10455(i64 %ajc$_37drop, i64 %arg8260, i64 %FaV$y, i64 %a7556); tail call
  ret void

else10446:
  %cloptr10457 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10459 = getelementptr inbounds i64, i64* %cloptr10457, i64 1                  ; &eptr10459[1]
  %eptr10460 = getelementptr inbounds i64, i64* %cloptr10457, i64 2                  ; &eptr10460[2]
  %eptr10461 = getelementptr inbounds i64, i64* %cloptr10457, i64 3                  ; &eptr10461[3]
  store i64 %a7551, i64* %eptr10459                                                  ; *eptr10459 = %a7551
  store i64 %a7554, i64* %eptr10460                                                  ; *eptr10460 = %a7554
  store i64 %cont7650, i64* %eptr10461                                               ; *eptr10461 = %cont7650
  %eptr10458 = getelementptr inbounds i64, i64* %cloptr10457, i64 0                  ; &cloptr10457[0]
  %f10462 = ptrtoint void(i64,i64,i64)* @lam8932 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10462, i64* %eptr10458                                                 ; store fptr
  %arg8268 = ptrtoint i64* %cloptr10457 to i64                                       ; closure cast; i64* -> i64
  %arg8267 = add i64 0, 0                                                            ; quoted ()
  %cloptr10463 = inttoptr i64 %arg8268 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10464 = getelementptr inbounds i64, i64* %cloptr10463, i64 0                 ; &cloptr10463[0]
  %f10466 = load i64, i64* %i0ptr10464, align 8                                      ; load; *i0ptr10464
  %fptr10465 = inttoptr i64 %f10466 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10465(i64 %arg8268, i64 %arg8267, i64 %FaV$y)       ; tail call
  ret void
}


define void @lam8932(i64 %env8933, i64 %_957658, i64 %a7557) {
  %envptr10467 = inttoptr i64 %env8933 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10468 = getelementptr inbounds i64, i64* %envptr10467, i64 3                ; &envptr10467[3]
  %cont7650 = load i64, i64* %envptr10468, align 8                                   ; load; *envptr10468
  %envptr10469 = inttoptr i64 %env8933 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10470 = getelementptr inbounds i64, i64* %envptr10469, i64 2                ; &envptr10469[2]
  %a7554 = load i64, i64* %envptr10470, align 8                                      ; load; *envptr10470
  %envptr10471 = inttoptr i64 %env8933 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10472 = getelementptr inbounds i64, i64* %envptr10471, i64 1                ; &envptr10471[1]
  %a7551 = load i64, i64* %envptr10472, align 8                                      ; load; *envptr10472
  %cloptr10473 = inttoptr i64 %a7551 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10474 = getelementptr inbounds i64, i64* %cloptr10473, i64 0                 ; &cloptr10473[0]
  %f10476 = load i64, i64* %i0ptr10474, align 8                                      ; load; *i0ptr10474
  %fptr10475 = inttoptr i64 %f10476 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10475(i64 %a7551, i64 %cont7650, i64 %a7554, i64 %a7557); tail call
  ret void
}


define void @lam8929(i64 %env8930, i64 %_957658, i64 %a7557) {
  %envptr10477 = inttoptr i64 %env8930 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10478 = getelementptr inbounds i64, i64* %envptr10477, i64 3                ; &envptr10477[3]
  %cont7650 = load i64, i64* %envptr10478, align 8                                   ; load; *envptr10478
  %envptr10479 = inttoptr i64 %env8930 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10480 = getelementptr inbounds i64, i64* %envptr10479, i64 2                ; &envptr10479[2]
  %a7554 = load i64, i64* %envptr10480, align 8                                      ; load; *envptr10480
  %envptr10481 = inttoptr i64 %env8930 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10482 = getelementptr inbounds i64, i64* %envptr10481, i64 1                ; &envptr10481[1]
  %a7551 = load i64, i64* %envptr10482, align 8                                      ; load; *envptr10482
  %cloptr10483 = inttoptr i64 %a7551 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10484 = getelementptr inbounds i64, i64* %cloptr10483, i64 0                 ; &cloptr10483[0]
  %f10486 = load i64, i64* %i0ptr10484, align 8                                      ; load; *i0ptr10484
  %fptr10485 = inttoptr i64 %f10486 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10485(i64 %a7551, i64 %cont7650, i64 %a7554, i64 %a7557); tail call
  ret void
}


define void @lam8926(i64 %env8927, i64 %cont7663, i64 %nL6$new) {
  %envptr10487 = inttoptr i64 %env8927 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10488 = getelementptr inbounds i64, i64* %envptr10487, i64 2                ; &envptr10487[2]
  %v9o$_37wind_45stack = load i64, i64* %envptr10488, align 8                        ; load; *envptr10488
  %envptr10489 = inttoptr i64 %env8927 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10490 = getelementptr inbounds i64, i64* %envptr10489, i64 1                ; &envptr10489[1]
  %nTN$_37common_45tail = load i64, i64* %envptr10490, align 8                       ; load; *envptr10490
  %arg8297 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7558 = call i64 @prim_vector_45ref(i64 %v9o$_37wind_45stack, i64 %arg8297)       ; call prim_vector_45ref
  %cloptr10491 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10493 = getelementptr inbounds i64, i64* %cloptr10491, i64 1                  ; &eptr10493[1]
  %eptr10494 = getelementptr inbounds i64, i64* %cloptr10491, i64 2                  ; &eptr10494[2]
  %eptr10495 = getelementptr inbounds i64, i64* %cloptr10491, i64 3                  ; &eptr10495[3]
  store i64 %cont7663, i64* %eptr10493                                               ; *eptr10493 = %cont7663
  store i64 %v9o$_37wind_45stack, i64* %eptr10494                                    ; *eptr10494 = %v9o$_37wind_45stack
  store i64 %nL6$new, i64* %eptr10495                                                ; *eptr10495 = %nL6$new
  %eptr10492 = getelementptr inbounds i64, i64* %cloptr10491, i64 0                  ; &cloptr10491[0]
  %f10496 = ptrtoint void(i64,i64,i64)* @lam8923 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10496, i64* %eptr10492                                                 ; store fptr
  %arg8301 = ptrtoint i64* %cloptr10491 to i64                                       ; closure cast; i64* -> i64
  %cloptr10497 = inttoptr i64 %nTN$_37common_45tail to i64*                          ; closure/env cast; i64 -> i64*
  %i0ptr10498 = getelementptr inbounds i64, i64* %cloptr10497, i64 0                 ; &cloptr10497[0]
  %f10500 = load i64, i64* %i0ptr10498, align 8                                      ; load; *i0ptr10498
  %fptr10499 = inttoptr i64 %f10500 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10499(i64 %nTN$_37common_45tail, i64 %arg8301, i64 %nL6$new, i64 %a7558); tail call
  ret void
}


define void @lam8923(i64 %env8924, i64 %_957664, i64 %FeO$tail) {
  %envptr10501 = inttoptr i64 %env8924 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10502 = getelementptr inbounds i64, i64* %envptr10501, i64 3                ; &envptr10501[3]
  %nL6$new = load i64, i64* %envptr10502, align 8                                    ; load; *envptr10502
  %envptr10503 = inttoptr i64 %env8924 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10504 = getelementptr inbounds i64, i64* %envptr10503, i64 2                ; &envptr10503[2]
  %v9o$_37wind_45stack = load i64, i64* %envptr10504, align 8                        ; load; *envptr10504
  %envptr10505 = inttoptr i64 %env8924 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10506 = getelementptr inbounds i64, i64* %envptr10505, i64 1                ; &envptr10505[1]
  %cont7663 = load i64, i64* %envptr10506, align 8                                   ; load; *envptr10506
  %cloptr10507 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10508 = getelementptr inbounds i64, i64* %cloptr10507, i64 0                  ; &cloptr10507[0]
  %f10509 = ptrtoint void(i64,i64)* @lam8921 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f10509, i64* %eptr10508                                                 ; store fptr
  %arg8304 = ptrtoint i64* %cloptr10507 to i64                                       ; closure cast; i64* -> i64
  %cloptr10510 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr10512 = getelementptr inbounds i64, i64* %cloptr10510, i64 1                  ; &eptr10512[1]
  %eptr10513 = getelementptr inbounds i64, i64* %cloptr10510, i64 2                  ; &eptr10513[2]
  %eptr10514 = getelementptr inbounds i64, i64* %cloptr10510, i64 3                  ; &eptr10514[3]
  %eptr10515 = getelementptr inbounds i64, i64* %cloptr10510, i64 4                  ; &eptr10515[4]
  store i64 %FeO$tail, i64* %eptr10512                                               ; *eptr10512 = %FeO$tail
  store i64 %cont7663, i64* %eptr10513                                               ; *eptr10513 = %cont7663
  store i64 %v9o$_37wind_45stack, i64* %eptr10514                                    ; *eptr10514 = %v9o$_37wind_45stack
  store i64 %nL6$new, i64* %eptr10515                                                ; *eptr10515 = %nL6$new
  %eptr10511 = getelementptr inbounds i64, i64* %cloptr10510, i64 0                  ; &cloptr10510[0]
  %f10516 = ptrtoint void(i64,i64,i64)* @lam8918 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10516, i64* %eptr10511                                                 ; store fptr
  %arg8303 = ptrtoint i64* %cloptr10510 to i64                                       ; closure cast; i64* -> i64
  %cloptr10517 = inttoptr i64 %arg8304 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10518 = getelementptr inbounds i64, i64* %cloptr10517, i64 0                 ; &cloptr10517[0]
  %f10520 = load i64, i64* %i0ptr10518, align 8                                      ; load; *i0ptr10518
  %fptr10519 = inttoptr i64 %f10520 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10519(i64 %arg8304, i64 %arg8303)                   ; tail call
  ret void
}


define void @lam8921(i64 %env8922, i64 %ahu$lst7685) {
  %cont7684 = call i64 @prim_car(i64 %ahu$lst7685)                                   ; call prim_car
  %ahu$lst = call i64 @prim_cdr(i64 %ahu$lst7685)                                    ; call prim_cdr
  %arg8308 = add i64 0, 0                                                            ; quoted ()
  %cloptr10521 = inttoptr i64 %cont7684 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10522 = getelementptr inbounds i64, i64* %cloptr10521, i64 0                 ; &cloptr10521[0]
  %f10524 = load i64, i64* %i0ptr10522, align 8                                      ; load; *i0ptr10522
  %fptr10523 = inttoptr i64 %f10524 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10523(i64 %cont7684, i64 %arg8308, i64 %ahu$lst)    ; tail call
  ret void
}


define void @lam8918(i64 %env8919, i64 %_957682, i64 %a7559) {
  %envptr10525 = inttoptr i64 %env8919 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10526 = getelementptr inbounds i64, i64* %envptr10525, i64 4                ; &envptr10525[4]
  %nL6$new = load i64, i64* %envptr10526, align 8                                    ; load; *envptr10526
  %envptr10527 = inttoptr i64 %env8919 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10528 = getelementptr inbounds i64, i64* %envptr10527, i64 3                ; &envptr10527[3]
  %v9o$_37wind_45stack = load i64, i64* %envptr10528, align 8                        ; load; *envptr10528
  %envptr10529 = inttoptr i64 %env8919 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10530 = getelementptr inbounds i64, i64* %envptr10529, i64 2                ; &envptr10529[2]
  %cont7663 = load i64, i64* %envptr10530, align 8                                   ; load; *envptr10530
  %envptr10531 = inttoptr i64 %env8919 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10532 = getelementptr inbounds i64, i64* %envptr10531, i64 1                ; &envptr10531[1]
  %FeO$tail = load i64, i64* %envptr10532, align 8                                   ; load; *envptr10532
  %arg8311 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %retprim7683 = call i64 @prim_make_45vector(i64 %arg8311, i64 %a7559)              ; call prim_make_45vector
  %cloptr10533 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr10535 = getelementptr inbounds i64, i64* %cloptr10533, i64 1                  ; &eptr10535[1]
  %eptr10536 = getelementptr inbounds i64, i64* %cloptr10533, i64 2                  ; &eptr10536[2]
  %eptr10537 = getelementptr inbounds i64, i64* %cloptr10533, i64 3                  ; &eptr10537[3]
  %eptr10538 = getelementptr inbounds i64, i64* %cloptr10533, i64 4                  ; &eptr10538[4]
  store i64 %FeO$tail, i64* %eptr10535                                               ; *eptr10535 = %FeO$tail
  store i64 %cont7663, i64* %eptr10536                                               ; *eptr10536 = %cont7663
  store i64 %v9o$_37wind_45stack, i64* %eptr10537                                    ; *eptr10537 = %v9o$_37wind_45stack
  store i64 %nL6$new, i64* %eptr10538                                                ; *eptr10538 = %nL6$new
  %eptr10534 = getelementptr inbounds i64, i64* %cloptr10533, i64 0                  ; &cloptr10533[0]
  %f10539 = ptrtoint void(i64,i64,i64)* @lam8915 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10539, i64* %eptr10534                                                 ; store fptr
  %arg8314 = ptrtoint i64* %cloptr10533 to i64                                       ; closure cast; i64* -> i64
  %arg8313 = add i64 0, 0                                                            ; quoted ()
  %cloptr10540 = inttoptr i64 %arg8314 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10541 = getelementptr inbounds i64, i64* %cloptr10540, i64 0                 ; &cloptr10540[0]
  %f10543 = load i64, i64* %i0ptr10541, align 8                                      ; load; *i0ptr10541
  %fptr10542 = inttoptr i64 %f10543 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10542(i64 %arg8314, i64 %arg8313, i64 %retprim7683) ; tail call
  ret void
}


define void @lam8915(i64 %env8916, i64 %_957676, i64 %JQP$f) {
  %envptr10544 = inttoptr i64 %env8916 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10545 = getelementptr inbounds i64, i64* %envptr10544, i64 4                ; &envptr10544[4]
  %nL6$new = load i64, i64* %envptr10545, align 8                                    ; load; *envptr10545
  %envptr10546 = inttoptr i64 %env8916 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10547 = getelementptr inbounds i64, i64* %envptr10546, i64 3                ; &envptr10546[3]
  %v9o$_37wind_45stack = load i64, i64* %envptr10547, align 8                        ; load; *envptr10547
  %envptr10548 = inttoptr i64 %env8916 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10549 = getelementptr inbounds i64, i64* %envptr10548, i64 2                ; &envptr10548[2]
  %cont7663 = load i64, i64* %envptr10549, align 8                                   ; load; *envptr10549
  %envptr10550 = inttoptr i64 %env8916 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10551 = getelementptr inbounds i64, i64* %envptr10550, i64 1                ; &envptr10550[1]
  %FeO$tail = load i64, i64* %envptr10551, align 8                                   ; load; *envptr10551
  %arg8316 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr10552 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10554 = getelementptr inbounds i64, i64* %cloptr10552, i64 1                  ; &eptr10554[1]
  %eptr10555 = getelementptr inbounds i64, i64* %cloptr10552, i64 2                  ; &eptr10555[2]
  %eptr10556 = getelementptr inbounds i64, i64* %cloptr10552, i64 3                  ; &eptr10556[3]
  store i64 %FeO$tail, i64* %eptr10554                                               ; *eptr10554 = %FeO$tail
  store i64 %v9o$_37wind_45stack, i64* %eptr10555                                    ; *eptr10555 = %v9o$_37wind_45stack
  store i64 %JQP$f, i64* %eptr10556                                                  ; *eptr10556 = %JQP$f
  %eptr10553 = getelementptr inbounds i64, i64* %cloptr10552, i64 0                  ; &cloptr10552[0]
  %f10557 = ptrtoint void(i64,i64,i64)* @lam8912 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10557, i64* %eptr10553                                                 ; store fptr
  %arg8315 = ptrtoint i64* %cloptr10552 to i64                                       ; closure cast; i64* -> i64
  %UrR$_957447 = call i64 @prim_vector_45set_33(i64 %JQP$f, i64 %arg8316, i64 %arg8315); call prim_vector_45set_33
  %arg8341 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7567 = call i64 @prim_vector_45ref(i64 %JQP$f, i64 %arg8341)                     ; call prim_vector_45ref
  %arg8343 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7568 = call i64 @prim_vector_45ref(i64 %v9o$_37wind_45stack, i64 %arg8343)       ; call prim_vector_45ref
  %cloptr10558 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr10560 = getelementptr inbounds i64, i64* %cloptr10558, i64 1                  ; &eptr10560[1]
  %eptr10561 = getelementptr inbounds i64, i64* %cloptr10558, i64 2                  ; &eptr10561[2]
  %eptr10562 = getelementptr inbounds i64, i64* %cloptr10558, i64 3                  ; &eptr10562[3]
  %eptr10563 = getelementptr inbounds i64, i64* %cloptr10558, i64 4                  ; &eptr10563[4]
  store i64 %FeO$tail, i64* %eptr10560                                               ; *eptr10560 = %FeO$tail
  store i64 %cont7663, i64* %eptr10561                                               ; *eptr10561 = %cont7663
  store i64 %v9o$_37wind_45stack, i64* %eptr10562                                    ; *eptr10562 = %v9o$_37wind_45stack
  store i64 %nL6$new, i64* %eptr10563                                                ; *eptr10563 = %nL6$new
  %eptr10559 = getelementptr inbounds i64, i64* %cloptr10558, i64 0                  ; &cloptr10558[0]
  %f10564 = ptrtoint void(i64,i64,i64)* @lam8900 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10564, i64* %eptr10559                                                 ; store fptr
  %arg8346 = ptrtoint i64* %cloptr10558 to i64                                       ; closure cast; i64* -> i64
  %cloptr10565 = inttoptr i64 %a7567 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10566 = getelementptr inbounds i64, i64* %cloptr10565, i64 0                 ; &cloptr10565[0]
  %f10568 = load i64, i64* %i0ptr10566, align 8                                      ; load; *i0ptr10566
  %fptr10567 = inttoptr i64 %f10568 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10567(i64 %a7567, i64 %arg8346, i64 %a7568)         ; tail call
  ret void
}


define void @lam8912(i64 %env8913, i64 %cont7677, i64 %pJY$l) {
  %envptr10569 = inttoptr i64 %env8913 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10570 = getelementptr inbounds i64, i64* %envptr10569, i64 3                ; &envptr10569[3]
  %JQP$f = load i64, i64* %envptr10570, align 8                                      ; load; *envptr10570
  %envptr10571 = inttoptr i64 %env8913 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10572 = getelementptr inbounds i64, i64* %envptr10571, i64 2                ; &envptr10571[2]
  %v9o$_37wind_45stack = load i64, i64* %envptr10572, align 8                        ; load; *envptr10572
  %envptr10573 = inttoptr i64 %env8913 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10574 = getelementptr inbounds i64, i64* %envptr10573, i64 1                ; &envptr10573[1]
  %FeO$tail = load i64, i64* %envptr10574, align 8                                   ; load; *envptr10574
  %a7560 = call i64 @prim_eq_63(i64 %pJY$l, i64 %FeO$tail)                           ; call prim_eq_63
  %a7561 = call i64 @prim_not(i64 %a7560)                                            ; call prim_not
  %cmp10575 = icmp eq i64 %a7561, 15                                                 ; false?
  br i1 %cmp10575, label %else10577, label %then10576                                ; if

then10576:
  %a7562 = call i64 @prim_cdr(i64 %pJY$l)                                            ; call prim_cdr
  %arg8323 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7680 = call i64 @prim_vector_45set_33(i64 %v9o$_37wind_45stack, i64 %arg8323, i64 %a7562); call prim_vector_45set_33
  %cloptr10578 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10580 = getelementptr inbounds i64, i64* %cloptr10578, i64 1                  ; &eptr10580[1]
  %eptr10581 = getelementptr inbounds i64, i64* %cloptr10578, i64 2                  ; &eptr10581[2]
  %eptr10582 = getelementptr inbounds i64, i64* %cloptr10578, i64 3                  ; &eptr10582[3]
  store i64 %pJY$l, i64* %eptr10580                                                  ; *eptr10580 = %pJY$l
  store i64 %JQP$f, i64* %eptr10581                                                  ; *eptr10581 = %JQP$f
  store i64 %cont7677, i64* %eptr10582                                               ; *eptr10582 = %cont7677
  %eptr10579 = getelementptr inbounds i64, i64* %cloptr10578, i64 0                  ; &cloptr10578[0]
  %f10583 = ptrtoint void(i64,i64,i64)* @lam8908 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10583, i64* %eptr10579                                                 ; store fptr
  %arg8327 = ptrtoint i64* %cloptr10578 to i64                                       ; closure cast; i64* -> i64
  %arg8326 = add i64 0, 0                                                            ; quoted ()
  %cloptr10584 = inttoptr i64 %arg8327 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10585 = getelementptr inbounds i64, i64* %cloptr10584, i64 0                 ; &cloptr10584[0]
  %f10587 = load i64, i64* %i0ptr10585, align 8                                      ; load; *i0ptr10585
  %fptr10586 = inttoptr i64 %f10587 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10586(i64 %arg8327, i64 %arg8326, i64 %retprim7680) ; tail call
  ret void

else10577:
  %retprim7681 = call i64 @prim_void()                                               ; call prim_void
  %arg8339 = add i64 0, 0                                                            ; quoted ()
  %cloptr10588 = inttoptr i64 %cont7677 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10589 = getelementptr inbounds i64, i64* %cloptr10588, i64 0                 ; &cloptr10588[0]
  %f10591 = load i64, i64* %i0ptr10589, align 8                                      ; load; *i0ptr10589
  %fptr10590 = inttoptr i64 %f10591 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10590(i64 %cont7677, i64 %arg8339, i64 %retprim7681); tail call
  ret void
}


define void @lam8908(i64 %env8909, i64 %_957678, i64 %luM$_957448) {
  %envptr10592 = inttoptr i64 %env8909 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10593 = getelementptr inbounds i64, i64* %envptr10592, i64 3                ; &envptr10592[3]
  %cont7677 = load i64, i64* %envptr10593, align 8                                   ; load; *envptr10593
  %envptr10594 = inttoptr i64 %env8909 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10595 = getelementptr inbounds i64, i64* %envptr10594, i64 2                ; &envptr10594[2]
  %JQP$f = load i64, i64* %envptr10595, align 8                                      ; load; *envptr10595
  %envptr10596 = inttoptr i64 %env8909 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10597 = getelementptr inbounds i64, i64* %envptr10596, i64 1                ; &envptr10596[1]
  %pJY$l = load i64, i64* %envptr10597, align 8                                      ; load; *envptr10597
  %a7563 = call i64 @prim_car(i64 %pJY$l)                                            ; call prim_car
  %a7564 = call i64 @prim_cdr(i64 %a7563)                                            ; call prim_cdr
  %cloptr10598 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10600 = getelementptr inbounds i64, i64* %cloptr10598, i64 1                  ; &eptr10600[1]
  %eptr10601 = getelementptr inbounds i64, i64* %cloptr10598, i64 2                  ; &eptr10601[2]
  %eptr10602 = getelementptr inbounds i64, i64* %cloptr10598, i64 3                  ; &eptr10602[3]
  store i64 %pJY$l, i64* %eptr10600                                                  ; *eptr10600 = %pJY$l
  store i64 %JQP$f, i64* %eptr10601                                                  ; *eptr10601 = %JQP$f
  store i64 %cont7677, i64* %eptr10602                                               ; *eptr10602 = %cont7677
  %eptr10599 = getelementptr inbounds i64, i64* %cloptr10598, i64 0                  ; &cloptr10598[0]
  %f10603 = ptrtoint void(i64,i64,i64)* @lam8906 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10603, i64* %eptr10599                                                 ; store fptr
  %arg8330 = ptrtoint i64* %cloptr10598 to i64                                       ; closure cast; i64* -> i64
  %cloptr10604 = inttoptr i64 %a7564 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10605 = getelementptr inbounds i64, i64* %cloptr10604, i64 0                 ; &cloptr10604[0]
  %f10607 = load i64, i64* %i0ptr10605, align 8                                      ; load; *i0ptr10605
  %fptr10606 = inttoptr i64 %f10607 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10606(i64 %a7564, i64 %arg8330)                     ; tail call
  ret void
}


define void @lam8906(i64 %env8907, i64 %_957679, i64 %bGw$_957449) {
  %envptr10608 = inttoptr i64 %env8907 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10609 = getelementptr inbounds i64, i64* %envptr10608, i64 3                ; &envptr10608[3]
  %cont7677 = load i64, i64* %envptr10609, align 8                                   ; load; *envptr10609
  %envptr10610 = inttoptr i64 %env8907 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10611 = getelementptr inbounds i64, i64* %envptr10610, i64 2                ; &envptr10610[2]
  %JQP$f = load i64, i64* %envptr10611, align 8                                      ; load; *envptr10611
  %envptr10612 = inttoptr i64 %env8907 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10613 = getelementptr inbounds i64, i64* %envptr10612, i64 1                ; &envptr10612[1]
  %pJY$l = load i64, i64* %envptr10613, align 8                                      ; load; *envptr10613
  %arg8332 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7565 = call i64 @prim_vector_45ref(i64 %JQP$f, i64 %arg8332)                     ; call prim_vector_45ref
  %a7566 = call i64 @prim_cdr(i64 %pJY$l)                                            ; call prim_cdr
  %cloptr10614 = inttoptr i64 %a7565 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10615 = getelementptr inbounds i64, i64* %cloptr10614, i64 0                 ; &cloptr10614[0]
  %f10617 = load i64, i64* %i0ptr10615, align 8                                      ; load; *i0ptr10615
  %fptr10616 = inttoptr i64 %f10617 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10616(i64 %a7565, i64 %cont7677, i64 %a7566)        ; tail call
  ret void
}


define void @lam8900(i64 %env8901, i64 %_957665, i64 %ZiR$_957446) {
  %envptr10618 = inttoptr i64 %env8901 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10619 = getelementptr inbounds i64, i64* %envptr10618, i64 4                ; &envptr10618[4]
  %nL6$new = load i64, i64* %envptr10619, align 8                                    ; load; *envptr10619
  %envptr10620 = inttoptr i64 %env8901 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10621 = getelementptr inbounds i64, i64* %envptr10620, i64 3                ; &envptr10620[3]
  %v9o$_37wind_45stack = load i64, i64* %envptr10621, align 8                        ; load; *envptr10621
  %envptr10622 = inttoptr i64 %env8901 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10623 = getelementptr inbounds i64, i64* %envptr10622, i64 2                ; &envptr10622[2]
  %cont7663 = load i64, i64* %envptr10623, align 8                                   ; load; *envptr10623
  %envptr10624 = inttoptr i64 %env8901 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10625 = getelementptr inbounds i64, i64* %envptr10624, i64 1                ; &envptr10624[1]
  %FeO$tail = load i64, i64* %envptr10625, align 8                                   ; load; *envptr10625
  %cloptr10626 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10627 = getelementptr inbounds i64, i64* %cloptr10626, i64 0                  ; &cloptr10626[0]
  %f10628 = ptrtoint void(i64,i64)* @lam8898 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f10628, i64* %eptr10627                                                 ; store fptr
  %arg8349 = ptrtoint i64* %cloptr10626 to i64                                       ; closure cast; i64* -> i64
  %cloptr10629 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr10631 = getelementptr inbounds i64, i64* %cloptr10629, i64 1                  ; &eptr10631[1]
  %eptr10632 = getelementptr inbounds i64, i64* %cloptr10629, i64 2                  ; &eptr10632[2]
  %eptr10633 = getelementptr inbounds i64, i64* %cloptr10629, i64 3                  ; &eptr10633[3]
  %eptr10634 = getelementptr inbounds i64, i64* %cloptr10629, i64 4                  ; &eptr10634[4]
  store i64 %FeO$tail, i64* %eptr10631                                               ; *eptr10631 = %FeO$tail
  store i64 %cont7663, i64* %eptr10632                                               ; *eptr10632 = %cont7663
  store i64 %v9o$_37wind_45stack, i64* %eptr10633                                    ; *eptr10633 = %v9o$_37wind_45stack
  store i64 %nL6$new, i64* %eptr10634                                                ; *eptr10634 = %nL6$new
  %eptr10630 = getelementptr inbounds i64, i64* %cloptr10629, i64 0                  ; &cloptr10629[0]
  %f10635 = ptrtoint void(i64,i64,i64)* @lam8895 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10635, i64* %eptr10630                                                 ; store fptr
  %arg8348 = ptrtoint i64* %cloptr10629 to i64                                       ; closure cast; i64* -> i64
  %cloptr10636 = inttoptr i64 %arg8349 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10637 = getelementptr inbounds i64, i64* %cloptr10636, i64 0                 ; &cloptr10636[0]
  %f10639 = load i64, i64* %i0ptr10637, align 8                                      ; load; *i0ptr10637
  %fptr10638 = inttoptr i64 %f10639 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10638(i64 %arg8349, i64 %arg8348)                   ; tail call
  ret void
}


define void @lam8898(i64 %env8899, i64 %d9K$lst7675) {
  %cont7674 = call i64 @prim_car(i64 %d9K$lst7675)                                   ; call prim_car
  %d9K$lst = call i64 @prim_cdr(i64 %d9K$lst7675)                                    ; call prim_cdr
  %arg8353 = add i64 0, 0                                                            ; quoted ()
  %cloptr10640 = inttoptr i64 %cont7674 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10641 = getelementptr inbounds i64, i64* %cloptr10640, i64 0                 ; &cloptr10640[0]
  %f10643 = load i64, i64* %i0ptr10641, align 8                                      ; load; *i0ptr10641
  %fptr10642 = inttoptr i64 %f10643 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10642(i64 %cont7674, i64 %arg8353, i64 %d9K$lst)    ; tail call
  ret void
}


define void @lam8895(i64 %env8896, i64 %_957672, i64 %a7569) {
  %envptr10644 = inttoptr i64 %env8896 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10645 = getelementptr inbounds i64, i64* %envptr10644, i64 4                ; &envptr10644[4]
  %nL6$new = load i64, i64* %envptr10645, align 8                                    ; load; *envptr10645
  %envptr10646 = inttoptr i64 %env8896 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10647 = getelementptr inbounds i64, i64* %envptr10646, i64 3                ; &envptr10646[3]
  %v9o$_37wind_45stack = load i64, i64* %envptr10647, align 8                        ; load; *envptr10647
  %envptr10648 = inttoptr i64 %env8896 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10649 = getelementptr inbounds i64, i64* %envptr10648, i64 2                ; &envptr10648[2]
  %cont7663 = load i64, i64* %envptr10649, align 8                                   ; load; *envptr10649
  %envptr10650 = inttoptr i64 %env8896 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10651 = getelementptr inbounds i64, i64* %envptr10650, i64 1                ; &envptr10650[1]
  %FeO$tail = load i64, i64* %envptr10651, align 8                                   ; load; *envptr10651
  %arg8356 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %retprim7673 = call i64 @prim_make_45vector(i64 %arg8356, i64 %a7569)              ; call prim_make_45vector
  %cloptr10652 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr10654 = getelementptr inbounds i64, i64* %cloptr10652, i64 1                  ; &eptr10654[1]
  %eptr10655 = getelementptr inbounds i64, i64* %cloptr10652, i64 2                  ; &eptr10655[2]
  %eptr10656 = getelementptr inbounds i64, i64* %cloptr10652, i64 3                  ; &eptr10656[3]
  %eptr10657 = getelementptr inbounds i64, i64* %cloptr10652, i64 4                  ; &eptr10657[4]
  store i64 %FeO$tail, i64* %eptr10654                                               ; *eptr10654 = %FeO$tail
  store i64 %cont7663, i64* %eptr10655                                               ; *eptr10655 = %cont7663
  store i64 %v9o$_37wind_45stack, i64* %eptr10656                                    ; *eptr10656 = %v9o$_37wind_45stack
  store i64 %nL6$new, i64* %eptr10657                                                ; *eptr10657 = %nL6$new
  %eptr10653 = getelementptr inbounds i64, i64* %cloptr10652, i64 0                  ; &cloptr10652[0]
  %f10658 = ptrtoint void(i64,i64,i64)* @lam8892 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10658, i64* %eptr10653                                                 ; store fptr
  %arg8359 = ptrtoint i64* %cloptr10652 to i64                                       ; closure cast; i64* -> i64
  %arg8358 = add i64 0, 0                                                            ; quoted ()
  %cloptr10659 = inttoptr i64 %arg8359 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10660 = getelementptr inbounds i64, i64* %cloptr10659, i64 0                 ; &cloptr10659[0]
  %f10662 = load i64, i64* %i0ptr10660, align 8                                      ; load; *i0ptr10660
  %fptr10661 = inttoptr i64 %f10662 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10661(i64 %arg8359, i64 %arg8358, i64 %retprim7673) ; tail call
  ret void
}


define void @lam8892(i64 %env8893, i64 %_957666, i64 %ocF$f) {
  %envptr10663 = inttoptr i64 %env8893 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10664 = getelementptr inbounds i64, i64* %envptr10663, i64 4                ; &envptr10663[4]
  %nL6$new = load i64, i64* %envptr10664, align 8                                    ; load; *envptr10664
  %envptr10665 = inttoptr i64 %env8893 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10666 = getelementptr inbounds i64, i64* %envptr10665, i64 3                ; &envptr10665[3]
  %v9o$_37wind_45stack = load i64, i64* %envptr10666, align 8                        ; load; *envptr10666
  %envptr10667 = inttoptr i64 %env8893 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10668 = getelementptr inbounds i64, i64* %envptr10667, i64 2                ; &envptr10667[2]
  %cont7663 = load i64, i64* %envptr10668, align 8                                   ; load; *envptr10668
  %envptr10669 = inttoptr i64 %env8893 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10670 = getelementptr inbounds i64, i64* %envptr10669, i64 1                ; &envptr10669[1]
  %FeO$tail = load i64, i64* %envptr10670, align 8                                   ; load; *envptr10670
  %arg8361 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr10671 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10673 = getelementptr inbounds i64, i64* %cloptr10671, i64 1                  ; &eptr10673[1]
  %eptr10674 = getelementptr inbounds i64, i64* %cloptr10671, i64 2                  ; &eptr10674[2]
  %eptr10675 = getelementptr inbounds i64, i64* %cloptr10671, i64 3                  ; &eptr10675[3]
  store i64 %FeO$tail, i64* %eptr10673                                               ; *eptr10673 = %FeO$tail
  store i64 %ocF$f, i64* %eptr10674                                                  ; *eptr10674 = %ocF$f
  store i64 %v9o$_37wind_45stack, i64* %eptr10675                                    ; *eptr10675 = %v9o$_37wind_45stack
  %eptr10672 = getelementptr inbounds i64, i64* %cloptr10671, i64 0                  ; &cloptr10671[0]
  %f10676 = ptrtoint void(i64,i64,i64)* @lam8889 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10676, i64* %eptr10672                                                 ; store fptr
  %arg8360 = ptrtoint i64* %cloptr10671 to i64                                       ; closure cast; i64* -> i64
  %ugd$_957450 = call i64 @prim_vector_45set_33(i64 %ocF$f, i64 %arg8361, i64 %arg8360); call prim_vector_45set_33
  %arg8385 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7576 = call i64 @prim_vector_45ref(i64 %ocF$f, i64 %arg8385)                     ; call prim_vector_45ref
  %cloptr10677 = inttoptr i64 %a7576 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10678 = getelementptr inbounds i64, i64* %cloptr10677, i64 0                 ; &cloptr10677[0]
  %f10680 = load i64, i64* %i0ptr10678, align 8                                      ; load; *i0ptr10678
  %fptr10679 = inttoptr i64 %f10680 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10679(i64 %a7576, i64 %cont7663, i64 %nL6$new)      ; tail call
  ret void
}


define void @lam8889(i64 %env8890, i64 %cont7667, i64 %hvO$l) {
  %envptr10681 = inttoptr i64 %env8890 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10682 = getelementptr inbounds i64, i64* %envptr10681, i64 3                ; &envptr10681[3]
  %v9o$_37wind_45stack = load i64, i64* %envptr10682, align 8                        ; load; *envptr10682
  %envptr10683 = inttoptr i64 %env8890 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10684 = getelementptr inbounds i64, i64* %envptr10683, i64 2                ; &envptr10683[2]
  %ocF$f = load i64, i64* %envptr10684, align 8                                      ; load; *envptr10684
  %envptr10685 = inttoptr i64 %env8890 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10686 = getelementptr inbounds i64, i64* %envptr10685, i64 1                ; &envptr10685[1]
  %FeO$tail = load i64, i64* %envptr10686, align 8                                   ; load; *envptr10686
  %a7570 = call i64 @prim_eq_63(i64 %hvO$l, i64 %FeO$tail)                           ; call prim_eq_63
  %a7571 = call i64 @prim_not(i64 %a7570)                                            ; call prim_not
  %cmp10687 = icmp eq i64 %a7571, 15                                                 ; false?
  br i1 %cmp10687, label %else10689, label %then10688                                ; if

then10688:
  %arg8366 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7572 = call i64 @prim_vector_45ref(i64 %ocF$f, i64 %arg8366)                     ; call prim_vector_45ref
  %a7573 = call i64 @prim_cdr(i64 %hvO$l)                                            ; call prim_cdr
  %cloptr10690 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10692 = getelementptr inbounds i64, i64* %cloptr10690, i64 1                  ; &eptr10692[1]
  %eptr10693 = getelementptr inbounds i64, i64* %cloptr10690, i64 2                  ; &eptr10693[2]
  %eptr10694 = getelementptr inbounds i64, i64* %cloptr10690, i64 3                  ; &eptr10694[3]
  store i64 %hvO$l, i64* %eptr10692                                                  ; *eptr10692 = %hvO$l
  store i64 %v9o$_37wind_45stack, i64* %eptr10693                                    ; *eptr10693 = %v9o$_37wind_45stack
  store i64 %cont7667, i64* %eptr10694                                               ; *eptr10694 = %cont7667
  %eptr10691 = getelementptr inbounds i64, i64* %cloptr10690, i64 0                  ; &cloptr10690[0]
  %f10695 = ptrtoint void(i64,i64,i64)* @lam8885 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10695, i64* %eptr10691                                                 ; store fptr
  %arg8370 = ptrtoint i64* %cloptr10690 to i64                                       ; closure cast; i64* -> i64
  %cloptr10696 = inttoptr i64 %a7572 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10697 = getelementptr inbounds i64, i64* %cloptr10696, i64 0                 ; &cloptr10696[0]
  %f10699 = load i64, i64* %i0ptr10697, align 8                                      ; load; *i0ptr10697
  %fptr10698 = inttoptr i64 %f10699 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10698(i64 %a7572, i64 %arg8370, i64 %a7573)         ; tail call
  ret void

else10689:
  %retprim7671 = call i64 @prim_void()                                               ; call prim_void
  %arg8383 = add i64 0, 0                                                            ; quoted ()
  %cloptr10700 = inttoptr i64 %cont7667 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10701 = getelementptr inbounds i64, i64* %cloptr10700, i64 0                 ; &cloptr10700[0]
  %f10703 = load i64, i64* %i0ptr10701, align 8                                      ; load; *i0ptr10701
  %fptr10702 = inttoptr i64 %f10703 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10702(i64 %cont7667, i64 %arg8383, i64 %retprim7671); tail call
  ret void
}


define void @lam8885(i64 %env8886, i64 %_957668, i64 %jrp$_957451) {
  %envptr10704 = inttoptr i64 %env8886 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10705 = getelementptr inbounds i64, i64* %envptr10704, i64 3                ; &envptr10704[3]
  %cont7667 = load i64, i64* %envptr10705, align 8                                   ; load; *envptr10705
  %envptr10706 = inttoptr i64 %env8886 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10707 = getelementptr inbounds i64, i64* %envptr10706, i64 2                ; &envptr10706[2]
  %v9o$_37wind_45stack = load i64, i64* %envptr10707, align 8                        ; load; *envptr10707
  %envptr10708 = inttoptr i64 %env8886 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10709 = getelementptr inbounds i64, i64* %envptr10708, i64 1                ; &envptr10708[1]
  %hvO$l = load i64, i64* %envptr10709, align 8                                      ; load; *envptr10709
  %a7574 = call i64 @prim_car(i64 %hvO$l)                                            ; call prim_car
  %a7575 = call i64 @prim_car(i64 %a7574)                                            ; call prim_car
  %cloptr10710 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr10712 = getelementptr inbounds i64, i64* %cloptr10710, i64 1                  ; &eptr10712[1]
  %eptr10713 = getelementptr inbounds i64, i64* %cloptr10710, i64 2                  ; &eptr10713[2]
  %eptr10714 = getelementptr inbounds i64, i64* %cloptr10710, i64 3                  ; &eptr10714[3]
  store i64 %hvO$l, i64* %eptr10712                                                  ; *eptr10712 = %hvO$l
  store i64 %v9o$_37wind_45stack, i64* %eptr10713                                    ; *eptr10713 = %v9o$_37wind_45stack
  store i64 %cont7667, i64* %eptr10714                                               ; *eptr10714 = %cont7667
  %eptr10711 = getelementptr inbounds i64, i64* %cloptr10710, i64 0                  ; &cloptr10710[0]
  %f10715 = ptrtoint void(i64,i64,i64)* @lam8883 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10715, i64* %eptr10711                                                 ; store fptr
  %arg8374 = ptrtoint i64* %cloptr10710 to i64                                       ; closure cast; i64* -> i64
  %cloptr10716 = inttoptr i64 %a7575 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10717 = getelementptr inbounds i64, i64* %cloptr10716, i64 0                 ; &cloptr10716[0]
  %f10719 = load i64, i64* %i0ptr10717, align 8                                      ; load; *i0ptr10717
  %fptr10718 = inttoptr i64 %f10719 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10718(i64 %a7575, i64 %arg8374)                     ; tail call
  ret void
}


define void @lam8883(i64 %env8884, i64 %_957669, i64 %sL8$_957452) {
  %envptr10720 = inttoptr i64 %env8884 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10721 = getelementptr inbounds i64, i64* %envptr10720, i64 3                ; &envptr10720[3]
  %cont7667 = load i64, i64* %envptr10721, align 8                                   ; load; *envptr10721
  %envptr10722 = inttoptr i64 %env8884 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10723 = getelementptr inbounds i64, i64* %envptr10722, i64 2                ; &envptr10722[2]
  %v9o$_37wind_45stack = load i64, i64* %envptr10723, align 8                        ; load; *envptr10723
  %envptr10724 = inttoptr i64 %env8884 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10725 = getelementptr inbounds i64, i64* %envptr10724, i64 1                ; &envptr10724[1]
  %hvO$l = load i64, i64* %envptr10725, align 8                                      ; load; *envptr10725
  %arg8377 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %retprim7670 = call i64 @prim_vector_45set_33(i64 %v9o$_37wind_45stack, i64 %arg8377, i64 %hvO$l); call prim_vector_45set_33
  %arg8380 = add i64 0, 0                                                            ; quoted ()
  %cloptr10726 = inttoptr i64 %cont7667 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10727 = getelementptr inbounds i64, i64* %cloptr10726, i64 0                 ; &cloptr10726[0]
  %f10729 = load i64, i64* %i0ptr10727, align 8                                      ; load; *i0ptr10727
  %fptr10728 = inttoptr i64 %f10729 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10728(i64 %cont7667, i64 %arg8380, i64 %retprim7670); tail call
  ret void
}


define void @lam8876(i64 %env8877, i64 %Oq4$lst7696) {
  %cont7695 = call i64 @prim_car(i64 %Oq4$lst7696)                                   ; call prim_car
  %Oq4$lst = call i64 @prim_cdr(i64 %Oq4$lst7696)                                    ; call prim_cdr
  %arg8395 = add i64 0, 0                                                            ; quoted ()
  %rva8682 = add i64 0, 0                                                            ; quoted ()
  %rva8681 = call i64 @prim_cons(i64 %Oq4$lst, i64 %rva8682)                         ; call prim_cons
  %rva8680 = call i64 @prim_cons(i64 %arg8395, i64 %rva8681)                         ; call prim_cons
  %cloptr10730 = inttoptr i64 %cont7695 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10731 = getelementptr inbounds i64, i64* %cloptr10730, i64 0                 ; &cloptr10730[0]
  %f10733 = load i64, i64* %i0ptr10731, align 8                                      ; load; *i0ptr10731
  %fptr10732 = inttoptr i64 %f10733 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10732(i64 %cont7695, i64 %rva8680)                  ; tail call
  ret void
}


define void @lam8872(i64 %env8873, i64 %rvp8697) {
  %_957693 = call i64 @prim_car(i64 %rvp8697)                                        ; call prim_car
  %rvp8696 = call i64 @prim_cdr(i64 %rvp8697)                                        ; call prim_cdr
  %a7577 = call i64 @prim_car(i64 %rvp8696)                                          ; call prim_car
  %na8684 = call i64 @prim_cdr(i64 %rvp8696)                                         ; call prim_cdr
  %arg8398 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %retprim7694 = call i64 @prim_make_45vector(i64 %arg8398, i64 %a7577)              ; call prim_make_45vector
  %cloptr10734 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10735 = getelementptr inbounds i64, i64* %cloptr10734, i64 0                  ; &cloptr10734[0]
  %f10736 = ptrtoint void(i64,i64,i64)* @lam8869 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10736, i64* %eptr10735                                                 ; store fptr
  %arg8401 = ptrtoint i64* %cloptr10734 to i64                                       ; closure cast; i64* -> i64
  %arg8400 = add i64 0, 0                                                            ; quoted ()
  %cloptr10737 = inttoptr i64 %arg8401 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10738 = getelementptr inbounds i64, i64* %cloptr10737, i64 0                 ; &cloptr10737[0]
  %f10740 = load i64, i64* %i0ptr10738, align 8                                      ; load; *i0ptr10738
  %fptr10739 = inttoptr i64 %f10740 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10739(i64 %arg8401, i64 %arg8400, i64 %retprim7694) ; tail call
  ret void
}


define void @lam8869(i64 %env8870, i64 %_957686, i64 %dz5$a) {
  %cloptr10741 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10742 = getelementptr inbounds i64, i64* %cloptr10741, i64 0                  ; &cloptr10741[0]
  %f10743 = ptrtoint void(i64,i64)* @lam8867 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f10743, i64* %eptr10742                                                 ; store fptr
  %arg8403 = ptrtoint i64* %cloptr10741 to i64                                       ; closure cast; i64* -> i64
  %cloptr10744 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr10746 = getelementptr inbounds i64, i64* %cloptr10744, i64 1                  ; &eptr10746[1]
  store i64 %dz5$a, i64* %eptr10746                                                  ; *eptr10746 = %dz5$a
  %eptr10745 = getelementptr inbounds i64, i64* %cloptr10744, i64 0                  ; &cloptr10744[0]
  %f10747 = ptrtoint void(i64,i64)* @lam8863 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f10747, i64* %eptr10745                                                 ; store fptr
  %arg8402 = ptrtoint i64* %cloptr10744 to i64                                       ; closure cast; i64* -> i64
  %rva8695 = add i64 0, 0                                                            ; quoted ()
  %rva8694 = call i64 @prim_cons(i64 %arg8402, i64 %rva8695)                         ; call prim_cons
  %cloptr10748 = inttoptr i64 %arg8403 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10749 = getelementptr inbounds i64, i64* %cloptr10748, i64 0                 ; &cloptr10748[0]
  %f10751 = load i64, i64* %i0ptr10749, align 8                                      ; load; *i0ptr10749
  %fptr10750 = inttoptr i64 %f10751 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10750(i64 %arg8403, i64 %rva8694)                   ; tail call
  ret void
}


define void @lam8867(i64 %env8868, i64 %yvY$lst7692) {
  %cont7691 = call i64 @prim_car(i64 %yvY$lst7692)                                   ; call prim_car
  %yvY$lst = call i64 @prim_cdr(i64 %yvY$lst7692)                                    ; call prim_cdr
  %arg8407 = add i64 0, 0                                                            ; quoted ()
  %rva8687 = add i64 0, 0                                                            ; quoted ()
  %rva8686 = call i64 @prim_cons(i64 %yvY$lst, i64 %rva8687)                         ; call prim_cons
  %rva8685 = call i64 @prim_cons(i64 %arg8407, i64 %rva8686)                         ; call prim_cons
  %cloptr10752 = inttoptr i64 %cont7691 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10753 = getelementptr inbounds i64, i64* %cloptr10752, i64 0                 ; &cloptr10752[0]
  %f10755 = load i64, i64* %i0ptr10753, align 8                                      ; load; *i0ptr10753
  %fptr10754 = inttoptr i64 %f10755 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10754(i64 %cont7691, i64 %rva8685)                  ; tail call
  ret void
}


define void @lam8863(i64 %env8864, i64 %rvp8693) {
  %envptr10756 = inttoptr i64 %env8864 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10757 = getelementptr inbounds i64, i64* %envptr10756, i64 1                ; &envptr10756[1]
  %dz5$a = load i64, i64* %envptr10757, align 8                                      ; load; *envptr10757
  %_957689 = call i64 @prim_car(i64 %rvp8693)                                        ; call prim_car
  %rvp8692 = call i64 @prim_cdr(i64 %rvp8693)                                        ; call prim_cdr
  %a7578 = call i64 @prim_car(i64 %rvp8692)                                          ; call prim_car
  %na8689 = call i64 @prim_cdr(i64 %rvp8692)                                         ; call prim_cdr
  %arg8410 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %retprim7690 = call i64 @prim_make_45vector(i64 %arg8410, i64 %a7578)              ; call prim_make_45vector
  %cloptr10758 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr10760 = getelementptr inbounds i64, i64* %cloptr10758, i64 1                  ; &eptr10760[1]
  store i64 %dz5$a, i64* %eptr10760                                                  ; *eptr10760 = %dz5$a
  %eptr10759 = getelementptr inbounds i64, i64* %cloptr10758, i64 0                  ; &cloptr10758[0]
  %f10761 = ptrtoint void(i64,i64,i64)* @lam8860 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10761, i64* %eptr10759                                                 ; store fptr
  %arg8413 = ptrtoint i64* %cloptr10758 to i64                                       ; closure cast; i64* -> i64
  %arg8412 = add i64 0, 0                                                            ; quoted ()
  %cloptr10762 = inttoptr i64 %arg8413 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10763 = getelementptr inbounds i64, i64* %cloptr10762, i64 0                 ; &cloptr10762[0]
  %f10765 = load i64, i64* %i0ptr10763, align 8                                      ; load; *i0ptr10763
  %fptr10764 = inttoptr i64 %f10765 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10764(i64 %arg8413, i64 %arg8412, i64 %retprim7690) ; tail call
  ret void
}


define void @lam8860(i64 %env8861, i64 %_957687, i64 %OhW$b) {
  %envptr10766 = inttoptr i64 %env8861 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10767 = getelementptr inbounds i64, i64* %envptr10766, i64 1                ; &envptr10766[1]
  %dz5$a = load i64, i64* %envptr10767, align 8                                      ; load; *envptr10767
  %arg8415 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %arg8414 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %jaq$_957453 = call i64 @prim_vector_45set_33(i64 %dz5$a, i64 %arg8415, i64 %arg8414); call prim_vector_45set_33
  %arg8418 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %arg8417 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %eN8$_957454 = call i64 @prim_vector_45set_33(i64 %OhW$b, i64 %arg8418, i64 %arg8417); call prim_vector_45set_33
  %arg8420 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7579 = call i64 @prim_vector_45ref(i64 %dz5$a, i64 %arg8420)                     ; call prim_vector_45ref
  %arg8422 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7580 = call i64 @prim_vector_45ref(i64 %OhW$b, i64 %arg8422)                     ; call prim_vector_45ref
  %retprim7688 = call i64 @prim__47(i64 %a7579, i64 %a7580)                          ; call prim__47
  %cloptr10768 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10769 = getelementptr inbounds i64, i64* %cloptr10768, i64 0                  ; &cloptr10768[0]
  %f10770 = ptrtoint void(i64,i64,i64)* @lam8852 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10770, i64* %eptr10769                                                 ; store fptr
  %arg8428 = ptrtoint i64* %cloptr10768 to i64                                       ; closure cast; i64* -> i64
  %arg8427 = add i64 0, 0                                                            ; quoted ()
  %cloptr10771 = inttoptr i64 %arg8428 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10772 = getelementptr inbounds i64, i64* %cloptr10771, i64 0                 ; &cloptr10771[0]
  %f10774 = load i64, i64* %i0ptr10772, align 8                                      ; load; *i0ptr10772
  %fptr10773 = inttoptr i64 %f10774 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10773(i64 %arg8428, i64 %arg8427, i64 %retprim7688) ; tail call
  ret void
}


define void @lam8852(i64 %env8853, i64 %_950, i64 %x) {
  %_951 = call i64 @prim_halt(i64 %x)                                                ; call prim_halt
  %rva8691 = add i64 0, 0                                                            ; quoted ()
  %rva8690 = call i64 @prim_cons(i64 %_951, i64 %rva8691)                            ; call prim_cons
  %cloptr10775 = inttoptr i64 %_951 to i64*                                          ; closure/env cast; i64 -> i64*
  %i0ptr10776 = getelementptr inbounds i64, i64* %cloptr10775, i64 0                 ; &cloptr10775[0]
  %f10778 = load i64, i64* %i0ptr10776, align 8                                      ; load; *i0ptr10776
  %fptr10777 = inttoptr i64 %f10778 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10777(i64 %_951, i64 %rva8690)                      ; tail call
  ret void
}


define void @lam8840(i64 %env8841, i64 %cont7711, i64 %Fod$_37foldl) {
  %envptr10779 = inttoptr i64 %env8841 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10780 = getelementptr inbounds i64, i64* %envptr10779, i64 3                ; &envptr10779[3]
  %rqD$_37foldr1 = load i64, i64* %envptr10780, align 8                              ; load; *envptr10780
  %envptr10781 = inttoptr i64 %env8841 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10782 = getelementptr inbounds i64, i64* %envptr10781, i64 2                ; &envptr10781[2]
  %POA$_37foldr = load i64, i64* %envptr10782, align 8                               ; load; *envptr10782
  %envptr10783 = inttoptr i64 %env8841 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10784 = getelementptr inbounds i64, i64* %envptr10783, i64 1                ; &envptr10783[1]
  %umA$_37map1 = load i64, i64* %envptr10784, align 8                                ; load; *envptr10784
  %arg8433 = add i64 0, 0                                                            ; quoted ()
  %cloptr10785 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr10787 = getelementptr inbounds i64, i64* %cloptr10785, i64 1                  ; &eptr10787[1]
  %eptr10788 = getelementptr inbounds i64, i64* %cloptr10785, i64 2                  ; &eptr10788[2]
  %eptr10789 = getelementptr inbounds i64, i64* %cloptr10785, i64 3                  ; &eptr10789[3]
  %eptr10790 = getelementptr inbounds i64, i64* %cloptr10785, i64 4                  ; &eptr10790[4]
  store i64 %umA$_37map1, i64* %eptr10787                                            ; *eptr10787 = %umA$_37map1
  store i64 %Fod$_37foldl, i64* %eptr10788                                           ; *eptr10788 = %Fod$_37foldl
  store i64 %POA$_37foldr, i64* %eptr10789                                           ; *eptr10789 = %POA$_37foldr
  store i64 %rqD$_37foldr1, i64* %eptr10790                                          ; *eptr10790 = %rqD$_37foldr1
  %eptr10786 = getelementptr inbounds i64, i64* %cloptr10785, i64 0                  ; &cloptr10785[0]
  %f10791 = ptrtoint void(i64,i64)* @lam8837 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f10791, i64* %eptr10786                                                 ; store fptr
  %arg8432 = ptrtoint i64* %cloptr10785 to i64                                       ; closure cast; i64* -> i64
  %cloptr10792 = inttoptr i64 %cont7711 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10793 = getelementptr inbounds i64, i64* %cloptr10792, i64 0                 ; &cloptr10792[0]
  %f10795 = load i64, i64* %i0ptr10793, align 8                                      ; load; *i0ptr10793
  %fptr10794 = inttoptr i64 %f10795 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10794(i64 %cont7711, i64 %arg8433, i64 %arg8432)    ; tail call
  ret void
}


define void @lam8837(i64 %env8838, i64 %XxJ$args7713) {
  %envptr10796 = inttoptr i64 %env8838 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10797 = getelementptr inbounds i64, i64* %envptr10796, i64 4                ; &envptr10796[4]
  %rqD$_37foldr1 = load i64, i64* %envptr10797, align 8                              ; load; *envptr10797
  %envptr10798 = inttoptr i64 %env8838 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10799 = getelementptr inbounds i64, i64* %envptr10798, i64 3                ; &envptr10798[3]
  %POA$_37foldr = load i64, i64* %envptr10799, align 8                               ; load; *envptr10799
  %envptr10800 = inttoptr i64 %env8838 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10801 = getelementptr inbounds i64, i64* %envptr10800, i64 2                ; &envptr10800[2]
  %Fod$_37foldl = load i64, i64* %envptr10801, align 8                               ; load; *envptr10801
  %envptr10802 = inttoptr i64 %env8838 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10803 = getelementptr inbounds i64, i64* %envptr10802, i64 1                ; &envptr10802[1]
  %umA$_37map1 = load i64, i64* %envptr10803, align 8                                ; load; *envptr10803
  %cont7712 = call i64 @prim_car(i64 %XxJ$args7713)                                  ; call prim_car
  %XxJ$args = call i64 @prim_cdr(i64 %XxJ$args7713)                                  ; call prim_cdr
  %WuA$f = call i64 @prim_car(i64 %XxJ$args)                                         ; call prim_car
  %a7495 = call i64 @prim_cdr(i64 %XxJ$args)                                         ; call prim_cdr
  %retprim7732 = call i64 @prim_car(i64 %a7495)                                      ; call prim_car
  %cloptr10804 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr10806 = getelementptr inbounds i64, i64* %cloptr10804, i64 1                  ; &eptr10806[1]
  %eptr10807 = getelementptr inbounds i64, i64* %cloptr10804, i64 2                  ; &eptr10807[2]
  %eptr10808 = getelementptr inbounds i64, i64* %cloptr10804, i64 3                  ; &eptr10808[3]
  %eptr10809 = getelementptr inbounds i64, i64* %cloptr10804, i64 4                  ; &eptr10809[4]
  %eptr10810 = getelementptr inbounds i64, i64* %cloptr10804, i64 5                  ; &eptr10810[5]
  %eptr10811 = getelementptr inbounds i64, i64* %cloptr10804, i64 6                  ; &eptr10811[6]
  %eptr10812 = getelementptr inbounds i64, i64* %cloptr10804, i64 7                  ; &eptr10812[7]
  store i64 %umA$_37map1, i64* %eptr10806                                            ; *eptr10806 = %umA$_37map1
  store i64 %WuA$f, i64* %eptr10807                                                  ; *eptr10807 = %WuA$f
  store i64 %XxJ$args, i64* %eptr10808                                               ; *eptr10808 = %XxJ$args
  store i64 %cont7712, i64* %eptr10809                                               ; *eptr10809 = %cont7712
  store i64 %Fod$_37foldl, i64* %eptr10810                                           ; *eptr10810 = %Fod$_37foldl
  store i64 %POA$_37foldr, i64* %eptr10811                                           ; *eptr10811 = %POA$_37foldr
  store i64 %rqD$_37foldr1, i64* %eptr10812                                          ; *eptr10812 = %rqD$_37foldr1
  %eptr10805 = getelementptr inbounds i64, i64* %cloptr10804, i64 0                  ; &cloptr10804[0]
  %f10813 = ptrtoint void(i64,i64,i64)* @lam8835 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10813, i64* %eptr10805                                                 ; store fptr
  %arg8442 = ptrtoint i64* %cloptr10804 to i64                                       ; closure cast; i64* -> i64
  %arg8441 = add i64 0, 0                                                            ; quoted ()
  %cloptr10814 = inttoptr i64 %arg8442 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10815 = getelementptr inbounds i64, i64* %cloptr10814, i64 0                 ; &cloptr10814[0]
  %f10817 = load i64, i64* %i0ptr10815, align 8                                      ; load; *i0ptr10815
  %fptr10816 = inttoptr i64 %f10817 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10816(i64 %arg8442, i64 %arg8441, i64 %retprim7732) ; tail call
  ret void
}


define void @lam8835(i64 %env8836, i64 %_957714, i64 %kok$acc) {
  %envptr10818 = inttoptr i64 %env8836 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10819 = getelementptr inbounds i64, i64* %envptr10818, i64 7                ; &envptr10818[7]
  %rqD$_37foldr1 = load i64, i64* %envptr10819, align 8                              ; load; *envptr10819
  %envptr10820 = inttoptr i64 %env8836 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10821 = getelementptr inbounds i64, i64* %envptr10820, i64 6                ; &envptr10820[6]
  %POA$_37foldr = load i64, i64* %envptr10821, align 8                               ; load; *envptr10821
  %envptr10822 = inttoptr i64 %env8836 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10823 = getelementptr inbounds i64, i64* %envptr10822, i64 5                ; &envptr10822[5]
  %Fod$_37foldl = load i64, i64* %envptr10823, align 8                               ; load; *envptr10823
  %envptr10824 = inttoptr i64 %env8836 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10825 = getelementptr inbounds i64, i64* %envptr10824, i64 4                ; &envptr10824[4]
  %cont7712 = load i64, i64* %envptr10825, align 8                                   ; load; *envptr10825
  %envptr10826 = inttoptr i64 %env8836 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10827 = getelementptr inbounds i64, i64* %envptr10826, i64 3                ; &envptr10826[3]
  %XxJ$args = load i64, i64* %envptr10827, align 8                                   ; load; *envptr10827
  %envptr10828 = inttoptr i64 %env8836 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10829 = getelementptr inbounds i64, i64* %envptr10828, i64 2                ; &envptr10828[2]
  %WuA$f = load i64, i64* %envptr10829, align 8                                      ; load; *envptr10829
  %envptr10830 = inttoptr i64 %env8836 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10831 = getelementptr inbounds i64, i64* %envptr10830, i64 1                ; &envptr10830[1]
  %umA$_37map1 = load i64, i64* %envptr10831, align 8                                ; load; *envptr10831
  %a7496 = call i64 @prim_cdr(i64 %XxJ$args)                                         ; call prim_cdr
  %retprim7731 = call i64 @prim_cdr(i64 %a7496)                                      ; call prim_cdr
  %cloptr10832 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr10834 = getelementptr inbounds i64, i64* %cloptr10832, i64 1                  ; &eptr10834[1]
  %eptr10835 = getelementptr inbounds i64, i64* %cloptr10832, i64 2                  ; &eptr10835[2]
  %eptr10836 = getelementptr inbounds i64, i64* %cloptr10832, i64 3                  ; &eptr10836[3]
  %eptr10837 = getelementptr inbounds i64, i64* %cloptr10832, i64 4                  ; &eptr10837[4]
  %eptr10838 = getelementptr inbounds i64, i64* %cloptr10832, i64 5                  ; &eptr10838[5]
  %eptr10839 = getelementptr inbounds i64, i64* %cloptr10832, i64 6                  ; &eptr10839[6]
  %eptr10840 = getelementptr inbounds i64, i64* %cloptr10832, i64 7                  ; &eptr10840[7]
  store i64 %umA$_37map1, i64* %eptr10834                                            ; *eptr10834 = %umA$_37map1
  store i64 %WuA$f, i64* %eptr10835                                                  ; *eptr10835 = %WuA$f
  store i64 %kok$acc, i64* %eptr10836                                                ; *eptr10836 = %kok$acc
  store i64 %cont7712, i64* %eptr10837                                               ; *eptr10837 = %cont7712
  store i64 %Fod$_37foldl, i64* %eptr10838                                           ; *eptr10838 = %Fod$_37foldl
  store i64 %POA$_37foldr, i64* %eptr10839                                           ; *eptr10839 = %POA$_37foldr
  store i64 %rqD$_37foldr1, i64* %eptr10840                                          ; *eptr10840 = %rqD$_37foldr1
  %eptr10833 = getelementptr inbounds i64, i64* %cloptr10832, i64 0                  ; &cloptr10832[0]
  %f10841 = ptrtoint void(i64,i64,i64)* @lam8833 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10841, i64* %eptr10833                                                 ; store fptr
  %arg8447 = ptrtoint i64* %cloptr10832 to i64                                       ; closure cast; i64* -> i64
  %arg8446 = add i64 0, 0                                                            ; quoted ()
  %cloptr10842 = inttoptr i64 %arg8447 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr10843 = getelementptr inbounds i64, i64* %cloptr10842, i64 0                 ; &cloptr10842[0]
  %f10845 = load i64, i64* %i0ptr10843, align 8                                      ; load; *i0ptr10843
  %fptr10844 = inttoptr i64 %f10845 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10844(i64 %arg8447, i64 %arg8446, i64 %retprim7731) ; tail call
  ret void
}


define void @lam8833(i64 %env8834, i64 %_957715, i64 %XrE$lsts) {
  %envptr10846 = inttoptr i64 %env8834 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10847 = getelementptr inbounds i64, i64* %envptr10846, i64 7                ; &envptr10846[7]
  %rqD$_37foldr1 = load i64, i64* %envptr10847, align 8                              ; load; *envptr10847
  %envptr10848 = inttoptr i64 %env8834 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10849 = getelementptr inbounds i64, i64* %envptr10848, i64 6                ; &envptr10848[6]
  %POA$_37foldr = load i64, i64* %envptr10849, align 8                               ; load; *envptr10849
  %envptr10850 = inttoptr i64 %env8834 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10851 = getelementptr inbounds i64, i64* %envptr10850, i64 5                ; &envptr10850[5]
  %Fod$_37foldl = load i64, i64* %envptr10851, align 8                               ; load; *envptr10851
  %envptr10852 = inttoptr i64 %env8834 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10853 = getelementptr inbounds i64, i64* %envptr10852, i64 4                ; &envptr10852[4]
  %cont7712 = load i64, i64* %envptr10853, align 8                                   ; load; *envptr10853
  %envptr10854 = inttoptr i64 %env8834 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10855 = getelementptr inbounds i64, i64* %envptr10854, i64 3                ; &envptr10854[3]
  %kok$acc = load i64, i64* %envptr10855, align 8                                    ; load; *envptr10855
  %envptr10856 = inttoptr i64 %env8834 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10857 = getelementptr inbounds i64, i64* %envptr10856, i64 2                ; &envptr10856[2]
  %WuA$f = load i64, i64* %envptr10857, align 8                                      ; load; *envptr10857
  %envptr10858 = inttoptr i64 %env8834 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10859 = getelementptr inbounds i64, i64* %envptr10858, i64 1                ; &envptr10858[1]
  %umA$_37map1 = load i64, i64* %envptr10859, align 8                                ; load; *envptr10859
  %cloptr10860 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr10862 = getelementptr inbounds i64, i64* %cloptr10860, i64 1                  ; &eptr10862[1]
  %eptr10863 = getelementptr inbounds i64, i64* %cloptr10860, i64 2                  ; &eptr10863[2]
  %eptr10864 = getelementptr inbounds i64, i64* %cloptr10860, i64 3                  ; &eptr10864[3]
  %eptr10865 = getelementptr inbounds i64, i64* %cloptr10860, i64 4                  ; &eptr10865[4]
  %eptr10866 = getelementptr inbounds i64, i64* %cloptr10860, i64 5                  ; &eptr10866[5]
  %eptr10867 = getelementptr inbounds i64, i64* %cloptr10860, i64 6                  ; &eptr10867[6]
  %eptr10868 = getelementptr inbounds i64, i64* %cloptr10860, i64 7                  ; &eptr10868[7]
  store i64 %umA$_37map1, i64* %eptr10862                                            ; *eptr10862 = %umA$_37map1
  store i64 %WuA$f, i64* %eptr10863                                                  ; *eptr10863 = %WuA$f
  store i64 %kok$acc, i64* %eptr10864                                                ; *eptr10864 = %kok$acc
  store i64 %cont7712, i64* %eptr10865                                               ; *eptr10865 = %cont7712
  store i64 %Fod$_37foldl, i64* %eptr10866                                           ; *eptr10866 = %Fod$_37foldl
  store i64 %POA$_37foldr, i64* %eptr10867                                           ; *eptr10867 = %POA$_37foldr
  store i64 %XrE$lsts, i64* %eptr10868                                               ; *eptr10868 = %XrE$lsts
  %eptr10861 = getelementptr inbounds i64, i64* %cloptr10860, i64 0                  ; &cloptr10860[0]
  %f10869 = ptrtoint void(i64,i64,i64)* @lam8831 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10869, i64* %eptr10861                                                 ; store fptr
  %arg8451 = ptrtoint i64* %cloptr10860 to i64                                       ; closure cast; i64* -> i64
  %cloptr10870 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10871 = getelementptr inbounds i64, i64* %cloptr10870, i64 0                  ; &cloptr10870[0]
  %f10872 = ptrtoint void(i64,i64,i64,i64)* @lam8810 to i64                          ; fptr cast; i64(...)* -> i64
  store i64 %f10872, i64* %eptr10871                                                 ; store fptr
  %arg8450 = ptrtoint i64* %cloptr10870 to i64                                       ; closure cast; i64* -> i64
  %arg8449 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr10873 = inttoptr i64 %rqD$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr10874 = getelementptr inbounds i64, i64* %cloptr10873, i64 0                 ; &cloptr10873[0]
  %f10876 = load i64, i64* %i0ptr10874, align 8                                      ; load; *i0ptr10874
  %fptr10875 = inttoptr i64 %f10876 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10875(i64 %rqD$_37foldr1, i64 %arg8451, i64 %arg8450, i64 %arg8449, i64 %XrE$lsts); tail call
  ret void
}


define void @lam8831(i64 %env8832, i64 %_957716, i64 %a7497) {
  %envptr10877 = inttoptr i64 %env8832 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10878 = getelementptr inbounds i64, i64* %envptr10877, i64 7                ; &envptr10877[7]
  %XrE$lsts = load i64, i64* %envptr10878, align 8                                   ; load; *envptr10878
  %envptr10879 = inttoptr i64 %env8832 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10880 = getelementptr inbounds i64, i64* %envptr10879, i64 6                ; &envptr10879[6]
  %POA$_37foldr = load i64, i64* %envptr10880, align 8                               ; load; *envptr10880
  %envptr10881 = inttoptr i64 %env8832 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10882 = getelementptr inbounds i64, i64* %envptr10881, i64 5                ; &envptr10881[5]
  %Fod$_37foldl = load i64, i64* %envptr10882, align 8                               ; load; *envptr10882
  %envptr10883 = inttoptr i64 %env8832 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10884 = getelementptr inbounds i64, i64* %envptr10883, i64 4                ; &envptr10883[4]
  %cont7712 = load i64, i64* %envptr10884, align 8                                   ; load; *envptr10884
  %envptr10885 = inttoptr i64 %env8832 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10886 = getelementptr inbounds i64, i64* %envptr10885, i64 3                ; &envptr10885[3]
  %kok$acc = load i64, i64* %envptr10886, align 8                                    ; load; *envptr10886
  %envptr10887 = inttoptr i64 %env8832 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10888 = getelementptr inbounds i64, i64* %envptr10887, i64 2                ; &envptr10887[2]
  %WuA$f = load i64, i64* %envptr10888, align 8                                      ; load; *envptr10888
  %envptr10889 = inttoptr i64 %env8832 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10890 = getelementptr inbounds i64, i64* %envptr10889, i64 1                ; &envptr10889[1]
  %umA$_37map1 = load i64, i64* %envptr10890, align 8                                ; load; *envptr10890
  %cmp10891 = icmp eq i64 %a7497, 15                                                 ; false?
  br i1 %cmp10891, label %else10893, label %then10892                                ; if

then10892:
  %arg8454 = add i64 0, 0                                                            ; quoted ()
  %cloptr10894 = inttoptr i64 %cont7712 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr10895 = getelementptr inbounds i64, i64* %cloptr10894, i64 0                 ; &cloptr10894[0]
  %f10897 = load i64, i64* %i0ptr10895, align 8                                      ; load; *i0ptr10895
  %fptr10896 = inttoptr i64 %f10897 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10896(i64 %cont7712, i64 %arg8454, i64 %kok$acc)    ; tail call
  ret void

else10893:
  %cloptr10898 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr10900 = getelementptr inbounds i64, i64* %cloptr10898, i64 1                  ; &eptr10900[1]
  %eptr10901 = getelementptr inbounds i64, i64* %cloptr10898, i64 2                  ; &eptr10901[2]
  %eptr10902 = getelementptr inbounds i64, i64* %cloptr10898, i64 3                  ; &eptr10902[3]
  %eptr10903 = getelementptr inbounds i64, i64* %cloptr10898, i64 4                  ; &eptr10903[4]
  %eptr10904 = getelementptr inbounds i64, i64* %cloptr10898, i64 5                  ; &eptr10904[5]
  %eptr10905 = getelementptr inbounds i64, i64* %cloptr10898, i64 6                  ; &eptr10905[6]
  %eptr10906 = getelementptr inbounds i64, i64* %cloptr10898, i64 7                  ; &eptr10906[7]
  store i64 %umA$_37map1, i64* %eptr10900                                            ; *eptr10900 = %umA$_37map1
  store i64 %WuA$f, i64* %eptr10901                                                  ; *eptr10901 = %WuA$f
  store i64 %kok$acc, i64* %eptr10902                                                ; *eptr10902 = %kok$acc
  store i64 %cont7712, i64* %eptr10903                                               ; *eptr10903 = %cont7712
  store i64 %Fod$_37foldl, i64* %eptr10904                                           ; *eptr10904 = %Fod$_37foldl
  store i64 %POA$_37foldr, i64* %eptr10905                                           ; *eptr10905 = %POA$_37foldr
  store i64 %XrE$lsts, i64* %eptr10906                                               ; *eptr10906 = %XrE$lsts
  %eptr10899 = getelementptr inbounds i64, i64* %cloptr10898, i64 0                  ; &cloptr10898[0]
  %f10907 = ptrtoint void(i64,i64,i64)* @lam8829 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10907, i64* %eptr10899                                                 ; store fptr
  %arg8458 = ptrtoint i64* %cloptr10898 to i64                                       ; closure cast; i64* -> i64
  %cloptr10908 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10909 = getelementptr inbounds i64, i64* %cloptr10908, i64 0                  ; &cloptr10908[0]
  %f10910 = ptrtoint void(i64,i64,i64)* @lam8814 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10910, i64* %eptr10909                                                 ; store fptr
  %arg8457 = ptrtoint i64* %cloptr10908 to i64                                       ; closure cast; i64* -> i64
  %cloptr10911 = inttoptr i64 %umA$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr10912 = getelementptr inbounds i64, i64* %cloptr10911, i64 0                 ; &cloptr10911[0]
  %f10914 = load i64, i64* %i0ptr10912, align 8                                      ; load; *i0ptr10912
  %fptr10913 = inttoptr i64 %f10914 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10913(i64 %umA$_37map1, i64 %arg8458, i64 %arg8457, i64 %XrE$lsts); tail call
  ret void
}


define void @lam8829(i64 %env8830, i64 %_957717, i64 %ssy$lsts_43) {
  %envptr10915 = inttoptr i64 %env8830 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10916 = getelementptr inbounds i64, i64* %envptr10915, i64 7                ; &envptr10915[7]
  %XrE$lsts = load i64, i64* %envptr10916, align 8                                   ; load; *envptr10916
  %envptr10917 = inttoptr i64 %env8830 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10918 = getelementptr inbounds i64, i64* %envptr10917, i64 6                ; &envptr10917[6]
  %POA$_37foldr = load i64, i64* %envptr10918, align 8                               ; load; *envptr10918
  %envptr10919 = inttoptr i64 %env8830 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10920 = getelementptr inbounds i64, i64* %envptr10919, i64 5                ; &envptr10919[5]
  %Fod$_37foldl = load i64, i64* %envptr10920, align 8                               ; load; *envptr10920
  %envptr10921 = inttoptr i64 %env8830 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10922 = getelementptr inbounds i64, i64* %envptr10921, i64 4                ; &envptr10921[4]
  %cont7712 = load i64, i64* %envptr10922, align 8                                   ; load; *envptr10922
  %envptr10923 = inttoptr i64 %env8830 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10924 = getelementptr inbounds i64, i64* %envptr10923, i64 3                ; &envptr10923[3]
  %kok$acc = load i64, i64* %envptr10924, align 8                                    ; load; *envptr10924
  %envptr10925 = inttoptr i64 %env8830 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10926 = getelementptr inbounds i64, i64* %envptr10925, i64 2                ; &envptr10925[2]
  %WuA$f = load i64, i64* %envptr10926, align 8                                      ; load; *envptr10926
  %envptr10927 = inttoptr i64 %env8830 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10928 = getelementptr inbounds i64, i64* %envptr10927, i64 1                ; &envptr10927[1]
  %umA$_37map1 = load i64, i64* %envptr10928, align 8                                ; load; *envptr10928
  %cloptr10929 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr10931 = getelementptr inbounds i64, i64* %cloptr10929, i64 1                  ; &eptr10931[1]
  %eptr10932 = getelementptr inbounds i64, i64* %cloptr10929, i64 2                  ; &eptr10932[2]
  %eptr10933 = getelementptr inbounds i64, i64* %cloptr10929, i64 3                  ; &eptr10933[3]
  %eptr10934 = getelementptr inbounds i64, i64* %cloptr10929, i64 4                  ; &eptr10934[4]
  %eptr10935 = getelementptr inbounds i64, i64* %cloptr10929, i64 5                  ; &eptr10935[5]
  %eptr10936 = getelementptr inbounds i64, i64* %cloptr10929, i64 6                  ; &eptr10936[6]
  store i64 %WuA$f, i64* %eptr10931                                                  ; *eptr10931 = %WuA$f
  store i64 %ssy$lsts_43, i64* %eptr10932                                            ; *eptr10932 = %ssy$lsts_43
  store i64 %kok$acc, i64* %eptr10933                                                ; *eptr10933 = %kok$acc
  store i64 %cont7712, i64* %eptr10934                                               ; *eptr10934 = %cont7712
  store i64 %Fod$_37foldl, i64* %eptr10935                                           ; *eptr10935 = %Fod$_37foldl
  store i64 %POA$_37foldr, i64* %eptr10936                                           ; *eptr10936 = %POA$_37foldr
  %eptr10930 = getelementptr inbounds i64, i64* %cloptr10929, i64 0                  ; &cloptr10929[0]
  %f10937 = ptrtoint void(i64,i64,i64)* @lam8827 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10937, i64* %eptr10930                                                 ; store fptr
  %arg8462 = ptrtoint i64* %cloptr10929 to i64                                       ; closure cast; i64* -> i64
  %cloptr10938 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10939 = getelementptr inbounds i64, i64* %cloptr10938, i64 0                  ; &cloptr10938[0]
  %f10940 = ptrtoint void(i64,i64,i64)* @lam8817 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10940, i64* %eptr10939                                                 ; store fptr
  %arg8461 = ptrtoint i64* %cloptr10938 to i64                                       ; closure cast; i64* -> i64
  %cloptr10941 = inttoptr i64 %umA$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr10942 = getelementptr inbounds i64, i64* %cloptr10941, i64 0                 ; &cloptr10941[0]
  %f10944 = load i64, i64* %i0ptr10942, align 8                                      ; load; *i0ptr10942
  %fptr10943 = inttoptr i64 %f10944 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10943(i64 %umA$_37map1, i64 %arg8462, i64 %arg8461, i64 %XrE$lsts); tail call
  ret void
}


define void @lam8827(i64 %env8828, i64 %_957718, i64 %JNX$vs) {
  %envptr10945 = inttoptr i64 %env8828 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10946 = getelementptr inbounds i64, i64* %envptr10945, i64 6                ; &envptr10945[6]
  %POA$_37foldr = load i64, i64* %envptr10946, align 8                               ; load; *envptr10946
  %envptr10947 = inttoptr i64 %env8828 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10948 = getelementptr inbounds i64, i64* %envptr10947, i64 5                ; &envptr10947[5]
  %Fod$_37foldl = load i64, i64* %envptr10948, align 8                               ; load; *envptr10948
  %envptr10949 = inttoptr i64 %env8828 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10950 = getelementptr inbounds i64, i64* %envptr10949, i64 4                ; &envptr10949[4]
  %cont7712 = load i64, i64* %envptr10950, align 8                                   ; load; *envptr10950
  %envptr10951 = inttoptr i64 %env8828 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10952 = getelementptr inbounds i64, i64* %envptr10951, i64 3                ; &envptr10951[3]
  %kok$acc = load i64, i64* %envptr10952, align 8                                    ; load; *envptr10952
  %envptr10953 = inttoptr i64 %env8828 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10954 = getelementptr inbounds i64, i64* %envptr10953, i64 2                ; &envptr10953[2]
  %ssy$lsts_43 = load i64, i64* %envptr10954, align 8                                ; load; *envptr10954
  %envptr10955 = inttoptr i64 %env8828 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10956 = getelementptr inbounds i64, i64* %envptr10955, i64 1                ; &envptr10955[1]
  %WuA$f = load i64, i64* %envptr10956, align 8                                      ; load; *envptr10956
  %arg8464 = add i64 0, 0                                                            ; quoted ()
  %a7498 = call i64 @prim_cons(i64 %kok$acc, i64 %arg8464)                           ; call prim_cons
  %cloptr10957 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr10959 = getelementptr inbounds i64, i64* %cloptr10957, i64 1                  ; &eptr10959[1]
  %eptr10960 = getelementptr inbounds i64, i64* %cloptr10957, i64 2                  ; &eptr10960[2]
  %eptr10961 = getelementptr inbounds i64, i64* %cloptr10957, i64 3                  ; &eptr10961[3]
  %eptr10962 = getelementptr inbounds i64, i64* %cloptr10957, i64 4                  ; &eptr10962[4]
  store i64 %WuA$f, i64* %eptr10959                                                  ; *eptr10959 = %WuA$f
  store i64 %ssy$lsts_43, i64* %eptr10960                                            ; *eptr10960 = %ssy$lsts_43
  store i64 %cont7712, i64* %eptr10961                                               ; *eptr10961 = %cont7712
  store i64 %Fod$_37foldl, i64* %eptr10962                                           ; *eptr10962 = %Fod$_37foldl
  %eptr10958 = getelementptr inbounds i64, i64* %cloptr10957, i64 0                  ; &cloptr10957[0]
  %f10963 = ptrtoint void(i64,i64,i64)* @lam8824 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10963, i64* %eptr10958                                                 ; store fptr
  %arg8469 = ptrtoint i64* %cloptr10957 to i64                                       ; closure cast; i64* -> i64
  %cloptr10964 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr10965 = getelementptr inbounds i64, i64* %cloptr10964, i64 0                  ; &cloptr10964[0]
  %f10966 = ptrtoint void(i64,i64,i64,i64)* @lam8820 to i64                          ; fptr cast; i64(...)* -> i64
  store i64 %f10966, i64* %eptr10965                                                 ; store fptr
  %arg8468 = ptrtoint i64* %cloptr10964 to i64                                       ; closure cast; i64* -> i64
  %cloptr10967 = inttoptr i64 %POA$_37foldr to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr10968 = getelementptr inbounds i64, i64* %cloptr10967, i64 0                 ; &cloptr10967[0]
  %f10970 = load i64, i64* %i0ptr10968, align 8                                      ; load; *i0ptr10968
  %fptr10969 = inttoptr i64 %f10970 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10969(i64 %POA$_37foldr, i64 %arg8469, i64 %arg8468, i64 %a7498, i64 %JNX$vs); tail call
  ret void
}


define void @lam8824(i64 %env8825, i64 %_957721, i64 %a7499) {
  %envptr10971 = inttoptr i64 %env8825 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10972 = getelementptr inbounds i64, i64* %envptr10971, i64 4                ; &envptr10971[4]
  %Fod$_37foldl = load i64, i64* %envptr10972, align 8                               ; load; *envptr10972
  %envptr10973 = inttoptr i64 %env8825 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10974 = getelementptr inbounds i64, i64* %envptr10973, i64 3                ; &envptr10973[3]
  %cont7712 = load i64, i64* %envptr10974, align 8                                   ; load; *envptr10974
  %envptr10975 = inttoptr i64 %env8825 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10976 = getelementptr inbounds i64, i64* %envptr10975, i64 2                ; &envptr10975[2]
  %ssy$lsts_43 = load i64, i64* %envptr10976, align 8                                ; load; *envptr10976
  %envptr10977 = inttoptr i64 %env8825 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10978 = getelementptr inbounds i64, i64* %envptr10977, i64 1                ; &envptr10977[1]
  %WuA$f = load i64, i64* %envptr10978, align 8                                      ; load; *envptr10978
  %cloptr10979 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr10981 = getelementptr inbounds i64, i64* %cloptr10979, i64 1                  ; &eptr10981[1]
  %eptr10982 = getelementptr inbounds i64, i64* %cloptr10979, i64 2                  ; &eptr10982[2]
  %eptr10983 = getelementptr inbounds i64, i64* %cloptr10979, i64 3                  ; &eptr10983[3]
  %eptr10984 = getelementptr inbounds i64, i64* %cloptr10979, i64 4                  ; &eptr10984[4]
  store i64 %WuA$f, i64* %eptr10981                                                  ; *eptr10981 = %WuA$f
  store i64 %ssy$lsts_43, i64* %eptr10982                                            ; *eptr10982 = %ssy$lsts_43
  store i64 %cont7712, i64* %eptr10983                                               ; *eptr10983 = %cont7712
  store i64 %Fod$_37foldl, i64* %eptr10984                                           ; *eptr10984 = %Fod$_37foldl
  %eptr10980 = getelementptr inbounds i64, i64* %cloptr10979, i64 0                  ; &cloptr10979[0]
  %f10985 = ptrtoint void(i64,i64,i64)* @lam8822 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f10985, i64* %eptr10980                                                 ; store fptr
  %arg8472 = ptrtoint i64* %cloptr10979 to i64                                       ; closure cast; i64* -> i64
  %cps_45lst7722 = call i64 @prim_cons(i64 %arg8472, i64 %a7499)                     ; call prim_cons
  %cloptr10986 = inttoptr i64 %WuA$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr10987 = getelementptr inbounds i64, i64* %cloptr10986, i64 0                 ; &cloptr10986[0]
  %f10989 = load i64, i64* %i0ptr10987, align 8                                      ; load; *i0ptr10987
  %fptr10988 = inttoptr i64 %f10989 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr10988(i64 %WuA$f, i64 %cps_45lst7722)               ; tail call
  ret void
}


define void @lam8822(i64 %env8823, i64 %_957719, i64 %J4l$acc_43) {
  %envptr10990 = inttoptr i64 %env8823 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10991 = getelementptr inbounds i64, i64* %envptr10990, i64 4                ; &envptr10990[4]
  %Fod$_37foldl = load i64, i64* %envptr10991, align 8                               ; load; *envptr10991
  %envptr10992 = inttoptr i64 %env8823 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10993 = getelementptr inbounds i64, i64* %envptr10992, i64 3                ; &envptr10992[3]
  %cont7712 = load i64, i64* %envptr10993, align 8                                   ; load; *envptr10993
  %envptr10994 = inttoptr i64 %env8823 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10995 = getelementptr inbounds i64, i64* %envptr10994, i64 2                ; &envptr10994[2]
  %ssy$lsts_43 = load i64, i64* %envptr10995, align 8                                ; load; *envptr10995
  %envptr10996 = inttoptr i64 %env8823 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr10997 = getelementptr inbounds i64, i64* %envptr10996, i64 1                ; &envptr10996[1]
  %WuA$f = load i64, i64* %envptr10997, align 8                                      ; load; *envptr10997
  %a7500 = call i64 @prim_cons(i64 %J4l$acc_43, i64 %ssy$lsts_43)                    ; call prim_cons
  %a7501 = call i64 @prim_cons(i64 %WuA$f, i64 %a7500)                               ; call prim_cons
  %cps_45lst7720 = call i64 @prim_cons(i64 %cont7712, i64 %a7501)                    ; call prim_cons
  %cloptr10998 = inttoptr i64 %Fod$_37foldl to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr10999 = getelementptr inbounds i64, i64* %cloptr10998, i64 0                 ; &cloptr10998[0]
  %f11001 = load i64, i64* %i0ptr10999, align 8                                      ; load; *i0ptr10999
  %fptr11000 = inttoptr i64 %f11001 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11000(i64 %Fod$_37foldl, i64 %cps_45lst7720)        ; tail call
  ret void
}


define void @lam8820(i64 %env8821, i64 %cont7723, i64 %zga$a, i64 %bu3$b) {
  %retprim7724 = call i64 @prim_cons(i64 %zga$a, i64 %bu3$b)                         ; call prim_cons
  %arg8482 = add i64 0, 0                                                            ; quoted ()
  %cloptr11002 = inttoptr i64 %cont7723 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11003 = getelementptr inbounds i64, i64* %cloptr11002, i64 0                 ; &cloptr11002[0]
  %f11005 = load i64, i64* %i0ptr11003, align 8                                      ; load; *i0ptr11003
  %fptr11004 = inttoptr i64 %f11005 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11004(i64 %cont7723, i64 %arg8482, i64 %retprim7724); tail call
  ret void
}


define void @lam8817(i64 %env8818, i64 %cont7725, i64 %udx$x) {
  %retprim7726 = call i64 @prim_car(i64 %udx$x)                                      ; call prim_car
  %arg8486 = add i64 0, 0                                                            ; quoted ()
  %cloptr11006 = inttoptr i64 %cont7725 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11007 = getelementptr inbounds i64, i64* %cloptr11006, i64 0                 ; &cloptr11006[0]
  %f11009 = load i64, i64* %i0ptr11007, align 8                                      ; load; *i0ptr11007
  %fptr11008 = inttoptr i64 %f11009 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11008(i64 %cont7725, i64 %arg8486, i64 %retprim7726); tail call
  ret void
}


define void @lam8814(i64 %env8815, i64 %cont7727, i64 %YlT$x) {
  %retprim7728 = call i64 @prim_cdr(i64 %YlT$x)                                      ; call prim_cdr
  %arg8490 = add i64 0, 0                                                            ; quoted ()
  %cloptr11010 = inttoptr i64 %cont7727 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11011 = getelementptr inbounds i64, i64* %cloptr11010, i64 0                 ; &cloptr11010[0]
  %f11013 = load i64, i64* %i0ptr11011, align 8                                      ; load; *i0ptr11011
  %fptr11012 = inttoptr i64 %f11013 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11012(i64 %cont7727, i64 %arg8490, i64 %retprim7728); tail call
  ret void
}


define void @lam8810(i64 %env8811, i64 %cont7729, i64 %wtu$lst, i64 %vMb$b) {
  %cmp11014 = icmp eq i64 %vMb$b, 15                                                 ; false?
  br i1 %cmp11014, label %else11016, label %then11015                                ; if

then11015:
  %arg8493 = add i64 0, 0                                                            ; quoted ()
  %cloptr11017 = inttoptr i64 %cont7729 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11018 = getelementptr inbounds i64, i64* %cloptr11017, i64 0                 ; &cloptr11017[0]
  %f11020 = load i64, i64* %i0ptr11018, align 8                                      ; load; *i0ptr11018
  %fptr11019 = inttoptr i64 %f11020 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11019(i64 %cont7729, i64 %arg8493, i64 %vMb$b)      ; tail call
  ret void

else11016:
  %retprim7730 = call i64 @prim_null_63(i64 %wtu$lst)                                ; call prim_null_63
  %arg8497 = add i64 0, 0                                                            ; quoted ()
  %cloptr11021 = inttoptr i64 %cont7729 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11022 = getelementptr inbounds i64, i64* %cloptr11021, i64 0                 ; &cloptr11021[0]
  %f11024 = load i64, i64* %i0ptr11022, align 8                                      ; load; *i0ptr11022
  %fptr11023 = inttoptr i64 %f11024 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11023(i64 %cont7729, i64 %arg8497, i64 %retprim7730); tail call
  ret void
}


define void @lam8803(i64 %env8804, i64 %cont7733, i64 %iBw$_37foldr) {
  %envptr11025 = inttoptr i64 %env8804 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11026 = getelementptr inbounds i64, i64* %envptr11025, i64 2                ; &envptr11025[2]
  %rqD$_37foldr1 = load i64, i64* %envptr11026, align 8                              ; load; *envptr11026
  %envptr11027 = inttoptr i64 %env8804 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11028 = getelementptr inbounds i64, i64* %envptr11027, i64 1                ; &envptr11027[1]
  %J7R$_37map1 = load i64, i64* %envptr11028, align 8                                ; load; *envptr11028
  %arg8500 = add i64 0, 0                                                            ; quoted ()
  %cloptr11029 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11031 = getelementptr inbounds i64, i64* %cloptr11029, i64 1                  ; &eptr11031[1]
  %eptr11032 = getelementptr inbounds i64, i64* %cloptr11029, i64 2                  ; &eptr11032[2]
  %eptr11033 = getelementptr inbounds i64, i64* %cloptr11029, i64 3                  ; &eptr11033[3]
  store i64 %J7R$_37map1, i64* %eptr11031                                            ; *eptr11031 = %J7R$_37map1
  store i64 %iBw$_37foldr, i64* %eptr11032                                           ; *eptr11032 = %iBw$_37foldr
  store i64 %rqD$_37foldr1, i64* %eptr11033                                          ; *eptr11033 = %rqD$_37foldr1
  %eptr11030 = getelementptr inbounds i64, i64* %cloptr11029, i64 0                  ; &cloptr11029[0]
  %f11034 = ptrtoint void(i64,i64)* @lam8800 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f11034, i64* %eptr11030                                                 ; store fptr
  %arg8499 = ptrtoint i64* %cloptr11029 to i64                                       ; closure cast; i64* -> i64
  %cloptr11035 = inttoptr i64 %cont7733 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11036 = getelementptr inbounds i64, i64* %cloptr11035, i64 0                 ; &cloptr11035[0]
  %f11038 = load i64, i64* %i0ptr11036, align 8                                      ; load; *i0ptr11036
  %fptr11037 = inttoptr i64 %f11038 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11037(i64 %cont7733, i64 %arg8500, i64 %arg8499)    ; tail call
  ret void
}


define void @lam8800(i64 %env8801, i64 %sTm$args7735) {
  %envptr11039 = inttoptr i64 %env8801 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11040 = getelementptr inbounds i64, i64* %envptr11039, i64 3                ; &envptr11039[3]
  %rqD$_37foldr1 = load i64, i64* %envptr11040, align 8                              ; load; *envptr11040
  %envptr11041 = inttoptr i64 %env8801 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11042 = getelementptr inbounds i64, i64* %envptr11041, i64 2                ; &envptr11041[2]
  %iBw$_37foldr = load i64, i64* %envptr11042, align 8                               ; load; *envptr11042
  %envptr11043 = inttoptr i64 %env8801 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11044 = getelementptr inbounds i64, i64* %envptr11043, i64 1                ; &envptr11043[1]
  %J7R$_37map1 = load i64, i64* %envptr11044, align 8                                ; load; *envptr11044
  %cont7734 = call i64 @prim_car(i64 %sTm$args7735)                                  ; call prim_car
  %sTm$args = call i64 @prim_cdr(i64 %sTm$args7735)                                  ; call prim_cdr
  %vY8$f = call i64 @prim_car(i64 %sTm$args)                                         ; call prim_car
  %a7481 = call i64 @prim_cdr(i64 %sTm$args)                                         ; call prim_cdr
  %retprim7754 = call i64 @prim_car(i64 %a7481)                                      ; call prim_car
  %cloptr11045 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr11047 = getelementptr inbounds i64, i64* %cloptr11045, i64 1                  ; &eptr11047[1]
  %eptr11048 = getelementptr inbounds i64, i64* %cloptr11045, i64 2                  ; &eptr11048[2]
  %eptr11049 = getelementptr inbounds i64, i64* %cloptr11045, i64 3                  ; &eptr11049[3]
  %eptr11050 = getelementptr inbounds i64, i64* %cloptr11045, i64 4                  ; &eptr11050[4]
  %eptr11051 = getelementptr inbounds i64, i64* %cloptr11045, i64 5                  ; &eptr11051[5]
  %eptr11052 = getelementptr inbounds i64, i64* %cloptr11045, i64 6                  ; &eptr11052[6]
  store i64 %J7R$_37map1, i64* %eptr11047                                            ; *eptr11047 = %J7R$_37map1
  store i64 %sTm$args, i64* %eptr11048                                               ; *eptr11048 = %sTm$args
  store i64 %iBw$_37foldr, i64* %eptr11049                                           ; *eptr11049 = %iBw$_37foldr
  store i64 %vY8$f, i64* %eptr11050                                                  ; *eptr11050 = %vY8$f
  store i64 %rqD$_37foldr1, i64* %eptr11051                                          ; *eptr11051 = %rqD$_37foldr1
  store i64 %cont7734, i64* %eptr11052                                               ; *eptr11052 = %cont7734
  %eptr11046 = getelementptr inbounds i64, i64* %cloptr11045, i64 0                  ; &cloptr11045[0]
  %f11053 = ptrtoint void(i64,i64,i64)* @lam8798 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11053, i64* %eptr11046                                                 ; store fptr
  %arg8509 = ptrtoint i64* %cloptr11045 to i64                                       ; closure cast; i64* -> i64
  %arg8508 = add i64 0, 0                                                            ; quoted ()
  %cloptr11054 = inttoptr i64 %arg8509 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11055 = getelementptr inbounds i64, i64* %cloptr11054, i64 0                 ; &cloptr11054[0]
  %f11057 = load i64, i64* %i0ptr11055, align 8                                      ; load; *i0ptr11055
  %fptr11056 = inttoptr i64 %f11057 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11056(i64 %arg8509, i64 %arg8508, i64 %retprim7754) ; tail call
  ret void
}


define void @lam8798(i64 %env8799, i64 %_957736, i64 %cHg$acc) {
  %envptr11058 = inttoptr i64 %env8799 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11059 = getelementptr inbounds i64, i64* %envptr11058, i64 6                ; &envptr11058[6]
  %cont7734 = load i64, i64* %envptr11059, align 8                                   ; load; *envptr11059
  %envptr11060 = inttoptr i64 %env8799 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11061 = getelementptr inbounds i64, i64* %envptr11060, i64 5                ; &envptr11060[5]
  %rqD$_37foldr1 = load i64, i64* %envptr11061, align 8                              ; load; *envptr11061
  %envptr11062 = inttoptr i64 %env8799 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11063 = getelementptr inbounds i64, i64* %envptr11062, i64 4                ; &envptr11062[4]
  %vY8$f = load i64, i64* %envptr11063, align 8                                      ; load; *envptr11063
  %envptr11064 = inttoptr i64 %env8799 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11065 = getelementptr inbounds i64, i64* %envptr11064, i64 3                ; &envptr11064[3]
  %iBw$_37foldr = load i64, i64* %envptr11065, align 8                               ; load; *envptr11065
  %envptr11066 = inttoptr i64 %env8799 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11067 = getelementptr inbounds i64, i64* %envptr11066, i64 2                ; &envptr11066[2]
  %sTm$args = load i64, i64* %envptr11067, align 8                                   ; load; *envptr11067
  %envptr11068 = inttoptr i64 %env8799 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11069 = getelementptr inbounds i64, i64* %envptr11068, i64 1                ; &envptr11068[1]
  %J7R$_37map1 = load i64, i64* %envptr11069, align 8                                ; load; *envptr11069
  %a7482 = call i64 @prim_cdr(i64 %sTm$args)                                         ; call prim_cdr
  %retprim7753 = call i64 @prim_cdr(i64 %a7482)                                      ; call prim_cdr
  %cloptr11070 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr11072 = getelementptr inbounds i64, i64* %cloptr11070, i64 1                  ; &eptr11072[1]
  %eptr11073 = getelementptr inbounds i64, i64* %cloptr11070, i64 2                  ; &eptr11073[2]
  %eptr11074 = getelementptr inbounds i64, i64* %cloptr11070, i64 3                  ; &eptr11074[3]
  %eptr11075 = getelementptr inbounds i64, i64* %cloptr11070, i64 4                  ; &eptr11075[4]
  %eptr11076 = getelementptr inbounds i64, i64* %cloptr11070, i64 5                  ; &eptr11076[5]
  %eptr11077 = getelementptr inbounds i64, i64* %cloptr11070, i64 6                  ; &eptr11077[6]
  store i64 %J7R$_37map1, i64* %eptr11072                                            ; *eptr11072 = %J7R$_37map1
  store i64 %cHg$acc, i64* %eptr11073                                                ; *eptr11073 = %cHg$acc
  store i64 %iBw$_37foldr, i64* %eptr11074                                           ; *eptr11074 = %iBw$_37foldr
  store i64 %vY8$f, i64* %eptr11075                                                  ; *eptr11075 = %vY8$f
  store i64 %rqD$_37foldr1, i64* %eptr11076                                          ; *eptr11076 = %rqD$_37foldr1
  store i64 %cont7734, i64* %eptr11077                                               ; *eptr11077 = %cont7734
  %eptr11071 = getelementptr inbounds i64, i64* %cloptr11070, i64 0                  ; &cloptr11070[0]
  %f11078 = ptrtoint void(i64,i64,i64)* @lam8796 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11078, i64* %eptr11071                                                 ; store fptr
  %arg8514 = ptrtoint i64* %cloptr11070 to i64                                       ; closure cast; i64* -> i64
  %arg8513 = add i64 0, 0                                                            ; quoted ()
  %cloptr11079 = inttoptr i64 %arg8514 to i64*                                       ; closure/env cast; i64 -> i64*
  %i0ptr11080 = getelementptr inbounds i64, i64* %cloptr11079, i64 0                 ; &cloptr11079[0]
  %f11082 = load i64, i64* %i0ptr11080, align 8                                      ; load; *i0ptr11080
  %fptr11081 = inttoptr i64 %f11082 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11081(i64 %arg8514, i64 %arg8513, i64 %retprim7753) ; tail call
  ret void
}


define void @lam8796(i64 %env8797, i64 %_957737, i64 %Mrs$lsts) {
  %envptr11083 = inttoptr i64 %env8797 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11084 = getelementptr inbounds i64, i64* %envptr11083, i64 6                ; &envptr11083[6]
  %cont7734 = load i64, i64* %envptr11084, align 8                                   ; load; *envptr11084
  %envptr11085 = inttoptr i64 %env8797 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11086 = getelementptr inbounds i64, i64* %envptr11085, i64 5                ; &envptr11085[5]
  %rqD$_37foldr1 = load i64, i64* %envptr11086, align 8                              ; load; *envptr11086
  %envptr11087 = inttoptr i64 %env8797 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11088 = getelementptr inbounds i64, i64* %envptr11087, i64 4                ; &envptr11087[4]
  %vY8$f = load i64, i64* %envptr11088, align 8                                      ; load; *envptr11088
  %envptr11089 = inttoptr i64 %env8797 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11090 = getelementptr inbounds i64, i64* %envptr11089, i64 3                ; &envptr11089[3]
  %iBw$_37foldr = load i64, i64* %envptr11090, align 8                               ; load; *envptr11090
  %envptr11091 = inttoptr i64 %env8797 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11092 = getelementptr inbounds i64, i64* %envptr11091, i64 2                ; &envptr11091[2]
  %cHg$acc = load i64, i64* %envptr11092, align 8                                    ; load; *envptr11092
  %envptr11093 = inttoptr i64 %env8797 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11094 = getelementptr inbounds i64, i64* %envptr11093, i64 1                ; &envptr11093[1]
  %J7R$_37map1 = load i64, i64* %envptr11094, align 8                                ; load; *envptr11094
  %cloptr11095 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr11097 = getelementptr inbounds i64, i64* %cloptr11095, i64 1                  ; &eptr11097[1]
  %eptr11098 = getelementptr inbounds i64, i64* %cloptr11095, i64 2                  ; &eptr11098[2]
  %eptr11099 = getelementptr inbounds i64, i64* %cloptr11095, i64 3                  ; &eptr11099[3]
  %eptr11100 = getelementptr inbounds i64, i64* %cloptr11095, i64 4                  ; &eptr11100[4]
  %eptr11101 = getelementptr inbounds i64, i64* %cloptr11095, i64 5                  ; &eptr11101[5]
  %eptr11102 = getelementptr inbounds i64, i64* %cloptr11095, i64 6                  ; &eptr11102[6]
  %eptr11103 = getelementptr inbounds i64, i64* %cloptr11095, i64 7                  ; &eptr11103[7]
  store i64 %J7R$_37map1, i64* %eptr11097                                            ; *eptr11097 = %J7R$_37map1
  store i64 %Mrs$lsts, i64* %eptr11098                                               ; *eptr11098 = %Mrs$lsts
  store i64 %cHg$acc, i64* %eptr11099                                                ; *eptr11099 = %cHg$acc
  store i64 %iBw$_37foldr, i64* %eptr11100                                           ; *eptr11100 = %iBw$_37foldr
  store i64 %vY8$f, i64* %eptr11101                                                  ; *eptr11101 = %vY8$f
  store i64 %rqD$_37foldr1, i64* %eptr11102                                          ; *eptr11102 = %rqD$_37foldr1
  store i64 %cont7734, i64* %eptr11103                                               ; *eptr11103 = %cont7734
  %eptr11096 = getelementptr inbounds i64, i64* %cloptr11095, i64 0                  ; &cloptr11095[0]
  %f11104 = ptrtoint void(i64,i64,i64)* @lam8794 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11104, i64* %eptr11096                                                 ; store fptr
  %arg8518 = ptrtoint i64* %cloptr11095 to i64                                       ; closure cast; i64* -> i64
  %cloptr11105 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11106 = getelementptr inbounds i64, i64* %cloptr11105, i64 0                  ; &cloptr11105[0]
  %f11107 = ptrtoint void(i64,i64,i64,i64)* @lam8773 to i64                          ; fptr cast; i64(...)* -> i64
  store i64 %f11107, i64* %eptr11106                                                 ; store fptr
  %arg8517 = ptrtoint i64* %cloptr11105 to i64                                       ; closure cast; i64* -> i64
  %arg8516 = call i64 @const_init_false()                                            ; quoted #f
  %cloptr11108 = inttoptr i64 %rqD$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11109 = getelementptr inbounds i64, i64* %cloptr11108, i64 0                 ; &cloptr11108[0]
  %f11111 = load i64, i64* %i0ptr11109, align 8                                      ; load; *i0ptr11109
  %fptr11110 = inttoptr i64 %f11111 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11110(i64 %rqD$_37foldr1, i64 %arg8518, i64 %arg8517, i64 %arg8516, i64 %Mrs$lsts); tail call
  ret void
}


define void @lam8794(i64 %env8795, i64 %_957738, i64 %a7483) {
  %envptr11112 = inttoptr i64 %env8795 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11113 = getelementptr inbounds i64, i64* %envptr11112, i64 7                ; &envptr11112[7]
  %cont7734 = load i64, i64* %envptr11113, align 8                                   ; load; *envptr11113
  %envptr11114 = inttoptr i64 %env8795 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11115 = getelementptr inbounds i64, i64* %envptr11114, i64 6                ; &envptr11114[6]
  %rqD$_37foldr1 = load i64, i64* %envptr11115, align 8                              ; load; *envptr11115
  %envptr11116 = inttoptr i64 %env8795 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11117 = getelementptr inbounds i64, i64* %envptr11116, i64 5                ; &envptr11116[5]
  %vY8$f = load i64, i64* %envptr11117, align 8                                      ; load; *envptr11117
  %envptr11118 = inttoptr i64 %env8795 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11119 = getelementptr inbounds i64, i64* %envptr11118, i64 4                ; &envptr11118[4]
  %iBw$_37foldr = load i64, i64* %envptr11119, align 8                               ; load; *envptr11119
  %envptr11120 = inttoptr i64 %env8795 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11121 = getelementptr inbounds i64, i64* %envptr11120, i64 3                ; &envptr11120[3]
  %cHg$acc = load i64, i64* %envptr11121, align 8                                    ; load; *envptr11121
  %envptr11122 = inttoptr i64 %env8795 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11123 = getelementptr inbounds i64, i64* %envptr11122, i64 2                ; &envptr11122[2]
  %Mrs$lsts = load i64, i64* %envptr11123, align 8                                   ; load; *envptr11123
  %envptr11124 = inttoptr i64 %env8795 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11125 = getelementptr inbounds i64, i64* %envptr11124, i64 1                ; &envptr11124[1]
  %J7R$_37map1 = load i64, i64* %envptr11125, align 8                                ; load; *envptr11125
  %cmp11126 = icmp eq i64 %a7483, 15                                                 ; false?
  br i1 %cmp11126, label %else11128, label %then11127                                ; if

then11127:
  %arg8521 = add i64 0, 0                                                            ; quoted ()
  %cloptr11129 = inttoptr i64 %cont7734 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11130 = getelementptr inbounds i64, i64* %cloptr11129, i64 0                 ; &cloptr11129[0]
  %f11132 = load i64, i64* %i0ptr11130, align 8                                      ; load; *i0ptr11130
  %fptr11131 = inttoptr i64 %f11132 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11131(i64 %cont7734, i64 %arg8521, i64 %cHg$acc)    ; tail call
  ret void

else11128:
  %cloptr11133 = call i64* @alloc(i64 64)                                            ; malloc
  %eptr11135 = getelementptr inbounds i64, i64* %cloptr11133, i64 1                  ; &eptr11135[1]
  %eptr11136 = getelementptr inbounds i64, i64* %cloptr11133, i64 2                  ; &eptr11136[2]
  %eptr11137 = getelementptr inbounds i64, i64* %cloptr11133, i64 3                  ; &eptr11137[3]
  %eptr11138 = getelementptr inbounds i64, i64* %cloptr11133, i64 4                  ; &eptr11138[4]
  %eptr11139 = getelementptr inbounds i64, i64* %cloptr11133, i64 5                  ; &eptr11139[5]
  %eptr11140 = getelementptr inbounds i64, i64* %cloptr11133, i64 6                  ; &eptr11140[6]
  %eptr11141 = getelementptr inbounds i64, i64* %cloptr11133, i64 7                  ; &eptr11141[7]
  store i64 %J7R$_37map1, i64* %eptr11135                                            ; *eptr11135 = %J7R$_37map1
  store i64 %Mrs$lsts, i64* %eptr11136                                               ; *eptr11136 = %Mrs$lsts
  store i64 %cHg$acc, i64* %eptr11137                                                ; *eptr11137 = %cHg$acc
  store i64 %iBw$_37foldr, i64* %eptr11138                                           ; *eptr11138 = %iBw$_37foldr
  store i64 %vY8$f, i64* %eptr11139                                                  ; *eptr11139 = %vY8$f
  store i64 %rqD$_37foldr1, i64* %eptr11140                                          ; *eptr11140 = %rqD$_37foldr1
  store i64 %cont7734, i64* %eptr11141                                               ; *eptr11141 = %cont7734
  %eptr11134 = getelementptr inbounds i64, i64* %cloptr11133, i64 0                  ; &cloptr11133[0]
  %f11142 = ptrtoint void(i64,i64,i64)* @lam8792 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11142, i64* %eptr11134                                                 ; store fptr
  %arg8525 = ptrtoint i64* %cloptr11133 to i64                                       ; closure cast; i64* -> i64
  %cloptr11143 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11144 = getelementptr inbounds i64, i64* %cloptr11143, i64 0                  ; &cloptr11143[0]
  %f11145 = ptrtoint void(i64,i64,i64)* @lam8777 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11145, i64* %eptr11144                                                 ; store fptr
  %arg8524 = ptrtoint i64* %cloptr11143 to i64                                       ; closure cast; i64* -> i64
  %cloptr11146 = inttoptr i64 %J7R$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr11147 = getelementptr inbounds i64, i64* %cloptr11146, i64 0                 ; &cloptr11146[0]
  %f11149 = load i64, i64* %i0ptr11147, align 8                                      ; load; *i0ptr11147
  %fptr11148 = inttoptr i64 %f11149 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11148(i64 %J7R$_37map1, i64 %arg8525, i64 %arg8524, i64 %Mrs$lsts); tail call
  ret void
}


define void @lam8792(i64 %env8793, i64 %_957739, i64 %NA8$lsts_43) {
  %envptr11150 = inttoptr i64 %env8793 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11151 = getelementptr inbounds i64, i64* %envptr11150, i64 7                ; &envptr11150[7]
  %cont7734 = load i64, i64* %envptr11151, align 8                                   ; load; *envptr11151
  %envptr11152 = inttoptr i64 %env8793 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11153 = getelementptr inbounds i64, i64* %envptr11152, i64 6                ; &envptr11152[6]
  %rqD$_37foldr1 = load i64, i64* %envptr11153, align 8                              ; load; *envptr11153
  %envptr11154 = inttoptr i64 %env8793 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11155 = getelementptr inbounds i64, i64* %envptr11154, i64 5                ; &envptr11154[5]
  %vY8$f = load i64, i64* %envptr11155, align 8                                      ; load; *envptr11155
  %envptr11156 = inttoptr i64 %env8793 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11157 = getelementptr inbounds i64, i64* %envptr11156, i64 4                ; &envptr11156[4]
  %iBw$_37foldr = load i64, i64* %envptr11157, align 8                               ; load; *envptr11157
  %envptr11158 = inttoptr i64 %env8793 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11159 = getelementptr inbounds i64, i64* %envptr11158, i64 3                ; &envptr11158[3]
  %cHg$acc = load i64, i64* %envptr11159, align 8                                    ; load; *envptr11159
  %envptr11160 = inttoptr i64 %env8793 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11161 = getelementptr inbounds i64, i64* %envptr11160, i64 2                ; &envptr11160[2]
  %Mrs$lsts = load i64, i64* %envptr11161, align 8                                   ; load; *envptr11161
  %envptr11162 = inttoptr i64 %env8793 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11163 = getelementptr inbounds i64, i64* %envptr11162, i64 1                ; &envptr11162[1]
  %J7R$_37map1 = load i64, i64* %envptr11163, align 8                                ; load; *envptr11163
  %cloptr11164 = call i64* @alloc(i64 56)                                            ; malloc
  %eptr11166 = getelementptr inbounds i64, i64* %cloptr11164, i64 1                  ; &eptr11166[1]
  %eptr11167 = getelementptr inbounds i64, i64* %cloptr11164, i64 2                  ; &eptr11167[2]
  %eptr11168 = getelementptr inbounds i64, i64* %cloptr11164, i64 3                  ; &eptr11168[3]
  %eptr11169 = getelementptr inbounds i64, i64* %cloptr11164, i64 4                  ; &eptr11169[4]
  %eptr11170 = getelementptr inbounds i64, i64* %cloptr11164, i64 5                  ; &eptr11170[5]
  %eptr11171 = getelementptr inbounds i64, i64* %cloptr11164, i64 6                  ; &eptr11171[6]
  store i64 %cHg$acc, i64* %eptr11166                                                ; *eptr11166 = %cHg$acc
  store i64 %NA8$lsts_43, i64* %eptr11167                                            ; *eptr11167 = %NA8$lsts_43
  store i64 %iBw$_37foldr, i64* %eptr11168                                           ; *eptr11168 = %iBw$_37foldr
  store i64 %vY8$f, i64* %eptr11169                                                  ; *eptr11169 = %vY8$f
  store i64 %rqD$_37foldr1, i64* %eptr11170                                          ; *eptr11170 = %rqD$_37foldr1
  store i64 %cont7734, i64* %eptr11171                                               ; *eptr11171 = %cont7734
  %eptr11165 = getelementptr inbounds i64, i64* %cloptr11164, i64 0                  ; &cloptr11164[0]
  %f11172 = ptrtoint void(i64,i64,i64)* @lam8790 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11172, i64* %eptr11165                                                 ; store fptr
  %arg8529 = ptrtoint i64* %cloptr11164 to i64                                       ; closure cast; i64* -> i64
  %cloptr11173 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11174 = getelementptr inbounds i64, i64* %cloptr11173, i64 0                  ; &cloptr11173[0]
  %f11175 = ptrtoint void(i64,i64,i64)* @lam8780 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11175, i64* %eptr11174                                                 ; store fptr
  %arg8528 = ptrtoint i64* %cloptr11173 to i64                                       ; closure cast; i64* -> i64
  %cloptr11176 = inttoptr i64 %J7R$_37map1 to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr11177 = getelementptr inbounds i64, i64* %cloptr11176, i64 0                 ; &cloptr11176[0]
  %f11179 = load i64, i64* %i0ptr11177, align 8                                      ; load; *i0ptr11177
  %fptr11178 = inttoptr i64 %f11179 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11178(i64 %J7R$_37map1, i64 %arg8529, i64 %arg8528, i64 %Mrs$lsts); tail call
  ret void
}


define void @lam8790(i64 %env8791, i64 %_957740, i64 %HOI$vs) {
  %envptr11180 = inttoptr i64 %env8791 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11181 = getelementptr inbounds i64, i64* %envptr11180, i64 6                ; &envptr11180[6]
  %cont7734 = load i64, i64* %envptr11181, align 8                                   ; load; *envptr11181
  %envptr11182 = inttoptr i64 %env8791 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11183 = getelementptr inbounds i64, i64* %envptr11182, i64 5                ; &envptr11182[5]
  %rqD$_37foldr1 = load i64, i64* %envptr11183, align 8                              ; load; *envptr11183
  %envptr11184 = inttoptr i64 %env8791 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11185 = getelementptr inbounds i64, i64* %envptr11184, i64 4                ; &envptr11184[4]
  %vY8$f = load i64, i64* %envptr11185, align 8                                      ; load; *envptr11185
  %envptr11186 = inttoptr i64 %env8791 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11187 = getelementptr inbounds i64, i64* %envptr11186, i64 3                ; &envptr11186[3]
  %iBw$_37foldr = load i64, i64* %envptr11187, align 8                               ; load; *envptr11187
  %envptr11188 = inttoptr i64 %env8791 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11189 = getelementptr inbounds i64, i64* %envptr11188, i64 2                ; &envptr11188[2]
  %NA8$lsts_43 = load i64, i64* %envptr11189, align 8                                ; load; *envptr11189
  %envptr11190 = inttoptr i64 %env8791 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11191 = getelementptr inbounds i64, i64* %envptr11190, i64 1                ; &envptr11190[1]
  %cHg$acc = load i64, i64* %envptr11191, align 8                                    ; load; *envptr11191
  %a7484 = call i64 @prim_cons(i64 %cHg$acc, i64 %NA8$lsts_43)                       ; call prim_cons
  %a7485 = call i64 @prim_cons(i64 %vY8$f, i64 %a7484)                               ; call prim_cons
  %cloptr11192 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr11194 = getelementptr inbounds i64, i64* %cloptr11192, i64 1                  ; &eptr11194[1]
  %eptr11195 = getelementptr inbounds i64, i64* %cloptr11192, i64 2                  ; &eptr11195[2]
  %eptr11196 = getelementptr inbounds i64, i64* %cloptr11192, i64 3                  ; &eptr11196[3]
  %eptr11197 = getelementptr inbounds i64, i64* %cloptr11192, i64 4                  ; &eptr11197[4]
  store i64 %vY8$f, i64* %eptr11194                                                  ; *eptr11194 = %vY8$f
  store i64 %rqD$_37foldr1, i64* %eptr11195                                          ; *eptr11195 = %rqD$_37foldr1
  store i64 %HOI$vs, i64* %eptr11196                                                 ; *eptr11196 = %HOI$vs
  store i64 %cont7734, i64* %eptr11197                                               ; *eptr11197 = %cont7734
  %eptr11193 = getelementptr inbounds i64, i64* %cloptr11192, i64 0                  ; &cloptr11192[0]
  %f11198 = ptrtoint void(i64,i64,i64)* @lam8788 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11198, i64* %eptr11193                                                 ; store fptr
  %arg8536 = ptrtoint i64* %cloptr11192 to i64                                       ; closure cast; i64* -> i64
  %cps_45lst7746 = call i64 @prim_cons(i64 %arg8536, i64 %a7485)                     ; call prim_cons
  %cloptr11199 = inttoptr i64 %iBw$_37foldr to i64*                                  ; closure/env cast; i64 -> i64*
  %i0ptr11200 = getelementptr inbounds i64, i64* %cloptr11199, i64 0                 ; &cloptr11199[0]
  %f11202 = load i64, i64* %i0ptr11200, align 8                                      ; load; *i0ptr11200
  %fptr11201 = inttoptr i64 %f11202 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11201(i64 %iBw$_37foldr, i64 %cps_45lst7746)        ; tail call
  ret void
}


define void @lam8788(i64 %env8789, i64 %_957741, i64 %a7486) {
  %envptr11203 = inttoptr i64 %env8789 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11204 = getelementptr inbounds i64, i64* %envptr11203, i64 4                ; &envptr11203[4]
  %cont7734 = load i64, i64* %envptr11204, align 8                                   ; load; *envptr11204
  %envptr11205 = inttoptr i64 %env8789 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11206 = getelementptr inbounds i64, i64* %envptr11205, i64 3                ; &envptr11205[3]
  %HOI$vs = load i64, i64* %envptr11206, align 8                                     ; load; *envptr11206
  %envptr11207 = inttoptr i64 %env8789 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11208 = getelementptr inbounds i64, i64* %envptr11207, i64 2                ; &envptr11207[2]
  %rqD$_37foldr1 = load i64, i64* %envptr11208, align 8                              ; load; *envptr11208
  %envptr11209 = inttoptr i64 %env8789 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11210 = getelementptr inbounds i64, i64* %envptr11209, i64 1                ; &envptr11209[1]
  %vY8$f = load i64, i64* %envptr11210, align 8                                      ; load; *envptr11210
  %arg8537 = add i64 0, 0                                                            ; quoted ()
  %a7487 = call i64 @prim_cons(i64 %a7486, i64 %arg8537)                             ; call prim_cons
  %cloptr11211 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11213 = getelementptr inbounds i64, i64* %cloptr11211, i64 1                  ; &eptr11213[1]
  %eptr11214 = getelementptr inbounds i64, i64* %cloptr11211, i64 2                  ; &eptr11214[2]
  store i64 %vY8$f, i64* %eptr11213                                                  ; *eptr11213 = %vY8$f
  store i64 %cont7734, i64* %eptr11214                                               ; *eptr11214 = %cont7734
  %eptr11212 = getelementptr inbounds i64, i64* %cloptr11211, i64 0                  ; &cloptr11211[0]
  %f11215 = ptrtoint void(i64,i64,i64)* @lam8785 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11215, i64* %eptr11212                                                 ; store fptr
  %arg8542 = ptrtoint i64* %cloptr11211 to i64                                       ; closure cast; i64* -> i64
  %cloptr11216 = call i64* @alloc(i64 8)                                             ; malloc
  %eptr11217 = getelementptr inbounds i64, i64* %cloptr11216, i64 0                  ; &cloptr11216[0]
  %f11218 = ptrtoint void(i64,i64,i64,i64)* @lam8783 to i64                          ; fptr cast; i64(...)* -> i64
  store i64 %f11218, i64* %eptr11217                                                 ; store fptr
  %arg8541 = ptrtoint i64* %cloptr11216 to i64                                       ; closure cast; i64* -> i64
  %cloptr11219 = inttoptr i64 %rqD$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11220 = getelementptr inbounds i64, i64* %cloptr11219, i64 0                 ; &cloptr11219[0]
  %f11222 = load i64, i64* %i0ptr11220, align 8                                      ; load; *i0ptr11220
  %fptr11221 = inttoptr i64 %f11222 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11221(i64 %rqD$_37foldr1, i64 %arg8542, i64 %arg8541, i64 %a7487, i64 %HOI$vs); tail call
  ret void
}


define void @lam8785(i64 %env8786, i64 %_957742, i64 %a7488) {
  %envptr11223 = inttoptr i64 %env8786 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11224 = getelementptr inbounds i64, i64* %envptr11223, i64 2                ; &envptr11223[2]
  %cont7734 = load i64, i64* %envptr11224, align 8                                   ; load; *envptr11224
  %envptr11225 = inttoptr i64 %env8786 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11226 = getelementptr inbounds i64, i64* %envptr11225, i64 1                ; &envptr11225[1]
  %vY8$f = load i64, i64* %envptr11226, align 8                                      ; load; *envptr11226
  %cps_45lst7743 = call i64 @prim_cons(i64 %cont7734, i64 %a7488)                    ; call prim_cons
  %cloptr11227 = inttoptr i64 %vY8$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11228 = getelementptr inbounds i64, i64* %cloptr11227, i64 0                 ; &cloptr11227[0]
  %f11230 = load i64, i64* %i0ptr11228, align 8                                      ; load; *i0ptr11228
  %fptr11229 = inttoptr i64 %f11230 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11229(i64 %vY8$f, i64 %cps_45lst7743)               ; tail call
  ret void
}


define void @lam8783(i64 %env8784, i64 %cont7744, i64 %EsV$a, i64 %bwR$b) {
  %retprim7745 = call i64 @prim_cons(i64 %EsV$a, i64 %bwR$b)                         ; call prim_cons
  %arg8549 = add i64 0, 0                                                            ; quoted ()
  %cloptr11231 = inttoptr i64 %cont7744 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11232 = getelementptr inbounds i64, i64* %cloptr11231, i64 0                 ; &cloptr11231[0]
  %f11234 = load i64, i64* %i0ptr11232, align 8                                      ; load; *i0ptr11232
  %fptr11233 = inttoptr i64 %f11234 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11233(i64 %cont7744, i64 %arg8549, i64 %retprim7745); tail call
  ret void
}


define void @lam8780(i64 %env8781, i64 %cont7747, i64 %atP$x) {
  %retprim7748 = call i64 @prim_car(i64 %atP$x)                                      ; call prim_car
  %arg8553 = add i64 0, 0                                                            ; quoted ()
  %cloptr11235 = inttoptr i64 %cont7747 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11236 = getelementptr inbounds i64, i64* %cloptr11235, i64 0                 ; &cloptr11235[0]
  %f11238 = load i64, i64* %i0ptr11236, align 8                                      ; load; *i0ptr11236
  %fptr11237 = inttoptr i64 %f11238 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11237(i64 %cont7747, i64 %arg8553, i64 %retprim7748); tail call
  ret void
}


define void @lam8777(i64 %env8778, i64 %cont7749, i64 %PLU$x) {
  %retprim7750 = call i64 @prim_cdr(i64 %PLU$x)                                      ; call prim_cdr
  %arg8557 = add i64 0, 0                                                            ; quoted ()
  %cloptr11239 = inttoptr i64 %cont7749 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11240 = getelementptr inbounds i64, i64* %cloptr11239, i64 0                 ; &cloptr11239[0]
  %f11242 = load i64, i64* %i0ptr11240, align 8                                      ; load; *i0ptr11240
  %fptr11241 = inttoptr i64 %f11242 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11241(i64 %cont7749, i64 %arg8557, i64 %retprim7750); tail call
  ret void
}


define void @lam8773(i64 %env8774, i64 %cont7751, i64 %VWz$lst, i64 %eWP$b) {
  %cmp11243 = icmp eq i64 %eWP$b, 15                                                 ; false?
  br i1 %cmp11243, label %else11245, label %then11244                                ; if

then11244:
  %arg8560 = add i64 0, 0                                                            ; quoted ()
  %cloptr11246 = inttoptr i64 %cont7751 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11247 = getelementptr inbounds i64, i64* %cloptr11246, i64 0                 ; &cloptr11246[0]
  %f11249 = load i64, i64* %i0ptr11247, align 8                                      ; load; *i0ptr11247
  %fptr11248 = inttoptr i64 %f11249 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11248(i64 %cont7751, i64 %arg8560, i64 %eWP$b)      ; tail call
  ret void

else11245:
  %retprim7752 = call i64 @prim_null_63(i64 %VWz$lst)                                ; call prim_null_63
  %arg8564 = add i64 0, 0                                                            ; quoted ()
  %cloptr11250 = inttoptr i64 %cont7751 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11251 = getelementptr inbounds i64, i64* %cloptr11250, i64 0                 ; &cloptr11250[0]
  %f11253 = load i64, i64* %i0ptr11251, align 8                                      ; load; *i0ptr11251
  %fptr11252 = inttoptr i64 %f11253 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11252(i64 %cont7751, i64 %arg8564, i64 %retprim7752); tail call
  ret void
}


define void @lam8766(i64 %env8767, i64 %cont7755, i64 %pTe$_37foldl1) {
  %arg8567 = add i64 0, 0                                                            ; quoted ()
  %cloptr11254 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11256 = getelementptr inbounds i64, i64* %cloptr11254, i64 1                  ; &eptr11256[1]
  store i64 %pTe$_37foldl1, i64* %eptr11256                                          ; *eptr11256 = %pTe$_37foldl1
  %eptr11255 = getelementptr inbounds i64, i64* %cloptr11254, i64 0                  ; &cloptr11254[0]
  %f11257 = ptrtoint void(i64,i64,i64,i64,i64)* @lam8763 to i64                      ; fptr cast; i64(...)* -> i64
  store i64 %f11257, i64* %eptr11255                                                 ; store fptr
  %arg8566 = ptrtoint i64* %cloptr11254 to i64                                       ; closure cast; i64* -> i64
  %cloptr11258 = inttoptr i64 %cont7755 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11259 = getelementptr inbounds i64, i64* %cloptr11258, i64 0                 ; &cloptr11258[0]
  %f11261 = load i64, i64* %i0ptr11259, align 8                                      ; load; *i0ptr11259
  %fptr11260 = inttoptr i64 %f11261 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11260(i64 %cont7755, i64 %arg8567, i64 %arg8566)    ; tail call
  ret void
}


define void @lam8763(i64 %env8764, i64 %cont7756, i64 %ezp$f, i64 %z3v$acc, i64 %gp4$lst) {
  %envptr11262 = inttoptr i64 %env8764 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11263 = getelementptr inbounds i64, i64* %envptr11262, i64 1                ; &envptr11262[1]
  %pTe$_37foldl1 = load i64, i64* %envptr11263, align 8                              ; load; *envptr11263
  %a7475 = call i64 @prim_null_63(i64 %gp4$lst)                                      ; call prim_null_63
  %cmp11264 = icmp eq i64 %a7475, 15                                                 ; false?
  br i1 %cmp11264, label %else11266, label %then11265                                ; if

then11265:
  %arg8571 = add i64 0, 0                                                            ; quoted ()
  %cloptr11267 = inttoptr i64 %cont7756 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11268 = getelementptr inbounds i64, i64* %cloptr11267, i64 0                 ; &cloptr11267[0]
  %f11270 = load i64, i64* %i0ptr11268, align 8                                      ; load; *i0ptr11268
  %fptr11269 = inttoptr i64 %f11270 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11269(i64 %cont7756, i64 %arg8571, i64 %z3v$acc)    ; tail call
  ret void

else11266:
  %a7476 = call i64 @prim_car(i64 %gp4$lst)                                          ; call prim_car
  %cloptr11271 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr11273 = getelementptr inbounds i64, i64* %cloptr11271, i64 1                  ; &eptr11273[1]
  %eptr11274 = getelementptr inbounds i64, i64* %cloptr11271, i64 2                  ; &eptr11274[2]
  %eptr11275 = getelementptr inbounds i64, i64* %cloptr11271, i64 3                  ; &eptr11275[3]
  %eptr11276 = getelementptr inbounds i64, i64* %cloptr11271, i64 4                  ; &eptr11276[4]
  store i64 %pTe$_37foldl1, i64* %eptr11273                                          ; *eptr11273 = %pTe$_37foldl1
  store i64 %gp4$lst, i64* %eptr11274                                                ; *eptr11274 = %gp4$lst
  store i64 %ezp$f, i64* %eptr11275                                                  ; *eptr11275 = %ezp$f
  store i64 %cont7756, i64* %eptr11276                                               ; *eptr11276 = %cont7756
  %eptr11272 = getelementptr inbounds i64, i64* %cloptr11271, i64 0                  ; &cloptr11271[0]
  %f11277 = ptrtoint void(i64,i64,i64)* @lam8761 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11277, i64* %eptr11272                                                 ; store fptr
  %arg8576 = ptrtoint i64* %cloptr11271 to i64                                       ; closure cast; i64* -> i64
  %cloptr11278 = inttoptr i64 %ezp$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11279 = getelementptr inbounds i64, i64* %cloptr11278, i64 0                 ; &cloptr11278[0]
  %f11281 = load i64, i64* %i0ptr11279, align 8                                      ; load; *i0ptr11279
  %fptr11280 = inttoptr i64 %f11281 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11280(i64 %ezp$f, i64 %arg8576, i64 %a7476, i64 %z3v$acc); tail call
  ret void
}


define void @lam8761(i64 %env8762, i64 %_957757, i64 %a7477) {
  %envptr11282 = inttoptr i64 %env8762 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11283 = getelementptr inbounds i64, i64* %envptr11282, i64 4                ; &envptr11282[4]
  %cont7756 = load i64, i64* %envptr11283, align 8                                   ; load; *envptr11283
  %envptr11284 = inttoptr i64 %env8762 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11285 = getelementptr inbounds i64, i64* %envptr11284, i64 3                ; &envptr11284[3]
  %ezp$f = load i64, i64* %envptr11285, align 8                                      ; load; *envptr11285
  %envptr11286 = inttoptr i64 %env8762 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11287 = getelementptr inbounds i64, i64* %envptr11286, i64 2                ; &envptr11286[2]
  %gp4$lst = load i64, i64* %envptr11287, align 8                                    ; load; *envptr11287
  %envptr11288 = inttoptr i64 %env8762 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11289 = getelementptr inbounds i64, i64* %envptr11288, i64 1                ; &envptr11288[1]
  %pTe$_37foldl1 = load i64, i64* %envptr11289, align 8                              ; load; *envptr11289
  %a7478 = call i64 @prim_cdr(i64 %gp4$lst)                                          ; call prim_cdr
  %cloptr11290 = inttoptr i64 %pTe$_37foldl1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11291 = getelementptr inbounds i64, i64* %cloptr11290, i64 0                 ; &cloptr11290[0]
  %f11293 = load i64, i64* %i0ptr11291, align 8                                      ; load; *i0ptr11291
  %fptr11292 = inttoptr i64 %f11293 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11292(i64 %pTe$_37foldl1, i64 %cont7756, i64 %ezp$f, i64 %a7477, i64 %a7478); tail call
  ret void
}


define void @lam8758(i64 %env8759, i64 %cont7758, i64 %MQy$_37length) {
  %arg8585 = add i64 0, 0                                                            ; quoted ()
  %cloptr11294 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11296 = getelementptr inbounds i64, i64* %cloptr11294, i64 1                  ; &eptr11296[1]
  store i64 %MQy$_37length, i64* %eptr11296                                          ; *eptr11296 = %MQy$_37length
  %eptr11295 = getelementptr inbounds i64, i64* %cloptr11294, i64 0                  ; &cloptr11294[0]
  %f11297 = ptrtoint void(i64,i64,i64)* @lam8755 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11297, i64* %eptr11295                                                 ; store fptr
  %arg8584 = ptrtoint i64* %cloptr11294 to i64                                       ; closure cast; i64* -> i64
  %cloptr11298 = inttoptr i64 %cont7758 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11299 = getelementptr inbounds i64, i64* %cloptr11298, i64 0                 ; &cloptr11298[0]
  %f11301 = load i64, i64* %i0ptr11299, align 8                                      ; load; *i0ptr11299
  %fptr11300 = inttoptr i64 %f11301 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11300(i64 %cont7758, i64 %arg8585, i64 %arg8584)    ; tail call
  ret void
}


define void @lam8755(i64 %env8756, i64 %cont7759, i64 %Yng$lst) {
  %envptr11302 = inttoptr i64 %env8756 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11303 = getelementptr inbounds i64, i64* %envptr11302, i64 1                ; &envptr11302[1]
  %MQy$_37length = load i64, i64* %envptr11303, align 8                              ; load; *envptr11303
  %a7472 = call i64 @prim_null_63(i64 %Yng$lst)                                      ; call prim_null_63
  %cmp11304 = icmp eq i64 %a7472, 15                                                 ; false?
  br i1 %cmp11304, label %else11306, label %then11305                                ; if

then11305:
  %arg8589 = add i64 0, 0                                                            ; quoted ()
  %arg8588 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %cloptr11307 = inttoptr i64 %cont7759 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11308 = getelementptr inbounds i64, i64* %cloptr11307, i64 0                 ; &cloptr11307[0]
  %f11310 = load i64, i64* %i0ptr11308, align 8                                      ; load; *i0ptr11308
  %fptr11309 = inttoptr i64 %f11310 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11309(i64 %cont7759, i64 %arg8589, i64 %arg8588)    ; tail call
  ret void

else11306:
  %a7473 = call i64 @prim_cdr(i64 %Yng$lst)                                          ; call prim_cdr
  %cloptr11311 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11313 = getelementptr inbounds i64, i64* %cloptr11311, i64 1                  ; &eptr11313[1]
  store i64 %cont7759, i64* %eptr11313                                               ; *eptr11313 = %cont7759
  %eptr11312 = getelementptr inbounds i64, i64* %cloptr11311, i64 0                  ; &cloptr11311[0]
  %f11314 = ptrtoint void(i64,i64,i64)* @lam8753 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11314, i64* %eptr11312                                                 ; store fptr
  %arg8593 = ptrtoint i64* %cloptr11311 to i64                                       ; closure cast; i64* -> i64
  %cloptr11315 = inttoptr i64 %MQy$_37length to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11316 = getelementptr inbounds i64, i64* %cloptr11315, i64 0                 ; &cloptr11315[0]
  %f11318 = load i64, i64* %i0ptr11316, align 8                                      ; load; *i0ptr11316
  %fptr11317 = inttoptr i64 %f11318 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11317(i64 %MQy$_37length, i64 %arg8593, i64 %a7473) ; tail call
  ret void
}


define void @lam8753(i64 %env8754, i64 %_957760, i64 %a7474) {
  %envptr11319 = inttoptr i64 %env8754 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11320 = getelementptr inbounds i64, i64* %envptr11319, i64 1                ; &envptr11319[1]
  %cont7759 = load i64, i64* %envptr11320, align 8                                   ; load; *envptr11320
  %arg8596 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %retprim7761 = call i64 @prim__43(i64 %arg8596, i64 %a7474)                        ; call prim__43
  %arg8598 = add i64 0, 0                                                            ; quoted ()
  %cloptr11321 = inttoptr i64 %cont7759 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11322 = getelementptr inbounds i64, i64* %cloptr11321, i64 0                 ; &cloptr11321[0]
  %f11324 = load i64, i64* %i0ptr11322, align 8                                      ; load; *i0ptr11322
  %fptr11323 = inttoptr i64 %f11324 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11323(i64 %cont7759, i64 %arg8598, i64 %retprim7761); tail call
  ret void
}


define void @lam8747(i64 %env8748, i64 %cont7762, i64 %uCZ$_37take) {
  %arg8601 = add i64 0, 0                                                            ; quoted ()
  %cloptr11325 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11327 = getelementptr inbounds i64, i64* %cloptr11325, i64 1                  ; &eptr11327[1]
  store i64 %uCZ$_37take, i64* %eptr11327                                            ; *eptr11327 = %uCZ$_37take
  %eptr11326 = getelementptr inbounds i64, i64* %cloptr11325, i64 0                  ; &cloptr11325[0]
  %f11328 = ptrtoint void(i64,i64,i64,i64)* @lam8744 to i64                          ; fptr cast; i64(...)* -> i64
  store i64 %f11328, i64* %eptr11326                                                 ; store fptr
  %arg8600 = ptrtoint i64* %cloptr11325 to i64                                       ; closure cast; i64* -> i64
  %cloptr11329 = inttoptr i64 %cont7762 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11330 = getelementptr inbounds i64, i64* %cloptr11329, i64 0                 ; &cloptr11329[0]
  %f11332 = load i64, i64* %i0ptr11330, align 8                                      ; load; *i0ptr11330
  %fptr11331 = inttoptr i64 %f11332 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11331(i64 %cont7762, i64 %arg8601, i64 %arg8600)    ; tail call
  ret void
}


define void @lam8744(i64 %env8745, i64 %cont7763, i64 %W8A$lst, i64 %GUn$n) {
  %envptr11333 = inttoptr i64 %env8745 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11334 = getelementptr inbounds i64, i64* %envptr11333, i64 1                ; &envptr11333[1]
  %uCZ$_37take = load i64, i64* %envptr11334, align 8                                ; load; *envptr11334
  %arg8603 = call i64 @const_init_int(i64 0)                                         ; quoted int
  %a7466 = call i64 @prim__61(i64 %GUn$n, i64 %arg8603)                              ; call prim__61
  %cmp11335 = icmp eq i64 %a7466, 15                                                 ; false?
  br i1 %cmp11335, label %else11337, label %then11336                                ; if

then11336:
  %arg8606 = add i64 0, 0                                                            ; quoted ()
  %arg8605 = add i64 0, 0                                                            ; quoted ()
  %cloptr11338 = inttoptr i64 %cont7763 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11339 = getelementptr inbounds i64, i64* %cloptr11338, i64 0                 ; &cloptr11338[0]
  %f11341 = load i64, i64* %i0ptr11339, align 8                                      ; load; *i0ptr11339
  %fptr11340 = inttoptr i64 %f11341 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11340(i64 %cont7763, i64 %arg8606, i64 %arg8605)    ; tail call
  ret void

else11337:
  %a7467 = call i64 @prim_null_63(i64 %W8A$lst)                                      ; call prim_null_63
  %cmp11342 = icmp eq i64 %a7467, 15                                                 ; false?
  br i1 %cmp11342, label %else11344, label %then11343                                ; if

then11343:
  %arg8610 = add i64 0, 0                                                            ; quoted ()
  %arg8609 = add i64 0, 0                                                            ; quoted ()
  %cloptr11345 = inttoptr i64 %cont7763 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11346 = getelementptr inbounds i64, i64* %cloptr11345, i64 0                 ; &cloptr11345[0]
  %f11348 = load i64, i64* %i0ptr11346, align 8                                      ; load; *i0ptr11346
  %fptr11347 = inttoptr i64 %f11348 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11347(i64 %cont7763, i64 %arg8610, i64 %arg8609)    ; tail call
  ret void

else11344:
  %a7468 = call i64 @prim_car(i64 %W8A$lst)                                          ; call prim_car
  %a7469 = call i64 @prim_cdr(i64 %W8A$lst)                                          ; call prim_cdr
  %arg8614 = call i64 @const_init_int(i64 1)                                         ; quoted int
  %a7470 = call i64 @prim__45(i64 %GUn$n, i64 %arg8614)                              ; call prim__45
  %cloptr11349 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11351 = getelementptr inbounds i64, i64* %cloptr11349, i64 1                  ; &eptr11351[1]
  %eptr11352 = getelementptr inbounds i64, i64* %cloptr11349, i64 2                  ; &eptr11352[2]
  store i64 %cont7763, i64* %eptr11351                                               ; *eptr11351 = %cont7763
  store i64 %a7468, i64* %eptr11352                                                  ; *eptr11352 = %a7468
  %eptr11350 = getelementptr inbounds i64, i64* %cloptr11349, i64 0                  ; &cloptr11349[0]
  %f11353 = ptrtoint void(i64,i64,i64)* @lam8740 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11353, i64* %eptr11350                                                 ; store fptr
  %arg8618 = ptrtoint i64* %cloptr11349 to i64                                       ; closure cast; i64* -> i64
  %cloptr11354 = inttoptr i64 %uCZ$_37take to i64*                                   ; closure/env cast; i64 -> i64*
  %i0ptr11355 = getelementptr inbounds i64, i64* %cloptr11354, i64 0                 ; &cloptr11354[0]
  %f11357 = load i64, i64* %i0ptr11355, align 8                                      ; load; *i0ptr11355
  %fptr11356 = inttoptr i64 %f11357 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11356(i64 %uCZ$_37take, i64 %arg8618, i64 %a7469, i64 %a7470); tail call
  ret void
}


define void @lam8740(i64 %env8741, i64 %_957764, i64 %a7471) {
  %envptr11358 = inttoptr i64 %env8741 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11359 = getelementptr inbounds i64, i64* %envptr11358, i64 2                ; &envptr11358[2]
  %a7468 = load i64, i64* %envptr11359, align 8                                      ; load; *envptr11359
  %envptr11360 = inttoptr i64 %env8741 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11361 = getelementptr inbounds i64, i64* %envptr11360, i64 1                ; &envptr11360[1]
  %cont7763 = load i64, i64* %envptr11361, align 8                                   ; load; *envptr11361
  %retprim7765 = call i64 @prim_cons(i64 %a7468, i64 %a7471)                         ; call prim_cons
  %arg8623 = add i64 0, 0                                                            ; quoted ()
  %cloptr11362 = inttoptr i64 %cont7763 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11363 = getelementptr inbounds i64, i64* %cloptr11362, i64 0                 ; &cloptr11362[0]
  %f11365 = load i64, i64* %i0ptr11363, align 8                                      ; load; *i0ptr11363
  %fptr11364 = inttoptr i64 %f11365 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11364(i64 %cont7763, i64 %arg8623, i64 %retprim7765); tail call
  ret void
}


define void @lam8733(i64 %env8734, i64 %cont7766, i64 %dlb$_37map) {
  %arg8626 = add i64 0, 0                                                            ; quoted ()
  %cloptr11366 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11368 = getelementptr inbounds i64, i64* %cloptr11366, i64 1                  ; &eptr11368[1]
  store i64 %dlb$_37map, i64* %eptr11368                                             ; *eptr11368 = %dlb$_37map
  %eptr11367 = getelementptr inbounds i64, i64* %cloptr11366, i64 0                  ; &cloptr11366[0]
  %f11369 = ptrtoint void(i64,i64,i64,i64)* @lam8730 to i64                          ; fptr cast; i64(...)* -> i64
  store i64 %f11369, i64* %eptr11367                                                 ; store fptr
  %arg8625 = ptrtoint i64* %cloptr11366 to i64                                       ; closure cast; i64* -> i64
  %cloptr11370 = inttoptr i64 %cont7766 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11371 = getelementptr inbounds i64, i64* %cloptr11370, i64 0                 ; &cloptr11370[0]
  %f11373 = load i64, i64* %i0ptr11371, align 8                                      ; load; *i0ptr11371
  %fptr11372 = inttoptr i64 %f11373 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11372(i64 %cont7766, i64 %arg8626, i64 %arg8625)    ; tail call
  ret void
}


define void @lam8730(i64 %env8731, i64 %cont7767, i64 %l3e$f, i64 %Yy4$lst) {
  %envptr11374 = inttoptr i64 %env8731 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11375 = getelementptr inbounds i64, i64* %envptr11374, i64 1                ; &envptr11374[1]
  %dlb$_37map = load i64, i64* %envptr11375, align 8                                 ; load; *envptr11375
  %a7461 = call i64 @prim_null_63(i64 %Yy4$lst)                                      ; call prim_null_63
  %cmp11376 = icmp eq i64 %a7461, 15                                                 ; false?
  br i1 %cmp11376, label %else11378, label %then11377                                ; if

then11377:
  %arg8630 = add i64 0, 0                                                            ; quoted ()
  %arg8629 = add i64 0, 0                                                            ; quoted ()
  %cloptr11379 = inttoptr i64 %cont7767 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11380 = getelementptr inbounds i64, i64* %cloptr11379, i64 0                 ; &cloptr11379[0]
  %f11382 = load i64, i64* %i0ptr11380, align 8                                      ; load; *i0ptr11380
  %fptr11381 = inttoptr i64 %f11382 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11381(i64 %cont7767, i64 %arg8630, i64 %arg8629)    ; tail call
  ret void

else11378:
  %a7462 = call i64 @prim_car(i64 %Yy4$lst)                                          ; call prim_car
  %cloptr11383 = call i64* @alloc(i64 40)                                            ; malloc
  %eptr11385 = getelementptr inbounds i64, i64* %cloptr11383, i64 1                  ; &eptr11385[1]
  %eptr11386 = getelementptr inbounds i64, i64* %cloptr11383, i64 2                  ; &eptr11386[2]
  %eptr11387 = getelementptr inbounds i64, i64* %cloptr11383, i64 3                  ; &eptr11387[3]
  %eptr11388 = getelementptr inbounds i64, i64* %cloptr11383, i64 4                  ; &eptr11388[4]
  store i64 %l3e$f, i64* %eptr11385                                                  ; *eptr11385 = %l3e$f
  store i64 %cont7767, i64* %eptr11386                                               ; *eptr11386 = %cont7767
  store i64 %dlb$_37map, i64* %eptr11387                                             ; *eptr11387 = %dlb$_37map
  store i64 %Yy4$lst, i64* %eptr11388                                                ; *eptr11388 = %Yy4$lst
  %eptr11384 = getelementptr inbounds i64, i64* %cloptr11383, i64 0                  ; &cloptr11383[0]
  %f11389 = ptrtoint void(i64,i64,i64)* @lam8728 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11389, i64* %eptr11384                                                 ; store fptr
  %arg8634 = ptrtoint i64* %cloptr11383 to i64                                       ; closure cast; i64* -> i64
  %cloptr11390 = inttoptr i64 %l3e$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11391 = getelementptr inbounds i64, i64* %cloptr11390, i64 0                 ; &cloptr11390[0]
  %f11393 = load i64, i64* %i0ptr11391, align 8                                      ; load; *i0ptr11391
  %fptr11392 = inttoptr i64 %f11393 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11392(i64 %l3e$f, i64 %arg8634, i64 %a7462)         ; tail call
  ret void
}


define void @lam8728(i64 %env8729, i64 %_957768, i64 %a7463) {
  %envptr11394 = inttoptr i64 %env8729 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11395 = getelementptr inbounds i64, i64* %envptr11394, i64 4                ; &envptr11394[4]
  %Yy4$lst = load i64, i64* %envptr11395, align 8                                    ; load; *envptr11395
  %envptr11396 = inttoptr i64 %env8729 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11397 = getelementptr inbounds i64, i64* %envptr11396, i64 3                ; &envptr11396[3]
  %dlb$_37map = load i64, i64* %envptr11397, align 8                                 ; load; *envptr11397
  %envptr11398 = inttoptr i64 %env8729 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11399 = getelementptr inbounds i64, i64* %envptr11398, i64 2                ; &envptr11398[2]
  %cont7767 = load i64, i64* %envptr11399, align 8                                   ; load; *envptr11399
  %envptr11400 = inttoptr i64 %env8729 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11401 = getelementptr inbounds i64, i64* %envptr11400, i64 1                ; &envptr11400[1]
  %l3e$f = load i64, i64* %envptr11401, align 8                                      ; load; *envptr11401
  %a7464 = call i64 @prim_cdr(i64 %Yy4$lst)                                          ; call prim_cdr
  %cloptr11402 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11404 = getelementptr inbounds i64, i64* %cloptr11402, i64 1                  ; &eptr11404[1]
  %eptr11405 = getelementptr inbounds i64, i64* %cloptr11402, i64 2                  ; &eptr11405[2]
  store i64 %cont7767, i64* %eptr11404                                               ; *eptr11404 = %cont7767
  store i64 %a7463, i64* %eptr11405                                                  ; *eptr11405 = %a7463
  %eptr11403 = getelementptr inbounds i64, i64* %cloptr11402, i64 0                  ; &cloptr11402[0]
  %f11406 = ptrtoint void(i64,i64,i64)* @lam8726 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11406, i64* %eptr11403                                                 ; store fptr
  %arg8639 = ptrtoint i64* %cloptr11402 to i64                                       ; closure cast; i64* -> i64
  %cloptr11407 = inttoptr i64 %dlb$_37map to i64*                                    ; closure/env cast; i64 -> i64*
  %i0ptr11408 = getelementptr inbounds i64, i64* %cloptr11407, i64 0                 ; &cloptr11407[0]
  %f11410 = load i64, i64* %i0ptr11408, align 8                                      ; load; *i0ptr11408
  %fptr11409 = inttoptr i64 %f11410 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11409(i64 %dlb$_37map, i64 %arg8639, i64 %l3e$f, i64 %a7464); tail call
  ret void
}


define void @lam8726(i64 %env8727, i64 %_957769, i64 %a7465) {
  %envptr11411 = inttoptr i64 %env8727 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11412 = getelementptr inbounds i64, i64* %envptr11411, i64 2                ; &envptr11411[2]
  %a7463 = load i64, i64* %envptr11412, align 8                                      ; load; *envptr11412
  %envptr11413 = inttoptr i64 %env8727 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11414 = getelementptr inbounds i64, i64* %envptr11413, i64 1                ; &envptr11413[1]
  %cont7767 = load i64, i64* %envptr11414, align 8                                   ; load; *envptr11414
  %retprim7770 = call i64 @prim_cons(i64 %a7463, i64 %a7465)                         ; call prim_cons
  %arg8644 = add i64 0, 0                                                            ; quoted ()
  %cloptr11415 = inttoptr i64 %cont7767 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11416 = getelementptr inbounds i64, i64* %cloptr11415, i64 0                 ; &cloptr11415[0]
  %f11418 = load i64, i64* %i0ptr11416, align 8                                      ; load; *i0ptr11416
  %fptr11417 = inttoptr i64 %f11418 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11417(i64 %cont7767, i64 %arg8644, i64 %retprim7770); tail call
  ret void
}


define void @lam8721(i64 %env8722, i64 %cont7771, i64 %Gfl$_37foldr1) {
  %arg8647 = add i64 0, 0                                                            ; quoted ()
  %cloptr11419 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11421 = getelementptr inbounds i64, i64* %cloptr11419, i64 1                  ; &eptr11421[1]
  store i64 %Gfl$_37foldr1, i64* %eptr11421                                          ; *eptr11421 = %Gfl$_37foldr1
  %eptr11420 = getelementptr inbounds i64, i64* %cloptr11419, i64 0                  ; &cloptr11419[0]
  %f11422 = ptrtoint void(i64,i64,i64,i64,i64)* @lam8718 to i64                      ; fptr cast; i64(...)* -> i64
  store i64 %f11422, i64* %eptr11420                                                 ; store fptr
  %arg8646 = ptrtoint i64* %cloptr11419 to i64                                       ; closure cast; i64* -> i64
  %cloptr11423 = inttoptr i64 %cont7771 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11424 = getelementptr inbounds i64, i64* %cloptr11423, i64 0                 ; &cloptr11423[0]
  %f11426 = load i64, i64* %i0ptr11424, align 8                                      ; load; *i0ptr11424
  %fptr11425 = inttoptr i64 %f11426 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11425(i64 %cont7771, i64 %arg8647, i64 %arg8646)    ; tail call
  ret void
}


define void @lam8718(i64 %env8719, i64 %cont7772, i64 %rOh$f, i64 %Kpl$acc, i64 %TtD$lst) {
  %envptr11427 = inttoptr i64 %env8719 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11428 = getelementptr inbounds i64, i64* %envptr11427, i64 1                ; &envptr11427[1]
  %Gfl$_37foldr1 = load i64, i64* %envptr11428, align 8                              ; load; *envptr11428
  %a7457 = call i64 @prim_null_63(i64 %TtD$lst)                                      ; call prim_null_63
  %cmp11429 = icmp eq i64 %a7457, 15                                                 ; false?
  br i1 %cmp11429, label %else11431, label %then11430                                ; if

then11430:
  %arg8651 = add i64 0, 0                                                            ; quoted ()
  %cloptr11432 = inttoptr i64 %cont7772 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11433 = getelementptr inbounds i64, i64* %cloptr11432, i64 0                 ; &cloptr11432[0]
  %f11435 = load i64, i64* %i0ptr11433, align 8                                      ; load; *i0ptr11433
  %fptr11434 = inttoptr i64 %f11435 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11434(i64 %cont7772, i64 %arg8651, i64 %Kpl$acc)    ; tail call
  ret void

else11431:
  %a7458 = call i64 @prim_car(i64 %TtD$lst)                                          ; call prim_car
  %a7459 = call i64 @prim_cdr(i64 %TtD$lst)                                          ; call prim_cdr
  %cloptr11436 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11438 = getelementptr inbounds i64, i64* %cloptr11436, i64 1                  ; &eptr11438[1]
  %eptr11439 = getelementptr inbounds i64, i64* %cloptr11436, i64 2                  ; &eptr11439[2]
  %eptr11440 = getelementptr inbounds i64, i64* %cloptr11436, i64 3                  ; &eptr11440[3]
  store i64 %rOh$f, i64* %eptr11438                                                  ; *eptr11438 = %rOh$f
  store i64 %a7458, i64* %eptr11439                                                  ; *eptr11439 = %a7458
  store i64 %cont7772, i64* %eptr11440                                               ; *eptr11440 = %cont7772
  %eptr11437 = getelementptr inbounds i64, i64* %cloptr11436, i64 0                  ; &cloptr11436[0]
  %f11441 = ptrtoint void(i64,i64,i64)* @lam8716 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11441, i64* %eptr11437                                                 ; store fptr
  %arg8658 = ptrtoint i64* %cloptr11436 to i64                                       ; closure cast; i64* -> i64
  %cloptr11442 = inttoptr i64 %Gfl$_37foldr1 to i64*                                 ; closure/env cast; i64 -> i64*
  %i0ptr11443 = getelementptr inbounds i64, i64* %cloptr11442, i64 0                 ; &cloptr11442[0]
  %f11445 = load i64, i64* %i0ptr11443, align 8                                      ; load; *i0ptr11443
  %fptr11444 = inttoptr i64 %f11445 to void (i64,i64,i64,i64,i64)*                   ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11444(i64 %Gfl$_37foldr1, i64 %arg8658, i64 %rOh$f, i64 %Kpl$acc, i64 %a7459); tail call
  ret void
}


define void @lam8716(i64 %env8717, i64 %_957773, i64 %a7460) {
  %envptr11446 = inttoptr i64 %env8717 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11447 = getelementptr inbounds i64, i64* %envptr11446, i64 3                ; &envptr11446[3]
  %cont7772 = load i64, i64* %envptr11447, align 8                                   ; load; *envptr11447
  %envptr11448 = inttoptr i64 %env8717 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11449 = getelementptr inbounds i64, i64* %envptr11448, i64 2                ; &envptr11448[2]
  %a7458 = load i64, i64* %envptr11449, align 8                                      ; load; *envptr11449
  %envptr11450 = inttoptr i64 %env8717 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11451 = getelementptr inbounds i64, i64* %envptr11450, i64 1                ; &envptr11450[1]
  %rOh$f = load i64, i64* %envptr11451, align 8                                      ; load; *envptr11451
  %cloptr11452 = inttoptr i64 %rOh$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11453 = getelementptr inbounds i64, i64* %cloptr11452, i64 0                 ; &cloptr11452[0]
  %f11455 = load i64, i64* %i0ptr11453, align 8                                      ; load; *i0ptr11453
  %fptr11454 = inttoptr i64 %f11455 to void (i64,i64,i64,i64)*                       ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11454(i64 %rOh$f, i64 %cont7772, i64 %a7458, i64 %a7460); tail call
  ret void
}


define void @lam8713(i64 %env8714, i64 %cont7775, i64 %aiy$y) {
  %arg8665 = add i64 0, 0                                                            ; quoted ()
  %cloptr11456 = call i64* @alloc(i64 16)                                            ; malloc
  %eptr11458 = getelementptr inbounds i64, i64* %cloptr11456, i64 1                  ; &eptr11458[1]
  store i64 %aiy$y, i64* %eptr11458                                                  ; *eptr11458 = %aiy$y
  %eptr11457 = getelementptr inbounds i64, i64* %cloptr11456, i64 0                  ; &cloptr11456[0]
  %f11459 = ptrtoint void(i64,i64,i64)* @lam8710 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11459, i64* %eptr11457                                                 ; store fptr
  %arg8664 = ptrtoint i64* %cloptr11456 to i64                                       ; closure cast; i64* -> i64
  %cloptr11460 = inttoptr i64 %cont7775 to i64*                                      ; closure/env cast; i64 -> i64*
  %i0ptr11461 = getelementptr inbounds i64, i64* %cloptr11460, i64 0                 ; &cloptr11460[0]
  %f11463 = load i64, i64* %i0ptr11461, align 8                                      ; load; *i0ptr11461
  %fptr11462 = inttoptr i64 %f11463 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11462(i64 %cont7775, i64 %arg8665, i64 %arg8664)    ; tail call
  ret void
}


define void @lam8710(i64 %env8711, i64 %cont7776, i64 %fhI$f) {
  %envptr11464 = inttoptr i64 %env8711 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11465 = getelementptr inbounds i64, i64* %envptr11464, i64 1                ; &envptr11464[1]
  %aiy$y = load i64, i64* %envptr11465, align 8                                      ; load; *envptr11465
  %cloptr11466 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11468 = getelementptr inbounds i64, i64* %cloptr11466, i64 1                  ; &eptr11468[1]
  %eptr11469 = getelementptr inbounds i64, i64* %cloptr11466, i64 2                  ; &eptr11469[2]
  store i64 %fhI$f, i64* %eptr11468                                                  ; *eptr11468 = %fhI$f
  store i64 %aiy$y, i64* %eptr11469                                                  ; *eptr11469 = %aiy$y
  %eptr11467 = getelementptr inbounds i64, i64* %cloptr11466, i64 0                  ; &cloptr11466[0]
  %f11470 = ptrtoint void(i64,i64)* @lam8708 to i64                                  ; fptr cast; i64(...)* -> i64
  store i64 %f11470, i64* %eptr11467                                                 ; store fptr
  %arg8667 = ptrtoint i64* %cloptr11466 to i64                                       ; closure cast; i64* -> i64
  %cloptr11471 = inttoptr i64 %fhI$f to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11472 = getelementptr inbounds i64, i64* %cloptr11471, i64 0                 ; &cloptr11471[0]
  %f11474 = load i64, i64* %i0ptr11472, align 8                                      ; load; *i0ptr11472
  %fptr11473 = inttoptr i64 %f11474 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11473(i64 %fhI$f, i64 %cont7776, i64 %arg8667)      ; tail call
  ret void
}


define void @lam8708(i64 %env8709, i64 %BiK$args7778) {
  %envptr11475 = inttoptr i64 %env8709 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11476 = getelementptr inbounds i64, i64* %envptr11475, i64 2                ; &envptr11475[2]
  %aiy$y = load i64, i64* %envptr11476, align 8                                      ; load; *envptr11476
  %envptr11477 = inttoptr i64 %env8709 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11478 = getelementptr inbounds i64, i64* %envptr11477, i64 1                ; &envptr11477[1]
  %fhI$f = load i64, i64* %envptr11478, align 8                                      ; load; *envptr11478
  %cont7777 = call i64 @prim_car(i64 %BiK$args7778)                                  ; call prim_car
  %BiK$args = call i64 @prim_cdr(i64 %BiK$args7778)                                  ; call prim_cdr
  %cloptr11479 = call i64* @alloc(i64 32)                                            ; malloc
  %eptr11481 = getelementptr inbounds i64, i64* %cloptr11479, i64 1                  ; &eptr11481[1]
  %eptr11482 = getelementptr inbounds i64, i64* %cloptr11479, i64 2                  ; &eptr11482[2]
  %eptr11483 = getelementptr inbounds i64, i64* %cloptr11479, i64 3                  ; &eptr11483[3]
  store i64 %fhI$f, i64* %eptr11481                                                  ; *eptr11481 = %fhI$f
  store i64 %cont7777, i64* %eptr11482                                               ; *eptr11482 = %cont7777
  store i64 %BiK$args, i64* %eptr11483                                               ; *eptr11483 = %BiK$args
  %eptr11480 = getelementptr inbounds i64, i64* %cloptr11479, i64 0                  ; &cloptr11479[0]
  %f11484 = ptrtoint void(i64,i64,i64)* @lam8706 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11484, i64* %eptr11480                                                 ; store fptr
  %arg8673 = ptrtoint i64* %cloptr11479 to i64                                       ; closure cast; i64* -> i64
  %cloptr11485 = inttoptr i64 %aiy$y to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11486 = getelementptr inbounds i64, i64* %cloptr11485, i64 0                 ; &cloptr11485[0]
  %f11488 = load i64, i64* %i0ptr11486, align 8                                      ; load; *i0ptr11486
  %fptr11487 = inttoptr i64 %f11488 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11487(i64 %aiy$y, i64 %arg8673, i64 %aiy$y)         ; tail call
  ret void
}


define void @lam8706(i64 %env8707, i64 %_957779, i64 %a7455) {
  %envptr11489 = inttoptr i64 %env8707 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11490 = getelementptr inbounds i64, i64* %envptr11489, i64 3                ; &envptr11489[3]
  %BiK$args = load i64, i64* %envptr11490, align 8                                   ; load; *envptr11490
  %envptr11491 = inttoptr i64 %env8707 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11492 = getelementptr inbounds i64, i64* %envptr11491, i64 2                ; &envptr11491[2]
  %cont7777 = load i64, i64* %envptr11492, align 8                                   ; load; *envptr11492
  %envptr11493 = inttoptr i64 %env8707 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11494 = getelementptr inbounds i64, i64* %envptr11493, i64 1                ; &envptr11493[1]
  %fhI$f = load i64, i64* %envptr11494, align 8                                      ; load; *envptr11494
  %cloptr11495 = call i64* @alloc(i64 24)                                            ; malloc
  %eptr11497 = getelementptr inbounds i64, i64* %cloptr11495, i64 1                  ; &eptr11497[1]
  %eptr11498 = getelementptr inbounds i64, i64* %cloptr11495, i64 2                  ; &eptr11498[2]
  store i64 %cont7777, i64* %eptr11497                                               ; *eptr11497 = %cont7777
  store i64 %BiK$args, i64* %eptr11498                                               ; *eptr11498 = %BiK$args
  %eptr11496 = getelementptr inbounds i64, i64* %cloptr11495, i64 0                  ; &cloptr11495[0]
  %f11499 = ptrtoint void(i64,i64,i64)* @lam8704 to i64                              ; fptr cast; i64(...)* -> i64
  store i64 %f11499, i64* %eptr11496                                                 ; store fptr
  %arg8676 = ptrtoint i64* %cloptr11495 to i64                                       ; closure cast; i64* -> i64
  %cloptr11500 = inttoptr i64 %a7455 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11501 = getelementptr inbounds i64, i64* %cloptr11500, i64 0                 ; &cloptr11500[0]
  %f11503 = load i64, i64* %i0ptr11501, align 8                                      ; load; *i0ptr11501
  %fptr11502 = inttoptr i64 %f11503 to void (i64,i64,i64)*                           ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11502(i64 %a7455, i64 %arg8676, i64 %fhI$f)         ; tail call
  ret void
}


define void @lam8704(i64 %env8705, i64 %_957780, i64 %a7456) {
  %envptr11504 = inttoptr i64 %env8705 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11505 = getelementptr inbounds i64, i64* %envptr11504, i64 2                ; &envptr11504[2]
  %BiK$args = load i64, i64* %envptr11505, align 8                                   ; load; *envptr11505
  %envptr11506 = inttoptr i64 %env8705 to i64*                                       ; closure/env cast; i64 -> i64*
  %envptr11507 = getelementptr inbounds i64, i64* %envptr11506, i64 1                ; &envptr11506[1]
  %cont7777 = load i64, i64* %envptr11507, align 8                                   ; load; *envptr11507
  %cps_45lst7781 = call i64 @prim_cons(i64 %cont7777, i64 %BiK$args)                 ; call prim_cons
  %cloptr11508 = inttoptr i64 %a7456 to i64*                                         ; closure/env cast; i64 -> i64*
  %i0ptr11509 = getelementptr inbounds i64, i64* %cloptr11508, i64 0                 ; &cloptr11508[0]
  %f11511 = load i64, i64* %i0ptr11509, align 8                                      ; load; *i0ptr11509
  %fptr11510 = inttoptr i64 %f11511 to void (i64,i64)*                               ; cast fptr; i64 -> void(...)*
  musttail call fastcc void %fptr11510(i64 %a7456, i64 %cps_45lst7781)               ; tail call
  ret void
}





@sym10040 = private unnamed_addr constant [10 x i8] c"%%promise\00", align 8
