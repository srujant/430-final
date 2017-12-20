

//#include "gc.h"    // Add back in and change tags if we want to use GC
#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include <iostream>
#include "uthash.h"

#define CLO_TAG 0
#define CONS_TAG 1
#define INT_TAG 2
#define STR_TAG 3
#define SYM_TAG 4
#define OTHER_TAG 6
#define ENUM_TAG 7


#define VECTOR_OTHERTAG 1
#define HASH_OTHERTAG 2
// Hashes, Sets, gen records, can all be added here


#define V_VOID 39  //32 +7 (+7 is for anything enumerable other than null)
#define V_TRUE 31  //24 +7
#define V_FALSE 15 //8  +7
#define V_NULL 0



#define MASK64 0xffffffffffffffff // useful for tagging related operations


#define ASSERT_TAG(v,tag,msg) \
    if(((v)&7ULL) != (tag)) \
        fatal_err(msg);

#define ASSERT_VALUE(v,val,msg) \
    if(((u64)(v)) != (val))     \
        fatal_err(msg);


#define DECODE_CLO(v) ((u64*)((v)&(7ULL^MASK64)))
#define ENCODE_CLO(v) (((u64)(v)) | CLO_TAG)

#define DECODE_CONS(v) ((u64*)((v)&(7ULL^MASK64)))
#define ENCODE_CONS(v) (((u64)(v)) | CONS_TAG)

#define DECODE_INT(v) ((s32)((u32)(((v)&(7ULL^MASK64)) >> 32)))
#define ENCODE_INT(v) ((((u64)((u32)(v))) << 32) | INT_TAG)

#define DECODE_STR(v) ((char*)((v)&(7ULL^MASK64)))
#define ENCODE_STR(v) (((u64)(v)) | STR_TAG)

#define DECODE_SYM(v) ((char*)((v)&(7ULL^MASK64)))
#define ENCODE_SYM(v) (((u64)(v)) | SYM_TAG)

#define DECODE_OTHER(v) ((u64*)((v)&(7ULL^MASK64)))
#define ENCODE_OTHER(v) (((u64)(v)) | OTHER_TAG)


// some apply-prim macros for expecting 1 argument or 2 arguments
#define GEN_EXPECT1ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 v0 = expect_args1(lst); \
        return g(v0); \
    } 

#define GEN_EXPECT2ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1);                                           \
    } 

#define GEN_EXPECT3ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        u64 v2 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1,v2);                                        \
    } 





// No mangled names
extern "C"
{



typedef uint64_t u64;
typedef int64_t s64;
typedef uint32_t u32;
typedef int32_t s32;


struct hash_tbl {
    u64 key;
    u64 value;
    UT_hash_handle hh;
};

u64 memory_cap = 0;

    
// UTILS


void fatal_err(const char* msg)
{
    printf("library run-time error: ");
    printf("%s", msg);
    printf("\n");
    exit(1);
}

u64* alloc(const u64 m)
{
    memory_cap = memory_cap + m;
    if(memory_cap > 268435456){
        fatal_err("Memory cap exceeded 256 mb.");
    }
    return (u64*)(malloc(m));
    //return new u64[m];
    //return (u64*)GC_MALLOC(m);
}

void check_size(u64 val){
    if (DECODE_INT(val) > 2147483647){
        fatal_err("One of the variables was given a value that was larger than the maximum value of an int causing an integer overflow.");
    }
}


void print_u64(u64 i)
{
    printf("%lu\n", i);
}

u64 expect_args0(u64 args)
{
    if (args != V_NULL)
        fatal_err("Expected value: null (in expect_args0). Prim cannot take arguments.");
    return V_NULL;
}

u64 expect_args1(u64 args)
{
    ASSERT_TAG(args, CONS_TAG, "Expected cons value (in expect_args1). Prim applied on an empty argument list.")
    u64* p = DECODE_CONS(args);
    ASSERT_VALUE((p[1]), V_NULL, "Expected null value (in expect_args1). Prim can only take 1 argument.")
    return p[0];
}

u64 expect_cons(u64 p, u64* rest)
{
    // pass a pair value p and a pointer to a word *rest                          
    // verifiies (cons? p), returns the value (car p) and assigns *rest = (cdr p) 
    ASSERT_TAG(p, CONS_TAG, "Expected a cons value. (expect_cons)")               

    u64* pp = DECODE_CONS(p);
    *rest = pp[1];
    return pp[0];
}

u64 expect_other(u64 v, u64* rest)
{
    // returns the runtime tag value
    // puts the untagged value at *rest
    ASSERT_TAG(v, OTHER_TAG, "Expected a vector or special value. (expect_other)")
    
    u64* p = DECODE_OTHER(v);
    *rest = p[1];
    return p[0];
}


/////// CONSTANTS
    
    
u64 const_init_int(s64 i)
{
    return ENCODE_INT((s32)i);
}

u64 const_init_void()
{
    return V_VOID;
}


u64 const_init_null()
{
    return V_NULL;
}


u64 const_init_true()
{
    return V_TRUE;
}

    
u64 const_init_false()
{
    return V_FALSE;
}

    
u64 const_init_string(const char* s)
{
    return ENCODE_STR(s);
}
        
u64 const_init_symbol(const char* s)
{
    return ENCODE_SYM(s);
}







/////////// PRIMS

    
///// effectful prims:

    
u64 prim_print_aux(u64 v) 
{
    if (v == V_NULL)
        printf("()");
    else if ((v&7) == CLO_TAG)
        printf("#<procedure>");
    else if ((v&7) == CONS_TAG)
    {
        u64* p = DECODE_CONS(v);
        printf("(");
        prim_print_aux(p[0]);
        printf(" . ");
        prim_print_aux(p[1]);
        printf(")");
    }
    else if ((v&7) == INT_TAG)
    {
        printf("%d", (int)((s32)(v >> 32)));
    }
    else if ((v&7) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", DECODE_STR(v));
    }
    else if ((v&7) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("%s", DECODE_SYM(v));
    }
    else if ((v&7) == OTHER_TAG
             && (VECTOR_OTHERTAG == (((u64*)DECODE_OTHER(v))[0] & 7)))
    {
        printf("#(");
        u64* vec = (u64*)DECODE_OTHER(v);
        u64 len = vec[0] >> 3;
        prim_print_aux(vec[1]);
        for (u64 i = 2; i <= len; ++i)
        {
            printf(",");
            prim_print_aux(vec[i]);
        }
        printf(")");
    }
    else if ((v&7) == OTHER_TAG
             && (HASH_OTHERTAG == (((u64*)DECODE_OTHER(v))[0])))
    {
        printf("HERE");
    }
    else
        printf("(print.. v); unrecognized value %lu", v);
    //...
    return V_VOID; 
}

u64 prim_print(u64 v) 
{
    if (v == V_NULL)
        printf("'()");
    else if ((v&7) == CLO_TAG)
        printf("#<procedure>");
    else if ((v&7) == CONS_TAG)
    {
        u64* p = (u64*)(v&(7ULL^MASK64));
        printf("'(");
        prim_print_aux(p[0]);
        printf(" . ");
        prim_print_aux(p[1]);
        printf(")");
    }
    else if ((v&7) == INT_TAG)
    {
        printf("%d", ((s32)(v >> 32)));
    }
    else if ((v&7) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", DECODE_STR(v));
    }
    else if ((v&7) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("'%s", DECODE_SYM(v));
    }
    else if ((v&7) == OTHER_TAG
             && (VECTOR_OTHERTAG == (((u64*)DECODE_OTHER(v))[0] & 7)))
    {
        printf("#(");
        u64* vec = (u64*)DECODE_OTHER(v);
        u64 len = vec[0] >> 3;
        prim_print(vec[1]);
        for (u64 i = 2; i <= len; ++i)
        {
            printf(",");
            prim_print(vec[i]);
        }
        printf(")");
    }
    else if ((v&7) == OTHER_TAG
             && (HASH_OTHERTAG == (((u64*)DECODE_OTHER(v))[0])))
    {
        printf("#hash(");
        u64* vec = (u64*)DECODE_OTHER(v);
        u64* ptr_to_hash = DECODE_OTHER(vec[1]);
        hash_tbl * tbls = (hash_tbl *) ptr_to_hash;
        hash_tbl * tbl;
        for(tbl=tbls; tbl != NULL; tbl=(hash_tbl*)(tbl->hh.next)) {
            if(tbl->hh.next == NULL){
                printf("(%d . %d)", DECODE_INT(tbl->key), DECODE_INT(tbl->value));
            }else{
                printf("(%d . %d) ", DECODE_INT(tbl->key), DECODE_INT(tbl->value));
            }
            
        }
        printf(")");
    }
    else
        printf("(print v); unrecognized value %lu", v);
    //...
    return V_VOID; 
}
GEN_EXPECT1ARGLIST(applyprim_print,prim_print)


u64 prim_halt(u64 v) // halt
{
    prim_print(v); // display the final value
    printf("\n");
    exit(0);
    return V_NULL; 
}


u64 applyprim_vector(u64 lst)
{
    memory_cap = memory_cap + (512*sizeof(u64));
    if(memory_cap > 268435456){
        fatal_err("Memory cap exceeded 256 mb.");
    }
    // pretty terrible, but works
    u64* buffer = (u64*)malloc(512*sizeof(u64));
    u64 l = 0;
    while ((lst&7) == CONS_TAG && l < 512) 
        buffer[l++] = expect_cons(lst, &lst);
    u64* mem = alloc((l + 1) * sizeof(u64));
    mem[0] = (l << 3) | VECTOR_OTHERTAG;
    for (u64 i = 0; i < l; ++i)
        mem[i+1] = buffer[i];
    delete [] buffer;
    return ENCODE_OTHER(mem);
}



u64 prim_make_45vector(u64 lenv, u64 iv)
{
    ASSERT_TAG(lenv, INT_TAG, "first argument to make-vector must be an integer")
    
    const u64 l = DECODE_INT(lenv);
    u64* vec = (u64*)alloc((l + 1) * sizeof(u64));
    vec[0] = (l << 3) | VECTOR_OTHERTAG;
    for (u64 i = 1; i <= l; ++i)
        vec[i] = iv;
    return ENCODE_OTHER(vec);
}
GEN_EXPECT2ARGLIST(applyprim_make_45vector, prim_make_45vector)


u64 prim_vector_45ref(u64 v, u64 i)
{
    ASSERT_TAG(i, INT_TAG, "second argument to vector-ref must be an integer")
    ASSERT_TAG(v, OTHER_TAG, "first argument to vector-ref must be a vector")

    if ((((u64*)DECODE_OTHER(v))[0]&7) != VECTOR_OTHERTAG)
        fatal_err("vector-ref not given a properly formed vector");

    u64* vec = (u64*)DECODE_OTHER(v);
    u64 len = vec[0] >> 3;

    if(len < DECODE_INT(i) || DECODE_INT(i) < 1){
        //fatal_err("illegal reference of a vector.");
    }


    return ((u64*)DECODE_OTHER(v))[1+(DECODE_INT(i))];
}
GEN_EXPECT2ARGLIST(applyprim_vector_45ref, prim_vector_45ref)


u64 prim_vector_45set_33(u64 a, u64 i, u64 v)
{
    ASSERT_TAG(i, INT_TAG, "second argument to vector-ref must be an integer")
    ASSERT_TAG(a, OTHER_TAG, "first argument to vector-ref must be an integer")

    if ((((u64*)DECODE_OTHER(a))[0]&7) != VECTOR_OTHERTAG)
        fatal_err("vector-ref not given a properly formed vector");

    u64* vec = (u64*)DECODE_OTHER(a);
    u64 len = vec[0] >> 3;
    if(len <= DECODE_INT(i) || DECODE_INT(i) < 1){
        //fatal_err("illegal reference of a vector.");
    }

        
    ((u64*)(DECODE_OTHER(a)))[1+DECODE_INT(i)] = v;
        
    return V_VOID;
}
GEN_EXPECT3ARGLIST(applyprim_vector_45set_33, prim_vector_45set_33)


///// void, ...

    
u64 prim_void()
{
    return V_VOID;
}


    



///// eq?, eqv?, equal?

    
u64 prim_eq_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eq_63, prim_eq_63)


u64 prim_eqv_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    //else if  // optional extra logic, see r7rs reference
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eqv_63, prim_eqv_63)

/*
u64 prim_equal_63(u64 a, u64 b)
{
    return 0;
}
GEN_EXPECT2ARGLIST(applyprim_equal_63, prim_equal_63)
*/


///// Other predicates


u64 prim_number_63(u64 a)
{
    // We assume that ints are the only number
    if ((a&7) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_number_63, prim_number_63)


u64 prim_integer_63(u64 a)
{
    if ((a&7) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_integer_63, prim_integer_63)


u64 prim_void_63(u64 a)
{
    if (a == V_VOID)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_void_63, prim_void_63)


u64 prim_procedure_63(u64 a)
{
    if ((a&7) == CLO_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_procedure_63, prim_procedure_63)


///// null?, cons?, cons, car, cdr


u64 prim_null_63(u64 p) // null?
{
    if (p == V_NULL)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_null_63, prim_null_63)    


u64 prim_cons_63(u64 p) // cons?
{
    if ((p&7) == CONS_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_cons_63, prim_cons_63)    


u64 prim_cons(u64 a, u64 b)
{
    u64* p = alloc(2*sizeof(u64));
    p[0] = a;
    p[1] = b;
    return ENCODE_CONS(p);
}
GEN_EXPECT2ARGLIST(applyprim_cons, prim_cons)


u64 prim_car(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);
    
    return v0;
}
GEN_EXPECT1ARGLIST(applyprim_car, prim_car)


u64 prim_cdr(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);
    
    return rest;
}
GEN_EXPECT1ARGLIST(applyprim_cdr, prim_cdr)


///// s32 prims, +, -, *, =, ...

    
u64 prim__43(u64 a, u64 b) // +
{
    ASSERT_TAG(a, INT_TAG, "(prim + a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim + a b); b is not an integer")

        //printf("sum: %d\n", DECODE_INT(a) + DECODE_INT(b));
    check_size(a);
    check_size(b);
    
    return ENCODE_INT(DECODE_INT(a) + DECODE_INT(b));
}

u64 applyprim__43(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(0);
    else
    {
        ASSERT_TAG(p, CONS_TAG, "Tried to apply + on non list value.")
        u64* pp = DECODE_CONS(p);
        return ENCODE_INT(DECODE_INT(pp[0]) + DECODE_INT(applyprim__43(pp[1])));
    }
}
    
u64 prim__45(u64 a, u64 b) // -
{
    ASSERT_TAG(a, INT_TAG, "(prim + a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim - a b); b is not an integer")
    
    return ENCODE_INT(DECODE_INT(a) - DECODE_INT(b));
}

u64 applyprim__45(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(0);
    else
    {
        ASSERT_TAG(p, CONS_TAG, "Tried to apply + on non list value.")
        u64* pp = DECODE_CONS(p);
        if (pp[1] == V_NULL)
            return ENCODE_INT(0 - DECODE_INT(pp[0]));
        else // ideally would be properly left-to-right
            return ENCODE_INT(DECODE_INT(pp[0]) - DECODE_INT(applyprim__43(pp[1])));
    }
}
    
u64 prim__42(u64 a, u64 b) // *
{
    ASSERT_TAG(a, INT_TAG, "(prim * a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim * a b); b is not an integer")
    
    return ENCODE_INT(DECODE_INT(a) * DECODE_INT(b));
}

u64 applyprim__42(u64 p)
{
    if (p == V_NULL)
        return ENCODE_INT(1);
    else
    {
        ASSERT_TAG(p, CONS_TAG, "Tried to apply + on non list value.")
        u64* pp = DECODE_CONS(p);
        return ENCODE_INT(DECODE_INT(pp[0]) * DECODE_INT(applyprim__42(pp[1])));
    }
}
    
u64 prim__47(u64 a, u64 b) // /
{
    ASSERT_TAG(a, INT_TAG, "(prim / a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim / a b); b is not an integer")

    if (b == ENCODE_INT(0)) {
        fatal_err("Division by 0 error. Cannot divide by 0.");
    }
    
    return ENCODE_INT(DECODE_INT(a) / DECODE_INT(b));
}
    
u64 prim__61(u64 a, u64 b)  // =
{
    ASSERT_TAG(a, INT_TAG, "(prim = a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim = a b); b is not an integer")
        
    if ((s32)((a&(7ULL^MASK64)) >> 32) == (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim__60(u64 a, u64 b) // <
{
    ASSERT_TAG(a, INT_TAG, "(prim < a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim < a b); b is not an integer")
    
    if ((s32)((a&(7ULL^MASK64)) >> 32) < (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}
    
u64 prim__60_61(u64 a, u64 b) // <=
{
    ASSERT_TAG(a, INT_TAG, "(prim <= a b); a is not an integer")
    ASSERT_TAG(b, INT_TAG, "(prim <= a b); b is not an integer")
        
    if ((s32)((a&(7ULL^MASK64)) >> 32) <= (s32)((b&(7ULL^MASK64)) >> 32))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim_not(u64 a) 
{
    if (a == V_FALSE)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_not, prim_not)


u64 prim_make_45hash(u64 lst)
{
    ASSERT_TAG(lst, CONS_TAG, "not given a list")

    struct hash_tbl * new_pair = NULL;
    struct hash_tbl * new_table = NULL;
    u64 pair;
    memory_cap = memory_cap + (2 * sizeof(u64));
    if(memory_cap > 268435456){
        fatal_err("Memory cap exceeded 256 mb.");
    }
    u64* vec = (u64*)alloc(2 * sizeof(u64));
    vec[0] = HASH_OTHERTAG;
    while ((lst&7) == CONS_TAG){
        struct hash_tbl * result = NULL;

        new_pair = (struct hash_tbl*)malloc(sizeof(struct hash_tbl));
        pair = expect_cons(lst, &lst);
        u64 key = expect_cons(pair, &pair);
        new_pair->key = key;
        new_pair->value = pair;
        HASH_FIND(hh, new_table, &key, sizeof(u64), result);
        if(result != NULL){
            result->value = pair;
        }else{
            HASH_ADD(hh, new_table, key, sizeof(u64), new_pair);
        }
    }
    vec[1] = ENCODE_OTHER(new_table);
    return ENCODE_OTHER(vec);
}

GEN_EXPECT1ARGLIST(applyprim_make_45hash, prim_make_45hash)

u64 prim_hash_45ref(u64 hash, u64 key)
{
    if ((hash&7) != OTHER_TAG || (HASH_OTHERTAG != (((u64*)DECODE_OTHER(hash))[0]))){
        fatal_err("Not given a hash.");
    }
    
    u64* vec = (u64*)DECODE_OTHER(hash);
    u64* ptr_to_hash = DECODE_OTHER(vec[1]);
    struct hash_tbl * tbls = (struct hash_tbl *) ptr_to_hash;
    struct hash_tbl * tbl = NULL;

    HASH_FIND(hh, tbls, &key, sizeof(u64), tbl);
    if(tbl != NULL){
        return tbl->value;
    }
    
    fatal_err("Given a key that is not found within the hash.");
    return V_VOID;
    

}

GEN_EXPECT2ARGLIST(applyprim_hash_45ref, prim_hash_45ref)

u64 prim_hash_45set_33(u64 hash, u64 key, u64 value)
{
    if ((hash&7) != OTHER_TAG || (HASH_OTHERTAG != (((u64*)DECODE_OTHER(hash))[0]))){
        fatal_err("Not given a hash.");
    }
    
    u64* vec = (u64*)DECODE_OTHER(hash);
    u64* ptr_to_hash = DECODE_OTHER(vec[1]);
    struct hash_tbl * tbls = (struct hash_tbl *) ptr_to_hash;
    struct hash_tbl * tbl = NULL;

    HASH_FIND(hh, tbls, &key, sizeof(u64), tbl);
    if(tbl != NULL){
        tbl->value = value;
    }else{
        struct hash_tbl * new_pair = NULL;

        new_pair = (struct hash_tbl*)malloc(sizeof(struct hash_tbl));
        new_pair->key = key;
        new_pair->value = value;
        HASH_ADD(hh, tbls, key, sizeof(u64), new_pair);
    }
    
    return V_VOID;
    

}

GEN_EXPECT3ARGLIST(applyprim_hash_45set_33, prim_hash_45set_33)

}





