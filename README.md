# 430 Final Project - Srujan Thotapally

This project is the culmination of a semester's worth of work that takes a scheme file and converts it into LLVM output that is then interpreted.

# High Level Description of methods (in order of call)

  - `create-binary.rkt`
    >This file aggregates all of the steps of the compiler into one command. The method `create-binary` is called, which sends the input through all of the steps and eventually produces llvm-output.
  - `top-level`
    > This method takes the top level input language and adds implicit begin forms explicitly, quoting all datums, desugaring defines nested in such begin forms to a letrec*, desugaring quasiquote and unquote. The output of top-level removes all defines and raises begins to one level.
- `desugar`
    >This method removes many of the input forms and yields terms in a small core language including only a let form, the lambda-calculus, conditionals, set!, call/cc, and explicit primitive-operation forms.
- `simplify-ir`
    >This method simplifies the input language and removes lists, map, foldl, foldr and other primitive methods and converts them into lambda functions.
- `assignment-convert`
    >This method converts the use of any variable as a reference of a variable in a vector of length 1.
- `alphatize`
    > This method makes sure that there is no shadowing and converts all variables into gensymed variables to ensure no reuse of variables
- `anf-convert`
    > This method partitions the grammar into atomic expressions (ae), which may be evaluated immediately (datums, variables, and lambdas), and complex expressions (e), which are not known trivially to terminate.
- `cps-convert`
    > Converts the input to continuation-passing style. No function call ever returns; rather the current continuation is invoked at reutrn points.
- `closure-convert`
    > This converts the input into procedures (procs) that replace all lambda abstractinos and replaces them with make-clousre and env-ref forms. Procs are first-order procedures that are converted into LLVM functions directly.
- `proc->llvm`
    > This converts the proc input into LLVM functions that are then run by the machine.
# Prims Supported
- `=` 
   - satisfies: `boolean?`
   - requires: arguments that satisfy `number?` and returns whether all of the elements are equal
- `>` 
   - satisfies: `boolean?`
   - requires: arguments that satisfy `real?` and returns whether all of the elements are decreasing
- `<` 
   - satisfies: `boolean?`
   - requires: arguments that satisfy `real?` and returns whether all of the elements are increasing
- `<=` 
   - satisfies: `boolean?`
   - requires: arguments that satisfy `real?` and returns whether all of the elements are non-decreasing
- `>=` 
   - satisfies: `boolean?`
   - requires: arguments that satisfy `real?` and returns whether all of the elements are non-increasing
- `+` 
   - satisfies: `number?`
   - requires: arguments that satisfy `number?` and returns the sum of arguments (0 if none are given)
- `-` 
   - satisfies: `number?`
   - requires: arguments that satisfy `number?` and returns the subtraction of arguments (the element itself if one is given)
- `*` 
   - satisfies: `number?`
   - requires: arguments that satisfy `number?` and returns the multiplication of arguments (1 if none are given)
- `/` 
   - satisfies: `number?`
   - requires: arguments that satisfy `number?` and returns the division of arguments (the element itself if one is given)
- `cons?` 
   - satisfies: `boolean?`
   - requires: any argument and returns whether that argument is a pair
- `null?` 
   - satisfies: `boolean?`
   - requires: any argument and returns whether that argument is the empty list.
- `cons` 
   - satisfies: `pair?`
   - requires: any two arguments and returns a newly allocated pair of the first and second arguments
- `car`
   - satisfies: `any`
   - requires: an argument that satisfies `pair?` and returns the first element of the pair argument
- `cdr`
   - satisfies: `any`
   - requires: an argument that satisfies `pair?` and returns the second element of the pair argument
- `vector`
   - satisfies: `vector?`
   - requires: any arguments and returns a newly allocated mutable vetor (an array in header.cpp) with as many slots as the length of arguemtns provided and the slots are initialized to contain the given arguments in order
- `make-vector`
   - satisfies: `vector?`
   - requires: the first argument satisfies `exact-nonnegative-integer?` while the second argument can be of any type. This returns a muteable vector (an array in header.cpp) with the number of slots specified by the first arguments initialized with the second argument.
- `vector-ref`
   - satisfies: `any`
   - requires: the first argument satisfies `vector?` while the second argument satisfies `exact-nonnegative-integer?`. This returns the element in the slot specified by the second argument inside the vector specified by the first argument.
- `vector-set!`
   - satisfies: `void?`
   - requires: the first argument satisfies `(and/c vector? (not/c immutable?))` while the second argument satisfies `exact-nonnegative-integer?`. The third argument can be of any type. This updates the vector specified in the first argument at the position specified in the second argument with the element specified in the third argument.
- `void`
   - satisfies: `void?`
   - requires: any arguments given are ignored and can be of any type.
- `void?`
   - satisfies: `boolean?`
   - requires: takes one argument and it can be of any type. It returns whether the argument is the constant void.
- `eq?`
   - satisfies: `boolean?`
   - requires: takes two arguments of any argument type and returns whether the two arguments refer to the same object.
- `eqv?`
   - satisfies: `boolean?`
   - requires: takes two arguments of any argument type and returns whether the two arguments satisfy the `eq?` predicate, unless otherwise specified for a particular datatype. (i.e. for two numbers, they satisfy the `eqv?` predicate if they are both equal and non-zero with the exact sameness and precision.
- `number?`
   - satisfies: `boolean?`
   - requires: takes an argument of any type and returns whether the argument is a number
- `integer?`
   - satisfies: `boolean?`
   - requires: takes an argument of any type and returns whether the argument is a integer
- `procedure?`
   - satisfies: `boolean?`
   - requires: takes an argument of any type and returns whether the argument is a procedure
- `not`
   - satisfies: `boolean?`
   - requires: takes an argument of any type and returns true if the argument is false, and false otherwise (i.e. if the argument is not a boolean value of true, then it just returns false)

#### **Prim operations added by me**

**NOTE** - The tests I made for hashes are inside the `public` folder.

- `make-hash`
   - satisfies: `(and/c hash? hash-equal?)`
   - requires: takes an agrument list that satisfies `listof pair?` and creates a hash mapping each car of the pair (detailed above) as the key and each cdr (detailed above) as the value.
   - **NOTE** - Currently the hash is only fully functional with integers. I was unable to include all data-types for this hash.
- `hash-ref`
    - satisfies: `any`
   - requires: two arguemnts, the first satisfying `hash?`, while the second argument, the key, can be of any type. If a key is not found, a fatal error is thrown, stating that the key was not found within the hash.
- `hash-set!`
    - satisfies: `void?`
   - requires: given three arguments, the first being a hash that satisfies `(and/c hash? (not/c immutable?))`, the second being a key of any type and the third being a value of any type. The key is searched for inside the hash and if it exists, the value is changed to the one specified in the third argument. However, if the key is not found, then the key value pair is then added to the given hash in the first argument.

##### Test the primitive hash functions

```sh
$ racket tests.rkt make-hash-2
$ racket tests.rkt make-hash-3
```

### Runtime exceptions that are handled
**NOTE** - The tests for runtime exceptions are stored in the `student` folder.

- Use of variables in let expressions that are not initialized
    - I go through the output language and store all variables inside a set and look through the set everytime a variable is referenced. If it is not inside the hash, I raise an exception.
```sh
$ racket tests.rkt letrecinit-1
$ racket tests.rkt letrecinit-2
```
- Division by 0 error
    - I go through and in the prim method for division in `header.cpp` I make sure that the divisor is not 0.
```sh
$ racket tests.rkt divby0-1
$ racket tests.rkt divby0-2
```

- Memory cap does not exceed 256 MB
    - Everytime `malloc` and `alloc` is used, I increase the counter I keep by the amount that is being dynamically allocated. If that counter is greater than 256 MB (in bytes), I throw an error.
```sh
$ racket tests.rkt memcheck-1
$ racket tests.rkt memcheck-2
```

- A hash is given to `hash-ref` and `hash-set!`, and that a key given to `hash-set!` exists within the hash.
    - I create my own proprietary tag by allocating an array of length 2, the first index storing my tag, and the second index storing my hash. When running `hash-ref` and `hash-set!`, I check to make sure that the input `&` 7 is the `OTHER_TAG` and that the first value of the array does indeed contain my proprietary tag `HASH_OTHERTAG`
```sh
$ racket tests.rkt hash-not-given
$ racket tests.rkt hash-ref-keynotexist
```

- Illegal vector references
    - In `vector-ref` and `vector-set!`, I check to make sure that the index being given is both greater than 0 (since the 0th index stores the length and the tag) and it is less than or equal to the length of the vector itself. 
```sh
$ racket tests.rkt illegal-ref-vector-1
$ racket tests.rkt illegal-ref-vector-2
```

### Changes made to `utils.rkt` and `tests.rkt`
I created my own method, `test-binary` that calls the method I mentioned above `create-binary`. `test-binary` is passed the expression that is being decoded, the path (which is broken down to just the filename such that the binary is created of the filename), and the folder (if the folder is `secret` then `eval-llvm` is not run such that an exception is not produced when running the tests). This is then passed to `eval-llvm` and `test-binary` which create the compiled executeable and check to see if the output is correct. For tests that do not run `eval-llvm`, `void` is returned such that the tests pass (since exceptions are thrown).

### BOEHM GC Attempt
I was able to get the github repository to compile on my computer, but unfortunately did not have enough time to implement BOEHM GC. Currently, the pertinent files that would be necessary to implement BOEHM GC is inside the `bdwgc` folder.