
# OCaml Towards Clarity and Grace

This is a guide towards writing better [OCaml] code.  We all like to
write [OCaml] code that is correct, maintainable, efficient, and maybe
even beautiful. The sad truth is that this cannot be achieved simply by
following some rules. Just like it is not enough for a book to be
composed of correct sentences and coherent paragraphs to be considered
good. However, a book violating these principles stands little chance of
finding many readers. Likewise, this guide tries to help with the small
structures in programming [OCaml] upon which we can hope to build bigger
ones.

[OCaml]:  https://www.ocaml.org/

## Other Resources

* [Upenn]'s style guide for student projects
* [OCaml Tutorial]
* [OCaml Manual]
* [Real World OCaml] Book

[Upenn]:            https  //www.seas.upenn.edu/~cis341/current/programming_style.shtml
[OCaml Tutorial]:   https   //ocaml.org/learn/tutorials/guidelines.html
[OCaml Manual]:     http    //caml.inria.fr/pub/docs/manual-ocaml/index.html
[Real World OCaml]: https   //realworldocaml.org/

## Indentation, Line Length

* Indentation must always reflect the logic of a program.

* In existing code, adopt the existing style for indentation: tabs or
  spaces.

* Line length should not exceed 100 characters. Break up long lines in
  new code in particular.

* For new code, prefer spaces over tabs and use [ocp-indent] to maintain
  consistent indentation. Note that [ocp-indent] does not break up long lines.

* Most OCaml code bases uses 2 spaces per indentation level.

Rationale:

* Overly long lines create problems when printing source code and when
  resolving merge conflicts in tools by forcing a lot of sideways
  scrolling.
* Spaces are unambiguous in meaning whereas tabs depend on the
  environment.
* [ocp-indent] is a proven way to re-establish consistent
  indentation while still leaving room for other aspects of code
  formatting.

[ocp-indent]: https://github.com/OCamlPro/ocp-indent

### Comments

Comments generally go before the code they are referencing. The possible
exception are declarations in interfaces (mli files, signatures) and
types where they can go after the declaration.

Syntactically there are two kinds of comments:

1. General comments, enclosed in `(*` and `*)`
2. Special comments, enclosed in `(**` and `*)`

Special comments are associated with type and values in a program and
treated specially by the compiler. They become available in automatically
generated documentation.  For the association to work, there must be no
empty line between a special comment and the element they are associated
with. See the section about [ocamldoc] for details.

Code should always be as clear as possible but that clarity cannot
always convey the reason behind a design. Comments have the role to
provide it: the why.

### What to Comment

* The purpose of a module or functor
* The purpose of a value or function in an signature (interface)
* The purpose of a type declaration or its components, if not obvious
* The purpose of record components and variants in types, if not obvious
* Unusual Algorithms and their complexity
* Invariants when not expressed as assertions
* Error handling

### What not to Comment

* Purpose of a local let binding - the name should tell it
* Every line in function

### Good Examples

* [Ocaml List](https://github.com/ocaml/ocaml/blob/trunk/stdlib/list.mli)
  The documentation of the standard list module. Each function is
  commented below its signature.

* [Container Hash Set](https://github.com/c-cube/ocaml-containers/blob/master/src/data/CCHashSet.mli)

* [Mtime](https://github.com/dbuenzli/mtime/blob/master/src/mtime.mli)
  with [implementation](https://github.com/dbuenzli/mtime/blob/master/src/mtime.ml)

### Caveats

Code duplication is bad but copying comments is worse. Be very careful
when copying comments to make sure they are not becoming misleading in a
new context.


[ocamldoc]: http://caml.inria.fr/pub/docs/manual-ocaml/ocamldoc.html

## Naming and Declarations

[OCaml] has a few conventions for names. In particular, capitalisation
is significant.

* Types: `lower_case`.
* Type variables: `'lower_case`
* Values and Functions: `lower_case`
* Constructors: `UpperCase` or `Upper_Case`
* Record Fields: `lower_case`
* Modules: `UpperCase`
* Signatures: `UpperCase`
* Module Types: `ALLCAPS`

Special cases:

* A file `abc.ml` is mapped to module `Abc` even though the filename is
  not capitalised. The corresponding interface is `abc.mli`.

* When a module defines a central (often abstract) data type, it is
  typically named `t`:

```
module Tree: sig
  type 'a t
  val empty: 'a t
end = struct
  type 'a t = Empty | Tree of 'a t * 'a * 'a t
  let empty = Empty
end
```




## Resources and Exceptions: use finally

Ensure that resources are de-allocated in the presence of exceptions.
This is typically done with a function like `finally`:

```
let finally (f: unit -> 'a) (free: unit -> 'b) =
  let res = try f () with exn -> free (); raise exn in
  free ();
  res

let with_file path (f: in_channel -> 'a) =
  let io = open_in path in
  finally
    (fun () -> f io)
    (fun () -> close_in io)

with_file "/etc/passwd" (fun io -> input_line io |> print_endline)
```

In the above code we make sure to close the file even if the function
reading from it throws an exception. A function like `finally` should be
already defined in existing projects so there is no need to re-implement
it locally.

Resources that need to be managed are not just files but can be
anything like database and network connections or timers.


## Constants

* Avoid using magic constants as literals like `86400` for the number of
  seconds in a day. Constants should be let-bound to a name:

```
let ( ** ) x y    = Int64.mul (Int64.of_int x) y
let sec           = 1L
let sec_per_min   = 60 ** sec
let sec_per_hour  = 60 ** sec_per_min
let sec_per_day   = 24 ** sec_per_hour
```

* For literals, picking the correct base can make them less magical:
  compare `255` (decimal) with `0xff` (hexadecimal) or `0b111_1111`
  (binary).

* Structure long literal numbers with a underscores for
  readability as in `0b1111_1111` above. It works in all bases, for
  example in `1_000_000`.

More details are in the [Ocaml Manual](http://caml.inria.fr/pub/docs/manual-ocaml/lex.html).

## Avoid Opening Modules Globally

For convenience, many developers open modules globally in order to gain
access to functions without having to use qualified access. Below,
`Printf`, `List`, and `Sys` are opened in this way:

```
open Printf
open List
open Sys

let rec join = function
  | []      -> ""
  | [x]     -> x
  | [x;y]   -> x ^ " and " ^ y
  | x::xs   -> x ^ ", "    ^ join xs

let main () =
  let argv = Array.to_list argv in
  let args = tl argv in
  match args with
  | []        -> printf "Hello, world!\n"
  | names     -> printf "Hello, %s!\n" (join names)

let () = main ()
```

#### Do This Instead

Between opening a module globally and not at all, several options exist.

* Always use fully qualified names (see the code below). This is the
  best solution if we need few values from a module and only
  sporadically.

  ```
  let main () =
    let argv = Array.to_list Sys.argv in
    let args = List.tl argv in
    match args with
    | []        -> Printf.printf "Hello, world!\n"
    | names     -> Printf.printf "Hello, %s!\n" (join names)
  ```

* Introduce aliases to shorten long module and function names. This is
  almost always a good solution.

  ```
  module L = List

  let printf  = Printf.printf
  let sprintf = Printf.sprintf
  ```

* Open a module locally with open

  Opening a module locally limits the scope for the open module:

  ```
  let main () =
    let open Printf in
    let argv = Array.to_list Sys.argv in
    let args = List.tl argv in
    match args with
    | []        -> printf "Hello, world!\n"
    | names     -> printf "Hello, %s!\n" (join names)

  ```

* Open a module locally with Module.()

  ```
  Int64.(add (of_int x) 1_000_000L)
  ```

  The code above is another way of writing the code below. The module
  `Int64` is open inside the parentheses.

  ```
  Int64.add (Int64.of_int x) 1_000_000L
  ```

  This is especially effective to access constructors that are defined
  inside a module


#### Rationale

Opening a module introduces all its values and constructors into the
local module. Since the definitions of these values and constructors are
not visible, it becomes very hard to understand the code without tool
support. While a developer might argue that tool support is available,
it is not during reviews on GitHub or when looking at a printout. The
problem is exaggerated when several modules are opened.




## Concurrency, Threads

## Avoid Deep Nesting

Code that is deeply nested is hard to read and hard to test. This
suggests that it should be restructured - probably by introducing new
functions.

## Equality: `!=` and `==` vs `<>` and `=`

Using `!=` and `==` for equality is probably wrong and you should use
`<>` and `=` instead.

[OCaml] has two kinds of equality:

1. Structural equality, tested with `=` and `<>`. This compares the
   shape of two values and is typically the correct choice.

2. Pointer equality, tested with `==` and `!=`. This compares the
   address in memory of two values. This is typically used in
   performance-oriented code. Pointer equality implies structural
   equality but not vice versa.



## if vs. match - use of semicolon

Be aware that statement sequences require `begin`/`end` in `if`
expressions.  [OCaml] has some inconsistencies how it handles statement
sequences (containing `;`). Compare:

```
match true with
  | true -> ()
  | false -> print_endline "1"; print_endline "2"
(* prints nothing *)
```
versus:

```
if false then print_endline "1"; print_endline "2"
(* prints 2 *)
```

A statement sequence inside an `if` or `else` branch must be grouped by
`begin`/`end` or parentheses to be governed by the guard.

```
if false then begin
  print_endline "1";
  print_endline "2"
end else begin
  print_endline "3";
  print_endline "4"
end
```

The danger of not realising what code is governed by an if-expression is
exasperated by incorrect indentation and by incremental changes to
existing code.




## Error Handling

## Scopimg

Use the module system to group values. It's quite common to define
simple values that belong together and to indicate this in their names:

```
let option_debug     = false
let option_verbosity = High
let option_log       = stdout
```
It is better to let the module system do the work:

```
module Option = struct
  let debug     = false
  let verbosity = High
  let log       = stdout
end
```

A value can now be accessed like in `Option.debug`. A module simply used
for grouping doesn't require an interface.


## Git - Commit Messages

## Special Topics

### Use Pattern Matching for Value Destruction

The readability of code is improved by using pattern matching rather
than (neeply nested) if-then-else control flow. Multiple patterns can be
matched at the same time.

```
type color = Red | Yellow | Green

let wait = function (* good *)
  | Red     -> seconds 30
  | Yellow  -> seconds 10
  | Green   -> seconds 0

let wait color = (* bad *)
  if color = Red then seconds 30
  else if color = Yellow then seconds 10
  else seconds 0
```

## Avoid using references

Most code does not require references. Introducing them should be well
justified. In particular, using references to implement loops and
similar local control flow is probably avoidable.

### naming conventions for types, values, modules, signatures
### Split imperative and functional code

### Introduce and Document Interfaces

Module interfaces are the best way to document and control an
implementation - employ them widely.

A module encapsulates code serving a specific purpose. An interface
should hide implementation details and document the API.

In [OCaml], every file `abc.ml` is a module and should come with an
interface `abc.mli`. In addition, a module can be part of a (file)
module using `struct .. end`. This is the only way to define functors.

```
module ID : sig
  type t

  val make: unit -> t
  (** [make] creates a unique token *)

  val equal: t -> t -> bool
  (** [equal] is true, if and only if two tokens are the same *)
end = struct
  type t = unit ref
  let make () = ref ()
  let equal x y = x == y (* use pointer equality *)
end
```

* Add list of examples here


### Avoid introducing new dependencies

Introducing new dependencies on outside libraries needs to be well
justified.

With Opam it is easy to install libraries and to use them. It is usually
better to use a well-established and maintained library than to roll
your own. This is especially true for well-established protocols,
formats and problems in general. But any new library also brings along
the responsibility to watch its development.

### Logging


### Compare Functions

[OCaml] provides a generic [compare] function but it can give unexpected
results when comparing complex values or not a desired order. This is
the reason that the [Map.Make] functor requires to define a compare
function.  Here is a recipe to define a custom compare function:

```
module Time = struct
  type 'a t =
    { hour:     int
    ; minutes:  int
    ; seconds:  int
    ; info:     'a
    }

  let (<?>) c (cmp,x,y) =
    if c = 0
    then cmp x y
    else c

  let compare t1 t2 =
    compare t1.hour t2.hour
    <?> (compare, t1.minutes, t2.minutes)
    <?> (compare, t1.seconds, t2.seconds)
end
```

### Tail Recursion

Recursive functions operating on large data structures need to be tail
recursive as otherwise the runtime stack may overflow.

While tail recursiveness is generally desirable, it is often not
required because data is not large. For performance it is usually better
to pay attention to allocation patterns than to tail recursion.


### complexity
### exceptions
### commit messages
### functions - argument order
### functions - pattern matching

