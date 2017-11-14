
# OCaml Towards Clarity and Grace

This is a guide towards writing better [OCaml] code.  We all like to
write [OCaml] code that is correct, maintainable, efficient, and maybe
even beautiful. The sad truth is that this cannot be achieved simply by
following some rules. Just like it is not enough for a book to be
composed of correct sentences and coherent paragraphs. However, a book
violating these principles stands little chance of finding many readers.
Likewise, this guide tries to help with the small structures in
programming [OCaml] upon which we can hope to build bigger ones.

File [README.html](README.html) is a self-contained HTML page of this
document and includes a table of contents.

[OCaml]:  https://www.ocaml.org/

## Other Resources

* [Upenn]'s style guide for student projects
* [OCaml Tutorial]
* [OCaml Manual]
* [Real World OCaml] Book

[Upenn]:            https://www.seas.upenn.edu/~cis341/current/programming_style.shtml
[OCaml Tutorial]:   https://ocaml.org/learn/tutorials/guidelines.html
[OCaml Manual]:     http://caml.inria.fr/pub/docs/manual-ocaml/index.html
[Real World OCaml]: https://realworldocaml.org/

## Indentation, Line Length

* Indentation must always reflect the logic of a program.

* In existing code, adopt the existing style for indentation: tabs or
  spaces.

* Line length should not exceed 100 characters. Break up long lines in
  new code in particular.

* For new code, prefer spaces over tabs and use [ocp-indent] to maintain
  consistent indentation. Note that [ocp-indent] does not break up long lines.

* Most OCaml code bases uses 2 spaces per indentation level.

* When making changes ensure that indentation is still correct after your change,
  re-indenting as necessary, but not excessively

Rationale:

* Overly long lines create problems when printing source code and when
  resolving merge conflicts in tools by forcing a lot of sideways
  scrolling.
* Spaces are unambiguous in meaning whereas tabs depend on the
  environment.
* [ocp-indent] is a proven way to re-establish consistent
  indentation while still leaving room for other aspects of code
  formatting.
* When adding an 'if' expression it might be necessary to reindent the whole body of a function,
  but avoid re-indenting the whole file, which causes merge conflicts with people working on
  other branches, it is best to plan ahead and do such large scale changes as separate commits.

[ocp-indent]: https://github.com/OCamlPro/ocp-indent

## Comments

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
* The purpose of a value or function in a signature (interface)
* The purpose of a type declaration or its components, if not obvious
* The purpose of record components and variants in types, if not obvious
* Unusual Algorithms and their complexity
* Invariants when not expressed as assertions
* Error handling
* Basic examples on how to use the library
* Known limitations
* Short introduction to the technology covered by the library, if not obvious

### What not to Comment

* Purpose of a local let binding - the name should tell it
* Every line in function

### Examples

* [Ocaml List](https://github.com/ocaml/ocaml/blob/trunk/stdlib/list.mli)
  The documentation of the standard list module. Each function is
  commented below its signature.

* [Container Hash Set](https://github.com/c-cube/ocaml-containers/blob/master/src/data/CCHashSet.mli)

* [Mtime](https://github.com/dbuenzli/mtime/blob/master/src/mtime.mli)
  with [implementation](https://github.com/dbuenzli/mtime/blob/master/src/mtime.ml)
  and [rendered documentation](http://erratique.ch/software/mtime/doc/Mtime)
  
* [Uunf](https://github.com/dbuenzli/uunf/blob/master/src/uunf.mli)
  with [implementation](https://github.com/dbuenzli/uunf/blob/master/src/uunf.ml) and
  [rendered documentation](http://erratique.ch/software/uunf/doc/Uunf)
  
* Introduction to the domain covered by the library, e.g. introduction to unicode in [Uucp](http://erratique.ch/software/uucp/doc/Uucp.html)

* Documentation stored together with code: squeezed [design](https://github.com/xapi-project/squeezed/tree/master/doc/design) and [diagrams](https://github.com/xapi-project/squeezed/blob/169e2e3004082a129b95ed6184a0ab04d20b7f28/lib/memory.ml#L91-L117)

### Caveats

Code duplication is bad but copying comments is worse. Be very careful
when copying comments to make sure they are not becoming misleading in a
new context.

### Viewing rendered documentation

You can use `odig odoc && odig doc` to view the documentation of all installed packages.
If your package uses jbuilder then you can also view the documentation of 
the package you are working on with `jbuilder doc`.

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

General considerations:

* Local names can be short, type variables very short.
* Use scoping (let, modules) to keep the number of names in a scope small.
* Avoid encoding the type into a name: `x_int` or `x_opt` is usually not
  better than `x`.
* In a functional language like [OCaml], using `get` as part of a name
  is often redundant unless it involves obtaining a value from a database
  or file.
* You may see auto-generated code use the style `.mIX_case` where `.MIX_case` was meant,
  but record fields cannot start with capital letters.
  Avoid this in code your write.

Order of declarations: in a module, typically the following order is
maintained unless dependencies force a different order or mixing
declarations:

1.  Exceptions
2.  Types
3.  Modules
4.  Values

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

## Scoping

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

* Structure long literal numbers with underscores for
  readability as in `0b1111_1111` above. It works in all bases, like for
  example in `1_000_000`.

More details are in the [Ocaml Manual](http://caml.inria.fr/pub/docs/manual-ocaml/lex.html).


## Introduce and Document Interfaces

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

### Do This Instead

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


### Rationale

Opening a module introduces all its values and constructors into the
local module. Since the definitions of these values and constructors are
not visible, it becomes very hard to understand the code without tool
support. While a developer might argue that tool support is available,
it is not during reviews on GitHub or when looking at a printout. The
problem is exaggerated when several modules are opened.

## Avoid using references

Most code does not require references. Introducing them should be well
justified. In particular, using references to implement loops and
similar local control flow is probably avoidable.

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

## Error Messages and Error Handling

* For handling errors programmatically, appropriate error values should
  be defined to avoid matching against strings (The standard library is
  violating this principle.)

* Log messages are typically created from strings. [OCaml] defines the
  following strings to report the location of an error: `__LOC__`,
  `__FILE__`, `__LINE__`, `__MODULE__` and a few more -- see
  [Ocaml Pervasives](https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html)

* Constructing log and error messages can be made simpler by defining a
  function that behaves like `Printf.sprintf`:

```
type 'a t = Ok of 'a | Error of string
let error fmt = Printf.kprintf (fun msg -> Error msg) fmt
```

* Messages going into a log still must make sense in the context of many
  other lines before and after in the log. Therefore they must contain
  enough detail.

* Don't split log messages because in the actual log they be no longer
  next to each other.


## Split imperative and functional code

Purely functional code is easiest to test. Therefore code should be as
functional as possible and imperative code minimised.


## Functions - Argument Order

In a function definition, the most common arguments should come first.

While a function can work with any order of formal arguments, putting
common arguments make it more usable.  For example:

```
let sum     = List.fold_left (+) 0
let default x = function
  | None   -> x
  | Some y -> y

let sum' xs  = xs |> List.map (default 0) |> sum
```

Function `sum: int list -> int` computes the sum from a list of
integers. It's definition is so concise because `List.fold_left` takes
its list argument last. Likewise, `default` and `List.map` combine well
because of the order of their arguments.

The best argument order is not always clear - so this should be just a
general consideration when writing code.

## Functions - Pattern Matching

Pattern matching is the preferred way to define functions. Patterns
are easier to read and extend than if-then-else expressions. Consider
matching multiple arguments at the same time to arrive at a short and
un-nested function definition.

The example below defines a binary tree and a function `sum` that sums
up the nodes along a path in the tree. It uses pattern matching
effectively by matching over two values (the current node, and the
current position in the path) at the same time.

```
type 'a tree
  = Empty
  | Node of 'a tree * 'a * 'a tree

type branch = Left | Right
type path   = branch list

let sum tree path =
  let rec loop acc path tree = match path, tree with
    | []          , _           -> acc
    | _::_        , Empty       -> failwith "path too long"
    | Left::path  , Node(l,n,r) -> loop (acc+n) path l
    | Right::path , Node(l,n,r) -> loop (acc+n) path r
  in
    loop 0 path tree
```

## Functions - Data Flow

The readability of code can often be improved by using the pipe operator
`|>` because it allows functions to be written such that data flows from
top to bottom and left to right.

Consider the following function `numbers` and `numbers'` that compute a
string from a list of optional numbers:

```
let xs = [Some 1; None; Some 2; None; None; Some 3; Some 4; Some 5] in

let (++) x xs = match x with
  | Some x -> x :: xs
  | None   -> xs

let numbers xs =
  String.concat "," (List.map string_of_int (List.fold_right (++) xs [])

let numbers' xs =
  List.fold_right (++) xs []
  |> List.map string_of_int
  |> String.concat ", " ```
```

The definition of `numbers` is more traditional and `numbers'` uses the
pipe operator. In the latter the data from the argument `xs` flows
through the function definition from top to bottom and left to right.
In the more traditional definition, data flows from right to left.






## Functions - Avoid Deep Nesting

Code that is deeply nested is hard to read and hard to test. This
suggests that it should be restructured - probably by introducing new
functions. Pattern matching is a another proven way to reduce the
nesting of code.


## Functions - Tail Recursion

Recursive functions operating on large data structures need to be tail
recursive as otherwise the runtime stack may overflow.

While tail recursiveness is generally desirable, it is often not
required because data is not large. For performance it is usually better
to pay attention to allocation patterns than to tail recursion.

Be aware that tail recursion can be inhibited by exceptions: the `loop`
below is not tail recursive!

```
let read_lines inc =
   let rec loop acc =
     try
       let l = input_line inc in
       loop (l :: acc)
     with End_of_file -> List.rev acc
   in
   loop []
```

To make it tail recursive, handling of values and exceptions need to be
combined into one `match` expression (available since OCaml 4.02):

```
let read_lines io =
  let rec loop acc =
      match input_line io with
      | l -> loop (l :: acc)
      | exception End_of_file -> List.rev acc
  in
  loop []
```

Function calls can be annotated to receive a compiler warning if a call
is not tail recursive as expected:

```
let read_lines io =
  let rec loop acc =
    try
      let l = input_line io in
      (loop [@tailcall]) (l :: acc)
    with End_of_file -> List.rev acc
  in
  loop []
```

The problem with this is that one has to be aware of the problem in the
first place to write such an annotation.


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


## Compare Functions

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


## Git - Commit Messages

* The subject should express what the commit achieves
* The body should provide context for the commit
* Subject and body should be clearly separated and working independently
* Commit messages must not exceed a line length of 80 characters
* Commits should include a signature (`git commit -s`)

Examples:

* https://github.com/xapi-project/xenopsd/pull/415/commits/fb6da3e9f9d5c1a17ee5ab3a904e5d90306980a4
* https://github.com/ocaml/ocaml/commit/63e1460a69758b029c2def42192f80faaac1b7b2


## Use Pattern Matching for Value Destruction

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

## Avoid introducing new dependencies

Introducing new dependencies on outside libraries needs to be well
justified.

With Opam it is easy to install libraries and to use them. It is usually
better to use a well-established and maintained library than to roll
your own. This is especially true for well-established protocols,
formats and problems in general. But any new library also brings along
the responsibility to watch its development.

