
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

[Upenn]:  https://www.seas.upenn.edu/~cis341/current/programming_style.shtml
[OCaml Tutorial]: https://ocaml.org/learn/tutorials/guidelines.html

## Indentation, Line Length, File Names

## Naming and Declarations

### Constants

## Concurrency, Threads

## Error Handling

## Git - Commit Messages

## Special Topics


### if vs. match - use of semicolon
### prefer pattern matching over if-then-else
### avoid global open
### avoid deep nesting
### naming conventions for types, values, modules, signatures
### Split imperative and functional code
### Avoid references
### Introduce interfaces
### Documentation of interfaces
### indentation
### line length
### Avoid introducing new dependencies
### Logging
### equality: != and == vs <> and =
### how to write a compare function
### tail recursion
### complexity
### exceptions
### finally: resource de-allocation
### commit messages
### functions - argument order
### functions - pattern matching

### Avoid Opening Modules Globally

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


