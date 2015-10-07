# Engineering Log

I began this repository because of what is essentially a napkin note (but I sling my laptop in one arm and type with the other).
Rather than simply delete it and lose the associated thoughts, I will transfer its information here, with a more professional format.





## 6 Oct 2015

A basic problem in the design of low-level languages is the question of primitives:

 * A portable language needs to expose primitive types and operations suitable for programmers.

 * A low-level language must expose primitive types and operations which can be mapped directly (or else very closely) onto hardware.

These two points are in in conflict, and must be reconciled.

### Conditional Compilation

A common theme in resolving these kinds of tensions is conditional compilation, so I laid out a plan to build the smallest possible conditional compiler:

 * A language which only accepts literals, and would place them into the static initialized data segment.
 * Allow for passing configuration options to the compiler.
 * Offer conditional compilation.
 * Offer condition expressions: eq, neq, in, or, and, any, all.
 * The automatic detection of configuration options like `arch`, `arch-mode`, `binfmt`, `abi`, or `os` is an orthogonal concern. For now, pass these explicitly.

I eventually decided to use only nullable string configuration options.
Arguments are passed to the compiler process as simple strings, or else are omitted.
Files full of string configurations can be easily read by humans and tracked by development tools.
Strings are the lowest common denominator in communicating systems.
Nullable strings can be tested for presence and equality.
Importantly, the empty string should not be confused with a string which is not present.

I found that only a few expressions are needed to create an intuitive abstract syntax for conditinoal expressions.
The primitive expressions test for presence `<cfg>` and equality with a constant `<cfg> = <string>`.
The compound expressions are `not` (negation), `all` (multi-way and), and `any` (multi-way or).
Other expressions (`in`, `!=`, `and`, `or`) are recommended in the concrete syntax as syntactic sugar.

Passing configuration options to the compiler requires some care to ensure the command-line arguments are familiar.
Below is a possible solution:

	* To set a cfg, use `--cfg <cfg>=<value>`, or `--cfg <cfg>` (which is equivalent to `--cfg <cfg>=''` and `--cfg <cfg>=`)
	* To unset a cfg, use `--no-cfg <cfg>`. This is only useful when the user wants to be very explicit, or when they have to override some existing framework's premature decisions.
	* <cfg> must match ascii regex `/[a-zA-Z][a-zA-Z0-9]*/`. This extraordinarily low denominator should be selected so that every system interoperates.

### Enforced Portability

Originally, I had expected to be able to require source files to declare their range of protability explicitly.
The compiler could then exhaustively check all conditional compilation branches for correctness, even before attempting code generation.

My first inkling that this could not happen was scoping rules.
If a function could be conditionally defined or declared, the compiler would need to track not only information about the function's name and type, but also the conditions under which the function is available for use.
That would not be a particularly difficult barrier on its own, but the conditions user might resonably define could be highly complex.
The compiler would not have a mere handful of possible targets, but a multitude:
even in x86 programming, someone might wish to condition their code depending on the multimedia extensions available on a particular processor model.
This goes well beyond the bounds of enumerating a few architectures and opertating systems.

Thinking more on this topic, I decided that only the maintainers of a project has the ability to test the implementation against multiple targets.
Furthermore, the maintainers can decide for themselves how broad or detailed their requirements go.
Finally, since any compiler I envisage must be a cross-compiler through-and-through (like the Plan 9 C compiler), the writer should be able to simply attempt compilation onto all desired targets using a single machine.
That is, built-in portability testing adds complexity, but no value, to a well-designed compiler.

### Primitive Types

In studying high-level languages, the only properties of primitive data are:
1) the literal representation of the data,
2) the type attributed to the data, 
3) the primitive operations with which that data may be involved
In low-level languages, to additional concerns are apparent:
4) the size of that data
5) the required alignment of that data

When building a machine-oriented compiler, it will be difficult to allow free reign to primitive operations.
In particular, even if the code generator could be configured to generate appropriate machine code (it probably could), the optimizer would also have to be configured with highly detailed information including compute time in cycles, effects on registers, and worse, effects on memory.

As such, I have decided to select a few widely-implemented groups of primitive operations:
 * address
 * offset
 * unsigned integer
 * signed integer (two's complement)
 * binary floating point (IEEE 754)
 * decimal floating point (IEEE 754)
A few more groups may be anticipated (such as for binary-coded decimal), and there may be additional formats added as computer science evolves, but several are left out intentionally.
In particular, complex numbers are better understood as a type constructor (Complex i32, Complex f64, &c) than a single type, and the same goes for rational numbers and intervals of numbers.
Further, characters and strings are encoding-dependent, rather than machine-dependent; I'd rather not select a particular encoding only to see it go obsolete, and bring the language with it.
Finally, operations on fixed-point numbers are by design implemented with the same instructions as integers, with the exception of conversion routines.

Given this small set of clear semantics, a general optimizer can make short work of reducing instruction sequences.
A machine-specific optimizer can also be deployed whenever we want to squeeze yet more performance out.
For a machine to target a data type we need only:
1) Inform the compiler that the machine supports the triple `<operation group, size, alignment>`.
2) Provide code generators for the primitive operations.
3) Provide a literal syntax, or else link in constructors.

#### Bits and Booleans

The question of bits also becomes important if we wish to represent bitfields.
Indeed, in a systems programming language, we probably must allow bitfields.
Even if we don't allow the full power of bitfields, packing multiple boolean flags into a single word will require some consideration of individual bits.