# Pancake Verifier

The Pancake Verifier is a verification tool for the Pancake systems programming language, built on the [CakeML](https://cakeml.org) verified compiler.
It transpiles Pancake code into the Viper intermediate language and uses the Viper toolchain for formal verification.

## Getting the verifier

The verifier is available as:
 - A standalone CLI program called `pancake2viper`, downloadable [here](https://github.com/alegnani/pancake-verifier/releases/)
 - An interactive online website [Pancake Playground](https://trustworthy.systems/pancake-playground/)
<!-- 
 - A VS Code extension, available [here](https://marketplace.visualstudio.com/items?itemName=alegnani.pancake-ide).
-->

## Dependencies

- JDK11 or newer
- Viper toolchain v4.3.1 (available as part of the [VS Code extension](https://marketplace.visualstudio.com/items?itemName=viper-admin.viper) with "Install Specific Version..." or as a standalone [release](https://github.com/viperproject/viper-ide/releases/tag/v4.3.1).
- CakeML compiler rev. [Job 2960](https://cakeml.org/regression.cgi/job/2960) 
- z3 (included in the Viper toolchain) 

### Configuration

 - The path to the CakeML compiler can be set via the `--cake <CAKE_PATH>` flag or by ensuring `cake` is in your system PATH or `$CAKE_ML` is set.
 - The path to `viperserver.jar` can be set via the `--viper <VIPER_PATH>` flag or by setting `$VIPER_HOME`.
 - The path to the `z3` executable can be set via the `--z3 <Z3_PATH>` flag or by setting `$Z3_EXE`.

A compatible z3 binary is included at `ViperTools/z3/bin/z3` when using the Viper VS Code extension, which is typically installed at `~/.config/Code/User/globalStorage/viper-admin.viper/Stable/ViperTools/` (Linux) or `~/.vscode/extensions/viper-admin.viper/dependencies/ViperTools` (Mac).

## How to run

<!-- 
### Standalone
-->

To transpile a Pancake file to Viper: 
```bash
pancake2viper transpile foobar.🥞 foobar.vpr
```

<!-- 
To verify a Pancake file: 
```bash
pancake2viper verify foobar.🥞
```
-->

Also supports input via stdin:
```bash
cat foo.🥞 bar.🥞 | pancake2viper verify -
```

<!-- 
To verify a Pancake program running on a 32-bit architecture and a static heap of 1 KiB:
```bash
pancake2viper verify --word-size 32 --heap-size 1024 foobar.🥞
```
-->

<!-- 
### VS Code Extension

Currently the extension is a bit more limited in functionality being stuck on an old version of `pancake2viper`.
On opening or modifying a Pancake file(.🥞 or .pnk) a Viper file is generated with the same name. This file is kept in sync with the Pancake source.
From the Command Palette (Ctrl+Shift+P) the current file can be verified using the `Pancake Verifier: Verify file` command.
-->

## Pancake annotations

```c
fun sum(1 n) {
    /@ requires 0 <= n @/
    /@ ensures retval == n * (n + 1) / 2 @/

    var i = 0;
    var accu = 0;
    while (i < n) {
        /@ invariant 0 <= i && i <= n @/
        /@ invariant accu == (i - 1) * i / 2 @/

        accu = accu + i;
        i = i + 1;
    }
    return accu + n;
}
```
Annotations in Pancake are specified using block comments (`/@ ... @/`) and are mostly the same as the ones available in Viper. It is recommended to first check out the (well written) [Viper tutorial](https://viper.ethz.ch/tutorial).

Note that they have to be inside a block (`{...}`) and not outside, as is the case in Viper.
```c
// Viper
method foo_bar() 
    ensures ...
{
    while true 
        invariant ...
    {
        ...
    }
}

// Pancake
fun foo_bar() {
    /@ ensures ... @/
    while (true) {
        /@ invariant ... @/
        ...
    }
    return 0;
}
```
The supported Viper annotations are: `requires` for preconditions, `ensures` for postconditions, `assert`, `refute`, `invariant`, `assume`, `inhale` (assume + gain of access permission),`exhale` (assert + loss of access permission), `fold` and `unfold` to fold/unfold predicates.

Annotations can use the arithmetic operators (`+`, `-`, `*`, `/`, `%`), logical operators (`!`, `&&`, `||`, `==>`, `<==>`) and comparison operators (`==`, `!=`, `<`, `<=`, `>`, `>=`) from Viper.
Annotations use Viper's semantics ,e.g. arithmetic operations are unbounded, but the following Pancake operators are supported:
| | |
|---|---|
| Unsigned comparisons | `<+`, `<=+`, `>+`, `>=+` (Not yet implemented) |
| Bitwise operations | `&`, `\|`, `^` (`!` is the boolean operator; TODO: bitwise NOT) |
| Shift operations | `<<`, `>>`, `>>>` |

Annotations support both an existential and a universal quantifier:
```c
forall i: Int :: { optional trigger(s) } 0 < i && i <= 10 ==> ...
exists i: Int :: 0 < i && i <= 10 ==> ...
```

All Pancake variables and arguments can be used in annotations (given that they are in scope).
The return value of a Pancake function can be accessed as `retval`.
For the result of a Viper function use `result` instead.


Some built-in functions are provided for convenience:
| | |
|---|---|
| `bounded8(x)`, `bounded16(x)`, `bounded32(x)`, `bounded64(x)` | Checks whether or not `x` is guaranteed to fit in a `u8`, `u16`, `u32`, `u64` |
| `bounded(x)` | Checks whether or not `x` is guaranteed to fit into a word |

### Reasoning about the heap

To reason about the heap of a Pancake program the `heap` variable can be used. The heap is represented as a word-indexed array of words.
`heap[@base]` is the first element of the heap.
To specify the access permissions of a region of the heap the Viper function `acc` is used e.g. `acc(heap[0])`.
For convenience the `acc(heap[0..42])` syntax can also be used for a region spanning more than a single word. Note that the upper bound is exclusive.

```c
fun main() {
    /@ requires acc(heap[@base..@base+2], write) @/
    /@ requires heap[@base] == 42 @/

    /@ ensures acc(heap[@base..@base+2], write) @/
    /@ ensures heap[@base] == old(heap)[@base] @/ // unchanged
    /@ ensures heap[@base+1] == heap[@base] @/

    var x = lds 1 @base;
    st @base + @biw, x;
    return 0;
}
```

> [!NOTE]
> The `heap[l..u]` syntax is can only be used inside of an `acc`. `heap[x..y] == heap[z..v] is (unfortunately) not going to work.

### Reasoning about shapes

Pancake compound shapes, like `<1, 2, <3, 4> >`, can be reasoned about in annotations.
This can be done by comparing single elements via the dot syntax (`foo.0 == bar.1.2`) or via structural equality (`foo == bar.1`).
Shapes can be return values or arguments of a Pancake function or the argument of Viper predicates or functions.
When used as an argument in Viper predicates or functions use the shape as the type.
```c
/@ predicate shape_pred(shape: {1, 2}) {
    shape.0 > 42 && shape.1 == < 1, 2 >
} @/
```

### Reasoning about shared memory

Reasoning about shared memory is done by providing a separate Viper file here in referred to as a "model".
The model consists of Viper fields that hold state and Viper methods that specify the effect of shared memory operations.
This includes the preconditions that must hold before a shared memory read/write, the postconditions (including the value read from the shared memory location) and how this operation affects the device state.

#### Shared read/writes

In order to use `!stX/!ldX` operations we need to define how certain memory regions behave.
This is done by providing a a Viper method in the model with the following signature:
```c
// define the behaviour of a shared memory load
method load_<ident>(heap: IArray, address: Int) returns (retval: Int)
    requires address == ...
    ensures 0 <= retval < ...

// define the behaviour of a shared memory store
method store_<ident>(heap: IArray, address: Int, value: Int)
    requires address == ...
    ensures boundedX(value)
```
These methods can then be registered in our Pancake with the following top-level annotation:
```c
/@ shared <op-type> <op-width> <ident>[<address>] @/
// op-type = r | w | rw (only load, only store, load/store)
// op-width = u8 | u16 | u32 | u64
// ident = the identifier used in the Viper model file
// address = expr | lower_bound..upper_bound | lower_bound..upper_bound:stride
```
The upper bound is exclusive and the default stride is 0x1.
Formally, the addresses that will be included are: forall offset > 0: lower_bound + offset * stride < upper_bound

For example, if we have a board with two UARTs(at addresses UART0_BASE: 0x1000, UART1_BASE: 0x2000) we can register a shared memory region for the data register for both devices as follows:
```c
// UART_SIZE = UART1_BASE - UART0_BASE
/@ shared rw u8 UART_DATA[UART0_BASE..UART1_BASE + UART_SIZE: UART_SIZE] // registers the `UART_DATA` function for both 0x1000 and 0x2000
```

If memory regions are overlapping the transpiler will issue warnings. These can be disabled with `--ignore-warnings`.

Given that the model includes a lot of boilerplate, a skeleton can be generate from a Pancake file with `/@ shared ... @/` annotations using `pancake2viper generate foo.🥞 model.pnk`.

#### State and model invariants

In order to keep the state of our model we can use Viper `Ref`s in the model and access it's fields. As this requires access permissions we can define a predicate and use that as a state invariant.

```c
////// Model

field data: Int

// Define a state invariant
predicate state_invariant(state: Ref) {
    acc(state.data) && bounded8(state.data)
}

// Define a load method for !ld8 0x1000
method load_data(heap: IArray, state: Ref, address: Int) returns (retval: Int)
    requires state_invariant(state)
    requires address == 4096
    ensures unfolding state_invariant(state) in retval == state.data
    ensures state_invariant(state)
{
    retval := state.data
}

////// Pancake

// Provide declaration for the state reference, state invariant and shared memory load
/@ model field state @/
/@ model predicate state_invariant(state: Ref) @/
/@ shared r u8 data[0x1000] @/
```

#### Extern predicates and fields

If you want to use predicates or fields defined in the model in the Pancake code provide declarations as follows:
```c
/@ extern field <name>: <type> @/
/@ extern predicate <name> @/
```

### Other examples

Annotation examples, showcasing all of the features, can be found in the [test folder](https://github.com/alegnani/pancake-verifier/tree/main/pancake2viper/tests).
