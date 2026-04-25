# Untyped Lambda Calculus REPL

A minimal, zero-dependency REPL for the untyped lambda calculus, implemented in Haskell. It features a hand-rolled parser combinator library, capture-avoiding substitution with automatic α-conversion, normal-order reduction to full normal form, and a small set of REPL meta-commands. I've avoided dependencies as an exercise to demonstrate as much of the theoretical logic as possible.

## Motivation

Haskell is, at its core, a typed lambda calculus manifested as a practical (for varying definitions of "practical") programming language. Implementing the untyped lambda calculus from scratch is a natural exercise for anyone who wants to understand the foundations on which Haskell was created. This project demonstrates that connection directly: the same concepts -- abstraction, application, substitution, and reduction -- are the foundation of both systems.

## Architecture

The program is organized into six sections:

### Abstract Syntax Tree (`Expr`)
```haskell
data Expr
  = Var String       -- Variables: x, y, foo
  | Lam String Expr  -- Abstractions: λx. e
  | App Expr Expr    -- Applications: e1 e2
```
The AST is intentionally minimal. It uses bare `String` names for variables, which keeps the code readable while the substitution layer handles name clashes.

### Pretty-Printer (`pretty`)
Renders `Expr` values as human-readable strings with minimal parentheses. It respects the standard precedence rules: application is left-associative.

### Substitution (`subst`, `freeVars`, `fresh`)
Implements capture-avoiding substitution `[e/x]body`:
- Computes free variables to detect capture risks.
- Automatically α-renames bound variables (appending primes `'`) when a name clash would capture a free variable from the substituted expression.

### Evaluation (`reduce1`, `normalize`)
Performs **full normalization** (not just weak head normal form). This means it reduces inside lambda bodies, so `λx.(λy.y) z` becomes `λx.z`. If you want call-by-name behavior, you'd need to stop at the `Lam` case.:
- `reduce1` takes a single reduction step, always choosing the leftmost-outermost redex first.
- `normalize` iterates `reduce1` up to a configurable step limit, returning the final term and the number of steps taken.

Because the evaluator reduces inside lambda bodies, it produces full normal forms, not merely weak head normal forms.

### Parser (`Parser`, `parseExpr`)
A small parser-combinator library built from scratch on top of Haskell's `Applicative` and `Alternative` typeclasses:
- Lexical analysis is separated from grammar rules via `lexeme` and `symbol`.
- Identifiers are alphanumeric and may contain primes (`x'`).
- Application is parsed as left-associative juxtaposition.
- Unicode lambdas (`λ`, `Λ`) are normalised to backslash (`\`) before parsing.

### REPL (`main`, `loop`)
An interactive loop that:
- Parses user input.
- Normalises the resulting expression.
- Reports whether the term reached a normal form or hit the step limit.
- Supports a small set of meta-commands.

## Usage

### Building

You need GHC (the Glasgow Haskell Compiler) installed.

```bash
ghc Main.hs -o lambda-repl
./lambda-repl
```

Or with `runghc` for quick testing without compilation:

```bash
runghc Main.hs
```

### The REPL Prompt

```
Untyped Lambda Calculus REPL
Type :help for usage information.

λ>
```

### Entering Expressions

You can use backslash or unicode lambda (lowercase 'λ' or uppercase 'Λ'):

```
λ> (\x.x) y
Parsed: (λx. x) y
Result: y
Normalized in 1 step(s).

λ> (\x.\y.x) a b c
Parsed: (λx. λy. x) a b c
Result: a c
Normalized in 2 step(s).

λ> (\f.\x.f x) x
Parsed: (λf. λx. f x) x
Result: λx'. x x'
Normalized in 1 step(s).
```

### Meta-Commands

| Command | Description |
|---------|-------------|
| `:quit`, `:q` | Exit the REPL. |
| `:steps N` | Set the maximum reduction step limit (default: 1000). |
| `:help`, `:h` | Display usage information. |

Example:

```
λ> :steps 10
Step limit set to 10

λ> (\x.x x)(\x.x x)
Parsed: (λx. x x) (λx. x x)
Result: (λx. x x) (λx. x x)
Step limit (10) reached — not in normal form.
```

## Limitations

- **Single-parameter lambdas only.** The parser does not accept sugared multi-parameter syntax like `\x y. e`. You must write nested abstractions: `\x.\y.e`.
- **No top-level definitions.** The REPL does not maintain an environment of named terms. Each expression is evaluated in isolation.
- **No tracing.** There is no built-in way to inspect intermediate reduction steps; only the final result is shown.
- **Fixed naming convention for fresh variables.** The α-renamer appends primes (`'`) to conflicting names. For very large terms this is correct but not the most efficient strategy.
- **No type system.** This is the *untyped* lambda calculus; all terms are accepted and evaluated, including non-terminating ones (which are caught by the step limit).

## Roadmap

Potential features to add in the future:

- **Top-level definitions** (`true = \t.\f.t`) so the REPL can accumulate a persistent environment.
- **Trace mode** (`:trace on`) to print every intermediate reduction step.
- **Church encoding library** with numeric literals and pre-defined combinators (booleans, numerals, pairs, `Y`, `Ω`).
- **Alternative evaluation strategies**: call-by-name (weak head normal form) and call-by-value.
- **De Bruijn index display** (`:show debruijn`) for teaching α-equivalence.
- **Simply Typed Lambda Calculus** extension with a bidirectional type checker.
- **Property-based testing** with QuickCheck to verify confluence and substitution laws.
