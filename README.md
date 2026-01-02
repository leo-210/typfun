# Typfun

Pronounced like "[Typhon](https://en.wikipedia.org/wiki/Typhon)", Typfun is a
basic functional programming language (it's a **typ**ed
**fun**ctional langage, thus *Typfun*).

This repository is an implementation of a parser for the language, as well as 
algorithm W, to type check a Hindley-Milner system.

## Syntax

The detailed syntax can be found in `./res/grammar.ebnf`.

```
// A comment

fn factorial(n) {
    if n = 0 or n = 1 then 1
    else n * factorial(n - 1)
}

// Tuple containing a string and a bool
let t = factorial 5, true in t
```

There is also the composition (`.`) operator, whose signature is : 
`. : a -> (a -> b) -> b`. It takes an argument, and a function to apply this
argument to the given function.
```
// println : str -> ()

// The following lines are equivalent
"hello world".prinln;
println("hello world");

fn add(a, b) { a + b }

let incr = 1.add in
incr(2)  // equivalent to incr(1, 2)
```

Finally, code inside brackets it interpreted as a function with no argument.
```
// Prints "hello" twice
let greetings = { "hello".println } in
greetings();
greetings()
```


## Type checker

The type checker assigns to every expressions a variable type. Depending on the
given code, those types need to follow constraints, which are solved by
unification.

For example, `a + b` has the type `int`, and enforces `a` and `b` to also be of
type `int`.

```
fn(a, b) { a + b } : int -> int -> int

fn f(a, b) { a } 
(f(1, true) + 1, f) : int * (int -> bool -> int)
```


## Credit

For the type checker, I could better understand how to structure my code thanks
to [this implementation of a Hindley-Milner system]
(https://github.com/toroidal-code/hm-ml/).
