# Lambda Calculus
Untyped lambda calculus REPL

## Features
 - Lazy evaluation
 - Loading files into the enviroment
 - Saving the defined variables from the enviroment into a file
 - Usefull commands
 - A default enviroment that gets loaded when the program starts
 - Detect when the evaluation of an expression matches whith an expression already defined

## Building and running

Navigate to the root directory of the project and run the following command:

```bash
cabal run
```

## Usage
Evaluate an expression by entering it in the REPL

Note that reduced form is supported

```lambda
λ> (\p q.p q p) (\x y.x) (\x y.y)
λx y.y
```
Give expressions a name with '=' if you want to use them later

Names must start with uppercase and its expression wont be evaluated until it's needed
```lambda
λ> Two = \f a.f (f a)
λ> Three = \f a.f (f (f a))
λ> Succ = \n f a.f (n f a)
λ> Add = \n k.n Succ k
λ> Add Three Two
λf a.f (f (f (f (f a))))
```