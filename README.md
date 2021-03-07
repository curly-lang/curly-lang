# Curly
Curly is a functional programming language that focuses on iterators. Some of its main implementation features include sum types, iterators, list comprehensions, and quantifiers.

## Example
```ocaml
module Main

fib =
    let fib_tail a: Int, b: Int, n: Int =
        if n == 0 then
            a
        else
            fib_tail b (a + b) (n - 1)
    in fib_tail 0 1

main = debug $ fib 50

```

## Build
Just type in the following:
```bash
git clone https://github.com/curly-lang/curly-lang && cd curly-lang && cargo build
```
This project depends on either `clang` or `gcc` for compiling, which can each be installed using your favourite package manager (`apt`/`pacman`/`dnf` for Linux and Homebrew/MacPorts for macOS).

Note: This repo has only been tested on Arch Linux, but should work on all other platforms rust supports without much issue.

## Progress
See TODO.md. Everything is highly experimental. Be cautious: code may be explosive.

## Support
Come to the [official discord server!](https://discord.gg/Gxfr6JDecv)
