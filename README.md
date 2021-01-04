# Curly
Curly is a functional programming language that focuses on iterators. Some of its main implementation features include lazy evaluation, list comprehensions, and quantifiers.

## Example
<pre>
primes = n <span class="hljs-keyword">in</span> (from <span class="hljs-number">2</span>) <span class="hljs-keyword">where</span>
    <span class="hljs-keyword">for</span> <span class="hljs-keyword">all</span> p <span class="hljs-keyword">in</span> (range <span class="hljs-number">2</span> n)
        n % p != <span class="hljs-number">0</span>
</pre>

## Build
Just type in the following:
```bash
git clone https://github.com/jenra-uwu/curly-lang && cd curly-lang && make
```
This project depends on `libedit-dev`/`libedit-devel`/`readline` (for Linux and macOS respectively) and `llvm`, which can each be installed using your favourite package manager (`apt`/`pacman`/`yum` for Linux and Homebrew/MacPorts for macOS).

Note: On Fedora, you also need to install `llvm-devel`.

Note: This repo has been tested on macOS, Ubuntu, and Fedora as of now, but will not build on Windows. Windows support is coming soon.

## Progress
The parser is done, and the type checker is mostly done. Current effort is focused on the LLVM backend. Everything is highly experimental. Be cautious: code may be explosive.
