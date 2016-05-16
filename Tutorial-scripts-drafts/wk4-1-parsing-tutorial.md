## Practical Parsing in Haskell using Parsec

### Installing Haskell on your system

The online environment is an easy and convenient way to get started with Haskell. However, it is limited in functionality because of security considerations (we can't allow loading external modules). So from this session on you'll need to have a Haskell installation on your own computer. The easiest way is to install the [Haskell Platform](https://www.haskell.org/platform/), it is available for all major operating systems and very easy to install, just follow the instructions.

### Using Haskell

There are two main ways to use Haskell: you can either use the interactive interpreter `ghci` or the compiler `ghc`. `ghci` is similar to the online interpreter, but more powerful as the security limitations of the online interpreter don't apply. It's a great tool for experimenting with code and [it can also be used as a debugger](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci-debugger.html).

For larger programs, typically consisting of many different modules, you will use the Glasgow Haskell Compiler, `ghc`. For building more complex programs and for installing existing modules from  [Hackage, the Haskell community's central package archive of open source software](https://hackage.haskell.org/), there is a separate tool called [Cabal](https://www.haskell.org/cabal/).

### A real-world parser using Parsec
