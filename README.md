---
title: "Advent of Code 2019"
output: html_document
css: modest.css
---
Code to solve the [Advent of Code](http://adventofcode.com/2019/) puzzles. This year, I'm using the puzzles to develop my skills in [Haskell](https://wiki.haskell.org/Haskell). I'm writing up a [commentary on these puzzles and my solutions](https://work.njae.me.uk/tag/advent-of-code/) on my blog.

[Learn you a Haskell](http://learnyouahaskell.com/chapters), [Introduction to Haskell 98](https://www.haskell.org/tutorial/index.html), and [Hackage](https://hackage.haskell.org/) are good resources.

The [Stack documentation](https://docs.haskellstack.org/en/stable/README/) and [How I Start: Haskell](http://howistart.org/posts/haskell/1/) are good sources of using the tools. 

# Toolchain

I'm using the basic Haskell Platform installation, together with `stack` to manage the packages and dependencies (install with
```
$ sudo aptitude install haskell-platform haskell-stack
```
), then updgrade with
```
 stack upgrade --binary-only
```
as the version in the Ubuntu repos is too old to work with current Haskell Stack package sets.

## Creating the repository and project
Create the repository as normal: create the project in Gitolite, clone it, and insert the `.gitignore` and `README.md` files.

There's one package per day, with the code for each package in sub-directories of the root directory. 

Create the basic `stack` project. This will create a new directory. Note that this new directory name can't have a hyphen-delimited word that's just digits, so the project will have to be `advent-of-code`

```
stack new advent-of-code --bare simple
```

Modify the `stack.yaml` file as needed, such as adding the `ghc-options` stanza. 

## Creating subsequent days

Each day lives in a separate directory, with its own `package.yaml` file and code in the `src` directory. (I based this configuration from [mstksg's setup](https://github.com/mstksg/advent-of-code-2018).)

Compile with
```
stack build
```
or 
```
stack build advent01
```

Run with
```
stack exec advent01
```

If you want to pass in additional RTS parameters, do it like this:
```
stack exec -- advent01 +RTS -K0 -RTS
```

Run interactively with
```
stack ghci advent01
```
or 
```
stack ghci advent01:exe:advent01
```
if the first form is ambiguous. 

To profile, use 
```
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" advent01
```
then run with
```
stack exec --profile -- advent01 +RTS -p -hy
```
Generate the profile graph with
```
stack exec hp2ps advent01.hp
```

# Packages

Stack is using the [14.16-lts resolver](https://www.stackage.org/lts-14.16) for packages, so make sure you read the [correct documentation for the packages included in it](https://www.stackage.org/lts-14.16/docs).

When you use a new package, use 

```
stack solver
```
to see how the `stack.yaml` file needs to change, and 
```
stack solver --update-yaml
```
to implement the changes.

# Readme

Build this readme file wth
```
pandoc -s README.md > README.html
```

(Using the [Modest style](https://github.com/markdowncss/modest).)
