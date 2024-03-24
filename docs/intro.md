## Introduction

Logic programming is based on Horn Clauses, which have a very simple set of operations that allow any computation to be created. The idea is simple, elegant and very powerful.

Horn Clauses however, are their own worst enemy because they do not play well with computers. Instead they live in a different world in which everything is enclosed, and all knowledge about the world exists within the program being run. This is both the best and worst part about using Horn Clauses exclusively for programming.

The Purity library tries to bridge the gap between the logic world and the computer world, allowing for pure Horn Clause programs to be written while allowing the programmer to more easily write code, and the computer to efficiently run the program.

## Motivation

This project started out when creating a [BrainF\*\*k interpreter](https://github.com/da-poodle/brainfuck) that didn't use any inbuilt predicates, and then after realising that it is actually possible to create complex programs using _only_ Horn Clauses, went on to do some experiments. To be honest, I think the experiments went a bit too far and now have created Purity! That being said, there was a purpose to my madness, I was interested in creating a Horn Clause only Prolog interpreter, but didn't think it would be useful if there was no code to run on it! The interpreter is still in the works, but the code is getting there.

## Download

To get the module version, read the quick start on how to install the SWI-Prolog package, or download the modules from
[github](https://github.com/da-poodle/purity)

## How To Use Purity

- [Quick Start](quickstart.md)
- [Foundations of Horn Clause only code](foundations.md)
- [Creating custom Domains](domains.md)
- [Horn Clause Only Meta predicates](meta.md)

## API Reference

The library API is getting bigger all the time, and so far the following is available

- [Domains](api_domains.md)
- [Core](api_purity.md)
- [Characters](api_characters.md)
- [Strings](api_strings.md)
- [Lists](api_lists.md)
- [Sets](api_sets.md)
- [Unary Numbers](api_unary.md)
