# tiger-haskell
Modern compiler implementation in Haskell (Tiger programming language)

This is an attempt to create a tiger programming language compiler
since I wanted to learn Haskell while doing something fun. A year
ago I took a Yale CPSC 421 class and we implemented a compiler for
the same language in SML. We had a lot of boilerplate code back
then, but this time I'd try to make everything from scratch.

I am very new to Haskell and I probably make a lot of mistakes. If you spot
anything bad, please let me know! I would greatly appreciate it!

# Current progress
IR translation is done. You can run `make semantdriver` to get an executable,
it will type check a tiger source file and print out the intermediate language.

I recently cleaned up the project of bad dependencies, and made a dependency
graph of the important modules in the project. It's still not as pretty as I
would like it to be, but you can check it out here:
![Dependency Graph](https://docs.google.com/drawings/d/1Uzp7IItQQETjUyGEWT7u9lKiqtFaDYzjdnlsTevO1aE/pub?w=609&h=410)

I would progressively clean up more of the denpendency relationship so there'll
be cleaner separations between layers of modules.
