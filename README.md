## README

A work-in-progress software analysis framework built around the [QBE] intermediate language.

### Motivation

Existing analysis frameworks are predominantly built around [LLVM].
Unfortunately, LLVM is a fast moving target with constant changes and updates to its intermediate language.
Therefore, tooling built around LLVM (e.g., [KLEE]) often requires dated LLVM versions.[^1]
Obtaining such ancient LLVM versions can be cumbersome and often hinders employment of these tools.
To overcome these issues, maintainers of analysis tooling need to constantly invest time to catch-up with LLVM releases, instead of focusing on improving their analysis framework.

In order to reduce maintenance burden, this project attempts to investigate utilization of another intermediate language for software analysis: [QBE].
QBE is a much [smaller scale project][QBE vs LLVM] than LLVM and thereby offers a higher degree of stability.
Further, QBE is much simpler than LLVM (e.g., providing fewer operations) and thereby also eases the implementation of certain analysis techniques.
Nonetheless, there are sophisticated compiler frontends that can emit a representation in the QBE intermediate language (which can then be analyzed using quebex!).
For example, [SCC], [cproc] or the [Hare compiler][Hare].

### Status

Proof of concept, not much to see here yet.
Come back later!

### Design Goals

This project is intentionally written in a simple subset of the [Haskell] programming language.
It should be usable by anyone with a basic Haskell background (e.g., as obtained by reading [Learn You a Haskell for Great Good!][learnyouahaskell]).
Further, the project should require minimal long-term maintenance and should also support older GHC versions.
Therefore, it uses the [GHC2021] language standard and avoids usage of additional language extensions.
Further, whenever possible, dependencies on external libraries that are [not bundled by GHC][GHC libraries] must be avoided.

### Planned Components

* `quebex`: Basic analysis library, which should provide
	* A modular interpreter for dynamic analysis based on monad transformers *[in progress]*
	* Basic framework for static analysis (CFG, call graphs, …) *[planned]*
* `quebex-syntax`: Parser for the QBE IL written in literate Haskell *[in progress]*
* `quebex-symex`: Symbolic execution based on the provided modular interpreter *[planned]*

Long term, it is also of interest to build lifters which generate QBE from binaries (e.g. with [libriscv]).

[^1]: At the time of writing, KLEE recommends LLVM 13 and the current version is LLVM 20.
[QBE]: https://c9x.me/compile/
[QBE vs LLVM]: https://c9x.me/compile/doc/llvm.html
[LLVM]: https://llvm.org/
[KLEE]: https://klee-se.org
[SCC]: https://www.simple-cc.org/
[cproc]: https://sr.ht/~mcf/cproc/
[Hare]: https://harelang.org/
[Haskell]: https://haskell.org/
[GHC2021]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html#extension-GHC2021
[GHC libraries]: https://ghc.gitlab.haskell.org/ghc/doc/libraries/index.html
[learnyouahaskell]: https://learnyouahaskell.github.io/chapters.html
[libriscv]: https://github.com/agra-uni-bremen/libriscv
