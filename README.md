<!--
SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>

SPDX-License-Identifier: GPL-3.0-only
-->

## README

A work-in-progress software analysis framework built around the [QBE] intermediate language.

### Motivation

Existing analysis frameworks are predominantly built around [LLVM].
Unfortunately, LLVM is a fast-moving target with constant changes and updates to its intermediate representation.
Therefore, tooling built on LLVM often requires dated LLVM versions (e.g., [KLEE] currently [recommends LLVM 13][KLEE LLVM] released in 2022).
Obtaining these LLVM versions can be cumbersome and often hinders employment of these tools.
To overcome these issues, maintainers of analysis tooling need to constantly invest time to catch up with LLVM releases instead of focusing on improving their analysis framework.

In order to reduce the maintenance burden, this project attempts to investigates the utilization of another intermediate language for software analysis: [QBE].
QBE is a much [smaller-scale project][QBE vs LLVM] than LLVM and thereby offers a higher degree of stability.
Further, QBE is simpler than LLVM (e.g., providing fewer operations) and thereby eases the implementation of analysis techniques.
Nonetheless, there exist compiler frontends that can emit a representation in the QBE intermediate representation (which can then be analyzed using quebex!).
For example, [SCC], [cproc], or the [Hare compiler][Hare].

### Status

Proof of concept, not much to see here yet.
Come back later!

### Installation

The framework consists of several components.
After cloning the repository, individual components can be installed using `cabal install`.
However, presently specific GHC versions are required; therefore, installation using [Guix] is recommended.
For example, in order to install the `quebex-cli` component using Guix:

```
$ guix time-machine -C .guix/channels.scm -- install -L .guix/modules/ quebex-cli
```

Afterwards, if Guix is configured correctly, `quebex-cli` should be available in your `$PATH`.
The following section demonstrates usage of `quebex-cli`.

### Demonstration

This framework is primarily *intended to be used as a library*, allowing the implementation of both static and dynamic analysis techniques based on QBE.
Presently, it focuses on dynamic analysis, and sufficient documentation of the library interface is lacking.
Nonetheless, it is already capable of executing QBE representations of medium-complexity C code (e.g., as emitted by [cproc]).
To experiment with the current capabilities, a command-line tool called `quebex-cli` is available that provides a frontend to quebex's [symbolic execution] library.
Symbolic execution is a dynamic software analysis technique that explores reachable program paths based on a symbolic input variable.
For example, consider the following C program:

```C
#include <stdio.h>

extern int make_symbolic_word(const char *name);

int main(void) {
	puts("<path>");

	int a = make_symbolic_word("a");
	if (a == 42) {
		puts("you found the answer");
	} else {
		puts("not the answer");
	}

	puts("</path>");
	return 0;
}
```

This program can be compiled using [cproc] as follows:

```
$ cproc -emit-qbe test.c
```

The resulting QBE representation (`test.qbe`) can be symbolically executed using quebex:

```
$ quebex-cli test.qbe
```

This will yield the following output:

```
<path>
not the answer
</path>
<path>
you found the answer
</path>
```

This tells us that quebex found two paths through our program based on the symbolic variable `a`.
In the future, it will be possible to obtain test inputs for each path in a standardized format using `quebex-cli` which can then be used to automatically [generate high-coverage tests][KLEE OSDI].
However, for now the focus is on improving the library, not the command-line interface.

### Design Goals

This project is intentionally written in a simple subset of the [Haskell] programming language.
It should be usable by anyone with a basic Haskell background (e.g., as obtained by reading [Learn You a Haskell for Great Good!][learnyouahaskell]).
Further, the project should require minimal long-term maintenance and should also support older GHC versions.
Therefore, it uses the [GHC2021] language standard and avoids usage of additional language extensions.
Further, whenever possible, dependencies on external libraries that are [not bundled by GHC][GHC libraries] must be avoided.

### Development

Code should be formatted using [ormolu][ormolu github].
Git hooks performing several sanity checks, including ensuring the proper code formatting, are available.
These hooks can be enabled using:

	$ git config --local core.hooksPath .githooks

Further, a [Guix] environment for development purposes can be obtained using:

	$ guix shell -L .guix/modules/ -m .guix/manifest.scm

### License

This project uses the [REUSE Specification] to indicated used software license.

[QBE]: https://c9x.me/compile/
[QBE vs LLVM]: https://c9x.me/compile/doc/llvm.html
[LLVM]: https://llvm.org/
[KLEE]: https://klee-se.org
[KLEE LLVM]: https://klee-se.org/build/build-llvm13/
[KLEE OSDI]: https://www.usenix.org/legacy/events/osdi08/tech/full_papers/cadar/cadar.pdf
[SCC]: https://www.simple-cc.org/
[cproc]: https://sr.ht/~mcf/cproc/
[Hare]: https://harelang.org/
[Haskell]: https://haskell.org/
[GHC]: https://www.haskell.org/ghc/
[GHC2021]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html#extension-GHC2021
[GHC libraries]: https://ghc.gitlab.haskell.org/ghc/doc/libraries/index.html
[learnyouahaskell]: https://learnyouahaskell.github.io/chapters.html
[libriscv]: https://github.com/agra-uni-bremen/libriscv
[ormolu github]: https://github.com/tweag/ormolu
[REUSE Specification]: https://reuse.software/spec-3.3/
[Guix]: https://guix.gnu.org
[symbolic exeuction]: https://en.wikipedia.org/wiki/Symbolic_execution
