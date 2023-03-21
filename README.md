# Juvix

<a href="https://github.com/anoma/juvix/actions/workflows/ci.yml">
<img alt="CI status" src="https://github.com/anoma/juvix/actions/workflows/ci.yml/badge.svg" />
</a>

<a href="https://github.com/anoma/juvix/actions/workflows/pages/pages-build-deployment"><img
src="https://github.com/anoma/juvix/actions/workflows/pages/pages-build-deployment/badge.svg"
alt="pages-build-deployment" /></a>

<a href="https://github.com/anoma/juvix/tags">
<img alt="" src="https://img.shields.io/github/v/release/anoma/juvix?include_prereleases" />
</a>

<a href="https://github.com/anoma/juvix/blob/main/LICENSE">
<img alt="LICENSE" src="https://img.shields.io/badge/license-GPL--3.0--only-blue.svg" />
</a>

<a href="https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=455254004">
<img height="20pt" alt="Open the Juvix Standard Lib in Github Codespace" src="https://github.com/codespaces/badge.svg" />
</a>

<a href="https://github.com/anoma/juvix">
<img align="right" width="270" alt="Juvix Mascot" src="assets/images/tara-seating.svg" />
</a>

Juvix is an open-source, constantly evolving functional programming language
designed for writing privacy-preserving decentralized applications. It enables
developers to write smart contracts and decentralized applications on blockchain
platforms in a secure and efficient way.

## Getting Started

To get started with Juvix, head over to the [documentation website](https://docs.juvix.org) to learn more about the language and its features. You can also find installation instructions and tutorials to help you get started with writing Juvix programs.
You can download the [latest release](https://github.com/Juvix-lang/juvix/releases) from the Juvix GitHub repository or use the web-based development environment, [Juvix Github
Codespace](https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=102404734&machine=standardLinux32gb&location=WestEurope), which provides a pre-configured workspace ready to use
with Juvix and the Haskell toolchain installed.

## The Juvix programming language

Juvix provides a high degree of assurance. The Juvix compiler runs
several static analyses which guarantee the absence of runtime errors.
Analyses performed include termination, arity, and type checking. As a
result, functional programs, especially validity predicates, can be
written with greater confidence in their correctness.

Some language features in Juvix include:

- unicode syntax
- parametric polymorphism
- inductive and parametric data types
- higher-order functions
- implicit arguments
- holes in expressions
- axioms for non-computable terms

The Juvix module system allows developers to break down their programs into smaller, reusable modules that can be compiled separately and combined to create larger programs. These modules can be used to build libraries, which can then be documented using Juvix's built-in documentation generation tool, see for example, [the Juvix standard
library's website](https://anoma.github.io/juvix-stdlib/). For further
details, please refer to [the Juvix
book](https://anoma.github.io/juvix/) which includes our [latest
updates](https://anoma.github.io/juvix/changelog.html).

## Related projects

If you're interested in Juvix, you may also want to explore the following related projects:

<!-- prettier-ignore -->
| Project | Description |
| --- | --- |
| [GEB](https://github.com/anoma/geb) | Intermediate language for writing compilers and one of the Juvix backends. |
| [VampIR](https://github.com/anoma/vamp-ir) | Proof-system-agnostic language for writing arithmetic circuit and one of the GEB backends.|

## Resources

Here is a summary of resources to help you learn more about Juvix:

<!-- prettier-ignore -->
| Resource | Description |
| --- | --- |
| [Official website](https://juvix.org) | The official website of Juvix, where you can find documentation, changelog, tutorials, and community resources. |
| [GitHub repository](https://github.com/anoma/juvix) | The official GitHub repository of Juvix, where you can find the source code and contribute to the project. |
| [Discord community](https://discord.gg/PfaaFVErHt) | The Juvix community on Discord is a space where you can connect with the developers behind Juvix and other members of the community who are passionate about privacy-preserving decentralized applications. It's a place where you can ask for help with using Juvix, discuss the latest features and updates, and get involved in the project. |
| [Twitter](https://twitter.com/juvixlang) | The official Twitter account of Juvix, where you can stay up-to-date with the latest news and announcements. |
| [Standard library](https://github.com/anoma/juvix-stdlib) | The Juvix standard library is a collection of pre-written functions and modules that come bundled with the Juvix programming language. It provides developers with a set of common and useful tools that they can use to build their Juvix programs without having to write everything from scratch. |
| [VSCode extension](https://github.com/anoma/vscode-juvix) | Support for the Juvix programming language with features such as syntax highlighting, error checking and many more directly in the VSCode editor.
| [Emacs Juvix mode](https://github.com/anoma/juvix-mode) | A major mode for Emacs that provides support for writing Juvix programs. |
| [Juvix Standard Lib Codespace](https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=102404734&machine=standardLinux32gb&location=WestEurope) | A web-based development environment for the Juvix standard library on GitHub. It provides a pre-configured workspace with the Juvix standard library installed and ready to use, so you can start using the library in your projects. Some examples of Juvix programs are also loaded in this environment. |
| [Juvix Github Codespace](https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=102404734&machine=standardLinux32gb&location=WestEurope) | This codespace provides a pre-configured workspace with Juvix and the Haskell toolchain installed. Everything is ready to use, so you can start developing/inspecting the Juvix compiler right away. |
| [Homebrew Juvix formula](https://github.com/anoma/homebrew-juvix) | A formula for Homebrew, a package manager for macOS and Linux, that allows you to easily install Juvix on your system. |
| [Juvix Nightly builds](https://github.com/anoma/nightly-juvix-builds) | Users can download and use these nightly builds to experiment with the latest changes to the Juvix Compiler. Nightly builds may contain new features, bug fixes, and other improvements to Juvix that are still in development and have not yet been released in an official version.|

## Contributing

If you're interested in contributing to Juvix, please see the [contributing guidelines](CONTRIBUTING.md) for more information. We welcome contributions of all kinds, from bug reports and feature requests to code contributions and documentation improvements.

## License

Juvix is open-source software released under the GNU General Public License v3.0. See the [LICENSE](LICENSE) file for more information.
