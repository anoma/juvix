# Juvix

<a href="https://github.com/anoma/juvix"><img align="right" width="300" alt="Tara the Juvix mascot" src="assets/images/tara-seating.svg" /></a>

<table>
<tr>
<th> CI Status </th>
</tr>
<tr>
<td>

<a
href="https://github.com/anoma/juvix/actions/workflows/ci.yml"><img
src="https://github.com/anoma/juvix/actions/workflows/ci.yml/badge.svg"
alt="The Juvix compiler CI" /></a>

</td>
</tr>
<tr>
<td>

<a
href="https://github.com/anoma/juvix-nightly-builds/actions/workflows/release-nightly.yaml"><img
src="https://github.com/anoma/juvix-nightly-builds/actions/workflows/release-nightly.yaml/badge.svg"
alt="Nightly build, release and benchmark" /></a>

</td>
</tr>
<tr>
<th> Codebase  </th>
</tr>
<tr>
<td><a href="https://github.com/anoma/juvix/tags"><img src="https://img.shields.io/github/v/release/anoma/juvix?include_prereleases"/></a>
</td>
</tr>
<tr>
<td>
<a href="https://github.com/anoma/juvix/blob/main/LICENSE"><img src="https://img.shields.io/badge/license-GPL--3.0--only-blue.svg"/></a>
</td>
</tr>
<tr>
<td> <a href="https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=102404734&machine=standardLinux32gb&location=WestEurope"><img height="20pt" alt="Open the Juvix Standard Lib in Github Codespace" src="https://github.com/codespaces/badge.svg"/> </a>
</td>
</tr>
</table>

Juvix is an open-source, constantly evolving functional programming language
designed for writing privacy-preserving decentralized applications. Using Juvix, developers can write high-level programs which can be compiled to WASM directly, or through [VampIR][vampir] to circuits for private execution with [Taiga][taiga] on [Anoma][anoma] or Ethereum.

## Getting Started

To get started with Juvix, head over to the [documentation website][juvix-book]
to learn more about the language and its features. You can also find
installation instructions and tutorials to help you get started with writing
Juvix programs. You can download the [latest release][latest-release] from the
Juvix GitHub repository or use the web-based development environment, [Juvix
Github Codespace][repo-codespace], which provides a pre-configured workspace
ready to use with Juvix and the Haskell toolchain installed.

## Language features

Juvix is designed with a focus on safety. The Juvix compiler runs several
static analyses which guarantee the absence of runtime errors. Analyses
performed include termination and type checking. As a result, functional
programs, especially validity predicates, can be written with greater confidence
in their correctness.

Some language features in Juvix include:

- Haskell/Agda-like syntax with support for Unicode
- Type inference
- Parametric polymorphism
- User defined inductive data types
- Higher-order functions
- Referential transparency

The Juvix module system allows developers to break down their programs into
smaller, reusable modules that can be compiled separately and combined to create
larger programs. These modules can be used to build libraries, which can then be
documented using Juvix's built-in documentation generation tool, see for
example, [the Juvix standard library's website][stdlib]. For further details,
please refer to [the Juvix book][juvix-book] which includes
our [latest updates][changelog].

## Related projects

If you're interested in Juvix, you may also want to explore the following related projects:

<!-- prettier-ignore -->
| Project | Description |
| ------- | ----------- |
| [GEB][geb] | Intermediate language for writing compilers and one of the Juvix backends. |
| [VampIR][vampir] | Proof-system-agnostic language for writing arithmetic circuits and one of the GEB backends.|
| [Taiga][taiga] | A framework for generalized shielded state transitions. |

## Resources

Here is a summary of resources to help you learn more about Juvix:

### Documentation

<!-- prettier-ignore -->
| Resource | Description |
| -------- | ----------- |
| [Official website][website] | The official website of Juvix, where you can find documentation, changelog, tutorials, and community resources. |
| [GitHub repository][repo] | The official GitHub repository of Juvix, where you can find the source code and contribute to the project. |

### Community

<!-- prettier-ignore -->
| Resource | Description |
| -------- | ----------- |
| [Discord community][discord] | The Juvix community on Discord is a space where you can connect with the developers behind Juvix and other members of the community who are passionate about privacy-preserving decentralized applications. It's a place where you can ask for help with using Juvix, discuss the latest features and updates, and get involved in the project. |
| [Twitter][twitter] | The official Twitter account of Juvix, where you can stay up-to-date with the latest news and announcements. |

### Libraries

<!-- prettier-ignore -->
| Resource | Description |
| -------- | ----------- |
| [Standard library][stdlib] | The Juvix standard library is a collection of pre-written functions and modules that come bundled with the Juvix programming language. It provides developers with a set of common and useful tools that they can use to build their Juvix programs without having to write everything from scratch. |

### IDE support

<!-- prettier-ignore -->
| Resource | Description |
| -------- | ----------- |
| [VSCode extension][vscode-plugin] | Support for the Juvix programming language with features such as syntax highlighting, error checking and many more directly in the VSCode editor.
| [Emacs Juvix mode][juvix-mode] | A major mode for Emacs that provides support for writing Juvix programs. |

### Development environments

<!-- prettier-ignore -->
| Resource | Description |
| -------- | ----------- |
| [Juvix Standard Lib Codespace][stdlib-codespace] | A web-based development environment for the Juvix standard library on GitHub. It provides a pre-configured workspace with the Juvix standard library installed and ready to use, so you can start using the library in your projects. Some examples of Juvix programs are also loaded in this environment. |
| [Juvix Github Codespace][repo-codespace] | This codespace provides a pre-configured workspace with Juvix and the Haskell toolchain installed. Everything is ready to use, so you can start developing/inspecting the Juvix compiler right away. |

### Installation

<!-- prettier-ignore -->
| Resource | Description |
| -------- | ----------- |
| [Homebrew Juvix formula][juvix-formula] | A formula for [Homebrew][homebrew], a package manager for macOS and Linux, that allows you to easily install Juvix on your system. |
| [Juvix Nightly builds][nightly-builds] | Users can download and use these nightly builds to experiment with the latest changes to the Juvix Compiler. Nightly builds may contain new features, bug fixes, and other improvements to Juvix that are still in development and have not yet been released in an official version.|

## Contributing

If you're interested in contributing to Juvix, please see the [contributing guidelines](CONTRIBUTING.md) for more information. We welcome contributions of all kinds, from bug reports and feature requests to code contributions and documentation improvements.

## License

Juvix is open-source software released under the GNU General Public License v3.0. See the [LICENSE](LICENSE.md) file for more information.

[anoma]: https://anoma.net
[changelog]: https://anoma.github.io/juvix/changelog.html
[discord]: https://discord.gg/PfaaFVErHt
[geb]: https://github.com/anoma/geb
[homebrew]: https://brew.sh
[juvix-book]: https://docs.juvix.org
[juvix-formula]: https://github.com/anoma/homebrew-juvix
[juvix-mode]: https://github.com/anoma/juvix-mode
[latest-release]: https://github.com/anoma/juvix/releases/latest
[nightly-builds]: https://github.com/anoma/juvix-nightly-builds
[repo-codespace]: https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=102404734&machine=standardLinux32gb&location=WestEurope
[repo]: https://github.com/anoma/juvix
[stdlib-codespace]: https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=102404734&machine=standardLinux32gb&location=WestEurope
[stdlib]: https://github.com/anoma/juvix-stdlib
[taiga]: https://github.com/anoma/taiga
[twitter]: https://twitter.com/juvixlang
[vampir]: https://github.com/anoma/vamp-ir
[vscode-plugin]: https://github.com/anoma/vscode-juvix
[website]: https://juvix.org
