# Changelog

<a href="https://github.com/anoma/juvix">
<img align="right" width="300" height="300" alt="Juvix Mascot" src="../assets/images/tara-smiling.svg" />
</a>

## [v0.3.1](https://github.com/anoma/juvix/tree/v0.3.1) (2023-03-31)

[Full Changelog](https://github.com/anoma/juvix/compare/v0.3.0...v0.3.1)

**Implemented enhancements:**

- Option `--show-args-num` [\#1946](https://github.com/anoma/juvix/pull/1946)
  ([lukaszcz](https://github.com/lukaszcz))
- Preserve the target type in letrec lifting
  [\#1945](https://github.com/anoma/juvix/pull/1945)
  ([janmasrovira](https://github.com/janmasrovira))
- Add syntax highlighting to Core error messages
  [\#1938](https://github.com/anoma/juvix/pull/1938)
  ([lukaszcz](https://github.com/lukaszcz))
- Add the `--unroll` option
  [\#1935](https://github.com/anoma/juvix/pull/1935)
  ([lukaszcz](https://github.com/lukaszcz))
- Preserve name and location information in Internal-to-Core
  [\#1933](https://github.com/anoma/juvix/pull/1933)
  ([lukaszcz](https://github.com/lukaszcz))
- Polymorphic type inference in Core
  [\#1931](https://github.com/anoma/juvix/pull/1931)
  ([lukaszcz](https://github.com/lukaszcz))
- Update README.md with Juvix nightly builds badge
  [\#1923](https://github.com/anoma/juvix/pull/1923)
  ([jonaprieto](https://github.com/jonaprieto))
- Create clean-up-cache.yaml
  [\#1915](https://github.com/anoma/juvix/pull/1915)
  ([jonaprieto](https://github.com/jonaprieto))
- Update GitHub pages deployment using deploy-pages action
  [\#1910](https://github.com/anoma/juvix/pull/1910)
  ([jonaprieto](https://github.com/jonaprieto))
- Check for recursive inductive types in the GEB pipeline
  [\#1909](https://github.com/anoma/juvix/pull/1909)
  ([lukaszcz](https://github.com/lukaszcz))
- CI pre-commit maintenance
  [\#1905](https://github.com/anoma/juvix/pull/1905)
  ([jonaprieto](https://github.com/jonaprieto))
- Add new README and md files
  [\#1904](https://github.com/anoma/juvix/pull/1904)
  ([jonaprieto](https://github.com/jonaprieto))
- Print JuvixCore correctly
  [\#1875](https://github.com/anoma/juvix/pull/1875)
  ([lukaszcz](https://github.com/lukaszcz))
- Pattern matching compilation
  [\#1874](https://github.com/anoma/juvix/pull/1874)
  ([lukaszcz](https://github.com/lukaszcz))
- CI Haskell maintenance
  [\#1797](https://github.com/anoma/juvix/pull/1797)
  ([jonaprieto](https://github.com/jonaprieto))

**Merged pull requests:**

- Let-folding after lifting
  [\#1955](https://github.com/anoma/juvix/pull/1955)
  ([lukaszcz](https://github.com/lukaszcz))
- Fix removal of polymorphic type arguments
  [\#1954](https://github.com/anoma/juvix/pull/1954)
  ([lukaszcz](https://github.com/lukaszcz))
- Fix a bug in closure traversal
  [\#1953](https://github.com/anoma/juvix/pull/1953)
  ([lukaszcz](https://github.com/lukaszcz))
- Update typecheck command to check for coverage
  [\#1952](https://github.com/anoma/juvix/pull/1952)
  ([janmasrovira](https://github.com/janmasrovira))
- CI: Ignore errors linux typecheck / format examples step
  [\#1950](https://github.com/anoma/juvix/pull/1950)
  ([paulcadman](https://github.com/paulcadman))
- Filter out type synonyms in RemoveTypeArgs
  [\#1949](https://github.com/anoma/juvix/pull/1949)
  ([lukaszcz](https://github.com/lukaszcz))
- Add fail nodes to Geb
  [\#1947](https://github.com/anoma/juvix/pull/1947)
  ([lukaszcz](https://github.com/lukaszcz))
- End-to-end Geb compilation tests
  [\#1942](https://github.com/anoma/juvix/pull/1942)
  ([lukaszcz](https://github.com/lukaszcz))
- Add juvix dev repl command
  [\#1941](https://github.com/anoma/juvix/pull/1941)
  ([paulcadman](https://github.com/paulcadman))
- Refactor Geb values
  [\#1940](https://github.com/anoma/juvix/pull/1940)
  ([lukaszcz](https://github.com/lukaszcz))
- Avoid capturing the same free variable multiple times in letrec lifting
  [\#1939](https://github.com/anoma/juvix/pull/1939)
  ([janmasrovira](https://github.com/janmasrovira))
- Add Judoc syntax reference
  [\#1934](https://github.com/anoma/juvix/pull/1934)
  ([janmasrovira](https://github.com/janmasrovira))
- Fix spacing of judoc in the formatter
  [\#1932](https://github.com/anoma/juvix/pull/1932)
  ([janmasrovira](https://github.com/janmasrovira))
- bench: Fix juvix compile flag for wasm
  [\#1925](https://github.com/anoma/juvix/pull/1925)
  ([paulcadman](https://github.com/paulcadman))
- Fix memory count for string operations
  [\#1924](https://github.com/anoma/juvix/pull/1924)
  ([lukaszcz](https://github.com/lukaszcz))
- Let folding
  [\#1921](https://github.com/anoma/juvix/pull/1921)
  ([lukaszcz](https://github.com/lukaszcz))
- Add a test suite for milestone examples
  [\#1920](https://github.com/anoma/juvix/pull/1920)
  ([paulcadman](https://github.com/paulcadman))
- Add --numeric-version flag
  [\#1918](https://github.com/anoma/juvix/pull/1918)
  ([jonaprieto](https://github.com/jonaprieto))
- Fix bug with unregistered builtin bool
  [\#1917](https://github.com/anoma/juvix/pull/1917)
  ([lukaszcz](https://github.com/lukaszcz))
- Recursion unrolling for functions
  [\#1912](https://github.com/anoma/juvix/pull/1912)
  ([lukaszcz](https://github.com/lukaszcz))
- Fix REPL state to include enough information to rerun the pipeline
  [\#1911](https://github.com/anoma/juvix/pull/1911)
  ([janmasrovira](https://github.com/janmasrovira))
- CI Haskell fix for macOS build
  [\#1908](https://github.com/anoma/juvix/pull/1908)
  ([jonaprieto](https://github.com/jonaprieto))
- Fix bug in IO runtime
  [\#1906](https://github.com/anoma/juvix/pull/1906)
  ([lukaszcz](https://github.com/lukaszcz))
- Fix JuvixAsm validation
  [\#1903](https://github.com/anoma/juvix/pull/1903)
  ([lukaszcz](https://github.com/lukaszcz))
- Fix registration of builtin inductive axioms
  [\#1901](https://github.com/anoma/juvix/pull/1901)
  ([paulcadman](https://github.com/paulcadman))
- internal-to-core: Fix index shifting of pattern arguments
  [\#1900](https://github.com/anoma/juvix/pull/1900)
  ([paulcadman](https://github.com/paulcadman))
- Fix de Bruijn indices in rmap
  [\#1898](https://github.com/anoma/juvix/pull/1898)
  ([lukaszcz](https://github.com/lukaszcz))
- Normalize types in repl
  [\#1897](https://github.com/anoma/juvix/pull/1897)
  ([janmasrovira](https://github.com/janmasrovira))
- Add MidSquareHash.juvix and fix types in MidSquareHash.jvc
  [\#1896](https://github.com/anoma/juvix/pull/1896)
  ([lukaszcz](https://github.com/lukaszcz))
- Automatically detect and split mutually recursive blocks in let expressions
  [\#1894](https://github.com/anoma/juvix/pull/1894)
  ([janmasrovira](https://github.com/janmasrovira))
- The `rmap` recursor
  [\#1893](https://github.com/anoma/juvix/pull/1893)
  ([lukaszcz](https://github.com/lukaszcz))
- Add `juvix format` command
  [\#1886](https://github.com/anoma/juvix/pull/1886)
  ([paulcadman](https://github.com/paulcadman))
- Make keyword `end` optional for top modules
  [\#1883](https://github.com/anoma/juvix/pull/1883)
  ([janmasrovira](https://github.com/janmasrovira))
- Add errors to the Core pipeline and check GEB prerequisites
  [\#1871](https://github.com/anoma/juvix/pull/1871)
  ([lukaszcz](https://github.com/lukaszcz))
- Test core to geb translation
  [\#1865](https://github.com/anoma/juvix/pull/1865)
  ([jonaprieto](https://github.com/jonaprieto))

## [v0.3.0](https://github.com/anoma/juvix/tree/v0.3.0) (2023-03-15)

[Full Changelog](https://github.com/anoma/juvix/compare/v0.2.9...v0.3.0)

**Implemented enhancements:**

- Avoid line breaks in applications within a type signature
  [#1850](https://github.com/anoma/juvix/issues/1850)
  ([paulcadman](https://github.com/paulcadman))
- Respect user's spacing decisions in the formatter
  [#1837](https://github.com/anoma/juvix/issues/1837)
  ([janmasrovira](https://github.com/janmasrovira))
- Formatter should not transform ASCII symbols to unicode by default
  [#1827](https://github.com/anoma/juvix/issues/1827)
  ([janmasrovira](https://github.com/janmasrovira))
- Enable match-to-case, nat-to-int and convert-builtins by default in
  REPL [#1825](https://github.com/anoma/juvix/issues/1825)
  ([lukaszcz](https://github.com/lukaszcz))
- The Juvix formatter works poorly with multi-line ifs
  [#1793](https://github.com/anoma/juvix/issues/1793)
  ([janmasrovira](https://github.com/janmasrovira))
- Add a lazy IO sequencing function (#1772)
  [#1773](https://github.com/anoma/juvix/issues/1773)
  ([lukaszcz](https://github.com/lukaszcz))
- Support LetRec in the GEB backend
  [#1756](https://github.com/anoma/juvix/issues/1756)
  ([janmasrovira](https://github.com/janmasrovira))
- Support integers in the GEB backend
  [#1753](https://github.com/anoma/juvix/issues/1753)
  ([lukaszcz](https://github.com/lukaszcz))
- GEB evaluator [#1751](https://github.com/anoma/juvix/issues/1751)
  ([jonaprieto](https://github.com/jonaprieto))
- Add debugging builtin functions
  [#1731](https://github.com/anoma/juvix/issues/1731)
  ([jonaprieto](https://github.com/jonaprieto))
- Non-judoc comments are removed when generating HTML output
  [#1723](https://github.com/anoma/juvix/issues/1723)
  ([janmasrovira](https://github.com/janmasrovira))
- Special syntax for `case`
  [#1716](https://github.com/anoma/juvix/issues/1716)
  ([janmasrovira](https://github.com/janmasrovira))
- Make \|\| and && lazy
  [#1701](https://github.com/anoma/juvix/issues/1701)
  ([lukaszcz](https://github.com/lukaszcz))
- It should be possible to specify multiple implicit type arguments at
  once [#1692](https://github.com/anoma/juvix/issues/1692)
  ([janmasrovira](https://github.com/janmasrovira))
- Naive compilation of complex pattern matches with match-expressions
  to decision trees with case-expressions
  [#1531](https://github.com/anoma/juvix/issues/1531)
  ([paulcadman](https://github.com/paulcadman))
- New compilation pipeline
  [#1832](https://github.com/anoma/juvix/pull/1832)
  ([lukaszcz](https://github.com/lukaszcz))
- Add internal core-eval option to evaluate named function identifier
  [#1819](https://github.com/anoma/juvix/pull/1819)
  ([paulcadman](https://github.com/paulcadman))
- Short syntax for sequences of function and datatype parameters
  [#1809](https://github.com/anoma/juvix/pull/1809)
  ([lukaszcz](https://github.com/lukaszcz))
- Add Geb Backend Evaluator with some extra subcommands
  [#1808](https://github.com/anoma/juvix/pull/1808)
  ([jonaprieto](https://github.com/jonaprieto))
- Add REPL option to apply Core transformations
  [#1796](https://github.com/anoma/juvix/pull/1796)
  ([paulcadman](https://github.com/paulcadman))
- String builtins [#1784](https://github.com/anoma/juvix/pull/1784)
  ([lukaszcz](https://github.com/lukaszcz))
- Use restore/save github action to speed up the CI testing
  [#1783](https://github.com/anoma/juvix/pull/1783)
  ([jonaprieto](https://github.com/jonaprieto))
- Fix minor issue with ==% for type equality
  [#1780](https://github.com/anoma/juvix/pull/1780)
  ([jonaprieto](https://github.com/jonaprieto))
- Add debugging builtin functions `trace` and `fail`
  [#1771](https://github.com/anoma/juvix/pull/1771)
  ([jonaprieto](https://github.com/jonaprieto))
- Keep regular comments in html output
  [#1766](https://github.com/anoma/juvix/pull/1766)
  ([janmasrovira](https://github.com/janmasrovira))
- Lazy boolean operators
  [#1743](https://github.com/anoma/juvix/pull/1743)
  ([lukaszcz](https://github.com/lukaszcz))
- Refactor `html` command with extra options
  [#1725](https://github.com/anoma/juvix/pull/1725)
  ([jonaprieto](https://github.com/jonaprieto))
- Add initial setup for codespaces
  [#1713](https://github.com/anoma/juvix/pull/1713)
  ([jonaprieto](https://github.com/jonaprieto))
- Typecheck let expressions
  [#1712](https://github.com/anoma/juvix/pull/1712)
  ([janmasrovira](https://github.com/janmasrovira))
- Use Smoke instead of shelltestrunner
  [#1710](https://github.com/anoma/juvix/pull/1710)
  ([jonaprieto](https://github.com/jonaprieto))
- Replace –output-dir flag by –internal-build-dir
  [#1707](https://github.com/anoma/juvix/pull/1707)
  ([jonaprieto](https://github.com/jonaprieto))
- Compiler output [#1705](https://github.com/anoma/juvix/pull/1705)
  ([jonaprieto](https://github.com/jonaprieto))
- Allow optional pipe before the first constructor for inductive type
  declarations [#1699](https://github.com/anoma/juvix/pull/1699)
  ([jonaprieto](https://github.com/jonaprieto))
- Nat builtins [#1686](https://github.com/anoma/juvix/pull/1686)
  ([lukaszcz](https://github.com/lukaszcz))

**Merged pull requests:**

- Remove dead code in `Internal`
  [#1891](https://github.com/anoma/juvix/pull/1891)
  ([janmasrovira](https://github.com/janmasrovira))
- Remove missing Juvix examples and webapp example from docs build
  [#1890](https://github.com/anoma/juvix/pull/1890)
  ([paulcadman](https://github.com/paulcadman))
- Fix type synonym in let
  [#1880](https://github.com/anoma/juvix/pull/1880)
  ([janmasrovira](https://github.com/janmasrovira))
- Update stack resolver to lts-20.12
  [#1873](https://github.com/anoma/juvix/pull/1873)
  ([paulcadman](https://github.com/paulcadman))
- Use Ape to format patterns
  [#1870](https://github.com/anoma/juvix/pull/1870)
  ([janmasrovira](https://github.com/janmasrovira))
- Fix Core-To-Geb translation
  [#1863](https://github.com/anoma/juvix/pull/1863)
  ([jonaprieto](https://github.com/jonaprieto))
- Remove the old C backend
  [#1862](https://github.com/anoma/juvix/pull/1862)
  ([lukaszcz](https://github.com/lukaszcz))
- Move `substEnv` to its own module
  [#1861](https://github.com/anoma/juvix/pull/1861)
  ([janmasrovira](https://github.com/janmasrovira))
- Add `_caseTypeWholeExpression` to Internal
  [#1860](https://github.com/anoma/juvix/pull/1860)
  ([janmasrovira](https://github.com/janmasrovira))
- remove old minihaskell files
  [#1859](https://github.com/anoma/juvix/pull/1859)
  ([jonaprieto](https://github.com/jonaprieto))
- Fix bugs in the Case translation in Core-to-Geb
  [#1858](https://github.com/anoma/juvix/pull/1858)
  ([lukaszcz](https://github.com/lukaszcz))
- Format examples [#1856](https://github.com/anoma/juvix/pull/1856)
  ([janmasrovira](https://github.com/janmasrovira))
- Sort the identifiers topologically in the Core-to-GEB translation
  [#1854](https://github.com/anoma/juvix/pull/1854)
  ([lukaszcz](https://github.com/lukaszcz))
- Add type info to the mid-square hashing function
  [#1853](https://github.com/anoma/juvix/pull/1853)
  ([lukaszcz](https://github.com/lukaszcz))
- Use APE mechanism to format Function expressions
  [#1852](https://github.com/anoma/juvix/pull/1852)
  ([paulcadman](https://github.com/paulcadman))
- Preserve single wildcards pretty printing function parameters
  [#1851](https://github.com/anoma/juvix/pull/1851)
  ([paulcadman](https://github.com/paulcadman))
- Add type annotation to case expression
  [#1849](https://github.com/anoma/juvix/pull/1849)
  ([janmasrovira](https://github.com/janmasrovira))
- Remove module parameters
  [#1848](https://github.com/anoma/juvix/pull/1848)
  ([janmasrovira](https://github.com/janmasrovira))
- Allow shadowing local variables with let function definitions
  [#1847](https://github.com/anoma/juvix/pull/1847)
  ([janmasrovira](https://github.com/janmasrovira))
- Add lambda type info
  [#1845](https://github.com/anoma/juvix/pull/1845)
  ([janmasrovira](https://github.com/janmasrovira))
- Improve comma formatting
  [#1842](https://github.com/anoma/juvix/pull/1842)
  ([janmasrovira](https://github.com/janmasrovira))
- Improve formatter [#1840](https://github.com/anoma/juvix/pull/1840)
  ([janmasrovira](https://github.com/janmasrovira))
- Respect lambda Ascii/Unicode
  [#1838](https://github.com/anoma/juvix/pull/1838)
  ([janmasrovira](https://github.com/janmasrovira))
- Fix `juvix init` [#1835](https://github.com/anoma/juvix/pull/1835)
  ([janmasrovira](https://github.com/janmasrovira))
- The formatter respects the ascii function arrow
  [#1834](https://github.com/anoma/juvix/pull/1834)
  ([janmasrovira](https://github.com/janmasrovira))
- Add `dev core from-concrete` command
  [#1833](https://github.com/anoma/juvix/pull/1833)
  ([janmasrovira](https://github.com/janmasrovira))
- Give proper errors for incorrect application of lazy builtins
  [#1830](https://github.com/anoma/juvix/pull/1830)
  ([lukaszcz](https://github.com/lukaszcz))
- Documentation: update language reference
  [#1829](https://github.com/anoma/juvix/pull/1829)
  ([lukaszcz](https://github.com/lukaszcz))
- Add compilation of complex pattern matching to case
  [#1824](https://github.com/anoma/juvix/pull/1824)
  ([paulcadman](https://github.com/paulcadman))
- Apply CI ghcup workaround to docs build
  [#1823](https://github.com/anoma/juvix/pull/1823)
  ([paulcadman](https://github.com/paulcadman))
- Update the Juvix tutorial for 0.3
  [#1822](https://github.com/anoma/juvix/pull/1822)
  ([lukaszcz](https://github.com/lukaszcz))
- Workaround ghcup issue on CI runner
  [#1821](https://github.com/anoma/juvix/pull/1821)
  ([paulcadman](https://github.com/paulcadman))
- Respect the `juvix dev highlight --format` flag when outputting
  errors [#1820](https://github.com/anoma/juvix/pull/1820)
  ([janmasrovira](https://github.com/janmasrovira))
- Comments about the usage of the JuvixCore recursors
  [#1818](https://github.com/anoma/juvix/pull/1818)
  ([lukaszcz](https://github.com/lukaszcz))
- Emacs mode and VSCode extension tutorials
  [#1815](https://github.com/anoma/juvix/pull/1815)
  ([lukaszcz](https://github.com/lukaszcz))
- Documentation: how to compile Juvix programs
  [#1813](https://github.com/anoma/juvix/pull/1813)
  ([lukaszcz](https://github.com/lukaszcz))
- Make '\>\>' lazy [#1812](https://github.com/anoma/juvix/pull/1812)
  ([lukaszcz](https://github.com/lukaszcz))
- Output proper GEB Lisp programs
  [#1810](https://github.com/anoma/juvix/pull/1810)
  ([lukaszcz](https://github.com/lukaszcz))
- Remove the usage annotation syntax
  [#1805](https://github.com/anoma/juvix/pull/1805)
  ([lukaszcz](https://github.com/lukaszcz))
- Mid-square hashing implemented in JuvixCore
  [#1804](https://github.com/anoma/juvix/pull/1804)
  ([lukaszcz](https://github.com/lukaszcz))
- Autocompletion for `dev core compilation --target`
  [#1803](https://github.com/anoma/juvix/pull/1803)
  ([janmasrovira](https://github.com/janmasrovira))
- Special syntax for case
  [#1800](https://github.com/anoma/juvix/pull/1800)
  ([janmasrovira](https://github.com/janmasrovira))
- Adapt benchmarks to the new pipeline
  [#1795](https://github.com/anoma/juvix/pull/1795)
  ([lukaszcz](https://github.com/lukaszcz))
- Support letrec lifting without lambda lifting
  [#1794](https://github.com/anoma/juvix/pull/1794)
  ([janmasrovira](https://github.com/janmasrovira))
- Use the reader effect
  [#1791](https://github.com/anoma/juvix/pull/1791)
  ([janmasrovira](https://github.com/janmasrovira))
- Remove braces from let expressions
  [#1790](https://github.com/anoma/juvix/pull/1790)
  ([janmasrovira](https://github.com/janmasrovira))
- Translate as-pattern binders to Core PatternBinders
  [#1789](https://github.com/anoma/juvix/pull/1789)
  ([paulcadman](https://github.com/paulcadman))
- Fix termination with as-patterns
  [#1787](https://github.com/anoma/juvix/pull/1787)
  ([janmasrovira](https://github.com/janmasrovira))
- Allow type signatures to have a body
  [#1785](https://github.com/anoma/juvix/pull/1785)
  ([janmasrovira](https://github.com/janmasrovira))
- Track builtins in the Core InfoTable
  [#1782](https://github.com/anoma/juvix/pull/1782)
  ([paulcadman](https://github.com/paulcadman))
- Pipes for lambda clauses
  [#1781](https://github.com/anoma/juvix/pull/1781)
  ([janmasrovira](https://github.com/janmasrovira))
- Support integers in the GEB backend
  [#1778](https://github.com/anoma/juvix/pull/1778)
  ([lukaszcz](https://github.com/lukaszcz))
- Add builtin nat and bool types as start nodes in reachability
  analysis [#1775](https://github.com/anoma/juvix/pull/1775)
  ([paulcadman](https://github.com/paulcadman))
- Update pre-commit [#1772](https://github.com/anoma/juvix/pull/1772)
  ([jonaprieto](https://github.com/jonaprieto))
- Parse JuvixCore with absolute paths
  [#1770](https://github.com/anoma/juvix/pull/1770)
  ([paulcadman](https://github.com/paulcadman))
- Use absolute path in Core Evaluator to generate source file location
  [#1769](https://github.com/anoma/juvix/pull/1769)
  ([paulcadman](https://github.com/paulcadman))
- Install wasmer binary from Github releases
  [#1765](https://github.com/anoma/juvix/pull/1765)
  ([jonaprieto](https://github.com/jonaprieto))
- Run the new Juvix formatter for all the Juvix examples
  [#1764](https://github.com/anoma/juvix/pull/1764)
  ([jonaprieto](https://github.com/jonaprieto))
- Fix let expressions in the repl
  [#1763](https://github.com/anoma/juvix/pull/1763)
  ([janmasrovira](https://github.com/janmasrovira))
- Improve arity inference for repl expressions
  [#1762](https://github.com/anoma/juvix/pull/1762)
  ([janmasrovira](https://github.com/janmasrovira))
- Fix broken links and other improvements
  [#1761](https://github.com/anoma/juvix/pull/1761)
  ([jonaprieto](https://github.com/jonaprieto))
- Translate Nat builtins to the correct Core Ops
  [#1760](https://github.com/anoma/juvix/pull/1760)
  ([paulcadman](https://github.com/paulcadman))
- Remove hlint from the CI and pre-commit config
  [#1759](https://github.com/anoma/juvix/pull/1759)
  ([jonaprieto](https://github.com/jonaprieto))
- Fix demo example build
  [#1757](https://github.com/anoma/juvix/pull/1757)
  ([paulcadman](https://github.com/paulcadman))
- Basic Geb integration
  [#1748](https://github.com/anoma/juvix/pull/1748)
  ([lukaszcz](https://github.com/lukaszcz))
- Fix macOS CI build [#1747](https://github.com/anoma/juvix/pull/1747)
  ([paulcadman](https://github.com/paulcadman))
- Adapt Juvix programs to the new pipeline
  [#1746](https://github.com/anoma/juvix/pull/1746)
  ([lukaszcz](https://github.com/lukaszcz))
- Fix link in README for the new docs
  [#1745](https://github.com/anoma/juvix/pull/1745)
  ([lukaszcz](https://github.com/lukaszcz))
- Move juvix-mode to a separate repository
  [#1744](https://github.com/anoma/juvix/pull/1744)
  ([jonaprieto](https://github.com/jonaprieto))
- Print comments when pretty printing concrete syntax
  [#1737](https://github.com/anoma/juvix/pull/1737)
  ([janmasrovira](https://github.com/janmasrovira))
- Demo [#1736](https://github.com/anoma/juvix/pull/1736)
  ([lukaszcz](https://github.com/lukaszcz))
- Update CI to install Smoke, Github actions, and Makefile fixes
  [#1735](https://github.com/anoma/juvix/pull/1735)
  ([jonaprieto](https://github.com/jonaprieto))
- Update stack.yaml [#1734](https://github.com/anoma/juvix/pull/1734)
  ([jonaprieto](https://github.com/jonaprieto))
- Fix Nat builtins [#1733](https://github.com/anoma/juvix/pull/1733)
  ([lukaszcz](https://github.com/lukaszcz))
- Script to count LOC
  [#1732](https://github.com/anoma/juvix/pull/1732)
  ([lukaszcz](https://github.com/lukaszcz))
- Give a proper type to literal Strings
  [#1730](https://github.com/anoma/juvix/pull/1730)
  ([paulcadman](https://github.com/paulcadman))
- Do not filter implicit args in internal to core translation
  [#1728](https://github.com/anoma/juvix/pull/1728)
  ([paulcadman](https://github.com/paulcadman))
- Fix de Brujin indexing of lambda arguments
  [#1727](https://github.com/anoma/juvix/pull/1727)
  ([paulcadman](https://github.com/paulcadman))
- Fix inference loop [#1726](https://github.com/anoma/juvix/pull/1726)
  ([janmasrovira](https://github.com/janmasrovira))
- Remove wildcard patterns from Internal
  [#1724](https://github.com/anoma/juvix/pull/1724)
  ([janmasrovira](https://github.com/janmasrovira))
- Restructure the documentation and add a tutorial
  [#1718](https://github.com/anoma/juvix/pull/1718)
  ([lukaszcz](https://github.com/lukaszcz))
- Improve error message for confusing ':=' with '='
  [#1715](https://github.com/anoma/juvix/pull/1715)
  ([lukaszcz](https://github.com/lukaszcz))
- Fix #1704 [#1711](https://github.com/anoma/juvix/pull/1711)
  ([janmasrovira](https://github.com/janmasrovira))
- Fix #1693 [#1708](https://github.com/anoma/juvix/pull/1708)
  ([janmasrovira](https://github.com/janmasrovira))
- Tests for the new compilation pipeline
  [#1703](https://github.com/anoma/juvix/pull/1703)
  ([lukaszcz](https://github.com/lukaszcz))
- Add printString and printBool support to legacy C backend
  [#1698](https://github.com/anoma/juvix/pull/1698)
  ([paulcadman](https://github.com/paulcadman))
- Add –show-de-bruijn option to `juvix repl`
  [#1694](https://github.com/anoma/juvix/pull/1694)
  ([lukaszcz](https://github.com/lukaszcz))
- Allow 'terminating' keyword with builtins
  [#1688](https://github.com/anoma/juvix/pull/1688)
  ([lukaszcz](https://github.com/lukaszcz))
- Remove unicode cons symbol
  [#1687](https://github.com/anoma/juvix/pull/1687)
  ([lukaszcz](https://github.com/lukaszcz))
- Change syntax for ind. data types and forbid the empty data type
  [#1684](https://github.com/anoma/juvix/pull/1684)
  ([jonaprieto](https://github.com/jonaprieto))
- Convert Nat literals to Core integers
  [#1681](https://github.com/anoma/juvix/pull/1681)
  ([lukaszcz](https://github.com/lukaszcz))
- Less verbose output from running `make check`
  [#1675](https://github.com/anoma/juvix/pull/1675)
  ([jonaprieto](https://github.com/jonaprieto))
- Remove where syntax
  [#1674](https://github.com/anoma/juvix/pull/1674)
  ([jonaprieto](https://github.com/jonaprieto))
- Benchmarks [#1673](https://github.com/anoma/juvix/pull/1673)
  ([janmasrovira](https://github.com/janmasrovira))
- JuvixCore to JuvixAsm translation
  [#1665](https://github.com/anoma/juvix/pull/1665)
  ([lukaszcz](https://github.com/lukaszcz))

## [v0.2.9](https://github.com/anoma/juvix/tree/v0.2.9) (2023-01-18)

[Full Changelog](https://github.com/anoma/juvix/compare/v0.2.8...v0.2.9)

**Implemented enhancements:**

- Refactor `html` command with extra options
  [#1725](https://github.com/anoma/juvix/pull/1725)
  ([jonaprieto](https://github.com/jonaprieto))
- Add initial setup for codespaces
  [#1713](https://github.com/anoma/juvix/pull/1713)
  ([jonaprieto](https://github.com/jonaprieto))
- Typecheck let expressions
  [#1712](https://github.com/anoma/juvix/pull/1712)
  ([janmasrovira](https://github.com/janmasrovira))
- Use Smoke instead of shelltestrunner
  [#1710](https://github.com/anoma/juvix/pull/1710)
  ([jonaprieto](https://github.com/jonaprieto))
- Replace –output-dir flag by –internal-build-dir
  [#1707](https://github.com/anoma/juvix/pull/1707)
  ([jonaprieto](https://github.com/jonaprieto))
- Compiler output [#1705](https://github.com/anoma/juvix/pull/1705)
  ([jonaprieto](https://github.com/jonaprieto))
- Allow optional pipe before the first constructor for inductive type
  declarations [#1699](https://github.com/anoma/juvix/pull/1699)
  ([jonaprieto](https://github.com/jonaprieto))
- Nat builtins [#1686](https://github.com/anoma/juvix/pull/1686)
  ([lukaszcz](https://github.com/lukaszcz))

**Merged pull requests:**

- Demo [#1736](https://github.com/anoma/juvix/pull/1736)
  ([lukaszcz](https://github.com/lukaszcz))
- Update stack.yaml [#1734](https://github.com/anoma/juvix/pull/1734)
  ([jonaprieto](https://github.com/jonaprieto))
- Fix Nat builtins [#1733](https://github.com/anoma/juvix/pull/1733)
  ([lukaszcz](https://github.com/lukaszcz))
- Script to count LOC
  [#1732](https://github.com/anoma/juvix/pull/1732)
  ([lukaszcz](https://github.com/lukaszcz))
- Give a proper type to literal Strings
  [#1730](https://github.com/anoma/juvix/pull/1730)
  ([paulcadman](https://github.com/paulcadman))
- Do not filter implicit args in internal to core translation
  [#1728](https://github.com/anoma/juvix/pull/1728)
  ([paulcadman](https://github.com/paulcadman))
- Fix de Brujin indexing of lambda arguments
  [#1727](https://github.com/anoma/juvix/pull/1727)
  ([paulcadman](https://github.com/paulcadman))
- Fix inference loop [#1726](https://github.com/anoma/juvix/pull/1726)
  ([janmasrovira](https://github.com/janmasrovira))
- Remove wildcard patterns from Internal
  [#1724](https://github.com/anoma/juvix/pull/1724)
  ([janmasrovira](https://github.com/janmasrovira))
- Restructure the documentation and add a tutorial
  [#1718](https://github.com/anoma/juvix/pull/1718)
  ([lukaszcz](https://github.com/lukaszcz))
- Improve error message for confusing ':=' with '='
  [#1715](https://github.com/anoma/juvix/pull/1715)
  ([lukaszcz](https://github.com/lukaszcz))
- Fix #1704 [#1711](https://github.com/anoma/juvix/pull/1711)
  ([janmasrovira](https://github.com/janmasrovira))
- Fix #1693 [#1708](https://github.com/anoma/juvix/pull/1708)
  ([janmasrovira](https://github.com/janmasrovira))
- Tests for the new compilation pipeline
  [#1703](https://github.com/anoma/juvix/pull/1703)
  ([lukaszcz](https://github.com/lukaszcz))
- Add printString and printBool support to legacy C backend
  [#1698](https://github.com/anoma/juvix/pull/1698)
  ([paulcadman](https://github.com/paulcadman))
- Add –show-de-bruijn option to `juvix repl`
  [#1694](https://github.com/anoma/juvix/pull/1694)
  ([lukaszcz](https://github.com/lukaszcz))
- Allow 'terminating' keyword with builtins
  [#1688](https://github.com/anoma/juvix/pull/1688)
  ([lukaszcz](https://github.com/lukaszcz))
- Remove unicode cons symbol
  [#1687](https://github.com/anoma/juvix/pull/1687)
  ([lukaszcz](https://github.com/lukaszcz))
- Change syntax for ind. data types and forbid the empty data type
  [#1684](https://github.com/anoma/juvix/pull/1684)
  ([jonaprieto](https://github.com/jonaprieto))
- Convert Nat literals to Core integers
  [#1681](https://github.com/anoma/juvix/pull/1681)
  ([lukaszcz](https://github.com/lukaszcz))
- Less verbose output from running `make check`
  [#1675](https://github.com/anoma/juvix/pull/1675)
  ([jonaprieto](https://github.com/jonaprieto))
- Remove where syntax
  [#1674](https://github.com/anoma/juvix/pull/1674)
  ([jonaprieto](https://github.com/jonaprieto))
- Benchmarks [#1673](https://github.com/anoma/juvix/pull/1673)
  ([janmasrovira](https://github.com/janmasrovira))
- JuvixCore to JuvixAsm translation
  [#1665](https://github.com/anoma/juvix/pull/1665)
  ([lukaszcz](https://github.com/lukaszcz))

## [v0.2.8](https://github.com/anoma/juvix/tree/v0.2.8) (2022-12-20)

[Full Changelog](https://github.com/anoma/juvix/compare/v0.2.7...v0.2.8)

**Implemented enhancements:**

- Support basic dependencies
  [#1622](https://github.com/anoma/juvix/pull/1622)
  ([janmasrovira](https://github.com/janmasrovira))

**Merged pull requests:**

- Refactor hie.yaml and add entry in the readme
  [#1672](https://github.com/anoma/juvix/pull/1672)
  ([janmasrovira](https://github.com/janmasrovira))
- Fix inline monospace formatted text in README
  [#1671](https://github.com/anoma/juvix/pull/1671)
  ([paulcadman](https://github.com/paulcadman))
- Pin mdbook to version 0.4.22 in docs build
  [#1670](https://github.com/anoma/juvix/pull/1670)
  ([paulcadman](https://github.com/paulcadman))
- Add option to specify Core transformations to
  `dev internal core-eval`
  [#1669](https://github.com/anoma/juvix/pull/1669)
  ([paulcadman](https://github.com/paulcadman))
- Revert "Ignore binaries generated by running some tests"
  [#1668](https://github.com/anoma/juvix/pull/1668)
  ([jonaprieto](https://github.com/jonaprieto))
- Add configuration files so the project can be built with cabal
  [#1667](https://github.com/anoma/juvix/pull/1667)
  ([paulcadman](https://github.com/paulcadman))
- Add documentation for compiling/running the TicTacToe example
  [#1664](https://github.com/anoma/juvix/pull/1664)
  ([paulcadman](https://github.com/paulcadman))
- Ignore binaries generated by running some tests
  [#1663](https://github.com/anoma/juvix/pull/1663)
  ([jonaprieto](https://github.com/jonaprieto))
- Conversion of Nat representation to JuvixCore integers
  [#1661](https://github.com/anoma/juvix/pull/1661)
  ([lukaszcz](https://github.com/lukaszcz))
- Move applications inside Lets and Cases
  [#1659](https://github.com/anoma/juvix/pull/1659)
  ([lukaszcz](https://github.com/lukaszcz))
- Run shelltests on macOS build
  [#1658](https://github.com/anoma/juvix/pull/1658)
  ([paulcadman](https://github.com/paulcadman))
- Restore macOS CI build/test
  [#1657](https://github.com/anoma/juvix/pull/1657)
  ([paulcadman](https://github.com/paulcadman))
- Remove type arguments and type abstractions from Nodes
  [#1655](https://github.com/anoma/juvix/pull/1655)
  ([lukaszcz](https://github.com/lukaszcz))
- Pretty printing of JuvixAsm code
  [#1650](https://github.com/anoma/juvix/pull/1650)
  ([lukaszcz](https://github.com/lukaszcz))
- Remove NameId from Core
  [#1649](https://github.com/anoma/juvix/pull/1649)
  ([lukaszcz](https://github.com/lukaszcz))
- Translation from JuvixAsm to C
  [#1619](https://github.com/anoma/juvix/pull/1619)
  ([lukaszcz](https://github.com/lukaszcz))

## [v0.2.7](https://github.com/anoma/juvix/tree/v0.2.7) (2022-12-05)

[Full Changelog](https://github.com/anoma/juvix/compare/v0.2.6...v0.2.7)

**Implemented enhancements:**

- Add juvix-repl-mode for emacs
  [#1612](https://github.com/anoma/juvix/pull/1612)
  ([paulcadman](https://github.com/paulcadman))
- Make lambda lifting correct when free variables occur in the types
  of binders [#1609](https://github.com/anoma/juvix/pull/1609)
  ([janmasrovira](https://github.com/janmasrovira))

**Merged pull requests:**

- Files pure refactor
  [#1652](https://github.com/anoma/juvix/pull/1652)
  ([janmasrovira](https://github.com/janmasrovira))
- Use the same stack version in all CI jobs and remove `stack setup`
  step [#1651](https://github.com/anoma/juvix/pull/1651)
  ([paulcadman](https://github.com/paulcadman))
- Fix 'not a primitive type' error message
  [#1648](https://github.com/anoma/juvix/pull/1648)
  ([lukaszcz](https://github.com/lukaszcz))
- Upgrade stack snapshot to use ghc-9.2.5
  [#1621](https://github.com/anoma/juvix/pull/1621)
  ([janmasrovira](https://github.com/janmasrovira))
- Add an emacs function to restart the REPL
  [#1618](https://github.com/anoma/juvix/pull/1618)
  ([paulcadman](https://github.com/paulcadman))
- Add types to Core functions and constructors when translating from
  Internal [#1617](https://github.com/anoma/juvix/pull/1617)
  ([paulcadman](https://github.com/paulcadman))
- Auto complete argument of 'dev core read -t'
  [#1616](https://github.com/anoma/juvix/pull/1616)
  ([janmasrovira](https://github.com/janmasrovira))
- Compute new entrypoint root when loading a file in the REPL
  [#1615](https://github.com/anoma/juvix/pull/1615)
  ([paulcadman](https://github.com/paulcadman))
- Compute maximum runtime stack height in JuvixReg
  [#1613](https://github.com/anoma/juvix/pull/1613)
  ([lukaszcz](https://github.com/lukaszcz))
- Remove shelltest threading
  [#1611](https://github.com/anoma/juvix/pull/1611)
  ([paulcadman](https://github.com/paulcadman))
- Use StackInfo and recurseS in the JuvixAsm to JuvixReg translation.
  [#1610](https://github.com/anoma/juvix/pull/1610)
  ([lukaszcz](https://github.com/lukaszcz))
- Precompute maximum heap allocation
  [#1608](https://github.com/anoma/juvix/pull/1608)
  ([lukaszcz](https://github.com/lukaszcz))
- Improvements to Juvix REPL
  [#1607](https://github.com/anoma/juvix/pull/1607)
  ([paulcadman](https://github.com/paulcadman))
- Fix discrepancy between Juvix and WASM pages
  [#1605](https://github.com/anoma/juvix/pull/1605)
  ([lukaszcz](https://github.com/lukaszcz))
- Compute JuvixAsm stack usage info
  [#1604](https://github.com/anoma/juvix/pull/1604)
  ([lukaszcz](https://github.com/lukaszcz))
- Improve As-Pattern parsing
  [#1603](https://github.com/anoma/juvix/pull/1603)
  ([ii8](https://github.com/ii8))
- Juvix core recursors should descend into nodes stored in infos
  [#1600](https://github.com/anoma/juvix/pull/1600)
  ([janmasrovira](https://github.com/janmasrovira))
- Add docs for installing the linux binary
  [#1599](https://github.com/anoma/juvix/pull/1599)
  ([paulcadman](https://github.com/paulcadman))
- Binder refactor [#1598](https://github.com/anoma/juvix/pull/1598)
  ([janmasrovira](https://github.com/janmasrovira))
- Juvix C runtime [#1580](https://github.com/anoma/juvix/pull/1580)
  ([lukaszcz](https://github.com/lukaszcz))
- As-patterns [#1576](https://github.com/anoma/juvix/pull/1576)
  ([ii8](https://github.com/ii8))
- Eta expansion at the top of each core function definition (#1481)
  [#1571](https://github.com/anoma/juvix/pull/1571)
  ([janmasrovira](https://github.com/janmasrovira))
- Add translation from Internal to Core
  [#1567](https://github.com/anoma/juvix/pull/1567)
  ([paulcadman](https://github.com/paulcadman))

## [v0.2.6](https://github.com/anoma/juvix/tree/v0.2.6) (2022-10-26)

[Full Changelog](https://github.com/anoma/juvix/compare/v0.2.5...v0.2.6)

**Implemented enhancements:**

- Support go to definition for the standard library
  [#1592](https://github.com/anoma/juvix/pull/1592)
  ([paulcadman](https://github.com/paulcadman))
- Add builtin if [#1585](https://github.com/anoma/juvix/pull/1585)
  ([paulcadman](https://github.com/paulcadman))
- Add builtin boolean
  [#1582](https://github.com/anoma/juvix/pull/1582)
  ([paulcadman](https://github.com/paulcadman))
- Add lambda expressions to internal and add typechecking support
  [#1538](https://github.com/anoma/juvix/pull/1538)
  ([janmasrovira](https://github.com/janmasrovira))

**Fixed bugs:**

- Fix arity checker bug
  [#1546](https://github.com/anoma/juvix/pull/1546)
  ([janmasrovira](https://github.com/janmasrovira))
- Look in patterns when building the dependency graph
  [#1536](https://github.com/anoma/juvix/pull/1536)
  ([janmasrovira](https://github.com/janmasrovira))

**Merged pull requests:**

- Update language reference to match current state of Juvix
  [#1594](https://github.com/anoma/juvix/pull/1594)
  ([paulcadman](https://github.com/paulcadman))
- Fix letrec printing
  [#1591](https://github.com/anoma/juvix/pull/1591)
  ([janmasrovira](https://github.com/janmasrovira))
- Update stdlib submodule with builtin changes
  [#1589](https://github.com/anoma/juvix/pull/1589)
  ([paulcadman](https://github.com/paulcadman))
- Rename builtin natural to nat and boolean to bool
  [#1588](https://github.com/anoma/juvix/pull/1588)
  ([paulcadman](https://github.com/paulcadman))
- Improve the test for eta-expansion of constructors and builtins
  [#1583](https://github.com/anoma/juvix/pull/1583)
  ([lukaszcz](https://github.com/lukaszcz))
- Properly newline expressions in the pretty printer
  [#1581](https://github.com/anoma/juvix/pull/1581)
  ([janmasrovira](https://github.com/janmasrovira))
- Letrec lifting [#1579](https://github.com/anoma/juvix/pull/1579)
  ([janmasrovira](https://github.com/janmasrovira))
- Add softlines between applications and hang definitions
  [#1578](https://github.com/anoma/juvix/pull/1578)
  ([janmasrovira](https://github.com/janmasrovira))
- Parse optional type info in JVC files
  [#1575](https://github.com/anoma/juvix/pull/1575)
  ([lukaszcz](https://github.com/lukaszcz))
- Fix symbol numbering bug
  [#1574](https://github.com/anoma/juvix/pull/1574)
  ([lukaszcz](https://github.com/lukaszcz))
- 1569 rewrite the test for lambda lifting to use evaluation
  [#1572](https://github.com/anoma/juvix/pull/1572)
  ([janmasrovira](https://github.com/janmasrovira))
- Remove lambda from reservedSymbols
  [#1568](https://github.com/anoma/juvix/pull/1568)
  ([lukaszcz](https://github.com/lukaszcz))
- Keywords refactor [#1566](https://github.com/anoma/juvix/pull/1566)
  ([janmasrovira](https://github.com/janmasrovira))
- remove ≔ from the language and replace it by :=
  [#1563](https://github.com/anoma/juvix/pull/1563)
  ([janmasrovira](https://github.com/janmasrovira))
- JuvixReg [#1551](https://github.com/anoma/juvix/pull/1551)
  ([lukaszcz](https://github.com/lukaszcz))
- Remove duplicate function in concrete analysis
  [#1550](https://github.com/anoma/juvix/pull/1550)
  ([ii8](https://github.com/ii8))
- Evaluator minor style refactor
  [#1547](https://github.com/anoma/juvix/pull/1547)
  ([janmasrovira](https://github.com/janmasrovira))
- Properly handle top lambdas in the termination checker
  [#1544](https://github.com/anoma/juvix/pull/1544)
  ([janmasrovira](https://github.com/janmasrovira))
- Mutual inference [#1543](https://github.com/anoma/juvix/pull/1543)
  ([janmasrovira](https://github.com/janmasrovira))
- Autocomplete ".jvc" input files for core {eval, read} commands
  [#1542](https://github.com/anoma/juvix/pull/1542)
  ([paulcadman](https://github.com/paulcadman))
- Add –show-de-bruijn to `core eval` command
  [#1540](https://github.com/anoma/juvix/pull/1540)
  ([paulcadman](https://github.com/paulcadman))
- Inductive types should depend on the types of their constructors
  [#1537](https://github.com/anoma/juvix/pull/1537)
  ([lukaszcz](https://github.com/lukaszcz))
- Parser labels [#1535](https://github.com/anoma/juvix/pull/1535)
  ([janmasrovira](https://github.com/janmasrovira))
- JuvixAsm [#1432](https://github.com/anoma/juvix/pull/1432)
  ([lukaszcz](https://github.com/lukaszcz))

## [v0.2.5](https://github.com/anoma/juvix/tree/v0.2.5) (2022-09-14)

[Full Changelog](https://github.com/anoma/juvix/compare/v0.2.4...v0.2.5)

**Fixed bugs:**

- Properly type check patterns that need normalization
  [#1472](https://github.com/anoma/juvix/pull/1472)
  ([janmasrovira](https://github.com/janmasrovira))
- Detect nested patterns as smaller in the termination checker
  [#1524](https://github.com/anoma/juvix/pull/1524)
- Fix developBeta in Core/Extra.hs
  [#1487](https://github.com/anoma/juvix/pull/1487)
  ([lukaszcz](https://github.com/lukaszcz))
- Core/Extra/Recursors/Collector bugfix
  [#1510](https://github.com/anoma/juvix/pull/1510)
  ([lukaszcz](https://github.com/lukaszcz))

**Merged pull requests:**

- Replace -\> by := in lambda syntax
  [#1533](https://github.com/anoma/juvix/pull/1533)
  ([janmasrovira](https://github.com/janmasrovira))
- 'Match' with complex patterns in Core
  [#1530](https://github.com/anoma/juvix/pull/1530)
  ([lukaszcz](https://github.com/lukaszcz))
- Refactor CLI [#1527](https://github.com/anoma/juvix/pull/1527)
  ([janmasrovira](https://github.com/janmasrovira))
- Add CanonicalProjection
  [#1526](https://github.com/anoma/juvix/pull/1526)
  ([janmasrovira](https://github.com/janmasrovira))
- Make comma a delimiter
  [#1525](https://github.com/anoma/juvix/pull/1525)
  ([lukaszcz](https://github.com/lukaszcz))
- Detect nested patterns as smaller in the termination checker
  [#1524](https://github.com/anoma/juvix/pull/1524)
  ([janmasrovira](https://github.com/janmasrovira))
- Disallow tab characters as spaces
  [#1523](https://github.com/anoma/juvix/pull/1523)
  ([janmasrovira](https://github.com/janmasrovira))
- Refactor `destruct` in Core/Extra/Base
  [#1522](https://github.com/anoma/juvix/pull/1522)
  ([lukaszcz](https://github.com/lukaszcz))
- JuvixCore primitive types
  [#1521](https://github.com/anoma/juvix/pull/1521)
  ([lukaszcz](https://github.com/lukaszcz))
- Enable autocompletion for the –theme flag
  [#1519](https://github.com/anoma/juvix/pull/1519)
  ([janmasrovira](https://github.com/janmasrovira))
- Stripped version of Core Node datatype
  [#1518](https://github.com/anoma/juvix/pull/1518)
  ([lukaszcz](https://github.com/lukaszcz))
- Add `internal core read` command
  [#1517](https://github.com/anoma/juvix/pull/1517)
  ([janmasrovira](https://github.com/janmasrovira))
- Implement some instances for BinderList
  [#1515](https://github.com/anoma/juvix/pull/1515)
  ([janmasrovira](https://github.com/janmasrovira))
- Back recursor types with type families
  [#1514](https://github.com/anoma/juvix/pull/1514)
  ([janmasrovira](https://github.com/janmasrovira))
- Eager evaluation of Constr arguments
  [#1513](https://github.com/anoma/juvix/pull/1513)
  ([lukaszcz](https://github.com/lukaszcz))
- Dynamic type in Core
  [#1508](https://github.com/anoma/juvix/pull/1508)
  ([lukaszcz](https://github.com/lukaszcz))
- LetRec in Core [#1507](https://github.com/anoma/juvix/pull/1507)
  ([lukaszcz](https://github.com/lukaszcz))
- Add Haddock and Agda licenses
  [#1506](https://github.com/anoma/juvix/pull/1506)
  ([janmasrovira](https://github.com/janmasrovira))
- Fix docs webapp examples CI build
  [#1505](https://github.com/anoma/juvix/pull/1505)
  ([paulcadman](https://github.com/paulcadman))
- Add CLI usage examples doc and integrate with README
  [#1504](https://github.com/anoma/juvix/pull/1504)
  ([paulcadman](https://github.com/paulcadman))
- Refactor BinderInfo
  [#1503](https://github.com/anoma/juvix/pull/1503)
  ([lukaszcz](https://github.com/lukaszcz))
- Make `juvix compile` default to native target
  [#1502](https://github.com/anoma/juvix/pull/1502)
  ([paulcadman](https://github.com/paulcadman))
- Refactor Node datatype
  [#1501](https://github.com/anoma/juvix/pull/1501)
  ([lukaszcz](https://github.com/lukaszcz))
- Clean up import list in Pipeline
  [#1499](https://github.com/anoma/juvix/pull/1499)
  ([jonaprieto](https://github.com/jonaprieto))
- Remove mono [#1497](https://github.com/anoma/juvix/pull/1497)
  ([jonaprieto](https://github.com/jonaprieto))
- Remove Haskell support
  [#1496](https://github.com/anoma/juvix/pull/1496)
  ([jonaprieto](https://github.com/jonaprieto))
- Implement lambda lifting
  [#1494](https://github.com/anoma/juvix/pull/1494)
  ([janmasrovira](https://github.com/janmasrovira))
- Document Emacs installation and the 'exec-path' problem
  [#1493](https://github.com/anoma/juvix/pull/1493)
  ([lukaszcz](https://github.com/lukaszcz))
- Add –allow-different-user to workflow stack command
  [#1492](https://github.com/anoma/juvix/pull/1492)
  ([paulcadman](https://github.com/paulcadman))
- Stack with github actions permissions workaround
  [#1490](https://github.com/anoma/juvix/pull/1490)
  ([paulcadman](https://github.com/paulcadman))
- Restructure recursors and add some lens interfaces
  [#1489](https://github.com/anoma/juvix/pull/1489)
  ([janmasrovira](https://github.com/janmasrovira))
- Add a github action to build a static linux binary
  [#1488](https://github.com/anoma/juvix/pull/1488)
  ([paulcadman](https://github.com/paulcadman))
- Fix developBeta in Core/Extra.hs
  [#1487](https://github.com/anoma/juvix/pull/1487)
  ([lukaszcz](https://github.com/lukaszcz))
- Add an option to show name ids in errors
  [#1486](https://github.com/anoma/juvix/pull/1486)
  ([lukaszcz](https://github.com/lukaszcz))

## [v0.2.4](https://github.com/anoma/juvix/tree/v0.2.4) (2022-08-19)

[Full Changelog](https://github.com/anoma/juvix/compare/v0.2.3...v0.2.4)

(Special version for Heliax's retreat in Italy)

**Implemented enhancements:**

- Add –stdin flag [#1459](https://github.com/anoma/juvix/pull/1459)
  ([janmasrovira](https://github.com/janmasrovira))

**Fixed bugs:**

- Fix typechecker [#1458](https://github.com/anoma/juvix/pull/1458)
  ([janmasrovira](https://github.com/janmasrovira))

**Merged pull requests:**

- use –stdin in flycheck mode
  [#1460](https://github.com/anoma/juvix/pull/1460)
  ([janmasrovira](https://github.com/janmasrovira))
- Add a native compile target for demos
  [#1457](https://github.com/anoma/juvix/pull/1457)
  ([paulcadman](https://github.com/paulcadman))
- Small changes for the presentation
  [#1456](https://github.com/anoma/juvix/pull/1456)
  ([jonaprieto](https://github.com/jonaprieto))
- Fixes TicTacToe Web example
  [#1454](https://github.com/anoma/juvix/pull/1454)
  ([paulcadman](https://github.com/paulcadman))
- Upgrade to ghc-9.2.4
  [#1451](https://github.com/anoma/juvix/pull/1451)
  ([janmasrovira](https://github.com/janmasrovira))

## [v0.2.3](https://github.com/anoma/juvix/tree/v0.2.3) (2022-08-15)

[Full Changelog](https://github.com/anoma/juvix/compare/v0.2.2...v0.2.3)

**Implemented enhancements:**

- add `name` and `version` to `juvix.yaml`
  [#1422](https://github.com/anoma/juvix/pull/1422)
  ([janmasrovira](https://github.com/janmasrovira))

**Fixed bugs:**

- Properly handle paragraphs in judoc
  [#1447](https://github.com/anoma/juvix/pull/1447)
  ([janmasrovira](https://github.com/janmasrovira))

**Merged pull requests:**

- Give a proper type to literal natural numbers
  [#1453](https://github.com/anoma/juvix/pull/1453)
  ([janmasrovira](https://github.com/janmasrovira))
- Add the option to output json in the `juvix internal highlight`
  command [#1450](https://github.com/anoma/juvix/pull/1450)
  ([janmasrovira](https://github.com/janmasrovira)) for supporting the
  new Juvix Mode for Visual Studio Code
  ([jonaprieto](https://github.com/anoma/vscode-juvix))
- Allow \_ in Wasm exported names to support Anoma signature
  [#1449](https://github.com/anoma/juvix/pull/1449)
  ([paulcadman](https://github.com/paulcadman))
- Add Towers of Hanoi and Pascal triangle examples
  [#1446](https://github.com/anoma/juvix/pull/1446)
  ([paulcadman](https://github.com/paulcadman))
- Add `juvix init` command
  [#1445](https://github.com/anoma/juvix/pull/1445)
  ([janmasrovira](https://github.com/janmasrovira))
- Refactor pretty to reduce duplication
  [#1443](https://github.com/anoma/juvix/pull/1443)
  ([janmasrovira](https://github.com/janmasrovira))
- Add initial support for examples in Html documentation
  [#1442](https://github.com/anoma/juvix/pull/1442)
  ([janmasrovira](https://github.com/janmasrovira))
- Add revisions to README
  [#1440](https://github.com/anoma/juvix/pull/1440)
  ([jonaprieto](https://github.com/jonaprieto))
- CI: Run build on push to main
  [#1437](https://github.com/anoma/juvix/pull/1437)
  ([paulcadman](https://github.com/paulcadman))
- Add doctor subcommand
  [#1436](https://github.com/anoma/juvix/pull/1436)
  ([paulcadman](https://github.com/paulcadman))
- CI checkout repo before cache and use recommended cache strategy
  [#1435](https://github.com/anoma/juvix/pull/1435)
  ([paulcadman](https://github.com/paulcadman))
- Various documentation adjustments
  [#1434](https://github.com/anoma/juvix/pull/1434)
  ([paulcadman](https://github.com/paulcadman))
- Setup Clang before building docs in CI
  [#1433](https://github.com/anoma/juvix/pull/1433)
  ([paulcadman](https://github.com/paulcadman))
- Major revisions to Makefile
  [#1431](https://github.com/anoma/juvix/pull/1431)
  ([jonaprieto](https://github.com/jonaprieto))
- Do not add `-src` suffix to links in HTML when running `juvix html`
  [#1429](https://github.com/anoma/juvix/pull/1429)
  ([paulcadman](https://github.com/paulcadman))
- Add a Web version of TicTacToe
  [#1427](https://github.com/anoma/juvix/pull/1427)
  ([paulcadman](https://github.com/paulcadman))
- WASM import all non-compile axioms with alphanum names in entrypoint
  [#1426](https://github.com/anoma/juvix/pull/1426)
  ([paulcadman](https://github.com/paulcadman))
- Export all functions with alpha numeric names from entrypoint module
  [#1425](https://github.com/anoma/juvix/pull/1425)
  ([paulcadman](https://github.com/paulcadman))
- Refactor [#1420](https://github.com/anoma/juvix/pull/1420)
  ([jonaprieto](https://github.com/jonaprieto))
- Permit axiom without a compile block
  [#1418](https://github.com/anoma/juvix/pull/1418)
  ([paulcadman](https://github.com/paulcadman))
- Implement an html documentation generator similar to haddock (#1413)
  [#1416](https://github.com/anoma/juvix/pull/1416)
  ([janmasrovira](https://github.com/janmasrovira))
- Fix version shell test for 0.2.2
  [#1415](https://github.com/anoma/juvix/pull/1415)
  ([paulcadman](https://github.com/paulcadman))
- Remove Int from stdlib and update SimpleFungibleToken example
  [#1414](https://github.com/anoma/juvix/pull/1414)
  ([paulcadman](https://github.com/paulcadman))

## [v0.2.2](https://github.com/anoma/juvix/tree/v0.2.2) (2022-07-25)

[Full Changelog](https://github.com/anoma/juvix/compare/v0.2.1...v0.2.2)

**Implemented enhancements:**

- Compute name dependency graph and filter unreachable definitions
  [#1408](https://github.com/anoma/juvix/pull/1408)
  ([lukaszcz](https://github.com/lukaszcz))
- Support type aliases
  [#1404](https://github.com/anoma/juvix/pull/1404)
  ([janmasrovira](https://github.com/janmasrovira))
- Add debugging custom function to Prelude
  [#1401](https://github.com/anoma/juvix/pull/1401)
  ([jonaprieto](https://github.com/jonaprieto))
- Add positivity check for data types
  [#1393](https://github.com/anoma/juvix/pull/1393)
  ([jonaprieto](https://github.com/jonaprieto))
- Keep qualified names
  [#1392](https://github.com/anoma/juvix/pull/1392)
  ([janmasrovira](https://github.com/janmasrovira))
- Direct translation from MicroJuvix to MiniC
  [#1386](https://github.com/anoma/juvix/pull/1386)
  ([lukaszcz](https://github.com/lukaszcz))
- Widens the accepted symbol list
  [#1385](https://github.com/anoma/juvix/pull/1385)
  ([mariari](https://github.com/mariari))
- Check all the type parameter names are different when declaring an
  inductive type [#1377](https://github.com/anoma/juvix/pull/1377)
  ([jonaprieto](https://github.com/jonaprieto))

**Fixed bugs:**

- Curly braces are allowed nested in patterns
  [#1380](https://github.com/anoma/juvix/pull/1380)
  ([janmasrovira](https://github.com/janmasrovira))

**Merged pull requests:**

- Add `Fail` effect (#1409)
  [#1411](https://github.com/anoma/juvix/pull/1411)
  ([janmasrovira](https://github.com/janmasrovira))
- Refactor of typechecking and other checking processes
  [#1410](https://github.com/anoma/juvix/pull/1410)
  ([jonaprieto](https://github.com/jonaprieto))
- Use bold for code in scoper error messages
  [#1403](https://github.com/anoma/juvix/pull/1403)
  ([janmasrovira](https://github.com/janmasrovira))
- Replace ppSimple by text
  [#1402](https://github.com/anoma/juvix/pull/1402)
  ([jonaprieto](https://github.com/jonaprieto))
- Implement some error messages (#1396)
  [#1400](https://github.com/anoma/juvix/pull/1400)
  ([lukaszcz](https://github.com/lukaszcz))
- Refactor childs of pattern parentheses and braces
  [#1398](https://github.com/anoma/juvix/pull/1398)
  ([janmasrovira](https://github.com/janmasrovira))
- Update Juvix standard-library
  [#1389](https://github.com/anoma/juvix/pull/1389)
  ([jonaprieto](https://github.com/jonaprieto))
- Fix documentation generation
  [#1387](https://github.com/anoma/juvix/pull/1387)
  ([jonaprieto](https://github.com/jonaprieto))
- Adds Collatz sequence generator example
  [#1384](https://github.com/anoma/juvix/pull/1384)
  ([paulcadman](https://github.com/paulcadman))
- html-examples [#1381](https://github.com/anoma/juvix/pull/1381)
  ([jonaprieto](https://github.com/jonaprieto))
- Refine hole in type signature to function type
  [#1379](https://github.com/anoma/juvix/pull/1379)
  ([janmasrovira](https://github.com/janmasrovira))
- Type checking fails when the type of a pattern is not given by the
  signature [#1378](https://github.com/anoma/juvix/pull/1378)
  ([janmasrovira](https://github.com/janmasrovira))
- Set cname for gh-pages action
  [#1376](https://github.com/anoma/juvix/pull/1376)
  ([paulcadman](https://github.com/paulcadman))
- Add fibonacci sequence example program
  [#1375](https://github.com/anoma/juvix/pull/1375)
  ([paulcadman](https://github.com/paulcadman))
- Fix Changelog links and minors
  [#1371](https://github.com/anoma/juvix/pull/1371)
  ([jonaprieto](https://github.com/jonaprieto))
- Add Version number to the emacs mode
  [#1320](https://github.com/anoma/juvix/pull/1320)
  ([mariari](https://github.com/mariari))

## New name: Juvix

Since version 0.2.2, the project has been renamed from "Mini Juvix" to
"Juvix". The new name reflects the fact that the project is no longer
just a compiler for a subset of Juvix, but a full implementation of the
language. Affected by this change are:

- Github repository moved from the Heliax organization to the Anoma
  organization. "anoma/juvix" is the new repository name.
- All references to "Mini Juvix" have been replaced with "Juvix".
  Unfortunetly,

due to the move, the old links to the Mini Juvix repository are broken
and will not be fixed.

## v0.2.1 (2022-07-12)

**Implemented enhancements:**

- Specialize commands of/for internal use MiniJuvix-#270
  ([jonaprieto](https://github.com/jonaprieto))
- Improve handling of location information for different objs
  MiniJuvix-#263 ([jonaprieto](https://github.com/jonaprieto))
- Add issues and PR templates MiniJuvix-#261
  ([jonaprieto](https://github.com/jonaprieto))
- Throw error when reading a file that conflicts with embedded stdlib
  MiniJuvix-#243 ([paulcadman](https://github.com/paulcadman))
- Embed standard library in the minijuvix binary MiniJuvix-#210
  ([paulcadman](https://github.com/paulcadman))

**Fixed bugs:**

- Fixed a bug with the path to walloc.c MiniJuvix-#237
  ([lukaszcz](https://github.com/lukaszcz))
- Perform ScopedToAbstract exactly once for each module MiniJuvix-#223
  ([paulcadman](https://github.com/paulcadman))

**Merged pull requests:**

- Label renaming MiniJuvix-#275
  ([jonaprieto](https://github.com/jonaprieto))
- Update link to discord MiniJuvix-#264
  ([Romainua](https://github.com/Romainua))
- Include `open import` statements when generating HTML MiniJuvix-#260
  ([paulcadman](https://github.com/paulcadman))
- Renaming MiniJuvix to Juvix MiniJuvix-#259
  ([jonaprieto](https://github.com/jonaprieto))
- Updates tests to use the updated standard library MiniJuvix-#253
  ([paulcadman](https://github.com/paulcadman))
- Enforce C99 standard in the generated C files MiniJuvix-#252
  ([lukaszcz](https://github.com/lukaszcz))
- Restore mascot images to the minijuvix book MiniJuvix-#250
  ([paulcadman](https://github.com/paulcadman))
- Allow jumping to another module in emacs MiniJuvix-#249
  ([janmasrovira](https://github.com/janmasrovira))
- Restore Juvix mascot image to README MiniJuvix-#248
  ([paulcadman](https://github.com/paulcadman))
- Add emacs option `minijuvix-disable-embedded-stdlib` MiniJuvix-#247
  ([paulcadman](https://github.com/paulcadman))
- Deprecate GHC backend MiniJuvix-#244
  ([lukaszcz](https://github.com/lukaszcz))
- Removed 'eval' and 'print' keywords (#214) MiniJuvix-#242
  ([lukaszcz](https://github.com/lukaszcz))
- Add option to disable minijuvix input method MiniJuvix-#239
  ([janmasrovira](https://github.com/janmasrovira))
- Remove the 'match' keyword MiniJuvix-#238
  ([lukaszcz](https://github.com/lukaszcz))
- Removed tests/positive/HelloWorld.mjuvix and specified clang version
  in the documentation MiniJuvix-#236
  ([lukaszcz](https://github.com/lukaszcz))
- Filter symbol entries properly in the scoper MiniJuvix-#234
  ([janmasrovira](https://github.com/janmasrovira))
- Use the ModulesCache for `open` statements in ScopedToAbstract pass
  MiniJuvix-#224 ([paulcadman](https://github.com/paulcadman))
- README: Include `--recursive` in git clone command to fetch stdlib
  MiniJuvix-#211 ([paulcadman](https://github.com/paulcadman))
- Update project description v0.2.0 MiniJuvix-#209
  ([jonaprieto](https://github.com/jonaprieto))
- Unify AST representation of types and expressions in MicroJuvix
  MiniJuvix-#188 ([janmasrovira](https://github.com/janmasrovira))

## v0.2.0 (2022-06-28)

**Implemented enhancements:**

- Support built in types MiniJuvix-#192
  ([janmasrovira](https://github.com/janmasrovira))
- Support partial application and closure passing in C backend
  MiniJuvix-#190 ([paulcadman](https://github.com/paulcadman))
- Allow `open import` statements MiniJuvix-#175
  ([janmasrovira](https://github.com/janmasrovira))
- Remove TypeAny and adapt typechecking for literals MiniJuvix-#173
  ([janmasrovira](https://github.com/janmasrovira))
- Allow holes to be refined into function types MiniJuvix-#165
  ([janmasrovira](https://github.com/janmasrovira))
- Support implicit arguments MiniJuvix-#144
  ([janmasrovira](https://github.com/janmasrovira))
- Add support for holes in type signatures MiniJuvix-#141
  ([janmasrovira](https://github.com/janmasrovira))
- Support function closures with no environment in minic
  MiniJuvix-#137 ([paulcadman](https://github.com/paulcadman))
- Add holes for expressions in function clauses and inference support
  MiniJuvix-#136 ([janmasrovira](https://github.com/janmasrovira))
- Add "-Oz" optimization flag to clang args MiniJuvix-#133
  ([paulcadman](https://github.com/paulcadman))
- Add version and help option and root command to the CLI
  MiniJuvix-#131 ([jonaprieto](https://github.com/jonaprieto))

**Fixed bugs:**

- Fix: Ignore implicit patterns and arguments in termination checking
  MiniJuvix-#172 ([janmasrovira](https://github.com/janmasrovira))
- Fix: pretty printing for terminating keyword MiniJuvix-#145
  ([jonaprieto](https://github.com/jonaprieto))

**Merged pull requests:**

- Fix: proper error handling for typechecker errors MiniJuvix-#189
  ([jonaprieto](https://github.com/jonaprieto))
- Add juvix version info and date to HTML output MiniJuvix-#186
  ([jonaprieto](https://github.com/jonaprieto))
- Fix: Add check for constructor return types MiniJuvix-#182
  ([jonaprieto](https://github.com/jonaprieto))
- Use Abstract name in Abstract syntax and Micro/MonoJuvix
  MiniJuvix-#181 ([janmasrovira](https://github.com/janmasrovira))
- Add an option to specify the path where to put the HTML output
  MiniJuvix-#179 ([jonaprieto](https://github.com/jonaprieto))
- Upgrade to ghc-9.2.3 MiniJuvix-#178
  ([janmasrovira](https://github.com/janmasrovira))
- Replace dead link in README with a link to the Juvix book
  MiniJuvix-#177 ([paulcadman](https://github.com/paulcadman))
- Embed HTML assets in the juvix binary MiniJuvix-#176
  ([paulcadman](https://github.com/paulcadman))
- Fix: identifiers with a keyword prefix cannot be parsed
  MiniJuvix-#171 ([janmasrovira](https://github.com/janmasrovira))
- Improve filepath equality MiniJuvix-#170
  ([janmasrovira](https://github.com/janmasrovira))
- Update validity predicate milestone example to 0.2 syntax
  MiniJuvix-#167 ([paulcadman](https://github.com/paulcadman))
- Fix links in documentation and update to new syntax MiniJuvix-#163
  ([paulcadman](https://github.com/paulcadman))
- Update stdlib to work with version 0.2 MiniJuvix-#160
  ([janmasrovira](https://github.com/janmasrovira))
- Update README usage example to use the compile command
  MiniJuvix-#158 ([paulcadman](https://github.com/paulcadman))
- Remove dead code related to the pipeline MiniJuvix-#156
  ([janmasrovira](https://github.com/janmasrovira))
- Add negative test for AppLeftImplicit MiniJuvix-#154
  ([janmasrovira](https://github.com/janmasrovira))
- Add positive test designed for implicit arguments MiniJuvix-#153
  ([janmasrovira](https://github.com/janmasrovira))
- Remove ExpressionTyped from MicroJuvix MiniJuvix-#143
  ([janmasrovira](https://github.com/janmasrovira))
- Revision for package.yaml and minor deletions MiniJuvix-#135
  ([jonaprieto](https://github.com/jonaprieto))

## v0.1.4 (2022-05-30)

**Merged pull requests:**

- Generic Errors and refactoring MiniJuvix-#123
  ([jonaprieto](https://github.com/jonaprieto))
- Only generates docs if the pull request merges MiniJuvix-#121
  ([jonaprieto](https://github.com/jonaprieto))
- Add initial docs generation website MiniJuvix-#119
  ([jonaprieto](https://github.com/jonaprieto))
- Fix internal link in README MiniJuvix-#116
  ([paulcadman](https://github.com/paulcadman))
- Add minic-runtime for linking without libc MiniJuvix-#113
  ([paulcadman](https://github.com/paulcadman))
- Add termination checking to the pipeline MiniJuvix-#111
  ([jonaprieto](https://github.com/jonaprieto))
- Support uncurried higher order functions MiniJuvix-#110
  ([paulcadman](https://github.com/paulcadman))
- Improve error generation and handling MiniJuvix-#108
  ([janmasrovira](https://github.com/janmasrovira))
- Add MiniC tests with clang+wasi-sdk MiniJuvix-#105
  ([paulcadman](https://github.com/paulcadman))
- Add usage example and move developer docs MiniJuvix-#96
  ([paulcadman](https://github.com/paulcadman))
- Refactor warning related stuff MiniJuvix-#91
  ([janmasrovira](https://github.com/janmasrovira))
- Remove Agda backend MiniJuvix-#86
  ([paulcadman](https://github.com/paulcadman))

**Implemented enhancements:**

- Add `compile` subcommand to generate binaries MiniJuvix-#128
- Add intervals to flycheck errors MiniJuvix-#124
- Improve error handling in juvix-mode MiniJuvix-#107
- Support multiple modules in compilation MiniJuvix-#93
- Add compile command to CLI MiniJuvix-#130
  ([paulcadman](https://github.com/paulcadman))
- Use Interval in GenericErrors MiniJuvix-#125
  ([janmasrovira](https://github.com/janmasrovira))
- Remove dev in the CI and other tweaks MiniJuvix-#118
  ([jonaprieto](https://github.com/jonaprieto))
- Highlight comments correctly MiniJuvix-#106
  ([janmasrovira](https://github.com/janmasrovira))
- Support multiple modules in compilation MiniJuvix-#100
  ([janmasrovira](https://github.com/janmasrovira))
- New target syntax and modular VP examples MiniJuvix-#92
  ([jonaprieto](https://github.com/jonaprieto))

**Fixed bugs:**

- Missing error messages when using throw/error MiniJuvix-#117
- Fix highlight of comments MiniJuvix-#104
- Fix juvix-mode coloring for projects with multiple modules
  MiniJuvix-#101
- Fix `highlight` command for modules with import statements
  MiniJuvix-#102 ([janmasrovira](https://github.com/janmasrovira))

**Closed issues:**

- Deprecate the class JuvixError MiniJuvix-#115
- Add ToGenericError instance for the infix parsing errors
  MiniJuvix-#114
- Compile to WASM without linking libc MiniJuvix-#112
- Add the termination checker to the pipeline MiniJuvix-#109
- Use clang + wasi-sdk instead of emcc to compile to WASM
  MiniJuvix-#103
- Move developer tooling docs out of README MiniJuvix-#95
- Add pre-commit checks to CI checks MiniJuvix-#94
- Support higher order functions in C backend MiniJuvix-#90
- Remove dev from the list of branches in the CI MiniJuvix-#89
- Refactor warning related stuff MiniJuvix-#87
- The Juvix website MiniJuvix-#51

## v0.1.3 (2022-05-05)

**Closed issues:**

- Monomorphisation naming inconsistency MiniJuvix-#84
- Remove BackendAgda MiniJuvix-#83
- Change terminating keyword behavior MiniJuvix-#81
- MonoJuvix `ExpressionTyped` is never used MiniJuvix-#79
- Bump stackage nightly and delete `allow-newer: true` from
  `stack.yaml` MiniJuvix-#75
- Generate automatically CHANGELOG and Github Release Notes
  MiniJuvix-#73
- Make flag –show-name-ids global MiniJuvix-#61
- Add C code generation backend MiniJuvix-#60
- Add polymorphism MiniJuvix-#59
- Add the compile keyword to the frontend syntax (support up to
  Scoping) MiniJuvix-#58
- Error with undefined or underscores MiniJuvix-#54
- Add support for other GHC and Stack stable version MiniJuvix-#52
- Autodetect output ANSI support when prettyprinting MiniJuvix-#38
- Terminating for type signatures MiniJuvix-#11

**Merged pull requests:**

- Remove agda backend MiniJuvix-#86
  ([paulcadman](https://github.com/paulcadman))
- 84 monomorphisation naming inconsistency MiniJuvix-#85
  ([janmasrovira](https://github.com/janmasrovira))
- Change terminating keyword behavior MiniJuvix-#82
  ([jonaprieto](https://github.com/jonaprieto))
- Remove unused constructor ExpressionTyped in Monojuvix MiniJuvix-#80
  ([janmasrovira](https://github.com/janmasrovira))
- Stricter stack builds and pedantic mode for CI MiniJuvix-#78
  ([jonaprieto](https://github.com/jonaprieto))
- Bump stackage version and remove allow-newer MiniJuvix-#76
  ([janmasrovira](https://github.com/janmasrovira))
- Add automatically updates/issues/merged PRs to the changelog
  MiniJuvix-#74 ([jonaprieto](https://github.com/jonaprieto))
- Add terminating keyword MiniJuvix-#71
  ([jonaprieto](https://github.com/jonaprieto))
- Monomorphization MiniJuvix-#70
  ([janmasrovira](https://github.com/janmasrovira))
- Remove StatementCompile in AST after scoping MiniJuvix-#69
  ([paulcadman](https://github.com/paulcadman))
- Add C code generation backend MiniJuvix-#68
  ([paulcadman](https://github.com/paulcadman))
- Check if stderr supports ANSI and print accordingly MiniJuvix-#67
  ([janmasrovira](https://github.com/janmasrovira))
- Add support for compile (by Jonathan) MiniJuvix-#66
  ([paulcadman](https://github.com/paulcadman))
- Add NameIdGen effect to the pipeline MiniJuvix-#64
  ([janmasrovira](https://github.com/janmasrovira))
- Make the `--show-name-ids` flag global MiniJuvix-#63
  ([janmasrovira](https://github.com/janmasrovira))
- Implement type checker with polymorphism MiniJuvix-#62
  ([janmasrovira](https://github.com/janmasrovira))

## v0.1.2 (2022-04-11)

**Closed issues:**

- Add en emacs mode with support for scoped highlighting MiniJuvix-#25
- Add support for project root detection through a juvix.yaml file
  MiniJuvix-#24
- Add CLI cmd to generate juvix autocompletion files for fish and zsh
  MiniJuvix-#23
- Add pretty and typecheck subcommands to the microjuvix CLI
  MiniJuvix-#21
- Translate identifiers from MicroJuvix to MiniHaskell (valid Haskell)
  MiniJuvix-#19
- Implement the MiniHaskell to Haskell translation (prettyprinter)
  MiniJuvix-#18
- Implementation of a typechecker for MicroJuvix MiniJuvix-#16
- Add references to the Abstract AST to update compilation to
  MiniHaskell MiniJuvix-#12
- Order in the house MiniJuvix-#10

**Merged pull requests:**

- The Juvix project now follows the same goals as the original Juvix
  project. MiniJuvix-#7 ([jonaprieto](https://github.com/jonaprieto))
- Dev→main MiniJuvix-#6 ([jonaprieto](https://github.com/jonaprieto))
- Big update including termination checking MiniJuvix-#5
  ([janmasrovira](https://github.com/janmasrovira))
- Parser and scoper MiniJuvix-#3
  ([jonaprieto](https://github.com/jonaprieto))
- Upgrade to ghc9 and use hpack MiniJuvix-#2
  ([janmasrovira](https://github.com/janmasrovira))
- Merge MiniJuvix-#1 ([jonaprieto](https://github.com/jonaprieto))

## v0.1.1 (2022-03-25)

- Add support in the parser/scoper for Axiom backends
- Add support for `foreign` keyword
- Add flag `--no-colors` for the scope command
- Upgrade to GHC 9.2.2
- Improve resolution of local symbols in the scoper
- Several new tests related to ambiguous symbols
- Add `--version` flag
- Add InfoTableBuilder effect for the scoper

**Closed issues:**

- Add diff output to the test suite MiniJuvix-#9
- Improve scoper ambiguity error messages MiniJuvix-#8
