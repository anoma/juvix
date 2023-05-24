# Contributing to Juvix

Thank you for considering contributing to Juvix! We welcome all contributions, big or small,
of any kind. We appreciate any help/feedback we can get.

## Getting Started

Make sure you have followed the [installation instructions][installation] and have a working Juvix installation. You can also use the web-based development environment ready to the Juvix development, [Juvix Github Codespace][juvix-codespace]

1. Fork the repository.
2. Clone your forked repository to your local machine.
3. Install [Stack][stack] if you
   haven't already.
4. Build the project by running `stack build`. To build the project with
   optimizations, run `stack build --fast`. To install the binaries to your
   local `~/.local/bin`, run `stack install`.
5. Run the tests by running `stack test`.
6. Make sure to install the [pre-commit][pre-commit] binary, so you
   can run the pre-commit hooks by running `make precommit` in the root
   directory of the project. All the Pull Requests will be checked by the
   pre-commit hooks.

## Making Changes

1. Create a new branch for your changes: `git checkout -b my-branch-name`. In case you are working on an issue, please name your branch after the issue number, e.g. `issue-123`.
2. Make your changes and commit them with a descriptive message.
3. Push your changes to your forked repository: `git push origin my-branch-name`.
4. Submit a pull request to the main repository with a concise description of your changes.
5. Make sure that your pull request passes all the tests and pre-commit hooks.

## Haskell Code Style

We value readability and maintainability over saving lines of code. The best
source of truth for the Juvix code style is the existing codebase. We strongly
encourage you to look at the existing code and follow the same style. Open an
issue if you have any questions, or better yet, join our
[Discord][discord] and ask there!

Some basic guidelines when writing code:

- Use clear and descriptive names for variables, functions, and types.
- Keep functions short and focused on a single task. Separate functions when
  they start to get too long.
- Use comments to explain complex or non-obvious code.
- Run `make format` to format your code with `ormolu`.

## Testing

Please include tests for any new functionality or bug fixes. The tests are
located in the `test` directory, the tests are written in Haskell and use the
tasty framework. To run the tests, run `stack test`. If you are changing the
CLI, please also update the smoke tests in the `tests/smoke` directory.

## Code Review

All pull requests will be reviewed by at least one member of the development team. Feedback may be provided on the code itself, as well as on the tests and documentation.

Thank you for contributing to Juvix!

[installation]: https://docs.juvix.org/dev/howto/installing/
[juvix-codespace]: https://github.com/codespaces/new?hide_repo_select=true&ref=main&repo=102404734&machine=standardLinux32gb&location=WestEurope
[stack]: https://docs.haskellstack.org/en/stable/README/
[pre-commit]: https://pre-commit.com/
[discord]: https://discord.gg/PfaaFVErHt
