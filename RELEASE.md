# Juvix Release Process

## Step 1. PR to update the release version

Merge a PR against anoma/juvix main that contains the following:

1. Update [package.yaml](package.yaml) with the new `version`
2. Update the `cli-flag-version` smoke test in [tests/smoke/Commands/version-help-doctor.smoke.yaml](tests/smoke/Commands/version-help-doctor.smoke.yaml)
3. Update [CHANGELOG.md](CHANGELOG.md)
   To generate the new CHANGELOG entries:

   1. In the GitHub Web UI Releases>Create a new release
   2. Fill in 'tag' and 'previous tag' fields
   3. Click 'Generate release notes' and copy the list of merged PRs into [CHANGELOG.md](CHANGELOG.md)
   4. :warning: *Do not create the Release*
   5. Apply the following sed scripts so that the markup matches previous releases:
      1. `s/*/-`
      2. `s/https:\/\/github.com\/anoma\/juvix\/pull\/\(\d*\)/\[\\#\1](\0)`
      3. `s/\(.*?\)by \(@.*?\) in \(\[.*\)/\1\3 \2`
      4. `s/@\(\(\w\|-\)*\)$/([\1](https:\/\/github.com\/\1))`

   NB: We used to use https://github.com/github-changelog-generator/github-changelog-generator to generate the CHANGELOG.md but this stopped working some time in 2024. The markup it used differs from the Github 'Generate release notes' which is why we need to modify it to match the old markup.
4. Make sure that the juvix-stdlib submodule references the main branchg of the juvix-stdlib repo.
5. Wait for tests to pass / approval and merge the PR into main

Example PR: https://github.com/anoma/juvix/pull/3254

## Step 2. Trigger the 'Juvix Release' workflow

The Juvix Release workflow builds the Juvix release binary for:

1. Linux x86_64 (statically linked)
2. macOS x86_64
3. macOS aarch64

It then creates a draft release on GitHub with tag `v$JUVIX_VERSION` where `$JUVIX_VERSION` is obtained from the output of `juvix --version`.

## Step 3: Update `homebrew-juvix` formula

Update the `version` class field and the sha256 hashes of the Juvix tarballs in [anoma/homebrew-juvix:Formula/juvix.rb](https://github.com/anoma/homebrew-juvix/blob/main/Formula/juvix.rb).

The sha256 hashes can be obtained from files (named `*.sha256`) in the release assets published in Step 2.

## Step 4: Publish the new 'Juvix Release'

In the Github Web UI edit the draft release created in Step 2. set it as latest release, and publish it.
