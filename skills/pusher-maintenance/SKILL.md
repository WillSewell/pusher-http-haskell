---
name: pusher-maintenance
description: Update dependency bounds, Stack resolvers, GHC test coverage, and prepare releases for WillSewell/pusher-http-haskell. Use for dependency release notifications or maintenance and release requests in this repository.
---

# Pusher HTTP Haskell maintenance

Apply only to `WillSewell/pusher-http-haskell`. Read repository instructions
and `CONTRIBUTING.md`, inspect the working tree, and preserve unrelated work.
Determine the actual remote default branch (historically `master`). Discover
current versions online; do not freeze resolver, compiler, or package versions
from this skill or reuse an old notification without checking its release.

## Scope and handoffs

A full maintenance invocation covers implementation, testing, pushing branches,
opening two successive PRs, and publishing the release after the user merges
them. The user reviews and merges both PRs. Never merge PRs or enable auto-merge.
Honor narrower requests such as preparing a PR without publishing a release.
Use `maintenance/` branches unless the user specifies otherwise.

Check GitHub authentication before opening PRs and Hackage credential
availability before the publication step. Use read-only checks where possible;
do not use a trial upload to test credentials. If interactive authentication
is needed, have the user authenticate in their terminal, not in chat. Complete
all independent preparation before handing a remaining action to the user.

Finish all independent work before each merge handoff. Watch CI for the current
PR head, fix failures, and report the ready PR. If it remains unmerged, leave a
clear resume point rather than pretending the release is complete. On resuming,
verify actual merge status and work from the merged default-branch contents;
a request to continue is not proof that a PR was merged. Do not create a
scheduled monitor unless requested.

## Dependency bounds and resolver updates

For the notified dependency, admit the newly released version and cap the upper
bound at the next second-component version: `2.3.0.0` means `<2.4`, `0.22.0`
means `<0.23`, and `1.16` means `<1.17`. This repository's convention is not a
generic interpretation of SemVer or PVP. Preserve existing lower bounds and
support for older releases unless incompatibility requires a change. Existing
`==A.B.*` constraints may become a range when adding a newer series. Do not
rewrite unconstrained test dependencies merely to normalize their formatting.

Always advance `stack.yaml` to the latest published LTS and `stack-nightly.yaml`
to the latest published nightly. Discover snapshots and their contents from
Stackage and its snapshot repository. Latest means the latest available
snapshot, not a guessed nightly date. Use GHC's release information to verify
the latest released stable compiler; prereleases are not the default target.

Ensure the upgraded dependency is actually tested at the new version, usually
in nightly. Add an exact Hackage version to that configuration's `extra-deps`
when its snapshot does not supply it. Add transitive overrides only as needed.
LTS and older-compiler configurations may deliberately retain older compatible
versions; do not force a new dependency onto a compiler it cannot support.
Record the dependency versions selected by each relevant build so that tests
against an old snapshot cannot be mistaken for testing the new release.

For each updated configuration, re-evaluate every `extra-deps` entry and every
`allow-newer-deps` entry, including GitHub pins. Prefer the resolver's version
when it is the pinned version or newer and is compatible with the build and
the intended test target. Verify cleanup by rebuilding. Version comparison
alone does not justify removing an override required by a compiler override.
Remove unused flags and `allow-newer` settings when their reason disappears.

When a transitive package's upper bound blocks an otherwise compatible build,
use `allow-newer: true` with a minimal explicit `allow-newer-deps` list. List
the packages declaring the restrictive bounds, not merely the dependency they
reject. Explain each exception in a comment or the PR. Do not use blanket
bound relaxation as the finished solution, and do not treat it as a fix for
source incompatibility or ignored lower bounds.

## GHC coverage

Preserve library support unless there is a demonstrated reason it must be
dropped. Test the latest patch release in each supported GHC major/minor
series with an LTS resolver, plus the latest released GHC through nightly.
Intermediate GHC series available only through nightly need no separate test
configuration. Preserve the existing lower library bounds; do not equate
`tested-with` with a Cabal constraint.

Use one LTS configuration per supported GHC series, selecting the latest LTS
resolver using that series. Always keep the latest published LTS in
`stack.yaml`; use explicitly versioned `stack-<lts-version>.yaml` files only
for older supported GHC series with an LTS, with matching `.lock` files.
Follow the repository convention. Update filenames and CI references when a
newer LTS for an older series exists. Check the compiler's actual patch version. If the
latest LTS for a series lags its latest patch release, use an explicit compiler
override and resolve the resulting package constraints rather than silently
testing an older patch. Explain overrides in the PR.

Always target the latest released GHC in `stack-nightly.yaml`. When nightly
lags GHC, set `compiler: ghc-<version>` and `compiler-check: match-exact` and
resolve boot-package, flag, and dependency differences. Remove obsolete
overrides once nightly supplies the desired compiler. Use only one nightly
configuration: `stack-nightly.yaml`, targeting the latest released GHC. When
multiple GHC series have no LTS yet, do not add separate nightly configurations
for the intermediate series (for example, no `stack-9.12.yaml` when the latest
LTS uses GHC 9.10 and nightly is overridden to GHC 9.14). Add a series to LTS
coverage once an LTS for it becomes available.

Use historical commit `fc50142b1147ab7a26767ec58dc90a839d3fe53a` as an example
of the file naming and CI `resolver-yaml` matrix structure, not as the current
support policy. Verify compiler versions rather than trusting old comments.
Inspect later support changes before choosing the supported range. Commit
`614ae9ccef3adfc79b6046f45a11d58fc663b704` (PR #178) deliberately removed GHC
8.8 through 9.8 coverage during the crypton/ram dependency upgrade and raised
`tested-with` to GHC >=9.10.3 because older dependency sets were incompatible.
Respect that support decision: do not resurrect those configurations or lower
`tested-with` during routine maintenance, even if a new dependency combination
can be made to compile. Preserve coverage within the currently supported range
and add newer GHC series according to the LTS/nightly policy above;
restoring dropped support requires an explicit user request.

Put every supported configuration in `.github/workflows/test.yml`. When an
older build fails, investigate compatible dependency selection and minimal
source fixes before concluding support must be dropped. Do not raise minimum
support just to simplify the matrix. If a drop is unavoidable, document the
specific incompatibility and impact in the PR the user will review. Keep
`tested-with`, matrix comments, and any existing support documentation accurate.

## Compile failures and upstream fixes

Distinguish Stack solver errors, compiler/boot-package mismatches, library
source errors, and test failures before choosing a fix. For upstream compile
incompatibilities, inspect the dependency's GitHub issues, PRs, and commits
for an existing compatible fix. Verify the fix addresses the observed error.

If no compatible release exists, pin a verified unreleased version in
`extra-deps` using a full immutable commit SHA. A fork containing an upstream
PR is acceptable when needed. Include `subdirs` for the required package in a
multi-package repository, and link the relevant upstream issue or PR in the
YAML comment or PR description. Do not pin a moving branch. Check package
identity and dependencies at that commit. Prefer an appropriate released
version once available; revisit GitHub pins at every resolver upgrade.

Make minimal compatibility changes in this library when necessary, preserving
its API and older supported builds. Follow existing formatting and HLint
requirements. If no viable combination exists, report the concrete failure
and attempted fixes rather than hiding it by disabling tests or coverage.

## Validation and dependency PR

Run `stack test` for the default configuration and
`stack --stack-yaml stack-nightly.yaml test` for nightly. Run the equivalent
command for every retained or restored GHC configuration. Regenerate lock files
using Stack after configuration changes; commit each affected `.yaml.lock`.
Do not hand-edit lock hashes or copy a lock file from another configuration.
Follow the repository's CI checks, including Haddock and HLint; all matrix jobs
must pass. If a compiler cannot run locally, report that limitation and require
its CI job to pass before calling the PR ready.

Treat failures on supported platforms as repository compatibility issues to
investigate, not merely obstacles to local validation. Separate a broken local
toolchain from a dependency's platform-specific build failure; verify the
architecture and compiler/tool versions without assuming why they were
installed or why previous builds worked. Temporary diagnostic workarounds may
help isolate the failure, but Linux CI passing does not resolve a macOS failure.

Consider a checked-in fix: a compatible upstream release or immutable fix,
dependency flag settings, or narrowly scoped platform configuration. Before
choosing a workaround that changes dependency backends, performance, security
properties or platform behavior, explain the observed failure, proposed scope,
alternatives and tradeoffs and ask the user to choose. For example, disabling
crypton's `support_s2n_bignum` flag globally may fix macOS compilation but also
changes the backend on Linux; discuss that scope rather than silently applying
the flag locally or globally. Validate the agreed solution on affected
platforms, and consider CI coverage that would catch the failure again.

Keep machine-specific compiler/linker paths outside the committed
configuration. Report any temporary local flag changes explicitly; do not
describe those builds as validation of the default flags or call the platform
issue resolved until the agreed checked-in configuration has been validated.

Review the final diff for bounds, actual resolved versions, minimal overrides,
compiler coverage, and lock files. Open a dependency PR explaining the version
being admitted, resolver and compiler changes, added/removed exceptions,
upstream fix links, and verification. Keep the package version unchanged in
this PR. Watch checks and fix failures; never bypass failing checks. Leave
the PR for the user to review and merge.

## Release PR and publication

After verifying the dependency PR has merged and its checks passed, fetch the
remote, switch the user's main checkout to the default branch and fast-forward
it to the remote default branch, preserving unrelated work. Create the separate
release branch from that updated commit. For routine bounds
maintenance, increment the fourth version component in
`pusher-http-haskell.cabal` (historically `2.1.0.24` to `2.1.0.25`). Use
`v<version>` for the commit/PR convention and eventual tag. If the work changes
the public API or drops library compatibility, assess the appropriate package
version rather than automatically using the routine bump. Do not invent a
changelog when the repository has none.

Validate the release, run `stack sdist`, and inspect the generated source
archive for the expected version and required files. Open the release PR,
watch its full CI matrix, resolve failures, and leave it for the user to merge.
Do not tag or upload unmerged PR contents.

After verifying that the release PR has merged, fetch the remote, switch the
user's main checkout to the default branch and fast-forward it to the remote
default branch. Do this yourself before publishing or asking the user to
upload; a temporary release checkout does not replace updating their checkout.
Confirm the release commit, package version, passing checks and intended
release content. If unrelated work prevents updating the main checkout safely,
preserve it and explain the concrete obstruction rather than resetting it.
If the default branch has advanced beyond the release commit, use a clean
checkout of the release commit for publication and make that distinction clear.

Run `stack sdist` again from the merged release commit and inspect the archive.
Validate the files declared by the Cabal package, including the Cabal file,
license, Setup.hs where present, and library/test sources; do not assume every
repository file belongs in the archive. Create and push only
the matching `v<version>` tag, then publish with `stack upload .` from the
repository root. Verify the tag points to the released commit and Hackage
shows the correct package version. If a tag or Hackage version already exists,
inspect it first; never replace a published tag or blindly retry an upload.
After an ambiguous upload failure, check Hackage before retrying. Report
authentication failures without exposing credentials. If the user must finish
the upload, first prepare and verify their checkout, then provide the exact
working directory and remaining command. State separately whether the tag was
pushed and whether Hackage publication succeeded. After the user reports an
upload, verify Hackage rather than retrying it. Do not claim publication
without verification.
