# RepeatScope public Beta Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Integrate the verified v1.11.1 release and deploy a numerically checked public research Beta.

**Architecture:** A clean allowlisted bundle wraps the existing nested Shiny app. Direct rsconnect upload to Posit Connect Cloud preserves the private source repository and uses the existing R algorithms unchanged.

**Tech Stack:** R 4.5.2, Shiny, rsconnect, PowerShell, existing JavaScript.

**Spec:** docs/superpowers/specs/2026-11-13-public-beta-design.md

## Global Constraints
- Preserve original input files, scientific algorithms, numerical precision, v1.9 and v1.11.1 checkouts.
- No real research files, saved projects, QA artifacts or credentials in Git or deployment.
- User authorized integration and public deployment; account access and financial decisions remain user actions.
- No numerical mismatch may be waived to meet today's deadline.

## Review Focus
- Nested app startup must resolve all methods/assets on Linux without a workstation path.
- Explicit deployment file list must exclude any future private/untracked file.
- Concurrent sessions must not share projects, downloads or mutable analysis state.
- Restored or changed projects must not show stale EI; method/window values remain separate.
- Hosted visitors must understand processing location and temporary session persistence.

### Task 1: Portable package and Beta behavior
**Files:** tools/prepare_beta.R, tools/deploy_beta.R, deployment/runtime-files.txt, deployment/app.R, development/shiny_app/R/ui.R, development/shiny_app/R/server.R, README.md, .gitignore, repository-files.txt; tests/test_deployment.R.
**Interfaces:** prepare_beta(root, destination) consumes runtime-files.txt and emits a clean app directory plus manifest; deploy script consumes that directory and an rsconnect account. Existing rs_server and scientific modules remain authoritative.
- [x] Verify clean v1.11.1 baseline.
- [x] Test that packaged app boots with relative roots, excludes unlisted files and preserves both EI windows; observe missing packaging failure.
- [x] Implement wrapper, allowlisted packager, explicit dependency metadata and deployment script.
- [x] Correct hosted-mode copy; add meaningful session/isolation coverage where needed.
- [x] Run synthetic R/JS suites and package acceptance; record runtime/dependency manifest.

### Task 2: Numerical verification, review and integration
**Interfaces:** packaged app from Task 1, local private fixtures from the existing v1.11.1 QA directory; GitHub PRs 1/2 and deployment PR.
- [x] Recalculate both methods/windows locally and compare with independently verified values; fingerprint scientific files.
- [x] Independent whole-branch deployment review; fix Important/Critical findings with regression tests.
- [x] Verify repository manifest; commit/push deployment branch.
- [x] Merge PRs 1/2 and deployment PR with expected-head protection; verify integrated source equals tested tree.

### Task 3: Cloud deployment and acceptance
**Interfaces:** verified clean package, user-authorized Connect Cloud login, rsconnect deployment record.
- [x] Complete account authorization, deploy reviewed package and monitor build logs.
- [x] Verify public HTTPS, synthetic import/calculation, both windows/methods, saved project/download and two independent sessions.
- [x] Record deployment ID/version, URL, limitations and rollback procedure; deliver verified Beta URL.

## Ledger
Ruling: user requested autonomous continuation; execute inline without another design approval. A scoped deployment review is delegated at the end as required by executing-plans.
Ruling: use a new managed worktree because the prior one belongs to the existing v1.11.1 chat and trial services; keep both originals available.
Pre-flight: Task 1 package is Task 2 verification target and Task 3 upload input. Cloud verification uses synthetic data only; private data never crosses that interface.

2026-11-13 pre-deployment evidence: all 23 synthetic R suites and four JavaScript suites pass. The selected 44-file release bundle passes startup, both EI windows, synthetic project round-trip and session-isolation acceptance; its dependency manifest records 61 packages. Independent raw-input refits and arithmetic reproduce 116 values over 29 private local records with zero result-table and dashboard error. This verifies configured computations, not biological validity.

Review ruling: fixed the Important finding that project structure was checked after field access. Inert malformed-object regressions fail before the fix and pass afterward; normal fitted projects still round-trip. Regraded the review's Minor selected-bundle gap to Important: deployment now compares every upload file with both its checksum and the reviewed source, and acceptance can exercise that exact bundle. A tampered bundle with updated internal checksums is rejected. No scientific algorithm files changed.

Integration: PRs #1 and #2 were merged with expected-head checks. The resulting main source matches the verified v1.11.1 baseline. User completed Connect Cloud authorization for siyufang; credentials remain in the local rsconnect store. Public visibility is explicit in the deployment call. Cloud Linux startup, numerical UI behavior and public access remain release gates.

## Verified public release — 2026-11-13

- Public application: https://01a1d5a3-31ce-5e40-498d-e4d4dc298453.share.connect.posit.cloud/
- Connect Cloud content ID: `01a1d5a3-31ce-5e40-498d-e4d4dc298453`; account `siyufang`; application version `1.11.1`; deployment record version `1` (the host does not return a bundle ID here).
- Application source: `009b3415b2d2a40292f9bc20325fdbcba82352bc`, merged by PR #3 into `8fcc3af2352983b1b67652de86486aa60f816597`; complete trees compared equal after merge. Subsequent fixes affect publishing tools, acceptance and documentation only; the 44 deployed application files are unchanged.
- Initial publish stopped before upload because publishing-tool httr2 required rlang >= 1.3.0 while the application library provided 1.1.6. Resolved by using the isolated publishing-tool library and the pre-generated application dependency manifest. A regression rejects additional files or stale checksums in that manifest. The exact selected bundle passes acceptance. No application dependency was silently upgraded to solve the client problem.
- Direct rsconnect upload completed successfully. Host UI reports Public; an independent HTTPS request without account cookies returned 200 with the application. Linux startup, assets and both analysis methods work.
- Browser import: synthetic Excel plate preserved codes 001–007; synthetic CSV produced 168 physical peaks, eight acquisitions and zero flagged source rows. No real research inputs were uploaded.
- Cloud synthetic download: 11 finite EI values per window (22 total) exactly match local refits, with maximum absolute error 0. Independent A04 sums match Tail10 `57821/28685` for both methods; Extended `88651/30843` Adaptive and `76076/30163` Bayesian.
- Independent simultaneous browser session began empty. Saved project uploaded/restored successfully; downloaded restored fits and every result row are identical. Browser warning/error logs were empty.
- Complete report ZIP downloaded with 39 nonempty artifacts; result CSV matches the project, and Cairo PNG/PDF outputs have valid signatures. Local evidence is retained only in ignored QA files and synthetic downloads.
- Scope: computational research Beta, not biological/clinical validation. Sessions can expire; visitors must download Save project and retain original inputs. Use only synthetic or authorized de-identified data.
- Rollback: use the matching reviewed source checkout and retained bundle/rsconnect record, verify hashes and frozen manifest, redeploy to this same content ID, then repeat synthetic public acceptance. Preserve prior source checkouts; never deploy the full research workspace.
