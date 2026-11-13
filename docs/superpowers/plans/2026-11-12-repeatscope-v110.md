# RepeatScope v1.10 Implementation Plan
> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Intermediate EI, gene profiles and a compact plate report.
**Architecture:** Revise allocation/eligibility, add versioned JSON adapter, simplify Shiny/dashboard views; scientific fitting unchanged.
**Tech Stack:** R/Shiny, JSON, JavaScript/CSS; no new dependencies.
**Spec:** docs/superpowers/specs/2026-11-12-repeatscope-v110-design.md

## Global Constraints
Original inputs and scientific modules unchanged. No real data in Git. Preserve v1.9 services. User opted out of questions; inline execution then independent review.

## Review Focus
Old fits/overrides; stale profile after gene change; multiple acquisitions/alleles; unknown uncertainty; regional tail fragments in normal wells.

### Task 1: Intermediate calculation
R/localization.R, engine.R, report.R, state.R; tests/test_intermediate_ei.R.
Interfaces: prepare_localized_project(p), threshold_results(p), analysis_fingerprint(p), eligibility(record,p).
- [x] RED: unique intermediate/boundary, mixed normal/intermediate, true ambiguity, shared tails, normal regional fragments.
- [x] Implement category/allocation policy and policy fingerprint.
- [x] GREEN targeted/synthetic regression, H06 unchanged; commit.

### Task 2: Profiles and setup
profiles/diseases.json, R/disease_profiles.R, bootstrap.R, ui.R, server_localization.R, server_reference.R, server.R, workflow.R, field_help.R; tests/test_disease_profiles.R.
Interfaces: disease_profiles(), find_disease_profile(gene), apply_disease_profile(p,gene), profile_setup(p).
- [x] RED: boundaries, aliases, conditional notes, unknown/changed gene, restored snapshot, channel and unknown uncertainty.
- [x] Implement catalog and adapter; integrate without replacing restored customization.
- [x] Simplify setup into defaults plus advanced settings; test server flow; commit.

### Task 3: Report and delivery
www/dashboard.js/html/css, R/ui.R/report.R, tests/test_dashboard_compact.cjs, run_shareable.R, repository-files.txt, .gitignore, README.md.
- [x] RED: compact tiles/method switching/multiple targets/detail retention.
- [x] Implement report, version v1.10, tests; commit.
- [x] All applicable tests, real-data rerun, protected file hashes.
- [x] Independent review; significant findings RED/GREEN; integrate/push authorized private repo.
- [x] Separate v1.10 services; original-file visible browser import, save/restore and trial links.

## Ledger
Baseline d783753 v1.9. Managed worktree created through Codex. Native execution.
Ruling: managed worktree is outside sandbox writable roots; use scoped escalated file writes there, preserving original checkout and running v1.9.

Task 1: complete. Synthetic intermediate suite RED then 11 PASS; existing shareable suite PASS; real Group 1 has 34 calculated method rows (17 wells), H06 Extended remains 3.52898696088 / 3.16422831238. Ruling: child sandbox could not restore cwd during ZIP regression; scoped elevated tests use the existing original app library, no packages installed.

Task 2: complete. Profile data tests RED then 35 PASS; server gene/change/unknown/restore/reset tests RED then 5 PASS. Ruling: initial catalog covers nine CAG/polyglutamine loci with explicit conditional ranges, not an unsupported claim of universal repeat-disease coverage. Unknown CE precision allows provisional calculations and never formal clearance.

Task 3: compact tile tests RED then PASS; existing JavaScript identity/paste/draft suites PASS. Main plate uses a selectable method; normal and numeric zero remain different, separate targets never average into one number. Final regressions and browser verification in progress.

Independent review: 3 Important findings reproduced RED and fixed GREEN: full CE uncertainty interval overlaps conditional disease bands; aliases compare by canonical gene identity without replacing saved custom settings/reference; unknown precision gates formal positive-control results even without localization status. Minor normal-conflict explanation corrected. Quiet input hints remain accessible after full-suite regression exposed a missing help contract.
Verification: full shareable synthetic suite and all four JavaScript suites PASS after fixes. Real-data rerun PASS: 17 Case 1 wells calculate both methods, H06 values unchanged. Seven protected input/method SHA256 hashes unchanged.
Declined to judge in review: final visual redesign (deferred by user), browser acceptance (parent verifying), clinical validity of all nine profiles, universal repeat-disease coverage, original scientific formulas.
Integration ruling: user authorized autonomous completion and explicitly requested no questions. Use the existing private repository, preserve v1.9 running checkout, publish the reviewed v1.10 branch with a reviewable PR; preserve managed worktree for the live trial.
Browser acceptance complete on the final logic: original Excel plate and TXT peaks imported through the visible Chrome UI; 96-row Excel paste applied (41 samples / 55 CE Fill / 41 complete genotypes); Group 3 and text Individual ID 003 each select 12 wells. ATXN2 entry loads the built-in snapshot, real reference/primers derive fixed length 103 bp and channel B without inventing CE precision.
Calculated both methods in browser: 29 finite results each, all 17 Case 1 wells included. A01 intermediate is 0.914086 in both methods; H06 Extended 3% is 3.528987 / 3.164228. Detailed peaks remain on click and comparisons start closed. Saved RDS contains identical original Excel/TXT bytes and all 58 EI rows match the independent rerun. Browser reload/upload/restore retains metadata, profile, matched structure and current fits without recalculation.
A small visual follow-up sets primary-button text white; verified after browser reload. The broader visual redesign remains deliberately deferred.
Local delivery: 3865 starts a blank project; 3866 restores the browser-verified real project for immediate trial. Both serve only 127.0.0.1 and use the managed v1.10 checkout. v1.9 original checkout/services preserved.
Repository review: private PR #1 contains code, profiles, synthetic fixtures and tests only. Local screenshots, logs, RDS and real input copies are excluded. No deferred Important/Critical review findings.