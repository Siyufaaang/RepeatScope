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
- [ ] RED: unique intermediate/boundary, mixed normal/intermediate, true ambiguity, shared tails, normal regional fragments.
- [ ] Implement category/allocation policy and policy fingerprint.
- [ ] GREEN targeted/synthetic regression, H06 unchanged; commit.

### Task 2: Profiles and setup
profiles/diseases.json, R/disease_profiles.R, bootstrap.R, ui.R, server_localization.R, server_reference.R, server.R, workflow.R, field_help.R; tests/test_disease_profiles.R.
Interfaces: disease_profiles(), find_disease_profile(gene), apply_disease_profile(p,gene), profile_setup(p).
- [ ] RED: boundaries, aliases, conditional notes, unknown/changed gene, restored snapshot, channel and unknown uncertainty.
- [ ] Implement catalog and adapter; integrate without replacing restored customization.
- [ ] Simplify setup into defaults plus advanced settings; test server flow; commit.

### Task 3: Report and delivery
www/dashboard.js/html/css, R/ui.R/report.R, tests/test_dashboard_compact.cjs, run_shareable.R, repository-files.txt, .gitignore, README.md.
- [ ] RED: compact tiles/method switching/multiple targets/detail retention.
- [ ] Implement report, version v1.10, tests; commit.
- [ ] All applicable tests, real-data rerun, protected file hashes.
- [ ] Independent review; significant findings RED/GREEN; integrate/push authorized private repo.
- [ ] Separate v1.10 services; original-file visible browser import, save/restore and trial links.

## Ledger
Baseline d783753 v1.9. Managed worktree created through Codex. Native execution.
Ruling: managed worktree is outside sandbox writable roots; use scoped escalated file writes there, preserving original checkout and running v1.9.

Task 1: complete. Synthetic intermediate suite RED then 11 PASS; existing shareable suite PASS; real Group 1 has 34 calculated method rows (17 wells), H06 Extended remains 3.52898696088 / 3.16422831238. Ruling: child sandbox could not restore cwd during ZIP regression; scoped elevated tests use the existing original app library, no packages installed.
