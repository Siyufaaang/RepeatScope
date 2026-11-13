# Methodology and Repeat Report Implementation Plan

> **For agentic workers:** Use superpowers:executing-plans inline. A fresh final reviewer checks the whole change.

**Goal:** Explain the analysis and add traceable allele repeat estimates without changing EI.

**Architecture:** Add a report-only derivation in report.R and wrap existing threshold_results. Expose the same values in dashboard, tables and ZIP; add static methodology and an input guide.

**Tech Stack:** R/Shiny, native HTML/CSS/JavaScript; existing Posit hosting.

**Spec:** docs/superpowers/specs/2026-11-13-methodology-repeat-report.md

## Global constraints
- No changes to engine, method modules, localization decisions or synthetic inputs.
- Separate repeat estimates, entered repeat evidence and EI; preserve acquisition/allele identity.
- No automatic disease classification or confidence claims.
- Retain deployment URL and monochrome/teal/purple design.

## Review focus
- Missing, invalid or stale mappings must not yield plausible-looking numbers.
- Multiple acquisitions and targets must remain separate.
- Normal targets can have repeat estimates while EI is unavailable.
- Unknown CE margin must not turn into zero uncertainty.
- Method narrative must describe actual seeded, frozen Bayesian selection.

## Task 1 — report derivation and integration
Files: report.R, import_export.R, server.R, dashboard HTML/JS/CSS; tests/test_repeat_report.R.
Interfaces: allele_repeat_report(project, variant) returns one row per record; report_results(project, variant) appends matching fields to unchanged threshold_results.
- [x] Write and run failing tests for theoretical/linear conversion, uncertainty, stale/invalid targets and unchanged EI.
- [x] Implement derivation and display/export integration; make comparison a non-collapsible section.
- [x] Run focused and existing synthetic tests.

## Task 2 — researcher explanation and release
Files: visuals.R, ui.R, workflow navigation, www/input-guide-zh.html, deployment manifests.
- [x] Add homepage formulas, method procedures, limitations and guide link.
- [x] Verify browser workflows and old EI values, run a fresh final code review, address material findings.
- [x] Update allowlists, integrate, package and deploy; verify public page and record outcomes.

## Verification record
- 25 synthetic R suites pass, including a new per-allele repeat-report suite; 5 JavaScript suites pass.
- Report wrapper preserves every original EI/status column exactly and does not mutate project/peak decisions. Multiple alleles and acquisitions tested independently.
- Fresh reviewer identified one Important freshness gap in pre-existing draft-structure fingerprint coverage. Regression reproduced RED; conditional draft fingerprint added; full suite GREEN. Existing confirmed/no-draft fingerprints preserved. No EI/selection algorithm changed.
- Local browser: homepage methodology and guide link present; comparison section visible without expansion; normal repeat count retained while EI unavailable; A04 Extended Adaptive 2.874266 / Bayesian 2.522163 unchanged, repeat estimate 64.000.
- No deferred review findings.
- Released in PR #6, merge 74bacf6; public deployment succeeded at the existing Posit content ID on 2026-11-13. Public methodology and standalone Chinese guide render; A04 repeat estimate 64.000 and EI difference 0.352104 verified; comparison section visible. Public report ZIP downloaded successfully with the new per-allele table. Source-generated ZIP contains 40 artifacts and workbook sheet. Confirmed saved cloud project retains every old result column exactly.
