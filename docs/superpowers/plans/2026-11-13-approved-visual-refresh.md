# RepeatScope approved visual refresh Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Apply the approved quiet monochrome/teal/purple visual system and publish it at the existing Beta URL without changing scientific results.

**Architecture:** Existing R Shiny UI and JavaScript remain authoritative. Add a presentation module, native landing artwork and shared theme; move the existing selected-record detail into a responsive side panel. The existing allowlisted bundle and rsconnect publishing path remain in use.

**Tech Stack:** R/Shiny/bslib, plain JavaScript/CSS/SVG, rsconnect, PowerShell.

**Spec:** docs/superpowers/specs/2026-11-13-approved-visual-design.md

## Global Constraints
- Preserve all scientific computation and saved inputs/results. No private input uploads or credentials in source/bundle.
- Keep the same Posit public content and existing account. The user authorized implementation and deployment without questions.
- Reuse exact approved logo source; reserve the user's forthcoming walkthrough video without fabricating a recording.
- Keep numerical labels dark; pair color with labels/markers; preserve responsive navigation and keyboard access.

## Review Focus
- Switching well, acquisition or target must not show another record's result, including after refresh.
- Missing/normal/stale results must never be labelled as finite numerical disagreement.
- All sizing peaks and their heights/positions must survive the new coloring and layout.
- Home/demo/navigation must not silently reset or corrupt a user's working project.
- New runtime assets must be allowlisted and work on deployed Linux as well as Windows.

### Task 1: Landing and shared presentation
**Files:** R/visuals.R, R/bootstrap.R, R/ui.R, R/workflow.R, www/visual.css, www/visual.js, www/brand-logo.png under development/shiny_app; deployment/runtime-files.txt; repository-files.txt.
**Interfaces:** `landing_ui()` returns native Shiny markup; existing `try_workflow` and `load_demo` actions remain the routes; `home_section` is presentation-only navigation.
- [x] Run existing shareable baseline suites and preserve scientific-file hashes.
- [x] Add approved assets, landing page, video reservation and monochrome global theme. Preserve all existing input IDs.
- [x] Test home navigation against a frozen project; existing workflow tests remain green.

### Task 2: Result and plate color semantics
**Files:** www/dashboard.js, www/dashboard.html, www/dashboard.css, R/server.R presentation section; tests/test_visual_semantics.cjs.
**Interfaces:** `dashboardComparison(record, stale)` returns equal/different/unavailable status using existing three-decimal display comparison; `dashboardPeakInk(peak, modal, method)` maps existing flags only. Selected-record DOM remains bound to the current payload.
- [x] Write and observe failing tests for finite/zero/missing/stale/multiple-record comparisons and every peak inclusion combination.
- [x] Implement tested presentation helpers and aligned responsive side panel; keep plot geometry and numerical formatting unchanged.
- [x] Apply stable group strips and neutral selection. Inspect Arrange plate, Import peaks and Reference & analysis in a browser.
- [x] Run relevant R and JS tests, including saved-record identity and layout draft behavior.

### Task 3: Verify, review, integrate and deploy
**Files:** runtime/repository allowlists, QA evidence (ignored), this plan ledger.
**Interfaces:** the reviewed source is the sole input of the frozen-manifest deployment bundle; the public app is checked with synthetic data only.
- [x] Run full shareable R/JS suites and exact numerical comparison against saved synthetic results; verify immutable scientific-file hashes.
- [x] Review responsive screens and critical navigation/input flows, including refresh and all-peaks view.
- [x] Obtain one independent whole-change code review; resolve material findings and rerun affected checks.
- [x] Commit/integrate the reviewed source, build the allowlisted bundle, publish to the existing content ID and confirm public startup and numerical equivalence.

## Ledger
Ruling: the explicit user approval covers execution, integration and existing-site deployment; no additional plan confirmation is needed.
Ruling: reuse the existing clean linked worktree and existing Posit hosting. Sites hosting does not run this R/Shiny application.
Ruling: CSS/layout changes use visual inspection rather than implementation-mirroring tests; new comparison/peak presentation semantics receive behavioral tests.
Pre-flight: Task 2 consumes shared tokens from Task 1; Task 3 must include every added runtime asset from both tasks and preserve the unchanged analysis modules.

Task 1: complete — native landing and theme rendered in Chrome; guided server test confirms home and return navigation preserve the complete project hash.
Task 2: complete — visual semantic tests observed RED then GREEN; record identity, compact dashboard, layout draft and paste suites pass. Browser verified A04 2.874266 / 2.522163, B03 unavailable Bayesian, both plot views, and all workflow screens.
Task 3 verification: 24 shareable R suites and 5 JS suites pass. All 12 recorded scientific files unchanged. Saved cloud fixture versus fresh local computation has exactly zero error across both windows and all 11 finite values per window; missing values match. Independent A04 arithmetic and saved ZIP audit pass.
Final review: one fresh read-only reviewer found one Important issue (global Demo could replace current project); no other issues or declined judgments.
Final: fixed Demo replacement guard — test_visual_navigation.R observed RED→GREEN; 24/24 R suites pass. Every visible demo entry now requests a transition; occupied sessions receive Save / Cancel / Replace choices before the existing action is invoked.
Ruling: retain the task worktree and QA evidence after integration because this is the ongoing user workspace; removing it would discard useful local release evidence. Windows-native commands and this ledger replace the skill's Bash workspace scripts.
Bundle: 48 allowlisted runtime files; frozen manifest verifies exact checksums; existing Posit deployment record copied into a new isolated release directory. No user data or credentials are in the upload list.
Browser integration: Shiny requires a one-argument custom-message callback. Added a regression reproducing the arity failure, corrected the callback, and reran all five JavaScript suites successfully.

Task 3: complete — PR #5 merged into main (7c3126997988baebb3891710d0428c06e4b9a0f8); 48-file final bundle published to existing Posit content 01a1d5a3-31ce-5e40-498d-e4d4dc298453. Public URL startup, all five workflow screens, fresh synthetic dual-method computation, A04 2.874266 / 2.522163 and delta 0.352104, 21 retained peaks per plot, two Adaptive-only peaks, B03 unavailable Bayesian, and empty browser error log verified after deployment.
