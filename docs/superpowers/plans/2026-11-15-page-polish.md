# RepeatScope local page polish implementation plan

> **For agentic workers:** Use superpowers:executing-plans to implement task-by-task. User has approved implementation; no further design gate is needed.

**Goal:** Polish four workflow pages locally using the approved monochrome design.
**Architecture:** Preserve Shiny IDs and scientific state. Reorganize UI containers, add scoped CSS, and compute exact nonzero well-role counts in the dashboard.
**Tech Stack:** R/Shiny, HTML/CSS, plain JavaScript, existing R and Node test runners.
**Spec:** `docs/superpowers/specs/2026-11-15-page-polish-design.md`

## Global constraints
- Local only, based on `65afd31`; no uploads or public changes.
- Homepage and scientific calculation files unchanged.
- Numeric threshold only, percent outside the input.
- Exact assigned categories, nonzero counts, one count per physical well.
- Keep imports, drafts/Undo, help, profile defaults, multiple-target access and stale-result protection.

## Review focus
- Mixed well roles and multiple records: unique well counts and all nonzero exact roles.
- Numeric input blank/invalid/out-of-range and late server echoes: preserve last valid threshold.
- Stale or failed imports: preserve existing validation and visible error messages.
- Long labels and narrow widths: local grid scrolling, no clipped form labels or page overflow.
- Reference profile changes and multiple targets: preserve existing input and selection behavior.

## Task 1: Report controls and category counts
Files: `www/dashboard.js`, `www/dashboard.html`, `www/threshold.js`, `R/ui.R`, tests `test_page_polish.cjs`.
Interface: `dashboardRoleSummary(wells)` returns ordered nonzero `{role,label,count}` rows; existing dashboard payload and `threshold_change` remain unchanged.
- [x] Add failing unique-well category and numeric-event tests; observe failure.
- [x] Implement role summary and numeric-only threshold UI while retaining canonical server state.
- [x] Run targeted JS/R checks.

## Task 2: Four page layouts
Files: `R/ui.R`, `R/server_import.R`, `www/workflow-polish.css`, deployment/repository manifests.
Interface: existing Shiny IDs remain; import preview status outputs are read-only UI additions.
- [x] Reorganize plate/import, sizing, reference and report containers.
- [x] Add scoped responsive CSS; keep landing-specific markup/assets unchanged.
- [x] Verify parsing and synthetic suite, including import errors and stale source rejection.

## Task 3: Browser verification and local delivery
- [x] Start the isolated local server; inspect all four pages with synthetic inputs.
- [x] Exercise role counts, numeric threshold, plate selection and narrow layout; check imports through synthetic suites (native file picker limitation below).
- [x] Review diff and scientific/homepage invariants; run full R/JS suites and manifest check (122 reviewed files).
- [x] Obtain fresh whole-change code review and address material findings.
- [x] Prepare the verified local checkpoint for delivery at `http://127.0.0.1:3881/`; retain branch `codex/page-polish-local`. Do not push or deploy.

## Verification evidence
- All 27 shareable R suites passed after the implementation; all seven JavaScript test files passed. New role-summary tests were first observed failing, then passed.
- Independent read-only review found no Critical, Important or Minor issues; it independently reran all seven JavaScript files and inspected the R-suite log.
- Browser: source/preview layouts, Arrange selection and bulk apply/Undo, primer/reference structure, dual-method calculation and report were inspected. The report showed `96 wells · 7 Sample · 1 PCR NTC · 88 Pending` without zero categories.
- Changing the numeric threshold from 3 to 5 changed the selected-well EI from 2.015723 to 1.875811 and its peak-height cutoff from 300 to 500 RFU.
- Layouts were checked at 1440, 760 and 390 pixel widths. No document horizontal overflow at narrow widths; wide plate grids retain local scrolling. Temporary viewport override was reset.
- Manual native file selection could not be completed: the browser tool did not open or expose a file chooser in either IAB or native Chrome. Import parsing, validation, stale-source rejection and data retention passed the synthetic suite and code review; this does not claim a completed manual file-picker test.
- Homepage assets and scientific calculation files remain unchanged. No public service was modified and no push/deployment was run.

## Authorized alignment fix and public release — 2026-11-15

The user subsequently requested a full-width header and an EI filter bug fix
(local checkpoint `050d0a0`), then identified the resulting homepage mismatch.
They explicitly authorized aligning the homepage, moving upload notifications
away from Next, and publishing the completed version to the existing public app.
These requests supersede the initial local-only and homepage constraints for
these specific changes. Private research inputs remain excluded.

- Shared responsive gutters align the logo, hero heading, section headings and
  workflow content. Dark landing sections paint to the viewport edges without
  introducing document overflow.
- Notifications sit below the measured responsive header, at the upper right.
  The bounded panel overrides bslib's doubled-ID positioning rules so mobile
  messages remain inside the viewport. Next and Save project stay clear.
- The notification/header resize regression was observed failing before the
  implementation, then passing. All 27 R suites and seven JavaScript files pass.
- Browser checks cover 1920, 1690 and 390 px widths. A synthetic sizing CSV was
  selected through the file chooser, imported, and advanced with Next while its
  success notification was visible. This resolves the earlier file-picker
  verification limitation. No private research data is part of cloud testing.
- Fresh read-only review identified header overlap in the initial toast position;
  the responsive header offset resolved it. Browser QA identified and corrected
  the bslib specificity conflict before publication.
- Publish the exact accepted 56-file bundle, with 61 frozen package dependencies,
  to existing Connect Cloud content `01a1d5a3-31ce-5e40-498d-e4d4dc298453` under
  account `siyufang`. Retain the previous verified bundle for rollback.
- [ ] Verify the final selected bundle and repository manifest; commit locally.
- [ ] Deploy to the existing public content and verify hosted behavior.
