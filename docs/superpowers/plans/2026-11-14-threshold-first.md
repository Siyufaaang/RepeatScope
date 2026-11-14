# Threshold-first local implementation plan

> **For agentic workers:** Use superpowers:executing-plans. Track each task below.

**Goal:** Apply the approved 3% assignment floor to both methods and their local animation.
**Architecture:** Filter model inputs in the application adapter after modal
localization; retain physical inventory/support. Generate the teaching trace from
that adapter and animate the resulting decisions without browser-side fitting.
**Tech Stack:** R/Shiny, vanilla JavaScript/SVG, existing synthetic test suites.
**Spec:** docs/superpowers/specs/2026-11-14-threshold-first.md

## Global constraints
- Local branch from v1.11.1; no deployment or remote push.
- Height >= 0.03 * own modal height; preserve raw peaks, modal and repeat indices.
- Keep scientific model source and original batch defaults unchanged.
- Real snapshot regression stays ignored/local; committed fixtures are synthetic.

## Review focus
- Threshold equality and final weak tail cannot shift later strong assignments.
- Low Bayesian competitor cannot create tail ambiguity or prevent valid seeds.
- Old cached fits must be stale; reviewed fits and exports use the new policy.
- Additional EI filtering below 3% must never reintroduce removed peaks.
- Animation consumes real trace, preserves physical positions and reduced-motion controls.

### Task 1: Scientific adapter and provenance
Files: R/engine.R, R/state.R, R/import_export.R, R/server.R,
R/field_help.R, tests/test_threshold_first.R, tests/run_shareable.R, README.md.
Interface: fit_record returns raw peaks plus a named assignment policy; both
methods receive the same eligible inventory. Existing method signatures stay fixed.
- [x] Run the baseline R/JS suites.
- [x] Add synthetic behavior tests: 299/300 RFU boundary at modal 10000;
  218.5 bp weak peak must not move a later eligible peak outside Adaptive tolerance;
  a weak peak next to a strong Bayesian candidate must not cause ambiguity;
  weak seed competitors must not stop otherwise valid seed calibration;
  old-policy fingerprints must invalidate results.
- [x] Watch the tests fail, add the shared adapter filter and provenance, pass tests.
- [x] Refit the 29 local SCA2 targets and match the previously calculated prefilter
  assignments and 116 EI values. Preserve snapshot hashes.

### Task 2: Local teaching animation
Files: tools/generate_method_trace.R, www/method-trace.json,
www/method-learning.js, www/methodology.html, www/learning.css,
tests/test_learning_player.cjs, tests/test_learning_ui.R.
Interface: trace includes raw peaks, eligible IDs and the assignment policy;
frames show inventory, zoom, threshold, eligible candidates, model assignment, EI.
- [x] Write failing behavior tests for filter-before-assignment, simultaneous
  Bayesian tail intervals, no accepted low peaks, and trace immutability.
- [x] Regenerate only the synthetic trace; implement automatic pause/replay flow,
  stable peak bars and properly sized separated chart containers.
- [x] Run full R and JS suites, repository allowlist check and local browser QA.
- [x] Fresh independent review, address substantive findings, retain local branch.

## Execution notes
The user explicitly authorized local implementation after comparing both orders.
This plan records that decision; it does not introduce another approval gate.
Implementation stays in this chat. Repository-required commits remain local.

## Approved animation follow-up — 2026-11-14
The user selected display mockup A: one square-root height chart, with original
RFU ticks labelled in thousands and no clipping of the modal. Keep the physical
inventory first, slow playback to 3–4 seconds, and restore previous/next controls
that pause playback. Preserve individual peak filtering without tail truncation;
no scientific model changes or public deployment are part of this follow-up.
- [x] Verify the new projection test fails, then implement the display transform.
- [x] Replace the two plots and add slower playback with manual navigation.
- [x] Verify browser behavior and full R/JS suites.
- [x] Verify the staged repository manifest; prepare the verified local commit.

Follow-up validation: all 27 shareable R suites and six JavaScript test files
pass. Existing R startup locale/package-version warnings remain. Browser QA
confirmed one plot, original RFU scale labels, threshold-before-exclusion,
unchanged physical coordinates, simultaneous Bayesian intervals (15 visible
windows), and retained eligible Adaptive peaks after the low-height gap.
Previous/Next, endpoint disabling, Play resume, and pause persistence across
method changes and a 760 px responsive layout were exercised. The viewport was
reset. Reduced-motion handling was reviewed in code, not OS-emulated.
A read-only reviewer found a pause reset on method-tab selection; the behavior
was reproduced in the browser, fixed, and checked again. Scientific code and the
frozen trace were unchanged. Screenshots and logs stay in ignored local qa/.

## Initial threshold-first verification record — 2026-11-14
- Baseline and final full R runs pass: 27 synthetic suites in the final run.
  Six JavaScript test files pass. Existing R startup locale/package-version
  warnings were present before the change; no test failures remain.
- New scientific and animation tests were observed failing before implementation.
- The local real-data regression matches 29 modal/support/index anchors, 58
  complete method results and 116 EI values against the prior prefilter experiment.
  The input snapshot is unchanged. Private results/logs stay outside version control.
- Browser checks at the default narrow view and 1280 px desktop width confirmed
  automatic playback, all physical bars fixed before/after selection, simultaneous
  tail intervals, separated charts and absence of next/back controls. The viewport
  override was reset. Reduced-motion behavior was reviewed in code, not OS-emulated.
- A fresh read-only review found one Important issue: manual high-peak exclusions
  in reviewed results could be mislabeled as below the assignment floor. Added a
  failing provenance regression, fixed the decision label, and reran the full suite.
  No Critical or deferred Minor findings remain. Reviewer did not repeat private
  data or browser checks; the implementer performed the checks listed above.
- Implementation choice: hierarchical experimental assignment uses the same
  eligible input inventory, preserving the application's shared assignment floor.
- Keep the local branch/worktree and localhost preview. No push or deployment.

## Authorized public release — 2026-11-14
The user approved publishing the verified threshold-first logic, single-chart
animation and updated methodology to the existing public website. This supersedes
the earlier local-only restriction. Runtime source remains the reviewed commits
15bcbfc and ba6e405; no additional scientific changes are part of publication.
- [x] Recheck complete R/JS suites and the exact 55-file release bundle.
- [ ] Integrate the reviewed source with expected-head protection.
- [ ] Deploy to existing Connect Cloud content and verify public behavior.
Use the tested application dependency manifest and the existing deployment record;
retain the previous release bundle for rollback. Publish synthetic assets only.
