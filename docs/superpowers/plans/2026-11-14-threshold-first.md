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

## Verification record — 2026-11-14
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
