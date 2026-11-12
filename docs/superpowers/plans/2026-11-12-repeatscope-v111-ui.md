# RepeatScope v1.11 UI implementation

Spec: `docs/superpowers/specs/2026-11-12-repeatscope-v111-ui-design.md`

Global constraints: presentation only; no new dependencies; preserve all bindings and analysis behavior; no private input or QA artifacts in Git. Baseline cded6a8. Use the existing isolated worktree on codex/repeatscope-v111-ui.

## Task 1: Unified design system and plate report
Interfaces: app.css owns global tokens and Shiny controls; dashboard.css consumes tokens and styles existing DOM; ui.R and dashboard.html own presentation markup; dashboard.js may change only presentation attributes/plot margins.

- Consolidate both stylesheets. Update version/cache labels and simplify visible report controls. Use existing disclosure elements for secondary explanations. Preserve all IDs and hooks.
- Style wells, legends, controls, plots, selection/focus/empty/difference states and narrow screens.
- Run existing JS tests and parse R UI. Expected: unchanged behavior, valid UI.

## Task 2: Real-data and visible-browser verification
Interfaces: new launchers on 3867/3868 consume the same verified v1.10 project; scientific source and original data are unchanged.

- Start separate blank and real-data trial services with hidden windows.
- Verify all 58 finite method results match the v1.10 fixture and all 96 positions remain present.
- Inspect report, both EI methods, modal details, folding/comparisons, arrangement, reference setup, welcome/import and narrow viewport in a visible browser; correct presentation defects.
- Run existing R regression suite. Expected: all pass and no scientific source diff.

## Task 3: Review and delivery
Interfaces: code review consumes baseline diff, verification logs and screenshots.

- Request one fresh-context whole-branch review; correct significant findings and verify.
- Update repository allowlist/manifest, verify packaging, commit and push the authorized feature branch; create a PR based on v1.10, preserving its separate review.
- Deliver new local trial links and screenshot. Expected: new UI is usable, no analysis changes or private data committed.

## Review Focus
Inspect preservation of input bindings and status visibility, long/multiple result labels, group and method distinction, control/empty wells, keyboard focus, overflow, chart axes, mobile layouts, and accidental analysis/data changes. CSS changes are verified visually, not by tests that merely mirror styles.

## Verification and delivery record

- Existing 22 R suites: all pass after the final UI/overflow fixes (`qa/v111-final-regression.log`, local only). Four existing JavaScript suites pass. Guided server navigation also passed after moving Calculate EI.
- Real-data recalculation: all 58 finite EI values match v1.10 at tolerance 1e-12. Both browser methods render all 96 positions and match all 58 displayed EI values. Normal, Review and CE Fill remain distinct.
- Visible browser: welcome/import, Arrange plate with 17-well Group selection, Excel-paste staging/application, reference setup, report, threshold 4% then reset to 3%, both methods, fold/unfold, comparisons, and well plots checked. Desktop report and mobile/long-label screenshots remain local.
- Independent read-only review found narrow Arrange grid overflow. Browser evidence before/after: document 702px at viewport 391px, then document 415px at viewport 430px with scrolling confined to the 650px plate.
- A 262-character label reproduced dialog horizontal overflow (938px client / 2609px scroll). Fixed by wrapping its flex child: desktop 938/938, mobile 328/328, Close remains inside the viewport. The acquisition selector's hidden state was checked with computed display:none, so no visibility change was needed. No deferred minor findings.
- The final runtime diff contains presentation changes only. Analysis expressions, thresholds, profiles, precision, and project schema are unchanged. Browser console has no warning/error entries.

Decisions: reuse the existing Shiny stack without Figma; a later template may require restyling. Use a PowerShell ledger equivalent on Windows and visual checks plus existing behavior tests instead of style-mirroring unit tests; uncommon visual edge cases still need browser inspection. Retain Bootstrap's verified hidden-selector behavior; recheck if Bootstrap changes. Preserve the worktree and submit a separate UI PR based on v1.10, as authorized in the session.
