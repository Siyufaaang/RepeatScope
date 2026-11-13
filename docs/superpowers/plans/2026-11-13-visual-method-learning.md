# Visual Method Learning Implementation Plan

> **For agentic workers:** Use superpowers:executing-plans inline, with a fresh final reviewer.

**Goal:** Make field guidance and methods understandable visually while retaining scientific behavior.
**Architecture:** Structured field-help metadata + delegated accessible popover; static MathML method panels + isolated SVG player driven by audited synthetic JSON from existing R functions.
**Tech stack:** R/Shiny, native HTML/MathML/SVG/CSS/JavaScript. No new runtime dependencies.
**Spec:** docs/superpowers/specs/2026-11-13-visual-method-learning.md

## Global constraints
- No engine, selection, reference, state/fingerprint, report math or user data changes.
- Keep existing Shiny input IDs, labels and event bindings; help must not mutate inputs.
- Preserve teal/purple semantic meaning, use text and shapes alongside color.
- Animation data is synthetic and independent of user projects; start paused, respect reduced motion.

## Review focus
- Conditional requirements must not imply unknown QC/uncertainty is a mandatory invented value.
- Help controls must work for dynamic fields and must not toggle adjacent checkboxes.
- Popovers must survive hover into content, dismiss with Escape, and stay within small viewports.
- Math notation and animated seed/window/threshold states must match frozen R outputs.
- Timers must stop when changing panels, navigating away, hiding the tab or finishing playback.

## Tasks
- [x] Field help: complete unit/example/requirement metadata; accessible inline help rendering and delegated behavior; tests for unchanged input values and required conditions.
- [x] Method visualization: generate frozen synthetic trace with existing methods; MathML equations and parameter keys; controllable SVG player; verify frames against method outputs and EI arithmetic.
- [x] Verify full R/JS suites, browser keyboard/hover/click and mobile, fresh review, runtime allowlist, merge/deploy existing public URL, public acceptance.

## Release acceptance
- PR #7 merged as 30adc72; presentation implementation f5f648f.
- 26 synthetic R suites and all 6 JavaScript suites pass. Frozen seed windows, predictions and EI match unchanged R methods.
- Independent fresh-context review: no material findings.
- Browser checks: MathML fractions/posteriors, click/focus/Escape help, unchanged adjacent checkbox, dynamic table help, mobile popup bounds, contained equation scrolling, playback and keyboard tab switching.
- Repository allowlist: 115 reviewed files. Deployment: 55 runtime files, 61 dependencies, existing public content ID retained.
- Public acceptance: EI/Adaptive/Bayesian panels loaded; Bayesian seed step displayed 210.100-211.900 bp; input help displayed meaning/unit/example; no browser errors captured.
- Existing public URL verified: https://01a1d5a3-31ce-5e40-498d-e4d4dc298453.share.connect.posit.cloud/
- Scientific calculation files and input defaults unchanged.
