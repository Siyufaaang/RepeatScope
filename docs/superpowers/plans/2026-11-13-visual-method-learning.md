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
- [ ] Verify full R/JS suites, browser keyboard/hover/click and mobile, fresh review, runtime allowlist, merge/deploy existing public URL, public acceptance.
