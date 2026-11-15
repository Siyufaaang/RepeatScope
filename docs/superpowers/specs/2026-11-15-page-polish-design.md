# RepeatScope local page polish

Approved in chat on 15 November 2026. Implement the four reviewed monochrome mockups, with subsequent corrections, against public-release source `65afd31`.

- Local only: no push, PR, upload or public deployment. Preserve the landing page and scientific calculations, including threshold-first assignment.
- Keep the existing black header, light body, fine rules, rectangular controls and green/purple scientific colors.
- Plate map: improve both import preview and Arrange plate. Compact selection controls, full 8 × 12 board, adjacent explicit bulk editor and existing Excel paste/Undo behavior.
- Sizing: source and column mapping on the left, readable preview and import checks on the right. Preserve source validation and flagged rows.
- Reference: retain gene profiles and reference/primer workflow. Put the structure and analysis setup in the right column; keep advanced controls accessible and help labels readable.
- Report: retain plate/selected-well comparison, paired physical plots and existing calculations. Compact controls and comparisons; threshold is one numeric field with `%` outside, without slider or Reset. Acquisition/allele selection remains conditional on multiple records.
- Report well summary: count unique physical wells by the exact assigned Role. Display only nonzero categories, including distinct control types, CE Fill, unused and pending when present. Never count records/alleles as extra wells or imply missing roles are controls.
- Mock image numbers, sequences, genotype text and comparison cutoffs are illustrative, not implementation requirements. Actual application data supplies every result.

Approach: preserve existing R/Shiny bindings and JavaScript state, reorganize UI containers and use a scoped workflow stylesheet. A full rewrite or the old v1.9 design would discard already approved behavior and is out of scope.

Verification: synthetic R/JS suites; meaningful tests for role counts and numeric threshold events; browser checks for every revised page, import previews, well selection, threshold propagation and narrow screens. Confirm landing/scientific files are unchanged.
