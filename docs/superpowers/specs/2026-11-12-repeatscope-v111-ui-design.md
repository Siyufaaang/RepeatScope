# RepeatScope v1.11 UI

User direction: unify the interface, especially the 96-well report, into a concise professional research tool. Do not change analysis logic. No Figma connection or template is needed. User has authorized autonomous implementation without questions.

Use the existing Shiny + HTML/CSS/SVG stack. One Segoe UI font family, neutral white surfaces, slate text, subdued blue accent, a small spacing scale, and consistent controls. Replace accumulated CSS overrides with organized styles. Keep the existing input IDs, handlers, formulas, profile data, target assignment, thresholds, precision and saved project format.

The plate is the report's main surface: white wells, a narrow group-color marker, sample names, tabular EI values, subdued well coordinates. Keep Normal and Review as text, preserve separate target values and controls, and retain accessible full labels. Diagnostics and explanation remain available in closed disclosure sections. Method selection, empty-row folding, and per-well plots remain functional. Modal plots use the same typography and readable axes.

All workflow pages receive the same visual system. Compact report controls and workflow navigation without hiding relevant stale/error state. At desktop size the full 8 x 12 plate should be visible with modest page scrolling; narrow screens may scroll the plate horizontally within its own region.

Acceptance: existing R/JS regressions pass; all scientific files and the real-data EI values remain unchanged; visible-browser inspection of report, well dialog, arrangement, reference setup, welcome/import and a narrow viewport; clean console/server logs; new local trial entry and reviewable Git branch.
