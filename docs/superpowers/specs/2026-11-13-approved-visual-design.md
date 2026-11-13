# Approved RepeatScope visual refresh

The user approved the homepage, results and Arrange plate previews, then explicitly authorized applying the design to the live website without further questions. Import plate, Import peaks and Reference & analysis use the same system.

## Design and behavior
- Quiet black header, cool off-white workspaces, square controls, restrained typography, generous whitespace and aligned peer headings.
- Teal `#029E73` for Adaptive identity and Adaptive-only peaks; purple `#A45A97` for Bayesian identity and Bayesian-only peaks. Both-included peaks remain graphite, excluded peaks gray, modal remains explicitly labelled. Amber `#A66B00` marks existing method disagreements, not errors. Missing comparisons remain distinct.
- Group colors are thin strips with names; selection stays black and independent of group. Use the user's Figma snake artwork as a real asset.
- Homepage: correctly drawn single normal peak; expanded modal followed by a decreasing tail. Add restrained square-pixel texture behind this trace inspired by the first mockup, without bringing back the old sinusoidal signal or oversized typography.
- Homepage has How it works (down arrow), bold demo link, five workflow diagrams, aligned demo/video sections and the user's feedback email `siyu.fang.24@ucl.ac.uk`. Try your data lives in the top navigation. Video is an honest reserved area until the user provides the actual recording.
- Results show plate and selected well side by side on desktop and stack on narrow screens. Preserve record/acquisition identity, both methods/windows, filters, thresholds, stale-result protections and all existing review/export actions.
- Keep the existing Shiny/Posit deployment and public URL. Do not migrate hosting or send private research data.

## Immutable scientific scope
No change to engine, localization, reference matching, model implementations, original data, saved results or numerical precision. The existing displayed-precision disagreement rule remains unchanged. Appearance changes consume the existing peak inclusion flags.

## Implementation choices
Use native HTML/CSS/SVG and existing Shiny bindings, not raster mockups as functioning screens. Keep report exports and scientific figure computation unchanged. Retain hosted-processing/session/research information in a compact expandable disclosure, while the footer contains only the feedback address.

## Approval
Approved in chat by the user's instruction to apply the previews, add the original mosaic background behind homepage peaks, finish autonomously, and ask no questions.
