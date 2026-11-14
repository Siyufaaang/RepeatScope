# Threshold-first local analysis

The user approved applying the locally compared order to both methods: fix the
target modal and physical search support, exclude Height < 0.03 * modal Height,
then calibrate/select peaks. Weak peaks must not influence Adaptive spacing,
Bayesian seed competition, or tail ambiguity. Equality is included and missing
repeat indices are never compressed. Raw peaks remain available for inspection.

The preceding local comparison considered filtering only during EI calculation
versus filtering before assignment. The user chose the latter after all 116
paired SCA2 brain EI values were unchanged, while weak peaks affected spacing.

Implementation choice: make 3% a common assignment floor in the application
adapter. Existing configurable EI filtering remains an additional filter on the
frozen assignments; values below 3% cannot recover excluded peaks. Keep historical
model functions and batch-runner defaults unchanged. Invalidate saved old-policy
fits and describe the assignment floor in exports and visible help.

The local teaching animation uses labelled synthetic data only. It starts with
the complete physical peak table/profile, zooms to the expansion tail, draws the
3% dashed line, fades excluded low peaks, then assigns eligible peaks. Peaks exist
at fixed size coordinates before selection; selection changes color. Bayesian
tail intervals appear together after seed calibration. Use automatic playback
with pause/replay, no next/previous buttons. Separate plot containers must not
overlap. Respect reduced motion.

Keep public hosting unchanged. Do not copy real research inputs into tracked
fixtures or browser assets. No website deployment, remote push, or video release.
