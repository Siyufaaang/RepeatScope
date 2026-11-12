# RepeatScope v1.10
User authorized autonomous implementation without questions on 2026-11-12.

## Rules
Intermediate and boundary targets may calculate EI; only normal targets are excluded by category. Missing targets, genuine physical ambiguity, failed fits and controls remain distinct from zero. Allocate unique trusted non-normal clusters; regional fragments never become automatic extra alleles. Normal-only prescreen with one trusted normal cluster remains normal despite weak regional fragments. Keep scientific fitting and EI formulas unchanged; fingerprint the new allocation policy so restored old fits require rerun.

## Profiles
Versioned local JSON for ATXN1/2/3, CACNA1A-SCA6, ATXN7, TBP-SCA17, HTT, ATN1 and AR. Store aliases, motifs/unit, counting definition, range bands, caveats, source and review date. Preserve conditional/uncertain ranges and use provisional labels for length estimates. Do not claim every repeat disease is supported.
Gene selection fills disease fields; unknown/changed gene cannot retain another gene's profile. Snapshot profile/version in saved projects; restored custom configuration stays intact. Profiles have no batch channel, primer length or CE uncertainty. Derive theoretical length from matched structure, select a channel only if unambiguous, and keep missing uncertainty explicitly unknown instead of inventing precision.

## Interface
Gene/profile, reference and primer inputs, concise analysis summary and Apply remain visible. Detailed ranges, coordinates, provenance and calibration are expandable. Main report shows well, sample and selected-method EI; other method, peaks, QC and target reasons move to clicked detail. Optional comparisons start collapsed. Multiple non-normal targets retain their individual values. Final visual redesign is deferred.

## Approach
Local JSON + R adapter preserves offline reproducibility. Hardcoded per-gene UI branches would not scale; runtime web lookups would silently change old projects.
Verify with RED/GREEN synthetic tests, real original-file imports, unchanged previously calculable EI, visible browser, save/restore and independent review. Keep real data local.
