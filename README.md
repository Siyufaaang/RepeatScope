# RepeatScope

RepeatScope v1.10 is a local R/Shiny application for plate-based repeat-tail analysis. It imports an Excel plate map and already-sized peak tables, records explicit genotype and sample metadata, and compares Adaptive and Bayesian calculations in a 96-well report.

This is a research prototype. Computational previews and formal evidence/QC are separate; software tests do not establish an experimentally validated assay.

## Run

Use R 4.5 or a compatible version, with `shiny`, `bslib`, `jsonlite`, `readxl`, `writexl`, `zip`, `curl`, `ggplot2` and `digest` installed. Run from the repository root:

```sh
Rscript --vanilla development/shiny_app/run.R
```

Open <http://127.0.0.1:3838/>. Set `REPEATSCOPE_PORT` to choose another local port. The server binds to loopback. On the existing Windows workstation:

```powershell
./development/shiny_app/run.ps1 -Port 3862
```

Use **Save project** to keep a session across server restarts. On the welcome page, **Continue a saved project or try synthetic data** provides the built-in synthetic demonstration.

## Workflow

1. Import and preview an 8 × 12 Excel plate map.
2. In **Arrange plate**, record explicit genotype, Group, Role and optional Individual ID. Group/Individual selectors only select wells. Use **Table / Excel paste** for cell editing or staged clipboard paste; apply edits explicitly, or discard/undo them.
3. Import an already-sized peak table. Physical peaks, acquisition identities and flagged rows remain available for review.
4. In **Reference & analysis**, enter a gene or disease alias to load its built-in profile, then match the reference and primers. Disease ranges and provenance are supplied by the profile; filters, custom ranges and CE calibration remain under **Advanced**.
5. **Results** opens a compact 96-well report: sample names and the selected method's EI. Switch Adaptive/Bayesian on the plate; open a well for both methods and physical peaks. Comparisons and group summaries are initially collapsed. Input changes hide stale EI until analysis is updated.

Leading zeros remain text. Blank pasted cells clear their field; omitted columns are unchanged. Invalid or stale table patches are rejected atomically. Group and Individual labels alone do not refit models. Genotype is never inferred from group membership.

## Disease profiles and EI
Intermediate targets calculate EI. Normal targets are excluded by category; missing or ambiguous physical targets remain unavailable, not zero. The application records provisional range estimates separately from EI and formal evidence/QC. Old v1.9 fits require a rerun under the new allocation policy.

The versioned JSON catalog at `development/shiny_app/profiles/diseases.json` covers ATXN1, ATXN2, ATXN3, CACNA1A/SCA6, ATXN7, TBP/SCA17, HTT, ATN1 and AR. Each profile includes aliases, counting notes, range bands, source URL and reviewed date. Conditional boundaries and interruption dependence are retained. This catalog is not a claim of support for every repeat disease or a clinical diagnostic service.

A project stores the profile snapshot it used; reopening does not silently replace customized ranges. Gene changes clear the previous gene's rules. Primer-dependent lengths, channel and CE uncertainty are not disease constants. Unknown CE precision permits nominal research calculations but does not establish formal range certainty. A single available channel can be selected automatically; multiple channels require an explicit choice.

## Repository contents

- `development/shiny_app/R/`, `www/`: application source.
- `development/v2_4/`: unchanged core and Adaptive method source, loaded in define-only mode by the app. Historical batch runners are not the application entry point.
- `development/benchmark/bayesian/`, `hierarchical/`: model source used by the app.
- `development/shiny_app/tests/`: synthetic tests that run without private research files.
- `development/shiny_app/examples/`: explicitly synthetic input examples and a blank plate template.

`repository-files.txt` and `.gitignore` allow only reviewed source files and synthetic examples. Real inputs, FSA files, project snapshots, research results, screenshots, local logs, package libraries and credentials are excluded. The full local real-data regression suite and its audit remain on the original workstation. The repository does not upload research data or run an externally hosted service.

## Verify

From the checkout root:

```sh
Rscript --vanilla development/shiny_app/tests/run_shareable.R
node development/shiny_app/tests/test_layout_paste.cjs
node development/shiny_app/tests/test_layout_drafts.cjs
node development/shiny_app/tests/test_dashboard_identity.cjs
node development/shiny_app/tests/test_dashboard_compact.cjs
```

The synthetic suite covers state, import/export, reference handling, methods, eligibility/QC, server navigation, threshold synchronization, v1.9 layout edits, atomic rejection, independent drafts and Undo. The local real-data checks are separate from these shareable tests.

Before committing new files, review their contents, add their paths to both `repository-files.txt` and `.gitignore`, and run `tools/verify_repository.ps1`. Do not use `git add -f` to bypass the allowlist. See [CONTRIBUTING.md](CONTRIBUTING.md).
