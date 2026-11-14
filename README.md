# RepeatScope

RepeatScope v1.11.1 is an R/Shiny application for plate-based repeat-tail analysis, with local and hosted research Beta entry points. It imports an Excel plate map and already-sized peak tables, records explicit genotype and sample metadata, and compares Adaptive and Bayesian calculations in a 96-well report.

v1.11 unifies typography, controls and group colors across the workflow. The white 96-well report emphasizes sample names and EI, with window/threshold controls and diagnostics available on demand. It preserves v1.10 analysis logic, profiles, precision and project format.

v1.11.1 fixes repeated flashing in Reference & analysis when typing ahead of a server response. Profile loading no longer writes an older gene value back into the active input. Analysis formulas and disease profiles are unchanged.

This is a research prototype. Computational previews and formal evidence/QC are separate; software tests do not establish an experimentally validated assay.

Both methods exclude peaks below
3% of the target's own modal height before seed calibration, spacing updates and
assignment. Equality is included. Physical peaks, modal and raw search support
remain available; missing repeat indices are not compressed. A low peak excludes
only itself, not later eligible candidates. Previously saved
fits must be rerun under this policy. The synthetic teaching animation
shows inventory, the modal and tail, the threshold, exclusion, assignment and EI
in that order. One chart uses a labelled square-root height scale with ticks in
thousands of original RFU; thresholding and EI retain untransformed heights.
Playback uses 3–4 seconds per step and simultaneous Bayesian tail intervals.
Back/Next pauses playback for manual reading; Play resumes.

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
5. **Results** opens a compact 96-well report: sample names and the selected method's EI. Use **Calculate EI** in the top navigation; expand **EI window & height threshold** to choose which fitted peaks contribute to EI. Switch Adaptive/Bayesian on the plate; open a well for both methods and physical peaks. Comparisons and group summaries are initially collapsed. Input changes hide stale EI until analysis is updated.

Leading zeros remain text. Blank pasted cells clear their field; omitted columns are unchanged. Invalid or stale table patches are rejected atomically. Group and Individual labels alone do not refit models. Genotype is never inferred from group membership.

## Disease profiles and EI
Intermediate targets calculate EI. Normal targets are excluded by category; missing or ambiguous physical targets remain unavailable, not zero. The application records provisional range estimates separately from EI and formal evidence/QC. Old v1.9 fits require a rerun under the new allocation policy.

New projects default to **Tail10**, which uses selected peaks at repeat offsets Δ0–10 from the modal peak. **Extended** uses selected peaks throughout the frozen search support. Both methods first enforce the 3% assignment floor. The chosen reporting height threshold then applies an additional EI filter; values below 3% cannot restore excluded peaks. EI is the peak-height-weighted mean repeat offset within that window; changing the reporting window or threshold recalculates EI from existing assignments without refitting either model. If the methods differ only beyond Δ10, their Tail10 EI can be exactly equal while their Extended EI differs. Compare methods using the same window and threshold; exports record the assignment policy separately.

The versioned JSON catalog at `development/shiny_app/profiles/diseases.json` covers ATXN1, ATXN2, ATXN3, CACNA1A/SCA6, ATXN7, TBP/SCA17, HTT, ATN1 and AR. Each profile includes aliases, counting notes, range bands, source URL and reviewed date. Conditional boundaries and interruption dependence are retained. This catalog is not a claim of support for every repeat disease or a clinical diagnostic service.

A project stores the profile snapshot it used; reopening does not silently replace customized ranges. Gene changes clear the previous gene's rules. Primer-dependent lengths, channel and CE uncertainty are not disease constants. Unknown CE precision permits nominal research calculations but does not establish formal range certainty. A single available channel can be selected automatically; multiple channels require an explicit choice.

## Repository contents

- `development/shiny_app/R/`, `www/`: application source.
- `development/v2_4/`: unchanged core and Adaptive method source, loaded in define-only mode by the app. Historical batch runners are not the application entry point.
- `development/benchmark/bayesian/`, `hierarchical/`: model source used by the app.
- `development/shiny_app/tests/`: synthetic tests that run without private research files.
- `development/shiny_app/examples/`: explicitly synthetic input examples and a blank plate template.

`repository-files.txt` and `.gitignore` allow only reviewed source files and synthetic examples. Real inputs, FSA files, project snapshots, research results, screenshots, local logs, package libraries and credentials are excluded. The full local real-data regression suite and its audit remain on the original workstation. Deployment contains only the narrower `deployment/runtime-files.txt` allowlist.

## Hosted research Beta

Public Beta (verified 2026-11-13): [Open RepeatScope](https://01a1d5a3-31ce-5e40-498d-e4d4dc298453.share.connect.posit.cloud/). Release evidence and rollback details are in [the release plan](docs/superpowers/plans/2026-11-13-public-beta.md#verified-public-release--2026-11-13).

The hosted entry point preserves the original source layout and scientific calculations. Visitors start an empty session or load the labelled synthetic demo. Uploads are processed by the hosting server; use only synthetic or de-identified data you are authorized to upload. Session state is temporary. Download **Save project** before leaving and retain your original inputs. A public website does not make another visitor's session your project, and computational previews are not diagnostic clearance.

Prepare a clean bundle from the repository root, using an R environment with the application dependencies installed:

```r
source('tools/prepare_beta.R')
bundle <- prepare_beta('.', '../repeatscope-beta-bundle')
rsconnect::writeManifest(appDir=bundle$directory, appFiles=bundle$files, appMode='shiny')
```

The destination must not exist. The packager uses the explicit runtime manifest, validates it against the repository allowlist, and records SHA256 for each copied file. Do not deploy the full research workspace. Verify the actual selected bundle with `Rscript --vanilla development/shiny_app/tests/test_deployment.R ../repeatscope-beta-bundle` and the normal suites before publishing. Both acceptance and deployment compare that bundle with the reviewed source, so an older bundle cannot pass solely on its own checksums.

Use `rsconnect` 1.11.2 or newer from CRAN and authorize the host separately with `rsconnect::connectCloudUser()`. Do not store tokens in this repository. Direct upload supports a private source repository:

```sh
Rscript --vanilla tools/deploy_beta.R ../repeatscope-beta-bundle ACCOUNT_NAME
```

Generate the manifest in the tested application library before switching to any separate publishing-tool library. The deployment command verifies bundle hashes and the manifest's exact file list/checksums before any upload, and uses that frozen manifest to preserve the tested R/package versions. Retain the ignored deployment directory and its rsconnect record for subsequent releases. To roll back, redeploy a previously verified bundle from its matching source checkout to the same content, then repeat the synthetic browser acceptance checks. Publication status and the final public URL must be verified on the hosting service; source integration alone does not establish a live deployment.

## Verify

From the checkout root:

```sh
Rscript --vanilla development/shiny_app/tests/run_shareable.R
node development/shiny_app/tests/test_layout_paste.cjs
node development/shiny_app/tests/test_layout_drafts.cjs
node development/shiny_app/tests/test_dashboard_identity.cjs
node development/shiny_app/tests/test_dashboard_compact.cjs
node development/shiny_app/tests/test_learning_player.cjs
node development/shiny_app/tests/test_visual_semantics.cjs
```

The synthetic suite covers state, import/export, reference handling, methods, eligibility/QC, server navigation, threshold synchronization, v1.9 layout edits, atomic rejection, independent drafts and Undo. An independently summed fixture checks equal Tail10 values, distinct Extended values, separate method peak flags and unchanged fits when the window changes. The local real-data checks are separate from these shareable tests.

Before committing new files, review their contents, add their paths to both `repository-files.txt` and `.gitignore`, and run `tools/verify_repository.ps1`. Do not use `git add -f` to bypass the allowlist. See [CONTRIBUTING.md](CONTRIBUTING.md).
