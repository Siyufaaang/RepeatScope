# Working on RepeatScope

Run commands from the repository root. Keep the v1.9 baseline available for rollback and make subsequent changes on a separate branch.

1. Check `git status` before editing and preserve unrelated local changes.
2. Make a focused change. Add a meaningful regression test when behavior changes.
3. Run `development/shiny_app/tests/run_shareable.R` with Rscript and the seven JavaScript test files listed in README.
4. Inspect `git diff` and `git diff --cached`. Review every new file before adding it to the allowlist.
5. Run `tools/verify_repository.ps1`, then commit. Use a pull request for changes needing review.

Real research files stay local. This public repository contains reviewed source and synthetic examples only. Do not commit raw peaks, FSA files, patient/sample metadata, project RDS files, real reports, QA screenshots or logs. Synthetic examples must remain clearly labelled. Do not paste credentials into source, issue descriptions, commit messages or logs.

The original workstation also has a larger private-data regression suite. It is intentionally absent from this repository and must not be represented as having run in a clean clone. Changes to scientific methods need the appropriate local numerical/physical-selection comparisons as well as synthetic tests.

The public web application and its latest release are linked from README. This repository does not currently include an open-source license. Existing local services do not need to be stopped for Git commits.
