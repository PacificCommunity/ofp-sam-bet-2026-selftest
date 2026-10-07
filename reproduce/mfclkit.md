# Selftest reproduction

[View the 2026 results](https://pacificcommunity.github.io/ofp-sam-bet-2026-selftest/selftest-report.html).
The saved results cover 50 completed simulation-refit replicates, with tag
overdispersion fixed at τ=2.

| Results | Contents |
| --- | --- |
| [HTML report](../results/selftest-report.html) | Methods, recovery figures and interpretation |
| [Figures](../results/figures/) | Three figures, each in PNG and PDF |
| [Run summary](../data/diagnostic/selftest-runs.csv) | Completion, convergence, objective and gradient by replicate |
| [Audit](../results/selftest-audit.csv) | Settings and source checksums |
| [R payload](../data/diagnostic/selftest-report-payload.rds) | Annual, parameter and assessment recovery; simulated-data checks |
| [Detailed results](saved-results.tar.gz) (3.5 MB) | Original RDS for all 50 replicates: simulation, tag checks, settings and model summaries |

From the repository root:

```sh
make verify
make results
```

`make results` rebuilds the report and figures from saved results. It needs R
with ggplot2, patchwork, dplyr, scales and jsonlite, plus `sha256sum`.
To read the saved values directly in R:

```r
x <- readRDS("data/diagnostic/selftest-report-payload.rds")
x$derived
```

Other components include `runs`, `parameters`, `management` and `simulation`.
`make help` lists the commands.

For the detailed original RDS, use `make extract OUT=/absolute/new-results`,
then `readRDS()`. `make list` lists `rep_001`–`rep_050`. R verifies every
[file checksum](saved-results.json) before extraction. These are summaries;
the original simulated inputs and fitted PARs remain missing.
Verification and extraction use base R and `sha256sum` (or `shasum`); no Python.

## Full refits with mfclkit

Use R on Linux x86-64 with `sha256sum` and the exact installed packages below.
Source installation requires authorised access to the private repositories.
The wrapper checks each package's `RemoteSha`.

| Package | Source revision |
| --- | --- |
| [mfclkit](https://github.com/PacificCommunity/ofp-sam-mfclkit/commit/c59065e9b5c754c235bbb0b988a83e3aaf3282e9) | `c59065e9b5c754c235bbb0b988a83e3aaf3282e9` |
| [FLR4MFCL](https://github.com/PacificCommunity/ofp-sam-flr4mfcl/commit/ff8367fcec19baff98333170c0f1bca3f9903029) | `ff8367fcec19baff98333170c0f1bca3f9903029` (1.7.2) |
| [mfclshiny](https://github.com/PacificCommunity/mfclshiny/commit/c665f579a9e63f5252918fb9cab034f0c1e33d5b) | `c665f579a9e63f5252918fb9cab034f0c1e33d5b` |

Prepare a new baseline from the checksum-pinned public files:

```sh
make prepare INPUT=/absolute/new-baseline
make refit-plan INPUT=/absolute/new-baseline OUT=/absolute/new-selftest
make refit INPUT=/absolute/new-baseline OUT=/absolute/new-selftest
```

The downloader retains the generating `final.par`, original six inputs,
Selftest `doitall.sh` and static F5 `mfclo64`. `make baseline-list` shows their
exact sources and hashes. `MFCL` defaults to `INPUT/mfclo64`; preparation runs
no model. The original Diagnostic INI is required—the later Jitter INI differs
by one byte.

`refit-plan` prints the recipe only. `refit` creates `OUT`; leave it absent,
outside the checkout and `INPUT`, with an existing parent. Use absolute paths.
Run outside an inherited Condor job.

The wrapper calls `mfk_run_selftest()` for replicates 1–50, seed `20260519`,
with the generating PAR, `conditional_postmixing` tag simulation, `doitall`
refits and final convergence exponent −4. Add `SELFTEST_REPS=1` for one replicate.
Runs are serial; new simulated inputs, final PARs, logs and
`new-selftest-runs.rds` remain under `OUT`.

The original 50 fitted PARs and matching simulated inputs are unavailable here,
so `make rerun` stops. These full refits have not been run or compared with the
2026 results. The historical R/RNG runtime is incompletely recorded; the seed
alone cannot establish identical historical simulations.
