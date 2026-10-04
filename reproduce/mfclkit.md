# Selftest reproduction

From the repository root, `make verify` checks the preserved files and
`make results` rebuilds the report from the 50 saved results. `make help`
lists the entry points. These commands do not refit the model.

The original 50 final PARs and matching simulated input sets were not recovered here.
`make rerun` therefore stops: the saved recovery tables cannot provide a
native saved-PAR rerun. New full refits are a separate operation.

## Full refits with mfclkit

Use R on Linux x86-64 with `sha256sum`, the pinned MFCL engine, and authorised
access to the private packages or their exact installed source revisions:

| Package | Source revision |
| --- | --- |
| mfclkit | `c59065e9b5c754c235bbb0b988a83e3aaf3282e9` |
| FLR4MFCL | `ff8367fcec19baff98333170c0f1bca3f9903029` (1.7.2) |
| mfclshiny | `c665f579a9e63f5252918fb9cab034f0c1e33d5b` |

These are the original native-fit revisions. The wrapper checks installed
`RemoteSha` values; it does not install packages or obtain credentials.
Use a host outside an inherited Condor job, because the historical self-test
runner contains Condor archive-cleanup code.

Prepare `INPUT` with `bet.frq`, `bet.ini`, `bet.tag`, `bet.age_length`,
`bet.reg_scaling`, `mfcl.cfg`, `doitall.sh`, and the generating `final.par`.
The wrapper checks their pinned sizes and SHA256 values. The public source is
[Jitter bb3f4016](https://github.com/PacificCommunity/ofp-sam-bet-2026-jitter/tree/bb3f4016b2d145f42c7a76072ed2b10b49aff71f/data/diagnostic/mfcl),
with `final.par` from its
[fitted reference](https://github.com/PacificCommunity/ofp-sam-bet-2026-jitter/tree/bb3f4016b2d145f42c7a76072ed2b10b49aff71f/data/diagnostic/reproduction/fitted-reference).
Use the original [Diagnostic INI](https://github.com/PacificCommunity/ofp-sam-bet-2026-diagnostic/blob/3abf0c64fb9b0c2d70b9c672dc7d9a655d3060d6/model/bet.ini);
the later Jitter INI differs by one byte. `MFCL` must be the published F5 engine
(SHA256 `f5bc1e232a86e51f920bce7271d8e0930d0b160e4d18dc46de44078f0fa24cd0`).

```sh
make refit-plan INPUT=/absolute/baseline OUT=/absolute/new-selftest MFCL=/absolute/mfclo64
make refit INPUT=/absolute/baseline OUT=/absolute/new-selftest MFCL=/absolute/mfclo64
```

`refit-plan` prints the recipe without loading packages, creating folders or
running a model. `refit` creates a fresh `OUT` outside the repository and
copies the baseline there. Existing input files are checked before and after.
The parent folder of `OUT` must exist. Leave `OUT` absent until `refit` creates it.

The wrapper calls `mfk_run_selftest()` for replicates 1–50, seed `20260519`,
with the fitted generating PAR, `conditional_postmixing` tag simulation,
`doitall` refits, final convergence exponent −4, and fixed tag overdispersion
τ=2. For a selected replicate, add `SELFTEST_REPS=1`; ranges and comma-separated
replicate numbers are also accepted. It runs serially and does not submit jobs.

Future runs retain their simulated native inputs, final PARs, logs and model
payloads: compact cleanup is disabled, with `keep_model_payload` and
`keep_sim_debug` enabled. This changes retention only. The inspected original archives used
compact cleanup and did not retain these native files. New fits have not been
executed or checked by these instructions; the historical R/RNG runtime is
incompletely recorded, so the fixed seed alone does not establish exact
historical input or result reproduction.
