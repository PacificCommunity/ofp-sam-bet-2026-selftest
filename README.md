[![Preservation checks](https://github.com/PacificCommunity/ofp-sam-bet-2026-selftest/actions/workflows/verify-preserved-results.yml/badge.svg?branch=main)](https://github.com/PacificCommunity/ofp-sam-bet-2026-selftest/actions/workflows/verify-preserved-results.yml?query=branch%3Amain)

# BET 2026 Diagnostic model self-test

[View results](https://pacificcommunity.github.io/ofp-sam-bet-2026-selftest/selftest-report.html).

The report covers 50 completed simulation-refit replicates for the BET 2026
Diagnostic model. The generating model and every refit fix tag overdispersion
at τ=2. Results include annual recovery, assessment quantities, parameter
recovery and pseudo-data centring checks.

From the repository root:

```sh
make verify
make list
make extract OUT=/tmp/bet-saved-results
make results
```

The runner checks `data/SHA256SUMS` and writes the self-contained HTML report,
PNG/PDF figures and copy-ready captions to `results/`. It rebuilds the report
from the saved payload without running MFCL.

The 50 original native final PARs and matching simulated input sets were not recovered here,
so a saved-PAR native rerun is unavailable. For new full simulation refits,
see the [mfclkit guide](reproduce/mfclkit.md): `make refit-plan` shows the pinned
recipe and `make refit` runs it in a fresh output folder with native files
retained. `make help` lists the commands.
