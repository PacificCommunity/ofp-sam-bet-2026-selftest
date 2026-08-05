# BET 2026 Diagnostic model self-test

This repository reproduces the report-ready self-test diagnostics for the 2026 WCPO bigeye tuna Diagnostic model. It uses a compact, path-free payload containing 50 completed simulation-refit replicates; it does not rerun MFCL.

The generating model and every refit fix the negative-binomial tag overdispersion parameter at τ = 2. The report includes annual recovery, key assessment-quantity and parameter recovery, pseudo-data centring checks, publication figures, and Word/LaTeX-ready tables and captions.

Run:

```sh
./run-report
```

Outputs are written to `results/`. The HTML report is self-contained; PNG and vector PDF figures are also retained. `data/SHA256SUMS` locks the public payload and report source.
