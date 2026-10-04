.PHONY: all validate clean

all:
	./run-report

validate:
	Rscript report/validate.R --outputs

clean:
	rm -rf results/figures results/tables results/selftest-report.html results/selftest-audit.csv results/figure-manifest.csv results/table-manifest.csv

# Saved results and new full refits are separate operations.
INPUT ?=
OUT ?=
MFCL ?=
SELFTEST_REPS ?= 1:50

.PHONY: help verify results rerun refit-plan refit

help:
	@printf '%s\n' 'make verify       Check the preserved source and saved files.' 'make results      Rebuild the report from saved results.' 'make rerun        Explain the missing saved native closure.' 'make refit-plan   Show the pinned full-refit recipe without executing it.' 'make refit INPUT=/absolute/baseline OUT=/absolute/fresh MFCL=/absolute/mfclo64' 'See reproduce/mfclkit.md for prerequisites and SELFTEST_REPS selection.'

verify:
	python3 ci/verify-preserved-files.py

results: all

rerun:
	@printf '%s\n' 'Saved-PAR native reruns need the 50 original final PARs and simulated inputs, which are absent here.' 'Use make results for the saved report, or read reproduce/mfclkit.md for new full refits.' >&2
	@exit 2

refit-plan:
	SELFTEST_REPS="$(SELFTEST_REPS)" Rscript --vanilla reproduce/refit.R --plan --input "$(INPUT)" --out "$(OUT)" --mfcl "$(MFCL)"

refit:
	SELFTEST_REPS="$(SELFTEST_REPS)" Rscript --vanilla reproduce/refit.R --run --input "$(INPUT)" --out "$(OUT)" --mfcl "$(MFCL)"
