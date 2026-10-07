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
MFCL ?= $(if $(strip $(INPUT)),$(INPUT)/mfclo64,)
SELFTEST_REPS ?= 1:50

.PHONY: help verify results list extract baseline-list prepare rerun refit-plan refit

help:
	@printf '%s\n' 'make verify       Check the preserved source and saved files with R.' 'make results      Rebuild the report from saved results.' 'make list         List the 50 saved replicate folders.' 'make extract OUT=/absolute/new-results' 'make prepare INPUT=/absolute/new-baseline' 'make rerun        Explain the missing saved native closure.' 'make refit-plan   Show the pinned full-refit recipe without executing it.' 'make refit INPUT=/absolute/baseline OUT=/absolute/fresh' 'MFCL defaults to INPUT/mfclo64. See reproduce/mfclkit.md for packages and SELFTEST_REPS.'

verify:
	Rscript --vanilla reproduce/verify.R --verify

list:
	Rscript --vanilla reproduce/verify.R --list

extract:
	Rscript --vanilla reproduce/verify.R --extract "$(OUT)"

baseline-list:
	Rscript --vanilla reproduce/baseline.R --list

prepare:
	Rscript --vanilla reproduce/baseline.R --prepare "$(INPUT)"

results: all

rerun:
	@printf '%s\n' 'Saved-PAR native reruns need the 50 original final PARs and simulated inputs, which are absent here.' 'Use make results for the saved report, or read reproduce/mfclkit.md for new full refits.' >&2
	@exit 2

refit-plan:
	SELFTEST_REPS="$(SELFTEST_REPS)" Rscript --vanilla reproduce/refit.R --plan --input "$(INPUT)" --out "$(OUT)" --mfcl "$(MFCL)"

refit:
	SELFTEST_REPS="$(SELFTEST_REPS)" Rscript --vanilla reproduce/refit.R --run --input "$(INPUT)" --out "$(OUT)" --mfcl "$(MFCL)"
