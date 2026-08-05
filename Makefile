.PHONY: all validate clean

all:
	./run-report

validate:
	Rscript report/validate.R --outputs

clean:
	rm -rf results/figures results/tables results/selftest-report.html results/selftest-audit.csv results/figure-manifest.csv results/table-manifest.csv
