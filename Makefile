LANG="C"
BENCH="CG"
CLASS="S"
PROCS=1
GRAIN=1

setup:
	./scripts/setup.sh

ENV:
	source ENV

bench:
	./scripts/make_single.sh $(LANG) $(BENCH) $(CLASS)

all_benches:
	./scripts/make_all.sh

run_bench:
	./scripts/run_single.sh $(LANG) $(BENCH) $(CLASS) $(PROCS) $(GRAIN)

run_all:
	./scripts/run_all.sh
