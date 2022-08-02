LANG="C"
BENCH="CG"
CLASS="S"

setup:
	./scripts/setup.sh

ENV:
	source ENV

bench:
	./scripts/make_single.sh $(LANG) $(BENCH) $(CLASS)

all_benches:
	./scripts/make_all.sh
