GHCFLAGS := -O2 -hidir obj -odir obj

.PHONY: bin/IterativeLearn
bin/IterativeLearn:
	ghc ${GHCFLAGS} -o $@ IterativeLearn.hs

.PHONY: clean
clean:
	rm -rf obj/*
	rm -rf bin/*
