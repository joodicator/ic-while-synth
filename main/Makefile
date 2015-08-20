GHCFLAGS := -O2 -hidir obj -odir obj

.PHONY: bin/IterativeLearn
IterativeLearn: bin/IterativeLearn
bin/IterativeLearn:
	ghc ${GHCFLAGS} -o $@ IterativeLearn.hs

.PHONY: bin/Profile
Profile: bin/Profile
bin/Profile:
	ghc ${GHCFLAGS} -o $@ Profile.hs

.PHONY: clean
clean:
	rm -rf obj/*
	rm -rf bin/*
