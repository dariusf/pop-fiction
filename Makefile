
.PHONY: exe drama clean

drama:
	swipl -g 'init(X)' src/tragedy.pl

exe:
	rm -rf _build
	mkdir -p _build
	swipl -o _build/tragedy -g 'init(X)' -c src/tragedy.pl --stand_alone=true

clean:
	rm -rf _build
