
.PHONY: exe drama clean

drama:
	swipl -g 'main' src/tragedy.pl

exe:
	rm -rf _build
	mkdir -p _build
	swipl -o _build/tragedy -g 'main(X)' -c src/tragedy.pl --stand_alone=true

clean:
	rm -rf _build
