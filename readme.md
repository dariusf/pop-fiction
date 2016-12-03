
```sh
docker run -v $(pwd):/pop-fiction -w /pop-fiction --rm -it swipl/swipl bash

$ swipl install.pl
$ swipl -l src/detective.pl

?- run.
```