swipl -g "consult('xsb/wuenic_ver_4.pl'), consult('xsb/%1'), estimate" -g halt
R --file=R\wuenic_ver_4.R --quiet --no-echo --args %1
swipl -g "(file_sha1('out/%1.v40.txt', V40), file_sha1('out/%1.R.txt', R), writeln(V40-v40), writeln(R-r))" -g halt

