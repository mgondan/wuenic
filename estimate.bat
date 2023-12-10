swipl -g "consult('xsb/wuenic_ver_4.pl'), consult('xsb/%1'), estimate" -g halt
R --file=R\wuenic_ver_4.R --args %1
swipl -g "(file_sha1('out/%1.v40.txt', V40), writeln(V40-v40))" -g halt
swipl -g "(file_sha1('out/%1.R.txt', R), writeln(R-r))" -g halt

