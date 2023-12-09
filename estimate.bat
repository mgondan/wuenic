swipl -g "consult('xsb/wuenic_ver_4.pl'), consult('xsb/%1'), estimate" -g halt
swipl -g "(file_sha1('out/%1.v40.txt', V40), writeln(v40-V40))" -g halt

rem "c:\Program Files\R\R-4.3.2\bin\x64\R.exe" --file=R\wuenic_ver_4.R --args arg