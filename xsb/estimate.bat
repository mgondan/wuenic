xsb64 -e "consult('wuenic_work.pl')." -e "consult(data)." -e "estimate."
"C:\Program Files\swipl\bin\swipl.exe" -g "(file_sha1('wuenic.out', SHA1), writeln(SHA1))" -g halt.
