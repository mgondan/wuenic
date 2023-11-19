call xsb64 --quietload --nobanner --noprompt -e "consult(wuenic_ver_4), consult('%1'), estimate, halt."
call xsb64 --quietload --nobanner --noprompt -e "consult(wuenic_ver_3_9), consult('%1'), estimate, halt." 2> old.txt
echo %1
"C:\Progra~1\swipl\bin\swipl.exe" -g "(file_sha1('%1.v40.txt', V40), file_sha1('%1.v39.txt', V39), (V39 = V40 -> true ; writeln(v40-V40), writeln(v39-V39)))" -g halt
