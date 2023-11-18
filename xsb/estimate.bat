call xsb64 --quietload --nobanner --noprompt -e "consult(wuenic_ver_3_9), consult('%1'), estimate, halt."
call xsb64 --quietload --nobanner --noprompt -e "consult(wuenic_ver_3), consult('%1'), estimate, halt." 2> old.txt
echo %1
"C:\Progra~1\swipl\bin\swipl.exe" -g "(file_sha1('%1.new.txt', New), file_sha1('%1.old.txt', Old), writeln(new-New), writeln(old-Old), halt)"
