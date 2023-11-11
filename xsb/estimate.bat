call xsb64 --quietload --nobanner --noprompt -e "consult(wuenic_work), consult(%1), estimate, halt."
echo New
"C:\Progra~1\swipl\bin\swipl.exe" -g "(file_sha1('%1.new.txt', SHA1), writeln(SHA1), halt)"

call xsb64 --quietload --nobanner --noprompt -e "consult(wuenic_ver_3), consult(%1), estimate, halt." 2> old.txt
echo Old
"C:\Progra~1\swipl\bin\swipl.exe" -g "(file_sha1('%1.old.txt', SHA1), writeln(SHA1), halt)"
