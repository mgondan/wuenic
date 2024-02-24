R --quiet --no-echo --file=R/wuenic_ver_4.R --args $1 2>warn4r.txt
R --quiet --no-echo --file=R/wuenic_ver_4.1.R --args $1 2>warn41r.txt
sha1sum out/$1.pl.R.txt
sha1sum out/$1.R41.txt