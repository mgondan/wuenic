BASE=`basename -s .pl $1`
R --quiet --no-echo --file=R/wuenic_ver_4.R --args $BASE 2>warn4r.txt
R --quiet --no-echo --file=R/wuenic_ver_4.1.R --args $BASE 2>warn41r.txt
sha1sum out/$BASE.pl.R.txt
sha1sum out/$BASE.R41.txt
