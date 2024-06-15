# wuenic41

Please make sure that R.exe and swipl.exe are on the PATH. R is used for the main task, swipl.exe is used for the SHA checksums.

Prepare
````
$ cd wuenic
$ R
R> remotes::install_github("mgondan/wuenic")
R> quit()
$ cp wuenic2023.mdb countries
````

For a single country
````
$ cd wuenic
$ estimate.bat bgd
$ sha1sum.bat bgd
````
The output file is found in the folder out, bgd.txt.

For all countries
````
$ cd wuenic
$ all.bat
$ sha1all.bat > v41.txt
````

Compare SHA to Version 4R:
````
$ diff v41.txt ..\wuenic4\v4R.txt
````

You will find harmless differences in irq, lao, nga, ton (see NEWS.md).
