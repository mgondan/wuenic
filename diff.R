results = function(fname, mask="%s.pl.v39.txt", path="xsb")
{
  if(!is.null(mask))
    fname = sprintf(mask, fname)

  if(!is.null(path))
    fname = file.path(path, fname)

  read.table(fname, sep='\t', header=TRUE)
}

oldres = function(ccode="and", path="xsb")
  results(ccode, mask="%s.pl.v39.txt", path=path)

newres = function(ccode="and", path="xsb")
  results(ccode, mask="%s.pl.v40.txt", path=path)

diffcov = function(ccode="bdi")
{
  old = oldres(ccode)
  new = newres(ccode)
  any(old$WUENIC != new$WUENIC)
}

diffall = function(path="xsb")
{
  l = list.files(path=path, pattern="^...\\.pl$")
  l = gsub("\\.pl$", "", l) # remove extension
  sapply(l, diffcov)
}

diffall()
