results = function(fname, mask="%s.pl.v39.txt", path="xsb")
{
  if(!is.null(mask))
    fname = sprintf(mask, fname)

  if(!is.null(path))
    fname = file.path(path, fname)

  read.table(fname, sep='\t', header=TRUE, row.names=NULL)
}

oldres = function(ccode="and", path="out")
  results(ccode, mask="%s.pl.v39.txt", path=path)

newres = function(ccode="and", path="out")
  results(ccode, mask="%s.pl.v40.txt", path=path)

diffcov = function(ccode="che")
{
  old = oldres(ccode)
  new = newres(ccode)
  any(old$WUENIC != new$WUENIC)
}

diffall = function(path="out")
{
  l = list.files(path=path, pattern="^...\\.pl$")
  l = gsub("\\.pl$", "", l) # remove extension
  sapply(l, diffcov)
}

# ccode = "che"
# args = commandArgs(trailingOnly=TRUE)
# if(length(args))
#     ccode = tools::file_path_sans_ext(args[1])
# print(sprintf("%s: Difference %s", ccode, diffcov(ccode)))