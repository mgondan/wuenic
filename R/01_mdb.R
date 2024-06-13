#' Interface to mdb-tools
#' 
#' @param mdb
#' Path to MS Access database (extension mdb)
#'
#' @param tab
#' Which table should be loaded
#'
#' @return
#' Data frame with the requested table.
#'
#' @details
#' Under Windows, install mdb-tools from Cygwin and put it into the PATH
#'
mdb_get <- function(mdb, tab)
{
  f <- tempfile()
  system2(command="mdb-export", args=c(mdb, tab), stdout=f)
  Hmisc::csv.get(f, datetimevars="updated", dateformat="%m/%d/%y")
}
