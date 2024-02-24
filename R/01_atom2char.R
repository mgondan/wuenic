# Prolog atoms to R character strings, variables to R character strings,
# list elements of type name:elem to named list elements
#
atom2char = function(q)
{
  if(is.expression(q))
    return(iconv(as.character(q), "latin1", "latin1"))
  
  if(is.symbol(q))
    return(iconv(as.character(q), "latin1", "latin1"))
  
  # Translate mmr/rubella to string
  if(is.call(q))
    if(as.character(q[[1]]) == "/")
      return(paste(collapse="/", as.character(q[-1])))
  
  if(is.call(q))
  { args <- as.list(q)
    args[-1] <- lapply(args[-1], FUN=atom2char)
    return(as.call(args))
  }
  
  if(is.list(q) && !is.null(names(q)))
    return(lapply(q, atom2char))
  
  if(is.list(q))
  { n = NULL
    for(i in 1:length(q))
    { item = as.list(q[[i]])
      q[[i]] = atom2char(item[[3]])
      n = c(n, atom2char(item[[2]]))
    }
    names(q) = n
    return(q)
  }
  
  return(q)
}
