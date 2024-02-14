# Convenience function: Replace NULL by NA in lists
padNA = function(q, v)
{
  qq = q[v]
  names(qq) = v
  qq[] = ifelse(lapply(qq, is.null), NA, qq)
  return(qq)
}

