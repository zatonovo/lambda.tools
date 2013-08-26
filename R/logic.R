# :vim set filetype=R

onlyif(f, x, TRUE) %as% f(x)
onlyif(f, x, FALSE) %as% x

#' Force values into bins
#' @param x The vector whose values should be binned
#' @param bins The available bins
#' @param attractor The method to attract values to the bins
quantize(x, bins=c(-1,0,1), metric=function(a,b) abs(a-b)) %as% {
  ds <- sapply(bins, function(b) metric(x,b))
  if (is.null(dim(ds))) ds <- t(ds)
  apply(ds,1, function(d) item(bins, which.min(d)))
}

.confine(x, min.level, max.level) %when% { x < min.level } %as% min.evel
.confine(x, min.level, max.level) %when% { x > max.level } %as% max.level
.confine(x, min.level, max.level) %as% x

confine(x, min.level=-1, max.level=1) %as%
  sapply(x, function(y) .confine(y,min.level,max.level))


use_default(NULL, default) %as% default
use_default(NA, default) %as% default
use_default(x, default) %as% x
