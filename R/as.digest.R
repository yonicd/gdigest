#' @export
as.digest <- function(x,...){
  structure(x,class=unique(c('digest',class(x))))
}
