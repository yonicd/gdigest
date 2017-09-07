#' @export
print.digest <- function(x,...){
  x <- data.frame(x,stringsAsFactors = FALSE)
  names(x) <- 'digest'
  print(x)
  invisible(x)
}
