#' @title unpack a memoised function
#' @description unpack elements from the body of the main memoise function call
#' @param f memoised function
#' @param ... parameters passed to f
#' @return list of internal objects associated with the memoised function
#' @rdname unpack_cache
#' @export 
#' @author Jonathan Sidi

unpack_cache <- function(f, ...) {
  if(!is.memoised(f)) stop("`f` is not a memoised function!", call. = FALSE)
  body <- body(f)
  body <- body[1:8]
  body[[9]] <- quote(sapply(ls(envir = encl),function(x) eval(parse(text = sprintf('`%s`',x))),simplify = FALSE))
  body(f) <- body
  
  f
}
