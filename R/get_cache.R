#' @title return the digest associated with a memoised call
#' @description return the digest associated with a memoised call
#' @param f memoised function
#' @param ... parameters passed to f
#' @return character
#' @rdname get_cache
#' @export 
#' @author Jonathan Sidi

get_cache <- function(f, ...) {
  if(!memoise::is.memoised(f)) stop("`f` is not a memoised function!", call. = FALSE)
  body <- body(f)
  body[[9]] <- quote(if (`_cache`$has_key(hash)) return(hash) else return(FALSE))
  body(f) <- body
  
  f
}
