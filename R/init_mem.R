#' @title wrapper to initialize memoise
#' @description wrapper to initialize memoise in parent environment
#' @param f function, Function of which to create a memoised copy
#' @param cache function, Cache function, Default: cache_memory()
#' @param fname character, name of function to assign in environment, Default: 'mem_f'
#' @return memoised function
#' @seealso 
#'  \code{\link[digest]{digest}}
#'  \code{\link[memoise]{memoise}}
#' @rdname init_mem
#' @export 
#' @author Jonathan Sidi
#' @importFrom digest digest
#' @importFrom memoise memoise
init_mem <- function(f,cache=cache_memory(),fname='mem_f'){
  
  fout <- memoise::memoise(function(...){
    on.exit({
      assign(digest::digest(list(...)),list(...),envir = sys.frame(-3))
    },add = TRUE)
    f(...)
  }, cache = cache)

  assign(fname,value = fout,envir = sys.frame(-1))
  
}