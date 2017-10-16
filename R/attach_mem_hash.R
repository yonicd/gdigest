#' @title join information of the memoised output with the input in the cache
#' @description join information of the memoised output with the input in the cache
#' @param x memoised output
#' @param db cache object
#' @param f memoised function
#' @param ... parameters passed to f
#' @return nothing
#' @details rds file written to db$path with the same name as the memoised output,
#' but with a leading `_`.
#' @rdname attach_mem_hash
#' @export 
#' @author Jonathan Sidi

attach_mem_hash <-function(x, db, f, ...){
  mem_hash<- get_cache(f)(...)
  temp <- get(x,envir = sys.frame(-1))
  #temp$fn <- body(unpack_cache(f)()$`_f`)[[3]]
  saveRDS(temp,file = file.path(db$path,sprintf('_%s',mem_hash)))
  rm(list=x,envir = sys.frame(-1))
}
