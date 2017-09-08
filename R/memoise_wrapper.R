#' @title wrapper for memoised function
#' @description wrapper of memoised function manage input storage
#' @param f memoised function
#' @param db cache object
#' @param keep.input boolean, store the input information of the call to db$path, Default: TRUE
#' @param ... parameters passed to f
#' @return memoised fucntion output
#' @rdname memoise_wrapper
#' @export 
#' @author Jonathan Sidi

memoise_wrapper <- function(f,db,keep.input=TRUE,...){
  ret <- f(...)
  find.hash <- ls()[which(sapply(ls(),nchar)==32)]
  if(keep.input&length(find.hash)>0) attach_mem_hash(x=find.hash,f=f,db=db,...)
  return(ret)
}