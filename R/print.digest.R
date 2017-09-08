#' @export
print.digest <- function(x,...){
  sapply(x,
         
         function(x){
           switch(class(x),
                  data.frame = dplyr::as.tbl(x),
                  x)
         } 
  )
}
