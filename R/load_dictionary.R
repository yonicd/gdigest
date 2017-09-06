#' @title Load the digest dictionary to the global environment
#' @description Load the digest dictionary to the global environment
#' @param storage character, path to dictionary file, Default: 'storage'
#' @return NULL
#' @examples 
#' load_dictionary()
#' dictionary
#' @rdname load_dictionary
#' @export 
#' @author Jonathan Sidi

load_dictionary <- function(storage='storage'){
  load(file.path(storage,'dictionary.rda'))
  assign('dictionary',value = dictionary,envir = sys.frame(-1))
}