#' @title Load the digest dictionary to the global environment
#' @description Load the digest dictionary to the global environment
#' @param local_storage character, subdirectory where digests are saved locally, Default: 'storage'
#' @param remote_storage character, subdirectory where digests are saved remotely, Default: 'storage'
#' @return NULL
#' @examples 
#' load_dictionary()
#' dictionary
#' @rdname load_dictionary
#' @export 
#' @author Jonathan Sidi
load_dictionary <- function(local_storage='storage',remote_storage='storage'){
  synch_remote(local_storage = local_storage,remote_storage = remote_storage,action='pull')
  
  dictionary <- ''
  
  if(file.exists(file.path(local_storage,'dictionary.rda')))
    load(file.path(local_storage,'dictionary.rda'))
  
  dictionary <- as.digest(dictionary)
  
  assign('dictionary',value = dictionary,envir = sys.frame(-1))
}
