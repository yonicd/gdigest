#' @title Retrieve current files in remote storage to local directory
#' @description FUNCTION_DESCRIPTION
#' @param repo character, user/repository
#' @param local_storage character, subdirectory where digests are saved locally, Default: 'storage'
#' @param remote_storage character, subdirectory where digests are saved remotely, Default: 'storage'
#' @param ... parameters to pass to \code{\link[vcs]{ls_remote}}
#' @return nothing
#' @seealso 
#'  \code{\link[vcs]{ls_remote}}
#' @rdname synch_remote
#' @export 
#' @author Jonathan Sidi
#' @importFrom vcs ls_remote
synch_remote <- function(repo, local_storage='storage', remote_storage='storage', ...){
  repo_current <- basename(vcs::ls_remote(repo, subdir = remote_storage, ...))
  
  new_from_remote <- setdiff(repo_current, list.files(local_storage))
  
  if(length(new_from_remote)>0){
    system('git fetch')
    system(sprintf('git checkout HEAD %s',remote_storage))
  }
  
  new_from_local <- setdiff(list.files(local_storage),repo_current)
  
  if(length(new_from_local)>0){
    system(sprintf("git add %s",remote_storage))
    system(sprintf("git commit -m 'add files %s' -- %s",paste0(new_from_local,collapse = ','),remote_storage))
    junk=system('git push origin master',intern = TRUE)
  }

}
