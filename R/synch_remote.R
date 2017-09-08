#' @title Retrieve current files in remote storage to local directory
#' @description FUNCTION_DESCRIPTION
#' @param file character, vector of files to retrieve from the repository, 
#' Default: c('dictionary.rda','README.md')
#' @param action character, push, pull or c('push','pull'), Default: 'pull'
#' @param repo character, user/repository
#' @param local_storage character, subdirectory where digests are saved locally, Default: '.rcache'
#' @param remote_storage character, subdirectory where digests are saved remotely, Default: '.rcache'
#' @param ... parameters to pass to \code{\link[vcs]{ls_remote}}
#' @return nothing
#' @seealso 
#'  \code{\link[vcs]{ls_remote}}
#' @rdname synch_remote
#' @export 
#' @author Jonathan Sidi
#' @importFrom vcs ls_remote
synch_remote <- function(file=c('dictionary.rda','README.md'),action='pull',repo=NULL, local_storage='.rcache', remote_storage='.rcache', ...){
  
  if(is.null(repo)){
    remote_v <- system('git remote -v',intern=TRUE)
    if(length(remote_v)==0) stop('No repository supplied and current working directory not a remote repo')
    repo=gsub('\\.(.*?)$','',gsub('^(.*?):','',remote_v[1])) 
  }
  
  repo_current <- basename(vcs::ls_remote(repo, subdir = remote_storage, ...))
  
  if('pull'%in%action){
    new_from_remote <- setdiff(repo_current, list.files(local_storage))
    
    if( length(new_from_remote)>0 ){
      system('git fetch')
      system(sprintf('git checkout %s',paste0(file.path(remote_storage,file),collapse=' ')))
    }
  }
  
  new_from_local <- setdiff(list.files(local_storage),repo_current)

  if('push'%in%action){
  
    if(!file.exists('.gitignore')) 
        file.create('.gitignore')
    
    gi <- readLines('.gitignore')
    gi <- gi[-grep('storage/(README|dictionary)',gi)]
    
    cat(gi,file = '.gitignore',append = FALSE,sep='\n')
    
    
  #load_dictionary()
  #cat(knitr::kable(dictionary,col.names = 'digest'),file=file.path(local_storage,'README.md'),sep='\n')
  
    if( length(new_from_local)>0 ){
      system(sprintf("git add %s",remote_storage))
      system(sprintf("git commit -m 'add files %s' -- %s",paste0(new_from_local,collapse = ','),remote_storage))
      system('git push origin master')
    }
  
  repo_current <- vcs::ls_remote(repo, subdir = remote_storage, ...)
  
  cat(unique(c(readLines('.gitignore'),repo_current)),file = '.gitignore',append = TRUE,sep='\n')
  
    
  }

}
