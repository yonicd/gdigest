#' @title create/update a remote digest dictionary
#' @description maintain a dictionary of a digest on a remote repository
#' @param ... objects to digest and add to dictionary
#' @param repo character, user/repository, Default: NULL
#' @param local_storage character, subdirectory where digests are saved locally, Default: 'storage'
#' @param remote_storage character, subdirectory where digests are saved remotely, Default: 'storage'
#' @return character, named vector of the saved digests and the names of the objects they represent
#' @examples 
#' digest_dictionary(mtcars)
#' digest_dictionary(iris)
#' digest_dictionary(iris,iris3)
#' 
#' rm(list=ls(pattern = '^_'))
#' 
#' digest_dictionary(mtcars,iris,iris3)
#' 
#' @rdname digest_dictionary
#' @export 
#' @importFrom fastdigest fastdigest
#' @author Jonathan Sidi

digest_dictionary <- function(..., repo=NULL, local_storage='storage',remote_storage='storage'){
  dots <- list(...)
  param.names <- as.character(match.call())[-1]
  param.names <- param.names[1:length(dots)]
  names(dots) <- param.names
  list2env(dots,envir = environment())
  p.env <- sys.frame(-1)
  
  if(!is.null(repo)) synch_remote(repo=repo, local_storage=local_storage, remote_storage=remote_storage)
  
  dictionary <- ''
  
  if(!dir.exists(local_storage)) dir.create(local_storage)
  
  if(file.exists(file.path(local_storage,'dictionary.rda')))
    load(file.path(local_storage,'dictionary.rda'))
  
  dictionary_new <- sapply(param.names,function(x){
    this.digest <- fastdigest::fastdigest(eval(parse(text = x)))
    if(this.digest%in%dictionary){
      load(file.path(local_storage,sprintf('_%s.rda',this.digest))) 
    }else{
      assign(this.digest,eval(parse(text = x)))
      save(list=this.digest,file=file.path(local_storage,sprintf('_%s.rda',this.digest))) 
    }
    if(!sprintf('_%s',this.digest)%in%ls(envir = p.env))
      assign(sprintf('_%s',this.digest),eval(parse(text = x)),envir = p.env)  
    
    return(this.digest)
  })
  
  dictionary_new <- dictionary_new[!duplicated(dictionary_new)]
  
  if(all(!nzchar(dictionary))){
    
    dictionary <- dictionary_new
    
    save(dictionary,file=file.path(local_storage,'dictionary.rda'))
    cat(knitr::kable(dictionary,col.names = 'digest'),file=file.path(local_storage,'README.md'))
    
  }else{
    diff_dictionary <- dictionary_new[dictionary_new%in%setdiff(dictionary_new,dictionary)]
    
    dictionary <- c(dictionary,diff_dictionary)
    
    if(length(diff_dictionary)>0){
      save(dictionary,file=file.path(local_storage,'dictionary.rda'))
      cat(knitr::kable(dictionary,col.names = 'digest'),file=file.path(local_storage,'README.md'),sep='\n')
      }
  }
  if(!is.null(repo)) synch_remote(repo=repo, local_storage=local_storage, remote_storage=remote_storage)
  invisible(dictionary)
}