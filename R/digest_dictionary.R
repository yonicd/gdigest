#' @title create/update a remote digest dictionary
#' @description maintain a dictionary of a digest on a remote repository
#' @param ... objects to digest and add to dictionary
#' @param storage character, directory to store the digests, Default: 'storage'
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

digest_dictionary <- function(..., storage='storage'){
  dots <- list(...)
  param.names <- as.character(match.call())[-1]
  names(dots) <- param.names
  list2env(dots,envir = environment())
  p.env <- sys.frame(-1)
  
  dictionary <- ''
  
  if(!dir.exists(storage)) dir.create(storage)
  
  if(file.exists(file.path(storage,'dictionary.rda')))
    load(file.path(storage,'dictionary.rda'))
  
  dictionary_new <- sapply(param.names,function(x){
    this.digest <- fastdigest::fastdigest(eval(parse(text = x)))
    if(this.digest%in%dictionary){
      load(file.path(storage,sprintf('_%s.rda',this.digest))) 
    }else{
      assign(this.digest,eval(parse(text = x)))
      save(list=this.digest,file=file.path(storage,sprintf('_%s.rda',this.digest))) 
    }
    if(!sprintf('_%s',this.digest)%in%ls(envir = p.env))
      assign(sprintf('_%s',this.digest),eval(parse(text = x)),envir = p.env)  
    
    return(this.digest)
  })
  
  dictionary_new <- dictionary_new[!duplicated(dictionary_new)]
  
  if(all(!nzchar(dictionary))){
    
    dictionary <- dictionary_new
    
    save(dictionary,file=file.path(storage,'dictionary.rda'))
    
  }else{
    diff_dictionary <- dictionary_new[dictionary_new%in%setdiff(dictionary_new,dictionary)]
    
    dictionary <- c(dictionary,diff_dictionary)
    
    if(length(diff_dictionary)>0)
      save(dictionary,file=file.path(storage,'dictionary.rda'))
  }
  
  invisible(dictionary)
}