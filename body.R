library(memoise)
db <- cache_filesystem("~/projects/gdigest/.rcache")
system('git add .rcache')

library(hbgd)

smc <- get_smocc_data()[1:2000,]

mem_wand <- memoise(function(...){
  on.exit({
    assign(digest::digest(list(...)),list(...),envir = sys.frame(-1))
  },add = TRUE)
  get_fit(...)
}, cache = db)

db$reset()

a <- memoise_wrapper(f=mem_wand,db=db,dat = smc, y_var = "haz")

synch_remote(action = 'push')
