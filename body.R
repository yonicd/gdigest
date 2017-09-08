db <- memoise::cache_filesystem("~/projects/gdigest/.rcache")
smc <- hbgd::get_smocc_data()[1:2000,]

mem_hgbd <- memoise::memoise(function(...){
  on.exit({
    assign(digest::digest(list(...)),list(...),envir = sys.frame(-1))
  },add = TRUE)
  hbgd::get_fit(...)
}, cache = db)

memofit(data=smc,db = db, f=mem_hgbd)

