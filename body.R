db_path <- "~/projects/gdigest/.rcache"
db <- memoise::cache_filesystem(db_path)
db$path <- db_path

smc <- hbgd::get_smocc_data()[1:2000,]

init_mem(f = hbgd::get_fit,cache = db,fname = 'mem_hgbd')

db$reset()

mem_hgbd(dat=smc, y_var = 'haz', method='brokenstick')

list.files(db_path)

memoise_wrapper(f=mem_hgbd,db=db, dat = smc, y_var = 'haz', method='brokenstick')

list.files(db_path)

unlink(db_path,recursive = TRUE,force = TRUE)

memofit(data=smc,db = db, f=mem_hgbd)

