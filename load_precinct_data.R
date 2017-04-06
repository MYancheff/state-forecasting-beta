
library(tidyverse)
library(data.table)


file_end = "(CSV Format).csv"
statewide_filenames = paste((2004:2012)[c(TRUE, FALSE)],"Statewide General Election",file_end)
gen_filenames = paste(c(2014,2016),"General Election Results",file_end)
all_filenames = c(statewide_filenames,gen_filenames)
all_paths = paste("data/",all_filenames,sep="")

load_file = function(filename){
  read_csv(filename,skip=3,na=c("","NA","*"))
}

files = lapply(all_paths,load_file)
data = rbindlist(files)
