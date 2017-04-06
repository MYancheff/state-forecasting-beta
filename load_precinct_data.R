
library(tidyverse)
library(data.table)

folder = "state-legislative-data/Precinct Level Election Results/"

file_end = "(CSV Format).csv"
statewide_filenames = paste((2004:2012)[c(TRUE, FALSE)],"Statewide General Election",file_end)
gen_filenames = paste(c(2014,2016),"General Election Results",file_end)
all_filenames = c(statewide_filenames,gen_filenames)
all_paths = paste(folder,all_filenames,sep="")

load_file = function(filename){
  read_csv(filename,skip=3,na=c("","NA","*"))
}

files = lapply(all_paths,load_file)
prec_data = rbindlist(files)

party_association = list(
  c("GEORGE W. BUSH","REP"),
  c("JOHN F. KERRY","DEM"),
  c("McCain, John","REP"),
  c("Obama, Barack","DEM"),
  c("Romney, Mitt","REP"),
  c("CLINTON, HILLARY","DEM"),
  c("TRUMP, DONALD J.","REP")
)
transpose(party_association)

##############################################
# Road data
##############################################

road_filenames = paste(folder,"Nevada Precinct Results ",(1984:1990)[c(TRUE, FALSE)],".tsv",sep="")

load_road = function(filename){
  read_tsv(filename)
}

road_files = lapply(road_filenames,load_road)
