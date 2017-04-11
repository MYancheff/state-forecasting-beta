
library(tidyverse)
library(data.table)

folder = "state-legislative-data/Precinct Level Election Results/"

first_years = (2004:2012)[c(TRUE, FALSE)]
later_years = c(2014,2016)
all_years = c(first_years,later_years)

file_end = "(CSV Format).csv"
statewide_filenames = paste(first_years,"Statewide General Election",file_end)
gen_filenames = paste(later_years,"General Election Results",file_end)
all_filenames = c(statewide_filenames,gen_filenames)
all_paths = paste(folder,all_filenames,sep="")

load_file = function(filename){
  read_csv(filename,skip=3,na=c("","NA","*"))
}

files = lapply(all_paths,load_file)
with_years = mapply(function(df,year){mutate(df,Year=year)},files,all_years,SIMPLIFY=FALSE)
prec_data = rbindlist(with_years)
#table(prec_data$Year)

house_name = "State Assembly, District"
senate_name = "State Senate, District"

is_assembly = function(group_name,contest){
  substr(contest,1,nchar(group_name))==group_name
}
district_num = function(group_name,assembly_contest){
  as.integer(trimws(substr(assembly_contest,1+nchar(group_name),100000000)))
}
filter_group = function(group_name,pdata){
  pdata %>%
    filter(is_assembly(group_name,Contest)) %>%
    mutate(district_num = district_num(group_name,Contest))
}

house_data = filter_group(house_name,prec_data)
senate_data = filter_group(senate_name,prec_data)

#precint_map = function(district_data){
#  district_data %>%
#    precint_
#}
#house_map = precint_map(house_data)
#senate_map = precint_map(senate_data)

#table(prec_data$Contest)

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

table(prec_data$Contest)

##############################################
# Road data
##############################################

road_filenames = paste(folder,"Nevada Precinct Results ",(1984:1990)[c(TRUE, FALSE)],".tsv",sep="")

load_road = function(filename){
  read_tsv(filename)
}

road_files = lapply(road_filenames,load_road)
