
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

prec_files = lapply(all_paths,load_file)
with_years = mapply(function(df,year){mutate(df,Year=year)},prec_files,all_years,SIMPLIFY=FALSE)
prec_data = rbindlist(with_years)
#table(prec_data$Year)

house_name = "State Assembly"
senate_name = "State Senate"

is_assembly = function(group_name,contest){
  substr(contest,1,nchar(group_name))==group_name
}
district_num = function(group_name,assembly_contest){
  intval = as.integer(trimws(substr(assembly_contest,nchar(assembly_contest)-1,100000000)))
}

leg_counties = c(
  "Clark",
  "Central",
  "Washoe",
  "Rural",
  "Capital"
)
match_counties = function(let3) leg_counties[which(substring(leg_counties,1,3)==let3)[[1]]]

house_prec_data = prec_data %>%
  filter(is_assembly(house_name,Contest)) %>%
  mutate(DIST_NUM = district_num(house_name,Contest))
senate_data_2004_2010 = prec_data %>%
  filter(is_assembly(senate_name,Contest),
         Year < 2012) %>%
  mutate(DIST_NAME = {
          name_3_letters = substring(Contest,nchar(senate_name)+3,nchar(senate_name)+5)
          toupper(Vectorize(match_counties)(name_3_letters))
        },
        DIST_NUM = {
          num_na = district_num(senate_name,Contest)
          # if district_num returns an NA, then it is the first district in the county
          ifelse(is.na(num_na),1,num_na)
        })

senate_data_2012 = prec_data %>%
  filter(is_assembly(senate_name,Contest),
         Year >= 2012) %>%
  mutate(DIST_NUM = district_num(senate_name,Contest))
  
get_num_map = function(num_data){
  num_map = num_data %>%
    select(DIST_NUM,Precinct) %>%
    group_by(Precinct) %>%
    summarize(DIST_NUMBER=DIST_NUM[1],
              is_consistent = all(DIST_NUM == DIST_NUMBER))
  # if it precincts do not consistently map to districts, something is very wrong
  # # find out what precincts are bad
  #tab = data.frame(table(num_map$is_consistent,num_map$Precinct)) %>%
  #  filter(Var1==FALSE,
  #         Freq!=0)
  #write(as.character(tab$Var2),"badprecincts3.txt")
  #stopifnot(all(num_map$is_consistent))
  num_map
}
house_map_2004_2010 = house_prec_data %>%
  filter(Year < 2012) %>%
  get_num_map()
house_map_2012 = house_prec_data %>%
  filter(Year >= 2012) %>%
  get_num_map()
senate_map_2012 = get_num_map(senate_data_2012)
senate_map_2004_2010 = {
  num_name_map = senate_data_2004_2010 %>%
  select(DIST_NUM,DIST_NAME,Precinct) %>%
  group_by(Precinct) %>%
  summarize(DIST_NUMBER=DIST_NUM[1],
            DISTRICT_NAME=DIST_NAME[1],
            is_consistent = all(DIST_NUM == DIST_NUMBER & DISTRICT_NAME == DIST_NAME))
  # if it precincts do not consistently map to districts, something is very wrong
  #stopifnot(all(num_name_map$is_consistent))
  num_name_map
}

rep_list = c(
  "GEORGE W. BUSH",
  "McCain, John",
  "Romney, Mitt",
  "TRUMP, DONALD J."
)
dem_list = c(
  "JOHN F. KERRY",
  "Obama, Barack",
  "CLINTON, HILLARY"
)

pres_data = prec_data %>%
  filter(Contest == "President and Vice President of the United States") %>%
  mutate(Party=ifelse(Selection %in% rep_list,"REP",
                      ifelse(Selection %in% dem_list , "DEM","OTHER")))

pres_data_2004_2010 = filter(pres_data,Year < 2012)
pres_data_2012 = filter(pres_data,Year >= 2012)

pres_summary_house_2004_2010 = pres_data_2004_2010 %>%
  left_join(house_map_2004_2010,by="Precinct") %>%
  group_by(DIST_NUMBER,Selection) %>%
  summarise(vote_count = sum(Votes)) %>%
  
  left_join(house_map,by="Precinct") %>%
  left_join(senate_map,by="Precinct") %>%
  rename(house_district = district_number.x,
         senate_district = district_number.y)

table(pres_data$senate_district,useNA="ifany")
table(pres_data$house_district,useNA="ifany")
table(senate_map$district_number,useNA="ifany")

##############################################
# Road data
##############################################

road_filenames = paste(folder,"Nevada Precinct Results ",(1984:1990)[c(TRUE, FALSE)],".tsv",sep="")

load_road = function(filename){
  read_tsv(filename)
}

road_files = lapply(road_filenames,load_road)
