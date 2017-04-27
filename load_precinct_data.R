
library(tidyverse)
library(data.table)

##################################################################
# Load precinct data
##################################################################

folder = "state-legislative-data/Precinct Level Election Results/"

first_years = (2004:2012)[c(TRUE, FALSE)]
later_years = c(2014,2016)
all_years = c(first_years,later_years)

file_end = "(CSV Format).csv"
# get a list of all the filenames of the files
statewide_filenames = paste(first_years,"Statewide General Election",file_end)
gen_filenames = paste(later_years,"General Election Results",file_end)
all_filenames = c(statewide_filenames,gen_filenames)
all_paths = paste(folder,all_filenames,sep="")

load_file = function(filename){
  read_csv(filename,skip=3,na=c("","NA","*"))
}

# get a dataframe of all the 
prec_files = lapply(all_paths,load_file)
with_years = mapply(function(df,year){mutate(df,Year=year)},prec_files,all_years,SIMPLIFY=FALSE)
prec_data = rbindlist(with_years) %>%
  mutate(Votes = ifelse(is.na(Votes),0,Votes)) %>%
  unite(UniquePrecinct,Precinct,Jurisdiction,Year,sep=".",remove=FALSE)

house_name = "State Assembly"
senate_name = "State Senate"

is_assembly = function(group_name,contest){
  substr(contest,1,nchar(group_name))==group_name
}
district_num = function(group_name,assembly_contest){
  num_str = substr(assembly_contest,nchar(assembly_contest)-1,100000000)
  trimed_str = trimws(num_str)
  as.integer(trimed_str)
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
  mutate(DIST_ID = as.character(district_num(house_name,Contest)),
         ElectionType="HOUSE")

all_senate_data = prec_data %>%
  filter(is_assembly(senate_name,Contest)) %>%
  mutate(ElectionType="SENATE")

senate_data_2004_2010 = all_senate_data %>%
  filter(Year < 2012) %>%
  mutate(DIST_ID = {
          name_3_letters = substring(Contest,nchar(senate_name)+3,nchar(senate_name)+5)
          dist_name = toupper(Vectorize(match_counties)(name_3_letters))
          
          num_na = district_num(senate_name,Contest)
          # if district_num returns an NA, then it is the first district in the county
          dist_num = ifelse(is.na(num_na),1,num_na)
          paste(dist_name,dist_num,sep="")
        })

senate_data_2012 = all_senate_data %>%
  filter(Year >= 2012) %>%
  mutate(DIST_ID = as.character(district_num(senate_name,Contest)))
  

district_data = rbind(house_prec_data,senate_data_2004_2010,senate_data_2012)

district_map = district_data %>%
  group_by(ElectionType,UniquePrecinct) %>%
  summarize(DISTRICT_ID=DIST_ID[1],
            is_consistent = all(DISTRICT_ID == DIST_ID)) %>%
  ungroup()
# if it precincts do not consistently map to districts, something is very wrong
# # find out what precincts are bad
#tab = data.frame(table(num_map$is_consistent,num_map$UniquePrecinct)) %>%
#  filter(Var1==FALSE,
#         Freq!=0)
#write(as.character(tab$Var2),"badprecincts3.txt")
stopifnot(sum(!district_map$is_consistent) < 12)

rep_list = c(
  "GEORGE W. BUSH",
  "McCain, John",
  "Romney, Mitt",
  "TRUMP, DONALD J.",
  
  #govenor names
  "GIBBONS, JIM",
  "Sandoval, Brian"
)
dem_list = c(
  "JOHN F. KERRY",
  "Obama, Barack",
  "CLINTON, HILLARY",
  
  #govenor names
  "TITUS, DINA",
  "Reid, Rory"
)
get_pres_gov_party = function(name){
  ifelse(name %in% rep_list,"REP",
         ifelse(name %in% dem_list , "DEM","OTHER"))
}

pres_contest_name = "President and Vice President of the United States"
gov_contest_name = "Governor"

#precincts_no_leg = prec_data %>%
#  filter(Contest == pres_contest_name)  %>%
#  left_join(district_map,by="UniquePrecinct")

pres_summary = prec_data %>%
  mutate(Race=ifelse(Contest == pres_contest_name,
                     "PRESIDENT",
                     ifelse(Contest == gov_contest_name,
                       "GOVENOR",NA))) %>%
  filter(!is.na(Race))  %>%
  left_join(district_map,by="UniquePrecinct") %>%
  group_by(Race,Year,ElectionType,DISTRICT_ID,Selection) %>%
  summarise(vote_count = sum(Votes),
            Party = get_pres_party(Selection[1])) %>%
  summarise(vote_dem = sum(ifelse(Party=="DEM",vote_count,0)),
            percent_dem = vote_dem/sum(vote_count),
            total_vote = sum(vote_count)) %>%
  ungroup() %>%
  gather(key=value_type,value=vote_value,vote_dem:total_vote)%>%
  unite(RaceValue,Race,value_type) %>%
  spread(RaceValue,vote_value)

#table(pres_summary$SEP_DIST_ID)

#out_district_data = district_data %>%
#  group_by(Year,ElectionType,DISTRICT_ID,Selection) %>%
  
#filter(pres_summary,is.na(ElectionType))

#pres_total = pres_summary %>%
#  group_by(Year) %>%
#  summarise(percent_dem = sum(vote_dem)/sum(total_vote))

#ggplot(pres_summary,aes(x=Year,y=percent_dem)) +
#  facet_grid(~ElectionType) + 
#  geom_line(aes(col=SEP_DIST_ID)) + 
#  geom_line(data=pres_total,mapping=aes(x=Year,y=percent_dem))

##############################################
# Road data
##############################################

#road_filenames = paste(folder,"Nevada Precinct Results ",(1984:1990)[c(TRUE, FALSE)],".tsv",sep="")

#load_road = function(filename){
#  read_tsv(filename)
#}

#road_files = lapply(road_filenames,load_road)

##############################################
# 1992-2002 Data Carson City
##############################################

#Import Carson City Data
library(readxl)
carsoncity1996to2002 <- read_excel("~/Desktop/state-forecasting-beta/state-legislative-data/Precinct Level Election Results/carsoncity1996to2002.xls")
View(carsoncity1996to2002)

#Select only the relevant data and columns for the purpose of this project
carsoncity1996to2002 <- carsoncity1996to2002 %>%
  #filter out rowtype = cards (this is documentation of the voting machines)
  filter(rowtype != "cards") %>%
  #filter out the cumulative final reports across all precincts in each election year
  filter(grepl('precinct', precname)) %>%
  #filter in only relevant races (president, governor, assembly and senate districts)
  filter(grepl('senate|assembly|governor|president|congress', officename),
         !grepl('lieutenant', officename)) %>%
  select(year, precname, rowtype, officename, cand, votes)

#Convert variable `officename` from characters into factors
carsoncity1996to2002$officename <- as.factor(carsoncity1996to2002$officename)

#extract the precinct's state assembly & senate districts
library(stringi)
precinct_district_1992to2002_Carson <- carsoncity1996to2002 %>%
  filter(grepl('senate|assembly', officename)) %>%
  select(1:4) %>%
  distinct() %>%
  mutate(SENATE_OR_HOUSE = ifelse(grepl('assembly', officename), 8, no = 9)) %>%
  mutate(DIST_NAME = ifelse(grepl('capital', officename), "CAPITAL",
                            ifelse(grepl('western', officename), "WESTERN", 
                            "CARSON CITY"))) %>%
  mutate(DISTRICT_NUM = stri_sub(officename, -2)) %>%
  select(-3)

#Since the senate districts doesnt have a number associated to it: 
#The value that get extracted for DISTRICT_NUM is ct (last 2 characters of "district")
#There are only 1 senate district each associated with "CAPITAL" AND "WESTERN"
#Hence, I recode "ct" to number 1 to follow the format of Carl's Data
precinct_district_1992to2002_Carson$DISTRICT_NUM[precinct_district_1992to2002_Washoe$DISTRICT_NUM == "ct"] <- 1

write.csv(precinct_district_1992to2002_Carson, "precinct to district cheatsheet Carson City 1992-2002.csv")
