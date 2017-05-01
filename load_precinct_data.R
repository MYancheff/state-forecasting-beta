
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

#MAIN DATA
#Wrangle data into a format that works the purpose of this project
carsoncity1996to2002_tidy <- carsoncity1996to2002 %>%
  #filter out rowtype = cards (this is documentation of the voting machines)
  filter(rowtype != "cards") %>%
  #filter out the cumulative final reports across all precincts in each election year
  filter(grepl('precinct', precname)) %>%
  #filter in only relevant races (president, governor, assembly and senate districts)
  filter(grepl('senate|assembly|governor|president|congress', officename),
         !grepl('lieutenant', officename)) %>%
  select(year, precname, rowtype, officename, cand, votes) %>%
  #mutate party-code for each candidate observations
  mutate(party_code = stri_sub(cand, -3)) %>%
  #rename the independent parties into OTHER
  mutate(PARTY_CODE = ifelse(grepl('dem', party_code), "DEM",
                             ifelse(grepl('rep', party_code), "REP", 
                                  "OTHER"))) %>%
  #select relevant collumns
  select(-cand, -party_code) %>%
  #sum up votes by PARTY_CODE
  group_by(year, precname, officename, PARTY_CODE) %>%
  summarize(VOTES = sum(votes))
  
#Convert variable `officename` from characters into factors
carsoncity1996to2002_tidy$officename <- as.factor(carsoncity1996to2002_tidy$officename)

#PRECINCT TO DISTRICT CHEATSHEET
#extract the precinct's state assembly & senate districts
library(stringi)
precinct_district_1992to2002_Carson <- carsoncity1996to2002_tidy %>%
  filter(grepl('senate|assembly', officename)) %>%
  select(1:4) %>%
  distinct() %>%
  mutate(SENATE_OR_HOUSE = ifelse(grepl('assembly', officename), 8, no = 9)) %>%
  mutate(DIST_NAME = ifelse(grepl('capital', officename), "CAPITAL",
                            ifelse(grepl('western', officename), "WESTERN", 
                            "CARSON CITY"))) %>%
  mutate(DISTRICT_NUM = stri_sub(officename, -2)) %>%
  select(-3, -4)

#Since the senate districts doesnt have a number associated to it: 
#The value that get extracted for DISTRICT_NUM is ct (last 2 characters of "district")
#There are only 1 senate district each associated with "CAPITAL" AND "WESTERN"
#Hence, I recode "ct" to number 1 to follow the format of Carl's Data
precinct_district_1992to2002_Carson$DISTRICT_NUM[precinct_district_1992to2002_Carson$DISTRICT_NUM == "ct"] <- 1

#Export CSV file of this precinct-to-district Cheatsheet for Carson City 
write.csv(precinct_district_1992to2002_Carson, "precinct to district cheatsheet Carson City 1992-2002.csv")

#Join precinct_to_district cheatsheet with the larger Carson City precinct data files
carsoncity1996to2002_tidy <- carsoncity1996to2002_tidy %>%
  left_join(precinct_district_1992to2002_Carson, by=c("year" = "year",
                                                      "precname" = "precname")) 

#Export CSV file of the Carson City precinct data 1996-2002 with legislative district tags 
write.csv(carsoncity1996to2002_tidy, "Carson City Precinct Level Results 1996-2002.csv")

##############################################
# 1992-2002 Data WASHOE COUNTY
##############################################

#Import the main Washoe precinct-level voting data from 1994-2002
library(readxl)
washoe1994to2002 <- read_excel("~/Desktop/state-forecasting-beta/state-legislative-data/Precinct Level Election Results/washoe1994to2002.xls")

  #Select the relevant collumns
washoe1994to2002$votetot <- as.integer(washoe1994to2002$votetot)
washoe1994to2002$repub <- as.integer(washoe1994to2002$repub)
washoe1994to2002$dem <- as.integer(washoe1994to2002$dem)

washoe1994to2002 <- washoe1994to2002 %>%
  select(year, 1:3, repub, dem, other)

#Import the precinct-to-district cheatsheet we created for Washoe County
Washoe_Precinct_to_District_Cheatsheet <- read_excel("~/Desktop/state-forecasting-beta/state-legislative-data/Precinct Level Election Results/Washoe Precinct to District Cheatsheet.xlsx")

  #Select relevant columns and filter out all the entry with DIST_NUM = 0
Washoe_Precinct_to_District_Cheatsheet <- Washoe_Precinct_to_District_Cheatsheet %>%
  select(-precnum1) %>%
  filter(DIST_NUM != 0)

#Join precinct_to_district cheatsheet with the larger Washoe County precinct data file
washoe1994to2002_tidy <- washoe1994to2002 %>%
  left_join(Washoe_Precinct_to_District_Cheatsheet, by=c("year" = "year",
                                                      "precname" = "precname", 
                                                      "precnum" = "precnum")) %>%
  filter(precname != "total") %>%
  rename(REP = repub,
         DEM = dem,
         OTHER = other)
    
  

washoe1994to2002_tidy$precname <- as.factor(washoe1994to2002_tidy$precname)    

#Export CSV file of the Washoe County precinct data 1994-2002 with legislative district tags 
write.csv(washoe1994to2002_tidy, "Washoe County Precinct Level Results 1994-2002.csv")


#Create an aggregate csv file of all available precinct-level data from Nevada, 1992-2002

  #Tidy Data (get all the data files into the same formats (same number of collumns))
    #Select relevant columns from Carson City Data
carsoncity1996to2002_tidy <- Carson_City_Precinct_Level_Results_1996_2002 %>% 
  select(-`X1`) %>%
    #spread PARTY_CODES to derive 3 separate collumns for REP, DEM, OTHER
  spread(key = `PARTY_CODE`, value = `VOTES`, fill = 0) %>%
    #mutate a new column capturing turnout
  mutate(Turnout = DEM + OTHER + REP) %>%
    #mutate a nonexistent precnum column
  mutate(precnum = NA) %>%
    #mutate a county column:
  mutate(county = "Carson_City")

    #mutate a county column for Clark County Data
NVclarkcounty1992to2002_tidy <- NV_Clark_County_Precinct_Level_Results_1992_2002 %>%
  mutate(county = "clark") %>%
    #mutate a DIST_NAME column: 
  mutate(DIST_NAME = "CLARK") %>%
    #Mutate a column capturing the type of contests/races
 mutate(officename = ifelse(grepl('1992|1996|2000', year), "president", "governor")) %>%
    #Select the relevants columns
  select(-rowtype, -RV, -Percent)

    #Add up voter turnout in Washoe County Data
washoe1994to2002_tidy$OTHER[is.na(washoe1994to2002_tidy$OTHER)] <- 0
washoe1994to2002_tidy <- washoe1994to2002_tidy %>%
  mutate(Turnout = REP + DEM + OTHER) %>%
    #Mutate a column capturing the type of contests/races
  mutate(officename = ifelse(grepl('1992|1996|2000', year), "president", "governor")) %>%
    #Reformat so that all the columns are in the same order
  rename(DISTRICT_NUM = DIST_NUM)

#Bind all 3 files together
precinct_data_1992to2002 <- rbind(carsoncity1996to2002_tidy, washoe1994to2002_tidy, NVclarkcounty1992to2002_tidy)
  
#Gather DEM, REP and OTHER into PARTY_CODE and VOTE  
precinct_data_1992to2002 <- precinct_data_1992to2002 %>% 
  gather(key = PARTY_CODE, value = VOTES, `DEM`, `REP`, `OTHER`) %>%
  #Rearrange column in a format that would makes the most sense to a reader
  select(year, county, precname, precnum, SENATE_OR_HOUSE, 
         DIST_NAME, DISTRICT_NUM, officename, Turnout, PARTY_CODE, VOTES)
  

#Export CSV file of the Washoe County precinct data 1994-2002 with legislative district tags 
write.csv(precinct_data_1992to2002, "Precinct Level Results 1992-2002 Washoe, Clark County & Carson City.csv")
  
##############################################
# Random codes
##############################################
carsoncity1996to2002_tidy$PARTY_CODE <- as.factor(carsoncity1996to2002_tidy$PARTY_CODE)
carsoncity1996to2002_tidy$VOTES <- as.integer(carsoncity1996to2002_tidy$VOTES)

gather(key = PARTY_CODE, value = VOTES, `DEM`, `REP`, `OTHER`)    