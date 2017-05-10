library(tidyverse)
library(data.table)
library(readxl)
library(dplyr)
library(stringi)
library(haven)

##################################################################
# Load precinct data 2004-2016
##################################################################

folder = "state-legislative-data/Precinct Level Election Results/"

first_years = (2004:2012)[c(TRUE, FALSE)]
later_years = c(2014, 2016)
all_years = c(first_years,later_years)

file_end = "(CSV Format).csv"
# get a list of all the filenames of the files
statewide_filenames = paste(first_years,"Statewide General Election",file_end)
gen_filenames = paste(later_years,"General Election Results",file_end)
all_filenames = c(statewide_filenames,gen_filenames)
all_paths = paste(folder,all_filenames,sep="")

load_file = function(filename){
  read_csv(filename,na=c("","NA","*"))
}

rep_list = c(
  "GEORGE W. BUSH",
  "McCain, John",
  "Romney, Mitt",
  "TRUMP, DONALD J.",
  
  #govenor candidate names
  "GIBBONS, JIM",
  "Sandoval, Brian",
  
  #Congressional Representative candidate names
  "GIBBONS, JIM",
  "HELLER, DEAN",
  "AMODEI, MARK E",
  "Porter, Jon",
  "Heck, Joe",
  "Perry, Mary", 
  "Teijeiro, Annette",
  "Edwards, Chris",
  "Wegner, Kenneth A.",
  "Russ Mickelson",
  "Danny Tarkanian"
)

dem_list = c(
  "JOHN F. KERRY",
  "Obama, Barack",
  "CLINTON, HILLARY",
  
  #govenor names
  "TITUS, DINA",
  "Reid, Rory",

  #Congressional Representatives names
  "Berkley, Shelley",
  "Dina Titus",
  "ROSEN, DANIEL",
  "Rosen, Jacky",
  "Bilbray, Erin",
  "Hafen, Tessa M.",
  "Oceguera, John",
  "ANGIE G. COCHRAN",
  "DERBY, JILL",
  "Price, Nancy",
  "KOEPNICK, SAMUEL",
  "Spees, Kristen",
  "EVANS, H.D. 'CHIP'",
  "Horsford, Steven A.",
  "KIHUEN, RUBEN"
)

get_pres_gov_party = function(name){
  ifelse(name %in% rep_list,"REP",
         ifelse(name %in% dem_list, "DEM","OTHER"))
}

# put all precinct level csv files for Nevada 2004-2016 into 1 dataframe 
prec_files = lapply(all_paths,load_file)
with_years = mapply(function(df,year){mutate(df,Year=year)},prec_files,all_years,SIMPLIFY=FALSE)
all_prec_data = rbindlist(with_years)  %>%
  mutate(Votes = ifelse(is.na(Votes),0,Votes)) %>%
  mutate(Year = as.integer(Year))
  
#precint to district mapping
cheatsheet2004_2010 <- all_prec_data  %>%
         #DistrictingSection = ifelse(Year %in% c(2012,2014,2016),2012,2004)) %>%
  #group_by(DistrictingSection) %>%
  filter(grepl('Senate|Assembly', Contest),
         Year %in% c(2004, 2006,2008,2010)) %>%
  mutate(DistrictingSection = ifelse(Year %in% c(2004, 2006),1,2)) %>%
  select(DistrictingSection,Jurisdiction, Precinct, Contest) %>%
  distinct() %>%
  mutate(SENATE_OR_HOUSE = ifelse(grepl('Assembly', Contest), 8, 9)) %>%
  mutate(DIST_NAME = ifelse(grepl('Washoe', Contest), "WASHOE",
                            ifelse(grepl('Clark', Contest), "CLARK",
                                   ifelse(grepl('Central', Contest), "CENTRAL",
                                          ifelse(grepl('Rural', Contest), "RURAL",
                                                 ifelse(grepl('Capital', Contest), "CAPITAL",
                                                        NA)))))) %>%
  mutate(DISTRICT_NUM = stri_sub(Contest, -2)) %>%
  #If DISTRICT_NUM = ct, then there is one only 1 district associated to DIST_NAME
  mutate(DISTRICT_NUM = ifelse(DISTRICT_NUM == "ct", 1, DISTRICT_NUM),
         DISTRICT_NUM = as.integer(DISTRICT_NUM)) %>%
  select(-Contest) 
    
cheatsheet2012_2016 <- all_prec_data %>%
  filter(Year %in% c(2012, 2014),
         grepl('Senate|Assembly', Contest)) %>%
  select(Jurisdiction, Precinct, Contest) %>%
  distinct() %>%
  mutate(SENATE_OR_HOUSE = ifelse(grepl('Assembly', Contest), 8, 9)) %>%
  mutate(DIST_NAME = NA, 
         DISTRICT_NUM = stri_sub(Contest, -2),
         DISTRICT_NUM = as.integer(DISTRICT_NUM)) %>%
  mutate(DistrictingSection = 3) %>%
  select(-Contest)

full_cheatsheet = rbind(cheatsheet2004_2010,cheatsheet2012_2016)

contest_names = c("Governor","President and Vice President of the United States")
officenames = c("governor","president")

officename_map = data.frame(contestname = contest_names,officename = officenames)

prec_data_election = all_prec_data %>%
  #filter in only relevant races (president, governor, assembly and senate districts)
  filter(grepl('Governor|President|Congress|United States Senator', Contest),
        !grepl('Lieutenant', Contest)) %>%
  mutate(PARTY_CODE = get_pres_gov_party(Selection)) %>%
  mutate(DistrictingSection = ifelse(Year %in% c(2004, 2006),1,
                                     ifelse(Year %in% c(2008, 2010),2, 3))) %>%
  left_join(full_cheatsheet, by=c("DistrictingSection" = "DistrictingSection","Precinct" = "Precinct","Jurisdiction" = "Jurisdiction")) %>%
  group_by(Year, Jurisdiction, Precinct, Contest) %>%
  mutate(Turnout =  sum(Votes)) %>%
  ungroup() %>%
  left_join(officename_map,by=c("Contest"="contestname")) %>%
  select(-Contest)

##############################################
# Road data
##############################################
#Import FIPS County_Code Data
FIPS_County_Code <- read_excel("state-legislative-data/Precinct Level Election Results/FIPS County Code.xlsx")

#Import and Wrangle the ROAD Data
filename = paste(folder,"temp_NV_only20170503.dta",sep="")
#install.packages("haven")
road_data = read_dta(filename) %>%
  gather(key=vote_type,value=VOTES,-(year:plc)) %>%
  mutate(VOTES=as.integer(VOTES)) %>%
  filter(!is.na(VOTES)) %>%
  mutate(
    #Recode vote_type into new variable `officename`, which indicates types of election
    officename=ifelse(grepl("gyyg_",vote_type),"governor",
                      ifelse(grepl("gyyp_",vote_type),"president",
                             NA)),# add more ifelse(grepl(...)...) here to add more elections
    PARTY_CODE=ifelse(grepl("_dv",vote_type),"DEM",
                      ifelse(grepl("_rv",vote_type),"REP",
                             ifelse(grepl("_o[1-9]v",vote_type),"OTHER",
                                    NA))),
    county_code = as.numeric(countyfips)) %>%
  filter(!is.na(officename),
         !is.na(PARTY_CODE)) %>%
  rename(senate_district_num=sd,
         house_num=hd,
         house_nname=hds,
         precinct=pname) %>%
  filter(house_num != 0 & senate_district_num != 0) %>%
  left_join(FIPS_County_Code, by=c("county_code" = "county_code")) %>%
  mutate(house_num=as.integer(house_num),
         senate_district_num=as.integer(senate_district_num)-200) %>%
  mutate(DIST_NAME = ifelse(senate_district_num %in% c(1:7), "CLARK",
                            ifelse(senate_district_num %in% c(8:10), "WASHOE", 
                                   ifelse(senate_district_num == 11, "CAPITOL",
                                          ifelse(senate_district_num == 12, "WESTERN",
                                                 ifelse(senate_district_num == 13, "CENTRAL",
                                                        "NORTHERN")))))) %>%
  mutate(DISTRICT_NUM = ifelse(senate_district_num == 10, 3,
                               ifelse(senate_district_num %in% c(11:14), 1, 
                                      ifelse(senate_district_num == 8, 1,
                                             ifelse(senate_district_num == 9, 2,
                                                    senate_district_num))))) %>%
  gather(key = SENATE_OR_HOUSE, value = DIST_NUM_ROAD, senate_district_num, house_num) %>%
  mutate(SENATE_OR_HOUSE = ifelse(SENATE_OR_HOUSE == "senate_district_num", 9, 8),
         DIST_NAME = ifelse(SENATE_OR_HOUSE == 9, DIST_NAME, NA), 
         DISTRICT_NUM = ifelse(SENATE_OR_HOUSE == 8, DIST_NUM_ROAD, DISTRICT_NUM)) %>%
  select(year, county, precinct, SENATE_OR_HOUSE, DIST_NAME, DISTRICT_NUM, officename, PARTY_CODE, VOTES)

##############################################
# 1992-2002 Data Clark County
##############################################
#Import the 1992-2002 Clark County Data, converted into machine-readable format
nvclark1992to2002 <- read_excel("state-legislative-data/Precinct Level Election Results/nvclark1992to2002.xls")

#Import precinct-to-district cheatsheets
  #1992-1994
CLARK_COUNTY_1992_1994_ASSEMBLY_DISTRICT <- read_excel("state-legislative-data/Precinct Level Election Results/CLARK COUNTY 1992-1994 ASSEMBLY DISTRICT.xlsx")
CLARK_COUNTY_1992_1994_SENATE_DISTRICT <- read_excel("state-legislative-data/Precinct Level Election Results/CLARK COUNTY 1992-1994 SENATE DISTRICT.xlsx")

cheatsheet1992_1994CLARK <- CLARK_COUNTY_1992_1994_ASSEMBLY_DISTRICT %>%
  left_join(CLARK_COUNTY_1992_1994_SENATE_DISTRICT, by=c("PRECINCT" = "PRECINCT")) %>%
  gather(key = SENATE_OR_HOUSE, value = DISTRICT_NUM, `8`, `9`) %>%
  mutate(precname = substring(PRECINCT, 1, 3), 
         precnum = stri_sub(PRECINCT, -3)) %>%
  mutate(precnum = as.integer(precnum))
  select(-PRECINCT)

  #1996-2000
CLARK_COUNTY_1996_2000_ASSEMBLY_DISTRICT <- read_excel("state-legislative-data/Precinct Level Election Results/CLARK COUNTY 1996-2000 ASSEMBLY DISTRICT.xlsx")  
CLARK_COUNTY_1996_2000_SENATE_DISTRICT <- read_excel("state-legislative-data/Precinct Level Election Results/CLARK COUNTY 1996-2000 SENATE DISTRICT.xlsx") 

cheatsheet1996_2000CLARK <- CLARK_COUNTY_1996_2000_ASSEMBLY_DISTRICT %>%
  left_join(CLARK_COUNTY_1996_2000_SENATE_DISTRICT, by=c("PRECINCT" = "PRECINCT")) %>%
  gather(key = SENATE_OR_HOUSE, value = DISTRICT_NUM, `8`, `9`)
  
  #2002
CLARK_COUNTY_2002_ASSEMBLY_DISTRICT <- read_excel("state-legislative-data/Precinct Level Election Results/CLARK COUNTY 2002 ASSEMBLY DISTRICT.xlsx")
CLARK_COUNTY_2002_SENATE_DISTRICT <- read_excel("state-legislative-data/Precinct Level Election Results/CLARK COUNTY 2002 SENATE DISTRICT.xlsx")
CLARK_COUNTY_2002_SENATE_DISTRICT <- CLARK_COUNTY_2002_SENATE_DISTRICT %>%
  mutate(PRECINCT = substring(PRECINCT, 1, 4)) %>%
  mutate(PRECINCT=as.integer(PRECINCT))

cheatsheet2002CLARK <- CLARK_COUNTY_2002_ASSEMBLY_DISTRICT %>%
  left_join(CLARK_COUNTY_2002_SENATE_DISTRICT, by=c("PRECINCT" = "PRECINCT")) %>%
  gather(key = SENATE_OR_HOUSE, value = DISTRICT_NUM, `8`, `9`)

#Isolate and Wrangle Data for 1992
nvclark1992_tidy <- nvclark1992to2002 %>%  
  filter(rowtype == "precinct", 
         year == 1992) %>%
  #v3 to v8 are independent parties in presidential race. Sum their votes into OTHER
  mutate(OTHER = v3+v4+v5+v6+v7+v8) %>%
  #Gather v1, v2 and OTHER into PARTY_CODE indicating the candidates' parties
  gather(key = PARTY_CODE, value = VOTES, v1, v2, OTHER) %>%
  #Rename v1 to REP and v2 to DEM
  mutate(PARTY_CODE = ifelse(PARTY_CODE == "v1", "REP",
                             ifelse(PARTY_CODE == "v2", "DEM",
                                    "OTHER"))) %>%
  #attach the corresponding state legislative district pags to each precinct
  left_join(cheatsheet1992_1994CLARK, by=c("precname" = "precname", "precnum" = "precnum")) %>%
  select(year, county, precname, precnum, SENATE_OR_HOUSE, DISTRICT_NUM, 
         Turnout, PARTY_CODE, VOTES, privacy)

#Isolate and Wrangle Data for 1994
nvclark1994_tidy <- nvclark1992to2002 %>%  
  filter(rowtype == "precinct",
         year == 1994) %>%
  #v2, v4 & v5 are independent parties in gubertorial race. Sum their votes into OTHER
  mutate(OTHER = v2+v4+v5) %>%
  #Gather v1, v3 and OTHER into PARTY_CODE indicating the candidates' parties
  gather(key = PARTY_CODE, value = VOTES, v1, v3, OTHER) %>%
  #Rename v1 to REP and v3 to DEM
  mutate(PARTY_CODE = ifelse(PARTY_CODE == "v1", "REP",
                             ifelse(PARTY_CODE == "v3", "DEM",
                                    "OTHER"))) %>%
  #attach the corresponding state legislative district pags to each precinct
  left_join(cheatsheet1992_1994CLARK, by=c("precname" = "precname", "precnum" = "precnum")) %>%
  select(year, county, precname, precnum, SENATE_OR_HOUSE, DISTRICT_NUM, 
         Turnout, PARTY_CODE, VOTES, privacy)
  
#Isolate and Wrangle Data for 1996
nvclark1996_tidy <- nvclark1992to2002 %>%  
  filter(rowtype == "precinct",
         year == 1996) %>%
  #v1, v4 to v8 are independent parties in presidential race. Sum their votes into OTHER
  mutate(OTHER = v1+v4+v5+v6+v7+v8) %>%
  #Gather v1, v2 and OTHER into PARTY_CODE indicating the candidates' parties
  gather(key = PARTY_CODE, value = VOTES, v2, v3, OTHER) %>%
  #Rename v3 to REP and v2 to DEM
  mutate(PARTY_CODE = ifelse(PARTY_CODE == "v3", "REP",
                             ifelse(PARTY_CODE == "v2", "DEM",
                                    "OTHER"))) %>%
  #attach the corresponding state legislative district pags to each precinct
  left_join(cheatsheet1996_2000CLARK, by=c("precnum" = "PRECINCT")) %>%
  select(year, county, precname, precnum, SENATE_OR_HOUSE, DISTRICT_NUM, 
         Turnout, PARTY_CODE, VOTES, privacy)

#Isolate and Wrangle Data for 1998
nvclark1998_tidy <- nvclark1992to2002 %>%  
  filter(rowtype == "precinct",
         year == 1998) %>%
  #v2, v4 & v5 are independent parties in gubertorial race. Sum their votes into OTHER
  mutate(OTHER = v2+v4+v5) %>%
  #Gather v1, v3 and OTHER into PARTY_CODE indicating the candidates' parties
  gather(key = PARTY_CODE, value = VOTES, v1, v3, OTHER) %>%
  #Rename v1 to REP and v3 to DEM
  mutate(PARTY_CODE = ifelse(PARTY_CODE == "v1", "REP",
                             ifelse(PARTY_CODE == "v3", "DEM",
                                    "OTHER"))) %>%
  left_join(cheatsheet1996_2000CLARK, by=c("precnum" = "PRECINCT")) %>%
  select(year, county, precname, precnum, SENATE_OR_HOUSE, DISTRICT_NUM, 
         Turnout, PARTY_CODE, VOTES, privacy)

#Isolate and Wrangle Data for 2000
nvclark2000_tidy <- nvclark1992to2002 %>%  
  filter(rowtype == "precinct",
         year == 2000) %>%
  #v1, v2 & v5:v8 are independent parties in presidential race. Sum their votes into OTHER
  mutate(OTHER = v1+v2+v5+v6+v7+v8) %>%
  #Gather v3, v4 and OTHER into PARTY_CODE indicating the candidates' parties
  gather(key = PARTY_CODE, value = VOTES, v3, v4, OTHER) %>%
  #Rename v1 to REP and v2 to DEM
  mutate(PARTY_CODE = ifelse(PARTY_CODE == "v3", "REP",
                             ifelse(PARTY_CODE == "v4", "DEM",
                                    "OTHER"))) %>%
  left_join(cheatsheet1996_2000CLARK, by=c("precnum" = "PRECINCT")) %>%
  select(year, county, precname, precnum, SENATE_OR_HOUSE, DISTRICT_NUM, 
         Turnout, PARTY_CODE, VOTES, privacy)

#Isolate and Wrangle Data for 2002
nvclark2002_tidy <- nvclark1992to2002 %>%  
  filter(rowtype == "precinct",
         year == 2002) %>%
  #v1, v3, v4, v6 and v7 are independent parties. Sum their votes into OTHER
  mutate(OTHER = v1+v3+v4+v6+v7) %>%
  #Gather v5, v2 and OTHER into PARTY_CODE indicating the candidates' parties
  gather(key = PARTY_CODE, value = VOTES, v2, v5, OTHER) %>%
  #Rename v2 to REP and v5 to DEM
  mutate(PARTY_CODE = ifelse(PARTY_CODE == "v1", "REP",
                             ifelse(PARTY_CODE == "v3", "DEM",
                                    "OTHER"))) %>%
  left_join(cheatsheet2002CLARK, by=c("precnum" = "PRECINCT")) %>%
  select(year, county, precname, precnum, SENATE_OR_HOUSE, DISTRICT_NUM, 
         Turnout, PARTY_CODE, VOTES, privacy)

#Rebind the tidied data for each election years together into 1 dataframe
nvclark1992to2002_tidy <- rbind(nvclark1992_tidy, nvclark1994_tidy, nvclark1996_tidy,
                                nvclark1998_tidy, nvclark2000_tidy, nvclark2002_tidy)
##############################################
# 1992-2002 Data Carson City
##############################################

#Import Carson City Data
carsoncity1996to2002 <- read_excel("state-legislative-data/Precinct Level Election Results/carsoncity1996to2002.xls")

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
  summarize(VOTES = sum(votes)) %>%
  ungroup()
  
#PRECINCT TO DISTRICT CHEATSHEET
#extract the precinct's state assembly & senate districts
precinct_district_1992to2002_Carson <- carsoncity1996to2002_tidy %>%
  filter(grepl('senate|assembly', officename)) %>%
  mutate(SENATE_OR_HOUSE = ifelse(grepl('assembly', officename), 8, no = 9)) %>%
  mutate(DIST_NAME = ifelse(grepl('capital', officename), "CAPITAL",
                            ifelse(grepl('western', officename), "WESTERN", 
                            "CARSON CITY"))) %>%
  mutate(DISTRICT_NUM = stri_sub(officename, -2)) %>%
  #recode ct to number 1 to indicate DISTRICT NAME = WESTERN, DIST_NUM = 1
  mutate(DISTRICT_NUM = ifelse(DISTRICT_NUM == "ct", 1, DISTRICT_NUM)) %>%
  select(year, precname, SENATE_OR_HOUSE, DIST_NAME, DISTRICT_NUM) %>%
  distinct()

#Since the senate districts doesnt have a number associated to it: 
#The value that get extracted for DISTRICT_NUM is ct (last 2 characters of "district")
#There are only 1 senate district each associated with either "CAPITAL" AND "WESTERN"
#Hence, I recode "ct" to number 1 to follow the format of Carl's Data

#Join precinct_to_district cheatsheet with the larger Carson City precinct data files
carsoncity1996to2002_tidy <- carsoncity1996to2002_tidy %>%
  left_join(precinct_district_1992to2002_Carson, by=c("year" = "year",
                                                      "precname" = "precname")) 

##############################################
# 1992-2002 Data WASHOE COUNTY
##############################################

#Import the main Washoe precinct-level voting data from 1994-2002
washoe1994to2002 <- read_csv("state-legislative-data/Precinct Level Election Results/washoe1994to2002.csv")

  #Select the relevant collumns
washoe1994to2002 <- washoe1994to2002 %>%
  select(year, 1:3, repub, dem, other)

#Import the precinct-to-district cheatsheet we created for Washoe County
Washoe_Precinct_to_District_Cheatsheet <- read_excel("state-legislative-data/Precinct Level Election Results/Washoe Precinct to District Cheatsheet.xlsx")

table(Washoe_Precinct_to_District_Cheatsheet$DIST_NUM)

  #Select relevant columns and filter out all the entry with DIST_NUM = 0
Washoe_Precinct_to_District_Cheatsheet <- Washoe_Precinct_to_District_Cheatsheet %>%
  #filters out precincts which aren't associated with a district (due to data entry ease)
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

##############################################
#All available precinct level voting returns, 1992-2002
##############################################

#Tidy Data (get all the data files into the same formats (same number of collumns))
carsoncity1996to2002_tidy <- carsoncity1996to2002_tidy %>% 
  #spread PARTY_CODES to derive 3 separate collumns for REP, DEM, OTHER
  spread(key = `PARTY_CODE`, value = `VOTES`, fill = 0) %>%
  #mutate a new column capturing turnout
  mutate(Turnout = DEM + OTHER + REP) %>%
  #mutate a nonexistent precnum column
  mutate(precnum = NA) %>%
  #mutate a county column:
  mutate(county = "Carson_City")

  #mutate a county column for Clark County Data
nvclark1992to2002_tidy <- nvclark1992to2002_tidy %>%
  mutate(county = "clark") %>%
  #mutate a DIST_NAME column: 
  mutate(DIST_NAME = "CLARK") %>%
  #Mutate a column capturing the type of contests/races
  mutate(officename = ifelse(grepl('1992|1996|2000', year), "president", "governor")) %>%
  #Select the relevants columns
  select(-privacy)

  #Add up voter turnout in Washoe County Data
washoe1994to2002_tidy <- washoe1994to2002_tidy %>%
  mutate(Turnout = REP + DEM + OTHER) %>%
  #Mutate a column capturing the type of contests/races
  mutate(officename = ifelse(grepl('1992|1996|2000', year), "president", "governor")) %>%
  #Reformat so that all the columns are in the same order
  rename(DISTRICT_NUM = DIST_NUM)

#Bind all 3 files together
precinct_data_1992to2002 <- rbind(carsoncity1996to2002_tidy, washoe1994to2002_tidy, nvclark1992to2002_tidy)
  
#Gather DEM, REP and OTHER into PARTY_CODE and VOTE  
precinct_data_1992to2002 <- precinct_data_1992to2002 %>% 
  gather(key = PARTY_CODE, value = VOTES, `DEM`, `REP`, `OTHER`) %>%
  #paste together precnum and precname to gether a precinct's name
  mutate(precinct = paste(precname, precnum)) %>%
  #Rearrange column in a format that would makes the most sense to a reader
  select(year, county, precinct, SENATE_OR_HOUSE, 
         DIST_NAME, DISTRICT_NUM, officename, Turnout, PARTY_CODE, VOTES)
  

#Export CSV file of the Washoe County precinct data 1994-2002 with legislative district tags 
write.csv(precinct_data_1992to2002, "Precinct Level Results 1992-2002 Washoe, Clark County & Carson City.csv")


#####################################################################
# Supplement carl's data for 2016
####################################################################

rep_list2016 = c(
  "Brean, Howard",
  "Hambrick, John",
  "McArthur, Richard",
  "Ham, Artemus \"Art\"",
  "Maffatt, Carlo",
  "Fawzy, Jennifer",
  "Ross, Norm",
  "Gardner, David",
  "Shelton, Shelly M.",
  "Riggins, Mark",
  "Anderson, Paul",
  "Howard, Melody",
  "Vaughan, Stan",
  "Newsome, Ronald",
  "DeCorte, Christine",
  "Edwards, Chris",
  "Linton, Carol",
  "Armstrong, Derek",
  "Pickard, Keith",
  "Riger Sr., Jim",
  "Woodbury, Melissa",
  "Tolles, Jill",
  "Weber, Bonnie",
  "Krasner, Lisa",
  "Cornwell, Wesley",
  "Scott, Lauren A",
  "Silberkraus, Stephen",
  "Dickman, Jill",
  "Hansen, Ira",
  "Ellison, John",
  "Williams, Matt",
  "Jones, Brent A.",
  "Oscarson, James",
  "Marchant, Jim",
  "Wheeler, Jim",
  "TITUS, ROBIN L.",
  "KRAMER, AL",
  "Scheff, Howard",
  "Phillips, Nick",
  
  #Senate Candidates
  "Ter-Petrosyan, Arsen \"Arsen T\"",
  "Palmerston, Dennis",
  "Buck, Carrie",
  "Seaman, Victoria",
  "Frazier, Jon",
  "Gansert, Heidi S.",
  "Bailey, Kent",
  "Hammond, Scott T.",
  "Goicoechea, Pete"
)

dem_list2016 = c(
  "Monroe-Moreno, Daniele",
  "Carver, Owen",
  "Araujo, Jr., Nelson",
  "Piro, John",
  "Miller, Brittney",
  "Neal, Dina",
  "McCurdy, II, William",
  "Frierson, Jason",
  "Yeager, Steve",
  "Brooks, Chris",
  "Diaz, Olivia",
  "Ohrenschall, James",
  "Anderson, Elliot",
  "Carlton, Maggie",
  "Swank, Heidi",
  "Thompson, Tyrone",
  "Carrillo, Richard A.",
  "Fumo, Ozzie",
  "Spiegel, Ellen",
  "Aguirre-Insua, Luis",
  "Jordahl, Craig",
  "Joiner, Amber",
  "Benitez-Thompson, Teresa",
  "Smith, Allen \"Eli\"",
  "Flores, Edgar",
  "Cohen, Lesley Elizabeth",
  "Sprinkle, Michael",
  "Cohen, Lesley Elizabeth",
  "Daly, Richard \"Skip\"",
  "Bilbray-Axelrod, Shannon",
  "Watkins, Justin",
  "DINI, GEORGE R.",
  "Lyttle, Sean D.",
  "GREEDY, MICHAEL L.",
  "Bustamante Adams, Irene",
  "Jauregui, Sandra",
  
  #Senate Candidates
  "Spearman, Patricia \"Pat\"",
  "Segerblom, \"Tick\"",
  "Atkinson, Kelvin",
  "Woodhouse, Joyce",
  "Cannizzaro, Nicole",
  "Parks, David",
  "Ford, Aaron D.",
  "Ratti, Julia",
  "Reese, Devon Thomas",
  "Marks, Alexander"
)

incumbent_list = c(
  "Neal, Dina",
  "Gardner, David",
  "Hambrick, John",
  "Araujo, Jr., Nelson",
  "Diaz, Olivia",
  "Ohrenschall, James",
  "Shelton, Shelly M.",
  "Anderson, Paul",
  "Carlton, Maggie",
  "Anderson, Elliot",
  "Swank, Heidi",
  "Thompson, Tyrone",
  "Carrillo, Richard A.",
  "Armstrong, Derek",
  "Spiegel, Ellen",
  "Joiner, Amber",
  "Woodbury, Melissa",
  "Benitez-Thompson, Teresa",
  "Flores, Edgar",
  "Sprinkle, Michael",
  "Silberkraus, Stephen",
  "Dickman, Jill",
  "Hansen, Ira",
  "Bilbray-Axelrod, Shannon",
  "Jones, Brent A.",
  "Oscarson, James",
  "Wheeler, Jim",
  "TITUS, ROBIN L.",
  "Bustamante Adams, Irene",
  
  #Senate Candidates
  "Spearman, Patricia \"Pat\"",
  "Segerblom, \"Tick\"",
  "Atkinson, Kelvin",
  "Woodhouse, Joyce",
  "Parks, David",
  "Ford, Aaron D.",
  "Ratti, Julia",
  "Hammond, Scott T.",
  "Goicoechea, Pete"
)
  
use_cheatsheet2012_2016 = cheatsheet2012_2016 %>%
  mutate(SENATE_OR_HOUSE=ifelse(SENATE_OR_HOUSE==8,"HOUSE","SENATE")) 

y2016_supp_data = all_prec_data %>%
  filter(Year == 2016,
         grepl('Senate|Assembly', Contest)) %>%
  mutate(SENATE_OR_HOUSE = ifelse(grepl('Senate',Contest),"SENATE","HOUSE")) %>%
  left_join(use_cheatsheet2012_2016,by=c("Precinct" = "Precinct","Jurisdiction" = "Jurisdiction","SENATE_OR_HOUSE"="SENATE_OR_HOUSE")) %>%
  group_by(Contest,Selection) %>%
  summarize(canidate_vote = sum(Votes),
            DISTRICT_NUM = DISTRICT_NUM[1],
            DIST_NAME = DIST_NAME[1],
            SENATE_OR_HOUSE=SENATE_OR_HOUSE[1]) %>%
  mutate(turnout = sum(canidate_vote)) %>%
  ungroup() %>%
  mutate(party_code = ifelse(Selection %in% dem_list2016, "DEM",
                             ifelse(Selection %>% rep_list2016, "REP",
                                    "OTHER"))) %>%
  mutate(INCUMBENCY_DUMMY = ifelse(Selection = incumbent_list, 1, 0))


write.csv(y2016_supp_data, "2016_supplement.csv")  
  
  
  
  
  
