---
  title: "Note 1: Examining Precinct Level Data"
---
  
  #Import Data
  library(readr)
PrecinctResults_2014 <- read_csv("data-raw/state-legislative-data/Precinct_Level_Election_Results/2014 General Election Results (CSV Format).csv")

#Filter out rows not related to presidental, state legislative or governor results
PrecinctResults_2014Tidy <- PrecinctResults_2014 %>%
  filter(grepl('Senate|Assembly|Governor|Congress', Contest)) %>%
  filter(!grepl('Lieutenant', Contest))

PrecinctResults_2014Tidy$Contest <- as.factor(PrecinctResults_2014Tidy$Contest)

#Cross reference to see if the only contests remaining are congress, state legislative or governor results
contest2014 <- PrecinctResults_2014Tidy %>%
  group_by(Contest) %>%
  summarize(n = n())

#Import Client Data
IN_SENATE = function(sen_or_house) sen_or_house == 8
IN_HOUSE = function(sen_or_house) sen_or_house == 9
GUESS_DAY = function(day) ifelse(is.na(day),7,day)
DEM_PARTY_CODE = 100
REP_PARTY_CODE = 200
is_dem = function(code) ifelse(code=="DEM",1,0)

data = read_dta("data-raw/state-legislative-data/SLERs1967to2015_20160912b_NV.dta")

#Transform Client Data into subset of just 2014 election
named_data = data %>%
  rename(ELECTION_YEAR=v05,
         ELECTION_MONTH=v06,
         ELECTION_DAY=v06b,
         SENATE_OR_HOUSE=v07,
         DISTRICT_NAME=v08,
         ALT_DISTRICT_NAME=v08z,
         DISTRICT_NUM=v09,
         ALT_DISTRICT_NUM=v09z,
         DISTRICT_TYPE=v12,
         NUM_WINNERS_FOR_POS=v13,
         TERM_LENGTH_BY_LAW=v14,
         TERM_LENGTH_ACTUAL=v15,
         ELECTION_TYPE=v16,
         ELECTION_DETERMINES_SITTING_LEG=v17,
         ELECTION_DETERMINES_SITTING_LEG_GEN=v17b,
         CANIDATE_ID=v18,
         PARTY_CODE_DETAILED=v20,
         PARTY_CODE_SIMPLIFIED=v21,
         INCUMBENCY_DUMMY=v22,
         CANIDATE_VOTE_TOTAL=v23,
         ELECTION_WINNER=v24) %>%
  mutate(election_full_date = as.Date(ISOdate(ELECTION_YEAR,ELECTION_MONTH,GUESS_DAY(ELECTION_DAY)))) %>% 
  mutate(party_code = ifelse(PARTY_CODE_SIMPLIFIED == DEM_PARTY_CODE,"DEM",
                             ifelse(PARTY_CODE_SIMPLIFIED == REP_PARTY_CODE,"REP",
                                    "OTHER"))) 

#Subset 2014 General Election portion of named_data (which is the scope of PrecinctResults_2014Tidy) 
general_election_data2014 = named_data %>%
  filter(ELECTION_TYPE=="G") %>%
  filter(ELECTION_YEAR == 2014) %>%
  select(v02, countyname, ELECTION_YEAR, SENATE_OR_HOUSE, ALT_DISTRICT_NUM, CANIDATE_ID, v19, v44, party_code, CANIDATE_VOTE_TOTAL, ELECTION_WINNER)   

#Compare the total count of votes for candidate reported in general_election_data2014 to that in PrecinctResults_2014Tidy) 
vote_total <- PrecinctResults_2014Tidy %>%
  group_by(Contest, Selection) %>%
  summarize(candidate_total = sum(Votes,na.rm = TRUE))

#Found no significant differences between the version reported in Client Data and that reported in Precinct Result for 2014 (when mannually examning them side by side)
#There is 1 disparity in name, but upon examination it's revealed to be an human error in the Client Data on candidate Loop, Marilyn Dondero who was accidentally imputted as DONDEROLOOP, MARILYN
#When summing up the total votes for each candidate for a state legislative position, there is some disparity, but not neligible. The biggest difference is  50 votes. 
#Client Data is consistent with that reported on the official website. 
#Therefore, it's safe to conjecture that some of those votes came from precincts that didn't report its count in PrecinctResults_2014 (therefore Votes = NA)

#Export csv file of tidied 2014 precinct data
write.csv(PrecinctResults_2014Tidy, file = "data/2014_General_Election_Precinct-Level_Results_Tidied.csv")
