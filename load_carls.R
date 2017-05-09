library(foreign)

IN_SENATE = function(sen_or_house) sen_or_house == 8
IN_HOUSE = function(sen_or_house) sen_or_house == 9
DEM_PARTY_CODE = 100
REP_PARTY_CODE = 200
is_dem = function(code) ifelse(code=="DEM",1,0)


data = read.dta("state-legislative-data/SLERs1967to2015_20160912b_NV.dta")

tables = lapply(data,function(x)table(x,useNA="ifany"))
info_densty = lapply(tables,dim)
tables["v09z"]


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
  mutate(party_code = ifelse(PARTY_CODE_SIMPLIFIED == DEM_PARTY_CODE,"DEM",
                             ifelse(PARTY_CODE_SIMPLIFIED == REP_PARTY_CODE,"REP",
                                    "OTHER")))

general_election_data = named_data %>%
  filter(ELECTION_DETERMINES_SITTING_LEG==1)

get_part = function(choose_from,condition){
  choose_from[[which(condition)[1]]]
}

use_data = general_election_data %>%
  select(ELECTION_WINNER,
         ELECTION_YEAR,
         SENATE_OR_HOUSE,
         party_code,
         INCUMBENCY_DUMMY,
         CANIDATE_VOTE_TOTAL,
         DISTRICT_NUM,
         DISTRICT_NAME) %>%
  mutate(Assembly=ifelse(IN_HOUSE(SENATE_OR_HOUSE),"HOUSE",
                        ifelse(IN_SENATE(SENATE_OR_HOUSE),"SENATE",NA))) %>%
  group_by(ELECTION_YEAR,Assembly,DISTRICT_NUM,DISTRICT_NAME) %>%
  summarise(wining_party = get_part(party_code,ELECTION_WINNER==1),#MULTI MEMBER DISTRICTS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            incumbent_factor = ifelse(all(INCUMBENCY_DUMMY==0),
                                      0,
                                      ifelse(get_part(party_code,INCUMBENCY_DUMMY==1)=="DEM",
                                            1,
                                            ifelse(get_part(party_code,INCUMBENCY_DUMMY==1)=="REP",
                                                   -1,0))),
            assembly_vote_composition = sum(ifelse(party_code=="DEM",CANIDATE_VOTE_TOTAL,
                                      ifelse(party_code=="REP",-CANIDATE_VOTE_TOTAL,0)))/sum(CANIDATE_VOTE_TOTAL)) %>%
  ungroup()
  

