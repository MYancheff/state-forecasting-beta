precinct\_deliverable
================

Codebook for precinct data
--------------------------

### Data sources (APA Citation)

#### Machine-Readable Data

-   Statewide precinct-level voting data for 2004-2016. **Nevada's Election Department, "Precinct-Level Results," Office of the Nevada Secretary of State. <http://nvsos.gov/sos/elections/election-information/precinct-level-results>**

-   Reported precinct-level voting data for 1984-2016, collected by Harvard University's Record of American Democracy Project. This project endeavors to find, convert and aggregate data that are going to be useful in conducting time-series empirical research on modern American politics (this includes precinct-level election returns, socioeconomic features, demographics...) They are the only source that provides statewide precinct-level voting returns prior to 2004. **Gary King, Bradley Palmquist, Greg Adams, Micah Altman, Kenneth Benoit, Claudine Gay, Jeffrey B. Lewis, Russ Mayer and Eric Reinhardt, "Record of American Democracy, All Precincts Data Files" and "Record of American Democracy, All Documentation and Miscellaneous Files," The Record of American Democracy, 1984-1990. Harvard University, Cambridge, MA \[producer\], Ann Arbor, MI: ICPSR \[distributor\]: 1997. <https://road.hmdc.harvard.edu/data>**

#### PDF-Converts

-   County-level precinct level election returns for 1992-2002 for Clark County in PDF format. **Election Department, "Past Elections," Nevada Clark County Official Website. <http://www.clarkcountynv.gov/election/Pages/ElectionHistory.aspx>**

-   County-level precinct level election returns for 1992-2002 for Washoe County in PDF format. **Registar of Voters, "Election Results Information," Nevada Washoe County Official Website. <https://www.washoecounty.us/voters/electionresults/index.php>**

-   County-level abstracts of vote for 1992-2002 for Carson City in scanned PDF format. **Election Departments, "Election Results And Historical Information," Carson City, Capital of Nevada Official Website. <http://www.carson.org/government/departments-a-f/clerk-recorder/elections-department/election-results-and-historical-information>**

### Column Specifications

-   YEAR: Year of Election. For this particular deliverable, this variable represents the every 2 year cycles for state general elections. It does not include special elections or primary elections.

-   COUNTY: local county names as listed in Nevada's official documents, including its redistricting maps. This is not the FIPS unique identifier code, so you may see names that overlaps with counties outside of Nevada. The data under this variable doesn't just pertain to just exclusively counties; it also include name of major jurisdictions that, according to the Nevada's Secretary of State, may be useful in uniquely identify the precincts within that district. Nevada did not experience a change in its county boundaries during the allotted period that is the scope of this deliverable (1984-2016)

-   PRECINCT: local name of precinct as assigned by the county election officials. Together with YEAR and COUNTY, it acts as a unique description or identifier of sort for each precinct level observation. Also, It must be noted that the names listed under PRECINCT is not standardized or consistent over time. This we believe could be attributed back to a lack of pattern or consistency in how the county's election departments collect and (as with the 2004-2016) report the data to the Secretary of State office. This will thus pose significant challenge in trying to use this deliverable to supplement/conduct precinct-level analysis. See below in **Methodology and Data Complication** for more detailed information.

-   STATE\_LEGISLATIVE\_CHAMBER: indicator whether or not district (which is a combination of the DIST\_NAME and DISTRICT\_NUM) that the *n* observation is denoting belongs to the State Assembly or the State Senate. When deciding whether or not to have two columns delineating State Senate and House Districts separately, we ultimately chose to gather the District Identifiers so that we have a consistent format for documenting the district names and numbers (as the naming convention for Nevada's state legislative districts does change from the 2002-2011 to the 2012- redistricting period). Furthermore, we feel that if you have a variable indicating whether or not the *n* observation(s) pertain to the House or Senate, you can presumably isolate out one of the chamber for specific analysis. It also make visualization easier, since ggplot can only map 1 variable to 1 aesthetic dimensions.

-   DIST\_NAME: District Name. Together with DISTRICT\_NUM, it creates a unique identifier for the legislative districts to which the precinct belongs. Almost all of the observations under this column is exclusively Senate Districts, as the naming convention for the assembly districts remains unchanged during the 1984-2016 period (1 to 42). However, it wasn't until the last redistricting period (2012-) that the Nevada's state legislature announced that it's moving in the same direction with its senate district's naming convention as it did with the assembly districts and associate each senate district with a unique integer from 1 to 21. Before 2012 (from 1984-2010), Nevada's unique identifier for its senate districts include an area indicator (Clark, Washoe, Capital, Northern, Western or Rural) followed by a number. The number starts over once you move to another area (For example, Clark 1-13, then Washoe 1-4). Since the one is repeated across jurisdictions, it's therefore important that we add a DIST\_NAME column, as the combination of district name and number is what gives the senate district a unique identifier. Not Standardized. Also did not take redistricting into account. The name reported here is the name given to that district during the redistricting process. We have not been able to find an effective way for figuring out how much of the district boundaries of previous redistricting periods change in significant ways in the next one, due to the lack of district shape files going back to even the 2002 redistricting date (much less 1994).

-   DISTRICT\_NUM: This is the number assigned to a House or Senate District in Nevada. The House's district numbers are distinctive, but the senate's district numbers (with the exception of 2012-2016) are not. See DIST\_NAME for a more detailed explanation of the naming convention associated to Nevada's state legislative districts. Not Standardized. Also did not take into redistricting. The name reported here is the name given to that district during the redistricting process.

-   OFFICENAME: This records the election races for which the political candidate represented in the *n* observation is running: for example, the presidential election. We decided to dismiss the smaller nonpartisans race because they can be relatively uninformative when it comes analyzing partisan election outcomes. We also decided, after much deliberation, to also dismiss the state senate and assembly races just to the lack of time and manpower that we need to recode the candidate names with their respective partisanship information for 42 assembly races every 2 years and 21 district races every year from 2004-2016. While I did keep U.S. representatives and Senators races, I do not have continuous data for these 2 races. This is primarily due to the fact that I only converted presidential and governor precinct-level data. Thus president and governor are the only 2 races that have continuous precinct level reporting from 1984-2016. The rest of the "bigger" election races are definitely missing chunks of data from 1992-2002

-   PARTY\_CODE: the candidate's party affiliation for the current election

-   VOTES: the total number within precinct *n* that votes for *y* PARTY\_CODE and *X* officename

-   TURNOUT: total number of votes cast at that particular precinct for that particular election. For some parts of these data, turnout are provided in the original dataframe (Clark County Data, 1992-2002). However, for about 80% percent of the precinct observations, I just ended up summing the votes for REP, DEM and OTHER within such precinct.

### Motivation of the Project

The purpose of this project for us to is produce a dataset that contains all available precinct-level voting returns that we could get our hands on in a format that is easy to manipulate in R and transfer well to data science use/statistical analysis. When undergoing our forecasting project for Nevada, we realized not only were Nevada's data collection subpar (which significantly complicates our import step), but that this kind of dataset that collate all the different precinct-data fragments for Nevada, get it into one place, properly cleaned up and ready for use in empirical research is not available period. This makes the process of data collection and importing data significantly more difficult, and suck time away from the actual wrangling and modeling aspects of research work. Furthermore, this kind of data can be extremely useful and even important in improving the fits of a forecasting model (for example), but right now they are extremely underutilized for any election science project conducted on Nevada because of its sheer inaccessibility. Thus, we hope to offer this up as a deliverable that may be helpful for future researchers who are looking for Nevada's voting data, as it offers up an option to at least explore (which is more than can be said before, in our opinion). This is something that we would have wanted at the start of our project to have (rather than having the wrestle with data collection)

### Methodology and Data Complications

One of the first thing we realized as we began collecting precinct-level data is that there is no single sources for collecting all of these precinct level data from 1984-2016. As a result, the main purpose of this deliverable is to produce a dataset that is composed of Nevada's precinct level election returns from 1984 to 2016. However, the official Secretary of State website only provide precinct-level results going back to 2004, and the data-frames within this category by itself were still missing major components, such as the fact that it didn't provide information on Candidate's party affiliation. The second source for data where the Record for American Democracy database, which bridges some of the gap in our data by covering precinct-level voting returns for general elections from 1984-1992. However, for the period of 1992 to 2002, there is no available online resources, period, that produce a machine-readable precinct-level voting data files. This is one major blind spot in Nevada's data collection, which historically has not been given a lot of attention until recent years (this is why you would see such a disparity between the quality of reporting for recent elections versus historical reporting). However, this missing data for the 1992-2002 period would result in the introduction of relatively significant biases when applying this data to modeling or statistical analysis, as it involves some kind of multiple imputation of the missing data over a long period of time (10 election cycles), and since is so little district level data available, this will probably not match up with the real value, and may introduce some significant bias and variance in modeling or analysis. Therefore, we set out on a quest to hunt down precinct level voting returns for 1992-2002 from individual counties in Nevada.

A cross-reference with the state legislative district map (otherwise referred in this codebook as the redistricting map) for the 1992-2001 redistricting period demonstrates that most counties fall entire within the boundary of 1 state senate or assembly districts. There are only 7 jurisdictions (Clark County, Carson City, Washoe, Eureka, Elko, Lyon and Lander Counties) that are either split between multiple senate and/or assembly districts. It must be noted that Clark County, Nevada accounts for approximately 60% of Nevada's population, and comprises of 26 of 42 assembly districts and 8 of 15 senate districts. This is significantly more than the rest of the other 6 counties, even Washoe who comes second and is only split across 4 senate districts and 9 assembly districts. Therefore, if we were to obtain precinct-level voting data for Clark County (and even better, Clark and Washoe), then we have successfully account for 80% of Nevada during this period. Thus, the data-collection goal was to get machine readable precinct-level data for at least Clark County Nevada from 1992-2002. This is where we ran into the next problem.

We could not find this data in machine-readable format. Because this is historical information, even if counties stored this information for the sake of record-keeping, it would have still requires significant manpower (that they cannot justify) to convert these pdf scans into a machine-readable format. As a result, for Clark County, Washoe County and Carson City, I managed to obtain either pdf scans of the precinct-level election reporting from 1992-2002 (for Clark) and 1994-2002(for Washoe) or abstracts of votes from 1996-2002 (which is the closest available duplicate for full data). Thus our job is to manually convert/import the information contained in pdf files into an excel spreadsheet that I can then read into R and further wrangle. Therefore, with this step in mind and despite having cleaned the data multiple times and check for error, it must be noted that this section of the deliverable may contain some data entry error.

Another problem that we encountered and couldn't resolve is the "privacy precincts". Privacy precincts are precincts that, due to the small number of turnout it has, are exempted by law from listing the breakdown of how many people voted for which party for privacy purposes. We couldn't figure out how to automatically redistribute those votes across all the candidates in R. We ended up filtering them out, just because their Turnout numbers don't add up to be significant portion of the total turnout county-wise for a specific general election.

After we got the data into R, we had to do major data wrangling in order to get them into a format consistent with the ROAD and official 2004-2016 data. With Clark County in particular, the way that data was converted (for convenience purposes) resulted in the candidates being listed as v1-v9. However, there is no specific ordering to this v1-v9 columns; what v1-v9 each stands for in 1992 (in that specific order, such as v1 = REP, v2 = DEM) may not be consistent for 1994 (v1 may be LIB, v2 may = REP). As the orderings are completely different, I have to split the Clark County 1992-2002 data up into 5 data-frames denoting 5 separate election years, and rename v1-v9 individually. I also fill in some missing information in the 3 1992-2002 data-frames (Clark, Washoe and Carson City) like turnout, county, PARTY\_CODE...

One of the most fundamental challenges, not just when dealing with the 1992-2002 missing data but for the entire precinct-level data in general, is the fact that there is no state-wise or even county-level precinct-to-district mapping that explicitly delineate which precincts (using local naming conventions to match with the precinct data that their election departments provided online) belong to which senate or house districts. Thus, the only way that we found for extracting this information is to look at the senate and assembly races and see which precincts were eligible to vote for which districts. However, 2 factors complicate this method of extracting precinct-to-district mapping from legislative elections in the precinct data: 1) not all senate districts went up to vote in the same year (half went up for vote during 1 election year and the other on the next), augmented by 2) there is no consistency in the naming patterns. Even though redistricting only happened every 10 years, which theoretically means that we could have just extract the precinct-to-district mapping for the first two election years to capture all of the senate and assembly district races and the precincts that are eligible to vote in them and (assuming that the naming convention stay exactly the same) apply it across the all observations within this 10 year redistricting band. However, the naming conventions to Nevada's precinct-level data (both our 1992-2002 manually imputed data and the official 2004-2016 data provided by the Secretary of State office) appears to change after a random period of times (2 to 4 years) Thus the noted data issues make it difficult to capture all of the precinct-to-district correlations, and for that reason we have founded that there is around a small number of observations every election year that ends up having an NA for DISTRICT\_NUM. However, when we check the total of votes for these NA observations, they only account for 1/10 of 1% of the data in this deliverable. Thus, despite not knowing how to resolve this issue and concerned that it might have introduced biases into the data, we elected to ignore it for now because the total votes lost is negligible in comparison to the larger dataset.

Then we reformatted the ROAD Data, the 1992-2002 data that we manually imputed, and the official 2004-2016 data available through the Nevada's secretary of State website so that they can be bind together into 1 comprehensive data-frame.

It must be noted that this is not the full precinct-level data of every single county in Nevada from 1984-2016. The deliverable only contains 1992-2002 data that that we can actually find by perusing through official county websites and public libraries. This doesnt include precinct-level election returns from any other counties beside Washoe, Clark and Carson City. It also only has, for the most part, continuous precinct-level returns for presidential, gubertorial and congressional races.

### Codes for Wrangling Precinct-Level Voting Returns (1984-2016)

``` r
library(tidyverse)
library(data.table)
library(readxl)
library(dplyr)
library(stringi)
library(haven)
```

#### PRECINCT-LEVEL VOTING DATA (2004-2016), OFFICE OF SECRETARY OF STATE

``` r
# Load precinct data 2004-2016

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

#List of republican candidates for Presidential, Gubertorial and Congressional Races
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

#List of democratic candidates for Presidential, Gubertorial and Congressional Races
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

#Create a function for replacing names with their party affiliations
get_pres_gov_party = function(name){
  ifelse(name %in% rep_list,"REP",
         ifelse(name %in% dem_list, "DEM","OTHER"))
}

# Put all precinct level csv files for Nevada 2004-2016 into 1 dataframe
prec_files = lapply(all_paths,load_file)
with_years = mapply(function(df,year){mutate(df,Year=year)},prec_files,all_years,SIMPLIFY=FALSE)
all_prec_data = rbindlist(with_years)  %>%
  mutate(Votes = ifelse(is.na(Votes),0,Votes)) %>%
  mutate(Year = as.integer(Year))

#Create the precint-to-district mapping

  #This is for the first redistricting period (2002-2010)
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

  #This is for the second redistricting period (2012-2016)    
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

#Bind the two precinct-to-district mappings to make join() with main data easier
full_cheatsheet = rbind(cheatsheet2004_2010,cheatsheet2012_2016)

#Associate long-form names of election races with short-form (to match the other dfs)
contest_names = c("Governor","President and Vice President of the United States")
officenames = c("governor","president")

officename_map = data.frame(contestname = contest_names,officename = officenames)

prec_data_election = all_prec_data %>%
  #filter in only relevant races (president, governor, assembly and senate districts)
  filter(grepl('Governor|President|Congress|United States Senator', Contest),
        !grepl('Lieutenant', Contest)) %>%
  #Derive a PARTY_CODE Variable from the Candidate Names
  mutate(PARTY_CODE = get_pres_gov_party(Selection)) %>%
  #Create DistrictingSection Var to facilitate join() with precinct-to-district maps
  mutate(DistrictingSection = ifelse(Year %in% c(2004, 2006),1,
                                     ifelse(Year %in% c(2008, 2010),2, 3))) %>%
  #Join with precinct-to-district mappings; attach district tags to precinct
  left_join(full_cheatsheet, by=c("DistrictingSection" = "DistrictingSection",
                                  "Precinct" = "Precinct",
                                  "Jurisdiction" = "Jurisdiction")) %>%
  #Mutate Turnout Variable by summing up DEM, REP, OTHER within individual precinct
  group_by(Year, Jurisdiction, Precinct, Contest) %>%
  mutate(Turnout =  sum(Votes)) %>%
  ungroup() %>%
  #Replace long-form names of election races with short-form (to match the other dfs)
  left_join(officename_map,by=c("Contest"="contestname")) %>%
  select(-Contest)
```

#### RECORD OF AMERICAN DEMOCRACY, REPORTED PRECINCT-LEVEL DATA (1984-1990)

``` r
#Import FIPS County_Code Data
FIPS_County_Code <- read_excel("state-legislative-data/Precinct Level Election Results/FIPS County Code.xlsx")

#Import and Wrangle the ROAD Data
filename = paste(folder,"temp_NV_only20170503.dta",sep="")

road_data = read_dta(filename) %>%
  gather(key=vote_type,value=VOTES,-(year:plc)) %>%
  #Convert VOTES from characters to integers to allow for arithmatic calculations
  mutate(VOTES=as.integer(VOTES)) %>%
  #filter out any observation that doesn't have a value for VOTES
  filter(!is.na(VOTES)) %>%
  mutate(
    #Recode vote_type into new variable `officename`, which indicates types of election
    officename=ifelse(grepl("gyyg_",vote_type),"governor",
                      ifelse(grepl("gyyp_",vote_type),"president",
                             NA)),
    #Recode the factors in vote_type into PARTY_CODE variable
    PARTY_CODE=ifelse(grepl("_dv",vote_type),"DEM",
                      ifelse(grepl("_rv",vote_type),"REP",
                             ifelse(grepl("_o[1-9]v",vote_type),"OTHER",
                                    NA))),
    #convert county_code variable from characters to numerics to join with FIPS-file
    county_code = as.numeric(countyfips)) %>%
  #Filter out NA for officename or PARTY_CODE (NA obs aren't useful for analysis)
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
  #Use the district codebook to recode district name and district number
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
  select(year, county, precinct, SENATE_OR_HOUSE, DIST_NAME, DISTRICT_NUM, officename, PARTY_CODE, VOTES) %>%
  #Mutate a Turnout Variable
  group_by(year, county, precinct, officename) %>%
  mutate(Turnout = sum(VOTES)) %>%
  ungroup()
```

#### 1992-2002 OF AVAILABLE PRECINCT-LEVEL VOTING RETURNS, PDF-TO-EXCEL CONVERTS

``` r
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
  mutate(precnum = as.integer(precnum)) %>%
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
```

``` r
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
```

``` r
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
```

``` r
#Tidy Data (get all the data files into the same formats (same number of collumns))
carsoncity1996to2002_tidy <- carsoncity1996to2002_tidy %>%
  #mutate a new column capturing turnout
  group_by(year, precname, officename) %>%
  mutate(Turnout = sum(VOTES)) %>%
  ungroup() %>%
  #mutate a nonexistent precnum column
  mutate(precnum = NA) %>%
  #mutate a county column
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
  rename(DISTRICT_NUM = DIST_NUM) %>%
  #Gather DEM, REP and OTHER into 1 collumn for PARTY_CODE
  gather(key = PARTY_CODE, value = VOTES, `DEM`, `REP`, `OTHER`)

#Bind all 3 files together
precinct_data_1992to2002 <- rbind(carsoncity1996to2002_tidy, washoe1994to2002_tidy, nvclark1992to2002_tidy)

#paste together precnum and precname to gether a precinct's name  
precinct_data_1992to2002 <- precinct_data_1992to2002 %>%
  mutate(precinct = paste(precname, precnum)) %>%
  #Rearrange column in a format that would makes the most sense to a reader
  select(year, county, precinct, SENATE_OR_HOUSE,
         DIST_NAME, DISTRICT_NUM, officename, Turnout, PARTY_CODE, VOTES)
```

#### Create a comprehensive dataset of all available precinct-level voting data, 1984-2016

``` r
#Reformat the prec_data_election df to match that of the others
prec_data_election <- prec_data_election %>%
  rename(county = Jurisdiction,
         precinct = Precinct,
         year = Year,
         VOTES = Votes) %>%
  select(-DistrictingSection, -Selection)

#Bind road_data (1984-1990), precinct_data_1992to2002 and prec_data_election (2004-2016)
precinct_deliverable <- rbind(road_data, precinct_data_1992to2002, prec_data_election)

#Rename all the precinct_deliverable variables so that they are all capitalized
precinct_deliverable <- precinct_deliverable %>%
  rename(YEAR = year,
         COUNTY = county,
         PRECINCT = precinct,
         OFFICENAME = officename,
         TURNOUT = Turnout,
         STATE_LEGISLATIVE_CHAMBER = SENATE_OR_HOUSE) %>%
  mutate(STATE_LEGISLATIVE_CHAMBER=ifelse(STATE_LEGISLATIVE_CHAMBER==8,"HOUSE","SENATE"))

glimpse(precinct_deliverable)

#Export CSV file of the Washoe County precinct data 1994-2002 with legislative district tags
write.csv(precinct_deliverable, "Nevada General Election Precinct Level Voting Results 1984-2016.csv")
```
