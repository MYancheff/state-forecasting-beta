#Import Nevada Data of Annual Median Household Income (Aggregated) 1984-2016
folder = "state-legislative-data/Economic Data/"
filename = "Nevada Annual Real Median Househod Income Data 1984-2017 BEA code MEHOINUSNVA672N.csv"
path = paste(folder, filename,sep="")
AggregateMedianIncomeData_Nevada = read_csv(path) %>%
  rename(VALUE = MEHOINUSNVA672N) %>%
  mutate(DATA_TYPE = "MED_HOUSE_INCOME")

#Import Nevada Data on % Change of Annual Median Household Income (Marginal) 1984-2016
MedianIncomeData_Nevada = read_csv("state-legislative-data/Economic Data/Nevada Percent Change in Annual Real Median Househod Income Data 1984-2016.csv") %>%
  rename(VALUE = MEHOINUSNVA672N_PCH) %>%
  mutate(DATA_TYPE = "MHI_Change_LOCAL")

#Import Nevada's Annual Unemployment Rate Data 1976-2016 
Unemployment_Nevada <- read_csv("state-legislative-data/Economic data/Nevada Annual Unemployment Rate 1976-2016.csv") %>% 
  rename(VALUE = NVUR) %>%
  mutate(DATA_TYPE = "Unemployment_LOCAL")

#Import National Data on % Change of Annual Median houseHold Income 1984-2016
MedianIncomeData_US <- read_csv("state-legislative-data/Economic data/US National-Level Percent Change in Annual Real Median Household Income 1984-2016.csv") %>% 
  rename(VALUE = MEHOINUSA672N_PCH) %>%
  mutate(DATA_TYPE = "MHI_Change_US")

#Import National Data on Percent Change of Annual Real GDP/Capita 1948-2016
GDPperCapita_US <- read_csv("state-legislative-data/Economic data/US National-Level Percent Change in Annual Real GDP per Capita 1948-2016.csv") %>% 
  rename(VALUE = A939RX0Q048SBEA_PCH) %>%
  mutate(DATA_TYPE = "GDPperCapita_CHANGE_US")

economic_data = rbind(
    GDPperCapita_US,
    MedianIncomeData_Nevada,
    MedianIncomeData_US,
    Unemployment_Nevada) %>%
  mutate(YEAR=as.integer(format(DATE,"%Y"))) %>%
  spread(key=DATA_TYPE,value=VALUE)%>%
  select(-DATE)
  