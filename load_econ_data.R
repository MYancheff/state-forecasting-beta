```{r load_economic_data}
#Import Nevada Data of Annual Median Household Income (Aggregated) 1984-2016
folder = "state-legislative-data/Economic Data/"
filename = "Nevada Annual Real Median Househod Income Data 1984-2017 BEA code MEHOINUSNVA672N.csv"
path = paste(folder, filename,sep="")
AggregateMedianIncomeData_Nevada = read_csv(path) %>%
  rename(MED_HOUSE_INCOME = MEHOINUSNVA672N) %>%
  mutate(YEAR = as.integer(format(DATE,"%Y")))

#Import Nevada Data on % Change of Annual Median Household Income (Marginal) 1984-2016
MedianIncomeData_Nevada = read_csv("state-legislative-data/Economic Data/Nevada Percent Change in Annual Real Median Househod Income Data 1984-2016.csv") %>%
  rename(MHI_Change_LOCAL = MEHOINUSNVA672N_PCH) %>%
  mutate(YEAR = as.integer(format(DATE,"%Y")))

#Import Nevada's Annual Unemployment Rate Data 1976-2016 
Unemployment_Nevada <- read_csv("state-legislative-data/Economic data/Nevada Annual Unemployment Rate 1976-2016.csv") %>% 
  rename(Unemployment_LOCAL = NVUR) %>%
  mutate(YEAR = as.integer(format(DATE,"%Y")))

#Import National Data on % Change of Annual Median houseHold Income 1984-2016
MedianIncomeData_US <- read_csv("state-legislative-data/Economic data/US National-Level Percent Change in Annual Real Median Household Income 1984-2016.csv") %>% 
  rename(MHI_Change_US = MEHOINUSA672N_PCH) %>%
  mutate(YEAR = as.integer(format(DATE,"%Y")))

#Import National Data on Percent Change of Annual Real GDP/Capita 1948-2016
GDPperCapita_US <- read_csv("state-legislative-data/Economic data/US National-Level Percent Change in Annual Real GDP per Capita 1948-2016.csv") %>% 
  rename(GDPperCapita_CHANGE_US = A939RX0Q048SBEA_PCH) %>%
  mutate(YEAR = as.integer(format(DATE,"%Y")))

#Join the Previous 4 Data Fragments Together into one Frame  (labeled Economic Data)
economic_data <- GDPperCapita_US %>%
  left_join(MedianIncomeData_Nevada, by=c("YEAR" = "YEAR")) %>%
  left_join(MedianIncomeData_US, by=c("YEAR" = "YEAR")) %>%
  left_join(Unemployment_Nevada, by=c("YEAR" = "YEAR")) %>%
  select(3, 2, 5, 7, 9)
```

```{r join econ with voting data}

with_economic = left_join(tot_data, economic_data ,by=c("ELECTION_YEAR"="YEAR"))
#economic_data %>% left_join(tot_data, by=c("YEAR"="ELECTION_YEAR"))
```

```{r graph}
#Calculate percentage of house captured by Democrats during a Given Year's General Election
percent_seat_data <- tot_data %>%
  group_by(ELECTION_YEAR, Assembly) %>%
  summarize(perc_seats_dem = sum(wining_party=="DEM")/n()) %>%
  left_join(economic_data,by=c("ELECTION_YEAR"="YEAR"))


#Tidy df so that we can graph data on a 2 dimensional plane 
#Exclude unemployment rate (it stretches te y-axis scale too far to capture insights on
# the correlating patterns between voting and economic data)
with_economic_Tidied1 <- percent_seat_data %>% gather(Type,Val,-ELECTION_YEAR, -Assembly, 
                                                      -Unemployment_LOCAL)
#Visualize 
graph1 <- ggplot(with_economic_Tidied1,aes(x=ELECTION_YEAR, y=Val, col=Type)) + 
  geom_line() + 
  facet_wrap(~Assembly)

#Tidy a separate df by gathering just Unemployment_Local  and perc_seats_dem under Type
with_economic_Tidied2 <- percent_seat_data %>% gather(key = Type, value = Percent, 
                                                      Unemployment_LOCAL, perc_seats_dem)
```