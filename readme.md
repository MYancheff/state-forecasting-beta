## State-Forecasting: Nevada

### Contributors

* Ben Black
* Thao Dinh

Data and guidance from Carl Klarner

### Description

Our project focuses on produces accurate forecasts of state legislative elections in a small number of states.  State legislative elections have not been sufficiently studied in comparison to presidential elections, yet their outcomes often have as much of, if not more impacts on what kind of legislations will get passed and what kind of policy directives is Congress championing (a factor which is heavily influenced by which party captures the majority seat). Our project will be looking at the outcomes of state legislative elections from 1950 to 2016 (or the latest date on which our client Carl Klarner have information) for the state of Nevada and, using this information, predict what will happen during the mid-term election of 2018. We picked Nevada because it presents an interesting case in the study of election science in which, despite voting blue in 2012, switched to red in the 2016 election and was one of the swing state that had the “Republican trifecta” in 2016. It also contains a large demographic of white working class, which was a key determinant in the 2016 election and are sensitive to changing economic conditions.

For the data science portion of our project, we are charged with importing, examining and cleaning, tidying into a manageable and malleable dataset, transforming the dataframe into that which are appropriate for our model, and create from this tidied time series data of Nevada’s legislative elections a model of the percentage of a legislative chamber being captured by Democrats as a functions of other district, state and national factors that might affect how a eligible person vote. We will collect and determine our independent variables from national-level data (Gallup polls, president's approval rating for both election and mid-term year, and changes in real income per capita as a proxy for overall economic conditions) state-level data (our choice of state's legislative spending, legislative composition after election year, state-level income growth, elected governor's party affiliation, term limits and state midterm penalty) and district levels data (district level voting composition, incumbency and contestation data...). Our data will be a mixture of data fragments collected by our client in previous years as well as independently collected datasets.  

Using these dependent and independent variables, we will produce forecasting models of legislative election outcomes. They will be tested for their goodness of fit and predictive capability. Our goal for the final end product should be to use the model to forecast for the upcoming elections in our state of choice, and co-write a piece with Carl about it that will hopefully get published.


### Basic timeline

Item | Date | Notes
--- | --- | ---
Wrangle precinct data | 4/12/17 | (only the data we have right now)
Make some plots of data | 4/14/17 | The plots will probably be mainly economic and precinct relationships
Poster Presentations | 5/7/2017 |
Final Paper | 5/10/2017 | Final submission date


### Notes

Write about how to transform the data, what we will use someof these transformations for. 