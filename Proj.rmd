----
 "Factors of the Cost of Housing"
 
 By Prescott Massingill
----

  Cost of living across off america has been hotly studied topic for a long time. This is because its understood that a greater understanding of the differences between systematic variations of cost of living helps policy makers craft proposals that are more effective. This is partticularly relevent with the main stream discussions of Universal Basic Income and Minimum Wage increases. These types of proposals effect people accross the crountry and the real impact of those types of transfers are determined by the cost of living in those area. There for further understanding how the effects of cost of living will create strong foundations as to means test these programs.
  
  The first Paper that is helping inform the research for the following anaylsis is "Determinants of Metropolittan Cost of Living Variations" writen by C. T. Haworth and D. W Rasmussen. This paper was writen in 1973 showing the depth of intrest for this topic through time and how we are still wressling with this topic to this day. The factors that this paper focused on was economies or diseconomies of scale in provision of public services, externalities affecting the compensation of tthoes employed in the city, and the cost of land. A reaccuring theme throughout the papers on the cost of living was the cost of land which is where the following analysis will start.
    The second Paper that helps guide this anaylsis is "Housing Demand, Cost-of-Living Inequality, and the Affordability Crsis" by David Albouy, Babriel Ehrlich, and Yingyi Liu. This paper demonstrates how Cost of living indexs deomonstrate that the poor are more heavily impacted by the relative increases to rent. This has led to real income inequality due to higher amounts of nominal income going toward housing. The regression used in this anaylsis will explain how the factors to the cost of housing effect different groups differently.
  
   This project will be analyzing the different factors that impact the cost of housing. Since there are many impacts I will narrow the scope of my analysis to several factors and narrow the geographical area in question. the American Community Survey data from the Census Bureau is used for this anaysis. 


```{r}
library("ggplot2")
library(tidycensus)
library(tidyverse)
library(viridis)
```

```{r}
#census_api_key("251f3a6133e1e608c5b54d57d3b90611e6d7d102",install = TRUE)
```


To get a broud picture of what the housing costs are like in America. First take the housing costs by state across america to get the big picture and get an expected range. These costs will be monthly housing costs

```{r}
v2 <- load_variables(year = "2018",
                     dataset = "acs5",
                     cache = TRUE)
```
```{r}
B25104 <- filter(v2, str_detect(name, "B25104"))

head(B25104)
```
```{r}
Hcost_state <- get_acs(geography = "state",
                     variables = c(HousingCost = "B25104_001"),
                    output = "wide" 
                     )
```

```{r}
ggplot(Hcost_state, aes(x = HousingCostE, y = reorder(NAME, HousingCostE))) + 
  labs(x = "ACS estimate", 
       y = "", 
       title = "Housing Costs by state",
       subtitle = "2014 - 2018 ACS") +
  geom_point(color = "navy") + 
  geom_errorbarh(aes(xmin = HousingCostE - HousingCostM, xmax = HousingCostE + HousingCostM)) 
  
  
```

As shown there is a wide range of Monthly housing costs compared across states. Monthly housing cost is not an effective way to compare housing costs across geographical locations since the the cost is relative to the income earned in that area. Lets take another look at this range but housing cost as a perportion of income.

```{r }
v3 <- load_variables(year = "2019",
                     dataset = "acs1",
                     cache = TRUE)
```
```{r}
B25106 <- filter(v3, str_detect(name, "B25106"))
```
```{r message=FALSE}
state_Costs <- get_acs(geography = "state",
                     variables =  c(HousingCost = "B25106_001"),
                     survey = "acs1",
                     output = "wide"
                     )
```
```{r}
ggplot(state_Costs, aes(x = HousingCostE, y = reorder(NAME, HousingCostE))) + 
  geom_point(color = "navy") + 
  geom_errorbarh(aes(xmin = HousingCostE - HousingCostM, xmax = HousingCostE + HousingCostM)) +
  labs(x = "ACS estimate", 
       y = "", 
       title = "Housing Costs as a proportion of House Hold Income by State",
       subtitle = "2014 - 2018 ACS")


```

  With the changes put in place the graphs appear almost identical. These values represent housing cost as a proportion of house hold income by month and the data used for this graph is taken from a more recent survey, the 2019 1 year acs survey. The acs 5 year survey will be used for the remainder of this analysis because it gives better data when it comes to the micro data in census tracts that will be used in the regression model.

  Next lets narrow the view more to a singular state and its said counties. There will still be a rural and urban divide that will be shown much like the state graph. This more narrow state/county view will  isolate factors that might get effected with large geographical difference.



```{r message=FALSE}
county_costs <- get_acs(geography = "county",
                     variables =  c(HousingCost = "B25106_001"),
                     survey = "acs5",
                     output = "wide",
                     state = "New York",
                     )
```
```{r}
county_costs2 <- county_costs %>%
  mutate(NAME = str_replace(NAME, " County, New York", ""))


  
ggplot(county_costs2, aes(x = HousingCostE, y = reorder(NAME, HousingCostE))) + 
  geom_point(size = 2, color = "darkgreen") + 
  theme_grey(base_size = 10) + 
  labs(title = "Housing Costs as a proportion of House Hold Income (Counties in New York)", 
       subtitle = "2014 - 2018 ACS", 
       x = "ACS estimate", 
       y = "") + 
  scale_x_continuous(labels = scales::dollar)
```

  This view gives a simmilar view to the state view where there is a visable curve into the extremes of true housing costs in specific areas. 
  
  The housing cost as a proportion of income will be refered to as the real housing costs for the remainder of this anaylsis.
  
```{r}
NY <- get_acs(geography = "county", variables = "B25106_001", state = "NY")

NY %>%
mutate(NAME = gsub(" County, New York", "", NAME)) %>%
 ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  theme_grey(base_size = 10) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 2) +
  labs(title = "Real Housing Cost income by county in New York",
       subtitle = "2014 - 2018 ACS",
       y = "",
       x = "ACS estimate")
```
  
  Going down lower into more micro data 3 counties will be picked within the curve namely being Queens, New York City, and Nassau. Picking counties that have larger differences as opposed to 3 counties all bunched in the lower end of the curve will make the factors that effect housing costs more apparent in the later regression.
  
```{r echo=FALSE, message=FALSE, results='hide'}
Queens <- get_acs(geography = "tract", variables = "B25106_001", 
                state = "NY", county = "Queens", geometry = TRUE)
```
```{r}
ggplot(Queens, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  labs(title = "Real Housing Cost in Queens County") +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")
```


```{r message=FALSE}
Newyork <- get_acs(geography = "tract", variables = "B25106_001", 
                state = "NY", county = "New York", geometry = TRUE)
```
```{r message=FALSE}
ggplot(Newyork, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  labs(title = "Real Housing Cost in New York city") +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")
```


```{r}
Nassau <- get_acs(geography = "tract", variables = "B25106_001", 
                state = "NY", county = "Nassau", geometry = TRUE)

ggplot(Nassau, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  labs(title = "Real Housing Cost in Nassau County") +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(option = "magma")
```

These three views show show the different ranges of real housing costs within these counties. New York being the largest with a top end of 8000 a month to Nassau with a top end of 3000 a month but having a much more consistant average price across the tracts within the county.

 The target of the following regression is to look at a few key factors such as population and mobilty. population in regards to density and mobility in regards to willingness to move houses which is measured by someone moving in the past 12 months. From there the regression focuses mainly on transportation. Types of transportation, walking, biking, cars, and public transit. Then on the time requirements in the overall transportation.
 
  Finally poverty level is also included to see if there are imbalances between socioeconomic classes with regards to real housing costs.
 
 poverty under 1 = B05010_002
 1-2 = B05010_010
 over 2 = B05010_018
```{r message=FALSE}
town_costs <- get_acs(geography = "tract",
                     variables =  c(HousingCost = "B25106_001", PovUnderOne = "B05010_002",PovOnetoTwo = "B05010_010",PovTwoPlus = "B05010_018", Totalpop = "B01003_001", Mobility = "B07001_001", Bike = "B08006_014", Walk = "B08006_015", PublicTrans = "B08006_025", Car = "B08006_019", WorkHome = "B08006_017", Ten = "B08134_003", Fifteen = "B08134_004", twenty = "B08134_005", thirty = "B08134_006", thirtyfive = "B08134_007", fortyfive = "B08134_008", sixty = "B08134_009", sixtyplus = "B08134_010"),
                    state = "New York",
                    county = c("Queens", "New York", "Nassau"),
                     output = "wide",
                    
                     
                     )
```

```{r}
lmliving_cost <- lm(town_costs$HousingCostE ~ town_costs$PovUnderOneE+ town_costs$PovOnetoTwoE+ town_costs$PovTwoPlusE + town_costs$TotalpopE +  town_costs$MobilityE + town_costs$BikeE + town_costs$WalkE + town_costs$PublicTransE + town_costs$CarE + town_costs$WorkHomeE + town_costs$TenE + town_costs$FifteenE + town_costs$twentyE + town_costs$thirtyE + town_costs$thirtyfiveE + town_costs$fortyfiveE + town_costs$sixtyE + town_costs$sixtyplusE)
```
```{r}
summary(lmliving_cost)
```

  Looking at the two over arching variables poulation and mobility, population causes an increase in the cost of housing which is to be expected. higher populated areas have higher demand for housing and land is a fixed amount. This effect might be higher in a area that were selected due to the areas all being near water. Water adjacent geographic areas limit the expansion of building causing a limited supply to housing.
  Mobility is statistically insignificant so can not be used as an explanitory variable.
  
 Next going into transportation, Walking, Cars, and public transit all have an effect that decrease the housing costs as a proportion of income. Car transportation is itself a cost so whether the cost of the car is greater than or less then the decreaseing effect on real income requires further anaylsis. Public transit also has a cost associated with it but it is usually far less then the cost of owning and maintaining a car. Walking has no cost associated with it meaning individuals about to walk to work do benefit from a lower real cost of housing. 
  the last type of transport is no transport, or in other words working form home. This interestingly has a increasing effect on real housing costs. This could be since people who work from home make less money as a proportion of housing costs. The other option is that indiviuals who work from home choose more expensive housing. This does not make sence in economic models because working from home increases ones choices for housing and increased choice on the consumer side provides lower end cost with equal utility.
  Going into transit time, all commute times have a increasing effect on real housing costs. Starting on the low end of cummute times the effect is small then gets larger with the peak being between 20-30 minutes then decreases to its lowest point which is 60 minutes plus of a commute. This effect is explained by individuals want to have short commutes. A thirty minute commute provides the benefit of a short enough commute while giving the distance desired to live in an area not near their employment. This distance between work and home can provide utility in that of good school districts or proximity to other locations such as night life and other activities individuals find value in.
 
  Lastly tying this regression back into the effects of real housing costs across different socioeconomic levels lets look at poverty levels. These are the ratio of income to poverty level under 1, beween 1 and 1.9, and over 2. The poverty levels all have a decreasing effect on real housing costs. Each tier has a different level of effect. between 1 and 1.9 having the largets effectt, then under 1, and finally 2 and over. This trend continuing the higher the income level the higher the cost of real housing although it is clearly not linear. The current real cost of housing is effecting individuals below and slightly above the poverty line less then more wealthy individuals. Whether this is due to public policy is an area that requires further analysis.

  Whats clear from this analysis is that transit in general is a factor on the real cost of housing and access to public transit helps midigate this pressure on individuals. Also how this the real cost of housing is effect by socioeconomic status is seen at least somewhat progressive. Meaning there is less of a burden on the lower end.

  Further analysis is recommended that could further this project. frist, whether cars over all impact on real income is positive or negative because it does bring down the real housing costs but it is a large cost unto its self. secondly, the effect of public policy on public transit to drive more equitable housing. Lastly, further anaylsis of increasing real housing costs across socioeconomic tiers.

