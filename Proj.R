
```{r}
library("ggplot2")
library(tidycensus)
library(tidyverse)
```

```{r}
census_api_key("251f3a6133e1e608c5b54d57d3b90611e6d7d102",install = TRUE)
```




```{r}
state_pop <- get_decennial(geography = "state",
                           variables = "P001001")
head(state_pop)

state_income <- get_acs(geography = "state",
                   variables = "B19013_001"     )
head(state_income)

summary(state_income)
```

Searching for dat looks like this
year =   year, or end year
dataset = acs or decennial then the number aftter is how many years after
  so acs5 will mean acs data for 5 years from the start date

```{r}
v16 <- load_variables(year = "2016",
                     dataset = "acs5",
                     cache = TRUE)
head(v16)
```


```{r}
B19001 <- filter(v16, str_detect(name, "B19001"))

B19001
```
```{r}
ny_income <- get_acs(geography = "county",
                     variables = c(hhincome = "B19013_001")
                     )
summary(ny_income)
ny_income
```
calling for my own data i found 2018 individual income in the past 12 months, from census reporter.
the data is from acs 2018 5 year. 
```{r}
v2 <- load_variables(year = "2018",
                     dataset = "acs5",
                     cache = TRUE)
head(v2)
```



set my data. 

```{r}
B06010 <- filter(v2, str_detect(name, "B06010"))

summary(B06010)
```




```{r}
county_income <- get_acs(geography = "county",
                     variables = c(hhincome = "B06010_001"
                                   ),
                     state = "New York",
                     output = "wide"
                     )
summary(county_income)
county_income
```


```{r}
new_county_income <- na.omit(county_income)
new_county_income
```


```{r}
ggplot(new_county_income, aes(x = hhincomeE, y = reorder(NAME, hhincomeE))) + 
  geom_point() 
```


```{r}
v3 <- load_variables(year = "2019",
                     dataset = "acs1",
                     cache = TRUE)
head(v3)
```
```{r}
B25106 <- filter(v3, str_detect(name, "B25106"))

summary(B25106)
B25106
```


```{r}
state_Costs <- get_acs(geography = "state",
                     variables =  c(HousingCost = "B25106_001"),
                     output = "wide"
                     )
summary(state_Costs)
state_Costs
```

```{r}
ggplot(state_Costs, aes(x = HousingCostE, y = reorder(NAME, HousingCostE))) + 
  geom_point(color = "navy") + 
  geom_errorbarh(aes(xmin = HousingCostE - HousingCostM, xmax = HousingCostE + HousingCostM)) 
  labs(x = "2019 ACS estimate", 
       y = "", 
       title = "Housing Costs as a proportion of House Hold Income")


```




```{r}
county_costs <- get_acs(geography = "county",
                     variables =  c(HousingCost = "B25106_001"),
                     output = "wide",
                     state = "New York",
                     
                     )
summary(county_costs)
county_costs
```
```{r}
county_costs2 <- county_costs %>%
  mutate(NAME = str_replace(NAME, " County, New York", ""))


  
ggplot(county_costs2, aes(x = HousingCostE, y = reorder(NAME, HousingCostE))) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_grey(base_size = 10) + 
  labs(title = "Housing Costs as a proportion of House Hold Income", 
       subtitle = "Counties in New York", 
       x = "2019 ACS estimate", 
       y = "") + 
  scale_x_continuous(labels = scales::dollar)
```
```{r}
county_costs
```

Possible X varibles
Age (Median) = B01002_001
Race = B02001_001
Total pop = B01003_001
Geographic Mobility by age = B07001_001


```{r}
town_costs <- get_acs(geography = "county",
                     variables =  c(HousingCost = "B25106_001", Age = "B01002_001",Race = "B02001_001", Totalpop = "B01003_001", Mobility = "B07001_001"  ),
                    state = "New York",
                     output = "wide",
                    
                     
                     )
summary(town_costs)
town_costs
```

```{r}
lmliving_cost <- lm(town_costs$HousingCostE ~ town_costs$AgeE + town_costs$RaceE + town_costs$TotalpopE + town_costs$MobilityE)
```

```{r}
lmliving_cost
summary(lmliving_cost)
```
```{r}
B25104 <- filter(v3, str_detect(name, "B25104"))

summary(B25104)
B25104
```


```{r}
h_cost <- get_acs(geography = "county",
                     variables =  c(HousingCost = "B25104_001"),
                     output = "wide",
                     state = "New York",
                     
                     )
summary(h_cost)
h_cost
```

```{r}
h_cost2 <- h_cost %>%
  mutate(NAME = str_replace(NAME, " County, New York", ""))


  
ggplot(h_cost2, aes(x = HousingCostE, y = reorder(NAME, HousingCostE))) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_grey(base_size = 10) + 
  labs(title = "Housing Costs by County", 
       subtitle = "Counties in New York", 
       x = "2019 ACS estimate", 
       y = "") + 
  scale_x_continuous(labels = scales::dollar)
```


example for creating nice plots with color, size of dots, and lables
```{r}
# Set dot color and size
g_color <- ggplot(ne_income, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_point(color = "navy", size = 4)

# Format the x-axis labels
g_scale <- g_color + 
  scale_x_continuous(labels = scales::dollar) + 
  theme_minimal(base_size = 18) 

# Label your x-axis, y-axis, and title your chart
g_label <- g_scale + 
  labs(x = "2016 ACS estimate", 
       y = "", 
       title = "Median household income by state")
  
g_label
```


```{r}
# Remove unnecessary content from the county's name
maine_inc2 <- maine_inc %>%
  mutate(NAME = str_replace(NAME, " County, Maine", ""))

# Build a margin of error plot incorporating your modifications
ggplot(maine_inc2, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) + 
  geom_point(size = 3, color = "darkgreen") + 
  theme_grey(base_size = 14) + 
  labs(title = "Median household income", 
       subtitle = "Counties in Maine", 
       x = "ACS estimate (bars represent margins of error)", 
       y = "") + 
  scale_x_continuous(labels = scales::dollar)
```

