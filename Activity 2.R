# install.packages(c("dplyr", "lubridate"))
#after this is done running, then put it as a comment so the packages don't install ever time I run the code
# load the packages
library(dplyr)
library(lubridate)

#read in the data
streamH <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")
siteInfo <- read.csv("/cloud/project/activtiy02/site_info.csv")

### In-Class Activity
#parsing the date, change character data into date/time
streamH$dateF <- ymd_hm(streamH$datetime, 
                        tz="America/New_York")

#example functions in lubridate
year(streamH$dateF)

leap_year(streamH$dateF)

#example functions in dplyr
# join site info and stream heights into a new data frame "floods"
floods <- full_join(streamH, # left table
                    siteInfo, # right table
                    by="siteID") # common identifier
#filtering data in dplyr
#creating a new df, "peace", and subset to only look at one river
peace <- floods %>%
  filter(siteID == 2295637)
#another example
example <- floods %>%
  filter(gheight.ft >= 10)

#plotting using base R
plot(peace$dateF, peace$gheight.ft, type = "l")
#default type is points, type=b means there will be points connected by lines, type=l means it will be a line

#to find the maximum height at each site, need to tell R to treat each name as a group, 
#then within each group find the summary statistics
max_height <- floods %>%
  group_by(names) %>%
  summarise(max_height_ft = max(gheight.ft, na.rm = TRUE), 
            mean_height_ft = mean(gheight.ft, na.rm = TRUE))


#Prompt 3, working together
#What was the earliest date that each river reached the flood stage?
min_flood <- floods %>%
  group_by(names) %>%
  summarise(min_flood_ft = min(flood.ft, na.rm = TRUE))

coochie <- floods %>%
  filter(siteID == 2312000)
#we were going to go site by site and set the individual flood conditions at each site 

## actually how to do this
flood_date <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

### In-Class Presentation

present_df <- data.frame(
  name = c("Bella", "Taylor", "Abby", "Rosey", "Nestor"), 
  age = c(21, 24, 23, 22, 19), 
  fav_color = c("green", "pink", "black", "green", "blue"), 
  n_siblings = c(1, 1, 1, 3, 2),
  height_in = c(66, 63, 70, 66, 71)
)

# example: use select function to only look at the name and fav_color columns
present_df %>% 
  select(c('name', 'fav_color'))

# example: use mutate function to convert height column from inches to cm
present_df %>%
  select(c(height_in)) %>%
  mutate(height_cm = height_in * 2.54)

# example code for presentation
floods_m <- floods %>%
  mutate(action_height = action.ft, mod_height = moderate.ft, major_height = major.ft) %>%
  select(-action.ft, -moderate.ft, -major.ft)

# in-class demo using floods df

# use select to create a df without variables
simple_floods <- floods %>%
  select(-c('agency', 'siteID', 'datetime', 'moderate.ft', 'action.ft', 'major.ft'))

# use mutate to create columns for metric units
mutate_floods <- simple_floods %>% 
  mutate(gheight.m = gheight.ft * 0.3048, 
         flood.m = flood.ft * 0.3048)


### Homework 2
# Q1: make a separate plot for stream stage data for each river

# first site is Fisheating Creek 
fisheat <- floods %>%
  filter(names == 'FISHEATING CREEK AT PALMDALE')

hist(fisheat$gheight.ft, 
     xlim = c(5,11))

# second site is Peace River
peace <- floods %>%
  filter(names == "PEACE RIVER AT US 17 AT ZOLFO SPRINGS")

hist(peace$gheight.ft)

# third site is Santa Fe River 
santa_fe <- floods %>%
  filter(names == "SANTA FE RIVER NEAR FORT WHITE")

hist(santa_fe$gheight.ft)

# fourth site is Withlacoochee River
withcooch <- floods %>%
  filter(names == "WITHLACOOCHEE RIVER AT US 301 AT TRILBY")

hist(withcooch$gheight.ft)

# Q2: earliest date of flood occurrence for each flood category in each river 
#from above, to find the earliest date each river reached flood stage specifically
flood_date <- floods %>%
  filter(gheight.ft >= flood.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

# first flood category is action
action_date <- floods %>%
  filter(gheight.ft >= action.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

# second flood category is moderate
mod_date <- floods %>%
  filter(gheight.ft >= moderate.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))

# third flood category is major
major_date <- floods %>%
  filter(gheight.ft >= major.ft) %>%
  group_by(names) %>%
  summarise(min_date = min(dateF))


# Q3: which river had the highest stream stage above its listed height in the major flood category











