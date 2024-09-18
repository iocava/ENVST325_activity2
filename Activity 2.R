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

#to find the maximum height at each site, need to tell R to treat each name as a group, then within each group find the summary statistics
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












