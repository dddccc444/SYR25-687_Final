## Intro to Data Science Project

## Compare US elections pre 2020 and post 2020 to determine the major voting shifts
##****************************************************************************************

##Load Libraries

library(tidyverse)
library(janitor)

if(!require(sqldf)){
  install.packages("sqldf")
  require(sqldf)
}

if(!require(lubridate)){
  install.packages("lubridate")
  require(lubridate)
}

}

if(!require(readxl)){
  install.packages("readxl")
  require(readxl)
}

if(!require(Amelia)){
  install.packages("Amelia")
  require(Amelia)
}

if(!require(tidymodels)){
  install.packages("tidymodels")
  require(tidymodels)
}

if(!require(caTools)){
  install.packages("caTools")
  require(caTools)
}

if(!require(pROC)){
  install.packages("pROC")
  require(pROC)
}

if(!require(brglm2)){
  install.packages("brglm2")
  require(brglm2)
}

if(!require(neuralnet)){
  install.packages("neuralnet")
  require(neuralnet)
}


if(!require(fastDummies)){
  install.packages("fastDummies")
  require(fastDummies)
}

if(!require(Metrics)){
  install.packages("Metrics")
  require(Metrics)
}


## Personal package that I have built
##install.packages("devtools")
##devtools::install_github("shorty",username="msuder29")

require(shorty)

##*************************************************************************************

## Get and read in files


## set working directory
library(tidyverse)
setwd("~/11.Data Science/IST 687/Project")

## read in Census data
county_data <- read_csv( "county_census_and_election_result.csv", 
                         col_types = cols(year = col_character(), 
                                          county_fips = col_character()))
view(county_data)
## read in meta data names
meta_data <- read_csv("meta_data.csv")
view(meta_data)

## replace % with percentage
new_names <- str_replace(meta_data$Description, "%","Percent")

## Replace headers for first 37 columns
colnames(county_data)[1:37] <- new_names
view(county_data)

## leverage janitor to normalize names with Underscores for whitespaces
county_data <- clean_names(county_data)
view(county_data)

## read in MIT Data
MIT_county_data <- read_csv( "countypres_2000-2020.csv")
view(MIT_county_data)

## get distinct data for State, county Name, and FIPS
mit_county_state <- MIT_county_data %>%
  select(state_po, county_fips, county_name) %>%
  distinct_all() %>% 
  filter(is.na(county_fips) == F)
view(mit_county_state)

## find duplicates
dups <- mit_county_state %>% 
  group_by(county_fips) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)
view(dups)
## determine from dups common naming convention
## city as county name looked to be a main issue
city_dups <- mit_county_state %>% 
  filter(county_fips %in% dups$county_fips
         , str_detect(county_name, " CITY"))
view(city_dups)

## update data to remove dups
## used a case statement to update the new names and keep the correct ones
mit_county_state <- MIT_county_data %>%
  select(state_po, county_fips, county_name) %>%
  mutate(county_name = case_when(county_fips == 51510 ~ "ALEXANDRIA"
                                 , county_fips %in% city_dups$county_fips ~ str_replace(county_name, " CITY", "")
                                 , T ~ county_name)) %>% 
  distinct_all() %>% 
  filter(is.na(county_fips) == F)
view(mit_county_state)


## combine data from census with MIT State and county info
combined_data <- county_data %>% 
  select(-state_po, -county_name) %>% 
  left_join(mit_county_state %>% mutate(county_fips = as.character(county_fips)), by = c("county_fip_code" ="county_fips")) 
view(combined_data)


## determine missingness of data
## determine if we are ok with percentage of missing data

## combined_data analysis
# missmap(combined_data)

## analyze change in election voting patterns

## Using MIT Voting Data
## get data in same format as combined dataset
## add column for Percentage of Vote

MIT_county_data <- MIT_county_data %>% 
  mutate(county_name = case_when(county_fips == 51510 ~ "ALEXANDRIA"
                                 # , county_fips %in% city_dups$county_fips ~ str_replace(county_name, " CITY", "")
                                 , T ~ county_name)) %>% 
  mutate(Percentage_of_Vote = round(candidatevotes / totalvotes, 2))


## create winning party by county and year
winning_party <- MIT_county_data %>% 
  filter(totalvotes > 0) %>% 
  group_by(year, state_po, county_name, county_fips, totalvotes) %>% 
  summarise(candidatevotes = max(candidatevotes)) %>% 
  left_join(MIT_county_data %>% select(year, party, state_po, county_name, county_fips,candidatevotes)
            , by = c("year", "candidatevotes", "state_po", "county_fips","county_name")) %>% 
  ungroup()



## historical county voting trends
## how consistent is each county in their votes and what if at any time has a county switched parties

## blank data frame for loop
compare_all_counties <- data.frame()

## get unique states for loop
states <- winning_party %>% select(state_po) %>% distinct_all()

## create state vector
states <-states$state_po

## begin loop

for (i in states) {
  

## create placeholder for data
placeholder <- winning_party %>% 
  filter(state_po == i) %>% ## filter by loop state
  mutate(county_name_fips = paste(county_name, " - ", county_fips, sep = "")) %>% 
  select(year, county_name_fips, party) %>% 
  mutate(year = as.character(year))

## create winners table
## pivot data to determin party changes
## reorder the columns for readability

compare_winners <- placeholder %>%
  pivot_wider(names_from = year, values_from = party) %>% 
  mutate("e1_e2" = if_else(`2000` == `2004`, 0,1)
         ,"e2_e3" = if_else(`2004` == `2008`, 0,1)
         ,"e3_e4" = if_else(`2008` == `2012`, 0,1)
         ,"e4_e5" = if_else(`2012` == `2016`, 0,1)
         ,"e5_e6" = if_else(`2016` ==`2020`, 0,1)
         ,"party_change" = e1_e2+ e2_e3 + e3_e4+e4_e5+ e5_e6) %>% 
  select(-e1_e2, -e2_e3,  -e3_e4,  -e4_e5,  -e5_e6) %>% 
  mutate(State = i) %>% 
  relocate(State,.before = "county_name_fips")

## bind columns to dataframe outside of loop
compare_all_counties <- rbind(compare_all_counties, compare_winners)

}

## filter counties that have changed over 2000-2020
county_party_changes <- compare_all_counties %>% 
  filter(party_change > 0) %>% 
  arrange(desc(party_change))



county_party_changes_names <- county_party_changes$county_name_fips



## Identify Margin of change for each county
## identify which county is considered safe or in play (Margin of Victory > 55%)

## create blank data frame for loop
county_margin_of_change <- data.frame()


## begin
for (i in states) {

  ## create placeholder for data
  placeholder <- winning_party %>% 
    filter(state_po == i) %>%   ## filter by loop state
    mutate(county_name_fips = paste(county_name, " - ", county_fips, sep = "")
           , pct_of_vote = round(candidatevotes / totalvotes,2)) %>% 
    select(year, county_name_fips, pct_of_vote) %>% 
    mutate(year = as.character(year))
  
  
  
  ## create winners table
  ## pivot data to determin party changes
  ## reorder the columns for readability
  
  compare_winners <- placeholder %>%
    pivot_wider(names_from = year, values_from = pct_of_vote) %>% 
    mutate("in_play_2000" = if_else(`2000` >= 0.55, 0, 1)
           ,"in_play_2004" = if_else(`2004` >= 0.55, 0, 1)
           ,"in_play_2008" = if_else(`2008` >= 0.55, 0, 1)
           ,"in_play_2012" = if_else(`2012` >= 0.55, 0, 1)
           ,"in_play_2016" = if_else(`2016` >= 0.55, 0, 1)
           ,"in_play_2020" = if_else(`2020` >= 0.55, 0, 1)
           ,"flippable" = in_play_2000 + in_play_2004 + in_play_2008 + in_play_2012 + in_play_2016 + in_play_2020) %>% 
    select(-in_play_2000 ,-in_play_2004 ,- in_play_2008 ,- in_play_2012 ,- in_play_2016 ,- in_play_2020) %>% 
    mutate(State = i) %>% 
    relocate(State,.before = "county_name_fips")
  
  
  ## bind columns to dataframe outside of loop 
  county_margin_of_change <- rbind(county_margin_of_change, compare_winners)
  
}

county_margin_of_change <- county_margin_of_change %>% 
  filter(flippable > 0) %>% 
  arrange(desc(flippable))


##********************************************************************************

## graph data for party changes greater than 0 inclusive of 2000-2020
ggplot(county_party_changes) +
  aes(x = State, y = party_change) +
  geom_col(fill = "#112446") +
  labs(
    title = "Party Changes by State",
    subtitle = "summation of changes in counties",
    caption = "inclusive of all elections from 2000-2020"
  ) +
  theme_minimal()


## graph data for in play counties inclusive of 2000-2020
ggplot(county_margin_of_change) +
  aes(x = State, y = flippable) +
  geom_col(fill = "#112446") +
  labs(
    title = "In Play Counties by State",
    subtitle = "Winning Party < 55% of Vote",
    caption = "inclusive of all elections from 2000-2020"
  ) +
  theme_minimal()

##********************************************************************************


## county Power

## get total votes for each state per year
statetotalvotes <- winning_party %>% 
  group_by(year, state_po) %>%
  summarise(state_totalvotes = sum(totalvotes))

## join total state votes to winners dataset
## create county power column
county_power <- winning_party %>% 
  left_join(statetotalvotes, by = c("state_po", "year")) %>% 
  mutate("County_Power" = totalvotes / state_totalvotes * 100)


## validate that all state total = 100

## validation is commented out but can be uncommeted to view the data frame
# county_power_validation <- county_power %>% 
#   group_by(year, state_po) %>% 
#   summarise(County_Power = sum(County_Power))
# 
# view(county_power_validation)



## top 5 most powerful counties per state
## sampling for PA

sample_PA <- county_power %>% 
  mutate(county_name_fips = paste(county_name, " - ", county_fips, sep = "")) %>% 
  filter(state_po == "PA") %>% 
  group_by(year, state_po) %>% 
  slice_max(County_Power, n = 5) %>% 
  arrange(year, desc(County_Power))





## how the power has changed over the last 6 elections
## graph the 5 most power counties and their winning vote power

ggplot(sample_PA) +
  aes(x = year, y = County_Power) +
  geom_line(colour = "#112446") +
  theme_minimal() +
  facet_wrap(vars(fct_reorder(county_name_fips, County_Power, .fun = max, .desc = TRUE)))


## Party Power

## summaries data by individual party wins by county for power % and county wins
party_power <- county_power %>% 
  mutate(county_name_fips = paste(county_name, " - ", county_fips, sep = "")) %>% 
  filter(state_po == "PA") %>% 
  group_by(year, state_po, party) %>% 
  summarise(Power_by_party = sum(County_Power)
            ,county_wins = n())


# Party Power Chart
ggplot(party_power) +
  aes(x = year, y = Power_by_party, colour = party) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(DEMOCRAT = "#0A00FA",
               REPUBLICAN = "#FF0000")
  ) 




## what counties have grown in votes and power over the last 6 elections

sample_PA_growth <- county_power %>% 
  mutate(county_name_fips = paste(county_name, " - ", county_fips, sep = "")) %>% 
  filter(state_po == "PA") %>% 
  arrange(county_name_fips, year) %>%  
  group_by(county_name_fips) %>%  # Group by county so lag only applies within each county
  mutate(votes_delta = totalvotes - lag(totalvotes) 
         ,votes_power_delta = County_Power - lag(County_Power))
  

## temporary table to look at in play counties by state and power
temp <- winning_party %>% 
  filter(state_po == "PA") %>% 
  mutate(county_name_fips = paste(county_name, " - ", county_fips, sep = "")
         , pct_of_vote = round(candidatevotes / totalvotes,2)) %>% 
  select(year, county_name_fips, pct_of_vote)


## how powerful are the counties that are in play and can that sway an election
in_play_with_power <- sample_PA_growth %>% mutate(county_name_fips = paste(county_name, " - ", county_fips, sep = "")) %>% 
  left_join(temp, by = c("year","county_name_fips")) %>% 
  filter(pct_of_vote < .55)

## who's at risk

power_grab <- in_play_with_power %>% 
  group_by(year, party) %>% 
  summarise(County_Power = sum(County_Power)
            , AVG_pct_of_vote = mean(pct_of_vote)
            , in_play_county = n()
            , Delta_totalvotes = sum(votes_delta))

