library(readr)
library(plyr)
library(dplyr) 
library(tidyverse)
library(ggplot2)
Beers <- read_csv("Live Session Assignments/MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/Beers.csv")
Breweries <- read_csv("Live Session Assignments/MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/Breweries.csv")

### 5. Which state has the maximum alcoholic (ABV) beer? Which state has the most
### bitter (IBU) beer?

# Using PLYR package, I can join based off the brewery id column
# But first I need to name the brew id column the same, and change the name column

colnames(Beers)[5] = "Brew_ID"
colnames(Beers)[1] = "Beer_Name"
colnames(Breweries)[2] = "Brewery_Name"
BB = join(Beers,Breweries,by = "Brew_ID",type = "full", match = "all")
summary(BB)

# Then I can create a new dataframe where I lose all the NA values in ABV/IBU, group by state and 
# summarize the ABV/IBU through mean.

State_ABV = BB %>% drop_na(ABV) %>% group_by(State) %>% summarize(avg_ABV = mean(ABV))
State_IBU = BB %>% drop_na(IBU) %>% group_by(State) %>% summarize(avg_IBU = mean(IBU))

# Joining the dataset into 1, for easier readability. Interestingly, no SD beers have registered IBU's. 

State_ABV_IBU = join(State_ABV,State_IBU,by = "State",type = "left", match = "all")
summary(State_ABV_IBU)

# Just removing these so I can easily use the 1 DF.

rm(State_ABV)
rm(State_IBU)

# I can now show by graph the top 5 states in ABV and IBU

test = State_ABV_IBU %>% drop_na()

# g1 = State_ABV_IBU %>% reorder(avg_ABV) %>% slice(1:5) %>% ggplot(State_ABV_IBU, aes(State, avg_ABV, color = State)) + geom_bar()
