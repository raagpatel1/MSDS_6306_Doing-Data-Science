library(tidyverse)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(GGally)
library(cowplot)
library(ggpubr)
library(devtools)
library(ggcorrplot)
library(ggfittext)


## Read in the .csv's

Beer = 
  read_csv("MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/Beers.csv")
Brewery = 
  read_csv("MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/Breweries.csv")



################################### PART 2 #####################################
#
# 2. Merge beer data with the breweries data. Print the first 6 observations and
# the last six observations to check the merged file.
#
################################### PART 2 #####################################

## Renamed the 'name' columns, as they were the same in both .csv files, which 
## were hard to distinguish when the tables combined. 

colnames(Beer)[1] = "Beer_Name"
colnames(Brewery)[2] = "Brewery_Name"

## Used a full join to make sure all data is accurately represented. 

BB = full_join(Beer,Brewery,by = c("Brewery_id" = "Brew_ID"))

## Upon inspection of the head and tail of the dataframe, everything seemed to 
## be in order.

head(BB,6)
tail(BB,6)


################################### PART 1 #####################################
#
# 1. How many breweries are present in each state?
#
################################### PART 1 #####################################

# For some reason I can't reoder the chart, maybe you'll have more luck than
# me. Also this graph is a work in progress, I think we can make it prettier.

BB %>% group_by(State) %>% summarize(Amount = n_distinct(Brewery_Name)) %>% 
  arrange(desc(Amount)) %>% slice(1:15) %>%
  ggplot(aes(x = reorder(State, Amount), y = Amount)) + 
  geom_segment(aes(x = State, xend = State, y = 0, yend = Amount)) + 
  geom_point(color = "black", size = 4, alpha = .6) +
  coord_flip() + theme_light() + 
  labs(title = "Amount of Breweries in Each State", x = "Amount", y = "State")


################################### PART 3 #####################################
#
# 3. Address the missing values in each column.
#
################################### PART 3 #####################################

## To address the missing values, we first must know where they are. 

summary(BB)
# NA's IBU :1005
# NA's ABV :62
sum(is.na(BB$Beer_Name))
# [1] 0
sum(is.na(BB$Beer_ID))
# [1] 0
sum(is.na(BB$Brew_ID))
# [1] 0
sum(is.na(BB$Style))
# [1] 5
sum(is.na(BB$Ounces))
# [1] 0
sum(is.na(BB$Brewery_Name))
# [1] 0
sum(is.na(BB$City))
# [1] 0
sum(is.na(BB$State))
# [1] 0

## Created a filtered dataframe where NA values that would skew the data were
## dropped. In further statistical analysis, having NULL data can offset the 
## results.

BB_F = BB %>% drop_na(IBU,ABV,Style)

################################### PART 4 #####################################
#
# 4. Compute the median alcohol content and international bitterness unit for
# each state. Plot a bar chart to compare.
#
################################### PART 4 #####################################

## We thought it'd be best to create the graphs this way, versus creating other
## dataframes because, if we needed to tinker with some parts, it was clearer to 
## do it this way. 

## The way these 'stanzas' of code works is, we take the filtered dataset 
## previously created, group the data by each state, then take the necessary 
## summary. We then organize the data, so that the highest/lowest value is at 
## the top, and we can slice the dataframe to show exactly what we want. Then
## we use standard ggplot functions, where we also reorder the bars for 
## readability. 

## Highest median ABV by state.

BB_F %>% group_by(State) %>% summarize(Median_ABV = median(ABV)) %>% 
  arrange(desc(Median_ABV)) %>% slice(1:6) %>%
  ggplot(aes(reorder(State,-Median_ABV), y = Median_ABV, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="States Highest Median ABV", x="State", y="Median ABV") + 
  geom_text(aes(label = round(Median_ABV,digits = 4)), vjust = -.5) + 
  theme(legend.position="none")

## Lowest median ABV by state.

BB_F %>% group_by(State) %>% summarize(Median_ABV = median(ABV)) %>% 
  arrange((Median_ABV)) %>% slice(1:6) %>%
  ggplot(aes(reorder(State,-Median_ABV), y = Median_ABV, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="States Lowest Median ABV", x="State", y="Median ABV") + 
  geom_text(aes(label = round(Median_ABV,digits = 4)), vjust = -.5) + 
  theme(legend.position="none")

## Highest median IBU by state.

BB_F %>% group_by(State) %>% summarize(Median_IBU = median(IBU)) %>% 
  arrange(desc(Median_IBU)) %>% slice(1:6) %>%
  ggplot(aes(reorder(State,-Median_IBU), y = Median_IBU, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="States Highest Median IBU", x="State", y="Median IBU") + 
  geom_text(aes(label = round(Median_IBU,digits = 4)), vjust = -.5) + 
  theme(legend.position="none")

## Lowest median IBU by state.

BB_F %>% group_by(State) %>% summarize(Median_IBU = median(IBU)) %>% 
  arrange((Median_IBU)) %>% slice(1:6) %>%
  ggplot(aes(reorder(State,Median_IBU), y = Median_IBU, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="States Lowest Median IBU", x="State", y="Median IBU") + 
  geom_text(aes(label = round(Median_IBU,digits = 4)), vjust = -.5) + 
  theme(legend.position="none")


################################### PART 5 #####################################
# 
# 5. Which state has the maximum alcoholic (ABV) beer? Which state has the most
# bitter (IBU) beer?
# 
################################### PART 5 #####################################

## Again, we decided to clearly show how the plots are made, by exposing all the
## intricacies of the plots. 

## The way that these 'stanzas' of code work is, by first taking the filtered 
## Dataframe, then grouping by each state, and summarizing the column of data 
## the way we wanted. We also included the beer name alongside the ABV/IBU 
## value, to show more information. Since multiple beer's had the same ABV, we
## decided to show the first beer, in alphabetical order. We then used typical 
## ggplot functions, and changed the y limits, to enchance readability. 

## Highest single beer ABV

BB_F %>% group_by(State) %>% 
  summarize(Max_ABV = max(ABV),Beer_Name = Beer_Name[1]) %>%
  arrange(desc(Max_ABV)) %>% 
  slice(1:5) %>% 
  ggplot(aes(reorder(State,-Max_ABV), y = Max_ABV, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="Highest Single ABV", x="State", y="ABV") + 
  geom_text(aes(label = round(Max_ABV,digits = 5)), vjust = -.5) +
  geom_text(aes(label = Beer_Name, vjust = -2), check_overlap = T) +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(.05,.13))

## Lowest single beer ABV

BB_F %>% group_by(State) %>% 
  summarize(min_ABV = min(ABV),Beer_Name = Beer_Name[1]) %>%
  arrange(desc(min_ABV)) %>% 
  slice(1:5) %>% 
  ggplot(aes(reorder(State,-min_ABV), y = min_ABV, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="Lowest Single ABV", x="State", y="ABV") + 
  geom_text(aes(label = round(min_ABV,digits = 5)), vjust = -.5) +
  geom_text(aes(label = Beer_Name, vjust = -2), check_overlap = T) +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(.0,.065))

## Highest single beer IBU

BB_F %>% group_by(State) %>% 
  summarize(Max_IBU = max(IBU),Beer_Name = Beer_Name[1]) %>%
  arrange(desc(Max_IBU)) %>% 
  slice(1:5) %>% 
  ggplot(aes(reorder(State,-Max_IBU), y = Max_IBU, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="Highest Single IBU", x="State", y="IBU") + 
  geom_text(aes(label = round(Max_IBU,digits = 5)), vjust = -.5) +
  geom_text(aes(label = Beer_Name, vjust = -2), check_overlap = T) +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0,150))

## Lowest single beer IBU

BB_F %>% group_by(State) %>% 
  summarize(min_IBU = min(IBU),Beer_Name = Beer_Name[1]) %>%
  arrange(desc(min_IBU)) %>% 
  slice(1:5) %>% 
  ggplot(aes(reorder(State,min_IBU), y = min_IBU, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="Lowest Single IBU", x="State", y="IBU") + 
  geom_text(aes(label = round(min_IBU,digits = 5)), vjust = -.5) +
  geom_text(aes(label = Beer_Name, vjust = -2), check_overlap = T) +
  theme(legend.position="none") +
  coord_cartesian(ylim = c(0,55))



################################### PART 6 #####################################
# 
# 6. Comment on the summary statistics and distribution of the ABV variable.
# 
################################### PART 6 #####################################

## Using the integrated region database in R, we created a dataframe where we took
## the abbreviated state name and it's corresponding region, then using a left
## join we was able to associate a region with it's corresponding state. we was 
## unsure whether to keep it in the same dataframe we created earlier (BB_F), so 
## we created a new one. 

State_Region = data.frame(state.abb, state.region)
BB_F_Region = left_join(BB_F,State_Region, by = c("State" = "state.abb"))

# sum(is.na(BB_F_Region$state.region))
# [1] 0


## Filling in the bars with the corresponding region vs the corresponsing state,
## allows for more inferences to be made, and is much easier to read.

BB_F_Region %>% ggplot(aes(x = ABV, fill = state.region)) + geom_histogram(color = 'black') + 
  labs(title="Distribution of ABV", x="ABV", y="Number of Beers")

BB_F_Region %>% ggplot(aes(x = ABV)) + geom_histogram(color = 'blue') + 
  labs(title="Distribution of ABV", x="ABV", y="Number of Beers")

BB_F_Region %>% ggplot(aes(x = ABV)) + geom_line(aes(fill=..count..),stat="bin") + 
  labs(title="Distribution of ABV", x="ABV", y="Number of Beers")

BB_F_Region %>% ggplot(aes(x = ABV)) + geom_boxplot()

summary(BB_F$ABV)
MinABV = which.min(BB_F$ABV)
BB_F[MinABV,]
MaxABV = which.max(BB_F$ABV)
BB_F[MaxABV,]



################################### PART 7 #####################################
# 
# 7. Is there an apparent relationship between the bitterness of the beer and 
# its alcoholic content? Draw a scatter plot. Make your best judgment of a 
# relationship and EXPLAIN your answer.
# 
################################### PART 7 #####################################


ABVvsIBU = BB_F %>% select(ABV,IBU)

corr = round(cor(ABVvsIBU),2)
head(corr)

#      ABV  IBU
# ABV 1.00 0.67
# IBU 0.67 1.00

ggplot(ABVvsIBU, aes(x = ABV, y = IBU)) + geom_point() +
  stat_cor(label.x = .03, label.y = 150) +
  geom_smooth(method = lm, se=FALSE) + geom_smooth(se = FALSE, color = "red") 



################################### PART 8 #####################################
# 
# 8. Budweiser would also like to investigate the difference with respect to IBU 
# and ABV between IPAs (India Pale Ales) and other types of Ale (any beer with 
# “Ale” in its name other than IPA). You decide to use KNN classification to 
# investigate this relationship. Provide statistical evidence one way or the 
# other. You can of course assume your audience is comfortable with percentages.
# KNN is very easy to understand conceptually. 
#
# In addition, while you have decided to use KNN to investigate this 
# relationship (KNN is required) you may also feel free to supplement your 
# response to this question with any other methods or techniques you have 
# learned. Creativity and alternative solutions are always encouraged. 
# 
################################### PART 8 #####################################



















################################### PART 9 #####################################
# 
# 9. Knock their socks off! Find one other useful inference from the data that 
# you feel Budweiser may be able to find value in. You must convince them why it 
# is important and back up your conviction with appropriate statistical evidence. 
# 
################################### PART 9 #####################################










