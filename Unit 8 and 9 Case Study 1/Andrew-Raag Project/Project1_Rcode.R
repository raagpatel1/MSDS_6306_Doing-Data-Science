library(readr)
library(plyr)
library(dplyr) 
library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(devtools)
library(ggcorrplot)

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

State_ABV_IBU = State_ABV_IBU[order(-State_ABV_IBU$avg_ABV),]
plot1data = State_ABV_IBU %>% slice(1:5)

State_ABV_IBU = State_ABV_IBU[order(-State_ABV_IBU$avg_IBU),]
plot2data = State_ABV_IBU %>% slice(1:5)

# This reorders the plot so it is easy to see which state has the highest ABV, 
# and also adds the value above each bar, rounded to 4 decimal places, for readability.
# Then just simple getting rid of the needless legend, and renaming the x/y axis labels.


g1 = ggplot(plot1data,aes(reorder(State,-avg_ABV),avg_ABV ,fill = State)) + 
     geom_col() + geom_text(aes(label = round(avg_ABV,digits = 4)), vjust = -.5) + 
     theme(legend.position="none") + labs(x = "State", title = "Average ABV", y ="")

g2 = ggplot(plot2data,aes(reorder(State,-avg_IBU),avg_IBU ,fill = State)) + 
  geom_col() + geom_text(aes(label = round(avg_IBU,digits = 1)), vjust = -.5) + 
  theme(legend.position="none") + labs(x = "State", title = "Average IBU", y = "")

ggarrange(g1,g2, labels = c("",""),ncol = 2,nrow = 1)

### Finish 5.

### 7. Is there an apparent relationship between the bitterness of the beer and its
### alcoholic content? Draw a scatter plot. Make your best judgment of a relationship
### and EXPLAIN your answer.

# Created a DF, have to drop all NA, since you can't compare one without the other. 

ABVvsIBU = BB %>% select(ABV,IBU) %>% drop_na()

# There is correlation between the 2 variables. Using pearson correlation, 
# because the data is relatively linear
corr = round(cor(ABVvsIBU),2)
head(corr)

#      ABV  IBU
# ABV 1.00 0.67
# IBU 0.67 1.00

ggplot(ABVvsIBU, aes(x = ABV, y = IBU)) + geom_point() + 
  geom_smooth(method = lm, se=FALSE) + geom_smooth(se = FALSE, color = "red")

### Finish 7.
