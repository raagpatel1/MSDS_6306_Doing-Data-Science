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

Beer = 
  read_csv("MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/Beers.csv")
Brewery = 
  read_csv("MSDS_6306_Doing-Data-Science/Unit 8 and 9 Case Study 1/Breweries.csv")

################################### PART 1 #####################################
#
# 1. How many breweries are present in each state?
#
################################### PART 1 #####################################

State_Count <- as.data.frame(table(Brewery$State))
State_Count <- arrange(State_Count, desc(Freq))
TopTen = head(State_Count,10)
LastTen = tail(State_Count,10)

State_Count %>% ggplot(aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity") + 
  labs(title="Number of Breweries per State", x="State", y="# of Breweries")

TopTen %>% ggplot(aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill="blue") + 
  labs(title="States with Most Breweries", x="State", y="# of Breweries") + 
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

LastTen %>% ggplot(aes(x = reorder(Var1, -Freq), y = Freq)) + 
  geom_bar(stat = "identity", fill="red") + 
  labs(title="States with Least Breweries", x="State", y="# of Breweries") + 
  geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25)

################################### PART 2 #####################################
#
# 2. Merge beer data with the breweries data. Print the first 6 observations and
# the last six observations to check the merged file.
#
################################### PART 2 #####################################

colnames(Beer)[1] = "Beer_Name"
colnames(Brewery)[2] = "Brewery_Name"
BB = full_join(Beer,Brewery,by = c("Brewery_id" = "Brew_ID"))
BB_F = BB %>% drop_na(IBU,ABV,Style)

head(BB,6)
tail(BB,6)

################################### PART 3 #####################################
#
# 3. Address the missing values in each column.
#
################################### PART 3 #####################################

# Just removing these so I can easily use the 1 DF. And we can look at the data.
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


################################### PART 4 #####################################
#
# 4. Compute the median alcohol content and international bitterness unit for
# each state. Plot a bar chart to compare.
#
################################### PART 4 #####################################

State_IBU = BB_F %>% select(State, IBU) %>% group_by(State)
State_ABV = BB_F %>% select(State, ABV) %>% group_by(State)

Median_State_ABV = BB_F %>% group_by(State) %>% summarize(Median_ABV = median(ABV))
Median_State_IBU = BB_F %>% group_by(State) %>% summarize(Median_IBU = median(IBU))

Median_State_ABV %>% arrange(desc(Median_ABV)) %>% slice(1:6) %>%
  ggplot(aes(reorder(State,-Median_ABV), y = Median_ABV, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="States Highest Median ABV", x="State", y="Median ABV") + 
  geom_text(aes(label = round(Median_ABV,digits = 4)), vjust = -.5) + 
  theme(legend.position="none")

Median_State_ABV %>% arrange((Median_ABV)) %>% slice(1:6) %>%
  ggplot(aes(reorder(State,-Median_ABV), y = Median_ABV, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="States Lowest Median ABV", x="State", y="Median ABV") + 
  geom_text(aes(label = round(Median_ABV,digits = 4)), vjust = -.5) + 
  theme(legend.position="none")

Median_State_IBU %>% arrange(desc(Median_IBU)) %>% slice(1:6) %>%
  ggplot(aes(reorder(State,-Median_IBU), y = Median_IBU, fill = State)) + 
  geom_bar(stat = "identity") +  
  labs(title="States Highest Median IBU", x="State", y="Median IBU") + 
  geom_text(aes(label = round(Median_IBU,digits = 4)), vjust = -.5) + 
  theme(legend.position="none")

Median_State_IBU %>% arrange((Median_IBU)) %>% slice(1:6) %>%
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





















################################### PART 7 #####################################
# 
# 7. Is there an apparent relationship between the bitterness of the beer and 
# its alcoholic content? Draw a scatter plot. Make your best judgment of a 
# relationship and EXPLAIN your answer.
# 
################################### PART 7 #####################################



















################################### PART 8 #####################################
# 
# 8. Budweiser would also like to investigate the difference with respect to IBU 
# and ABV between IPAs (India Pale Ales) and other types of Ale (any beer with 
# "Ale" in its name other than IPA). You decide to use KNN classification to 
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










