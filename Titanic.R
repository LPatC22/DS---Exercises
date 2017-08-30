# titanic is available in your workspace

#Load packages
library(readr)
library(ggplot2)

# Notes:
# a) Downloaded into my computer the Titanic dataset from Kaggle in CSV format
# b) Edited the copied & pasted script from the tutorial to change the name of the columns, 
#    as they were starting with capital letters: Pclass to pclass, Sex to sex, and so on. 

# 0.- Load the data into RStudio
titanic <- read_csv('titanic_csv.csv')
# 1 - Check the structure of titanic
str(titanic)

# 2 - Use ggplot() for the first instruction
ggplot(titanic, aes(x = pclass, fill = sex)) + 
  geom_bar(position = "dodge")

# 3 - Plot 2, add facet_grid() layer
ggplot(titanic, aes(x = pclass, fill = sex)) + 
  geom_bar(position = "dodge") + 
  facet_grid(. ~ survived)

# 4 - Define an object for position jitterdodge, to use below
posn.jd <- position_jitterdodge(0.5, 0, 0.6)

# 5 - Plot 3, but use the position object from instruction 4
ggplot(titanic, aes(x = pclass, y = age, color = sex)) + 
  geom_point(size = 3, alpha = 0.5, position = posn.jd) + 
  facet_grid(. ~ survived)