# Project: Data Wrangling Exercise 2: Dealing with Missing Values
 
#Load packages
library(readr) 
library(dplyr)
library(tidyr)
 
# 0.- Load data into RStudio
tdf <- read_csv('titanic_csv.csv')

# Attribute Information:  
# 
#    1. Ticket classs                         pclass      (categorical)  
#                                                1 = first, 2 = 2nd, 3 = 3rd
#    2. Survival                              survival    (binary)        
#                                                0 = No, 1 = Yes
#    3. Name                                  name        (character)
#    4. Sex                                   sex         (categorical)      
#    5. Age                                   age         (numerical)
#    6. Number of siblings / spouses aboard   sibsp       (numerical)           
#    7. Number of parents / children aboard   parch       (numerical)        
#    8. Ticket number                         ticket      (numerical)    
#    9. Passenger fare                        fare        (numerical)      
#    10. Cabin number                          cabin      (numerical)          
#    11. Port of Embarkation                  embarked    (categorical)   C = Cherbourg
#                                                                         Q = Queenstown
#                                                                         S = Southhampton
#    12. Boat number                          boat        (character)  
#    13. Body                                 body        (categorical)             
#    14. Home destination                     Destination (character) 

                                      
#Take a look at the data structure
str(tdf)

#Find missing values in the entire dataframe
summary(tdf)

# 1. Port of Embarkment
# Find missing values and replace them for "S", Southhamton
# Find rows (indices) with missing values (NAs) in Port of Embarkement
ind <- which(is.na(tdf$embarked))
# Look at the full rows for records missing EmbPort
#tdf[ind, ]
# Replace missing values with "S"
tdf$embarked[ind] <- "S"

# 2. Age 
# Calculate the mean of the age column and round it
age_mean <- round(mean(tdf$age, na.rm = TRUE, trim = 0))
#age_mean  
# Find rows with missing values in Age
indma <- which(is.na(tdf$age))
#tdf[indma,3]
# Use the mean found to populate the missing values in "age" column
tdf$age[indma] <- age_mean

# 3. Lifeboat
# Find missing values in "boat" column (passenger did not make it to a boat)
indmb <- which(is.na(tdf$boat))
#indmb
#tdf[indmb,3]
# Populate these missing values with "None"
tdf$boat[indmb] <- "None"

# 4. Create a new column, "has_cabin_number", 
#    and assign 1 if there is a cabin number, and 0 otherwise
tdf_2 <- tdf %>%
  mutate(has_cabin_number = ifelse(is.na(tdf$cabin), 0, 1))

# Create CSV file 
write.csv(tdf_2, file="titanic_clean.csv", row.name = TRUE)




        




           
         
       
      





