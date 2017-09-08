# Project: Data Visualization - Exercise 2 on Contraception Method Dataset
# Mosaic Plots

#Generalizations 

# Load all packages
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggthemes)
library(readr)

# Read Contraception Method Dataset
cmc <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data", sep=",")

# Attribute Information: 
# 
#    1. Wife's age                     (numerical)
#    2. Wife's education               (categorical)      1=low, 2, 3, 4=high
#    3. Husband's education            (categorical)      1=low, 2, 3, 4=high
#    4. Number of children ever born   (numerical)
#    5. Wife's religion                (binary)           0=Non-Islam, 1=Islam
#    6. Wife's now working?            (binary)           0=Yes, 1=No
#    7. Husband's occupation           (categorical)      1, 2, 3, 4
#    8. Standard-of-living index       (categorical)      1=low, 2, 3, 4=high
#    9. Media exposure                 (binary)           0=Good, 1=Not good
#    10. Contraceptive method used     (class attribute)  1=No-use 
#                                                         2=Long-term
#                                                         3=Short-term

#Add column names
colnames(cmc) <- c("WifeAge","WifeEdu","HusbandEdu","Children","WifeReligion",
                   "Working","HusbandOcc","SOLIndex","MediaExp","Method")

#Have a look at the data structure
str(cmc)

#Convert to factor variables the ones that are categorical
cmc[2:3] <- lapply(cmc[2:3], factor)
cmc[5:10] <- lapply(cmc[5:10], factor)


# Script generalized into a function
mosaicGG <- function(data, X, FILL) {
  
  # Proportions in raw data
  DF <- as.data.frame.matrix(table(data[[X]], data[[FILL]]))
  DF$groupSum <- rowSums(DF)
  DF$xmax <- cumsum(DF$groupSum)
  DF$xmin <- DF$xmax - DF$groupSum
  DF$X <- row.names(DF)
  DF$groupSum <- NULL
  DF_melted <- melt(DF, id = c("X", "xmin", "xmax"), variable.name = "FILL")
  DF_melted <- DF_melted %>%
    group_by(X) %>%
    mutate(ymax = cumsum(value/sum(value)),
           ymin = ymax - value/sum(value))
  
  # Chi-sq test
  results <- chisq.test(table(data[[FILL]], data[[X]])) # fill and then x
  resid <- melt(results$residuals)
  names(resid) <- c("FILL", "X", "residual")
  
  # Merge data
  DF_all <- merge(DF_melted, resid)
  
  # Positions for labels
  DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2
  index <- DF_all$xmax == max(DF_all$xmax)
  DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2
  
  # plot:
  g <- ggplot(DF_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin,
                          xmax = xmax, fill = residual)) +
    geom_rect(col = "white") +
    geom_text(aes(x = xtext, label = X),
              y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = ytext, label = FILL),
              size = 3, hjust = 1, show.legend = FALSE) +
    scale_fill_gradient2("Residuals") +
    scale_x_continuous("Individuals", expand = c(0,0)) +
    scale_y_continuous("Proportion", expand = c(0,0)) +
    theme_tufte() +
    theme(legend.position = "bottom")
  print(g)
}
 
# Method described by wife's age
mosaicGG(cmc, X = "WifeAge", FILL = "Method")

# Method described by children ever born
mosaicGG(cmc, X = "Children", FILL = "Method")

# Method described by wife's age
mosaicGG(cmc, X = "SOLIndex", FILL = "Method") 

# Method described by husband's occupation
mosaicGG(cmc, X = "HusbandOcc", FILL = "Method") 
 
# Method described by media exposure
mosaicGG(cmc, X = "MediaExp", FILL = "Method") 

# Method described by wife's religion
mosaicGG(cmc, X = "WifeReligion", FILL = "Method") 

# Wife's education described by husband's education
mosaicGG(cmc, X = "HusbandEdu", FILL = "WifeEdu") 

# Wife's education described by wife's now working
mosaicGG(cmc, X = "Working", FILL = "WifeEdu") 
