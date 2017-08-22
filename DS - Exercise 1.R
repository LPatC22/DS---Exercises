# Project: Data Wrangling Exercise 1: Basic Data Manipulation

#Load packages
library(readr)
library(dplyr)
library(tidyr)

# 0.- Load the data into RStudio
my_ds <- read_csv('refine_original_CSV_2.csv')
# See my dataset
my_ds

# Take a look at the structure of my_ds
# str(my_ds)

# 1.- Clean up brand names
# Clean up 'company' column to have all the brand names standarized

# Approach 1: Convert brand name to all lower case using lapply?
#my_ds$company_lowercase <- lapply(my_df$company, tolower)

# Approach 2: Convert brand names to all lower case using Mutate
my_ds2 <- my_ds %>%
# Using the 'tolower' function
#mutate(company = tolower(company))
# Using 'casefold' case
mutate(company = casefold(company, upper = FALSE))
print(my_ds2, n=26)

# Correct misspelling
my_ds3 <- my_ds2 %>%
mutate(company = case_when(
  substr(.$company, 0, 1) == "a" ~ "akzo", 
  substr(.$company, 0, 1) == "f" ~ "philips",
  substr(.$company, 0, 1) == "p" ~ "philips",
  substr(.$company, 0, 1) == "v" ~ "van houten",
  substr(.$company, 0, 1) == "u" ~ "unilever"))
# View the resulting dataset
print(my_ds3, n=26)

# 2: Separate product code and number
my_ds4 <- my_ds3 %>%
separate(Product.code.number, c("product_code", "product_number"), sep = "-")
print(my_ds4, n=26)

# 3: Add product categories
# Create a new column called product_category based on the product_code
my_ds5 <- my_ds4 %>%
  mutate(product_category = case_when( 
    .$product_code == "p" ~ "Smartphone",
    .$product_code == "v" ~ "TV",
    .$product_code == "x" ~ "Laptop",
    .$product_code == "q" ~ "Tablet"))

print(my_ds5, n=26)

# 4: Add full address for decoding
# Create a new column called 'full_address' containing address, city, and county
# separated by commas
my_ds6 <- my_ds5 %>%
  mutate(full_address = paste(address, city, country, sep = ","))
print(my_ds6, n=26)

# 5: Create dummy variables for company and product category assigning boolean values
my_ds7 <- my_ds6 %>%
 mutate(company_philips = ifelse(company == "philips", 1, 0),
        company_akzo = ifelse(company == "akzo", 1, 0),
        company_van_houten = ifelse(company == "van houten", 1, 0),
        company_unilever = ifelse(company == "unilever", 1, 0),
        product_smartphone = ifelse(product_category == "Smartphone", 1, 0),
        product_tv = ifelse(product_category == "TV", 1, 0),
        product_laptop = ifelse(product_category == "Laptop", 1, 0),
        product_tablet = ifelse(product_category == "Tablet", 1, 0))
print(my_ds7, n=26)

 # Create CSV file
write.csv(my_ds7, file="refine_clean.csv", row.names = FALSE)




        




           
         
       
      





