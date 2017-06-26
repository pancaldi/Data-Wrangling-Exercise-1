# load data and packages

library(readxl)
dfOriginal <- read_excel('C/Users/jaqo_000/GoogleDrive/refine_original.csv.xls')
destfile <- "refine.xls"
View(copy_of_refine_original_csv)
library("tidyverse")



# clean up brand names

clean_col <- destfile %>%
  mutate(company = tolower(company) %>% 
         first_letter = substr(lower_company, 0, 1),
         clean_company = ifelse(first_letter == "p", "philips",
                                ifelse(first_letter == "a", "akzo",
                                       ifelse(first_letter == "v", "van Houten",
                                              ifelse(first_letter == "u", "unilever", first_letter))))) 
         

# separate product code and number

separate_product <- company %>%
  separate('product code/number', c("product_code", "product_number"), sep = "_", remove = TRUE)


# add product categories

product_code <- c("p", "v", "x", "q")
product_category <- c("Smartphone", "TV", "Laptop", "Tablet")
ProCatLookup <- data.frame(product_code, product_category)

# full address for geocoding

full_address <- product_category %>%
  unite(fulladdress, address:country, sep = "_", remove = TRUE)

# create dummy variables for company & product category

dummy_variables <- full_address %>%
  mutate(company_binary = 1, product_binary = 1) %>%
  mutate(company = paste0("company_", company), category = paste0("product_", category)) %>%
  spread(company, company_binary, fill = 0) %>%
  spread(category, product_binary, fill = 0)
 
# save the output as a CSV file & submit to github

write.csv(dummy_variables, destfile = "refine_clean.csv") 

 

