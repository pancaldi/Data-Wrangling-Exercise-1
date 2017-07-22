# create path to the file and load packages

library(readr)
refine_original_csv <- read_excel("C:/Users/jaqo_000/Desktop/refine_original.csv")
View(refine_original_csv)
library(dplyr)

# clean up company brand names

clean_companies <- refine_original_csv %>%
  mutate(company, company = tolower(company)) %>%
  mutate(company = case_when(company %in% c("phillipS", "fillips", "phlips", "phillps", "phllips") ~ "philips", TRUE ~ company)) %>%
  mutate(company = case_when(company %in% c("akz0", "ak zo", "AKZO", "Akzo") ~ "akzo", TRUE ~ company)) %>%
  mutate(company = case_when(company %in% c("Van Houten", "van houten") ~ "van Houten", TRUE ~ company)) %>%
  mutate(company = case_when(company %in% c("unilver", "Unilever") ~ "unilever", TRUE ~ company)) %>%
  
          
# separate product code and product number         

separate_product <- refine_original_csv %>% 
  separate('Product code/ number', c("product_code", "product_number"), sep = "_", remove = TRUE)


# add product categories

product_code <- c("p", "v", "x", "q")
product_category <- c("Smartphone", "TV", "Laptop", "Tablet")
ProCatLookup <- data.frame(product_code, product_category)

# set up full address for geocoding

full_address <- refine_original_csv %>% 
  unite('fulladdress', c(address, city, country), sep = ", ", remove = TRUE)

# create dummy variables for company & product category

dummy_variables <- refine_original_csv %>%
  mutate(company_binary = 1, product_binary = 1) %>%
  mutate(company = paste0("company_", company), category = paste0("product_", category)) %>%
  spread(company, company_binary, fill = 0) %>%
  spread(category, product_binary, fill = 0) 
 
# save the output as a CSV file & submit to github

write.csv(refine_original_csv, file = "refine_clean.csv") 

 

