# load data

library(tidyr)
pp <- read_csv("refine_original"/docs.google.com/open?id/data)
tbl_df(pp) 

#clean up brand names

clean_companies <- pp %>%
  mutate(company = tolower(company)) %>%
  mutate(company = case_when(
    .$company %in% c("phillips", "phillps", "fillips", "phlips" , "phllips" , "phillipS") ~ "philips"
    .$company %in% c("akz0", "ak zo" , "AKZO", "Akzo") ~ "akzo"
    .$company %>%  c("Van Houten" , "van Houten") ~ "van houten"
    .$company %in% c("Unilever", "unilver") ~ "unilever",
    TRUE ~ .$company)
  )

#separate product code and number

separate_product <- clean_companies %>%
  separate('product code / number' , c("product_code", "product_number"), sep= "_",
           remove = TRUE) 

#add product categories

product_categories <- separate_product %>%
  mutate(category = case_when(
    .$product_code %in% c("p") ~ 'smartphone'
    .$product_code %in% c("v") ~ 'TV'
    .$product_code %in% c("x") ~ 'laptop'
    .$product_code %in% c("q") ~ 'tablet'
    TRUE ~ .$product_code))  

#add full address for geocoding

full_address <- paste(df$address, df$city, df$country, sep = ",") 
df <- cbind(df, full-address)

#create dummy variables for company & product category

dummy_variables <- combined_address %>%
mutate(company_binary = 1, product_binary = 1) %>%
mutate(company = past0("company_", company), category = paste0(product_", category)) %>%
spread(company, company_binary, fill = 0) %>%
spread(category, product_binary, fill = 0) 

#save the output as a CSV file & submit to github

write.csv(dummy_variables, file = "refine_clean.csv") 
