
# load data

library(tidyr)
library(dummies)
library(stringdist)
pp <- read_csv("refine_original"/docs.google.com/open?id/data)
tbl_df(pp) 

#clean up brand names

closematch <- function(x, y) {
wt <- c()
  for (i in 1:length(y)) {
    wt[i] <- stringdist(x, y[i])
    }
  which.min(wt)
  return(y[which.min(wt)])
  }
brandName <- c("philips", "akzo", "van houten", "unilever")
df&company <- sapply(df$company, closeMatch, y = brandName)

#separate product code and number

colnames(df)[which(names(df) == "product.code...number")] <- "productcode_number"
df <- separate(df, productcode_number, c("productcode", "number"), sep = "_", remove = FALSE)

#add product categories

productcode <- c("p", "v", "x", "q")
productcategory <- c("Smartphone", "TV", "Laptop", "Tablet")
prodcatMatch <- df(productcode, productcategory)
df <- left_join(df, prodcatMatch)

#add full address for geocoding

full_address <- paste(df$address, df$city, df$country, sep = ",") 
df <- cbind(df, full_address)

#create dummy variables for company & product category

company <- gsub(" ", "_", df$company)
df2 <- dummy(company, sep = "_")
df <- cbind(df, df2)
product <- tolower(df$productcategory)
df3 <- dummy(product, sep = "_")
df <- cbind(df, df3)

#save the output as a CSV file & submit to github

write.csv(dummy_variables, file = "refine_clean.csv") 
