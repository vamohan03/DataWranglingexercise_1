install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")

library("dplyr")
library("stringr")
library("tidyr")

#Import the refine_original.csv file to Datasheet_1

Datasheet_1 <- read.csv("C:\\Users\\vamoh\\Documents\\SpringBoard-IntroToDataScience\\DataWranglingExercise1\\refine_original.csv")

#View Datasheet_1

print(Datasheet_1)

#View colnames of Datasheet_1

colnames(Datasheet_1)

#problem-1 clean up company names (Was unable to solve using regex, so using in diferent method which would be ineffiecient for large dataset, please share the efficient solution)
# Changing the column name "1..company" to "company"

colnames(Datasheet_1)[1] <- "company"

# creating a vector "dummy_company" and assigned appropriate values and then assigning these vector values to "compnay" column of Datasheet_1

dummy_company <- vector(mode = "character", length=25)
dummy_company[1:6] <- "philips"
dummy_company[7:13] <- "akzo"
dummy_company[14:16] <- "philips"
dummy_company[17:21] <- "van houten"
dummy_company[22:25] <- "unilever"

Datasheet_1$company <- dummy_company

#problem-2 - Separate Product Code and Number into 2 separate columns

Datasheet_1 <- separate(Datasheet_1, col = Product.code...number, into = c("product_code", "product_number"), sep = "-")

#problem-3 Add a column product category
# Creating an empty vector 'product_category' and then using 'for-loop' on the product_code to assign values 

product_category <- c()

for (i in 1:length(Datasheet_1$product_code)) {
  if (Datasheet_1$product_code[i] == "p") {
    product_category[i] <- "smartphone"
  } else if (Datasheet_1$product_code[i] == "v") {
    product_category[i] <- "Tv"
  } else if (Datasheet_1$product_code[i] == "x") {
    product_category[i] <- "Laptop"
  } else if (Datasheet_1$product_code[i] == "q") {
    product_category[i] <- "Tablet"
  }
}

#Adding the 'poduct_name' vector as new column to the Datasheet_1

Datasheet_1 <- cbind(Datasheet_1,product_category)

#Problem-4 - Add full address for GeoCoding

Datasheet_1 <- unite(Datasheet_1, full_address, address, city, country, sep = ",")

#problem-5 - create dummy variables for company and product category
#creating a function
Assign_Binary_value <- function (column_name, value) {
  output <- c()
  for (i in 1:length(column_name)) {
    if (column_name[i] == value) {
      output[i] <- 1
    } else {
      output[i] <- 0
    }
  }
  output
}

#using the function "Assign Binary Value" to create dummy variables for company & product category
product_smartphone <- Assign_Binary_value(column_name = Datasheet_1$product_category, value = "smartphone")
product_Tv <- Assign_Binary_value(column_name = Datasheet_1$product_category, value = "Tv")
product_laptop <- Assign_Binary_value(column_name = Datasheet_1$product_category, value = "Laptop")
product_tablet <- Assign_Binary_value(column_name = Datasheet_1$product_category, value = "Tablet")

company_philips <- Assign_Binary_value(column_name = Datasheet_1$company, value = "philips")
company_akzo <- Assign_Binary_value(column_name = Datasheet_1$company, value = "akzo")
company_van_houten <- Assign_Binary_value(column_name = Datasheet_1$company, value = "van houten")
company_unilever <- Assign_Binary_value(column_name = Datasheet_1$company, value = "unilever")

Datasheet_1 <- cbind(Datasheet_1, product_smartphone, product_Tv, product_laptop, product_tablet, company_philips, company_akzo, company_van_houten, company_unilever)

write.csv(Datasheet_1, "refine_clean.csv")

####################-Rough-Work-Below-#########################

# company <- vector(mode = "character", length=25)
# company[1:6] <- "philips"
# company[7:13] <- "akzo"
# company[14:16] <- "philips"
# company[17:21] <- "van houten"
# company[22:25] <- "unilever"
# 
# Datasheet_2 <- cbind(Datasheet_1,company2)
# Datasheet_2$ï..company <- company2
# colnames(Datasheet_2)[1] <- "company"

# comp_names <- c("Phillips", "phillips", "philips", "phillps", "phillips", "fillips")
# for (i in 1:length(company))
# if (company[i] == comp_names ){
#   company2[i] = "Philips"
# } else {
#   company2[i] = "blue"
# }
# 
# gsub(pattern = "^a", replacement = "Akzo", company)


# product_smartphone <- c()
# 
# for (i in 1:length(Datasheet_1$product_name)) {
#   if (Datasheet_1$product_name[i] == "smartphone") {
#     product_smartphone[i] <- 1
#   } else {
#     product_smartphone[i] <- 0
#   }
# }
# 
# Datasheet_2 <- cbind(Datasheet_1, product_smartphone)
# Datasheet_3 <- cbind(Datasheet_2, product_Tv)
# Datasheet_4 <- cbind(Datasheet_1, product_smartphone, product_Tv)
# 
# head(Datasheet_3,10) 
# 
# Assign_Binary_value <- function (x, y) {
#   output <- c()
#   for (i in 1:length(x)) {
#     if (x[i] == y) {
#       output[i] <- 1
#     } else {
#       output[i] <- 0
#     }
#   }
#   output
# }
# 
# product_Tv <- Assign_Binary_value(x = Datasheet_1$product_name, y = "Tv")
# 
# product_name_1 <-list(product_name)
# product_Tv <- list(length(product_name_1))
# 
# product_vector <- c("a", "b", "c")
# product_a <- c()
# product_b <- c()
# for (i in 1:length(product_vector)) {
#   if (product_vector[i] == "a") {
#     product_a[i] <- 1
#   } else {
#     product_a[i] <- 0
#   }
# }  
# 
# my_func <- function (x, y) {
#   output <- c()
#   for (i in 1:length(x)) {
#     if (x[i] == y) {
#       output[i] <- 1
#     } else {
#       output[i] <- 0
#     }
#   }
#   output
# }
# 
# product_b <- my_func(product_vector, "b")
