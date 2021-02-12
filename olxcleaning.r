#libraries
library(ggplot2)

olx <- read.csv("/home/afrolone/Desktop/FAX/CS498 Computing with data/mobilede/olx-ba-scraping-script/olxData.csv", 
                colClasses = c("price" = "character"))

olx <- olx[olx$prodYear != "",]

olx <- olx[!is.na(olx$X),]
olx$X <- NULL

olx$location <- as.factor(olx$location)
olx$model <- as.factor(olx$model)
olx$prodYear <- as.numeric(olx$prodYear)

olx <- olx[!is.na(olx$prodYear) ,]

olx[nchar(olx$mileage) > 9, ]$mileage = ""
olx[nchar(olx$mileage) > 8,]$mileage = substr(olx[nchar(olx$mileage) > 8,]$mileage,1,nchar(olx[nchar(olx$mileage) > 8,]$mileage)-1)
olx$mileage <- as.numeric(olx$mileage) * 1000

olx$type <- as.factor(olx$type)
olx$fuel <- as.factor(olx$fuel)
olx$transmission <- as.factor(olx$transmission)
olx$numOfDoors <- as.factor(olx$numOfDoors)

olx[olx$powerkw > 250 & (!is.na(olx$powerkw)), ]$powerkw = NA
olx[olx$powerkw < 37 & (!is.na(olx$powerkw)),]$powerkw = NA

olx$price <- as.numeric(olx$price)
#olx[(olx$price < 1000) & (!is.na(olx$price)),]$price <- olx[(olx$price < 1000) & (!is.na(olx$price)),]$price * 100
olx[(olx$price <= 200) & (!is.na(olx$price)) & (olx$prodYear < 2000), ]$price <- olx[(olx$price <= 200) & (!is.na(olx$price)) & (olx$prodYear < 2000), ]$price * 100

olx[(olx$price <= 200) & (!is.na(olx$price)) & (olx$prodYear >= 2000),]$price <- olx[(olx$price <= 200) & (!is.na(olx$price)) & (olx$prodYear >= 2000),]$price * 100

olx[(olx$price > 200) & (olx$price < 1000) & (!is.na(olx$price)) & (olx$prodYear >= 2000),]$price <- olx[(olx$price > 200) & (olx$price < 1000) & (!is.na(olx$price)) & (olx$prodYear >= 2000), ]$price * 100

## DEALING WITH NAs
## TODO

# POTRAŽNJA
# Potražnja
# potraznja
# potražnja


#grepl( needle, haystack, fixed = TRUE)



















