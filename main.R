
#lbiraries
library(ggplot2)
library(sf)
library(data.table)
library(rgdal)
#library(ggmap) # requires api key
library(tidygeocoder)



df$X <- NULL
df$Name <- as.character(df$Name)

# let's get the german postal codes
# for the cars that are in Germany
germanAddress <- as.character(df$Address)

for(i in 1:length(germanAddress)) {
  if (grepl(pattern = "DE", germanAddress[i], fixed = TRUE)){
    germanAddress[i] <- substr(strsplit(germanAddress[i], split = "DE")[[1]][2], 2, nchar(strsplit(germanAddress[i], split = "DE")[[1]][2]))
  } else {
    germanAddress[i] <- ""
  }
}

# since German postal code has only
# five digits we can simply extract
# those first five chars
germanPostalCode <- substr(germanAddress, 1, 5)
germanPostalCodeTwoDigits <- substr(germanPostalCode, 1, 2)

df$germanPostalCodec <- germanPostalCode
df$germanPostalCodeTwoDigits <- germanPostalCodeTwoDigits
# shapefiles

germany_map <- read_sf('./map/plz-2stellig.shp', 'plz-2stellig')
plot(germany_map)


distinctCodes <- levels(as.factor(germanPostalCodeTwoDigits))
averagePrice <- rep(0, 186)
germany_map$averagePrice <- averagePrice
rm(averagePrice)
for (i in 1:length(germany_map$plz)) {
  germany_map$averagePrice[i] <- mean(df[df$germanPostalCodeTwoDigits == germany_map$plz[i], ]$Preis, na.rm = TRUE)
}

# distinct classes
germany_map$densClass <- cut(germany_map$averagePrice, breaks=c(0,15000,20000,25000,30000,35000, Inf),
                             labels=c('< 15000','15000-20000','20000-25000','25000-30000','30000-35000' , '> 35000'))

# Map c('green','purple','black','yellow','blue' , 'gray')
ggplot() + 
  geom_sf(aes(fill=averagePrice), color = 'transparent', data = germany_map)+
  scale_fill_viridis_c()+
  labs(title = "đe je mećka najefitnija")+
  theme_void() 

# Map with Discrete values

 ggplot() + 
  geom_sf(aes(fill=densClass), color = 'black', lwd=0.3, data = germany_map)+
   scale_fill_manual(values =c('#e1e0f9','#c0bef7','#827ef7','#4a44ed','#241faf' , '#0f0d3d'))+
  theme_void() 
p + 
p + scale_fill_viridis_b()

# ANOTHER MAP

#latlongdata <- read.csv("Germany_complete_gis_info.csv", stringsAsFactors = FALSE )
rm(Germany_complete_gis_info)
latlongdata <- data.frame(Germany_complete_gis_info) ## it must be updated externally

germanylat <- NULL
germanylon <- NULL

for (i in 1:length(df$Name)) {
  print(i)
  if(df$germanPostalCodec[i] == "" || is.na(df$germanPostalCodec[i])) {
    germanylat[i] <- ""
    germanylon[i] <- ""
  } else {
    if (length(latlongdata[latlongdata$X == df$germanPostalCodec[i], ]$lat) == 0) {
      germanylat[i] <- ""
      germanylon[i] <- ""
    } else
    {
      germanylat[i] <- latlongdata[latlongdata$X == df$germanPostalCodec[i], ]$lat
      germanylon[i] <- latlongdata[latlongdata$X == df$germanPostalCodec[i], ]$long
    }
  }
}
df$germanylat <- germanylat
df$germanylon <- germanylon

dfmap <- data.frame(df$Allradantrieb)
dfmap$germanylat <- germanylat
dfmap$germanylon <- germanylon
dfmap <- as.matrix.data.frame(dfmap)
dfmap <- as.data.table(dfmap)
dfmap$germanylat <- as.numeric(dfmap$germanylat)
dfmap$germanylon <- as.numeric(dfmap$germanylon )
dfmap <- dfmap[dfmap$germanylat != "", ]
dfmap <- dfmap[!is.na(dfmap$df.Allradantrieb), ]
coordinates(dfmap) <- c("germanylon", "germanylat")

plot(dfmap, pch = 20, col = "red")
# /home/afrolone/Desktop/FAX/CS498 Computing with data/mobilede/mobiledeAnalysis/map/plz-1stellig.shp
deu <- readOGR("/home/afrolone/Desktop/FAX/CS498 Computing with data/mobilede/mobiledeAnalysis/map/Igismap/Germany_Polygon.shp")
plot(deu)
points(dfmap[dfmap$df.Allradantrieb == TRUE, ], pch = 20, col = "steelblue")
points(dfmap[dfmap$df.Allradantrieb == FALSE, ], pch = 20, col = "red")

deusf <- read_sf("/home/afrolone/Desktop/FAX/CS498 Computing with data/mobilede/mobiledeAnalysis/map/Igismap/Germany_Polygon.shp")

sites <- data.frame(
  longitude = df[!is.na(df$Allradantrieb) & df$Allradantrieb == TRUE, ]$germanylon,
  latitude = df[!is.na(df$Allradantrieb) & df$Allradantrieb == TRUE, ]$germanylat
  )
sites <- sites[sites$latitude != "", ]
(sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                   crs = 4326, agr = "constant"))

sites1 <- data.frame(
  longitude = df[!is.na(df$Allradantrieb) & df$Allradantrieb == FALSE, ]$germanylon,
  latitude = df[!is.na(df$Allradantrieb) & df$Allradantrieb == FALSE, ]$germanylat
)
sites1 <- sites1[sites1$latitude != "", ]
(sites1 <- st_as_sf(sites1, coords = c("longitude", "latitude"), 
                   crs = 4326, agr = "constant"))

ggplot(data = deusf)+
  geom_sf() +
  geom_sf(data = sites1, size = 3, shape = 20, fill = "steelblue", col = "red") +
  geom_sf(data = sites, size = 3, shape = 20, fill = "steelblue", col = "green")+
  coord_sf(xlim = c(5.8, 14.8), ylim = c(47, 56), expand = TRUE) +
  theme_void()


# Percentage of Viermatics


for (i in 1:length(germany_map$plz)) {
  dax <- NULL
  print(i)
  dax <- df[df$germanPostalCodeTwoDigits == germany_map$plz[i], ]$awd
  # table(as.factor(df$Allradantrieb))/length(df$Allradantrieb)
  germany_map$viermaticsPRC[i] <- (table(as.factor(dax))/length(dax))
}

# another map the percentage of viermatics
ggplot() + 
  geom_sf(aes(fill=viermaticsPRC), color = 'transparent', data = germany_map)+
  scale_fill_viridis_c()+
  labs(fill = "The percent of 4Matics")+
  theme_void() 

# Percentage of taxis 
for (i in 1:length(germany_map$plz)) {
  dax <- NULL
  print(i)
  dax <- df[df$germanPostalCodeTwoDigits == germany_map$plz[i], ]$Taxi
  germany_map$viermaticsPRC[i] <- 1 - (table(as.factor(dax))/length(dax))
}

ggplot() + 
  geom_sf(aes(fill=viermaticsPRC), color = 'transparent', data = germany_map)+
  scale_fill_viridis_c()+
  labs(fill = "The percentage of Taxis")+
  theme_void() 

# Percentage of diesels
#dfx <- df[(df$fuelSorted == "Diesel" ,]
for (i in 1:length(germany_map$plz)) {
  dax <- NULL
  print(i)
  dax <- df[
    df$germanPostalCodeTwoDigits == germany_map$plz[i] &
    !is.na(germanPostalCodeTwoDigits) &
    df$germanPostalCodeTwoDigits != ""
    , 
    ]$years
  val <- sum(dax, na.rm = TRUE)/length(dax)
  if (!is.na(val)) {
    germany_map$viermaticsPRC[i] <- val
  } else {
    germany_map$viermaticsPRC[i] <- 0.00
  }
  print(paste0("Postal code: ", germany_map$plz[i], " average ", germany_map$viermaticsPRC[i]))
}

ggplot() + 
  geom_sf(aes(fill=viermaticsPRC), color = 'transparent' , data = germany_map)+ #
  scale_fill_gradient(low = "beige", high = "blue", na.value = NA)+
  labs(fill = "Average first
registration year")+
  theme_void() 

# Percentage of AMGs
for (i in 1:length(germany_map$plz)) {
  dax <- NULL
  print(i)
  dax <- df[
    df$germanPostalCodeTwoDigits == germany_map$plz[i] &
    !is.na(germanPostalCodeTwoDigits) &
    df$germanPostalCodeTwoDigits != "" &
    (df$model == "E 43" | df$model == "E 50" | df$model == "E 53" | df$model == "E 60" |
    df$model == "E 55" | df$model == "E 63")
    , 
  ]$model
  val <- length(dax)
  if (!is.na(val)) {
    germany_map$viermaticsPRC[i] <- val
  } else {
    germany_map$viermaticsPRC[i] <- 0
  }
  print(paste0("Postal code: ", germany_map$plz[i], " average ", germany_map$viermaticsPRC[i]))
}

ggplot() + 
  geom_sf(aes(fill=viermaticsPRC), color = 'transparent' , data = germany_map)+ #
  scale_fill_gradient(low = "beige", high = "orange", na.value = NA)+
  labs(fill = "Number of AMGs")+
  theme_void() 





