
#between 2010 2015
ggplot() + 
  geom_point(data = df[
    df$Preis < 37500 &
      df$Kilometerstand < 750000 &
      df$Kraftstoffart == "Diesel" &
      !is.na(df$Leistungkw) &
      #df$Leistungkw < 150 &
      df$years <= 2015 &
      df$years >= 2010 &
      !is.na(df$Hubraum) &
      df$Kategorie != "Andere" #&
    #(df$Hubraum > 1800 & df$Hubraum < 2250) 
    ,], mapping = aes(x = Kilometerstand, y = PreisBosnian), color = "green") +
  geom_smooth(data = df[
    df$Preis < 37500 &
      df$Kilometerstand < 750000 &
      df$Kraftstoffart == "Diesel" &
      !is.na(df$Leistungkw) &
      #df$Leistungkw < 150 &
      df$years <= 2015 &
      df$years >= 2010 &
      !is.na(df$Hubraum) &
      df$Kategorie != "Andere" #&
    #(df$Hubraum > 1800 & df$Hubraum < 2250) 
    ,], mapping = aes(x = Kilometerstand, y = PreisBosnian), color = "black") +
  geom_point(data = olx[
    !is.na(olx$price) &
      (olx$prodYear >= 2010) & 
      (olx$prodYear <= 2015) & 
      #(olx$model == "E 220" | olx$model == "E 200") &
      (olx$mileage < 600000), 
  ], mapping = aes(x = mileage, y = price), color = "red") +
  geom_smooth(data = olx[
    !is.na(olx$price) &
      (olx$prodYear >= 2010) & 
      (olx$prodYear <= 2015) & 
      #(olx$model == "E 220" | olx$model == "E 200") &
      (olx$mileage < 600000), 
  ], mapping = aes(x = mileage, y = price), color = "red")


# between 2010 2020 
# between 0 to 500000
ggplot() + 
  geom_point(data = df[
    df$Preis < 37500 &
      df$Kilometerstand < 500000 &
      df$Kraftstoffart == "Diesel" &
      !is.na(df$Leistungkw) &
      #df$Leistungkw < 150 &
      df$years <= 2020 &
      df$years >= 2010 &
      !is.na(df$Hubraum) &
      df$Kategorie != "Andere" #&
    #(df$Hubraum > 1800 & df$Hubraum < 2250) 
    ,], mapping = aes(x = Kilometerstand, y = PreisBosnian), color = "green") +
  geom_smooth(data = df[
    df$Preis < 37500 &
      df$Kilometerstand < 500000 &
      df$Kraftstoffart == "Diesel" &
      !is.na(df$Leistungkw) &
      #df$Leistungkw < 150 &
      df$years <= 2020 &
      df$years >= 2010 &
      !is.na(df$Hubraum) &
      df$Kategorie != "Andere" #&
    #(df$Hubraum > 1800 & df$Hubraum < 2250) 
    ,], mapping = aes(x = Kilometerstand, y = PreisBosnian), color = "black") +
  geom_point(data = olx[
    !is.na(olx$price) &
      (olx$prodYear >= 2010) & 
      (olx$prodYear <= 2020) & 
      #(olx$model == "E 220" | olx$model == "E 200") &
      (olx$mileage < 500000), 
  ], mapping = aes(x = mileage, y = price), color = "red") +
  geom_smooth(data = olx[
    !is.na(olx$price) &
      (olx$prodYear >= 2010) & 
      (olx$prodYear <= 2020) & 
      #(olx$model == "E 220" | olx$model == "E 200") &
      (olx$mileage < 500000), 
  ], mapping = aes(x = mileage, y = price), color = "red")

# between 2010 2020
ggplot() + 
  geom_point(data = df[
    df$Preis < 37500 &
      df$Kilometerstand < 500000 &
      df$Kraftstoffart == "Diesel" &
      !is.na(df$Leistungkw) &
      #df$Leistungkw < 150 &
      df$years <= 2020 &
      df$years >= 2010 &
      !is.na(df$Hubraum) &
      df$Kategorie != "Andere" #&
    #(df$Hubraum > 1800 & df$Hubraum < 2250) 
    ,], mapping = aes(x = Kilometerstand, y = PreisBosnian), color = "green") +
  geom_smooth(data = df[
    df$Preis < 37500 &
      df$Kilometerstand < 500000 &
      df$Kraftstoffart == "Diesel" &
      !is.na(df$Leistungkw) &
      #df$Leistungkw < 150 &
      df$years <= 2020 &
      df$years >= 2010 &
      !is.na(df$Hubraum) &
      df$Kategorie != "Andere" #&
    #(df$Hubraum > 1800 & df$Hubraum < 2250) 
    ,], mapping = aes(x = Kilometerstand, y = PreisBosnian), color = "black") +
  geom_point(data = olx[
    !is.na(olx$price) &
      (olx$prodYear >= 2010) & 
      (olx$prodYear <= 2020) & 
      #(olx$model == "E 220" | olx$model == "E 200") &
      (olx$mileage < 500000), 
  ], mapping = aes(x = mileage, y = price), color = "red") +
  geom_smooth(data = olx[
    !is.na(olx$price) &
      (olx$prodYear >= 2010) & 
      (olx$prodYear <= 2020) & 
      #(olx$model == "E 220" | olx$model == "E 200") &
      (olx$mileage < 500000), 
  ], mapping = aes(x = mileage, y = price), color = "red")


# between 2010 and 2015
ggplot() + 
  geom_point(data = df[
    df$PreisBosnian < 75000 &
      df$Kilometerstand < 500000 &
      #df$Kilometerstand > 10000 &
      df$Kraftstoffart == "Diesel" &
      !is.na(df$Leistungkw) &
      #df$Leistungkw < 150 &
      df$years <= 2015 &
      df$years >= 2010 &
      !is.na(df$Hubraum) &
      df$Kategorie != "Andere" #&
    #(df$Hubraum > 1800 & df$Hubraum < 2250) 
    ,], mapping = aes(x = Kilometerstand, y = PreisBosnian), color = "green") +
  geom_smooth(data = df[
    df$PreisBosnian < 75000 &
      df$Kilometerstand < 500000 &
      #df$Kilometerstand > 10000 &
      df$Kraftstoffart == "Diesel" &
      !is.na(df$Leistungkw) &
      #df$Leistungkw < 150 &
      df$years <= 2015 &
      df$years >= 2010 &
      !is.na(df$Hubraum) &
      df$Kategorie != "Andere" #&
    #(df$Hubraum > 1800 & df$Hubraum < 2250) 
    ,], mapping = aes(x = Kilometerstand, y = PreisBosnian), color = "black") +
  geom_point(data = olx[
    !is.na(olx$price) &
      olx$price < 75000 &
      (olx$prodYear >= 2010) & 
      (olx$prodYear <= 2015) & 
      #(olx$model == "E 220" | olx$model == "E 200") &
      (olx$mileage < 500000), 
  ], mapping = aes(x = mileage, y = price), color = "red") +
  geom_smooth(data = olx[
    !is.na(olx$price) &
      olx$price < 75000 &
      (olx$prodYear >= 2010) & 
      (olx$prodYear <= 2015) & 
      #(olx$model == "E 220" | olx$model == "E 200") &
      (olx$mileage < 500000), 
  ], mapping = aes(x = mileage, y = price), color = "red")

## JAAAKO BIITAN GRAAAF
## BROOJ AUTA PO GODINAMA
## DEUU
ggplot(df[
  df$years >= 2000 & df$years < 2021
  ,]) + 
  geom_bar(aes(x = as.character(years), fill = years, color= years), position = "dodge", width = 0.8) + 
  labs(x = "year")

ggplot(olx[
  olx$prodYear >= 2000 & olx$prodYear < 2021
  ,]) + 
  geom_bar(aes(x = as.character(prodYear), fill = prodYear, color= prodYear), position = "dodge", width = 0.8) + 
  labs(x = "year")

# broj taksija po godištu u najslabijih modela
ggplot(df[
  df$years >= 2000 & df$years < 2021 &
  df$fuelSorted == "Diesel" &
  !is.na(df$Leistungkw) &
  df$Leistungkw < 140
  ,]) + 
  geom_bar(
    aes(x = as.character(years), fill = Taxi, color= Taxi),
    position = position_dodge2(width = 0.9, preserve = "single"), 
    width = 0.8
  ) + 
  labs(x = "year")

# broj 4matica po godištu
library(reshape2)

# awd vs others per year
ggplot(df[
  df$years >= 2000 & df$years < 2021
  ,]) + 
  geom_bar(aes(x = as.character(years), fill = awd, color= awd), position = "dodge", width = 0.8) + 
  labs(x = "year")

# point plottovi taksija
ggplot(df[
    df$years >= 2009 & df$years < 2021 &
    df$fuelSorted == "Diesel" &
    !is.na(df$Leistungkw) &
    df$Leistungkw < 140 &
    df$Preis < 50000 &
    df$Kilometerstand < 875000
  ,]) +
  aes(x = Kilometerstand, y = Preis, fill = Taxi, color= Taxi) +
  geom_point() +
  #geom_smooth() +
  scale_color_manual(values = c("Taxi" = "#5dff00", "Not Taxi" = "#a200ff")) +
  scale_fill_manual(values = c("Taxi" = "#5dff00", "Not Taxi" = "#a200ff")) +
  labs(x = "Mileage(in kms)", y = "Price (EUR)") + 
  facet_wrap(~years)

# plot olx po modelima geom bar
ggplot(olx[
  olx$prodYear >= 1995 
  & olx$fuel == "Dizel"
  & (olx$model == "E 200" | olx$model == "E 220")
,]) + 
  geom_bar(
    aes(x = prodYear, fill = model, color= model),
    position = position_dodge2(width = 0.9, preserve = "single"),
    width = 0.8
  ) + 
  labs(x = "year")

# plot olx po pogonu geom bar
ggplot(olx[
  olx$prodYear >= 1995 
  & olx$fuel == "Dizel"
  ,]) + 
  geom_bar(
    aes(x = prodYear, fill = layout, color= layout),
    position = position_dodge2(width = 0.9, preserve = "single"),
    width = 0.8
  ) + 
  labs(x = "year")

# density plot za taksi
ggplot(df[
  df$years >= 2009 & df$years < 2021 &
    df$fuelSorted == "Diesel" &
    !is.na(df$Leistungkw) &
    df$Leistungkw < 140 &
    df$Preis < 50000 &
    df$Kilometerstand < 875000
  ,]) +
  aes(x = Kilometerstand, fill = Taxi, color= Taxi) +
  #geom_point() +
  #geom_smooth() +
  geom_density(alpha = 0.2) +
  labs(x = "Mileage(in kms)", y = "Price (EUR)") #+ 
  #facet_wrap(~years)

# density plot za 4matic
ggplot(df[
  df$years >= 2009 & df$years < 2021 &
    !is.na(df$Leistungkw) &
    df$Leistungkw < 140 &
    df$Preis < 50000 &
    df$Kilometerstand < 875000
  ,]) +
  aes(x = Kilometerstand, fill = awd, color= awd) +
  #geom_point() +
  #geom_smooth() +
  geom_density(alpha = 0.2) +
  labs(x = "Mileage(in kms)") #+ 
#facet_wrap(~years)

# density plot za gorivo
ggplot(df[
  df$years >= 2010 & df$years < 2019 &
    !is.na(df$Leistungkw) &
    df$Leistungkw < 140 &
    df$Preis < 50000 &
    (df$fuelSorted == "Diesel" | df$fuelSorted == "Benzin") &
    df$Kilometerstand < 875000
  ,]) +
  aes(x = Kilometerstand, fill = fuelSorted, color= fuelSorted) +
  #geom_point() +
  #geom_smooth() +
  geom_density(alpha = 0.2) +
  labs(x = "Mileage(in kms)") + 
  facet_wrap(~years)

# density plot za gorivo po cijenama
ggplot(df[
  df$years >= 2010 & df$years < 2019 &
    !is.na(df$Leistungkw) &
    df$Leistungkw < 140 &
    df$Preis < 50000 &
    (df$fuelSorted == "Diesel" | df$fuelSorted == "Benzin") &
    df$Kilometerstand < 875000
  ,]) +
  aes(x = Preis, fill = fuelSorted, color= fuelSorted) +
  #geom_point() +
  #geom_smooth() +
  geom_density(alpha = 0.2) +
  labs(x = "Mileage(in kms)") + 
  facet_wrap(~years)

# density plot za allrad po cijenama
ggplot(df[
  df$years >= 2010 & df$years < 2019 &
  df$Preis < 100000 &
  df$awd == "4Matic"
  ,]) +
  aes(x = Preis, fill = Preis, color="Red") +
  #geom_point() +
  #geom_smooth() +
  geom_density(color="Red") +
  geom_density(data = df[
    df$years >= 2010 & df$years < 2019 &
    df$awd != "4Matic" &
    df$Preis < 100000
    ,], 
  mapping = aes(Preis, fill=Preis), color = "Green") +
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(x = "Price in EUR") + 
  facet_wrap(~years)

# density plot za allrad po kilometrima
ggplot(df[
  df$years >= 2010 & df$years < 2019 &
    df$Preis < 100000 &
    df$Kilometerstand < 500000 &
    df$awd == "4Matic"
  ,]) +
  aes(x = Kilometerstand, fill = Kilometerstand, color="Red") +
  #geom_point() +
  #geom_smooth() +
  geom_density(color="Red") +
  geom_density(data = df[
    df$years >= 2010 & df$years < 2019 &
      df$awd != "4Matic" &
      df$Kilometerstand < 500000 &
      df$Preis < 100000
    ,], 
    mapping = aes(Kilometerstand, fill=Kilometerstand), color = "Green") +
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(x = "Mileage in kms") 

# density plot za allrad po kilometrima
ggplot(df[
  df$years >= 2010 & df$years < 2019 &
    df$Preis < 100000 &
    df$Kilometerstand < 500000 &
    df$awd == "4Matic"
  ,]) +
  aes(x = Kilometerstand, fill = Kilometerstand, color="Red") +
  #geom_point() +
  #geom_smooth() +
  geom_density(color="Red") +
  geom_density(data = df[
    df$years >= 2010 & df$years < 2019 &
      df$awd != "4Matic" &
      df$Kilometerstand < 500000 &
      df$Preis < 100000
    ,], 
    mapping = aes(Kilometerstand, fill=Kilometerstand), color = "Green") +
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(x = "Mileage in kms")

# density plot za allrad po features
ggplot(df[
  df$years >= 2010 & df$years < 2019 &
    df$Preis < 100000 &
    df$Kilometerstand < 500000 &
    df$awd == "4Matic"
  ,]) +
  aes(x = numoffeatures, fill = numoffeatures, color="Red") +
  #geom_point() +
  #geom_smooth() +
  geom_density(color="Red") +
  geom_density(data = df[
    df$years >= 2010 & df$years < 2019 &
      df$awd != "4Matic" &
      df$Kilometerstand < 500000 &
      df$Preis < 100000
    ,], 
    mapping = aes(numoffeatures, fill=numoffeatures), color = "Green") +
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )+
  facet_wrap(~years)+
  labs(x = "Number of features")

# density plot za taksi po features
ggplot(df[
  df$years >= 2010 & df$years < 2019 &
    df$Preis < 100000 &
    df$Kilometerstand < 500000 &
    df$Taxi == "Taxi"
  ,]) +
  aes(x = numoffeatures, fill = numoffeatures, color="Red") +
  #geom_point() +
  #geom_smooth() +
  geom_density(color= "#5dff00") +
  geom_density(data = df[
    df$years >= 2010 & df$years < 2019 &
      df$Taxi != "Taxi" &
      df$Kilometerstand < 500000 &
      df$Preis < 100000
    ,], 
    mapping = aes(numoffeatures, fill=numoffeatures), color = "#a200ff") +
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )+
  facet_wrap(~years)+
  labs(x = "Number of features")

# GERMAN CARS BY TRANSMISSION TYPE BY EVERY YEAR (bar plot)
ggplot(df[
  df$years >= 1990 & df$years < 2021 &
    df$Preis < 100000 &
    df$Kilometerstand < 750000 &
    (df$Getriebe == "Automatik" | df$Getriebe == "Schaltgetriebe")
  ,]) +
  aes(x = years, fill=Getriebe) + # , color= Getriebe
  geom_bar()

# Price per year and type
ggplot(df[
  df$years >= 2000 & df$years < 2021 &
  (df$carType == "Kombi" | df$carType == "Limousine")
  ,], aes(x=factor(years), y=Preis), fill = red) + stat_summary(fun.y="mean", geom="bar") +
  facet_wrap(~carType)

# JAKO BITNOO
# Price per year and type 1980-2000
ggplot(df[
  df$years >= 1980 & df$years < 2001 &
    (df$carType == "Kombi" | df$carType == "Limousine")
  ,], aes(x=factor(years), y=Preis), fill = carType, color = carType) + stat_summary(fun.y="mean", geom="bar") +
  scale_color_manual(values = c("Kombi" = "#5dff00", "Limousine" = "#a200ff")) +
  scale_fill_manual(values = c("Kombi" = "#5dff00", "Limousine" = "#a200ff")) +
  facet_wrap(~carType) +
  labs(x = "Years (1980-2000)")

# JAKO BITNOO
# Price per year for taxis 2010-2021
ggplot(df[
  df$years >= 2010 & df$years < 2021 &
  df$carType == "Kombi"
  ,], aes(x=factor(years), y=Preis), fill = carType, color = carType) + stat_summary(fun.y="mean", geom="bar") +
  facet_wrap(~Taxi)

# JAKO BITNOO
# Kilometers per year for taxis 2010-2021
ggplot(df[
  df$years >= 2010 & df$years < 2021 &
  df$carType == "Kombi"
  ,], aes(x=factor(years), y=Kilometerstand), fill = carType, color = carType) + stat_summary(fun.y="mean", geom="bar") +
  facet_wrap(~Taxi)

# GERMAN CARS BY CAR TYPE BY EVERY YEAR (POINT PLOT)
ggplot(df[
  df$years >= 2000 & df$years < 2021 &
    df$Preis < 100000 &
    df$Kilometerstand < 750000
  ,]) +
  aes(x=Preis, y = Kilometerstand,fill=carType, color= carType) +
  geom_point()

# density plot za gasoline vs diesel po features
ggplot(df[
  df$years >= 2010 & df$years < 2019 &
    df$Preis < 100000 &
    df$Kilometerstand < 500000 &
    df$fuelSorted == "Diesel"
  ,]) +
  aes(x = numoffeatures, fill = numoffeatures, color="Red") +
  #geom_point() +
  #geom_smooth() +
  geom_density(color="Red") +
  geom_density(data = df[
    df$years >= 2010 & df$years < 2019 &
      df$fuelSorted == "Benzin" &
      df$Kilometerstand < 500000 &
      df$Preis < 100000
    ,], 
    mapping = aes(numoffeatures, fill=numoffeatures), color = "Green") +
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )+
  #facet_wrap(~years)+
  labs(x = "Number of features")

# leather gasoline vs diesel
# ggplot() +
#   geom_point(data = df[
#     df$years >=2010 & df$years <= 2020 &
#     df$fuelSorted == "Diesel"
#     ,],
#     aes(y = Preis, x = Kilometerstand, color = interior, fill = interior), shape = 2) +
#   geom_point(data = df[
#     df$years >=2010 & df$years <= 2020 &
#     df$fuelSorted == "Benzin"
#     ,],
#     aes(y = Preis, x = Kilometerstand, color = interior, fill = interior), shape = 1) 


((table(df[df$fuelSorted == "Diesel",]$interior))/length(df[df$fuelSorted == "Diesel",]$interior))*100
((table(df[df$fuelSorted == "Benzin",]$interior))/length(df[df$fuelSorted == "Benzin",]$interior))*100
((table(df[df$years == 2020, ]$awd))/length(df[df$years == 2020, ]$awd))*100

mean(df[df$years >= 2010 & df$years < 2021 & df$awd == "4Matic" & df$Kilometerstand > 300000 & df$carType == "Kombi" , ]$Preis)
mean(df[df$years >= 2010 & df$years < 2021 & df$awd != "4Matic" & df$Kilometerstand > 300000 & df$carType == "Kombi", ]$Preis)

mean(df[df$years >= 2010 & df$years < 2021 & df$awd == "4Matic" & df$Kilometerstand > 300000 & df$carType == "Limousine", ]$Preis)
mean(df[df$years >= 2010 & df$years < 2021 & df$awd != "4Matic" & df$Kilometerstand > 300000 & df$carType == "Limousine", ]$Preis)


mean(df[df$years >= 2010 & df$years < 2011 & df$awd == "4Matic" & df$Kilometerstand > 300000 & df$carType == "Kombi", ]$Preis)
mean(df[df$years >= 2010 & df$years < 2011 & df$awd != "4Matic" & df$Kilometerstand > 300000 & df$carType == "Kombi", ]$Preis)

mean(df[df$years >= 2010 & df$years < 2011 & df$awd == "4Matic" & df$Kilometerstand > 300000 & df$carType == "Limousine", ]$Preis)
mean(df[df$years >= 2010 & df$years < 2011 & df$awd != "4Matic" & df$Kilometerstand > 300000 & df$carType == "Limousine", ]$Preis)


table(df$fuelSorted)/length(df$fuelSorted) * 100
table(olx$fuel)[2:5]/length(olx$fuel) * 100



for(i in 2010:2020){
  print(i)
  print((((table(df[df$awd == "4Matic" & df$years == i,]$interior))
   /length(df[df$awd == "4Matic" & df$years == i,]$interior))*100)[c("Vollleder", "Teilleder", "Stoff", "Alcantara")])
}

(((table(df[df$awd == "4Matic" & df$years >= 2010,]$interior))/length(df[df$awd == "4Matic" & df$years >= 2010,]$interior))*100)

(((table(df[df$awd != "4Matic" & df$years >= 2010,]$interior))/length(df[df$awd != "4Matic" & df$years >= 2010,]$interior))*100)


ggplot(df[
  (df$Kraftstoffart == "Diesel") &
  (df$Kilometerstand < 500000) &
  (df$years >= 2009) &
  (df$years <= 2017),
])+
  aes(Kilometerstand,  fill=Kilometerstand) +
  geom_density(color="Green")+
  geom_density(data =df[
    (df$Kraftstoffart == "Benzin") &
    (df$Kilometerstand < 500000) &
    (df$years >= 2009) &
    (df$years <= 2017),
  ], 
  mapping = aes(Kilometerstand, fill=Kilometerstand), color = "Red") +
  theme_bw() + 
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(title =" ", x = "Mileage in kms", y = "Frequency") +
  facet_wrap(~years)

mean(df[
  df$Kilometerstand > 325000 &
    df$Preis < 100000 &
    df$years >= 2010 &
    (df$carType == "Kombi")&
    df$awd == "4Matic"
  , ]$Preis)

mean(df[
  df$Kilometerstand > 325000 &
    df$Preis < 100000 &
    df$years >= 2010 &
    (df$carType == "Kombi")&
    df$awd != "4Matic"
  , ]$Preis)

## JAKO BITNOOO !!!!
ggplot(data = df[
  df$Kilometerstand < 350000 &
    df$Preis < 100000 &
    df$years >= 2010 &
    (df$carType == "Kombi" | df$carType == "Limousine")
  , ]) + 
  aes(x = Kilometerstand, y = Preis, color= awd) + 
  #geom_point(size = 2.5)+
  geom_smooth() +
  labs(y = "Price in EUR", x = "Mileage in kms") + 
  facet_wrap(~carType)

# razliciti modeli na mobile.de
ggplot(df[
  df$years >= 2000 &
  df$years <= 2020 &
  !is.na(df$Hubraum) &
  df$Hubraum > 1490 &
  (df$fuelSorted == "Diesel" | df$fuelSorted == "Benzin")
, ]) + 
  geom_bar(
    aes(x = years, fill = as.factor(Hubraum), color= as.factor(Hubraum)),
    position = position_dodge2(width = 0.9, preserve = "single"),
    width = 0.8
  ) + 
  facet_wrap(~fuelSorted) #gsub("([0-9]+).*$", "\\1", df$Name[2])





