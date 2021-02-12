options(scipen = 999)   

#graf1
ggplot(data = olx[!is.na(olx$price) & olx$prodYear > 1995, ]) + aes(price ,prodYear, color = model) + geom_point()

#graf2 kilometraza modela e200 i e220
# bosna vs njemacka
ggplot(data = olx[!is.na(olx$price) & olx$prodYear == 2010 & (olx$model == "E 220" | olx$model == "E 200"), ])+
  aes(mileage) +
  geom_density()

# df[(df$years > 1995) & (df$Kraftstoffart == "Diesel") & (df$Hubraum > 1800 & df$Hubraum < 2250),]$Name
# selected years

ggplot(df[(df$years == 2010) & (df$Kraftstoffart == "Diesel") & (df$Hubraum > 1800 & df$Hubraum < 2250) & (df$Kilometerstand < 600000),])+
  aes(Kilometerstand) +
  geom_density()

#graf2 kilometraza modela e200 i e220 FACETT PO GODINAMAAA!!!
# bosna vs njemacka
ggplot(data = olx[!is.na(olx$price) & (olx$prodYear >= 2010) & (olx$prodYear <= 2018) & (olx$model == "E 220" | olx$model == "E 200"), ])+
  aes(mileage) +
  geom_density() +
  facet_wrap(~prodYear)

# df[(df$years > 1995) & (df$Kraftstoffart == "Diesel") & (df$Hubraum > 1800 & df$Hubraum < 2250),]$Name
# selected years

ggplot(df[
  !is.na(df$years) &
    !is.na(df$Hubraum)  &
    (df$Kraftstoffart == "Diesel") &
    (df$Hubraum > 1800 & df$Hubraum < 2250) &
    (df$Kilometerstand < 650000) &
    (df$years >= 2010) &
    (df$years <= 2018),
])+
  aes(Kilometerstand) +
  geom_density() +
  facet_wrap(~years)

#graf2 kilometraza modela e200 i e220 USPOREDBA!!!
# Po kilometrazi izmedju 2010 i 2018
# bosna vs njemacka
ggplot(data = olx[
  !is.na(olx$price) &
  (olx$prodYear >= 2010) & 
  (olx$prodYear <= 2018) & 
  (olx$model == "E 220" | olx$model == "E 200") &
  (olx$mileage < 600000), 
  ])+
  aes(mileage) +
  geom_density(color="black", fill="green") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

##Germany
ggplot(df[
  !is.na(df$years) &
    !is.na(df$Hubraum)  &
    (df$Kraftstoffart == "Diesel") &
    (df$Hubraum > 1800 & df$Hubraum < 2250) &
    (df$Kilometerstand < 600000) &
    (df$years >= 2010) &
    (df$years <= 2018),
])+
  aes(Kilometerstand) +
  geom_density(color="black", fill="green")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## GRAF KILOMETRAZE JEDAN PORED DRUGOG

ggplot(df[
  !is.na(df$years) &
    !is.na(df$Hubraum)  &
    (df$Kraftstoffart == "Diesel") &
    (df$Hubraum > 1800 & df$Hubraum < 2250) &
    (df$Kilometerstand < 600000) &
    (df$years >= 2010) &
    (df$years <= 2018),
])+
  aes(Kilometerstand,  fill=Kilometerstand) +
  geom_density(color="Red")+
  geom_density(data = olx[
    !is.na(olx$price) &
      (olx$prodYear >= 2010) & 
      (olx$prodYear <= 2018) & 
      (olx$model == "E 220" | olx$model == "E 200") &
      (olx$mileage < 600000), 
  ], 
               mapping = aes(mileage, fill=mileage), color = "Green") +
  theme_bw() + 
  theme(
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     axis.line = element_line(colour = "black")
                     ) +
  labs(title =" ", x = "Mileage in kms", y = "Frequency")

## Regression line fitted to base models

#DEU
ggplot(data = df[
  df$Preis < 37500 &
  df$Kilometerstand < 750000 &
  df$Kraftstoffart == "Diesel" &
  !is.na(df$Leistungkw) &
  df$Leistungkw < 150 &
  df$years <= 2015 &
  df$years >= 2010 &
  !is.na(df$Hubraum) &
  df$Kategorie != "Andere" &
  (df$Hubraum > 1800 & df$Hubraum < 2250) 
  ,]) + 
  aes(x = Kilometerstand, y = Preis, color = Kategorie) + 
  geom_point() +
  geom_smooth()

#BOS
ggplot(data = olx[
  !is.na(olx$price) &
    (olx$prodYear >= 2010) & 
    (olx$prodYear <= 2015) & 
    (olx$model == "E 220" | olx$model == "E 200") &
    (olx$mileage < 600000), 
]) + 
  aes(x = mileage, y = price, color = prodYear) + 
  geom_point() +
  geom_smooth()


# above two graphs combined
df$PreisBosnian <- df$Preis * 2.5

ggplot() + 
  geom_point(data = df[
      df$Kilometerstand < 600000 &
      df$Preis < 100000 &
      df$Kraftstoffart == "Diesel" &
      df$years <= 2020 &
      df$years >= 2000
    ,], mapping = aes(x = Kilometerstand, y = Preis), color = "#0300ff", size = 0.8) +
  geom_point(data = df[
      df$Kilometerstand < 600000 &
      df$Preis < 100000 &
      df$Kraftstoffart == "Benzin" &
      df$years <= 2020 &
      df$years >= 2000
    ,], mapping = aes(x = Kilometerstand, y = Preis), color = "#fcff00", size = 0.6) +
  geom_smooth(data = df[
      df$Kilometerstand < 600000 &
      df$Preis < 100000 &
      df$Kraftstoffart == "Benzin" &
      df$years <= 2020 &
      df$years >= 2000
    ,], mapping = aes(x = Kilometerstand, y = Preis), color = "gold") +
  geom_smooth(data = df[
      df$Kilometerstand < 600000 &
      df$Preis < 100000 &
      df$Kraftstoffart == "Diesel" &
      df$years <= 2020 &
      df$years >= 2000
    ,], mapping = aes(x = Kilometerstand, y = Preis), color = "dark blue")


## Viermatics vs others

# kilometers and price
ggplot(data = df[
  !is.na(df$Allradantrieb) &
  df$Kilometerstand < 500000 &
  df$Preis < 100000 &
  df$years >= 2010 &
  df$Kraftstoffart == "Diesel"
  , ]) + 
  aes(x = Kilometerstand, y = Preis, color=years) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Allradantrieb)

# kilometers density plot
ggplot(data = df[
  !is.na(df$Allradantrieb) &
    df$Kilometerstand < 500000 &
    df$Preis < 100000 &
    df$years >= 2010 &
    df$Kraftstoffart == "Diesel"
  , ]) + 
  aes(x = Kilometerstand) +
  geom_density() +
  facet_wrap(~Allradantrieb)

# price density plot
ggplot(data = df[
  !is.na(df$Allradantrieb) &
    df$Kilometerstand < 500000 &
    df$Preis < 100000 &
    df$years >= 2010 &
    df$Kraftstoffart == "Diesel"
  , ]) + 
  aes(x = Preis, fill = Kraftstoffart, color = Kraftstoffart) +
  geom_density() +
  facet_wrap(~Allradantrieb)

# some other experiments

#Price
ggplot(data = df[
  !is.na(df$Allradantrieb) &
    df$Kilometerstand < 500000 &
    df$Preis < 100000 &
    df$years >= 2010
  , ], aes(x = Preis, fill = Allradantrieb, color = Allradantrieb)) + 
  geom_density(alpha = 0.2, na.rm = TRUE) +
  facet_wrap(~years)

ggplot(data = df[
  !is.na(df$Allradantrieb) &
    df$Kilometerstand < 500000 &
    df$Preis < 100000 &
    df$years == 2011
  , ], aes(x = Kilometerstand, fill = Allradantrieb, color = Allradantrieb)) + 
  geom_density(alpha = 0.2) 

## BAR CHARTS
ggplot(df[
  !is.na(df$Allradantrieb) &
    df$Kilometerstand < 500000 &
    df$Preis < 100000 &
    df$years >= 2010 
  , ], aes(Kilometerstand, Preis, color = Kraftstoffart, fill = Kraftstoffart)) + 
  geom_bar(stat = "summary_bin", fun = mean)

ggplot(df[
  !is.na(df$Allradantrieb) &
    df$Kilometerstand < 500000 &
    df$Preis < 100000 &
    df$years >= 2010 
  , ], aes(Kilometerstand, Preis, color = Kraftstoffart, fill = Kraftstoffart)) + 
  geom_bar(stat = "summary_bin", fun = mean)

## JAAAKO BIITAN GRAAAF
## BROOJ AUTA PO GODINAMA
## DEUU
ggplot(df[
  df$years >= 1975 & df$years < 2000
,]) + 
  geom_bar(aes(x = as.character(years), fill = years, color= years), position = "dodge", width = 0.8) + 
  labs(x = "year")

ggplot(olx[
  olx$prodYear >= 2000 & olx$prodYear < 2021
,]) + 
  geom_bar(aes(x = as.character(prodYear), fill = prodYear, color= prodYear), position = "dodge", width = 0.8) + 
  labs(x = "year")

# GERMAN CARS BY FUEL TYPE BY EVERY YEAR
ggplot(df[
  df$years >= 1980 & df$years < 2000
  ,]) + 
  geom_bar(aes(x = as.character(years), fill = fuelSorted, color= fuelSorted), position = "dodge", width = 0.8) + 
  labs(x = "year")

# GERMAN CARS BY CAR TYPE BY EVERY YEAR
ggplot(df[
  df$years >= 2000 & df$years < 2021
  ,]) + 
  geom_bar(aes(x = as.character(years), fill = carType, color= carType), position = "dodge", width = 0.8) + 
  labs(x = "year")

# GERMAN CARS BY TRANSMISSION TYPE BY EVERY YEAR
ggplot(df[
  df$years >= 1980 & df$years < 2021
  ,]) + 
  geom_bar(aes(x = as.character(years), fill = Getriebe, color= Getriebe), position = "dodge", width = 0.8) + 
  labs(x = "year")

# GERMAN CARS BY CAR TYPE BY EVERY YEAR (POINT PLOT)
ggplot(df[
  df$years >= 2000 & df$years < 2021 &
  df$Preis < 100000 &
  df$Kilometerstand < 750000
  ,]) +
  aes(x=Preis, y = Kilometerstand,fill=carType, color= carType) +
  geom_point()

# GERMAN CARS BY FUEL TYPE BY EVERY YEAR (POINT PLOT)
ggplot(df[
  df$years >= 2000 & df$years < 2021 &
    df$Preis < 100000 &
    df$Kilometerstand < 750000
  ,]) +
  aes(x=Preis, y = Kilometerstand,fill=fuelSorted, color= fuelSorted) +
  geom_point()

# GERMAN CARS BY TRANSMISSION TYPE BY EVERY YEAR (POINT PLOT)
ggplot(df[
  df$years >= 2000 & df$years < 2021 &
    df$Preis < 100000 &
    df$Kilometerstand < 750000
  ,]) +
  aes(x=Preis, y = Kilometerstand,fill=Getriebe, color= Getriebe) +
  geom_point()

# GERMAN CARS BY COLOR AND TYPE
ggplot(df[
  df$years >= 2000 & df$years < 2021 &
  df$carType == "Kombi"
  ,]) + 
  geom_bar(aes(x = as.character(years), y = Preis, fill = Preis, color= Preis), position = "dodge", width = 0.8)+ 
  labs(x = "year")

# Price per year
ggplot(df[
  df$years >= 2000 & df$years < 2021
,], aes(x=factor(years), y=Preis), fill = red) + stat_summary(fun.y="mean", geom="bar") +
  facet_wrap(~carType)

# Price graphing per year
ggplot(data = df[
  !is.na(df$Allradantrieb) &
    df$Kilometerstand < 500000 &
    df$Preis < 100000 &
    df$years >= 2010
  , ]) + 
  aes(x = Kilometerstand, y = Preis, color=fuelSorted) +
  geom_point() +
  facet_wrap(~years)

ggplot(data = df[
  !is.na(df$Allradantrieb) &
    df$Kilometerstand < 500000 &
    df$Preis < 100000 &
    df$years >= 2010
  , ]) + 
  aes(x = Kilometerstand, y = Preis, color=carType) +
  geom_point() +
  facet_wrap(~years)

## with two data fuel loess lines
ggplot(data = df[
  !is.na(df$Allradantrieb) &
    df$Kilometerstand < 500000 &
    df$Preis < 100000 &
    df$years >= 2010 &
    (df$fuelSorted == "Diesel" | df$fuelSorted == "Benzin")
  , ]) + 
  aes(x = Kilometerstand, y = Preis) +
  geom_point() +
  geom_smooth(data = df[
    !is.na(df$Allradantrieb) &
      df$Kilometerstand < 500000 &
      df$Preis < 100000 &
      df$years >= 2010 &
      (df$fuelSorted == "Diesel" | df$fuelSorted == "Benzin")
    , ],
    mapping = aes(x = Kilometerstand, y = Preis, color=fuelSorted))

## density plot of kilometers diesel vs gasoline
ggplot(df[
  !is.na(df$years) &
    !is.na(df$Hubraum)  &
    (df$Kraftstoffart == "Diesel") &
    (df$Hubraum > 1800 & df$Hubraum < 2250) &
    (df$Kilometerstand < 200000) &
    (df$years >= 2010) &
    (df$years <= 2020),
])+
  aes(Kilometerstand,  fill=Kilometerstand) +
  geom_density(color="Red")+
  geom_density(data = df[
    !is.na(df$years) &
      !is.na(df$Hubraum)  &
      (df$Kraftstoffart == "Benzin") &
      (df$Hubraum > 1800 & df$Hubraum < 2250) &
      (df$Kilometerstand < 200000) &
      (df$years >= 2010) &
      (df$years <= 2020),
  ],
  mapping = aes(Kilometerstand, fill=Kilometerstand), color = "Green") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(title =" ", x = "Mileage in kms", y = "Frequency")

# Price graphing per year
ggplot(data = df[
  !is.na(df$Allradantrieb) &
  df$Kilometerstand < 500000 &
  df$Preis < 100000 &
  df$years >= 2010 &
  (df$fuelSorted == "Diesel" | df$fuelSorted == "Benzin")
  , ]) + 
  aes(x = Kilometerstand, y = Preis, color=fuelSorted) + # #ff000d
  geom_point() +
  scale_color_manual(values = c("Diesel" = "dark blue", "Benzin" = "brown")) +
  scale_fill_manual(values = c("Diesel" = "dark blue", "Benzin" = "brown")) +
  facet_wrap(~years)

# VIERMATIC AGAIN

ggplot(data = df[
    df$Kilometerstand < 500000 &
    df$Preis < 100000 &
    df$years >= 2010 &
    (df$fuelSorted == "Diesel" | df$fuelSorted == "Benzin")
  , ]) + 
  aes(x = Kilometerstand, y = Preis, color= awd) + 
  geom_point(size = 2.5) +
  facet_wrap(~years)

ggplot(data = df[
  df$Kilometerstand < 450000 &
    df$Preis < 100000 &
    df$years >= 2010 &
    (df$fuelSorted == "Diesel" | df$fuelSorted == "Benzin")
  , ]) + 
  aes(x = Kilometerstand, y = Preis, color= awd) + 
  #geom_point(size = 2.5)+
  geom_smooth() +
  labs(y = "Price in EUR", x = "Mileage in kms")

# Viermatic facetting according to the car types
ggplot(data = df[
  df$Kilometerstand < 400000 &
    df$Preis < 100000 &
    df$years >= 2010 &
    (df$fuelSorted == "Diesel" | df$fuelSorted == "Benzin")
  , ]) + 
  aes(x = Kilometerstand, y = Preis, color= awd) + 
  #geom_point(size = 2.5)+
  geom_smooth() +
  labs(y = "Price in EUR", x = "Mileage in kms") + 
  facet_wrap(~carType)

# Viermatic facetting according to the years ##### THIS ONEEE!!!!
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

# Viermatic facetting according to the years and fuel (regression line)
ggplot(data = df[
  df$Kilometerstand < 350000 &
    df$Preis < 100000 &
    df$years >= 2010 &
    (df$carType == "Kombi" | df$carType == "Limousine") &
    (df$fuelSorted == "Diesel")
  , ]) + 
  aes(x = Kilometerstand, y = Preis, color= awd) + 
  #geom_point(size = 2.5)+
  geom_smooth() +
  labs(y = "Price in EUR", x = "Mileage in kms") + 
  facet_wrap(~carType)

# Viermatic facetting according to the years and fuel (scatter plot)
ggplot(data = df[
  df$Kilometerstand < 350000 &
    df$Preis < 100000 &
    df$years >= 2000 &
    (df$carType == "Kombi" | df$carType == "Limousine") &
    (df$fuelSorted == "Diesel")
  , ]) + 
  aes(x = Kilometerstand, y = Preis, color= awd) + 
  geom_point(size = 2.5)+
  #geom_smooth() +
  labs(y = "Price in EUR", x = "Mileage in kms") + 
  facet_wrap(~carType)

# GERMAN CARS BY COLOR AND TYPE
ggplot(df[
  df$years >= 2000 & df$years < 2021 &
  df$model != "" &
  (df$fuelSorted == "Diesel" | df$fuelSorted == "Benzin")
  ,]) + 
  geom_bar(aes(x = years, fill = model, color= model), position = "stack", width = 0.8)+ 
  labs(x = "year") +
  facet_wrap(~fuelSorted)

# GERMAN CARS BY COLOR AND TYPE (diesel)
ggplot(df[
  df$years >= 2000 & df$years < 2021 &
    df$model != "" &
    df$model != "E 63" &
    df$fuelSorted == "Diesel"
  ,]) + 
  geom_bar(aes(x = years, fill = model, color= model), position = "fill", width = 0.8)+ 
  labs(x = "year")

# GERMAN CARS BY WHETHER THEY ARE TAXI (diesel)
ggplot(df[
  df$years >= 2010 & df$years < 2021 &
    df$model != "" &
    df$model != "E 63" &
    (df$model == "E 200" | df$model == "E 220" | df$model == "") &
    df$fuelSorted == "Diesel"
  ,]) + 
  geom_bar(aes(x = as.factor(years), fill = Taxi, color= Taxi), position = "fill", width = 0.8)+ 
  labs(x = "year")

# BOSNIAN CARS BY WHETHER THEY ARE TAXI (diesel)
ggplot(olx[
  olx$fuel == "Dizel" &
  olx$prodYear >= 2010 & olx$prodYear <= 2020 &
  (olx$model == "E 200" | olx$model == "E 220")
  ,]) + 
  geom_bar(aes(x = as.factor(prodYear), fill = Taxi, color= Taxi), position = "fill", width = 0.8)+ 
  labs(x = "year")

# BOSNIAN CARS BY COLOR AND TYPE (diesel)
ggplot(olx[
    olx$prodYear >= 2000 & olx$prodYear <= 2020
  ,]) + 
  #geom_bar(aes(x = prodYear, fill = fuel, color= fuel), position = "fill", width = 0.8)+
  geom_density(aes(x = prodYear)) + 
  labs(x = "year") 

table(df[df$fuelSorted == "Diesel" & df$model != "" & df$model != "E 63", ]$model)/length(df[df$fuelSorted == "Diesel" & df$model != "" & df$model != "E 63", ]$model)

# GERMAN CARS BY TRANSMISSION
ggplot(df[
  df$years >= 1990 & df$years < 2021 &
  !is.na(df$Getriebe) &
  df$Getriebe != "" &
  df$Getriebe != "Halbautomatik"
  ,]) + 
  geom_bar(aes(x = as.factor(years), fill = Getriebe, color= Getriebe), position = "fill", width = 0.8)+ 
  labs(x = "year")

mean(
  df[
    (df$model == "E 200" | df$model == "E 220") &
    df$years == 2016 &
    df$Taxi == "Taxi"
    ,
  ]$Preis
)

mean(
  df[
    (df$model == "E 200" | df$model == "E 220") &
    df$years == 2016 &
    df$Taxi != "Taxi"
    ,
  ]$Preis
)

table(df[df$Taxi == "Taxi",]$interior)/ length(df[df$Taxi == "Taxi",]$interior) * 100

table(df[df$Taxi != "Taxi",]$interior)/ length(df[df$Taxi != "Taxi",]$interior) * 100

table(
  df[
    df$years == 2020 
    ,]$fuelSorted
)/length(
  df[
    df$years == 2020 
    ,]$fuelSorted
)*100

table(
  df[
    df$years == 2020 
    ,]$awd
)/length(
  df[
    df$years == 2020 
    ,]$awd
)*100













