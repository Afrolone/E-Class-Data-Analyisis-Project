
table(df$Kraftstoffart)

dieselOrGasoline <- function(it) {
  res = ""
  if(
    grepl("Diesel", it, fixed=TRUE) &&
    grepl("Hybrid", it, fixed=TRUE)) {
    res = "Diesel Hybrid"
  } else if (grepl("Diesel", it, fixed=TRUE)) {
    res = "Diesel"
  } else if(
    grepl("Benzin", it, fixed=TRUE) &&
    grepl("Hybrid", it, fixed=TRUE)
  ) {
    res = "Benzin Hybrid"
  } else if(
    grepl("Benzin", it, fixed=TRUE)
  ) {
    res = "Benzin"
  } else if(
    grepl("LPG", it, fixed=TRUE) ||
    grepl("CNG", it, fixed=TRUE)
  ) {
    res = "LPG/CNG"
  } else {
    res= "Andere"
  }
  return(res)
}

# the fucntion argument is the Kraftstoffart
# column of mobile.de dataframe

newFuelVec <- function(vec){
  newVec <- ""
  for(i in 1: length(vec)){
    newVec[i] <- dieselOrGasoline(vec[i])
  }
  return(newVec)
}

getCarType <- function(it){
  res = ""
  if(
    grepl("Limousine", it, fixed=TRUE)
    ) {
    res = "Limousine"
  } else if (
    grepl("Kombi", it, fixed=TRUE)
    ) {
    res = "Kombi"
  } else if (
    grepl("Sportwagen", it, fixed=TRUE) ||
    grepl("Coupé", it, fixed=TRUE) ||
    grepl("Cabrio", it, fixed=TRUE) ||
    grepl("Roadster", it, fixed=TRUE)
  ) {
    res = "Coupe/Cabrio"
  }
    else {
    res= "Andere"
  }
  return(res)
}
newCarTypeVec <- function(vec){
  newVec <- ""
  for(i in 1: length(vec)){
    newVec[i] <- gerCarType(vec[i])
  }
  return(newVec)
}

# This function takes the whole
# dataframe as argument and returns
# a vector containing whether a 
# car at a particular index is a
# taxi or not
taxiColor <- function(df){
  res <- NULL
  for(i in 1 : length(df$Name)) {
    print(i)
    if (grepl("hellelfenbein", df$FarbeHersteller[i], fixed=TRUE)) { #HELLELFENBEIN
      res[i] = "Taxi" 
    } else if (grepl("HELLELFENBEIN", df$FarbeHersteller[i], fixed=TRUE)) {
      res[i] = "Taxi"
    } else if (grepl("Beige", df$Farbe[i], fixed=TRUE)) {
      res[i] = "Taxi"
    } else if (grepl("Taxi", df$Name[i], fixed=TRUE)) {
      res[i] = "Taxi"
    } else {
      res[i] = "Not Taxi"
    }
  }
  return(res)
}

df$fuelSorted <- newFuelVec(df$Kraftstoffart)
df$carType <- newCarTypeVec(df$Kategorie)

awd <- function(df) {
  res <- NULL
  for(i in 1 : length(df$Name)) {
    print(i)
    if (!is.na(df$Allradantrieb[i])) {
      if(df$Allradantrieb[i] == TRUE) {
        res[i] = "4Matic" 
      } else if (grepl("4Matic", df$Name[i], fixed=TRUE)) {
        res[i] = "4Matic"
      } else if (grepl("4MATIC", df$Name[i], fixed=TRUE)) {
        print(df$Name[i])
        res[i] = "4Matic"
      } else if (grepl("Allradantrieb", df$Name[i], fixed=TRUE)) {
        print(df$Name[i])
        res[i] = "4Matic"
      } else if (grepl("Allrad", df$Name[i], fixed=TRUE)) {
        print(df$Name[i])
        res[i] = "4Matic"
      } else if (grepl("ALLRADANTRIEB", df$Name[i], fixed=TRUE)) {
        print(df$Name[i])
        res[i] = "4Matic"
      } else if (grepl("ALLRAD", df$Name[i], fixed=TRUE)) {
        print(df$Name[i])
        res[i] = "4Matic"
      } else {
        res[i] = "Not 4Matic"
      }
    } else {
      if (grepl("4Matic", df$Name[i], fixed=TRUE)) {
        res[i] = "4Matic"
      } else if (grepl("4MATIC", df$Name[i], fixed=TRUE)) {
        print(df$Name[i])
        res[i] = "4Matic"
      } else if (grepl("Allradantrieb", df$Name[i], fixed=TRUE)) {
        print(df$Name[i])
        res[i] = "4Matic"
      } else if (grepl("Allrad", df$Name[i], fixed=TRUE)) {
        print(df$Name[i])
        res[i] = "4Matic"
      } else if (grepl("ALLRADANTRIEB", df$Name[i], fixed=TRUE)) {
        print(df$Name[i])
        res[i] = "4Matic"
      } else if (grepl("ALLRAD", df$Name[i], fixed=TRUE)) {
        print(df$Name[i])
        res[i] = "4Matic"
      } else {
        res[i] = "Not 4Matic"
      }
    }
  }
  return(res)
}

allrad <- awd(df)

df$awd <- allrad

olxAllrad <- function(olx){
  res = NULL # "Sva četiri"
  for(i in 1: length(olx$name)){
    if(olx$layout[i] == ""){
      if(grepl("4MATIC", olx$name[i], fixed=TRUE)){
        res[i] = "Sva četiri"
      } else if(grepl("4matic", olx$name[i], fixed=TRUE)) {
        res[i] = "Sva četiri"
      } else if(grepl("4x4", olx$name[i], fixed=TRUE)) {
        res[i] = "Sva četiri"
      } else if(grepl("4matik", olx$name[i], fixed=TRUE)) {
        res[i] = "Sva četiri"
      } else {
        res[i] = "Zadnji"
      }
    } else {
      res[i] = olx$layout[i]
    }
  }
  return(res)
}
olxTaxi <- function(olx){
  res = NULL
  for (i in 1:length(olx$name)){
    if(olx$color[i] == "Žuta"){
      res[i] = "Taxi"
    } else {
      if(grepl("taksi", olx$name[i], fixed=TRUE)) {
        res[i] = "Taxi"
      } else if(grepl("Taksi", olx$name[i], fixed=TRUE)) {
        res[i] = "Taxi"
      } else if(grepl("TAKSI", olx$name[i], fixed=TRUE)) {
        res[i] = "Taxi"
      } else if(grepl("Taxi", olx$name[i], fixed=TRUE)) {
        res[i] = "Taxi"
      } else if(grepl("taxi", olx$name[i], fixed=TRUE)) {
        res[i] = "Taxi"
      } else if(grepl("TAXI", olx$name[i], fixed=TRUE)) {
        res[i] = "Taxi"
      } else {
        res[i] = "Not a taxi"
      }
    }
  }
  return(res)
}

olxallradvar <- olxAllrad(olx)

olx$layout <- olxallradvar


olxTaxiVar <- olxTaxi(olx)

dfInterior <- function(df) {
  res <- NULL
  for (i in 1:length(df$Name)){
    print(i)
    if (is.null(df$Innenausstattung[i])){
      res[i] = "Undefiniert"
    } else {
      if(is.na(df$Innenausstattung[i])){
        res[i] = "Undefiniert"
      } else {
        if(df$Innenausstattung[i] == "") {
          res[i] = "Undefiniert"
        } else if(grepl("Alcantara", df$Innenausstattung[i], fixed=TRUE)) {
          res[i] = "Alcantara"
        } else if (grepl("Andere", df$Innenausstattung[i], fixed=TRUE)) {
          res[i] = "Andere"
        } else if (grepl("Stoff", df$Innenausstattung[i], fixed=TRUE)) {
          res[i] = "Stoff"
        } else if (grepl("Teilleder", df$Innenausstattung[i], fixed=TRUE)) { #
          res[i] = "Teilleder"
        } else if (grepl("Velours", df$Innenausstattung[i], fixed=TRUE)) {
          res[i] = "Velours"
        } else if (grepl("Vollleder", df$Innenausstattung[i], fixed=TRUE)) {
          res[i] = "Vollleder"
        } else {
          res[i] = "Andere"
        }
      }
    }
  }
  return(res)
}


df$interior <- interior

dfNumOfFeatures <- function(df) {
  res <- 0
  for (i in 1 : length(df$Name)){ # 27:128
    res[i] <- 0
    for(j in 27 : 128){
      if(!is.null(df[i,j])){
        if(!is.na(df[i,j])){
          if(df[i, j] == TRUE){
            res[i] <- res[i] + 1
          } 
        }
      }
    }
    print(paste0("result for ", i , " is: ", res[i]))
  }
  return(res)
}

numoffeatures <- dfNumOfFeatures(df)

pie(
  table(
    df[
      df$awd == "4Matic"
      ,]$interior
  )
)

pie(
  table(
    df[
      df$Taxi != "Taxi"
      ,]$interior
  )
)

df$numoffeatures <- numoffeatures

dfMODEL <- function(df){
  res <- NULL
  x <- NULL
  for(i in 1:length(df$Name)) {
    x <- gsub("([0-9]+).*$", "\\1", df$Name[i])
    res[i] <- substr(x, nchar(x)-5, nchar(x))
    if(grepl("E", res[i]) || grepl("e", res[i])){
      if(grepl("E", res[i])) {
        res[i] <- strsplit(res[i], "E")[[1]][2]
        res[i] <- substr(res[i], nchar(res[i])-2, nchar(res[i]))
      }
      if(grepl("e", res[i])) {
        res[i] <- strsplit(res[i], "e")[[1]][2]
        res[i] <- substr(res[i], nchar(res[i])-2, nchar(res[i]))
      }
    } else {
      res[i] <- substr(res[i], nchar(res[i])-3, nchar(res[i]))
    }
    print(res[i])
  }
  return(res)
}



modl <- dfMODEL(df)

for(i in 1: length(modl)){
  if(nchar(modl[i]) != 3){
    #begin
    if(nchar(modl[i]) == 2){
      if (modl[i] == "63" || modl[i] == "63"){
        modl[i] = modl[i]
      } else {
        modl[i] = ""
      }
    } else {
      if(substr(modl[i], nchar(modl[i]), nchar(modl[i])) == "0"){
        if(substr(modl[i], nchar(modl[i])-1, nchar(modl[i])-1) == "1") {
          modl[i] = ""
        } else
          modl[i] = substr(modl[i], nchar(modl[i])-2, nchar(modl[i]))
      } else {
        if(substr(modl[i], nchar(modl[i])-1, nchar(modl[i])) == "63" || substr(modl[i], nchar(modl[i])-1, nchar(modl[i])) == "55"){
          modl[i] = substr(modl[i], nchar(modl[i])-1, nchar(modl[i]))
        } else {
          modl[i] = ""
        }
      }
    }
    #end
  }
}

table(modl)

namesVec <- names(table(modl))

df$model <- modl

df[df$model == "850",]$Name

namesVec <- namesVec[c(8, 9, 10, 11, 12, 24, 30, 31, 32, 33,34, 35, 36, 37, 38, 39,40,42, 43, 44, 45, 46, 47, 48)]
namesVec <- trimws(namesVec)
newstring <- paste0(namesVec, collapse = ' ')

df$model <- trimws(df$model)

for(i in 1:length(df$Name)){
  if(grepl(df$model[i], newstring, fixed = TRUE)){
    df$model[i] <- paste0("E ",df$model[i])
  } else {
    df$model[i] <- ""
  }
}
  
  
df[df$model == "E " | df$model == "E 20" | df$model == "E 2", ]$model <- ""
df[df$model == "E 35", ]$model <- "E 350"

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

