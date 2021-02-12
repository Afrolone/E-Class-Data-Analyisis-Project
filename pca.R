# PCA done on the mobile dataset

summary(df)

testdf <- data.frame(
  df$UserRating,
  df$Preis,
  df$Kilometerstand,
  df$Hubraum,
  df$Emissionen,
  df$Leistunghp,
  df$years,
  df$consumptionCombined
  )

rm(list=setdiff(ls(), "df"))

#impute missin values with median
# combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

testdf$df.UserRating[is.na(testdf$df.UserRating)] <- mean(testdf$df.UserRating, na.rm = TRUE)













































































