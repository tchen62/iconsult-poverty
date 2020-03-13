library("sqldf")
library("rlist")
library('tidycensus')
library('dplyr')
library(tidyr)
library(readr)
#table has problem
# B06004 
# B07004 
# B07404 
# B08105 
# B08505 
# B22005 
# B28009 
# B99251 
# B99270 


v_2018 = load_variables(2018, "acs5", cache = TRUE)

attr_names <- strsplit(v_2018$name, '   ')[1:25274]
list_names <- c()
for (s in attr_names){
  list_names <- append(list_names, substr(s, 1, 6))
}

list_names_unique <- unique(list_names)
skip <- c()
for (i in list_names_unique){
  df <- get_acs(geography = "tract", table = c(i), key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                state = "NY", county = "Onondaga", year = 2018, survey="acs5", geometry = FALSE,cache_table = TRUE)
  abc <- sqldf("Select count(GEOID) from df where GEOID <= 36067006200")  # get census tract from 1 to 61.03
  cut_point <- abc$"count(GEOID)"
  df <- df[1:cut_point, 1:4]
  if (nrow(df) < 56){
    skip <- append(skip,i)
  }
}
probelm_table <- c('B06004','B07004','B07404','B08105','B08505','B22005','B28009','B99251','B99270')
skip <- append(skip,probelm_table)

skip_num <- c()
for (h in skip){
  skip_num <- append(skip_num,which(list_names_unique == h))
}

list_names_unique_new <- list_names_unique[-c(skip_num)]


keys = c("c31267aeafe69d6320e2c6ce231cdc28aa59f175", "cacf453730ea631ec447a5a961b79842770c0b2f", 
         "bed52d7f99d905dfc116dc70b4ae900567ca45b7", "71acff7a50e167ef1402920f23f04a9097bacc38", 
         "743a238b0dd9e5c86aa396c11ac8eff705823d7a", "77cc34ef66ff3a0edf9eb7abe75c6b0feb6b9b3d", 
         "5563978cbe6e53b3e15ef4f557642d46bc4ba752", "c4292a084e03f48e05a34a2429882fd3ea7d8650", 
         "854d52c35e3dcf67e3c35de14744df86cd8fba01", "ba66758ec071524b373a8891e09f355d78aee8a2", 
         "c31267aeafe69d6320e2c6ce231cdc28aa59f175", "cacf453730ea631ec447a5a961b79842770c0b2f", 
         "bed52d7f99d905dfc116dc70b4ae900567ca45b7", "71acff7a50e167ef1402920f23f04a9097bacc38", 
         "743a238b0dd9e5c86aa396c11ac8eff705823d7a","6bef287462dbef1bdafdb3401c86178d1eca4a9d")

poverty_2018<- get_acs(geography = "tract", table = c("B17001"), key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                       state = "NY", county = "Onondaga", year = 2018, survey="acs5", geometry = FALSE,cache_table = TRUE)

abc <- sqldf("Select count(GEOID) from poverty_2018 where GEOID <= 36067006200")  # get census tract from 1 to 61.03
cut_point <- abc$"count(GEOID)"
cutpoint_proverty_2018 = poverty_2018[1:cut_point, 1:4]

# we can use split function to divide dataframe according to GEOID.

#GEOIDs are numeric codes that uniquely identify all administrative/legal and statistical geographic areas for which the Census Bureau tabulates data

list_df <- split(cutpoint_proverty_2018, cutpoint_proverty_2018$GEOID)
#list2env(list_df, envir= .GlobalEnv) #split the list into separate datasets

#crete a empty dataframe
dfnew = data.frame(GEOID=character(),
                   Name=character(),
                   variable=character(),
                   estimate=double(),
                   stringsAsFactors = FALSE
)

for(j in list_df){
  fourth.column <- j[,4]
  normed <- sapply(fourth.column, function(x) x / x[1])
  normed <- data.frame(normed)
  # give column names for dataframe
  names(normed) <- c("normalized")
  # two dataframes combine and must keep them with same number of rows
  New <- cbind(normed,j) 
  # two dataframes combine and must keep them with same number of columns
  dfnew <- rbind(dfnew,New)
}

# get a vector as base of poverty rate which stand for B17001(poverty_rate)
poverty_rate <- c()
a = 2
for (i in 0:54){
  poverty_rate <- append(poverty_rate,dfnew$normalized[a])
  a = a + 59 
}


###############################################################################################################

for (i in list_names_unique_new){
  df <- get_acs(geography = "tract", table = c(i), key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                state = "NY", county = "Onondaga", year = 2018, survey="acs5", geometry = FALSE,cache_table = TRUE)
  abc <- sqldf("Select count(GEOID) from df where GEOID <= 36067006200")  # get census tract from 1 to 61.03
  cut_point <- abc$"count(GEOID)"
  df <- df[1:cut_point, 1:4]
  list_df <- split(df, df$GEOID)
  dfnew = data.frame(GEOID=character(),
                     Name=character(),
                     variable=character(),
                     estimate=double(),
                     stringsAsFactors = FALSE)
  for(j in list_df){
      fourth.column <- j[,4]
      normed <- sapply(fourth.column, function(x) x / x[1])
      normed <- data.frame(normed)
      # give column names for dataframe
      names(normed) <- c("normalized")
      # two dataframes combine and must keep them with same number of rows
      New <- cbind(normed,j) 
      # two dataframes combine and must keep them with same number of columns
      dfnew <- rbind(dfnew,New)
   }
  B <- c()
  corr <- c()
  row <- nrow(dfnew)/55
  for (h in 2:row){
    for (b in 1:55){
      B <- append(B,dfnew$normalized[h])
      h <- h + row
    }
    corr <- append(corr,cor(B,poverty_rate))
    B <- c()
  }
  vname <- dfnew$variable[2:row]
  write_csv(data.frame(vname,corr), 'corr2.csv',append = TRUE)
}


#以追加方式写入csv的方法
#demo_Test <- data.frame(c('a,b,c'),c(1,2,3))
#write_csv(demo_Test,'corr.csv', append = TRUE)
# vname <- c()
# for (h in 2:row){
#   vname <- append(vname,dfnew$variable[h])
# }
# 
# hhhhh <- dfnew$variable[2:56]
# 
# demo_Test <- data.frame(hhhhh,poverty_rate)
# write_csv(demo_Test,'corr.csv', append = TRUE)






