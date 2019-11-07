

poverty<- get_acs(geography = "tract", table = c("B17001"), key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                  state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = TRUE)  %>% 
  filter(variable == "B17001_001" | variable == "B17001_002")



#Getting 55 tracts from table
abc <- sqldf("Select count(GEOID) from poverty where GEOID <= '36067005500'") #to get a count till 55 tracts
#print(abc)
cut_point <- abc$`count(GEOID)` 
#print(cut_point)
cutpoint_dataframe = poverty[1:cut_point, 1:4]

list_df <- split(cutpoint_dataframe, cutpoint_dataframe$GEOID) #split the dataset into a list of datasets based on the value of iris$Species
list2env(list_df, envir= .GlobalEnv) #split the list into separate datasets

dfnew = data.frame(GEOID=character(),
               Name=character(),
               variable=character(),
               estimate=double(),
               stringsAsFactors = FALSE
)

for(j in list_df){
  fourth.column <- j[,4]
  normed <- sapply(fourth.column, function(x) x / x[1])
  normed = data.frame(normed)
  names(normed)=c("normalized")
  New <- cbind(normed,j)
  #assign(paste0("x", i), New)
  #assign(paste0("DF", i$GEOID), New)
  dfnew =rbind(dfnew,New)
}

dfnew <- dfnew[, -5]

pqr <- spread(dfnew, variable, normalized)
