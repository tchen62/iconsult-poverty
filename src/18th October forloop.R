library(dplyr)
library(tidyverse)
#install.packages('tidycensus')
library(tidycensus)
library(magrittr) # Added to remove piping
library(sqldf)


#Base Table: B17001

k=0
v = load_variables(2017, "acs5", cache = TRUE)
attr_names <- strsplit(v$name, '   ')[1:23348]
list_names <- c()
for (s in attr_names){
  list_names <- append(list_names, substr(s, 1, 6))
}

poverty<- get_acs(geography = "tract", table = 'B17001', key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                  state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = TRUE) #It contains 1 new table for each iteration


list_names <- unique(strsplit(list_names, ' '))
test = DF21spread
for(i in list_names[1:42])
{
  poverty<- get_acs(geography = "tract", table = c(i), key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                    state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = TRUE) #It contains 1 new table for each iteration
  
  
  
  #Getting 55 tracts from table
  if (nrow(poverty) < 280) next #removing the particular table having one row
  else
    abc <- sqldf("Select count(GEOID) from poverty where GEOID <= '36067005500'") #to get a count till 55 tracts
  #print(abc)
  cut_point <- abc$`count(GEOID)` 
  #print(cut_point)
  cutpoint_dataframe = poverty[1:cut_point, 1:4]
  
  
  
  
  list_df <- split(cutpoint_dataframe, cutpoint_dataframe$GEOID) #split the dataset into a list of datasets based on the value of iris$Species
  list2env(list_df, envir= .GlobalEnv) #split the list into separate datasets
  
  #View(list_df)
  
  #w=list()
  
  df= data.frame(GEOID=character(),
                 Name=character(),
                 variable=character(),
                 estimate=double(),
                 stringsAsFactors = FALSE
  )
  
  for(j in list_df){
    
    
    fourth.column <- j[,4]
    
    if(fourth.column[1,1]==0){ #if first cell of fourth column (estimate) is 0 then keep it as 0
      normed <- sapply(fourth.column, function(x) x[1])
      normed = data.frame(normed)
      names(normed)=c("normalized")
      New <- cbind(normed,j)
      df=rbind(df,New)
      #df=df[-c(5)]
    }
    
    else{   
      normed <- sapply(fourth.column, function(x) x / x[1])
      normed = data.frame(normed)
      names(normed)=c("normalized")
      New <- cbind(normed,j)
      #assign(paste0("x", i), New)
      #assign(paste0("DF", i$GEOID), New)
      df=rbind(df,New)
      
      #df=df[-c(5)]
    }
    
    
  }
  
  df=df[-c(5)] #new
  df[is.na(df)] <- 0
  
  k=k+1
  
  test <- left_join(test, spread(df, variable, normalized), by ='GEOID')
  
  assign(paste0("DF", k), df)
  
  
  #View(poverty[1:cut_point, 1:4])
  
  #  while (GEOID)
  #  {
  #   statement
  #  }
  
  
  # typeof(poverty)
  #poverty1 <- unlist(poverty)
  #poverty_filtered <- filter(get(paste(i, "_001", sep = '')) %in% poverty1 )
  #poverty_filtered <- 
  #View(poverty)
  #View(poverty1)
  #View(poverty_filtered)
  # print(nrow(poverty))
  
  # if(nrow(poverty)<280) next
  # 
  # else
  # print(i)
  #   list_firstrow <-  poverty[[3:4]][1:nrow(poverty)]
  # 
  # View(list_firstrow)
  #filter(variable == paste(i, "_001", sep = '') | variable == paste(i, "_002", sep = ''))
  #print(typeof(poverty))
  # print("Working")
  # for(j in nrow(poverty)){
  #   newdf <- data.frame(get(paste(i, "j", sep = ''))/get(paste(i, "_001", sep = '')))
  #   print(newdf)
  # }
  
}








# for(i in seq('36067000100','36067005500','1'))
# {
#   subset(cutpoint_dataframe, cutpoint_dataframe$GEOID == 'i')
# }









#names(DF1)[1]="Normalized" ****** Rename a column by index in a data frame
#DF1=DF1[-c(5)] ****************** Drop a column by index in a dataframe but we have to reassign it to a dataframe


#A for loop to spread and join each table


DF21spread <- spread(DF21, variable, normalized)
test <- spread(DF20, variable, normalized) %>%
  left_join(DF21spread, by ='GEOID')

for(i in ncol(test)) {
  assign(      test[,i+2]/test[,ncol(test)]
               
               
               
               
poverty_rates=poverty %>%
  mutate(poverty_rates)
               


for(i in 4:ncol(test)){}
  t=test[,c(1,i)] %>%
    filter(colnames(test[i]) != 0) %>%
    leftjoin(Poverty_rates_tract by GEOIDs)
  
  
  
  
  corr = cor(t[,2],t[,3])
  
  assign(df,c(colname(test[i]),corr))
  
  cor()
  
  
}
               
               