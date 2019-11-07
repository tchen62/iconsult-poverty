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

list_names <- unique(strsplit(list_names, ' '))

################################################ Base Table #############################################################
poverty<- get_acs(geography = "tract", table = c("B17001"), key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                  state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = TRUE)



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

test <- spread(dfnew, variable, normalized)
#ncol(test)
test <- test[, c(1,2,4)]
#ncol(test)
###########################################################################################################


for(i in list_names[1:10])
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
    
    if(fourth.column[1,1]==0 | is.na(fourth.column[1,1])){ #if first cell of fourth column (estimate) is 0 or NA then keep it as 0
      normed <- sapply(fourth.column, function(x) 0)
    }
    
    else{   
      normed <- sapply(fourth.column, function(x) x / x[1])
    }
    normed = data.frame(normed)
    names(normed)=c("normalized")
    New <- cbind(normed,j)
    #assign(paste0("x", i), New)
    #assign(paste0("DF", i$GEOID), New)
    df=rbind(df,New)
    
  }
  
  df=df[-c(5)] #new
  df[is.na(df)] <- 0
  
  k=k+1
  
  no_col <- ncol(test)
  newspread  <- spread(df, variable, normalized)
  test <- left_join(test, newspread, by ='GEOID')
  test <- test[, -c(no_col+1, no_col+2)]
  
  
  assign(paste0("DF", k), df)
}
