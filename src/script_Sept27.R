library(dplyr)
library(tidyverse)
#install.packages('tidycensus')
library(tidycensus)
library(magrittr) # Added to remove piping
library(sqldf)




#install.packages("rlist")
library("rlist")
library('tidycensus')


k=0
v = load_variables(2017, "acs5", cache = TRUE)
attr_names <- strsplit(v$name, '   ')[1:23348]
list_names <- c()
for (s in attr_names){
  list_names <- append(list_names, substr(s, 1, 6))
}
list_names <- unique(strsplit(list_names, ' '))

keys = c("c31267aeafe69d6320e2c6ce231cdc28aa59f175", "cacf453730ea631ec447a5a961b79842770c0b2f", 
         "bed52d7f99d905dfc116dc70b4ae900567ca45b7", "71acff7a50e167ef1402920f23f04a9097bacc38", 
         "743a238b0dd9e5c86aa396c11ac8eff705823d7a", "77cc34ef66ff3a0edf9eb7abe75c6b0feb6b9b3d", 
         "5563978cbe6e53b3e15ef4f557642d46bc4ba752", "c4292a084e03f48e05a34a2429882fd3ea7d8650", 
         "854d52c35e3dcf67e3c35de14744df86cd8fba01", "ba66758ec071524b373a8891e09f355d78aee8a2", 
         "c31267aeafe69d6320e2c6ce231cdc28aa59f175", "cacf453730ea631ec447a5a961b79842770c0b2f", 
         "bed52d7f99d905dfc116dc70b4ae900567ca45b7", "71acff7a50e167ef1402920f23f04a9097bacc38", 
         "743a238b0dd9e5c86aa396c11ac8eff705823d7a")

skip <- c(43, 53, 69, 91, 119, 313, 530, 535:542, 558:562, 573, 574, 598, 606:609, 618:621)

list_row_1 = list()
f = seq(1, 42)

for (j in keys){
  for(i in f){
    if(is.element(i, skip)){
      i<-i+1
    }
    else{
      poverty = get_acs(geography = "tract", table = list_names[i] , key = j,
                        state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = FALSE)
      #print(i)
      #print(list_names[i])
      #print(j)
      
      if (nrow(poverty) < 280){ 
        next}
      else
        abc <- sqldf("Select count(GEOID) from poverty where GEOID <= '36067005500'")
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
        normed <- sapply(fourth.column, function(x) x / x[1])
        normed = data.frame(normed)
        
        names(normed)=c("normalized")
        if(normed$normalized > 1){ 
          next}
        
        else{
          New <- cbind(normed,j)
          #assign(paste0("x", i), New)
          #assign(paste0("DF", i$GEOID), New)
          df=rbind(df,New)
        }
        
        
        
        
      }
      
      k=k+1
      
      assign(paste("DF", k), df)
      
      
      
      
      
      
      if(nrow(poverty)==140){
        list_row_1 = list.append(list_row_1, list_names[i])
      }
    }
    if(i%%42==0){
      i<-i+1
      f = seq(i,621)
      break
    }
  }
}

From: Vijay Arun Bhat <vibhat@syr.edu>
  Sent: Friday, September 27, 2019 1:25 PM
To: Group-Poverty Project <povertyproject@groups.syr.edu>
  Subject: Fwd: Poverty Project latest code (I will be uploading it in github)



Get Outlook for iOS
From: Parshva Jatin Shah <pjshah@syr.edu>
  Sent: Saturday, September 14, 2019 7:37 PM
To: Isha Satshil Havaldar; Nitin Nagpal; Vijay Arun Bhat; Om Jitendra Dhuru; Shuying Zhao
Subject: Poverty Project latest code (I will be uploading it in github)

library(dplyr)
library(tidyverse)
#install.packages('tidycensus')
library(tidycensus)
library(magrittr) # Added to remove piping
library(sqldf)

k=0
v = load_variables(2017, "acs5", cache = TRUE)
attr_names <- strsplit(v$name, '   ')[1:23348]
list_names <- c()
for (s in attr_names){
  list_names <- append(list_names, substr(s, 1, 6))
}
list_names <- unique(strsplit(list_names, ' '))

for(i in list_names[1:42])
{
  poverty<- get_acs(geography = "tract", table = c(i), key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                    state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = TRUE)
  
  #Getting 55 tracts from table
  if (nrow(poverty) < 280) next
  else
    abc <- sqldf("Select count(GEOID) from poverty where GEOID <= '36067005500'")
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
    if(fourth.column$estimate[1] == 0){
      normed <- sapply(fourth.column, function(x) x * 0)
    }else if(fourth.column$estimate[1]!= 0){
      normed <- sapply(fourth.column, function(x) x / x[1])
    }
    
    normed = data.frame(normed)
    
    names(normed)=c("normalized")
    if(normed$normalized > 1){ 
      next}
    else{
      New <- cbind(normed,j)
      #assign(paste0("x", i), New)
      #assign(paste0("DF", i$GEOID), New)
      df=rbind(df,New)
    }
    
    
    
    
  }
  
  k=k+1
  
  assign(paste("DF", k), df)
  
  
  
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













test <- spread(`36067004900`, variable, estimate) %>%
  left_join(Poverty_table, by ='GEOID')

for(i in ncol(test)) {
  assign(      test[,i+2]/test[,ncol(test)]
}

