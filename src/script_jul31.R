library(dplyr)
library(tidyverse)
install.packages('tidycensus')
library(tidycensus)
library(magrittr) # Added to remove piping

v = load_variables(2017, "acs5", cache = TRUE)
attr_names <- strsplit(v$name, '   ')[1:23348]
list_names <- c()
for (s in attr_names){
  list_names <- append(list_names, substr(s, 1, 6))
}
list_names <- unique(strsplit(list_names, ' '))

for(i in list_names[1:3])
{
  poverty<- get_acs(geography = "tract", table = c(i), key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                    state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = TRUE)
  
  #Getting 55 tracts from table
  if (nrow(poverty) < 280) next
  else
    abc <- sqldf("Select count(GEOID) from poverty where GEOID = '36067000100'")
  print(abc)
  cut_point <- abc*55
  print(cut_point)
  #print(poverty[1:4][1:cut_point])

  
  
  
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