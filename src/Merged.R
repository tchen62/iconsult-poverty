#Date: September 9, 2019
#Author: 1. Ashokkumar Sharma
#        2. Shivani Kulkarni


install.packages("sqldf")
library("sqldf")
library("rlist")
library('tidycensus')
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
k=0

for (j in keys){
  for(i in f){
    if(is.element(i, skip)){
      i<-i+1
    }
    else{
      poverty = get_acs(geography = "tract", table = list_names[i] , key = j,
          state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = TRUE)
      
  
      #if(nrow(d)==140){
        #list_row_1 = list.append(list_row_1, list_names[i])
      #}
      
      if (nrow(poverty) < 280) next
      else
        abc <- sqldf("Select count(GEOID) from poverty where GEOID <= '36067005500'")
      print(abc)
      cut_point <- abc$`count(GEOID)`
      print(cut_point)
      cutpoint_dataframe = print(poverty[1:cut_point, 1:4])
      
      
      
      
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
        New <- cbind(normed,j)
        #assign(paste0("x", i), New)
        #assign(paste0("DF", i$GEOID), New)
        df=rbind(df,New)
        
        
        
        
        
      }
      
      k=k+1
      
      assign(paste0("DF", k), df)
    
    }
    if(i%%42==0){
      i<-i+1
      f = seq(i,621)
      break
    }
  }
}
