#install.packages('sqldf')
#install.packages('rlist')
#install.packages('tidycensus')
#install.packages('dplyr')
#install.packages('tidyr')
library("sqldf")
library("rlist")
library('tidycensus')
library('dplyr')
library('tidyr')
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

skip <- c(43, 53, 69, 91, 119, 212, 313, 530, 535:542, 558:562, 573, 574, 598, 606:609, 618:621)

list_row_1 = list()
f = seq(1, 42)
k=0

################################################ Base Table #############################################################
poverty<- get_acs(geography = "tract", table = c("B17001"), key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                  state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = TRUE)
variables <- load_variables(2017,"acs5",cache=TRUE)
#Getting 55 tracts from table
abc <- sqldf("Select count(GEOID) from poverty where GEOID <= '36067006200'") #to get a count till 55 tracts
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
test <- test[, c(1,2,4)]

###########################################################################################################

for (j in keys){
  for(i in f){
    if(i==621){
      break
    }
    if(is.element(i, skip)){
      i<-i+1
    }
    else{
      poverty<- get_acs(geography = "tract", table = list_names[i], key = "6bef287462dbef1bdafdb3401c86178d1eca4a9d",
                        state = "NY", county = "Onondaga", year = 2017, survey="acs5", geometry = FALSE,cache_table = TRUE) #It contains 1 new table for each iteration
      print(i)
      print(list_names[i])
      print(j)
      #Getting 55 tracts from table
      if (nrow(poverty) < 280) next #removing the particular table having one row
      else
        abc <- sqldf("Select count(GEOID) from poverty where GEOID <= '36067006200'") #to get a count till 55 tracts
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
        fourth.column[1,1]
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
    if(i%%42==0){
      i<-i+1
      f = seq(i,621)
      break
    }
  }
}

nrow(test)
ncol(test)
test1<-t(test)
nrow(test1)

census_tract = c()
corr = c()
for (i in 4:length(test)){
  a <- cor(test[,i], test[,4])
  census_tract <- c(census_tract, colnames(test)[i])
  corr <- c(corr, a)
}
correlation_tracts <- na.omit(data.frame(census_tract,corr))
correlation_tracts <- correlation_tracts[with(correlation_tracts, order(-corr)),]
correlation_tracts$label <- variables$label[match(correlation_tracts$census_tract,variables$name)]
correlation_tracts$concept<-variables$concept[match(correlation_tracts$census_tract,variables$name)]
write.csv(correlation_tracts, 'correlation_tracts_labeled_sorted.csv')
#First 50 corr (positive)
head(correlation_tracts,50)
#Last 50 corr (Negative)
tail(correlation_tracts,50)

#For positive correlations
#People below poverty level also seem to have some kind of disability(auditory, vision, cognitive, ambulatory, self-care). This could be one area for City of syracuse to focus on.
#Auditory(hearing impairment) 97%, vision 97%, cognitive 93%, ambulatory(walking) 93%, self-care disability 93%.
#There are also age based disabilities in the same categories that come up in negative correlation implying that young people who have disabilities dont contribute to poverty as much
#as older people.
#Their poverty level may be helped by providing disability specific aids to them. correlation ranges from 97% to 93% suggesting most poor people have some form of disability.
#People below poverty level also seem to have a veteran status. correlation is 85% suggesting if a person is veteran, there is an 85% chance that they are below poverty level.
#Housing costs with high tenure also seem to affect people below poverty level, Correlation is 61.9%. This can be helped by subsidized housing plans for people below poverty level.
#Other psosible areas of focus: Class of worker, marital status, school enrollment(college or graduate), worker experience(by income and earnings). 
#These areas need to be drilled down in detail to find out which specific type affects poverty level the most.
#
#For negative correlations
#Health insurance has a negative role to play. 97% People below poverty line do not haveaccess to health insurance(private or otherwise).
#98.5% people below pverty level seem to have negtive correlation with school enrollment(graduate or college).
#Income has a negative correlation of 70% suggesting that income although is a factor, it is still affected by all of the other factors discussed above.
#56% of the people below pverty level have not used food stamps or may not have access to them.
# 55% Females below pverty level from 6-18 years do not have health insurance coverage.
# 59% people below poverty level who are employed by for-profit companies have not received their salaries or wages.
#64% females have none to negligible aggregate work hours suggesting they do not earn or work resulting in poverty.
#66% females workers below pverty level work within resident state and the other work out of state of residence.
#85% female veterans are also belo poverty level.


#geographical mobility suggests that 99% people below pverty level are unable to travel to or settle in a different state than their origin.