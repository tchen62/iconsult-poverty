for(j in list_df){
  
  
  
  test <- list_df[1]  
  
  test<- as.data.frame(test)
  
  test[1,4] <- 0  
  fourth.column <- dplyr::select(as.data.frame(test),4)
  
  # this code lets to divide if first row is 0 and replaces value of all rows as 0
  normed <- case_when(fourth.column[1,1] == 0 ~ 0, TRUE ~ sapply(fourth.column, function(x) x / x[1]))
  normed = data.frame(normed)
  
  names(normed)=c("normalized")
  if(normed$normalized > 1){ 
    next}
  else{
    New <- cbind(normed,test)
    #assign(paste0("x", i), New)
    #assign(paste0("DF", i$GEOID), New)
    df=rbind(df,New)
  }
}
