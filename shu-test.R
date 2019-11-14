library(readr)
final<-read.csv("final_table.csv",header = T, stringsAsFactors = F)

census_tract = c()
corr = c()
for (i in 4:length(final)){
  a <- cor(final[,3], final[,i])
  census_tract <- c(census_tract, colnames(final)[i])
  corr <- c(corr, a)
}
correlation_tracts <- data.frame(census_tract, corr)
write.csv(data.frame(census_tract, corr), 'correlation_tract.csv')
