library(readr)
corr_table<- read.csv('correlation_tract.csv',header = T, stringsAsFactors = F)


neg_sort_corr<-corr_table[order(corr_table$corr),]
neg_top50<-neg_sort_corr[1:50,]

neg_top50$table<-substring(neg_top50$census_tract,1,6)
neg_top50$vairable<-substring(neg_top50$census_tract,8,10)

pos_sort_corr<-corr_table[order(-corr_table$corr),]
pos_top50<-pos_sort_corr[1:50,]

pos_top50$table<-substring(pos_top50$census_tract,1,6)
pos_top50$vairable<-substring(pos_top50$census_tract,8,10)

pos_top50

library('tidycensus')
variables<- load_variables(2017,"acs5",cache=TRUE)

neg_top50$label<-variables$label[match(neg_top50$census_tract,variables$name)]
neg_top50$concept<-variables$concept[match(neg_top50$census_tract,variables$name)]

pos_top50$label<-variables$label[match(pos_top50$census_tract,variables$name)]
pos_top50$concept<-variables$concept[match(pos_top50$census_tract,variables$name)]

#scatter plots
final<-read.csv('final_table.csv',header = T, stringsAsFactors = F)

par(mfrow=c(2,2))
#two positive relationship plots
plot_B07012_002<-data.frame(final$B17001_002,final$B07012_002)
plot(plot_B07012_002,main='plot for B07012_002')

plot_B14006_002<-data.frame(final$B17001_002,final$B14006_002)
plot(plot_B14006_002,main='plot for B14006_002')

#two negative relationship plots

plot_B14006_012<-data.frame(final$B17001_002,final$B14006_012)
plot(plot_B14006_012,main='plot for B14006_012')

plot_B16009_015<-data.frame(final$B17001_002,final$B16009_015)
plot(plot_B16009_015,main='plot for B16009_015')

