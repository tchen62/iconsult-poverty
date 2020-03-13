# Name : Tiehao Chen

test <- read.csv('corr2.csv',header = FALSE, stringsAsFactors = FALSE)

list_names <- c()
for (s in test$V1){
  list_names <- append(list_names, substr(s, 1, 6))
}
list_names_unique <- unique(list_names)
###############################################################################
neg_sort_corr<-test[order(test$V2),]
neg_top50<-neg_sort_corr[1:50,]

##############################################################################
neg_top50$table<-substring(neg_top50$V1,1,6)
neg_top50$vairable<-substring(neg_top50$V1,8,10)


##############################################################################
pos_sort_corr<-test[order(-test$V2),]
pos_top50<-pos_sort_corr[1:50,]

pos_top50$table<-substring(pos_top50$V1,1,6)
pos_top50$vairable<-substring(pos_top50$V1,8,10)


##############################################################################
library('tidycensus')
variables<- load_variables(2018,"acs5",cache=TRUE)

neg_top50$label <- variables$label[match(neg_top50$V1,variables$name)]
neg_top50$concept<-variables$concept[match(neg_top50$V1,variables$name)]

pos_top50$label<-variables$label[match(pos_top50$V1,variables$name)]
pos_top50$concept<-variables$concept[match(pos_top50$V1,variables$name)]

################################################################################
library(ggplot2)

ggplot(data = pos_top50, aes(x = V1, y = V2)) + geom_point(alpha = .5, color = 'red') + 
    theme(axis.text.x = element_text(angle =90, hjust=0.5,vjust=0.5, face = "bold")) + 
    xlab('Variable name')+
    ylab('Correlation')+
    theme(axis.title.x = element_text(size =15,  face ="bold"))+
    theme(axis.title.y = element_text(size =15,  face ="bold"))+
    theme(panel.grid = element_blank()) +
    ggtitle('Top Positive Correlation Variable')+
    theme(plot.title = element_text(size =15,  face ="bold", hjust = 0.5))

ggplot(data = neg_top50, aes(x = V1, y = V2)) + geom_point(alpha = .5, color = 'red') + 
  theme(axis.text.x = element_text(angle =90, hjust=0.5,vjust=0.5, face = "bold")) + 
  xlab('Variable name')+
  ylab('Correlation')+
  theme(axis.title.x = element_text(size =15,  face ="bold"))+
  theme(axis.title.y = element_text(size =15,  face ="bold"))+
  theme(panel.grid = element_blank()) +
  ggtitle('Top Negtive Correlation Variable')+
  theme(plot.title = element_text(size =15,  face ="bold", hjust = 0.5))
    
    

