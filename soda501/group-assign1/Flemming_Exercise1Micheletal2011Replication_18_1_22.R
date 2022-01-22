# SoDA 501 Exercise 1, Michel et al. 2011 Replication

library(vroom)
library(dplyr)
library(ggplot2)
############# read in ngram data
engrams<-vroom("googlebooks-eng-all-1gram-20120701-1.gz", col_names = F)
#trim to just 4 digit numbers that could be years
engrams<-engrams[nchar(engrams$X1)==4,]
engrams<-engrams[!grepl("\\D", engrams$X1),]
#trim to ngrams and years 1875-1975
engrams$X1<-as.numeric(engrams$X1)
engrams<-engrams[engrams$X1>1874 & engrams$X1<1976,]
engrams<-engrams[engrams$X2>1874 & engrams$X2<1976,]
#rename variables for clarity
engrams<-engrams %>% 
  rename(
    ngram = X1,
    year = X2,
    match_count = X3,
    volume_count = X4
  )

#read in total_count
total<-read.table(text = gsub(",", "\t", readLines
                              ("googlebooks-eng-all-totalcounts-20120701.txt")))
total<-as.data.frame(matrix(total,ncol =4,byrow = T))
#rename variables for clarity
total<-total %>% 
  rename(
    year = V1,
    match_count = V2,
    page_count = V3,
    volume_count = V4
  )

##############part B recreating the main part of figure 3a
#subsetting data for 1883, 1910, 1950
fig3dat<- subset(engrams, ngram == 1883 | ngram == 1910 | ngram == 1950)
total$year<-as.numeric(total$year)
total$match_count<-as.numeric(total$match_count)
total <- subset(total, year >= 1875 & year <= 1975)
#merging total counts and raw counts
fig3dat <- merge(fig3dat, total, "year")
#calculate proportions for each ngram
fig3dat$match_prop<-fig3dat$match_count.x/fig3dat$match_count.y

#graph proportion against year
ggplot(data = fig3dat, aes(year, match_prop))+
  geom_line(aes(group = ngram, color=factor(ngram)))
