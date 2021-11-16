library(LTMRdata)
library(tidyverse)
library(lubridate)
library(ggmap)


data_deltasmelt <-LTMRdata::fish(sources=c("Baystudy", "Suisun", "FMWT", "SKT", "DJFMP", "EDSM", "TMM", "SLS","STN"),species="Hypomesus transpacificus", size_cutoff=NULL,remove_unknown_lengths=TRUE) %>%
  filter(Count>0)

data_deltasmelt$Count<-as.integer(data_deltasmelt$Count)

data_deltasmelt_adult<-data_deltasmelt %>% filter(year(Date)>=1994,Length>=55) %>% mutate(broodyear=ifelse(month(Date)>=5,year(Date),year(Date)-1))

data_deltasmelt_adult<-data_deltasmelt_adult[rep(1:nrow(data_deltasmelt_adult), data_deltasmelt_adult$Count),]
data_deltasmelt_adult$Count<-1


data_deltasmelt_sum<-data_deltasmelt_adult %>% group_by(broodyear) %>%  
  summarise(quantile = scales::percent(c(0, 0.05, 0.1, 0.9, 0.95, 1.0)),
            date = quantile(Date, c(0, 0.05, 0.1, 0.9, 0.95, 1.0), type=1)) 

data_deltasmelt_sum$date<-format(data_deltasmelt_sum$date, format="%B %d %Y")

data_deltasmelt_sum<-spread(data_deltasmelt_sum,quantile,date)
str(data_deltasmelt_sum)

col_order <- c("broodyear", "0.0%", "5.0%",
               "10.0%", "90.0%","95.0%","100.0%")
data_deltasmelt_sum2 <- data_deltasmelt_sum[, col_order]

data_deltasmelt_sum2 <- left_join(data_deltasmelt_sum2,data_deltasmelt_adult %>% group_by(broodyear) %>% summarise(sample_size=sum(Count)))

write.csv(data_deltasmelt_sum2,file = "ExampleDeltaSmelt.csv",row.names = FALSE)
