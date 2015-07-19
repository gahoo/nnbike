library(jsonlite)
library(magrittr)
library(parallel)
library(plyr)
library(ggplot2)
library(reshape2)

getBikeStatus<-function(timestamp){
  message(as.POSIXct(timestamp/1000, origin="1970-01-01"))
  Sys.sleep(20)
  url<-sprintf("http://nnbike.citycome.com/Home/BikeStatus?t=%s", as.character(timestamp))
  status<-readLines(url, warn = FALSE) %>%
    sub(pattern='var gBikeStatus=', replacement='') %>%
    fromJSON(simplifyVector = FALSE)
  status<-lapply(status, as.data.frame)
  do.call(rbind, status)
}

now<-floor(as.numeric(Sys.time())) * 1000
now<-1436889600000
seconds<-now-(60 * 24 *6.5):(60 * 24 *6.7) * 1000 * 60
seconds<-now-1:(60 * 2) * 1000 * 60
seconds<-c(1436800494000, 1436800594000)

end<-60 * 24 * 1: 7
start<-c(1,1+end[1:6])

as.POSIXct(seconds/1000, origin="1970-01-01")

error_idx<-rep(T, length(seconds))

seconds_status<-list()
while(sum(error_idx)>0){
  seconds_status[error_idx]<-mclapply(seconds[error_idx], getBikeStatus, mc.cores=1)
  error_idx<-sapply(seconds_status, function(x){ifelse(class(x)=="try-error",T,F)})
  message("remaining error:", sum(error_idx))
}

names(seconds_status)<-seconds
seconds_status<-ldply(seconds_status, .id='timestamp')

#save(seconds_status, file='seconds_status.RData')
load('seconds_status.RData')

seconds_status_molen<-melt(seconds_status)
seconds_status_molen<-within(seconds_status_molen, {
  timestamp<-timestamp %>%
    as.character %>%
    as.numeric %>%
    "/"(b=1000) %>%
    as.POSIXct(origin="1970-01-01")
  })

pdf('NO.pdf', height=90)
ggplot(seconds_status_molen) +
  aes(x=timestamp, y=value, color=variable, group=NO) +
  geom_line() +
  facet_grid(NO ~ ., scale='free_y')
dev.off()

test<-subset(seconds_status_molen, NO=="002017" & variable %in% c('E','T','F'))
ggplot(test) +
  aes(x=timestamp, y=value, fill=variable) +
  geom_area()

station<-readLines('BikeStation.json') %>%
  fromJSON(simplifyVector = FALSE) %>%
  lapply(as.data.frame) %>%
  do.call(what=rbind)

library(dygraphs)

test<-subset(seconds_status, NO=="001022", select=c('timestamp','E','T','F')) %>%
  within(expr={
  timestamp<-timestamp %>%
    as.character %>%
    as.numeric %>%
    "/"(b=1000) %>%
    as.POSIXct(origin="1970-01-01")
})

rownames(test)<-test$timestamp
test$timestamp<-NULL

shrinkTimestamp<-function(df){
  record_cnts<-nrow(df)
  records<-as.matrix(df[c('E', 'T', 'F')])
  records_change<-records[2:record_cnts,]-records[1:(record_cnts-1),]
  no_change_idx<-!apply(records_change,1,function(x)(sum(x!=0)))==0
  no_change_region_idx<-no_change_idx[1:(record_cnts-2)]|no_change_idx[2:(record_cnts-1)]
  df_no_change_idx<-c(T,no_change_region_idx,T)#no_change_idx[record_cnts-1]
  df[df_no_change_idx,]
}

NOs<-unique(seconds_status$NO)
shrinkStatus<-lapply(NOs, function(x){
  message(x)
  shrinkTimestamp(subset(seconds_status, NO == x))
})

sapply(shrinkStatus, nrow)

dygraph(shrinkTimestamp(test)) %>%
  dySeries("E", label = "Error") %>%
  dySeries("T", label = "Occupied") %>%
  dySeries("F", label = "Empty") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

