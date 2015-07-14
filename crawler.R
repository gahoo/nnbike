library(jsonlite)
library(magrittr)
library(parallel)
library(plyr)
library(ggplot2)
library(reshape2)

getBikeStatus<-function(timestamp){
  message(as.POSIXct(timestamp/1000, origin="1970-01-01"))
  #Sys.sleep(2)
  url<-sprintf("http://nnbike.citycome.com/Home/BikeStatus?t=%s", as.character(timestamp))
  status<-readLines(url, warn = FALSE) %>%
    sub(pattern='var gBikeStatus=', replacement='') %>%
    fromJSON(simplifyVector = FALSE)
  status<-lapply(status, as.data.frame)
  do.call(rbind, status)
}

now<-floor(as.numeric(Sys.time())) * 1000
seconds<-now-1:(60 * 24 *7) * 1000 * 60

end<-60 * 24 * 1: 7
start<-c(1,1+end[1:6])

as.POSIXct(seconds/1000, origin="1970-01-01")

seconds_status<-mclapply(seconds, getBikeStatus, mc.cores=1)

empty_num<-sum(sapply(seconds_status, is.null))
if(empty_num>0){
  stop('something goes wrong')
}

names(seconds_status)<-seconds
seconds_status<-ldply(seconds_status, .id='timestamp')

save(seconds_status, file='seconds_status.RData')

seconds_status_molen<-melt(seconds_status)
seconds_status_molen<-within(seconds_status_molen, {
  timestamp<-timestamp %>%
    as.character %>%
    as.numeric %>%
    "/"(b=1000) %>%
    as.POSIXct(origin="1970-01-01")
  })

ggplot(seconds_status_molen) +
  aes(x=timestamp, y=value, color=NO, group=NO) +
  geom_line() +
  facet_grid(variable ~ ., scale='free_y')