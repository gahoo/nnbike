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

ggplot(seconds_status_molen) +
  aes(x=timestamp, y=value, color=NO, group=NO) +
  geom_line() +
  facet_grid(variable ~ ., scale='free_y')

test<-subset(seconds_status_molen, NO=="001082" & variable %in% c('E','T','F'))
ggplot(test) +
  aes(x=timestamp, y=value, fill=variable) +
  geom_area()

station<-readLines('BikeStation.json') %>%
  fromJSON(simplifyVector = FALSE) %>%
  lapply(as.data.frame) %>%
  do.call(what=rbind)
