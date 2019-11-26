#' @title Function read and parse tidal times from the site http://www.waterlevels.gc.ca
#' @description read and parse tidal times from the site http://www.waterlevels.gc.ca
#' currently configured to read tides from Hopewell Cape (#170).
#' Uses the package rvest for the html parsing.
#' @param tide_date date in the format yyyymmdd (e.g. 20181201)
#' @param site_id code for the tidal site
#' @return a dataframe with the tide times corrected to UTC, the tidal height and
#' if they correspond to high tide (PM) or low tide (BM)
#' @export
tide_info_ca<-function(tide_date,site_id = 5855){

#tide_date<-20190104



#this is here because the read_html function is not being exported by the rvest package
library('rvest')
#tide_date<-"20181201"

  year<-substr(tide_date,1,4)
  month<-substr(tide_date,6,7)
  day<-substr(tide_date,9,10)

  tide_date<-paste(year,month,day,sep='')

url<-paste('http://www.waterlevels.gc.ca/eng/station?type=0&date=',year,'%2F',month,'%2F',day,'&sid=',site_id,'&tz=UTC&pres=1',sep='')

webpage <- read_html(url)

#tidal_type<-

#rvest::html_text(rvest::html_nodes(webpage,'.time'))
#rvest::html_text(rvest::html_nodes(webpage,'.time , tr:nth-child(2) th'))
#rvest::html_text(rvest::html_nodes(webpage,'.heightMeters , .time , tr:nth-child(2) th'))

a<-rvest::html_text(rvest::html_nodes(webpage,' .time, .heightMeters'))

#to get the positions of the "marker" (m) in the object
marker<-which(a=="(m)")

#the data are between the first and the second (m)
dta<-a[2:(marker[2]-1)]

#separating time from tidal height
tidal_time<-numeric()
tidal_height<-numeric()
counter<-1
for (i in 1:((length(dta)/2))){
tidal_time[i]<-dta[counter]
tidal_height[i]<-dta[counter+1]
counter<-counter+2
}

#start constructing the dataframe
#it should have these 3 columns: tidal_type, tidal_time, tidal_height
tidal_table<-as.data.frame(matrix(NaN, ncol = 3, nrow = (length(dta)/2)))
names(tidal_table)<-c('tidal_type','tidal_time','tidal_height')

tidal_table$tidal_height<-as.numeric(tidal_height)

#constructing the time variable
t1<-paste(tide_date,tidal_time)

tidal_table$tidal_time<-strptime(t1,format='%Y%m%d %H:%M',tz='UTC')

#############################################################################
#calculating the type of tide. Low tide (BM), hight tide (PM)
#############################################################################
for (i in 1:(length(dta)/2-1)){
  #print(i)
  condition<-tidal_table$tidal_height[i]>tidal_table$tidal_height[i+1]
  #print(condition)
  if (condition==TRUE){
    tidal_table$tidal_type[i]<-"PM"
    tidal_table$tidal_type[i+1]<-"BM"}
  if (condition==FALSE){
    tidal_table$tidal_type[i]<-"BM"
    tidal_table$tidal_type[i+1]<-"PM"}
}



#############################################################################
#final output
#############################################################################

return(tidal_table)

}
