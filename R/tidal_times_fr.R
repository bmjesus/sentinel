#' @title Function read and parse tidal times from the site http://maree.info/
#' @description read and parse tidal times from the site http://maree.info/,
#' currently configured to read tides from Le croisic. Converts the time to UTC.
#' Uses the package rvest for the html parsing.
#' @param tide_date date in the format yyyymmdd (e.g. 20181201)
#' @param site_id numerical code to the tidal station, e.g. 114 for Le Croisic
#' @return a dataframe with the tide times corrected to UTC, the tidal height and
#' if they correspond to high tide (PM) or low tide (BM)
#' @export
tide_info_fr<-function(tide_date,site_id=114){

#this is here because the read_html function is not being exported by the rvest package
#should be removed in the future if rvest is corrected
library('rvest')
#tide_date<-"20181201"

  #tide_date<-"2018-12-01"

  year<-substr(tide_date,1,4)
  month<-substr(tide_date,6,7)
  day<-substr(tide_date,9,10)

  tide_date<-paste(year,month,day,sep='')


  url <- paste("http://maree.info/",site_id,"?d=",tide_date,sep="")

  webpage <- read_html(url)

  tidal_type<-rvest::html_text(rvest::html_nodes(webpage,'.PMBM'))
  if (nchar(tidal_type)==6){
    tidal_type<-c(substr(tidal_type, 1, 2),substr(tidal_type, 3, 4),substr(tidal_type, 5, 6))
  }else{
    tidal_type<-c(substr(tidal_type, 1, 2),substr(tidal_type, 3, 4),substr(tidal_type, 5, 6),substr(tidal_type,7,8))
  }

#############################################################################
##harvesting and parsing section
#############################################################################
  b<-rvest::html_text(rvest::html_nodes(webpage,'.SEPV'))
  tidal_time<-b[1]
  tidal_height<-b[2]
  tidal_height<-gsub("m",'',tidal_height)
  tidal_height<-gsub(",",'.',tidal_height)

  if(nchar(tidal_time)==20){
    tidal_time<-c(substr(tidal_time, 1, 5),substr(tidal_time, 6, 10),substr(tidal_time, 11, 15),substr(tidal_time,16,20))

  }else{
    tidal_time<-c(substr(tidal_time, 1, 5),substr(tidal_time, 6, 10),substr(tidal_time, 11, 15))
  }


  if(nchar(tidal_height)==16){
    tidal_height<-c(substr(tidal_height, 1, 4),substr(tidal_height, 5, 8),substr(tidal_height, 9, 12),substr(tidal_height,13,16))
    tidal_height<-as.numeric(tidal_height)
  }else{
    tidal_height<-c(substr(tidal_height, 1, 4),substr(tidal_height, 5, 8),substr(tidal_height, 9, 12))
    tidal_height<-as.numeric(tidal_height)
  }


  tidal_table<-as.data.frame(cbind(tidal_type,tidal_time,tidal_height),stringsAsFactors=FALSE)
  tidal_table$tidal_height<-as.numeric(tidal_table$tidal_height)

  #############################################################################
  #section for UTC extraction
  #############################################################################

  utc1<-tryCatch(rvest::html_text(rvest::html_nodes(webpage,'.UT1')),error = function(e) {utc1 <- "error"})

  utc2<-tryCatch(rvest::html_text(rvest::html_nodes(webpage,'.UT2')), error = function(e){utc2 <- "error"})


  tryCatch(if(nchar(utc1[2])>0){utc<-utc1[2]},error = function(e){})
  tryCatch(if(nchar(utc2[2])>0){utc<-utc2[2]},error = function(e){})

  utc<-substr(utc,1,5)

#print(utc)

#############correcting tidal_time to make it utc

#converting the tidal time to POSIX format
t1<-paste(tide_date,tidal_table$tidal_time)
tidal_table$tidal_time<-strptime(t1,format='%Y%m%d %Hh%M',tz='UTC')

#subtracting one or two hours depending on the date and
#replacing the tidal_time in the data.frame
if(utc=="UTC+1"){tidal_table$tidal_time<-tidal_table$tidal_time-60*60}
if(utc=="UTC+2"){tidal_table$tidal_time<-tidal_table$tidal_time-2*60*60}



  #############################################################################
  #final output
  #############################################################################

  return(tidal_table)

}
