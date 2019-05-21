#' @title Function to recover tidal information between two dates 
#' @description Function to recover tidal information between two dates. Read and parse tidal times from the site whttps://tides.mobilegeographics.com so an internet connection is needed
#' @param start_date date in the format "yyyy-mm-dd" (e.g. "2018-12-01")
#' @param end_date date in the format "yyyy-mm-dd" (e.g. "2018-12-01")
#' @param site_id code for the tidal site (check the site url to find the code of the tidal station)
#' @return a dataframe with the tide times corrected to UTC, the tidal height and
#' if they correspond to high tide (PM) or low tide (BM)
#' @export

tidal_tables<-function(start_date,end_date,site_id){

  date_sequence<-seq.Date(from=as.Date(start_date),to=as.Date(end_date),by=1)
  #date_sequence<-gsub("-",'',date_sequence)
  temp_date<-data.frame()
  for (i in 1:length(date_sequence)){
    temp<-sentinel::tide_info_all(date_sequence[i],site_id = site_id)
    temp_date<-rbind(temp_date,temp)
  }
    return(temp_date)
  
}