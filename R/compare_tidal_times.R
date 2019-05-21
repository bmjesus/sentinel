#' @title Compares a name of a sentinel file with the tide from that date
#' @description Compares a name of a sentinel file with the tide from that date and if
#' it is within the time window defined by the user return TRUE
#' @param sat_info Name of a Sentinel file, the date will be extracted from here
#' @param time_window Time window (in minutes) around low tide for selecting an image
#' @param country code for the country tides. Currently fr for France and ca for Canada
#' @param site_id numerical code to the tidal station, e.g. 114 for Le Croisic
#' @return a list with the tidal info and an element TRUE/FALSE if it fits the selection criteria (time_window)
#' @export

compare_times<-function(sat_info,time_window,country='fr',site_id=NULL){

#convert time window to seconds
#time_window<-time_window*60

#extracting the date and time from the sentinel title, assuming it follows this format
#sat_info<-'S2B_MSIL1C_20180901T151639_N0206_R025_T20TLR_20180901T204231.SAFE'

sat_info<-substr(sat_info,12,26)
sat_info<-gsub("T",' ',sat_info)

#print(sat_info)

t1<-substr(sat_info,1,8)

#rebuilding the date variable in the format yyyy-mm-dd

my_year<-substr(t1,1,4)
my_month<-substr(t1,5,6)
my_day<-substr(t1,7,8)


t1<-paste(my_year,"-",my_month,"-",my_day,sep='')

#print(t1)

#checking the tide website for tidal times
#this function is specific to France Le Croisic (http://maree.info/114)
#could be adapted to other sites and replaced here by another function

if (country=='fr'){
  tide_info_df<-tide_info_fr(t1,site_id)
}
if (country=='ca'){
  tide_info_df<-tide_info_ca(t1)
}

if (country=='all'){
  tide_info_df<-tide_info_all(tide_date=t1,site_id=as.character(site_id))
}

#print(tide_info_df)

sat_info<-strptime(sat_info,format='%Y%m%d %H%M%S',tz='UTC')

#adding the time difference to the tidal table
#it is in absolute time so it is not possible to know if it is before or after LT
tide_info_df$time_difference<-round(abs(difftime(sat_info, tide_info_df$tidal_time,
                                       units = c("mins"))),0)

tide_info_time_BM<-tide_info_df$tidal_time[tide_info_df$tidal_type=='BM']
time_difference<-round(difftime(sat_info, tide_info_time_BM,
                          units = c("mins")),0)
time_difference<-abs(time_difference)


#print(time_difference)
#selection of image based on the absolute time difference
selected<-if(any(time_difference<=time_window)){TRUE}else{FALSE}

#replacing absolute time by the relative time, i.e. negative if image was acquired
#before low the tide peak and positive if it was aquired after
time_difference<-round(difftime(sat_info, tide_info_time_BM,
                                units = c("mins")),0)

#Output section
output<-list(tide_info_df,time_difference,selected)

return(output)

}




