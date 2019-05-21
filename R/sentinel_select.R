#' @title Function to query Sentinel Copernicus to check for available images
#' @description Requires a registred  Copernicus SciHub account (https://scihub.copernicus.eu/)
#' Uses the package getSpatialData to query and download images.
#' @param start_date intial date in the format yyyy-mm-dd (e.g. 2018-01-01)
#' @param end_date final date in the format yyyy-mm-dd (e.g. 2018-12-01)
#' @param aoi a shapefile with the area to be sampled
#' @param download_path path for the folder where the images should be downloaded
#' @param login user name for the Copernicus SciHub account
#' @param pw password for the Copernicus SciHub account
#' @param platform Choice of Satellite (Sentinel-1, Sentinel-2, Sentinel-3)
#' @param product Choice of product type (L1C, L2Ap and L2A available)
#' @param id code to select both satellites (S2), only satellite A (S2A) and only satellite B (S2B)
#' @param cloud_percentage parameter to exclude images that have less than the percentage of clouds defined in this parameter
#' @return A dataframe with the list of files avilable for potential download
#' @export
sentinel_query<-function(start_date,
         end_date,
         aoi,
         download_path='./',
         login,
         pw,
         platform,
         product,
         id,
         cloud_percentage){

#login in SciHub
getSpatialData::login_CopHub(username=login, password = pw)

#defining a download path
#see if I can remove this from here and leave it to the other function to decide on
#download or not
getSpatialData::set_archive(download_path)

#defining the area of interest using a shapefile
aoi<-sf::st_read(aoi)
aoi<-aoi$geometry
getSpatialData::set_aoi(aoi)

#defining time frame
time_window <- c(start_date, end_date)
print(time_window)

#returning the results
records<-getSpatialData::getSentinel_query(time_range = time_window, platform = platform)

#filtering the data using the cloud cover percentage
records$highprobacloudspercentage<-as.numeric(records$highprobacloudspercentage)

records <- records[which(records$highprobacloudspercentage <= cloud_percentage),]

#print(records)
#filtering by product type
if (product=='L1C'&id=='S2'){
  records_filtered <- records[which(records$processinglevel == "Level-1C"),]
}

if (product=='L1C'&id=='S2A'){
  records_filtered <- records[which(records$processinglevel == "Level-1C"&records$platformserialidentifier =='Sentinel-2A'),]
}

if (product=='L1C'&id=='S2B'){
  records_filtered <- records[which(records$processinglevel == "Level-1C"&records$platformserialidentifier =='Sentinel-2B'),]
}


if (product=='L2A'&id=='S2'){
  records_filtered <- records[which(records$processinglevel == "Level-2A"),]
}


if (product=='L2A'&id=='S2A'){
  records_filtered <- records[which(records$processinglevel == "Level-2A"&records$platformserialidentifier =='Sentinel-2A'),]
}


if (product=='L2A'&id=='S2B'){
  records_filtered <- records[which(records$processinglevel == "Level-2A"&records$platformserialidentifier =='Sentinel-2B'),]
}


if (product=='L2Ap'&id=='S2'){
  records_filtered <- records[which(records$processinglevel == "Level-2Ap"),]
}


if (product=='L2Ap'&id=='S2A'){
  records_filtered <- records[which(records$processinglevel == "Level-2Ap"&records$platformserialidentifier =='Sentinel-2A'),]
}


if (product=='L2Ap'&id=='S2B'){
  records_filtered <- records[which(records$processinglevel == "Level-2Ap"&records$platformserialidentifier =='Sentinel-2B'),]
}





#print(records_filtered)
return(records_filtered)



}





