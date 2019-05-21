#' @title Select sentinel images that correspond to low tide periods and downloads it if required by the user
#' @description Requires a registred Copernicus SciHub account
#' @param start_date intial date in the format yyyy-mm-dd (e.g. 2018-01-01)
#' @param end_date final date in the format yyyy-mm-dd (e.g. 2018-12-01)
#' @param aoi shapefile with area of interest
#' @param download the user can decide to decide to download the images or not, default=FALSE
#' @param download_path path for the folder where images will be downloaded
#' @param login user name for the Copernicus SciHub account
#' @param pw password for the Copernicus SciHub account
#' @param platform Choice of Satellite (Sentinel-1, Sentinel-2, Sentinel-3)
#' @param product Choice of product type (L1C, L2Ap and L2A available)
#' @param time_window Time window (in hours) around low tide for selecting an image
#' @param id code to select both satellites (S2), only satellite A (S2A) and only satellite B (S2B)
#' @param country code to select tidal tables from a particular country, fr for France and all for all other countries
#' @param site_id numerical code to the tidal station, e.g. 114 for Le Croisic
#' @param cloud_percentage parameter to exclude images that have less than the percentage of clouds defined in this parameter
#' @return a dataframe listing all the images that are available and/or downloaded
#' @export
sentinel_low_tide_select<-function(start_date,
                                   end_date,
                                   aoi,
                                   download=FALSE,
                                   download_path,
                                   login,
                                   pw,
                                   platform,
                                   product,
                                   id,
                                   time_window,
                                   country,
                                   site_id,
                                   cloud_percentage,
                                   tile_id = NULL)
  {

#1 - select date, select aoi, product, download path, platform, download option
#creates a list of images available for download
image_list<-sentinel_query(start_date = start_date,end_date = end_date,
                 aoi=aoi, download_path = download_path,
                 login=login,pw=pw,platform = platform,
                 product = product,id=id,cloud_percentage = cloud_percentage,
                 tile_id = tile_id)

#2 - test if the images fit low tide times within a time window
#creates a list of images that fit within the specified low tide period
  list_download<-data.frame()
  for (i in 1:length(image_list$title)){
    result_comparison<-compare_times(sat_info=image_list$title[i],time_window,country = country, site_id=site_id)
    if (result_comparison[[3]]==TRUE){
      print(image_list$title[i])
      list_download<-rbind(list_download,image_list[i,])
    }
  }

#3- donwload images if selected
dir_out<-download_path

if (download==TRUE){
    getSpatialData::getSentinel_data(records=list_download,dir_out = dir_out)
  }

#returns a list of all the images that were downloaded, this might be useful for further processing
return(list_download)

}
