#' @title function to calculate NDVI from SAFE Sentinel folders
#' @description function to calculate NDVI from SAFE Sentinel folders
#' @param image_folder folder with the SAFE sentinel structure
#' @param clip_layer optional layer to clip the original image
#' @param export optional parameter to export the image in Tif format
#' @return a stack object with the ndvi image
#' @export
ndwi_sentinel<-function(image_folder,clip_layer = NaN,export = FALSE){

  #list_of_files<-list.files(image_folder, pattern="L2")
  #NEED to FIX folder path for something intelgent
    base_dir<-getwd()
    #print(base_dir)

    #setwd(image_folder)


    green<- list.files(path=image_folder,full.names = TRUE,recursive=TRUE, pattern="B03_10m.jp2")
    #red<-paste(base_dir,image_folder,red,sep='/')
    nir<- list.files(path=image_folder,full.names = TRUE,recursive=TRUE, pattern="B08_10m.jp2")
    #nir<-paste(base_dir,image_folder,nir,sep='/')
    bands<-c(green,nir)

    print(bands)

    #new calculations
    green<-raster::brick(green)
    nir<-raster::brick(nir)

    if(is.na(clip_layer) == FALSE){
      clip_layer<-rgdal::readOGR(clip_layer)

      green<-raster::crop(x = green, y = clip_layer)
      nir<-raster::crop(x = nir, y = clip_layer)
    }

    ndwi<-(green-nir)/(green+nir)

    setwd(base_dir)

    #if export the image is set to TRUE

    if (export == TRUE){

      #setting up the export name

      output_name <- list.files(path=image_folder,full.names = FALSE,recursive=TRUE, pattern="B03_10m.jp2")
      #calculating the total length of the path so that we can count from the end
      my_length<-nchar(output_name)
      output_name<-substr(output_name,my_length-27,my_length-12)
      output_name <- paste('ndwi', output_name,'.tif',sep='')

      #export the image to the working directory
      writeRaster(ndwi, filename = output_name,
                  datatype="FLT4S",
                  options=c("compress=lzw"),
                  overwrite=T)
    }

    return(ndwi)

}


