#' @title Function read and parse tidal times from the site ' https://tides.mobilegeographics.com
#' @description read and parse tidal times from the site ' https://tides.mobilegeographics.com
#' @param tide_date date in the format "yyyy-mm-dd" (e.g. "2018-12-01")
#' @param site_id code for the tidal site
#' @return a dataframe with the tide times corrected to UTC, the tidal height and
#' if they correspond to high tide (PM) or low tide (BM)
#' @export
tide_info_all<-function(tide_date,site_id){
#this is here because the read_html function is not being exported by the rvest package

  library('rvest')
  #tide_date<-"2018-12-01"

  year<-substr(tide_date,1,4)
  month<-substr(tide_date,6,7)
  day<-substr(tide_date,9,10)


url<- paste('https://tides.mobilegeographics.com/locations/',site_id,'.html?y=',year,'&m=',month,'&d=',day,sep="")

webpage <- read_html(url)

p1<-data.table::fread(rvest::html_text(rvest::html_nodes(webpage,'.predictions-table')),
                      fill=TRUE)

#creating the date to select lines
year<-as.character(year)
month<-as.character(month)
day<-as.character(day)

#select just the lines that correspond to the right date
select_date<-paste(year,"-",month,"-",day,sep='')
p1<-p1[p1$V1==select_date,]

#removing the lines that do not correspond to tidal info
p1<-p1[p1$V9=='Tide',]

#print(p1)
#creating the time column formated in UTC
var_temp<-paste(p1$V1,p1$V3,p1$V4, sep=" ")
#print(var_temp)

code_timezone<-unique(p1$V5)

#print(code_timezone)

#AM/PM format causes some problems in non-english systems so in these
#I removed the %p option and it works in most situations

my_locale<-Sys.getlocale(category = "LC_ALL")
my_locale<-substr(my_locale,1,11)

#print(my_locale)

if(my_locale=="fr_FR.UTF-8"){
  formatted.time <- strptime(var_temp, format="%Y-%m-%d %I:%M %p",tz='UTC')
}else{
  formatted.time <- strptime(var_temp, format="%Y-%m-%d %I:%M %p",
                             tz='UTC')
}

#print(formatted.time)

#converting to UTC depending on the time zone code written on the website
if(code_timezone=="CEST"){formatted.time<-formatted.time-2*60*60}
if(code_timezone=="CET"){formatted.time<-formatted.time-1*60*60}
if(code_timezone=="ADT"){formatted.time<-formatted.time+3*60*60}
if(code_timezone=="AST"){formatted.time<-formatted.time+4*60*60}
if(code_timezone=="WEST"){formatted.time<-formatted.time-1*60*60}

#print(formatted.time)


#print(formatted.time)

#start constructing the dataframe
#it should have these 3 columns: tidal_type, tidal_time, tidal_height
tidal_table<-as.data.frame(matrix(NaN, ncol = 3, nrow = (nrow(p1))))
names(tidal_table)<-c('tidal_type','tidal_time','tidal_height')

#adding tidal height
tidal_table$tidal_height<-as.numeric(p1$V6)
#adding tidal time
tidal_table$tidal_time<-formatted.time


#############################################################################
#calculating the type of tide. Low tide (BM), hight tide (PM)
#############################################################################
for (i in 1:(nrow(p1)-1)){
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

