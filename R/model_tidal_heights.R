#' @title Function to predict the tidal height
#' @description Function to predict the tidal height in a specified site at a user specified time
#' @param start_date date in the format "yyyy-mm-dd" (e.g. "2018-12-01")
#' @param end_date date in the format "yyyy-mm-dd" (e.g. "2018-12-01")
#' @param location to chose the web site to get the tidal information, Fr,Ca or all
#' @param site_id code for the tidal site (check the site url to find the code of the tidal station)
#' @param user_time the specific time to determine the tidal height
#' @return a dataframe with the tide times corrected to UTC, the tidal height and
#' if they correspond to high tide (PM) or low tide (BM)
#' @export

tide_height<-function(start_date,
                       end_date,
                       location = "all",
                       site_id,
                       user_time = NULL
                       ){

tmp_table<-tidal_tables(start_date = start_date,end_date =end_date,location=location,site_id = site_id)

par(mar=c(4,4,1,1))
plot(tmp_table$tidal_time,tmp_table$tidal_height,ylab="Tidal height (m)",xlab="Time")

sequence_tide2<-numeric()
tidal_height2<-numeric()

for (i in 1:((length(tmp_table$tidal_time))-1)){

#print(i)
#create a time sequence between the first 2 points
my_sequence<-seq(tmp_table$tidal_time[i],tmp_table$tidal_time[i+1],by='10 mins')

#print(my_sequence)

sequence_tide<-numeric()
for (v in 1:length(my_sequence)){
  my_tide<-tidal_height(H1=tmp_table$tidal_height[i],
                        H2=tmp_table$tidal_height[i+1],
                        T1=tmp_table$tidal_time[i],
                        T2=tmp_table$tidal_time[i+1],
                        t=my_sequence[v])
  sequence_tide<-c(sequence_tide,my_tide)
}
sequence_tide2<-c(sequence_tide2,my_sequence)
tidal_height2<-c(tidal_height2,sequence_tide)
#points(my_sequence,sequence_tide,cex=0.5,pch=21,col=2,bg=2)

}

points(sequence_tide2,tidal_height2,cex=0.5,pch=21,col=3,bg=3,type='l')


#print(sequence_tide)

#x_axis<-my_sequence[1:match(tmp_table$tidal_time[2],my_sequence)]
#print(x_axis)

if (is.null(user_time)==FALSE){

  user_time<-lubridate::ymd_hm(user_time,tz='UTC')

#need to introduce a filter that finds the H1,H2, T1, T2 that are closer to the
#user time, maybe do some difftime and select the smallest differences?

  my_tide<-tidal_height(H1 = tmp_table$tidal_height[3],
                        H2 = tmp_table$tidal_height[4],
                        T1 = tmp_table$tidal_time[3],
                        T2 = tmp_table$tidal_time[4],
                        t = user_time)
  #print(my_tide)

  points(user_time,my_tide,pch=21,bg=2,cex=1.5)
}
return(my_tide)
}

