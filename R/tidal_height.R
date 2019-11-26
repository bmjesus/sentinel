#' @title internal funtion
#' @description internal function
#' @param H1 height 1
#' @param H2 height 2
#' @param T1 time 1
#' @param T2 time 2
#' @param t time point
#' @return numeric value with tidal height
#' @export
tidal_height<-function(H1,H2,T1,T2,t){
  a<-as.numeric(t-T1, units = "mins")
  b<-as.numeric(T2-T1, units = "mins")
  phi<-pi*((a/b)+1)
  #print(a)

  return( H1+((H2-H1)/2)*(1+cos(phi)))


}

