#' This function produces survival demo data
#'
#' @param hazard is the survival hazard
#' @param censor_hazard is the hazard used to censor data
#' @param pts is the number of patient in the data
#' @param maxtime is the tiem after which all events are censored
#' @param arm is a label add to all the data
#'
#' @return return a data frame with the survival demo data
#' @export
#'
#' @examples demo_data(hazard=1.5,censor_hazard=.9,pts=150,maxtime=3,arm=2)
#'
survivval_demo_data <- function(hazard=1.2,censor_hazard=1,pts=160,maxtime=4,arm=1){
  t1 <- -(1/hazard)*log(runif(pts))
  t2 <- -(1/censor_hazard)*log(runif(pts))
  index <- t2<t1
  t <- t1
  t[index] <- t2[index]
  event <- matrix(1,pts,1)
  event[index] <- 0
  index <- t>maxtime
  t[index] <- maxtime
  event[index] <- 0
  df <- data.frame(cbind(t,event,matrix(arm,pts,1)))
  colnames(df) <- c('Time','Event','Arm')
  return(df)
}
#testxxx
