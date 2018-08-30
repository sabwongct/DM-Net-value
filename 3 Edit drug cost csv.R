drugcost <- read.csv("C:/Users/janet/Documents/Codes/DM Project/4 drug cost.csv")

LOCF <- function(x){
  if()
  if(is.na(tmp[,-1])){
    
  }


}
LOCF <- function(x) {
  # Last Observation Carried Forward (for a left to right series)
  LOCF <- max(which(!is.na(x))) # the location of the Last Observation to Carry Forward
  x[LOCF:length(x)] <- x[LOCF]
  return(x)
}


a
data.frame(lapply(a, LOCF))
