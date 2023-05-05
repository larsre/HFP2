#' Data table for descriptive statistics - total
#' 
#' Displays summarized statistics, including no. of transect lines and kilometers
#' taxated, number of total observations and the number of birds counted.
#' @keywords table descriptive statistics total
#' @export
#' @examples
#' Desc_all

Desc_all <- function(){
  
  temp <- as.data.frame(matrix(ncol=5, nrow=1))
  temp[,2] <- dim(Sample_table)[1]
  temp[,3] <- round(sum(Sample_table$Effort),1)
  temp[,4] <- dim(Obs_table)[1]
  temp[,5] <- sum(Data_table$size)
  colnames(temp) <- c("-", "Ant. linjer", "km taksert", "Ant. observasjoner", "Ant. fugl")
  temp
}