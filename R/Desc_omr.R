#' Data table for descriptive statistics - per area
#' 
#' Displays summarized statistics per area, including number of transect lines and 
#' kilometers inventoried, number of total observations and the number of birds counted.
#' @keywords table descriptive statistics area
#' @export
#' @examples
#' Desc_omr

Desc_omr <- function(){
  
  Navn <- sort(unique(Sample_table$Region.Label))
  dims <- length(Navn)
  temp <- as.data.frame(matrix(ncol=5, nrow=dims+1))
  
  temp[1,1] <- "Totalt"
  temp[1,2] <- dim(Sample_table)[1]
  temp[1,3] <- round(sum(Sample_table$Effort),1)
  temp[1,4] <- dim(Obs_table)[1]
  temp[1,5] <- sum(Data_table$size)
  
  for(i in 1:dims){
    tempo2 <- subset(Sample_table, Region.Label==Navn[i])
    temp[i+1,1] <- paste(Navn[i])
    temp[i+1,2] <- dim(tempo2)[1]
    temp[i+1,3] <- round(sum(tempo2$Effort),1)
    tempo3 <- subset(Obs_table, Region.Label==Navn[i])
    temp[i+1,4] <- dim(tempo3)[1]
    tempo4 <- subset(Data_table, Strata==Navn[i])
    temp[i+1,5] <- sum(tempo4$size)    
  }
  
  colnames(temp) <- c("Omr\u00E5de", "Ant. linjer", "Km. taksert", "Ant. observasjoner", "Ant. fugl")
  temp
}
