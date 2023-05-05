#' Data table for supporting data
#' 
#' Generates a data table with an overview of the Line ID's and
#' the stratification label ('År' or 'Områdenavn') for each observation.
#' @param strat Level of stratification - 'No', 'OmradeNavn' or 'Year'
#' @keywords table lineID stratification
#' @export
#' @examples
#' obs_tab(strat = LEVEL)

obs_tab <- function(strat){
  
  B <- subset(d, LinjeAvstand>-1)
  temp <-B[, c("LinjeID" , "cs" , "LinjeAvstand", "Year", "OmradeNavn")]     
  temp$objekt <- seq(1:dim(temp)[1])
  temp$new_lineID <- switch(strat, 
                            Year={paste(temp$Year, temp$LinjeID, sep="_")},
                            OmradeNavn={temp$LinjeID},
                            No={temp$LinjeID})
  
  Obs_table <- switch(strat, 
                      Year={temp[, c("objekt", "new_lineID", "Year")]},
                      OmradeNavn={temp[, c("objekt", "new_lineID", "OmradeNavn")]}, 
                      No={temp[, c("objekt", "new_lineID", "Year")]})
  
  colnames(Obs_table) <- c("object", "Sample.Label", "Region.Label")
  Obs_table
  }