#' Data table for cluster size - total
#' 
#' Generates a data table with an overview of the number of observations,
#' the distance from the transect line to each observation, and the tetraonid cluster size.
#' @param strat Level of stratification - 'No', 'OmradeNavn' or 'Year'
#' @param dataset custom dataset or subset (in case of multiple estimates from same data)
#' @keywords table cluster size total
#' @export
#' @examples
#' dat_tab(strat = LEVEL)
#' dat_tab(dataset = DATA, strat = LEVEL)

dat_tab <- function(strat, dataset){
  if(missing(dataset)) {
    indata <- d
  } else {
    indata <- dataset
  }
  
  B <- subset(indata, LinjeAvstand > -1)
  temp <- B[, c("LinjeID" , "cs" , "LinjeAvstand", "Year", "OmradeNavn")]     
  temp$objekt <- seq(1:dim(temp)[1])
  temp$new_lineID <- switch(strat, 
                            Year={paste(temp$Year, temp$LinjeID, sep="_")},
                            OmradeNavn={temp$LinjeID},
                            No={temp$LinjeID})
  B_dat <- switch(strat, 
                  Year={temp[, c("objekt", "LinjeAvstand", "cs", "Year")]},
                  OmradeNavn={temp[, c("objekt", "LinjeAvstand", "cs", "OmradeNavn")]},
                  No={temp[, c("objekt", "LinjeAvstand", "cs")]})
  
  colnames(B_dat) <- switch(strat,
                            Year={c("object", "distance", "size", "Strata")},
                            OmradeNavn={c("object", "distance", "size", "Strata")},
                            No={c("object", "distance", "size")})
  
  Data_table <- transform(B_dat, distance=distance/1000)
  Data_table
}
