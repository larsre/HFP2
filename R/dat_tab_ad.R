#' Data table for cluster size - adults only
#'
#' Generates a data table with an overview of the number of observations,
#' the distance from the transect line to each observation, and the adult tetraonid cluster size.
#' @param strat Level of stratification - 'No', 'OmradeNavn' or 'Year'
#' @param dataset custom dataset or subset (in case of multiple estimates from same data)
#' @keywords table cluster size adult
#' @export
#' @examples
#' dat_tab_ad(strat = LEVEL)
#' dat_tab_ad(dataset = DATA, strat = LEVEL)

dat_tab_ad <- function(strat, dataset){
  if(missing(dataset)) {
    indata <- d
  } else {
    indata <- dataset
  }
  
  B <- subset(indata, LinjeAvstand > -1)
  temp <- B[, c("LinjeID" , "ad" , "LinjeAvstand", "Year", "OmradeNavn")]     
  temp$objekt <- seq(1:dim(temp)[1])
  temp$new_lineID <- switch(strat, 
                            Year={paste(temp$Year, temp$LinjeID, sep="_")},
                            OmradeNavn={temp$LinjeID},
                            No={temp$LinjeID})
  B_dat <- switch(strat, 
                  Year={temp[, c("objekt", "LinjeAvstand", "ad", "Year")]},
                  OmradeNavn={temp[, c("objekt", "LinjeAvstand", "ad", "OmradeNavn")]},
                  No={temp[, c("objekt", "LinjeAvstand", "ad")]})
  
  colnames(B_dat) <- switch(strat,
                            Year={c("object", "distance", "size", "Strata")},
                            OmradeNavn={c("object", "distance", "size", "Strata")},
                            No={c("object", "distance", "size")})
  
  Data_table <- transform(B_dat, distance=distance/1000)
  Data_table
}