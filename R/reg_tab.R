#' Data table for stratification level area coverage
#' 
#' Generates a data table with an overview of the area covered for each
#' stratification level ('No'/'Year' defaults to 1000).
#' @param strat Level of stratification - 'No', 'OmradeNavn' or 'Year'
#' @keywords table area coverage
#' @export
#' @examples
#' reg_tab(strat = LEVEL)

reg_tab <- function(strat){
  Region_tab <- switch(strat,
                       Year={as.data.frame(cbind(Region.Label=sort(unique(d$Year)), Area=1000))},
                       OmradeNavn={as.data.frame(cbind(Region.Label=paste(unique(d$OmradeNavn)), Area=1000))},
                       No={as.data.frame(cbind(Region.Label=sort(unique(d$Year)), Area=1000))}
                      )
  
  Region_tab <- transform(Region_tab, Area=as.numeric(Area))
}
