#' Data table for model estimates - per area
#' 
#' Displays the estimated total density of tetraonids, the density of
#' adults, and the production of juveniles, with upper and lower
#' confidence levels and coefficient of variation, for each level of
#' stratification ('OmradeNavn').
#' NB! 'strat' must be 'OmradeNavn' - only use if this level of stratification
#' has been used throughout the analysis.
#' @param strat Level of stratification - 'No', 'OmradeNavn' or 'Year'
#' @keywords table model estimates area
#' @export
#' @examples
#' omr_res(strat = LEVEL)

omr_res <- function(strat=strat){

  Navn <- sort(unique(d$OmradeNavn))
  dims <- length(Navn)
  temp <- as.data.frame(matrix(ncol=6, nrow=3*dims+dims))
  OmrNa <- c(1,seq(5, 100, by=4))
  S1 <- seq(2,100, by=4)
  S2 <- seq(4,100, by=4)
  
  for(i in 1:dims){
    temp[OmrNa[i],1] <- paste(Navn[i])
    temp[S1[i]:S2[i],2] <- c("Total tetthet", "Tetthet av voksen fugl", "Kyllingproduksjon")
    
    temp[S1[i],3] <- paste(round(ds.model1$dht$individuals$D$Estimate[i], 1))
    temp[S1[i],4] <- paste(round(ds.model1$dht$individuals$D$lcl[i], 1))
    temp[S1[i],5] <- paste(round(ds.model1$dht$individuals$D$ucl[i], 1))
    temp[S1[i],6] <- paste(round(ds.model1$dht$individuals$D$cv[i], 2))
    
    temp[S1[i]+1,3] <- paste(round(ds.model2$dht$individuals$D$Estimate[i], 1))
    temp[S1[i]+1,4] <- paste(round(ds.model2$dht$individuals$D$lcl[i], 1))
    temp[S1[i]+1,5] <- paste(round(ds.model2$dht$individuals$D$ucl[i], 1))
    temp[S1[i]+1,6] <- paste(round(ds.model2$dht$individuals$D$cv[i], 2))
    
    temp[S1[i]+2,3] <- round(Rekrutt_strat[i,1],1)
    temp[S1[i]+2,4] <- round(Rekrutt_strat[i,1]-(2*Rekrutt_strat[i,2]),1)
    temp[S1[i]+2,5] <- round(Rekrutt_strat[i,1]+(2*Rekrutt_strat[i,2]),1)
    temp[S1[i]+2,6] <- round(Rekrutt_strat[i,2]/Rekrutt_strat[i,1],2)
    
  }
  
  colnames(temp) <- c("Omr\u00E5denavn", " - ", "Estimat", "Nedre CL", "\u00D8vre CL", "CV")
  rownames(temp) <- NULL
  
  temp
}
