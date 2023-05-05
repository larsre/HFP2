#' Data table for model estimates - combined total and per area
#' 
#' Displays the estimated total density of tetraonids, the density of
#' adults, and the production of juveniles, with upper and lower
#' confidence levels and coefficient of variation. Additionally, displays 
#' the estimates for each area ('OmradeNavn'), if this level
#' of stratification is specified.
#' @param strat Level of stratification - 'No', 'Omradenavn' or 'Year'
#' @keywords table model estimates combined
#' @export
#' @examples
#' felles_res(strat = LEVEL)

felles_res <- function(strat=strat){
  
  Navn <- sort(unique(d$OmradeNavn))
  dims <- length(Navn)
  temp <- as.data.frame(matrix(ncol=6, nrow=5*dims+1))
  OmrNa <- c(5,seq(9, 100, by=4))
  S1 <- seq(6,100, by=4)
  S2 <- seq(8,100, by=4)
  
  temp[1,1] <- paste("Samleestimat")
  temp[2:4,2] <- c("Total tetthet", "Tetthet av voksen fugl", "Kyllingproduksjon")
  
  tempo <- dim(ds.model1$dht$individuals$D)[1]
  temp[2,3] <- paste(round(ds.model1$dht$individuals$D$Estimate[tempo], 1))
  temp[2,4] <- paste(round(ds.model1$dht$individuals$D$lcl[tempo], 1))
  temp[2,5] <- paste(round(ds.model1$dht$individuals$D$ucl[tempo], 1))
  temp[2,6] <- paste(round(ds.model1$dht$individuals$D$cv[tempo], 2))
  
  tempo <- dim(ds.model1$dht$individuals$D)[1]
  temp[3,3] <- paste(round(ds.model2$dht$individuals$D$Estimate[tempo], 1))
  temp[3,4] <- paste(round(ds.model2$dht$individuals$D$lcl[tempo], 1))
  temp[3,5] <- paste(round(ds.model2$dht$individuals$D$ucl[tempo], 1))
  temp[3,6] <- paste(round(ds.model2$dht$individuals$D$cv[tempo], 2))
  
  temp[4,3] <- round(Rekrutt_all[1],1)
  temp[4,4] <- round(Rekrutt_all[1]-(1.96*Rekrutt_all[2]),1)
  temp[4,5] <- round(Rekrutt_all[1]+(1.96*Rekrutt_all[2]),1)
  temp[4,6] <- round(Rekrutt_all[2]/Rekrutt_all[1],2)
  
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