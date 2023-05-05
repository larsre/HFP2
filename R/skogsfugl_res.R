#' Data table for model estimates - forest birds only
#' 
#' Displays the estimated total density of tetraonids, the density of
#' adults, and the production of juveniles, with upper and lower
#' confidence levels and coefficient of variation. For use with
#' Capercaillie and Black grouse only.
#' @param strat Level of stratification - 'No', 'OmradeNavn' or 'Year'
#' @keywords table model estimates forest birds
#' @export
#' @examples
#' skogsfugl_res(strat = LEVEL)

skogsfugl_res <- function(strat=strat){
  
  temp <- as.data.frame(matrix(ncol=5, nrow=3))
  
  temp[,1] <- c("Total tetthet", "Tetthet av voksen fugl", "Kyllingproduksjon")
  tempo <- dim(ds.model1$dht$individuals$D)[1]
  temp[1,2] <- paste(round(ds.model1$dht$individuals$D$Estimate[tempo], 1))
  temp[1,3] <- paste(round(ds.model1$dht$individuals$D$lcl[tempo], 1))
  temp[1,4] <- paste(round(ds.model1$dht$individuals$D$ucl[tempo], 1))
  temp[1,5] <- paste(round(ds.model1$dht$individuals$D$cv[tempo], 2))
  
  tempo <- dim(ds.model2$dht$individuals$D)[1]
  temp[2,2] <- paste(round(ds.model2$dht$individuals$D$Estimate[tempo], 1))
  temp[2,3] <- paste(round(ds.model2$dht$individuals$D$lcl[tempo], 1))
  temp[2,4] <- paste(round(ds.model2$dht$individuals$D$ucl[tempo], 1))
  temp[2,5] <- paste(round(ds.model2$dht$individuals$D$cv[tempo], 2))
  
  temp[3,2] <- round(Rekrutt_skogsfugl[1],1)
  temp[3,3] <- max(0.1, round(Rekrutt_skogsfugl[1]-(1.96*Rekrutt_skogsfugl[2]),1))
  temp[3,4] <- round(Rekrutt_skogsfugl[1]+(1.96*Rekrutt_skogsfugl[2]),1)
  temp[3,5] <- round(Rekrutt_skogsfugl[2]/Rekrutt_skogsfugl[1],2)
  
  
  colnames(temp) <- c(" - ", "Estimat", "Nedre CL", "\u00D8vre CL", "CV")
  rownames(temp) <- NULL
  
  temp
}