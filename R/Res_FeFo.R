#' Data table for model estimates - FeFo
#' Displays the estimated total density of tetraonids, the density of
#' adults, and the production of juveniles, with upper and lower
#' confidence levels and coefficient of variation. This function only
#' applies to FeFo (Finnmarkseiendommen), and includes total estimates
#' and estimates per region.
#' @param strat Level of stratification ('No', 'OmradeNavn' or 'Year')
#' @keywords table model estimates FeFo
#' @export
#' @examples
#' \dontrun{
#' Res_FeFo(strat = LEVEL)
#' }

Res_FeFo <- function(strat=strat){
  
  Navn <- sort(unique(d$OmradeNavn))
  dims <- length(Navn)
  temp <- as.data.frame(matrix(ncol=6, nrow=15))
  
  #Samle-estimat
  
  temp[1,1] <- paste("Samleestimat")
  temp[2:4,2] <- c("Total tetthet", "Tetthet av voksen fugl", "Kyllingproduksjon")
  
  #Samle-estimat
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
  
  #Vest Finnmark kyst
  temp[5,1] <- paste("Vest Finnmark kyst")
  temp[c(6:8),2] <- c("Total tetthet", "Tetthet av voksen fugl", "Kyllingproduksjon")
  
  temp[6,3] <- paste(round(ds.model1_vest$dht$individuals$D$Estimate[1], 1))
  temp[6,4] <- paste(round(ds.model1_vest$dht$individuals$D$lcl[1], 1))
  temp[6,5] <- paste(round(ds.model1_vest$dht$individuals$D$ucl[1], 1))
  temp[6,6] <- paste(round(ds.model1_vest$dht$individuals$D$cv[1], 2))
  
  temp[7,3] <- paste(round(ds.model2_vest$dht$individuals$D$Estimate[1], 1))
  temp[7,4] <- paste(round(ds.model2_vest$dht$individuals$D$lcl[1], 1))
  temp[7,5] <- paste(round(ds.model2_vest$dht$individuals$D$ucl[1], 1))
  temp[7,6] <- paste(round(ds.model2_vest$dht$individuals$D$cv[1], 2))
  
  temp[8,3] <- round(Rekrutt_all_vest[1],1)
  temp[8,4] <- round(Rekrutt_all_vest[1]-(2*Rekrutt_all_vest[2]),1)
  temp[8,5] <- round(Rekrutt_all_vest[1]+(2*Rekrutt_all_vest[2]),1)
  temp[8,6] <- round(Rekrutt_all_vest[2]/Rekrutt_all_vest[1],2)
  
  #\u00D8st-Finnmark
  temp[9,1] <- paste("\u00D8st Finnmark")
  temp[c(10:12),2] <- c("Total tetthet", "Tetthet av voksen fugl", "Kyllingproduksjon")
  
  temp[10,3] <- paste(round(ds.model1_east$dht$individuals$D$Estimate[1], 1))
  temp[10,4] <- paste(round(ds.model1_east$dht$individuals$D$lcl[1], 1))
  temp[10,5] <- paste(round(ds.model1_east$dht$individuals$D$ucl[1], 1))
  temp[10,6] <- paste(round(ds.model1_east$dht$individuals$D$cv[1], 2))
  
  temp[11,3] <- paste(round(ds.model2_east$dht$individuals$D$Estimate[1], 1))
  temp[11,4] <- paste(round(ds.model2_east$dht$individuals$D$lcl[1], 1))
  temp[11,5] <- paste(round(ds.model2_east$dht$individuals$D$ucl[1], 1))
  temp[11,6] <- paste(round(ds.model2_east$dht$individuals$D$cv[1], 2))
  
  temp[12,3] <- round(Rekrutt_all_east[1],1)
  temp[12,4] <- round(Rekrutt_all_east[1]-(2*Rekrutt_all_east[2]),1)
  temp[12,5] <- round(Rekrutt_all_east[1]+(2*Rekrutt_all_east[2]),1)
  temp[12,6] <- round(Rekrutt_all_east[2]/Rekrutt_all_east[1],2)  
  
  #Indre Finnmark
  temp[13,1] <- paste("Indre Finnmark")
  temp[c(14:16),2] <- c("Total tetthet", "Tetthet av voksen fugl", "Kyllingproduksjon")
  
  temp[14,3] <- paste(round(ds.model1_indre$dht$individuals$D$Estimate[1], 1))
  temp[14,4] <- paste(round(ds.model1_indre$dht$individuals$D$lcl[1], 1))
  temp[14,5] <- paste(round(ds.model1_indre$dht$individuals$D$ucl[1], 1))
  temp[14,6] <- paste(round(ds.model1_indre$dht$individuals$D$cv[1], 2))
  
  temp[15,3] <- paste(round(ds.model2_indre$dht$individuals$D$Estimate[1], 1))
  temp[15,4] <- paste(round(ds.model2_indre$dht$individuals$D$lcl[1], 1))
  temp[15,5] <- paste(round(ds.model2_indre$dht$individuals$D$ucl[1], 1))
  temp[15,6] <- paste(round(ds.model2_indre$dht$individuals$D$cv[1], 2))
  
  temp[16,3] <- round(Rekrutt_all_indre[1],1)
  temp[16,4] <- round(Rekrutt_all_indre[1]-(2*Rekrutt_all_indre[2]),1)
  temp[16,5] <- round(Rekrutt_all_indre[1]+(2*Rekrutt_all_indre[2]),1)
  temp[16,6] <- round(Rekrutt_all_indre[2]/Rekrutt_all_indre[1],2) 
  
  colnames(temp) <- c("Omr\u00E5denavn", " - ", "Estimat", "Nedre CL", "\u00D8vre CL", "CV")
  rownames(temp) <- NULL
  
  temp
}