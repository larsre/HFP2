#' Data table for export of estimates - per area
#' 
#' Generates a data table with estimates, confidence levels, brood sizes
#' etc. per area, ready for import into HFP.
#' @param userID The user ID of the person analysing the data
#' @param Navn The name of the person analysing the data
#' @keywords table export area
#' @export
#' @examples
#' \dontrun{
#' Resultat_for_opplast_OmrID(userID=16, Navn="Erlend Nilsen")
#' }

Resultat_for_opplast_OmrID <- function(userID=16, Navn="Erlend Nilsen"){

  tempo <- dim(ds.model1$dht$individuals$D)[1]
  O_Navn <- sort(unique(d$OmradeNavn))
  
  temp <- as.data.frame(matrix(ncol=22 , nrow=tempo-1))
  
  for(i in 1:(tempo-1)){
    temp[i,1] <- paste(userID)  # Inserting userID
    temp[i,2] <- paste(format(Sys.Date(), "%d.%m.%Y")) #Inserting date
    temp[i,3] <- paste(Navn) # inserting name
    temp[i,4] <- ifelse(Art1=="Lirype", 1, 6) # inserting speciesID
    temp[i,5] <- d$RegionID[1] # inserting RegionID
    temp[i,6] <- unique(d[which(d$OmradeNavn==O_Navn[i]),]$OmradeID)
    temp[i,7] <- d$Year[1]
    temp[i,22] <- paste(d$Rapporteringsniva[1])
    
    # Density
    tempo <- dim(ds.model1$dht$individuals$D)[1]
    temp[i,8] <- round(ds.model1$dht$individuals$D$Estimate[i], 1)
    temp[i,9] <- round(ds.model1$dht$individuals$D$cv[i], 2)
    temp[i,10] <- round(ds.model1$dht$individuals$D$lcl[i], 2)
    temp[i,11] <- round(ds.model1$dht$individuals$D$ucl[i], 2)
    
    # Adult density
    tempo <- dim(ds.model1$dht$individuals$D)[1]
    temp[i,12] <- round(ds.model2$dht$individuals$D$Estimate[i], 1)
    temp[i,13] <- round(ds.model2$dht$individuals$D$cv[i], 2)
    temp[i,14] <- round(ds.model2$dht$individuals$D$lcl[i], 2)
    temp[i,15] <- round(ds.model2$dht$individuals$D$ucl[i], 2)
    
    # Effective strip width (ESW)
    E_P <- summary(ds.model1)$ds$average.p
    ESW <- as.integer(E_P*as.numeric(ds.model1$ddf$meta.data$width)*1000)
    ESW_cv <- summary(ds.model1)$ds$average.p.se/summary(ds.model1)$ds$average.p
    temp[i,16] <- ESW
    temp[i,17] <- round(ESW_cv,2)
    
    # Recruits/production
    temp[i,18] <- Rekrutt_strat[i,1]
    temp[i,19] <- Rekrutt_strat[i,1]-(1.96*Rekrutt_strat[i,2])
    temp[i,20] <- Rekrutt_strat[i,1]+(1.96*Rekrutt_strat[i,2])
    temp[i,21] <- round(Rekrutt_strat[i,2]/Rekrutt_strat[i,1],2)  
    
  }
  colnames(temp) <- c("FK_BrukerID", "Dato", "AnalysertAv", "FK_ArtID", "FK_RegionID", 
                      "FK_OmradeID","Aar", "Tetthet", "Tetthet_CV", 
                      "Tetthet_Nedre","Tetthet_Ovre", "Voksen_Tetthet", "Voksen_Tetthet_cv",
                      "Voksen_Tetthet_Nedre", "Voksen_Tetthet_Ovre", "ESW",
                      "ESW_cv", "Kylling_Par", "Kylling_Par_Nedre", "Kylling_Par_Ovre",
                      "Kylling_Par_cv", "RapporteringsNiva")
  rownames(temp) <- NULL
  
  temp
}