#' Data table for export of estimates - forest birds
#' 
#' Generates a data table with estimates, confidence levels, brood sizes
#' etc., ready for import into HFP. Only applies to forest birds, i.e.
#' Capercaillie and Black grouse.
#' @param userID The user ID of the person analysing the data
#' @param Navn The name of the person analysing the data
#' @keywords table export forest birds
#' @export
#' @examples
#' Resultat_for_opplast_RappNiva_skogsfugl(userID=16, Navn="Erlend Nilsen")

Resultat_for_opplast_RappNiva_skogsfugl <- function(userID=as.integer(), Navn=as.character()){
  
  temp <- as.data.frame(matrix(ncol=22 , nrow=1))
  
  temp[1,1] <- paste(userID)  # Inserting userID
  temp[1,2] <- paste(format(Sys.Date(), "%d.%m.%Y")) #Inserting date
  temp[1,3] <- paste(Navn) # inserting name
  temp[1,4] <- ifelse(Art1=="Lirype", 1, 6) # inserting speciesID
  temp[1,5] <- d$RegionID[1] # inserting RegionID
  temp[1,6] <- "NULL"
  temp[1,7] <- d$Year[1]
  temp[1,22] <- paste(d$Rapporteringsniva[1])
  
  ### Tetthet
  tempo <- dim(ds.model1$dht$individuals$D)[1]
  temp[1,8] <- round(ds.model1$dht$individuals$D$Estimate[tempo], 1)
  temp[1,9] <- round(ds.model1$dht$individuals$D$cv[tempo], 2)
  temp[1,10] <- round(ds.model1$dht$individuals$D$lcl[tempo], 2)
  temp[1,11] <- round(ds.model1$dht$individuals$D$ucl[tempo], 2)
  
  ### Voksentetthet
  tempo <- dim(ds.model1$dht$individuals$D)[1]
  temp[1,12] <- round(ds.model2$dht$individuals$D$Estimate[tempo], 1)
  temp[1,13] <- round(ds.model2$dht$individuals$D$cv[tempo], 2)
  temp[1,14] <- round(ds.model2$dht$individuals$D$lcl[tempo], 2)
  temp[1,15] <- round(ds.model2$dht$individuals$D$ucl[tempo], 2)
  
  ### ESW
  
  E_P <- summary(ds.model1)$ds$average.p
  ESW <- as.integer(E_P*as.numeric(ds.model1$ddf$meta.data$width)*1000)
  ESW_cv <- summary(ds.model1)$ds$average.p.se/summary(ds.model1)$ds$average.p
  temp[1,16] <- ESW
  temp[1,17] <- round(ESW_cv,2)
  
  
  ### Rekrutter
  temp[1,18] <- Rekrutt_skogsfugl[1]
  temp[1,19] <- Rekrutt_skogsfugl[1]-(1.96*Rekrutt_skogsfugl[2])
  temp[1,20] <- Rekrutt_skogsfugl[1]+(1.96*Rekrutt_skogsfugl[2])
  temp[1,21] <- round(Rekrutt_skogsfugl[2]/Rekrutt_skogsfugl[1],2)  
  
  colnames(temp) <- c("FK_BrukerID", "Dato", "AnalysertAv", "FK_ArtID", "FK_RegionID", 
                      "FK_OmradeID","Aar", "Tetthet", "Tetthet_CV", 
                      "Tetthet_Nedre","Tetthet_Ovre", "Voksen_Tetthet", "Voksen_Tetthet_cv",
                      "Voksen_Tetthet_Nedre", "Voksen_Tetthet_Ovre", "ESW",
                      "ESW_cv", "Kylling_Par", "Kylling_Par_Nedre", "Kylling_Par_Ovre",
                      "Kylling_Par_cv", "RapporteringsNiva")
  rownames(temp) <- NULL
  
  temp
}