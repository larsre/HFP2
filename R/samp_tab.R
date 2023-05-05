#' Data table for sampling effort
#' 
#' Generates a data table with an overview of the LineID's, stratification
#' level ('År' or 'Områdenavn') and the effort (distance taxated) for each LineID.
#' @param strat Level of stratification - 'No', 'OmradeNavn' or 'Year'
#' @keywords table sampling effort
#' @export
#' @examples
#' samp_tab(strat = LEVEL)

samp_tab <- function(strat){
  
  Sample_tab <- switch(strat,
                       
                       Year={
                         sample_tab <- matrix(ncol=3, nrow=0)
                         tempA <- sort(unique(d$Year))
                         
                         for(i in unique(tempA)){
                           temp1 <- subset(d, Year==i)
                           tempB <- sort(unique(temp1$LinjeID))
                           
                           for(j in unique(tempB)){
                             temp2 <- subset(temp1, LinjeID==j)
                             tempD <- as.data.frame(cbind(paste(i,j, sep="_"), i)) 
                             tempE <- as.numeric(temp2$LengdeTaksert[1])
                             tempF <- cbind(tempD, tempE)
                             sample_tab <- rbind(sample_tab, tempF)
                           }
                         }
                       }, 
                       
                       OmradeNavn={
                         tempA <- sort(unique(d$LinjeID))
                         sample_tab <- as.data.frame(matrix(ncol=3, nrow=0))
                         
                         for(i in unique(tempA)){
                           temp1 <- subset(d, LinjeID==i)
                           tempB <- temp1[c("LinjeID", "OmradeNavn", "LengdeTaksert")][1,]
                           sample_tab <- rbind(sample_tab, tempB)
                         }
                       }, 
                       
                       No={
                         
                         tempA <- sort(unique(d$LinjeID))
                         sample_tab <- as.data.frame(matrix(ncol=3, nrow=0))
                         
                         for(i in unique(tempA)){
                           temp1 <- subset(d, LinjeID==i)
                           tempB <- temp1[c("LinjeID", "Year", "LengdeTaksert")][1,]
                           sample_tab <- rbind(sample_tab, tempB)
                         }
                       }
                    )
  
  Sample_tab <- as.data.frame(sample_tab)
  colnames(Sample_tab) <- c("Sample.Label", "Region.Label", "Effort")
  Sample_tab <- transform(Sample_tab, Effort=Effort/1000)
  Sample_tab
}