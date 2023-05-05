#' Figure - detection probability - alternate graph
#' 
#' Plots two graphs: a) detection probability with increasing distance from 
#' the transect line, and b) detection curve with standard error.
#' @param klasser Number of classes in the histogram
#' @param model1 The Distance sampling model to be used
#' @keywords figure detection probability alternate
#' @export
#' @examples
#' \dontrun{
#' Fig1_B(klasser=11, model1=ds.model1)
#' }

Fig1_B <- function(klasser=11, model1=ds.model1){
  
  Art2 <- ifelse(Art1=="Lirype", "lirype", "skogsfugl")
  
  
  # Extract model parameters
  model <- model1$ddf
  ltmodel <- model$ds 
  width <- model$meta.data$width 
  left <- model$meta.data$left 
  ddfobj <- ltmodel$aux$ddfobj 
  point <- ltmodel$aux$point 
  int.range <- ltmodel$aux$int.range
  Nhat <-model$Nhat
  
  breaks <- seq(left,width, length.out=klasser) 
  nc <- length(breaks)-1
  selected <- rep(TRUE,nrow(ddfobj$xmat))
  xmat <- ddfobj$xmat
  expected.counts <- (breaks[2:(nc+1)]-breaks[1:nc])*(Nhat/breaks[nc+1])
  
  hdat <- xmat$distance
  hist.obj <- hist(hdat, breaks=breaks, plot=FALSE) 
  hist.obj$density <- hist.obj$counts/expected.counts 
  hist.obj$density[expected.counts==0] <- 0 
  freq <- hist.obj$density 
  hist.obj$equidist <- FALSE 
  
  sigma <- exp( summary(model1)$ds$coef$key.scale$estimate) *1000
  sigma_upper <- exp( (summary(model1)$ds$coef$key.scale$estimate)+(summary(model1)$ds$coef$key.scale$se) )*1000
  sigma_lower <- exp( (summary(model1)$ds$coef$key.scale$estimate)-(summary(model1)$ds$coef$key.scale$se) )*1000
  
  
  # Estimate effective strip width (ESW) 
  E_P <- summary(model1)$ds$average.p
  ESW <- as.integer(E_P*as.numeric(width)*1000)
  ESW_cv <- summary(model1)$ds$average.p.se/summary(model1)$ds$average.p
  ESW_se <- as.integer(ESW*ESW_cv)
  
  
  # Estimate g(x) for half-normal model
  x <- seq(left*1000, width*1000, length.out=1000)
  
  x2 <- x^2
  sigma2 <- sigma^2
  sigma2_lower <- sigma_lower^2
  sigma2_upper <- sigma_upper^2
  
  gx <- exp(-x2/(2*sigma2))
  gx_lower <- exp(-x2/(2*sigma2_lower))
  gx_upper <- exp(-x2/(2*sigma2_upper))  
  
  
  # Plot g(x) and observations
  par(mfrow=c(1,2))
  
  par(bty="l")
  plot(x=c(0, 0), y=c(0,0), xlim=c(0, max(breaks*1000)), ylim=c(0, max(hist.obj$density)+0.1), 
       type="n", xlab="Linjeavstand", ylab="Oppdagbarhet")  
  
  
  for(i in 1:nc){
    temp1 <- hist.obj$density[i]
    temp2 <- breaks[c(i, i+1)]
    temp2 <- temp2*1000
    polygon(x=c(temp2[1], temp2[1], temp2[2], temp2[2]), y=c(0, temp1, temp1, 0), col="slategray2")    
  }
  
  lines(x, gx, lwd=3, col="dark red")
  mtext(side=3, outer=T, paste("Oppdagbarhetskurve for", Art2, "i", d$OmradeNavn[1], "i", d$Year[1]), line=-1, cex=1.1, adj=0.1)
  mtext(side=3, outer=T, paste("ESW: ", ESW, "meter (SE:", ESW_se, "meter)"), line=-2, cex=0.9, adj=0.07)
  mtext(side=3, outer=T, paste("Trunkering: ", width*1000, "meter"), line=-3, cex=0.9, adj=0.06)
  
  
  # Plot g(x) with standard error
  par(bty="l")
  plot(x, gx, ylim=c(0,1), xlim=c(left, width*1000), lwd=3, type="l", xlab="Linjeavstand", ylab="Oppdagbarhet")
  lines(x, gx_lower)
  lines(x, gx_upper)
  polygon(x=c(x, rev(x)), y=c(gx_upper, rev(gx_lower)), col=adjustcolor("blue", alpha=0.1), border=NA)  
}
