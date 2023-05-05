#' Estimate brood size - forest birds
#' 
#' Estimates brood size, i.e. the number of chicks per adult female,
#' based on the level of stratification.
#' NB! Only for use with forest birds, i.e. Capercaillie and Black grouse.
#' @param strat Level of stratification - 'No', 'OmradeNavn' or 'Year'
#' @keywords brood size birds forest
#' @export
#' @examples
#' rec(strat = LEVEL)

rec_skogsfugl <- function(strat){
  
  switch(strat,
         OmradeNavn={
           d_p <- subset(d, cs>0)
           y <- cbind(d_p$AntallKylling, d_p$AntallHunn)
           M1 <- glm(y~as.factor(d_p$OmradeNavn)-1, family="binomial")
           Res1 <- matrix(ncol=3, nrow=length(unique(d_p$OmradeNavn)))
           
           Res1 <- as.matrix(summary(M1)$coef[,c(1,2)])
           Res2 <- exp(Res1[,1])/(1+exp(Res1[,1]))
           Res3 <- exp(Res1[,2])/(1+exp(Res1[,2]))
           
           Res4 <- matrix(ncol=2, nrow=dim(Res1)[1])
           
           for(i in 1:dim(Res1)[1]){
             temp <- rnorm(10000, mean=Res1[i,1], sd=Res1[i,2])
             temp2 <- (exp(temp))/(1+exp(temp))
             temp3 <- temp2/((1-temp2))
             Res4[i,1] <- mean(temp3)
             Res4[i,2] <- sd(temp3)
           }
         },
         
         Year={
           d_p <- subset(d, cs>0)
           y <- cbind(d_p$AntallKylling, d_p$AntallHunn)
           M1 <- glm(y~as.factor(d_p$Year)-1, family="binomial")
           Res1 <- matrix(ncol=3, nrow=length(unique(d_p$Year)))
           
           Res1 <- as.matrix(summary(M1)$coef[,c(1,2)])
           Res2 <- exp(Res1[,1])/(1+exp(Res1[,1]))
           Res3 <- exp(Res1[,2])/(1+exp(Res1[,2]))
           
           Res4 <- matrix(ncol=2, nrow=dim(Res1)[1])
           
           for(i in 1:dim(Res1)[1]){
             temp <- rnorm(10000, mean=Res1[i,1], sd=Res1[i,2])
             temp2 <- (exp(temp))/(1+exp(temp))
             temp3 <- temp2/((1-temp2))
             Res4[i,1] <- mean(temp3)
             Res4[i,2] <- sd(temp3)
           }
         },
         
         No={
           d_p <- subset(d, cs>0)
           y <- cbind(d_p$AntallKylling, d_p$AntallHunn)
           M1 <- glm(y~1, family="binomial")
           Res1 <- matrix(ncol=3, nrow=1)
           
           Res1 <- as.matrix(summary(M1)$coef[,c(1,2)])
           Res2 <- exp(Res1[1])/(1+exp(Res1[1]))
           Res3 <- exp(Res1[2])/(1+exp(Res1[2]))
           
           Res4 <- matrix(ncol=2, nrow=1)
           
           temp <- rnorm(100000, mean=Res1[1], sd=Res1[2])
           temp2 <- (exp(temp))/(1+exp(temp))
           temp3 <- temp2/((1-temp2))
           Res4[1,1] <- mean(temp3)
           Res4[1,2] <- sd(temp3)
         }
  )
  
  Res4 <- round(Res4, 2)
  colnames(Res4) <- c("Kylling/par", "SE")
  rownames(Res4) <- switch(strat, 
                           No={paste("Estimat")},
                           Year={paste(sort(unique(d_p$Year)))},
                           OmradeNavn={paste(sort(unique(d_p$OmradeNavn)))}
  )
  Res4
}