if(Sys.info()["user"] == "jbrussow"){
  setwd("C:/Users/jbrussow/Dropbox/REMS/11 Comps/")
} else if (Sys.info()["user"] == "Jen"){
  setwd("C:/Users/Jen/Dropbox/REMS/11 Comps/")
}

#install.packages("R2OpenBUGS")
#install.packages("coda")

library("R2OpenBUGS")
library("coda")

#number of people
N <- 2000
#number of items
n <- 60
nreps <- 1
#rho is the amount of DIF explained by the second-order factors
rho <- c(0.4,0.5,0.6)
#P_REF is the proportion of people in the reference group
P_REF <- c(0.5,0.75,0.9)

#true_D is the array that will hold true DIF amounts
true_D <- array(0,c(nreps,n,3,3))
#est_D_reg is the array that will hold the estimated amounts of DIF from the two-stage analysis
est_D_reg <- array(0,c(nreps,n,3,3))
#est_D_HGLM is the array that will hold the estimated amounts of DIF from the hierarchical analysis
est_D_HGLM <- array(0,c(nreps,n,3,3))
#est_R2 is the array that will hold the estimated R-squared values for the amount of DIF explained by the second-order factors
est_R2 <- array(0,c(nreps,2,3,3))
#est_coef is the array that will store the estimated coefficients from the logistic regression
est_coef <- array(0,c(nreps,4,3,3))
#true_a is the array that will store the items' true a-parameters
true_a <- array(0,c(nreps,n,3,3))
#true_b is the array that will store the items' true b-parameters
true_b <- array(0,c(nreps,n,3,3))

#Loop over rho conditions (i.e., the amount of DIF explained by second-order factors)
for (iRHO in 1:3) {
  #Loop over Prop conditions (i.e., the proportion of people in the reference group)
  for (iPROP in 1:3) {
    #Loop over replications
    for (k in 1:nreps) {
    
      ###Generate items with random b, a, and DIF params
      b <- rnorm(n,0,1)
      a <- runif(n,0.5,3.5)
      D <- rnorm(n,0,0.1)
      
      #this must be setting the params on the first three items to make those the DIF items
      b[1] <- -1
      a[1] <- 1.5
      D[1] <- rnorm(1,1,0.1)
      
      b[2] <- 0
      a[2] <- 1.5
      D[2] <- rnorm(1,1,0.1)
      
      b[3] <- 1
      a[3] <- 1.5
      D[3] <- rnorm(1,1,0.1)
      
      #saving the true values for this iteration
      true_a[k,,iRHO,iPROP] <- a
      true_b[k,,iRHO,iPROP] <- b
      true_D[k,,iRHO,iPROP] <- D
      
      #setting the true values for mean DIF, SD of DIF, and the params to go into the l/ogistic regression
      #but what if the true mean of DIF isn't actually this based on the estimated data
      true_mean_D <- 0.95*0 + 0.05*1
      true_SD_D <- sqrt(0.95*0.01 + 0.05*0.01 + 0.95*0.05*(0-1)^2)
      true_slope <- rho[iRHO]*(true_SD_D/1)
      true_int <- true_mean_D - true_slope*0
      
      #zD is the z-score of the amount of DIF
      zD <- (D-mean(D))/sd(D)
      #e1 is presumably the amount of error??
      e1 <- rnorm(n,0,sqrt(1-rho[iRHO]^2))
      #V1 is maybe the amount of variance?
      V1 <- sqrt(rho[iRHO]^2)*zD + e1
      
      #Generate people
      #t is theta scores
      T <- rnorm(N,0,1)
      #setting the theta scores for the reference group to be -.5 less than they would have been
      T[(N*P_REF[iPROP]+1):N] <- T[(N*P_REF[iPROP]+1):N]-0.5
      
      #g is group membership
      G <- rep(0,N)
      #setting the group membership for the people in the reference group
      G[(N*P_REF[iPROP]+1):N] <- 1
      
      #Generate data
      #p is the probability of a correct response to each item
      p <- matrix(0,N,n)
      #x is actual item responses
      x <- matrix(0,N,n)
      #looping over people and items to determine probabilities and item responses
      for (i in 1:N) {
      	for (j in 1:n){
      	  #probability is determined by an IRT model with the DIF amount and group membership parameters included
        	p[i,j] <- 1/(1+exp(-a[j]*(T[i]-(b[j]+D[j]*G[i]))))
      	  r <- runif(1,0,1)
      		if (r <= p[i,j]) {
      		  x[i,j] <- 1
      		}
    	  }
      }
      #X would be the raw sum scores for each person
      X <- rowSums(x) 
      #Z would be the Z-score of each person's raw sum scores
      Z <- (X-mean(X))/sd(X)
      
      #Two-stage DIF analyses
      #for each items' responses
      for (j in 1:n) {
        #item responses predicted by the sumscore and the grouping variable
        log_DIF_2 <- glm( x[,j] ~ Z + G,family="binomial")
        #turn intercepts into thresholds, save as DIF estimate
        #estimated DIF for this item and replication is the group coeffiecient/the total score condition coefficient
        est_D_reg[k,j,iRHO,iPROP] <- -coef(log_DIF_2)[3]/coef(log_DIF_2)[2]
      }
      #linear model to predict the amount of estimated DIF from the variance?? what is happening
      reg1 <- lm(est_D_reg[k,,iRHO,iPROP] ~ V1 )
      #saving the estimated coefficients from the model
      est_coef[k,1:2,iRHO,iPROP] <- coef(reg1)[1:2]
      #saving the r-squared from the model
      est_R2[k,1,iRHO,iPROP] <- summary.lm(reg1)$r.squared
      
      #Bayesian hierarchical DIF analysis
      b.dat <- list("N","n","x","Z","G","V1")
      b.par <- list("b","a","R2")
      OUT <- bugs(data=b.dat,inits=NULL,param=b.par, 
      model.file="Logreg.txt",n.chains=2, 
      n.iter=800, n.burn=300, n.thin=1,debug=FALSE)
      #OUT$summary
      
      #turn intercepts into thresholds, save as DIF estimate
      est_D_HGLM[k,,iRHO,iPROP] <- -OUT$mean$b[,3]/OUT$mean$b[,2]
      est_coef[k,3:4,iRHO,iPROP] <- -OUT$mean$a[1:2]
      est_R2[k,2,iRHO,iPROP] <- OUT$mean$R2
      
    }
  }
}

#ugly base graphics, yuck!
pdf(file="DIF_plots.pdf")
#Loop over rho conditions
for (iRHO in 1:3) {
  #Loop over Prop conditions
  for (iPROP in 1:3) {
    
    write.csv(true_D[,,iRHO,iPROP],file=paste("DIF_true",rho[iRHO],P_REF[iPROP],".csv"))
    write.csv(est_D_reg[,,iRHO,iPROP],file=paste("DIF_est_reg",rho[iRHO],P_REF[iPROP],".csv"))
    write.csv(est_D_HGLM[,,iRHO,iPROP],file=paste("DIF_est_HGLM",rho[iRHO],P_REF[iPROP],".csv"))
    
    bias_reg <- est_D_reg[,,iRHO,iPROP]-true_D[,,iRHO,iPROP]
    bias_HGLM <- est_D_HGLM[,,iRHO,iPROP]-true_D[,,iRHO,iPROP]
    
    write.csv(bias_reg,file=paste("DIF_bias_reg",rho[iRHO],P_REF[iPROP],".csv"))
    write.csv(bias_HGLM,file=paste("DIF_bias_HGLM",rho[iRHO],P_REF[iPROP],".csv"))
    
    true_coef <- c(true_int,true_slope)
    write.csv(rbind(true_coef,est_coef[,,iRHO,iPROP]),file=paste("DIF_int_slope",rho[iRHO],P_REF[iPROP],".csv"))
    write.csv(est_R2[,,iRHO,iPROP],file=paste("DIF_R2",rho[iRHO],P_REF[iPROP],".csv"))
    
    plot(true_D[,3:n,iRHO,iPROP],est_D_reg[,3:n,iRHO,iPROP],ylab="estimated DIF(reg)",
    xlab=expression(paste("DIF estimates for ",DIF[true]," ~ N(0,",0.1^2,")")),
    main=paste("r =",round(cor(as.vector(true_D[,3:n,iRHO,iPROP]),as.vector(est_D_reg[,3:n,iRHO,iPROP])),3)),
    sub=paste("IV(r) =",rho[iRHO],"Ref prop =",P_REF[iPROP]))
    
    plot(true_D[,1:2,iRHO,iPROP],est_D_reg[,1:2,iRHO,iPROP],ylab="estimated DIF(reg)",
    xlab=expression(paste("DIF estimates for ",DIF[true]," ~ N(1,",0.1^2,")")),
    main=paste("r =",round(cor(as.vector(true_D[,1:2,iRHO,iPROP]),as.vector(est_D_reg[,1:2,iRHO,iPROP])),3)),
    sub=paste("IV(r) =",rho[iRHO],"Ref prop =",P_REF[iPROP]))
    
    plot(true_D[,3:n,iRHO,iPROP],est_D_HGLM[,3:n,iRHO,iPROP],ylab="estimated DIF(HGLM)",
    xlab=expression(paste("DIF estimates for ",DIF[true]," ~ N(0,",0.1^2,")")),
    main=paste("r =",round(cor(as.vector(true_D[,3:n,iRHO,iPROP]),as.vector(est_D_HGLM[,3:n,iRHO,iPROP])),3)),
    sub=paste("IV(r) =",rho[iRHO],"Ref prop =",P_REF[iPROP]))
    
    plot(true_D[,1:2,iRHO,iPROP],est_D_HGLM[,1:2,iRHO,iPROP],ylab="estimated DIF(HGLM)",
    xlab=expression(paste("DIF estimates for ",DIF[true]," ~ N(1,",0.1^2,")")),
    main=paste("r =",round(cor(as.vector(true_D[,1:2,iRHO,iPROP]),as.vector(est_D_HGLM[,1:2,iRHO,iPROP])),3)),
    sub=paste("IV(r) =",rho[iRHO],"Ref prop =",P_REF[iPROP]))
    
    plot(density(est_D_HGLM[,3:n,iRHO,iPROP],bw=sd(est_D_HGLM[,3:n,iRHO,iPROP])),
    lwd=3,col="orange",xlim=c(-1,1),main="",
    xlab=expression(paste("DIF estimates for ",DIF[true]," ~ N(0,",0.1^2,")")))
    text(-.65,1.5,paste("mean(HGLM) =",round(mean(est_D_HGLM[,3:n,iRHO,iPROP]),2)),col="orange")
    text(-.65,1.35,paste("  SD(HGLM) =",round(sd(est_D_HGLM[,3:n,iRHO,iPROP]),2)),col="orange")
    lines(density(est_D_reg[,3:n,iRHO,iPROP],bw=sd(est_D_reg[,3:n,iRHO,iPROP])),lwd=3)
    text(.65,1.0,paste("mean(reg) =",round(mean(est_D_reg[,3:n,iRHO,iPROP]),2)))
    text(.65,0.85,paste("  SD(reg) =",round(sd(est_D_reg[,3:n,iRHO,iPROP]),2)))
    
    plot(density(est_D_HGLM[,1:2,iRHO,iPROP],bw=sd(est_D_HGLM[,1:2,iRHO,iPROP])),
    lwd=3,col="orange",main="",xlim=c(-1,3),
    xlab=expression(paste("DIF estimates for ",DIF[true]," ~ N(1,",0.1^2,")")))
    text(-0.35,0.6,paste("mean(HGLM) =",round(mean(est_D_HGLM[,1:2,iRHO,iPROP]),3)),col="orange")
    text(-0.35,0.5,paste("  SD(HGLM) =",round(sd(est_D_HGLM[,1:2,iRHO,iPROP]),3)),col="orange")
    lines(density(est_D_reg[,1:2,iRHO,iPROP],bw=sd(est_D_reg[,1:2,iRHO,iPROP])),lwd=3)
    text(2.40,0.5,paste("mean(reg) =",round(mean(est_D_reg[,1:2,iRHO,iPROP]),3)))
    text(2.40,0.4,paste("  SD(reg) =",round(sd(est_D_reg[,1:2,iRHO,iPROP]),3)))
    
    YY <- P_REF[iPROP]
    ZZ <- rho[iRHO]^2
    hist(est_R2[,1,iRHO,iPROP],main=paste("True =",ZZ," Prop(ref) =",YY),
    freq=FALSE,xlab=expression(paste(R^2," DIF(reg)")),col="darkgrey",
    sub=paste("mean =",round(mean(est_R2[,1,iRHO,iPROP]),3),", SD =",round(sd(est_R2[,1,iRHO,iPROP]),3)))
    lines(density(est_R2[,1,iRHO,iPROP],bw=sd(est_R2[,1,iRHO,iPROP])),lwd=3)
    
    hist(est_R2[,2,iRHO,iPROP],main=paste("True =",ZZ," Prop(ref) =",YY),
    freq=FALSE,xlab=expression(paste(R^2," DIF(HGLM)")),col="orange",
    sub=paste("mean =",round(mean(est_R2[,2,iRHO,iPROP]),3),", SD =",round(sd(est_R2[,2,iRHO,iPROP]),3)))
    lines(density(est_R2[,2,iRHO,iPROP],bw=sd(est_R2[,2,iRHO,iPROP])),lwd=3)
    
  }
}

dev.off()
