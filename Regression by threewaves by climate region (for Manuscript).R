library(glmmTMB)



data.wave<-vector("list",3)
data.wave[[1]]<-read.csv("PATHNAME\Wave1.csv")
data.wave[[2]]<-read.csv("PATHNAME\\Wave2.csv")
data.wave[[3]]<-read.csv("PATHNAME\\Wave3.csv")

for (ii in seq(3)) {
  data.wave[[ii]]$climate2 <- data.wave[[ii]]$climate
  data.wave[[ii]][data.wave[[ii]]$climate=="Southwest","climate2"] <- "West"
  data.wave[[ii]][data.wave[[ii]]$climate=="Northwest","climate2"] <- "West"
}

climate_region<-unique(data.wave[[1]]$climate2)[!is.na(unique(data.wave[[1]]$climate2))]
#climate_region<-unique(Data_waves[[1]]$climate)[!is.na(unique(Data_waves[[1]]$climate))]

n_cl<-length(climate_region)



### MODEL: BLACK ONLY  + AGE40-64, Age85+ SEXRATIO + POP_DENSITY ADJUSTED 

Cases.fit<-vector("list",1)
Cases.fit.uni<-vector("list",1)
for (aa in seq(n_cl)) {
  
  Cases.fit[[aa]]<-vector("list",1)
  Cases.fit.uni[[aa]]<-vector("list",1)
  
  for (bb in seq(5)) {
    Cases.fit.uni[[aa]][[bb]] <- vector("list",1)
  }
}

for (aa in seq(n_cl)) {
  
  for (ii in seq(3)) {
    data_fit <- subset(data.wave[[ii]], climate2==climate_region[aa])
    
    Cases.fit[[aa]][[ii]]<-glmmTMB(cases ~ 
                                     scale(data_fit$BLACK_ALL)+
                                     scale(data_fit$AGE_GR2)+scale(data_fit$AGE_GR4)+scale(data_fit$MALE)+
                                     scale(data_fit$POP_DENSITY)+
                                     (1|State)
                                   + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    
    Cases.fit.uni[[aa]][[1]][[ii]]<-glmmTMB(cases ~ scale(data_fit$BLACK_ALL)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[2]][[ii]]<-glmmTMB(cases ~ scale(data_fit$AGE_GR2)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[3]][[ii]]<-glmmTMB(cases ~ scale(data_fit$AGE_GR4)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[4]][[ii]]<-glmmTMB(cases ~ scale(data_fit$MALE)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[5]][[ii]]<-glmmTMB(cases ~ scale(data_fit$POP_DENSITY)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    print(paste(aa,ii))
  }
}

# 
# 
# library(DHARMa)
# set.seed(123)
# res = simulateResiduals(Fit.Cases.firstwave)
# plot(res, rank=T)
# 
# res = simulateResiduals(Fit.Deaths.firstwave)
# plot(res, rank=T)

Results.Cases <- Results.Cases.Uni <- vector("list",1)
for (aa in seq(n_cl)) {
  Results.Cases[[aa]] <- Results.Cases.Uni[[aa]] <-vector("list",3)
  
  for (bb in seq(3)) {
    Results.Cases[[aa]][[bb]] <- Results.Cases.Uni[[aa]][[bb]] <-matrix(NA,nrow=(nrow(coef(summary(Cases.fit[[aa]][[bb]]))$cond)-1),ncol=3)
    rownames(Results.Cases[[aa]][[bb]]) <- rownames(Results.Cases.Uni[[aa]][[bb]]) <-rownames(coef(summary(Cases.fit[[aa]][[bb]]))$cond)[-1]
    
    for (ii in seq((nrow(coef(summary(Cases.fit[[aa]][[bb]]))$cond)-1))) {
      Results.Cases[[aa]][[bb]][ii,] <-exp(c(coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1],coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1]-1.96*coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,2],
                                             coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1]+1.96*coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,2]))
      
      Results.Cases.Uni[[aa]][[bb]][ii,] <-exp(c(coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1],coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]-1.96*coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2],
                                                 coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]+1.96*coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2]))
    }
  }
}

BLACK_Cases.Uni <- BLACK_Cases <- vector("list",1)
for (aa in seq(n_cl)) {
  BLACK_Cases[[aa]] <-matrix(NA,nrow=3,ncol=3)
  BLACK_Cases.Uni[[aa]] <-matrix(NA,nrow=3,ncol=3)
  for (ii in seq(3)) {
    BLACK_Cases[[aa]][ii,] <- Results.Cases[[aa]][[ii]][1,]
    BLACK_Cases.Uni[[aa]][ii,] <- Results.Cases.Uni[[aa]][[ii]][1,]
  }
}








### MODEL: HISPANIC ONLY  + AGE40-64, Age85+ SEXRATIO + POP_DENSITY ADJUSTED 

Cases.fit<-vector("list",1)
Cases.fit.uni<-vector("list",1)
for (aa in seq(n_cl)) {
  
  Cases.fit[[aa]]<-vector("list",1)
  Cases.fit.uni[[aa]]<-vector("list",1)
  
  for (bb in seq(5)) {
    Cases.fit.uni[[aa]][[bb]] <- vector("list",1)
  }
}

for (aa in seq(n_cl)) {
  
  for (ii in seq(3)) {
    data_fit <- subset(data.wave[[ii]], climate2==climate_region[aa])
    
    Cases.fit[[aa]][[ii]]<-glmmTMB(cases ~ 
                                     scale(data_fit$HISPANIC)+
                                     scale(data_fit$AGE_GR2)+scale(data_fit$AGE_GR4)+scale(data_fit$MALE)+
                                     scale(data_fit$POP_DENSITY)+
                                     (1|State)
                                   + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    
    Cases.fit.uni[[aa]][[1]][[ii]]<-glmmTMB(cases ~ scale(data_fit$HISPANIC)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[2]][[ii]]<-glmmTMB(cases ~ scale(data_fit$AGE_GR2)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[3]][[ii]]<-glmmTMB(cases ~ scale(data_fit$AGE_GR4)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[4]][[ii]]<-glmmTMB(cases ~ scale(data_fit$MALE)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[5]][[ii]]<-glmmTMB(cases ~ scale(data_fit$POP_DENSITY)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    print(paste(aa,ii))
  }
}

# 
# 
# library(DHARMa)
# set.seed(123)
# res = simulateResiduals(Fit.Cases.firstwave)
# plot(res, rank=T)
# 
# res = simulateResiduals(Fit.Deaths.firstwave)
# plot(res, rank=T)

Results.Cases <- Results.Cases.Uni <- vector("list",1)
for (aa in seq(n_cl)) {
  Results.Cases[[aa]] <- Results.Cases.Uni[[aa]] <-vector("list",3)
  
  for (bb in seq(3)) {
    Results.Cases[[aa]][[bb]] <- Results.Cases.Uni[[aa]][[bb]] <-matrix(NA,nrow=(nrow(coef(summary(Cases.fit[[aa]][[bb]]))$cond)-1),ncol=3)
    rownames(Results.Cases[[aa]][[bb]]) <- rownames(Results.Cases.Uni[[aa]][[bb]]) <-rownames(coef(summary(Cases.fit[[aa]][[bb]]))$cond)[-1]
    
    for (ii in seq((nrow(coef(summary(Cases.fit[[aa]][[bb]]))$cond)-1))) {
      Results.Cases[[aa]][[bb]][ii,] <-exp(c(coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1],coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1]-1.96*coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,2],
                                             coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1]+1.96*coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,2]))
      
      Results.Cases.Uni[[aa]][[bb]][ii,] <-exp(c(coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1],coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]-1.96*coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2],
                                                 coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]+1.96*coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2]))
    }
  }
}

HISPANIC_Cases.Uni <- HISPANIC_Cases <- vector("list",1)
for (aa in seq(n_cl)) {
  HISPANIC_Cases[[aa]] <-matrix(NA,nrow=3,ncol=3)
  HISPANIC_Cases.Uni[[aa]] <-matrix(NA,nrow=3,ncol=3)
  for (ii in seq(3)) {
    HISPANIC_Cases[[aa]][ii,] <- Results.Cases[[aa]][[ii]][1,]
    HISPANIC_Cases.Uni[[aa]][ii,] <- Results.Cases.Uni[[aa]][[ii]][1,]
  }
}






### MODEL: WHITE_NH ONLY  + AGE40-64, Age85+ SEXRATIO + POP_DENSITY ADJUSTED 

Cases.fit<-vector("list",1)
Cases.fit.uni<-vector("list",1)
for (aa in seq(n_cl)) {
  
  Cases.fit[[aa]]<-vector("list",1)
  Cases.fit.uni[[aa]]<-vector("list",1)
  
  for (bb in seq(5)) {
    Cases.fit.uni[[aa]][[bb]] <- vector("list",1)
  }
}

for (aa in seq(n_cl)) {
  
  for (ii in seq(3)) {
    data_fit <- subset(data.wave[[ii]], climate2==climate_region[aa])
    
    Cases.fit[[aa]][[ii]]<-glmmTMB(cases ~ 
                                     scale(data_fit$WHITE_NH)+
                                     scale(data_fit$AGE_GR2)+scale(data_fit$AGE_GR4)+scale(data_fit$MALE)+
                                     scale(data_fit$POP_DENSITY)+
                                     (1|State)
                                   + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    
    Cases.fit.uni[[aa]][[1]][[ii]]<-glmmTMB(cases ~ scale(data_fit$WHITE_NH)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[2]][[ii]]<-glmmTMB(cases ~ scale(data_fit$AGE_GR2)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[3]][[ii]]<-glmmTMB(cases ~ scale(data_fit$AGE_GR4)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[4]][[ii]]<-glmmTMB(cases ~ scale(data_fit$MALE)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[5]][[ii]]<-glmmTMB(cases ~ scale(data_fit$POP_DENSITY)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    print(paste(aa,ii))
  }
}

# 
# 
# library(DHARMa)
# set.seed(123)
# res = simulateResiduals(Fit.Cases.firstwave)
# plot(res, rank=T)
# 
# res = simulateResiduals(Fit.Deaths.firstwave)
# plot(res, rank=T)

Results.Cases <- Results.Cases.Uni <- vector("list",1)
for (aa in seq(n_cl)) {
  Results.Cases[[aa]] <- Results.Cases.Uni[[aa]] <-vector("list",3)
  
  for (bb in seq(3)) {
    Results.Cases[[aa]][[bb]] <- Results.Cases.Uni[[aa]][[bb]] <-matrix(NA,nrow=(nrow(coef(summary(Cases.fit[[aa]][[bb]]))$cond)-1),ncol=3)
    rownames(Results.Cases[[aa]][[bb]]) <- rownames(Results.Cases.Uni[[aa]][[bb]]) <-rownames(coef(summary(Cases.fit[[aa]][[bb]]))$cond)[-1]
    
    for (ii in seq((nrow(coef(summary(Cases.fit[[aa]][[bb]]))$cond)-1))) {
      Results.Cases[[aa]][[bb]][ii,] <-exp(c(coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1],coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1]-1.96*coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,2],
                                             coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1]+1.96*coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,2]))
      
      Results.Cases.Uni[[aa]][[bb]][ii,] <-exp(c(coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1],coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]-1.96*coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2],
                                                 coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]+1.96*coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2]))
    }
  }
}

WHITE_NH_Cases.Uni <- WHITE_NH_Cases <- vector("list",1)
for (aa in seq(n_cl)) {
  WHITE_NH_Cases[[aa]] <-matrix(NA,nrow=3,ncol=3)
  WHITE_NH_Cases.Uni[[aa]] <-matrix(NA,nrow=3,ncol=3)
  for (ii in seq(3)) {
    WHITE_NH_Cases[[aa]][ii,] <- Results.Cases[[aa]][[ii]][1,]
    WHITE_NH_Cases.Uni[[aa]][ii,] <- Results.Cases.Uni[[aa]][[ii]][1,]
  }
}









### MODEL: ATKINSON ONLY  + AGE40-64, Age85+ SEXRATIO + POP_DENSITY ADJUSTED 

Cases.fit<-vector("list",1)
Cases.fit.uni<-vector("list",1)
for (aa in seq(n_cl)) {
  
  Cases.fit[[aa]]<-vector("list",1)
  Cases.fit.uni[[aa]]<-vector("list",1)
  
  for (bb in seq(5)) {
    Cases.fit.uni[[aa]][[bb]] <- vector("list",1)
  }
}

for (aa in seq(n_cl)) {
  
  for (ii in seq(3)) {
    data_fit <- subset(data.wave[[ii]], climate2==climate_region[aa])
    
    Cases.fit[[aa]][[ii]]<-glmmTMB(cases ~ 
                                     scale(data_fit$ATKINSON)+
                                     scale(data_fit$AGE_GR2)+scale(data_fit$AGE_GR4)+scale(data_fit$MALE)+
                                     scale(data_fit$POP_DENSITY)+
                                     (1|State)
                                   + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    
    Cases.fit.uni[[aa]][[1]][[ii]]<-glmmTMB(cases ~ scale(data_fit$ATKINSON)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[2]][[ii]]<-glmmTMB(cases ~ scale(data_fit$AGE_GR2)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[3]][[ii]]<-glmmTMB(cases ~ scale(data_fit$AGE_GR4)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[4]][[ii]]<-glmmTMB(cases ~ scale(data_fit$MALE)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[5]][[ii]]<-glmmTMB(cases ~ scale(data_fit$POP_DENSITY)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    print(paste(aa,ii))
  }
}

# 
# 
# library(DHARMa)
# set.seed(123)
# res = simulateResiduals(Fit.Cases.firstwave)
# plot(res, rank=T)
# 
# res = simulateResiduals(Fit.Deaths.firstwave)
# plot(res, rank=T)

Results.Cases <- Results.Cases.Uni <- vector("list",1)
for (aa in seq(n_cl)) {
  Results.Cases[[aa]] <- Results.Cases.Uni[[aa]] <-vector("list",3)
  
  for (bb in seq(3)) {
    Results.Cases[[aa]][[bb]] <- Results.Cases.Uni[[aa]][[bb]] <-matrix(NA,nrow=(nrow(coef(summary(Cases.fit[[aa]][[bb]]))$cond)-1),ncol=3)
    rownames(Results.Cases[[aa]][[bb]]) <- rownames(Results.Cases.Uni[[aa]][[bb]]) <-rownames(coef(summary(Cases.fit[[aa]][[bb]]))$cond)[-1]
    
    for (ii in seq((nrow(coef(summary(Cases.fit[[aa]][[bb]]))$cond)-1))) {
      Results.Cases[[aa]][[bb]][ii,] <-exp(c(coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1],coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1]-1.96*coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,2],
                                             coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1]+1.96*coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,2]))
      
      Results.Cases.Uni[[aa]][[bb]][ii,] <-exp(c(coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1],coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]-1.96*coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2],
                                                 coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]+1.96*coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2]))
    }
  }
}

ATKINSON_Cases.Uni <- ATKINSON_Cases <- vector("list",1)
for (aa in seq(n_cl)) {
  ATKINSON_Cases[[aa]] <-matrix(NA,nrow=3,ncol=3)
  ATKINSON_Cases.Uni[[aa]] <-matrix(NA,nrow=3,ncol=3)
  for (ii in seq(3)) {
    ATKINSON_Cases[[aa]][ii,] <- Results.Cases[[aa]][[ii]][1,]
    ATKINSON_Cases.Uni[[aa]][ii,] <- Results.Cases.Uni[[aa]][[ii]][1,]
  }
}




### MODEL: CORRRATIO ONLY  + AGE40-64, Age85+ SEXRATIO + POP_DENSITY ADJUSTED 

Cases.fit<-vector("list",1)
Cases.fit.uni<-vector("list",1)
for (aa in seq(n_cl)) {
  
  Cases.fit[[aa]]<-vector("list",1)
  Cases.fit.uni[[aa]]<-vector("list",1)
  
  for (bb in seq(5)) {
    Cases.fit.uni[[aa]][[bb]] <- vector("list",1)
  }
}

for (aa in seq(n_cl)) {
  
  for (ii in seq(3)) {
    data_fit <- subset(data.wave[[ii]], climate2==climate_region[aa])
    
    Cases.fit[[aa]][[ii]]<-glmmTMB(cases ~ 
                                     scale(data_fit$CORRRATIO)+
                                     scale(data_fit$AGE_GR2)+scale(data_fit$AGE_GR4)+scale(data_fit$MALE)+
                                     scale(data_fit$POP_DENSITY)+
                                     (1|State)
                                   + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    
    Cases.fit.uni[[aa]][[1]][[ii]]<-glmmTMB(cases ~ scale(data_fit$CORRRATIO)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[2]][[ii]]<-glmmTMB(cases ~ scale(data_fit$AGE_GR2)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[3]][[ii]]<-glmmTMB(cases ~ scale(data_fit$AGE_GR4)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[4]][[ii]]<-glmmTMB(cases ~ scale(data_fit$MALE)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    Cases.fit.uni[[aa]][[5]][[ii]]<-glmmTMB(cases ~ scale(data_fit$POP_DENSITY)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    print(paste(aa,ii))
  }
}

# 
# 
# library(DHARMa)
# set.seed(123)
# res = simulateResiduals(Fit.Cases.firstwave)
# plot(res, rank=T)
# 
# res = simulateResiduals(Fit.Deaths.firstwave)
# plot(res, rank=T)

Results.Cases <- Results.Cases.Uni <- vector("list",1)
for (aa in seq(n_cl)) {
  Results.Cases[[aa]] <- Results.Cases.Uni[[aa]] <-vector("list",3)
  
  for (bb in seq(3)) {
    Results.Cases[[aa]][[bb]] <- Results.Cases.Uni[[aa]][[bb]] <-matrix(NA,nrow=(nrow(coef(summary(Cases.fit[[aa]][[bb]]))$cond)-1),ncol=3)
    rownames(Results.Cases[[aa]][[bb]]) <- rownames(Results.Cases.Uni[[aa]][[bb]]) <-rownames(coef(summary(Cases.fit[[aa]][[bb]]))$cond)[-1]
    
    for (ii in seq((nrow(coef(summary(Cases.fit[[aa]][[bb]]))$cond)-1))) {
      Results.Cases[[aa]][[bb]][ii,] <-exp(c(coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1],coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1]-1.96*coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,2],
                                             coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,1]+1.96*coef(summary(Cases.fit[[aa]][[bb]]))$cond[1+ii,2]))
      
      Results.Cases.Uni[[aa]][[bb]][ii,] <-exp(c(coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1],coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]-1.96*coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2],
                                                 coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]+1.96*coef(summary(Cases.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2]))
    }
  }
}

CORRRATIO_Cases.Uni <- CORRRATIO_Cases <- vector("list",1)
for (aa in seq(n_cl)) {
  CORRRATIO_Cases[[aa]] <-matrix(NA,nrow=3,ncol=3)
  CORRRATIO_Cases.Uni[[aa]] <-matrix(NA,nrow=3,ncol=3)
  for (ii in seq(3)) {
    CORRRATIO_Cases[[aa]][ii,] <- Results.Cases[[aa]][[ii]][1,]
    CORRRATIO_Cases.Uni[[aa]][ii,] <- Results.Cases.Uni[[aa]][[ii]][1,]
  }
}





jpeg("PATHNAME/Figure SS.jpg",
     width=16,height=24,units="in",res=900,pointsize=25)
par(mfrow=c(5,1),mar=c(4,4,2,1),oma=c(0,0,1,0))

plot(seq(3),WHITE_NH_Cases[[1]][,1],type="n",xlim=c(1.2,20.8),ylim=c(0.3,3),log="y",xaxt="n",ylab="Relative rate",xlab="Wave")
abline(h=1,col="grey90")
for (aa in seq(n_cl)) {
  arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),WHITE_NH_Cases.Uni[[aa]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),WHITE_NH_Cases.Uni[[aa]][,3],code=0)
  lines(seq(1-0.1+3*(aa-1),3*(aa)-0.1),WHITE_NH_Cases.Uni[[aa]][,1], type="b",col="darkred",pch=16)
  
  arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),WHITE_NH_Cases[[aa]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),WHITE_NH_Cases[[aa]][,3],code=0)
  lines(seq(1+0.1+3*(aa-1),3*(aa)+0.1),WHITE_NH_Cases[[aa]][,1], type="b",col="darkblue",pch=17)
}
axis(1,seq(1,21),rep(c(1,2,3),7),cex=0.8)
axis(3.5,seq(2,20,by=3),climate_region,cex.axis=0.9,line=-3,lwd=0)
abline(v=seq(3.5,18.5,by=3),col="grey")
text("A. % of Non-Hispanic White", x=-1.1,y=4,xpd=T,cex=1.3,adj=0)


plot(seq(3),BLACK_Cases[[1]][,1],type="n",xlim=c(1.2,20.8),ylim=c(0.3,3),log="y",xaxt="n",ylab="Relative rate",xlab="Wave")
abline(h=1,col="grey90")
for (aa in seq(n_cl)) {
  arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),BLACK_Cases.Uni[[aa]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),BLACK_Cases.Uni[[aa]][,3],code=0)
  lines(seq(1-0.1+3*(aa-1),3*(aa)-0.1),BLACK_Cases.Uni[[aa]][,1], type="b",col="darkred",pch=16)
  
  arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),BLACK_Cases[[aa]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),BLACK_Cases[[aa]][,3],code=0)
  lines(seq(1+0.1+3*(aa-1),3*(aa)+0.1),BLACK_Cases[[aa]][,1], type="b",col="darkblue",pch=17)
}
axis(1,seq(1,21),rep(c(1,2,3),7),cex=0.8)
axis(3.5,seq(2,20,by=3),climate_region,cex.axis=0.9,line=-3,lwd=0)
abline(v=seq(3.5,18.5,by=3),col="grey")
text("B. % of Black/African American", x=-1.1,y=4,xpd=T,cex=1.3,adj=0)


plot(seq(3),HISPANIC_Cases[[1]][,1],type="n",xlim=c(1.2,20.8),ylim=c(0.3,3),log="y",xaxt="n",ylab="Relative rate",xlab="Wave")
abline(h=1,col="grey90")
for (aa in seq(n_cl)) {
  arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),HISPANIC_Cases.Uni[[aa]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),HISPANIC_Cases.Uni[[aa]][,3],code=0)
  lines(seq(1-0.1+3*(aa-1),3*(aa)-0.1),HISPANIC_Cases.Uni[[aa]][,1], type="b",col="darkred",pch=16)
  
  arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),HISPANIC_Cases[[aa]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),HISPANIC_Cases[[aa]][,3],code=0)
  lines(seq(1+0.1+3*(aa-1),3*(aa)+0.1),HISPANIC_Cases[[aa]][,1], type="b",col="darkblue",pch=17)
}
axis(1,seq(1,21),rep(c(1,2,3),7),cex=0.8)
axis(3.5,seq(2,20,by=3),climate_region,cex.axis=0.9,line=-3,lwd=0)
abline(v=seq(3.5,18.5,by=3),col="grey")
text("C. % of Hispanic", x=-1.1,y=4,xpd=T,cex=1.3,adj=0)



plot(seq(3),ATKINSON_Cases[[1]][,1],type="n",xlim=c(1.2,20.8),ylim=c(0.3,3),log="y",xaxt="n",ylab="Relative rate",xlab="Wave")
abline(h=1,col="grey90")
for (aa in seq(n_cl)) {
  arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),ATKINSON_Cases.Uni[[aa]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),ATKINSON_Cases.Uni[[aa]][,3],code=0)
  lines(seq(1-0.1+3*(aa-1),3*(aa)-0.1),ATKINSON_Cases.Uni[[aa]][,1], type="b",col="darkred",pch=16)
  
  arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),ATKINSON_Cases[[aa]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),ATKINSON_Cases[[aa]][,3],code=0)
  lines(seq(1+0.1+3*(aa-1),3*(aa)+0.1),ATKINSON_Cases[[aa]][,1], type="b",col="darkblue",pch=17)
}
axis(1,seq(1,21),rep(c(1,2,3),7),cex=0.8)
axis(3.5,seq(2,20,by=3),climate_region,cex.axis=0.9,line=-3,lwd=0)
abline(v=seq(3.5,18.5,by=3),col="grey")
text("D. Atkinson index", x=-1.1,y=4,xpd=T,cex=1.3,adj=0)


plot(seq(3),CORRRATIO_Cases[[1]][,1],type="n",xlim=c(1.2,20.8),ylim=c(0.3,3),log="y",xaxt="n",ylab="Relative rate",xlab="Wave")
abline(h=1,col="grey90")
for (aa in seq(n_cl)) {
  arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),CORRRATIO_Cases.Uni[[aa]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),CORRRATIO_Cases.Uni[[aa]][,3],code=0)
  lines(seq(1-0.1+3*(aa-1),3*(aa)-0.1),CORRRATIO_Cases.Uni[[aa]][,1], type="b",col="darkred",pch=16)
  
  arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),CORRRATIO_Cases[[aa]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),CORRRATIO_Cases[[aa]][,3],code=0)
  lines(seq(1+0.1+3*(aa-1),3*(aa)+0.1),CORRRATIO_Cases[[aa]][,1], type="b",col="darkblue",pch=17)
}
axis(1,seq(1,21),rep(c(1,2,3),7),cex=0.8)
axis(3.5,seq(2,20,by=3),climate_region,cex.axis=0.9,line=-3,lwd=0)
abline(v=seq(3.5,18.5,by=3),col="grey")
text("E. Eta-squared", x=-1.1,y=4,xpd=T,cex=1.3,adj=0)

dev.off()







