
data.wave<-vector("list",3)
data.wave[[1]]<-read.csv("PATHNAME\\Wave1.csv")
data.wave[[2]]<-read.csv("PATHNAME\\Wave2.csv")
data.wave[[3]]<-read.csv("PATHNAME\\Wave3.csv")

n_cl<-1

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
    data_fit <- data.wave[[ii]]
    
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
    data_fit <- data.wave[[ii]]
    
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
    data_fit <- data.wave[[ii]]
    
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
    data_fit <- data.wave[[ii]]
    
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
    data_fit <- data.wave[[ii]]
    
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







##########################MORTALITY RATE
##########################MORTALITY RATE
##########################MORTALITY RATE
##########################MORTALITY RATE






### MODEL: BLACK ONLY  + AGE40-64, Age85+ SEXRATIO + POP_DENSITY ADJUSTED 

deaths.fit<-vector("list",1)
deaths.fit.uni<-vector("list",1)
for (aa in seq(n_cl)) {
  
  deaths.fit[[aa]]<-vector("list",1)
  deaths.fit.uni[[aa]]<-vector("list",1)
  
  for (bb in seq(5)) {
    deaths.fit.uni[[aa]][[bb]] <- vector("list",1)
  }
}

for (aa in seq(n_cl)) {
  
  for (ii in seq(3)) {
    data_fit <- data.wave[[ii]]
    
    deaths.fit[[aa]][[ii]]<-glmmTMB(deaths ~ 
                                     scale(data_fit$BLACK_ALL)+
                                     scale(data_fit$AGE_GR2)+scale(data_fit$AGE_GR4)+scale(data_fit$MALE)+
                                     scale(data_fit$POP_DENSITY)+
                                     (1|State)
                                   + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    
    deaths.fit.uni[[aa]][[1]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$BLACK_ALL)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[2]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$AGE_GR2)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[3]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$AGE_GR4)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[4]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$MALE)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[5]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$POP_DENSITY)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    print(paste(aa,ii))
  }
}

# 
# 
# library(DHARMa)
# set.seed(123)
# res = simulateResiduals(Fit.deaths.firstwave)
# plot(res, rank=T)
# 
# res = simulateResiduals(Fit.Deaths.firstwave)
# plot(res, rank=T)

Results.deaths <- Results.deaths.Uni <- vector("list",1)
for (aa in seq(n_cl)) {
  Results.deaths[[aa]] <- Results.deaths.Uni[[aa]] <-vector("list",3)
  
  for (bb in seq(3)) {
    Results.deaths[[aa]][[bb]] <- Results.deaths.Uni[[aa]][[bb]] <-matrix(NA,nrow=(nrow(coef(summary(deaths.fit[[aa]][[bb]]))$cond)-1),ncol=3)
    rownames(Results.deaths[[aa]][[bb]]) <- rownames(Results.deaths.Uni[[aa]][[bb]]) <-rownames(coef(summary(deaths.fit[[aa]][[bb]]))$cond)[-1]
    
    for (ii in seq((nrow(coef(summary(deaths.fit[[aa]][[bb]]))$cond)-1))) {
      Results.deaths[[aa]][[bb]][ii,] <-exp(c(coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1],coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1]-1.96*coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,2],
                                             coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1]+1.96*coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,2]))
      
      Results.deaths.Uni[[aa]][[bb]][ii,] <-exp(c(coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1],coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]-1.96*coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2],
                                                 coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]+1.96*coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2]))
    }
  }
}

BLACK_deaths.Uni <- BLACK_deaths <- vector("list",1)
for (aa in seq(n_cl)) {
  BLACK_deaths[[aa]] <-matrix(NA,nrow=3,ncol=3)
  BLACK_deaths.Uni[[aa]] <-matrix(NA,nrow=3,ncol=3)
  for (ii in seq(3)) {
    BLACK_deaths[[aa]][ii,] <- Results.deaths[[aa]][[ii]][1,]
    BLACK_deaths.Uni[[aa]][ii,] <- Results.deaths.Uni[[aa]][[ii]][1,]
  }
}








### MODEL: HISPANIC ONLY  + AGE40-64, Age85+ SEXRATIO + POP_DENSITY ADJUSTED 

deaths.fit<-vector("list",1)
deaths.fit.uni<-vector("list",1)
for (aa in seq(n_cl)) {
  
  deaths.fit[[aa]]<-vector("list",1)
  deaths.fit.uni[[aa]]<-vector("list",1)
  
  for (bb in seq(5)) {
    deaths.fit.uni[[aa]][[bb]] <- vector("list",1)
  }
}

for (aa in seq(n_cl)) {
  
  for (ii in seq(3)) {
    data_fit <- data.wave[[ii]]
    
    deaths.fit[[aa]][[ii]]<-glmmTMB(deaths ~ 
                                     scale(data_fit$HISPANIC)+
                                     scale(data_fit$AGE_GR2)+scale(data_fit$AGE_GR4)+scale(data_fit$MALE)+
                                     scale(data_fit$POP_DENSITY)+
                                     (1|State)
                                   + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    
    deaths.fit.uni[[aa]][[1]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$HISPANIC)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[2]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$AGE_GR2)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[3]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$AGE_GR4)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[4]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$MALE)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[5]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$POP_DENSITY)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    print(paste(aa,ii))
  }
}

# 
# 
# library(DHARMa)
# set.seed(123)
# res = simulateResiduals(Fit.deaths.firstwave)
# plot(res, rank=T)
# 
# res = simulateResiduals(Fit.Deaths.firstwave)
# plot(res, rank=T)

Results.deaths <- Results.deaths.Uni <- vector("list",1)
for (aa in seq(n_cl)) {
  Results.deaths[[aa]] <- Results.deaths.Uni[[aa]] <-vector("list",3)
  
  for (bb in seq(3)) {
    Results.deaths[[aa]][[bb]] <- Results.deaths.Uni[[aa]][[bb]] <-matrix(NA,nrow=(nrow(coef(summary(deaths.fit[[aa]][[bb]]))$cond)-1),ncol=3)
    rownames(Results.deaths[[aa]][[bb]]) <- rownames(Results.deaths.Uni[[aa]][[bb]]) <-rownames(coef(summary(deaths.fit[[aa]][[bb]]))$cond)[-1]
    
    for (ii in seq((nrow(coef(summary(deaths.fit[[aa]][[bb]]))$cond)-1))) {
      Results.deaths[[aa]][[bb]][ii,] <-exp(c(coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1],coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1]-1.96*coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,2],
                                             coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1]+1.96*coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,2]))
      
      Results.deaths.Uni[[aa]][[bb]][ii,] <-exp(c(coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1],coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]-1.96*coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2],
                                                 coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]+1.96*coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2]))
    }
  }
}

HISPANIC_deaths.Uni <- HISPANIC_deaths <- vector("list",1)
for (aa in seq(n_cl)) {
  HISPANIC_deaths[[aa]] <-matrix(NA,nrow=3,ncol=3)
  HISPANIC_deaths.Uni[[aa]] <-matrix(NA,nrow=3,ncol=3)
  for (ii in seq(3)) {
    HISPANIC_deaths[[aa]][ii,] <- Results.deaths[[aa]][[ii]][1,]
    HISPANIC_deaths.Uni[[aa]][ii,] <- Results.deaths.Uni[[aa]][[ii]][1,]
  }
}






### MODEL: WHITE_NH ONLY  + AGE40-64, Age85+ SEXRATIO + POP_DENSITY ADJUSTED 

deaths.fit<-vector("list",1)
deaths.fit.uni<-vector("list",1)
for (aa in seq(n_cl)) {
  
  deaths.fit[[aa]]<-vector("list",1)
  deaths.fit.uni[[aa]]<-vector("list",1)
  
  for (bb in seq(5)) {
    deaths.fit.uni[[aa]][[bb]] <- vector("list",1)
  }
}

for (aa in seq(n_cl)) {
  
  for (ii in seq(3)) {
    data_fit <- data.wave[[ii]]
    
    deaths.fit[[aa]][[ii]]<-glmmTMB(deaths ~ 
                                     scale(data_fit$WHITE_NH)+
                                     scale(data_fit$AGE_GR2)+scale(data_fit$AGE_GR4)+scale(data_fit$MALE)+
                                     scale(data_fit$POP_DENSITY)+
                                     (1|State)
                                   + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    
    deaths.fit.uni[[aa]][[1]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$WHITE_NH)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[2]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$AGE_GR2)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[3]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$AGE_GR4)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[4]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$MALE)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[5]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$POP_DENSITY)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    print(paste(aa,ii))
  }
}

# 
# 
# library(DHARMa)
# set.seed(123)
# res = simulateResiduals(Fit.deaths.firstwave)
# plot(res, rank=T)
# 
# res = simulateResiduals(Fit.Deaths.firstwave)
# plot(res, rank=T)

Results.deaths <- Results.deaths.Uni <- vector("list",1)
for (aa in seq(n_cl)) {
  Results.deaths[[aa]] <- Results.deaths.Uni[[aa]] <-vector("list",3)
  
  for (bb in seq(3)) {
    Results.deaths[[aa]][[bb]] <- Results.deaths.Uni[[aa]][[bb]] <-matrix(NA,nrow=(nrow(coef(summary(deaths.fit[[aa]][[bb]]))$cond)-1),ncol=3)
    rownames(Results.deaths[[aa]][[bb]]) <- rownames(Results.deaths.Uni[[aa]][[bb]]) <-rownames(coef(summary(deaths.fit[[aa]][[bb]]))$cond)[-1]
    
    for (ii in seq((nrow(coef(summary(deaths.fit[[aa]][[bb]]))$cond)-1))) {
      Results.deaths[[aa]][[bb]][ii,] <-exp(c(coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1],coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1]-1.96*coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,2],
                                             coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1]+1.96*coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,2]))
      
      Results.deaths.Uni[[aa]][[bb]][ii,] <-exp(c(coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1],coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]-1.96*coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2],
                                                 coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]+1.96*coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2]))
    }
  }
}

WHITE_NH_deaths.Uni <- WHITE_NH_deaths <- vector("list",1)
for (aa in seq(n_cl)) {
  WHITE_NH_deaths[[aa]] <-matrix(NA,nrow=3,ncol=3)
  WHITE_NH_deaths.Uni[[aa]] <-matrix(NA,nrow=3,ncol=3)
  for (ii in seq(3)) {
    WHITE_NH_deaths[[aa]][ii,] <- Results.deaths[[aa]][[ii]][1,]
    WHITE_NH_deaths.Uni[[aa]][ii,] <- Results.deaths.Uni[[aa]][[ii]][1,]
  }
}









### MODEL: ATKINSON ONLY  + AGE40-64, Age85+ SEXRATIO + POP_DENSITY ADJUSTED 

deaths.fit<-vector("list",1)
deaths.fit.uni<-vector("list",1)
for (aa in seq(n_cl)) {
  
  deaths.fit[[aa]]<-vector("list",1)
  deaths.fit.uni[[aa]]<-vector("list",1)
  
  for (bb in seq(5)) {
    deaths.fit.uni[[aa]][[bb]] <- vector("list",1)
  }
}

for (aa in seq(n_cl)) {
  
  for (ii in seq(3)) {
    data_fit <- data.wave[[ii]]
    
    deaths.fit[[aa]][[ii]]<-glmmTMB(deaths ~ 
                                     scale(data_fit$ATKINSON)+
                                     scale(data_fit$AGE_GR2)+scale(data_fit$AGE_GR4)+scale(data_fit$MALE)+
                                     scale(data_fit$POP_DENSITY)+
                                     (1|State)
                                   + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    
    deaths.fit.uni[[aa]][[1]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$ATKINSON)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[2]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$AGE_GR2)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[3]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$AGE_GR4)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[4]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$MALE)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[5]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$POP_DENSITY)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    print(paste(aa,ii))
  }
}

# 
# 
# library(DHARMa)
# set.seed(123)
# res = simulateResiduals(Fit.deaths.firstwave)
# plot(res, rank=T)
# 
# res = simulateResiduals(Fit.Deaths.firstwave)
# plot(res, rank=T)

Results.deaths <- Results.deaths.Uni <- vector("list",1)
for (aa in seq(n_cl)) {
  Results.deaths[[aa]] <- Results.deaths.Uni[[aa]] <-vector("list",3)
  
  for (bb in seq(3)) {
    Results.deaths[[aa]][[bb]] <- Results.deaths.Uni[[aa]][[bb]] <-matrix(NA,nrow=(nrow(coef(summary(deaths.fit[[aa]][[bb]]))$cond)-1),ncol=3)
    rownames(Results.deaths[[aa]][[bb]]) <- rownames(Results.deaths.Uni[[aa]][[bb]]) <-rownames(coef(summary(deaths.fit[[aa]][[bb]]))$cond)[-1]
    
    for (ii in seq((nrow(coef(summary(deaths.fit[[aa]][[bb]]))$cond)-1))) {
      Results.deaths[[aa]][[bb]][ii,] <-exp(c(coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1],coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1]-1.96*coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,2],
                                             coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1]+1.96*coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,2]))
      
      Results.deaths.Uni[[aa]][[bb]][ii,] <-exp(c(coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1],coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]-1.96*coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2],
                                                 coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]+1.96*coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2]))
    }
  }
}

ATKINSON_deaths.Uni <- ATKINSON_deaths <- vector("list",1)
for (aa in seq(n_cl)) {
  ATKINSON_deaths[[aa]] <-matrix(NA,nrow=3,ncol=3)
  ATKINSON_deaths.Uni[[aa]] <-matrix(NA,nrow=3,ncol=3)
  for (ii in seq(3)) {
    ATKINSON_deaths[[aa]][ii,] <- Results.deaths[[aa]][[ii]][1,]
    ATKINSON_deaths.Uni[[aa]][ii,] <- Results.deaths.Uni[[aa]][[ii]][1,]
  }
}




### MODEL: CORRRATIO ONLY  + AGE40-64, Age85+ SEXRATIO + POP_DENSITY ADJUSTED 

deaths.fit<-vector("list",1)
deaths.fit.uni<-vector("list",1)
for (aa in seq(n_cl)) {
  
  deaths.fit[[aa]]<-vector("list",1)
  deaths.fit.uni[[aa]]<-vector("list",1)
  
  for (bb in seq(5)) {
    deaths.fit.uni[[aa]][[bb]] <- vector("list",1)
  }
}

for (aa in seq(n_cl)) {
  
  for (ii in seq(3)) {
    data_fit <- data.wave[[ii]]
    
    deaths.fit[[aa]][[ii]]<-glmmTMB(deaths ~ 
                                     scale(data_fit$CORRRATIO)+
                                     scale(data_fit$AGE_GR2)+scale(data_fit$AGE_GR4)+scale(data_fit$MALE)+
                                     scale(data_fit$POP_DENSITY)+
                                     (1|State)
                                   + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    
    deaths.fit.uni[[aa]][[1]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$CORRRATIO)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[2]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$AGE_GR2)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[3]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$AGE_GR4)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[4]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$MALE)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    deaths.fit.uni[[aa]][[5]][[ii]]<-glmmTMB(deaths ~ scale(data_fit$POP_DENSITY)+(1|State)
                                            + offset(log(POP)), data = data_fit,family = nbinom2, ziformula  = ~ 1
    )
    print(paste(aa,ii))
  }
}

# 
# 
# library(DHARMa)
# set.seed(123)
# res = simulateResiduals(Fit.deaths.firstwave)
# plot(res, rank=T)
# 
# res = simulateResiduals(Fit.Deaths.firstwave)
# plot(res, rank=T)

Results.deaths <- Results.deaths.Uni <- vector("list",1)
for (aa in seq(n_cl)) {
  Results.deaths[[aa]] <- Results.deaths.Uni[[aa]] <-vector("list",3)
  
  for (bb in seq(3)) {
    Results.deaths[[aa]][[bb]] <- Results.deaths.Uni[[aa]][[bb]] <-matrix(NA,nrow=(nrow(coef(summary(deaths.fit[[aa]][[bb]]))$cond)-1),ncol=3)
    rownames(Results.deaths[[aa]][[bb]]) <- rownames(Results.deaths.Uni[[aa]][[bb]]) <-rownames(coef(summary(deaths.fit[[aa]][[bb]]))$cond)[-1]
    
    for (ii in seq((nrow(coef(summary(deaths.fit[[aa]][[bb]]))$cond)-1))) {
      Results.deaths[[aa]][[bb]][ii,] <-exp(c(coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1],coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1]-1.96*coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,2],
                                             coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,1]+1.96*coef(summary(deaths.fit[[aa]][[bb]]))$cond[1+ii,2]))
      
      Results.deaths.Uni[[aa]][[bb]][ii,] <-exp(c(coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1],coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]-1.96*coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2],
                                                 coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,1]+1.96*coef(summary(deaths.fit.uni[[aa]][[ii]][[bb]]))$cond[2,2]))
    }
  }
}

CORRRATIO_deaths.Uni <- CORRRATIO_deaths <- vector("list",1)
for (aa in seq(n_cl)) {
  CORRRATIO_deaths[[aa]] <-matrix(NA,nrow=3,ncol=3)
  CORRRATIO_deaths.Uni[[aa]] <-matrix(NA,nrow=3,ncol=3)
  for (ii in seq(3)) {
    CORRRATIO_deaths[[aa]][ii,] <- Results.deaths[[aa]][[ii]][1,]
    CORRRATIO_deaths.Uni[[aa]][ii,] <- Results.deaths.Uni[[aa]][[ii]][1,]
  }
}





















jpeg("PATHNAME/Figure2.jpg",
     width=16,height=12,units="in",res=900,pointsize=17)
par(mfrow=c(2,1),mar=c(4,4,2,1),oma=c(0,0,1,0))
plot(seq(3),BLACK_Cases[[1]][,1],type="n",xlim=c(1.2,14.8),ylim=c(0.4,3),log="y",xaxt="n",ylab="Relative rate",xlab="Wave")
abline(h=1,col="grey90")
legend("bottomright",c("Unadjusted","Adjusted"),col=c("plum2","navy"),pch=c(16,17),bty="n")
aa<-1
  arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),WHITE_NH_Cases.Uni[[1]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),WHITE_NH_Cases.Uni[[1]][,3],code=0)
  points(seq(1-0.1+3*(aa-1),3*(aa)-0.1),WHITE_NH_Cases.Uni[[1]][,1],col="plum2",pch=16,cex=0.8)
  
  arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),WHITE_NH_Cases[[1]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),WHITE_NH_Cases[[1]][,3],code=0)
  points(seq(1+0.1+3*(aa-1),3*(aa)+0.1),WHITE_NH_Cases[[1]][,1],col="navy",pch=17,cex=0.8)

text(x=2,y=2.8,"% of Non-Hispanic White",cex=0.9)
axis(1,at=c(0,16),c(NA,NA),cex=0.8,lwd.ticks=0)
axis(1,seq(1,15),rep(c(1,2,3),5),cex=0.8)
abline(v=seq(3.5,12.5,by=3),col="grey")
text(x=-0.5,y=3.8,"A. COVID-19 case rate",xpd=T,adj=0)

aa<-2
  arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),BLACK_Cases.Uni[[1]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),BLACK_Cases.Uni[[1]][,3],code=0)
  points(seq(1-0.1+3*(aa-1),3*(aa)-0.1),BLACK_Cases.Uni[[1]][,1],col="plum2",pch=16,cex=0.8)
  
  arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),BLACK_Cases[[1]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),BLACK_Cases[[1]][,3],code=0)
  points(seq(1+0.1+3*(aa-1),3*(aa)+0.1),BLACK_Cases[[1]][,1],col="navy",pch=17,cex=0.8)

text(x=5,y=2.8,"% of Black/African American",cex=0.9)


aa<-3
  arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),HISPANIC_Cases.Uni[[1]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),HISPANIC_Cases.Uni[[1]][,3],code=0)
  points(seq(1-0.1+3*(aa-1),3*(aa)-0.1),HISPANIC_Cases.Uni[[1]][,1],col="plum2",pch=16,cex=0.8)
  
  arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),HISPANIC_Cases[[1]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),HISPANIC_Cases[[1]][,3],code=0)
  points(seq(1+0.1+3*(aa-1),3*(aa)+0.1),HISPANIC_Cases[[1]][,1],col="navy",pch=17,cex=0.8)

text(x=8,y=2.8,"% of Hispanic",cex=0.9)


aa<-4
arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),ATKINSON_Cases.Uni[[1]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),ATKINSON_Cases.Uni[[1]][,3],code=0)
points(seq(1-0.1+3*(aa-1),3*(aa)-0.1),ATKINSON_Cases.Uni[[1]][,1],col="plum2",pch=16,cex=0.8)

arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),ATKINSON_Cases[[1]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),ATKINSON_Cases[[1]][,3],code=0)
points(seq(1+0.1+3*(aa-1),3*(aa)+0.1),ATKINSON_Cases[[1]][,1],col="navy",pch=17,cex=0.8)

text(x=11,y=2.8,"Atkinson index",cex=0.9)



aa<-5
arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),CORRRATIO_Cases.Uni[[1]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),CORRRATIO_Cases.Uni[[1]][,3],code=0)
points(seq(1-0.1+3*(aa-1),3*(aa)-0.1),CORRRATIO_Cases.Uni[[1]][,1],col="plum2",pch=16,cex=0.8)

arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),CORRRATIO_Cases[[1]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),CORRRATIO_Cases[[1]][,3],code=0)
points(seq(1+0.1+3*(aa-1),3*(aa)+0.1),CORRRATIO_Cases[[1]][,1],col="navy",pch=17,cex=0.8)

text(x=14,y=2.8,"Eta-squared",cex=0.9)




plot(seq(3),BLACK_deaths[[1]][,1],type="n",xlim=c(1.2,14.8),ylim=c(0.4,3),log="y",xaxt="n",ylab="Relative rate",xlab="Wave")
abline(h=1,col="grey90")
aa<-1
arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),WHITE_NH_deaths.Uni[[1]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),WHITE_NH_deaths.Uni[[1]][,3],code=0)
points(seq(1-0.1+3*(aa-1),3*(aa)-0.1),WHITE_NH_deaths.Uni[[1]][,1],col="plum2",pch=16,cex=0.8)

arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),WHITE_NH_deaths[[1]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),WHITE_NH_deaths[[1]][,3],code=0)
points(seq(1+0.1+3*(aa-1),3*(aa)+0.1),WHITE_NH_deaths[[1]][,1],col="navy",pch=17,cex=0.8)

text(x=2,y=2.8,"% of Non-Hispanic White",cex=0.9)
axis(1,at=c(0,16),c(NA,NA),cex=0.8,lwd.ticks=0)
axis(1,seq(1,15),rep(c(1,2,3),5),cex=0.8)
abline(v=seq(3.5,12.5,by=3),col="grey")
text(x=-0.5,y=3.8,"B. COVID-19 mortality rate",xpd=T,adj=0)

aa<-2
arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),BLACK_deaths.Uni[[1]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),BLACK_deaths.Uni[[1]][,3],code=0)
points(seq(1-0.1+3*(aa-1),3*(aa)-0.1),BLACK_deaths.Uni[[1]][,1],col="plum2",pch=16,cex=0.8)

arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),BLACK_deaths[[1]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),BLACK_deaths[[1]][,3],code=0)
points(seq(1+0.1+3*(aa-1),3*(aa)+0.1),BLACK_deaths[[1]][,1],col="navy",pch=17,cex=0.8)

text(x=5,y=2.8,"% of Black/African American",cex=0.9)


aa<-3
arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),HISPANIC_deaths.Uni[[1]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),HISPANIC_deaths.Uni[[1]][,3],code=0)
points(seq(1-0.1+3*(aa-1),3*(aa)-0.1),HISPANIC_deaths.Uni[[1]][,1],col="plum2",pch=16,cex=0.8)

arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),HISPANIC_deaths[[1]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),HISPANIC_deaths[[1]][,3],code=0)
points(seq(1+0.1+3*(aa-1),3*(aa)+0.1),HISPANIC_deaths[[1]][,1],col="navy",pch=17,cex=0.8)

text(x=8,y=2.8,"% of Hispanic",cex=0.9)


aa<-4
arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),ATKINSON_deaths.Uni[[1]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),ATKINSON_deaths.Uni[[1]][,3],code=0)
points(seq(1-0.1+3*(aa-1),3*(aa)-0.1),ATKINSON_deaths.Uni[[1]][,1],col="plum2",pch=16,cex=0.8)

arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),ATKINSON_deaths[[1]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),ATKINSON_deaths[[1]][,3],code=0)
points(seq(1+0.1+3*(aa-1),3*(aa)+0.1),ATKINSON_deaths[[1]][,1],col="navy",pch=17,cex=0.8)

text(x=11,y=2.8,"Atkinson index",cex=0.9)



aa<-5
arrows(seq(1-0.1+3*(aa-1),3*(aa)-0.1),CORRRATIO_deaths.Uni[[1]][,2],seq(1-0.1+3*(aa-1),3*(aa)-0.1),CORRRATIO_deaths.Uni[[1]][,3],code=0)
points(seq(1-0.1+3*(aa-1),3*(aa)-0.1),CORRRATIO_deaths.Uni[[1]][,1],col="plum2",pch=16,cex=0.8)

arrows(seq(1+0.1+3*(aa-1),3*(aa)+0.1),CORRRATIO_deaths[[1]][,2],seq(1+0.1+3*(aa-1),3*(aa)+0.1),CORRRATIO_deaths[[1]][,3],code=0)
points(seq(1+0.1+3*(aa-1),3*(aa)+0.1),CORRRATIO_deaths[[1]][,1],col="navy",pch=17,cex=0.8)

text(x=14,y=2.8,"Eta-squared",cex=0.9)

dev.off()








