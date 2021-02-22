
# Cases<-read.csv("C:\\Users\\hongh\\Dropbox (Yale_FES)\\COVID19\\USCOVID\\JohnsHopkins\\1029\\Cases_1028.csv")
# Deaths<-read.csv("C:\\Users\\hongh\\Dropbox (Yale_FES)\\COVID19\\USCOVID\\JohnsHopkins\\1029\\Deaths_1028.csv")

cases.data<-read.csv("C:\\Users\\hongh\\Dropbox (Yale_FES)\\COVID19\\USCOVID\\NYT\\1220\\us-counties.csv")

cases.data.NYC<-read.csv("C:\\Users\\hongh\\Dropbox (Yale_FES)\\COVID19\\USCOVID\\NYC\\1220\\boroughs-case-hosp-death.csv")

cases.data$date <- as.Date(cases.data$date,format="%Y-%m-%d")

cases.data <- cases.data[,c("date","fips","cases","deaths","state")]

colnames(cases.data)[2]<-"FIPS_COUNTY"
cases.data<-cases.data[!is.na(cases.data$FIPS_COUNTY),]



X.Time <- seq(as.Date("2020-01-21",origin="1970-01-01",format="%Y-%m-%d"), as.Date("2020-12-20",origin="1970-01-01",format="%Y-%m-%d"),by=1)
n_county<-unique(cases.data$FIPS_COUNTY)

cases<-data.frame(matrix(NA,nrow=length(n_county)*length(X.Time),ncol=1))
cases$date <- rep(X.Time,length(n_county))
cases$FIPS_COUNTY <- rep(cases.data[!duplicated(cases.data$FIPS_COUNTY),c("FIPS_COUNTY")],each=length(X.Time))
cases$state <- rep(cases.data[!duplicated(cases.data$FIPS_COUNTY),c("state")],each=length(X.Time))

cases<-cases[,c("date","FIPS_COUNTY","state")]

cases<-merge(cases,cases.data[,c(1,2,3,4)],by=c("date","FIPS_COUNTY"),all.x=T)

cases[is.na(cases[,"cases"]),"cases"] <-0
cases[is.na(cases[,"deaths"]),"deaths"] <-0

colnames(cases)[c(4,5)] <- c("C_cases","C_deaths")

cases<-cases[cases$FIPS<=58000,]

cases<-cases[order(cases$FIPS_COUNTY,cases$date),]
cases$cases <-cases$C_cases
cases$deaths <-cases$C_deaths
for (i in seq(length(unique(cases$FIPS_COUNTY)))) {
  for (aa in seq(2,length(X.Time))) {
  cases[(length(X.Time)*(i-1)+aa),"cases"] <- cases[(length(X.Time)*(i-1)+aa),"C_cases"]-cases[(length(X.Time)*(i-1)+aa-1),"C_cases"]
  cases[(length(X.Time)*(i-1)+aa),"deaths"] <- cases[(length(X.Time)*(i-1)+aa),"C_deaths"]-cases[(length(X.Time)*(i-1)+aa-1),"C_deaths"]
  }
print(i)
}

aa<-cases[cases$cases< -2, ]
aa<-aa[order(aa$cases),]

cases2 <- cases
cases2$cases <- ifelse(cases2$cases<0,0,cases2$cases)
cases2$deaths <- ifelse(cases2$deaths<0,0,cases2$deaths)


Manhattan <- data.frame(date=as.Date(cases.data.NYC$DATE_OF_INTEREST,format="%m/%d/%Y"),cases=cases.data.NYC$MN_CASE_COUNT,cases=cases.data.NYC$MN_DEATH_COUNT,FIPS_COUNTY=36061, state="New York")
Queens <- data.frame(date=as.Date(cases.data.NYC$DATE_OF_INTEREST,format="%m/%d/%Y"),cases=cases.data.NYC$QN_CASE_COUNT,cases=cases.data.NYC$QN_DEATH_COUNT,FIPS_COUNTY=36081, state="New York")
Brooklyn <- data.frame(date=as.Date(cases.data.NYC$DATE_OF_INTEREST,format="%m/%d/%Y"),cases=cases.data.NYC$BK_CASE_COUNT,cases=cases.data.NYC$BK_DEATH_COUNT,FIPS_COUNTY=36047, state="New York")
Bronx <- data.frame(date=as.Date(cases.data.NYC$DATE_OF_INTEREST,format="%m/%d/%Y"),cases=cases.data.NYC$BX_CASE_COUNT,cases=cases.data.NYC$BX_DEATH_COUNT,FIPS_COUNTY=36005, state="New York")
Staten <- data.frame(date=as.Date(cases.data.NYC$DATE_OF_INTEREST,format="%m/%d/%Y"),cases=cases.data.NYC$SI_CASE_COUNT,cases=cases.data.NYC$SI_DEATH_COUNT,FIPS_COUNTY=36085, state="New York")


NewYorkCity<-rbind(Manhattan,Queens,Brooklyn,Bronx,Staten)
colnames(NewYorkCity)[3] <- "deaths"

NewYorkCity$date <- as.Date(NewYorkCity$date,format="%Y-%m-%d")
NewYorkCity<-NewYorkCity[,c("date","FIPS_COUNTY","cases","deaths")]

cases.NYC<-data.frame(date=matrix(NA,nrow=5*length(X.Time),ncol=1))
cases.NYC$date <- rep(X.Time,5)
cases.NYC$FIPS_COUNTY <- rep(NewYorkCity[!duplicated(NewYorkCity$FIPS_COUNTY),c("FIPS_COUNTY")],each=length(X.Time))
cases.NYC$state <- rep(NewYorkCity[!duplicated(NewYorkCity$FIPS_COUNTY),c("state")],each=length(X.Time))

cases.NYC$date <- as.Date(cases.NYC$date,format="%Y-%m-%d")

cases.NYC<-merge(cases.NYC,NewYorkCity,by=c("date","FIPS_COUNTY"),all.x=T)
cases.NYC[is.na(cases.NYC[,"cases"]),"cases"] <-0
cases.NYC[is.na(cases.NYC[,"deaths"]),"deaths"] <-0




cases.NYC<-cases.NYC[order(cases.NYC$FIPS_COUNTY,cases.NYC$date),]
cases.NYC$C_cases <-cases.NYC$cases
cases.NYC$C_deaths <-cases.NYC$deaths
for (i in seq(length(unique(cases.NYC$FIPS_COUNTY))) ) {
  for (aa in seq(2,length(X.Time))) {
    cases.NYC[(length(X.Time)*(i-1)+aa),"C_cases"] <- sum(cases.NYC[(length(X.Time)*(i-1)+1):(length(X.Time)*(i-1)+aa),"cases"])
    
    cases.NYC[(length(X.Time)*(i-1)+aa),"C_deaths"] <- sum(cases.NYC[(length(X.Time)*(i-1)+1):(length(X.Time)*(i-1)+aa),"deaths"])
    
  }
  
}


cases.NYC$state <- "New York"
cases.NYC <- cases.NYC[,colnames(cases2)]

cases.fin<-rbind(cases2,cases.NYC)

Cases.dat <- cases.fin

library(lubridate)
Cases.dat$month<-month(Cases.dat$date)
Cases.dat$doy <- yday(Cases.dat$date)


X.Time <- seq(as.Date("2020-01-21",origin="1970-01-01",format="%Y-%m-%d"), as.Date("2020-12-18",origin="1970-01-01",format="%Y-%m-%d"),by=1)

Cases.dat <-subset(Cases.dat,date<=max(X.Time))
write.csv(Cases.dat,"C:\\Users\\hongh\\Dropbox (Yale_FES)\\COVID19\\USCOVID\\NYT\\1220\\transposed_allcounties.csv")





















X.Time <- seq(as.Date("2020-01-21",origin="1970-01-01",format="%Y-%m-%d"), as.Date("2020-12-18",origin="1970-01-01",format="%Y-%m-%d"),by=1)

Cases.dat<-read.csv("C:\\Users\\hongh\\Dropbox (Yale_FES)\\COVID19\\USCOVID\\NYT\\1220\\transposed_allcounties.csv")

#TS.Cases <- merge(Cases.dat,Firstdate,by="FIPS_COUNTY")
TS.Cases <- merge(Cases.dat,DEMOGRAPHIC,by="FIPS_COUNTY")
TS.Cases <- merge(TS.Cases, POVERTY.dat,by="FIPS_COUNTY")
TS.Cases <- merge(TS.Cases, POP_DENSITY,by="FIPS_COUNTY")
TS.Cases <- merge(TS.Cases, RACE.dat,by="FIPS_COUNTY")
TS.Cases$BLACK_H <- TS.Cases$BLACK_ALL - TS.Cases$BLACK_NH
TS.Cases <- merge(TS.Cases, US_BLACK_ATKINSON,by="FIPS_COUNTY")
TS.Cases <- merge(TS.Cases, US_BLACK_CORRRATIO,by="FIPS_COUNTY")
TS.Cases <- merge(TS.Cases, HIP,by="FIPS_COUNTY")


TS.Cases$AGE_GR4_q <- ifelse(TS.Cases$AGE_GR4<quantile(TS.Cases$AGE_GR4,0.20,na.rm=T),0,
                             ifelse(TS.Cases$AGE_GR4<quantile(TS.Cases$AGE_GR4,0.40,na.rm=T),1,       
                                    ifelse(TS.Cases$AGE_GR4<quantile(TS.Cases$AGE_GR4,0.60,na.rm=T),2,
                                           ifelse(TS.Cases$AGE_GR4<quantile(TS.Cases$AGE_GR4,0.80,na.rm=T),3,4))))

TS.Cases$AGE_GR3_q <- ifelse(TS.Cases$AGE_GR3<quantile(TS.Cases$AGE_GR3,0.20,na.rm=T),0,
                             ifelse(TS.Cases$AGE_GR3<quantile(TS.Cases$AGE_GR3,0.40,na.rm=T),1,       
                                    ifelse(TS.Cases$AGE_GR3<quantile(TS.Cases$AGE_GR3,0.60,na.rm=T),2,
                                           ifelse(TS.Cases$AGE_GR3<quantile(TS.Cases$AGE_GR3,0.80,na.rm=T),3,4))))


TS.Cases$AGE_GR2_q <- ifelse(TS.Cases$AGE_GR2<quantile(TS.Cases$AGE_GR2,0.20,na.rm=T),0,
                             ifelse(TS.Cases$AGE_GR2<quantile(TS.Cases$AGE_GR2,0.40,na.rm=T),1,       
                                    ifelse(TS.Cases$AGE_GR2<quantile(TS.Cases$AGE_GR2,0.60,na.rm=T),2,
                                           ifelse(TS.Cases$AGE_GR2<quantile(TS.Cases$AGE_GR2,0.80,na.rm=T),3,4))))


TS.Cases$AGE_GR1_q <- ifelse(TS.Cases$AGE_GR1<quantile(TS.Cases$AGE_GR1,0.20,na.rm=T),0,
                             ifelse(TS.Cases$AGE_GR1<quantile(TS.Cases$AGE_GR1,0.40,na.rm=T),1,       
                                    ifelse(TS.Cases$AGE_GR1<quantile(TS.Cases$AGE_GR1,0.60,na.rm=T),2,
                                           ifelse(TS.Cases$AGE_GR1<quantile(TS.Cases$AGE_GR1,0.80,na.rm=T),3,4))))


TS.Cases$Poverty_q <- ifelse(TS.Cases$POVERTY<quantile(TS.Cases$POVERTY,0.20,na.rm=T),0,
                              ifelse(TS.Cases$POVERTY<quantile(TS.Cases$POVERTY,0.40,na.rm=T),1,       
                                     ifelse(TS.Cases$POVERTY<quantile(TS.Cases$POVERTY,0.60,na.rm=T),2,
                                            ifelse(TS.Cases$POVERTY<quantile(TS.Cases$POVERTY,0.80,na.rm=T),3,4))))


TS.Cases$POP_DENSITY_q <- ifelse(TS.Cases$POP_DENSITY<quantile(TS.Cases$POP_DENSITY,0.20,na.rm=T),0,
                             ifelse(TS.Cases$POP_DENSITY<quantile(TS.Cases$POP_DENSITY,0.40,na.rm=T),1,       
                                    ifelse(TS.Cases$POP_DENSITY<quantile(TS.Cases$POP_DENSITY,0.60,na.rm=T),2,
                                           ifelse(TS.Cases$POP_DENSITY<quantile(TS.Cases$POP_DENSITY,0.80,na.rm=T),3,4))))


TS.Cases$WHITE_NH_q <- ifelse(TS.Cases$WHITE_NH<quantile(TS.Cases$WHITE_NH,0.20,na.rm=T),0,
                                 ifelse(TS.Cases$WHITE_NH<quantile(TS.Cases$WHITE_NH,0.40,na.rm=T),1,       
                                        ifelse(TS.Cases$WHITE_NH<quantile(TS.Cases$WHITE_NH,0.60,na.rm=T),2,
                                               ifelse(TS.Cases$WHITE_NH<quantile(TS.Cases$WHITE_NH,0.80,na.rm=T),3,4))))



TS.Cases$BLACK_NH_q <- ifelse(TS.Cases$BLACK_NH<quantile(TS.Cases$BLACK_NH,0.20,na.rm=T),0,
                                 ifelse(TS.Cases$BLACK_NH<quantile(TS.Cases$BLACK_NH,0.40,na.rm=T),1,       
                                        ifelse(TS.Cases$BLACK_NH<quantile(TS.Cases$BLACK_NH,0.60,na.rm=T),2,
                                               ifelse(TS.Cases$BLACK_NH<quantile(TS.Cases$BLACK_NH,0.80,na.rm=T),3,4))))

TS.Cases$BLACK_H_q <- ifelse(TS.Cases$BLACK_H<=quantile(TS.Cases$BLACK_H,0.20,na.rm=T),0,
                              ifelse(TS.Cases$BLACK_H<quantile(TS.Cases$BLACK_H,0.40,na.rm=T),1,       
                                     ifelse(TS.Cases$BLACK_H<quantile(TS.Cases$BLACK_H,0.60,na.rm=T),2,
                                            ifelse(TS.Cases$BLACK_H<quantile(TS.Cases$BLACK_H,0.80,na.rm=T),3,4))))


TS.Cases$BLACK_ALL_q <- ifelse(TS.Cases$BLACK_ALL<quantile(TS.Cases$BLACK_ALL,0.20,na.rm=T),0,
                              ifelse(TS.Cases$BLACK_ALL<quantile(TS.Cases$BLACK_ALL,0.40,na.rm=T),1,       
                                     ifelse(TS.Cases$BLACK_ALL<quantile(TS.Cases$BLACK_ALL,0.60,na.rm=T),2,
                                            ifelse(TS.Cases$BLACK_ALL<quantile(TS.Cases$BLACK_ALL,0.80,na.rm=T),3,4))))



TS.Cases$HISPANIC_q <- ifelse(TS.Cases$HISPANIC<quantile(TS.Cases$HISPANIC,0.20,na.rm=T),0,
                               ifelse(TS.Cases$HISPANIC<quantile(TS.Cases$HISPANIC,0.40,na.rm=T),1,       
                                      ifelse(TS.Cases$HISPANIC<quantile(TS.Cases$HISPANIC,0.60,na.rm=T),2,
                                             ifelse(TS.Cases$HISPANIC<quantile(TS.Cases$HISPANIC,0.80,na.rm=T),3,4))))



TS.Cases$ATKINSON_q <- ifelse(TS.Cases$ATKINSON<quantile(TS.Cases$ATKINSON,0.20,na.rm=T),0,
                              ifelse(TS.Cases$ATKINSON<quantile(TS.Cases$ATKINSON,0.40,na.rm=T),1,       
                                     ifelse(TS.Cases$ATKINSON<quantile(TS.Cases$ATKINSON,0.60,na.rm=T),2,
                                            ifelse(TS.Cases$ATKINSON<quantile(TS.Cases$ATKINSON,0.80,na.rm=T),3,4))))


TS.Cases$CORRRATIO_q <- ifelse(TS.Cases$CORRRATIO<quantile(TS.Cases$CORRRATIO,0.20,na.rm=T),0,
                              ifelse(TS.Cases$CORRRATIO<quantile(TS.Cases$CORRRATIO,0.40,na.rm=T),1,       
                                     ifelse(TS.Cases$CORRRATIO<quantile(TS.Cases$CORRRATIO,0.60,na.rm=T),2,
                                            ifelse(TS.Cases$CORRRATIO<quantile(TS.Cases$CORRRATIO,0.80,na.rm=T),3,4))))


TS.Cases$gini99_q <- ifelse(TS.Cases$gini99<quantile(TS.Cases$gini99,0.20,na.rm=T),0,
                               ifelse(TS.Cases$gini99<quantile(TS.Cases$gini99,0.40,na.rm=T),1,       
                                      ifelse(TS.Cases$gini99<quantile(TS.Cases$gini99,0.60,na.rm=T),2,
                                             ifelse(TS.Cases$gini99<quantile(TS.Cases$gini99,0.80,na.rm=T),3,4))))



TS.Cases.POP_DENSITY <- vector("list")
TS.Cases.POP_DENSITY_AVG <- vector("list")
TS.Cases.POP_DENSITY_POP <- vector("list")
TS.Deaths.POP_DENSITY <- vector("list")
TS.Deaths.POP_DENSITY_AVG <- vector("list")
TS.Deaths.POP_DENSITY_POP <- vector("list")

TS.Cases.AGE_GR1 <- vector("list")
TS.Cases.AGE_GR1_AVG <- vector("list")
TS.Cases.AGE_GR1_POP <- vector("list")
TS.Deaths.AGE_GR1 <- vector("list")
TS.Deaths.AGE_GR1_AVG <- vector("list")
TS.Deaths.AGE_GR1_POP <- vector("list")

TS.Cases.AGE_GR2 <- vector("list")
TS.Cases.AGE_GR2_AVG <- vector("list")
TS.Cases.AGE_GR2_POP <- vector("list")
TS.Deaths.AGE_GR2 <- vector("list")
TS.Deaths.AGE_GR2_AVG <- vector("list")
TS.Deaths.AGE_GR2_POP <- vector("list")

TS.Cases.AGE_GR3 <- vector("list")
TS.Cases.AGE_GR3_AVG <- vector("list")
TS.Cases.AGE_GR3_POP <- vector("list")
TS.Deaths.AGE_GR3 <- vector("list")
TS.Deaths.AGE_GR3_AVG <- vector("list")
TS.Deaths.AGE_GR3_POP <- vector("list")

TS.Cases.AGE_GR4 <- vector("list")
TS.Cases.AGE_GR4_AVG <- vector("list")
TS.Cases.AGE_GR4_POP <- vector("list")
TS.Deaths.AGE_GR4 <- vector("list")
TS.Deaths.AGE_GR4_AVG <- vector("list")
TS.Deaths.AGE_GR4_POP <- vector("list")


TS.Cases.Poverty <- vector("list")
TS.Cases.Poverty_AVG <- vector("list")
TS.Cases.Poverty_POP <- vector("list")
TS.Deaths.Poverty <- vector("list")
TS.Deaths.Poverty_AVG <- vector("list")
TS.Deaths.Poverty_POP <- vector("list")


TS.Cases.WHITE_NH <- vector("list")
TS.Cases.WHITE_NH_AVG <- vector("list")
TS.Cases.WHITE_NH_POP <- vector("list")
TS.Deaths.WHITE_NH <- vector("list")
TS.Deaths.WHITE_NH_AVG <- vector("list")
TS.Deaths.WHITE_NH_POP <- vector("list")

TS.Cases.BLACK_NH <- vector("list")
TS.Cases.BLACK_NH_AVG <- vector("list")
TS.Cases.BLACK_NH_POP <- vector("list")
TS.Deaths.BLACK_NH <- vector("list")
TS.Deaths.BLACK_NH_AVG <- vector("list")
TS.Deaths.BLACK_NH_POP <- vector("list")


TS.Cases.BLACK_H <- vector("list")
TS.Cases.BLACK_H_AVG <- vector("list")
TS.Cases.BLACK_H_POP <- vector("list")
TS.Deaths.BLACK_H <- vector("list")
TS.Deaths.BLACK_H_AVG <- vector("list")
TS.Deaths.BLACK_H_POP <- vector("list")

TS.Cases.BLACK_ALL <- vector("list")
TS.Cases.BLACK_ALL_AVG <- vector("list")
TS.Cases.BLACK_ALL_POP <- vector("list")
TS.Deaths.BLACK_ALL <- vector("list")
TS.Deaths.BLACK_ALL_AVG <- vector("list")
TS.Deaths.BLACK_ALL_POP <- vector("list")


TS.Cases.HISPANIC <- vector("list")
TS.Cases.HISPANIC_AVG <- vector("list")
TS.Cases.HISPANIC_POP <- vector("list")
TS.Deaths.HISPANIC <- vector("list")
TS.Deaths.HISPANIC_AVG <- vector("list")
TS.Deaths.HISPANIC_POP <- vector("list")


TS.Cases.ATKINSON <- vector("list")
TS.Cases.ATKINSON_AVG <- vector("list")
TS.Cases.ATKINSON_POP <- vector("list")
TS.Deaths.ATKINSON <- vector("list")
TS.Deaths.ATKINSON_AVG <- vector("list")
TS.Deaths.ATKINSON_POP <- vector("list")


TS.Cases.CORRRATIO <- vector("list")
TS.Cases.CORRRATIO_AVG <- vector("list")
TS.Cases.CORRRATIO_POP <- vector("list")
TS.Deaths.CORRRATIO <- vector("list")
TS.Deaths.CORRRATIO_AVG <- vector("list")
TS.Deaths.CORRRATIO_POP <- vector("list")


TS.Cases.gini99 <- vector("list")
TS.Cases.gini99_AVG <- vector("list")
TS.Cases.gini99_POP <- vector("list")
TS.Deaths.gini99 <- vector("list")
TS.Deaths.gini99_AVG <- vector("list")
TS.Deaths.gini99_POP <- vector("list")

TS.Cases.New.AGE_GR1 <- vector("list")
TS.Cases.New.AGE_GR1_AVG <- vector("list")
TS.Cases.New.AGE_GR1_POP <- vector("list")
TS.Deaths.New.AGE_GR1 <- vector("list")
TS.Deaths.New.AGE_GR1_AVG <- vector("list")
TS.Deaths.New.AGE_GR1_POP <- vector("list")

TS.Cases.New.AGE_GR2 <- vector("list")
TS.Cases.New.AGE_GR2_AVG <- vector("list")
TS.Cases.New.AGE_GR2_POP <- vector("list")
TS.Deaths.New.AGE_GR2 <- vector("list")
TS.Deaths.New.AGE_GR2_AVG <- vector("list")
TS.Deaths.New.AGE_GR2_POP <- vector("list")


TS.Cases.New.AGE_GR3 <- vector("list")
TS.Cases.New.AGE_GR3_AVG <- vector("list")
TS.Cases.New.AGE_GR3_POP <- vector("list")
TS.Deaths.New.AGE_GR3 <- vector("list")
TS.Deaths.New.AGE_GR3_AVG <- vector("list")
TS.Deaths.New.AGE_GR3_POP <- vector("list")

TS.Cases.New.AGE_GR4 <- vector("list")
TS.Cases.New.AGE_GR4_AVG <- vector("list")
TS.Cases.New.AGE_GR4_POP <- vector("list")
TS.Deaths.New.AGE_GR4 <- vector("list")
TS.Deaths.New.AGE_GR4_AVG <- vector("list")
TS.Deaths.New.AGE_GR4_POP <- vector("list")

TS.Cases.New.Poverty <- vector("list")
TS.Cases.New.Poverty_AVG <- vector("list")
TS.Cases.New.Poverty_POP <- vector("list")
TS.Deaths.New.Poverty <- vector("list")
TS.Deaths.New.Poverty_AVG <- vector("list")
TS.Deaths.New.Poverty_POP <- vector("list")


TS.Cases.New.POP_DENSITY <- vector("list")
TS.Cases.New.POP_DENSITY_AVG <- vector("list")
TS.Cases.New.POP_DENSITY_POP <- vector("list")
TS.Deaths.New.POP_DENSITY <- vector("list")
TS.Deaths.New.POP_DENSITY_AVG <- vector("list")
TS.Deaths.New.POP_DENSITY_POP <- vector("list")

TS.Cases.New.WHITE_NH <- vector("list")
TS.Cases.New.WHITE_NH_AVG <- vector("list")
TS.Cases.New.WHITE_NH_POP <- vector("list")
TS.Deaths.New.WHITE_NH <- vector("list")
TS.Deaths.New.WHITE_NH_AVG <- vector("list")
TS.Deaths.New.WHITE_NH_POP <- vector("list")

TS.Cases.New.BLACK_NH <- vector("list")
TS.Cases.New.BLACK_NH_AVG <- vector("list")
TS.Cases.New.BLACK_NH_POP <- vector("list")
TS.Deaths.New.BLACK_NH <- vector("list")
TS.Deaths.New.BLACK_NH_AVG <- vector("list")
TS.Deaths.New.BLACK_NH_POP <- vector("list")


TS.Cases.New.BLACK_H <- vector("list")
TS.Cases.New.BLACK_H_AVG <- vector("list")
TS.Cases.New.BLACK_H_POP <- vector("list")
TS.Deaths.New.BLACK_H <- vector("list")
TS.Deaths.New.BLACK_H_AVG <- vector("list")
TS.Deaths.New.BLACK_H_POP <- vector("list")

TS.Cases.New.BLACK_ALL <- vector("list")
TS.Cases.New.BLACK_ALL_AVG <- vector("list")
TS.Cases.New.BLACK_ALL_POP <- vector("list")
TS.Deaths.New.BLACK_ALL <- vector("list")
TS.Deaths.New.BLACK_ALL_AVG <- vector("list")
TS.Deaths.New.BLACK_ALL_POP <- vector("list")


TS.Cases.New.HISPANIC <- vector("list")
TS.Cases.New.HISPANIC_AVG <- vector("list")
TS.Cases.New.HISPANIC_POP <- vector("list")
TS.Deaths.New.HISPANIC <- vector("list")
TS.Deaths.New.HISPANIC_AVG <- vector("list")
TS.Deaths.New.HISPANIC_POP <- vector("list")


TS.Cases.New.ATKINSON <- vector("list")
TS.Cases.New.ATKINSON_AVG <- vector("list")
TS.Cases.New.ATKINSON_POP <- vector("list")
TS.Deaths.New.ATKINSON <- vector("list")
TS.Deaths.New.ATKINSON_AVG <- vector("list")
TS.Deaths.New.ATKINSON_POP <- vector("list")


TS.Cases.New.CORRRATIO <- vector("list")
TS.Cases.New.CORRRATIO_AVG <- vector("list")
TS.Cases.New.CORRRATIO_POP <- vector("list")
TS.Deaths.New.CORRRATIO <- vector("list")
TS.Deaths.New.CORRRATIO_AVG <- vector("list")
TS.Deaths.New.CORRRATIO_POP <- vector("list")




library(lubridate)

TS.Cases$doy <- yday(TS.Cases$date)


for (ii in seq(5)) {
  TS.Cases.AGE_GR1[[ii]] <- subset(TS.Cases, AGE_GR1_q==(ii-1) )
  TS.Cases.AGE_GR1_AVG[[ii]]<-aggregate(TS.Cases.AGE_GR1[[ii]]$C_cases,by=list(TS.Cases.AGE_GR1[[ii]]$doy),FUN=sum)
  TS.Cases.AGE_GR1_POP[[ii]]<-aggregate(TS.Cases.AGE_GR1[[ii]]$POP,by=list(TS.Cases.AGE_GR1[[ii]]$doy),FUN=sum)
  TS.Cases.AGE_GR1_AVG[[ii]][,2] <- TS.Cases.AGE_GR1_AVG[[ii]][,2]/TS.Cases.AGE_GR1_POP[[ii]][,2]
  
  TS.Cases.AGE_GR2[[ii]] <- subset(TS.Cases, AGE_GR2_q==(ii-1) )
  TS.Cases.AGE_GR2_AVG[[ii]]<-aggregate(TS.Cases.AGE_GR2[[ii]]$C_cases,by=list(TS.Cases.AGE_GR2[[ii]]$doy),FUN=sum)
  TS.Cases.AGE_GR2_POP[[ii]]<-aggregate(TS.Cases.AGE_GR2[[ii]]$POP,by=list(TS.Cases.AGE_GR2[[ii]]$doy),FUN=sum)
  TS.Cases.AGE_GR2_AVG[[ii]][,2] <- TS.Cases.AGE_GR2_AVG[[ii]][,2]/TS.Cases.AGE_GR2_POP[[ii]][,2]
  
  TS.Cases.AGE_GR3[[ii]] <- subset(TS.Cases, AGE_GR3_q==(ii-1) )
  TS.Cases.AGE_GR3_AVG[[ii]]<-aggregate(TS.Cases.AGE_GR3[[ii]]$C_cases,by=list(TS.Cases.AGE_GR3[[ii]]$doy),FUN=sum)
  TS.Cases.AGE_GR3_POP[[ii]]<-aggregate(TS.Cases.AGE_GR3[[ii]]$POP,by=list(TS.Cases.AGE_GR3[[ii]]$doy),FUN=sum)
  TS.Cases.AGE_GR3_AVG[[ii]][,2] <- TS.Cases.AGE_GR3_AVG[[ii]][,2]/TS.Cases.AGE_GR3_POP[[ii]][,2]
  
  TS.Cases.AGE_GR4[[ii]] <- subset(TS.Cases, AGE_GR4_q==(ii-1) )
  TS.Cases.AGE_GR4_AVG[[ii]]<-aggregate(TS.Cases.AGE_GR4[[ii]]$C_cases,by=list(TS.Cases.AGE_GR4[[ii]]$doy),FUN=sum)
  TS.Cases.AGE_GR4_POP[[ii]]<-aggregate(TS.Cases.AGE_GR4[[ii]]$POP,by=list(TS.Cases.AGE_GR4[[ii]]$doy),FUN=sum)
  TS.Cases.AGE_GR4_AVG[[ii]][,2] <- TS.Cases.AGE_GR4_AVG[[ii]][,2]/TS.Cases.AGE_GR4_POP[[ii]][,2]
  
TS.Cases.Poverty[[ii]] <- subset(TS.Cases, Poverty_q==(ii-1) )
TS.Cases.Poverty_AVG[[ii]]<-aggregate(TS.Cases.Poverty[[ii]]$C_cases,by=list(TS.Cases.Poverty[[ii]]$doy),FUN=sum)
TS.Cases.Poverty_POP[[ii]]<-aggregate(TS.Cases.Poverty[[ii]]$POP,by=list(TS.Cases.Poverty[[ii]]$doy),FUN=sum)
TS.Cases.Poverty_AVG[[ii]][,2] <- TS.Cases.Poverty_AVG[[ii]][,2]/TS.Cases.Poverty_POP[[ii]][,2]

TS.Cases.POP_DENSITY[[ii]] <- subset(TS.Cases, POP_DENSITY_q==(ii-1) )
TS.Cases.POP_DENSITY_AVG[[ii]]<-aggregate(TS.Cases.POP_DENSITY[[ii]]$C_cases,by=list(TS.Cases.POP_DENSITY[[ii]]$doy),FUN=sum)
TS.Cases.POP_DENSITY_POP[[ii]]<-aggregate(TS.Cases.POP_DENSITY[[ii]]$POP,by=list(TS.Cases.POP_DENSITY[[ii]]$doy),FUN=sum)
TS.Cases.POP_DENSITY_AVG[[ii]][,2] <- TS.Cases.POP_DENSITY_AVG[[ii]][,2]/TS.Cases.POP_DENSITY_POP[[ii]][,2]


TS.Cases.WHITE_NH[[ii]] <- subset(TS.Cases, WHITE_NH_q==(ii-1) )
TS.Cases.WHITE_NH_AVG[[ii]]<-aggregate(TS.Cases.WHITE_NH[[ii]]$C_cases,by=list(TS.Cases.WHITE_NH[[ii]]$doy),FUN=sum)
TS.Cases.WHITE_NH_POP[[ii]]<-aggregate(TS.Cases.WHITE_NH[[ii]]$POP,by=list(TS.Cases.WHITE_NH[[ii]]$doy),FUN=sum)
TS.Cases.WHITE_NH_AVG[[ii]][,2] <- TS.Cases.WHITE_NH_AVG[[ii]][,2]/TS.Cases.WHITE_NH_POP[[ii]][,2]

TS.Cases.BLACK_NH[[ii]] <- subset(TS.Cases, BLACK_NH_q==(ii-1) )
TS.Cases.BLACK_NH_AVG[[ii]]<-aggregate(TS.Cases.BLACK_NH[[ii]]$C_cases,by=list(TS.Cases.BLACK_NH[[ii]]$doy),FUN=sum)
TS.Cases.BLACK_NH_POP[[ii]]<-aggregate(TS.Cases.BLACK_NH[[ii]]$POP,by=list(TS.Cases.BLACK_NH[[ii]]$doy),FUN=sum)
TS.Cases.BLACK_NH_AVG[[ii]][,2] <- TS.Cases.BLACK_NH_AVG[[ii]][,2]/TS.Cases.BLACK_NH_POP[[ii]][,2]

TS.Cases.BLACK_H[[ii]] <- subset(TS.Cases, BLACK_H_q==(ii-1) )
TS.Cases.BLACK_H_AVG[[ii]]<-aggregate(TS.Cases.BLACK_H[[ii]]$C_cases,by=list(TS.Cases.BLACK_H[[ii]]$doy),FUN=sum)
TS.Cases.BLACK_H_POP[[ii]]<-aggregate(TS.Cases.BLACK_H[[ii]]$POP,by=list(TS.Cases.BLACK_H[[ii]]$doy),FUN=sum)
TS.Cases.BLACK_H_AVG[[ii]][,2] <- TS.Cases.BLACK_H_AVG[[ii]][,2]/TS.Cases.BLACK_H_POP[[ii]][,2]

TS.Cases.BLACK_ALL[[ii]] <- subset(TS.Cases, BLACK_ALL_q==(ii-1) )
TS.Cases.BLACK_ALL_AVG[[ii]]<-aggregate(TS.Cases.BLACK_ALL[[ii]]$C_cases,by=list(TS.Cases.BLACK_ALL[[ii]]$doy),FUN=sum)
TS.Cases.BLACK_ALL_POP[[ii]]<-aggregate(TS.Cases.BLACK_ALL[[ii]]$POP,by=list(TS.Cases.BLACK_ALL[[ii]]$doy),FUN=sum)
TS.Cases.BLACK_ALL_AVG[[ii]][,2] <- TS.Cases.BLACK_ALL_AVG[[ii]][,2]/TS.Cases.BLACK_ALL_POP[[ii]][,2]

TS.Cases.HISPANIC[[ii]] <- subset(TS.Cases, HISPANIC_q==(ii-1) )
TS.Cases.HISPANIC_AVG[[ii]]<-aggregate(TS.Cases.HISPANIC[[ii]]$C_cases,by=list(TS.Cases.HISPANIC[[ii]]$doy),FUN=sum)
TS.Cases.HISPANIC_POP[[ii]]<-aggregate(TS.Cases.HISPANIC[[ii]]$POP,by=list(TS.Cases.HISPANIC[[ii]]$doy),FUN=sum)
TS.Cases.HISPANIC_AVG[[ii]][,2] <- TS.Cases.HISPANIC_AVG[[ii]][,2]/TS.Cases.HISPANIC_POP[[ii]][,2]

TS.Cases.CORRRATIO[[ii]] <- subset(TS.Cases, CORRRATIO_q==(ii-1) )
TS.Cases.CORRRATIO_AVG[[ii]]<-aggregate(TS.Cases.CORRRATIO[[ii]]$C_cases,by=list(TS.Cases.CORRRATIO[[ii]]$doy),FUN=sum)
TS.Cases.CORRRATIO_POP[[ii]]<-aggregate(TS.Cases.CORRRATIO[[ii]]$POP,by=list(TS.Cases.CORRRATIO[[ii]]$doy),FUN=sum)
TS.Cases.CORRRATIO_AVG[[ii]][,2] <- TS.Cases.CORRRATIO_AVG[[ii]][,2]/TS.Cases.CORRRATIO_POP[[ii]][,2]

TS.Cases.ATKINSON[[ii]] <- subset(TS.Cases, ATKINSON_q==(ii-1) )
TS.Cases.ATKINSON_AVG[[ii]]<-aggregate(TS.Cases.ATKINSON[[ii]]$C_cases,by=list(TS.Cases.ATKINSON[[ii]]$doy),FUN=sum)
TS.Cases.ATKINSON_POP[[ii]]<-aggregate(TS.Cases.ATKINSON[[ii]]$POP,by=list(TS.Cases.ATKINSON[[ii]]$doy),FUN=sum)
TS.Cases.ATKINSON_AVG[[ii]][,2] <- TS.Cases.ATKINSON_AVG[[ii]][,2]/TS.Cases.ATKINSON_POP[[ii]][,2]

TS.Cases.New.AGE_GR1[[ii]] <- subset(TS.Cases, AGE_GR1_q==(ii-1) )
TS.Cases.New.AGE_GR1_AVG[[ii]]<-aggregate(TS.Cases.New.AGE_GR1[[ii]]$cases,by=list(TS.Cases.New.AGE_GR1[[ii]]$doy),FUN=sum)
TS.Cases.New.AGE_GR1_POP[[ii]]<-aggregate(TS.Cases.New.AGE_GR1[[ii]]$POP,by=list(TS.Cases.New.AGE_GR1[[ii]]$doy),FUN=sum)
TS.Cases.New.AGE_GR1_AVG[[ii]][,2] <- TS.Cases.New.AGE_GR1_AVG[[ii]][,2]/TS.Cases.New.AGE_GR1_POP[[ii]][,2]

TS.Cases.New.AGE_GR2[[ii]] <- subset(TS.Cases, AGE_GR2_q==(ii-1) )
TS.Cases.New.AGE_GR2_AVG[[ii]]<-aggregate(TS.Cases.New.AGE_GR2[[ii]]$cases,by=list(TS.Cases.New.AGE_GR2[[ii]]$doy),FUN=sum)
TS.Cases.New.AGE_GR2_POP[[ii]]<-aggregate(TS.Cases.New.AGE_GR2[[ii]]$POP,by=list(TS.Cases.New.AGE_GR2[[ii]]$doy),FUN=sum)
TS.Cases.New.AGE_GR2_AVG[[ii]][,2] <- TS.Cases.New.AGE_GR2_AVG[[ii]][,2]/TS.Cases.New.AGE_GR2_POP[[ii]][,2]

TS.Cases.New.AGE_GR3[[ii]] <- subset(TS.Cases, AGE_GR3_q==(ii-1) )
TS.Cases.New.AGE_GR3_AVG[[ii]]<-aggregate(TS.Cases.New.AGE_GR3[[ii]]$cases,by=list(TS.Cases.New.AGE_GR3[[ii]]$doy),FUN=sum)
TS.Cases.New.AGE_GR3_POP[[ii]]<-aggregate(TS.Cases.New.AGE_GR3[[ii]]$POP,by=list(TS.Cases.New.AGE_GR3[[ii]]$doy),FUN=sum)
TS.Cases.New.AGE_GR3_AVG[[ii]][,2] <- TS.Cases.New.AGE_GR3_AVG[[ii]][,2]/TS.Cases.New.AGE_GR3_POP[[ii]][,2]

TS.Cases.New.AGE_GR4[[ii]] <- subset(TS.Cases, AGE_GR4_q==(ii-1) )
TS.Cases.New.AGE_GR4_AVG[[ii]]<-aggregate(TS.Cases.New.AGE_GR4[[ii]]$cases,by=list(TS.Cases.New.AGE_GR4[[ii]]$doy),FUN=sum)
TS.Cases.New.AGE_GR4_POP[[ii]]<-aggregate(TS.Cases.New.AGE_GR4[[ii]]$POP,by=list(TS.Cases.New.AGE_GR4[[ii]]$doy),FUN=sum)
TS.Cases.New.AGE_GR4_AVG[[ii]][,2] <- TS.Cases.New.AGE_GR4_AVG[[ii]][,2]/TS.Cases.New.AGE_GR4_POP[[ii]][,2]

TS.Cases.New.Poverty[[ii]] <- subset(TS.Cases, Poverty_q==(ii-1) )
TS.Cases.New.Poverty_AVG[[ii]]<-aggregate(TS.Cases.New.Poverty[[ii]]$cases,by=list(TS.Cases.New.Poverty[[ii]]$doy),FUN=sum)
TS.Cases.New.Poverty_POP[[ii]]<-aggregate(TS.Cases.New.Poverty[[ii]]$POP,by=list(TS.Cases.New.Poverty[[ii]]$doy),FUN=sum)
TS.Cases.New.Poverty_AVG[[ii]][,2] <- TS.Cases.New.Poverty_AVG[[ii]][,2]/TS.Cases.New.Poverty_POP[[ii]][,2]

TS.Cases.New.POP_DENSITY[[ii]] <- subset(TS.Cases, POP_DENSITY_q==(ii-1) )
TS.Cases.New.POP_DENSITY_AVG[[ii]]<-aggregate(TS.Cases.New.POP_DENSITY[[ii]]$cases,by=list(TS.Cases.New.POP_DENSITY[[ii]]$doy),FUN=sum)
TS.Cases.New.POP_DENSITY_POP[[ii]]<-aggregate(TS.Cases.New.POP_DENSITY[[ii]]$POP,by=list(TS.Cases.New.POP_DENSITY[[ii]]$doy),FUN=sum)
TS.Cases.New.POP_DENSITY_AVG[[ii]][,2] <- TS.Cases.New.POP_DENSITY_AVG[[ii]][,2]/TS.Cases.New.POP_DENSITY_POP[[ii]][,2]

TS.Cases.New.WHITE_NH[[ii]] <- subset(TS.Cases, WHITE_NH_q==(ii-1) )
TS.Cases.New.WHITE_NH_AVG[[ii]]<-aggregate(TS.Cases.New.WHITE_NH[[ii]]$cases,by=list(TS.Cases.New.WHITE_NH[[ii]]$doy),FUN=sum)
TS.Cases.New.WHITE_NH_POP[[ii]]<-aggregate(TS.Cases.New.WHITE_NH[[ii]]$POP,by=list(TS.Cases.New.WHITE_NH[[ii]]$doy),FUN=sum)
TS.Cases.New.WHITE_NH_AVG[[ii]][,2] <- TS.Cases.New.WHITE_NH_AVG[[ii]][,2]/TS.Cases.New.WHITE_NH_POP[[ii]][,2]


TS.Cases.New.BLACK_NH[[ii]] <- subset(TS.Cases, BLACK_NH_q==(ii-1) )
TS.Cases.New.BLACK_NH_AVG[[ii]]<-aggregate(TS.Cases.New.BLACK_NH[[ii]]$cases,by=list(TS.Cases.New.BLACK_NH[[ii]]$doy),FUN=sum)
TS.Cases.New.BLACK_NH_POP[[ii]]<-aggregate(TS.Cases.New.BLACK_NH[[ii]]$POP,by=list(TS.Cases.New.BLACK_NH[[ii]]$doy),FUN=sum)
TS.Cases.New.BLACK_NH_AVG[[ii]][,2] <- TS.Cases.New.BLACK_NH_AVG[[ii]][,2]/TS.Cases.New.BLACK_NH_POP[[ii]][,2]


TS.Cases.New.BLACK_H[[ii]] <- subset(TS.Cases, BLACK_H_q==(ii-1) )
TS.Cases.New.BLACK_H_AVG[[ii]]<-aggregate(TS.Cases.New.BLACK_H[[ii]]$cases,by=list(TS.Cases.New.BLACK_H[[ii]]$doy),FUN=sum)
TS.Cases.New.BLACK_H_POP[[ii]]<-aggregate(TS.Cases.New.BLACK_H[[ii]]$POP,by=list(TS.Cases.New.BLACK_H[[ii]]$doy),FUN=sum)
TS.Cases.New.BLACK_H_AVG[[ii]][,2] <- TS.Cases.New.BLACK_H_AVG[[ii]][,2]/TS.Cases.New.BLACK_H_POP[[ii]][,2]

TS.Cases.New.BLACK_ALL[[ii]] <- subset(TS.Cases, BLACK_ALL_q==(ii-1) )
TS.Cases.New.BLACK_ALL_AVG[[ii]]<-aggregate(TS.Cases.New.BLACK_ALL[[ii]]$cases,by=list(TS.Cases.New.BLACK_ALL[[ii]]$doy),FUN=sum)
TS.Cases.New.BLACK_ALL_POP[[ii]]<-aggregate(TS.Cases.New.BLACK_ALL[[ii]]$POP,by=list(TS.Cases.New.BLACK_ALL[[ii]]$doy),FUN=sum)
TS.Cases.New.BLACK_ALL_AVG[[ii]][,2] <- TS.Cases.New.BLACK_ALL_AVG[[ii]][,2]/TS.Cases.New.BLACK_ALL_POP[[ii]][,2]

TS.Cases.New.HISPANIC[[ii]] <- subset(TS.Cases, HISPANIC_q==(ii-1) )
TS.Cases.New.HISPANIC_AVG[[ii]]<-aggregate(TS.Cases.New.HISPANIC[[ii]]$cases,by=list(TS.Cases.New.HISPANIC[[ii]]$doy),FUN=sum)
TS.Cases.New.HISPANIC_POP[[ii]]<-aggregate(TS.Cases.New.HISPANIC[[ii]]$POP,by=list(TS.Cases.New.HISPANIC[[ii]]$doy),FUN=sum)
TS.Cases.New.HISPANIC_AVG[[ii]][,2] <- TS.Cases.New.HISPANIC_AVG[[ii]][,2]/TS.Cases.New.HISPANIC_POP[[ii]][,2]

TS.Cases.New.CORRRATIO[[ii]] <- subset(TS.Cases, CORRRATIO_q==(ii-1) )
TS.Cases.New.CORRRATIO_AVG[[ii]]<-aggregate(TS.Cases.New.CORRRATIO[[ii]]$cases,by=list(TS.Cases.New.CORRRATIO[[ii]]$doy),FUN=sum)
TS.Cases.New.CORRRATIO_POP[[ii]]<-aggregate(TS.Cases.New.CORRRATIO[[ii]]$POP,by=list(TS.Cases.New.CORRRATIO[[ii]]$doy),FUN=sum)
TS.Cases.New.CORRRATIO_AVG[[ii]][,2] <- TS.Cases.New.CORRRATIO_AVG[[ii]][,2]/TS.Cases.New.CORRRATIO_POP[[ii]][,2]

TS.Cases.New.ATKINSON[[ii]] <- subset(TS.Cases, ATKINSON_q==(ii-1) )
TS.Cases.New.ATKINSON_AVG[[ii]]<-aggregate(TS.Cases.New.ATKINSON[[ii]]$cases,by=list(TS.Cases.New.ATKINSON[[ii]]$doy),FUN=sum)
TS.Cases.New.ATKINSON_POP[[ii]]<-aggregate(TS.Cases.New.ATKINSON[[ii]]$POP,by=list(TS.Cases.New.ATKINSON[[ii]]$doy),FUN=sum)
TS.Cases.New.ATKINSON_AVG[[ii]][,2] <- TS.Cases.New.ATKINSON_AVG[[ii]][,2]/TS.Cases.New.ATKINSON_POP[[ii]][,2]


TS.Deaths.AGE_GR1[[ii]] <- subset(TS.Cases,AGE_GR1_q==(ii-1) )
TS.Deaths.AGE_GR1_AVG[[ii]]<-aggregate(TS.Deaths.AGE_GR1[[ii]]$C_deaths,by=list(TS.Deaths.AGE_GR1[[ii]]$doy),FUN=sum)
TS.Deaths.AGE_GR1_POP[[ii]]<-aggregate(TS.Deaths.AGE_GR1[[ii]]$POP,by=list(TS.Deaths.AGE_GR1[[ii]]$doy),FUN=sum)
TS.Deaths.AGE_GR1_AVG[[ii]][,2] <- TS.Deaths.AGE_GR1_AVG[[ii]][,2]/TS.Deaths.AGE_GR1_POP[[ii]][,2]

TS.Deaths.AGE_GR2[[ii]] <- subset(TS.Cases,AGE_GR2_q==(ii-1) )
TS.Deaths.AGE_GR2_AVG[[ii]]<-aggregate(TS.Deaths.AGE_GR2[[ii]]$C_deaths,by=list(TS.Deaths.AGE_GR2[[ii]]$doy),FUN=sum)
TS.Deaths.AGE_GR2_POP[[ii]]<-aggregate(TS.Deaths.AGE_GR2[[ii]]$POP,by=list(TS.Deaths.AGE_GR2[[ii]]$doy),FUN=sum)
TS.Deaths.AGE_GR2_AVG[[ii]][,2] <- TS.Deaths.AGE_GR2_AVG[[ii]][,2]/TS.Deaths.AGE_GR2_POP[[ii]][,2]

TS.Deaths.AGE_GR3[[ii]] <- subset(TS.Cases,AGE_GR3_q==(ii-1) )
TS.Deaths.AGE_GR3_AVG[[ii]]<-aggregate(TS.Deaths.AGE_GR3[[ii]]$C_deaths,by=list(TS.Deaths.AGE_GR3[[ii]]$doy),FUN=sum)
TS.Deaths.AGE_GR3_POP[[ii]]<-aggregate(TS.Deaths.AGE_GR3[[ii]]$POP,by=list(TS.Deaths.AGE_GR3[[ii]]$doy),FUN=sum)
TS.Deaths.AGE_GR3_AVG[[ii]][,2] <- TS.Deaths.AGE_GR3_AVG[[ii]][,2]/TS.Deaths.AGE_GR3_POP[[ii]][,2]

TS.Deaths.AGE_GR4[[ii]] <- subset(TS.Cases,AGE_GR4_q==(ii-1) )
TS.Deaths.AGE_GR4_AVG[[ii]]<-aggregate(TS.Deaths.AGE_GR4[[ii]]$C_deaths,by=list(TS.Deaths.AGE_GR4[[ii]]$doy),FUN=sum)
TS.Deaths.AGE_GR4_POP[[ii]]<-aggregate(TS.Deaths.AGE_GR4[[ii]]$POP,by=list(TS.Deaths.AGE_GR4[[ii]]$doy),FUN=sum)
TS.Deaths.AGE_GR4_AVG[[ii]][,2] <- TS.Deaths.AGE_GR4_AVG[[ii]][,2]/TS.Deaths.AGE_GR4_POP[[ii]][,2]

TS.Deaths.Poverty[[ii]] <- subset(TS.Cases,Poverty_q==(ii-1) )
TS.Deaths.Poverty_AVG[[ii]]<-aggregate(TS.Deaths.Poverty[[ii]]$C_deaths,by=list(TS.Deaths.Poverty[[ii]]$doy),FUN=sum)
TS.Deaths.Poverty_POP[[ii]]<-aggregate(TS.Deaths.Poverty[[ii]]$POP,by=list(TS.Deaths.Poverty[[ii]]$doy),FUN=sum)
TS.Deaths.Poverty_AVG[[ii]][,2] <- TS.Deaths.Poverty_AVG[[ii]][,2]/TS.Deaths.Poverty_POP[[ii]][,2]

TS.Deaths.POP_DENSITY[[ii]] <- subset(TS.Cases,POP_DENSITY_q==(ii-1) )
TS.Deaths.POP_DENSITY_AVG[[ii]]<-aggregate(TS.Deaths.POP_DENSITY[[ii]]$C_deaths,by=list(TS.Deaths.POP_DENSITY[[ii]]$doy),FUN=sum)
TS.Deaths.POP_DENSITY_POP[[ii]]<-aggregate(TS.Deaths.POP_DENSITY[[ii]]$POP,by=list(TS.Deaths.POP_DENSITY[[ii]]$doy),FUN=sum)
TS.Deaths.POP_DENSITY_AVG[[ii]][,2] <- TS.Deaths.POP_DENSITY_AVG[[ii]][,2]/TS.Deaths.POP_DENSITY_POP[[ii]][,2]


TS.Deaths.WHITE_NH[[ii]] <- subset(TS.Cases,WHITE_NH_q==(ii-1) )
TS.Deaths.WHITE_NH_AVG[[ii]]<-aggregate(TS.Deaths.WHITE_NH[[ii]]$C_deaths,by=list(TS.Deaths.WHITE_NH[[ii]]$doy),FUN=sum)
TS.Deaths.WHITE_NH_POP[[ii]]<-aggregate(TS.Deaths.WHITE_NH[[ii]]$POP,by=list(TS.Deaths.WHITE_NH[[ii]]$doy),FUN=sum)
TS.Deaths.WHITE_NH_AVG[[ii]][,2] <- TS.Deaths.WHITE_NH_AVG[[ii]][,2]/TS.Deaths.WHITE_NH_POP[[ii]][,2]

TS.Deaths.BLACK_NH[[ii]] <- subset(TS.Cases,BLACK_NH_q==(ii-1) )
TS.Deaths.BLACK_NH_AVG[[ii]]<-aggregate(TS.Deaths.BLACK_NH[[ii]]$C_deaths,by=list(TS.Deaths.BLACK_NH[[ii]]$doy),FUN=sum)
TS.Deaths.BLACK_NH_POP[[ii]]<-aggregate(TS.Deaths.BLACK_NH[[ii]]$POP,by=list(TS.Deaths.BLACK_NH[[ii]]$doy),FUN=sum)
TS.Deaths.BLACK_NH_AVG[[ii]][,2] <- TS.Deaths.BLACK_NH_AVG[[ii]][,2]/TS.Deaths.BLACK_NH_POP[[ii]][,2]

TS.Deaths.BLACK_H[[ii]] <- subset(TS.Cases,BLACK_H_q==(ii-1) )
TS.Deaths.BLACK_H_AVG[[ii]]<-aggregate(TS.Deaths.BLACK_H[[ii]]$C_deaths,by=list(TS.Deaths.BLACK_H[[ii]]$doy),FUN=sum)
TS.Deaths.BLACK_H_POP[[ii]]<-aggregate(TS.Deaths.BLACK_H[[ii]]$POP,by=list(TS.Deaths.BLACK_H[[ii]]$doy),FUN=sum)
TS.Deaths.BLACK_H_AVG[[ii]][,2] <- TS.Deaths.BLACK_H_AVG[[ii]][,2]/TS.Deaths.BLACK_H_POP[[ii]][,2]

TS.Deaths.BLACK_ALL[[ii]] <- subset(TS.Cases,BLACK_ALL_q==(ii-1) )
TS.Deaths.BLACK_ALL_AVG[[ii]]<-aggregate(TS.Deaths.BLACK_ALL[[ii]]$C_deaths,by=list(TS.Deaths.BLACK_ALL[[ii]]$doy),FUN=sum)
TS.Deaths.BLACK_ALL_POP[[ii]]<-aggregate(TS.Deaths.BLACK_ALL[[ii]]$POP,by=list(TS.Deaths.BLACK_ALL[[ii]]$doy),FUN=sum)
TS.Deaths.BLACK_ALL_AVG[[ii]][,2] <- TS.Deaths.BLACK_ALL_AVG[[ii]][,2]/TS.Deaths.BLACK_ALL_POP[[ii]][,2]

TS.Deaths.HISPANIC[[ii]] <- subset(TS.Cases,HISPANIC_q==(ii-1) )
TS.Deaths.HISPANIC_AVG[[ii]]<-aggregate(TS.Deaths.HISPANIC[[ii]]$C_deaths,by=list(TS.Deaths.HISPANIC[[ii]]$doy),FUN=sum)
TS.Deaths.HISPANIC_POP[[ii]]<-aggregate(TS.Deaths.HISPANIC[[ii]]$POP,by=list(TS.Deaths.HISPANIC[[ii]]$doy),FUN=sum)
TS.Deaths.HISPANIC_AVG[[ii]][,2] <- TS.Deaths.HISPANIC_AVG[[ii]][,2]/TS.Deaths.HISPANIC_POP[[ii]][,2]

TS.Deaths.CORRRATIO[[ii]] <- subset(TS.Cases,CORRRATIO_q==(ii-1) )
TS.Deaths.CORRRATIO_AVG[[ii]]<-aggregate(TS.Deaths.CORRRATIO[[ii]]$C_deaths,by=list(TS.Deaths.CORRRATIO[[ii]]$doy),FUN=sum)
TS.Deaths.CORRRATIO_POP[[ii]]<-aggregate(TS.Deaths.CORRRATIO[[ii]]$POP,by=list(TS.Deaths.CORRRATIO[[ii]]$doy),FUN=sum)
TS.Deaths.CORRRATIO_AVG[[ii]][,2] <- TS.Deaths.CORRRATIO_AVG[[ii]][,2]/TS.Deaths.CORRRATIO_POP[[ii]][,2]

TS.Deaths.ATKINSON[[ii]] <- subset(TS.Cases,ATKINSON_q==(ii-1) )
TS.Deaths.ATKINSON_AVG[[ii]]<-aggregate(TS.Deaths.ATKINSON[[ii]]$C_deaths,by=list(TS.Deaths.ATKINSON[[ii]]$doy),FUN=sum)
TS.Deaths.ATKINSON_POP[[ii]]<-aggregate(TS.Deaths.ATKINSON[[ii]]$POP,by=list(TS.Deaths.ATKINSON[[ii]]$doy),FUN=sum)
TS.Deaths.ATKINSON_AVG[[ii]][,2] <- TS.Deaths.ATKINSON_AVG[[ii]][,2]/TS.Deaths.ATKINSON_POP[[ii]][,2]


TS.Deaths.New.AGE_GR1[[ii]] <- subset(TS.Cases,AGE_GR1_q==(ii-1) )
TS.Deaths.New.AGE_GR1_AVG[[ii]]<-aggregate(TS.Deaths.New.AGE_GR1[[ii]]$deaths,by=list(TS.Deaths.New.AGE_GR1[[ii]]$doy),FUN=sum)
TS.Deaths.New.AGE_GR1_POP[[ii]]<-aggregate(TS.Deaths.New.AGE_GR1[[ii]]$POP,by=list(TS.Deaths.New.AGE_GR1[[ii]]$doy),FUN=sum)
TS.Deaths.New.AGE_GR1_AVG[[ii]][,2] <- TS.Deaths.New.AGE_GR1_AVG[[ii]][,2]/TS.Deaths.New.AGE_GR1_POP[[ii]][,2]

TS.Deaths.New.AGE_GR2[[ii]] <- subset(TS.Cases,AGE_GR2_q==(ii-1) )
TS.Deaths.New.AGE_GR2_AVG[[ii]]<-aggregate(TS.Deaths.New.AGE_GR2[[ii]]$deaths,by=list(TS.Deaths.New.AGE_GR2[[ii]]$doy),FUN=sum)
TS.Deaths.New.AGE_GR2_POP[[ii]]<-aggregate(TS.Deaths.New.AGE_GR2[[ii]]$POP,by=list(TS.Deaths.New.AGE_GR2[[ii]]$doy),FUN=sum)
TS.Deaths.New.AGE_GR2_AVG[[ii]][,2] <- TS.Deaths.New.AGE_GR2_AVG[[ii]][,2]/TS.Deaths.New.AGE_GR2_POP[[ii]][,2]

TS.Deaths.New.AGE_GR3[[ii]] <- subset(TS.Cases,AGE_GR3_q==(ii-1) )
TS.Deaths.New.AGE_GR3_AVG[[ii]]<-aggregate(TS.Deaths.New.AGE_GR3[[ii]]$deaths,by=list(TS.Deaths.New.AGE_GR3[[ii]]$doy),FUN=sum)
TS.Deaths.New.AGE_GR3_POP[[ii]]<-aggregate(TS.Deaths.New.AGE_GR3[[ii]]$POP,by=list(TS.Deaths.New.AGE_GR3[[ii]]$doy),FUN=sum)
TS.Deaths.New.AGE_GR3_AVG[[ii]][,2] <- TS.Deaths.New.AGE_GR3_AVG[[ii]][,2]/TS.Deaths.New.AGE_GR3_POP[[ii]][,2]

TS.Deaths.New.AGE_GR4[[ii]] <- subset(TS.Cases,AGE_GR4_q==(ii-1) )
TS.Deaths.New.AGE_GR4_AVG[[ii]]<-aggregate(TS.Deaths.New.AGE_GR4[[ii]]$deaths,by=list(TS.Deaths.New.AGE_GR4[[ii]]$doy),FUN=sum)
TS.Deaths.New.AGE_GR4_POP[[ii]]<-aggregate(TS.Deaths.New.AGE_GR4[[ii]]$POP,by=list(TS.Deaths.New.AGE_GR4[[ii]]$doy),FUN=sum)
TS.Deaths.New.AGE_GR4_AVG[[ii]][,2] <- TS.Deaths.New.AGE_GR4_AVG[[ii]][,2]/TS.Deaths.New.AGE_GR4_POP[[ii]][,2]

TS.Deaths.New.Poverty[[ii]] <- subset(TS.Cases,Poverty_q==(ii-1) )
TS.Deaths.New.Poverty_AVG[[ii]]<-aggregate(TS.Deaths.New.Poverty[[ii]]$deaths,by=list(TS.Deaths.New.Poverty[[ii]]$doy),FUN=sum)
TS.Deaths.New.Poverty_POP[[ii]]<-aggregate(TS.Deaths.New.Poverty[[ii]]$POP,by=list(TS.Deaths.New.Poverty[[ii]]$doy),FUN=sum)
TS.Deaths.New.Poverty_AVG[[ii]][,2] <- TS.Deaths.New.Poverty_AVG[[ii]][,2]/TS.Deaths.New.Poverty_POP[[ii]][,2]

TS.Deaths.New.POP_DENSITY[[ii]] <- subset(TS.Cases,POP_DENSITY_q==(ii-1) )
TS.Deaths.New.POP_DENSITY_AVG[[ii]]<-aggregate(TS.Deaths.New.POP_DENSITY[[ii]]$deaths,by=list(TS.Deaths.New.POP_DENSITY[[ii]]$doy),FUN=sum)
TS.Deaths.New.POP_DENSITY_POP[[ii]]<-aggregate(TS.Deaths.New.POP_DENSITY[[ii]]$POP,by=list(TS.Deaths.New.POP_DENSITY[[ii]]$doy),FUN=sum)
TS.Deaths.New.POP_DENSITY_AVG[[ii]][,2] <- TS.Deaths.New.POP_DENSITY_AVG[[ii]][,2]/TS.Deaths.New.POP_DENSITY_POP[[ii]][,2]

TS.Deaths.New.WHITE_NH[[ii]] <- subset(TS.Cases,WHITE_NH_q==(ii-1) )
TS.Deaths.New.WHITE_NH_AVG[[ii]]<-aggregate(TS.Deaths.New.WHITE_NH[[ii]]$deaths,by=list(TS.Deaths.New.WHITE_NH[[ii]]$doy),FUN=sum)
TS.Deaths.New.WHITE_NH_POP[[ii]]<-aggregate(TS.Deaths.New.WHITE_NH[[ii]]$POP,by=list(TS.Deaths.New.WHITE_NH[[ii]]$doy),FUN=sum)
TS.Deaths.New.WHITE_NH_AVG[[ii]][,2] <- TS.Deaths.New.WHITE_NH_AVG[[ii]][,2]/TS.Deaths.New.WHITE_NH_POP[[ii]][,2]


TS.Deaths.New.BLACK_NH[[ii]] <- subset(TS.Cases,BLACK_NH_q==(ii-1) )
TS.Deaths.New.BLACK_NH_AVG[[ii]]<-aggregate(TS.Deaths.New.BLACK_NH[[ii]]$deaths,by=list(TS.Deaths.New.BLACK_NH[[ii]]$doy),FUN=sum)
TS.Deaths.New.BLACK_NH_POP[[ii]]<-aggregate(TS.Deaths.New.BLACK_NH[[ii]]$POP,by=list(TS.Deaths.New.BLACK_NH[[ii]]$doy),FUN=sum)
TS.Deaths.New.BLACK_NH_AVG[[ii]][,2] <- TS.Deaths.New.BLACK_NH_AVG[[ii]][,2]/TS.Deaths.New.BLACK_NH_POP[[ii]][,2]


TS.Deaths.New.BLACK_H[[ii]] <- subset(TS.Cases,BLACK_H_q==(ii-1) )
TS.Deaths.New.BLACK_H_AVG[[ii]]<-aggregate(TS.Deaths.New.BLACK_H[[ii]]$deaths,by=list(TS.Deaths.New.BLACK_H[[ii]]$doy),FUN=sum)
TS.Deaths.New.BLACK_H_POP[[ii]]<-aggregate(TS.Deaths.New.BLACK_H[[ii]]$POP,by=list(TS.Deaths.New.BLACK_H[[ii]]$doy),FUN=sum)
TS.Deaths.New.BLACK_H_AVG[[ii]][,2] <- TS.Deaths.New.BLACK_H_AVG[[ii]][,2]/TS.Deaths.New.BLACK_H_POP[[ii]][,2]

TS.Deaths.New.BLACK_ALL[[ii]] <- subset(TS.Cases,BLACK_ALL_q==(ii-1) )
TS.Deaths.New.BLACK_ALL_AVG[[ii]]<-aggregate(TS.Deaths.New.BLACK_ALL[[ii]]$deaths,by=list(TS.Deaths.New.BLACK_ALL[[ii]]$doy),FUN=sum)
TS.Deaths.New.BLACK_ALL_POP[[ii]]<-aggregate(TS.Deaths.New.BLACK_ALL[[ii]]$POP,by=list(TS.Deaths.New.BLACK_ALL[[ii]]$doy),FUN=sum)
TS.Deaths.New.BLACK_ALL_AVG[[ii]][,2] <- TS.Deaths.New.BLACK_ALL_AVG[[ii]][,2]/TS.Deaths.New.BLACK_ALL_POP[[ii]][,2]

TS.Deaths.New.HISPANIC[[ii]] <- subset(TS.Cases,HISPANIC_q==(ii-1) )
TS.Deaths.New.HISPANIC_AVG[[ii]]<-aggregate(TS.Deaths.New.HISPANIC[[ii]]$deaths,by=list(TS.Deaths.New.HISPANIC[[ii]]$doy),FUN=sum)
TS.Deaths.New.HISPANIC_POP[[ii]]<-aggregate(TS.Deaths.New.HISPANIC[[ii]]$POP,by=list(TS.Deaths.New.HISPANIC[[ii]]$doy),FUN=sum)
TS.Deaths.New.HISPANIC_AVG[[ii]][,2] <- TS.Deaths.New.HISPANIC_AVG[[ii]][,2]/TS.Deaths.New.HISPANIC_POP[[ii]][,2]

TS.Deaths.New.CORRRATIO[[ii]] <- subset(TS.Cases,CORRRATIO_q==(ii-1) )
TS.Deaths.New.CORRRATIO_AVG[[ii]]<-aggregate(TS.Deaths.New.CORRRATIO[[ii]]$deaths,by=list(TS.Deaths.New.CORRRATIO[[ii]]$doy),FUN=sum)
TS.Deaths.New.CORRRATIO_POP[[ii]]<-aggregate(TS.Deaths.New.CORRRATIO[[ii]]$POP,by=list(TS.Deaths.New.CORRRATIO[[ii]]$doy),FUN=sum)
TS.Deaths.New.CORRRATIO_AVG[[ii]][,2] <- TS.Deaths.New.CORRRATIO_AVG[[ii]][,2]/TS.Deaths.New.CORRRATIO_POP[[ii]][,2]

TS.Deaths.New.ATKINSON[[ii]] <- subset(TS.Cases,ATKINSON_q==(ii-1) )
TS.Deaths.New.ATKINSON_AVG[[ii]]<-aggregate(TS.Deaths.New.ATKINSON[[ii]]$deaths,by=list(TS.Deaths.New.ATKINSON[[ii]]$doy),FUN=sum)
TS.Deaths.New.ATKINSON_POP[[ii]]<-aggregate(TS.Deaths.New.ATKINSON[[ii]]$POP,by=list(TS.Deaths.New.ATKINSON[[ii]]$doy),FUN=sum)
TS.Deaths.New.ATKINSON_AVG[[ii]][,2] <- TS.Deaths.New.ATKINSON_AVG[[ii]][,2]/TS.Deaths.New.ATKINSON_POP[[ii]][,2]

}




TS.Cases.Cumulative.All<-aggregate(TS.Cases$C_cases,by=list(TS.Cases$doy),FUN=sum)
TS.Cases.Cumulative.POP<-aggregate(TS.Cases$POP,by=list(TS.Cases$doy),FUN=sum)
TS.Cases.Cumulative.All[,2]<-TS.Cases.Cumulative.All[,2]/TS.Cases.Cumulative.POP[,2]

TS.Deaths.Cumulative.All<-aggregate(TS.Cases$C_deaths,by=list(TS.Cases$doy),FUN=sum)
TS.Deaths.Cumulative.POP<-aggregate(TS.Cases$POP,by=list(TS.Cases$doy),FUN=sum)
TS.Deaths.Cumulative.All[,2]<-TS.Deaths.Cumulative.All[,2]/TS.Deaths.Cumulative.POP[,2]

TS.Cases.New.All<-aggregate(TS.Cases$cases,by=list(TS.Cases$doy),FUN=sum)
TS.Cases.New.POP<-aggregate(TS.Cases$POP,by=list(TS.Cases$doy),FUN=sum)
TS.Cases.New.All[,2]<-TS.Cases.New.All[,2]/TS.Cases.New.POP[,2]

TS.Deaths.New.All<-aggregate(TS.Cases$deaths,by=list(TS.Cases$doy),FUN=sum)
TS.Deaths.New.POP<-aggregate(TS.Cases$POP,by=list(TS.Cases$doy),FUN=sum)
TS.Deaths.New.All[,2]<-TS.Deaths.New.All[,2]/TS.Deaths.New.POP[,2]



jpeg("C:\\Users\\hongh\\Dropbox (Yale_FES)\\COVID19\\USCOVID\\Race_COVID\\Manuscript\\Figures\\Figure1_TSplot.jpg",width=12,height=4,units="in",res=900,pointsize=10)
par(mfrow=c(1,2))
plot(x=X.Time,y=TS.Cases.New.All[,2]*100000,col="navy",type="l", ylab="Cumulative cases (per 100,000)",xlab="Date",ylim=c(0,100),lwd=2,bty="n",xaxt="n")
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("A. COVID-19 cases", x=as.Date("2019/12/10"),y=115,xpd=T,cex=1.3,adj=0)

plot(x=X.Time,y=TS.Deaths.New.All[,2]*100000,col="navy",type="l", ylab="Cumulative deaths (per 100,000)",xlab="Date",ylim=c(0,1.5),lwd=2,bty="n",xaxt="n")
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("B. COVID-19 deaths", x=as.Date("2019/12/10"),y=1.7,xpd=T,cex=1.3,adj=0)
dev.off()



pal <- colorRampPalette(c('plum2','khaki4', 'navy'))

X.Time <- seq(as.Date("2020-01-21",origin="1970-01-01",format="%Y-%m-%d"), as.Date("2020-12-18",origin="1970-01-01",format="%Y-%m-%d"),by=1)

###Figure 2
jpeg("C:\\Users\\hongh\\Dropbox (Yale_FES)\\COVID19\\USCOVID\\Race_COVID\\Manuscript\\Figures\\Figure2.jpg",width=16,height=20,units="in",res=900,pointsize=20)
par(mfrow=c(6,2),mar=c(4,4,3.5,1),mgp=c(1.9,0.6,0),las=1)
plot(x=X.Time,y=TS.Cases.New.POP_DENSITY_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative cases \n (per 100,000)",xlab="Date",ylim=c(0,250),lwd=2,bty="n",xaxt="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Cases.New.POP_DENSITY_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
legend("topleft",bty="n",paste0("Q",seq(1,5)),col=pal(5),lwd=2)
text("A. Population density and COVID-19 cases", x=as.Date("2019/11/27"),y=300,xpd=T,cex=1.3,adj=0)

plot(x=X.Time,y=TS.Deaths.New.POP_DENSITY_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative deaths \n  (per 100,000)",xlab="Date",ylim=c(0,4),lwd=2,bty="n",xaxt="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Deaths.New.POP_DENSITY_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("B. Population density and COVID-19 deaths", x=as.Date("2019/11/27"),y=4.77,xpd=T,cex=1.3,adj=0)


###WHITE_NH
plot(x=X.Time,y=TS.Cases.New.WHITE_NH_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative cases \n  (per 100,000)",xlab="Date",ylim=c(0,100),lwd=2,bty="n",xaxt="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Cases.New.WHITE_NH_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("C. Non-Hispanic White and COVID-19 cases", x=as.Date("2019/11/27"),y=120,xpd=T,cex=1.3,adj=0)



plot(x=X.Time,y=TS.Deaths.New.WHITE_NH_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative deaths \n  (per 100,000)",xlab="Date",ylim=c(0,4),lwd=2,bty="n",xaxt="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Deaths.New.WHITE_NH_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("D. Non-Hispanic White and COVID-19 deaths", x=as.Date("2019/11/27"),y=4.82,xpd=T,cex=1.3,adj=0)


###BLACK_ALL
plot(x=X.Time,y=TS.Cases.New.BLACK_ALL_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative cases \n  (per 100,000)",xlab="Date",ylim=c(0,100),lwd=2,bty="n",xaxt="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Cases.New.BLACK_ALL_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("E. Black/African American and COVID-19 cases", x=as.Date("2019/11/27"),y=120,xpd=T,cex=1.3,adj=0)

plot(x=X.Time,y=TS.Deaths.New.BLACK_ALL_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative deaths \n  (per 100,000)",xlab="Date",ylim=c(0,4),lwd=2,bty="n",xaxt="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Deaths.New.BLACK_ALL_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("F. Black/African American and COVID-19 deaths", x=as.Date("2019/11/27"),y=4.82,xpd=T,cex=1.3,adj=0)



###HISPANIC
plot(x=X.Time,y=TS.Cases.New.HISPANIC_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative cases \n  (per 100,000)",xlab="Date",ylim=c(0,100),lwd=2,bty="n",xaxt="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Cases.New.HISPANIC_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("G. Hispanic and COVID-19 cases", x=as.Date("2019/11/27"),y=120,xpd=T,cex=1.3,adj=0)

plot(x=X.Time,y=TS.Deaths.New.HISPANIC_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative deaths \n  (per 100,000)",xlab="Date",ylim=c(0,4),lwd=2,bty="n",xaxt="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Deaths.New.HISPANIC_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("H. Hispanic and COVID-19 deaths", x=as.Date("2019/11/27"),y=4.82,xpd=T,cex=1.3,adj=0)

###ATKINSON
plot(x=X.Time,y=TS.Cases.New.ATKINSON_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative cases \n  (per 100,000)",xlab="Date",ylim=c(0,200),lwd=2,xaxt="n",bty="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Cases.New.ATKINSON_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("I. Atkinson index and COVID-19 cases", x=as.Date("2019/11/27"),y=245,xpd=T,cex=1.3,adj=0)

plot(x=X.Time,y=TS.Deaths.New.ATKINSON_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative deaths \n  (per 100,000)",xlab="Date",ylim=c(0,4),lwd=2,xaxt="n",bty="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Deaths.New.ATKINSON_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("J. Atkinson index and COVID-19 deaths", x=as.Date("2019/11/27"),y=4.87,xpd=T,cex=1.3,adj=0)

###CORRRATIO
plot(x=X.Time,y=TS.Cases.New.CORRRATIO_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative cases \n  (per 100,000)",xlab="Date",ylim=c(0,200),lwd=2,xaxt="n",bty="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Cases.New.CORRRATIO_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("K. Eta-squared and COVID-19 cases", x=as.Date("2019/11/27"),y=245,xpd=T,cex=1.3,adj=0)

plot(x=X.Time,y=TS.Deaths.New.CORRRATIO_AVG[[5]][,2]*100000,col=pal(5)[5],type="l", ylab="Cumulative deaths \n  (per 100,000)",xlab="Date",ylim=c(0,4),lwd=2,xaxt="n",bty="n")
for (ii in seq(4)) {
  lines(x=X.Time,y=TS.Deaths.New.CORRRATIO_AVG[[ii]][,2]*100000,col=pal(5)[ii],lwd=2)
}
axis.Date(1,at=seq(as.Date("2020/1/21"),as.Date("2020/12/18"),"weeks"), format = "%m-%d")
text("L. Eta-squared and COVID-19 deaths", x=as.Date("2019/11/27"),y=4.87,xpd=T,cex=1.3,adj=0)

dev.off()




