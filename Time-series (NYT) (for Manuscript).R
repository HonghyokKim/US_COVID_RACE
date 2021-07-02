
jpeg("PATHNAME\\FigureS1_TSplot.jpg",width=12,height=4,units="in",res=900,pointsize=10)
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

###Figure 1
jpeg("PATHNAME\\Figure1.jpg",width=16,height=20,units="in",res=900,pointsize=20)
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




