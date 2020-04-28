
#####  FUNC: PLOT FITS  #######
plotfits <- function(SIR.unique, lengthsim){
  for(i in (1:nrow(SIR.unique))){
    assign(paste("plot", i, sep = ""), 
           epid(eta=SIR.unique$eta[i],alpha=SIR.unique$alpha[i],
                delta=SIR.unique$delta[i],mu=SIR.unique$mu[i],gamma1=SIR.unique$gamma1[i],
                gamma2=SIR.unique$gamma2[i],gamma3=SIR.unique$gamma3[i],gamma4=SIR.unique$gamma4[i],
                gamma5=SIR.unique$gamma5[i],gamma6=SIR.unique$gamma6[i],
                gamma7=SIR.unique$gamma7[i], psi=SIR.unique$psi[i],chi=SIR.unique$chi[i],betaA=SIR.unique$betaA[i],betaI=SIR.unique$betaI[i],
                homeschool=SIR.unique$homeschool[i],lockdown=SIR.unique$lockdown[i],
                distancing=SIR.unique$distancing[i],employment=SIR.unique$employment[i],initialinfected = SIR.unique$initialinfected[i], gamma8 = SIR.unique$gamma8[i], gamma9 = SIR.unique$gamma9[i], 
                UR1 = SIR.unique$UR1[i],  # UR2 = SIR.unique$UR2[i],   UR3 = SIR.unique$UR3  ,UR4 = SIR.unique$UR4[i], 
                w=SIR.unique$w[i],yprop=SIR.unique$yprop[i],rgnonIC=SIR.unique$rgnonIC[i],
                b=SIR.unique$b[i],omega1=SIR.unique$omega1[i],
                omega2=SIR.unique$omega2[i],omega3=SIR.unique$omega3[i],omega4=SIR.unique$omega4[i],
                omega5=SIR.unique$omega5[i],omega6=SIR.unique$omega6[i],
                omega7=SIR.unique$omega7[i],omega8=SIR.unique$omega8[i],
                omega9=SIR.unique$omega9[i],
                kappa=SIR.unique$kappa[i],
                rho=SIR.unique$rho[i],phi=SIR.unique$phi[i],
                epid.duration = lengthsim, #length of simulation
                func.indicator="returnout"))}
  
  #print(plot1)
  datanew.cases.entering.hospital.cumsum=data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    datanew.cases.entering.hospital.cumsum[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$new.cases.entering.hospital.cumsum 
  }
  datatemp <- datanew.cases.entering.hospital.cumsum[,2:ncol(datanew.cases.entering.hospital.cumsum)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  datatemp$lower2 <- apply(  datatemp ,1,quantile,probs=c(0.25),na.rm=TRUE)
  datatemp$upper2 <- apply(  datatemp ,1,quantile,probs=c(0.75),na.rm=TRUE)
  datanew.cases.entering.hospital.cumsum$median<- datatemp$median
  datanew.cases.entering.hospital.cumsum$lower<- datatemp$lower
  datanew.cases.entering.hospital.cumsum$upper<- datatemp$upper
  datanew.cases.entering.hospital.cumsum$lower2<- datatemp$lower2
  datanew.cases.entering.hospital.cumsum$upper2<- datatemp$upper2
  
  datanew.cases.entering.IC=data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    datanew.cases.entering.IC[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$new.cases.entering.IC
  }
  datatemp <- datanew.cases.entering.IC[,2:ncol(datanew.cases.entering.IC)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  datatemp$lower2 <- apply(  datatemp ,1,quantile,probs=c(0.25),na.rm=TRUE)
  datatemp$upper2 <- apply(  datatemp ,1,quantile,probs=c(0.75),na.rm=TRUE)
  datanew.cases.entering.IC$median<- datatemp$median
  datanew.cases.entering.IC$lower<- datatemp$lower
  datanew.cases.entering.IC$upper<- datatemp$upper
  datanew.cases.entering.IC$lower2<- datatemp$lower2
  datanew.cases.entering.IC$upper2<- datatemp$upper2
  
  dataD=data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataD[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$D
  }
  datatemp <- dataD[,2:ncol(dataD)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  datatemp$lower2 <- apply(  datatemp ,1,quantile,probs=c(0.25),na.rm=TRUE)
  datatemp$upper2 <- apply(  datatemp ,1,quantile,probs=c(0.75),na.rm=TRUE)
  dataD$median<- datatemp$median
  dataD$lower<- datatemp$lower
  dataD$upper<- datatemp$upper
  dataD$lower2<- datatemp$lower2
  dataD$upper2<- datatemp$upper2
  dataDcom <- data.frame("Date"=dataD$Date,"Dead"=dataD$median)
  dataDcom<- gather(dataDcom, key = "variable", value = "value",
                    Dead)
  
  dataIC=data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataIC[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$C #+ eval(parse( text=paste("plot", i, sep = "") ))$B
  }
  datatemp <- dataIC[,2:ncol(dataIC)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  datatemp$lower2 <- apply(  datatemp ,1,quantile,probs=c(0.25),na.rm=TRUE)
  datatemp$upper2 <- apply(  datatemp ,1,quantile,probs=c(0.75),na.rm=TRUE)
  dataIC$median<- datatemp$median
  dataIC$lower<- datatemp$lower
  dataIC$upper<- datatemp$upper
  dataIC$lower2<- datatemp$lower2
  dataIC$upper2<- datatemp$upper2
  
  databeds=data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    databeds[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$H #+ eval(parse( text=paste("plot", i, sep = "") ))$W
  }
  datatemp <- databeds[,2:ncol(databeds)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  datatemp$lower2 <- apply(  datatemp ,1,quantile,probs=c(0.25),na.rm=TRUE)
  datatemp$upper2 <- apply(  datatemp ,1,quantile,probs=c(0.75),na.rm=TRUE)
  databeds$median<- datatemp$median
  databeds$lower<- datatemp$lower
  databeds$upper<- datatemp$upper
  databeds$lower2<- datatemp$lower2
  databeds$upper2<- datatemp$upper2
  
  datanewcases=data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    datanewcases[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$new.cases.entering.hospital
  }
  datatemp <- datanewcases[,2:ncol(datanewcases)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  datanewcases$median<- datatemp$median
  datanewcases$lower<- datatemp$lower
  datanewcases$upper<- datatemp$upper
  
  dataI =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataI[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$I
  }
  datatemp <- dataI[,2:ncol(dataI)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataI$median<- datatemp$median
  dataI$lower<- datatemp$lower
  dataI$upper<- datatemp$upper
  
  dataA =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataA[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$A
  }
  datatemp <- dataA[,2:ncol(dataA)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataA$median<- datatemp$median
  dataA$lower<- datatemp$lower
  dataA$upper<- datatemp$upper
  
  datacom <- data.frame("Date"=dataI$Date,"Symptomatic"=dataI$median,"Asymptomatic"=dataA$median)
  datacom<- gather(datacom, key = "variable", value = "value",
                   Symptomatic, Asymptomatic)
  
  dataSP =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataSP[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$S + eval(parse( text=paste("plot", i, sep = "") ))$P
  }
  datatemp <- dataSP[,2:ncol(dataSP)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  dataSP$median<- datatemp$median
  
  dataE =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataE[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$E 
  }
  datatemp <- dataE[,2:ncol(dataE)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataE$median<- datatemp$median
  dataE$lower<- datatemp$lower
  dataE$upper<- datatemp$upper
  
  dataAI =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataAI[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$A +eval(parse( text=paste("plot", i, sep = "") ))$I
  }
  datatemp <- dataAI[,2:ncol(dataAI)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataAI$median<- datatemp$median
  dataAI$lower<- datatemp$lower
  dataAI$upper<- datatemp$upper
  
  
  
  dataHWCB =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataHWCB[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$H +
      #eval(parse( text=paste("plot", i, sep = "") ))$W+ 
      eval(parse( text=paste("plot", i, sep = "") ))$C #+eval(parse( text=paste("plot", i, sep = "") ))$B
  }
  datatemp <- dataHWCB[,2:ncol(dataHWCB)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  dataHWCB$median<- datatemp$median
  
  dataR =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataR[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$R 
  }
  datatemp <- dataR[,2:ncol(dataR)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataR$median<- datatemp$median
  dataR$lower<- datatemp$lower
  dataR$upper<- datatemp$upper
  
  datarecovering.in.IC =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    datarecovering.in.IC[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$recovering.in.IC
  }
  datatemp <- datarecovering.in.IC[,2:ncol(datarecovering.in.IC)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datarecovering.in.IC$median<- datatemp$median
  
  datadying.in.IC =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    datadying.in.IC[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$dying.in.IC
  }
  datatemp <- datadying.in.IC[,2:ncol(datadying.in.IC)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datadying.in.IC$median<- datatemp$median
  
  datarecovering.in.nonIC =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    datarecovering.in.nonIC[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$recovering.in.nonIC
  }
  datatemp <- datarecovering.in.nonIC[,2:ncol(datarecovering.in.nonIC)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datarecovering.in.nonIC$median<- datatemp$median
  
  datadying.in.nonIC =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    datadying.in.nonIC[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$dying.in.nonIC
  }
  datatemp <- datadying.in.nonIC[,2:ncol(datadying.in.nonIC)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datadying.in.nonIC$median<- datatemp$median
  
  dataleavingIC <- data.frame(Date=datarecovering.in.IC$Date,
                              Recovery=datarecovering.in.IC$median,
                              Death=datadying.in.IC$median)
  dataleavingIC <- gather(dataleavingIC, key = "variable", value = "value",
                          Recovery, Death)
  
  dataleavingnonIC <- data.frame(Date=datarecovering.in.nonIC$Date,
                                 Recovery=datarecovering.in.nonIC$median,
                                 Death=datadying.in.nonIC$median)
  dataleavingnonIC <- gather(dataleavingnonIC, key = "variable", value = "value",
                             Recovery, Death)
  #  print(datadying.in.IC)
  
  dataALL <- data.frame(Date=dataSP$Date,
                        Susceptible=dataSP$median,
                        Exposed=dataE$median,
                        Infectious=dataAI$median,Hospital=dataHWCB$median, Recovered=dataR$median,
                        Dead=dataD$median,total=dataSP$median+dataE$median+dataAI$median+dataHWCB$median+dataR$median+dataD$median
  )
  #print(dataALL)
  datanewcaseslong <- data.frame(Date=datanewcases$Date,
                                 Median=datanewcases$median,
                                 blah=datanewcases$median)
  datanewcaseslong<- gather(datanewcaseslong, key = "variable", value = "value",
                            Median )
  
  datanew.cases.entering.IClong<- data.frame(Date=datanew.cases.entering.IC$Date,
                                             Median=datanew.cases.entering.IC$median,
                                             blah=datanew.cases.entering.IC$median)
  datanew.cases.entering.IClong<- gather(datanew.cases.entering.IClong, key = "variable", value = "value",
                                         Median )
  
  
  dataALL1<- gather(dataALL, key = "variable", value = "value",
                    Exposed,
                    Recovered,Infectious)
  
  dataALL2<- gather(dataALL, key = "variable", value = "value",
                    Susceptible)
  
  dataALL3<- gather(dataALL, key = "variable", value = "value",
                    Hospital)
  
  
  ##make a table of key info
  ICbedcapacity=1000000
  nonICbedcapacity=1000000
  
  datatab=data.frame("Date"=as.Date(plot1$Date, format = "%d/%m/%Y"),
                     #"Total_acute_bed_demand" = round(databeds$median*100/nonICbedcapacity,1),
                     #"Total_ICU_bed_demand" =round(dataIC$median*100/ICbedcapacity,1),
                     
                     "Cumulative"=  round(datanew.cases.entering.hospital.cumsum$median,0),
                     "Lowercases" = round(datanew.cases.entering.hospital.cumsum$lower,0),
                     "Uppercases" = round(datanew.cases.entering.hospital.cumsum$upper,0),
                     #"New_diag" = round(datanewcases$median,0),
                     "CumulativeDEAD"=  round(dataD$median,0),
                     "LowercasesDEAD" = round(dataD$lower,0),
                     "UppercasesDEAD" = round(dataD$upper,0)
  )
  
  colnames(datatab) <- c("Date",
                         "Cumulative cases",#"%IC bed\ndemand",
                         
                         "lower",
                         "upper",
                         # "New diagnoses in 24hr",
                         "CUMDEAD",
                         "LOWDEAD",
                         "UPDEAD"
  )#,"Deaths in\nhospital")
  
  
  datatab2=data.frame("Date"=as.Date(plot1$Date, format = "%d/%m/%Y"),
                      "Emed"=  round(dataE$median,0),
                      "Elow" = round(dataE$lower,0),
                      "Ehigh" = round(dataE$upper,0),
                      
                      "AImed"=  round(dataAI$median,0),
                      "AIlow" = round(dataAI$lower,0),
                      "AIhigh" = round(dataAI$upper,0),
                      "Rmed"=  round(dataR$median,0),
                      "Rlow" = round(dataR$lower,0),
                      "Rhigh" = round(dataR$upper,0),
                      "Amed"=  round(dataA$median,0),
                      "Alow" = round(dataA$lower,0),
                      "Ahigh" = round(dataA$upper,0),
                      "Imed"=  round(dataI$median,0),
                      "Ilow" = round(dataI$lower,0),
                      "Ihigh" = round(dataI$upper,0)
                      
  )
  
  datatab3=data.frame("Date"=as.Date(plot1$Date, format = "%d/%m/%Y"),
                      
                      "OnonIC"=  round(databeds$median,0),
                      "OnonIClow"=  round(databeds$lower,0),
                      "OnonICupper"=  round(databeds$upper,0),
                      "OIC"=  round(  dataIC$median,0),
                      "OIClow"=  round(dataIC$lower,0),
                      "OICupper"=  round(dataIC$upper,0)
  )
  
  datatab$Date <- as.Date(datatab$Date, "%d/%m/%y")
  datatabsummary <- datatab[seq(from=1,to=nrow(datatab),by=1),]
  datatabsummary2 <- datatab2[seq(from=1,to=nrow(datatab2),by=1),]
  datatabsummary3 <- datatab3[seq(from=1,to=nrow(datatab3),by=1),]
  
  plotdet =list(xlab(label=""), 
                theme_minimal() ,
                theme(panel.border = element_rect(linetype = "solid", fill = NA)) ,
                theme(text = element_text(size=13,colour="black")) ,
                theme(axis.text.x = element_text(color="black", 
                                                 size=13),
                      axis.text.y = element_text(color="black", 
                                                 size=13)) , theme(legend.title = element_blank()) ,
                scale_color_manual(name="",values = c("Daily Cases" = cols[1])),
                theme(legend.position = 'bottom'),guides(colour=guide_legend(ncol=3)),
                theme(axis.text.x = element_text(angle = 90, hjust = 1)),
                geom_vline(xintercept=COVIDBYDAY[time1,]$Date, colour=wes_palette("Zissou1",n=5)[1]) ,
                geom_vline(xintercept=COVIDBYDAY[time2,]$Date, colour=wes_palette("Zissou1",n=5)[3]) ,
                geom_vline(xintercept=COVIDBYDAY[time3,]$Date, colour=wes_palette("Zissou1",n=5)[5]) ,
                theme(plot.title = element_text(hjust = 0.5,size=13))
  )
  #  print(datanew.cases.entering.hospital.cumsum)
  P1<- ggplot(data= datanew.cases.entering.hospital.cumsum, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    annotate("text", x = COVIDBYDAY[time1,]$Date -3, y = max(COVIDBYDAY$MAX,datanew.cases.entering.hospital.cumsum$upper,na.rm=TRUE)/2, label = "Social Distancing",angle=90,colour=wes_palette("Zissou1",n=5)[1],size=4)+
    annotate("text", x = COVIDBYDAY[time2,]$Date -2, y = max(COVIDBYDAY$MAX,datanew.cases.entering.hospital.cumsum$upper,na.rm=TRUE)/2, label = "School Closures",angle=90,colour=wes_palette("Zissou1",n=5)[3],size=4)+
    annotate("text", x = COVIDBYDAY[time3,]$Date +3, y = max(COVIDBYDAY$MAX,datanew.cases.entering.hospital.cumsum$upper,na.rm=TRUE)/2, label = "Lockdown",angle=90,colour=wes_palette("Zissou1",n=5)[5],size=4)+
    #geom_errorbar(data=COVIDBYDAY,aes(x=Date,ymin=CUMSUM, ymax=CUMSUM*(1+UR1max)), width=0.1,colour=cols[5],alpha=0.5)+
    geom_point(data=COVIDBYDAY[-(completeat:lastat),],aes(x=Date,y=CUMSUM,colour="Observed"), width=0.8,size=3)+#,colour="black")+
  
    #  geom_point(data=COVIDBYDAY[completeat:lastat,],aes(x=Date,y=CUMSUM,colour="Observed"), width=0.8,size=3,alpha=0.4)+#,colour="black")+
    
    # geom_point(data=COVIDBYDAY[-fittingtimecases,],aes(x=Date,y=CASES), width=0.8,size=3,colour="grey")+
    geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.25,fill = "#666666",size=0.5)+
    geom_ribbon(aes(ymin=lower2, ymax=upper2), linetype=0, alpha=0.5,size=0.5,fill="#666666")+
    geom_line(aes(y=median,colour="Predicted"),size=1.5) +
    ylab(label="")+
    ggtitle("Cumulative cases (tested in hospital)")+
    plotdet+theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    theme(
      legend.position = c(1, 0),
      legend.justification = c("right", "bottom"),
      legend.box.just = "right",
      legend.margin = margin(6, 6, 6, 6)
    )    + guides(colour=guide_legend(nrow=2,byrow=TRUE))+
    scale_colour_manual(values = c(cols[5],"black"))
  
  
  P3<- ggplot(data= dataIC, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    # geom_errorbar(data=beds,aes(x=Date,ymin=IC, ymax=IC*(1+UR2max)), width=0.8,size=1,colour=cols[5],alpha=0.25)+
    #  geom_point(data=beds,aes(x=Date,y=IC), width=0.8,size=2,colour=cols[5])+
    # geom_errorbar(data=beds,aes(x=Date,ymin=ICMIN, ymax=ICMAX), width=0.8,size=1,colour="black",alpha=0.25)+theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    # geom_errorbar(data=beds,aes(x=Date,ymin=ICMIN, ymax=ICMAX), width=0.8,size=1,colour="grey",alpha=0.25)+theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.25,fill = "#666666",size=0.5)+
    geom_ribbon(aes(ymin=lower2, ymax=upper2), linetype=0, alpha=0.5,size=0.5,fill="#666666")+
    geom_line(aes(y=median,colour="Number of confirmed COVID-19 patients in HDU/ITU"),size=1.5,alpha=0.8,colour="black") +
    ylab(label="")+
    ggtitle("COVID-19 patients in IC beds")+
    plotdet +
    scale_x_date(breaks = pretty_breaks(18))+
    ylim(c(0, max(databeds$upper)+10))
  
  
  
  P4<- ggplot(data= databeds, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.25,fill = "#666666",size=0.5)+
    geom_ribbon(aes(ymin=lower2, ymax=upper2), linetype=0, alpha=0.5,size=0.5,fill="#666666")+
    geom_line(aes(y=median,colour="Number of confirmed COVID-19 patients in acute beds"),size=1.5,alpha=0.8,colour="black") +
    ylab(label="")+
    ggtitle(label="COVID-19 patients occupying acute beds")+
    plotdet+# theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    scale_x_date(breaks = pretty_breaks(18))+
    ylim(c(0, max(databeds$upper)+10))
  
  
  P2<- ggplot(data= datanew.cases.entering.IClong, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    annotate("text", x = COVIDBYDAY[time1,]$Date -1, y = max(datanew.cases.entering.IC$upper,na.rm=TRUE)/2, label = "",angle=90,colour=wes_palette("Zissou1",n=5)[1],size=5)+
    annotate("text", x = COVIDBYDAY[time2,]$Date -1, y = max(datanew.cases.entering.IC$upper,na.rm=TRUE)/2, label = "",angle=90,colour=wes_palette("Zissou1",n=5)[3],size=5)+
    annotate("text", x = COVIDBYDAY[time3,]$Date -1, y = max(datanew.cases.entering.IC$upper,na.rm=TRUE)/2, label = "",angle=90,colour=wes_palette("Zissou1",n=5)[5],size=5)+
    #geom_point(data=beds,aes(x=Date,y=allbeds), width=0.8,size=3,colour="black")+
    # geom_errorbar(data=beds,aes(x=Date,ymin=allbeds, ymax=allbedsMAX), width=0.8,size=1,colour="black",alpha=0.25)+
    # geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.1,fill = "#666666",size=0.5)+
    geom_bar(aes(y=value,fill=variable),size=1.5,stat = "identity") +
    ylab(label="")+
    ggtitle(label="Hospitalised in IC beds last 24h")+
    plotdet+ 
    scale_fill_manual(values = cols[11],name= "" #guide = guide_legend(reverse = TRUE)
    )+ theme(legend.position = "none") + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    ylim(c(0, max(datanewcases$median, COVIDBYDAY[-(completeat:lastat),]$CASES,na.rm = TRUE)+10))
  
  P5<- ggplot(data= datanewcaseslong, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    annotate("text", x = COVIDBYDAY[time1,]$Date -1, y = max(datanewcases$upper,na.rm=TRUE)/2, label = "",angle=90,colour=wes_palette("Zissou1",n=5)[1],size=5)+
    annotate("text", x = COVIDBYDAY[time2,]$Date -1, y = max(datanewcases$upper,na.rm=TRUE)/2, label = "",angle=90,colour=wes_palette("Zissou1",n=5)[3],size=5)+
    annotate("text", x = COVIDBYDAY[time3,]$Date -1, y = max(datanewcases$upper,na.rm=TRUE)/2, label = "",angle=90,colour=wes_palette("Zissou1",n=5)[5],size=5)+
    #geom_point(data=beds,aes(x=Date,y=allbeds), width=0.8,size=3,colour="black")+
    # geom_errorbar(data=beds,aes(x=Date,ymin=allbeds, ymax=allbedsMAX), width=0.8,size=1,colour="black",alpha=0.25)+
    # geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.1,fill = "#666666",size=0.5)+
    geom_bar(aes(y=value,fill=variable),size=1.5,stat = "identity") +
    geom_point(data=COVIDBYDAY[-(completeat:lastat),],aes(y=CASES), width=0.8,size=2,colour=cols[5])+
    #geom_point(data=COVIDBYDAY[(completeat:lastat),],aes(y=CASES), width=0.8,size=2,colour=cols[5],alpha=0.4)+
    ylab(label="")+
    ggtitle(label="Hospitalised in acute beds last 24h")+
    plotdet+ 
    scale_fill_manual(values = cols[11],name= "" #guide = guide_legend(reverse = TRUE)
    )+ theme(legend.position = "none")+theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    ylim(c(0, max(datanewcases$median, COVIDBYDAY[-(completeat:lastat),]$CASES,na.rm = TRUE)+10))
  
  
  P6 <- ggplot(data= dataD, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    # geom_errorbar(data=deaths,aes(x=Date,ymin=CUMSUM, ymax=CUMSUM*(1+UR4max)), width=0.8,size=1,colour=cols[5],alpha=0.25)+
    geom_point(data=DEATHSBYDAY[-(completeat:lastat),],aes(y=CUMSUM), width=0.8,size=3,colour=cols[5])+
   # geom_point(data=DEATHSBYDAY[(completeat:lastat),],aes(y=CUMSUM), width=0.8,size=3,colour=cols[5],alpha=0.4)+
    # geom_errorbar(data=beds,aes(x=Date,ymin=ICMIN, ymax=ICMAX), width=0.8,size=1,colour="black",alpha=0.25)+theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    # geom_errorbar(data=beds,aes(x=Date,ymin=ICMIN, ymax=ICMAX), width=0.8,size=1,colour="grey",alpha=0.25)+theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.25,fill = "#666666",size=0.5)+
    geom_ribbon(aes(ymin=lower2, ymax=upper2), linetype=0, alpha=0.5,size=0.5,fill="#666666")+
    geom_line(aes(y=median,colour="Estimate of total deaths"),size=1.5,alpha=0.8,colour="black") +
    ylab(label="")+
    ggtitle("Cumulative deaths")+
    plotdet + scale_x_date(breaks = pretty_breaks(18))#+ theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  
  
  
  P7<- ggplot(data= datacom, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    geom_bar(aes(y=value,fill=variable),size=1.5,stat = "identity") +
    ylab(label="")+
    ggtitle(label="Infections in community")+
    plotdet+ #theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    scale_fill_manual(values = cols[9:10],name= "" #guide = guide_legend(reverse = TRUE)
    )+ theme(
      legend.position = c(.05, .95),
      legend.justification = c("left", "top"),
      legend.box.just = "left",
      legend.margin = margin(6, 6, 6, 6)
    )+ scale_x_date(breaks = pretty_breaks(18))
  
  P8<- ggplot(data= dataALL1, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    geom_bar(aes(y=value,fill=factor(variable,levels = c("Recovered","Exposed","Infectious"))),size=1.5,stat = "identity") +
    ylab(label="")+
    ggtitle(label="Size of E-I-R populations")+
    plotdet+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    # scale_fill_discrete(name = "", labels = c("Infected", "Dead", "Hospitalised","Recovered"))+
    scale_fill_manual(values = c(cols[3],"cornflowerblue",cols[c(2)]),name= "", #guide = guide_legend(reverse = TRUE),
                      breaks = c("Recovered","Exposed","Infectious"))+ theme(
                        legend.position = c(.05, .95),
                        legend.justification = c("left", "top"),
                        legend.box.just = "left",
                        legend.margin = margin(6, 6, 6, 6)
                      )+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 
  
  P9<- ggplot(data= dataALL2, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    geom_bar(aes(y=value,fill=variable),size=1.5,stat = "identity") +
    ylab(label="")+
    ggtitle(label="")+
    plotdet+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    # scale_fill_discrete(name = "", labels = c("Infected", "Dead", "Hospitalised","Recovered"))+
    scale_fill_manual(values = cols[6],name= "", #guide = guide_legend(reverse = TRUE),
                      breaks = c("Susceptible"))+ theme(
                        legend.position = c(.05, .95),
                        legend.justification = c("left", "top"),
                        legend.box.just = "left",
                        legend.margin = margin(6, 6, 6, 6)
                      )
  
  P10<- ggplot(data= dataALL3, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    geom_bar(aes(y=value,fill=variable),size=1.5,stat = "identity") +
    ylab(label="")+
    ggtitle(label="Total in hospital")+
    plotdet+ #theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    # scale_fill_discrete(name = "", labels = c("Infected", "Dead", "Hospitalised","Recovered"))+
    scale_fill_manual(values = cols[8],name= "", #guide = guide_legend(reverse = TRUE),
                      breaks = c("Hospital"))+ theme(
                        legend.position = c(.05, .95),
                        legend.justification = c("left", "top"),
                        legend.box.just = "left",
                        legend.margin = margin(6, 6, 6, 6)
                      )+ theme(legend.position = "none")+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 
  
  P11<- ggplot(data= dataleavingIC, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    geom_bar(aes(y=value,fill=variable),size=1.5,stat = "identity") +
    ylab(label="")+
    ggtitle(label="Leaving IC in last 24 hours")+
    plotdet+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    # scale_fill_discrete(name = "", labels = c("Infected", "Dead", "Hospitalised","Recovered"))+
    scale_fill_manual(values = c("slategray4",cols[c(3)]),name= "" #, #guide = guide_legend(reverse = TRUE),
                      # breaks = c("Recovered","Dying")
    )+ theme(
      legend.position = c(.05, .95),
      legend.justification = c("left", "top"),
      legend.box.just = "left",
      legend.margin = margin(6, 6, 6, 6)
    )+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    ylim(c(0, max(datarecovering.in.nonIC$median + datadying.in.nonIC$median)+10))
  
  P12<- ggplot(data= dataleavingnonIC, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    geom_bar(aes(y=value,fill=variable),size=1.5,stat = "identity") +
    ylab(label="")+
    ggtitle(label="Leaving acute beds in last 24 hours")+
    plotdet+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    # scale_fill_discrete(name = "", labels = c("Infected", "Dead", "Hospitalised","Recovered"))+
    scale_fill_manual(values = c("slategray4",cols[c(3)]),name= "" #, 
                      #breaks = c("Recovered","Dying")
    )+ theme(
      legend.position = c(.05, .95),
      legend.justification = c("left", "top"),
      legend.box.just = "left",
      legend.margin = margin(6, 6, 6, 6)
    )+ theme(axis.title.x = element_blank(), axis.text.x = element_blank())  +
    ylim(c(0, max(datarecovering.in.nonIC$median + datadying.in.nonIC$median)))
  
  TITLE <-  paste0("COVID-19 ",SPECREGION,sep="")
  
  #ALL 
  grid.arrange(rbind(ggplotGrob(P1), ggplotGrob(P6), ggplotGrob(P8), ggplotGrob(P10),ggplotGrob(P7), size = "last"),
               rbind(ggplotGrob(P5), ggplotGrob(P12), ggplotGrob(P4) ,ggplotGrob(P2), ggplotGrob(P11), ggplotGrob(P3), size = "last"),
  top = textGrob(TITLE,gp=gpar(fontsize=20,font=3)),ncol=2)
  
    #tableGrob(datatabsummary, theme=ttheme_default(base_size = 7) ,rows = NULL),
  
   #FIGURE 2 
#grid.arrange(rbind(ggplotGrob(P1), ggplotGrob(P6),size = "last")) 
 # print(datatabsummary) #table 1 
  
  #FIGURE 3
  #grid.arrange(rbind(ggplotGrob(P8), ggplotGrob(P7),size = "last"))     
 #  print(datatabsummary2) #table 2 EIR and community
  
  #FIGURE 4
 # grid.arrange(rbind(ggplotGrob(P5), ggplotGrob(P12), ggplotGrob(P4),size = "last"),
 #              rbind(ggplotGrob(P2), ggplotGrob(P11), ggplotGrob(P3),size = "last"),ncol=2)     
 #  print(datatabsummary3) #table 2 EIR and community
  
 # return(databeds) #for the peak of non-IC capacity
 # return(dataIC) #for the peak of IC capacity
 #return(datanewcases) #for peak of acute beds in 24hrs
#  return(datanew.cases.entering.IC) #for peak of IC in 24hrs
  
}
