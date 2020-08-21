###############################  COVID-19 MODEL  ##################################
###############################    Ross Booton   ##################################
###############################   JUNE 2020     ##################################
#### Data should be updated from GOV.UK https://coronavirus.data.gov.uk/#regions for the South West 
### need "coronavirus-cases_latest.csv" and "coronavirus-deaths_latest.csv" from Download the latest cases data as CSV .... and Download the latest deaths data as CSV 
# updated for deaths in region with https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
###regional data included as .csv on GitHub (coronavirus-deaths-regional.csv) - from ONS data
setwd("~/Documents/Bristol/COVID-19")

################################## SPECIFY REGION / AREA ####################################
SPECREGION <- toupper("South West") #"South West" #"England" #
#initialise all functions and data we need 
IN <- function(Q) {
  if (Q==TRUE) {

    ############################### load packages ####################################
    library(pacman)
    pacman::p_load(deSolve,ggplot2,lhs,wesanderson,cowplot,data.table,grid,bayestestR,RColorBrewer,
                   socialmixr,
                   IMIS,dplyr,reshape,pbapply,gridExtra,gmp,gtable,magick,yarrr,tidyr,scales,parallel,compiler) 
    ############################### useful functions####################################
    cols2<<- brewer.pal(8,"Dark2")
    cols3<<- brewer.pal(9,"Set1")
    darkcols<<- brewer.pal(8,"Paired")
    getPalette = colorRampPalette(piratepal(palette = "basel",length=10))
    cols <<- c(unname(piratepal(palette = "basel",length=10))[1:6] ,"grey",unname(piratepal(palette = "pony",length=10))[2:5],
               unname(piratepal(palette = "basel",length=10))[7:10],unname(piratepal(palette = "pony",length=10))[7:8],"white")
    ############################### read in data ####################################
    ### from: https://coronavirus.data.gov.uk/#regions
    ALLCASES <<- fread("coronavirus-cases_latest.csv")
    ALLCASES <<- select(ALLCASES, "Area name", "Area code", "Area type", "Specimen date","Daily lab-confirmed cases", "Cumulative lab-confirmed cases")
    ALLDEATHS <<- fread("coronavirus-deaths_latest.csv")
    ALLDEATHSREGIONAL <<- fread("coronavirus-deaths-regional.csv") #from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
    colnames(ALLDEATHS)[5:6] <<- c("Daily hospital deaths", "Cumulative hospital deaths")
    ALLDEATHS <<- rbind(ALLDEATHS,ALLDEATHSREGIONAL,fill=TRUE)
    J <<- unique(ALLCASES$`Specimen date`,ALLDEATHS$`Reporting date`)
   # J <<- c(J,"2020-01-20") # NEW start date is 20th Jan 2020
   # J <<- c(J,"2019-12-01") # NEW start date is 20th Jan 2020
    ALLDATES <<- J[order(J)]
    ALLDATES<<- seq(from=as.Date(ALLDATES[1]),to=as.Date(last(ALLDATES)), by=1) 
    colnames(ALLCASES)[c(1,2,3,4,5,6)] <<- c("Area","AreaCode","AreaType","Date","CASES","CUMSUM") #DAY 1 is 30th Jan 2020
    ALLCASES$Area <<- toupper( ALLCASES$Area)
    #chop0 <<- 20 #how many 0 cases to chop out of LL fitting method 
    #ALLREGIONS <<- unique(ALLCASES$`Area`)
    COVIDBYDAY<<-filter(ALLCASES, Area == SPECREGION)
    COVIDBYDAY <<- COVIDBYDAY[order(COVIDBYDAY$Date),]
    COVIDBYDAY$Date <<- as.Date(COVIDBYDAY$Date)
    tempdat <<- data.frame(Area = SPECREGION, AreaCode=COVIDBYDAY[1,2],AreaType=COVIDBYDAY[1,3], Date =ALLDATES,CASES=NA,CUMSUM = NA)
    tempdat<<- tempdat[(tempdat$Date %in% c(COVIDBYDAY$Date))==FALSE,] #Reformatting data into 30th JAN onwards with NA / 0 introduced for no observations
    COVIDBYDAY <<- rbind(COVIDBYDAY,tempdat)
    COVIDBYDAY <<- COVIDBYDAY[order(COVIDBYDAY$Date),]
    miss <<- is.na(COVIDBYDAY$CASES)
    COVIDBYDAY$CASES[miss] <<- 0
    COVIDBYDAY$CUMSUM <- cumsum(COVIDBYDAY$CASES)
    COVIDBYDAY$CUMSUM[COVIDBYDAY$CASES == 0] <<- NA
    COVIDBYDAY$CASES[COVIDBYDAY$CASES == 0] <<- NA

   # COVIDBYDAY <<-   COVIDBYDAY[COVIDBYDAY$Date>="2020-02-25",] #USE ONLY DATA AFTER 1st MARCH 2020
    LA <<- nrow(COVIDBYDAY)
    #DEATHS only available for "United Kingdom" "England" "Scotland" "Northern Ireland" "Wales"
    colnames(ALLDEATHS)[c(1,2,3,4,5,6)] <<- c("Area","AreaCode","AreaType","Date","CASES","CUMSUM") #DAY 1 is 30th Jan 2020
    ALLDEATHS$Area <<- toupper( ALLDEATHS$Area)
    DEATHSBYDAY<<-filter(ALLDEATHS, Area == SPECREGION)
    DEATHSBYDAY <<- DEATHSBYDAY[order(DEATHSBYDAY$Date),]
    DD <<- if (SPECREGION %in% toupper( unique(ALLDEATHSREGIONAL$`Area name` )) ) {"%d/%m/%y"} else {"%Y-%m-%d"}
    DEATHSBYDAY$Date <<- as.Date(DEATHSBYDAY$Date,
                                 format = DD
    )
    tempdat <<- data.frame(Area = SPECREGION, AreaCode=DEATHSBYDAY[1,2],AreaType=DEATHSBYDAY[1,3], Date =ALLDATES,CASES=NA,CUMSUM = NA)
    tempdat<<- tempdat[(tempdat$Date %in% c(DEATHSBYDAY$Date))==FALSE,] #Reformatting data into 30th JAN onwards with NA / 0 introduced for no observations
    DEATHSBYDAY <<- rbind(DEATHSBYDAY,tempdat)
    DEATHSBYDAY <<- DEATHSBYDAY[order(DEATHSBYDAY$Date),]
    miss <<- is.na(DEATHSBYDAY$CASES)
    DEATHSBYDAY$CASES[miss] <<- 0
    DEATHSBYDAY$CUMSUM <<- cumsum(DEATHSBYDAY$CASES)
    DEATHSBYDAY$CUMSUM[DEATHSBYDAY$CUMSUM == 0] <<- NA
    DEATHSBYDAY$CASES[DEATHSBYDAY$CASES == 0] <<- NA
    DEATHSBYDAY$CASESORIG <<-  DEATHSBYDAY$CASES
    DEATHSBYDAY$CUMSUMORIG <<- DEATHSBYDAY$CUMSUM
    DEATHSBYDAY$CASES <<-  as.numeric(DEATHSBYDAY$CASES) * 0.839 #assume 83.9% of deaths are in hospital
    DEATHSBYDAY$CUMSUM <<-  as.numeric(DEATHSBYDAY$CUMSUM) * 0.839 #assume 83.9% of deaths are in hospital
    DEATHSBYDAY$CUMSUM[is.na(DEATHSBYDAY$CASES )] <<- NA
 
    time1 <<- as.numeric(as.Date("2020-03-15") - COVIDBYDAY[1,]$Date + 1)
   # time2 <<- as.numeric(as.Date("2020-03-20") - COVIDBYDAY[1,]$Date + 1)
    time2 <<- as.numeric(as.Date("2020-03-23") - COVIDBYDAY[1,]$Date + 1)
  #  time4 <<- as.numeric(as.Date("2020-05-01") - COVIDBYDAY[1,]$Date + 1)
    #DEATHSBYDAY <<-   DEATHSBYDAY[DEATHSBYDAY$Date>="2020-02-25",] #USE ONLY DATA AFTER 1st MARCH 2020
    
   # DEATHSBYDAY <<- DEATHSBYDAY[-c(1:chop),]
    ###DEMOGRAPHIC DATA
    DEMOALL <<- fread("DEMOGRAPHY.csv")
    DEMOALL$Name <<- toupper( DEMOALL$Name)
    
    #colnames(DEMOALL) <<- c("Name",0:90)
    DEMO<<-filter(DEMOALL, Name == SPECREGION)
    DEMO<<- DEMO[,-c(1:2)]
    ###POLYMOD DATA 
    data(polymod)
    AGELIM <<- c(0,5,18,30,40,50,60,70)
    Z <<- AGELIM 
    AGEPOP<<-c(sum(DEMO[0:(Z[2])]), 
               sum(DEMO[(Z[2]+1):(Z[3])]),
               sum(DEMO[(Z[3]+1):(Z[4])]),
               sum(DEMO[(Z[4]+1):(Z[5])]),
               sum(DEMO[(Z[5]+1):(Z[6])]),
               sum(DEMO[(Z[6]+1):(Z[7])]),
               sum(DEMO[(Z[7]+1):(Z[8])]),
               sum(DEMO[(Z[8]+1):(ncol(DEMO))]))
    #POP <<- POLYDATA$demography
    POLYDATA<<-   contact_matrix(polymod, countries = "United Kingdom", age.limits = AGELIM)
    AGEGRP <<- as.matrix(POLYDATA$matrix)
    mev <<- (max(Re(eigen(AGEGRP)$value)))
    N_AGE <<- ncol(AGEGRP)
    N_COMP <<- 11
    #DAY ONE IS 30th JAN 2020
    LAMBDA <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
   # OMEGA <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    OMEGA <<- as.matrix((fread("comix.csv"))) #data from COMIX study
    XI <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
   DELTA <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    COMP1 <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    COMP2 <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    COMP3 <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    CONTACTMATRIX <<- AGEGRP
    UR1max <<- 0 #set to zero to hit the line as best we can 
    set.seed(3333) # 
    
  #  if (nrow(COVIDBYDAY) <= nrow(DEATHSBYDAY) ){
  # CEXTRA <-  COVIDBYDAY[nrow(COVIDBYDAY),]
  # CEXTRA$Date <- CEXTRA$Date + nrow(DEATHSBYDAY) - nrow(COVIDBYDAY) 
  #CEXTRA$CASES<-NA
  #CEXTRA$CUMSUM <- NA
  #COVIDBYDAY <<-rbind(COVIDBYDAY,CEXTRA)
   # }
    LLdeaths <<-   rep(NA,max(nrow(COVIDBYDAY),nrow(DEATHSBYDAY)))
    LLhospday <<-   rep(NA,max(nrow(COVIDBYDAY),nrow(DEATHSBYDAY)))
    totaldeaths <- rep(0,(max(nrow(COVIDBYDAY),nrow(DEATHSBYDAY) )))
    totalhospday <- rep(0,(max(nrow(COVIDBYDAY),nrow(DEATHSBYDAY) )))
    }}
B<-IN(TRUE)

####time varying array for remainder of time from startdate to enddate
startdate <- as.Date("2020-05-11") #e.g. 11th May 2020 when lockdown started to lessen
starttime <- as.numeric(startdate - COVIDBYDAY[1,]$Date + 1)
enddate <- as.Date("2020-12-01") #e.g. 1st Dec 2020 how long do you want the simulation to run for?
endtime <- as.numeric(enddate - COVIDBYDAY[1,]$Date + 1)

#make nxn array for the rest of the time e.g. 11th May - 1st Dec 2020
XYZ <- array(NA, c(N_AGE,N_AGE,endtime))

#then just fill it in with whatever you like e.g. on 11th May 2020 lets say school contacts closed still but  contacts 30% more
futuredate1 <- startdate
futureINTMAT1 <- OMEGA #set everything to equal lockdown contact rates from comix
futureeffectsizeSCHOOLS1 <- 1 #no change
futureeffectsize1 <- 1.3 #30% increase
futureINTMAT1[1:2,1:2] <- futureINTMAT1[1:2,1:2]*futureeffectsizeSCHOOLS1 #change age groups 1 and 2 
futureINTMAT1[3:8,3:8] <- futureINTMAT1[3:8,3:8]*futureeffectsize1 #change age groups 3 to 8 

futuredate2 <- as.Date("2020-06-01") #e.g. 1st June 2020, lets say the government change to fully open schools and allow other people to contact 50% 
futureINTMAT2 <- OMEGA #set to comix ockdown contact rates
futureeffectsizeSCHOOLS2 <- 1.5 #schools open 
futureeffectsize2 <- 1.5 #50% increase
futureINTMAT2[1:2,1:2] <- futureINTMAT2[1:2,1:2]*futureeffectsizeSCHOOLS2 #change age groups 1 and 2 
futureINTMAT2[3:8,3:8] <- futureINTMAT2[3:8,3:8]*futureeffectsize2 #change age groups 3 to 8 


futuredate3 <- as.Date("2020-07-01") #1st July lets say change to completely normal but 75% reduced contacts compared to pre pandemic
futureINTMAT3 <- AGEGRP #set to POLYMOD normal life / 2 
futureeffectsizeSCHOOLS3 <- 0.75 #polymod normal /2
futureeffectsize3 <- 0.75 #polymod normal
futureINTMAT3[1:2,1:2] <- futureINTMAT3[1:2,1:2]*futureeffectsizeSCHOOLS3 #change age groups 1 and 2 
futureINTMAT3[3:8,3:8] <- futureINTMAT3[3:8,3:8]*futureeffectsize3 #change age groups 3 to 8 

futuredate4 <- as.Date("2020-08-01") #1st August then lets say change to complete lockdown
futureINTMAT4 <- OMEGA #set to comix post-lockdown contact rates

futuredate5 <- as.Date("2020-11-10") #then lets say everything back to normal daily life on 10th November but with some social distancing = 60% of usual contacts
futureINTMAT5 <- AGEGRP*0.6 #set to POLYMOD normal life
futureeffectsizeSCHOOLS5 <- 1 #no change
futureeffectsize5 <- 1 #no change
futureINTMAT5[1:2,1:2] <- futureINTMAT5[1:2,1:2]*futureeffectsizeSCHOOLS5 #change age groups 1 and 2 
futureINTMAT5[3:8,3:8] <- futureINTMAT5[3:8,3:8]*futureeffectsize5 #change age groups 3 to 8 


futuretime1 <- as.numeric(futuredate1 - COVIDBYDAY[1,]$Date )
futuretime2 <- as.numeric(futuredate2 - COVIDBYDAY[1,]$Date)
futuretime3 <- as.numeric(futuredate3 - COVIDBYDAY[1,]$Date )
futuretime4 <- as.numeric(futuredate4 - COVIDBYDAY[1,]$Date )
futuretime5 <- as.numeric(futuredate5 - COVIDBYDAY[1,]$Date )

XYZ[,,futuretime1:(futuretime2-1)] <- futureINTMAT1
XYZ[,,futuretime2:(futuretime3-1)] <- futureINTMAT2
XYZ[,,futuretime3:(futuretime4-1)] <- futureINTMAT3
XYZ[,,futuretime4:(futuretime5-1)] <- futureINTMAT4
XYZ[,,futuretime5:(endtime)] <- futureINTMAT5 #back to normal (!)


################         FUNC: DESOLVE FUNCTION           ######################
XY.model <- function(time, y, params) {#XY.model is the function for solving our ODEs
  dy = array(0, dim = c(N_COMP, N_AGE)) 
  y = array(y,  dim = c(N_COMP, N_AGE))
  Rt <- 0 
#  R0 <- params["R0"][[1]]
  kappa <- params["kappa"][[1]]
  rgnonIC <-params["rgnonIC"][[1]]
 # rgIC <-params["rgIC"][[1]]
  delta <- params["delta"][[1]]
  rho <- params["rho"][[1]]
  eta <- params["eta"][[1]]
  alpha <- params["alpha"][[1]]
  mu <- params["mu"][[1]]
  gamma <- c(params["gamma1"][[1]],params["gamma2"][[1]],params["gamma3"][[1]],params["gamma4"][[1]],params["gamma5"][[1]],params["gamma6"][[1]],params["gamma7"][[1]],params["gamma8"][[1]])
  omega <- c(params["omega1"][[1]],params["omega2"][[1]],params["omega3"][[1]],params["omega4"][[1]],params["omega5"][[1]],params["omega6"][[1]],params["omega7"][[1]],params["omega8"][[1]]) 
  psi <- params["psi"][[1]]
  betaA <- params["betaA"][[1]]
  betaI <- betaA
  LAMBDA[2,2] <- LAMBDA[2,2]*params["homeschool"][[1]]   ###UPDATE
#  OMEGA <- OMEGA*params["lockdown"][[ 1]]
  XI <- XI*params["distancing"][[1]]  #DISTANCING MATRIX  #advice to avoid pubs, clubs, theatres and other public institutions occured on 16th March 2020
 # DELTA[4:7,4:7] <- DELTA[4:7,4:7]*params["employment"][[1]]  #EMPLOYMENT MATRIX
  #######################  FOI  calculation  #######################
  sumN = apply(y,c(2),sum ) #total in each age group N_g
  sumX = apply(y,c(1),sum ) #total in each compartment X_i
  FOIMAT <- array(0,dim=c(N_AGE,N_AGE))
  lambda <- array(0,dim=N_AGE)
  endphase <- params["endphase"][[1]]
 #CONTACTMATRIX <- AGEGRP
#OMEGA <- OMEGA*params["lockdown"][[1]]
  
  STARTMAT <- array(0, dim = c(N_AGE, N_AGE))
  ENDMAT <- array(0, dim = c(N_AGE, N_AGE))
  rdecrease <- array(0, dim = 1) 
  if (time < time1)  {#NORMAL BEFORE 16TH MARCH 2020
    CONTACTMATRIX <- AGEGRP} else if ((time >=time1) & (time < time2))  {#SOCIAL DISTANCING ENCOURAGED 15th MARCH 2020
      STARTMAT <- AGEGRP
      ENDMAT <- AGEGRP*XI
       rdecrease <- (STARTMAT - ENDMAT)/(time2 - time1)
      CONTACTMATRIX <- STARTMAT - rdecrease*(time-time1+1)} else if ((time >=time2) & (time < (time2+endphase) )) {  ###need to UPDATE
        STARTMAT <- AGEGRP*XI
        COMP1 <-  (AGEGRP* ((LAMBDA <= XI)*LAMBDA + (LAMBDA > XI)*XI))
        ENDMAT <- ((COMP1 <= OMEGA)*COMP1 + (COMP1 > OMEGA)*OMEGA) 
        rdecrease <- (STARTMAT - ENDMAT)/(time2 +endphase - time2)
         CONTACTMATRIX <- STARTMAT - rdecrease*(time-time2+1) } else if ((time >=(time2+endphase)) & (time < starttime)) {
           COMP1 <- AGEGRP*((LAMBDA <= XI)*LAMBDA + (LAMBDA > XI)*XI) #take min of lambda and xi 
           CONTACTMATRIX <- ((COMP1 <= OMEGA)*COMP1 + (COMP1 > OMEGA)*OMEGA) #take min of all
           } else { #after starttime
             CONTACTMATRIX <- XYZ[,,time] #rest of time in future is an array
  }
Rt <- (max(Re(eigen(CONTACTMATRIX)$value)))*(1/mu)*betaI
  
  for (i in 1:N_AGE){
      FOIMAT[i,] <- betaI*CONTACTMATRIX[i,]*( (y[4,i]/sumN[]) + (y[5,i]/sumN[]) )
  }

#initialinfected <- params["initialinfected"][[1]]
  
  
  lambda <- apply(FOIMAT,c(2),sum ) #add up to give vector of lambda_g
  epsilon <- 1- rgnonIC 
  ###############################    MODEL EQUATIONS     #####################################
 # for (g in 1:N_AGE){
    #### Susceptible = S ####
    dy[1,] <- -lambda[] * y[1,] 
    #### Shielded = P ####
    dy[2,] <- 0 #-lambda[g] * y[2,g] 
    #### Exposed = E ####
    dy[3,] <- lambda[] * y[1,] - eta * y[3,] #+ ifelse(time <= 30 , initialinfected, 0 ) 
    #### Asymptomatic = A####
    dy[4,] <- eta * (1 - delta)* y[3,] - alpha * y[4,]
    #### Infectious  = I ####
    dy[5,] <- eta * delta * y[3,] - mu * y[5, ]
    #### Hospitalised in non- IC bed = H ####
    dy[6,] <-  mu * gamma[] * y[5, ] - rho* y[6,]
    #### Hospitalised in IC bed = C ####
    dy[7,] <-  epsilon*rho* y[6,] - psi * y[7,]
    #### Recovering in non- IC bed = W ####
    dy[8,] <- 0   #(1-epsilon)*rho*y[6,g] + chi*y[9,g] - phi*y[8,g] 
    #### Recovering in IC bed = B ####
    dy[9,] <- 0  #psi * (1 - omega) * y[7,g] - chi * y[9,g]
    #### Recovered ####
    dy[10,] <- alpha * y[4,]  + mu * (1 - gamma[]) * y[5, ] + psi * (1 - omega[]) * y[7,] + (1 - kappa) * (1 - epsilon) * rho  * y[6,]
    #### Dead ####
    dy[11,] <-  psi * omega[] * y[7, ] + (1- epsilon)*kappa*rho*y[6,]
   # }
  list(as.vector(dy),"Rt"=Rt)
}

XY.model <- cmpfun(XY.model)
################        EPID SIMULATOR        ######################
epid.start <- 0
epid <- function(eta,alpha,delta,mu,
                 gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,
                 psi,chi,betaA,betaI, rho,phi,w,yprop,rgnonIC,b, omega1, omega2, omega3, omega4, omega5, omega6, omega7, omega8, kappa,
                 homeschool,lockdown,distancing,employment,initialinfected,gamma8, UR1, endphase, #UR2, UR3, UR4,
                 epid.duration = nrow(COVIDBYDAY) - 1, #length of simulation
                 func.indicator) #plot or no plot?
{  ##set up initial population in each state for each group
  foo <- array(1:(N_COMP*N_AGE),dim=c(N_COMP,N_AGE)) #temp array used for indexing
  y_vec <- rep(0,(N_COMP*N_AGE))
  y_vec[as.vector(foo[1,])] <- AGEPOP #*(1-AGESHIELD) #uninfected age group 1 to N_AGE for SW ENGLAND DEMOGRAPHICS 2018
  y_vec[as.vector(foo[5,])] <- initialinfected #seed initialinfected infected in ALL age group , exposed
  names(y_vec) <- paste0("y", 1:(N_COMP*N_AGE))
  init<-y_vec[1:(N_COMP*N_AGE)]
  #params is the data from outside this function, which is used through the Sampling function
  params <- list(eta=eta,alpha=alpha,mu=mu,delta=delta,
                 gamma1=gamma1, gamma2=gamma2, gamma3=gamma3,gamma4=gamma4, gamma5=gamma5, gamma6=gamma6,gamma7=gamma7, 
                 psi=psi,betaA=betaA,betaI=betaI,rho=rho,rgnonIC=rgnonIC,
                 omega1=omega1, omega2=omega2, omega3=omega3, omega4=omega4, 
                 omega5=omega5, omega6=omega6, omega7=omega7, omega8=omega8,
                 kappa=kappa,
                 homeschool=homeschool,lockdown=lockdown,distancing=distancing,employment=employment,initialinfected=initialinfected,gamma8=gamma8,
                 endphase=endphase)
                
  vectTime <- c(0,1:epid.duration)  ## vectTime is rescaled in order to cope with the solver function		
  out <- as.data.frame(lsoda(y=init,time=vectTime,func=XY.model,parms=params))  #run the model using desolve
  out$time = out$time+epid.start #rescale the time so that it runs from day 0
  out$temp <- out$time
  out <- out[,-1]
  names(out)[names(out)=="temp"] <- "time"
  
  out<- out %>% dplyr::mutate(N = rowSums(.[as.vector(foo[,])]),
                              S = rowSums(.[as.vector(foo[1,])]),
                             # P = rowSums(.[as.vector(foo[2,])]),
                              E = rowSums(.[as.vector(foo[3,])]),
                              A = rowSums(.[as.vector(foo[4,])]),
                              I = rowSums(.[as.vector(foo[5,])]),
                              H = rowSums(.[as.vector(foo[6,])]),
                              C = rowSums(.[as.vector(foo[7,])]),
                             # W = rowSums(.[as.vector(foo[8,])]),
                            #  B = rowSums(.[as.vector(foo[9,])]),
                              R = rowSums(.[as.vector(foo[10,])]),
                              D = rowSums(.[as.vector(foo[11,])]),
                        

                              new.cases.entering.hospital = mu*gamma1*rowSums(.[as.vector(foo[5,1])])+ 
                              mu*gamma2*rowSums(.[as.vector(foo[5,2])])+
                                mu*gamma3*rowSums(.[as.vector(foo[5,3])]) + 
                              mu*gamma4*rowSums(.[as.vector(foo[5,4])])+ mu*gamma5*rowSums(.[as.vector(foo[5,5])])+
                                mu*gamma6*rowSums(.[as.vector(foo[5,6])])+ mu*gamma7*rowSums(.[as.vector(foo[5,7])])+
                              mu*gamma8*rowSums(.[as.vector(foo[5,8])]),
                            
                              new.cases.entering.hospital.cumsum = cumsum(new.cases.entering.hospital),
                            new.cases.entering.IC = rho*(1-rgnonIC)*rowSums(.[as.vector(foo[6,])]),
                            
                        dying.in.IC = psi*omega1*rowSums(.[as.vector(foo[7,1])])+ psi*omega2*rowSums(.[as.vector(foo[7,2])])+
                          psi*omega3*rowSums(.[as.vector(foo[7,3])]) + 
                          psi*omega4*rowSums(.[as.vector(foo[7,4])])+ psi*omega5*rowSums(.[as.vector(foo[7,5])])+
                          psi*omega6*rowSums(.[as.vector(foo[7,6])])+ psi*omega7*rowSums(.[as.vector(foo[7,7])])+
                          psi*omega8*rowSums(.[as.vector(foo[7,8])]),
                        
                        recovering.in.IC = psi*(1-omega1)*rowSums(.[as.vector(foo[7,1])])+ psi*(1-omega2)*rowSums(.[as.vector(foo[7,2])])+
                          psi*(1-omega3)*rowSums(.[as.vector(foo[7,3])]) + 
                          psi*(1-omega4)*rowSums(.[as.vector(foo[7,4])])+ psi*(1-omega5)*rowSums(.[as.vector(foo[7,5])])+
                          psi*(1-omega6)*rowSums(.[as.vector(foo[7,6])])+ psi*(1-omega7)*rowSums(.[as.vector(foo[7,7])])+
                          psi*(1-omega8)*rowSums(.[as.vector(foo[7,8])]),
                        
                        dying.in.nonIC = kappa*rho*(1-(1-rgnonIC))*rowSums(.[as.vector(foo[6,])]),
                        recovering.in.nonIC = (1-kappa)*rho*(1-(1-rgnonIC))*rowSums(.[as.vector(foo[6,])])
  )

  out$Date <- out$time + COVIDBYDAY$Date[1]
 #print(out)
  
   
 totaldeaths <- out$D[1:(max(nrow(COVIDBYDAY),nrow(DEATHSBYDAY) ))]
 totalhospday <- out$new.cases.entering.hospital[1:(max(nrow(COVIDBYDAY),nrow(DEATHSBYDAY) ))]
 casestemp <- COVIDBYDAY
 deathstemp <- DEATHSBYDAY
 
 LLdeaths <- (deathstemp$CUMSUM)*log(totaldeaths)  - totaldeaths #- lfactorial(deathstemp$CUMSUM)
 LLhospday <- (casestemp$CASES)*log(totalhospday)  - totalhospday #- lfactorial(casestemp$CASES)
  
  toReturn <- as.vector(c( 
                           LLdeaths,
                          LLhospday)) 
  names(toReturn) <- c( 
                        paste0("LLdeaths",1: length(LLdeaths)),
    #paste0("totaldeaths",1: (length(totaldeaths))),
                       # "LLtotaldeaths",
                        paste0("LLhospday",1: length(LLhospday))) #,  
                       # "LLtotalhosp")
  if (func.indicator=="returnout"){	
    ## the function returns important information,
    return(out)
  } else if (func.indicator=="returnindicators") {
    return(toReturn)
  } else { #run some plots
    print("no") #male female plot
  }
}


epid <- cmpfun(epid)

###############      FUNC: Latin Hypercube Sampling (LHS)       ###############
Sampling <- function(aaa){
  Samples_SIR <- randomLHS(aaa, 16)
  Samples_SIR[,1] <- 1.63 + (3.95-1.63)*Samples_SIR[,1]  #2.79 +- 1.16 #sample R0 instead of beta 0.05 + (0.15-0.05)*Samples_SIR[,1] # 0.04 + (0.10-0.04)*Samples_SIR[,1] #0.02 + (0.15-0.02)*Samples_SIR[,7] # betaA / betaI
  Samples_SIR[,2] <-  0.1 + (0.37-0.1)*Samples_SIR[,2] #0.17 + (0.37-0.17)*Samples_SIR[,2] # https://www.gstatic.com/covid19/mobility/2020-04-05_GB_Mobility_Report_en.pdf
  Samples_SIR[,3] <- 0.5 + (1.0-0.5)*Samples_SIR[,3] # distancing # 
  Samples_SIR[,4] <- 0.7 + (1.0 - 0.7)*Samples_SIR[,4] #  epsilon = 1 - rgnonIC
  Samples_SIR[,5] <-  0.05 + (0.35-0.05)*Samples_SIR[,5]    #kappa
  Samples_SIR[,6] <-  2 + (14-2)*Samples_SIR[,6] #rho 
  Samples_SIR[,7] <-  2 + (14-2)*Samples_SIR[,7]  # psi 
  Samples_SIR[,8] <-  2 + (11-2)*Samples_SIR[,8]  # mu 
   Samples_SIR[,9] <-  0.7315 + (0.9105-0.7315)*Samples_SIR[,9]  # delta 
   Samples_SIR[,10] <- 0 + (10-0)*Samples_SIR[,10]  # initial infected
   Samples_SIR[,11] <- 0.0204 + (0.070-0.0204)*Samples_SIR[,11]  # gamma4
   Samples_SIR[,12] <- 0.0253 + (0.0868-0.0253)*Samples_SIR[,12]  # gamma5
   Samples_SIR[,13] <- 0.0486 + (0.1670-0.0486)*Samples_SIR[,13]  # gamma6
   Samples_SIR[,14] <- 0.0701 + (0.240-0.0701)*Samples_SIR[,14]  # gamma7
   Samples_SIR[,15] <- 0.0987 + (0.376-0.0987)*Samples_SIR[,15]  # gamma8
   Samples_SIR[,16] <- 1 + (31-1)*Samples_SIR[,16]  # endphase
  paramsMat_SIR <- data.frame(eta=1/5.1, 
                              alpha= (1/ Samples_SIR[,8]), 
                              delta=Samples_SIR[,9], 
                              mu= (1/ Samples_SIR[,8]),
                              gamma1=0 ,gamma2=0.0408/100 ,gamma3=0.0104 ,
                              gamma4=Samples_SIR[,11],gamma5=Samples_SIR[,12],
                              gamma6=Samples_SIR[,13],gamma7=Samples_SIR[,14],
                              gamma8=Samples_SIR[,15], 
                             # omega=0,
                              psi=(1/Samples_SIR[,7]),
                             # chi=0,
                              betaA=Samples_SIR[,1]/(Samples_SIR[,8]*mev) ,
                              betaI=Samples_SIR[,1]/(Samples_SIR[,8]*mev), 
                              homeschool= 0.05,
                              lockdown= Samples_SIR[,2],
                              distancing= Samples_SIR[,3],
                              employment= 0.44,
                           
                              rgnonIC= Samples_SIR[,4],
                              kappa =Samples_SIR[,5],
                              rho = 1/(Samples_SIR[,6]), 
                              initialinfected =  Samples_SIR[,10] , #Samples_SIR[,16] ,
                              UR1 = 0, 
                             endphase = Samples_SIR[,16],
                             omega1 = 0, omega2 = 0, omega3 = 0.181, 
                            omega4 = 0.181, omega5 = 0.247, omega6 = 0.393,
                             omega7 = 0.539, omega8 = 0.653, #omega9 =0,
                            R0 = Samples_SIR[,1]
                  ) #Samples_SIR[,15]) #, UR2 = Samples_SIR[,18],
  # UR3 = Samples_SIR[,19], UR4 = Samples_SIR[,20])
  return(paramsMat_SIR)
}

#####  FUNC: RUN MODEL AND RETURN CSV FILES IN YOUR WORKING DIRECTORY    #######
outFUN <- function(bbb,a0){ #feed in the sample (bbb), along with the number of the file you want to name (a0)
  cl<-makeCluster(detectCores()-1)
  
  x <- bbb
  
  clusterExport(cl=cl, varlist = c("x", "epid", "LA", "N_COMP", "N_AGE", "AGEPOP", "XY.model",
                                   
                                   "LAMBDA", "OMEGA", "XI", "DELTA", "time1", "AGEGRP", "time2",
                                   
                                  # "time3",
                                 "starttime", "XYZ",
                                 "epid.start", "COVIDBYDAY", "DEATHSBYDAY"), envir=environment())
  
  clusterEvalQ(cl=cl,c(library(deSolve, library(data.table), library(magrittr))))
  
  
  
  outMat = parApply(cl, x,1,function(x) { # see below for working example of this function
    
    epid(as.list(x)$eta,as.list(x)$alpha,as.list(x)$delta,as.list(x)$mu,
         
         as.list(x)$gamma1,as.list(x)$gamma2,as.list(x)$gamma3,as.list(x)$gamma4,as.list(x)$gamma5,as.list(x)$gamma6,as.list(x)$gamma7,
         
         as.list(x)$psi,as.list(x)$chi,as.list(x)$betaA,as.list(x)$betaI,as.list(x)$rho,as.list(x)$phi,
         
         as.list(x)$w,as.list(x)$yprop,as.list(x)$rgnonIC,as.list(x)$b,
         
         as.list(x)$omega1, as.list(x)$omega2, as.list(x)$omega3, as.list(x)$omega4, as.list(x)$omega5, as.list(x)$omega6, as.list(x)$omega7, as.list(x)$omega8,        
         
         as.list(x)$kappa, as.list(x)$homeschool,as.list(x)$lockdown,as.list(x)$distancing,as.list(x)$employment,as.list(x)$initialinfected,as.list(x)$gamma8,
         
         as.list(x)$UR1,    as.list(x)$endphase, 
         
         epid.duration = nrow(COVIDBYDAY)  - 1 #length of simulation
         
         , func.indicator="returnindicators")})
  
  ResMat =  cbind(bbb,t(outMat))
  fwrite(ResMat,paste0("OUT_", a0,".csv"))
  stopCluster(cl)}



outFUN <- cmpfun(outFUN)
##################         FITTING      ######################## 

Nsamples<- 100000#how many samples?
p1<-Sampling(Nsamples)  
#Run the simulator outFUN for the Latin-Hypercube samples generated above
outFUN(p1,1 )#the file name.. let's call it OUT_1 so a0=1 here
FULLDATA<-fread("OUT_1.csv") #then read in the csv file you generated above

#Data from around 5 days ago can be considered complete, so set last 5 to = NA
completeat <- length(COVIDBYDAY$CASES)-4 #Data from around 5 days ago can be considered complete, so set last 5 to = NA
lastat <- length(COVIDBYDAY$CASES)
for (i in completeat:lastat){
#eval(parse(text=paste0("FULLDATA$","LL",i,"<- NA",sep=""))) 
  eval(parse(text=paste0("FULLDATA$","LLdeaths",i,"<- NA",sep=""))) 
  eval(parse(text=paste0("FULLDATA$","LLhospday",i,"<- NA",sep=""))) 
}


FULLDATA$LLtotaldeaths <- rowSums(FULLDATA[,
                                     eval(parse(text=paste0("LLdeaths",1,sep=""))):eval(parse(text=paste0("LLdeaths",(nrow(COVIDBYDAY)),sep="")))
                                     ],na.rm = TRUE)
FULLDATA$LLtotalhosp <- rowSums(FULLDATA[,
                                     eval(parse(text=paste0("LLhospday",1,sep=""))):eval(parse(text=paste0("LLhospday",(nrow(COVIDBYDAY)),sep="")))
                                     ],na.rm = TRUE)

casemean <- mean(COVIDBYDAY$CASES,na.rm=TRUE)
deathsmean <- mean(DEATHSBYDAY$CUMSUM,na.rm=TRUE)
deathsmean[is.nan(deathsmean)] <- 1
FULLDATA$LLALL <- (FULLDATA$LLtotalhosp/sqrt(casemean)) + 
  (FULLDATA$LLtotaldeaths/sqrt(deathsmean)) 

#FULLDATA <- FULLDATA[order(-FULLDATA$LLtotalhosp),] 
FULLDATA <- FULLDATA[order(-FULLDATA$LLALL),] #order them

top100 <- head(FULLDATA,100)
source("plotfitsV2.R")
plotfits(top100,102) 
