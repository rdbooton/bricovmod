###############################  COVID-19 MODEL  ##################################
###############################    Ross Booton   ##################################
###############################   APRIL 2020     ##################################
setwd("~/Documents/Bristol/COVID-19")

################################## SPECIFY REGION / AREA ####################################
SPECREGION <- "South West" #"South West" #"England" #

#initialise all functions and data we need 
IN <- function(Q) {
  if (Q==TRUE) {
    ############################### load packages ####################################
    library(pacman)
    pacman::p_load(deSolve,ggplot2,lhs,wesanderson,cowplot,data.table,grid,bayestestR,RColorBrewer,
                   socialmixr,
                   IMIS,dplyr,reshape,pbapply,gridExtra,gmp,gtable,magick,yarrr,tidyr,scales,parallel) 
    ############################### useful functions####################################
    cols2<<- brewer.pal(8,"Dark2")
    cols3<<- brewer.pal(9,"Set1")
    darkcols<<- brewer.pal(8,"Paired")
    getPalette = colorRampPalette(piratepal(palette = "basel",length=10))
    cols <<- c(unname(piratepal(palette = "basel",length=10))[1:6] ,"grey",unname(piratepal(palette = "pony",length=10))[2:5],
               unname(piratepal(palette = "basel",length=10))[7:10],unname(piratepal(palette = "pony",length=10))[7:8],"white")
    ############################### read in data ####################################
    ### from: https://coronavirus.data.gov.uk/#regions
    ALLCASES <<- fread("coronavirus-cases.csv")
    ALLDEATHS <<- fread("coronavirus-deaths.csv")
    ALLDEATHSREGIONAL <<- fread("coronavirus-deaths-regional.csv") #from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
    ALLDEATHS <<- rbind(ALLDEATHS,ALLDEATHSREGIONAL,fill=TRUE)
    J <<- unique(ALLCASES$`Specimen date`)
    ALLDATES <<- J[order(J)]
    ALLDATES<<- seq(from=as.Date(ALLDATES[1]),to=as.Date(last(ALLDATES)), by=1) 
    colnames(ALLCASES)[c(1,2,3,4,5,6)] <<- c("Area","AreaCode","AreaType","Date","CASES","CUMSUM") #DAY 1 is 30th Jan 2020
    
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
    COVIDBYDAY$CUMSUM <<- cumsum(COVIDBYDAY$CASES)
    COVIDBYDAY$CUMSUM[COVIDBYDAY$CASES == 0] <<- NA
    COVIDBYDAY$CASES[COVIDBYDAY$CASES == 0] <<- NA
    COVIDBYDAY <<- COVIDBYDAY[-nrow(COVIDBYDAY),] #remove latest point as not fully reported yet
   # COVIDBYDAY <<-   COVIDBYDAY[COVIDBYDAY$Date>="2020-02-25",] #USE ONLY DATA AFTER 1st MARCH 2020
    LA <<- nrow(COVIDBYDAY)
    #DEATHS only available for "United Kingdom" "England" "Scotland" "Northern Ireland" "Wales"
    colnames(ALLDEATHS)[c(1,2,3,4,5,6)] <<- c("Area","AreaCode","AreaType","Date","CASES","CUMSUM") #DAY 1 is 30th Jan 2020
    DEATHSBYDAY<<-filter(ALLDEATHS, Area == SPECREGION)
    DEATHSBYDAY <<- DEATHSBYDAY[order(DEATHSBYDAY$Date),]
    DD <<- if (SPECREGION %in% unique(ALLDEATHSREGIONAL$`Area name` ) ) {"%d/%m/%y"} else {"%Y-%m-%d"}
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
    DEATHSBYDAY <<- DEATHSBYDAY[-nrow(DEATHSBYDAY),] #re order
    DEATHSBYDAY$CASESORIG <<-  DEATHSBYDAY$CASES
    DEATHSBYDAY$CUMSUMORIG <<- DEATHSBYDAY$CUMSUM
    DEATHSBYDAY$CASES <<-  as.numeric(DEATHSBYDAY$CASES) * 0.839
    DEATHSBYDAY$CUMSUM <<-  as.numeric(DEATHSBYDAY$CUMSUM) * 0.839
    DEATHSBYDAY$CUMSUM[is.na(DEATHSBYDAY$CASES )] <<- NA
  #  DEATHSBYDAY$CASES[DEATHSBYDAY$CASES == 0] <<- NA
    DEATHSFITTING <<- DEATHSBYDAY[complete.cases(DEATHSBYDAY[,-(2:3)]),] #fitting data to use for target fitting
    
    time1 <<- as.numeric(as.Date("2020-03-15") - COVIDBYDAY[1,]$Date + 1)
    time2 <<- as.numeric(as.Date("2020-03-20") - COVIDBYDAY[1,]$Date + 1)
    time3 <<- as.numeric(as.Date("2020-03-23") - COVIDBYDAY[1,]$Date + 1)
    
    #DEATHSBYDAY <<-   DEATHSBYDAY[DEATHSBYDAY$Date>="2020-02-25",] #USE ONLY DATA AFTER 1st MARCH 2020
    
   # DEATHSBYDAY <<- DEATHSBYDAY[-c(1:chop),]
    ###DEMOGRAPHIC DATA
    DEMOALL <<- fread("DEMOGRAPHY.csv")
    #colnames(DEMOALL) <<- c("Name",0:90)
    DEMO<<-filter(DEMOALL, Name == SPECREGION)
    DEMO<<- DEMO[,-c(1:2)]
    ###POLYMOD DATA 
    data(polymod)
    AGELIM <<- c(0,12,17,19,25,40,50,60,70)
    POLYDATA<<-   contact_matrix(polymod, countries = "United Kingdom", age.limits = AGELIM, split = TRUE)
    Z <<- AGELIM 
    AGEPOP<<-c(sum(DEMO[0:(Z[2])]), 
               sum(DEMO[(Z[2]+1):(Z[3])]),
               sum(DEMO[(Z[3]+1):(Z[4])]),
               sum(DEMO[(Z[4]+1):(Z[5])]),
               sum(DEMO[(Z[5]+1):(Z[6])]),
               sum(DEMO[(Z[6]+1):(Z[7])]),
               sum(DEMO[(Z[7]+1):(Z[8])]),
               sum(DEMO[(Z[8]+1):(Z[9])]),
               sum(DEMO[(Z[9]+1):(ncol(DEMO))]))
    POP <<- POLYDATA$demography
    AGEGRP <<- as.matrix(POLYDATA$matrix)
    N_AGE <<- ncol(AGEGRP)
    N_COMP <<- 11
    AGEGRP2 <<- AGEGRP
    rownames(AGEGRP2) <<- c("1","2","3","4","5","6","7","8","9")
    colnames(AGEGRP2) <<- c("1","2","3","4","5","6","7","8","9")
    #DAY ONE IS 30th JAN 2020
    LAMBDA <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    OMEGA <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    XI <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    DELTA <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    COMP1 <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    COMP2 <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    COMP3 <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    dy = array(0, dim = c(N_COMP, N_AGE)) #8 states and 16 age groups of 5 years
    CONTACTMATRIX <<- AGEGRP
    UR1max <<- 0 #set to zero to hit the line as best we can 
    set.seed(3333) # 
    }}
B<-IN(TRUE)

################         FUNC: DESOLVE FUNCTION           ######################
XY.model <- function(time, y, params) {#XY.model is the function for solving our ODEs
  dy = array(0, dim = c(N_COMP, N_AGE)) #8 states and 16 age groups of 5 years
  y = array(y,  dim = c(N_COMP, N_AGE))
  kappa <- params["kappa"][[1]]
  rgnonIC <-params["rgnonIC"][[1]]
 # rgIC <-params["rgIC"][[1]]
  delta <- params["delta"][[1]]
  rho <- params["rho"][[1]]
  eta <- c(params["eta"][[1]])
  alpha <- c(params["alpha"][[1]])
  mu <- c(params["mu"][[1]])
  gamma <- c(params["gamma1"][[1]],params["gamma2"][[1]],params["gamma3"][[1]],params["gamma4"][[1]],params["gamma5"][[1]],params["gamma6"][[1]],params["gamma7"][[1]],params["gamma8"][[1]],params["gamma9"][[1]])
  omega <- c(params["omega1"][[1]],params["omega2"][[1]],params["omega3"][[1]],params["omega4"][[1]],params["omega5"][[1]],params["omega6"][[1]],params["omega7"][[1]],params["omega8"][[1]],params["omega9"][[1]])
  psi <- c(params["psi"][[1]])
  betaA <- rep(c(params["betaA"][[1]]),N_AGE) 
  betaI <- rep(c(params["betaI"][[1]]),N_AGE) 
  LAMBDA[1:3,1:3] <- LAMBDA[1:3,1:3]*params["homeschool"][[1]]
  #LOCKDOWN matrix occured on 24th March 2020, puclic events banned, lockdown ordered
  OMEGA <- OMEGA*params["lockdown"][[1]]
  #DISTANCING MATRIX  #advice to avoid pubs, clubs, theatres and other public institutions occured on 16th March 2020
  XI <- XI*params["distancing"][[1]]
  #EMPLOYMENT MATRIX
  DELTA[4:7,4:7] <- DELTA[4:7,4:7]*params["employment"][[1]]
  #######################  FOI  calculation  #######################
  sumN = apply(y,c(2),sum ) #total in each age group N_g
  sumX = apply(y,c(1),sum ) #total in each compartment X_i
  FOIMAT <- array(0,dim=c(N_COMP,N_AGE))
  lambda <- array(0,dim=N_AGE)

  if (time <= time1)  {#NORMAL BEFORE 16TH MARCH 2020
    CONTACTMATRIX <- AGEGRP} else if ((time >=time1) & (time < time2))  {#SOCIAL DISTANCING ENCOURAGED 16th MARCH 2020
      CONTACTMATRIX <- AGEGRP*XI} else if ((time >=time2) & (time < time3)) {#SCHOOLS CLOSED 21st MARCH 2020
        #COMP1 <- (LAMBDA <= XI)*LAMBDA + (LAMBDA > XI)*XI #take min of lambda and xi 
        CONTACTMATRIX <- AGEGRP* ((LAMBDA <= XI)*LAMBDA + (LAMBDA > XI)*XI)} else {# { #LOCKDOWN after 24th MARCH 2020
          COMP1 <- ((LAMBDA <= XI)*LAMBDA + (LAMBDA > XI)*XI) #take min of lambda and xi 
          COMP2 <- ((DELTA <= OMEGA)*DELTA + (DELTA > OMEGA)*OMEGA)
          COMP3 <- ((COMP1 <= COMP2)*COMP1 + (COMP1 > COMP2)*COMP2) #take min of all
          CONTACTMATRIX <- AGEGRP*COMP3} 
  for (i in 1:N_AGE){
    for (g in 1:N_AGE){
      FOIMAT[i,g] <- CONTACTMATRIX[i,g]*( (betaA[i]*y[4,i]/sumN[g]) + (betaI[i]*y[5,i]/sumN[g]) )
    }}
  lambda <- apply(FOIMAT,c(2),sum ) #add up to give vector of lambda_g
  epsilon <- 1- rgnonIC  #(pi * (1- rgnonIC)) + (1-pi)
 # omega <- 1 - rgIC #(squiggle * (1- rgIC)) + (1-squiggle)
  ###############################    MODEL EQUATIONS     #####################################
  for (g in 1:N_AGE){
    #### Susceptible = S ####
    dy[1,g] <- -lambda[g] * y[1,g] 
    #### Shielded = P ####
    dy[2,g] <- 0 #-lambda[g] * y[2,g] 
    #### Exposed = E ####
    dy[3,g] <- lambda[g] * y[1,g] - eta * y[3,g]
    #### Asymptomatic = A####
    dy[4,g] <- eta * (1 - delta)* y[3,g] - alpha * y[4,g]
    #### Infectious  = I ####
    dy[5,g] <- eta * delta * y[3,g] - mu * y[5,g ]
    #### Hospitalised in non- IC bed = H ####
    dy[6,g] <-  mu * gamma[g] * y[5, g] - rho* y[6,g]
    #### Hospitalised in IC bed = C ####
    dy[7,g] <-  epsilon*rho* y[6,g] - psi * y[7,g]
    #### Recovering in non- IC bed = W ####
    dy[8,g] <- 0   #(1-epsilon)*rho*y[6,g] + chi*y[9,g] - phi*y[8,g] 
    #### Recovering in IC bed = B ####
    dy[9,g] <- 0  #psi * (1 - omega) * y[7,g] - chi * y[9,g]
    #### Recovered ####
    dy[10,g] <- alpha * y[4,g]  + mu * (1 - gamma[g]) * y[5, g] + psi * (1 - omega[g]) * y[7,g] + (1 - kappa) * (1 - epsilon) * rho  * y[6,g]
    #### Dead ####
    dy[11,g] <-  psi * omega[g] * y[7, g] + (1- epsilon)*kappa*rho*y[6,g]
    }
  list(as.vector(dy))
}


################        EPID SIMULATOR        ######################
epid.start <- 0
epid <- function(eta,alpha,delta,mu,
                 gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,
                 psi,chi,betaA,betaI, rho,phi,w,yprop,rgnonIC,b, omega1, omega2, omega3, omega4, omega5, omega6, omega7, omega8, omega9, kappa,
                 homeschool,lockdown,distancing,employment,initialinfected,gamma8,gamma9, UR1, #UR2, UR3, UR4, 
                 epid.duration = nrow(COVIDBYDAY) - 1, #length of simulation
                 func.indicator) #plot or no plot?
{  ##set up initial population in each state for each group
  foo <- array(1:(N_COMP*N_AGE),dim=c(N_COMP,N_AGE)) #temp array used for indexing
  y_vec <- rep(0,(N_COMP*N_AGE))
  AGESHIELD <- c(0,0,0,0,0,0,0,0,0) #NEED TO CHECK THIS 
  y_vec[as.vector(foo[1,])] <- AGEPOP*(1-AGESHIELD) #uninfected age group 1 to N_AGE for SW ENGLAND DEMOGRAPHICS 2018
 # y_vec[as.vector(foo[2,])] <- AGEPOP* AGESHIELD
  y_vec[as.vector(foo[5,5])] <- initialinfected #seed initialinfected infected in 5TH 25-39 age group , infectious
  names(y_vec) <- paste0("y", 1:(N_COMP*N_AGE))
  init<-y_vec
  #params is the data from outside this function, which is used through the Sampling function
  params <- list(eta=eta,alpha=alpha,mu=mu,delta=delta,
                 gamma1=gamma1, gamma2=gamma2, gamma3=gamma3,gamma4=gamma4, gamma5=gamma5, gamma6=gamma6,gamma7=gamma7, 
                 psi=psi,chi=chi,betaA=betaA,betaI=betaI,rho=rho,phi=phi,w=w,yprop=yprop,rgnonIC=rgnonIC,b=b,
                 omega1=omega1, omega2=omega2, omega3=omega3, omega4=omega4, 
                 omega5=omega5, omega6=omega6, omega7=omega7, omega8=omega8, omega9=omega9,
                 kappa=kappa,
                 homeschool=homeschool,lockdown=lockdown,distancing=distancing,employment=employment,initialinfected=initialinfected,gamma8=gamma8,gamma9=gamma9)
  #print(params)
  ## vectTime is rescaled in order to cope with the solver function		
  vectTime <- c(0,1:epid.duration)
  #run the model using desolve
  out <- as.data.frame(ode(y=init,time=vectTime,func=XY.model,parms=params))
  out$time = out$time+epid.start #rescale the time so that it runs from day 0
  out$temp <- out$time
  out <- out[,-1]
  names(out)[names(out)=="temp"] <- "time"
  
  out<- out %>% dplyr::mutate(N = rowSums(.[as.vector(foo[,])]),
                              S = rowSums(.[as.vector(foo[1,])]),
                              P = rowSums(.[as.vector(foo[2,])]),
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
                              mu*gamma8*rowSums(.[as.vector(foo[5,8])])+ mu*gamma9*rowSums(.[as.vector(foo[5,9])]),
                            
                              new.cases.entering.hospital.cumsum = cumsum(new.cases.entering.hospital),
                            new.cases.entering.IC = rho*(1-rgnonIC)*rowSums(.[as.vector(foo[6,])]),
                        dying.in.IC = psi*omega1*rowSums(.[as.vector(foo[7,1])])+ psi*omega2*rowSums(.[as.vector(foo[7,2])])+
                          psi*omega3*rowSums(.[as.vector(foo[7,3])]) + 
                          psi*omega4*rowSums(.[as.vector(foo[7,4])])+ psi*omega5*rowSums(.[as.vector(foo[7,5])])+
                          psi*omega6*rowSums(.[as.vector(foo[7,6])])+ psi*omega7*rowSums(.[as.vector(foo[7,7])])+
                          psi*omega8*rowSums(.[as.vector(foo[7,8])])+ psi*omega9*rowSums(.[as.vector(foo[7,9])]),
                        
                        recovering.in.IC = psi*(1-omega1)*rowSums(.[as.vector(foo[7,1])])+ psi*(1-omega2)*rowSums(.[as.vector(foo[7,2])])+
                          psi*(1-omega3)*rowSums(.[as.vector(foo[7,3])]) + 
                          psi*(1-omega4)*rowSums(.[as.vector(foo[7,4])])+ psi*(1-omega5)*rowSums(.[as.vector(foo[7,5])])+
                          psi*(1-omega6)*rowSums(.[as.vector(foo[7,6])])+ psi*(1-omega7)*rowSums(.[as.vector(foo[7,7])])+
                          psi*(1-omega8)*rowSums(.[as.vector(foo[7,8])])+ psi*(1-omega9)*rowSums(.[as.vector(foo[7,9])]),
                        dying.in.nonIC = kappa*rho*(1-(1-rgnonIC))*rowSums(.[as.vector(foo[6,])]),
                        recovering.in.nonIC = (1-kappa)*rho*(1-(1-rgnonIC))*rowSums(.[as.vector(foo[6,])])
  )

#print(out)
  out$Date <- out$time + COVIDBYDAY$Date[1]
#print(out$Date )
 # print(out$dying.in.IC)

 # totalcases <- rep(NA,nrow(COVIDBYDAY) )
#  LLcases <- rep(NA,nrow(COVIDBYDAY))
  totaldeaths <-   rep(NA,nrow(COVIDBYDAY))
  LLdeaths <-   rep(NA,nrow(COVIDBYDAY))
 
 #  totalhospday <-   rep(NA,nrow(COVIDBYDAY))
  LLhospday <-   rep(NA,nrow(COVIDBYDAY))
  
   casestemp <- COVIDBYDAY
deathstemp <- DEATHSBYDAY
  
 # casestemp$CUMSUM <- as.numeric(COVIDBYDAY$CUMSUM) + (UR1)*as.numeric(COVIDBYDAY$CUMSUM )
#  casestemp$CASES <- as.numeric(COVIDBYDAY$CASES) + (UR1)*as.numeric(COVIDBYDAY$CASES )
  
 #totalcases <- out$new.cases.entering.hospital.cumsum
 totaldeaths <- out$D[1:(nrow(COVIDBYDAY))]
 totalhospday <- out$new.cases.entering.hospital[1:(nrow(COVIDBYDAY))]
 
#print(deathstemp$CUMSUM)
#print( totaldeaths)
 #LLcases <- (casestemp$CUMSUM)*log(totalcases)  - totalcases #- lfactorial(casestemp$CUMSUM[i])
 
 LLdeaths <- (deathstemp$CUMSUM)*log(totaldeaths)  - totaldeaths #- lfactorial(deathstemp$CUMSUM)
 LLhospday <- (casestemp$CASES)*log(totalhospday)  - totalhospday #- lfactorial(casestemp$CASES)
 
 #totaldeaths <- out$D[1:(nrow(COVIDBYDAY))][which(!is.na(DEATHSBYDAY$CUMSUM))]
 # LLtotal <- sum(LLcases,na.rm=TRUE)
 # LLtotaldeaths <- sum(LLdeaths,na.rm=TRUE)
 # LLtotalhosp <- sum(LLhospday,na.rm=TRUE)
  #print(LLcases)
  #print(LLdeaths)
  
  toReturn <- as.vector(c( #totalcases,
                        #  LLcases, #LLtotal,
                           LLdeaths,# LLtotaldeaths,
                          # totaldeaths,
                          LLhospday)) #, LLtotalhosp))
  names(toReturn) <- c( #paste0("totalcases", 1: nrow(COVIDBYDAY)),
                      #  paste0("LL", 1: length(LLcases)), 
                       # "LLtotal",
                        paste0("LLdeaths",1: length(LLdeaths)),
    #paste0("totaldeaths",1: (length(totaldeaths))),
                       # "LLtotaldeaths",
                        paste0("LLhospday",1: length(LLhospday))) #,  
                       # "LLtotalhosp")
 # print(out)
  if (func.indicator=="returnout"){	
    ## the function returns important information,
    return(out)
  } else if (func.indicator=="returnindicators") {
    return(toReturn)
  } else { #run some plots
   print("no") #male female plot
  }
}

###############      FUNC: Latin Hypercube Sampling (LHS)       ###############
Sampling <- function(aaa){
  Samples_SIR <- randomLHS(aaa, 8)
 # Samples_SIR[,1] <- 0.00622 + (0.0213-0.00622)*Samples_SIR[,1] # gamma4
#  Samples_SIR[,2] <- 0.00622 + (0.070-0.00622)*Samples_SIR[,2] # gamma5
#  Samples_SIR[,3] <- 0.0253 + (0.0868-0.0253)*Samples_SIR[,3] # gamma6
#  Samples_SIR[,4] <- 0.0486 + (0.1670-0.0486)*Samples_SIR[,4] # gamma7
#  Samples_SIR[,5] <- 0.0701 + (0.240-0.0701)*Samples_SIR[,5] # gamma8
#  Samples_SIR[,6] <- 0.0987 + (0.376-0.0987)*Samples_SIR[,6] # gamma9
  
  Samples_SIR[,1] <- 0.04 + (0.08-0.04)*Samples_SIR[,1] #0.02 + (0.15-0.02)*Samples_SIR[,7] # betaA / betaI
  Samples_SIR[,2] <- 0.17 + (0.37-0.17)*Samples_SIR[,2] # https://www.gstatic.com/covid19/mobility/2020-04-05_GB_Mobility_Report_en.pdf
  Samples_SIR[,3] <- 0.3 + (0.9-0.3)*Samples_SIR[,3] # distancing # 
  Samples_SIR[,4] <- 0.7 + (1.0 - 0.7)*Samples_SIR[,4] #  epsilon = 1 - rgnonIC
  Samples_SIR[,5] <-  0.05 + (0.2-0.05)*Samples_SIR[,5]    #kappa
  Samples_SIR[,6] <-  4 + (14-4)*Samples_SIR[,6] #rho 
  Samples_SIR[,7] <-  4 + (12-4)*Samples_SIR[,7]  # psi 
 # Samples_SIR[,8] <-  3 + (14-3)*Samples_SIR[,8]  # alpha 
  Samples_SIR[,8] <-  4 + (14-4)*Samples_SIR[,8]  # mu 
 # Samples_SIR[,16] <- 0 + (1-0)*Samples_SIR[,16] #initial infected
 # Samples_SIR[,17] <- 0 + (0.25-0)*Samples_SIR[,17] #UR1
 # Samples_SIR[,16] <-  0.2 + (0.4-0.2)*Samples_SIR[,16] # omega = 1-rgIC #we are sampling for rgIC
  paramsMat_SIR <- data.frame(eta=1/5.1, 
                              alpha= (1/5), 
                              delta=0.821, 
                              mu= (1/ Samples_SIR[,8]),
                              gamma1=0 ,gamma2=0.0408/100 ,gamma3=0.0408/100 ,
                              gamma4=0.0104,gamma5=0.0223,
                              gamma6=0.0425,gamma7=0.0816,
                              gamma8=0.118,gamma9=0.175,
                             # omega=0,
                              psi=(1/Samples_SIR[,7]),
                             # chi=0,
                              betaA=Samples_SIR[,1],
                              betaI=Samples_SIR[,1], 
                              homeschool= 0.05,
                              lockdown= Samples_SIR[,2],
                              distancing= Samples_SIR[,3],
                              employment= 0.44,
                           
                              rgnonIC= Samples_SIR[,4],
                              kappa =Samples_SIR[,5],
                              rho = 1/(Samples_SIR[,6]), 
                              initialinfected = 1, #Samples_SIR[,16] ,
                              UR1 = 0,
                             omega1 = 0, omega2 = 0, omega3 = 0.221, 
                            omega4 = 0.221, omega5 = 0.221, omega6 = 0.266,
                             omega7 = 0.423, omega8 = 0.574, omega9 = 0.683
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
                                   
                                   "time3", "epid.start", "COVIDBYDAY", "DEATHSBYDAY"), envir=environment())
  
  clusterEvalQ(cl=cl,c(library(deSolve, library(data.table), library(magrittr))))
  
  
  
  outMat = parApply(cl, x,1,function(x) { # see below for working example of this function
    
    epid(as.list(x)$eta,as.list(x)$alpha,as.list(x)$delta,as.list(x)$mu,
         
         as.list(x)$gamma1,as.list(x)$gamma2,as.list(x)$gamma3,as.list(x)$gamma4,as.list(x)$gamma5,as.list(x)$gamma6,as.list(x)$gamma7,
         
         as.list(x)$psi,as.list(x)$chi,as.list(x)$betaA,as.list(x)$betaI,as.list(x)$rho,as.list(x)$phi,
         
         as.list(x)$w,as.list(x)$yprop,as.list(x)$rgnonIC,as.list(x)$b,
         
         as.list(x)$omega1, as.list(x)$omega2, as.list(x)$omega3, as.list(x)$omega4, as.list(x)$omega5, as.list(x)$omega6, as.list(x)$omega7, as.list(x)$omega8, as.list(x)$omega9,        
         
         as.list(x)$kappa, as.list(x)$homeschool,as.list(x)$lockdown,as.list(x)$distancing,as.list(x)$employment,as.list(x)$initialinfected,as.list(x)$gamma8,as.list(x)$gamma9,
         
         as.list(x)$UR1, 
         
         epid.duration = nrow(COVIDBYDAY)  - 1 #length of simulation
         
         , func.indicator="returnindicators")})
  
  ResMat =  cbind(bbb,t(outMat))
  write.csv(ResMat,paste0("OUT_", a0,".csv"))
  stopCluster(cl)}


##################         FITTING      ######################## 

Nsamples<- 2000#how many samples? 
p1<-Sampling(Nsamples)  
#Run the simulator outFUN for the Latin-Hypercube samples generated above
ptm <- proc.time() #time run 
outFUN(p1,1 )#the file name.. let's call it OUT_1 so a0=1 here
proc.time() - ptm
timeX = (proc.time() - ptm) / Nsamples #time per run
#how long have you got in seconds?
seconds = 3600 #1 hour
totalin1hour = seconds/timeX[3]
totalin1hour
#FULLDATA_orig <-fread("OUT_1.csv") 
#FULLDATA<-fread("OUT_1.csv")

FULLDATA_orig <-fread("FULLDATA_SW.csv") 
FULLDATA<-fread("FULLDATA_SW.csv")
#FULLDATA<-fread("FULLDATA3.csv") 
#farbackcases = 10 #how far back to fit to?
##Log-likelihood  function
#USE nrow(COVIDBYDAY) - 2 as final 

#Data from around 5 days ago can be considered complete, so set last 5 to = NA
completeat <- length(COVIDBYDAY$CASES)-4
lastat <- length(COVIDBYDAY$CASES)
for (i in completeat:lastat){
#eval(parse(text=paste0("FULLDATA$","LL",i,"<- NA",sep=""))) 
  eval(parse(text=paste0("FULLDATA$","LLdeaths",i,"<- NA",sep=""))) 
  eval(parse(text=paste0("FULLDATA$","LLhospday",i,"<- NA",sep=""))) 
}

#FULLDATA$LLtotalcases <- rowSums(FULLDATA[,
#                                eval(parse(text=paste0("LL",1,sep=""))):eval(parse(text=paste0("LL",(nrow(COVIDBYDAY)),sep="")))
#                               ],na.rm = TRUE)
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

FULLDATA <- FULLDATA[order(-FULLDATA$LLALL),] #order them

FULLDATA$LL <- FULLDATA$LLALL - max(FULLDATA$LLALL) -1  #rescale log likelihood
#Calculate the importance weights
FULLDATA$IW <- exp(FULLDATA$LL)/sum(exp(FULLDATA$LL))
summary(FULLDATA$IW)
#hist(FULLDATA$IW)
head(FULLDATA[order(-FULLDATA$IW),]$IW,10)
# We sample them with replacement using their importance weights (the formal SIR)
ID <- 1:length(FULLDATA$IW)
Size_SIR <- 1000
ID_SIR <- sample(x=ID, size=Size_SIR, replace=TRUE, prob=FULLDATA$IW)
length(unique(ID_SIR))
SIR.subset <- subset(FULLDATA[ID_SIR,]) 
SIR.unique <- unique( SIR.subset ) #length is equal to length(unique(ID_SIR))
#plotfits(SIR.unique,100)
write.csv(SIR.subset,"SIR.subset.csv")
top100 <- head(FULLDATA,100)
#plotfits(rbind(FULLDATA[1:2,],FULLDATA[1:2,]),100)
write.csv(top100,"top100.csv")

source("plotfits.R")
plotfits(top100,183)

#way to extract peak of patients in hospital
BEDPEAK <- plotfits(top100,250) #183
peaktime <- NA 
for (i in 2:101){
  peaktime[i] <-  which.max( BEDPEAK[,i] )}
peaktime<- peaktime[-1]
round(c(quantile(peaktime, c(0.025, 0.975)) ,median=median(peaktime)),4)
COVIDBYDAY$Date[1] + 103.475
COVIDBYDAY$Date[1] + 165
COVIDBYDAY$Date[1] + 138

options(scipen=5)
options(max.print=10000)
#dev.off()

#5 X 6 for 2x1
plotfits(FULLDATA[1:20,],83)


par(mfrow=c(1,1))
mycol <- rgb(255, 0, 0, max = 255, alpha = 100, names = "blue50")
boxplot(FULLDATA_orig$eta,FULLDATA_orig$alpha,FULLDATA_orig$delta,FULLDATA_orig$mu,FULLDATA_orig$omega9,FULLDATA_orig$psi, FULLDATA_orig$chi,
        FULLDATA_orig$betaA, FULLDATA_orig$betaI,FULLDATA_orig$homeschool,FULLDATA_orig$lockdown,FULLDATA_orig$distancing,FULLDATA_orig$employment,FULLDATA_orig$rgnonIC,FULLDATA_orig$omega9,FULLDATA_orig$rho,FULLDATA_orig$phi,FULLDATA_orig$kappa,
        col=grey(0.6),names=c(expression(eta),expression(alpha),expression(delta),expression(mu),expression(omega9),expression(psi),expression(chi),
                              expression(betaA),expression(betaI),expression(homeschool),expression(lockdown),expression(distancing),expression(employment),expression(rgnonIC),expression(omega9),expression(rho),expression(phi),expression(kappa) ),
        show.names=TRUE,main="",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2)
boxplot(FULLDATA$eta,FULLDATA$alpha,FULLDATA$delta,FULLDATA$mu,FULLDATA$omega9,FULLDATA$psi, FULLDATA$chi,
        FULLDATA$betaA, FULLDATA$betaI,FULLDATA$homeschool,FULLDATA$lockdown,FULLDATA$distancing,FULLDATA$employment,FULLDATA$rgnonIC,FULLDATA$rgIC,FULLDATA$rho,FULLDATA$phi,FULLDATA$kappa,
        col=mycol,show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2)


round(1/c(quantile(FULLDATA$eta, c(0.025, 0.975)) ,median=median(FULLDATA$eta)),1) #
round(c(quantile(FULLDATA$delta, c(0.025, 0.975)) ,median=median(FULLDATA$delta)),3) #
round(1/c(quantile(FULLDATA$alpha, c(0.025, 0.975)) ,median=median(FULLDATA$alpha)),1)
round(1/c(quantile(FULLDATA$mu, c(0.025, 0.975)) ,median=median(FULLDATA$mu)),1) #
round(1/c(quantile(FULLDATA$rho, c(0.025, 0.975)) ,median=median(FULLDATA$rho)),1) #
round(c(quantile(FULLDATA$gamma1, c(0.025, 0.975)) ,median=median(FULLDATA$gamma1)),4)*100 #
round(c(quantile(FULLDATA$gamma2, c(0.025, 0.975)) ,median=median(FULLDATA$gamma2)),4)*100 #
round(c(quantile(FULLDATA$gamma3, c(0.025, 0.975)) ,median=median(FULLDATA$gamma3)),4)*100 #
round(c(quantile(FULLDATA$gamma4, c(0.025, 0.975)) ,median=median(FULLDATA$gamma4)),4)*100 #
round(c(quantile(FULLDATA$gamma5, c(0.025, 0.975)) ,median=median(FULLDATA$gamma5)),4)*100#
round(c(quantile(FULLDATA$gamma6, c(0.025, 0.975)) ,median=median(FULLDATA$gamma6)),4)*100#
round(c(quantile(FULLDATA$gamma7, c(0.025, 0.975)) ,median=median(FULLDATA$gamma7)),4)*100#
round(c(quantile(FULLDATA$gamma8, c(0.025, 0.975)) ,median=median(FULLDATA$gamma8)),4)*100#
round(c(quantile(FULLDATA$gamma9, c(0.025, 0.975)) ,median=median(FULLDATA$gamma9)),4)*100#
round(1/c(quantile(FULLDATA$psi, c(0.025, 0.975)) ,median=median(FULLDATA$psi)),1) #

100-round(c(quantile(FULLDATA$rgnonIC, c(0.025, 0.975)) ,median=median(FULLDATA$rgnonIC)),3)*100 #
100-round(c(quantile(FULLDATA$omega8, c(0.025, 0.975)) ,median=median(FULLDATA$omega8)),3)*100 #omega8
100-round(c(quantile(FULLDATA$omega9, c(0.025, 0.975)) ,median=median(FULLDATA$omega9)),3)*100 #omega9
round(c(quantile(FULLDATA$kappa, c(0.025, 0.975)) ,median=median(FULLDATA$kappa)),3)*100 #

round(c(quantile(FULLDATA$homeschool, c(0.025, 0.975)) ,median=median(FULLDATA$homeschool)),3)*100 #
100-round(c(quantile(FULLDATA$distancing, c(0.025, 0.975)) ,median=median(FULLDATA$distancing)),3)*100 #
round(c(quantile(FULLDATA$employment, c(0.025, 0.975)) ,median=median(FULLDATA$employment)),3)*100 #
100 - round(c(quantile(FULLDATA$lockdown, c(0.025, 0.975)) ,median=median(FULLDATA$lockdown)),3)*100 #

round(c(quantile(FULLDATA$betaA, c(0.025, 0.975)) ,median=median(FULLDATA$betaA)),3) #


