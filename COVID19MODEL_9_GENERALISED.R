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
    COVIDBYDAY$CUMSUM[COVIDBYDAY$CUMSUM == 0] <<- NA
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
    DEATHSBYDAY$CUMSUM[DEATHSBYDAY$CASES == 0] <<- NA
    DEATHSBYDAY <<- DEATHSBYDAY[-nrow(DEATHSBYDAY),] #remove latest point as not fully reported yet
    
    
    time1 <<- as.numeric(as.Date("2020-03-16") - COVIDBYDAY[1,]$Date + 1)
    time2 <<- as.numeric(as.Date("2020-03-21") - COVIDBYDAY[1,]$Date + 1)
    time3 <<- as.numeric(as.Date("2020-03-24") - COVIDBYDAY[1,]$Date + 1)
    
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
    CONTACTMATRIX <<- AGEGRP}}
B<-IN(TRUE)

################         FUNC: DESOLVE FUNCTION           ######################
XY.model <- function(time, y, params) {#XY.model is the function for solving our ODEs
  dy = array(0, dim = c(N_COMP, N_AGE)) #8 states and 16 age groups of 5 years
  y = array(y,  dim = c(N_COMP, N_AGE))
  kappa <- params["kappa"][[1]]
  rgnonIC <-params["rgnonIC"][[1]]
  rgIC <-params["rgIC"][[1]]
  delta <- params["delta"][[1]]
  rho <- params["rho"][[1]]
  eta <- c(params["eta"][[1]])
  alpha <- c(params["alpha"][[1]])
  mu <- c(params["mu"][[1]])
  gamma <- c(params["gamma1"][[1]],params["gamma2"][[1]],params["gamma3"][[1]],params["gamma4"][[1]],params["gamma5"][[1]],params["gamma6"][[1]],params["gamma7"][[1]],params["gamma8"][[1]],params["gamma9"][[1]])
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
  omega <- 1 - rgIC #(squiggle * (1- rgIC)) + (1-squiggle)
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
    dy[10,g] <- alpha * y[4,g]  + mu * (1 - gamma[g]) * y[5, g] + psi * (1 - omega) * y[7,g] + (1 - kappa) * (1 - epsilon) * rho  * y[6,g]
    #### Dead ####
    dy[11,g] <-  psi * omega * y[7, g] + (1- epsilon)*kappa*rho*y[6,g]
    }
  list(as.vector(dy))
}


################        EPID SIMULATOR        ######################
epid.start <- 0
epid <- function(eta,alpha,delta,mu,
                 gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,
                 psi,chi,betaA,betaI, rho,phi,w,yprop,rgnonIC,b, rgIC, kappa,
                 homeschool,lockdown,distancing,employment,initialinfected,gamma8,gamma9, UR1, #UR2, UR3, UR4, 
                 epid.duration = LA + 2, #length of simulation
                 func.indicator) #plot or no plot?
{  ##set up initial population in each state for each group
  foo<- array(1:(N_COMP*N_AGE),dim=c(N_COMP,N_AGE)) #temp array used for indexing
  y_vec <- rep(0,(N_COMP*N_AGE))
  AGESHIELD <- c(0,0,0,0,0,0,0,0,0) #NEED TO CHECK THIS 
  y_vec[as.vector(foo[1,])] <- AGEPOP*(1-AGESHIELD) #uninfected age group 1 to N_AGE for SW ENGLAND DEMOGRAPHICS 2018
  y_vec[as.vector(foo[2,])] <- AGEPOP* AGESHIELD
  y_vec[as.vector(foo[5,])] <- initialinfected #seed initialinfected infected in each age group , infectious
  names(y_vec) <- paste0("y", 1:(N_COMP*N_AGE))
  init<-y_vec
  #params is the data from outside this function, which is used through the Sampling function
  params <- list(eta=eta,alpha=alpha,mu=mu,delta=delta,
                 gamma1=gamma1, gamma2=gamma2, gamma3=gamma3,gamma4=gamma4, gamma5=gamma5, gamma6=gamma6,gamma7=gamma7, 
                 psi=psi,chi=chi,betaA=betaA,betaI=betaI,rho=rho,phi=phi,w=w,yprop=yprop,rgnonIC=rgnonIC,b=b,rgIC=rgIC,kappa=kappa,
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
                        dying.in.IC = psi*(1-rgIC)*rowSums(.[as.vector(foo[7,])]),
                        recovering.in.IC = psi*(1-(1-rgIC))*rowSums(.[as.vector(foo[7,])]),
                        dying.in.nonIC = kappa*rho*(1-(1-rgnonIC))*rowSums(.[as.vector(foo[6,])]),
                        recovering.in.nonIC = (1-kappa)*rho*(1-(1-rgnonIC))*rowSums(.[as.vector(foo[6,])])
  )


  out$Date <- out$time + COVIDBYDAY$Date[1]

 # print(out$dying.in.IC)

  totalcases <- rep(NA,nrow(COVIDBYDAY) )
  LLcases <- rep(NA,nrow(COVIDBYDAY))
  totaldeaths <-   rep(NA,nrow(COVIDBYDAY))
  LLdeaths <-   rep(NA,nrow(COVIDBYDAY))
 
   totalhospday <-   rep(NA,nrow(COVIDBYDAY))
  LLhospday <-   rep(NA,nrow(COVIDBYDAY))
  
   casestemp <- COVIDBYDAY
  deathstemp <- DEATHSBYDAY
  
  casestemp$CUMSUM <- as.numeric(COVIDBYDAY$CUMSUM) + (UR1)*as.numeric(COVIDBYDAY$CUMSUM )
  casestemp$CASES <- as.numeric(COVIDBYDAY$CASES) + (UR1)*as.numeric(COVIDBYDAY$CASES )
  
 totalcases <- out$new.cases.entering.hospital.cumsum
 totaldeaths <- out$D
 totalhospday <- out$new.cases.entering.hospital
 LLcases <- (casestemp$CUMSUM)*log(totalcases)  - totalcases #- lfactorial(casestemp$CUMSUM[i])
 LLdeaths <- (deathstemp$CUMSUM)*log(totaldeaths)  - totaldeaths #- lfactorial(deathstemp$CUMSUM[i])
 LLhospday <- (casestemp$CASES)*log(totalhospday)  - totalhospday #- lfactorial(casestemp$CASES[i])
 

  LLtotal <- sum(LLcases,na.rm=TRUE)
  LLtotaldeaths <- sum(LLdeaths,na.rm=TRUE)
  LLtotalhosp <- sum(LLhospday,na.rm=TRUE)
  #print(LLcases)
  #print(LLdeaths)
  
  toReturn <- as.vector(c( #totalcases,
                          LLcases,LLtotal,
                           LLdeaths, LLtotaldeaths,
                          LLhospday, LLtotalhosp))
  names(toReturn) <- c( #paste0("totalcases", 1: nrow(COVIDBYDAY)),
                        paste0("LL", 1: length(LLcases)), 
                        "LLtotal",
                        paste0("LLdeaths",1: length(LLdeaths)),
                        "LLtotaldeaths",
                        paste0("LLhospday",1: length(LLhospday)), 
                        "LLtotalhosp")
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
  Samples_SIR <- randomLHS(aaa, 18)
  Samples_SIR[,1] <- 0.016 + (0.208-0.016)*Samples_SIR[,1] # gamma4
  Samples_SIR[,2] <- 0.143 + (0.208-0.143)*Samples_SIR[,2] # gamma5
  Samples_SIR[,3] <- 0.143 + (0.283-0.143)*Samples_SIR[,3] # gamma6
  Samples_SIR[,4] <- 0.212 + (0.301-0.212)*Samples_SIR[,4] # gamma7
  Samples_SIR[,5] <- 0.205 + (0.435-0.205)*Samples_SIR[,5] # gamma8
  Samples_SIR[,6] <- 0.286 + (0.703-0.286)*Samples_SIR[,6] # gamma9
  Samples_SIR[,7] <- 0.03 + (0.08-0.03)*Samples_SIR[,7] #0.02 + (0.15-0.02)*Samples_SIR[,7] # betaA / betaI
  #Samples_SIR[,8] <- 0.01 + (0.1-0.01)*Samples_SIR[,8] # homeschool #0-10% of children are going to school
  Samples_SIR[,8] <- 0.17 + (0.37-0.17)*Samples_SIR[,8] # https://www.gstatic.com/covid19/mobility/2020-04-05_GB_Mobility_Report_en.pdf
  Samples_SIR[,9] <- 0.3 + (0.9-0.3)*Samples_SIR[,9] # distancing # 
  # Samples_SIR[,11] <- 0.36 + (0.52-0.36)*Samples_SIR[,11] # employment 
  Samples_SIR[,10] <- 0.7 + (1.0 - 0.7)*Samples_SIR[,10] #  epsilon = 1 - rgnonIC
  Samples_SIR[,11] <-  0 + (0.2-0)*Samples_SIR[,11]    #kappa
  Samples_SIR[,12] <- 4 + (19-4)*Samples_SIR[,12] #rho 
  Samples_SIR[,13] <-  4 + (12-4)*Samples_SIR[,13]  # psi 
  Samples_SIR[,14] <-  3 + (10-3)*Samples_SIR[,14]  # alpha 
  Samples_SIR[,15] <-  4 + (10-4)*Samples_SIR[,15]  # mu 

  Samples_SIR[,16] <- 0 + (2-0)*Samples_SIR[,16] #initial infected
  Samples_SIR[,17] <- 0 + (0.25-0)*Samples_SIR[,17] #UR1
  Samples_SIR[,18] <- 0.299 + (0.699-0.299)*Samples_SIR[,18] # omega = 1-rgIC #we are sampling for rgIC
  paramsMat_SIR <- data.frame(eta=1/5.1, 
                              alpha=(1/Samples_SIR[,14]), 
                              delta=0.821, 
                              mu= (1/ Samples_SIR[,15]),
                              gamma1=0.0205 ,gamma2=0.0205 ,gamma3=0.0205 ,
                              gamma4=Samples_SIR[,1],gamma5=Samples_SIR[,2],
                              gamma6=Samples_SIR[,3],gamma7=Samples_SIR[,4],
                              gamma8=Samples_SIR[,5],gamma9=Samples_SIR[,6],
                             # omega=0,
                              psi=(1/Samples_SIR[,13]),
                             # chi=0,
                              betaA=Samples_SIR[,7],
                              betaI=Samples_SIR[,7], 
                              homeschool= 0.05,
                              lockdown= Samples_SIR[,8],
                              distancing= Samples_SIR[,9],
                              employment= 0.44,
                           
                              rgnonIC= Samples_SIR[,10],
                              kappa =Samples_SIR[,11],
                             
                              rgIC=  Samples_SIR[,18],
                              rho = 1/(Samples_SIR[,12]), 
                               
                              initialinfected = Samples_SIR[,16] , #1, # 
                              UR1 = Samples_SIR[,17]) #Samples_SIR[,15]) #, UR2 = Samples_SIR[,18],
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
         
         as.list(x)$w,as.list(x)$yprop,as.list(x)$rgnonIC,as.list(x)$b, as.list(x)$rgIC, as.list(x)$kappa,
         
         as.list(x)$homeschool,as.list(x)$lockdown,as.list(x)$distancing,as.list(x)$employment,as.list(x)$initialinfected,as.list(x)$gamma8,as.list(x)$gamma9,
         
         as.list(x)$UR1, 
         
         epid.duration = LA + 2 #length of simulation
         
         , func.indicator="returnindicators")})
  

  
  ResMat =  cbind(bbb,t(outMat))
  write.csv(ResMat,paste0("OUT_", a0,".csv"))
  stopCluster(cl)}


##################         FITTING      ######################## 
UR1max <- 0.25
Nsamples<- 1000 #how many samples? 
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
FULLDATA_orig <-fread("OUT_1.csv") 
FULLDATA<-fread("OUT_1.csv")
#FULLDATA<-fread("FULLDATA3.csv") 
#farbackcases = 10 #how far back to fit to?
##Log-likelihood  function
#USE nrow(COVIDBYDAY) - 2 as final 
#FULLDATA$LLtotal <- rowSums(FULLDATA[,
 #                               eval(parse(text=paste0("LL",(nrow(COVIDBYDAY)-farbackcases),sep=""))):eval(parse(text=paste0("LL",(nrow(COVIDBYDAY)),sep="")))
  #                             ],na.rm = TRUE)
#FULLDATA <- FULLDATA[order(-FULLDATA$LLtotal),] #order them

#plotfits(rbind(FULLDATA[1:5,],FULLDATA[1:5,]),70)

#ROWS<-nrow(FULLDATA)
#FULLDATA$LLALL <- FULLDATA$LLtotal + FULLDATA$LLtotaldeaths
#OR METHOD WITH SQRT OF N TO SCALE LL FOR EACH METRIC
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
Size_SIR <- 10000
ID_SIR <- sample(x=ID, size=Size_SIR, replace=TRUE, prob=FULLDATA$IW)
length(unique(ID_SIR))
SIR.subset <- subset(FULLDATA[ID_SIR,]) 
SIR.unique <- unique( SIR.subset ) #length is equal to length(unique(ID_SIR))
#plotfits(SIR.unique,100)

FULLDATA <- head(FULLDATA,100)
#plotfits(rbind(FULLDATA[1:2,],FULLDATA[1:2,]),100)

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
                b=SIR.unique$b[i],rgIC=SIR.unique$rgIC[i],kappa=SIR.unique$kappa[i],
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
  
  datasymp =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    datasymp[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$I
  }
  datatemp <- datasymp[,2:ncol(datasymp)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  datasymp$median<- datatemp$median
  datasymp$lower<- datatemp$lower
  datasymp$upper<- datatemp$upper
  
  dataasymp =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataasymp[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$A
  }
  datatemp <- dataasymp[,2:ncol(dataasymp)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  datatemp$lower <- apply(  datatemp ,1,quantile,probs=c(0.025),na.rm=TRUE)
  datatemp$upper <- apply(  datatemp ,1,quantile,probs=c(0.975),na.rm=TRUE)
  dataasymp$median<- datatemp$median
  dataasymp$lower<- datatemp$lower
  dataasymp$upper<- datatemp$upper
  
  datacom <- data.frame("Date"=datasymp$Date,"Symptomatic"=datasymp$median,"Asymptomatic"=dataasymp$median)
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
  dataE$median<- datatemp$median
  
  dataAI =data.frame(Date=plot1$Date)
  for (i in 1:nrow(SIR.unique)) {
    dataAI[,1+i] <- eval(parse( text=paste("plot", i, sep = "") ))$E +eval(parse( text=paste("plot", i, sep = "") ))$I
  }
  datatemp <- dataAI[,2:ncol(dataAI)]
  datatemp$median <- apply(  datatemp ,1,median,na.rm=TRUE)
  dataAI$median<- datatemp$median
  
  
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
  dataR$median<- datatemp$median
  
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
  
  dataleavingIC <- data.frame(Date=datasymp$Date,
                           Recovery=datarecovering.in.IC$median,
                           Death=datadying.in.IC$median)
  dataleavingIC <- gather(dataleavingIC, key = "variable", value = "value",
                          Recovery, Death)
  
  dataleavingnonIC <- data.frame(Date=datasymp$Date,
                              Recovery=datarecovering.in.nonIC$median,
                              Death=datadying.in.nonIC$median)
  dataleavingnonIC <- gather(dataleavingnonIC, key = "variable", value = "value",
                          Recovery, Death)
#  print(datadying.in.IC)
  
  dataALL <- data.frame(Date=datasymp$Date,
                        Susceptible=dataSP$median,
                        Exposed=dataE$median,
                        Infected=dataAI$median,Hospital=dataHWCB$median, Recovered=dataR$median,
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
                    Exposed, Infected,
                    Recovered)
  
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
                     
                     "Cumulative"= round(cumsum(datanewcases$median),0),
                     "New_diag" = round(datanewcases$median,0))
  #"Deaths_in_hospital" = round(dataD$median,0))
  
  colnames(datatab) <- c("Date",
                         "Cumuative cases",#"%IC bed\ndemand",
                         "New diagnoses\nin 24hr")#,"Deaths in\nhospital")
  datatab$Date <- as.Date(datatab$Date, "%d/%m/%y")
  datatabsummary <- datatab[seq(from=1,to=nrow(datatab),by=5),]
  
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
                geom_vline(xintercept=COVIDBYDAY[time1,]$Date, colour=wes_palette("Zissou1",n=5)[1],linetype="dotted") ,
                geom_vline(xintercept=COVIDBYDAY[time2,]$Date, colour=wes_palette("Zissou1",n=5)[3],linetype="dotted") ,
                geom_vline(xintercept=COVIDBYDAY[time3,]$Date, colour=wes_palette("Zissou1",n=5)[5],linetype="dotted") ,
                theme(plot.title = element_text(hjust = 0.5,size=13))
  )
#  print(datanew.cases.entering.hospital.cumsum)
  P1<- ggplot(data= datanew.cases.entering.hospital.cumsum, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    annotate("text", x = COVIDBYDAY[time1,]$Date -1, y = max(COVIDBYDAY$MAX,datanew.cases.entering.hospital.cumsum$upper,na.rm=TRUE)/2, label = "Social Distancing",angle=90,colour=wes_palette("Zissou1",n=5)[1],size=3)+
    annotate("text", x = COVIDBYDAY[time2,]$Date -1, y = max(COVIDBYDAY$MAX,datanew.cases.entering.hospital.cumsum$upper,na.rm=TRUE)/2, label = "School Closures",angle=90,colour=wes_palette("Zissou1",n=5)[3],size=3)+
    annotate("text", x = COVIDBYDAY[time3,]$Date -1, y = max(COVIDBYDAY$MAX,datanew.cases.entering.hospital.cumsum$upper,na.rm=TRUE)/2, label = "Lockdown",angle=90,colour=wes_palette("Zissou1",n=5)[5],size=3)+
    geom_errorbar(data=COVIDBYDAY,aes(x=Date,ymin=CUMSUM, ymax=CUMSUM*(1+UR1max)), width=0.1,colour=cols[5],alpha=0.5)+
    geom_point(data=COVIDBYDAY,aes(x=Date,y=CUMSUM,colour="Observed"), width=0.8,size=2)+#,colour="black")+
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
    scale_x_date(breaks = pretty_breaks(18))
  
  
  
  P4<- ggplot(data= databeds, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
   # geom_errorbar(data=beds,aes(x=Date,ymin=nonIC, ymax=nonIC*(1+UR3max)), width=0.8,size=1,colour=cols[5],alpha=0.25)+
  #  geom_point(data=beds,aes(x=Date,y=nonIC), width=0.8,size=2,colour=cols[5])+

    geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.25,fill = "#666666",size=0.5)+
    geom_ribbon(aes(ymin=lower2, ymax=upper2), linetype=0, alpha=0.5,size=0.5,fill="#666666")+
    geom_line(aes(y=median,colour="Number of confirmed COVID-19 patients in non-IC beds"),size=1.5,alpha=0.8,colour="black") +
    ylab(label="")+
    ggtitle(label="COVID-19 patients occupying non-beds")+
    plotdet+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) #+
    #geom_hline(yintercept=nonICbedcapacity, colour="orangered1",linetype="solid") #+
    #annotate("text", x = COVIDBYDAY[nrow(COVIDBYDAY)/2,]$Date, y = nonICbedcapacity-(nonICbedcapacity*0.1), label = "non-IC bed capacity",colour="orangered1")
  
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
    ggtitle(label="Hospitalised in IC last 24h")+
    plotdet+ 
    scale_fill_manual(values = cols[11],name= "" #guide = guide_legend(reverse = TRUE)
    )+ theme(legend.position = "none") + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 
  
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
    geom_point(data=COVIDBYDAY,aes(y=CASES), width=0.8,size=2,colour=cols[5])+
    ylab(label="")+
    ggtitle(label="Hospitalised in non-IC last 24h")+
    plotdet+ 
    scale_fill_manual(values = cols[11],name= "" #guide = guide_legend(reverse = TRUE)
    )+ theme(legend.position = "none")+theme(axis.title.x = element_blank(), axis.text.x = element_blank())
    
  
  P6 <- ggplot(data= dataD, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
   # geom_errorbar(data=deaths,aes(x=Date,ymin=CUMSUM, ymax=CUMSUM*(1+UR4max)), width=0.8,size=1,colour=cols[5],alpha=0.25)+
    geom_point(data=DEATHSBYDAY,aes(y=CUMSUM), width=0.8,size=2,colour=cols[5])+
    # geom_errorbar(data=beds,aes(x=Date,ymin=ICMIN, ymax=ICMAX), width=0.8,size=1,colour="black",alpha=0.25)+theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    # geom_errorbar(data=beds,aes(x=Date,ymin=ICMIN, ymax=ICMAX), width=0.8,size=1,colour="grey",alpha=0.25)+theme(axis.title.x = element_blank(), axis.text.x = element_blank())+
    geom_ribbon(aes(ymin=lower, ymax=upper), linetype=0, alpha=0.25,fill = "#666666",size=0.5)+
    geom_ribbon(aes(ymin=lower2, ymax=upper2), linetype=0, alpha=0.5,size=0.5,fill="#666666")+
    geom_line(aes(y=median,colour="Estimate of total deaths"),size=1.5,alpha=0.8,colour="black") +
    ylab(label="")+
    ggtitle("Total deaths")+
    plotdet + theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  
  
  
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
    geom_bar(aes(y=value,fill=variable),size=1.5,stat = "identity") +
    ylab(label="")+
    ggtitle(label="Size of populations")+
    plotdet+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    # scale_fill_discrete(name = "", labels = c("Infected", "Dead", "Hospitalised","Recovered"))+
    scale_fill_manual(values = c("cornflowerblue",cols[c(2,3)]),name= "", #guide = guide_legend(reverse = TRUE),
                      breaks = c("Exposed","Infected","Recovered"))+ theme(
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
                      )+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 
  
  P12<- ggplot(data= dataleavingnonIC, aes(x=Date))+
    annotate(geom = "rect", xmin=as.Date(last(COVIDBYDAY$Date),"%Y-%m-%d")+1, xmax=as.Date(first(COVIDBYDAY$Date),"%Y-%m-%d") + lengthsim, 
             ymin=0, ymax=Inf,
             fill = "cornflowerblue", alpha = 0.1)+
    geom_bar(aes(y=value,fill=variable),size=1.5,stat = "identity") +
    ylab(label="")+
    ggtitle(label="Leaving non-IC in last 24 hours")+
    plotdet+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    # scale_fill_discrete(name = "", labels = c("Infected", "Dead", "Hospitalised","Recovered"))+
    scale_fill_manual(values = c("slategray4",cols[c(3)]),name= "" #, 
                      #breaks = c("Recovered","Dying")
                      )+ theme(
                        legend.position = c(.05, .95),
                        legend.justification = c("left", "top"),
                        legend.box.just = "left",
                        legend.margin = margin(6, 6, 6, 6)
                      )+ theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 
  
 TITLE <-  paste0("COVID-19 ",SPECREGION,sep="")
  
  grid.arrange(rbind(ggplotGrob(P1), ggplotGrob(P6), ggplotGrob(P8), ggplotGrob(P10),ggplotGrob(P7), size = "last"),
               rbind(ggplotGrob(P5), ggplotGrob(P12), ggplotGrob(P4) ,ggplotGrob(P2), ggplotGrob(P11), ggplotGrob(P3), size = "last"),
              
               #tableGrob(datatabsummary, theme=ttheme_default(base_size = 7) ,rows = NULL),
               top = textGrob(TITLE,gp=gpar(fontsize=20,font=3)),ncol=2)
 # print(datatabsummary)
}
#the best few fits
plotfits(FULLDATA,200)

#options(scipen=5)
#dev.off()
plotfits(FULLDATA[1:10,],80)


par(mfrow=c(1,1))
mycol <- rgb(255, 0, 0, max = 255, alpha = 100, names = "blue50")
boxplot(FULLDATA_orig$eta,FULLDATA_orig$alpha,FULLDATA_orig$delta,FULLDATA_orig$mu,FULLDATA_orig$omega,FULLDATA_orig$psi, FULLDATA_orig$chi,
        FULLDATA_orig$betaA, FULLDATA_orig$betaI,FULLDATA_orig$homeschool,FULLDATA_orig$lockdown,FULLDATA_orig$distancing,FULLDATA_orig$employment,FULLDATA_orig$rgnonIC,FULLDATA_orig$rgIC,FULLDATA_orig$rho,FULLDATA_orig$phi,FULLDATA_orig$kappa,
        col=grey(0.6),names=c(expression(eta),expression(alpha),expression(delta),expression(mu),expression(omega),expression(psi),expression(chi),
                              expression(betaA),expression(betaI),expression(homeschool),expression(lockdown),expression(distancing),expression(employment),expression(rgnonIC),expression(rgIC),expression(rho),expression(phi),expression(kappa) ),
        show.names=TRUE,main="",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2)
boxplot(FULLDATA$eta,FULLDATA$alpha,FULLDATA$delta,FULLDATA$mu,FULLDATA$omega,FULLDATA$psi, FULLDATA$chi,
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
round(c(quantile(FULLDATA$kappa, c(0.025, 0.975)) ,median=median(FULLDATA$kappa)),3)*100 #

round(c(quantile(FULLDATA$homeschool, c(0.025, 0.975)) ,median=median(FULLDATA$homeschool)),3)*100 #
100-round(c(quantile(FULLDATA$distancing, c(0.025, 0.975)) ,median=median(FULLDATA$distancing)),3)*100 #
round(c(quantile(FULLDATA$employment, c(0.025, 0.975)) ,median=median(FULLDATA$employment)),3)*100 #
100 - round(c(quantile(FULLDATA$lockdown, c(0.025, 0.975)) ,median=median(FULLDATA$lockdown)),3)*100 #

round(c(quantile(FULLDATA$betaA, c(0.025, 0.975)) ,median=median(FULLDATA$betaA)),3) #


