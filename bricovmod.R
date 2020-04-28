###############################  COVID-19 MODEL  ##################################
###############################    Ross Booton   ##################################
###############################   APRIL 2020     ##################################
#### Data should be updated from GOV.UK https://coronavirus.data.gov.uk/#regions for the South West 
### need "coronavirus-cases_latest.csv" and "coronavirus-deaths_latest.csv" from Download the latest cases data as CSV .... and Download the latest deaths data as CSV 
# updated for deaths in region with https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
###regional data included as .csv on GitHub (coronavirus-deaths-regional.csv) - from ONS data
setwd("~/Documents/Bristol/COVID-19") # set working directory
################################## SPECIFY REGION / AREA ####################################
SPECREGION <- toupper("South West")
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
    ALLCASES <<- fread("coronavirus-cases_latest.csv") #download from https://coronavirus.data.gov.uk/#regions
    ALLCASES <<- select(ALLCASES, "Area name", "Area code", "Area type", "Specimen date","Daily lab-confirmed cases", "Cumulative lab-confirmed cases")
    ALLDEATHS <<- fread("coronavirus-deaths_latest.csv")
    ALLDEATHSREGIONAL <<- fread("coronavirus-deaths-regional.csv") #from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
    ALLDEATHS <<- rbind(ALLDEATHS,ALLDEATHSREGIONAL,fill=TRUE)
    J <<- unique(ALLCASES$`Specimen date`)
    ALLDATES <<- J[order(J)]
    ALLDATES<<- seq(from=as.Date(ALLDATES[1]),to=as.Date(last(ALLDATES)), by=1) 
    colnames(ALLCASES)[c(1,2,3,4,5,6)] <<- c("Area","AreaCode","AreaType","Date","CASES","CUMSUM") #DAY 1 is 30th Jan 2020
    ALLCASES$Area <- toupper( ALLCASES$Area)
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
    LA <<- nrow(COVIDBYDAY)
    #DEATHS only available for "United Kingdom" "England" "Scotland" "Northern Ireland" "Wales"
    colnames(ALLDEATHS)[c(1,2,3,4,5,6)] <<- c("Area","AreaCode","AreaType","Date","CASES","CUMSUM") #DAY 1 is 30th Jan 2020
    ALLDEATHS$Area <<- toupper( ALLDEATHS$Area)
    DEATHSBYDAY<<-filter(ALLDEATHS, Area == SPECREGION)
    DEATHSBYDAY <<- DEATHSBYDAY[order(DEATHSBYDAY$Date),]
    DD <<- if (SPECREGION %in% unique(ALLDEATHSREGIONAL$`Area name` ) ) {"%d/%m/%y"} else {"%Y-%m-%d"}
    DEATHSBYDAY$Date <<- as.Date(DEATHSBYDAY$Date,
                                 format = DD)
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
    DEATHSBYDAY$CASES <<-  as.numeric(DEATHSBYDAY$CASES) * 0.839 #assume 83.9% of deaths are in hospital
    DEATHSBYDAY$CUMSUM <<-  as.numeric(DEATHSBYDAY$CUMSUM) * 0.839 #assume 83.9% of deaths are in hospital
    DEATHSBYDAY$CUMSUM[is.na(DEATHSBYDAY$CASES )] <<- NA
    DEATHSFITTING <<- DEATHSBYDAY[complete.cases(DEATHSBYDAY[,-(2:3)]),] #fitting data to use if doing target fitting
    time1 <<- as.numeric(as.Date("2020-03-15") - COVIDBYDAY[1,]$Date + 1)
    time2 <<- as.numeric(as.Date("2020-03-20") - COVIDBYDAY[1,]$Date + 1)
    time3 <<- as.numeric(as.Date("2020-03-23") - COVIDBYDAY[1,]$Date + 1)
    DEMOALL <<- fread("DEMOGRAPHY.csv")     ###DEMOGRAPHIC DATA
    DEMOALL$Name <<- toupper( DEMOALL$Name)
    DEMO<<-filter(DEMOALL, Name == SPECREGION)
    DEMO<<- DEMO[,-c(1:2)]
    data(polymod)     ###POLYMOD DATA 
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
    LAMBDA <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    OMEGA <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    XI <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    DELTA <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    COMP1 <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    COMP2 <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    COMP3 <<- matrix(1, nrow=N_AGE,ncol=N_AGE)
    dy = array(0, dim = c(N_COMP, N_AGE)) # 11 states and 9 age groups 
    CONTACTMATRIX <<- AGEGRP
    UR1max <<- 0 
    set.seed(3333) # so reproducible
  }}
B<-IN(TRUE) #run all of above

################         FUNC: DESOLVE FUNCTION           ######################
XY.model <- function(time, y, params) {#XY.model is the function for solving our ODEs
  dy = array(0, dim = c(N_COMP, N_AGE)) 
  y = array(y,  dim = c(N_COMP, N_AGE))
  kappa <- params["kappa"][[1]]
  rgnonIC <-params["rgnonIC"][[1]]
  epsilon <- 1- rgnonIC 
  delta <- params["delta"][[1]]
  rho <- params["rho"][[1]]
  eta <- params["eta"][[1]]
  alpha <- params["alpha"][[1]]
  mu <- params["mu"][[1]]
  gamma <- c(params["gamma1"][[1]],params["gamma2"][[1]],params["gamma3"][[1]],params["gamma4"][[1]],params["gamma5"][[1]],params["gamma6"][[1]],params["gamma7"][[1]],params["gamma8"][[1]],params["gamma9"][[1]])
  omega <- c(params["omega1"][[1]],params["omega2"][[1]],params["omega3"][[1]],params["omega4"][[1]],params["omega5"][[1]],params["omega6"][[1]],params["omega7"][[1]],params["omega8"][[1]],params["omega9"][[1]])
  psi <- params["psi"][[1]]
  betaA <- params["betaA"][[1]]
  betaI <- params["betaI"][[1]]
  LAMBDA[1:3,1:3] <- LAMBDA[1:3,1:3]*params["homeschool"][[1]]
  OMEGA <- OMEGA*params["lockdown"][[1]]   #LOCKDOWN matrix occured on 24th March 2020, puclic events banned, lockdown ordered
  XI <- XI*params["distancing"][[1]]  #DISTANCING MATRIX  #advice to avoid pubs, clubs, theatres and other public institutions occured on 16th March 2020
  DELTA[4:7,4:7] <- DELTA[4:7,4:7]*params["employment"][[1]]  #EMPLOYMENT MATRIX
  #######################  FOI  calculation  #######################
  sumN = apply(y,c(2),sum ) #total in each age group N_g
  sumX = apply(y,c(1),sum ) #total in each compartment X_i
  FOIMAT <- array(0,dim=c(N_AGE,N_AGE))
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
    FOIMAT[i,] <- betaI*CONTACTMATRIX[i,]*( (y[4,i]/sumN[]) + (y[5,i]/sumN[]) )}
  lambda <- apply(FOIMAT,c(2),sum ) #add up to give vector of lambda_g
  ###############################    MODEL EQUATIONS     #####################################
  # for (g in 1:N_AGE){
  #### Susceptible = S ####
  dy[1,] <- -lambda[] * y[1,] 
  #### Shielded = P ####
  dy[2,] <- 0 #-lambda[g] * y[2,g] 
  #### Exposed = E ####
  dy[3,] <- lambda[] * y[1,] - eta * y[3,]
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
  list(as.vector(dy))
}

XY.model <- cmpfun(XY.model) #compile
################        EPID SIMULATOR        ######################
epid.start <- 0
epid <- function(eta,alpha,delta,mu,
                 gamma1,gamma2,gamma3,gamma4,gamma5,gamma6,gamma7,
                 psi,chi,betaA,betaI, rho,phi,w,yprop,rgnonIC,b, omega1, omega2, omega3, omega4, omega5, omega6, omega7, omega8, omega9, kappa,
                 homeschool,lockdown,distancing,employment,initialinfected,gamma8,gamma9, UR1, #some variables to use in function, some which we no longer use in this version
                 epid.duration = nrow(COVIDBYDAY) - 1, #length of simulation
                 func.indicator) #plot or no plot?
{  ##set up initial population in each state for each group
  foo <- array(1:(N_COMP*N_AGE),dim=c(N_COMP,N_AGE)) #temp array used for indexing
  y_vec <- rep(0,(N_COMP*N_AGE))
  y_vec[as.vector(foo[1,])] <- AGEPOP 
  y_vec[as.vector(foo[5,5])] <- initialinfected #seed initialinfected infected in 5TH 25-39 age group , 1 infectious at end of Jan 2020
  names(y_vec) <- paste0("y", 1:(N_COMP*N_AGE))
  init<-y_vec
  params <- list(eta=eta,alpha=alpha,mu=mu,delta=delta,   #params is the data from outside this function, which is used through the Sampling function
                 gamma1=gamma1, gamma2=gamma2, gamma3=gamma3,gamma4=gamma4, gamma5=gamma5, gamma6=gamma6,gamma7=gamma7, 
                 psi=psi,betaA=betaA,betaI=betaI,rho=rho,rgnonIC=rgnonIC,
                 omega1=omega1, omega2=omega2, omega3=omega3, omega4=omega4, 
                 omega5=omega5, omega6=omega6, omega7=omega7, omega8=omega8, omega9=omega9,
                 kappa=kappa,
                 homeschool=homeschool,lockdown=lockdown,distancing=distancing,employment=employment,initialinfected=initialinfected,gamma8=gamma8,gamma9=gamma9)
  vectTime <- c(0,1:epid.duration)  ## vectTime is rescaled in order to cope with the solver function		
  out <- as.data.frame(lsoda(y=init,time=vectTime,func=XY.model,parms=params))  #run the model using desolve
  out$time = out$time+epid.start #rescale the time so that it runs from day 0
  out$temp <- out$time
  out <- out[,-1]
  names(out)[names(out)=="temp"] <- "time"
  out<- out %>% dplyr::mutate(N = rowSums(.[as.vector(foo[,])]),
                              S = rowSums(.[as.vector(foo[1,])]),
                              # P = rowSums(.[as.vector(foo[2,])]), #extra which we dont use
                              E = rowSums(.[as.vector(foo[3,])]),
                              A = rowSums(.[as.vector(foo[4,])]),
                              I = rowSums(.[as.vector(foo[5,])]),
                              H = rowSums(.[as.vector(foo[6,])]),
                              C = rowSums(.[as.vector(foo[7,])]),
                              # W = rowSums(.[as.vector(foo[8,])]), #extra which we dont use
                              #  B = rowSums(.[as.vector(foo[9,])]), #extra which we dont use
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
  
  out$Date <- out$time + COVIDBYDAY$Date[1]
  LLdeaths <-   rep(NA,nrow(COVIDBYDAY))
  LLhospday <-   rep(NA,nrow(COVIDBYDAY))
  totaldeaths <- out$D[1:(nrow(COVIDBYDAY))]
  totalhospday <- out$new.cases.entering.hospital[1:(nrow(COVIDBYDAY))]
  casestemp <- COVIDBYDAY
  deathstemp <- DEATHSBYDAY
  LLdeaths <- (deathstemp$CUMSUM)*log(totaldeaths)  - totaldeaths #- lfactorial(deathstemp$CUMSUM)
  LLhospday <- (casestemp$CASES)*log(totalhospday)  - totalhospday #- lfactorial(casestemp$CASES)
  toReturn <- as.vector(c( 
    LLdeaths,
    LLhospday)) 
  names(toReturn) <- c( 
    paste0("LLdeaths",1: length(LLdeaths)),
    
    paste0("LLhospday",1: length(LLhospday)))
  if (func.indicator=="returnout"){	
    return(out)
  } else if (func.indicator=="returnindicators") {
    return(toReturn)
  } else { print("invalid function indicator") }}

epid <- cmpfun(epid)
###############      FUNC: Latin Hypercube Sampling (LHS)       ###############
Sampling <- function(aaa){
  Samples_SIR <- randomLHS(aaa, 9)
  Samples_SIR[,1] <- 0.04 + (0.10-0.04)*Samples_SIR[,1] #0.02 + (0.15-0.02)*Samples_SIR[,7] # betaA / betaI
  Samples_SIR[,2] <- 0.17 + (0.37-0.17)*Samples_SIR[,2] # https://www.gstatic.com/covid19/mobility/2020-04-05_GB_Mobility_Report_en.pdf
  Samples_SIR[,3] <- 0.3 + (0.9-0.3)*Samples_SIR[,3] # distancing # 
  Samples_SIR[,4] <- 0.7 + (1.0 - 0.7)*Samples_SIR[,4] #  epsilon = 1 - rgnonIC
  Samples_SIR[,5] <-  0.05 + (0.35-0.05)*Samples_SIR[,5]    #kappa
  Samples_SIR[,6] <-  4 + (14-4)*Samples_SIR[,6] #rho 
  Samples_SIR[,7] <-  4 + (12-4)*Samples_SIR[,7]  # psi 
  Samples_SIR[,8] <-  4 + (14-4)*Samples_SIR[,8]  # mu 
  Samples_SIR[,9] <-  0.7315 + (0.9105-0.7315)*Samples_SIR[,9]  # delta 
  paramsMat_SIR <- data.frame(eta=1/5.1, 
                              alpha= (1/5), 
                              delta=Samples_SIR[,9], 
                              mu= (1/ Samples_SIR[,8]),
                              gamma1=0 ,gamma2=0.0408/100 ,gamma3=0.0408/100 ,
                              gamma4=0.0104,gamma5=0.0223,
                              gamma6=0.0425,gamma7=0.0816,
                              gamma8=0.118,gamma9=0.175,
                             psi=(1/Samples_SIR[,7]),
                         betaA=Samples_SIR[,1],
                              betaI=Samples_SIR[,1], 
                              homeschool= 0.05,
                              lockdown= Samples_SIR[,2],
                              distancing= Samples_SIR[,3],
                              employment= 0.44,
                           rgnonIC= Samples_SIR[,4],
                              kappa =Samples_SIR[,5],
                              rho = 1/(Samples_SIR[,6]), 
                              initialinfected = 1, 
                              UR1 = 0, omega1 = 0, omega2 = 0, omega3 = 0.221, 
                              omega4 = 0.221, omega5 = 0.221, omega6 = 0.266,
                              omega7 = 0.423, omega8 = 0.574, omega9 = 0.683
  ) 
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
  fwrite(ResMat,paste0("OUT_", a0,".csv"))
  stopCluster(cl)}
outFUN <- cmpfun(outFUN)

##################         FITTING      ######################## 
Nsamples<- 100000 #how many samples? We run for 100k in our paper
p1<-Sampling(Nsamples)  
ptm <- proc.time() #time run 
outFUN(p1,1 )##Run the simulator outFUN for the Latin-Hypercube samples generated above, with file name OUT_1.csv in your working directory
proc.time() - ptm

FULLDATA<-fread("OUT_1.csv") #then read in the csv file you generated above
completeat <- length(COVIDBYDAY$CASES)-4 #Data from around 5 days ago can be considered complete, so set last 5 to = NA
lastat <- length(COVIDBYDAY$CASES)
for (i in completeat:lastat){
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
FULLDATA <- FULLDATA[order(-FULLDATA$LLALL),] #order them

top100 <- head(FULLDATA,100)
source("plotfits.R") #use plotting function plotfits.R and comment out dependending on which plots / outputs needed
plotfits(top100,81) #set end time for the best 100 fits according to LL



