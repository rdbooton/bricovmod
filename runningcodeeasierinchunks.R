N=100000
p1<-Sampling(N) 
write.csv(p1,"p1.csv")
p1<-fread("p1.csv")
K=1000 #how many slices

###### 
# OUT_1 to OUT_K
for (i in 1:K){
  assign((paste0("DATA_", (i 
  ) ) ) , p1[(1+((i-1)*(nrow(p1)/K))):(i*(nrow(p1)/K)),] )
}

for (i in 1:K){ #
  DATA = eval(parse(text=paste0("DATA_", i)))
  outFUN(DATA,i)
  print(i)
}


FULLDATA_SW <- NULL
for (i in 1:K){
  temp <- read.csv(paste0("OUT_", i,".csv"))
  FULLDATA_SW <- rbind(FULLDATA_SW,temp)
}
write.csv(FULLDATA_SW,"FULLDATA_SW.csv")








Nsamples<- 50000 #started at 17.53pm 17th April 2020
p1<-Sampling2(Nsamples)  #how many samples? 
write.csv(p1,"p1.csv")
p1<-fread("p1.csv")
#Run the simulator outFUN for the Latin-Hypercube samples generated above
N=2500
p1.1 <- p1[1:N,]
p1.2 <- p1[5001:10000,]
p1.3 <- p1[10001:15000,]
p1.4 <- p1[15001:20000,]
p1.5 <- p1[20001:25000,]
p1.6 <- p1[25001:30000,]
p1.7 <- p1[30001:35000,]
p1.8 <- p1[35001:40000,]
p1.9 <- p1[40001:45000,]
p1.10 <- p1[45001:50000,]

p1.11 <- p1[50001:55000,]
p1.12 <- p1[55001:60000,]
p1.13 <- p1[60001:65000,]
p1.14 <- p1[65001:70000,]
p1.15 <- p1[70001:75000,]
p1.16 <- p1[75001:80000,]
p1.17 <- p1[80001:85000,]
p1.18 <- p1[85001:90000,]
p1.19 <- p1[90001:95000,]
p1.20 <- p1[95001:100000,]

outFUN(p1.1,11 )
outFUN(p1.2,12 )
outFUN(p1.3,13 )
outFUN(p1.4,14 )
outFUN(p1.5,15 )
outFUN(p1.6,16 )
outFUN(p1.7,17 )
outFUN(p1.8,18 )
outFUN(p1.9,19 )
outFUN(p1.10,20 )

outFUN(p1.11,21 )
outFUN(p1.12,22 )
outFUN(p1.13,23 )
outFUN(p1.14,24 )
outFUN(p1.15,25 )
outFUN(p1.16,26 )
outFUN(p1.17,27 )
outFUN(p1.18,28 )
outFUN(p1.19,29 )
outFUN(p1.20,30 )

OUT_1 <- fread("OUT_11.csv")
OUT_2 <- fread("OUT_12.csv")
OUT_3 <- fread("OUT_13.csv")
OUT_4 <- fread("OUT_14.csv")
OUT_5 <- fread("OUT_15.csv")
OUT_6 <- fread("OUT_16.csv")
OUT_7 <- fread("OUT_17.csv")
OUT_8 <- fread("OUT_18.csv")
OUT_9 <- fread("OUT_19.csv")
OUT_10 <- fread("OUT_20.csv")

OUT_11 <- fread("OUT_21.csv")
OUT_12 <- fread("OUT_22.csv")
OUT_13 <- fread("OUT_23.csv")
OUT_14 <- fread("OUT_24.csv")
OUT_15 <- fread("OUT_25.csv")
OUT_16 <- fread("OUT_26.csv")
OUT_17 <- fread("OUT_27.csv")
OUT_18 <- fread("OUT_28.csv")
OUT_19 <- fread("OUT_29.csv")
OUT_20 <- fread("OUT_30.csv")

FULLDATA <- rbind(OUT_1, OUT_2, OUT_3, OUT_4, OUT_5, OUT_6, OUT_7, OUT_8, OUT_9, OUT_10,
                  OUT_11, OUT_12, OUT_13, OUT_14, OUT_15, OUT_16, OUT_17, OUT_18, OUT_19, OUT_20)
write.csv(FULLDATA,"FULLDATA.csv")



