packages <- c("Hmisc")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

#Chinook data
dat=data.frame(matrix(c(  1,	19,	 51,	91,	 72,	23,	 3,	 260,	 0.0140,
                          2,	12,	 33,	60,	 51,	16,	 0,	 174,	 0.0030,
                         34, 133,	243, 228,	159,	40,	10,	 847,	 0.0106,
                         67, 163,	154,	86,	 25,	 6,	 0,	 501,	 0.0140,
                         62, 166,	160,	92,	 27,	 4,	 1,	 512,	 0.0410,
                        126, 313,	325, 179,	 81,	18,	 4,	1046,	 0.0239,
                        342, 475,	249,	72,	 15,	 2,	 0,	1155,	 0.0139,
                         25,	85,	125, 130,	 94,	29,	 8,	 496,	 0.0060,
                         65, 160,	188, 143,	 54,	17,	 4,	 631,	 0.0127,
                         63, 114,	 90,	31,	  8,	 2,	 0,	 308,	 0.0260,
                         48, 122,	118,	78,	 35,	11,	 3,	 415,	 0.0072),byrow=T,ncol=9))

colnames(dat)=c("0","1","2","3","4","5","6","Total","SAR")
rownames(dat)=c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014")

year.byp=dat[,1:7]
sars=dat[,9]
yearly.n=dat[,8]

ss.multiplier=1    #This is the sample size multiplier, ranging from 1 to 9; to generate Figure 2 in Storch et al. (in review); in this version of the code,  the multiplier will have to be adjusted manually
sim.n=ss.multiplier*yearly.n
tot.n=sum(sim.n)

sims=100  #At small simulation sizes, output may vary slightly among runs (i.e., at a given miltiplier)
Pr.z=rep(NA,sims)
Pr.z.re=rep(NA,sims)
Pr.z.binom=rep(NA,sims)
for (j in 1:sims){
  sim.dat=data.frame(year=rep(NA,tot.n),nbypass=rep(NA,tot.n),ad.ret=rep(NA,tot.n))

  y04=rep(2004,sim.n[1])
  byp04=rep(NA,sim.n[1])
  ad.04=rep(NA,sim.n[1])
  lsar=rep(NA,sim.n[1])
  for (i in 1:sim.n[1]){
    byp04[i]=as.integer(rMultinom(rbind(year.byp[1,]/yearly.n[1]),1))
    lsar[i]=log(sars[1]/(1-sars[1]))-.117*byp04[i]  # -0.117 is the estimated effect of each bypass for Chinook from Tuomikoski et al. (2010)
    ad.04[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  y05=rep(2005,sim.n[2])
  byp05=rep(NA,sim.n[2])
  ad.05=rep(NA,sim.n[2])
  lsar=rep(NA,sim.n[2])
  for (i in 1:sim.n[2]){
    byp05[i]=as.integer(rMultinom(rbind(year.byp[2,]/yearly.n[2]),1))
    lsar[i]=log(sars[2]/(1-sars[2]))-.117*byp05[i]
    ad.05[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  y06=rep(2006,sim.n[3])
  byp06=rep(NA,sim.n[3])
  ad.06=rep(NA,sim.n[3])
  lsar=rep(NA,sim.n[3])
  for (i in 1:sim.n[3]){
    byp06[i]=as.integer(rMultinom(rbind(year.byp[3,]/yearly.n[3]),1))
    lsar[i]=log(sars[3]/(1-sars[3]))-.117*byp06[i]
    ad.06[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  y07=rep(2007,sim.n[4])
  byp07=rep(NA,sim.n[4])
  ad.07=rep(NA,sim.n[4])
  lsar=rep(NA,sim.n[4])
  for (i in 1:sim.n[4]){
    byp07[i]=as.integer(rMultinom(rbind(year.byp[4,]/yearly.n[4]),1))
    lsar[i]=log(sars[4]/(1-sars[4]))-.117*byp07[i]
    ad.07[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  y08=rep(2008,sim.n[5])
  byp08=rep(NA,sim.n[5])
  ad.08=rep(NA,sim.n[5])
  lsar=rep(NA,sim.n[5])
  for (i in 1:sim.n[5]){
    byp08[i]=as.integer(rMultinom(rbind(year.byp[5,]/yearly.n[5]),1))
    lsar[i]=log(sars[5]/(1-sars[5]))-.117*byp08[i]
    ad.08[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  y09=rep(2009,sim.n[6])
  byp09=rep(NA,sim.n[6])
  ad.09=rep(NA,sim.n[6])
  lsar=rep(NA,sim.n[6])
  for (i in 1:sim.n[6]){
    byp09[i]=as.integer(rMultinom(rbind(year.byp[6,]/yearly.n[6]),1))
    lsar[i]=log(sars[6]/(1-sars[6]))-.117*byp09[i]
    ad.09[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  y10=rep(2010,sim.n[7])
  byp10=rep(NA,sim.n[7])
  ad.10=rep(NA,sim.n[7])
  lsar=rep(NA,sim.n[7])
  for (i in 1:sim.n[7]){
    byp10[i]=as.integer(rMultinom(rbind(year.byp[7,]/yearly.n[7]),1))
    lsar[i]=log(sars[7]/(1-sars[7]))-.117*byp10[i]
    ad.10[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  y11=rep(2011,sim.n[8])
  byp11=rep(NA,sim.n[8])
  ad.11=rep(NA,sim.n[8])
  lsar=rep(NA,sim.n[8])
  for (i in 1:sim.n[8]){
    byp11[i]=as.integer(rMultinom(rbind(year.byp[8,]/yearly.n[8]),1))
    lsar[i]=log(sars[8]/(1-sars[8]))-.117*byp11[i]
    ad.11[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  y12=rep(2012,sim.n[9])
  byp12=rep(NA,sim.n[9])
  ad.12=rep(NA,sim.n[9])
  lsar=rep(NA,sim.n[9])
  for (i in 1:sim.n[9]){
    byp12[i]=as.integer(rMultinom(rbind(year.byp[9,]/yearly.n[9]),1))
    lsar[i]=log(sars[9]/(1-sars[9]))-.117*byp12[i]
    ad.12[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  y13=rep(2013,sim.n[10])
  byp13=rep(NA,sim.n[10])
  ad.13=rep(NA,sim.n[10])
  lsar=rep(NA,sim.n[10])
  for (i in 1:sim.n[10]){
    byp13[i]=as.integer(rMultinom(rbind(year.byp[10,]/yearly.n[10]),1))
    lsar[i]=log(sars[10]/(1-sars[10]))-.117*byp13[i]
    ad.13[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  y14=rep(2014,sim.n[11])
  byp14=rep(NA,sim.n[11])
  ad.14=rep(NA,sim.n[11])
  lsar=rep(NA,sim.n[11])
  for (i in 1:sim.n[11]){
    byp14[i]=as.integer(rMultinom(rbind(year.byp[11,]/yearly.n[11]),1))
    lsar[i]=log(sars[11]/(1-sars[11]))-.117*byp14[i]
    ad.14[i]=rbinom(1,1,1/(1+exp(-lsar[i])))
  }

  sim.dat$year=as.factor(c(y04,y05,y06,y07,y08,y09,y10,y11,y12,y13,y14))
  sim.dat$nbypass=c(byp04,byp05,byp06,byp07,byp08,byp09,byp10,byp11,byp12,byp13,byp14)
  sim.dat$ad.ret=c(ad.04,ad.05,ad.06,ad.07,ad.08,ad.09,ad.10,ad.11,ad.12,ad.13,ad.14)

  res1=glm(ad.ret~year+nbypass,binomial,data=sim.dat)
  Pr.z[j]=summary(res1)$coefficients[12,4]

  print(j)
}
sum(Pr.z<.05)/sims #This is the estimated power for the chosen multiplier (line 30)
