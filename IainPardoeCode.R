oscars <- read.table("..\\..\\R programs\\oscars\\oscars.csv",
                     header = TRUE, sep = ",")

attach(oscars)

oscars$DPrN <- ifelse(DD==1, log(DPrN+1), 0)
oscars$MPrN <- ifelse(MM==1, log(MPrN+1), 0)
oscars$MPrW <- ifelse(MM==1, log(MPrW+1), 0)
oscars$FPrW <- ifelse(FF==1, log(FPrW+1), 0)

oscars$Ch[Year==1968&Name=="HepburnK"] <- 2
oscars$Ch[Year==1932&Name=="March"] <- 2
oscars$Ch[Year==1928&Name=="Jannings"&Movie=="Way"] <- 2
oscars$Ch[Year==1928&Name=="Gaynor"&Movie=="Street"] <- 2
oscars$Ch[Year==1928&Name=="Gaynor"&Movie=="Sunrise"] <- 2

oscars$Length <- oscars$Length/100
oscars$Days <- oscars$Days/100

sum(oscars$Ch==1)
# 318

# load R2Winbugs

detach(oscars)

mwl <- 80

res.Rhat <- array(NA, c(70,30))
delta.med <- delta.sd <- array(NA, c(70,29))
corr.prop <- array(NA, c(70,5))
incorrect <- array(NA, c(70,120))
pred <- array(NA, c(70,4,3))
pred.i <- array(NA, c(70,4))

#############
# 1938-2007 #
#############

#k <- 1937
k <- 2006
oscarsx <- oscars[oscars$Year<=k & oscars$Year>max(k-mwl,1927),]
attach(oscarsx)
T <- length(Year) # total observations
N <- length(unique(Comp)) # competitions
Event <- N-Comp+min(Comp)
P <- rep(NA, N)
Nominee <- NULL
for (i in 1:N) {
  P[i] <- sum(Event==i) # nominees
  Nominee <- c(Nominee,1:P[i])
}
Choice <- 2-Ch

#for(k in 1938:2007) {
k <- 2007

Nx.order <-
  order(c(mean(Dir[PP==1 & Ch==1])-mean(Dir[PP==1 & Ch==2]),
          mean(Aml[PP==1 & Ch==1])-mean(Aml[PP==1 & Ch==2]),
          mean(Afl[PP==1 & Ch==1])-mean(Afl[PP==1 & Ch==2]),
          mean(Ams[PP==1 & Ch==1])-mean(Ams[PP==1 & Ch==2]),
          mean(Afs[PP==1 & Ch==1])-mean(Afs[PP==1 & Ch==2]),
          mean(Scr[PP==1 & Ch==1])-mean(Scr[PP==1 & Ch==2]),
          mean(Cin[PP==1 & Ch==1])-mean(Cin[PP==1 & Ch==2]),
          mean(Art[PP==1 & Ch==1])-mean(Art[PP==1 & Ch==2]),
          mean(Cos[PP==1 & Ch==1])-mean(Cos[PP==1 & Ch==2]),
          mean(Sco[PP==1 & Ch==1])-mean(Sco[PP==1 & Ch==2]),
          mean(Son[PP==1 & Ch==1])-mean(Son[PP==1 & Ch==2]),
          mean(Edi[PP==1 & Ch==1])-mean(Edi[PP==1 & Ch==2]),
          mean(Sou[PP==1 & Ch==1])-mean(Sou[PP==1 & Ch==2]),
          mean(Eff[PP==1 & Ch==1])-mean(Eff[PP==1 & Ch==2]),
          mean(Mak[PP==1 & Ch==1])-mean(Mak[PP==1 & Ch==2]),
          mean(AD[PP==1 & Ch==1])-mean(AD[PP==1 & Ch==2])))
Nx <- c(4^(0:16) %*%
          rbind(rbind(Dir, Aml, Afl, Ams, Afs, Scr, Cin, Art, Cos, Sco,
                      Son, Edi, Sou, Eff, Mak, AD)[Nx.order,], Pic))
N1 <- N2 <- N3 <- rep(0, T)
for (i in 1:T) {
  if(Nx[i]==rev(sort(unique(Nx[Year==Year[i]])))[1]) N1[i] <- 1
  if(Nx[i]==rev(sort(unique(Nx[Year==Year[i]])))[2]) N2[i] <- 1
  if(Nx[i]==rev(sort(unique(Nx[Year==Year[i]])))[3]) N3[i] <- 1
}

X <- if (k<1939) cbind(N1,PN,PD,DPrN,MPrN,FPrW) else {
  if(k<1944) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP) else {
    if(k<1945) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1) else {
   if(k<1946) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
      Gd) else {
      if(k<1951) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
       Gd,Gdr) else {
  if(k<1952) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
         DGA,Gdr,PGA) else {
        if(k<1956) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                DGA,Gdr,PGA,Gf2) else {
                                                if(k<1959) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                         DGA,Gdr,PGA,Gf2,Gmc) else {
                                                               if(k<1961) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                 DGA,Gdr,PGA,Gf2,Gmc,N2,N3) else {
                                                                                           if(k<1965) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                            DGA,Gdr,PGA,Gmc,N2,N3) else  {
                                                                                                                           if(k<1973) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                                   DGA,Gdr,PGA,Gmc,N2,N3,Gm2) else {
                                                                                                                                         if(k<1995) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                                                              DGA,Gdr,PGA,Gf2,Gmc,N2,N3,Gm2) else {
                                                                                                                                                   if(k<1996) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                                                                            DGA,Gdr,PGA,Gf2,Gmc,N2,N3,Gm2,SAM) else {
                                                                                                                                                                                  cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                                                                                       DGA,Gdr,PGA,Gf2,Gmc,N2,N3,Gm2,SAM,SAF)}}}}}}}}}}}}}
D <- dim(X)[2]

data <- list("Event", "Nominee", "Choice", "X", "N", "P", "T", "D")
inits <- function(){
  list(delta=rnorm(D,0,1))
}
params <- c("delta")

results <- bugs(data, inits, params,
                "C:/Documents and Settings/ipardoe/My Documents/R programs/oscars/oscars7.txt",
                n.chains=3, n.iter=4000, n.thin=1)
attach.bugs(results)
print(results, digits.summary=2)
#Inference for Bugs model at "C:/Documents and Settings/Iain Pardoe/My Documents/R programs/oscars/oscars7.txt", fit using winbugs,
# 3 chains, each with 4000 iterations (first 2000 discarded)
# n.sims = 6000 iterations saved
#            mean   sd   2.5%    25%    50%    75%  97.5% Rhat n.eff
#delta[1]    0.60 0.26   0.10   0.43   0.59   0.77   1.11 1.01   190
#delta[2]    0.31 0.09   0.13   0.25   0.32   0.38   0.50 1.00  1800
#delta[3]    1.20 0.63  -0.02   0.78   1.17   1.59   2.50 1.00  1200
#delta[4]    0.79 0.38   0.06   0.54   0.81   1.04   1.54 1.01   280
#delta[5]    0.64 0.35  -0.03   0.42   0.63   0.86   1.34 1.00  1500
#delta[6]   -1.13 0.51  -2.16  -1.46  -1.10  -0.79  -0.18 1.00  2400
#delta[7]    0.13 0.10  -0.07   0.06   0.13   0.19   0.33 1.00   760
#delta[8]    1.34 0.45   0.48   1.05   1.33   1.64   2.27 1.00  5800
#delta[9]   -3.25 1.01  -5.30  -3.93  -3.23  -2.55  -1.32 1.01   280
#delta[10]   0.86 0.33   0.21   0.63   0.87   1.07   1.53 1.02   110
#delta[11]   1.36 1.14  -0.51   0.54   1.25   2.07   3.94 1.00  6000
#delta[12]   2.28 0.37   1.58   2.00   2.27   2.52   3.09 1.00  4300
#delta[13]   1.50 0.33   0.86   1.27   1.49   1.73   2.12 1.00  1400
#delta[14]   3.64 0.58   2.62   3.21   3.61   4.04   4.90 1.00  1000
#delta[15]   0.73 0.41  -0.03   0.45   0.74   1.00   1.50 1.01   210
#delta[16]   1.77 0.37   1.09   1.53   1.74   2.02   2.50 1.00   610
#delta[17]   0.98 0.45   0.07   0.68   0.99   1.28   1.84 1.00   770
#delta[18]   0.84 0.58  -0.34   0.46   0.87   1.24   1.94 1.01   330
#delta[19]   1.00 0.27   0.49   0.82   0.99   1.18   1.54 1.02   230
#delta[20]   0.23 0.29  -0.31   0.03   0.23   0.41   0.81 1.02   110
#delta[21]   1.42 0.62   0.15   1.03   1.43   1.83   2.60 1.00   860
#delta[22]   2.12 0.82   0.57   1.55   2.14   2.64   3.82 1.01   560
#delta[23]   2.20 0.74   0.79   1.69   2.17   2.70   3.72 1.00   780
#deviance  567.06 6.61 556.40 562.20 566.30 571.00 581.90 1.00  6000

#For each parameter, n.eff is a crude measure of effective sample size,
#and Rhat is the potential scale reduction factor (at convergence, Rhat=1).

#pD = 21.9 and DIC = 588.9 (using the rule, pD = Dbar-Dhat)
#DIC is an estimate of expected predictive error (lower deviance is better).

delta.med.last <- apply(delta, 2, median)

res.Rhat[k-1937,] <- replace(res.Rhat[k-1937,], 1:(D+1),
                             results$summary[,"Rhat"])
delta.med[k-1937,] <- replace(delta.med[k-1937,], 1:D,
                              apply(delta, 2, median))
delta.sd[k-1937,] <- replace(delta.sd[k-1937,], 1:D,
                             apply(delta, 2, sd))
lp <- X %*% delta.med[k-1937, 1:D]
lpsum <- rep(NA, N)
for (i in 1:N) lpsum[i] <- sum(exp(lp[Event==i]))
pr <- exp(lp) / lpsum[Event]
prmax <- rep(NA, N)
for (i in 1:N) prmax[i] <- max(pr[Event==i])
prwin <- rep(NA, T)
for (i in 1:T) prwin[i] <- if(pr[i]==prmax[Event[i]]) 1 else 0

correct <- rep(NA, N)
for (i in 1:N) correct[i] <-
  if(sum(Choice[Event==i]==prwin[Event==i])==P[i]) 1 else 0
corr.prop[k-1937,1] <- sum(correct) / length(correct)

E.PP <- E.DD <- E.MM <- E.FF <- rep(NA, N)
for (i in 1:N) {
  E.PP[i] <- PP[Event==i][1]
  E.DD[i] <- DD[Event==i][1]
  E.MM[i] <- MM[Event==i][1]
  E.FF[i] <- FF[Event==i][1]
}
corr.prop[k-1937,2] <- sum(correct[E.PP==1]) /
  length(correct[E.PP==1]) 
corr.prop[k-1937,3] <- sum(correct[E.DD==1]) /
  length(correct[E.DD==1]) 
corr.prop[k-1937,4] <- sum(correct[E.MM==1]) /
  length(correct[E.MM==1]) 
corr.prop[k-1937,5] <- sum(correct[E.FF==1]) /
  length(correct[E.FF==1])

incorrect[k-1937,] <- replace(incorrect[k-1937,],
                              1:length(which(correct==0)),
                              which(correct==0))

detach(bugs.sims)
detach(oscarsx)
oscarsx <- oscars[oscars$Year<=k & oscars$Year>max(k-mwl,1927),]
attach(oscarsx)
T <- length(Year) # total observations
N <- length(unique(Comp)) # competitions
Event <- N-Comp+min(Comp)
P <- rep(NA, N)
Nominee <- NULL
for (i in 1:N) {
  P[i] <- sum(Event==i) # nominees
  Nominee <- c(Nominee,1:P[i])
}
Choice <- 2-Ch
Nx <- c(4^(0:16) %*%
          rbind(rbind(Dir, Aml, Afl, Ams, Afs, Scr, Cin, Art, Cos, Sco,
                      Son, Edi, Sou, Eff, Mak, AD)[Nx.order,], Pic))
N1 <- N2 <- N3 <- rep(0, T)
for (i in 1:T) {
  if(Nx[i]==rev(sort(unique(Nx[Year==Year[i]])))[1]) N1[i] <- 1
  if(Nx[i]==rev(sort(unique(Nx[Year==Year[i]])))[2]) N2[i] <- 1
  if(Nx[i]==rev(sort(unique(Nx[Year==Year[i]])))[3]) N3[i] <- 1
}

X <- if (k<1939) cbind(N1,PN,PD,DPrN,MPrN,FPrW) else {
  if(k<1944) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP) else {
    if(k<1945) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,
                     Gf1) else {
                       if(k<1946) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                        Gd) else {
                                          if(k<1951) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                           Gd,Gdr) else {
                                                             if(k<1952) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                              DGA,Gdr,PGA) else {
                                                                                if(k<1956) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                 DGA,Gdr,PGA,Gf2) else {
                                                                                                   if(k<1959) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                    DGA,Gdr,PGA,Gf2,Gmc) else {
                                                                                                                      if(k<1961) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                                       DGA,Gdr,PGA,Gf2,Gmc,N2,N3) else {
                                                                                                                                         if(k<1965) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                                                          DGA,Gdr,PGA,Gmc,N2,N3) else  {
                                                                                                                                                            if(k<1973) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                                                                             DGA,Gdr,PGA,Gmc,N2,N3,Gm2) else {
                                                                                                                                                                               if(k<1995) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                                                                                                DGA,Gdr,PGA,Gf2,Gmc,N2,N3,Gm2) else {
                                                                                                                                                                                                  if(k<1996) cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                                                                                                                   DGA,Gdr,PGA,Gf2,Gmc,N2,N3,Gm2,SAM) else {
                                                                                                                                                                                                                     cbind(N1,PN,PD,DPrN,MPrN,FPrW,DN,MP,MPrW,FP,DP,Gm1,Gf1,
                                                                                                                                                                                                                           DGA,Gdr,PGA,Gf2,Gmc,N2,N3,Gm2,SAM,SAF)}}}}}}}}}}}}}

lp <- X %*% delta.med[k-1937, 1:D]
lpsum <- rep(NA, N)
for (i in 1:N) lpsum[i] <- sum(exp(lp[Event==i]))
pr <- exp(lp) / lpsum[Event]
prmax <- rep(NA, N)
for (i in 1:N) prmax[i] <- max(pr[Event==i])
prwin <- rep(NA, T)
for (i in 1:T) prwin[i] <- if(pr[i]==prmax[Event[i]]) 1 else 0

for (i in 1:4) {
  pred[k-1937,i,] <-
    replace(pred[k-1937,i,],
            1:max(length(which(Choice[Event==i]==1)),
                  length(which(prwin[Event==i]==1))),
            paste("Winner:",
                  Name[Event==i][which(Choice[Event==i]==1)],
                  round(pr[Event==i][which(Choice[Event==i]==1)], 2),
                  "vs. Predicted:",
                  Name[Event==i][which(prwin[Event==i]==1)], 
                  round(pr[Event==i][which(prwin[Event==i]==1)], 2)))
  pred.i[k-1937,i] <-
    if(any(which(Choice[Event==i]==1)==which(prwin[Event==i]==1))) 1
  else 0
}

#}

max(res.Rhat, na.rm=T)
# 1.020977
max(delta.sd, na.rm=T)
# 1.135347
max(delta.med, na.rm=T)
# 3.6135
min(delta.med, na.rm=T)
# -3.234

lp <- X[1:20,] %*% delta.med.last
N <- 4 # competitions
Event <- rep(1:4,rep(5,4))
lpsum <- rep(NA, N)
for (i in 1:N) lpsum[i] <- sum(exp(lp[Event==i]))
pr <- exp(lp) / lpsum[Event]
for (i in 1:20) print(paste(round(pr[i],3), Name[i]))

# "0.703 Country"
# "0.183 Blood"
# "0.063 Clayton"
# "0.032 Atonement"
# "0.019 Juno"

# "0.963 Coens"
# "0.022 Anderson"
# "0.009 Gilroy"
# "0.005 Reitman"
# "0.001 Schnabel"

# "0.934 DayLewis"
# "0.037 Depp"
# "0.021 Clooney"
# "0.004 Jones"
# "0.004 Mortensen"

# "0.718 Christie"
# "0.107 Cotillard"
# "0.095 Page"
# "0.040 Blanchett"
# "0.040 Linney"

#Front-runners

Name[1:5]
N1[1:5]
N2[1:5]
N3[1:5]

#N1 Country
#N2 Blood
#N3 Clayton

delta.med.last[c(1,19,20)]
# 0.58890 0.98565 0.22880
round(10*delta.med.last[c(1,19,20)],1)
# 5.9 9.9 2.3

#Picture - PN,PD,Gdr,Gmc,PGA

delta.med.last[c(2,3,15,18,16)]
# 0.3153 1.1730 0.7426 0.8741 1.7430
round(10*delta.med.last[c(2,3,15,18,16)],1)
# 3.2 11.7  7.4  8.7 17.4
round(10*Nom[1:5]*delta.med.last[2],1)
# 22.1 12.6 22.1 25.2 25.2
round(10*X[1:5,]%*%delta.med.last,1)

[,1]
[1,] 29.5
[2,] 24.3
[3,] 36.1
[4,] 60.3
[5,] 46.8

#Country   5.9 + 25.2 + 11.7       + 17.4 = 60.3  N1,8N,PD,PGA
#Blood     9.9 + 25.2 + 11.7              = 46.8  N2,8N,PD
#Clayton   2.3 + 22.1 + 11.7              = 36.1  N3,7N,PD
#Atonement       22.1        + 7.4        = 29.5     7N,   Gdr
#Juno            12.6 + 11.7              = 24.3     4N,PD

#Director - DN,DP,DPrN,DGA

delta.med.last[c(7,11,4,14)]
# 0.1298 1.2550 0.8110 3.6135
round(10*delta.med.last[c(7,11,4,14)],1)
# 1.3 12.5  8.1 36.1
round(10*Nom[6:10]*delta.med.last[7],1)
# 1.3  5.2  9.1 10.4 10.4
round(10*DPrN[6:10]*delta.med.last[4],1)
# 0.0 0.0 0.0 5.6 0.0
round(10*X[6:10,]%*%delta.med.last,1)

[,1]
[1,]  1.3
[2,] 17.7
[3,] 23.9
[4,] 70.6
[5,] 32.8

#Coens    5.9 + 10.4 + 12.5 + 5.6 + 36.1 = 70.6  N1,8N,DP,1PrN,DGA
#Anderson 9.9 + 10.4 + 12.5              = 32.8  N2,8N,DP
#Gilroy   2.3 +  9.1 + 12.5              = 23.9  N3,7N,DP
#Reitman         5.2 + 12.5              = 17.7     4N,DP
#Schnabel        1.3                     =  1.3     1N

#Actor - MP,MPrN,MPrW,Gm1,Gm2,SAM

delta.med.last[c(8,5,9,12,21,22)]
# 1.3320  0.6344 -3.2340  2.2710  1.4330  2.1420
round(10*delta.med.last[c(8,5,9,12,21,22)],1)
# 13.3   6.3 -32.3  22.7  14.3  21.4
round(10*MPrN[11:15]*delta.med.last[5],1)
# 0.0 8.8 7.0 0.0 0.0
round(10*MPrW[11:15]*delta.med.last[9],1)
# 0.0 -22.4   0.0   0.0   0.0
round(10*X[11:15,]%*%delta.med.last,1)

[,1]
[1,] 15.6
[2,] 53.7
[3,] 21.3
[4,]  0.0
[5,]  0.0

#DayLewis 9.9 + 13.3 + 8.8 - 22.4 + 22.7 + 21.4 = 53.7  N2,MP,3PrN,1PrW,Gm1,SAM
#Depp                + 7.0        + 14.3        = 21.3        2PrN      Gm2
#Clooney  2.3 + 13.3                            = 15.6  N3,MP
#Jones                                          =  0.0 
#Mortensen                                      =  0.0

#Actress - FP,FPrW,Gf1,Gf2,SAF

delta.med.last[c(10,6,13,17,23)]
# 0.8716 -1.1040  1.4920  0.9881  2.1660
round(10*delta.med.last[c(10,6,13,17,23)],1)
# 8.7 -11.0  14.9   9.9  21.7
round(10*FPrW[16:20]*delta.med.last[6],1)
# 0.0 -7.7  0.0  0.0  0.0
round(10*X[16:20,]%*%delta.med.last,1)

[,1]
[1,]  0.0
[2,] 28.9
[3,]  9.9
[4,]  0.0
[5,]  8.7

#Christie      - 7.7 + 14.9 + 21.7 = 28.9     1FPrW,Gf1,SAF
#Cotillard              9.9        =  9.9           Gf2
#Page      8.7                     =  8.7  FP
#Blanchett                         =  0.0
#Linney                            =  0.0