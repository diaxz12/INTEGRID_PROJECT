library(survival)
library(survminer)
library(MASS)
library(compiler)
library(ggfortify)
library(plyr)
library(dplyr)
library(readxl)
library(xlsx)
library(prodlim)
library(wesanderson)


#Load the data
df <- as.data.frame(read_xlsx("DatasetComplete.xlsx",sheet="Test2015"))

#Filter data
df <- df[, c("Distrito", 
             "Fabricante", 
             "Posto de Transformação",
             "Idade",
             "Tecnologia", 
             "Failure"
             )]

#Change col names
colnames(df) <- c("District", 
                  "Manufacturer", 
                  "PT", 
                  "Age", 
                  "Tec",
                  "Failure"
                  )


#Remove negative Ages -> in case of test 
df <- df[df$Age > 0, ]


# Convert to category variables
df$District <- as.factor(df$District)
df$Manufacturer <- as.factor(df$Manufacturer)
df$PT <- as.factor(df$PT)
df$Tec <- as.factor(df$Tec)

#Cox PHM model
phm <- coxph(Surv(Age, Failure) ~ District + Manufacturer + PT + Tec, method="efron", data = df)
coefs<-summary(phm)
################################################# Histograms ######################################################
# h<-hist(df$District[df$Failure==1],
#      main="Total number of failures per District", 
#      xlab="Districts", 
#      ylab="Number of failures",
#      border="gray28",
#      xlim=c(1,18),
#      col="light grey",
#      las=1
# )
# axis(side=1, at=seq(1,18, 1), labels =seq(1,18,by= 1), las=1 )
# text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
# 
# h<-hist(df$PT[df$Failure==1],
#      main="Total number of failures per disposal type", 
#      xlab="Disposal types", 
#      ylab="Number of failures" ,
#      border="gray28", 
#      col="light grey",
#      xaxt="none"
#     
# )
# axis(side=1, at=seq(1,2, by=1))
# text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
# 
# h<-hist(df$Manufacturer[df$Failure==1],
#      main="Total number of failures per manufactuer", 
#      xlab="Manufacturer", 
#      ylab="Number of failures" ,
#      border="gray28", 
#      col="light grey",
#      xaxt="none"
# )
# 
# axis(side=1, at=seq(1,3, by=1))
# text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
# 
# h<-hist(df$PT[df$Failure==1],
#         main="Total number of failures per technology", 
#         xlab="Technology", 
#         ylab="Number of failures" ,
#         border="gray28", 
#         col="light grey",
#         xaxt="none"
#         
# )
# axis(side=1, at=seq(1,2, by=1))
# text(h$mids,h$counts,labels=h$counts, adj=c(0.5, -0.5))
# 
# 
# 
# hist(df$Failure)
# hist(df$Age[df$Failure==1])
########################################################Individual impact##########################################
# New data
DRC_df <- with(df,
               data.frame(District = c('1', '2',  '9', '10', '13','15', '18'),
                          PT = c('1', '1','1', '1','1', '1','1'),
                          Manufacturer = c('1', '1', '1', '1','1', '1','1'),
                          Tec = c('1', '1', '1','1', '1','1','1')
               )
)

# Survival curves
fit <- survfit(phm, newdata = DRC_df)
plot(fit, data=DRC_df, col = c("red", "saddlebrown",  "orangered", "firebrick1", 
                               "limegreen",  "red4",  "darkviolet"),
     xlab='Age', ylab=expression(hat(S)(t)), lwd=2,  ylim=0:1)

legend(0.1, 0.5,  legend=c("District=1","District=2","District=9","District=10",
                           "District=13","District=15",
                            "District=18"),  c("red", "saddlebrown",  "orangered", "firebrick1", 
                                                                           "black",  "red4",  "darkviolet"),
       lty=1, cex=1.2, text.font=6,   box.lty=0, lwd=2)



# ggsurvplot(fit,conf.int = FALSE, legend.labs=c("District=1","District=2","District=3","District=4","District=5",
#                                            "District=6","District=7","District=8","District=9","District=10",
#                                            "District=11","District=12","District=13","District=14","District=15",
#                                            "District=16","District=17","District=18"),
#            data=DRC_df, linetype = 1, palette = c("firebrick","darkorchid4","lightgrey","turquoise1","black","midnightblue",
#                                                   "orangered", "seagreen", "red", "violetred1", "powderblue", "darkslategrey",
#                                                   "yellow", "lightpink4", "saddlebrown", "green", "steelblue2", "darkseagreen1"))
# 

PT_df <- with(df,
               data.frame(District = c('1', '1'),
                          PT = c('1', '2'),
                          Manufacturer = c('1', '1'),
                          Tec = c('1', '1' ))
              )


# Survival curves
fit <- survfit(phm, newdata = PT_df)
plot(fit, data=PT_df, xlab='Age', ylab=expression(hat(S)(t)), lwd=2,  ylim=0:1, col=c("brown2","dodgerblue4"))

legend(0.5, 0.25,  legend=c("Disposal=1","Disposal=2"), col=c("brown2","dodgerblue4"),
        lty=1, cex=1.3, text.font=6,  box.lty=0, lwd=2)

#ggsurvplot(fit, conf.int = FALSE, legend.labs=c("Disposal=1","Disposal=2"),
           #data=PT_df, linetype = 1, xlab="Age")

MANU_df <- with(df,
              data.frame(District = c('1', '1', '1'),
                         PT = c('1', '1', '1'),
                         Manufacturer = c('1', '2', '3'),
                         Tec = c('1', '1','1' ))
                )


# Survival curves
fit <- survfit(phm, newdata = MANU_df)
plot(fit, data=MANU_df, xlab='Age', ylab=expression(hat(S)(t)), lwd=2,  ylim=0:1, col=c("brown2","dodgerblue4", "seagreen"))

legend(0.5, 0.25,  legend=c("Manufacturer=1","Manufacturer=2", "Manufacturer=3"), col=c("brown2","dodgerblue4", "seagreen"),
       lty=1, cex=1.3, text.font=6,  box.lty=0, lwd=2)

# ggsurvplot(fit, conf.int = FALSE, legend.labs=c("Manufacturer=1","Manufacturer=2", "Manufacturer=3"),
#            data=DRC_df, linetype = 1, xlab="Age")


tec_df <- with(df,
              data.frame(District = c('1', '1'),
                         PT = c('1', '1'),
                         Manufacturer = c('1', '1'),
                         Tec = c('1', '2' ))
                      )


# Survival curves
fit <- survfit(phm, newdata = tec_df)
plot(fit, data=tec_df, xlab='Age', ylab=expression(hat(S)(t)), lwd=2,  ylim=0:1, col=c("brown2","dodgerblue4"))

legend(0.5, 0.25,  legend=c("Technology=1","Technology=2"), col=c("brown2","dodgerblue4"),
       lty=1, cex=1.3, text.font=6,  box.lty=0, lwd=2)

# ggsurvplot(fit, conf.int = FALSE, legend.labs=c("Technology=1","Techonology=2"),
#            data=DRC_df, linetype = 1, xlab="Age")

############################################Baselie kaplan meier################################################
km<-summary(survfit(Surv(Age,Failure)~ District + Manufacturer + PT + Tec , data=df))

table<-data.frame(Age = km$time,
                  Survival = km$surv,
                  Strata = km$strata)

table$Strata<-gsub("[^0-9.]","",table$Strata) #All codes
table<-table[table$Strata=="1111",]


S0_KM<-table$Survival

ages_KM<-table$Age

################################################################################################################

# Mean survival curve 
ggsurvplot(survfit(phm, data=df), palette = "#2E9FDF", xlab="Age", xscale=1, data=df )

# Baseline
d.phm = coxph.detail(phm)
times = c(0,d.phm$t)
h0 = c(0,d.phm$hazard)
S0 = exp(-cumsum(h0))

beta = phm$coef
mean = c(phm$means)
x = c(0)-mean
#Real baseline!!!!!!!!!!!!!!!!!!!!
Sx = S0 ^ exp(t(beta) %*% x)
ages<-times

# Baseline survival curve 
plot(times, Sx, xlab='Age', ylab=expression(hat(S0)(t)),
     ylim=0:1,  type="s",
     col="white", 
     #main="Baseline survival curves",
     lwd=2)

lines(ages, Sx, type="s", col="firebrick4", lwd=2)
lines(times, S0, type="s", col="deepskyblue4", lwd=2)
lines(ages_KM, S0_KM, type="s", col="limegreen", lwd=2)

legend(0, 0.25, legend=c("Cox PHM baseline", "Baseline with covariates at their mean value", "Kaplan-Meier baseline"),
       col=c("firebrick4","deepskyblue4","limegreen"), lty=1, cex=1.3, text.font=6,  box.lty=0, lwd=2)

#Prediction horizon
horizon <- 3

#################################################################################################################################
##################################################Parameterization###############################################################
#################################################################################################################################

#Data to change baseline to fit the curve
Wdata<- data.frame(cbind(ages, Sx))
Wdata_KM<-data.frame(cbind(ages_KM, S0_KM))
#Weibull
fitWeib<-nls(Sx~exp(-(ages/a)^b),start=list(a=70,b=1),lower=c(.001,.001),algorithm = "port",trace=F,data=Wdata)
fitWeib_KM<-nls(S0_KM~exp(-(ages_KM/a)^b),start=list(a=70,b=1),lower=c(.001,.001),algorithm = "port",trace=F,data=Wdata_KM)

#Estimate parameters
#a is the scale parameter
params <- fitWeib$m$getPars() 
a = params[["a"]]
b = params[["b"]]
cor(Sx,predict(fitWeib))

params_km <- fitWeib_KM$m$getPars() 
aK = params_km[["a"]]
bK = params_km[["b"]]
# Plot real baseline and its fit
plot(Sx~ages,
     data=Wdata, 
     type="l", 
     xlab="Age", 
     ylab=expression(hat(S0)(t)), 
     lty=1,
     col="firebrick4", 
     #main="Baseline Weibull Survival Curve",
     lwd=2)

lines(ages,predict(fitWeib),
      col="black", 
      xlab="Age", 
      ylab="Survival probability", 
      lty=2,
      lwd=2)
lines(S0_KM~ages_KM,
      data=Wdata_KM, 
      type="l", 
      xlab="Age", 
      ylab=expression(hat(S0)(t)), 
      lty=1,
      col="deepskyblue4", 
      #main="Baseline Weibull Survival Curve",
      lwd=2)
lines(ages_KM,predict(fitWeib_KM),
      col="grey", 
      xlab="Age", 
      ylab="Survival probability", 
      lty=2,
      lwd=2)


legend(0, 0.22, legend=c("Fitted curve", "Weibull survival function"),
       col=c("firebrick4","black"), lty=1:2, cex= 1.3, text.font=6,  box.lty=0, lwd=2)

#Vector with new  baseline for the different ages
newS0<-c()
newAges<-c()
# Parametrization -> for each age, a survival probability
for (i in 1:(max(df$Age)+horizon)){ #Maximum age is 88
  newS0[i]<- exp(-(i/a)^b) #Weibull
  newAges[i] <- i 
}

plot(newS0~newAges,
     type="l", 
     xlab="Age", 
     ylab=expression(hat(S0)(t)), 
     lty=1,
     col="firebrick4", 
     #main="Baseline Weibull Survival Curve",
     lwd=2)
legend(0, 0.2, legend=c(" Parametric baseline "),
       col=c("firebrick4"), lty=1, cex=1.3, text.font=6,  box.lty=0, lwd=2)

#################################################################################################################################
################################# Predict number of failures between 2015 and 2018 ####################################################
#################################################################################################################################

df<-df[df$Failure<1, ] #PTs that have not failed yet

#Variables
coefs<-coefs$coef[1:21]
index<-as.numeric(rownames(df))
n<-nrow(df) #number of PTs

#Coefficients vectors
coef_District <- c()
coef_Manufacturer <- c()
coef_PT <- c()
coef_Tec <- c()
#Probabilities
S1 <- c() #2015
S2 <- c() #2018
PF <- c() # Failure probability between 2015 and 2018

#Convert to numeric 
df$District<-as.numeric(df$District)
df$PT<-as.numeric(df$PT)
df$Manufacturer<-as.numeric(df$Manufacturer)
df$Tec<-as.numeric(df$Tec)

# Filters
District1 <- df[which(df$District == 1),] #filter District=1
District1rows<- as.numeric(rownames(District1)) # vector of ids

m1 <- df[which(df$Manufacturer == 1),] #filter manufacturer=1
m1rows<- as.numeric(rownames(m1)) # vector of ids

pt1 <- df[which(df$PT == 1),] #filter PT=1
pt1rows<- as.numeric(rownames(pt1)) # vector of ids

n1 <- df[which(df$Tec == 1),] #filter Tec=1
n1rows<- as.numeric(rownames(n1)) # vector of ids

Survival_probs<-matrix(data=NA,nrow=nrow(df),ncol=horizon+1)

for(i in 1:n){ #PTs
  
  # #District
  if (any ( index[i] == District1rows )){
    coef_District[i]<-0
    coef_District<-c(coef_District, coef_District[i])
  }
  else{
    coef_District[i]<-coefs[df$District[i]-1]
    coef_District<-c(coef_District, coef_District[i])
  }

  #PT
  if (any ( index[i] == pt1rows )){
    coef_PT[i]<-0
    coef_PT<-c(coef_PT, coef_PT[i])
  }
  else{
    coef_PT[i]<-coefs[df$PT[i]+18]
    coef_PT<-c(coef_PT, coef_PT[i])
  }

  #Manufaturer
  if (any ( index[i] == m1rows )){
    coef_Manufacturer[i]<-0
    coef_Manufacturer<-c(coef_Manufacturer, coef_Manufacturer[i])
  }
  else{
    coef_Manufacturer[i]<-coefs[df$Manufacturer[i]+16]
    coef_Manufacturer<-c(coef_Manufacturer, coef_Manufacturer[i])
  }
  
  #Tec
  if (any ( index[i] == n1rows )){
    coef_Tec[i]<-0
    coef_Tec<-c(coef_Tec, coef_Tec[i])
  }
  else{
    coef_Tec[i]<-coefs[df$Tec[i]+19]
    coef_Tec<-c(coef_Tec, coef_Tec[i])
  }
 
  # p<-match(df$Age[i],newAges) #get position first probability
  # Survival_probs[i,1]<- newS0[p]^exp(coef_District[i] + coef_PT[i] + coef_Manufacturer[i] + coef_Tec[i]) 
  # 
  # for(z in 1:horizon){
  #   p<-match(df$Age[i]+z,newAges) #get position
  #   Survival_probs[i,z+1]<- newS0[p]^exp(coef_District[i] + coef_PT[i] + coef_Manufacturer[i] + coef_Tec[i]) #PT survival in 201x
  # }
  # 
  # PF[i] = (Survival_probs[i,1] - Survival_probs[i,horizon+1])/Survival_probs[i,1]
  # S1<-c(S1,Survival_probs[i,1])
  # S2<-c(S2, Survival_probs[i,horizon+1])

  p<-match(df$Age[i],newAges) #get position
  S1[i]<- newS0[p]^exp(coef_District[i] + coef_PT[i] + coef_Manufacturer[i] + coef_Tec[i]) #PT survival in 201x

  p<-match((df$Age[i]+horizon),newAges) #get position
  S2[i]<- newS0[p]^exp(coef_District[i] + coef_PT[i] + coef_Manufacturer[i] + coef_Tec[i]) #PT survival in 2018

  PF[i] = (S1[i]-S2[i])/(S1[i]) #Failure probability between 2015 and 2018
  S1<-c(S1,S1[i])
  S2<-c(S2,S2[i])
  PF<-c(PF,PF[i])
}
sum(PF)

############################### Predict number of failures#### ####################################################
#Simulation to predict how many failures will occur
# i<-1
# failures<-0
# niter <- 1000
# a<-1
# total<-array(data=NA,dim = niter)
# for(a in 1:niter){
#   failures<-0
#   for(i in 1:n){
#     random<- (1-runif(1,0,1))*S1[i]
#     if(random > S2[i]){ # IF TRUE-> FAIL-> check if random number is inside PF; random number is S2
#       failures<-failures+1
#       }
#     total[a]<-sum(failures)
#   }
# }
# # Mean variance Distribution parameters
# miu<-mean(total)
# sigma<-sd(total)

#Simulation to predict in each year how many failures will occur
set.seed(384859)
i<-1
niter <- 1000
a<-1
total<-matrix(data=0,nrow=horizon,ncol=niter) #Colums are the scenarios and rows are the years

for(a in 1:niter){
  
  for(i in 1:n){
    random<- (1-runif(1,0,1))*S1[i]
    
    if(random > S2[i]){ # IF TRUE-> FAIL-> check if random number is inside PF; random number is S2
     
      if(random > Survival_probs[i,2]){ #Fails in year 1
        total[1,a]<-total[1,a]+1
      }
      else if(random <= Survival_probs[i,2] && random > Survival_probs[i,3]){#Fails in year 2
        total[2,a]<-total[2,a]+1
      }
      else if(random <= Survival_probs[i,3] && random > Survival_probs[i,4]){#Fails in year 3
        total[3,a]<-total[3,a]+1
      }
      else if(random <= Survival_probs[i,4] && random > Survival_probs[i,5]){#Fails in year 4
        total[4,a]<-total[4,a]+1
      }
      else if(random <= Survival_probs[i,5] && random > Survival_probs[i,6]){#Fails in year 5
        total[5,a]<-total[5,a]+1
      }
    }
    
  }
}
# Mean variance Distribution parameters
mean(total[2,])
sd(total[2,])


