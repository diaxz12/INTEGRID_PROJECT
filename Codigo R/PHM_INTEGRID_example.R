library(survival)
library(survminer)
library(MASS)
library(compiler)
library(ggfortify)
library(plyr)
library(dplyr)
library(readxl)
library(prodlim)
library(wesanderson)


#Load the data (this path should be changed accordingly)
load("dados.rda")
#use this example when connecting to the database to retrieve data to use in the model 
#dados <- as.data.frame(read_xlsx("path_to_database"))

# Convert to categorical variables
dados$District <- as.factor(dados$District)
dados$Manufacturer <- as.factor(dados$Manufacturer)
dados$Installation_type <- as.factor(dados$Installation_type)
dados$Technology <- as.factor(dados$Technology)

#Load predictive model
load("modelo_INESCTEC.rda") 

#weights of each factor
coefs<-summary(phm)

########################################################Plots for each Individual impact##########################################
# Survival curves for each Installation type
PT_dados <- with(dados,
               data.frame(District = c('1', '1'),
                          Installation_type = c('1', '2'),
                          Manufacturer = c('1', '1'),
                          Technology = c('1', '1' ))
              )


# Fit Survival curve to the specified combination
fit <- survfit(phm, newdata = PT_dados)

#plot results
ggsurvplot(fit, conf.int = FALSE, legend.labs=c("Installation type = 1","Installation type=2"),
           data=PT_dados, linetype = 1, xlab="Age")

# Survival curves for each available manufacturer
MANU_dados <- with(dados,
              data.frame(District = c('1', '1', '1'),
                         Installation_type = c('1', '1', '1'),
                         Manufacturer = c('1', '2', '3'),
                         Technology = c('1', '1','1' ))
                )


# Fit Survival curve to the specified combination
fit <- survfit(phm, newdata = MANU_dados)

#plot results
ggsurvplot(fit, conf.int = FALSE, legend.labs=c("Manufacturer = 1","Manufacturer = 2", "Manufacturer = 3"),
           data=MANU_dados, linetype = 1, xlab="Age")

###############################################################################################################
# Theoretical curve for all power transformers based on the available data
d.phm = coxph.detail(phm)
times = c(0,d.phm$t)
h0 = c(0,d.phm$hazard)
S0 = exp(-cumsum(h0))

#theoretical curve parameters
beta = phm$coef
mean = c(phm$means)
x = c(0)-mean

#Theoretical curve discretized for each year
Sx = S0 ^ exp(t(beta) %*% x)
ages<-times

#data frame of the theoretical curve
baseline<-data.frame("PT Age"= times, "Survival probability"=Sx)

# Baseline survival curve plot
ggplot(baseline) +
  geom_line(aes(x = times, y = Sx)) + ggtitle("Baseline survival curve plot") + 
  xlab("PT Age") + ylab("Survival probability (%)") +
  theme_bw() + ylim(0,1)


