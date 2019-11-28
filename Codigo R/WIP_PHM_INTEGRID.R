####################################################################################################
####################################################################################################
###                                                                                              ###
###Projeto Integrid                                                                              ###
###R script to predict faults for MV/LV trnasformers and to calculate the failure probability.   ###
###Empresa: INESCTEC                                                                             ###
###Authors: Alexandra Oliveira, Luis Dias e Luis Guimaraes                                       ###
###                                                                                              ###
####################################################################################################
####################################################################################################

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
###############Functions R scripts#################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

# Fucntion to install and/or load the required packages
verificar_packages <- function(){
  
  #carregar os packages que permitem ler, analisar e transformar dados respetivamente
  Required_Packages=c("survival","survminer","MASS","compiler","ggfortify", "plyr", "dplyr", "readxl", "prodlim", "wesanderson","ggplot2","ggthemes","tcltk","plotly")
  
  Remaining_Packages <- Required_Packages[!(Required_Packages %in% installed.packages()[,"Package"])];
  
  if(length(Remaining_Packages))
  {
    install.packages(Remaining_Packages);
  }
  for(package_name in Required_Packages)
  {
    library(package_name,character.only=TRUE,quietly=TRUE);
  }
  
}

#Function to process and convert data to the format required by the INESC TEC algorithm
process_data<- function(data){
  
  #verify if the numerical variables are in the correct format
  intended_numeric_variables<-c("Manufacturing year","Data-freeze year","Data-freeze year","Age","Failure")
  for(i in intended_numeric_variables){
      #convert from character to numeric
      if(is.character(data[,i])==TRUE){
        data[,i]<-as.factor(unlist(data[,i]))
        data[,i]<-as.numeric(unlist(data[,i]))
      }
  }
    
  #convert from factor to numeric
  if(is.factor(data[,i])==TRUE){
    data[,i]<-as.numeric(unlist(data[,i]))
  }
    
  #verify if the categorical variables are in the correct format
  intended_categorical_variables<-c("District","Manufacturer","Installation_type","Technology")
  for(i in intended_categorical_variables){
      
    #convert from character to factor
    if(is.character(data[,i])==TRUE){
      data[,i]<-as.factor(unlist(data[,i]))
    }
  
    #convert from numeric to factor
    if(is.numeric(data[,i])==TRUE){
      data[,i]<-as.factor(unlist(data[,i]))
    }
  }
  
  #Remove NA values from the final dataset
  data[complete.cases(data),]
  
  return(data)
}

predict_failure<-function(transformer_data,baseline,algorithm_coefs){
  
  #exemplo ellevium (depois e preciso apagar)
  transformer_data$`Manufacturing year`<-c(2012,1994,1980,1970,2010)
  transformer_data$`Data-freeze year`<-c(2019,2019,2010,2005,2017)
  transformer_data$District<-c("Aveiro","Braga","Coimbra","Guarda","Porto")
  transformer_data$Manufacturer<-c("EFACEC","SIEMENS","EFACEC","SIEMENS","SIEMENS")
  transformer_data$Installation_type<-c("WITHOUT BOOTH","WITH BOOTH","WITH BOOTH","WITHOUT BOOTH","WITHOUT BOOTH")
  transformer_data$Technology<-c("ECONOMICAL","ROBUST","ROBUST","ROBUST","ECONOMICAL")

  #main variables for the plot
  Sx_predict<-array(data = NA, dim = nrow(baseline))
  label_predict<-array(data = NA, dim = nrow(baseline))
  Age_plot<-0
  
  for(i in 1:nrow(transformer_data)){
    
    #auxiliary variables
    Sx_predict_aux<-array(data = NA, dim = nrow(baseline))
    
    #adjust baseline according to the provided transformer data
    Sx_predict_aux <- baseline$Survival.probability^exp(algorithm_coefs[transformer_data$District[i]]+algorithm_coefs[transformer_data$Manufacturer[i]]+
                                                          algorithm_coefs[transformer_data$Technology[i]]+algorithm_coefs[transformer_data$Installation_type[i]])
    
    #save the label
    label_predict_aux<-array(data = i, dim = nrow(baseline))
    if(i==1){
      label_predict<-label_predict_aux
      Sx_predict<-Sx_predict_aux
      Age_plot<-seq(1,nrow(baseline))
    }else{
      label_predict<-c(label_predict,label_predict_aux)
      Sx_predict<-c(Sx_predict,Sx_predict_aux)
      Age_plot<-c(Age_plot,seq(1,nrow(baseline)))
    }
  }

    # Survival curves for each District
  plot_Sx_predict<-data.frame("Transformer"=as.factor(label_predict),"Age"=Age_plot,"Survival_probability"=Sx_predict)
  
  # plot results(output)
  Transformer_plot<-ggplot(plot_Sx_predict) +
    ggtitle("Transformer survival probability") + 
    xlab("PT age") + ylab("Survival probability (%)") +
    theme_bw() + ylim(0,1)+geom_line(aes(x = Age, y = Survival_probability, group = Transformer, colour = Transformer))+theme_economist()+ scale_fill_economist()
  
  #plotly version
  plot_font <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  plot_x_label <- list(
    title = "PT age",
    titlefont = plot_font
  )
  plot_y_label <- list(
    title = "Survival probability (%)",
    titlefont = plot_font
  )
  
  p <- plot_ly(plot_Sx_predict, x = ~Age, y = ~Survival_probability, color = ~Transformer, colors = "Paired") %>%
    add_lines()%>%
    layout(title = "Transformer survival probability", xaxis = plot_x_label, yaxis = plot_y_label)
  
  return(plot_Sx_predict)
}


#Function to build INESC TEC model (falta decidir os outputs que vamos guardar)!!!!!!!!
call_INESC_TEC_algorithm<- function(data,confidence_level,horizon){

  #Cox PHM model(output)
  phm <- coxph(Surv(Age, Failure) ~ District + Manufacturer + Installation_type + Technology, method="efron", data = data)
  
  #save variables coeficientes for future analysis(output)
  coefs<-summary(phm)
  coefs<-coefs$coefficients[1:nrow(coefs$coefficients),"coef"]
  
  #updata baseline values for the coefs array
  coefs[paste0("District",unique(data$District)[which.min(order(unique(data$District)))])]<-0
  coefs[paste0("Manufacturer",unique(data$Manufacturer)[which.min(order(unique(data$Manufacturer)))])]<-0
  coefs[paste0("Installation_type",unique(data$Installation_type)[which.min(order(unique(data$Installation_type)))])]<-0
  coefs[paste0("Technology",unique(data$Technology)[which.min(order(unique(data$Technology)))])]<-0
  
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
  Sx = S0 ^ exp(sum(t(beta)*x))
  ages<-times
  
  #Parameterization<---------------------------->
  
  #Data to change baseline to fit the curve
  Wdata<- data.frame(cbind(ages, Sx))

  #fit Weibull curve
  #starting parameters
  start_a<-10
  fitWeib<-0
  exit<-0
  
  while(fitWeib==0){
    try(fitWeib<-nls(Sx~exp(-(ages/a)^b),start=list(a=start_a,b=1),lower=c(.001,.001),algorithm = "port",trace=F,data=Wdata),silent = TRUE)
    start_a<-start_a+10
    if(is.list(fitWeib))break
    if(start_a>=500){
      print("Cannot obtain a continuous survivability curve with the provided data")
      exit<-1
      break
    }
  }
  
  #break algorithm in case of not being able to fit a continous curve to the provided data
  if (exit==1) {
    break
  }
  
  #Estimate parameters
  #a is the scale parameter
  params <- fitWeib$m$getPars() 
  a = params[["a"]]
  b = params[["b"]]
  
  #Evaluate the baseline (non-parameterized) to the a parameterized distribution (fitness evaluation)
  correlation<-cor(Sx,predict(fitWeib))
  
  #Update results based on the Weibull fit
  Wdata$Sx<-predict(fitWeib)
  
  #Vector with new  baseline for the different ages
  newS0<-c()
  newAges<-c()
  
  # Parametrization for each age, a survival probability
  for (i in 1:100){ #Maximum age specified is 100
    newS0[i]<- exp(-(i/a)^b) #Weibull
    newAges[i] <- i 
  }
  
  #data frame of the theoretical curve
  baseline<-data.frame("PT Age"= newAges, "Survival probability"=newS0)
  
  #baseline curve for all the equipment based on the provided data (output)
  baseline_plot<-ggplot(baseline) +
    geom_line(aes(x = PT.Age, y = Survival.probability)) + ggtitle("Baseline survival curve") + 
    xlab("PT age") + ylab("Survival probability (%)") +
    theme_bw() + ylim(0,1)
  
  #plotly version
  plot_font <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  plot_x_label <- list(
    title = "PT age",
    titlefont = plot_font
  )
  plot_y_label <- list(
    title = "Survival probability (%)",
    titlefont = plot_font
  )
  
  p <- plot_ly(baseline, x = ~PT.Age, y = ~Survival.probability, colors = "Paired") %>%
    add_lines()%>%
    layout(title = "Baseline survival curve", xaxis = plot_x_label, yaxis = plot_y_label)
  
  #################################################################################################################################
  ################################## Plots to study individual impact of each variable ############################################
  #################################################################################################################################
  
  ########################################################Plots to study individual impact of each variable##########################################
  # Survival curve for each District<---------------------------->
  #main variables for the plot
  Sx_district<-array(data = NA, dim = nrow(baseline))
  label_district<-array(data = NA, dim = nrow(baseline))
  Age_plot<-0
  
  for(i in unique(data[,"District"])){
    
    #auxiliary variables
    Sx_district_aux<-array(data = NA, dim = nrow(baseline))
    
    #adjust baseline according to the district
    Sx_district_aux <- baseline$Survival.probability^exp(coefs[paste0("District",i)])
    
    #save the label
    label_district_aux<-array(data = i, dim = nrow(baseline))
    if(which(unique(data[,"District"])==i)==1){
      label_district<-label_district_aux
      Sx_district<-Sx_district_aux
      Age_plot<-seq(1,nrow(baseline))
    }else{
      label_district<-c(label_district,label_district_aux)
      Sx_district<-c(Sx_district,Sx_district_aux)
      Age_plot<-c(Age_plot,seq(1,nrow(baseline)))
    }
  }
  
  #correct the values for the baseline District value
  Sx_district[which(is.na(Sx_district))]<-baseline$Survival.probability[1:nrow(baseline)]
  
  # Survival curves for each District
  plot_Sx_district<-data.frame("District"=as.factor(label_district),"Age"=Age_plot,"Survival_probability"=Sx_district)
  
  # plot results(output)
  District_plot<-ggplot(plot_Sx_district) +
    ggtitle("Each District impact on the survival probability") + 
    xlab("PT age") + ylab("Survival probability (%)") +
    theme_bw() + ylim(0,1)+geom_line(aes(x = Age, y = Survival_probability, group = District, colour = District))+theme_economist()+ scale_fill_economist()
  
  #plotly version
  plot_font <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  plot_x_label <- list(
    title = "PT age",
    titlefont = plot_font
  )
  plot_y_label <- list(
    title = "Survival probability (%)",
    titlefont = plot_font
  )
  
  p <- plot_ly(plot_Sx_district, x = ~Age, y = ~Survival_probability, color = ~District, colors = "Paired") %>%
    add_lines()%>%
    layout(title = "Each District impact on the survival probability", xaxis = plot_x_label, yaxis = plot_y_label)
    
  
  # Survival curves for each Installation type<---------------------------->
  #main variables for the plot
  Sx_Installation_type<-array(data = NA, dim = nrow(baseline))
  label_Installation_type<-array(data = NA, dim = nrow(baseline))
  Age_plot<-0
  
  for(i in unique(data[,"Installation_type"])){
    
    #auxiliary variables
    Sx_Installation_type_aux<-array(data = NA, dim = nrow(baseline))
    
    #adjust baseline according to the Installation type
    Sx_Installation_type_aux <- baseline$Survival.probability^exp(coefs[paste0("Installation_type",i)])
    
    #save the label
    label_Installation_type_aux<-array(data = i, dim = nrow(baseline))
    if(which(unique(data[,"Installation_type"])==i)==1){
      label_Installation_type<-label_Installation_type_aux
      Sx_Installation_type<-Sx_Installation_type_aux
      Age_plot<-seq(1,nrow(baseline))
    }else{
      label_Installation_type<-c(label_Installation_type,label_Installation_type_aux)
      Sx_Installation_type<-c(Sx_Installation_type,Sx_Installation_type_aux)
      Age_plot<-c(Age_plot,seq(1,nrow(baseline)))
    }
  }
  
  #correct the values for the baseline District value
  Sx_Installation_type[which(is.na(Sx_Installation_type))]<-baseline$Survival.probability[1:nrow(baseline)]
  
  # Survival curves for each District
  plot_Sx_Installation_type<-data.frame("Installation_type"=as.factor(label_Installation_type),"Age"=Age_plot,"Survival_probability"=Sx_Installation_type)
  
  # plot results(output)
  Installation_type_plot<-ggplot(plot_Sx_Installation_type) +
    ggtitle("Each Installation type impact on the survival probability") + 
    xlab("PT age") + ylab("Survival probability (%)") +
    theme_bw() + ylim(0,1)+geom_line(aes(x = Age, y = Survival_probability, group = Installation_type, colour = Installation_type))+theme_economist()+ scale_fill_economist()
  
  #plotly version
  plot_font <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  plot_x_label <- list(
    title = "PT age",
    titlefont = plot_font
  )
  plot_y_label <- list(
    title = "Survival probability (%)",
    titlefont = plot_font
  )
  
  p <- plot_ly(plot_Sx_Installation_type, x = ~Age, y = ~Survival_probability, color = ~Installation_type, colors = "Paired") %>%
    add_lines()%>%
    layout(title = "Each Installation type impact on the survival probability", xaxis = plot_x_label, yaxis = plot_y_label)
  
  
  # Survival curves for each available manufacturer<---------------------------->
  #main variables for the plot
  Sx_Manufacturer<-array(data = NA, dim = nrow(baseline))
  label_Manufacturer<-array(data = NA, dim = nrow(baseline))
  Age_plot<-0
  
  for(i in unique(data[,"Manufacturer"])){
    
    #auxiliary variables
    Sx_Manufacturer_aux<-array(data = NA, dim = nrow(baseline))
    
    #adjust baseline according to the Installation type
    Sx_Manufacturer_aux <- baseline$Survival.probability^exp(coefs[paste0("Manufacturer",i)])
    
    #save the label
    label_Manufacturer_aux<-array(data = i, dim = nrow(baseline))
    if(which(unique(data[,"Manufacturer"])==i)==1){
      label_Manufacturer<-label_Manufacturer_aux
      Sx_Manufacturer<-Sx_Manufacturer_aux
      Age_plot<-seq(1,nrow(baseline))
    }else{
      label_Manufacturer<-c(label_Manufacturer,label_Manufacturer_aux)
      Sx_Manufacturer<-c(Sx_Manufacturer,Sx_Manufacturer_aux)
      Age_plot<-c(Age_plot,seq(1,nrow(baseline)))
    }
  }
  
  #correct the values for the baseline District value
  Sx_Manufacturer[which(is.na(Sx_Manufacturer))]<-baseline$Survival.probability[1:nrow(baseline)]
  
  # Survival curves for each District
  plot_Sx_Manufacturer<-data.frame("Manufacturer"=as.factor(label_Manufacturer),"Age"=Age_plot,"Survival_probability"=Sx_Manufacturer)
  
  # plot results(output)
  Manufacturer_plot<-ggplot(plot_Sx_Manufacturer) +
    ggtitle("Each Manufacturer impact on the survival probability") + 
    xlab("PT age") + ylab("Survival probability (%)") +
    theme_bw() + ylim(0,1)+geom_line(aes(x = Age, y = Survival_probability, group = Manufacturer, colour = Manufacturer))+theme_economist()+ scale_fill_economist()
  
  #plotly version
  plot_font <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  plot_x_label <- list(
    title = "PT age",
    titlefont = plot_font
  )
  plot_y_label <- list(
    title = "Survival probability (%)",
    titlefont = plot_font
  )
  
  p <- plot_ly(plot_Sx_Manufacturer, x = ~Age, y = ~Survival_probability, color = ~Manufacturer, colors = "Paired") %>%
    add_lines()%>%
    layout(title = "Each Manufacturer impact on the survival probability", xaxis = plot_x_label, yaxis = plot_y_label)
  
  # Survival curves for each available Technology<---------------------------->
  #main variables for the plot
  Sx_Technology<-array(data = NA, dim = nrow(baseline))
  label_Technology<-array(data = NA, dim = nrow(baseline))
  Age_plot<-0
  
  for(i in unique(data[,"Technology"])){
    
    #auxiliary variables
    Sx_Technology_aux<-array(data = NA, dim = nrow(baseline))
    
    #adjust baseline according to the Installation type
    Sx_Technology_aux <- baseline$Survival.probability^exp(coefs[paste0("Technology",i)])
    
    #save the label
    label_Technology_aux<-array(data = i, dim = nrow(baseline))
    if(which(unique(data[,"Technology"])==i)==1){
      label_Technology<-label_Technology_aux
      Sx_Technology<-Sx_Technology_aux
      Age_plot<-seq(1,nrow(baseline))
    }else{
      label_Technology<-c(label_Technology,label_Technology_aux)
      Sx_Technology<-c(Sx_Technology,Sx_Technology_aux)
      Age_plot<-c(Age_plot,seq(1,nrow(baseline)))
    }
  }
  
  #correct the values for the baseline District value
  Sx_Technology[which(is.na(Sx_Technology))]<-baseline$Survival.probability[1:nrow(baseline)]
  
  # Survival curves for each District
  plot_Sx_Technology<-data.frame("Technology"=as.factor(label_Technology),"Age"=Age_plot,"Survival_probability"=Sx_Technology)
  
  # plot results(output)
  Technology_plot<-ggplot(plot_Sx_Technology) +
    ggtitle("Each Technology impact on the survival probability") + 
    xlab("PT age") + ylab("Survival probability (%)") +
    theme_bw() + ylim(0,1)+geom_line(aes(x = Age, y = Survival_probability, group = Technology, colour = Technology))+theme_economist()+ scale_fill_economist()
  
  
  #plotly version
  plot_font <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  plot_x_label <- list(
    title = "PT age",
    titlefont = plot_font
  )
  plot_y_label <- list(
    title = "Survival probability (%)",
    titlefont = plot_font
  )
  
  p <- plot_ly(plot_Sx_Technology, x = ~Age, y = ~Survival_probability, color = ~Technology, colors = "Paired") %>%
    add_lines()%>%
    layout(title = "Each Technology impact on the survival probability", xaxis = plot_x_label, yaxis = plot_y_label)
  
  #################################################################################################################################
  ####################### Predict number of failures between Current year and Prediction year #####################################
  #################################################################################################################################
  
  #Prediction horizon for the simulation
  horizon <- 3 #(input)

  data<-data[data$Failure==0, ] #PTs that have not failed yet
  
  #Save index of the dataframe
  index<-as.numeric(rownames(data))
  numPT<-nrow(data) #number of PTs
  
  #Coefficients vectors
  coef_District <- array(data = NA, dim = numPT)
  coef_Manufacturer <- array(data = NA, dim = numPT)
  coef_Installation_type <- array(data = NA, dim = numPT)
  coef_Technology <- array(data = NA, dim = numPT)
  
  #Probabilities
  S1 <- array(data = NA, dim = numPT) #Current year survival curve
  S2 <- array(data = NA, dim = numPT) #Year for the prediction survival curve
  PF <- array(data = NA, dim = numPT) # Failure probability between Current year and Prediction year

  # Filters
  
  #District
  for(i in unique(data[,"District"])){
    coef_District[which(data$District==i)]<-coefs[paste0("District",i)]  #filter District corresponding coeficient
  }
  coef_District[which(is.na(coef_District))]<-0  #filter District=1 <-- baseline
  
  #Manufacturer
  for(i in unique(data[,"Manufacturer"])){
    coef_Manufacturer[which(data$Manufacturer==i)]<-coefs[paste0("Manufacturer",i)]  #filter Manufacturer corresponding coeficient
  }
  coef_Manufacturer[which(is.na(coef_Manufacturer))]<-0  #filter Manufacturer=1 <-- baseline
  
  #Installation_type
  for(i in unique(data[,"Installation_type"])){
    coef_Installation_type[which(data$Installation_type==i)]<-coefs[paste0("Installation_type",i)]  #filter Installation_type corresponding coeficient
  }
  coef_Installation_type[which(is.na(coef_Installation_type))]<-0  #filter Installation_type=1 <-- baseline
  
  #Installation_type
  for(i in unique(data[,"Technology"])){
    coef_Technology[which(data$Technology==i)]<-coefs[paste0("Technology",i)]  #filter Technology corresponding coeficient
  }
  coef_Technology[which(is.na(coef_Technology))]<-0  #filter Technology=1 <-- baseline
  
  # create progress bar
  total <- numPT
  pb <- tkProgressBar(title = "Making predictions", min = 0, max = total, width = 300)
  
  #Failure prediction for the specified horizon --> Total number of failures
  for(i in 1:numPT){ 
      #update progress bar at multiples of 1000
      if(i%%1000==0 || i==numPT){
        setTkProgressBar(pb, i, label=paste( round(i/total*100, 0),"% completed"))
      }
    
      p<-match(data$Age[i],newAges) #get position
      S1[i]<- newS0[p]^exp(coef_District[i] + coef_Installation_type[i] + coef_Manufacturer[i] + coef_Technology[i]) #PT survival in current year
      
      p<-match((data$Age[i]+horizon),newAges) #get position
      S2[i]<- newS0[p]^exp(coef_District[i] + coef_Installation_type[i] + coef_Manufacturer[i] + coef_Technology[i]) #PT survival in Predicted Year
      
      PF[i] = (S1[i]-S2[i])/(S1[i]) #Failure probability between Current year and Prediction year
      S1<-c(S1,S1[i])
      S2<-c(S2,S2[i])
      PF<-c(PF,PF[i])
  }
  #finish progress bar
  close(pb)
  
  #Expected number of failures for the year 2xxx
  average_number_of_failures<-sum(PF)
  
  #Simulation to predict how many failures will occur
  #auxiliar index and counters
  failures<-0
  
  #number of iterations used in the simulation
  niter<-1000

  #save results for each simulated scenario
  total<-array(data=NA,dim = niter)
  
  # create progress bar
  total <- niter
  pb_scenarios <- tkProgressBar(title = "Generating scenarios", min = 0, max = total, width = 300)
  
  # check if generated scenario coincides with calculated PF
  for(a in 1:niter){
    
    #update progress bar at multiples of 10
    if(i%%10==0 || i==numPT){
      setTkProgressBar(pb_scenarios, a, label=paste( round(a/niter*100, 0),"% completed"))
    }
    
    #generating scenarions
    failures<-0
    for(i in 1:numPT){
      random<- (1-runif(1,0,1))*S1[i]
      if(random > S2[i]){ 
        failures<-failures+1
      }
      total[a]<-sum(failures)
    }
  }
  #finish progress bar
  close(pb_scenarios)
  
  # Mean variance Distribution parameters for the generated scenarios(output)
  miu<-mean(total)
  sigma<-sd(total)
  
  #Worst scenario for the total number of failures(output)
  Worst_case_scenario<-miu-qnorm(confidence_level,0,1)*sigma

  #Expected scenario for the total number of failures(output)
  Expected_case_scenario<-miu
  
  #Best scenario for the total number of failures(output)
  Best_case_scenario<-miu-qnorm(confidence_level,0,1)*sigma
}

#Test script
#load libraries
verificar_packages()

#Load the data
dados <- as.data.frame(read_xlsx("~/Desktop/Doutoramento DEGI/G-Candidaturas e Propostas/Projeto Europeu Integrid/Github/Dados/DatasetComplete_Ellevium.xlsx",sheet="DB_with_names"))
data<-process_data(dados)
#testar funcao main
call_INESC_TEC_algorithm(data,0.95,3)







