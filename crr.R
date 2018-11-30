
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, stats,lubridate,scales, zoo)

#Converting dateTime to year-month
y<-year(transactionData$dateTime)
m<-month(transactionData$dateTime)
transactionData$dateTime<-as.yearmon(paste(y,m, sep="-"),"%Y-%m")
#removing y, m
rm(y,m)
monthsData<-unique(transactionData$dateTime)
siteDescription<-sort(unique(transactionData$siteDescription))
#regrouping data to determine the number of customers each month
transactionByYear<- data.frame(transactionData %>%group_by(site=transactionData$siteDescription, 
                                                           monthYear=transactionData$dateTime, cardId) %>%
                                 summarise(total=n()))

#crrChoice is the frequency of crr calculation according to the user's choice
crrChoice<-1
sites<- "site1"
#The function crr calculates the CRR for a data table transactionByYear
crr <-function(transactionByYear,crrChoice, sites){
  #Initialization of dt1: the data of the first month 
  if (sites== "Tous les sites"){
    dt1<-filter(transactionByYear,transactionByYear$monthYear == monthsData[1] )
  }else{
    dt1<-filter(transactionByYear,
                (transactionByYear$monthYear == monthsData[1] )&(transactionByYear$site==sites))
  }
  
  dt_ref<- dt1 #reference for crr calculation
  #Initialization of the total number of customers, new customers & CRR
  numb_tot<-length(dt1$cardId)
  numb_new<-c(0)
  
  crr_loc<-c(1)
  
  #Calculating the CRR for every month of mois_data
  
  for (i in 2:length(monthsData)){
    
    if (sites== "Tous les sites"){
      dt2<-filter(transactionByYear,(transactionByYear$monthYear == monthsData[i] ))
    }else{
      dt2<-filter(transactionByYear,
                  (transactionByYear$monthYear == monthsData[i] )&(transactionByYear$site==sites))
    }
    
    numb_tot<-c(numb_tot,length(dt2$cardId))
    numb_new<-c(numb_new,length(dt2$cardId)-length(intersect(dt1$cardId,dt2$cardId)))
    
    crr_loc<-c(crr_loc, (numb_tot[i]-numb_new[i])/numb_tot[i-1])
    if (is.nan(crr_loc[i]) | crr_loc[i]== 0|is.infinite(crr_loc[i])) 
    { 
      crr_loc[i]<-crr_loc[i-1]
    }
    
    if (i %% crrChoice == 0) {
      
      dt1<-dt_ref
      dt_ref<-dt2
    }
    
    else {
      #print(i %% choix_crr )
      dt1<- dt2
    }
    
    
  }
  
  #putting the data in a data_frame RR
  CRR_tab <-cbind(numb_tot,numb_new,percent(crr_loc))
  CRR_tab<-data.frame(CRR_tab)
  colnames(CRR_tab)<-c("Tot_cust","New_cust","CRR")
  
  RR<-data.frame( Mois= monthsData,CRR = CRR_tab$CRR)
  
  return(RR)
}
rm(crrChoice,sites)