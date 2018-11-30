#Calculate the Average Inter-perchace Time for the month m of the year y

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,tidyverse,lubridate,scales, zoo)


  #ticketsPerCust  determines the number of purchase operations per Site-year-month and customer
  ticketsPerCust <- transactionData %>%
    group_by(Site=siteDescription, date=dateTime, card=cardId) %>%
    summarise(averagePerPerson=round(30/n_distinct(ticketNumber)))
  
#Calculate the AIT
  Calcul_AIT <- function(ticketsPerCust,site){
    AIT <- subset(ticketsPerCust, ticketsPerCust$Site==site) %>%
      group_by(date,Site)%>%
      summarise(AIT = round(mean(averagePerPerson))) 
  AIT<-data.frame(AIT)
  return(AIT)
  
  }
  Calcul_AIT_All<-function(ticketsPerCust)
  {
  AIT <- ticketsPerCust%>%
      group_by(date)%>%
      summarise(AIT = round(mean(averagePerPerson)))
    
  AIT<-data.frame(AIT)
    
    return(AIT)
  }


