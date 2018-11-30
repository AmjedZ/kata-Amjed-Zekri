#Customer Data Analysis Dashboard
#Importing code
source.with.encoding("~/Data_cleaning.R","UTF-8")
source.with.encoding("~/crr.R","UTF-8")
source.with.encoding("~/ait.R","UTF-8")

#Importing libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyverse, shiny, shinydashboard, zoo, scales, ggplot2, stats)
#User Interface:

ui <- dashboardPage(
  
    dashboardHeader(title = "ICM Dashboard"),
    
    dashboardSidebar( 
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
        )
      ),
      
    dashboardBody(
      tabItems(
        # Dashboard content
        tabItem(tabName = "dashboard",
                fluidRow(
                #CRR
                box(title = "Customer Retention Rate & Retention Rate", collapsible = TRUE,
                    status = "primary", solidHeader = TRUE,
                    
                    selectizeInput(
                      inputId = 'siteSelection', label = "Selectionner un Site",
                      choices = c("Tous les sites",as.list(siteDescription)), multiple= FALSE
                    ),
                    selectizeInput(
                      inputId = 'perMonthSelection', 
                      label = "Selectionner la fréquence de calcul (X mois CRR)",
                      choices = c(1,2,3,4,6,12), multiple= FALSE
                    ),
                    plotOutput("plotCRR")
                    
                ),
                #AIT
                box(
                  title = "Average Inter-purchase Time", collapsible = TRUE,
                  status = "primary", solidHeader = TRUE,
                  selectizeInput(
                    inputId = 'siteSelectionAIT', label = "Selectionner un Site",
                    choices = c("Tous les sites",as.list(siteDescription)), multiple=FALSE
                  ),
                  sliderInput(
                    inputId = 'targetSelection', label = "Selectionnez votre Target",
                    min = 1, max = 30, value = 14, step = 1
                  ),
                  plotOutput("plotAIT")
                )
                )
        )
      )
    )
)
    

#Shiny Server

server <- function(input, output) {
  
  #Plot of the CRR
  output$plotCRR<-renderPlot({
    
    d<-crr(transactionByYear ,as.numeric(input$perMonthSelection),input$siteSelection)
    
    d$CRR<-as.numeric(sub("%", "e-2", d$CRR)) #convert to percentage
    #breaks according to the per x month selection
    if (input$perMonthSelection != 1){
      bks<-c(1,subset(c(1:nrow(d)),
                      c(1:nrow(d))%%as.numeric(input$perMonthSelection)==0))
    }
    else {
      bks<-subset(c(1:nrow(d)),
                  c(1:nrow(d))%%as.numeric(input$perMonthSelection)==0)
    }
    
    ggplot(subset(d,is.element(as.numeric(rownames(d)),bks)), 
           aes(as.factor(d$Mois[bks]),d$CRR[bks],group=1))+
      geom_point()+
      geom_line()+
      ggtitle(paste("CRR pour le site : ", input$siteSelection, sep = " "))+
      geom_text(label = percent(d$CRR[bks]), aes(y= d$CRR[bks] + 0.05), colour="red")+
      theme(panel.background = element_rect(fill = "lightblue"),
            legend.position = "none")+
      theme_bw()+
      xlab("Mois")+
      ylab("Customer Retention Rate (par mois)")+
      scale_y_discrete(labels=percent, limits = c(0,1))
    
  })
  #Plot of the AIT
  output$plotAIT<-renderPlot({
    
    #Filtering according to site Selection
    if (input$siteSelectionAIT == "Tous les sites"){
      d<-Calcul_AIT_All(ticketsPerCust)
    }else {
      d<-Calcul_AIT(ticketsPerCust, input$siteSelectionAIT)
    }
    
    
    #Plotting the AIT per month
    ggplot(d, aes(as.factor(d$date),d$AIT))+
      geom_point()+
      geom_line(group=1)+
      geom_hline(yintercept =as.numeric(input$targetSelection)  , color="blue")+
      geom_text(aes(0.8,input$targetSelection,label = "Target", vjust = -1), col="blue")+
      ggtitle(paste("AIT pour le site : ", input$siteSelectionAIT, sep = " "))+
      geom_text(label = d$AIT, aes(y= d$AIT + 1), colour="red")+
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
      xlab("Mois")+
      ylab("Average Interpurchase Time (jours)")+
      ylim(0, 30)
    
    
  } )
  
}
shinyApp(ui, server)
