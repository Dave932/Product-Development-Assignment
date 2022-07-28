library(shiny)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(modeldata)
library(tidyverse)
library(plotly)
Universalbank <- Universalbank
Universalbank$Bins <- cut(Universalbank$Income,3, labels = c ("Lower", "Middle", "Upper"))
# This function will bin out income into three catagories, so that it will be easy for analysis.
ui <-
  dashboardPage( 
          dashboardHeader(title= "Analysis of Universal Banks", titleWidth = 270),
          dashboardSidebar( 
            sidebarMenu(
            menuItem("Plots", tabName = "dashboard", icon = icon("tree")), 
            menuItem("Table", tabName = "Table", icon = icon("table"), 
                     badgeLabel = "Summary", badgeColor ="green" )
            
          )),
          dashboardBody(
            tabItems(
              tabItem(tabName = "dashboard",
                      fluidRow(
                        tabBox(
                          id = "tabset1", height = "250px", 
                          tabPanel("Introduction", "Univesral bank is a relativly small bank
                                   that is growing rapidly interms of overall customer acquisation. the majority of the 
                                   customers are liability customers with varying size of relationship with the bank. 
                                   The bank is intersted to bring more loan business, in particular to 
                                   explore ways of converting its liabilty(deposit) customers to personal loan customers.
                                    The bank's dataset includes data on 5000 customers. the data include customer 
                                    demographic information (age, income, etc...), 
                                    customers response to the last personal loan campain and
                                            customers relation with the bank( mortgage, security account, etc..)  
                                    Income was a contineous variable in the original dataset, but I had to do preprocess 
                                            the data and bin income variable into three levels so that 
                                            I can clearly see the realtionship between it and accepting personal loan"), 
                          tabPanel("Variables", "ID, Personal Loan, Age, Experience, Income, Zip Code, Family, CCAvg,
                                            Education, Mortgage, Securities Account, CD Account, Online, CreditCard"),
                          tabPanel("Personal loan vs Income Plot", " From the barplot on the right, we can clearly say that the middle 
                                            and upper income custmors accepted the personal loan offer more than the lower income customers.
                                            So when we design our next loan campain, we should target those people with the positive 
                                            loan response (middle and upper income customers")
                        ), 
                        fluidRow(box(plotOutput("bank_bar")),
                                 box(title = "Histogram of Income",status = "primary",
                                     solidHeader = T, plotOutput("histogram")),
                                 box(title = "Control for Histogram", status = "primary", solidHeader = T, 
                                     sliderInput("bins", "Number of Breaks", 1, 100, 10)),
                       )
                       )
                      ),
              tabItem(tabName = "Table", 
                      fluidRow(
                        tabBox(
                          tabPanel("Plot", plotOutput("Plot")),
                          tabPanel("Delta", tableOutput("Universalbank")),
                          tabPanel("Summary", verbatimTextOutput("Summaries"))
                        ))) 
                      
              )
            )
          )


server <- function (input, output){
  Universalbank$Bins <- cut(Universalbank$Income,3, labels = c ("Lower", "Middle", "Upper"))
  
  output$histogram <- renderPlot({
      hist(Universalbank$Income, breaks = input$bins)
      })
  output$bank_bar<-renderPlot({
    Universalbank %>% 
      ggplot(aes(y=`Personal Loan`, x=Bins)) + 
      geom_point(position = "jitter", color = "blue", alpha = 1/5) + 
      ggtitle("Personal_Loan vs Income Level")+
      labs(x= "Income Level")+
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 13),
            plot.title = element_text(size = 15, face = "bold"))
  })
  output$Universalbank <-renderTable({
    Universalbank[, c(Universalbank$`Personal Loan`, Universalbank$Bins)]
  })
output$Summaries <- renderPrint({
  summary(Universalbank[, c("Personal_Loan",input$Bins)])
})
output$Plot <- renderPlot({
  with(Universalbank, boxplot(Universalbank$`Personal Loan`~Universalbank$Bins))
})
}


shinyApp(ui, server)