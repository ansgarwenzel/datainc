
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library("shinyURL")
load("data_for_ui.RData") 
shinyUI(  
  navbarPage("Exploratory Analysis MOT Data UK", id="navbarid",
             tabPanel("Geographic Distribution of Passrates",
             
sidebarLayout(

  sidebarPanel(h2("Please choose"),
               br(),
               h4("Highlight a specific Postcode area:"),
               selectInput("postcode", label = "Select postcode to highlight",
                           choices = postcodes),
               br(),
               h4("Restrict to one type of test:"),
               selectInput("testtype", label="Select test type",
                           choices = testtypes),
               br(),
               textOutput("testtypetext"),
               br(),
               h4("Restrict to specific failure types:"),
               selectInput("resulttype",label="Select failure type",
                           choices = failures, selected = "P"
                  ),
               br(),
               textOutput("resulttypetext")
               ),
  mainPanel(
 #   h2("Distribution")
    plotOutput("plot1"),
    br(),
    h4("Explanation Test Types"),
    tableOutput("TableTest"),
    h4("Explanation Result Types"),
    tableOutput("TableResult")
    
  )
  
),value="panel1"
),tabPanel("Analysis Outcomes by Vehicle Age and Brand",
sidebarLayout(
  sidebarPanel(h2("Please choose"),
               br(),
               h4("Restrict to a specific class of vehicle"),
               selectInput("vehicleclass",label = "Select vehicle class",
                           choices = vehicleclasses),
               textOutput("vehicleclasstext"),
               br(),
               conditionalPanel(
                 condition = "input.vehicleclass=='All'",
                           selectInput("makes",label = "Select brand",
                                       choices = makes)
                 ),
               conditionalPanel(
                 condition = "input.vehicleclass==1",
                 selectInput("makes1",label = "Select brand",
                             choices = makes_1)
               ),
               conditionalPanel(
                 condition = "input.vehicleclass==2",
                 selectInput("makes2",label = "Select brand",
                             choices = makes_2)
               ),
               conditionalPanel(
                 condition = "input.vehicleclass==3",
                 selectInput("makes3",label = "Select brand",
                             choices = makes_3)
               ),
               conditionalPanel(
                 condition = "input.vehicleclass==4",
                 selectInput("makes4",label = "Select brand",
                             choices = makes_4)
               ),
               conditionalPanel(
                 condition = "input.vehicleclass=='4A'",
                 selectInput("makes4a",label = "Select brand",
                             choices = makes_4a)
               ),
               conditionalPanel(
                 condition = "input.vehicleclass==5",
                 selectInput("makes5",label = "Select brand",
                             choices = makes_5)
               ),
               conditionalPanel(
                 condition = "input.vehicleclass=='5A'",
                 selectInput("makes5a",label = "Select brand",
                             choices = makes_5a)
               ),
               conditionalPanel(
                 condition = "input.vehicleclass==7",
                 selectInput("makes7",label = "Select brand",
                             choices = makes_7)
               )
  ),
  mainPanel(
   # h2("Distribution")
    plotOutput("plot2"),
    br(),
    h4("Explanation Result Types"),
    tableOutput("TableResult2"),
    h4("Explanation Vehicle Classes"),
    tableOutput("TableVehicle")
  )
  ),value="panel2"
)
))
