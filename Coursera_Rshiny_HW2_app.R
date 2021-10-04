#
#ping Hu
#9-30-2021
#
library(rsconnect)
library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(ggplot2)

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5","newsint","gender","educ","CC18_308a","region"))
dat<-drop_na(dat)


# Define UI for application that draws a histogram
ui <- navbarPage("My Application",
  tabPanel("Page 1",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          sliderInput("bins",
                      "Select the political view point on the Five Point Ideology scale ( 1 = Very liberal, 5=Very conservative):",
                      min = 1,max = 5,value = 3)),
      mainPanel(
        tabsetPanel(
          tabPanel("Tab1", plotOutput("distPlot")),
          tabPanel("Tab2", plotOutput("TrumpPlot"))
  ))))),
  
  tabPanel("Page 2",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("ggg", 
                             h3("Select Gender"), 
                             choices = list("Male 1" = 1, 
                                            "Female 2" = 2),
                             selected = 1
                             )
        ),
        mainPanel(plotOutput("pg2Plot"))
      )
    )
  ),
  
  tabPanel("Page 3",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput("sRegion", "Select Region:",
                      c("Northwest 1" = 1,
                        "MidWest 2" = 2,
                        "South 3" = 3,
                        "West 4" = 4))
        ),
        mainPanel(DT::dataTableOutput("pg3Table"))
    )))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    x    <- dat[dat$ideo5==input$bins,]$pid7 
    hist(x,breaks=7, main="pid7: 1=Strong Democrat,4=Independent,7=strong Republican",
         xlab="pid7 political view points", col = 'darkgray', border = 'white')
  })
  
  output$TrumpPlot <- renderPlot({
    x    <- dat[dat$ideo5==input$bins,]$CC18_308a 
    hist(x, main="CC18_308a Trump Support: 1=strongly approve, 4=strongly disapprove", breaks=c(0,1,2,3,4),
         xlab="Trump Support", col = 'darkgray', border = 'white')
  })   
  
  output$pg2Plot <- renderPlot({
    print(input$ggg)
    data2 =data.frame( dat[dat$gender==input$ggg,] )
    ttt<-ggplot( data2, aes(x = educ, y = pid7)) +
      geom_jitter() + geom_smooth(method = lm)
    ggplotly(p = ttt,
             height = 650,
             width = 480)
   
  })   
  
  output$pg3Table = DT::renderDataTable({
    dat[dat$region==input$sRegion,]}
  )

}

# Run the application 
shinyApp(ui = ui, server = server)

