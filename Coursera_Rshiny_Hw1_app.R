#
#Ping Hu
#9-29-2021
#
library(shiny)
library(tidyverse)
library(rsconnect)
#####Import Data

dat<-read_csv(url("https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1"))
dat<- dat %>% select(c("pid7","ideo5"))
dat<-drop_na(dat)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Survey Summary: 1 Strong Democrat -- 7 strong Republican"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Select the political view point on the Five Point Ideology scale ( 1 = Very liberal, 5=Very conservative):",
                     min = 1,
                     max = 5,
                     value = 3)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- dat[dat$ideo5==input$bins,]$pid7 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x,breaks=7, main="pid7: 1=Strong Democrat,4=Independent,7=strong Republican",
              xlab="pid7 political view points", col = 'darkgray', border = 'white')
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

