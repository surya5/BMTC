library(shiny)
library("plotly")
library(lubridate)
require(rgl)
require(akima)
library(dplyr)


tripData <- read.csv("maturewithdate.csv")


attemp2 <- read.csv("attemp2.csv")
masterData = attemp2
colnames(masterData)[1:7] = c("schedule_no","route_no","from_stop_seq_no","till_stop_seq_no","tickets","passengers","total_ticket_amount")

masterData = masterData[complete.cases(masterData), ]

masterData1 = masterData[! masterData$route_no=="" , ]
masterData1 = masterData1[! masterData1$till_stop_seq_no == "",]
#masterData = masterData[!(length(masterData$route_no)<3 & length(masterData$route_no)>0), ]
masterData1 = na.omit(masterData)
#have only numerics in till_stop_seq_no
masterData1= masterData1[grep('^[0-9]', masterData1$till_stop_seq_no),]



masterData1[3:7] = lapply(masterData1[3:7], as.numeric)
masterData1=na.omit(masterData1)
## Data cleaning ends here



#reomve rows where total tickets is less than 20
masterData1 = masterData1[masterData1$tickets > 20 ,]
#remove rows where from stop is greater than equal to till stop
masterData1 = masterData1[(masterData1$from_stop_seq_no < masterData1$till_stop_seq_no),]


#remove rows that have more than 60 bus stops
masterData = masterData1[masterData1$till_stop_seq_no < 60 ,]



routes <- as.vector(unique(masterData[,'route_no']))

ui <- fluidPage(
  titlePanel("Demand Analysis"),
  selectInput("routeInput", "Select the route",
              choices = routes),
  plotlyOutput("plot"),
  plotlyOutput("occ"),
  mainPanel(
    textOutput("result1"),
    textOutput("result2"),
    textOutput("result3"),
    textOutput("result4")
  )
)


server <- function(input, output, session) {
  
  junk <- reactive(
    {
      y <- input$routeInput
      list(y = y)
    })
  
  trip <- reactive(
    {
      trips = sapply(tripData[(tripData$route_no == input$routeInput), c("count")], as.numeric)
      totalTrips <- sum(trips)
      totalTrips
    }
  )
  
  datesMax <- reactive(
    {
      dates = tripData[(tripData$route_no == input$routeInput), c("ticket_date")]
      parsedDates <- parse_date_time(dates,
                                     orders = c("d m y", "d B Y", "m/d/y"),
                                     locale = "eng")
      max(parsedDates)
    }
  )
  datesMin <- reactive(
    {
      dates = tripData[(tripData$route_no == input$routeInput), c("ticket_date")]
      parsedDates <- parse_date_time(dates,
                                     orders = c("d m y", "d B Y", "m/d/y"),
                                     locale = "eng")
      min(parsedDates)
    }
  )
  
  graph <- reactive(
    {
      dump = masterData[(masterData$route_no == input$routeInput), ]
      
      dump1 = dump
      #dump1[3:7] = sapply(dump[3:7], as.numeric)
      kasa1 <<- dump1
      dump1 = dump1%>% group_by(from_stop_seq_no,till_stop_seq_no) %>% summarize(Total_Revenue = sum(total_ticket_amount))
      
      kasa <<- dump1
      
      ######
      
      mydata = kasa1
      mydata1 <<- mydata
      max_stop = max(mydata[3], mydata[4])
      min_stop = min(mydata[3])
      stop_vector = seq(min_stop,max_stop,1)
      #my_func = function(x) (sum(mydata[6][mydata[3]==x]) - sum(mydata[6][mydata[4]==x]))
      
      occ_data = cumsum(sapply(stop_vector, function(x) (sum(mydata$passengers[mydata$from_stop_seq_no == x]) - sum(mydata$passengers[mydata$till_stop_seq_no == x]))))
      testy_value <<- occ_data
      testx_value <<- stop_vector
      xy_dataframe <<- cbind(testx_value,testy_value)
      junk_value  <<- junk_value +1
      
      
      
      #####
      
      
      
      
      
      # x = dump1[,1]
      # y = dump1[,2]
      # z = dump1[,3]
      x = dump1$from_stop_seq_no
      y = dump1$till_stop_seq_no
      z = dump1$Total_Revenue
      s=interp( y, x, z)
      #s=interp( y, x, z,testx_value,testy_value)
      s = c(s,a = list(testx_value),b = list(testy_value))
      final_matrix <<- s
      
    
    
      s
    })
  
  
 
  
  
  
  output$result1 <- renderText({
    paste("You chose", junk()$y)
  })
  
  output$result2 <- renderText({
    paste("Number of trips in this route is ",trip())
  })
  
  output$result3 <- renderText({
    paste("Earliest date: ",datesMin())
  })
  
  output$result4 <- renderText({
    paste("Latest date: ",datesMax())
  })
  
  output$plot <- renderPlotly({
    plot_ly(z = graph()$z) %>% add_surface()
    #plot_ly( x = xvalue(), y = yvalue(), type = 'scatter', mode = 'lines')
    #plot_ly( x = bus_occ()$vec1, y = bus_occ()$vec2, type = 'scatter', mode = 'lines')
  })
  
  
  output$occ <- renderPlotly({
    #plot_ly(z = graph()$z) %>% add_surface()
    # plot_ly( x = bus_occ()$vec1, y = bus_occ()$vec2, type = 'scatter', mode = 'lines')
    #plot_ly( data = xydf(),x = ~testx_value, y = ~testy_value, type = 'scatter', mode = 'lines')
    #plot_ly( x = xvalue(), y = yvalue(), type = 'scatter', mode = 'lines')
    plot_ly( x = graph()$a, y = graph()$b, type = 'scatter', mode = 'lines')
  })

}
shinyApp(ui = ui, server = server)
