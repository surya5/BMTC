library(shiny)
library("plotly")
library(lubridate)
require(rgl)
require(akima)
library(dplyr)
library(DT)

tripData <- read.csv("maturewithdate.csv")

attemp3 <- read.csv("attemp3.csv")
attemp3 = attemp3[,c(1,2,3,4,5,6)]
attemp3 = na.omit(attemp3)
attemp3 = attemp3[!attemp3$stopID=="",]
attemp3 = attemp3[!attemp3$stopID.1=="",]

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
    textOutput("result4"),
    DT::dataTableOutput("stops")
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
      # trips = sapply(tripData[(tripData$route_no == input$routeInput), c("count")], as.numeric)
      trips = as.numeric(tripData[,4][tripData$route_no == input$routeInput])
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
      testy_value <<- occ_data/trip()
      testx_value <<- stop_vector
      xy_dataframe <<- cbind(testx_value,testy_value)
      # junk_value  <<- junk_value + 1
      
      
      
      #####
      
      
      
      
      
      # x = dump1[,1]
      # y = dump1[,2]
      # z = dump1[,3]
      x = dump1$from_stop_seq_no
      y = dump1$till_stop_seq_no
      z = dump1$Total_Revenue/trip()
      s=interp( y, x, z)
      #s=interp( y, x, z,testx_value,testy_value)
      s = c(s,a = list(testx_value),b = list(testy_value))
      final_matrix <<- s
      
    
    
      s
    })
  
  stopsTable <- reactive(
    {
      bus = attemp3[(attemp3$route == input$routeInput), c(3,4,5,6)]

      bus1 = bus[,c(1,3)]
      bus2 = bus[,c(2,4)]
      names(bus2) <- c("stopID","stop")
      busFinal <<- rbind(bus1,bus2)
      busFinal$stopID <- as.numeric(busFinal$stopID)
      busFinal$stop <- as.character(busFinal$stop)
      busData <<- busFinal

      max_stop = max(busFinal[1])
      min_stop = min(busFinal[1])

      stopID <<- c(min_stop:max_stop)
      busTemp = data.frame(stopID)
      busTemp$stopID <- as.numeric(busTemp$stopID)
      busTemp <<- busTemp
      stopTable <- merge( busTemp, busData, by = "stopID")
      stopTable <- unique(stopTable)

      stopTable
    }
  )
  
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
    #plot_ly( x = bus_occ()$vec1, y = bus_occ()$vec2, type = 'scatter', mode = 'lines')
    #plot_ly( data = xydf(),x = ~testx_value, y = ~testy_value, type = 'scatter', mode = 'lines')
    #plot_ly( x = xvalue(), y = yvalue(), type = 'scatter', mode = 'lines')
    plot_ly( x = graph()$a, y = graph()$b, type = 'scatter', mode = 'lines')
  })
  
  output$stops <- renderDataTable({
    stopsTable()
  })

}
shinyApp(ui = ui, server = server)
