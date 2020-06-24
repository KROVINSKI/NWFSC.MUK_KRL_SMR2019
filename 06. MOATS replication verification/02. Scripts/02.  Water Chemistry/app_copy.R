#######
## MOATS Plots to launch the app

library(shiny)
library(ggplot2)
library(stringr)

options(shiny.maxRequestSize=50*1024^2) 

moatsIDs <- paste("M0", 1:9, sep="")
moatsIDs <- c(moatsIDs, paste("M", 10:13, sep = ""))


# Define UI ----
ui <- fluidPage(
  titlePanel("MOATS"),
  sidebarLayout(
    sidebarPanel( width = 2,
       fileInput("files", h4("File input"), multiple = TRUE, accept = c(".lvm", ".txt")),
       textInput("avgWin", "Moving average window (nObs)", "4"),
       radioButtons("plotType", h4("Plot Variable"),
                    choices = list("Aquarium Temperature" = "aTemperature",
                                   "Sensor Temperature" = "sTemperature",
                                   "pH" = "pH", 
                                   "Dissolved Oxygen" = "DO"), selected = "aTemperature"),
       checkboxGroupInput("moats", 
                          h4("Graph MOATS"), choices = moatsIDs,
                          selected = moatsIDs),
       sliderInput("ySlider", "Y-axis Range)", 
                   min = 0, max = 20, value = c(6, 12)),
       checkboxInput("yRangeCheckbox", "Limit Graph Y-axixs Range", value = FALSE),
       dateRangeInput("dates", h4("Date range"), start = "2019-09-01"),
       downloadButton("downloadData", "Download")
    ),
    mainPanel(
      width = 10,
      # this suppresses error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      plotOutput("plot", width = "100%", height = "740px")
      #dataTableOutput("dTable")
      )
  )
)

# Define server logic ----
server <- function(input, output) {
  values <- reactiveValues(mData = NULL, test = NULL)

  values$mData <- observeEvent(input$files, {
    
    # Create a Progress object
    progress <- shiny::Progress$new(min = 1, max = length(input$files$name))
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Processing File", value = 0)
    
    # d <- data.frame(moats = character(), dateTime = character(), aTemperature = numeric(), 
    #                 sTemperature = numeric(), pH = numeric(), DO = numeric(), salinity = numeric(),
    #                 stringsAsFactors = FALSE)
    d <- NULL
    for(i in 1:length(input$files$name)){
      # Set the progress bar, and update the detail text.
      progress$set(value = i, detail = paste(i, " of ", length(input$files$name)))
      moatsID <- word(input$files$name[i], 1, 1, sep = "_")
      fileStartDateTime <- word(input$files$name[i], 2, 3, sep = "_")
      fileStartDateTime <- as.POSIXct(strptime(fileStartDateTime, "%y-%m-%d_%H%M"))
      dtemp <- read.table(input$files$datapath[i], header = FALSE, sep = "\t", skip = 22, skipNul = TRUE)
      #difference (in seconds) from start of program and start of file
      offsetTime <- dtemp$V1[1]
      dateTime <- fileStartDateTime + dtemp$V1 - offsetTime
      aTemperature <- dtemp$V5
      sTemperature <- dtemp$V8
      pH <- dtemp$V2
      DO <- dtemp$V11
      salinity <- dtemp$V14
      moats <- rep(moatsID, length(pH))
      dx <- data.frame(moats, dateTime, aTemperature, sTemperature, pH, DO, salinity)
      #this next loop creates the short dataset, averaging over winlen number of records
      #take the total number of data rows, divide by winLen, and round to nearest integer.
      winLen <- as.numeric(input$avgWin)
      sLen <- as.integer(nrow(dx)/winLen)
      #create shortened data frame
      dShort <-dx[1:sLen, ]
      dStartIndex <- 1
      dEndIndex <- winLen
      for (j in 1:sLen){
        dShort[j, ]<- dx[dStartIndex, ]
        dShort[j, 3:7] <- apply(dx[dStartIndex:dEndIndex,3:7],2,mean)
        dStartIndex <- dStartIndex + winLen
        dEndIndex <- dEndIndex + winLen
      }
      d <- rbind(d, dShort)
      #output$dTable <- renderDataTable({d})
    }
    values$mData <- d
    
  })
  #output$testText <- renderText(values$test)

  output$plot <- renderPlot({
    d <- values$mData
    #convert to numberic
    d[3:7] <- sapply(d[3:7],as.numeric)
    #subset selected moats
    d <- subset(d, (moats %in% input$moats))
    startGraphDateTime <- as.POSIXct(as.Date(input$dates[1]))
    endGraphDateTime <- as.POSIXct(as.Date(input$dates[2]))
    #add a day to make sure it gets all the way to midnight
    endGraphDateTime <- endGraphDateTime + 86400
    d <- subset(d, dateTime >= startGraphDateTime & dateTime < endGraphDateTime)
    yLimits <- c(0, 20)
    if(input$plotType == "aTemperature"){
      yLimits <- c(min(d$aTemperature), max(d$aTemperature))
    }
    if(input$plotType == "sTemperature"){
      yLimits <- c(min(d$sTemperature), max(d$sTemperature))
    }
    if(input$plotType == "pH"){
      yLimits <- c(min(d$pH), max(d$pH))
    }
    if(input$plotType == "DO"){
      yLimits <- c(min(d$DO), max(d$DO))
    }
    if(input$yRangeCheckbox){
      yLimits <- c(input$ySlider[1], input$ySlider[2])
    }
    
    ggplot(d, aes_string("dateTime", input$plotType)) +
    geom_line(aes(colour = moats)) +
      ylim(yLimits) +
      theme_bw(base_size = 24)
  })
  
  #output$dTable <- renderDataTable(values$chData) 
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("MoatsData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$mData, file, row.names = FALSE)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)