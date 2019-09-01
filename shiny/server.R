server = function(input,output,session) {
  hideTab("tabset","LVL 1 to LVL 2",session)
  hideTab("tabset","LVL 2 to LVL 3",session)
  hideTab("tabset","LVL 3 to LVL 4",session)
  
  hideElement("turb1file")
  hideElement("turb2file")
  hideElement("turb3file")
  hideElement("levelfile")
  hideElement("precipfile")
  
  # Max file upload size (currently 100 MB)
  options(shiny.maxRequestSize=100*1024^2) 
  
  #----------------------------------------------
  # Server code for Start Menu
  #----------------------------------------------
  observeEvent(input$level, {
    if (input$level == 1){
      showElement("turb1file")
      hideElement("turb2file")
      hideElement("turb3file")
      hideElement("levelfile")
      hideElement("precipfile")
    } else if (input$level == 2) {
      showElement("turb1file")
      showElement("turb2file")
      hideElement("turb3file")
      showElement("levelfile")
      showElement("precipfile")
    } else if (input$level == 3) {
      showElement("turb1file")
      hideElement("turb2file")
      showElement("turb3file")
      showElement("levelfile")
      showElement("precipfile")
    }
  })
  
  observeEvent(input$start,{
    withProgress(message = "Loading",value = 0.5, {
      hideTab("tabset","Start",session)
      if ("1" %in% input$level) {
        showTab("tabset","LVL 1 to LVL 2",session = session)
      }
      if ("2" %in% input$level) {
        showTab("tabset","LVL 2 to LVL 3",session = session)
      }
      if ("3" %in% input$level) {
        showTab("tabset","LVL 3 to LVL 4",session = session)
      }
      
      DataType <<- as.numeric(input$level)
      StartDate <<- input$startdate
      EndDate <<- input$enddate
      
      
      if (input$level == "1") {
        TurbidityLVL1 <<- read.csv(input$turb1file$datapath)
        TurbidityLVL1[is.na(TurbidityLVL1)] <<- NA
        TurbidityLVL1$DateTime <<- as.POSIXct(TurbidityLVL1$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        TurbidityLVL1 <<- TurbidityLVL1[TurbidityLVL1$DateTime >= as.POSIXct(StartDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                                    & TurbidityLVL1$DateTime < as.POSIXct(EndDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),]
        TurbidityLVL1$DataValue[TurbidityLVL1$DataValue < 0] <<- NA # Makes code more efficient and simpler
        TurbidityLVL1["QualifierID"] <<- 0
        
        updateNumericInput(session,"ResetValue",
                           value = unname(quantile(TurbidityLVL1$DataValue,0.75,na.rm = T)))
        updateNumericInput(session,"TransitionValue",
                           value = unname(quantile(TurbidityLVL1$DataValue,0.9,na.rm = T)))
        updateNumericInput(session,"MaxChange",
                           value = unname(quantile(abs(diff(TurbidityLVL1$DataValue)), 0.99,na.rm= T)))
        updateNumericInput(session,"MaxValue",
                           value = 5000)
        updateSliderInput(session,"x_range1",
                          min = as.POSIXct(StartDate),
                          max = as.POSIXct(EndDate),
                          value = c(as.POSIXct(StartDate),
                                    as.POSIXct(StartDate) + 60*60*24*30),
                          timezone = +0000)
        
        observe({
          
          ResetValue <<- input$ResetValue
          TransitionValue <<- input$TransitionValue
          MaxChange <<- input$MaxChange
          MaxValue <<- input$MaxValue
      
        })
        
        output$plot1 = renderPlot({
          
          plotturb1 = subset(TurbidityLVL1,DateTime >= as.POSIXct(input$x_range1[1]) & DateTime <= as.POSIXct(input$x_range1[2]))
          
          ggplot(plotturb1,aes(DateTime, DataValue)) +
            geom_line(size = 1, colour = "darkgreen") +
            geom_point(shape = 23, size = 2, fill = "yellow",colour = "darkgreen") +
            ylim(input$y_range1[1],input$y_range1[2]) +
            ylab("Turbidiity") +
            scale_x_datetime(date_labels =  "%Y-%m-%d %R",limits = c(input$x_range1[1],input$x_range1[2])) +
            theme_linedraw() +
            theme(
              axis.title.y  = element_text(face = "bold", size=16, margin = margin(r=10)),
              axis.text.x  = element_text(size=12),
              axis.text.y  = element_text(size=12),
              axis.title.x = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=2)
            )
        })
  
      } else if (input$level == "2") {
        TurbidityLVL1 <<- read.csv(input$turb1file$datapath)
        TurbidityLVL1[is.na(TurbidityLVL1)] <<- NA
        TurbidityLVL1$DateTime <<- as.POSIXct(TurbidityLVL1$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        TurbidityLVL1 <<- TurbidityLVL1[TurbidityLVL1$DateTime >= as.POSIXct(StartDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                                        & TurbidityLVL1$DateTime < as.POSIXct(EndDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),]
        TurbidityLVL1$DataValue[TurbidityLVL1$DataValue < 0] <<- NA # Makes code more efficient and simpler
        TurbidityLVL1["QualifierID"] <<- 0
        
        TurbidityLVL2 <<- read.csv(input$turb2file$datapath)
        TurbidityLVL2[is.na(TurbidityLVL2)] <<- NA
        TurbidityLVL2$DateTime <<- as.POSIXct(TurbidityLVL2$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        TurbidityLVL2 <<- TurbidityLVL2[TurbidityLVL2$DateTime >= as.POSIXct(StartDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                                        & TurbidityLVL2$DateTime < as.POSIXct(EndDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),]
        TurbidityLVL2$DataValue[TurbidityLVL2$DataValue == -9999] <<- NA # Makes code more efficient and simpler
        
        PrecipData <<- read.csv(input$precipfile$datapath)
        PrecipData [PrecipData == "NULL"] <<- NA
        PrecipData$DateTime <<- as.POSIXct(PrecipData$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        PrecipData <<- PrecipData[PrecipData$DateTime >= as.POSIXct(StartDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                                        & PrecipData$DateTime < as.POSIXct(EndDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),]
        
        LevelData <<- read.csv(input$levelfile$datapath)
        LevelData[,1] <<- as.POSIXct(LevelData[,1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        LevelData <<- LevelData[LevelData[,1] >= as.POSIXct(StartDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                                & LevelData[,1] < as.POSIXct(EndDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),]
        
        updateSliderInput(session,"x_range2",
                          min = as.POSIXct(StartDate),
                          max = as.POSIXct(EndDate),
                          value = c(as.POSIXct(StartDate),
                                    as.POSIXct(StartDate) + 60*60*24*30),
                          timezone = +0000)
        
        ReactiveTurbidityLVL2 <<- reactiveValues(Data = TurbidityLVL2)
        
      } else if (input$level == "3") {
        TurbidityLVL1 <<- read.csv(input$turb1file$datapath)
        TurbidityLVL1[is.na(TurbidityLVL1)] <<- NA
        TurbidityLVL1$DateTime <<- as.POSIXct(TurbidityLVL1$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        TurbidityLVL1 <<- TurbidityLVL1[TurbidityLVL1$DateTime >= as.POSIXct(StartDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                                        & TurbidityLVL1$DateTime < as.POSIXct(EndDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),]
        TurbidityLVL1$DataValue[TurbidityLVL1$DataValue < 0] <<- NA # Makes code more efficient and simpler
        TurbidityLVL1["QualifierID"] <<- 0
        
        TurbidityLVL3 <<- read.csv(input$turb3file$datapath)
        TurbidityLVL3[is.na(TurbidityLVL3)] <<- NA
        TurbidityLVL3$DateTime <<- as.POSIXct(TurbidityLVL3$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        TurbidityLVL3 <<- TurbidityLVL3[TurbidityLVL3$DateTime >= as.POSIXct(StartDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                                        & TurbidityLVL3$DateTime < as.POSIXct(EndDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),]
        TurbidityLVL3$DataValue[TurbidityLVL3$DataValue == -9999] <<- NA # Makes code more efficient and simpler
        
        PrecipData <<- read.csv(input$precipfile$datapath)
        PrecipData [PrecipData == "NULL"] <<- NA
        PrecipData$DateTime <<- as.POSIXct(PrecipData$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        PrecipData <<- PrecipData[PrecipData$DateTime >= as.POSIXct(StartDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                                  & PrecipData$DateTime < as.POSIXct(EndDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),]
        
        LevelData <<- read.csv(input$levelfile$datapath)
        LevelData[,1] <<- as.POSIXct(LevelData[,1], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
        LevelData <<- LevelData[LevelData[,1] >= as.POSIXct(StartDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
                                & LevelData[,1] < as.POSIXct(EndDate,format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),]

        updateSliderInput(session,"x_range3",
                          min = as.POSIXct(StartDate),
                          max = as.POSIXct(EndDate),
                          value = c(as.POSIXct(StartDate),
                                    as.POSIXct(StartDate) + 60*60*24*30),
                          timezone = +0000)
        
        ReactiveTurbidityLVL3 <<- reactiveValues(Data = TurbidityLVL3)
        updateRadioGroupButtons(session,"plottype3", selected = "Turbidity")
        
      } 
    })
  })
  
  #----------------------------------------------
  # Server code for LVL1 to LVL2 Panel  
  #----------------------------------------------
  observeEvent(input$run1,{
    withProgress(message = 'Calculation in progress', value = 0.5, {
      
      TurbidityLVL2 <<- LVL1toLVL2(TurbidityLVL1,ResetValue,TransitionValue,MaxChange,MaxValue)
      setProgress(value = 1, message = "Complete")
      Sys.sleep(0.5)
      
    })
    
    output$plot1 = renderPlot({
      
      plotturb1 = subset(TurbidityLVL1,DateTime >= as.POSIXct(input$x_range1[1]) & DateTime <= as.POSIXct(input$x_range1[2]))
      plotturb2 = subset(TurbidityLVL2,DateTime >= as.POSIXct(input$x_range1[1]) & DateTime <= as.POSIXct(input$x_range1[2]))
      
      ggplot(plotturb1,aes(DateTime, DataValue)) +
        geom_line(size = 1, colour = "darkgreen") +
        geom_point(shape = 23, size = 2, fill = "yellow",colour = "darkgreen") +
        geom_line(data = plotturb2,
                  size = 1, colour = "black") +
        geom_point(data = plotturb2, 
                   shape = 23, size = 2, fill = "grey",colour = "black") +
        ylim(input$y_range1[1],input$y_range1[2]) +
        ylab("Turbidiity") +
        scale_x_datetime(date_labels =  "%Y-%m-%d %R",limits = c(input$x_range1[1],input$x_range1[2])) +
        theme_linedraw() +
        theme(
          axis.title.y  = element_text(face = "bold", size=16, margin = margin(r=10)),
          axis.text.x  = element_text(size=12),
          axis.text.y  = element_text(size=12),
          axis.title.x = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=2)
        )
    })
  })
  
  observeEvent(input$dblclick1, {
    brush = input$brush1
    if (!is.null(brush)) {
      updateSliderInput(session,"x_range1",value = c(as.POSIXct.numeric(
        brush$xmin, origin = "1970-01-01 00:00:00"),as.POSIXct.numeric(brush$xmax, origin = "1970-01-01 00:00:00")),
        min = as.POSIXct(StartDate),
        max = as.POSIXct(EndDate))
      updateSliderInput(session,"y_range1",value = c(brush$ymin,brush$ymax),
                        min = 0,
                        max = 5000)
    } else {
      
    }
  })
  
  observeEvent(input$LVL1addhour, {
    updateSliderInput(session,"x_range1",value = c(input$x_range1[1] + 3600,input$x_range1[2] + 3600),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  }) 
  observeEvent(input$LVL1add1, {
    updateSliderInput(session,"x_range1",value = c(input$x_range1[1] + 86400,input$x_range1[2] + 86400),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL1add7, {
    updateSliderInput(session,"x_range1",value = c(input$x_range1[1] + 604800,input$x_range1[2] + 604800),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL1add30, {
    updateSliderInput(session,"x_range1",value = c(input$x_range1[1] + 2592000,input$x_range1[2] + 2592000),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL1removehour, {
    updateSliderInput(session,"x_range1",value = c(input$x_range1[1] - 3600,input$x_range1[2] - 3600),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL1remove1, {
    updateSliderInput(session,"x_range1",value = c(input$x_range1[1] - 86400,input$x_range1[2] - 86400),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL1remove7, {
    updateSliderInput(session,"x_range1",value = c(input$x_range1[1] - 604800,input$x_range1[2] - 604800),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL1remove30, {
    updateSliderInput(session,"x_range1",value = c(input$x_range1[1] - 2592000,input$x_range1[2] - 2592000),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  
  observeEvent(input$save1, {
    withProgress(value = 0.5, message = "Saving data",{
      write.csv(TurbidityLVL2,paste(MainLocation,"LVL2.csv", sep = ""), row.names = F,na = "")
      setProgress(value = 1, message = "Save complete")
      Sys.sleep(3)
    })
  })
  
  #----------------------------------------------
  # Server code for LVL2 to LVL3 Panel  
  #----------------------------------------------
  observeEvent(input$mark_area2, {
    
    if (input$section_type2 == "1 - To be modelled"){
      
      # Cut isolated data, change value to NA, then add back into dataset. If no data is selected, do nothing.
      SelectedTurbidityLVL2 <- brushedPoints(ReactiveTurbidityLVL2$Data, input$brush2)
      SelectedTurbidityLVL2 <- SelectedTurbidityLVL2[is.na(SelectedTurbidityLVL2$DateTime) == F,]
      
      if (nrow(SelectedTurbidityLVL2) >= 1 ){
        
        SelectedTurbidityLVL2$DataValue = NA
        SelectedTurbidityLVL2$QualifierID = 0
        ReactiveTurbidityLVL2$Data <<- ReactiveTurbidityLVL2$Data[!ReactiveTurbidityLVL2$Data$DateTime %in% SelectedTurbidityLVL2$DateTime, ] # This cuts data
        ReactiveTurbidityLVL2$Data <<- rbind(ReactiveTurbidityLVL2$Data,SelectedTurbidityLVL2) # This adds data back with NA values
        
      } else {}
      
    } else {
      brush = input$brush2
      
      # Cut isolated data, change value to orignal value, then add back into dataset. If no data is selected, do nothing.
      SelectedTurbidityLVL1 <<- subset(TurbidityLVL1, DateTime >= as.POSIXct.numeric(brush$xmin, origin = "1970-01-01 00:00:00") & 
                                        DateTime <= as.POSIXct.numeric(brush$xmax, origin = "1970-01-01 00:00:00"))
      SelectedTurbidityLVL2 <<- subset(ReactiveTurbidityLVL2$Data, DateTime >= as.POSIXct.numeric(brush$xmin, origin = "1970-01-01 00:00:00") & 
                                        DateTime <= as.POSIXct.numeric(brush$xmax, origin = "1970-01-01 00:00:00"))
      
      if (nrow(SelectedTurbidityLVL1) >=1 & nrow(SelectedTurbidityLVL2) >= 1){
        SelectedTurbidityLVL1$QualifierID = 0
        ReactiveTurbidityLVL2$Data <<- ReactiveTurbidityLVL2$Data[!ReactiveTurbidityLVL2$Data$DateTime %in% SelectedTurbidityLVL2$DateTime,] # This cuts data
        ReactiveTurbidityLVL2$Data <<- rbind(ReactiveTurbidityLVL2$Data,SelectedTurbidityLVL1) # This adds data back with the orignal values
        
      } else {}
    }
  })
  
  observeEvent(input$dblclick2, {
    brush = input$brush2
    if (!is.null(brush)) {
      updateSliderInput(session,"x_range2",value = c(as.POSIXct.numeric(
        brush$xmin, origin = "1970-01-01 00:00:00"),as.POSIXct.numeric(brush$xmax, origin = "1970-01-01 00:00:00")),
        min = as.POSIXct(StartDate),
        max = as.POSIXct(EndDate))
      updateSliderInput(session,"y_range2",value = c(brush$ymin,brush$ymax),
                        min = 0,
                        max = 5000)
      
      
    } else {
      
    }
  })
  
  observeEvent(input$plottype2, {
    if (input$plottype2 == "Turbidity") {
      output$plot2 = renderPlot({
        
        plotturb1 = subset(TurbidityLVL1,DateTime >= as.POSIXct(input$x_range2[1]) & DateTime <= as.POSIXct(input$x_range2[2]))
        plotturb3 = subset(ReactiveTurbidityLVL2$Data,DateTime >= as.POSIXct(input$x_range2[1]) & DateTime <= as.POSIXct(input$x_range2[2]))
        
        ggplot(plotturb1,aes(DateTime, DataValue)) +
          geom_line(size = 1, colour = "darkgreen") +
          geom_point(shape = 23, size = 2, fill = "yellow",colour = "darkgreen") +
          geom_line(data = plotturb3,
                    size = 1, colour = "black") +
          geom_point(data = plotturb3, 
                     shape = 23, size = 2, fill = "grey",colour = "black") +
          ylim(input$y_range2[1],input$y_range2[2]) +
          ylab("Turbidiity") +
          scale_x_datetime(date_labels =  "%Y-%m-%d %R",limits = c(input$x_range2[1],input$x_range2[2])) +
          theme_linedraw() +
          theme(
            axis.title.y  = element_text(face = "bold", size=16, margin = margin(r=10)),
            axis.text.x  = element_text(size=12),
            axis.text.y  = element_text(size=12),
            axis.title.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=2)
          )
      })
    } else if (input$plottype2 == "Level") {
      output$plot2 = renderPlot({
        
        plotlevel = subset(LevelData,DateTime >= as.POSIXct(input$x_range2[1]) & DateTime <= as.POSIXct(input$x_range2[2]))
        
        ggplot(plotlevel,aes(DateTime, DataValue)) +
          geom_line(size = 1, colour = "orange") +
          geom_point(shape = 24,size = 2, colour = "black", fill = "gold") +
          ylim(max(LevelData$DataValue) - 4,max(LevelData$DataValue))+
          ylab("Water Level") +
          scale_x_datetime(date_labels =  "%Y-%m-%d %R",limits = c(input$x_range2[1],input$x_range2[2])) +
          theme_linedraw() +
          theme(
            axis.title.y  = element_text(face = "bold", size=16, margin = margin(r=10)),
            axis.text.x  = element_text(size=12),
            axis.text.y  = element_text(size=12),
            axis.title.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=2)
          )
      })
    } else if (input$plottype2 == "Precip") {
      output$plot2 = renderPlot({
        
        plotprecip = subset(PrecipData,DateTime >= as.POSIXct(input$x_range2[1]) & DateTime <= as.POSIXct(input$x_range2[2]))
        
        ggplot(plotprecip,aes(DateTime, DataValue)) +
          geom_line(size = 1, colour = "navyblue") +
          geom_point(shape = 23,size = 2, colour = "black", fill = "blue") +
          ylim(0, 20) +
          ylab("Precipitation") +
          scale_x_datetime(date_labels =  "%Y-%m-%d %R",limits = c(input$x_range2[1],input$x_range2[2])) +
          theme_linedraw() +
          theme(
            axis.title.y  = element_text(face = "bold", size=16, margin = margin(r=10)),
            axis.text.x  = element_text(size=12),
            axis.text.y  = element_text(size=12),
            axis.title.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=2)
          )
      })
    }
  })
  
  observeEvent(input$LVL2addhour, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1] + 3600,input$x_range2[2] + 3600),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })  
  observeEvent(input$LVL2add1, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1] + 86400,input$x_range2[2] + 86400),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL2add7, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1] + 604800,input$x_range2[2] + 604800),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL2add30, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1] + 2592000,input$x_range2[2] + 2592000),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL2removehour, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1] - 3600,input$x_range2[2] - 3600),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL2remove1, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1] - 86400,input$x_range2[2] - 86400),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL2remove7, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1] - 604800,input$x_range2[2] - 604800),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL2remove30, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1] - 2592000,input$x_range2[2] - 2592000),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL2dayscale, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1],input$x_range2[1] + 86400),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL2weekscale, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1],input$x_range2[1] + 604800),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL2monthscale, {
    updateSliderInput(session,"x_range2",value = c(input$x_range2[1],input$x_range2[1] + 2592000),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  
  observeEvent(input$save2, {
    withProgress(value = 0.5, message = "Saving data",{
      TurbidityLVL3 <<- ReactiveTurbidityLVL2$Data
      
      write.csv(TurbidityLVL3,paste(MainLocation,"LVL3.csv", sep = ""), row.names = F, na = "")
      setProgress(value = 1, message = "Save complete")
      Sys.sleep(3)
    })
  })
  
  #----------------------------------------------
  # Server code for LVL3 to LVL4 Panel  
  #----------------------------------------------
  observeEvent(input$dblclick3, {
    brush = input$brush3
    if (!is.null(brush)) {
      updateSliderInput(session,"x_range3",value = c(as.POSIXct.numeric(
        brush$xmin, origin = "1970-01-01 00:00:00"),as.POSIXct.numeric(brush$xmax, origin = "1970-01-01 00:00:00")),
        min = as.POSIXct(StartDate),
        max = as.POSIXct(EndDate))
      updateSliderInput(session,"y_range3",value = c(brush$ymin,brush$ymax),
                        min = 0,
                        max = 5000)
      
      
    } else {
      
    }
  })
  
  observeEvent(input$plottype3, {
    if (input$plottype3 == "Turbidity") {
      output$plot3 = renderPlot({
        
        plotturb1 = subset(TurbidityLVL1,DateTime >= as.POSIXct(input$x_range3[1]) & DateTime <= as.POSIXct(input$x_range3[2]))
        plotturb4 = subset(ReactiveTurbidityLVL3$Data,DateTime >= as.POSIXct(input$x_range3[1]) & DateTime <= as.POSIXct(input$x_range3[2]))
        
        # Setup vector of fill for level 4 data points (grey are measured values, orange are modelled values)
        lvl4colours = as.data.frame(plotturb4[["QualifierID"]])
        if (length(lvl4colours[lvl4colours[,1] == 0,1]) > 0) {
          lvl4colours[lvl4colours[,1] == 0,2] = "grey"
        }
        if (length(lvl4colours[lvl4colours[,1] == 1,1]) > 0){
          lvl4colours[lvl4colours[,1] == 1,2] = "orange"
        }

        ggplot(plotturb1,aes(DateTime, DataValue)) +
          geom_line(size = 1, colour = "darkgreen") +
          geom_point(shape = 23, size = 2, fill = "yellow",colour = "darkgreen") +
          geom_line(data = plotturb4,
                    size = 1, colour = "black") +
          geom_point(data = plotturb4, 
                     shape = 23, size = 2, fill = 
                        if (nrow(lvl4colours) > 0){lvl4colours[[2]]}
                        else{ "white"},
                     colour = "black") +
          ylim(input$y_range3[1],input$y_range3[2]) +
          ylab("Turbidiity") +
          scale_x_datetime(date_labels =  "%Y-%m-%d %R",limits = c(input$x_range3[1],input$x_range3[2])) +
          theme_linedraw() +
          theme(
            axis.title.y  = element_text(face = "bold", size=16, margin = margin(r=10)),
            axis.text.x  = element_text(size=12),
            axis.text.y  = element_text(size=12),
            axis.title.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=2)
          )
      })
    } else if (input$plottype3 == "Level") {
      output$plot3 = renderPlot({
        
        plotlevel = subset(LevelData,DateTime >= as.POSIXct(input$x_range3[1]) & DateTime <= as.POSIXct(input$x_range3[2]))
        
        ggplot(plotlevel,aes(DateTime, DataValue)) +
          geom_line(size = 1, colour = "orange") +
          geom_point(shape = 24,size = 2, colour = "black", fill = "gold") +
          ylim(max(LevelData$DataValue) - 4,max(LevelData$DataValue)) +
          ylab("Water Level") +
          scale_x_datetime(date_labels =  "%Y-%m-%d %R",limits = c(input$x_range3[1],input$x_range3[2])) +
          theme_linedraw() +
          theme(
            axis.title.y  = element_text(face = "bold", size=16, margin = margin(r=10)),
            axis.text.x  = element_text(size=12),
            axis.text.y  = element_text(size=12),
            axis.title.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=2)
          )
      })
    } else if (input$plottype3 == "Precip") {
      output$plot3 = renderPlot({
        
        plotprecip = subset(PrecipData,DateTime >= as.POSIXct(input$x_range3[1]) & DateTime <= as.POSIXct(input$x_range3[2]))
        
        ggplot(plotprecip,aes(DateTime, DataValue)) +
          geom_line(size = 1, colour = "navyblue") +
          geom_point(shape = 23,size = 2, colour = "black", fill = "blue") +
          ylim(0, 20) +
          ylab("Precipitation") +
          scale_x_datetime(date_labels =  "%Y-%m-%d %R",limits = c(input$x_range3[1],input$x_range3[2])) +
          theme_linedraw() +
          theme(
            axis.title.y  = element_text(face = "bold", size=16, margin = margin(r=10)),
            axis.text.x  = element_text(size=12),
            axis.text.y  = element_text(size=12),
            axis.title.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=2)
          )
      })
    }
  })
  
  observeEvent(input$run3,{
    withProgress(message = 'Calculation in progress', value = 0.4, {
      
      source(paste(ScriptLocation,"\\LVL3toLVL4.R", sep = ""))
      
      ModelledData <<- cbind.data.frame(alldata$DateTime,dff)
      colnames(ModelledData) <<- c("DateTime","DataValue")
      
      # Get datetime of missing values and fill with modelled
      MissingDates <<- data.frame("DateTime" = ReactiveTurbidityLVL3$Data[is.na(ReactiveTurbidityLVL3$Data[["DataValue"]]), "DateTime"])
      MissingDates[,2] <<- ModelledData$DataValue[match(MissingDates$DateTime,ModelledData$DateTime)]
      
      ReactiveTurbidityLVL3$Data[match(MissingDates$DateTime, ReactiveTurbidityLVL3$Data[["DateTime"]]),"DataValue"] <<- MissingDates[[2]]
      ReactiveTurbidityLVL3$Data[match(MissingDates$DateTime, ReactiveTurbidityLVL3$Data[["DateTime"]]), "QualifierID"] <<- 1
      
      output$plot3 = renderPlot({
        
        plotturb1 = subset(TurbidityLVL1,DateTime >= as.POSIXct(input$x_range3[1]) & DateTime <= as.POSIXct(input$x_range3[2]))
        plotturb4 = subset(ReactiveTurbidityLVL3$Data,DateTime >= as.POSIXct(input$x_range3[1]) & DateTime <= as.POSIXct(input$x_range3[2]))
        
        # Setup vector of fill for level 4 data points (grey are measured values, orange are modelled values)
        lvl4colours = as.data.frame(plotturb4[["QualifierID"]])
        if (length(lvl4colours[lvl4colours[,1] == 0,1]) > 0) {
          lvl4colours[lvl4colours[,1] == 0,2] = "grey"
        }
        if (length(lvl4colours[lvl4colours[,1] == 1,1]) > 0){
          lvl4colours[lvl4colours[,1] == 1,2] = "orange"
        }
        
        ggplot(plotturb1,aes(DateTime, DataValue)) +
          geom_line(size = 1, colour = "darkgreen") + 
          geom_point(shape = 23, size = 2, fill = "yellow",colour = "darkgreen") +
          geom_line(data = plotturb4,
                    size = 1, colour = "black") +
          geom_point(data = plotturb4, 
                     shape = 23, size = 2, fill = 
                        if (nrow(lvl4colours) > 0){lvl4colours[[2]]}
                        else{ "white"},
                     colour = "black") +
          ylim(input$y_range3[1],input$y_range3[2]) +
          ylab("Turbidiity") +
          scale_x_datetime(date_labels =  "%Y-%m-%d %R",limits = c(input$x_range3[1],input$x_range3[2])) +
          theme_linedraw() +
          theme(
            axis.title.y  = element_text(face = "bold", size=16, margin = margin(r=10)),
            axis.text.x  = element_text(size=12),
            axis.text.y  = element_text(size=12),
            axis.title.x = element_blank(),
            panel.border = element_rect(colour = "black", fill=NA, size=2)
          )
      })
      
      setProgress(value = 1, message = "Complete")
      Sys.sleep(0.5)
      
    })

  })
  
  observeEvent(input$LVL3addhour, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1] + 3600,input$x_range3[2] + 3600),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })  
  observeEvent(input$LVL3add1, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1] + 86400,input$x_range3[2] + 86400),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL3add7, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1] + 604800,input$x_range3[2] + 604800),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL3add30, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1] + 2592000,input$x_range3[2] + 2592000),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL3removehour, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1] - 3600,input$x_range3[2] - 3600),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL3remove1, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1] - 86400,input$x_range3[2] - 86400),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL3remove7, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1] - 604800,input$x_range3[2] - 604800),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL3remove30, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1] - 2592000,input$x_range3[2] - 2592000),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL3dayscale, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1],input$x_range3[1] + 86400),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL3weekscale, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1],input$x_range3[1] + 604800),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  observeEvent(input$LVL3monthscale, {
    updateSliderInput(session,"x_range3",value = c(input$x_range3[1],input$x_range3[1] + 2592000),
                      min = as.POSIXct(StartDate),
                      max = as.POSIXct(EndDate))
  })
  
  observeEvent(input$save3, {
    withProgress(value = 0.5, message = "Saving data",{
      TurbidityLVL4 <<- ReactiveTurbidityLVL3$Data
      
      write.csv(TurbidityLVL4,paste(MainLocation,"LVL4.csv", sep = ""), row.names = F,na = "")
      setProgress(value = 1, message = "Save complete")
      Sys.sleep(3)
    })
  })
}