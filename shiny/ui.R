ui = fluidPage(title = "Turbidity Cleaner", shinyjs::useShinyjs(),
  tabsetPanel(id = "tabset",
    #---------------------------------------------- 
    # UI code for Start Menu
    #----------------------------------------------
    tabPanel("Start",
             fluidRow(
               column(12, align = "center",
                      titlePanel(strong("Turbidity Cleaner")),
                      selectizeInput("level","Data Level", 
                                     choices = list(" " = "", 1,2,3), width = "100px"),
                      fileInput("turb1file", "Choose Level 1 Turbidity CSV File", accept = c("text/csv",
                                "text/comma-separated-values,text/plain", ".csv")),
                      fileInput("turb2file", "Choose Level 2 Turbidity CSV File", accept = c("text/csv",
                                "text/comma-separated-values,text/plain", ".csv")),
                      fileInput("turb3file", "Choose Level 3 Turbidity CSV File", accept = c("text/csv",
                                "text/comma-separated-values,text/plain", ".csv")),
                      fileInput("levelfile", "Choose Water Level CSV File", accept = c("text/csv",
                                "text/comma-separated-values,text/plain", ".csv")),
                      fileInput("precipfile", "Choose Precipitation CSV File", accept = c("text/csv",
                                "text/comma-separated-values,text/plain", ".csv"))
               )
             ),
             fluidRow(
               column(4),
               column(2, align = "center",
                      textInput("startdate","Start Date-time", value = "2015-01-01 00:00:00",
                                placeholder = "yyyy-mm-dd HH:MM:SS")
               ),
               column(2, align = "center",
                      textInput("enddate","End Date-time", value = "2019-01-01 00:00:00",
                                placeholder = "yyyy-mm-dd HH:MM:SS")
               ),
               column(4)
             ),
             fluidRow(
               column(12, align = "center",
                      actionButton("start", "Start"),
                      titlePanel(h6(Version)),
                      titlePanel(h6("Developed by Meguel Yousif and Hannah Burdett. 
                                    For any inquiries, please email mayousif@uwaterloo.ca."))
               )
             )
    ),
    #---------------------------------------------- 
    # UI code for LVL1 to LVL2  
    #----------------------------------------------
    tabPanel("LVL 1 to LVL 2",
      sidebarLayout(
        sidebarPanel(width = 3,
          numericInput("ResetValue","Reset Value", value = NA),
          numericInput("TransitionValue","Transition Value",value = NA),
          numericInput("MaxChange","Max Acceptable Change in Turbidity",value = NA),
          numericInput("MaxValue","Max Acceptable Turbidiity Value",value = NA),
          actionButton("run1", "Run"),
          actionButton("save1", "Save")
        ),
        
        mainPanel(width = 9,
          plotOutput("plot1", brush = brushOpts(id = "brush1"), dblclick = "dblclick1"),
          sliderInput("y_range1", 
                     "Choose Turbidity Range:", 
                     min = 0,
                     max = 5000,
                     value = c(0,5000),
                     ticks = F, width = "100%"),
          sliderInput("x_range1", 
                     "Choose X-Axis Range:", 
                     min = as.POSIXct("2015-01-01 12:00:00"),
                     max = as.POSIXct("2015-01-01 12:00:00"),
                     value = c(as.POSIXct("2015-01-01 12:00:00"),
                               as.POSIXct("2015-01-01 12:00:00")),
                     ticks = F, step = 900, timezone = +0000, width = "100%"),
          fluidRow(align = "center",
            actionGroupButtons(c("LVL1addhour","LVL1add1","LVL1add7","LVL1add30"), 
            c("+1 Hour","+1 Day", "+1 Week", "+1 Month"), size = "lg")
          ),
          fluidRow(align = "center",
            actionGroupButtons(c("LVL1removehour","LVL1remove1","LVL1remove7","LVL1remove30"), 
            c("-1 Hour","-1 Day", "-1 Week", "-1 Month"), size = "lg")
          )
        )
      )
    ),
    #---------------------------------------------- 
    # UI code for LVL2 to LVL3
    #----------------------------------------------
    tabPanel("LVL 2 to LVL 3",
      sidebarLayout(
        sidebarPanel(
          sliderInput("y_range2", 
                      "Choose Turbidity Range:", 
                      min = 0,
                      max = 5000,
                      value = c(0,5000),
                      ticks = F
          ),
          sliderInput("x_range2", 
                      "Choose X-Axis Range:", 
                      min = as.POSIXct("2015-01-01 12:00:00"),
                      max = as.POSIXct("2015-01-01 12:00:00"),
                      value = c(as.POSIXct("2015-01-01 12:00:00"),
                                as.POSIXct("2015-01-01 12:00:00")),
                      ticks = F, step = 900, timezone = +0000
          ),
          fluidRow(
            column(12, align = "center",
                   actionGroupButtons(c("LVL2removehour","LVL2remove1",
                                        "LVL2remove7","LVL2remove30"), 
                                      c("-1 Hour","-1 Day", "-1 Week", "-1 Month"))
            )
          ),
          fluidRow(
            column(12, align = "center",
                   actionGroupButtons(c("LVL2addhour","LVL2add1",
                                        "LVL2add7","LVL2add30"), 
                                      c("+1 Hour","+1 Day", "+1 Week", "+1 Month"))
            )
          ),
          fluidRow(
            column(12, align = "center",
                   actionGroupButtons(c("LVL2dayscale","LVL2weekscale","LVL2monthscale"), 
                                      c("Scale to Day","Scale to Week", "Scale to Month"))
            )
          ),
          fluidRow(
            column(12, align = "center",
                   radioGroupButtons("plottype2", choices = c("Turbidity", "Level", "Precip"), selected = "Turbidity")
            )
          ),
          fluidRow(
            selectInput("section_type2",label = "Section Type?",choices =  c("1 - To be modelled","2 - Replace with original"))
          ),
          actionButton("mark_area2", "Mark Section"),
          actionButton("save2", "Save")
        ),
        
        mainPanel(
          plotOutput(outputId = "plot2",brush = brushOpts(id = "brush2"),dblclick = "dblclick2", height = "500px")
        )
      )
    ),
    #---------------------------------------------- 
    # UI code for LVL3 to LVL4
    #----------------------------------------------
    tabPanel("LVL 3 to LVL 4",
      sidebarLayout(
        sidebarPanel(
          sliderInput("y_range3", 
                      "Choose Turbidity Range:", 
                      min = 0,
                      max = 5000,
                      value = c(0,5000),
                      ticks = F
          ),
          sliderInput("x_range3", 
                      "Choose X-Axis Range:", 
                      min = as.POSIXct("2015-01-01 12:00:00"),
                      max = as.POSIXct("2015-01-01 12:00:00"),
                      value = c(as.POSIXct("2015-01-01 12:00:00"),
                                as.POSIXct("2015-01-01 12:00:00")),
                      ticks = F, step = 900, timezone = +0000
          ),
          fluidRow(
            column(12, align = "center",
                   actionGroupButtons(c("LVL3removehour","LVL3remove1",
                                        "LVL3remove7","LVL3remove30"), 
                                      c("-1 Hour","-1 Day", "-1 Week", "-1 Month"))
            )
          ),
          fluidRow(
            column(12, align = "center",
                   actionGroupButtons(c("LVL3addhour","LVL3add1",
                                        "LVL3add7","LVL3add30"), 
                                      c("+1 Hour","+1 Day", "+1 Week", "+1 Month"))
            )
          ),
          fluidRow(
            column(12, align = "center",
                   actionGroupButtons(c("LVL3dayscale","LVL3weekscale","LVL3monthscale"), 
                                      c("Scale to Day","Scale to Week", "Scale to Month"))
            )
          ),
          fluidRow(
            column(12, align = "center",
                   radioGroupButtons("plottype3", choices = c("Turbidity", "Level", "Precip"), selected = NA)
            )
          ),
          actionButton("run3", "Run"),
          actionButton("save3", "Save")
        ),
        mainPanel(
          plotOutput(outputId = "plot3",brush = brushOpts(id = "brush3"),dblclick = "dblclick3")
        )
      )
    )
  )
)

