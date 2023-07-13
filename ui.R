#######################################################################
# User interface for the drawROI shiny app. 
# 
# The drawROI app is developed and maintained by Bijan Seyednasrollah.
# The main initial development was done in May to November, 2017.
#
# Most recent release: https://github.com/bnasr/drawROI
#######################################################################
print('UI Page Top')

fluidPage(
  theme= shinytheme('darkly'),
  shinyjs::useShinyjs(),
  tags$head(tags$style(HTML( "#Select1 ~ .selectize-control.single .selectize-input {border: 1px solid #fff;}"))),
  # dashboardPage( 
  tabsetPanel(
    tabPanel('ROI Tool',
             # headerPanel("PhenoCam ROI Tool"),
             br(),
             sidebarPanel(width = 4,
                          conditionalPanel(condition="output.showClearCache=='TRUE'", 
                                           fixedRow(
                                             column(9, htmlOutput('cacheDir')),
                                             column(1, actionButton('clearCache', label = NULL, icon = icon('trash'), 
                                                                    style="border-color: #303030; align:center; background-color:#303030; font-weight: bold;font-size: 100%;"))
                                             
                                           )),
                          
                          span(textOutput('showClearCache'), style="border-color: #303030; align:center; background-color:#303030; font-size: 0%;"),
                          
                          
                          # fluidRow(
                          #   column(1, actionButton('previousSite', label = NULL, icon = icon('arrow-circle-left'), width = '100%',  style= "border-color: #f5f5f5; align:center; background-color:#f5f5f5; color:#337ab7; font-size: 100%;font-weight: bold;")),
                          #   column(7, p()),
                          #   column(1, actionButton('nextSite', label = NULL, icon = icon('arrow-circle-right'), width = '100%',  style="border-color: #f5f5f5; align:center; background-color:#f5f5f5; color:#337ab7; font-size: 100%;font-weight: bold;"))
                          # ),
                          
                          selectInput("siteType", NULL, choices = c('All', 'Type I', 'Type II', 'Type III', 'NEON', 'SPRUCE'), width = '100'),
                          fluidRow(
                            column(1, actionButton('previousSite', label = NULL, icon = icon('arrow-circle-left'), width = '100%',  style= "border-color: #303030; align:center; background-color:#303030; color:#337ab7; font-size: 100%;font-weight: bold;")),
                            
                            column(7, 
                                   selectInput("siteName", NULL, choices = c('Select a Site'=''))
                            ),
                            column(1, actionButton('nextSite', label = NULL, icon = icon('arrow-circle-right'), width = '100%',  style="border-color: #303030; align:center; background-color:#303030; color:#337ab7; font-size: 100%;font-weight: bold;")),
                            
                            column(2, strong(actionButton('siteInfo', label = NULL, icon = icon('info'), width = '100%', style="border-color: #303030; align:center; background-color:#303030; color:#337ab7; font-size: 100%;font-weight: bold;")),
                                   bsModal("modalSiteInfo", "Site Info", "siteInfo", 
                                           size = "medium",
                                           footer = NULL, 
                                           tableOutput("tblSiteInfo")
                                   )
                            )
                          ),
                          
                          br(),
                          fluidRow(
                            column(10, selectInput("roiName", "ROI", 'New ROI')),
                            column(2, actionButton('refreshROI', label = NULL, icon = icon('sync'), width = '100%', style="border-color: #303030; align:center; background-color:#303030; color:#337ab7; font-size: 100%;font-weight: bold;")),
                            
                            br(),
                            br(),
                            column(2, strong(textOutput('noROI')))
                          ),
                          selectInput("vegType", "Vegetation Type", choices = list('Agriculture (AG)'='AG')),
                          selectInput('nextROIID', 'ROI ID', choices = c(1)),
                          textInput('roiDescription','Description', placeholder = 'Enter a description for the ROI'),
                          textInput('roiOwner','Owner', placeholder = 'Enter your name'),
                          hr(),
                          strong(textOutput('roiFileName')),
                          br(),
                          conditionalPanel('input.siteName!=""', {
                            selectInput("maskName", label = 'Mask', choices = 'New mask')}),
                          # textOutput('maskfilename'),
                          conditionalPanel('input.siteName!=""', {
                            strong('Sample Image:')}),
                          textOutput('sampleImagePath'),
                          br(),
                          conditionalPanel('input.siteName!=""', { fluidRow(
                            column(6, actionButton( 'matchStart', 'Match start', width = '100%', style='background-color:#666; color:#fff;font-weight: bold;')),
                            column(6, actionButton( 'matchEnd', 'Match end', width = '100%', style='background-color:#666; color:#fff;font-weight: bold;'))
                          )
                          }),
                          br(),
                          # TODO this section fails due to date selection.
                          conditionalPanel('input.siteName!=""', {fluidRow(
                            column(1, strong('from', style='font-size:70%;font-weight: bold;')),

                            tryCatch({
                              column(5, dateInput('maskStartDate', label = NULL, value =  '2001-01-01', startview = 'day'))
                              # column(5, airDatepickerInput('maskStartDate', label = NULL, value = as.Date('2001-01-01', format = "%Y-%m-%d"), view = c("days", "months", "years")))
                            }, error = function(err){
                              print(paste0('Error in StartDate'))
                            }, warning = function(wrn){
                              print(paste0('Warning in StartDate'))
                            }),
                            
                            column(4, textInput('maskStartTime', label = NULL, value = '00:08:00')),
                            column(1, '')
                          )}),
                          conditionalPanel('input.siteName!=""', {fluidRow(
                            column(1, strong('to', style='font-size:70%')),
                            # print(paste0('input.siteName: ')),
                            # column(5, dateInput('maskEndDate', label = NULL, value =  '9999-12-31', startview = 'day')),
                            
                            tryCatch({
                              column(5, dateInput('maskEndDate', label = NULL, value =  '9999-12-31', startview = 'day'))
                              # column(5, airDatepickerInput('maskEndDate', label = NULL, value = as.Date('2024-12-31', format = "%Y-%m-%d"), view = c("days", "months", "years")))
                            }, error = function(err){
                              print(paste0('Error in EndDate'))
                            }, warning = function(wrn){
                              print(paste0('Warning in EndDate'))
                            }),
                            
                            column(4, textInput('maskEndTime', label = NULL, value = '00:00:00')),
                            column(1, checkboxInput('openEnd', label = '', value = F))
                          )}),
                          
                          
                          br(),
                          conditionalPanel('input.siteName!=""', {fluidRow(
                            # column(6, actionButton("emailROI", "Submit for review", icon = icon('send'), width = "100%")),
                            column(6, p()),
                            column(6, downloadButton("downloadROI", "Download ROI files"))
                          )})
                          # br(),
                          # br(),
                          # passwordInput("password", label = NULL, placeholder = 'Password to save in the database'),
                          # actionButton("saveROI", "Save ROI files in the database", icon = icon('list-alt'), width = "100%")
                          
             ),
             
             
             
             
             conditionalPanel('input.siteName!=""', {
               
               mainPanel(
                 # br(),
                 fluidRow(
                   column(2,
                          strong(dateInput('gotoDate', label = 'Goto Date'), style='font-size:100%;font-weight: bold;')
                          # fluidRow(
                          #   column(9, strong(dateInput('gotoDate', label = ''), style='font-size:20%;font-weight: bold;')),
                          #   column(3, actionButton('gotoDateButton', label = NULL, icon = icon('sync'), width = '100%',
                          #                          style="background-color: #222222; border-color: #222222; align:center; font-size: 100%;font-weight: bold;"))
                          # )
                   ),
                   column(2, sliderInput('hazeThreshold', label = 'Haze threshold', min = 0, max = 1, value = .4, step = .01)),
                   
                   column(2, selectInput('shiftsList1', label = 'Horizon Shifts', choices = c(Choose=''), width = '100%')),
                   # column(1, actionButton('goShift1', label = NULL, icon = icon('sync'), width = '100%', 
                   #                        style="border-color: #fff; align:center; font-size: 200%;font-weight: bold;")),
                   # column(2, selectInput('shiftsList1.Threshold', label = 'Threshold (px)', choices = c(1:10, 15:30), selectize = T, selected = 10)),
                   column(2, sliderInput('shiftsList1.Threshold', label = 'Threshold (px)', min = 1, max = 100, value = 20, step = 1)),
                   
                   column(2, selectInput('shiftsList2', label = 'Correlation Shifts', choices = c(Choose=''), width = '100%')),
                   # column(1, actionButton('goShift2', label = NULL, icon = icon('sync'), width = '100%', 
                   #                        style="border-color: #fff; align:center; font-size: 200%;font-weight: bold;"))
                   # column(2, selectInput('shiftsList2.Threshold', label = 'Threshold', selectize = T, choices = c(.01, 0.02, .05, .1, .15, .2, .3, .4, .5, .6, .75), selected = .05))
                   column(2, sliderInput('shiftsList2.Threshold', label = 'Threshold (R)', min = 0, max = 1, value = .02, step = .01))
                 ),
                 
                 tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max {visibility: hidden !important;}'))),
                 conditionalPanel(
                   condition = "input.cliShow",
                   sliderInput(inputId = "clRange",
                               label =  NULL,
                               min = 1, max = 2,
                               ticks = F,
                               animate=F,
                               value = c(1, 2),
                               step = 1,
                               width = '100%'),
                   
                   fluidRow(
                     # column(3, plotOutput("clhistPlot", height = 'auto')),
                     column(12, plotOutput("clPlot", click = "clPoint", height = 'auto'))
                   ),
                   br()
                 ),
                 
                 fluidRow( 
                   # column(3, 
                   #        fluidRow( 
                   #          column(8, strong(dateInput('gotoDate', label = ''), style='font-size:20%;font-weight: bold;')),
                   #          column(4, actionButton('gotoDateButton', label = NULL, icon = icon('sync'), width = '100%', style="background-color: #222222; border-color: #222222; align:center; font-size: 200%;font-weight: bold;"))
                   #        )
                   # ),
                   column(12, sliderInput(inputId = "contID",
                                         label =  NULL,
                                         min = 1, max = 1,
                                         ticks = F,
                                         animate=F,
                                         value = 1,
                                         step = 1,
                                         width = '100%'))
                 ),
                 plotOutput("timePlot", height='auto'),
                 
                 fluidRow(
                   column(1, strong()),
                   column(1, actionButton("back", "", icon = icon('minus'), width = '100%', style="background-color: #222222; border-color: #222222")),
                   column(1, actionButton("backplay", "", icon = icon('backward'), width = '100%', style="background-color: #222222; border-color: #222222; align:center")),
                   column(1, actionButton("pause", "", icon = icon('stop'), width = '100%',  style="background-color: #222222; border-color: #222222")),
                   column(1, actionButton("play", "", icon = icon('forward'), width = '100%', style="background-color: #222222; border-color: #222222; align:center")),
                   column(1, actionButton("forw", "", icon = icon('plus'), width = '100%',  style="background-color: #222222; border-color: #222222")),
                   # column(1, strong()),
                   column(1, fluidRow(
                     column(6, checkboxInput("linkedImageShow", label = NULL, value = FALSE)),
                     column(6, actionButton("linkedImage", "", icon = icon('copy'), style='font-weight: bold; background-color: #222222;border-color: #222222'))
                   )),
                   column(4, fluidRow(
                     column(3, checkboxInput("lastNextDayShow", label = 'Pre/Nxt', value = F)),
                     column(3, checkboxInput("cliShow", label = 'CLI', value = F)),
                     column(3, checkboxInput("corShow", label = 'Cor', value = F)),
                     column(3, checkboxInput("hrzShow", label = 'Hrz', value = F))
                   ))
                   
                 ),
                 conditionalPanel(
                   condition = "input.linkedImageShow",
                   fluidRow(
                     column(1, strong()),
                     column(5, plotOutput("imagePlot", click = "newPoint", dblclick = 'gapPoint', height = 'auto')),
                     column(5, plotOutput("imagePlot2", height = 'auto')),
                     column(1, strong())
                   )
                 ),
                 conditionalPanel(
                   condition = "!input.linkedImageShow",
                   fluidRow(
                     column(1, strong()),
                     column(10, plotOutput("imagePlotBig", click = "newPoint", dblclick = 'gapPoint', height = 'auto')),
                     column(1, strong())
                   )
                 ),
                 
                 br(),
                 fluidRow(
                   column(1, strong()),
                   column(5,  fluidRow(
                     column(4, actionButton("clearCanvas", "Erase", icon = icon('eraser'), class="btn-primary", width = "100%", style='font-weight: bold;')),
                     column(4,  actionButton("undoCanvas", "Undo", icon = icon('undo'), class="btn-primary", width = "100%", style='font-weight: bold;')),
                     column(4, actionButton("acceptCanvas", "Accept", icon = icon('thumbs-up'), class="btn-primary", width = "100%", style='font-weight: bold;'))
                   )),
                   column(5,  fluidRow(
                     column(4, checkboxInput('showMask', label = 'Show Mask', value = F)),
                     column(4, checkboxInput('showGrid', label = 'Show Grid', value = F)),
                     column(4, colourpicker::colourInput(inputId = 'roiColors', allowTransparent=T, label = NULL, value = '#f1f27280',  showColour = 'background'))
                   )),
                   column(1, strong())
                 ),
                 
                 conditionalPanel(
                   condition = "input.lastNextDayShow",
                   br(),
                   fluidRow(
                     column(1, strong()),
                     column(5, plotOutput("previousDay", height = 'auto')),
                     column(5, plotOutput("nextDay", height = 'auto')),
                     column(1, strong())
                   )
                 ) ,
                 # fluidRow(
                 #   br(),
                 #   
                 #   column(1, strong()),
                 #   # column(5, plotOutput("maskPlot", height = 'auto')),
                 #   # column(1, strong()),
                 #   column(6, checkboxInput('showMask', label = 'Show Mask', value = F)),
                 #   column(1, 
                 #          br(),
                 #          colourpicker::colourInput(inputId = 'roiColors',  allowTransparent=T,  label = NULL, value = '#ab522200',  showColour = 'background')
                 #   ),
                 #   column(1, strong()),
                 #   column(2, 
                 #          br(),
                 #          actionButton("clearCanvas", "Erase", icon = icon('eraser'), class="btn-primary", width = "110px", style='font-weight: bold;'),
                 #          br(),br(),
                 #          actionButton("undoCanvas", "Undo", icon = icon('undo'), class="btn-primary", width = "110px", style='font-weight: bold;'),
                 #          br(),br(),
                 #          actionButton("acceptCanvas", "Accept", icon = icon('thumbs-up'), class="btn-primary", width = "110px", style='font-weight: bold;')
                 #   ),
                 #   column(2, strong())
                 # ),
                 br()
                 
                 # fluidRow(
                 #   column(1, strong()),
                 #   column(5, 
                 #          fluidRow(
                 #            column(4, actionButton("clearCanvas", "Erase", icon = icon('eraser'), class="btn-primary", width = "100%", style='font-weight: bold;')),
                 #            column(4, actionButton("undoCanvas", "Undo", icon = icon('undo'), class="btn-primary", width = "100%", style='font-weight: bold;')),
                 #            column(4, actionButton("acceptCanvas", "Accept", icon = icon('thumbs-up'), class="btn-primary", width = "100%", style='font-weight: bold;'))
                 #          )),
                 #   column(5,
                 #          fluidRow(
                 #            column( 5, shinyjs::colourInput(inputId = 'roiColors', allowTransparent=T, 
                 #                                                 transparentText = 'clear',
                 #                                                 label = NULL,value = '#ab5222', showColour = 'background'))
                 #            # column( 7, p())
                 #          )
                 #   ),
                 #   column(1, strong())
                 # ),
                 
                 
                 # hr(),
                 
                 
                 
               )
             })
    ),
    
    tabPanel('Time Series Extraction Tool',
             # conditionalPanel('input.siteName!=""', {
             fluidPage(
               br(),
               fluidRow(
                 column(2, 
                        radioButtons('ccRange', label = NULL, choices = c('Week', 'Month', 'Year', 'Entire data'), width = "330px",inline = F),
                        
                        selectInput('ccInterval', label = 'Interval', choices = c(1:7, 10, 15, 20, 30), selected = 1, width = '50px'),
                        
                        actionButton("startExtractCC", "Extract", icon = icon('chart-line'), onclick="Shiny.onInputChange('stopThis',false)", width = "110px", style="background-color:#666; color:#fff;font-weight: bold;"),
                        hr(),
                        checkboxGroupInput('ccBand', label = NULL, choices = c(Red='R', Green='G', Blue='B', Haze= 'H'), selected = c('R','G','B'), width = '100%', inline = F),
                        hr(),
                        downloadButton("downloadTSData", "Download\t")
                 ),
                 column(10, plotlyOutput(outputId = "timeSeriesPlotly", height = "500px", width = "100%"))
                 
               )
             )
             # }),
             # conditionalPanel('input.siteName==""', {strong('Select a site from the ROI Tool first!')})
    ),
    
    ## tabPanel('FOV Shifts Monitor', 
    ##          tags$iframe(src='http://134.114.109.3:3838/phenoShifts/', height=1000, width='100%')
    ## ),
    
    tabPanel('Report Errors', 
             fluidPage(
               # headerPanel('Report an error'),
               br(),
               sidebarPanel(    
                 textInput('errorUser', label = 'Your contact info', placeholder = 'Name', width = '100%'),
                 textInput('errorEmail', label = NULL, placeholder = 'Email', width = '100%'),
                 selectInput('errorSite', label = 'Site', choices = ''),
                 selectizeInput('errorOS', label = 'System info', choices = c('Windows','Linux','MacOS', 'Android', 'iOS')),
                 selectizeInput('errorBrowser', label = NULL, choices = c('Firefox', 'Google Chrome', 'Internet Explorer', 'Opera','Safari','Others')),
                 dateInput("errorDate", label = "Error date/time", value = Sys.Date()),
                 textInput("errorTime", label = NULL, value = strftime(Sys.time(), format = '%H:%M:%S'), placeholder = 'What time did the error occure?'),
                 selectInput('errorType', label = 'It crashed?', choices = c('Yes, it crashed.', 
                                                                             'No, it just returned an error message.',
                                                                             'No, but the output does not make sense.',
                                                                             'No, I have some suggestions.'), selected = ''),
                 actionButton("errorSend", "Submit", width = '100%', icon = icon('paper-plane'), class="btn-primary")
               ),
               
               mainPanel( 
                 textAreaInput('errorMessage', label = 'Explain the error please.', cols = 200, rows = 20) 
               )               
             )
             # ),
             # 
             # tabPanel('Simple Tutorial', 
             #          fluidPage(
             #            # HTML('<img src="phenoCamROI.guide.png"  alt="This is alternate text" , width="100%">')
             #            includeMarkdown('drawROI.Guide.md')
             #          )
    ),
    tabPanel('About',{
      includeHTML('about.html')
    })
    
  )
)



