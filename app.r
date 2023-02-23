library(shiny)
library(shinydashboard)
library(ggplot2)
library(quantmod)
library(scales)
library(tidyr)
library(tibble)
library(bslib)
library(plotly)
library(corrplot)
library(RColorBrewer)
library(scales)
library(caret)
library(randomForest)
library(ggtext)
library(rpart)
library(caTools)
library(ggdendro)
library(reshape2)
library(pROC)
library(party)
library(reprtree)
library(prophet)
library(dygraphs)
library(forecast)
library(tseries)
library(lubridate)
library(tidyverse)


ui <- 
  fillPage(
    
    tags$style(HTML("


.box.box-solid.box-primary>.box-header {
  color:#fff;
  background:#A6E6AA
                    }

.box.box-solid.box-primary{
border-bottom-color:#A6E6AA;
border-left-color:#A6E6AA;
border-right-color:#A6E6AA;
border-top-color:#A6E6AA;
}

.box.box-solid.box-danger>.box-header {
  color:#fff;
  background:#00CBCB
                    }

.box.box-solid.box-danger{
border-bottom-color:#00CBCB;
border-left-color:#00CBCB;
border-right-color:#00CBCB;
border-top-color:#00CBCB;
}")),
    
    theme = bs_theme(
      version = 4, bootswatch = 'flatly'),
    
    navbarPage(
      
      
      
      title = ' ',
      
      
      tabPanel('Home',
               mainPanel(
                 fluidRow(style = 'margin-bottom:2rem;',
                   column(width = 8,                   tags$div(style = "margin-left:5rem; margin-top:10rem;",
                                                                h1('Forecasting | Stock Price Movement Analysis'),h4('Historical Data and Moving Average',
                                                                                                                     br(),br(),br(),br(),br(),br(),br(),
                                                                                                                     tags$img(style = 'margin-left:3rem; margin-bottom:2rem;',src="https://logos-download.com/wp-content/uploads/2020/06/RStudio_Logo.png",width="333.3px", height = "116.67px")
                                                                                                                     
                                                                                                                     )
                                                                
                   )),
                   column(width = 4,                   tags$div(style = 'margin-right:5rem;margin-top:1rem;',
                                                                tags$img(style = 'margin-right:10px;',src = 'https://img.freepik.com/free-vector/gradient-stock-market-concept_23-2149154185.jpg?w=740&t=st=1675604785~exp=1675605385~hmac=3828edfd56f9f7ec4f561095ec647021b314f83c3d83ae66e622ff6b26009466',width="650px", height = "650px")
                                                                
                                                                
                   )
                          
                          )

                   
                 )

               )#mainPanel

               ),#home
      
      tabPanel('Dataset',style = "margin-top:-15px;",
               tabBox(id='t1',width = 12,
                      tabsetPanel(
                        tabPanel('Collect Data',
                                 
                                 wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                           sidebarLayout(
                                             sidebarPanel(width = 3,
                                                          
                                                          wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                                                    
                                                                    h5('Setting Stock Price'),
                                                                    
                                                                    textInput('ticker','Select Ticker :',value = 'AAPL'),
                                                                    
                                                                    dateRangeInput(inputId = 'date', 'Select Range Date :',
                                                                                   start = Sys.Date()-910,
                                                                                   end = Sys.Date()
                                                                                   
                                                                                   
                                                                                   
                                                                    ),
                                                                    
                                                                    h5('Input Moving Average Day'),
                                                                    
                                                                    fluidRow(
                                                                      column(width = 6,numericInput('sma','SMA :',value = 10)),
                                                                      column(width = 6,numericInput('ema','EMA :',value = 10))
                                                                    ),
                                                                    
                                                                    fluidRow(
                                                                      column(width = 6,numericInput('wma','WMA :',value = 10)),
                                                                      column(width = 6,numericInput('evwma','EVWMA :',value = 10))
                                                                    ),
                                                                    
                                                                    fluidRow(
                                                                      column(width = 6,numericInput('vwap','VWAP :',value = 10)),
                                                                      column(width = 6,numericInput('rsi','RSI :',value = 14))
                                                                    ),
                                                                    
                                                                    fluidRow(
                                                                      column(width = 6,numericInput('macd1','MACD nFast :',value = 12)),
                                                                      column(width = 6,numericInput('macd2','MACD nSlow :',value = 26))
                                                                    ),
                                                                    
                                                                    fluidRow(
                                                                      column(width = 6,numericInput('signal','Signal :',value = 9)),
                                                                    ),
                                                                    
                                                                    actionButton('button','Enter',class = 'btn-block')
                                                                    
                                                                    
                                                                    
                                                                    
                                                          )#sidepanel1
                                                          
                                             ),

                                             mainPanel(
                                               DT::dataTableOutput("data"))#mainpanel1
                                             )#sidelayout1
                                           
                                 )

                        ),#tabpnel1

                        tabPanel('Descriptive Statistics',
                                 wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                           box(id="boxplot",width = 12,title = 'Box Plots',status = 'primary',color='aqua',solidHeader = T,
                                               fluidRow(
                                                 
                                                 column(width = 12,
                                                        plotlyOutput('box')
                                                 ),
                                               ),#fluidrow
                                               
                                               fluidRow(
                                                 column(width = 12,
                                                        plotlyOutput('box1')
                                                        
                                                 )
                                                 
                                                 
                                               ),
                                           ),
                                           
                                           box(id="boxplot",width = 12,title = 'Heat Maps',status = 'danger',color='aqua',solidHeader = T,
                                               fluidRow(
                                                 #column(width =6,plotOutput('cors',height = 350)),
                                                 column(width = 12,plotOutput('corrm'))
                                               )
                                               
                                           )

                                 )#welpanel
    
                        ),#paneldeskriptiv

                        tabPanel('Visualization',
                                 wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                           box(width=12,id="boxplot",title = 'Candle Plot',status = 'danger',color='aqua',solidHeader = T,
                                               radioButtons('ma','Moving Average :',choices = c('None','SMA','EMA','WMA','EVWMA','VWAP'),selected = 'None',inline = T),
                                               plotlyOutput('candlePlot',height = 750)
                                           ),
                                           
                                           
                                           box(width=12,id="boxplot",title = "RSI - Relative Strength Index",status = 'danger',color='aqua',solidHeader = T,
                                               plotlyOutput('rsi1')),
                                           
                                           box(width=12,id="boxplot",title = 'MACD - Moving Average Convergence Divergence',status = 'danger',color='aqua',solidHeader = T,
                                               plotlyOutput('macd'))
                                           
                                           
                                 )
                                 
                                 
                        )#panelviz
                        
                      )
                
                 
                 
               )#tabbox
               
               
               ),#panel1
      
      tabPanel('Modeling',style = "margin-top:-15px;",
               tabBox(id='t3',width = 12,
                      tabsetPanel(
                        
                        
                        tabPanel('Timeseries',
                                 tabsetPanel(
                                   tabPanel('Prophet',
                                            wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                                      fluidRow(
                                                        column(width = 12,
                                                               box(width=12,id="boxplot",title = "Visualization",status = 'danger',color='aqua',solidHeader = T,
                                                                   fluidRow(
                                                                     column(width = 1,
                                                                    
                                                                            fluidRow(
                                                                              column(width = 12,
                                                                                     h5('Make Future Data'),
                                                                                     
                                                                                     fluidRow(
                                                                                       column(width = 12,numericInput('tgl','Periods :',value = 30)),
                                                                                       
                                                                                     )
                                                                                     
                                                                              )
                                                                              ),
                                                                            
                                                                            fluidRow(
                                                                              column(width = 12,
                                                                                     h5('Split Dataset'),
                                                                                     
                                                                                     fluidRow(
                                                                                       column(width = 12, dateInput("testdate", "Test Data Date:", value = Sys.Date()-60))
                                                                                       
                                                                                     )
                                                                                     
                                                                              )
                                                                            ),
                                                                            
                                                                            ),
                                                                     column(width = 11,dygraphOutput('prop',height = 600))
                                                                     )
                                                                   ),
                                                               box(width=12,id="boxplot",title = "Summary",status = 'danger',color='aqua',solidHeader = T,
                                                                fluidRow(
                                                                  column(width = 12,p('Observing the components again :'),plotOutput('propsum',height = 600))
                                                                         ),
                                                                #fluidRow(
                                                                  #column(width = 12,p('Plot Cross-Val Actual vs Predict :'),plotlyOutput('crossprop',height = 600))
                                                                #)
                                                               ),
                                                               
                                                               box(width=12,id="boxplot",title = "Performance of Prophet",status = 'danger',color='aqua',solidHeader = T,
                                                                   fluidRow(
                                                                   column(width = 12,p('In sample performance :'),verbatimTextOutput('perform_in'))
                                                                   ),
                                                                   fluidRow(
                                                                     column(width = 12,plotlyOutput("perform_inplot"))
                                                                   ),
                                                                   fluidRow(
                                                                     column(width = 12,p('Out sample performance :'),verbatimTextOutput('perform_out'))
                                                                   ),
                                                                   fluidRow(
                                                                     column(width = 12,plotlyOutput("perform_outplot"))
                                                                   ),
                                                                   
                                                                   
                                                                   )
                                                               )
                                                        )
                                                      
                                                      )
                                     
                                     
                                   )#panel
                                   
                                   
                                 )#setpanel
                                 
                                 ),#timeseries
                        tabPanel('Regression',
                                 tabsetPanel(
                                   tabPanel('Linear Regression',
                                            wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                                      fluidRow(
                                                        column(width = 12,
                                                               box(width=12,id="boxplot",title = "Visualization",status = 'danger',color='aqua',solidHeader = T,
                                                                   fluidRow(
                                                                     column(width = 2,
                                                                            fluidRow(
                                                                              column(width = 12,
                                                                                     h5('Custom'),
                                                                                     
                                                                                     fluidRow(
                                                                                       column(width = 12,numericInput('train','Train Data :',value = .7)),
                                                                                       
                                                                                     ),
                                                                                     fluidRow(
                                                                                       
                                                                                       column(width = 12,numericInput('test','Test Data :',value = .3))
                                                                                       
                                                                                     ),
                                                                                     fluidRow(
                                                                                       
                                                                                       column(width = 12,radioButtons(inputId = 'fit',label = 'Metode Smooth :',choices = c('loess','lm'),selected = 'lm',inline = T))
                                                                                       
                                                                                       
                                                                                     ),
                                                                                     fluidRow(
                                                                                       
                                                                                       column(width = 12,selectInput(inputId = "varx1",label = 'Variabel X :', choices =  c('Open','High','Low','Close','Volume','SMA','EMA','WMA',"EVWMA", "VWAP", 'RSI','MACD','Signal'),selected = "Open"))
                                                                                       
                                                                                     )
                                                                              )),
                                                                     ),
                                                                     column(width = 10,plotlyOutput('lr')),
                                                                   )
                                                                   
                                                                   ),
                                                               box(width=12,id="boxplot",title = "Summary",status = 'danger',color='aqua',solidHeader = T,
                                                                   
                                                                   fluidRow(
                                                                     column(width = 12,column(width = 12,plotOutput("scatter1")))
                                                                     
                                                                   ),
                                                                   
                                                                   fluidRow(
                                                                     column(width = 6,column(width = 12,p('Summary Model :'),verbatimTextOutput("infolm1"))),
                                                                     column(width = 6,
                                                                            fluidRow(column(width = 12,p('Model Prediction :'),verbatimTextOutput("infolm"))),
                                                                            fluidRow(column(width = 12,p('Model Validation :'),verbatimTextOutput("crosslm")))
                                                                            )
                                                                     
                                                                   )
                                                                   )
                                                               
     
                                                        )#column

                                                      )#fluidrow
                                                      
                                                      
                                            )#wellpanel
                                            
                                            
                                            
                                   ),#linear regression

                                 )#tabsetpanel
                                 
                                 ),#regression
                        tabPanel('Classification',
                                 tabsetPanel(
                               
                                   tabPanel('Random Forest',
                                            wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                                                      fluidRow(
                                                        column(width = 12,
                                                               box(width=12,id="boxplot",title = "Visualization",status = 'danger',color='aqua',solidHeader = T,
                                                                   fluidRow(
                                                                     column(width = 1,
                                                                            fluidRow(
                                                                              column(width = 12,
                                                                                     h5('Custom'),
                                                                                     
                                                                                     fluidRow(
                                                                                       column(width = 12,numericInput('train','Train Data :',value = .7)),
                                                                                      
                                                                                     ),
                                                                                     fluidRow(
                                                                                       
                                                                                       column(width = 12,numericInput('test','Test Data :',value = .3))
                                                                                       
                                                                                     ),
                                                                                     fluidRow(
                                                                                      
                                                                                       column(width = 12,numericInput('tree','nTree :',value = 100))
                                                                                     ),
                                                                                     fluidRow(
                                                                                       
                                                                                       column(width = 12,numericInput('try','mTry :',value = 4))
                                                                                       
                                                                                     ),
                                                                                     fluidRow(
                                                                                       
                                                                                       column(width = 12,numericInput('do','do.Trace :',value = 100))
                                                                                       
                                                                                     ),
                                                                              )),
                                                                            ),
                                                                     column(width = 11,
                                                                            p('Note :'),p("0 = Down  |  1 = Up  |  This may take a while"),plotOutput('randomforest',height = 750))
                                                                   )),
                                                               box(width=12,id="boxplot",title = "Summary",status = 'danger',color='aqua',solidHeader = T,
                                                                   fluidRow(
                                                                     column(width = 6, plotOutput('randomforest1',height = 450)),
                                                                     column(width = 6,plotOutput('randomforest2',height = 450))
                                                                   ),
                                                                   fluidRow(
                                                                     column(width = 12,p('Summary Model :'),verbatimTextOutput('randomforest3'))
                                                                     
                                                                   ),
                                                                   fluidRow(
                                                                     column(width = 12,p('Model Validation :'),verbatimTextOutput('crossrf'))
                                                                   )
                                                                   
                                                                   
                                                               )
   
                                                        )
                                                      )
                                                      
                                                      )

                                            ),#randomforest
                                   
                                 )
                                 )
                        
                        
                      )
                      
                      
                      
               )#tabbox
               
               
      ),#tab panel modeling
      
 
      tabPanel(tags$a("Instagram",href="https://www.instagram.com/rfdharmaa/",style = "margin-left:60rem;font-size = 30px")),
      tabPanel(tags$a("LinkedIn",href="https://www.linkedin.com/in/firdausdharmaa/",style = "margin-left:1rem;font-size = 30px"))
    )#navbar
    


    
  )#fluidpage

server <- function(input,output){
  

  
  stock <- eventReactive(input$button, {
    req(input$ticker,input$date)
    
    getSymbols(input$ticker,
               from = input$date[1], to = input$date[2],
               auto.assign = F) %>%
    as.data.frame() %>%
    `colnames<-` (c('Open','High','Low','Close','Volume','Adjusted')) %>%
    rownames_to_column(var = 'Date') %>%
      mutate(SMA = SMA(Close,n = input$sma)) %>%
      mutate(EMA = EMA(Close,n = input$ema)) %>%
      mutate(WMA = WMA(Close,n = input$wma)) %>%
      mutate(EVWMA = EVWMA(Close,Volume,n = input$evwma)) %>%
      mutate(VWAP = VWAP(Close,Volume,n = input$vwap)) %>%
      mutate(RSI = RSI(Close,n = input$rsi)) %>%
      mutate(MACD = EMA(Close,n = input$macd2) - EMA(Close,input$window,n = input$macd1)) %>%
      mutate(Signal = EMA(MACD,n = input$signal)) %>%
      mutate(Direction = ifelse(Open-Close >0,
                             'Down',
                             'Up')) %>%
      mutate(Histo = MACD-Signal)
    
  })
  
  
  cp <- eventReactive(input$button, {
    req(input$ticker,input$date,input$dateBreaks)
    
    
    
  })
  
  
  output$data<- DT::renderDataTable({
    DT::datatable(stock()[1:16])
  })
  
  
  output$box <- renderPlotly({
    
    fig <- plot_ly(type = "box")
    
    fig1 <- fig %>% add_boxplot(y = stock()$Open, name = "Open", boxpoints = 'outliers',
                                marker = list(color = 'rgb(7,40,89)'),
                                line = list(color = 'rgb(7,40,89)'))
    
    fig2 <- fig %>% add_boxplot(y = stock()$High, name = "High", boxpoints = 'outliers',
                                marker = list(color = 'rgb(7,40,89)'),
                                line = list(color = 'rgb(7,40,89)'))
    
    fig3 <- fig %>% add_boxplot(y = stock()$Low, name = "Low", boxpoints = 'outliers',
                                marker = list(color = 'rgb(7,40,89)'),
                                line = list(color = 'rgb(7,40,89)'))
    
    fig4 <- fig %>% add_boxplot(y = stock()$Close, name = "Close", boxpoints = 'outliers',
                                marker = list(color = 'rgb(7,40,89)'),
                                line = list(color = 'rgb(7,40,89)'))
    
    fig5 <- fig %>% add_boxplot(y = stock()$Adjusted, name = "Adjusted", boxpoints = 'outliers',
                                marker = list(color = 'rgb(7,40,89)'),
                                line = list(color = 'rgb(7,40,89)'))
    
    fig6 <- fig %>% add_boxplot(y = stock()$Volume, name = "Volume", boxpoints = 'outliers',
                                marker = list(color = 'rgb(7,40,89)'),
                                line = list(color = 'rgb(7,40,89)'))
    
    fig <- subplot(fig1, fig2,fig3,fig4,fig5,fig6)
    
    
    
    
    
    
  })
  
  output$box1 <- renderPlotly({
    
    fig1 <- plot_ly(type = 'box',y = stock()$SMA, name = "SMA", boxpoints = 'outliers',
                    marker = list(color = 'rgb(7,40,89)'),
                    line = list(color = 'rgb(7,40,89)'))
    fig2 <- plot_ly(type = 'box',y = stock()$EMA, name = "EMA", boxpoints = 'outliers',
                    marker = list(color = 'rgb(7,40,89)'),
                    line = list(color = 'rgb(7,40,89)'))
    fig3 <- plot_ly(type = 'box',y = stock()$WMA, name = "WMA", boxpoints = 'outliers',
                    marker = list(color = 'rgb(7,40,89)'),
                    line = list(color = 'rgb(7,40,89)'))
    fig4 <- plot_ly(type = 'box',y = stock()$EVWMA, name = "EVWMA", boxpoints = 'outliers',
                    marker = list(color = 'rgb(7,40,89)'),
                    line = list(color = 'rgb(7,40,89)'))
    fig5 <- plot_ly(type = 'box',y = stock()$VWAP, name = "VWAP", boxpoints = 'outliers',
                    marker = list(color = 'rgb(7,40,89)'),
                    line = list(color = 'rgb(7,40,89)'))
    fig6 <- plot_ly(type = 'box',y = stock()$RSI, name = "RSI", boxpoints = 'outliers',
                    marker = list(color = 'rgb(7,40,89)'),
                    line = list(color = 'rgb(7,40,89)'))
    fig7 <- plot_ly(type = 'box',y = stock()$MACD, name = "MACD", boxpoints = 'outliers',
                    marker = list(color = 'rgb(7,40,89)'),
                    line = list(color = 'rgb(7,40,89)'))
    fig8 <- plot_ly(type = 'box',y = stock()$Signal, name = "Signal", boxpoints = 'outliers',
                    marker = list(color = 'rgb(7,40,89)'),
                    line = list(color = 'rgb(7,40,89)'))
    
    fig <- subplot(fig1, fig2,fig3,fig4,fig5,fig6,fig7,fig8)
    
    
  })
  
  
  
  
  
  
  
  
  
  #Correlation Matrix
  output$corrm <- renderPlot({
    
    M <- cor(na.omit(stock()[2:15]))
    col <- colorRampPalette(c("#BB4444", "#EE9988", 
                              "#FFFFFF", "#77AADD",
                              "#4477AA"))
    
    cor.mtest <- function(mat, ...) 
    {
      mat <- as.matrix(mat)
      n <- ncol(mat)
      p.mat<- matrix(NA, n, n)
      diag(p.mat) <- 0
      for (i in 1:(n - 1)) 
      {
        for (j in (i + 1):n)
        {
          tmp <- cor.test(mat[, i], mat[, j], ...)
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
      }
      colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
      p.mat
    }
    
    # matrix of the p-value of the correlation
    p.mat <- cor.mtest(na.omit(stock()[2:15]))
    
    corrplot(M, method = "color", col = col(200),  
             type = "upper", order = "hclust", 
             addCoef.col = "black", # Add coefficient of correlation
             tl.col="black", tl.srt = 45, # Text label color and rotation
             
             # Combine with significance
             p.mat = p.mat, sig.level = 0.05, insig = "blank",
             
             # hide correlation coefficient
             # on the principal diagonal
             diag = FALSE 
    )
    
    
  })
  
  
  
  output$candlePlot <- renderPlotly({
    
    if(input$ma == "None"){
      fig <- stock() %>% plot_ly(x = ~Date, type="candlestick",
                                 open = ~Open, close = ~Close,
                                 high = ~High, low = ~Low, name = input$ticker,
                                 increasing = list(line = list(color = 'Forest Green')), decreasing = list(line = list(color = 'Red')))
      fig <- fig %>% layout(yaxis = list(title = "Price"))
      
      # plot volume bar chart
      fig2 <- stock()
      fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = paste(input$ticker,"Volume"),
                               color = ~Direction, colors = c('#17BECF','#7F7F7F')) 
      fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))
      
      
      # subplot with shared x axis
      fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
                     shareX = TRUE, titleY = TRUE)
      
      fig <- fig %>% layout(title = paste(input$ticker, " : ",input$date[1],"to",input$date[2]),
                            xaxis = list(type = 'date',tickformat = "%d %B (%a)<br>%Y"),
                            legend = list(orientation = 'h', x = 0.5, y = 1,
                                          xanchor = 'center', yref = 'paper',
                                          font = list(size = 10),
                                          bgcolor = 'transparent'))
      fig <- fig %>%  layout(
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),plot_bgcolor='#e5ecf6')
    }
    
    
    else{
      fig <- stock() %>% plot_ly(x = ~Date, type="candlestick",
                                 open = ~Open, close = ~Close,
                                 high = ~High, low = ~Low, name = input$ticker,
                                 increasing = list(line = list(color = 'Forest Green')), decreasing = list(line = list(color = 'Red')))
      fig <- fig %>% add_lines(x = ~Date, y = ~get(input$ma), name = input$ma,
                               line = list(color = '#17BECF', width = 0.5),
                               hoverinfo = "none", inherit = F) 
      fig <- fig %>% layout(yaxis = list(title = "Price"))
      
      # plot volume bar chart
      fig2 <- stock()
      fig2 <- fig2 %>% plot_ly(x=~Date, y=~Volume, type='bar', name = paste(input$ticker,"Volume"),
                               color = ~Direction, colors = c('#17BECF','#7F7F7F')) 
      fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))
      
      rs <- list(visible = TRUE,
                 xanchor = 'top', yref = 'paper',
                 font = list(size = 9),
                 buttons = list(
                   list(count=1,
                        label='RESET',
                        step='all'),
                   list(count=1,
                        label='1 YR',
                        step='year',
                        stepmode='backward'),
                   list(count=3,
                        label='3 MO',
                        step='month',
                        stepmode='backward'),
                   list(count=1,
                        label='1 MO',
                        step='month',
                        stepmode='backward')
                 ))
      # subplot with shared x axis
      fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
                     shareX = TRUE, titleY = TRUE)
      
      fig <- fig %>% layout(title = paste(input$ticker, " : ",input$date[1],"to",input$date[2]),
                            xaxis = list(type = 'date',tickformat = "%d %B (%a)<br>%Y"),
                            legend = list(orientation = 'h', x = 0.5, y = 1,
                                          xanchor = 'center', yref = 'paper',
                                          font = list(size = 10),
                                          bgcolor = 'transparent'))
      fig <- fig %>%  layout(
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),plot_bgcolor='#e5ecf6')

  }})
  
  
  output$macd <- renderPlotly({
    fig1 <- plot_ly(stock(), x = ~Date ,y = ~Histo, type = 'bar', name = 'MACD Hist',color = I('#17BECF'))%>%
      add_lines(x = ~Date, y = ~MACD,line = list(color = 'blue', width = 0.75),inherit = F,name = 'MACD Lines')%>%
      add_lines(x = ~Date, y = ~Signal,line = list(color = 'red', width = 0.75),inherit = F,name = 'Signal Lines')
  
    fig1 <- fig1 %>%
      layout(
        xaxis = list(type = 'date',tickformat = "%d %B (%a)<br>%Y"
                     ,zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff',title = 'MACD Histogram'),
        plot_bgcolor='#e5ecf6')
  })
  
  output$rsi1 <- renderPlotly({
    fig <- plot_ly(stock(), type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~Date, y = ~RSI, name = 'RSI Lines',line = list(color = 'purple', width = 0.75))%>%
      layout(showlegend = F)
    options(warn = -1)
    
    fig <- fig %>%
      layout(
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6'
        )
    
    fig <- fig %>% layout(yaxis = list(title = "RSI"),xaxis = list(type = 'date',tickformat = "%d %B (%a)<br>%Y"))
    
    
    
  })
  
  
  
  
  output$cors <- renderPlot({
    M <- cor(na.omit(stock()[2:15]))
    corrplot(M, method="color",tl.col = "black",order = "hclust",tl.srt = 45)
  })
  
  
  
  
  
  #Modeling
  
  
  #linear Regression
  output$lr <- renderPlotly({
    
    sample <- sample(c(TRUE, FALSE), nrow(stock()), replace=TRUE, prob=c(as.numeric(input$train),as.numeric(input$test)))
    copydf <- stock()
    copydf[2:15] <- scale(copydf[2:15])
    train <- na.omit(copydf[sample, ])
    test <- na.omit(copydf[!sample, ])
    
    
    p <- train %>%
      ggplot(aes(x= get(input$varx1), y= Close)) +
      geom_point()+
      geom_smooth(method = get(input$fit)) +
      labs(title = paste('Hubungan Antara Variabel', input$varx1,'dan Close'),
           x = input$varx1,
           y = "Close")+
      theme(plot.title = element_textbox_simple(size = 10,halign = 0.5))
    
    ggplotly(p)
    
  })
  
  output$infolm <- renderPrint({
    sample <- sample(c(TRUE, FALSE), nrow(stock()), replace=TRUE, prob=c(as.numeric(input$train),as.numeric(input$test)))
    
    copydf <- stock()
    copydf[2:15] <- scale(copydf[2:15])
    train <- na.omit(copydf[sample, ])
    test <- na.omit(copydf[!sample, ])
    
    model <- lm(Close~Open + High + Low + Volume + SMA + WMA + EVWMA + VWAP + RSI +MACD+Signal,data=train)
    predictions <- predict(model, test)
    print(data.frame( R2 = R2(predictions, test$Close),
                      RMSE = RMSE(predictions, test$Close),
                      MAE = MAE(predictions, test$Close)))
    
  })
  output$infolm1 <- renderPrint({
    sample <- sample(c(TRUE, FALSE), nrow(stock()), replace=TRUE, prob=c(as.numeric(input$train),as.numeric(input$test)))
    
    copydf <- stock()
    copydf[2:15] <- scale(copydf[2:15])
    train <- na.omit(copydf[sample, ])
    test <- na.omit(copydf[!sample, ])
    
    model <- lm(Close~Open + High + Low + Volume + SMA + WMA + EVWMA + VWAP + RSI +MACD+Signal,data=train)
    summary(model)
    
  })
  
  output$crosslm <- renderPrint({

    copydf <- stock()
    copydf[2:15] <- scale(copydf[2:15])
    train_control <- trainControl(method = "LOOCV")
    
    model <- train(Close~Open + High + Low + Volume + SMA + WMA + EVWMA + VWAP + RSI +MACD+Signal,data=na.omit(copydf),method = 'lm',trControl = train_control)
    print(model)
  })
  
  
  output$scatter1 <- renderPlot({
    sample <- sample(c(TRUE, FALSE), nrow(stock()), replace=TRUE, prob=c(as.numeric(input$train),as.numeric(input$test)))
    copydf <- stock()
    copydf[2:15] <- scale(copydf[2:15])
    train <- na.omit(copydf[sample, ])
    test <- na.omit(copydf[!sample, ])
    
    plot(predict(lm(Close~Open + High + Low + Volume + SMA + WMA + EVWMA + VWAP+RSI +MACD+Signal,data=train), test) - test$Close, main = "Hubungan Model Antara Predict dengan Actual",ylab='Predict - Actual')
    
  })
  
  
  output$randomforest1 <- renderPlot({
    sample <- sample(c(TRUE, FALSE), nrow(stock()), replace=TRUE, prob=c(as.numeric(input$train),as.numeric(input$test)))
    
    copydf <- stock()
    copydf[2:15] <- scale(copydf[2:15])
    copydf$Direction <- ifelse(copydf$Direction == "Up", 1, 0)
    copydf$Direction <- as.factor(copydf$Direction)
    train <- na.omit(copydf[sample, ])
    test <- na.omit(copydf[!sample, ])
    
    model <- randomForest(Direction ~ SMA + +EMA + WMA + EVWMA + VWAP + RSI + MACD + Signal,
                          data = train,importance = TRUE,proximity = TRUE,type="classification",  ntree= as.numeric(input$tree),mtry = as.numeric(input$try), do.trace=as.numeric(input$do))
    
    plot(model,main = 'Model')
    
  })
  
  output$randomforest2 <- renderPlot({
    sample <- sample(c(TRUE, FALSE), nrow(stock()), replace=TRUE, prob=c(as.numeric(input$train),as.numeric(input$test)))
    
    copydf <- stock()
    copydf[2:15] <- scale(copydf[2:15])
    copydf$Direction <- ifelse(copydf$Direction == "Up", 1, 0)
    copydf$Direction <- as.factor(copydf$Direction)
    train <- na.omit(copydf[sample, ])
    test <- na.omit(copydf[!sample, ])
    
    model <- randomForest(Direction ~ SMA +EMA + WMA + EVWMA + VWAP + RSI + MACD + Signal,
                          data = train,type="classification",  ntree= as.numeric(input$tree),mtry = as.numeric(input$try), do.trace=as.numeric(input$do))
    varImpPlot(model,main = 'Variable Importance Plot')
    
  })
  
  
  
  output$randomforest <- renderPlot({
    sample <- sample(c(TRUE, FALSE), nrow(stock()), replace=TRUE, prob=c(as.numeric(input$train),as.numeric(input$test)))
    copydf <- stock()
    copydf[2:15] <- scale(copydf[2:15])
    copydf <- stock() %>% mutate_if(is.numeric, round, digits = 4)
    copydf$Direction <- ifelse(copydf$Direction == "Up", 1, 0)
    copydf$Direction <- as.factor(copydf$Direction)

    train <- na.omit(copydf[sample, ])
    test <- na.omit(copydf[!sample, ])
    
    model <- randomForest(Direction ~ SMA +EMA + WMA + EVWMA + VWAP + RSI + MACD + Signal, data = train,importance = TRUE,proximity = TRUE,type="classification",  ntree= as.numeric(input$tree),mtry = as.numeric(input$try), do.trace=as.numeric(input$do))
    
    reprtree:::plot.getTree(model,col = 'blue')
  })
  

  
  
  output$randomforest3 <- renderPrint({
    sample <- sample(c(TRUE, FALSE), nrow(stock()), replace=TRUE, prob=c(as.numeric(input$train),as.numeric(input$test)))
    
    copydf <- stock()
    copydf[2:15] <- scale(copydf[2:15])
    copydf$Direction <- ifelse(copydf$Direction == "Up", 1, 0)
    copydf$Direction <- as.factor(copydf$Direction)
    train1 <- na.omit(copydf[sample, ])
    test1 <- na.omit(copydf[!sample, ])
    
    model <- randomForest(Direction ~ SMA +EMA + WMA + EVWMA + VWAP + RSI + MACD + Signal, data = train1,importance = TRUE,proximity = TRUE,type="classification",  ntree= as.numeric(input$tree),mtry = as.numeric(input$try), do.trace=as.numeric(input$do))

    predictions <- predict(model, test1)
    
    a <- confusionMatrix(predictions, test1$Direction)
    print(a)
    
    print(model)
  })
  

  
  
  
  output$crossrf <- renderPrint({
    copydf <- stock()
    copydf[2:15] <- scale(copydf[2:15])
    train_control <- trainControl(method = "LOOCV")
    
    model <- train(Direction~Open + High + Low + Volume + SMA + WMA + EVWMA + VWAP + RSI +MACD+Signal,data=na.omit(copydf),method = 'rf',trControl = train_control)

    print(model)
  })
  
  
  output$prop <- renderDygraph({
    df <- subset(stock(),select = c("Date","Close"))
    colnames(df) <- c('ds','y')
    
    
    train <- df %>% 
      filter(ds <= ymd(input$testdate))
    
    model <- prophet(train)
    
    future <- make_future_dataframe(model,periods = as.numeric(input$tgl))
    
    forecast <- predict(model,future)
    
    dyplot.prophet(model,forecast)
    
  })

  output$propsum <- renderPlot({
    
    df <- subset(stock(),select = c("Date","Close"))
    colnames(df) <- c('ds','y')
    
    train <- df %>% 
      filter(ds <= ymd(input$testdate))
    
    model <- prophet(train)
    
    future <- make_future_dataframe(model,periods = as.numeric(input$tgl))
    
    forecast <- predict(model,future)
    
    prophet_plot_components(model,forecast)
    
  })

  output$crossprop1 <- renderPlotly ({
    df <- subset(stock(),select = c("Date","Close"))
    colnames(df) <- c('ds','y')
    
    train <- df %>% 
      filter(ds <= ymd(input$testdate))
    
    model <- prophet(train)
    
    df.cv <- cross_validation(model, initial = 731, period = 30, horizon = 365, units = 'days')
    
    x <- df.cv %>% 
      ggplot()+
      geom_point(aes(ds,y)) +
      geom_point(aes(ds,yhat,color=factor(cutoff)))+
      theme_bw()+
      xlab("Date")+
      ylab("daily_mean_TEMP_abs") +
      scale_color_discrete(name = 'Cutoff')
    
    ggplotly(x)
    
  })
  
  output$perform_in <- renderPrint({
    
    df <- subset(stock(),select = c("Date","Close"))
    colnames(df) <- c('ds','y')
    
    train_iden <- df %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds <= ymd(input$testdate))
    test <- df %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds > ymd(input$testdate))
    
    model <- prophet(train_iden)
    
    forecast_insample = predict(model,train_iden)
    forecast_metric_data = forecast_insample %>% 
      as_tibble() %>% 
      mutate(ds = as.Date(ds)) %>% 
      filter(ds<=ymd(input$testdate))
    
    

    RMSE = sqrt(mean((train_iden$y - forecast_metric_data$yhat)^2))
    MAE = mean(abs(train_iden$y - forecast_metric_data$yhat))
    MAPE = mean(abs((train_iden$y - forecast_metric_data$yhat)/train_iden$y))
    
    print(paste("RMSE:",round(RMSE,2)))
    print(paste("MAE:",round(MAE,2)))
    print(paste("MAPE:",round(MAPE,2)))
    
 
  })
  
  output$perform_inplot <- renderPlotly({
    df <- subset(stock(),select = c("Date","Close"))
    colnames(df) <- c('ds','y')
    
    train_iden <- df %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds <= ymd(input$testdate))
    test <- df %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds > ymd(input$testdate))
    
    model <- prophet(train_iden)
    
    forecast_insample = predict(model,train_iden)
    forecast_metric_data = forecast_insample %>% 
      as_tibble() %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds<=ymd(input$testdate))
    
    c <- ggplot()+
      geom_line(aes(train_iden$ds ,train_iden$y), color = 'green')+
      geom_line(aes(forecast_metric_data$ds,forecast_metric_data$yhat),color='red',
                alpha = 1)+
      theme_bw()+
      xlab("Date")+
      ylab("daily_mean_TEMP_abs") +
      ggtitle("In Sample Performance Model")+
      scale_colour_manual("Keterangan :",
                          values = c("Actual" = "green","Predict" = "red"))

    ggplotly(c)%>% layout(showlegend = T)
  })
  
  
  output$perform_out <- renderPrint({
    df <- subset(stock(),select = c("Date","Close"))
    colnames(df) <- c('ds','y')
    
    train <- df %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds <= ymd(input$testdate))
    test <- df %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds > ymd(input$testdate))
    
    model <- prophet(train)
    future <- make_future_dataframe(model,periods = as.numeric(input$tgl))
    forecast <- predict(model,future)
    
    forecast_metric_data = forecast %>% 
      as_tibble() %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds > ymd(input$testdate))
    
    
    RMSE = sqrt(mean((test$y - forecast_metric_data$yhat)^2))
    MAE = mean(abs(test$y - forecast_metric_data$yhat))
    MAPE = mean(abs((test$y - forecast_metric_data$yhat)/test$y))
  
    print(paste("RMSE:",round(RMSE,2)))
    print(paste("MAE:",round(MAE,2)))
    print(paste("MAPE:",round(MAPE,2)))
  })
  
  
  output$perform_outplot <- renderPlotly({
    df <- subset(stock(),select = c("Date","Close"))
    colnames(df) <- c('ds','y')
    
    train <- df %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds <= ymd(input$testdate))
    test <- df %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds > ymd(input$testdate))
    
    model <- prophet(train)
    future <- make_future_dataframe(model,periods = as.numeric(input$tgl))
    forecast <- predict(model,future)
    

    forecast_metric_data = forecast %>% 
      as_tibble() %>% 
      mutate(ds = as.POSIXct(ds),format = "%Y-%m-%d") %>% 
      filter(ds>ymd(input$testdate))
    
    d <- ggplot()+
      geom_line(aes(test$ds ,test$y), color = 'green')+
      geom_line(aes(forecast_metric_data$ds,forecast_metric_data$yhat),color='red',
                alpha = 1)+
      theme_bw()+
      xlab("Date")+
      ylab("daily_mean_TEMP_abs")+
      ggtitle("Out Sample performance Model") + 
      scale_colour_manual("Keterangan :",
                          values = c("Actual" = "green","Predict" = "red"))

    ggplotly(d)%>% layout(showlegend = T)
  })
  
  
  
  
}

shinyApp(ui,server)