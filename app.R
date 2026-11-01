# BDA400 A06 — Tejeswari Ramoju | Shiny portfolio dashboard
# Run: install.packages(c("shiny","ggplot2","quantmod","TTR"))
# Open app.R in RStudio and click Run App.
# Select symbol/dates and press Fetch stock data.
# Requires internet for Yahoo Finance.
# Install: install.packages(c("shiny","ggplot2","quantmod","TTR"))
# STEP 1 — Data collection and package setup
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

# Convert downloaded OHLC time series to ggplot-compatible rows.
as_prices <- function(x) {
  data.frame(Date=as.Date(index(x)),Open=as.numeric(Op(x)),
             High=as.numeric(Hi(x)),Low=as.numeric(Lo(x)),
             Close=as.numeric(Cl(x)))
}
# Aggregate actual OHLC bars for selected period.
aggregate_prices <- function(x, period) {
  if(period=="Weekly") x <- to.weekly(x,indexAt="endof",name=NULL)
  if(period=="Monthly") x <- to.monthly(x,indexAt="endof",name=NULL)
  as_prices(x)
}
# Compute technical indicators and crossing-event signals.
calculate_indicators <- function(d,short,long) {
  d$ShortMA <- as.numeric(SMA(d$Close,n=short))
  d$LongMA <- as.numeric(SMA(d$Close,n=long))
  d$RSI <- as.numeric(RSI(d$Close,n=14))
  m <- MACD(d$Close,nFast=12,nSlow=26,nSig=9,maType="EMA")
  d$MACD <- as.numeric(m[,1]);d$MACDSignal <- as.numeric(m[,2])
  diff <- d$ShortMA-d$LongMA
  previous <- c(NA,head(diff,-1))
  d$Signal <- ifelse(!is.na(diff)&!is.na(previous)&previous<=0&diff>0,"Buy",
               ifelse(!is.na(diff)&!is.na(previous)&previous>=0&diff<0,"Sell","Hold"))
  d
}
# STEP 2 — Dashboard UI, inputs, and visualization controls
ui <- fluidPage(
 titlePanel("Portfolio Technical Analysis — R Shiny"),
 sidebarLayout(
  sidebarPanel(
   textInput("ticker","Yahoo Finance ticker","AAPL"),
   dateRangeInput("dates","Date range",start=Sys.Date()-365*2,end=Sys.Date()),
   selectInput("period","Time frame",c("Daily","Weekly","Monthly")),
   selectInput("type","Chart type",c("Line","Area","Candlestick")),
   checkboxGroupInput("indicators","Technical indicators",
     choices=c("Moving Averages","RSI","MACD"),
     selected=c("Moving Averages","RSI","MACD")),
   numericInput("short","Short MA periods",20,min=2,max=100),
   numericInput("long","Long MA periods",50,min=3,max=250),
   checkboxInput("annotations","Show crossover annotations",TRUE),
   actionButton("refresh","Fetch stock data"),
   helpText("Yahoo Finance requires internet access. Educational signals only.")
  ),
  mainPanel(
   textOutput("status"),plotOutput("price",height="360px"),
   conditionalPanel("input.indicators.includes('RSI')",plotOutput("rsi",height="180px")),
   conditionalPanel("input.indicators.includes('MACD')",plotOutput("macd",height="180px")),
   h4("Recent observations"),tableOutput("recent")
  )
 )
)
# STEP 3 — Reactive data, indicators, and visualizations
# STEP 4 — Trading rules and annotated crossover signals
server <- function(input,output,session) {
  # Data download is triggered by Refresh; cache data while toggling charts.
  stock <- eventReactive(input$refresh,{
    tryCatch(getSymbols(toupper(trimws(input$ticker)),src="yahoo",
                        from=as.Date(input$dates[1])-365,
                        to=as.Date(input$dates[2])+1,auto.assign=FALSE),
             error=function(e) structure(list(message=conditionMessage(e)),class="download_error"))
  },ignoreNULL=FALSE)
  prices <- reactive({
    x <- stock()
    validate(need(!inherits(x,"download_error"),
                  if(inherits(x,"download_error")) x$message else TRUE))
    validate(need(input$short<input$long,"Short MA must be smaller than long MA."))
    d <- aggregate_prices(x,input$period)
    validate(need(nrow(d)>input$long+5,"Not enough historical observations."))
    d <- calculate_indicators(d,input$short,input$long)
    d <- d[d$Date>=as.Date(input$dates[1]) & d$Date<=as.Date(input$dates[2]),]
    validate(need(nrow(d)>0,"No data in selected date range."))
    d
  })
  output$status <- renderText({
    d <- prices()
    paste(toupper(input$ticker),input$period,"—",nrow(d),"observations")
  })
  output$price <- renderPlot({
    d <- prices()
    p <- ggplot(d,aes(x=Date))
    if(input$type=="Line") p <- p+geom_line(aes(y=Close),color="#286aa7")
    if(input$type=="Area") p <- p+geom_area(aes(y=Close),fill="#8dbbdc",alpha=.65)+
       geom_line(aes(y=Close),color="#286aa7")
    if(input$type=="Candlestick") {
      d$Rising <- d$Close>=d$Open
      p <- p+geom_linerange(aes(ymin=Low,ymax=High,color=Rising))+
        geom_rect(data=d,aes(xmin=Date-.3,xmax=Date+.3,
                            ymin=pmin(Open,Close),ymax=pmax(Open,Close),fill=Rising))+
        scale_color_manual(values=c("TRUE"="#15805d","FALSE"="#c45436"))+
        scale_fill_manual(values=c("TRUE"="#15805d","FALSE"="#c45436"))+
        guides(color="none",fill="none")
    }
    if("Moving Averages"%in%input$indicators)
      p <- p+geom_line(aes(y=ShortMA,color="Short MA"),na.rm=TRUE)+
        geom_line(aes(y=LongMA,color="Long MA"),na.rm=TRUE)+
        scale_color_manual(values=c("Short MA"="#bb6a19","Long MA"="#7a4ab0"))
    if(isTRUE(input$annotations)) {
      events <- d[d$Signal!="Hold",]
      if(nrow(events)>0)
        p <- p+geom_point(data=events,aes(y=Close,shape=Signal),size=3)+
          geom_text(data=events,aes(y=Close,label=Signal),vjust=-.8,
                    check_overlap=TRUE,size=3)
    }
    p+labs(title=paste(toupper(input$ticker),input$period,"prices"),
           y="Price (quote currency)",x=NULL)+theme_minimal()
  })
  output$rsi <- renderPlot({
    d <- prices()
    ggplot(d,aes(Date,RSI))+geom_line(color="#7851a9",na.rm=TRUE)+
      geom_hline(yintercept=c(30,70),linetype="dashed")+
      coord_cartesian(ylim=c(0,100))+labs(title="RSI (14)",x=NULL,y="RSI")+theme_minimal()
  })
  output$macd <- renderPlot({
    d <- prices()
    ggplot(d,aes(Date))+geom_col(aes(y=MACD-MACDSignal),fill="#9bbbd4",na.rm=TRUE)+
      geom_line(aes(y=MACD,color="MACD"),na.rm=TRUE)+
      geom_line(aes(y=MACDSignal,color="Signal"),na.rm=TRUE)+
      labs(title="MACD (12,26,9)",x=NULL,y="MACD")+theme_minimal()
  })
  output$recent <- renderTable({
    d <- tail(prices()[,c("Date","Close","ShortMA","LongMA","Signal")],10)
    d[,2:4] <- lapply(d[,2:4,drop=FALSE],round,2)
    d
  })
}
shinyApp(ui,server)
