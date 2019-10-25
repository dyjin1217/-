rm(list = ls()); gc(reset = T)
options("digits.secs")

setwd("~/power")
#setwd("D:\\2019\\RE2019-23-06\\power\\data")
#dir<-("D:\\2019\\RE2019-23-06\\power\\data")
dir<-("./data/")

#install.packages("dplyr")
#install.packages("data.table")
#install.packages("shiny")
#install.packages("plyr")
#install.packages("tidyr")
#install.packages("lubridate")
#install.packages("reshape2")
#install.packages("quantmod")
#install.packages("dygraphs")

library(dplyr)
library(data.table)
library(shiny)
library(plyr)
library(tidyr)
library(reshape2)
library(lubridate)
library(dygraphs)
library(plotly)
library(quantmod)

#data <- fread(list.files()[1], header = T) %>% data.frame(stringsAsFactors = F)
#head(power_dat,10)

file_list <- list.files(dir)

data <- data.frame()

for(file in file_list) {
  #temp<-read.csv(pasted(dir, file, sep="\\"), header=TRUE, sep=",", stringsAsFactors=FALSE)
  temp<-read.csv(paste0(dir, file), header=TRUE, stringsAsFactors=FALSE, fileEncoding = "CP949", encoding = "UTF-8")
  data <- rbind(data, temp)
}


power_dat_long <- gather(data, hour, power, starts_with('h'))

power_dat_long$date <- gsub("-", "", power_dat_long$date)
power_dat_long$date <- as.Date(power_dat_long$date,"%Y%m%d")
power_dat_long <- power_dat_long[order(as.Date(power_dat_long$date, format="%Y%m%d")),]

#power_dat <- cbind(hour1 = as.numeric(gsub("^h", "", power_dat_long$hour)), power_dat_long)

#power_dat$ymd = ifelse(power_dat$hour1 == 24, power_dat$date+days[1], power_dat$date)
#power_dat$new_time = ifelse(power_dat$hour1 == 24, "00", power_dat$hour1)
#power_dat$new_date = strptime(paste(power_dat$date, power_dat$new_time), "%Y-%m-%d %H" , tz = "GMT") %>% as.POSIXct()

power_dat <- cbind(hour1 = as.numeric(gsub("h", "", power_dat_long$hour)), power_dat_long)
power_dat$ymd = ifelse(power_dat$hour1 == 24, ymd(power_dat$date) + days(1), power_dat$date)
power_dat$new_time = ifelse(power_dat$hour1 == 24, "00", power_dat$hour1)
power_dat$ymd <- as.Date(power_dat$ymd, origin="1970-1-1")
power_dat$new_date = strptime(paste(power_dat$date, power_dat$hour1), format="%Y-%m-%d %H" , tz = "GMT") %>% as.POSIXct()

#un <- unique(power_dat$new_date)
#s <- as.numeric(format(power_dat$new_date, "%Y%m%d%H"))
#s <- s + 1
#ds <- as_datetime(s,"y%m%d%H")
#power_dat$new_date <- 

#s <- format(power_dat$new_date, "%Y-%m-%d %H:%M:%S")
#z <- ymd_hms(s)
#a <- as_datetime(z)

s <- as.numeric(format(power_dat$new_date, "%Y%m%d%H%M%S"))
#us <- unique(s)
#View(us)
#s <- s + 1
#dt <- as_datetime(s,format="%Y%m%d%H%M%S")
#dt <- strptime(s, format="%Y%m%d%H%M%S", tz = "GMT") %>% as.POSIXct()
dt <- strptime(s, format="%Y%m%d%H%M%S", tz = "GMT") %>% as.POSIXct()
power_dat$new_date <- dt
temp <- power_dat


result <- NULL
ui = fluidPage(
  titlePanel("Power"), 
  sidebarLayout(
    sidebarPanel(
      selectInput("var", "power_var", choices = colnames(power_dat)[7:9]),
      selectInput("period", "period", choices = c("year","month","day","hour"),selected = "hour"),
      actionButton("do", "View")),
      #sliderInput("date", "date",timezone = "GMT", min = min(power_dat$new_date), max = max(power_dat$new_date), value=max(power_dat$new_date))), 
    mainPanel(plotlyOutput("Plot1"))
  )
)

server = function(input, output){
  
  observeEvent(input$do, {
  output$Plot1 = renderPlotly({
    print(input$power)
    selectedData1 = reactive({
      result <- power_dat[, c("new_date",input$var,"power")]
      colnames(result)[2] <- "var"
      result
    })
    
    period = reactive({
      result <- input$period
      result
    })
    
    result <- selectedData1()
    period <- period()
    print(period)
    
    if(period == "day"){
      result$new_date <- as.Date(result$new_date)
      result <- aggregate(power ~ new_date + var ,data = result ,sum)
      
    } else if(period == "month"){
      print("month 실행0")
      temp <- format(result$new_date, format="%Y-%m")
      result$new_date <- as.Date(as.yearmon(temp))
      print("month 실행1")
      result <- aggregate(power ~ new_date + var ,data = result ,sum)
      print("month 실행2")
      
    }
      else if(period == "year"){
      temp <- format(result$new_date, format="%Y")
      result$new_date <- as.Date(as.yearmon(paste0(temp ,"-01")))
      result <- aggregate(power ~ new_date + var ,data = result ,sum)
    }
    #result <- aggregate(power ~ new_date ,data = result ,sum)
    #p <- plot_ly() %>% add_lines(data=result, x = ~new_date, y = ~power, mode='bar',color= ~var)
    p <- plot_ly(data=result, x = ~new_date, y = ~power, type='bar', color= ~var,colors = 'Reds') %>%  layout(yaxis = list(title = 'value'), barmode = 'stack')
    p$elementId <- NULL
    p
  })
  })
}

shinyApp(ui = ui, server = server)

