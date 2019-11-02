#The purpose of this function is to take the output of the function state_indicator
#as an input and produce a visualization of the trend in overdoses over time in each 
#state.

library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)

#inputs:
#state_indicator_output, the output from the state_indicator function
#graph_type, the type of graph your looking for. default is a bar line graph. Either: "line" or "bar"



od.vis<-function(state_indicator_output, graph_type= "line"){
        Mnnth<-c("January","February","March","April","May","June","July","August","September","October","November","December")
        numb<-1:12
        matcher<-cbind(Mnnth,numb)
        num.mnth<-match(state_indicator_output$Month,matcher)
        state_indicator_output<-cbind(state_indicator_output, num.mnth)
        Date<-c()
        for(i in 1:nrow(state_indicator_output)){
                Date[i]<-paste(state_indicator_output[i,1],state_indicator_output[i,4],"01",sep="/")
        }
        deaths.date<-cbind(state_indicator_output,Date)
        deaths.date<-deaths.date[c(3,5)]
        deaths.date$Date<- as.Date(deaths.date$Date, "%Y/%m/%d")
        deaths.date$Deaths<-as.numeric(deaths.date$Deaths)
        # graph by month:
        ggplot(data = deaths.date, aes(Date, Deaths)) +
                stat_summary(fun.y = sum, # adds up all observations for the month
                             geom = graph_type) + # or "bar"
                scale_x_date(labels = date_format("%Y-%m"), breaks = "6 month") # custom x-axis label
}

od.vis(x)
od.vis(x,"bar")
