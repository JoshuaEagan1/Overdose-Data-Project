#use this to collect the data and graphs for a particular state

#Run entire File

state.name="Nevada"
state.abv="NV"
file.path=" "

#file.path should be a point to the directory where you would like the new graphs to be located.

setwd("file.path")
load("new_ODDF.Rda")

dir.create(paste(state.name, " Visualizations"))
setwd(paste("file.path", "/",state.name, " Visualizations", sep=""))
#change / to \ if running this on a MAC Operating System.


#Don't Edit Code Below this Line
#################################################################################################

load("new_ODDF.Rda")

library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)


state_indicator<-function(data,St,Idcr){
        ODDF<-data
        StIdcr<-ODDF %>% mutate(Year=factor(Year)) %>%
                mutate(Month=factor(Month))%>%
                filter(State==St,Indicator==Idcr)%>%
                select(Data.Value,Predicted.Value,Year,Month)
        Deaths<-c()
        SIPV<-StIdcr$Predicted.Value
        SIDV<-StIdcr$Data.Value
        for(i in 1:length(SIPV)){
                if(is.na(SIPV[i])){
                        val<-SIDV[i]
                }else{
                        val<-SIPV[i]
                }
                Deaths<-append(Deaths,val)
        }
        finished<-StIdcr%>%select(Year,Month)%>%
                cbind(Deaths)
        finished
}

od.vis<-function(state_indicator_output, graph_type= "line", desired_title="Number of Deaths by Month"){
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
                scale_x_date(labels = date_format("%Y-%m"), breaks = "6 month") + # custom x-axis label
                ggtitle(desired_title)
}

Cocaine<-state_indicator(VSRR_OD_Counts, state.abv, "Cocaine (T40.5)")
Heroin<-state_indicator(VSRR_OD_Counts, state.abv, "heroin (T40.1)")
Methadone<-state_indicator(VSRR_OD_Counts, state.abv, "methadone (T40.3)")
Opioids<-state_indicator(VSRR_OD_Counts, state.abv, "Opioids (T40.0-T40.4,T40.6)")
Psychostimulants<-state_indicator(VSRR_OD_Counts, state.abv, "Psychostimulants with abuse potential (T43.6)")
Synthetic_Opioids<-state_indicator(VSRR_OD_Counts, state.abv, "Synthetic opioids, excl. methadone (T40.4)")
Synthetic_Opioid_Analgesics<-state_indicator(VSRR_OD_Counts, state.abv, "synthetic opioid analgesics other than methadone (T40.4)")
Natural_and_Semisynthetic_Opioids<-state_indicator(VSRR_OD_Counts, state.abv, "Natural & semi-synthetic opioids (T40.2)")
Overdoses<-state_indicator(VSRR_OD_Counts, state.abv, "Number of Drug Overdose Deaths")
Deaths<-state_indicator(VSRR_OD_Counts, state.abv, "Number of Deaths")

###################################################################################################

#Plots

if (exists("Cocaine")){
        od.vis(Cocaine, "line", paste("Number of Fatal Cocaine Overdoses by Month in", state.name, sep=" "))
        ggsave(paste(state.abv,"Cocaine Deaths Line.jpeg"))
        od.vis(Cocaine,"bar", paste("Number of Fatal Cocaine Overdoses by Month in", state.name, sep=" "))
        ggsave(paste(state.abv,"Cocaine Deaths Bar.jpeg"))
}

if (exists("Heroin")){
od.vis(Herion, "line", paste("Number of Fatal Heroin Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Heroin Deaths Line.jpeg"))
od.vis(Heroin,"bar", paste("Number of Fatal Heroin Overdoses by Month in",state.name, sep=" "))
ggsave(paste(state.abv,"Heroin Deaths Bar.jpeg"))
}

if (exists("Methadone")){
od.vis(Methadone, "line", paste("Number of Fatal Methadone Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Methadone Deaths Line.jpeg"))
od.vis(Methadone,"bar", paste("Number of Fatal Methadone Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Methadone Deaths Bar.jpeg"))
}

if (exists("Opioids")){
od.vis(Opioids, "line", paste("Number of Fatal Opioid Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Opioid Deaths Line.jpeg"))
od.vis(Opioids,"bar", paste("Number of Fatal Opioid Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Opioid Deaths Bar.jpeg"))
}

if (exists("Psychostimulants")){
od.vis(Psychostimulants, "line", paste("Number of Fatal Psychostimulant Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Psychostimulant Deaths Line.jpeg"))
od.vis(Psychostimulants,"bar", paste("Number of Fatal Psychostimulant Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Psychostimulant Deaths Bar.jpeg"))
}

if (exists("Synthetic_Opioids")){
od.vis(Synthetic_Opioids, "line", paste("Number of Fatal Synthetic Opioid Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Synthetic Opioid Deaths Line.jpeg"))
od.vis(Synthetic_Opioids,"bar", paste("Number of Fatal Synthetic Opioid Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Synthetic Opioid Deaths Bar.jpeg"))
}

if (exists("Synthetic_Opioid_Analgesics")){
od.vis(Synthetic_Opioid_Analgesics, "line", paste("Number of Fatal Synthetic Opioid Analgesic Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Synthetic Opioid Analgesic Deaths Line.jpeg"))
od.vis(Synthetic_Opioid_Analgesics,"bar", paste("Number of Fatal Synthetic Opioid Analgesic Overdoses by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Synthetic Opioid Analgesic Deaths Bar.jpeg"))
}

if (exists("Natural_and_Semisynthetic_Opioids")){
od.vis(Natural_and_Semisynthetic_Opioids, "line", paste("Number of Natural and Semi-Synthetic Opioids Deaths by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Natural and Semi-Synthetic Opioid Deaths Line.jpeg"))
od.vis(Natural_and_Semisynthetic_Opioids,"bar", paste("Number of Natural and Semi-Synthetic Opioids Deaths by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Natural and Semi-Synthetic Opioid Deaths Bar.jpeg"))
}

if (exists("Overdoses")){
od.vis(Overdoses, "line", paste("Number of Overdose Deaths by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Total Overdose Deaths Line.jpeg"))
od.vis(Overdoses,"bar", paste("Number of Overdose Deaths by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Total Overdose Deaths Bar.jpeg"))
}

if (exists("Deaths")){
od.vis(Deaths, "line", paste("Total Number Deaths by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Total Deaths Line.jpeg"))
od.vis(Deaths,"bar", paste("Total Number Deaths by Month in", state.name, sep=" "))
ggsave(paste(state.abv,"Total Deaths Bar.jpeg"))
}
