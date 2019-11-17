library(dplyr)

#Following two lines create a .Rda file with the most up to date version of the VSRR's vital signs
#Overdose death count statistics. The load() line imports this object into R. This is to circumnavigate
#the time consuming operation of getting the data from the CDC's API everytime. 

VSRR_OD_Counts<-read.csv(url("https://data.cdc.gov/api/views/xkb8-kh2a/rows.csv?accessType=DOWNLOAD"))
save(internet_data,file="C:/Users/Josh/Dropbox/R studio/ODvis/new_ODDF.Rda")

load("new_ODDF.Rda")

#For the St arugument, use the two letter abbreviation of the state. For the idcr argument, choose
#from:

"Cocaine (T40.5)"
"heroin (T40.1)"
"methadone (T40.3)"
"Opioids (T40.0-T40.4,T40.6)"
"Percent with drugs specified"
"Psychostimulants with abuse potential (T43.6)"
"Synthetic opioids, excl. methadone (T40.4)"
"synthetic opioid analgesics other than methadone (T40.4)"
"Natural & semi-synthetic opioids (T40.2)"
"Number of Drug Overdose Deaths"
"Number of Deaths"

#Data for all indicators is available for these areas: CT, DC, ME, NC, NH, NM, NV, NY, OK, OR, RI,
#SC, US, UT, VA, VT, WA.

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

ODCT<-state_indicator(VSRR_OD_Counts,"MO", "Number of Drug Overdose Deaths")
