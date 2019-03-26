# Overdose-Data-Project

This function takes a state and an indicator, selects *Data.Value* if *Predicted.Value* is not Present 
or selects *Predicted.Value* if it is present, and returns a dataframe with *Year*, *Month*, and the selected
value. It removes the month of August in 2018, because this month is incomplete. Be sure to pass both
arguments in quotations. For the **St** arugument, use the two letter abreviation of the state. For the
**idcr** argument, choose from: 

1. "Cocaine (T40.5)" 
2. "heroin (T40.1)"
3. "methadone (T40.3)"
4. "Opioids (T40.0-T40.4,T40.6)"
5. "Percent with drugs specified"
6. "Psychostimulants with abuse potential (T43.6)"
7. "Synthetic opioids, excl. methadone (T40.4)"
8. "synthetic opioid analgesics other than methadone (T40.4)"
9. "Natural & semi-synthetic opioids (T40.2)"
10. "Number of Drug Overdose Deaths", or "Number of Deaths."

Also, if the following error message 

```
"Error in data.frame(..., check.names = FALSE) :
arguments imply differing number of rows: 0, 87"
```
is returned, the the data for this indicator and state is not available

```
state_indicator<-function(St,Idcr){
        ODDF<-read.csv("C:/Users/Josh/Desktop/Overdoses.csv")
        StIdcr<-ODDF %>% mutate(Year=factor(Year)) %>%
                mutate(Month=factor(Month))%>%
                filter(State==St,Indicator==Idcr)%>%
                select(Data.Value,Predicted.Value,Year,Month)
        ind_vect<-c()
        SIPV<-StIdcr$Predicted.Value
        SIDV<-StIdcr$Data.Value
        for(i in 1:87){
                if(is.na(SIPV[i])){
                        val<-SIDV[i]
                }else{
                        val<-SIPV[i]
                }
                ind_vect<-append(ind_vect,val)
        }
        finished<-StIdcr%>%select(Year,Month)%>%
                cbind(ind_vect)%>%
                slice(1:43)
        finished
}
```
