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