# Overdose-Data-Project
---

This project will display and visualize state level data about overdoses of different types of drugs. The 
data these functions are based on was originally harvested from Healthdata.gov, but I retreived it from 
data.gov at the link below. for technical notes about the database, or to download an updated version, visit
https://catalog.data.gov/dataset/vsrr-provisional-drug-overdose-death-counts-54e35


## state_indicator

state_indicator() takes a state and an indicator, selects *Data.Value* if *Predicted.Value* is not Present 
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
10. "Number of Drug Overdose Deaths"
11. "Number of Deaths."

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

### Example output
```
> state_indicator("MO","Number of Drug Overdose Deaths")
   Year     Month ind_vect
1  2015   January     1113
2  2015  February     1112
3  2015     March     1097
4  2015     April     1077
5  2015       May     1099
6  2015      June     1123
7  2015      July     1110
8  2015    August     1076
9  2015 September     1087
10 2015   October     1077
11 2015  November     1097
12 2015  December     1094
13 2016   January     1102
14 2016  February     1128
15 2016     March     1136
16 2016     April     1157
17 2016       May     1153
18 2016      June     1163
19 2016      July     1198
20 2016    August     1243
21 2016 September     1254
22 2016   October     1295
23 2016  November     1335
24 2016  December     1392
25 2017   January     1401
26 2017  February     1396
27 2017     March     1422
28 2017     April     1410
29 2017       May     1410
30 2017      June     1408
31 2017      July     1400
32 2017    August     1397
33 2017 September     1427
34 2017   October     1434
35 2017  November     1415
36 2017  December     1406
37 2018   January     1395
38 2018  February     1410
39 2018     March     1413
40 2018     April     1447
41 2018       May     1462
42 2018      June     1505
43 2018      July     1537
```

```
> state_indicator("MO","Cocaine (T40.5)")

 Error in data.frame(..., check.names = FALSE) : 
  arguments imply differing number of rows: 0, 87 
```

```
> state_indicator("CT","Natural & semi-synthetic opioids (T40.2)")
   Year     Month ind_vect
1  2015   January      172
2  2015  February      170
3  2015     March      176
4  2015     April      165
5  2015       May      168
6  2015      June      176
7  2015      July      185
8  2015    August      179
9  2015 September      183
10 2015   October      186
11 2015  November      190
12 2015  December      184
13 2016   January      182
14 2016  February      186
15 2016     March      179
16 2016     April      190
17 2016       May      188
18 2016      June      190
19 2016      July      185
20 2016    August      194
21 2016 September      194
22 2016   October      197
23 2016  November      203
24 2016  December      216
25 2017   January      211
26 2017  February      207
27 2017     March      216
28 2017     April      211
29 2017       May      226
30 2017      June      219
31 2017      July      222
32 2017    August      216
33 2017 September      213
34 2017   October      209
35 2017  November      193
36 2017  December      188
37 2018   January      181
38 2018  February      187
39 2018     March      179
40 2018     April      180
41 2018       May      167
42 2018      June      168
43 2018      July      169
```
