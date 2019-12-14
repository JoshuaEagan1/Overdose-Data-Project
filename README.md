# Overdose-Data-Project

This project displays and visualizes US state level counts of overdose deaths of different categories of drugs.
All Code was written in R version 3.61. The data that corresponds to this code is the VSRR Provisional Drug 
Overdose Death Counts dataset published by the Center for Disease Control.

For technical notes about the database, visit 
[Data.gov's VSRR notes](https://catalog.data.gov/dataset/vsrr-provisional-drug-overdose-death-counts-54e35)



## state_indicator

`state_indicator()` takes a dataset, state, an indicator, selects *Data.Value* if *Predicted.Value* is not present 
or selects *Predicted.Value* if it is present, and returns a dataframe with *Year*, *Month*, and the selected
value. It removes the month of August in 2018, because this month is incomplete. Be sure to pass both
arguments in quotations, edit the file path in the `read.csv` argument, and to have the DPLYR package loaded.

For the **data** argument, use the following chunk of code to pull the most recent version of this dataset:

```
VSRR_OD_Counts<-read.csv(url("https://data.cdc.gov/api/views/xkb8-kh2a/rows.csv?accessType=DOWNLOAD"))
save(VSRR_OD_Counts,file="new_ODDF.Rda")
```

Be sure to have your working directoy set to the appropriate file folder to save this R object. Use the following line
to access the R object you saved:
```
load("new_ODDF.Rda")
```

The reason for this way of doing it instead of just loading the object from the API each time you wish to use the 
`state_indicator()` function is because it caches the time-consuming operation of loading the data from the CDC's API.


For the **St** arugument, use the two letter abbreviation of the state. For the **idcr** argument, choose from: 

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

Data for all indicators is available for
these areas: CT, DC, ME, NC, NH, NM, NV, NY, OK, OR, RI, SC, US, UT, VA, VT, WA. The value YC which represents
New York City also contains complete data. Many states began collecting data on all the statistics in 
the middle of 2018, but have incomplete data before that point.

```
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
```

### Example output
```
> state_indicator(VSRR_OD_Counts, "MO", "Number of Drug Overdose Deaths")
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
> state_indicator(VSRR_OD_Counts,"MO", "Cocaine (T40.5)")
   Year     Month Deaths
1  2015     April     NA
2  2015    August     NA
3  2015  December     NA
4  2015  February     NA
5  2015   January     NA
6  2015      July     NA
7  2015      June     NA
8  2015     March     NA
9  2015       May     NA
10 2015  November     NA
11 2015   October     NA
12 2015 September     NA
13 2016     April     NA
14 2016    August     NA
15 2016  December     NA
16 2016  February     NA
17 2016   January     NA
18 2016      July     NA
19 2016      June     NA
20 2016     March     NA
21 2016       May     NA
22 2016  November     NA
23 2016   October     NA
24 2016 September     NA
25 2017     April     NA
26 2017    August     NA
27 2017  December     NA
28 2017  February     NA
29 2017   January     NA
30 2017      July     NA
31 2017      June     NA
32 2017     March     NA
33 2017       May     NA
34 2017  November    149
35 2017   October     NA
36 2017 September     NA
37 2018     April     NA
38 2018    August     NA
39 2018  December    140
40 2018  February    134
41 2018   January     NA
42 2018      July     NA
43 2018      June     NA
44 2018     March    135
45 2018       May     NA
46 2018  November    138
47 2018   October    138
48 2018 September     NA
49 2019     April    150
50 2019  February    152
51 2019   January    144
52 2019     March    146
```

```
> state_indicator(VSRR_OD_Counts,"CT","Natural & semi-synthetic opioids (T40.2)")
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


## od.vis
`od.vis()` is a function that visualizes the overdose statistics produced by the `state_indicator` function. This function produces
either a bar or a line graph. 

### Inputs
The `state_indicator_output` input takes the output from the state_indicator function. 
The `graph_type` input takes either `"bar"` or `"line"` with a default of `"line"`. This determines which type of graph is displayed: bar or line.

### Packages
Be sure to have the following packages loaded:

```
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
```

```
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
```

### Output
![alt text](http://url/to/img.png)
![Number of Overdose Deaths in Missouri](https://github.com/JoshuaEagan1/Overdose-Data-Project/blob/master/FL%20Total%20Overdose%20Deaths%20Line.jpeg/to/img.png)

![Number of Cocaine Overdose Deaths in Nevada](https://github.com/JoshuaEagan1/Overdose-Data-Project/blob/master/MO%20Total%20Overdose%20Deaths%20Bar.jpeg/to/img.jpeg)

![Total Number of Deaths in Florida (line graph)](http://url/to/img.png)

https://github.com/JoshuaEagan1/Overdose-Data-Project/blob/master/FL%20Total%20Overdose%20Deaths%20Line.jpeg

https://github.com/JoshuaEagan1/Overdose-Data-Project/blob/master/MO%20Total%20Overdose%20Deaths%20Bar.jpeg

https://github.com/JoshuaEagan1/Overdose-Data-Project/blob/master/NV%20Cocaine%20Deaths%20Bar.jpeg


### Credit
I borrowed a section of code from the following article on R-bloggers.com to construct the od.vis function [Visualizing Weekly or Monthly Totals](https://www.r-bloggers.com/plot-weekly-or-monthly-totals-in-r/) (see the "graph by month" section.)

## State Graph Compiler

This R file provides a quick way to fetch the graphs and statistics for a particular state. The State name,
State Abreviation, and a file path must be specified. The File creates a new file folder containing the 
relevant graphs. Since some states just started collecting information on more substances in 2018, some of 
the graphs returned will have incomplete data before 2018.

