# Report: Analysis of NOAA weather data for severity of population and economic impact

## Introduction
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This report involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.

The basic goal of this report is to explore the NOAA Storm Database and answer some basic questions about severe weather events as below:

* Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
* Across the United States, which types of events have the greatest economic consequences?

The analysis in this report summarizes the top 10 weather events causing the highest impact to population and economy factors such as property and crops. The analysis can provide indicators on how best to prioritize resources for relief.      

  
### 0. Install packages



```r
packages <- c('ggplot2','dplyr','zoo','reshape2')
check_packages <- function (...) {
  lapply(list(...), function (p) {
     if (! p %in% installed.packages()) {
        install.packages(p)
        require(p,character.only=TRUE)
     }
  })
}
```


### 1. Download storm data & convert column names to lower case for consistency



```r
zipfilename <- "repdata%2Fdata%2FStormData.csv.bz2"

fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"


if (!file.exists(zipfilename)) {
    download.file(fileURL, dest=zipfilename,mode="wb",quiet=FALSE) 
   }

df <- read.csv(bzfile("repdata%2Fdata%2FStormData.csv.bz2"),header=TRUE,stringsAsFactors = FALSE)

colnames(df) <- tolower(colnames(df)) 
```


### 2. Data Processing & transformation

The following steps prepare the data for processing. The conversion from dmgexp (codes) to the multiplication factor is needed to compute costs. For example the value 'k' corresponds to 1000's.

* prepare a lookup table to convert prop damage codes  
  ("" "-" "?" "+" "0" "1" "2" "3" "4" "5" "6" "7" "8" "B" "h" "H" "K" "m" "M") 
  to multiplication factors  
  (0,0,0,1,10,10,10,10,10,10,10,10,10,109,102,102,103,106,106)
* The above multplication factors are applied to derive property and crop damage costs




```r
df.damage <- subset(df,select=c(evtype, propdmg,propdmgexp,cropdmg,cropdmgexp))
Symbol <- sort(unique(as.character(df.damage$propdmgexp)))
Factor <- c(0,0,0,1,10,10,10,10,10,10,10,10,10,10^9,10^2,10^2,10^3,10^6,10^6)

convert.Factor <- data.frame(Symbol, Factor)

df.damage$Prop.Factor <- convert.Factor$Factor[match(df.damage$propdmgexp, convert.Factor$Symbol)]
df.damage$Crop.Factor <- convert.Factor$Factor[match(df.damage$cropdmgexp, convert.Factor$Symbol)]
```


### 3. Analysis question 1 - Most harmful events to population health

The code below processes the data to determine which type of events are most harmful with respect to population health. 

The results show the top 10 event types causing fatalities and injuries



```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.4.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
hdf <- aggregate(data=df,cbind(fatalities,injuries) ~ evtype,FUN=sum) %>% arrange (desc(fatalities))
head(hdf,10)
```

```
##            evtype fatalities injuries
## 1         TORNADO       5633    91346
## 2  EXCESSIVE HEAT       1903     6525
## 3     FLASH FLOOD        978     1777
## 4            HEAT        937     2100
## 5       LIGHTNING        816     5230
## 6       TSTM WIND        504     6957
## 7           FLOOD        470     6789
## 8     RIP CURRENT        368      232
## 9       HIGH WIND        248     1137
## 10      AVALANCHE        224      170
```

The results show that Tornado's are by far the biggest cause of fatalities and injuries followed by Excessive heat and Flash floods.


### 4. Analysis question 2 - Events causing greatest economic consequences


The following steps are executed:

* Compute property and  crop damage costs
* Compute total damage cost as of property and crop damagae costs
* The top 10 costs by total damage is produced as output


```r
df.damage <- df.damage %>% mutate(propdmg = propdmg*Prop.Factor, cropdmg = cropdmg*Crop.Factor, total.dmg = propdmg+cropdmg)

edf <- aggregate(data=df.damage,cbind(total.dmg,propdmg,cropdmg) ~ evtype,FUN=sum) %>% arrange (desc(total.dmg))
 
head(edf,10)
```

```
##               evtype    total.dmg      propdmg     cropdmg
## 1              FLOOD 150319678250 144657709800  5661968450
## 2  HURRICANE/TYPHOON  71913712800  69305840000  2607872800
## 3            TORNADO  57352117607  56937162897   414954710
## 4        STORM SURGE  43323541000  43323536000        5000
## 5               HAIL  18757611527  15732073877  3025537650
## 6        FLASH FLOOD  17562132111  16140815011  1421317100
## 7            DROUGHT  15018672000   1046106000 13972566000
## 8          HURRICANE  14610229010  11868319010  2741910000
## 9        RIVER FLOOD  10148404500   5118945500  5029459000
## 10         ICE STORM   8967041810   3944928310  5022113500
```

The results show that Floods are the biggest cause of total damage arising out of property and crop damage costs


### 5. Figure - Analysis on damage impact of top 10 weather events to population

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.3
```

```r
library(reshape2)
par(mfrow=c(1,1))
dfmelt <- melt(hdf[1:10,],id.vars='evtype')
ggplot(dfmelt,aes(x=reorder(evtype,-value),y=value)) + geom_bar(stat='identity',aes(fill=variable),position='dodge') +  theme(axis.text.x = element_text(angle=45,hjust=1))+ggtitle("Top 10 Events with highest population impact") +labs(x="EVENT TYPE", y="Total impact")
```

![](Project_2_Weather_event_analysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



### 6. Figure - Analysis on damage impact of top 10 weather events to economy


```r
library(reshape2)
edfmelt <- melt(edf[1:10,],id.vars='evtype')
ggplot(edfmelt,aes(x=reorder(evtype,-value),y=value)) + geom_bar(stat='identity',aes(fill=variable),position='dodge') +  theme(axis.text.x = element_text(angle=45,hjust=1))+ggtitle("Top 10 Events with Highest impact") +labs(x="EVENT TYPE", y="Total economic impact")
```

![](Project_2_Weather_event_analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

### 7. Summary of Results

This report analysis the weather data over many years to produce a report on a) which events are the most impactful to the population b) which events produce the most impacful economic consequences.

The report details the top events for both these analysis including tables and charts showing quantitatively and visually the top events

* the analysis determines that tornadoes are the largest cause of fatalities and injuries
* the analysis determines that Floods are largest cuase of economic consequences


