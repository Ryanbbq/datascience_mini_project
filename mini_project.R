---
title: "Datascience Mini-Project"
author: "Ryan L. and Joel C."
date: "March 29, 2018"
output: html_document
---

```{r global_options, include=FALSE}
knitr::opts_chunk$set(prompt=TRUE, comment="", echo=TRUE)
```
## This data set provides information on motor vehicle operators (drivers) involved in traffic collisions occuring on county and local roadways. The dataset reports details of all traffic collisions occurring on county and local roadways within [Montgomery County](https://data.montgomerycountymd.gov/Public-Safety/Crash-Reporting-Drivers-Data/mmzv-x632), as collected via the Automated Crash Reporting System (ACRS) of the Maryland State Police, and reported by the Montgomery County Police, Gaithersburg Police, Rockville Police, or the Maryland-National Capital Park Police. This dataset shows each collision data recorded and the drivers involved.


```{r}
dat = read.csv("https://raw.githubusercontent.com/Ryanbbq/datascience_mini_project/master/Crash_Reporting.csv")

```

## Montgomery County Plots
#### This graph shows the categories of substance abuse present in drivers of Montgomery County, and the total number of crashes. The bar 'other' represents the category of substance abuse contributed which contains the sub-categories; combination contributed, illegal drugs contributed, medication contributed and combined substance present for the number of collisions in Montgomery County.  

```{r}
par(mar=c(4,15,3,4))
sub_abuse <- dat$Driver.Substance.Abuse[dat$Driver.Substance.Abuse != "UNKNOWN" & dat$Driver.Substance.Abuse != "NONE DETECTED" & dat$Driver.Substance.Abuse != "N/A"]

sub_abuse[grepl("OTHER|COMBINATION CONTRIBUTED|ILLEGAL DRUG CONTRIBUTED|MEDICATION CONTRIBUTED|ALCOHOL CONTRIBUTED",sub_abuse)] <- "OTHER"

sub_abuse <- table(sub_abuse)
sub_abuse <- sub_abuse[sub_abuse != 0]

barplot(sort(sub_abuse,decreasing=T),
        horiz=T,
        las=1,
        main="Substance Abuse Present and Total Crashes",
        col="red",
        xlim=c(0,2000),
        xlab="Number of Crashes")

```

```{r}
par(mar=c(4,15,3,4))
vehicle_movement <- dat$Vehicle.Movement[dat$Vehicle.Movement != "UNKNOWN" & dat$Vehicle.Movement != "N/A" ]
vehicle_movement[grepl("OTHER|LEAVING TRAFFIC LANE|RIGHT TURN ON RED|NEGOTIATING A CURVE|ENTERING TRAFFIC LANE|PASSING|DRIVERLESS MOVING VEH.|LEAVING TRAFFIC LANE|RIGHT TURN ON RED",vehicle_movement)] <- "OTHER"
vehicle_movement <- table(vehicle_movement)
vehicle_movement <- vehicle_movement[vehicle_movement != 0]


barplot(sort(vehicle_movement,decr=T),horiz=T,las=1,col="red",xlim=c(0,25000),main="Type of Vehicle Movement and Total Crashes",xlab="Number of Crashes")

```



```{r}
par(mar=c(4,25,3,4))

distracted_driving <- dat$Driver.Distracted.By[dat$Driver.Distracted.By != "UNKNOWN" & dat$Driver.Distracted.By != "NOT DISTRACTED"]

distracted_driving[grepl("SMOKING RELATED|DIALING CELLULAR PHONE|TEXTING FROM A CELLULAR PHONE|EATING OR DRINKING|BY MOVING OR DRINKING|BY MOVING OBJECT IN VEHICLE|NO DRIVER PRESENT|USING DEVICE OBJECT BROUGHT INTO VEHICLE|USING OTHER DEVICE CONTROLS INTEGRAL TO VEHICLE| ADJUSTING AUDIO AND OR CLIMATE CONTROLS|OTHER CELLULAR PHONE RELATED|ADJUSTING AUDIO AND OR CLIMATE CONTROLS|TALKING OR LISTENING TO CELLULAR PHONE|TALKING OR LISTENING TO CELLULAR PHONE",distracted_driving)] <- "OTHER DISTRACTION"

distracted_driving <- table(distracted_driving)

distracted_driving <- distracted_driving[distracted_driving != 0]



barplot(sort(distracted_driving,decreasing=T),
        horiz=T,
        las=1,
        main="Distracted Drivers and Crashes",
        col="blue",
        xlim=c(0,8000))

```



```{r}
par(mar=c(2,13,3,2.5))
type_of_collision <- dat$Collision.Type[dat$Collision.Type != "N/A" & dat$Collision.Type != "UNKNOWN"]
type_of_collision <- table(type_of_collision)
type_of_collision <- type_of_collision[type_of_collision != 0]
barplot(sort(type_of_collision,decreasing=T),
        horiz=T,
        las=1,
        main="Types of Collisions",
        col="Blue",
        xlim=c(0,25000))
```

# Number of collisions on a clear day
```{r}
par(mar=c(3,9,3,2.5))
#clear_day <- dat$Weather[dat$Weather != "UNKNOWN" & dat$Weather != "OTHER" & #dat$Weather != "N/A"]

clear_day <- table(dat$Weather[dat$Weather=="CLEAR"])
not_clear <- table(dat$Weather[dat$Weather!="CLEAR"])

barplot(sort(clear_day, decreasing=T),sort(not_clear,decreasing = T),
        main="Was the collision on a clear day?",
        ylim=c(0,50000))

```

# Collisions on a clear day and Alcohol present

```{r}
par(mar=c(2,13,3,2.5))
barplot(sort(table(dat$Weather == "CLEAR")),decreasing=T) ~ sort(table(dat$Driver.Substance.Abuse == "ALCOHOL PRESENT"))

```

```{r}
library(ggplot2)
library(maps)
p <- ggplot(dat,aes(x=Longitude,y=Latitude))





all_states <- map_data("state")

alabama <-subset(all_states,region%in% c("alabama"))

p <- p + geom_polygon( data=all_states, aes(x=long, y=lat, group = group),colour="white", fill="grey10")  + geom_point(color="yellow")


p <- p + geom_point(mapping=NULL,data=dat)

par(pty="s")

plot(Latitude~Longitude,
     data=dat,
     asp=1,
     pch=1,
     col="red",
     xlim=c(-77.5,-77.0),
     ylim=c(39.0,39.5),
     xlab="Latitude",
     ylab="Longitude")

fit = lm(Latitude ~ Longitude, data=dat)

abline(fit,
       lwd=2,
       col="darkolivegreen1")




```




```{r}
passenger_cars <- dat$Vehicle.Body.Type[dat$Vehicle.Body.Type=="PASSENGER CAR"]

```


#Collisions caused by No Traffic Lights VS Collisions occured at Stop Signs
```{r}
nolights=dat[dat$Traffic.Control=="NO CONTROLS",]
lights=dat[dat$Traffic.Control=="STOP SIGN",]
par(mar=c(3,14,3,3))
par(mfrow=c(2,1))
barplot(table(nolights$Injury.Severity),horiz = T,las=1,main = "No Traffic Light Collisions that end injury",xlim = c(0,10000),col = "firebrick")
barplot(table(lights$Injury.Severity),horiz = T,las=1,main = "Stop Sign collisions that end injury",xlim = c(0,10000),col = "firebrick")
```


#Municipalities ordered by fatal crashes
```{r}
par(mar=c(3,14,3,3))
deaths=dat[dat$ACRS.Report.Type=="Fatal Crash" & dat$ACRS.Report.Type !=  "N/A",]
deaths = table(deaths$Municipality)
barplot(sort(deaths,decreasing = TRUE),horiz = TRUE,las=1,main = "Municipalities ordered by Fatal Crashes",col = "firebrick",xlim = c(0,80))
```


#Collisions where alcohol is present and the crash wasnt there fault
```{r}
drunk=dat[dat$Driver.Substance.Abuse=="ALCOHOL PRESENT",]
barplot(sort(table(drunk$Driver.At.Fault=="Yes"),decreasing = TRUE),horiz = TRUE,las=1,main = "Collisions where alcohol is present and the crash wasnt there fault",col = "firebrick",xlab = "Amount of crashes")
```


#Which one caused more injury,a transit bus or school bus?
```{r}
transitbus=dat[dat$Vehicle.Body.Type=="TRANSIT BUS" ,]
schoolbus=dat[dat$Vehicle.Body.Type=="SCHOOL BUS" ,]
par(mar=c(3,14,3,3))
par(mfrow=c(2,1))
barplot(table(transitbus$Injury.Severity),horiz = T,las=1,main = "Injuries caused from transitbus",xlim = c(0,1000),col = "firebrick")
barplot(table(schoolbus$Injury.Severity),horiz = T,las=1,main = "Injuries caused from schoolbus",xlim = c(0,1000),col = "firebrick")
```
