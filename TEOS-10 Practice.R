##############ExploringTEOS-10andHydrostationSData
#CatherineHernandez
#catherine.hernandez2@upr.edu
#24March2022
########Loadlibraries
library(tidyverse)
#install.packages("seacarb")
library(gsw)
library(ggplot2)


## Pick 2-3 questions to write a cohesive reporte using the data. You can focus on changes in one parameter throuhg depth, time, season,etc, or how they co-vary.
# Be sure to include a descrpition of the data (see url at top of your code oru use goolge to search for Hydrostation s), explain your questions and why you are asking them
# and answer your questions with publication quality plots, tables and text.
#1 How do T, S, sigma-theta, and o2 vary with depth?
#2. How do T, S, sigma-theta, and o2 vary through time? Are there differences in this relationship between sallow and deep waters?
### surface waters= <30 m
### Deep waters= > 2000 m
#3) Are there seasonal differences in T, S, sigma-theta, and O2?
### winter= December, january, February, March (cooler temperatures, deeper mixing?)
### Summer= July, August, September, October (warmer temperatures, Shallower mixing?)
#4) How do T, S, Sigma-theta and O2 co-vary?
#5) Are there differences in T, S and O2 between hydostations and Beacon sites?


####letsgetsomedata
hydrostation_bottle<-read_delim("http://batsftp.bios.edu/Hydrostation_S/bottle/hydrostation_bottle.txt",
                                delim="\t", escape_double =FALSE,
                                col_names=FALSE,trim_ws=TRUE, skip=31)
View(hydrostation_bottle)
#rename the columns

colnames(hydrostation_bottle)=
  c("Id","yyyymmd","decy","time","latN","lonW","Depth","Pres","Temp","CTD_S","Sal1","Sig-th","O2(1)","OxFix","Anom1")

#yyyymmdd = Year Month Day   
#decy   = Decimal Year     
#time   = Time (hhmm)      
#latN   = Latitude (Deg N) 
#lonW   = Longitude (Deg W)
#Depth  = Depth (m)                  
#Temp   = Temperature ITS-90 (C) 
#Pres   = CTD Pressure (dbar)   
#CTD_S  = CTD Salinity (PSS-78)      
#Sal1   = Salinity-1 (PSS-78)        
#Sig-th = Sigma-Theta (kg/m^3)       
#O2(1)  = Oxygen-1 (umol/kg)          
#OxFixT = Oxygen Fix Temp (C)        
#Anom1  = Oxy Anomaly-1 (umol/kg)    
#/Quality flags
#-999  = No data
#0 = Less than detection limit

hydrostation_bottle %>% ggplot()+ geom_line(aes(x=decy, y=Temp))

hydrostation_bottle %>%
  filter(`Sig-th`!=-999 & Depth < 20) %>%
  ggplot()+geom_line(mapping = aes(x=decy,y=`Sig-th`))+
  xlab("Year")+
  ylab ("Sigma-Theta σθ (kg/m^3)")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=15))






