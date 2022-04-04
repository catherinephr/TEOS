##############ExploringTEOS-10andHydrostationSData
#CatherineHernandez
#catherine.hernandez2@upr.edu
#24March2022
########Loadlibraries
library(tidyverse)
#install.packages("seacarb")
library(gsw)


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

#lets see what our density data looks like
hydrostation_bottle %>%
  ggplot()+geom_point(mapping = aes(x=decy,y=`Sig-th`))

hydrostation_bottle %>%
  filter(`Sig-th`!=-999) %>%
  ggplot()+geom_point(mapping = aes(x=decy,y=`Sig-th`))
#it looks like seasonal signal, lets see a line plot
hydrostation_bottle %>%
  filter(`Sig-th`!=-999 & Depth < 20) %>%
  ggplot()+geom_line(mapping = aes(x=Temp,y=`Sig-th`))

#but we're still missing half our data 
#so lets see if we can fill that in from our TEOS-10 package gsw

?gsw_sigma0 # using help says we need absolute salinity and conservative temperature to calculate sigma-theta at 0dbars
#Absolute salinity[g/kg]
#Conservative Temperature[degC]

#We need to get abolute salinity
?gsw_SA_from_SP
# practical salinity
# sea pressure (dbar)
#longitude in decimal degrees
#latitude in decimal degrees


hydrostation_bottle %>%
  ggplot()+geom_point(mapping = aes(x=decy,y= Depth))

#We're missign pressure but we have depth

?gsw_p_from_z
#Add a sea pressure column using gsw
 hydrostation_bottle=
   hydrostation_bottle %>%
   mutate(sea_pressure=gsw_p_from_z(Depth*-1,latN))
 #
 hydrostation_bottle %>%
   ggplot()+geom_point(mapping = aes(x=Pres,y= sea_pressure))
 
 hydrostation_bottle=
 hydrostation_bottle %>%
mutate(absolute_sal=gsw_SA_from_SP (Sal1,sea_pressure,360-lonW,latN))

hydrostation_bottle%>%
  filter(Sal1 >30) %>%
  ggplot()+geom_point(mapping=aes(x=Sal1,absolute_sal))

?gsw_CT_from_t

hydrostation_bottle=
  hydrostation_bottle %>%
  filter(Temp != -999) %>%
  mutate(sigma_theta=gsw_sigma0(absolute_sal,conservative_temp))

hydrostation_bottle %>%
  filter(`Sig-th` != -999) %>%
 ggplot()+geom_point(mapping = aes(x=`Sig-th`,y=sigma_theta))
  

hydrostation_bottle %>%
  filter(sigma_theta<20) %>% 
View()

# We can see that ctd salinity does not agree with bottle salinity
#operating under the hypothesis that there was an error in the bottle sample, we can instead calculate our resulting parameters using the CTD_S instead.

erroneous_sal=hydrostation_bottle %>%
 filter(sigma_theta<20) %>%
  mutate(sea_pressure=gsw_p_from_z(Depth*-1,latN)) %>%
  mutate(absolute_sal=gsw_SA_from_SP(CTD_S,sea_pressure,360-lonW,latN))%>%
  mutate(conservative_temp=gsw_CT_from_t(absolute_sal,Temp,sea_pressure)) %>%
  mutate(sigma_theta=gsw_sigma0(absolute_sal,conservative_temp))
  View(erroneous_sal)
  
  correct_sal=hydrostation_bottle %>%
    filter(sigma_theta>20)
  
  hydroS=rbind(correct_sal,erroneous_sal)
  
  hydroS%>%
    ggplot()+geom_point(mapping = aes(x=sigma_theta, y=Depth))+scale_y_reverse()+
    scale_x_continuous(position = "top")+
    theme_classic()
  

  # March 31/2022
  
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
  
  
 
  
  hydrostation_bottle %>% ggplot()+ geom_line(aes(x=decy, y=Temp))
  
  # Sigma-Theta vs year
  
  hydroS %>%
    filter(sigma_theta!=-999 & Depth <15) %>%
    ggplot()+geom_line(mapping = aes(x=decy,y=sigma_theta))+
    xlab("Year")+
    ylab ("Sigma-Theta σθ (kg/m^3)")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text=element_text(size=15))
  

  # Temperature vs Year
  
  hydroS%>%
    filter(Depth <15) %>%
    ggplot()+geom_line(mapping = aes(x=decy,y=`Temp`))+
    xlab("Year")+
    ylab (" Temperature (°C)")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text=element_text(size=15))
  
  # Year vs Salinity
  
  hydroS%>%
    filter(absolute_sal>30 &Depth <15) %>%
    ggplot()+geom_line(mapping = aes(x=decy,y=`absolute_sal`))+
    xlab("Year")+
    ylab (" Salinity (°PSS)")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text=element_text(size=15))


# Year vs O2
  
  hydroS%>%
    filter(Depth <15 & `O2(1)`!=-999) %>%
    ggplot()+geom_line(mapping = aes(x=decy,y=`O2(1)`))+
    xlab("Year")+
    ylab (" Oxygen-1 (umol/kg)")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text=element_text(size=15))
  
  
  
  hydroS%>%
    filter(Depth <15 & `O2(1)`!=-999) %>%
    ggplot()+geom_line(mapping = aes(x=decy,y=`O2(1)`))+
    xlab("Year")+
    ylab (" Oxygen-1 (umol/kg)")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text=element_text(size=15))
  
  # to find miss data
  
  hydroS%>%
    filter(Depth <15 & `O2(1)`!=-999) %>%
    ggplot()+geom_line(mapping = aes(x=decy,y=`O2(1)`))+
    xlab("Year")+
    ylab (" Oxygen-1 (umol/kg)")+ scale_x_continuous(limits = c(1977,1981),breaks = seq(1977,1981,1))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text=element_text(size=15))
  
  df <- data.frame (decy= 1979)
 
  
  
  
  
  
  





  
  
  


