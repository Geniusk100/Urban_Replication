rm(list=ls())
library(tidyverse)
library(readr)
#%%%%%%%%%%%%%%1867%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X1867_Burned <- read_csv("Road width clean/1867_Burned.csv")%>%
  mutate(burned=1)%>%
  mutate(year=1867)

library(readr)
X1867_Unburned <- read_csv("Road width clean/1867_Unburned.csv")%>%
  mutate(burned=0)%>%
  mutate(year=1867)
#%%%%%%%%%%%%%%1867%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
  
  
#%%%%%%%%%%%%%%1873%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
library(readr)
X1873_Burned <- read_csv("Road width clean/1873_Burned.csv")%>%
  mutate(burned=1)%>%
  rename(length=Shape_Le_1)%>%
  mutate(year=1867)


library(readr)
X1873_Unburned <- read_csv("Road width clean/1873_Unburned.csv")%>%
  mutate(burned=0)%>%
  mutate(year=1867)
  

#%%%%%%%%%%%%%%1873%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
  
  
#%%%%%%%%%%%%%%1882%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
library(readr)
X1882_Burned <- read_csv("Road width clean/1882_Burned.csv")%>%
  mutate(burned=1)%>%
  rename(length=Shape_Le_1)%>%
  mutate(year=1882)
View(X1882_Burned)


library(readr)
X1882_Unburned <- read_csv("Road width clean/1882_Unburned.csv")%>%
  mutate(burned=0)%>%
  rename(length=Shape_Le_1)%>%
  mutate(year=1882)
View(X1882_Unburned)

#%%%%%%%%%%%%%%1882%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   




#%%%%%%%%%%%%%%1895%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
library(readr)
X1890_Burned <- read_csv("Road width clean/1890_Burned.csv")%>%
  mutate(burned=1)%>%
  rename(length=Shape_Le_1)%>%
  mutate(year=1895)
View(X1890_Burned)

library(readr)
X1890_Unburned <- read_csv("Road width clean/1890_Unburned.csv")%>%
  mutate(burned=0)%>%
  rename(length=Shape_Le_1)%>%
  mutate(year=1895)
View(X1890_Unburned)

#%%%%%%%%%%%%%%1890%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

#%%%%%%%%%%%%%%1882%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
library(readr)
DistToFire_Burned <- read_csv("Road width clean/DistToFire_Burned.csv")%>%
  select(-OBJECTID)%>%
  rename(OBJECTID=oid_save)%>%
  mutate(OBJECTID=OBJECTID+1)%>%
  select(OBJECTID, NEAR_DIST, Shape_Length)%>%
  rename(dist_sl=Shape_Length)%>%
  mutate(burned=1)
View(DistToFire_Burned)

library(readr)
DistToFire_Unburned <- read_csv("Road width clean/DistToFire_Unburned.csv")%>%
  select(-OBJECTID)%>%
  rename(OBJECTID=oid_save)%>%
  mutate(OBJECTID=OBJECTID+1)%>%
  select(OBJECTID, NEAR_DIST, Shape_Length)%>%
  rename(dist_sl=Shape_Length)%>%
  mutate(burned=0)
View(DistToFire_Unburned)
#%%%%%%%%%%%%%%1882%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


#%%%%%%%%%%%%%%2014%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
library(readr)
Modern_Burned <- read_csv("Road width clean/Modern_Burned.csv")%>%
  mutate(burned=1)%>%
  mutate(year=2014)%>%
  mutate(width = RightSidew+ RightShoul+ MedianWidt+ LeftSidewa+ LeftShould+ SurfaceWid)%>%
  mutate(width2 = RightOfWay)%>%
  select( year, burned, length, width)
View(Modern_Burned)

library(readr)
Modern_Unburned <- read_csv("Road width clean/Modern_Unburned.csv")%>%
  mutate(burned=0)%>%
  mutate(year=2014)%>%
  mutate(width = RightSidew+ RightShoul+ MedianWidt+ LeftSidewa+ LeftShould+ SurfaceWid)%>%
  mutate(width2 = RightOfWay)%>%
  select( year, burned, length, width)
View(Modern_Unburned)
View(X1873_Unburned)
View(X1873_Burned)
#%%%%%%%%%%%%%%2014%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
  






View(X1867_Unburned)

View(X1867_Burned)