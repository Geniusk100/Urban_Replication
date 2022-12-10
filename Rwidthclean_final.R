rm(list=ls())
library(tidyverse)
library(readr)
#%%%%%%%%%%%%%%1867%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X1867_Burned <- read_csv("Road width clean/1867_Burned.csv")%>%
  rename_all(tolower)%>%
  mutate(burned=1)
  
X1867_Unburned <- read_csv("Road width clean/1867_Unburned.csv")%>%
  rename_all(tolower)%>%
  mutate(burned=0)

rb01<-rbind(X1867_Unburned, X1867_Burned)%>%
  mutate(year=1867)%>%
  mutate(width=ifelse(roadw_1867!=0,roadw_1867,0 ))%>%
  mutate(width=ifelse(roadw_67!=0,(width+roadw_67)/2,width ))%>%
  select(objectid, full_name, length,burned, width, year )

#%%%%%%%%%%%%%%1867%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
  
  
#%%%%%%%%%%%%%%1873%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
library(readr)
X1873_Burned <- read_csv("Road width clean/1873_Burned.csv")%>%
  rename_all(tolower)%>%
  mutate(burned=1)

X1873_Unburned <- read_csv("Road width clean/1873_Unburned.csv")%>%
  rename_all(tolower)%>%
  mutate(burned=0)
rb02<-rbind(X1873_Unburned, X1873_Burned)%>%
  rename(length=shape_le_1)%>%
  mutate(year=1873)%>%
  mutate(width=ifelse(roadw_1873 !=0,roadw_1873 ,0 ))%>%
  mutate(width=ifelse(roadw_73 !=0,(width+roadw_73)/2,width ))%>%
  select(objectid, full_name, length,burned, width, year )

rb03<-rbind(rb02, rb01)

#%%%%%%%%%%%%%%1873%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
  
  
#%%%%%%%%%%%%%%1882%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
library(readr)
X1882_Burned <- read_csv("Road width clean/1882_Burned.csv")%>%
  rename_all(tolower)%>%
  mutate(burned=1)

X1882_Unburned <- read_csv("Road width clean/1882_Unburned.csv")%>%
  rename_all(tolower)%>%
  mutate(burned=0)

rb04<-rbind(X1882_Unburned, X1882_Burned)%>%
  rename(length=shape_le_1)%>%
  mutate(year=1882)%>%
  mutate(width=ifelse(roadw_1882!=0,roadw_1882,0 ))%>%
  mutate(width=ifelse(roadw_82!=0,(width+roadw_82)/2,width ))%>%
  select(objectid, full_name, length,burned, width, year )

rb05<-rbind(rb04, rb03)
#%%%%%%%%%%%%%%1882%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   




#%%%%%%%%%%%%%%1895%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
library(readr)
X1890_Burned <- read_csv("Road width clean/1890_Burned.csv")%>%
  rename_all(tolower)%>%
  mutate(burned=1)
X1890_Unburned <- read_csv("Road width clean/1890_Unburned.csv")%>%
  rename_all(tolower)%>%
  mutate(burned=0)  
  
rb06<-rbind(X1890_Unburned, X1890_Burned)%>%  
  rename(length=shape_le_1)%>%
  mutate(year=1895)%>%
  mutate(width=ifelse(roadw_1880!=0,roadw_1880,0 ))%>%
  mutate(width=ifelse(roadw_80!=0,(width+roadw_80)/2,width ))%>%
  select(objectid, full_name, length,burned, width, year )
  
rb07<-rbind(rb06, rb05)
#%%%%%%%%%%%%%%1890%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 



#%%%%%%%%%%%%%%2014%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
library(readr)
library(plyr)
Modern_Burned <- read_csv("Road width clean/Modern_Burned.csv")%>%
  rename_all(tolower)%>%
  mutate(burned=1)
Modern_Unburned <- read_csv("Road width clean/Modern_Unburned.csv")%>%
  rename_all(tolower)%>%
  mutate(burned=0)
rb08<-rbind(Modern_Unburned, Modern_Burned)%>% 
  mutate(width = rightsidew+ rightshoul+ medianwidt+ leftsidewa+ leftshould+ surfacewid)%>%
  mutate(width2 = rightofway)%>%
  mutate(year=2014)%>%
  select( length, burned, width,width2, year)


rb09<-rbind.fill(rb08, rb07)


#%%%%%%%%%%%%%%2014%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%1882%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
library(readr)
DistToFire_Burned <- read_csv("Road width clean/DistToFire_Burned.csv")%>%
  rename_all(tolower)%>%
  select(-objectid)%>%
  mutate(objectid=oid_save)%>%
  mutate(objectid=objectid+1)%>%
  select(objectid, near_dist, shape_length)%>%
  mutate(dist_sl=shape_length)%>%
  mutate(burned=1)
rw1 <- right_join(DistToFire_Burned, rb09, by = c("objectid", "burned"))

DistToFire_Unburned <- read_csv("Road width clean/DistToFire_Unburned.csv")%>%
  rename_all(tolower)%>%
  select(-objectid)%>%
  mutate(near_distu=near_dist)%>%
  mutate(objectid=oid_save)%>%
  mutate(objectid=objectid+1)%>%
  select(objectid, near_distu, shape_length)%>%
  mutate(dist_sl=shape_length)%>%
  mutate(burned=0)
rw2 <- right_join(DistToFire_Unburned, rw1, by = c("objectid", "burned"))
#%%%%%%%%%%%%%%1882%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

  
  





