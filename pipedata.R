rm(list=ls())
library(tidyverse)
library(readr)
WaterPipesBurned <- read_csv("Burned Pipes/WaterPipesBurned.csv")%>%
  rename_all(tolower) %>%
  mutate(burned=1)

WaterPipesUnburned <- read_csv("Burned Pipes/WaterPipesUnburned.csv")%>%
  rename_all(tolower) %>%
  mutate(burned=0)
rb1<-rbind(WaterPipesUnburned,WaterPipesBurned )
#%%%%%%%%%%%%%%1853%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(readr)
Pipes1853Burned <- read_csv("Burned Pipes/Pipes1853Burned.csv")%>%
  rename_all(tolower) %>%
  mutate(burned=1)

Pipes1853UnBurned <- read_csv("Burned Pipes/Pipes1853UnBurned.csv")%>%
  rename_all(tolower) %>%
  mutate(burned=0)


rb2<-rbind(Pipes1853UnBurned,Pipes1853Burned )%>%
  mutate(objectid = (oid_save+1)) %>%
  select(distpipew, mainpipew, objectid, length, burned)
Wp_data <- right_join(rb2, rb1, by = c("objectid", "burned"))%>%
  select(-shape_leng)%>%
  mutate(waterdate=as.character(waterdate))%>%
  mutate(w_pipe_in=as.character(w_pipe_in))%>%
  mutate(w_pipeleng=as.character(w_pipeleng))%>%
  mutate(w_comments = paste(waterdate,"/", w_pipe_in,"/", w_pipeleng, ";"))%>%
  mutate(waterdate=as.numeric(waterdate))%>%
  mutate(w_pipe_in=as.numeric(w_pipe_in))%>%
  mutate(w_pipeleng=as.numeric(w_pipeleng))%>%
  mutate(waterdate=1852)%>%
  mutate(w_pipe_in = distpipew)%>%
  mutate(w_pipe_in= ifelse(w_pipe_in==0," .",w_pipe_in))
                        
#%%%%%%%%%%%%%%1853%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(readr)

DistToFire_Burned <- read_csv("Burned Pipes/DistToFire_Burned.csv")%>%
  rename_all(tolower) %>%
  select(-objectid)%>%
  rename(objectid=oid_save)%>%
  mutate(objectid=objectid+1)%>%
  select(objectid, near_dist, shape_length)%>%
  rename(dist_sl=shape_length)%>%
  mutate(burned=1)

Wp3_data <- right_join(DistToFire_Burned,Wp_data, by = c("objectid", "burned"))

library(readr)
DistToFire_Unburned <- read_csv("Burned Pipes/DistToFire_Unburned.csv")%>%
  rename_all(tolower) %>%
  select(-objectid)%>%
  rename(objectid=oid_save)%>%
  rename(near_distu=near_dist)%>%
  mutate(objectid=objectid+1)%>%
  select(objectid, near_distu, shape_length)%>%
  rename(dist_sl=shape_length)%>%
  mutate(burned=0)
Wp4_data <- full_join(DistToFire_Unburned,Wp3_data, by = c("objectid", "burned"))









