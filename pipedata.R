rm(list=ls())
library(tidyverse)
library(readr)
WaterPipesBurned <- read_csv("Burned Pipes/WaterPipesBurned.csv")%>%
  mutate(burned=1)
library(readr)
WaterPipesUnburned <- read_csv("Burned Pipes/WaterPipesUnburned.csv")%>%
  mutate(burned=0)
library(readr)
Pipes1853Burned <- read_csv("Burned Pipes/Pipes1853Burned.csv")%>%
  mutate(burned=1)
library(readr)
Pipes1853UnBurned <- read_csv("Burned Pipes/Pipes1853UnBurned.csv")%>%
  mutate(burned=1)%>%
  mutate(OBJECTID = (oid_save+1)) %>%
  select(DistPipeW, OBJECTID, length, burned)
Wp_data <- left_join(WaterPipesBurned, Pipes1853UnBurned, by = c("OBJECTID", "burned"))%>%
  select(-Shape_Leng)%>%
  mutate(WaterDate=1852)%>%
  mutate(W_Pipe_in= DistPipeW)%>%
  mutate(W_comments =paste(as.character(WaterDate),"/", as.character(W_Pipe_in),"/", as.character(W_PipeLeng), ";"))

Wp2_data <- left_join(WaterPipesUnburned, Pipes1853UnBurned, by = c("OBJECTID", "burned"))%>%
  select(-Shape_Leng)%>%
  mutate(WaterDate=1852)%>%
  mutate(W_Pipe_in= DistPipeW)%>%
  mutate(W_comments =paste(as.character(WaterDate),"/", as.character(W_Pipe_in),"/", as.character(W_PipeLeng), ";"))

library(readr)
DistToFire_Burned <- read_csv("Burned Pipes/DistToFire_Burned.csv")%>%
  select(-OBJECTID)%>%
  rename(OBJECTID=oid_save)%>%
  mutate(OBJECTID=OBJECTID+1)%>%
  select(OBJECTID, NEAR_DIST, Shape_Length)%>%
  rename(dist_sl=Shape_Length)%>%
  mutate(burned=1)
Wp3_data <- left_join(Wp_data, DistToFire_Burned, by = c("OBJECTID", "burned"))

library(readr)
DistToFire_Unburned <- read_csv("Burned Pipes/DistToFire_Unburned.csv")
View(DistToFire_Unburned)

View(DistToFire_Burned)











Wp_data$WaterDate <- as.numeric(Wp_data$WaterDate)
Wp_data$W_Pipe_in <- as.numeric(Wp_data$W_Pipe_in)
Wp_data$W_PipeLeng <- as.numeric(Wp_data$W_PipeLeng)


View(Pipes1853UnBurned)



View(Pipes1853Burned)
  
View(WaterPipesUnburned)
View(WaterPipesBurned)
