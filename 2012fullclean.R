
install.packages("data.table")
install.packages("tidyverse")

library(tidyverse)
#----------------------------------------------------------------------------------------------------------------------------------------------
AllParcels2012 <- read_csv("C:/Users/K3rgo/OneDrive/Desktop/New folder (2)/Urban/New folder/AllParcels2012.txt") 
AllParcels2012 <- AllParcels2012 %>% select(WARD, PARCEL, PID_LONG, SHAPE_area, point_x, point_y, fire_dist) 
  df <- data.frame(AllParcels2012) #Creates a data frame#
  df<- df %>% distinct() # Removes duplicated values#
  df$PID_LONG <- as.numeric(df$PID_LONG)
  df %>% count(PID_LONG) #counts values in the data set#
  df %>% filter(PARCEL == "NA")
  df <- df %>% group_by(WARD, PARCEL, PID_LONG) 
 df <- df %>%  summarize(fire_dist = mean(fire_dist), SHAPE_area = mean(SHAPE_area), point_x = mean(point_x), point_y = mean(point_y))
  
#----------------------------------------------------------------------------------------------------------------------------------------------
  
  
  

 #----------------------------------------------------------------------------------------------------------------------------------------------
 
library(readr)
SampleParcels <- read_csv("SampleParcels.csv")
df_2 <- data.frame(SampleParcels)
df_2 <- df_2 %>% group_by(WARD, PARCEL, PID_LONG)%>%  
  summarize(SampleArea = mean(SampleArea))
df_2$PID_LONG <- as.numeric(df_2$PID_LONG)
df_3 <- left_join(df, df_2,by = c("WARD", "PARCEL", "PID_LONG")) %>%
  mutate (sample_frac= SampleArea/SHAPE_area) %>% 
  mutate(sample_frac = ifelse(SampleArea/SHAPE_area > 1, 1 ,0))
 

View(df_3)
#----------------------------------------------------------------------------------------------------------------------------------------------




#----------------------------------------------------------------------------------------------------------------------------------------------

library(readr)
BurnedParcels <- read_csv("BurnedParcels.txt") 
df_4 <- data.frame(BurnedParcels)
df_4 <- df_4 %>% group_by(WARD, PARCEL, PID_LONG) 
df_4 <- df_4 %>%  summarize(BurnedArea = mean(BurnedArea)) 
df_4$PID_LONG <- as.numeric(df_4$PID_LONG)
df_5 <- left_join(df_3, df_4,by = c("WARD", "PARCEL", "PID_LONG"))%>%
  mutate (burned_frac= BurnedArea/SHAPE_area) %>% 
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  rename_all(tolower) %>%
View(BurnedParcels)

#----------------------------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------------------------------------------

library(readr)
ConstructionParcels <- read_csv("ConstructionParcels.txt")
df_6 <- data.frame(ConstructionParcels)
df_6 <- df_6 %>% group_by(WARD, PARCEL, PID_LONG) 
df_6 <- df_6 %>%  summarize(ConstArea = mean(ConstArea)) 
df_6$PID_LONG <- as.numeric(df_6$PID_LONG)
df_7 <- left_join(df_5, df_6,by = c("WARD", "PARCEL", "PID_LONG"))%>%
  mutate (const_frac= ConstArea/SHAPE_area)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

View(ConstructionParcels)
#----------------------------------------------------------------------------------------------------------------------------------------------



#----------------------------------------------------------------------------------------------------------------------------------------------

library(readr)
SampleParcelCentroids <- read_csv("SampleParcelCentroids.txt")
SampleParcelCentroids <- SampleParcelCentroids %>% select(bad_points, block_id, wharf, dist_burne, burned, S_point_y, S_point_x, WARD, PARCEL, PID_LONG)
df_8 <- data.frame(SampleParcelCentroids)%>%
  mutate(burnedarea = 0)%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
df_8 <- df_8 %>% group_by(WARD, PARCEL, PID_LONG)
df_8 <- df_8 %>%  summarize(S_point_x = mean(S_point_x),S_point_y = mean(S_point_y),burned = mean(burned),dist_burne = mean(dist_burne),wharf=mean(wharf),block_id=mean(block_id), bad_points=mean(bad_points))
df_8$PID_LONG <- as.numeric(df_8$PID_LONG)
df_9 <- left_join(df_7, df_8,by = c("WARD", "PARCEL", "PID_LONG"))%>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))
  
View(SampleParcelCentroids)
#----------------------------------------------------------------------------------------------------------------------------------------------






#----------------------------------------------------------------------------------------------------------------------------------------------

library(readr)
DATA2012_FULL <- read_csv("DATA2012-FULL.txt")
DATA2012_FULL <- DATA2012_FULL %>% select(PID, CM_ID, ST_NUM, ST_NAME, ST_NAME_SUF, ZIPCODE, OWNER, AV_LAND, AV_BLDG, AV_TOTAL, GROSS_TAX, LAND_SF, LU, YR_BUILT)

df_10 <- data.frame(DATA2012_FULL) %>% 
  mutate(ST_NUM = str_remove_all(ST_NUM," "))%>%
  mutate(ST_NAME = str_remove_all(ST_NAME," "))%>%
  mutate(ST_NAME_SUF = str_remove_all(ST_NAME_SUF," "))%>%
  mutate(ST_NUM = str_replace_all(ST_NUM,"_ ", " - "))%>%
  mutate(ST_NUM = str_replace_all(ST_NUM," _"," - "))%>%
  mutate(PID_LONG = as.numeric(PID)) 

df_10 <- df_10 %>% group_by(PID, CM_ID, ST_NUM, ST_NAME, ST_NAME_SUF, ZIPCODE) 
df_10 <- df_10 %>%  summarize(OWNER = first(OWNER),AV_LAND = first(AV_LAND),AV_BLDG = first(AV_BLDG), AV_TOTAL = first(AV_TOTAL), GROSS_TAX = first(GROSS_TAX), LAND_SF = first(LAND_SF), LU = first(LU), YR_BUILT=min(YR_BUILT) )
df_10$CM_ID <- as.numeric(df_10$CM_ID)
df_11 <-df_10 %>%  
  mutate(PID_LONG = as.numeric(PID))  %>%
  mutate(strpid = as.character(PID_LONG))%>%
  mutate(address =paste(ST_NUM, ST_NAME, ST_NAME_SUF, as.character(ZIPCODE)))%>%
  mutate(condo_id= 0) %>%
  mutate(CM_ID = ifelse(!(is.na(CM_ID)), PID_LONG,CM_ID))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 0302953018, 302953010,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse( PID_LONG == 305424300 | PID_LONG == 305424030,305424020, PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse( PID_LONG == 303041300 | PID_LONG == 303041010,303041000, PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse( PID_LONG == 304304402 | PID_LONG == 304304401,304304400, PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse( PID_LONG == 30511201 | PID_LONG == 305112012,305112010, PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse( PID_LONG == 304826012 | PID_LONG == 304826014, 304826010, PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 500043011, 500043010,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304133001, 302953010,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG ==  301674001, 301674000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 500001001, 500001000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 305651001, 305651000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 500045001, 500045000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304500200, 304500000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304890100, 304890000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304692051, 304692050,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 305380001, 305380000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304893001, 304893000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG ==  302862001, 302862000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304788001, 500001000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304102001, 305651000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304605001, 500045000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304692050, 304500000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304821001, 304102000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304841001, 304692050,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304605001, 304605000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304692050, 304692000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304821001, 304821000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304841001, 304841000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 304860001, 304860000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 305106001, 305106000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 305107001, 305107000 ,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 305777001, 305777000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 302952014, 302952010,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 303028500, 303028300,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 305107001, 305107000 ,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse(PID_LONG == 303740000, 303747000,PID_LONG ))
df_11<-mutate(df_11, PID_LONG = ifelse( PID_LONG == 304870400 | PID_LONG == 304870020, 304870010, PID_LONG ))
df_11<-mutate(df_11, CM_ID = ifelse(CM_ID == 303740000, 303747000,CM_ID ))
df_11<-mutate(df_11, PID_LONG = ifelse( PID_LONG == 304832420 | PID_LONG == 304832400| PID_LONG == 304832020, 304832010, PID_LONG ))


df_11$n_index <- 1:nrow(df_10)
#305424300 %in% df_11$PID_LONG
#df_11$condo_id[!(is.na(CM_ID)), condo_id:=CM_ID]
 str(df_11$CM_ID)

summarise(OWNER = first(OWNER),AV_LAND = first(AV_LAND),AV_BLDG = first(AV_BLDG), AV_TOTAL = first(AV_TOTAL), GROSS_TAX = first(GROSS_TAX), LAND_SF = first(LAND_SF), LU = first(LU), YR_BUILT=min(YR_BUILT) )
#----------------------------------------------------------------------------------------------------------------------------------------------

#datatest <- DATA2012_FULL %>% 
 # select(-3:-5, -7:-9)
#stringer: str_pad(zip,)



#----------------------------------------------------------------------------------------------------------------------------------------------

library(readr)
X2012MapPointsGIS_2_24_13_1_ <- read_csv("2012MapPointsGIS_2_24_13 (1).csv")
View(X2012MapPointsGIS_2_24_13_1_)
#----------------------------------------------------------------------------------------------------------------------------------------------
