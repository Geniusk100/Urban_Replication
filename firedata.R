rm(list=ls())
library(tidyverse)
library(readr)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% gis_dta%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(readr)
MapPointsGIS_11_21_16 <- read_csv("FAD/MapPointsGIS_11_21_16.csv") %>%
  rename_all(tolower) %>%
  rename(map_id=newid, point_x=xcoord, point_y=ycoord, street_burned=burned, nearest_street_name=full_name, nearest_street_length=shape_leng, dist_to_cbd=nearsh, distance=nearburned)%>%
  mutate(sample=ifelse(samplearea!="NULL", 1,0))%>%
  mutate(burned=0)%>%
  mutate(year = ifelse( year == 2871 | year == 4185| year == 1981| year == 1971, 1871, year ))%>%
  filter(!(year%in%c("1869","1871") & map_id=="0")) %>%
  select(-bad_points,-nearborder,-samplearea)%>%
  arrange(year, map_id)%>%
  distinct()
MP01<- MapPointsGIS_11_21_16  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%GIS_DATA%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%database_map_link.data%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(readr)
fid_entry_master_3_7_13 <- read_csv("FAD/fid_entry_master_3_7_13.csv")%>%
  rename_all(tolower)%>%
  rename(map_id=fid)%>%
  arrange(database_line)
 MP02<-fid_entry_master_3_7_13
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%database_map_link.data%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(readr)
library(haven) 
library(plyr)
database_7Mar13 <- read_csv("FAD/database_7Mar13.csv")%>%
  rename_all(tolower)
MP03<-database_7Mar13
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

database1869_71_19Jan16 <- read_dta("C:/Users/K3rgo/OneDrive/Desktop/New folder (2)/Urban/New folder/cleaning process/input186971/database1869-71_19Jan16.dta")
MP04<-database1869_71_19Jan16
MP05<-rbind.fill(MP04,MP03)%>%
  rename_all(tolower) %>%
  mutate(book=ifelse(book=="A", 1,book))%>%
  mutate(book=ifelse(book=="-", 1,book))%>%
  mutate(book=as.character(book))%>%
  mutate(line=tolower(line))%>%
  mutate(total=ifelse(line=="t",1,0))%>%
  mutate(line = str_remove_all(line," "))%>%
  mutate(page = str_replace_all(page," _"," - "))
 
  
df_01 <- right_join(MP05, MP02,by = c("database_line"))
 

df_01%>%
  filter(!((total=="1" & line==" .") & year%in%c("1869","1871") & merge=="1" & n=="2") )%>%
  filter(!(database_line%in%c("100770","100775") & year%in%c("1869","1871")) )%>%
  arrange( year, ward, book, page,line)%>%
#by year ward book page line:  gen n = [_N]
  select(-n, -merge)%>%
  filter(!(ward=="5" & book=="2" & year=="1867" & page=="200" & line>="2") )%>%
  filter(!(ward=="5" & book=="2" & year=="1867" & page>="201" & page<="204") )%>%
  filter(!(ward=="12" & book=="2" & year=="1882" & page>="417" & page<="421") )%>%
  filter(!(ward=="7" & book=="1" & year=="1894" & page=="176") )%>%
  filter(!(database_line >= "22568" & database_line <= "22573" & year%in%c("1869","1871")) )%>%
  filter(!(database_line >= "92520" & database_line <= "92523" & year%in%c("1869","1871")) )%>%
  filter(!( database_line >= "94816" & database_line <= "94823" & year%in%c("1869","1871")) )%>%
  filter(!(database_line >= "92524" & database_line <= "92526" & year%in%c("1869","1871")) )%>%
  filter(!(database_line >= "92524" & database_line <= "92526" & year%in%c("1869","1871")) )%>%
  filter(!((map_id== " ." & total=="0") & year%in%c("1869","1871")) )%>%
  arrange(year, ward, book, page, line, database_line)
  

 
  mutate(map_id = ifelse( map_id == na | year == 1869| year == 1871, database_line_6971,map_id ))
  #gen temp_plot_unit = real(plotsize) ~= .
  #bys year map_id : egen has_plot_unit = max(temp_plot_unit)
  
#Total 
mutate(exempt = "")
mutate(total_value = tolower(total_value))
mutate(exempt = ifelse(total_value == "exempt" ,"Total",exempt ))
mutate(value_total = as.numeric(total_value))
select(-total_value)


#Land
mutate(value_land = tolowe(value_land))
#replace exempt = exempt + "Land " if value_land == "exempt"
mutate(value_land_num = as.numeric(value_land))
#tab value_land if value_land_num==.
select(-value_land)
rename(value_land = value_land_num)
  


#Building
mutate(value_building = tolower(value_building))
mutate(exempt=ifelse(value_building == "exempt",(exempt + "Building "),0 ))
mutate(value_building_num = as.numeric(value_building))
#tab value_building if value_building_num==.
select(-value_building)
rename(value_building=value_building_num)

muatate(cityandchurch=0)
#replace cityandchurch=1 if regexm(trim(lower(owner)), "city of boston") 
#replace cityandchurch=1 if regexm(trim(lower(owner)), "church") 
#replace cityandchurch=1 if regexm(trim(lower(owner)), "chapel") 
#replace cityandchurch=1 if regexm(trim(lower(owner)), "school") 
#replace cityandchurch=1 if regexm(trim(lower(owner)), "christian society") 
#replace cityandchurch=1 if owner == "United States" | owner == "United States of America" // the post office and customs house
#replace cityandchurch=1 if owner == "Commonwealth of Massachusetts" // the post office
filter(!(cityandchurch=="1" ) )
 select(-cityandchurch)








X1871_Point_File_Database$book <-as.numeric(X1871_Point_File_Database$book)
X1871_Point_File_Database$page <-as.numeric(X1871_Point_File_Database$page)
X1871_Point_File_Database$line <-as.numeric(X1871_Point_File_Database$line)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

