install.packages("stargazer")
rm(list=ls())
library(tidyverse)
library(lmtest)
library(sandwich)
library(haven)
library(ggplot2)
library(stargazer)
FireWorking <- read_dta("FAD/FireWorking.dta")
rdf1<-FireWorking%>%
  rename_all(tolower)%>%
  mutate(year_1872=year )%>%
  mutate(year_1872=1872)%>%
  mutate(burned_1872 = year_1872*burned)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg01 <- lm(lnvalue_land_ft  ~    burned_1867  + burned_1869+ burned_1871+year_1869+ year_1871 +year_1872
            
            , data=rdf1)  
summary(reg01)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg02 <- lm(lnvalue_land_plot  ~   burned_1869  + burned_1871+ burned_1872+ year_1871 +year_1872
            +    lnland_ft_n_1867_1869   + lnland_ft_blk_1867_1869  +  lnland_ft_n_1867_1871 
            + lnland_ft_blk_1867_1871   +   lnland_ft_n_1867_1872  + lnland_ft_blk_1867_1872 
            , data=rdf1)  
summary(reg02)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg03 <- lm(lnvalue_land_ft  ~   burned_1871+ burned_1872 +year_1872
            +     lnland_ft_n_1869_1872    +  lnland_ft_n_1867_1872  +  lnland_ft_blk_1869_1872
            + lnland_ft_blk_1867_1872    +    lnland_ft_n_1869_1871  +  lnland_ft_n_1867_1871 
            +lnland_ft_blk_1869_1871+lnland_ft_blk_1867_1871 , data=rdf1)  
summary(reg03)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg04 <- lm(lnvalue_land_ft  ~ burned_1872 +
            +      lnland_ft_n_1871_1872    +  lnland_ft_n_1869_1872   +   lnland_ft_n_1867_1872 
            + lnland_ft_blk_1871_1872    +    lnland_ft_blk_1869_1872   +  lnland_ft_blk_1867_1872  
             , data=rdf1)  
summary(reg04)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg05 <- lm(lnvalue_land_ft  ~  burned_1867+ burned_1869 + burned_1871+burned_1872 +
              +      year_1869    +   year_1871   +    year_1872  
         
            , data=rdf1)  
summary(reg05)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg06 <- lm(lnvalue_land_ft  ~  burned_1869+ burned_1871+burned_1872 + year_1871+ year_1872
              + lnland_ft_n_1867_1869 +  lnland_ft_blk_1867_1869   +   lnland_ft_n_1867_1871 
            + lnland_ft_blk_1867_1871     +     lnland_ft_n_1867_1872   +  lnland_ft_blk_1867_1872  
            , data=rdf1)  
summary(reg06)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg07 <- lm(lnvalue_land_ft  ~  burned_1871+burned_1872 + year_1871+ year_1872
            +  lnland_ft_n_1869_1872  +   lnland_ft_n_1867_1872    +   lnland_ft_blk_1869_1872 
            + lnland_ft_blk_1867_1872     +      lnland_ft_n_1869_1871   +   lnland_ft_n_1867_1871  
            + lnland_ft_blk_1869_1871+lnland_ft_blk_1867_1871, data=rdf1)  
summary(reg07)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg08 <- lm(lnvalue_land_ft  ~  burned_1871+burned_1872 
            +  lnland_ft_n_1871_1872   +    lnland_ft_n_1869_1872     +   lnland_ft_n_1867_1872  
            + lnland_ft_blk_1871_1872     +      lnland_ft_blk_1869_1872  +   lnland_ft_blk_1867_1872   
            , data=rdf1)  
summary(reg08)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg09 <- lm(lnvalue_land_ft  ~  burned_1867+ burned_1869 +burned_1871+burned_1872 
            +     year_1869+ year_1871+ year_1872
            , data=rdf1)  
summary(reg09)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg10 <- lm(lnvalue_land_ft  ~   burned_1869 +burned_1871+burned_1872 
            +  year_1871+ year_1872+lnland_ft_n_1867_1869+lnland_ft_blk_1867_1869+ lnland_ft_n_1867_1871
            +lnland_ft_blk_1867_1871+  lnland_ft_n_1867_1872+ lnland_ft_blk_1867_1872, data=rdf1)  
summary(reg10)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg11 <- lm(lnvalue_land_ft  ~ burned_1871+burned_1872 
            +  year_1872+lnland_ft_n_1869_1872+lnland_ft_n_1867_1872 + lnland_ft_blk_1869_1872 
            +lnland_ft_blk_1867_1872+   lnland_ft_n_1869_1871+  lnland_ft_n_1867_1871+lnland_ft_blk_1869_1871+lnland_ft_blk_1867_1871, data=rdf1)  
summary(reg11)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg12 <- lm(lnvalue_land_ft  ~ burned_1872 
            +   lnland_ft_n_1871_1872+ lnland_ft_n_1869_1872+ lnland_ft_n_1867_1872 + lnland_ft_blk_1871_1872
            +lnland_ft_blk_1869_1872+   lnland_ft_blk_1867_1872, data=rdf1)  
summary(reg12)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg13 <- lm(lnvalue_building_ft  ~  burned_1867+ burned_1869 +burned_1871+burned_1872 
            +     year_1869+ year_1871+ year_1872
            , data=rdf1)  
summary(reg13)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg14 <- lm(lnvalue_building_ft  ~  burned_1869 +burned_1871+burned_1872 + year_1871
            +  year_1872+ lnbuilding_ft_n_1867_1869+ lnbuilding_ft_blk_1867_1869  +  lnbuilding_ft_n_1867_1871
            +lnbuilding_ft_blk_1867_1871 +     lnbuilding_ft_n_1867_1872+   lnbuilding_ft_blk_1867_1872  , data=rdf1)  
summry(reg14)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg15 <- lm(lnvalue_building_ft  ~ burned_1871+burned_1872 
            +  year_1872+lnbuilding_ft_n_1869_1872 + lnbuilding_ft_n_1867_1872 + lnbuilding_ft_blk_1869_1872 
            +lnbuilding_ft_blk_1867_1872 +    lnbuilding_ft_n_1869_1871+   lnbuilding_ft_n_1867_1871 +lnbuilding_ft_blk_1869_1871+lnbuilding_ft_blk_1867_1871 , data=rdf1)  
summary(reg15)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg16 <- lm(lnvalue_building_ft  ~ burned_1872 
            +    lnbuilding_ft_n_1871_1872+ lnbuilding_ft_n_1869_1872+  lnbuilding_ft_n_1867_1872 + lnbuilding_ft_blk_1871_1872
            +lnbuilding_ft_blk_1869_1872 +   lnbuilding_ft_blk_1867_1872 , data=rdf1)  
summary(reg16)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg017 <- lm(lnvalue_building_ft  ~  burned_1867 +burned_1869 +burned_1871+   burned_1872
             +   year_1869  +  year_1871   + year_1872 
             
             , data=rdf1)  
summary(reg17)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg018 <- lm(lnvalue_building_ft  ~  burned_1869 +burned_1871+   burned_1872
              +  year_1871   + year_1872 + lnbuilding_ft_n_1867_1869 +lnbuilding_ft_blk_1867_1869
             + lnbuilding_ft_n_1867_1871+lnbuilding_ft_blk_1867_1871 + lnbuilding_ft_n_1867_1872
             +lnbuilding_ft_blk_1867_1872 , data=rdf1)  
summary(reg18)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg019 <- lm(lnvalue_building_ft  ~  burned_1871+   burned_1872
             +   year_1872 +  lnbuilding_ft_n_1869_1872 + lnbuilding_ft_n_1867_1872 
             + lnbuilding_ft_blk_1869_1872+lnbuilding_ft_blk_1867_1872 + lnbuilding_ft_n_1869_1871
             + lnbuilding_ft_n_1867_1871+lnbuilding_ft_blk_1869_1871+lnbuilding_ft_blk_1867_1871 , data=rdf1)  
summary(reg19)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg020 <- lm(lnvalue_building_ft  ~   burned_1872
            +   lnbuilding_ft_n_1871_1872  + lnbuilding_ft_n_1869_1872  + lnbuilding_ft_n_1867_1872 
            + lnbuilding_ft_blk_1871_1872  +  lnbuilding_ft_blk_1869_1872  + lnbuilding_ft_blk_1867_1872
            , data=rdf1)  
summary(reg20)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg21 <- lm(lnvalue_building_ft  ~ burned_1867 +burned_1869+  burned_1871 +  burned_1872+ year_1869+year_1871 +year_1872, data=rdf1)  
summary(reg21)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg22 <- lm(lnvalue_building_ft  ~   burned_1869+ burned_1871 + burned_1872+ year_1871 +year_1872
            +  lnbuilding_ft_n_1867_1869  +lnbuilding_ft_blk_1867_1869  +lnbuilding_ft_n_1867_1871
            + lnbuilding_ft_blk_1867_1871  +  lnbuilding_ft_n_1867_1872 + lnbuilding_ft_blk_1867_1872
            , data=rdf1)  
summary(reg22)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg23 <- lm(lnvalue_building_ft  ~   burned_1871 
            + burned_1872 + year_1872 
            + lnbuilding_ft_n_1869_1872  + lnbuilding_ft_n_1867_1872 +lnbuilding_ft_blk_1869_1872 
            +lnbuilding_ft_blk_1867_1872 + lnbuilding_ft_n_1869_1871 +lnbuilding_ft_n_1867_1871+lnbuilding_ft_blk_1869_1871+lnbuilding_ft_blk_1867_1871, data=rdf1)  
summary(reg23)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg24 <- lm(lnvalue_building_ft  ~ burned_1872 
            +lnbuilding_ft_n_1871_1872+lnbuilding_ft_n_1869_1872
            +lnbuilding_ft_n_1867_1872 +lnbuilding_ft_blk_1871_1872 +lnbuilding_ft_blk_1869_1872 
            +lnbuilding_ft_blk_1867_1872, data=rdf1)
summary(reg24) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
reg0 <- lm( rdf1$lnvalue_land_ft ~ (rdf1$dist_rd <= 0)+rdf1$lnland_ft_blk_1872_1873
            +rdf1$lnland_ft_blk_1869_1873+rdf1$lnland_ft_blk_1869_1873
            +rdf1$lnland_ft_blk_1867_1873 +rdf1$lnland_ft_n_1872_1873 +rdf1$lnland_ft_n_1871_1873
            +rdf1$lnland_ft_n_1869_1873+rdf1$lnland_ft_n_1867_1873)
summary(reg0)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
stargazer(reg0, type = "text", title = "Table 1: Regression Results",dep.var.labels=c("Total Land Value"), digits=1, out="table.text")
reg1 <- lm( rdf1$value_total ~ rdf1$dist_rd+rdf1$dist_to_cbd_1869+rdf1$distance)
reg2 <- lm( rdf1$value_total ~ rdf1$dist_to_cbd_1871+rdf1$dist_to_cbd_1873)
reg3 <- lm( rdf1$value_total ~ rdf1$dist_to_cbd+rdf1$distance+rdf1$dist_to_cbd_1873)
stargazer(reg1,reg2, reg3,align=TRUE, type = "text", title = "Table 1: Regression Results",dep.var.labels=c("Total House Value"), digits=1, out="table.text")


