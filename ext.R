library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(here)

source('C:/Users/vance/Documents/myR/functions/getSQL.r')


query1<- "SELECT  a.[spg_id]
      ,[idx_name]
      ,[idx_date]
      ,[idx]
	  ,[vendor_visual]
	  ,[vendor_breed_code]
  FROM [OADB].[dbo].[idxHistory] a
  INNER JOIN [OADB].[dbo].[Boars] b on a.spg_id=b.spg_id"
ext1<-getSQL('Intranet',query=query1)

query2<- "SELECT a.[StudID]
	  ,a.[Date_Shipped]
      ,a.[BatchNum]
	  ,b.[BoarID]
  FROM [Intranet].[dbo].[Boar_Distrib] a
  INNER JOIN [Intranet].[dbo].[Boar_Split] b on a.[BatchNum]=b.[BatchNum] and a.StudID=b.StudID
  WHERE [Dest]!='* TRASH *'
  AND [Date_Shipped]>'2021-06-01'
  AND [Breed] in ('PICL02','PICL03','DNA200','DNA400')"
ext2<-getSQL('Intranet',query=query2)

query3<-"SELECT a.[BoarID]
      ,a.[Breed]
	  ,b.[idx]
  FROM [Intranet].[dbo].[Boar_Pig] a
  inner join [OADB].[reports].[idxCurrent] b on a.[Name] = b.[spg_id]
  WHERE [Breed] in ('PICL02', 'PICL03','DNA200','DNA400')
  and [Status] = 'WORKING'"

ext3<-getSQL('Intranet', query = query3)

ext4<-ext1 %>% 
  mutate(idx_date=as.Date(idx_date),
         idx_newdate=idx_date+3,
         idx_week=floor_date(x = idx_newdate,unit = "week",week_start = 1),
         code=paste(vendor_visual,idx_week),
         std_idx=ifelse(vendor_breed_code%in%c('PICL02','PICL03'),(idx-100)/20,
                        ifelse(vendor_breed_code%in%c('DNA200','DNA400'),(idx-100)/12,0)))

ext4<-ext4[!duplicated(ext4$code),]

ext5<-ext4 %>% 
  filter(idx_week>=floor_date(x = today(),unit = "week",week_start = 1)-91,
         vendor_breed_code%in%c('PICL02', 'PICL03','DNA200','DNA400')) %>% 
  mutate(vendor_visual=as.character(paste(vendor_visual)))

ggplot(data = ext5, aes(x = idx_week,y = std_idx, colour=vendor_visual))+
  geom_line()+ theme(legend.position = "none")

ext6<-ext5 %>% 
  group_by(vendor_breed_code, idx_week) %>% 
  summarise(breed_idx=mean(std_idx))

ggplot(data = ext6, aes(x = idx_week,y = breed_idx, colour=vendor_breed_code))+
  geom_line()+ theme(legend.position = "right")

ext7<-ext2 %>% 
  mutate(ship_week=floor_date(x = Date_Shipped,unit = "week",week_start = 1),
         code=paste(BoarID,ship_week))

ext7<-ext7[!duplicated(ext7$code),]

ext8<-left_join(x = ext7,y = ext5,by=c("BoarID"="vendor_visual","ship_week"="idx_week"))

ext9<-ext8 %>% 
  filter(!is.na(std_idx)) %>% 
  group_by(vendor_breed_code,ship_week) %>% 
  summarise(breed_idx=mean(std_idx))

ggplot(data = ext9, aes(x = ship_week,y = breed_idx, colour=vendor_breed_code))+
  geom_line()+ theme(legend.position = "right")

ext10<-ext3 %>% 
  mutate(std_idx=ifelse(Breed%in%c('PICL02','PICL03'),(idx-100)/20,
                        ifelse(Breed%in%c('DNA200','DNA400'),(idx-100)/12,0)))

ext11<-ext10 %>% 
  group_by(Breed) %>% 
  summarise('Mean Std Index'=mean(std_idx))
