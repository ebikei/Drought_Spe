x<-c("dplyr","ggplot2","data.table","stringr")
lapply(x, require, character.only=T)

drive=c("C:\\Users\\kebisu\\Documents\\Research\\DroughtSpecies\\Data")
setwd(drive)

load('PM25SpeciesData.RData') #df6

### Check Frequency of the monitor
df6$n=sequence(rle(df6$FIPS)$lengths)
First_Date=data.frame(df6[!duplicated(df6$FIPS),c('FIPS','Date.Local')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(df6[!duplicated(df6$FIPS,fromLast=TRUE),c('FIPS','Date.Local','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='FIPS') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
		filter(Freq<8,PeriodLength>2000)



## load Drought data from Jesse

load('DroughtDataTrimmed.Rdata') #combinedDrought.trimmed
Drought_FIPS=unique(combinedDrought.trimmed$GEOID)


Obs_Drought_List=mutate(Obs_List,FIPS_County=substr(FIPS,1,5)) %>%
	filter(FIPS_County %in% Drought_FIPS) %>%
	arrange(Freq)
Obs_Drought_List

## After checking, I decided to use 040139997 (Maricopa AZ), 530330080 (King WA), 310550019 (Cass, ND)  and 060658001(Riverside, CA)
## for prelininary analysis
Drought_df=rename(combinedDrought.trimmed,FIPS_County=GEOID,Date.Local=Date..Local.) %>%
	arrange(FIPS_County,Date.Local)
Drought_df$FIPS_County=as.character(Drought_df$FIPS_County)
Drought_df$Date.Local=as.Date(Drought_df$Date.Local,format="%Y-%m-%d")

#List=c('040139997','530330080','060658001','310550019','401431127')
List=Obs_Drought_List$FIPS
df7=filter(df6,FIPS %in% List) %>%
	select(-sum,-diff,-diff_percent,-n) %>%
	mutate(FIPS_County=substr(FIPS,1,5)) %>%
	merge(Drought_df,by=c('FIPS_County','Date.Local')) %>%
	arrange(FIPS_County,Date.Local)
df7$season2=ifelse(df7$season=='spring'|df7$season=='summer','Warm','Cool')

df8=group_by(df7,FIPS_County,season2,DM.USDM) %>%
	summarize(PM25=mean(PM25_total,na.rm=TRUE)) %>%
	arrange(FIPS_County,season2,-(PM25)) %>%
	data.frame()

df9=df8[!(duplicated(df8[c('FIPS_County','season2')])),]
df9=filter(df9,!(FIPS_County %in% c('06019','27053','38017','53033')))
 

table(df9$DM.USDM,df9$season2)
prop.table(table(df9$DM.USDM,df9$season2),2)



df8=group_by(df7,FIPS_County,season2,DM.USDM.update) %>%
	summarize(PM25=mean(PM25_total,na.rm=TRUE)) %>%
	arrange(FIPS_County,season2,-(PM25)) %>%
	data.frame()

df9=df8[!(duplicated(df8[c('FIPS_County','season2')])),]

table(df9$DM.USDM.update,df9$season2)
prop.table(table(df9$DM.USDM.update,df9$season2),2)

