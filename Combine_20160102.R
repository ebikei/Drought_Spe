x<-c("dplyr","ggplot2","data.table")
lapply(x, require, character.only=T)

#drive=c("C:\\Users\\kebisu\\Downloads")
drive=c('F:\\GDrive\\Research\\Drought_Species\\Data')
setwd(drive)

#load('C:\\Users\\kebisu\\Documents\\Research\\DroughtSpecies\\Data\\PM25SpeciesData.RData')#df6
#load("C:\\Users\\kebisu\\Documents\\Research\\DroughtSpecies\\Data\\PM10_Data_20160928.RData") #PM10_AQS2
load('PM25SpeciesData.RData')#df6
load("PM10_Data_20160928.RData") #PM10_AQS2

df6=rename(df6,Date=Date.Local)

df=left_join(df6,PM10_AQS2,by=c('FIPS','Date')) %>%
	mutate(PMC_total=PM10_total-PM25_total) %>%
	select(-sum,-diff,-diff_percent) %>%
	mutate(FIPS_County=substr(FIPS,1,5))

### Check Frequency of the monitor
df$n=sequence(rle(df$FIPS)$lengths)
First_Date=data.frame(df[!duplicated(df$FIPS),c('FIPS','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(df[!duplicated(df$FIPS,fromLast=TRUE),c('FIPS','Date','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='FIPS') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
		#filter(Freq<8,PeriodLength>2880)
		filter(Freq<8,PeriodLength>1440)

df2=filter(df,FIPS %in% Obs_List$FIPS) %>%
	select(-FIPS,-n) %>%
	group_by(FIPS_County,Date) %>%
	summarise_each(funs(mean(., na.rm = TRUE))) %>%
	data.frame()

PM25SpecData=df2
save(PM25SpecData,file='CleanedSpecies_20170101.RData')

### Check Frequency of the monitor
df2$n=sequence(rle(df2$FIPS)$lengths)
First_Date=data.frame(df2[!duplicated(df2$FIPS),c('FIPS_County','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(df2[!duplicated(df2$FIPS,fromLast=TRUE),c('FIPS_County','Date','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='FIPS_County') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
		filter(Freq<8,PeriodLength>1440)
save(Obs_List,file='SpiciesMonitor_20170101.RData')

#####################################################
#####################################################

load('DroughtDataTrimmed.Rdata') #combinedDrought.trimmed
Drought_df=rename(combinedDrought.trimmed,FIPS_County=GEOID,Date=Date..Local.) %>%
	arrange(FIPS_County,Date)
Drought_df$FIPS_County=as.character(Drought_df$FIPS_County)
Drought_df$Date=as.Date(Drought_df$Date,format="%Y-%m-%d")

##04013 (Maricopa AZ), 53033 (King WA), 31055 (Cass, ND)  and 06065 (Riverside, CA)
List=c('04013','53033','06065','31055')
df7=filter(df2,FIPS_County %in% List) %>%
	merge(Drought_df,by=c('FIPS_County','Date')) %>%
	arrange(FIPS_County,Date)
df7$season2=ifelse(df7$season=='spring'|df7$season=='summer','Warm','Cool')
df7$DM.USDM=ordered(df7$DM.USDM,levels=c('No Drought','Abnormally Dry','Moderate Drought','Severe Drought','Extreme Drought'))

df8=group_by(df7,FIPS_County,season2,DM.USDM) %>%
	summarize(n=n(),PM25=mean(PM25_total,na.rm=TRUE),PM10=mean(PM10_total,na.rm=TRUE),
		PMC=mean(PMC_total,na.rm=TRUE),Ammonium_Ion=mean(Ammonium_Ion,na.rm=TRUE),
		EC=mean(EC,na.rm=TRUE),OC=mean(OC,na.rm=TRUE),
		Total_Nitrate=mean(Total_Nitrate,na.rm=TRUE),Sodium_Ion=mean(Sodium_Ion,na.rm=TRUE),
		Sulfate=mean(Sulfate,na.rm=TRUE),Sulfur=mean(Sulfur,na.rm=TRUE)) %>%
	arrange(FIPS_County,season2,DM.USDM) %>%
	data.frame()

write.csv(df8,file='C:\\Users\\kebisu\\Downloads\\result.csv')


df7=merge(df2,Drought_df,by=c('FIPS_County','Date')) %>%
	arrange(FIPS_County,Date)

length(unique(df2$FIPS_County))
length(unique(Drought_df$FIPS_County))
length(unique(df7$FIPS_County))

df7$season2=ifelse(df7$season=='spring'|df7$season=='summer','Warm','Cool')
df7$DM.USDM=ordered(df7$DM.USDM,levels=c('No Drought','Abnormally Dry','Moderate Drought','Severe Drought','Extreme Drought'))
df8=group_by(df7,FIPS_County,season2,DM.USDM) %>%
	summarize(n=n(),PM25=mean(PM25_total,na.rm=TRUE),PM10=mean(PM10_total,na.rm=TRUE),
		PMC=mean(PMC_total,na.rm=TRUE),Ammonium_Ion=mean(Ammonium_Ion,na.rm=TRUE),
		EC=mean(EC,na.rm=TRUE),OC=mean(OC,na.rm=TRUE),
		Total_Nitrate=mean(Total_Nitrate,na.rm=TRUE),Sodium_Ion=mean(Sodium_Ion,na.rm=TRUE),
		Sulfate=mean(Sulfate,na.rm=TRUE),Sulfur=mean(Sulfur,na.rm=TRUE)) %>%
	arrange(FIPS_County,season2,DM.USDM) %>%
	data.frame()
