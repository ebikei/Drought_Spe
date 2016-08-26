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
		filter(Freq<12,PeriodLength>3650)



## load Drought data from Jesse

load('DroughtDataTrimmed.Rdata') #combinedDrought.trimmed
Drought_FIPS=unique(combinedDrought.trimmed$GEOID)

Obs_Drought_List=mutate(Obs_List,FIPS_County=substr(FIPS,1,5)) %>%
	filter(FIPS_County %in% Drought_FIPS) %>%
	arrange(Freq)
Obs_Drought

## After checking, I decided to use 040139997 (Maricopa AZ), 530330080 (King WA), and 060658001(Riverside, CA)
## for prelininary analysis
Drought_df=rename(combinedDrought.trimmed,FIPS_County=GEOID,Date.Local=Date..Local.) %>%
	arrange(FIPS_County,Date.Local)
Drought_df$FIPS_County=as.character(Drought_df$FIPS_County)
Drought_df$Date.Local=as.Date(Drought_df$Date.Local,format="%Y-%m-%d")

List=c('040139997','530330080','060658001')
df7=filter(df6,FIPS %in% List) %>%
	select(-sum,-diff,-diff_percent,-n) %>%
	mutate(FIPS_County=substr(FIPS,1,5)) %>%
	merge(Drought_df,by=c('FIPS_County','Date.Local')) %>%
	arrange(FIPS_County,Date.Local)

save(df7,file='PreliminaryData.RData')

group_by(df7,FIPS_County,DM.USDM) %>%
	summarize(PM25=mean(PM25_total,na.rm=TRUE)) %>%
	arrange(FIPS_County,PM25)

group_by(df7,FIPS_County,DM.USDM) %>%
	summarize(Total_Nitrate=mean(Total_Nitrate,na.rm=TRUE)) %>%
	arrange(FIPS_County,Total_Nitrate)

group_by(df7,FIPS_County,DM.USDM) %>%
	summarize(OC=mean(OC,na.rm=TRUE)) %>%
	arrange(FIPS_County,OC)

tt=filter(combinedDrought.trimmed,GEOID=='53033')
table(tt$DM.USDM)
