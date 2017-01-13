x<-c("dplyr","ggplot2","data.table")
lapply(x, require, character.only=T)

drive=c("C:\\Users\\kebisu\\Documents\\Research\\DroughtSpecies\\Data")
setwd(drive)

#load('DroughtDataTrimmed.Rdata') #combinedDrought.trimmed
#length(unique(combinedDrought.trimmed$GEOID))
load('PM25SpeciesData.RData') #df6
df6=rename(df6,Date=Date.Local)
df=df6

########################
## Take average by county level
########################
df=mutate(df,FIPS2=substr(FIPS,1,5))
listvar=c('sum','diff','diff_percent','FIPS')
df2=select(df,-one_of(listvar)) %>%
	group_by(FIPS2,Date) %>%   
	summarise_each(funs(mean(., na.rm = TRUE))) %>%
	data.frame()

spring=c('03','04','05')
summer=c('06','07','08')
fall=c('09','10','11')
df2$Season=ifelse(substr(df2$Date,6,7) %in% spring,'2Spring',ifelse(substr(df2$Date,6,7) %in% summer,'3Summer',ifelse(substr(df2$Date,6,7) %in% fall,'4Fall','1Winter')))
df2$n=sequence(rle(df2$FIPS2)$lengths)
First_Date=data.frame(df2[!duplicated(df2$FIPS2),c('FIPS2','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(df2[!duplicated(df2$FIPS2,fromLast=TRUE),c('FIPS2','Date','n')])
names(Last_Date)[2]='LastObsDate'
Obs_List=merge(First_Date,Last_Date,by='FIPS2') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n)
Obs_List$Ind=0
#i=189
for (i in 1:dim(Obs_List)[1]){
	df_temp=filter(df2,FIPS2==Obs_List$FIPS2[i]) %>%
		mutate(Year_Season=paste(substr(Date,1,4),'_',Season,sep=''))
	temp2=table(df_temp$Year_Season) %>%
		data.frame() %>%
		filter(Freq>5)
	Obs_List$Ind[i]=ifelse(dim(temp2)[1]>=20,1,0)
}

table(Obs_List$Ind)
filter(Obs_List,Freq<7,Ind==0)

Obs_List2=filter(Obs_List,Ind==1,PeriodLength>1825,Freq<=15)
dim(Obs_List2)

FIPS_List=select(Obs_List2,FIPS2,FirstObsDate,LastObsDate,n,PeriodLength,Freq) %>%
	rename(FIPS=FIPS2)

save(FIPS_List,file='FIPS_List.RData')

#### Link to Species Data
load("PM10_Data_20160928.RData") #PM10_AQS2
PM10_df=mutate(PM10_AQS2,FIPS2=substr(FIPS,1,5)) %>%
	group_by(FIPS2,Date) %>%
	summarise(PM10_total=mean(PM10_total,na.rm=TRUE)) %>%
	data.frame()

Species_DF=left_join(df2,PM10_df,by=c('FIPS2','Date')) %>%
	mutate(PMC_total=PM10_total-PM25_total)  %>%
	select(-Season,-n) %>%
	filter(FIPS2 %in% unique(FIPS_List$FIPS)) %>%
	rename(FIPS=FIPS2)

save(Species_DF,file='Species_DF.RData')

rm(list=ls())

