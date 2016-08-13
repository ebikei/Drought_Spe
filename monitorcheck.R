x<-c("dplyr","ggplot2","data.table","reshape2")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)


load("PM25_Species_Data_20160212.RData") #PM25_Spec_Data 

unique(PM25_Spec_Data$ParameterName)

DF1=mutate(PM25_Spec_Data,FIPS=substr(FIPS_POC,1,9)) %>%
	select(-ParameterCode,-MDL_Flag) %>%
	group_by(FIPS,Date,ParameterName) %>%
	summarize(Value2=mean(Value,na.rm=TRUE))

DF2=filter(DF1,ParameterName!='EC CSN PM2.5 LC TOT',ParameterName!='UV Carbon PM2.5 at 370 nm')
DF3=dcast(DF2,FIPS+Date~ParameterName,value.var="Value2")

## Combine PM2.5 total mass value here ### 
DF3=arrange(DF3,FIPS,Date)
DF3$n=sequence(rle(DF3$FIPS)$lengths)
First_Date=data.frame(DF3[!duplicated(DF3$FIPS),c('FIPS','Date')])
names(First_Date)[2]='FirstObsDate'
Last_Date=data.frame(DF3[!duplicated(DF3$FIPS,fromLast=TRUE),c('FIPS','Date','n')])
names(Last_Date)[2]='LastObsDate'

Obs_List=merge(First_Date,Last_Date,by='FIPS') %>%
		mutate(PeriodLength=as.numeric(LastObsDate-FirstObsDate),Freq=PeriodLength/n) %>%
		filter(FirstObsDate<'2002-01-01',LastObsDate>'2012-12-31',PeriodLength>4000,Freq<4)		

write.csv(Obs_List,file="C:\\Users\\kebisu\\Documents\\Research\\DroughtSpecies\\Data\\ObsList.csv")

