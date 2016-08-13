x<-c("dplyr","ggplot2","data.table")
lapply(x, require, character.only=T)

drive=c("K:\\AirData\\OriginalData")
setwd(drive)


############Download AQS Site
######################
PM25_AQS=data.frame(matrix(nrow=0,ncol=0))
PM25_Monitor=data.frame(matrix(nrow=0,ncol=0))
test2=c(1990:2016)

# Load Parameter List; Pick up species I want to DL.
## Note this list is created by downloading all year and take out parameter name.
## Code is not saved, so if need to update list, create code from scratch.

load('K:\\AirData\\OriginalData\\PM25Spec_List.RData') #PM25Spec_List
ex_list=c('Ambient Max Temperature','Ambient Min Temperature','Ambient Temperature',
	'EC1 PM2.5 LC','EC2 PM2.5 LC','Sample Baro Pressure','Sample Max Baro Pressure',
	'EC3 PM2.5 LC','EC CSN_Rev Unadjusted PM2.5 LC TOR','Sample Min Baro Pressure',
	'EC CSN_Rev Unadjusted PM2.5 LC TOT','EC1 CSN_Rev Unadjusted PM2.5 LC',
	'EC2 CSN_Rev Unadjusted PM2.5 LC','EC3 CSN_Rev Unadjusted PM2.5 LC',
	'OC CSN Unadjusted PM2.5 LC TOT','OC CSN_Rev Unadjusted PM2.5 LC TOR',
	'OC CSN_Rev Unadjusted PM2.5 LC TOT','OC1 CSN Unadjusted PM2.5 LC TOT',
	'OC1 CSN_Rev Unadjusted PM2.5 LC','OC1 PM2.5 LC','OC2 CSN Unadjusted PM2.5 LC TOT',
	'OC2 CSN_Rev Unadjusted PM2.5 LC','OC2 PM2.5 LC','OC3 CSN Unadjusted PM2.5 LC TOT',
	'OC3 CSN_Rev Unadjusted PM2.5 LC','OC3 PM2.5 LC','OC4 CSN Unadjusted PM2.5 LC TOT',
	'OC4 CSN_Rev Unadjusted PM2.5 LC','OC4 PM2.5 LC','OCX Carbon PM2.5 LC',
	'OP CSN PM2.5 LC TOT','OP CSN_Rev Unadjusted PM2.5 LC TOR','OP CSN_Rev Unadjusted PM2.5 LC TOT',
	'OP PM2.5 LC TOR','OP PM2.5 LC TOT','Optical EC PM2.5 LC TOT','UV Carbon PM2.5 LC',
	'Samarium PM2.5 LC','Rubidium PM2.5 LC','Phosphorus PM2.5 LC','Antimony PM2.5 LC','Beryllium PM2.5 LC',
	'Gallium PM2.5 LC','Indium PM2.5 LC','Iridium PM2.5 LC','Lanthanum PM2.5 LC','Molybdenum PM2.5 LC',
	'Niobium PM2.5 LC','Tantalum PM2.5 LC','Terbium PM2.5 LC','Tungsten PM2.5 LC','Yttrium PM2.5 LC',
	'Zirconium PM2.5 LC')
PM25Spec_List_filter=PM25Spec_List[! PM25Spec_List %in% ex_list]
Outside_main=c('02','15','66','72','78','80')

PM25_Spec_Data=data.frame()
ptm <- proc.time()
for (i in 1:length(test2)){  
	tryCatch({

	url=paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_SPEC_",test2[i],".zip",sep='')
	download.file(url,'temp2.zip')
	temp=read.csv(unz('temp2.zip',paste("daily_SPEC_",test2[i],".csv",sep='')),header=TRUE) %>%
		filter(State.Code!='CC') %>%
		mutate(State.Code=sprintf("%02d",as.numeric(as.character(State.Code)))) %>%
		mutate(FIPS=paste(sprintf("%02d",as.numeric(as.character(State.Code))),sprintf("%03d",as.numeric(as.character(County.Code))),sprintf("%04d",as.numeric(as.character(Site.Num))),sep=''),
		FIPS_POC=paste(FIPS,sprintf("%02d",as.numeric(as.character(POC))),sep='')) %>%
		filter(!State.Code %in% Outside_main, !Parameter.Name %in% ex_list) %>%
		mutate(Date=as.Date(as.character(Date.Local),format="%Y-%m-%d"),ParameterName=as.character(Parameter.Name)) %>%
		select(FIPS_POC,Date,Parameter.Code,ParameterName,Arithmetic.Mean)

	temp$FIPS_POC[substr(temp$FIPS_POC,1,5)=='12086']=paste('12025',substr(temp$FIPS_POC[substr(temp$FIPS_POC,1,5)=='12086'],6,11),sep='')
	temp=arrange(temp,ParameterName,FIPS_POC,Arithmetic.Mean) %>%
		mutate(willdelete=paste(ParameterName,FIPS_POC,sep=''))
	temp$Numbering=sequence(rle(c(temp$willdelete))$lengths)
	temp=select(temp,-willdelete)

	url2=paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/annual_all_",test2[i],".zip",sep='')
	download.file(url2,'temp2.zip')
	temp_mon=read.csv(unz('temp2.zip',paste("annual_all_",test2[i],".csv",sep='')),header=TRUE) %>%
		filter(State.Code!='CC') %>%
		mutate(State.Code=sprintf("%02d",as.numeric(as.character(State.Code)))) %>%
		mutate(FIPS=paste(sprintf("%02d",as.numeric(as.character(State.Code))),sprintf("%03d",as.numeric(as.character(County.Code))),sprintf("%04d",as.numeric(as.character(Site.Num))),sep=''),
		FIPS_POC=paste(FIPS,sprintf("%02d",as.numeric(as.character(POC))),sep='')) %>%
		filter(!State.Code %in% Outside_main, Parameter.Name %in% unique(temp$ParameterName)) %>%
		mutate(ParameterName=as.character(Parameter.Name),Numb_BelowMDL=Num.Obs.Below.MDL) %>%
		select(FIPS_POC,Parameter.Code,ParameterName,Numb_BelowMDL) %>%
		arrange(ParameterName,FIPS_POC)
	temp_mon$FIPS_POC[substr(temp_mon$FIPS_POC,1,5)=='12086']=paste('12025',substr(temp_mon$FIPS_POC[substr(temp_mon$FIPS_POC,1,5)=='12086'],6,11),sep='')

	temp2=left_join(temp,temp_mon,by=c('FIPS_POC','ParameterName')) %>%
		rename(ParameterCode=Parameter.Code.x) %>%
		select(-Parameter.Code.y)

	temp3=filter(temp2,Numbering==Numb_BelowMDL) %>%
		rename(estMDL_value=Arithmetic.Mean) %>%
		select(FIPS_POC,ParameterName,estMDL_value)

	temp4=left_join(temp2,temp3,by=c('FIPS_POC','ParameterName')) %>%
		mutate(MDL_Flag=ifelse(Arithmetic.Mean>estMDL_value|Numb_BelowMDL==0,0,1)) 
	temp4$MDL_Flag[is.na(temp4$MDL_Flag)]=0

	temp4=select(temp4,FIPS_POC,Date,ParameterCode,ParameterName,Arithmetic.Mean,MDL_Flag) %>%
		rename(Value=Arithmetic.Mean) %>%
		arrange(FIPS_POC,ParameterName,Date)

	PM25_Spec_Data=rbind(PM25_Spec_Data,temp4)
	rm(temp,temp2,temp3,temp4,temp_mon)

	}, error=function(e){})
}
proc.time() - ptm #This takes about 40min


save(PM25_Spec_Data,file="PM25_Species_Data_20160212.RData") #PM25_Spec_Data 

rm(list=ls())
