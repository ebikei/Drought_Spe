x<-c("dplyr","ggplot2","data.table","stringr")
lapply(x, require, character.only=T)

drive=c("C:\\Users\\kebisu\\Documents\\Research\\DroughtSpecies\\Data")
setwd(drive)

#load('K:\\AirData\\OriginalData\\PM25Spec_List.RData') #PM25Spec_List
spec.list=read.csv('K:\\AirData\\OriginalData\\parameters.csv',header=TRUE) 


test=filter(spec.list,substr(Parameter.Code,1,2)=='88'|Parameter.Code=='81102') %>%
	arrange(Parameter.Code) %>%
	filter(Parameter!='Bismuth')
test2=c(1990:2016)
output=data.frame()
for (i in 1:length(test2)){
	url=paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/annual_all_",test2[i],".zip",sep='')
	download.file(url,'C:\\Users\\kebisu\\Downloads\\temp2.zip')
	temp=read.csv(unz('C:\\Users\\kebisu\\Downloads\\temp2.zip',paste("annual_all_",test2[i],".csv",sep='')),header=TRUE) 
	temp2=filter(temp,Parameter.Code %in% test$Parameter.Code)
	temp2$Year=test2[i]
	output=rbind(output,temp2)
	rm(url,temp,temp2)
}

output$State.Code=str_sub(paste('0',output$State.Code,sep=''),-2,-1)
output$County.Code=str_sub(paste('00',output$County.Code,sep=''),-3,-1)
output$Site.Num=str_sub(paste('000',output$Site.Num,sep=''),-4,-1)
output$POC=str_sub(paste('0',output$POC,sep=''),-2,-1)

Outside_main=c('02','15','66','72','78','80','CC')
output2=filter(output,!(State.Code %in% Outside_main))
output2$Parameter.Name=as.character(output2$Parameter.Name)
output2$Method.Name=as.character(output2$Method.Name)

sort(table(output2$Parameter.Name))
item=c('PM2.5 - Local Conditions','Zinc PM2.5 LC','Sulfate PM2.5 LC','Sulfur PM2.5 LC','Silicon PM2.5 LC',
	'Aluminum PM2.5 LC','Sodium PM2.5 LC','Total Nitrate PM2.5 LC','Ammonium Ion PM2.5 LC',
	'Sodium Ion Pm2.5 LC','Chlorine PM2.5 LC','Calcium PM2.5 LC','EC PM2.5 LC TOR','OC4 PM2.5 LC','OC3 PM2.5 LC',
	'OC2 PM2.5 LC','OC1 PM2.5 LC')
i=17
check=filter(output2,Parameter.Name %in% item[i])
table(check$Method.Name,check$Year)
check2=filter(output2,Parameter.Name %in% item[i],State.Code=='06')
table(check2$Method.Name,check2$Year)

## After checking this year & method table distirbution, I decided
## only include method whose proportion is more than 10% in each trimester
## 2001-2005,2006-2010,2011-2015
## This will take account for method trend over years
## Only method which was constantly used over 2001-2015 will be used.
## Data were removed before 2001 and after 2015
## Code is following

output3=filter(output2,Year>2000,Year<2016,Method.Name!='') %>%
	mutate(FIPSPOC=paste(State.Code,County.Code,Site.Num,POC,sep='')) %>%
	arrange(FIPSPOC,Year)
list=unique(output3$Parameter.Name)
SpeciesMethod=data.frame()

ptm <- proc.time()
for (i in 1:length(list)){
	tryCatch({
	temp_first=filter(output3,Parameter.Name==list[i],Year %in% c(2001:2005))
	First=sort(prop.table(table(temp_first$Method.Name))) %>%
		data.frame() %>%
		filter(Freq>0.1) %>%
		select(Var1)
	First$Var1=as.character(First$Var1)

	temp_second=filter(output3,Parameter.Name==list[i],Year %in% c(2006:2010))
	Second=sort(prop.table(table(temp_second$Method.Name))) %>%
		data.frame() %>%
		filter(Freq>0.1) %>%
		select(Var1)
	Second$Var1=as.character(Second$Var1)

	temp_third=filter(output3,Parameter.Name==list[i],Year %in% c(2011:2015))
	Third=sort(prop.table(table(temp_third$Method.Name))) %>%
		data.frame() %>%
		filter(Freq>0.1) %>%
		select(Var1)
	Third$Var1=as.character(Third$Var1)

	all=inner_join(First,Second,by='Var1') %>%
		inner_join(Third,by='Var1')

	temp=filter(output3,Parameter.Name==list[i],Method.Name %in% all$Var1) %>%
		select(FIPSPOC,Parameter.Code,Parameter.Name,Method.Name,Latitude,Longitude,Year) %>%
		distinct(FIPSPOC,Parameter.Code,Parameter.Name,Method.Name,Latitude,Longitude)
	SpeciesMethod=rbind(SpeciesMethod,temp)
	rm(temp_first,First,temp_second,Second,temp_third,Third,all,temp)
	}, error=function(e){})
}
proc.time() - ptm 

save(SpeciesMethod,file='SpeciesMethod.RData')
