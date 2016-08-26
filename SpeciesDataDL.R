x<-c("dplyr","ggplot2","data.table","stringr")
lapply(x, require, character.only=T)

drive=c("C:\\Users\\kebisu\\Documents\\Research\\DroughtSpecies\\Data")
setwd(drive)

load('SpeciesMethod.RData')#SpeciesMethod
SpeciesMethod=mutate(SpeciesMethod,FIPSPOC_Parameter=paste(FIPSPOC,'_',Parameter.Code,sep=''))

test=c(2001:2015)
df=data.frame()

#i=5
for (i in 1:length(test)){
	url=paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_SPEC_",test[i],".zip",sep='')
	download.file(url,'C:\\Users\\kebisu\\Downloads\\temp2.zip')
	temp=read.csv(unz('C:\\Users\\kebisu\\Downloads\\temp2.zip',paste("daily_SPEC_",test[i],".csv",sep='')),header=TRUE) 
	temp$State.Code=str_sub(paste('0',temp$State.Code,sep=''),-2,-1)
	temp$County.Code=str_sub(paste('00',temp$County.Code,sep=''),-3,-1)
	temp$Site.Num=str_sub(paste('000',temp$Site.Num,sep=''),-4,-1)
	temp$POC=str_sub(paste('0',temp$POC,sep=''),-2,-1)
	temp=mutate(temp,FIPSPOC=paste(State.Code,County.Code,Site.Num,POC,sep=''),FIPSPOC_Parameter=paste(FIPSPOC,'_',Parameter.Code,sep=''))
	temp$Method.Code=as.character(temp$Method.Code)
	temp$Method.Name=as.character(temp$Method.Name)
	temp$State.Name=as.character(temp$State.Name)
	temp$County.Name=as.character(temp$County.Name)
	temp$City.Name=as.character(temp$City.Name)
	temp$Parameter.Name=as.character(temp$Parameter.Name)
	temp$Date.Local=as.Date(as.character(temp$Date.Local),format="%Y-%m-%d")

	temp2=filter(temp,FIPSPOC_Parameter %in% SpeciesMethod$FIPSPOC_Parameter)
	temp2$FIPSPOC[substr(temp2$FIPSPOC,1,5)=='12086']=paste('12025',substr(temp2$FIPSPOC[substr(temp2$FIPSPOC,1,5)=='12086'],6,11),sep='')
	temp2$FIPSPOC_Parameter=paste(temp2$FIPSPOC,'_',temp2$Parameter.Code,sep='')
	
	temp2=select(temp2,FIPSPOC,FIPSPOC_Parameter,Parameter.Name,Parameter.Code,Date.Local,Units.of.Measure,Arithmetic.Mean,
		State.Name,County.Name,City.Name,Latitude,Longitude,Datum,Method.Name,Method.Code,AQI,Sample.Duration,Observation.Count,Observation.Percent)
	df=rbind(df,temp2)
	rm(temp,temp2)
}

df2=mutate(df,FIPS=substr(FIPSPOC,1,9)) %>%
	group_by(FIPSPOC_Parameter,Date.Local) %>%
	summarize(Value=mean(Arithmetic.Mean))
