	
test2=c(1990:2016)
i=23
url=paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/annual_all_",test2[i],".zip",sep='')
download.file(url,'C:\\Users\\Keita\\Downloads\\temp2.zip')


temp=read.csv(unz('C:\\Users\\Keita\\Downloads\\temp2.zip',paste("annual_all_",test2[i],".csv",sep='')),header=TRUE) 


