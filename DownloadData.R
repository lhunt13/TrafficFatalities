#Download the data files
path <- "/Users/lamarhuntiii/Documents/Second Year Classes/Data Science/data"
for(i in 2008:2015){
  dir.create(paste(path,"/fars",i,sep=""))
  assign(paste(path,i,"/zip.zip", sep=""),file())
  
  if((i >= 1975 & i <= 1993) | (i >= 2001 & i <= 2011)){
    file <- paste("ftp://ftp.nhtsa.dot.gov/fars/",i,"/DBF/FARS",i,".zip", sep="")
  }
  if(i >= 1994 & i <= 2000){
    file <- paste("ftp://ftp.nhtsa.dot.gov/fars/",i,"/DBF/FARSDBF",substr(i,3,4),".zip", sep="")
  }
  if(i == 2012){
    file <- paste("ftp://ftp.nhtsa.dot.gov/fars/",i,"/National/DBF/FARS",i,".zip", sep="")
  }
  if(i >= 2013){
    file <- paste("ftp://ftp.nhtsa.dot.gov/fars/",i,"/National/FARS",i,"NationalDBF.zip", sep="")
  }
  download.file(file, paste(path,"/fars",i,"/zip.zip", sep=""))
  unzip(paste(path,"/fars",i,"/zip.zip", sep=""),exdir=paste(path,"/fars",i,sep=""))
}

