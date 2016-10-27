path <- "./data"
for(i in 2000:2015){
  #person file
  if(i <= 1982 | i >= 1994){
    assign(paste0("person",i),read.dbf(paste0(path, "/fars",i,"/person.dbf")))
  }
  else{
    assign(paste0("person",i),read.dbf(paste0(path, "/fars",i,"/per",i,".dbf")))
  }
}