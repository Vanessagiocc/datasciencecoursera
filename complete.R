complete<-function(directory,id=1:332){
  files<-list.files(directory,full.names=TRUE)
  
  data<-data.frame("id"=numeric(),"nobs"=numeric())
  a<-1
  for (i in id) {
    nobs <- nrow(na.omit(read.csv(files[i])))
    data[a,]<-list(i,nobs)
	a<-a+1
  }
  data
}