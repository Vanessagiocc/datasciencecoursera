corr<-function(directory,threshold=0){
  files<-list.files(directory,full.names=TRUE)
  x<-c()
  y<-c()
  b<-1
  z<-data.frame()
  for(i in seq_along(files)){  
    z<-na.omit(read.csv(files[i]))
    if(nrow(z)>threshold){
      x[b]<-cor(z$sulfate,z$nitrate)
      y[b]<-i
      b<-b+1
    }
  }
  names(x)<-y
  x
}