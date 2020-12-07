add2<-function(x,y)
{
  x+y
}
columnmean<-function(y,removeNA=TRUE)
  {
      nc=ncol(y)
     means<-numeric(nc)
  for(i in 1:nc){
        means[i]<-mean(y[,i],na.rm=removeN}
     data<-data.frame("id"=numeric(),"nobs"=numeric())
    
               for (i in 1:2) {
               nobs <- nrow(na.omit(read.csv(files[i])))
                data[i,]<-list(i,nobs)
         +           }