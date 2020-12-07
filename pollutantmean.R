pollutantmean<-function(directory,pollutant,id=1:332){
files<-list.files(directory,full.names=TRUE)
lista<-vector(mode="list",length=length(files))
 for(i in seq_along(files)){
lista[[i]]<-read.csv(files[[i]])
}

data<-do.call(rbind,lista)
datasub<-data.frame()

 for(i in id){

datasub<-rbind(datasub,data[which(data[,"ID"]==i),])

}

mean(datasub[,pollutant],na.rm=TRUE)

}
