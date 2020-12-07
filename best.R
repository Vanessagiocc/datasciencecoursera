best<-function(state,outcome){
  data<-read.csv("outcome-of-care-measures.csv",na.strings = "Not Available",stringsAsFactor=FALSE)
  marcador<-c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  if(outcome%in%names(marcador)==FALSE){  
    stop('invalid outcome')
  }
  if(state%in%data[,7]==FALSE){
    stop('invalid state')
  }
  index<-marcador[outcome]
  subdata<-data[which(data[,7]==state),c(2,7,index)]
  datasinNA<-subdata[complete.cases(subdata),]
  dataordenada<-arrange(datasinNA,(datasinNA[3]))
  dataordenada[1,1]
}