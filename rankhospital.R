rankhospital<-function(state,outcome,num){
  # funcion que a determinado estado y outcome (que es el tipo de enfermedad), retorna un ranking indicado por num
  # outcome es el nombre abreviado de la columna, en esa columna residen los radios de mortalidad
  
  data<-read.csv("outcome-of-care-measures.csv",na.strings = "Not Available",stringsAsFactor=FALSE)
  marcador<-c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  if(outcome%in%names(marcador)==FALSE){ #valida si outcome pertenece a las 3 variables "heart...
    stop('invalid outcome')
  }
  if(state%in%data[,7]==FALSE){#valida si existe estado indicado, en el data frame
    stop('invalid state')
  }
  
  index<-marcador[outcome] #guarda el indice de columna en que aparece el outcome indicado
  subdata<-data[which(data[,"State"]==state),c(2,7,index)]
  datasinNA<-subdata[complete.cases(subdata),]
  dataorden<-arrange(datasinNA,datasinNA[3],datasinNA[1])
  x<-nrow(dataorden)
  if(class(num)=="character"){
    if(num=="best"){
      dataorden[1,1]
    }
    else if(num=="worst"){
      
      dataorden[x,1]
    }

  }
  else{
    if(num>x | num<=0){ #Si el numero de ranking excede la cantidad de filas, retorna 0
      NA
    }else{
      datafinal<-dataorden[1:num,c(1,3)] #subconjunto
      
      ranking<-list("Rank"=1:num)
      return<-cbind(datafinal,ranking) #Agrega una columna ranking al data frame
      names(return)<-c("Hospital","Rate","Ranking")
      rename(return,Rate=names(return[2])) #renombre 
      return              
    }
    
  }
  
}