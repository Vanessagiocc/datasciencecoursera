rankall<-function(outcome,num){
 #De un tipo de enfermedad especifico, lista todos los estados y hospitales asociados al ranking pedido, "best","worst" u otro numero
  data<-read.csv("outcome-of-care-measures.csv",na.strings = "Not Available",stringsAsFactors = FALSE)
  marcador<-c('heart attack'=11, 'heart failure'=17, 'pneumonia'=23)
  if(outcome%in%names(marcador)==FALSE){ #valida si outcome pertenece a las 3 variables "heart...
    stop('invalid outcome')
  }
  index<-marcador[outcome] #guarda el indice de columna en que aparece el outcome indicado
  subdata<-data[,c(7,index,2)]
  names(subdata)<-c("State","Rate","Hospital")
  datasinNA<-subdata[complete.cases(subdata),]
  dataorden<-arrange(datasinNA,datasinNA[1],datasinNA[2],datasinNA[3])#ordena, primero se pone como argumento la data, y luego las variables por las que se ordenará
  #Luego de ordenar y sacar NA de datos
  #Se realiza un split, con lo que se tiene una lista de data frame ordenados por State
  statesplit<-split(dataorden,dataorden$State)
  #Se realiza una función a utilizar con sapply
  #Según num ingresado (ranking) se retorna el nombre del hospital asociado a ese ranking
  #Como sapply funciona como for, esta funcion va a recibir como argumentos, aparte de num,
  #cada data frame de la lista, es decir, va a ingresa de a un "state", lo que tiene una columna de state(mismo repetido),rate y nombre hospital
  
   seleccion<-function(x,num){
    
    z<-nrow(x)
    if(class(num)=="character"){
      if(num=="best"){
        x[[1,3]]
      }
      else if (num=="worst"){
        
        x[[z,3]]
      }
      else{
        stop('invalid num')
      }
    }
    else{
      if(num>z | num<=0){ #Si el numero de ranking excede la cantidad de filas, retorna 0
        NA
      }
      else{
        x[[num,3]]
      }
    }
   }
   
  listahospitales<-sapply(statesplit,seleccion,num)
  #Con uso de sapply se obtiene un vector cuyos nombres names() serán los states y valores de vector serán los nombres de hospitales
  resultado<-data.frame("Hospital"=listahospitales,"Status"=names(listahospitales))
  resultado

}
  