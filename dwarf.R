set.seed(600)
Trial<-10000000
NumberofBed<-7
NumberofRight<-0
for (i in 1:Trial){
  Bed<-c(1:NumberofBed)
  Dwarf<-c(1:NumberofBed)
  Dwarf[1]<-ceiling(runif(1,0,1)*(NumberofBed-1))+1
  for (j in 2:NumberofBed){
    index<-which(Bed==Dwarf[j-1])
    Bed<-Bed[-index]
    if (Dwarf[j] %in% Bed){
        Dwarf[j]<-j  
      }
      else {
        Dwarf[j]<-sample(Bed,1)
      }
  }
  if (Dwarf[NumberofBed]==NumberofBed){
  NumberofRight<-NumberofRight+1
  }
}
Proportion<-NumberofRight/Trial