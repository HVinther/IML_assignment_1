library(CASdatasets)
library(splitTools)

loadData<-function(seed = 2024){
  # Henter datasættene som beskrevet, men i funktionens enviroment istedet for det globale
  data(freMPL1,envir = environment())
  
  data(freMPL2,envir = environment())
  
  data(freMPL3,envir = environment())
  
  data(freMPL4,envir = environment())
  
  freMPL3 <- subset( freMPL3 , select = -DeducType )  
  
  freMPL4 <- subset( freMPL4 , select = -DeducType )  
  
  freMPL <- rbind(freMPL1,freMPL2,freMPL3,freMPL4)
  
  set.seed(seed) 
  
  # De relevante objekter skubbes ud til det globale enviroment
  freMPL <<- freMPL
  ind <<- partition(freMPL$ClaimInd, p = c(train = 0.8, test = 0.2)) #### train and test have the same claim frequency
  train <<- freMPL[ind$train, ] 
  test <<- freMPL[ind$test, ]
  
  # Tømmer alt fra det funktionens enviroment, ie. de store dataset, som allerede er gemt i det globale enviroment
  rm(list = ls())
}

