#Use Random matrix
#plants of 2 or more species
#not more than one individual in one cell
#survive or die over time
#survive =repro.  need to replace each time in the matrix - not just one single column, 
#but the rows and columns around that cell/entire matrix, as they disperse into area around them
#competition context - need to have two species fight to survive in the area 
#which species make it into each slot
#wrapper for species properties

mat <- mat_function(10,10)
x <- 10
y <- 10
mat_function <- function (x, y){
    mat <- matrix(ncol<-x, nrow<-y)
    return(mat)
}
  
mat <- mat_function(10,10)
  x <- 10
  y <- 10

  
setup.plants <- function (repro, survive, comp.mat, names=NULL){
  if(is.null (names))
    names <- letters [seq_along(repro)]
  if (length(repro)) !=length (survive)
    stop ("Reproduction and survival parameters needed for all species")
  repro <- setNames (repro,names)
  return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}

#Repro = vector, same length as matrix, prob. a member of that species will reproduce
repro <- numeric (length = 3)
plant.repro <- append (repro, 0.3, 0.2)

#survive = probability (0-1)
#comp.mat = dim = # plant species, prob. species will survive when in comes into contact with
#another species. 

Weeds <-
Grass <-


survive <- function (cell, info){
  if(runif(1)) <- info$survive[plant])
}

plant_time <- function (plants, terrain, info){
  survive <- function (plant, info){
    #survive function
  }
  #looping et al. 
  return (new.plants.matrix)
}

plants <- array ("", dim=c(dim(terrain), timeteps +1)) #timestep +1 allows us to record the old timestep in order to keep track of where in time we are
for (i in seq_len(dim(plants)[3]))
  plants [,,i][is.na(terrain)] <- NA
#Fills in whatever is NA as water



  