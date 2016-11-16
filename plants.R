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
    mat <- matrix(ncol=x, nrow=y)
    return(mat)
}

mat <- mat_function(10,10)

p_mat <- mat_function(10,10)
p_mat <- replicate(10,rnorm(10,rnorm(1),runif(1, min=0, max=1)))
plants_happy_home <- abs(p_mat)


#Repro = vector, same length as matrix, prob. a member of that species will reproduce
repro <- numeric (length = 3)
plant.repro <- append (repro, 0.3, 0.2, 0.4)
#these are not working - getting an unused argument error.  For some reason
#repro this way becomes 0,0,0, and plant.repro becomes 0,0.3,0,0
#but somehow just append seems to know that it is numeric

repro1 <- append(.3,.2,.4)

repro500 <- c(.3,.2,.4)

#comp.mat = dim = # plant species, prob. species will survive when in comes into contact with
#another species.

c_mat <- mat_function(3,3)
c_mat[1,1] <- .5
c_mat[2,1] <- .7
c_mat[3,1] <- .85
c_mat[1,2] <- .3
c_mat[1,3] <- .21
c_mat[2,3] <- .73
c_mat[3,2] <- .02
c_mat[2,2] <- .5
c_mat[3,3] <- .5

#survive = probability (0-1)

survive_plants <- c(.2,.3,.4)

#plant things

setup.plants <- function (repro, survive, comp.mat, names=NULL){
  if(is.null (names))
    names <- letters [seq_along(repro)]
  if (length(repro) !=length (survive))
  stop ("Reproduction and survival parameters needed for all species")
  if (length(repro) !=nrow (comp.mat))
  stop ("Reproduction and comp parameters needed for all species")
  if (ncol(comp.mat) != nrow(comp.mat))
  stop ("comp needs row and column to match")
  repro <- setNames (repro,names)
  return(list(repro=repro, survive=survive, comp.mat=comp.mat, names=names))
}

info <- setup.plants(repro500, survive_plants, c_mat)


######need to define cell, so will make a new terrain matrix of numbers and NA
t_mat <- mat_function(3,3)
t_mat[1,1] <- 5.32
t_mat[2,1] <- 0.73
t_mat[3,1] <- NA
t_mat[1,2] <- 0.90
t_mat[1,3] <- 2.72
t_mat[2,3] <- 1.37
t_mat[3,2] <- NA
t_mat[2,2] <- 0.59
t_mat[3,3] <- 5.05
t_mat

survive <- function (cell, info, name){
  if(is.na(cell))
    return(na)
  if((cell)=='')
    return(' ')
  if(runif(1) <= info$survive[name])
    return(name)
  if(runif(1) >= info$survive[name])
    return(' ')
}
#too loop through matrix
  for (i in 1:nrow(plants)){
    for(j in 1:ncol(plants)){
      plants_happy_home <- survive (plants[i,j], info)
      return (new.plant.matrix)
    }
  }
  #probably will not need next part until later but I tried it anyway#
  if("a","b","c"(cell))
    return(c_mat)
}
#If NA = water, return NA for survival
#If cell is empty return ""
#If cell has a plant, run survival, return
#"a""b""c" if survives, or "" if no survival

plant_time <- function (plants, t_mat, info){
  survive <- function (plant, info){
    #survive function
  }
  #looping et al.
  return (new.plants.matrix)
}

run_plants_run <- function (t_mat, num.timesteps, info){
  plants <- array ("", dim=c(dim(t_mat), timesteps +1)) #timestep +1 allows us to record the old timestep in order to keep track of where in time we are
  for (i in seq_len(dim(plants)[3]))
    plants [,,i][is.na(t_mat)] <- NA
      for(i in seq_len(dim(plants)[3]))
        print(k)
        plants[i,j,k] <- plant.time(plants[,,k], info)
}

plants <- array ("", dim=c(dim(t_mat), timesteps +1)) #timestep +1 allows us to record the old timestep in order to keep track of where in time we are
for (i in seq_len(dim(plants)[3]))
  plants [,,i][is.na(t_mat)] <- NA
#Fills in whatever is NA as water

rpt <- run_plants_run(t_mat, timesteps, info)

timesteps <-5

