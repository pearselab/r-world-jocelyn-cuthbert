#notes

mat_function <- function (x, y){
  mat <- matrix(ncol=x, nrow=y)
  return(mat)
}

mat <- mat_function(10,10)

p_mat <- replicate(10,rnorm(10,rnorm(1),runif(1, min=0, max=1)))
plants_happy_home <- abs(p_mat)
plants_happy_home

#This seems like a good start, but not exactly what I need, so I manually made what I
#needed to start.
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

#Repro = vector, same length as matrix, prob. a member of that species will reproduce
repro500 <- c(.6,.2,.3)
repro500
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
survive_plants <- c(.9,.3,.4)
#so now I am getting names, need to go ahead and establish that
names <- c("weed", "flower", "grass")
rownames(c_mat) <-names
colnames(c_mat) <- names
#plant things

setup.plants <- function (repro, survive, comp.mat, names=c("weed","flower","grass")){
  if (length(repro) !=length (survive))
    stop ("Reproduction and survival parameters needed for all species")
  if (length(repro) !=nrow (c_mat))
    stop ("Reproduction and comp parameters needed for all species")
  if (ncol(c_mat) != nrow(c_mat))
    stop ("comp needs row and column to match")
  repro <- setNames (repro,names)
  return(list(repro=repro, survive=survive, c_mat=c_mat, names=names))
}

info <- setup.plants(repro500, survive_plants, c_mat)

info

timesteps <-5
plants <- array ("", dim=c(dim(t_mat), timesteps +1)) #timestep +1 allows us to record the old timestep
plants_traveling_through_time <- function (plants, info)

  #planting the garden!  If I was actually planting a garden it would be all weeds though.  And the flowers
  #and grass would have a 0% survival chance.
  plants[1,1,1]<- " "
plants[1,2,1]<-"weed"
plants[1,3,1]<-"flower"
plants[2,1,1]<-" "
plants[2,2,1]<-"flower"
plants[2,3,1]<-"grass"
plants[3,3,1]<-"grass"
plants[3,2,1]<-" "
plants[3,1,1]<-"weed"

#setting up the founatin.  Or making a puddle.  Up to you. In my life highly likely a puddle

for(k in seq_len(dim(plants)[3]))
  plants[,,k][is.na(t_mat)] <- NA

survive <- function (cell, info, name){
  youaretheweakestlink <- runif(1)
  if(is.na(cell))
    return(na)
  if((cell)=='')
    return(' ')
  if(youaretheweakestlink <= info$survive[cell])
    return(name)
  if(youaretheweakestlink >= info$survive[cell])
    return(' ')
  for (k in seq_len(dim(plants)[3]-1))
    for (i in 1:nrow(plants)){
      for(j in 1:ncol(plants)){
        plants_happy_home <- survive (plants[i,j,k], info)
        plants[i,j,(k+1)] <- plants_happy_home
      }
    }
  return (plants)
}

#If NA = water, return NA for survival
#If cell is empty return ""
#If cell has a plant, run survival, return
#"a""b""c" if survives, or "" if no survival


timesteps <-5
plants <- array ("", dim=c(dim(t_mat), timesteps +1)) #timestep +1 allows us to record the old timestep
plants_traveling_through_time <- function (plants, info){
  survive <- function (plant, info){
  }
  return (new.plants.matrix)
}

run_plants_run <- function (t_mat, num.timesteps, info){
  plants <- array ("", dim=c(dim(t_mat), timesteps +1)) #timestep +1 allows us to record the old timestep in order to keep track of where in time we are
  for (i in seq_len(dim(plants)[3]))
    plants [,,i][is.na(t_mat)] <- NA
  for(i in seq_len(dim(plants)[3]))
    print(k)
  plants[i,j,k] <- plants_traveling_through_time(plants[,,k], info)
}

run_plants_run
plant <- reproduce(row, column, plants, info)

Plants_making_new_plants <- function(row, col, plants, info){
  places_youll_go <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))

  return(plants) }
