#notes
#This is a random number generator...I'm going to go ahead and just define the cells of the matrix
#p_mat <- replicate(10,rnorm(10,rnorm(1),runif(1, min=0, max=1)))
#plants_happy_home <- abs(p_mat)
#plants_happy_home

mat_function <- function (x, y){
  mat <- matrix(ncol=x, nrow=y)
  return(mat)
}

#mat <- mat_function(3,3)
#mat
#Don't think I need this - 
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

#reproductionfunction = vector, same length as matrix, prob. a member of that species will reproductionfunctionduce

reproductionfunction <- c(.6,.2,.3)
reproductionfunction

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
c_mat

#survive = probability (0-1)

live_plants_live <- c(.9,.3,.4)
live_plants_live

#need to setup my pretty pretty garden

setup.plants <- function (reproductionfunction, live_plants_live, c_mat, names=c("weed","flower","grass")){
  if (length(reproductionfunction) !=length (live_plants_live))
    stop ("reproductionfunctionduction and survival parameters needed for all species")
  if (length(reproductionfunction) !=nrow (c_mat))
    stop ("reproductionfunctionduction and comp parameters needed for all species")
  if (ncol(c_mat) != nrow(c_mat))
    stop ("comp needs row and column to match")
  reproductionfunction <- setNames (reproductionfunction,names)
  return(list(reproductionfunction=reproductionfunction, live_plants_live=live_plants_live, c_mat=c_mat, names=names))
}
#ok....everything has worked up to here

info <- setup.plants(reproductionfunction, live_plants_live, c_mat)

info

timesteps <-5
plants <- array ("", dim=c(dim(t_mat), timesteps +1)) #timestep +1 allows us to record the old timestep
plants_traveling_through_time <- function (plants, info)

#planting the garden!  If I was actually planting a garden it would be all weeds though.  And the flowers
#and grass would have a 0% survival chance.  Telling it where to start for time=1. Or 0.  I'm actually not sure.
  
plants[1,1,1]<- " "
plants[1,2,1]<-"weed"
plants[1,3,1]<-"flower"
plants[2,1,1]<-" "
plants[2,2,1]<-"flower"
plants[2,3,1]<-"grass"
plants[3,3,1]<-"grass"
plants[3,2,1]<-" "
plants[3,1,1]<-"weed"

#setting up the fountain.  Or making a puddle.  Up to you. In my life highly likely a puddle

for(k in seq_len(dim(plants)[3]))
  plants[,,k][is.na(t_mat)] <- NA

timesteps <-5
#plants <- array ("", dim=c(dim(t_mat), timesteps +1)) #timestep +1 allows us to record the old timestep
# plants_traveling_through_time <- function (plants, info){
#   survive <- function (plant, info){
#   }
#   return (new.plants.matrix)
# }
plants_traveling_through_time <- function (plants, info){
survival_of_the_fittest <- function (cell, info, name){
  youaretheweakestlink <- runif(1)
  if(is.na(cell))
    return(na)
  if((cell)=='')
    return(' ')
  if(youaretheweakestlink <= info$live_plants_live[cell])
    return(name)
  if(youaretheweakestlink >= info$live_plants_live[cell])
    return(' ')
  #If NA = water, return NA for survival
  #If cell is empty return ""
  #If cell has a plant, run survival, return
  #"a""b""c" if survives, or "" if no survival
}
#run-ecosystem: I like to loop it loop it
  for (k in seq_len(dim(plants)[3]-1))
    for (i in 1:nrow(plants)){
      for(j in 1:ncol(plants)){
        t_mat <- survival_of_the_fittest(plants[i,j,k], info)
        plants[i,j,(k+1)] <- t_mat
      }
    }
  return (plants)
}
plants_traveling_through_time (plants, info)


#####ummmm  I don't think I actually need this.  I did everything seperately....
#unless I need to do it within the array

#run_plants_run <- function (plants, info, timesteps){
 #use my mad established planting skills
# plants <- array ("", dim=c(dim(t_mat), timesteps +1)) #timestep +1 allows us to record the old timestep in order to keep track of where in time we are
#  plants[1,1,1]<- " "
#  plants[1,2,1]<-"weed"
#  plants[1,3,1]<-"flower"
#  plants[2,1,1]<-" "
#  plants[2,2,1]<-"flower"
#  plants[2,3,1]<-"grass"
#  plants[3,3,1]<-"grass"
#  plants[3,2,1]<-" "
#  plants[3,1,1]<-"weed"
  #using my mad puddle skills
#  for(k in seq_len(dim(plants)[3]))
#    plants[,,k][is.na(t_mat)] <- NA
  #you can do it my creation!  LIVE!  LIVE THORUGH TIME!
#  plants <- plants_traveling_through_time(plants, info)
#  return(plants)
#}

#run_plants_run (plants, info, timesteps)


######make them do things.  Like make new plants

Plants_making_new_plants <- function(row, col, plants, info){
  places_youll_go <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))

  return(plants) }


