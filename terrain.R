# R world is made up of grid cells, the elements of a matrix
#basis = terrain, will be a numeric matrix whose elements represent height
#make a square matrix with odd dimensions:
mat_function <- function (x, y){
  mat <- matrix(ncol=x, nrow=y)
  return(mat)
}

mat <- mat_function(5,5)
x <- 5
y <- 5
#x <- n^2+1 , so 5, 9
#y <- n^2+1 , so 5, 9
#pick a starting height for four courners
# need to create the corners of the matrix
up_left <- round (rnorm (1, mean=4), digits=1)
up_right <-round (rnorm (1, mean=4), digits=1)
Lower_left <-round (rnorm (1, mean=4), digits=1)
Lower_right <-round (rnorm (1, mean=4), digits=1)

#add values
mat[1,1] <- up_left
mat[1,y] <- up_right
mat[x,1] <- Lower_left
mat[x,y] <- Lower_right

mat
# have corners, everythihgn else is NA

#repeat Diamond step, square step until matrix is filled with values

diamond_step <- function (mat){
  x <- ncol(mat)
  y <- nrow(mat)
  up_l <- mat [1,1]
  up_r <- mat [1,y]
  low_left <- mat [x,1]
  low_right <- mat [x,y]
  mean_vector <- c(up_l, up_r, low_left, low_right)
  mean_corners <- mean(mean_vector)
  median_x <- median(1:x)
  median_y <- median (1:y)
  mat[median_y, median_x] <- mean_corners
  return(mat)
}
mat <- diamond_step(mat)
mat
#Yay!  Now we also have one value in the middle!



# square_step <- 


square_step <- function (mat){
  x <- ncol(mat)
  y <- nrow(mat) 
  median_x <- median(1:x)
  median_y <- median (1:y)
  mat[median_y, median_x] <- mean(c(mat[median_x,y], mat[median_x,1], mat[1,median_y], mat[median_x,y]))
  mat[1,median_y] <- mean(c(mat [1,1], mat [x,1],mat[median_y, median_x]), na.rm=TRUE)
  mat[x,median_y] <- mean(c(mat [1,y],mat [1,y],mat[median_y, median_x]), na.rm=TRUE)
  mat[median_x,y] <- mean(c(mat [x,y],mat [1,y],mat[median_y, median_x]), na.rm=TRUE)
  mat[median_x,1] <- mean(c(mat [x,y],mat [x,1],mat[median_y, median_x]), na.rm=TRUE)
  return(mat)
}

mat <- square_step(mat)
mat

# diamond_square_step <- #needs to call both former functions in turn until the matrix
#is filled up, needs to initialize everything - seed w/starting values, create matrix, etc. 
#should step through matrix, affecting smaller and smaller chunks
#Add a little bit of noise with rnorm that decreases with each iteration (sd argument)

diamond_square_step <- function (mat){
  mat_function <- function (x, y){
    mat <- matrix(ncol=x, nrow=y)
    return(mat)
  }
  mat <- mat_function(9,9)
  x <- ncol(mat)
  y <- nrow(mat) 
  median_x <- median(1:x)
  median_y <- median (1:y)
  mean_vector <- c(mat [1,1], mat [1,y], mat [x,1], mat [x,y])
  mean_corners <- mean(mean_vector)
  mat[median_y, median_x] <- mean(c(mat[median_x,y], mat[median_x,1], mat[1,median_y], mat[median_x,y]))
  mat[1,median_y] <- mean(c(mat [1,1], mat [x,1],mat[median_y, median_x]), na.rm=TRUE)
  mat[x,median_y] <- mean(c(mat [1,y],mat [1,y],mat[median_y, median_x]), na.rm=TRUE)
  mat[median_x,y] <- mean(c(mat [x,y],mat [1,y],mat[median_y, median_x]), na.rm=TRUE)
  mat[median_x,1] <- mean(c(mat [x,y],mat [x,1],mat[median_y, median_x]), na.rm=TRUE)
  return(mat)
}
# doesn't work even once - need to make it create matrix, run diamond step, run square step, 
#then need to figure out how to loop it for smaller and smaller squares,
#not just one big outside square

mat <- diamond_square_step

mat


#write a function called make_terrain that will act as a wrapper for the diamond step algorithm
#This should be adding in water as well (height values of NA)




