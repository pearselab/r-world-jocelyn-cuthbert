# R world is made up of grid cells, the elements of a matrix
#basis = terrain, will be a numeric matrix whose elements represent height
#make a square matrix with odd dimesnsions:

mat_function <- function (x, y){
  mat <- matrix(ncol<-x, nrow<-y)
  return(mat)
}

mat <- mat_function(11,11)
x <- 11
y <- 11


#pick a starting height for four courners
  #repeat Diamond step, square step until matrix is filled with values

# diamond_step <- 
# square_step <- 
# diamond_square_step <- #needs to call both former functions in turn until the matrix
#is filled up, needs to initialize everything - seed w/starting values, create matrix, etc. 
#should step through matrix, affecting smaller and smaller chunks
#Add a little bit of noise with rnorm that decreases with each iteration (sd argument)

#write a function called make_terrain that will act as a wrapper for the diamond step algorithm
#This should be adding in water as well (height values of NA)




