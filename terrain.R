# # R world is made up of grid cells, the elements of a matrix
# #basis = terrain, will be a numeric matrix whose elements represent height
# #pick a starting height for four courners
# # need to create the corners of the matrix
# up_left <- round (rnorm (1, mean=4), digits=1)
# up_right <-round (rnorm (1, mean=4), digits=1)
# Lower_left <-round (rnorm (1, mean=4), digits=1)
# Lower_right <-round (rnorm (1, mean=4), digits=1)
# 
# #add values
# mat[1,1] <- up_left
# mat[1,y] <- up_right
# mat[x,1] <- Lower_left
# mat[x,y] <- Lower_right
# 
# mat
# # have corners, everythihgn else is NA

#make a square matrix with odd dimensions:
mat_function <- function (n){
  x <- (2^n)+1
  y <- (2^n)+1
  mat <- matrix(ncol=(2^n)+1, nrow=(2^n)+1)
  up_left <- round (rnorm (1, mean=4), digits=1)
  up_right <-round (rnorm (1, mean=4), digits=1)
  Lower_left <-round (rnorm (1, mean=4), digits=1)
  Lower_right <-round (rnorm (1, mean=4), digits=1)
  mat[1,1] <- up_left
  mat[1,y] <- up_right
  mat[x,1] <- Lower_left
  mat[x,y] <- Lower_right
  return(mat)
}

mat <- mat_function(2)
mat
#x <- n^2+1 , so 5, 9
#y <- n^2+1 , so 5, 9


#repeat Diamond step, square step until matrix is filled with values

diamond_step <- function (mat,n){
  x <- (2^n)+1
  y <- (2^n)+1
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
mat <- diamond_step(mat,2)
mat
#Yay!  Now we also have one value in the middle!



# square_step <-


square_step <- function (mat, n){
  x <- (2^n)+1
  y <- (2^n)+1
  median_x <- median(1:x)
  median_y <- median (1:y)
  mat[median_y, median_x] <- mean(c(mat[median_x,y], mat[median_x,1], mat[1,median_y], mat[median_x,y]))
  mat[1,median_y] <- mean(c(mat [1,1], mat [x,1],mat[median_y, median_x]), na.rm=TRUE)
  mat[x,median_y] <- mean(c(mat [1,y],mat [1,y],mat[median_y, median_x]), na.rm=TRUE)
  mat[median_x,y] <- mean(c(mat [x,y],mat [1,y],mat[median_y, median_x]), na.rm=TRUE)
  mat[median_x,1] <- mean(c(mat [x,y],mat [x,1],mat[median_y, median_x]), na.rm=TRUE)
  return(mat)
}

mat <- square_step(mat, 2)
mat

# diamond_square_step <- #needs to call both former functions in turn until the matrix
#is filled up, needs to initialize everything - seed w/starting values, create matrix, etc.
#should step through matrix, affecting smaller and smaller chunks
#Add a little bit of noise with rnorm that decreases with each iteration (sd argument)

# diamond_square_step.fail <- function (mat){
#   mat_function <- function (x, y){
#     mat <- matrix(ncol=x, nrow=y)
#     return(mat)
#   }
#   mat <- mat_function(9,9)
#   x <- ncol(mat)
#   y <- nrow(mat)
#   median_x <- median(1:x)
#   median_y <- median (1:y)
#   mean_vector <- c(mat [1,1], mat [1,y], mat [x,1], mat [x,y])
#   mean_corners <- mean(mean_vector)
#   mat[median_y, median_x] <- mean(c(mat[median_x,y], mat[median_x,1], mat[1,median_y], mat[median_x,y]))
#   mat[1,median_y] <- mean(c(mat [1,1], mat [x,1],mat[median_y, median_x]), na.rm=TRUE)
#   mat[x,median_y] <- mean(c(mat [1,y],mat [1,y],mat[median_y, median_x]), na.rm=TRUE)
#   mat[median_x,y] <- mean(c(mat [x,y],mat [1,y],mat[median_y, median_x]), na.rm=TRUE)
#   mat[median_x,1] <- mean(c(mat [x,y],mat [x,1],mat[median_y, median_x]), na.rm=TRUE)
#   return(mat)
# }
# # doesn't work even once - need to make it create matrix, run diamond step, run square step,
# #then need to figure out how to loop it for smaller and smaller squares,
# #not just one big outside square
# 
# mat <- diamond_square_step.fail (mat)
# 
# mat


#write a function called make_terrain that will act as a wrapper for the diamond step algorithm
#This should be adding in water as well (height values of NA)

#this works...now I need to add the loop

#need to loop smaller and smaller
#seq (1,9,8)
#mat <- matrix (1:81) 9x9 has 81 numbers
#need to do everything in quarters
#mat[1:5,1:5] = quarters of the 9X9
#seq(1,9 by=8) will get numbers, 1,9
#seq(1,9 by=4) will get numbers 1,5,9
#seq(1,9, by=2) will get odd numbers
#seq(1,9 by=4)[2] will get 5-9
#changing the by # in the loop across all the columns and rows
#but I don't want to have to tell it how big my matrix is
#I want it to look at the matrix I have entered and work accordingly
#I don't know how to do that for the by piece of it - I want to change the by not manually, but based on what the matrix size is

Diamond_square_step_dont_fail <- function(mat,n){
  #defining i and j so that the grid has defined odd sides. This was an attempt to bypass the issues I was having with only being able to get grids for 5 and 9 length. 
  i <- (2^n)-1
  j <- (2^n)-1
  #creating a matrix
  mat <- mat_function (n)
  #applying my square step
  mat <- square_step (mat,n)
  #applying my diamond step
  mat <- diamond_step (mat,n)
  #defining my by variable as something that will change as it loops. So if n was two, this would give me by 4,2,1 all minus one, to keep it within the dimensions of the matrix. so 3 and 1. 
  for (k in 2^(n:1)-1)
    #defining my i and j again. I know I only need to do it once, I am trying still to figure out which is correct
    for (i in seq(from=1, to=(ncol(mat)), by=k-1)){
      for (j in seq(from=1, to=(nrow(mat)), by=k-1)){
        #Applying my functions across the matrix. Right now it is i or j, 1:j+k-1 which is out of the dimensions because i or j + k would be too large, but it worked when I used it for 5 or 9 again, so I am just trying to figure out that piece. 
        mat[j:(j+k-1), i:(i+k-1)] <- square_step(mat[j:(j+k-1), i:(i+k-1)],n)
        mat[j:(j+k-1), i:(i+k-1)] <- diamond_step(mat[j:(j+k-1), i:(i+k-1)],n)
      }
    }
  return(mat)
}

mat <- Diamond_square_step_dont_fail (mat,2)
mat



# I have made sure like 5 times that there is no where I am going out of bounds.
#I know the issue is within my square step and diamond step but I can't quite get it in my head what the issue is

image(mat)

mat_function <- function (x, y){
  mat <- matrix(ncol=x, nrow=y)
  up_left <- round (rnorm (1, mean=4), digits=1)
  up_right <-round (rnorm (1, mean=4), digits=1)
  Lower_left <-round (rnorm (1, mean=4), digits=1)
  Lower_right <-round (rnorm (1, mean=4), digits=1)
  mat[1,1] <- up_left
  mat[1,y] <- up_right
  mat[x,1] <- Lower_left
  mat[x,y] <- Lower_right
  return(mat)
}

mat <- mat_function(5,5)
mat
#x <- n^2+1 , so 5, 9
#y <- n^2+1 , so 5, 9


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

diamond_square_step.fail <- function (mat){
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

mat <- diamond_square_step.fail (mat)

mat


#write a function called make_terrain that will act as a wrapper for the diamond step algorithm
#This should be adding in water as well (height values of NA)

Diamond_square_step_dont_fail <- function(mat){
  mat <- mat_function (5,5)
  mat <- square_step (mat)
  mat <- diamond_step (mat)
}
}
mat <- Diamond_square_step_dont_fail (mat)

mat

#this works...now I need to add the loop

#need to loop smaller and smaller
#seq (1,9,8)
#mat <- matrix (1:81) 9x9 has 81 numbers
#need to do everything in quarters
#mat[1:5,1:5] = quarters of the 9X9
#seq(1,9 by=8) will get numbers, 1,9
#seq(1,9 by=4) will get numbers 1,5,9
#seq(1,9, by=2) will get odd numbers
#seq(1,9 by=4)[2] will get 5-9
#changing the by # in the loop across all the columns and rows
#but I don't want to have to tell it how big my matrix is
#I want it to look at the matrix I have entered and work accordingly
#I don't know how to do that for the by piece of it - I want to change the by not manually, but based on what the matrix size is

Diamond_square_step_dont_fail <- function(mat,x,y){
  mat <- mat_function (x,y)
  mat <- square_step (mat)
  mat <- diamond_step (mat)
  for (i in seq(from=1, to=(ncol(mat)-1), by=2^(4:1))){
    mat[j:(j+2), i:(i+2)] <- square_step(mat[j:(j+2), i:(i+2)])
    mat[j:(j+2), i:(i+2)] <- diamond_step(mat[j: (j+2), i:(i+2)])
    for (j in seq(from=1, to=(nrow(mat)-1), by=2^(4:1))){
      mat[j:(j+2), i:(i+2)] <- square_step(mat[j:(j+2), i:i(i+2)])
      mat[j:(j+2), i:(i+2)] <- diamond_step(mat[j:(j+2), i:(i+2)])
    }
  }
  return(mat)
}

mat <- Diamond_square_step_dont_fail (mat, x=5,y=5)

mat


Diamond_square_step_dont_fail <- function(mat,x,y){
  mat <- mat_function (x,y)
  mat <- square_step (mat)
  mat <- diamond_step (mat)
  for (i in seq(from=1, to=(ncol(mat)-1), by=2^(2:1))){
    mat[j:(j+2), i:(i+2)] <- square_step(mat[j:(j+2), i:(i+2)])
    mat[j:(j+2), i:(i+2)] <- diamond_step(mat[j: (j+2), i:(i+2)])
    for (j in seq(from=1, to=(nrow(mat)-1), by=2^(2:1))){
      mat[j:(j+2), i:(i+2)] <- square_step(mat[j:(j+2), i:i(i+2)])
      mat[j:(j+2), i:(i+2)] <- diamond_step(mat[j:(j+2), i:(i+2)])
    }
  }
  return(mat)
}

mat <- Diamond_square_step_dont_fail (mat, x=5,y=5)
