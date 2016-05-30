## Assignment 2 for Coursera Data Science R programming.
## This assignment is used to illistrate the use 
## of R caching operator <<-  

## This function will 
## 1.) set the value of a matrix
## 2.) get the value of a matrix
## 3.) store (cache) the inverse value of a matrix.
## 3.) get the (cached) inverse value of a matrix.  
makeCacheMatrix <- function(x = matrix()) {

  mat_inv <- NULL
  
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inverse) mat_inv <<- inverse
  get_inverse <- function() mat_inv
  
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
  
  
}


## This function will get the inverse of a matrix. 

cacheSolve <- function(x, ...) {
  
  ## Do we already have the inverse of the matrix? 
  mat_inv <- x$get_inverse()
  if(!is.null(mat_inv)) {
    message("getting cached data.")
    return(mat_inv)
    
  } else {
    ## We don't, so determine what the inverse should be.  
    data <- x$get()
    inv <- solve(data)
    x$set_inverse(inv)
    inv
    
  }    
  
}

# Test case
# x <- rbind( c(2,4), c(6,8))
# m <- matrixCacheMatrix(x)
# cacheSolve(m)
#     [1,]      [,2]
#[1,]   -1.00   0.50
#[2,]    0.75  -0.25
#
# cacheSolve(m)
# getting cached data.
#     [1,]      [,2]
#[1,]   -1.00   0.50
#[2,]    0.75  -0.25



