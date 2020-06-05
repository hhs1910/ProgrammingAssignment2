## The objective of this assignment is to create a matrix object that can cache its inverse

## We first define a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Step 1 is to initialize the inverse property
  inv <- NULL
  
  ## Step 2 is the method to set the matrix
  set <- function (matrix) {
    
    mat <<- matrix
    inv <<- NULL
    
  }
  
  ## Step 3 is the method to get the matrix
  get <- function() {
    
    mat
    
  }
  
  ## Step 4 is a function which creates the inverse of the matrix
  setInverse <- function(inverse) {
    
    inv <<- inverse
    
  }
  
  ## Step 5 is the function to get the inverse of the matric
  getInverse <- function() {
    
    inv
    
  }
  
  ## And finally we return the list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The next step is to compute the inverse of the matrix returned by "makeCacheMatrix"
## which has been defined above. If the inverse has been calculated previously, 
## then the function (cachesolve) should retrieve the inverse from this cache.

cacheSolve <- function(x, ...) {
  
  mat <- x$getInverse()
  
  ## Return the inverse if already set
  if(!is.null(mat)) {
    
    message("retrieving cached data")
    return(mat)
    
  }
  
  ## Get the matrix from object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  mat <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(mat)
  
  ## Return the matrix
  mat
}
