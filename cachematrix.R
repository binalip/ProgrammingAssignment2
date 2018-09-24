## Storing the computationally costly computation (i.e. Matrix inversion) in cash memory
## so that when calling matrix inversion again, re computation is not necessary.
## It is assumed that the matrix supplied is always invertible. 

## Example usage
mcm <- makeCacheMatrix()
mcm$set(matrix( c ( 1, 2, 2, 1 ), 2, 2))
cacheSolve(mcm) 


## Creates a cash memory to store the matrix value using the lexical scope.
makeCacheMatrix <- function(x) {
  
  # Variable to store cash value
  cacheStorage = NULL
  
  #set the value of the vector
  set = function(val) {
    #Check if val is a matrix
    if(!is.matrix(val)){
      try(throw("Invalid argument type")); print("makeCacheMatrix.set accept a matrix parameter");
    }
    
    matrixValue <<- val
    cacheStorage <<- NULL
  }
  
  get <- function() matrixValue
  setMatrix <- function(inverse) cacheStorage <<- inverse
  getInverse <- function() cacheStorage
  
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## Calculate the inverse of the matrix if cash value is not available 
## else calculate and assign to cash
cacheSolve <- function(x, ...) {
  # Get the cashed value from the makeCacheMatrix
  cache <- x$getInverse()
  
  #Return the inverted cashed metric if true
  if (!is.null(cache)) {
    message("Value from Cash")
    return(cache)
  }else{
    # Create matrix
    matrixValue <- x$get()
    #Computing the inverse of the square matrix
    cacheStorage <- solve(matrixValue, ...)
    x$setMatrix(cacheStorage)
    message("Value added to Cash")
    return (cacheStorage)
  }
}
