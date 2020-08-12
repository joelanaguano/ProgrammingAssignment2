## The functions do:

matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE) 
invmatrix <- makeCacheMatrix(matrix)
invmatrix

cacheSolve(invmatrix)


## This function creates a matrix that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


##  This function returns the inverse of the matrix makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv 
}
