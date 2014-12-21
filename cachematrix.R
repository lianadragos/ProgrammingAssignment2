## Creates an object holding a matrix (input argument), its inverse 
## and the four methods get, set, getInverse, setInverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(Y) {
    x <<- Y
    inverse <<- NULL
  }
  get <- function() x
  setmatrix <- function(solved) inverse <<- solved
  getmatrix <- function() inverse
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Takes a makeCacheMatrix object as an argument, return the stored inverse if present 
## otherwise calculates the inverse, stores it and then returns it. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getmatrix()
  if(!is.null(inverse)) {
    message("getting cache data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setmatrix(inverse)
  inverse
}
