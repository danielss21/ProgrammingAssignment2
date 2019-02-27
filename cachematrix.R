## These functions works togheter to evaluate a inverse matrix of a given input.
## To save computational time and avoid calculate the inverse matrix evrery 
## time calling solve(), cacheSolve will just calculate the inverse if he 
## wasan´t calculated before.


## This function creates a list of functions, using lexical scoping 
## to cache the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inversem <- NULL
  set <- function(y) {
    x <<- y
    inversem <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inversem <<- solve
  getsolve <- function() inversem
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function takes the returned list of makeCacheMatrix, if the inverse has
## already been calculated then the function will retrieve the cached matrix, else
## cacheSolve will evaluate the inverse and return the solution.

cacheSolve <- function(x, ...) {
  inversem <- x$getsolve()
  if(!is.null(inversem)) {
    message("getting cached data")
    return(inversem)
  }
  data <- x$get()
  inversem <- solve(data, ...)
  x$setsolve(inversem)
  inversem
  ## Return a matrix that is the inverse of 'x'
}
