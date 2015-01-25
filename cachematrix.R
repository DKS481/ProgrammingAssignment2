## D Smith Johns Hopkins R Programming Coursera Programming Assignment 2
## The below functions are used to cache the inverse of a matrix


## A List containg functions to 1) Set the Matrix, 2) Get the Matrix, 3)Set the Inverse, 4) Get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) inv <<- inverse
  getmatrix <- function() inv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Checks if inverse has already been solved for, if not it calcuates the inverse, else it returns cached data.

cacheSolve <- function(x, ...) {
  inv <- x$getmatrix()
  if(!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmatrix(inv)
  inv
}
