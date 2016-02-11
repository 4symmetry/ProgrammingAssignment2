## This script takes a square invertible matrix, calculates its inverse and 
## caches the result. 

## makeCacheMatrix creates a list of functions that set and get
## the matrix, set the solved/inverted matrix and get the solved/
##inverted matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) inv <<- inverse
  getsolve <- function() inv
  list(set =set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## cacheSolve uses the created list of functions in makeCacheMatrix to calculate the inverse
## of the matrix stored by makeCacheMatrix$set to calculate the inverse x$getsolve
## and cache it. Before it calculates the inverse, though, it checks if
## an inverse has already been calculated and cached in inv. If so, it skips
## the calculation, says "getting cached data" and retrieves the cached inversed matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getsolve()
    if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setsolve(inv)
    inv
}
