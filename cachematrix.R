## makeacheMatrix is a Parent function/environmnt containing 
## (i) a variable for storing inverse of given matrix. 
## (ii) get and set functions for the inverse variable
## a list containing the set/get functions so that those functions can be made
## available to the caller of makeacheMatrix as a list of funtions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inver) inv <<- inver
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##CacheSolve gets a list of functions as an argument. 
##That list is created by makeCacheMatrix Function thatalso provides envronent to cache inverse of the matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
