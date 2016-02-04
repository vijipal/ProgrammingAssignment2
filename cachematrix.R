## The functions below help us to compute the inverse of a matrix. 
# When the inverse of a matrix is calculated it is cached so any 
#further calls for inverse takes it from cache instead of computing again. 

## this function is a list containing 4 functions, set, get, getInv and setInv

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}



#computes the inverse of a matrix and returns it. If cached version of inverse 
#already present, function does not compute but returns the cached result

cacheSolve <- function(x, ...) {
# Returns a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
