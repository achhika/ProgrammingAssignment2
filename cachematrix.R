## These functions create a cached matrix object
## allowing a user to calculate matrix inverse once and cache it
## rather than recomputing each time.

## This function creates a matrix object with the ability to cache the inverse.

##Initialize an empty matrix and then setting and caching a matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##returns a list of functions
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix()
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## This function on the first call 
## will solve the inverse of a "cached matrix" object
## return the inverse and store that inverse in the cache 
## on the subsequent calls it retreives the cache rather than recomputing
## the cache retrieval is flagged by the print statement

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##If the matrix is empty, then the inverse of the matrix is computed and returned
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
