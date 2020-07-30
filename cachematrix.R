## The pair of functions given below are used to cache the inverse of a matrix


## The function 'makeCacheMatrix' is creates a special matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  get <- function() x
  getInvr <- function() {
    if(length(x)%%sqrt(length(x))==0) { ## to ensure matrix is a square matrix.
      invr <<- solve(x) ##solve() function is used to calculate the inverse.
    }
  }
  list(get = get, getInvr = getInvr)

}


## The function 'cacheSolve'computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(!is.atomic(x)){      # to check if the given parameter is atomic
    invr <- x$getInvr()
    if(!is.null(invr)) {
      message("getting  the cached data")
      return(invr)
    }     
  } else {        # if not atomic,then convert.
    message("getting the inverse as no cached data was found")
    return(makeCacheMatrix(x)$getInvr())
  } 
  
}
