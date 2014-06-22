## These functions are able to get a matrix and calculate its inverse

## The first function caches a matrix and get an inverve, througth
## two chanels. The first one, hte inverse is set by the user, 
## The second one, the inverse is calculate by the function.
## Pay attention that for the orignal matrix is used the parameter x, 
## and for the inverse is used the parameter m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  makeinverse <-function(data){
    inverse<- solve(data)
    m<<- inverse
  }
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       makeinverse= makeinverse,
       getinverse = getinverse)
}


## This function takes a list were is set a matrix and check, 
## if the inverse of the matrix is set, then just get that inverse, 
## if the nverse is not set, the function calculates it.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
