##We create an R function that is able to cache potentially time-consuming computations.In the particular case, we write functions which cache the inverse of a matrix.
##We actually write two funtions namely "makeCacheMatxix" & "cacheSolve". The first funtion creates a special matrix while the second function efficiently returns the inverse of the particular special matrix if it already exists(computed) i:e  it `get`s the inverse from the cache and skips the computation or else it first computes the inverse of the special matrix and then cache the same.


## The first function, "makeCacheMatrix" creates a special "matrix".Its a list of several functions required to obtain the special matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix"
##created with the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
