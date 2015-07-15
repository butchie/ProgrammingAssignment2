## This set of functions chhecks whether the inverse of a matrix has been stored into the cache. If it has, it 
##retrieves the inverse matrix. If not, the inverse is calculated via the solve() function and strored in the cache

## creates a list to set/get the matrix entries and set/get the inverse, along with the matrix entries

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
  
## check to see whether inverse has been calculated, if not calculate and store, if so return inverse

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
