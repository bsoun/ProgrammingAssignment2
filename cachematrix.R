## This function creates a special "matrix" object that can cache its inverse.
## This function can only calcute the inverse of a squarred matrix (solve function used)


## makeCacheMatrix creates a special "matrix", which is really an enveloppe
makeCacheMatrix <- function(x = matrix()) {
  inv <- "No value inverse matrix" 
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## If the matrix has not changed and inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
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