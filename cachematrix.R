## Creates a cached copy of the inverse of a matrix
## x - a square invertible matrix
## returns a list that can function as a cached matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Creates inverse of a matrix
## Will return from the cache if previously assigned
## x - a makeCacheMatrix object
## Return a matrix that is the inverse of the matrix in 'x'
##
## Ex. 
##a <- matrix(c(9,1,3,6,13,11,7,0,5,7,4,7,2,6,1,10),4,4)
##b <- makeCacheMatrix(a)
##c <- cacheSolve(b) builds the inverted matrix, loads to cache and returns it
##d <- cacheSolve(b) returns inverted matrix from cache
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
