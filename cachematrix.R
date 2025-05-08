## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y){
      x <<- y
      j <<- NULL
    }
    get <- function()x
    setInverse <- function(inverse) j <<- inverse
    getInverse <- function() j 
    #list the names of all methods that will be known to the outside world
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

#solve(B) - if impossible
B <- matrix(c(1:4),2,2)
B1 <- makeCacheMatrix(B)
#Return inverse after computation
cacheSolve(B1) 
#inverse returned from cache
cacheSolve(B1)
