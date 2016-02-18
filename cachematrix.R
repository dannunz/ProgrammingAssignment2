##  This program computes and caches the inverse of a matrix. If the inverse has already been computed 
## it reads the inverse from the cache instead of computing it

## This function caches the inverse of the matrix X

makeCacheMatrix <- function(X = matrix()) {
  M <- NULL
  set <- function(y) {
    # assignment operator <<- allows variable to persist in 
    # the function closure environment
    X <<- y
    M <<- NULL
  }
  # accessed variable using get  
  #Note get looks up the chain of parents until it finds a binding 
  get <- function() X
  setinv <- function(solve) M <<- solve
  getinv <- function() M
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function checks to see if the inverse is already computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(X=matrix(), ...) {
    M <- X$getinv()
    if(!is.null(M)) {
      message("getting cached data")
      return(M)
    }
    
    data <- X$get()
    m <- solve(data, ...)
    X$setinv(M)
    M
  }
}
