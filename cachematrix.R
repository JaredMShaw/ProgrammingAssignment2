## makeCacheMatrix builds a list of functions that
## store and retrieve a passed value from memory

##execute with 
## mx <- matrix(c(1,5,44,8),ncol=2)
## x <- makeCacheMatrix(mx)
## cacheSolve(x)

makeCacheMatrix <- function(x = vector()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  #store the functions in a named vector
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##cacheSolve checkes if an object is stored in memory
##and returns it if it exists,
##otherwise, it finds the inverse value of the matrix,
##caches it, and returns the value

cacheSolve <- function(x,...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  d <- x$get()
  m <- solve(d,...)
  x$setinverse(m)
  m
}

