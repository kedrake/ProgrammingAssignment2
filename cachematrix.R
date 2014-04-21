## The following functions cache the inverse of a matrix and then either return the cached
## matrix if it hasn't changed or find the inverse of the new matrix.
## **These functions assume the matrix given is invertable.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## The output is a list of functions that are used as arguments in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL     #assigns m null each time makeCacheMatrix is run, so cacheSolve knows it's a new matrix.
  set <- function(y){  #creates func where y gets set to x, & m=NULL
    x <<- y
    m <<- NULL
  }
  
  get <- function() x  #returns x(which is really y) when get is called
  setinverse <- function(inverse) m <<- inverse #changes value of m to inverse if the inverse has already been found.
  getinverse <- function() m   #returns m (which is inverse)
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## cacheSolve takes arguments from makeCacheMatrix and either returns the inverse 
## if it hasn't been found before or returns the cached inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {   #if there is a value in m, returns message and that value.
    message("getting cached data")
    return(m)
  }
  data <- x$get()   #if m is NULL, finds inverse of the matrix and returns this.
  m <- solve(data, ...)
  x$setinverse(m)  #puts m value in setinverse func, so it can be cached.
  m
}
