## Programming Assignment 2 of R Programming on Coursera by Jason Huang.
## This file includes 2 functions, 
## One to make a special matrix that can cache it's inverse matrix.
## The other is to calculate the inverse of the special matrix,
## It the inverse has been in the cache, get it from cache directly,
## if not, caculate and cache it.

## Input a matrix and return a special including four functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y #set the matrix to y
    inv <<- NULL #set the cached inverse to NULL because the matrix has changed
  }
  get <- function() x #get the matrix content
  setinv <- function(inver) inv <<- inver #directly set the inverse in cache
  getinv <- function() inv #get the cached inverse.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Input a special matrix which made by makeCacheMatrix(),
## and then output the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  #if there is cached inverse in x, directly return the cached data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #if no cached data, calculate it by calling solve()
  data <- x$get()
  inv <- solve(x$get())
  x$setinv(inv) #cache the result into x
  inv #return the inverse matrix result
}
