## This function calculates the inverse of a matrix and stores the value of the inverse
## in a cache. If the same matrix is called again, the function simply returns the
## previously stored value of the inverse. If a new matrix is called, the function
## calculates the inverse of the new matrix, stores this new inverse in the cache,
## and returns the calculated value of the inverse.
##  makeCacheMatrix is a function that stores a list of 4 functions:getmatrix, 
##  setmatrix, setinverse, getinverse.

  makeCacheMatrix <- function(x = matrix()) {
## Initializing a variable inverse
  inverse <- NULL
  
## setmatrix substitutes the matrix x with y in the makeCacheMatrix function. 
## setmatrix also resets inverse to NULL because the old inverse of the old matrix 
## is not required.
  setmatrix <- function(y) {
  x <<- y
  inverse <<- NULL
  }
  
## getmatrix returns the input matrix in the makeCacheMatrix function.  
  getmatrix  <- function() x

## setinverse stores the value of the variable inverse calculated in the main function. 
  setinverse <- function(inv) inverse <<- inv

## getinverse returns the value of the inverse.
  getinverse <- function()inverse
  
## list stores the 4 functions in makeCacheMatrix
  list(getmatrix=getmatrix,setmatrix=setmatrix, setinverse=setinverse, getinverse=getinverse)
  }
  
## cacheSolve function returns the inverse of x. 
  cacheSolve <- function(x, ...) {
## if x$getinverse is NOT null, cacheSolve simply returns the stored value of inverse.
  inverse <- x$getinverse()
  if(!is.null(inverse)){
  message("getting cached data")
  return(inverse)
  }
## if x$getinverse is null (i.e.the inverse of the input matrix has not been calculated before)
## cacheSolve retrieves the matrix (x$getmatrix),
## calculates the inverse(solve()), 
## resets the value of the inverse in the cache(x$setinverse),
## and returns the inverse.
  mymatrix <- x$getmatrix()
  inverse  <- solve(mymatrix)
  x$setinverse(inverse)
  inverse
  }
