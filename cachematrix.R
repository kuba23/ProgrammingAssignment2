## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates matrix object that can cache its inverse 
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 

## Write a short comment describing this function
## I set input x as a matrix, solved value inv as null, changed references

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## similar changes to the first function

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
