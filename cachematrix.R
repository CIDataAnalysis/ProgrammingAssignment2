## These two functions 1) create a special "matrix" object that can cache its inverse, and
## 2) look up the value of the special matrix or (if not previously cached) computes and caches it.

## The function "makeCacheMatrix" receives a invertible matrix as input. The function creates a list that 
## a) sets the matrix, b) gets the matrix, c) sets the inverse of the matrix, and 
## d) gets the inverse of the matrix.


  minv <- NULL
  set <- function(y){
    x <<- y
    minv <<- NULL
  }
  get  <- function() x
  setinverse <- function(solve) minv <<- solve
  getinverse <- function() minv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## The function "cacheSolve" receives an object of the type created by the function "makeCacheMatrix."
## It checks to see whether the inverse of the matrix input into "makeCacheMatrix" has already been calculated.
## If so, it retrieves and returns the inverse; otherwise, it calculates the inverse and 
## sets the inverse in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinverse()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data  <- x$get()
  minv  <-  solve(data, ...)
  x$setinverse(minv)
  minv
}