## The first function below will create a special matrix 
## that can cache its inverse.  The second one will either
## a) calcualtes the inverse of the first special matrix or
## b) retrieve the cache inverse if it has been calculated.  Both of these 
## functions are from the programming assignment with only a couple of words changed.

## First function creates a list of functions that will
## set the value of the matrix, get the value of the matrix,
## set the value of the inversed matrix, and get the value of the
## inversed matrix.  

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y 
    m<<-NULL 
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function will calculate the inverse of the special matrix.  
## This function first: gets the cached inversed matrix if it has already been calculated.
## if there is no cache inversed matrix, then it calculates the inverse matrix and caches it.


cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

