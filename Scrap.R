makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Makes special vecotr.
## Set value of vector
## Get the value of vecotr
## Set the value of vector
## Get the value of the vector

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Checks if mean has been calculated.  
## If so, get mean from cache and skips calculation
## If not, calculates mean and caches it

crazy <- function() {
  x <<- 3.14
  print(x)
  { print(x);
  x <- 42; print(x)
  }
  print(x)
}
