
## The inverse of a matrix can be cache in very long vectors 
## where the contents of a vector
## has not changed, in order for the mean of the matrix 
## to be looked up in the cache when needed instead of computing the mean repeatedly. 
## This function creates a special "matrix" MakeVector that cache it's inverse.


MakeVector <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set,
       get = get,
       setmean = setmean,
       getmean= getmean)
}


## This function calculates the inverse of the  "MakeVector"
## and also retrieve the inverse 
## from the cache in cases where the matrix has not changed and the inverse has been computed.

cachemean <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setmean(m)
  m
}


cachematrix <- MakeVector(matrix(7:10, 2, 2))

cachematrix$get()

cachematrix$getmean()

cachemean(cachematrix)

cachemean(cachematrix)


cachematrix$getmean()

cachematrix$set(matrix(c(6, 8, 3, 2), 2, 2))

cachematrix$get()

cachematrix$getmean()

cachemean(cachematrix)

cachemean(cachematrix)

cachematrix$getmean()



