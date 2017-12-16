## The first function, makeCachMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of inverse matrix
## get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matr) m <<- matr
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## it is the second part of the above function, it checks whether cache data in parent environment exists or not
## if exists use cache data, else calculate and set inverse matrix in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    
    m <- solve(data, ...)
    x$setmatrix(m)
    m
  }
