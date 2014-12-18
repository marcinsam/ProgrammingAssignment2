# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  cm <- NULL
  set <- function(y) {
    x <<- y
    cm <<- NULL
  }
  
  get <- function() x
  setcm <- function (cachematrix) cm <<- cachematrix
  getcm <- function() cm
  list(set = set, get = get, setcm = setcm, 
       getcm = getcm)
}


# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the
# cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cm <- x$getcm()
  if(!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  cmdata <- x$get()
  cm <- solve(cmdata)
  x$setcm(cm)
  cm
}