makeCacheMatrix <- function(x = matrix()) {
  #Assigns a new object to be used for the inverted matrix
  minv <- NULL
  
  # sets values when running. makes minv Null to overwrite a any pervious values
  set <- function(y){
    x <<- y
    minv <<- NULL
  }
  
  # gets the matrix
  get <- function() x
  #sets the inverse matrix to minv
  setinv <- function(inv) minv <<- inv
  #gets the inverse matrix
  getinv <- function() minv
  #elements are named in the a list so that we can use '$' extract
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

cacheSolve <- function(x, ...) {
  ## Pull the inverse matrix of 'x' and used cached inverse matrix if there
  minv <- x$getinv()
  if(!is.null(minv)){
    message("getting data from the cache")
    return(minv)
  }
  #if inversed matrix is not cached we run the solve function and set the inverse matrix
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  return(minv)
  
}