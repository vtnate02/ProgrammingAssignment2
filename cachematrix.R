##FUNCTIONSET PURPOSE
##Compute the inverse of a matrix once, and cache it rather than rerun the computation each time

## Function 1 - makeCacheMatrix
##Define list of functions to evaluate/retrieve matrix and its potentially cached inverse
makeCacheMatrix <- function(x = matrix()) {
  ## 1-Define evironment object for inverse
  mat_inv <- NULL
  ## 2.1-Define set matrix function
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  ## 2.2-Define get matrix function
  get <- function() x
  ## 2.3-Define store inverse function
  setinv <- function(inverse) mat_inv <<- inverse
  ## 2.4-Define cached inverse retrieval function
  getinv <- function() mat_inv
  ## 3-Return the list of functions
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
##Function 2 - cashSolve
##Looks to see if inverse has already been cached
##if yes, pull from cache, if no compute and return
cacheSolve <- function(x, ...) {
  ##Get cached inverse value using getinv function
  mat_inv <- x$getinv()
  ##Is inverse null?
  if(!is.null(mat_inv)) {
    ##Y ->Call getinv function to retrieve cached inverse
    message("Retrieved from Cache")
    return(mat_inv)
  }
  ##N ->
  ## 1-Call Get function to retreive matrix
  temp <- x$get()
  ## 2- Use solve() to take matrix and return inverse
  mat_inv <- solve(temp) 
  ## 3- Cache inverse
  x$setinv(mat_inv)
  ## 4- Return inverse
  mat_inv
}
