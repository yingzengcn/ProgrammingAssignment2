## R homework2, inverse matrix

## Cache matrix function

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y){
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(inverse) invm <<- inverse
  getinvmatrix <- function() invm
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## do the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm =x$getinvmatrix
  if(!is.null(invm)){
    message ("getting cached inverse matrix data...")
    return(invm)
  }
  
  ## otherwise, use slove function to do the inverse
  mat.data = x$get()
  invm =slove(mat.data,...)
  
  x$setinvmatrix(invm)
  return(invm)
}
