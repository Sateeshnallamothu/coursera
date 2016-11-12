## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the cache variable 
  matinv = NULL
  ##  setmat to store the matrix in the environment other than local. use <<- operator
  ## but this function is obsolete as we never going to call. we can still use the main function with 
  ## out setmat().
  setmat <- function(y) {
    x <<- y
    matinv <<- NULL
  }
   
  ## getmat will retrieve the matrix from memory
  getmat <- function() x
  ## setinv will set store the matrix inverse in memory 
  setinv <- function(inv) matinv <<- inv
  ## getinv will retrieve the saved matrix inverse
  getinv <- function() matinv
  ## function output... last stmt to be executed will be returned.  
  ## use above defined function names as the list element names. 
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## first, check if the inverse is already saved or not.
  ## if not saved in mem, then the output will be NULL. use is.null to check for true or flase
  ## if saved , then simply return it from the mem using getmat()
  ## if not saved, then find the inverse using 'solve()' function and save in memory.
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    message("getting cached inverse matrix")
    ##  return will return the object.
    return(matinv)
  }
  mat <- x$getmat()
  matinv <- solve(mat, ...)
  x$setinv(matinv)
  return(matinv)
}
