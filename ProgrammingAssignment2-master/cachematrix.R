## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set the value of matrix x and turn inv to null
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  #get the matrix
  get <- function() x
  #set the value of the inverse of matrix x
  setinv <- function(inverse) inv <<- inverse
  #get the inverse
  getinv <- function() inv
  #put the functions above into a list
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #get the inverse of matrix x
  inv <- x$getinv();
  #check whether the inverse of x has already been cached
  #if it has been cached, then return the value inv
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  #if the inverse of matrix x was not cached
  #then calculate its inverse and cache the value via fuction setinv
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
