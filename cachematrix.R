##I am using two functions, makeCacheMatrix() and cacheSolve() to calculate the inverse matrix of a give matrix, 
##based on Object-Oriented Programming(OOP), which is efficient using the caching feature of R

## Based on the example given in the assignment question, I did a few crucial modifications to get the
## desired the result.
## As for the first function, it is designed to become a place where storing information now (in itself) or later ( in cacheSolve() function） that we are going to refer in cacheSolve() later 
## within this function I defined four internal functions, set(), get(), setsolve()and getsolve().
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Based on the example given in the assignment question, I did a few crucial modification to get the
## desired the result.
## As for the second function, it is designed to assess the internal functions defined in the first funtion
## whithin this function I firstly assess the getsolve()function which defined in previous function, then I
## use the if() to figure out whether the targeted matrix is calculated by R before or not. If it is repeated
## matrix, the second function will just caching the data from the "the storage device" , function 1,to returen the 
## result, inverse matrix. However, if the targetd matrix is a new matrix which haven't been calculated or assessed before
## then, the secondfunction, cacheSolve, will start to calculate the inverse matrix of this new targeted matrix
## and mark it as an assessed matrix through the function setsolve(), which is the situation I previously mentioned "later storing information situation"

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setsolve(i)
  i}
