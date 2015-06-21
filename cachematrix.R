## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The function below is able to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  #set the matrix
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  #get the matrix
  get <- function() {
    m
  }
  #set how to invett the matrix
  setinv <- function(inverse) {
    i <<- inverse
  }
  #get the inverse 
  getinv <- function(){
    i
  } 
  #list of methods used
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
##Te function calculates the inverse of the matrix, resulted by the above function. But if there is
##already the inverse and the matrix is not changed the below function just keep the results from
##the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #return the inverse of the matrix x
  m <- x$getinv()
  if( !is.null(m) ) {
    
    message("getting cached data, it is already done the inverse")
    
    return(m)
  }
  #1) calculate the matrix from the our object,
  #2) get the inverse with the moltiplication
  #3) set the inverse to our m
  #4) return m
  data <- x$get()
  m <- solve(data) %*% data
  x$setinv(m)
  m
}
