##This function creates a special matrix object. 

##It attaches functions to the matrix object that allow
##the value of matrix to be set and fetched

##It also stores the inverse of the matrix in a variable
##that can be set and retrieved from another context

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){ 
    x
  }
  setInv <- function(inverse) {
    inv <<- inverse
  }
  getInv <- function() {
    inv
  }
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

##This function returns the inverse of a matrix

##It first tries to get the inverse from cache by using 
##the object returned by makeCacheMatrix

##And if retrieval from cache is not successful, it calculates
##inverse using the solve() function

##Once the inverse is calculated, the result is cached in object returned
##by makeCacheMatrix


cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  message("getting uncached data")
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
