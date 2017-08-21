## The first function, makeCacheMatrix creates a special "matrix"
## The second function, cacheSolve calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function.


## makeCacheMatrix creates a special "matrix" which is really a list containing a function to
##### 1) set the value of the matrix
##### 2) get the value of the matrix
##### 3) set the matrix inverse
##### 4) get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      inv.x <- NULL
      set <- function(y){
            x <<- y
            inv.x <<- NULL
      }
      get <- function() x
      setinv <- function(x_inv) inv.x <<- x_inv
      getinv <- function() inv.x
      list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the matrix inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
      x.inv <- x$getinv()
      if (!is.null(x.inv)){
            message("getting cached data")
            return(x.inv)
      }
      data <- x$get()
      x.inv <- solve(data,...)
      x$setinv(x.inv)
      x.inv
}
