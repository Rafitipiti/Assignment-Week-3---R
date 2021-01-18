
##The first function, makeVector creates a special "vector", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the inverse of the matrix
##get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
 	set <- function(y) {
              x <<- y
              inv <<- NULL
      }
      get <- function() x
      setinv <- function(inver) inv <<- inver
      getinv <- function() inv
	list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the 
## cache via the setinv function.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
      	message("getting cached data")
            return(inv)
      }
	data <- x$get()
	inv = solve(data, ...)
	x$setinv(inv)
      inv
}

## Test
vec <- c(-3,5,1,0)
x <- matrix(vec, nrow = 2, ncol = 2)
x
test <- makeCacheMatrix(x)
cacheSolve(test)
