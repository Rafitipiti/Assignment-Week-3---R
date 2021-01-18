
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

	########################
	
	x$setinv(inv)
      inv

}

a <- sample(1:10,1)
b <- sample(1:10,1)
vec <- sample(1:1000,a*b)
x <- matrix(vec, nrow = a, ncol = b)
x
makeCacheMatrix(x)


