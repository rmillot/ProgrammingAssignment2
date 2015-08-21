## There are two functions created with the goal being the caching of an inversion of a
## matrix

## The makeCacheMatrix function creates a matrix in the working environment
## then inverts the matrix and stores it in cache to be used by Solve function later on

makeCacheMatrix <- function(x = matrix()) {
      cache <- NULL
      set <- function(y)  {
      	x <<- y
      	cache <<- NULL
      	}
      get <- function() x
      setmatrix <- function(inverse) cache <<- inverse
      getinverse <- function () cache
      list(set = set, get = get, setmatrix = setmatrix, getinverse = getinverse) 	
}


## The cacheSolve function creates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix already exists in the cache, it does not create a new matrix
## If the inverted matrix does not exist in the cache, it is created in the 
##  working environment and the inverted value is stored in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getinverse()
        if (!is.null(cache))  {
        	message("getting cached data")
        	return(cache)
}
		matrix <- x$get()
		cache <- solve(matrix, ...)
		x$setinv(cache)
		return(cache)
}		
