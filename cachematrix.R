## Inverting a matrix can be a CPU time consuming activity. 
## Especially when it has to be performed repeatedly.

## The first function, makeVector creates a special 
## "vector", which is really a list containing a 
## function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	Get <- function() x
	Setmatrix <- function(solve) m <<- solve
	Getmatrix <- function() m
	list(set=set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}

## The second function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	m <- x$getmatrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	matrix <- x$mget()
	m <- solve(matrix, ...)
	x$setmatrix(m)
	m
}
