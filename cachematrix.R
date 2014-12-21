## Put comments here that give an overall description of what your
## functions do

## That function save in the cache the inverse of the matrix
## it return a list of set, get, setsolve and getsolve subfunction

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
    	set <- function(y) {
        	x <<- y
        	m <<- NULL
    	}
    	get <- function() x
    	setsolve <- function(solve) m <<- solve
    	getsolve <- function() m
    	list(set = set, get = get,
         	setsolve = setsolve,
         	getsolve = getsolve)}


## that function return the inverse of a matrix
## but it test it before to know if the inverse 
## was previously saved in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    	m <- x$getsolve()
    	if(!is.null(m)) {
        	message("getting cached data")
        	return(m)
    	}
    	data <- x$get()
    	m <- solve(data, ...)
    	x$setsolve(m)
    	m
}
