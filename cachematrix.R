## These 2 fucntion provide a mechanism & matrix 
## which can store it's inverse. By doing so, this 
## special can save time of caculating matrix's inverse
## repeatedly. 

## This function creates a special "matrix" object 
## that can cache its inverse.
## This function will return a list which contain 
## 4 fucntions
##
## set the value of the matrix
## get the value of the matrix
## setsolve the value of the inverse
## getsolve the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	r <- NULL
	
	set <- function(y) {
		x <<- y
		r <<- NULL
	}
	
	get <- function() x
	
	setsolve <- function(solve) r <<- solve
	
	getsolve <- function() r
	
	list(set = set, get = get, 
		 setsolve = setsolve,
		 getsolve = getsolve)
}


## This fucntion calculates the inverse of the special "matrix" 
## created by makeCacheMatrix.  
## However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setmean 
## function.

cacheSolve <- function(x, ...) {
         
	r <- x$getsolve()
	
	if(!is.null(r)) {
		message("getting cached solve")
		return(r)
	}
	
	data <- x$get()
	r <- solve(data, ...)
	x$setsolve(r)
	r
}
