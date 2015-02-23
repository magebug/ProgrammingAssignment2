## R function that is able to cache potentially time-consuming computations. 

## makeCacheMatrix: 
## creates a special "Matrix", which is really a list containing a function to
## "set" function - sets the value of the matrix
## "get" function - gets the value of the matrix
## "setInverse" - sets the value of the inverse
## "getInverse" - gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL

	#set function
	set <- function(y) {
		x <<- y
		inverseMatrix <<- NULL
	}
	
	#get function
	get <- function() {
		x
	}
	
	#setInverse function
	setInverse <- function(inverse) {
		inverseMatrix <<- inverse
	}
	
	#getInverse function
	getInverse <- function() {
		inverseMatrix
	}
	
	#return list of functions
	list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}


## cacheSolve:
## if matrix is not cached then get data, inverse data, cache data
## else return cached inverse

cacheSolve <- function(x, ...) {

	inverseMatrix <- x$getInverse()
	
	if(is.null(inverseMatrix)) {
		#get matrix data
		data <- x$get()
		
		#calculate the inverse
		inverseMatrix <- solve(data, ...)
		#cache the inverse of the matrix
		x$setInverse(inverseMatrix)
	}
	## Return a matrix that is the inverse of 'x'
	inverseMatrix
}
