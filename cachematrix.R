##The function, makeCacheMatrix creates a special "matrix",
##which is really a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse matrix
##    get the value of the inverse matrix

makeCacheMatrix <- function(thisMatrix = matrix()) {
	thisMatrixInverse <- NULL
	set <- function(inputMatrix) {
		thisMatrix <<- inputMatrix
		thisMatrixInverse <<- NULL
	}
	
	get <- function(){
		return(thisMatrix)
	}
	
	setInverse <- function(inverse){
		thisMatrixInverse <<- inverse
	}
	
	getInverse <- function(){
		return(thisMatrixInverse)
	}
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Set the inverse matrix (if not already set) then,
## return the inverse matrix of 'myMatrix'
cacheSolve <- function(myMatrix, ...) {
	inverseMatrix <- myMatrix$getInverse()
	if(!is.null(inverseMatrix)) {
		message("getting cached data")
		return(inverseMatrix)
	}
	thisMatrix <- myMatrix$get()
	inverseMatrix <- solve(thisMatrix, ...)
	myMatrix$setInverse(inverseMatrix)
	return(inverseMatrix)
}
