## Function allows for the storage and calling of a matrix inversion in order
## to prevent from constant computation of matrix inverse.

## Function will allow multiple functions involving the inverse matrix to be run, and called when needed 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL							
	set <- function(y) {				## Changes the matrix stored in to 
		x <<- y					## a new input
		m <<- NULL					## Then clears the previous cached matrix
	}
	get <- function () {				## Calls the matrix that was stored
		x
	}
	setinverse <- function(solve) {		## Store the Value of Inverse into m
		m <<- solve
	}
	getinverse <- function() {			## Calls the inverse matrix stored as m 
		m
	}
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## List of all the functions that can be called from this function
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()					## Stores the inverse matrix from the input matrix into m
	if(!is.null(m)){						## If m already has a value will return a message and the matrix
		message("getting cached data")
		return(m)
	}
	data <- x$get()						## If not it will store the matrix from makeCacheMatrix initial input
	m <- solve(data, ...)					## Will inverse the matrix
	x$setinverse(m)						## Then store the new inverted matrix as m, and then return the value
	m
}
