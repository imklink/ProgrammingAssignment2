## makeCacheMatrix
## Creates a special "matrix" object that saves the matrix x, and its inverse, inv.
## The returned list object has the following methods:
## set: sets matrix x, and resets inverse matrix inv
## get: returns matrix x
## setInv: saves the inverse matrix that was created with 'Solve'
## getInv: returns cached value of inverse matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		
		get <- function() x
		setInv <- function(inverse) inv <<- inverse
		getInv <- function() inv
		list(set=set, get=get, setInv=setInv, getInv=getInv)
		
}


## Function operates on the object created by makeCacheMatrix.
## cacheSolve takes this object as argument 'x'. The function returns
## the cached value of the matrix if it exists, otherwise it calculates
## the inverse, 'inv', with the 'solve' function and returns this value using
## 'setinv'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
        
}