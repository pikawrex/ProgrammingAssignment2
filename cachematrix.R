## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makes a matrix object with functions of setting a solved inverse, or getting the stored inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	# get returns the x initially used to define makeCacheMatrix object
	get <- function()x
	# setInverse solves for the inverse of matrix x, then stores the solution to inverse variable within object
	setInverse <- function(solve) inverse <<- solve
	# getInverse returns the stored inverse value within object
	getInverse <- function() inverse
	#returns a list of functions to be associated with matrix object
	list(set=set, get= get, setInverse = setInverse, getInverse = getInverse)	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	# prints message and returns inverse if cached inverse has been calculated previously
	if(!is.null(inverse){
		print("getting cached inverse")
		return(inverse)
	}
	# returns data initially used to make x(makeCacheMatrix object)
	data <- x$get
	# use inbuilt solve function to solve for inverse of data
	inverse <- solve(data,...)
	# use setInverse function within object to store solution of inverse calculated
	x$setInverse(inverse)
	inverse
}
