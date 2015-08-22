## Put comments here that give an overall description of what your
## functions do
## These three functions handle the inversing of a Matrix with caching to improve performance
## 'makeCacheMatrix' --> Creates a Matrix that has ability to cache its inverse
## 'cacheSolve' --> Returns the inverse of a matrix , using the cache capabilities
## 'compareMatrix' --> Internal function that returns a boolean to indicate if two number matrices are eqaul or not


## 'makeCacheMatrix'
##  creates a list of functions to handle matrix with cache
## set(),get() --> set or get the matrix 
## setinverse(), getinverse()  --> set or get the inverse matrix in the cache/from the cache

makeCacheMatrix <- function(x = matrix()) {
	inversematrix <- NULL
	set <- function(y=matrix) {
                if(!is.matrix(y)) return ('Error:Argument should be a Matrix')
		    x <<- y
                inversematrix <<- NULL
     	}
	get <- function() x
	setinverse <- function(inverse) inversematrix <<- inverse
	getinverse <- function() inversematrix
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}

## 'compareMatrix'
## compare two matrices
## if they have the same nuber of rows and columns then it checks the numeric values
## it returns TRUE if the two matrices are the same
## it returns FALSE if the two matrices are not the same, either by dimensions or by the values

compareMatrix<-function (m1=matrix,m2=matrix) {

	if (nrow(m1) == nrow(m2) && ncol(m1) == ncol(m2)) {
		vdif <- abs(as.vector(m1)-as.vector(m2))
		sdif <- sum(vdif)
		return(sdif == 0)
	} else return(FALSE)
}

## 'cacheSolve'
## Return a matrix that is the inverse of the 'x' matrix previously created through the makeCachematrix function
## an optional matrix as argument can be passed; if it is passed it replaces the current matrix value
## It uses the cache if it is filled, and if the original matrix is the same as the optional argument passed,
## or, if no argument was passed and the inverse had already been calculated.
## Returns the inverse matrix
## prints a 'From the cache' message if the cache is used


cacheSolve <- function(x, ...) {

	if(!missing('...') && !is.matrix(...)) return('Error:optional argument should be either blank or a matrix')
	
	if(!missing('...'))
		if(!compareMatrix(x$get(),...)) x$set(...)
	
	matrix <-x$get()
	inverse<-x$getinverse()
	if(is.null(inverse)) {
		inverse <- solve(matrix)
		x$setinverse(inverse)
	} else print ("From the cache")
	inverse
}

