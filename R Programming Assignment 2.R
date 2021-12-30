#library MASS allows for the calculation of non-square matrices 
library(MASS)

#this portion of the function creates the cache matrix#

makeCacheMatrix <- function(x=matrix()){
	inv <- NULL
	set <- function(y){
		x <<- y 
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

#this portion is used to get the cache data
cacheSolve <- function(x,...){
	inv <- x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat <-x$get()
	inv <- solve(mat,...)
	x$setInverse(inv)
	inv
}