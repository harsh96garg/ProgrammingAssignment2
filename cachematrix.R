## These functions create a special "matrix" and caches its
## inverse

## This function creates a special "matrix", which is really a list containing four functions
makeCacheMatrix <- function(x = matrix()) {
		 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##This function calculates the inverse of the special "matrix", by
## first, check its value from memory; and if it is not there, by
## calculating the inverse, storing it in memory and returning it
 
cacheSolve <- function(x, ...) {
 	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } else{
        data <- x$get()
         m<- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
	}
}

