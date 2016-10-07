## This function creates a special "matrix" object that can cache its inverse



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        #set the value of the inverse matrix using solve function
        setInverse <- function(solve) m <<- solve
        #get the value of the inverse matrix
        getInverse <- function() m
        #make a list of the four functions above
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        # assign the inverse matrix to m
        m <- x$getInverse()
        # if has already computed inverse matrix, use the existing value
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        #assign the matrix to variable called data
        data <- x$get()
        #use solve function to inverse the matrix and assign that to value m
        m <- solve(data, ...)
        #set inverse matrix
        x$setInverse(m)
        m
}