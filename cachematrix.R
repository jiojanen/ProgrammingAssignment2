## makeCacheMatrix 
##  Creates a matrix object that may contain a stored matrix inverse.
## cacheSolve
##  Returns matrix inverse for the CacheMatrix-object by returning 
##  the stored inverse. If stored inverse does not exist, compute and store
##  the inverse.

## makeCacheMatrix creates a "CacheMatrix"-object, that has functions
## for getting and setting the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {

    # Initialize inverse matrix to null.
    inv_x <- NULL
    
    # Set function is used to set the matrix-class value and initialize 
    #  matrix inverse to null
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    
    # Return the matrix class value
    get <- function() x
    
    # Set the given inverse matrix into inv_x
    setinverse <- function(inverse) inv_x <<- inverse
    
    # Return stored inverse matrix
    getinverse <- function() inv_x
    
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Solve matrix inverse for a "CacheMatrix"-object. If stored inverse exist,
## return it. If stored inverse does not exist, compute inverse and store
## it to the CacheMatrix-object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    # Get inverse matrix from CacheMatrix object.
    inv_x <- x$getinverse()
    # If store inverse matrix is not null, return the value.
    if(!is.null(inv_x)) {
        message("Returning cached matrix inverse.")
        return(inv_x) 
    }
    
    # Following code is run only if stored non-null inverse matrix could 
    # not be returned.
    
    # Get the matrix-class data from CacheMatrix object.
    data <- x$get()
    # Solve inverse. As per assignment instructions, one may assume that 
    # all matrices are invertible square matrices.
    inv_x <- solve(data,...)
    # Store the computed inverse into the CacheMatrix object.
    x$setinverse(inv_x)
    
    # Return inverse matrix.
    inv_x
    
}
