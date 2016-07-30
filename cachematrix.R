# This function creates a special "matrix" object that can cache its inverse.
# It takes x, an invertible (n-by-n) matrix, as argument and returns a list
# of accessor function for that matrix.
# 
# Specifically, it returns functions to:
#         Set and return the specified matrix
#         Set and return the inverse of the specified matrix

# If no matrix is specified in the function call, 
# a random 3x3 matrix is generated.
makeCacheMatrix <- function(x = matrix(rep(sample(1:5, 1), 9), ncol = 3)) {
        inv <- NULL     #inverse is set to NULL 
        #(the inverse hasn't been calculated yet)
        
        #setter function that overwrites the previously stored matrix
        #and caches it's value into x
        set <- function(y) {
                x <<- y
                inv <<- NULL    #reset the value for inv since 
                #we haven't calculated it for this new matrix
        }
        
        get <- function() x     #return the currently stored matrix
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" object returned by 
# the makeCacheMatrix function. If the inverse has already been calculated 
# (and the matrix has not changed), then the inverse from cache is returned.

# Note that the first argument x is a list of accessor function for the matrix 
# of concern, not the matrix itself.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setInverse(inv)
        inv
}