# This function creates a special "matrix" object that can cache its inverse.
# It takes x, an invertible (square) matrix, as argument and returns a list
# of accessor functions for that matrix.
# 
# Specifically, it returns functions to:
#         Set and return the specified matrix
#         Set and return the inverse of the specified matrix

# If no matrix is specified in the function call, 
# the default argument is a "random" diagonal 3x3 matrix.
makeCacheMatrix <- function(x = matrix(  c(sample(1:10, 1), 0, 0,
                                           0, sample(1:10, 1), 0,
                                           0, 0, sample(1:10, 1) ), ncol = 3)) {
        inv <- NULL     #inverse is set to NULL 
                        #(the inverse hasn't been calculated yet)
        
        ##------------Accessor Functions-------------------------------##
        #setter function that overwrites the previously stored matrix
        #and caches its value into x
        set <- function(y) {
                x <<- y         
                inv <<- NULL    #reset the value for inv since 
                                #we haven't calculated it for this new matrix
        }
        
        get <- function() x     #returns the currently stored matrix
        
        #setter function that overwrites the previously stored inverse
        #matrix and caches its value into inv
        setInverse <- function(inverse) inv <<- inverse
        
        #returns the currently stored inverse matrix
        getInverse <- function() inv
        ##-------------------------------------------------------------##
        
        #return value for this function; a list with named elements containing 
        # the above accessor functions
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)
} #end makeCacheMatrix


# This function computes the inverse of the special "matrix" object returned by 
# the makeCacheMatrix function. If the inverse has already been calculated 
# (and the matrix has not changed), then the inverse from cache is returned.

# Note that the first argument x is a list of accessor function for the matrix 
# of concern, not the matrix itself.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()   #retrive the currently stored inverse matrix
        
        #if inv already contains an inverse matrix, return it 
        #and end the function call
        if (!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        
        #otherwise retrieve the currently stored matrix, calculate its inverse,
        #store that inverse matrix, and return it
        mat <- x$get()
        inv <- solve(mat)
        x$setInverse(inv)
        inv
}#end cacheSolve