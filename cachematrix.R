## The script cachematrix.R tries to retreive a 
## cached inverse of a matrix. If there is no cached value, 
## it will compute the inverse and store it in cache.


## To test the program, use the following example 
## [Input Matrix must be square and invertible]
## ==========================================================
## > mat1 <- matrix(rnorm(4), 2)
## > mat2 <- matrix(1:4,2)
## > cm1 <- makeCacheMatrix(mat1)
## > cacheSolve(cm1) # computes inverse value
## > cacheSolve(cm1) # returns inverse from cache
## > cm1$set_matrix(mat2)
## > cacheSolve(cm1) # computes inverse value for new matrix
## > cacheSolve(cm1) # returns inverse from cache
## ==========================================================


## The function makeCacheMatrix() creates a list of functions to 
## set/get a matrix, as well as to set/get the inverse of the matrix.
## Initially, the value of 'm_inv' will be NULL, and can be set by the user 
## with the help of the set_inverse() function. 

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set_matrix <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get_matrix <- function() {
                x
        }
        set_inverse <- function(inv_value) {
                m_inv <<- inv_value      
        }
        get_inverse <- function() {
                m_inv
        }
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse, get_inverse = get_inverse)
}


## CacheSolve() prints the matrix's inverse from the cache (if available).
## If not, it computes and prints the inverse of the given matrix, 
## and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$get_inverse()
        if(!is.null(m_inv)) {
                message("getting cached inverse data")
                return(m_inv)
        }
        data <- x$get_matrix()
        message("The computed inverse is")
        m_inv <- solve(data, ...)
        x$set_inverse(m_inv)
        m_inv
}
