## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # check to see if the input matrix data can be inverted
        if(det(x)==0) {print("error: this matrix is not invertible (determinant equals zero). Use different input matrix.")}
        
        else {
                matrix.inverse_cache <- NULL # initialize matrix_cache variable for later use
                
                set_matrix.new <- function(matrix.new) { # provide a function that can specify new matrix if needed
                        x <<- matrix.new # x will be set to new matrix data
                        matrix.inverse_cache <<- NULL}
                
                get_matrix <- function() x # Return the value from input matrix "x"
                
                # function used to store cache data
                set_matrix_inverse <- function(cache_data) matrix.inverse_cache <<- cache_data
                get_matrix_inverse <- function() matrix.inverse_cache # return the value from matrix.inverse_cache
                
                list(set_matrix.new=set_matrix.new,
                     get_matrix= get_matrix, 
                     set_matrix_inverse = set_matrix_inverse,
                     get_matrix_inverse = get_matrix_inverse) # return the list of all 4 functions
                }
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        get_matrix_inverse <- x$get_matrix_inverse() # execute the "get_matrix_inverse" function
                                                     # which stores the get_matrix_inverse data in cache
        
        if(!is.null(get_matrix_inverse)) { # check if there is data stored in get_matrix_inverse
                message("getting cached data")
                return(get_matrix_inverse)
        }
        data <- x$get_matrix() # get the data from the makeCacheMatrix object 
        matrix.inverse_cache <- solve(data, ...) # calculate the inverse of the matrix
        x$set_matrix_inverse(cache_data = matrix.inverse_cache) # add the calculated data to "makeCacheMatrix" object
        matrix.inverse_cache # print the value on the console
        
}


# examples of 3 by 3 matrix
x1 <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

my_matrix <- makeCacheMatrix(x1)
cacheSolve(my_matrix) 
x1 %*% cacheSolve(my_matrix) 

# example of 2 by 2 matrix
x2 <- matrix(c(1,2, 4,9), nrow = 2)

my_matrix <- makeCacheMatrix(x2)
cacheSolve(my_matrix) 
x2 %*% cacheSolve(my_matrix) 

# non-invertible matrix
x3 <- matrix(1:9, nrow = 3)
my_matrix <- makeCacheMatrix(x3)







