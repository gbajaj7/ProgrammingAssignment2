## The below functions are meant to cache the inverse of a matrix and return the cached result if it maches the matrix
## Assumption: The matrix supplied is always invertible.

## Function: makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
              {
                z <- NULL
                set <- function(b) 
                  {
                      x <<- b
                      z <<- NULL
                  }
                get <- function() x

                set_inverse <- function(solve) z <<- solve

                get_inverse <- function() z

                list(set = set, 
                     get = get,
                     set_inverse = set_inverse,
                     get_inverse = get_inverse)
              }

## Function: cacheSolve
## This function computes the inverse of the invertible matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the function 
## returns the inverse from the cache.

cacheSolve <- function(x, ...) 
              {
		## Return a matrix that is the inverse of 'x'
                t <- x$get_inverse()
                if(!is.null(t)) 
                  {
                      message("Got the data from cache!")
                      return(t)
                  }
                data <- x$get()
                t <- solve(data, ...)
                x$set_inverse(t)
		message("Inverse of the given matrix is ")
                return(t)
              }

