## Ali Hmaou - 26/07/2015
## This script is part of the Programming Assignment 2 of the Coursera R Programming course.
## It illustrates the implementation of a caching pattern.



# DEBUG : cleaning up the workspace
rm(list=ls())


## makeCacheMatrix turns a matrix into a list containing getters and setters, can be seen as a cacheable matrix. 
## The inverted matrix is null as long as it is not set with the setInvertedMatrix function.

makeCacheMatrix <- function(simpleMatrix = matrix()) {
      
      ## instanciating invertedMatrix with null
      invertedMatrix <- NULL
      
      ## setter for the matrix
      set <- function(otherSimpleMatrix) {
            ## the '<<-' operator allows to write in the global environment
            simpleMatrix <<- otherSimpleMatrix
            invertedMatrix <<- NULL
      }
      
      ## getter for the matrix
      get <- function()
            simpleMatrix
      
      ## setter for the inverted matrix 
      setInvertedMatrix <- function(invertedMatrix)
            ## the '<<-' operator allows to write in the global environment
            invertedMatrix <<- invertedMatrix
      
      ## getter for the inverted matrix
      getInvertedMatrix <- function()
            invertedMatrix
      
      ## returned list
      list(
            set = set, get = get, setInvertedMatrix = setInvertedMatrix, getInvertedMatrix = getInvertedMatrix
      )

}


## cacheSolve function is an encapsulation of Solve function, it returns the inverted matrix of a cacheable matrix.
## The cacheable matrix is a list object that is the result of makeCacheMatrix(on matrix).
## If the cacheablematrix$invertedMatrix has already been computed (eg. cacheablematrix$getInvertedMatrix is null) 
## then the result is not computed and is directly returned from the cacheablematrix$getInvertedMatrix function.

cacheSolve <- function(x, ...) {
      
      ## Reading the value of invertedMatrix from the cacheable matrix
      invertedMatrix <- x$getInvertedMatrix()
      
      ## Testing wether if the inverted matrix has already been calculated
      if (!is.null(invertedMatrix)) {
            message("getting cached data")
            # The call of the return function skips the computations and gives quickly the asked result
            return(invertedMatrix)
      }
      
      ## When then invertedMatrix is not yet computed
      
      ## Retrieving the matrix from the cacheable matrix to use it with the classic solve function
      data <- x$get()
      
      ## computation of the inverted matrix
      ## the assignment symbol "<<-" 
      invertedMatrix <- solve(data, ...)
      
      ## using the cacheable matrix setter to set the value of the invertedMatrix attribute
      x$setInvertedMatrix(invertedMatrix)
      
      ## Returns the inverted matrix freshly computed
      invertedMatrix
}


## Test with a sample matrix (90000 cases)
testCacheMatrix<-makeCacheMatrix(matrix(rnorm(10000),100,100))

## Testing calculation time ~4s
invertedTestMatrix<-solve(testCacheMatrix$get())

## Using de cacheSolve instead of Solve :
invertedTestMatrix<-cacheSolve(testCacheMatrix)
invertedTestMatrix<-cacheSolve(testCacheMatrix)
## message #Getting Cached data is well returned at the second execution :
# > invertedTestMatrix<-cacheSolve(testCacheMatrix)
# getting cached data

ls()
#[1] "cacheSolve"         "invertedTestMatrix" "makeCacheMatrix"    "testCacheMatrix"   