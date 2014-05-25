## Author:  TS Johns
## Date:    2014-05-25
## About:   This package conatins implementation of Programming Assigment 2, for the R Programming course (aprt of John Hopkins - Data science Track)
##          The R package contains two functions used for storing an in-memory cached version of teh function
## Credits: This package was produced as a clone of the repo https://github.com/rdpeng/ProgrammingAssignment2
##
## Included Functions:
## - makeCacheMatrix
## - cacheSolve


## This function takes a matrix as input and utilizes internal functional variable to store in memory cache of inverse
## Function returns list of the following functions, which can be used against the returned object
## - set
## - get
## - getInverse
## - setInverse
makeCacheMatrix <- function(x = matrix()) {
  ##Internal variable used to store cached matrix inverse
  ##Note: Variable is initialized to NULL at first
  inv <- NULL
  
  ##Sets matrix data, via <<-
  ##Note: When new matrix set, cache is reset back to NULL
  ##Note: Error created if supplied item is not a matrix and if not square - (on error matrix cache will be reset to null)
  set <- function(newMatrix) {
    ##Note: stored variable is set to NULL initially to ensure it is NULL even if error occurs
    inv <<- NULL
  
    if(is.null(newMatrix))
      stop("Error in supplied arguments: Input is null")
    else if(!is.matrix(newMatrix))
      stop("Error in supplied arguments: Input is not matrix")
    else if(ncol(newMatrix) != nrow(newMatrix))
      stop("Error in supplied arguments: Input is not square matrix")
    else
      x <<- newMatrix  ##Succesully set new matrix data    
  }
  
  ##Returns matrix data
  ##Note: simple getter so one line function
  get <- function() x

  ##Returns cached matrix inverse
  ##Note: simple getter so one line function
  getInverse <- function() inv
  
  ##Sets matrix inverse to internal cached variable, via <<-
  ##Note: Inverse is assumed to be computed outside function and shoudl be linked to previously set matrix
  ##Note: Error created if supplied item is not a matrix and if not square (on error inv will be reset to NULL)
  setInverse <- function(matInv) {
    ##Set to NULL in case of error
    inv <<- NULL
    
    if(is.null(matInv))
      stop("Error in supplied arguments: Input is null")
    else if(!is.matrix(matInv))
      stop("Error in supplied arguments: Input is not matrix")
    else if(ncol(matInv) != nrow(matInv))
      stop("Error in supplied arguments: Input is not square matrix")
    else
      inv <<- matInv  ##Succesully set to desired inverse
  }
  
  
  ##Final output - Return list of functions
  list(get = get, set = set,
       getInverse = getInverse, setInverse = setInverse)
}


## This function takes a "special" matrix craeted by makeCacheMatrix function and returns its inverse
## Note: Function utilizes in-memory cache variable to check to see if inverse is already computed, if not inverse is called via solev and stored in-memory
## Extra parameters may be passed to solve() via standard ...
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
