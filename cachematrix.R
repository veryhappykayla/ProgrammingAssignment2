## Coursera: Introduction to R Programming
## Programming Assignment 2: Lexical Scoping

## Matrix inversion is usually a costly computation, and there can be benefit to
## caching the value of the computation instead of computing it repeatedly.

## These functions take advantage of the scoping rules of the R language,
## manipulating them to preserve state inside of an R object.

##------------------------------------------------------------------------------

## makeCacheMatrix
##
## Creates a special "matrix", which is really a list containing functions to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the matrix's inverse (setsolve)
## 4. get the value of the matrix's inverse (getsolve)

makeCacheMatrix <- function(x = matrix()) {

    # Initialize cached inverse value to NULL -- i.e. not yet calculated
    m <- NULL
    
    # 1. Function to set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # 2. Function to get the vaule of the matrix
    get <- function() x
    
    # 3. Function to set the value of the matrix's inverse
    setsolve <- function(solve) m <<- solve
    
    # 4. Function to get the value of the matrix's inverse
    getsolve <- function() m
    
    # Store the four functions as a list, and return it
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

##------------------------------------------------------------------------------

## CacheSolve
##
## Computes the inverse of the "matrix" object returned by makeCacheMatrix.
## If the matrix is unchanged and the inverse has already been calculated,
## the inverse's value is retrived quickly from the cache rather than
## re-calculated (a possibly costly computation)

cacheSolve <- function(x, ...) {
    
    # Get the current value of the matrix inverse that has perhaps been stored
    m <- x$getsolve()
    
    # If the matrix already has a cached inverse value stored, then return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # If the matrix does not have a cached value already stored,
    # then calculate, store it, and return it
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}