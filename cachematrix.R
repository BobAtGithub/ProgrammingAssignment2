##
## This source contains two functions: makeCacheMatrix and cacheSolve 
##
## makeCacheMatrix is used to store and retrive a matrix and its inverse
##
## cacheSolve is used to do the actual matrix inversion, and store the result 
## back in the space created by makeCacheMatrix. 
##
## If the inverse has already been calculated it will be retieved to save time
## 
## Usage:       m<-makeCacheMatrix()
##              m$set(matrix)
##              cachesolve(m)
##              cachsolve(m)    ## repeated retrieves from cache
##
## Assumes the souce matrix is invertible, works on any size matrix

## ----------------------------------------------------------------------------
## 
## makeCacheMatrix stores a matrix and the results of its inversion 
## 
## Description
## Creates a list which comprises of 4 functions to store/retrieve
## a matrix and its inverse in its parent space. It does not do the inversion 
## itself.
##
## Usage
##      create          : m<-makeCacheMatrix()
##      Set             : m$set(matrix)        
##      get             : m$get()
##      setinverse      : m$setinverse(matrix_inverse)
##      getinverse      : m$getinverse()

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL       ## new input means reset previous inverse
        }
        get <-function() x
        setinverse <-function(solve) m<<-solve
        getinverse <-function() m
        list (set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## ----------------------------------------------------------------------------
## 
## cacheSolve returns the inverse of the martix referenced by the
## list object created by makeCacheMatrix, also storing a reference to the 
## result via the list
## 
## Description
## If the result has already been calculated before, it does not recalc 
## It uses the R function - "solve" to do the inversion, and gives a message
## when retieving from cache
## 
## Usage : cacheSolve(m) {repeated calls will retrieve same result}

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m       
}