## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1. makeCacheMatrix: This function creates a special "matrix" object that 
##    can cache its inverse.


makeCacheMatrix <- function(mtx = matrix()) {
  mtx <- NULL
  set <- function(mtxp){
    x   <<- mtxp
    mtx <<- NULL
  }
  get       <- function() x
  setmatrix <- function(solve) mtx <<- solve
  getmatrix <- function() mtx
  list(  set       = set
         ,get       = get
         ,setmatrix = setmatrix
         ,getmatrix = getmatrix)
}


## Write a short comment describing this function
## 2. cacheSolve: This function computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. If the inverse has already been 
##    calculated (and the matrix has not changed), then the cachesolve should 
##    retrieve the inverse from the cache.

cacheSolve <- function(mx=matrix(), ...) {
  mtx<-mx$getmatrix()
  if(!is.null(mtx)){
    message("getting cached data")
    return(mtx)
  }
  matrix<-mx$get()
  mtx<-solve(matrix, ...)
  mx$setmatrix(mtx)
  mtx
}

###  Always input squre matrix which can be invertible
###  Testing
###   msr <- makeCacheMatrix()
###   msr$set(matrix(4:7,2,2))
###   cacheSolve(msr)
###   

###   input
##          [,1] [,2]
##    [1,]    4    6
##    [2,]    5    7

###   Output
##          [,1] [,2]
##    [1,] -3.5    3
##    [2,]  2.5   -2
