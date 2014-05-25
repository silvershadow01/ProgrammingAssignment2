##  These 2 functions are able to create a system than can cache the inverse of a matrix 
##  so that subsequent calls to compute the inverse can get the value
## from the cache instead of re-computing it
##  
## 
##  
## The function creates a List with 4 functions
## 1) set the value of matrix 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
       invx <- NULL
       setmx <- function(y) {
              x <<- y
              invx <<- NULL
       }
       getmx <- function() x
       setmxinv <- function(mxinv) invx <<- mxinv
       getmxinv <- function() invx
       list( setmx = setmx,getmx=getmx,
             setmxinv = setmxinv, getmxinv = getmxinv)

}


## This function calculates the inverse of a special vector ( List with functions) 
## It checks to see if the inverse has already been calculates and if so 
## returns the inverse from the cache. Otherwise it calculates the inverse
## and sets the value for it in the cache(variable:invx) and returns the value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         mxinv <- x$getmxinv()
         if(!is.null(mxinv)) {
               message("getting cached data")
               return(mxinv)
         }
         mx <- x$getmx()
         mxinv <- solve(mx)
         x$setmxinv(mxinv)
         mxinv         
 }

