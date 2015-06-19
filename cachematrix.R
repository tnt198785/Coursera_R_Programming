## makeCacheMatrix function will set a matrix to cache the inverse of a matrix that
## has been calculated
## cacheSolve function will seach cache for the inverse that is to be calculated.
## Return cached inverse if data is available.
## If no record is found, it will calculate the inverse for the new matrix.

## set a matrix to cache inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y) {
  x<<-y
  inv<-NULL
}
get<-function () x
setInverse<-function(inverse) inv<<-inverse
getInverse<-function() inv
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## search cache for data and return cached data or new inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setInverse(inv)
  inv
  
}
