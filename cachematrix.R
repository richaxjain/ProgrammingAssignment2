## The function is used to get the inverse of the matrix from the cache
##

## The first function, makeCacheMatrix creates a special "matrix", which is  a list containing a function to
##perform the following tasks
##1 - set the value of the matrix
##2- get the value of the matrix
##3 -set the value of the inverse of matrix
##4- get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  ## Set the value of matrix
  set<- function(mx){
    x<<-mx
    inv<<-NULL
  }
  get <- function()x
  setinv<- function(inverse) inv <<- inverse
  getinv<- function()inv
  list(get=get,set=set,setinv=setinv,getinv=getinv)

}


## The function cacheSolve calculates the inverse of the special "matrix" created with the 
##makeCacheMatrix function. It first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
##the inverse of the matrix(using 'solve' function in R) and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data<- x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    inv
}
