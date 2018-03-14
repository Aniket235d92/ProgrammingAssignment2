##Function to get,set matrix
makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ##Getting the value of function
  get<-function()x
  ##Setting the value of function
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  
  ##Preparing the list
  list(set=set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
  ##Checks to see if inverse is present
  m <- x$getinverse()
  m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##Inverse has not been calculated
  data <- x$get()

  ##Calculating inverse using Solve function
  c<-solve(data)
  
  x$setinverse(c)
  ##Prinitng the final value of Inverse matrix
  c
  
}
