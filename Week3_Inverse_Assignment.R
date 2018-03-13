makeCacheMatrix <- function(x = matrix()) {
  print("Inside")
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  
  list(set=set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()

  c<-solve(data)
  x$setinverse(c)
  
}
