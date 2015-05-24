## Put comments here that give an overall description of what your
## functions do

## Adds the objects/functions get, set, getinverse, setinverse to the Matrix x
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#gets the cached inverse for a matrix, or calculates if empty
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

# fork repo
# git clone https://github.com/rdpeng/ProgrammingAssignment2.git
# make changes
# git add .
# git commit -m 'test'
# git push origin master