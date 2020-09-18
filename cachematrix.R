## ProgrammingAssigment2

#makeCaheMatrix, creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL
    set <- function(y){
      x<<-y
      inverse<<-NULL
    }
    get<-function() x
    setInverse <- function(solve) inverse<<-solve
    getInverse <-function() inverse
    list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
}

## cacheSolve, computes the inverse of the special "matrix" returned by `makeCacheMatrix`
  #If the inverse has already been calculated (and the matrix has not changed), then 'cacheSolve'
  #should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
      message("getting cached")
      return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    print(inverse) #Prints the inverse of the index matrix 'x'
}
