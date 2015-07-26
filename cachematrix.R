## makeCacheMatrix is a function that creates a special "vector", which is really a list containing a function to
## 1. Set the value of the matrix 
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse
## 5. Get the cached value of the matrix
## 6. Set the cached value of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  cachedMatrix <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    cachedMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  getcachedMatrix <- function() cachedMatrix
  setcachedMatrix <- function(matrixCache) cachedMatrix <<- matrixCache
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse,getcachedMatrix=getcachedMatrix, setcachedMatrix=setcachedMatrix)
}


## The following function calculates the inverse of the special "vector" created with the above function makeCacheMatrix.
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.
##We will compare if matrix has changed by comparing the input matrix with cachedMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        data <- x$get()
        cachedMatrix <- x$getcachedMatrix()
        if(!is.null(inv) && !is.null(cachedMatrix)  && all(data==cachedMatrix))
        {
          message("getting cached data")
          return(inv)
        }
        inv <- solve(data)
        x$setinverse(inv)
        x$setcachedMatrix(data)
        
        inv## Return a matrix that is the inverse of 'x'
}
