## Put comments here that give an overall description of what your
## functions do
## 
## Description: In the assignment we were given two example functions. 
## The solution is a very small modification to the provided examples. 
## Below is an description on how they works and in the end
## I have provided an example how they may be used

## 1/ The first function returns
## an object that stores the data that we provide when initialised 
## (lexical scoping means that it will use the value provided when created). The function is generic so it
## can store any kind of object. It also provides a method to change it's value (set). 
## The created object also have methods to set and get a cached value/result
## When the object is created the cached value is initialized to NULL
##
## 2/ The created object will be passed to a special function that can 
## retrive the cached value, and if this value exists (i.e. !=NULL) 
## it will return that value. However, if this value is NULL
## it will perform it's calculations and set the cached values
## since we use <<- inside our object this will affect the 
## object outside of the function as well. This function that 
## operates on our object will return the result (calculated or from cach)

## First function is completely generic (as long as we don't have a check on input)
## Second function operates on the object created by the first function
## except for the calculation performed on the input object this function is also generic
## We could change the first function to be able to  store a function that performs a calculation
## and is retrieved by the second function or we could provide an extra argument to provide
## what kind of operations we like to perform
## However, for the given task we should only make a function that can invert a 
## matrix and we should assume that we provide an invertible matrix (thus we don't check input)



## Write a short comment describing this function
## Creates an object that stores a matrix. First a matrix provided (or default) when 
## we create the object, then also a functionallity to set and get this matrix.
## The setResult/getResult is provided for the function that performs 
## calculations on this object

makeCacheMatrix <- function(x = matrix()) {
  cachedResult <- NULL
  set <- function(y) {
    x <<- y
    cachedResult <<- NULL
  }
  get <- function() x
  setResult <- function(result) cachedResult <<- result
  getResult <- function() cachedResult
  list(set = set, get = get,
       setResult = setResult,
       getResult = getResult)
}

## Write a short comment describing this function
## cacheSolve operates on an object created by the makeCacheMatrix function
## It will check if a result already has been produced. If not it will
## perform the calculations (in this case matrice inversion with the solve function)
## and cache the result as well as returning the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachedResult <- x$getResult()
  if(!is.null(cachedResult)) {
    message("getting cached data")
    return(cachedResult)
  }
  data <- x$get()
  result <- solve(data, ...)
  x$setResult(result)
  
  return(result)
}

# Examples
# > m <- matrix(c(1,2,3,4), nrow = 2)
# > my_matrix <- makeCacheMatrix(m)
# > my_matrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(my_matrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(my_matrix)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > minverted <- cacheSolve(my_matrix)
# getting cached data
# > my_matrix$set(minverted)
# > cacheSolve(my_matrix)
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(my_matrix)
# getting cached data
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

