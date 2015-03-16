# Written By Sunny Not, Date March 16 2015, Corusera R programming Course.
## ************************************************************************************
## ************************************************************************************

## makeCacheMatrix: The goal of makeCacheMatrix function is to set up the input matrix such that we can store and access 
# a cached value for its inverse calculations. The goal of this function along with the cacheSolve
# function is to reduce redundency around matrix inversion calculations. If the matrix is once
# inversed, store the inverse matrix in cache and use it in future occasians.
# The makeCacheMatrix function specifically prepares the input matrix and  outputs is a list
# from which we can access the input matrix itself (with x$get()), access its invserse (with x$setinv),
#access its cached inverse (by x$getinv). I have added another child function called getenv, so that
# I can track which enviroment certain variables are living in.

### Example usage: 
### > m <- matrix(c(0, -3, 1, 1), 2,2)
### > x <- makeCacheMatrix(m)
### > inv <- cacheSolve(x)
# or alternatively:
### > cacheSolve(makeCacheMatrix(m))

makeCacheMatrix <- function(x = matrix()) {

# makeCacheMatrix is a function with input argument that is a matrix. 
# This is a parent function in a parent envirement and it has some child functions inside of it.
        
        inv <- NULL
# Initially the value of inv (the inverse of the input matrix) is set to NULL, meaning there 
# is no cached value yet.

        print(environment())
        evn <- environment()
        print(parent.env(evn))
# These lines are to show how the enviroment assigment work. So we can see a different number
# for the current envirement (inside of the function) and the parent enviroment which is 
# the global enviroment. These lines are added to assist the understanding of lexical scoping, 
# so we can examine which envireoment we are in. These are ofcourse non-essenial
# parts of this code.

        set <- function(y) {
                x <<- y
## Note that the "<<-" operator assigns the value to a variable in the parent environment, 
# which means x will be in the parent enviroment, so it will be in the enviroment of the parent 
# function makeCacheMatrix and not in the set function.
                inv <<- NULL
        }
# set is a function that takes a matrix as input. This matix is the inverse of an matrix. So 
# if it is given then the set function will set 'x' to be equal to the input inverse matrix. 
# This is basically to cache the value of the invserve matrix and store it as y to be accessed 
# later. The second line will put the NULL into variable 'inv' which is the variable for inverse
# of the global input matrix.

        get <- function() x
# get is another funtion defined inside of the parent function makeCacheMatrix. All it does is 
# to return the variable 'x', which is what was set in the set function as the cached inverse matrix.

        setinv <- function(solve) inv <<- solve

# The setinv function will calculate the inverse of a matrix using the R  function solve.
# The result of this calulation will be then stored in varianbe named 'inv'.

        getinv <- function() inv

# The getin function will retreive the value of the variable 'inv'.

        getevn<- function() environment()
# The getenv function here simply return the value of the current enviroment and can be used
# for debugging to make sure we are dealing with the intended enviroment. It is a non-essenial
# part of this code.

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv,
             getevn = getevn)
# We are again in the parent function's enviroment and this list is what the function
# makeCacheMatrix returns, which is a list containing all these child functions that we defined
# inside of the parent function. Each child function can then be accessed as an element of
# the list.

}

## ************************************************************************************
## ************************************************************************************

## Function cacheSolve returns a matrix which is the inverse of the input matrix. 
## Either by computing the inverse on the spot or retrieving an exisiting cached 
## value for the inverse if it exists.
## This function has to be run after makeCacheMatrix has prepared the input matrix for 
## the cache storage to be possible.

cacheSolve <- function(x) {
## Return a matrix that is the inverse of 'x'.
        
        
        inv <- x$getinv()
# The value for variable 'inv' is set to the return of the getinv function. Recall that from 
# makeCacheMatrix function, we can access the getinv which retireves the value of cached inverse.
        
        if(!is.null(inv)) {
                message("getting cached data")
                
# Now if already a cached value existed, variable 'inv' will have that value and be returend and
# the function is exited.
        }
        data <- x$get()
        inv <- solve(data)
# Note that solve function will automatically generate an error if the input matrix is not a 
# square matrix.
        x$setinv(inv)
        inv 
# and if the cached value did NOT exist the previous if statment will not satisfy. In this 
# case the input matrix will be used to solve for the inverse right away.
# After the inverse is computed the value of the inverse will be stored with the use of 
# setinv function, so it is now cached.
# Also the last line of the function, here value of 'inv' variable which is the inverse of the 
# matrix will be returned.
}
