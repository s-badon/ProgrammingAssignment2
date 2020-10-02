## This pair of functions caches the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { #initialize x as a matrix
        inv <- matrix() #initialize the object that will store the inverse of the matrix
        set <- function(y) { #create a function that assigns the values of x and inv in the parent environment
            x <<- y #assign value of x in the parent environment to the input of the set function
            inv <<- matrix() #initiate inv in the parent environment, this clears previously cached values when input x is changed
        }
        get <- function() x #create a function that retrieves the value of x from the parent environment
        setinv <- function(inverse) inv <<- inverse #create a function that sets the value of inv in the parent environment
        getinv <- function() inv #create a function that retrieves the value of inv from the parent environment
        list(set = set,  #create a list that contains each of these functions with names
             get = get,
             setinv = setinv,
             getinv = getinv)
    }



## This function computes the inverse of the special matrix oject returned by makeCacheMatrix above 
## (if inverse has already been calculated and matrix has not changed then the function retrives the inverse from the cache)

cacheSolve <- function(input.object, ...) { #input is the resulting object from makeCacheMatrix
    inv.local <- input.object$getinv() #gets value of inv from the input object
    if(!is.na(inv.local[1,1])) { #check if there is a cached value of inv, if yes then retrieve it
        message("getting cached data")
        return(inv.local)
    }
    data <- input.object$get() #if no cached value, get the stored matrix
    inv.local.calc <- solve(data, ...) #calculate the inverse of the matrix
    input.object$setinv(inv.local.calc) #set the inverse in the input object
    inv.local.calc #print the inverse
}
