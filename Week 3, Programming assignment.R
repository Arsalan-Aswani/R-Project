# Define the makeVector function
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

# Define the cachemean function
cachemean <- function(x, ...) {
        m <- x$getmean()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

# Example usage
# Create a special vector
special_vector <- makeVector(1:10)

# Calculate the mean using cachemean
cachemean(special_vector)  # This will compute the mean
cachemean(special_vector)  # This will use the cached mean

# Let's change the vector data
special_vector$set(11:20)

# The cached mean will be cleared
# So, next time we call cachemean, it will recompute the mean
cachemean(special_vector)

