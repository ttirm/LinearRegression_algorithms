
# Load Data
data_x <- read.table("https://raw.githubusercontent.com/BradNeuberg/andrew-ng-course/master/linear_regression_2/ex3x.dat", 
                   header=FALSE)
names(data_x) <- c("x1", "x2")
data_x <- scale(data_x)
data_y <- read.table("https://raw.githubusercontent.com/BradNeuberg/andrew-ng-course/master/linear_regression_2/ex3y.dat", 
                     header=FALSE)
names(data_y) <- "y"

data <- data.frame(data_y, x0 = rep(1, nrow(data_x)), data_x)

summary(lm(y ~ x1 + x2, data = data))



# Define learning Rate
lrate <- 0.01

# Define iterations
iter <- 1000

# Define current parameters
#init_m <- rep(0, ncol(data)-1)
init_m <- matrix(c(0,0,0), nrow = 1)
init_error <- 0

error <- function(data, m){
    N <- nrow(data)
    
    y <- as.matrix(data[,1])
    
    x <- as.matrix(data[,-1])
    
    (1/N) * sum((y - (x %*% t(m)))^2)
}

gradient_step <- function(data, lrate, m){
    
    N <- nrow(data)
    #grad_m <- 0

    
    y <- as.matrix(data[,1])
    
    x <- as.matrix(data[,-1])

    # Define partial derivative to m
    grad_m <- (1/N)*(t(x) %*% ((x %*% t(m)) - y))

    
    m <- m - (lrate * t(grad_m))


}

gradient_descend <- function(lrate, iter, data, error, init_m){
    
    curr_m <- init_m
    curr_error <- init_error
    
    for(i in seq_len(iter)){
        curr_m <- gradient_step(data, lrate, curr_m)
        curr_error <- error(data, curr_m)

    }
    print(curr_m)
    print(curr_error)
    # print("The final b is: " + curr_b + ", and the final m is: " + curr_m + ", with an error of: " + curr_error)
}


gradient_descend(lrate, iter, data, error, init_m)
