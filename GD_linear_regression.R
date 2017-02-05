
# Load Data
data <- read.csv("https://raw.githubusercontent.com/llSourcell/linear_regression_live/master/data.csv", header = FALSE)
names(data) <- c("x", "y")

# Define learning Rate
lrate <- 0.00001

# Define iterations
iter <- 1000

# Define current parameters
init_b <- 0
init_m <- 0
init_error <- 0

error <- function(data, m, b){
    N <- nrow(data)
    (1/N)*sum((data[,'y'] - (data[,'x']* m +b))^2)
}

gradient_step <- function(data, lrate, b, m){
    
    N <- nrow(data)
    grad_b <- 0
    grad_m <- 0
    
    grad_b = (2/N)*sum(-data[,'y'] - (data[,'x'] * m +b))

    # Define partial derivative to m
    grad_m = (2/N)*sum(-data[,'x'] * (data[,'y'] - (data[,'x'] * m +b)))
    
    # for(j in seq_len(nrow(data))){
    #     # Define partial derivative to b
    #     grad_b = grad_b + -(2/N)*(data[j,'y'] - (data[j,'x'] * m +b))
    # 
    #     # Define partial derivative to m
    #     grad_m = grad_m + (2/N)*(data[j,'x'] * (data[j,'y'] - (data[j,'x'] * m +b)))
    # }
    
    new_b <- b - (lrate * grad_b)
    new_m <- m - (lrate * grad_m)
    c(new_b, new_m)

    
}

gradient_descend <- function(lrate, iter, data, error, init_b, init_m){
    
    curr_b <- init_b
    curr_m <- init_m
    curr_error <- init_error
    
    for(i in seq_len(iter)){
        result <- gradient_step(data, lrate, curr_b, curr_m)
        curr_b <- result[1]
        curr_m <- result[2]
        curr_error <- error(data, curr_m, curr_b)
    }
    print(curr_m)
    print(curr_b)
    print(curr_error)
    # print("The final b is: " + curr_b + ", and the final m is: " + curr_m + ", with an error of: " + curr_error)
}


gradient_descend(lrate, iter, data, error, init_b, init_m)
