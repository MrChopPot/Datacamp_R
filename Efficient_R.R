### Writing Efficient R Code

### 1. The Art of Benchmarking

# Print the R version details using version
version

# Assign the variable major to the major component
major <- version$major

# Assign the variable minor to the minor component
minor <- version$minor

# How long does it take to read movies from CSV?
system.time(read.csv("movies.csv"))

# How long does it take to read movies from RDS?
system.time(readRDS("movies.rds"))

# Load the microbenchmark package
library(microbenchmark)

# Compare the two functions
compare <- microbenchmark(read.csv("movies.csv"), 
                          readRDS("movies.rds"), 
                          times = 10)

# Print compare
compare

# Load the benchmarkme package
library(benchmarkme)

# Assign the variable ram to the amount of RAM on this machine
ram <- get_ram()
ram

# Assign the variable cpu to the cpu specs
cpu <- get_cpu()
cpu

# Load the package
library("benchmarkme")

# Run the io benchmark
res <- benchmark_io(runs = 1, size = 5)

# Plot the results
plot(res)

#######################

### 2. Fine Tuning: Efficient Base R

# Use <- with system.time() to store the result as res_grow
system.time(res_grow <- growing(n = 30000))

# Use <- with system.time() to store the result as res_allocate
n <- 30000
system.time(res_allocate <- pre_allocate(n))

# Store your answer as x2_imp
x2_imp <- x * x

# Initial code
n <- 100
total <- 0
x <- runif(n)
for(i in 1:n) 
    total <- total + log(x[i])

# Rewrite in a single line. Store the result in log_sum
log_sum <- sum(log(x))

# Which is faster, mat[, 1] or df[, 1]? 
microbenchmark(mat[, 1], df[, 1])

# Which is faster, mat[1, ] or df[1, ]? 
microbenchmark(mat[1, ], df[1, ])

#######################

### 3. Diagnosing Problems: Code Profiling





























