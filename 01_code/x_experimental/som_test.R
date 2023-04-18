# Load required libraries
library(tidyverse)
library(kohonen)

# Generate sample data
set.seed(123)
data <- data.frame(x = rnorm(1000), y = rnorm(1000))  %>% as.matrix()

# Perform SOM
som.res <- som(data)

# Visualize SOM
plot(som.res)
