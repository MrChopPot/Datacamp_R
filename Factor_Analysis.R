### Factor Analysis in R

### 1. Evaluating your measure with factor analysis

# Load the psych package
library(psych)
 
# Conduct a single-factor EFA
EFA_model <- fa(gcbs)

# View the results
EFA_model

# Set up the single-factor EFA
EFA_model <- fa(gcbs)

# View the factor loadings
EFA_model$loadings

# Create a path diagram of the items' factor loadings
fa.diagram(EFA_model)

