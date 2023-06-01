
# Email from KB on 01/05/2023
# Sample size calculation


# Linear regression of gene expression with the predictors concentration, 
# cell age, treatment (two levels), cell type (two levels), and media (two levels). 
# $R^2$ of 0.1 between the predictors and the response level. 
# We want a power of 90% and a significance level of 0.05.

# pwr package
pacman::p_load(pwr)

R2 <- 0.1
pwr.f2.test(u = 5, f2 = R2/(1-R2), sig.level = 0.05, power = 0.9)
v <- 147.8645

# sample size = v + u + 1
ceiling(v) + 5 + 1
