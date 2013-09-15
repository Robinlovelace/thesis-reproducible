Spatial microsimulation in R: a beginner’s
guide to iterative proportional fitting (IPF)

========================================================

Introduction
-------
This section contributes to my thesis on the energy costs of travel to work.


Input data
----
Spatial microsimulation requires two input datasets: individual-level data, where rows represent individuals, and geographically aggregated data, where rows represent areas. The following code creates example datasets, based on a survey of 5 individuals and 5 small areas. The spatial microsimulation model will select individuals based on age, sex and mode of transport. For consistency with the (larger) model used for the paper, we will refer to the individual-level data as USd (short for Understanding Society dataset) and the geographic data as all.msim (all constraint variables).

### Individual-level data

```r
# Read in the data in long form (normaly read.table() used)
c.names <- c("id", "age", "sex")
USd <- c(1, 59, "m", 2, 54, "m", 3, 35, "m", 4, 73, "f", 5, 49, "f")
USd <- matrix(USd, nrow = 5, byrow = T)  # Convert long data into matrix, by row
USd <- data.frame(USd)  # Convert this into a dataframe
names(USd) <- c.names  # Add correct column names
USd$age <- as.numeric(levels(USd$age)[USd$age])  # Age is a numeric variable

USd  # Show the data frame in R
```

```
##   id age sex
## 1  1  59   m
## 2  2  54   m
## 3  3  35   m
## 4  4  73   f
## 5  5  49   f
```


### Geographical data

```r
# Read in the data in long form (normaly read.table() used)
category.labels <- c("16-49", "50+" # Age constraint 
             ,"m", "f" # Sex constraint
             #,"bicycle", "bus", "car.d", "car.p", "walk" # Mode constraint
             )
all.msim <- c(  8, 4,    6, 6,   #0.001, 1, 8, 1, 0.001, # Car dominated
                2, 8,    4, 6,   #0.001, 3, 5, 1, 1, # Elderly
                7, 4,    3, 8,   #1, 2, 5, 2, 1, # Female dominated
                5, 4,    7, 2,   #2, 1, 3, 1, 2, # Male dominated
                7, 3,    6, 4    #,   7, 0.001, 2, 0.001, 1  # Many cyclists, young
                )
all.msim <- matrix(all.msim, nrow = 5, byrow = T) # Convert long data into matrix, by row
all.msim <- data.frame(all.msim) # Convert this into a dataframe
names(all.msim) <- category.labels # Add correct column names
all.msim # Show the data frame in R
```

```
##   16-49 50+ m f
## 1     8   4 6 6
## 2     2   8 4 6
## 3     7   4 3 8
## 4     5   4 7 2
## 5     7   3 6 4
```

```r

# Check totals for each constraint match
rowSums(all.msim[,1:2]) # Age constraint
```

```
## [1] 12 10 11  9 10
```

```r
rowSums(all.msim[,3:4]) # Sex constraint
```

```
## [1] 12 10 11  9 10
```

```r

rowSums(all.msim[,1:2]) == rowSums(all.msim[,3:4]) 
```

```
## [1] TRUE TRUE TRUE TRUE TRUE
```

```r
#rowSums(all.msim[,6:10])# Mode constraint
```


Reweighting the survey dataset
---
Iterative proportional fitting will determine the weight allocated to each individual for each zone to best match the geographically aggregated data. A weight matrix is therefore created, with rows corresponding to individuals and columns to zones.

### Create weights: one set of weights for each constraint and one for starting

```r
weights0 <- array(dim = c(nrow(USd), nrow(all.msim)))
weights1 <- array(dim = c(nrow(USd), nrow(all.msim)))
weights2 <- array(dim = c(nrow(USd), nrow(all.msim)))
# weights3 <- array(dim=c(nrow(USd),nrow(all.msim)))

weights0[, ] <- 1  # sets initial weights to 1
```


### Create survey aggregate arrays (for direct comparison with the geographical aggregate data)

```r
USd.agg <- array(dim = c(nrow(all.msim), ncol(all.msim)))
USd.agg1 <- array(dim = c(nrow(all.msim), ncol(all.msim)))
USd.agg2 <- array(dim = c(nrow(all.msim), ncol(all.msim)))
colnames(USd.agg1) <- category.labels
```


### Convert survey data into wide form
This step allows the individual-level data to be compared with the aggregated data directly

```r
USd.cat <- array(rep(0), dim = c(nrow(USd), length(category.labels != 0)))

USd.cat[which(USd$age < 50), 1] <- 1
USd.cat[which(USd$age >= 50), 2] <- 1
USd.cat[which(USd$sex == "m"), 3] <- 1
USd.cat[which(USd$sex == "f"), 4] <- 1
sum(USd.cat)  # Should be 10
```

```
## [1] 10
```

```r

for (i in 1:nrow(all.msim)) {
    # Loop creating aggregate values (to be repeated later)
    USd.agg[i, ] <- colSums(USd.cat * weights0[, i])
}

# Test results
USd.agg
```

```
##      [,1] [,2] [,3] [,4]
## [1,]    2    3    3    2
## [2,]    2    3    3    2
## [3,]    2    3    3    2
## [4,]    2    3    3    2
## [5,]    2    3    3    2
```

```r
all.msim
```

```
##   16-49 50+ m f
## 1     8   4 6 6
## 2     2   8 4 6
## 3     7   4 3 8
## 4     5   4 7 2
## 5     7   3 6 4
```

```r
plot(as.vector(as.matrix(all.msim)), as.vector(as.matrix(USd.agg)), xlab = "Constraints", 
    ylab = "Model output")
abline(a = 0, b = 1)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
cor(as.vector(as.matrix(all.msim)), as.vector(as.matrix(USd.agg)))
```

```
## [1] -0.1568
```

Note that for USd.agg, the results are the same for every zone, as each individual has a weight of 1 for every zone.
The next stage is to apply the first constraint, to adjust the weights of each individual so they match the age constraints.

### Constraint 1: age

```r
for (j in 1:nrow(all.msim)) {
    weights1[which(USd$age < 50), j] <- all.msim[j, 1]/USd.agg[j, 1]
    weights1[which(USd$age >= 50), j] <- all.msim[j, 2]/USd.agg[j, 2]
}
# Aggregate the results for each zone
for (i in 1:nrow(all.msim)) {
    USd.agg1[i, ] <- colSums(USd.cat * weights0[, i] * weights1[, i])
}
# Test results
USd.agg1
```

```
##      16-49 50+     m     f
## [1,]     8   4 6.667 5.333
## [2,]     2   8 6.333 3.667
## [3,]     7   4 6.167 4.833
## [4,]     5   4 5.167 3.833
## [5,]     7   3 5.500 4.500
```

```r
all.msim
```

```
##   16-49 50+ m f
## 1     8   4 6 6
## 2     2   8 4 6
## 3     7   4 3 8
## 4     5   4 7 2
## 5     7   3 6 4
```

```r
plot(as.vector(as.matrix(all.msim)), as.vector(as.matrix(USd.agg1)), xlab = "Constraints", 
    ylab = "Model output")
abline(a = 0, b = 1)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
cor(as.vector(as.matrix(all.msim)), as.vector(as.matrix(USd.agg1)))
```

```
## [1] 0.6967
```

As indicated by the plots and the correlation values, the fit between the individual-level data and the aggregate constraints (the inpute data) has been vastly improved, just by constraining by a single variable. We will perform the test after each constraint to ensure our model is improving. To see how the weights change for each individual for each area, type weights1 for constraint 1.

```r
weights1
```

```
##       [,1]  [,2]  [,3]  [,4] [,5]
## [1,] 1.333 2.667 1.333 1.333  1.0
## [2,] 1.333 2.667 1.333 1.333  1.0
## [3,] 4.000 1.000 3.500 2.500  3.5
## [4,] 1.333 2.667 1.333 1.333  1.0
## [5,] 4.000 1.000 3.500 2.500  3.5
```


### Constraint 2: sex

```r
for (j in 1:nrow(all.msim)) {
    weights2[which(USd$sex == "m"), j] <- all.msim[j, 3]/USd.agg1[j, 3]
    weights2[which(USd$sex == "f"), j] <- all.msim[j, 4]/USd.agg1[j, 4]
}

weights3 <- weights0 * weights1 * weights2
for (i in 1:nrow(all.msim)) {
    USd.agg2[i, ] <- colSums(USd.cat * weights3[, i])
}
# Test results
USd.agg2
```

```
##       [,1]  [,2] [,3] [,4]
## [1,] 8.100 3.900    6    6
## [2,] 2.268 7.732    4    6
## [3,] 7.496 3.504    3    8
## [4,] 4.691 4.309    7    2
## [5,] 6.929 3.071    6    4
```

```r
all.msim
```

```
##   16-49 50+ m f
## 1     8   4 6 6
## 2     2   8 4 6
## 3     7   4 3 8
## 4     5   4 7 2
## 5     7   3 6 4
```

```r
plot(as.vector(as.matrix(all.msim)), as.vector(as.matrix(USd.agg2)), xlab = "Constraints", 
    ylab = "Model output")
abline(a = 0, b = 1)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r
cor(as.vector(as.matrix(all.msim)), as.vector(as.matrix(USd.agg2)))
```

```
## [1] 0.9942
```

Again the correlation has improved. Now onto the 3rd constraint

Iterations
-------------------------
The correlation has been improved from one constraint to the next.
Even the after the final constraint (mode), which differs greatly from the survey data for some zones, the correlation has improved. This illustrates the robustness of the IPF method. Also note that the population of each simulated zone is correct.  The next step is to perform further iterations, using the results of the first iteration (weights4) as our starting point.

```r
weights0 <- weights3
USd.agg.1 <- USd.agg  # Saving this for future reference
weights3[, 1]
```

```
## [1] 1.2 1.2 3.6 1.5 4.5
```



After running this command, simply run the model again, beginning from the loop at the end of the R code section titled "Convert survey data into wide form". After running constraint 3 the second time, the correlation is higher: 0.89 instead of 0.86 . Because this is a relatively simple example, the fit between constraint and simulated aggregate variables will not improve much beyond this point (notice the similarity between the plot below - of the final result after the 2nd iteration - and the plot above).





After the second iteration, the results are as follows:

```r
plot(as.vector(as.matrix(all.msim)), as.vector(as.matrix(USd.agg2)), xlab = "Constraints", 
    ylab = "Model output")
abline(a = 0, b = 1)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
cor(as.vector(as.matrix(all.msim)), as.vector(as.matrix(USd.agg2)))
```

```
## [1] 1
```



Interrogating the results
---
To view the characteristics of representative individuals for each zone, the vector associated with the zone in question can be called from the final weight matrix (weights4 in this case). The individuals that best match (have the highest weights) for zone five for example, which contains a high proportion of young cyclists, can be viewed with the following command: 

```r
cbind(weights3[, 5], USd)
```

```
##   weights3[, 5] id age sex
## 1         1.068  1  59   m
## 2         1.068  2  54   m
## 3         3.864  3  35   m
## 4         0.866  4  73   f
## 5         3.134  5  49   f
```

The output from this command illustrates why the final solution of the IPF procedure is not perfect (i.e. why the correlation between the constraint and simulated aggregates cannot be 1): there is only 1 cyclist in the sample population, so she must be replicated 7 times to fulfill the number of cyclists in the constraint variables, distorting the other results. This example demonstrates the importance of having a large and diverse survey dataset from which individuals can be sampled.

For some areas the results are better than others. A breakdown of model fit by area can be seen by recursively running the correlation command:


```r
for (i in 1:nrow(all.msim)) {
    all.msim$cor[i] <- cor(as.vector(as.matrix(all.msim[i, 1:4])), USd.agg2[i, 
        ])
}
all.msim
```

```
##   16-49 50+ m f cor
## 1     8   4 6 6   1
## 2     2   8 4 6   1
## 3     7   4 3 8   1
## 4     5   4 7 2   1
## 5     7   3 6 4   1
```

Note that zones 1 and 4 are simulated well. This can be explained because their attributes already fitted with those of original dataset well:

```r
for (i in 1:nrow(all.msim)) {
    all.msim$cor[i] <- cor(as.vector(as.matrix(all.msim[i, 1:4])), USd.agg.1[i, 
        ])
}
all.msim
```

```
##   16-49 50+ m f     cor
## 1     8   4 6 6 -0.7071
## 2     2   8 4 6  0.4472
## 3     7   4 3 8 -0.9701
## 4     5   4 7 2  0.5547
## 5     7   3 6 4 -0.3162
```

```r
USd.agg.1
```

```
##      [,1] [,2] [,3] [,4]
## [1,]    2    3    3    2
## [2,]    2    3    3    2
## [3,]    2    3    3    2
## [4,]    2    3    3    2
## [5,]    2    3    3    2
```

This illustrates the importance of using a sample survey dataset that is fairly representative of the aggregated constraint table.


Taking IPF further
----
This document has provided a succinct run-through of the IPF procedure in R. This simplified example illustrates how the R programming language is well-suited to the task, with a number of in-built functions to manipulate and analyse the data. Because R is an object-orientated programming language with many add-ons and the ability to define new functions, the capabilities outlined above only scratch the surface of what is possible. For example, it would be possible to create new individuals which have the characteristics most needed to improve the overall model fit. Also, the possibility of grouping individuals into household units would greatly add to the technique's ability to simulate reality (many statistics are collected, and many decisions are made, at the household level).
Another ‘add-on’ that would be particularly useful for IPF would be the
ability to ‘integerise’ the results. Methods for performing integerisation in R
are compared in a paper that accompanies this document.

The advancement of the IPF procedure will depend not only on application, but understanding
of the underlying theory. This topic is beyond the scope of this practical guide.
Some references on the underlying theory and applications of IPF are provided below: 

Key references on IPF and spatial microsimultion 
---
Note: this list is just a starter and is no way comprehensive

[Deming, W., 1940](http://www.jstor.org/stable/2235722). On a least squares adjustment of a sampled frequency table when the expected marginal totals are known. The Annals of Mathematical Statistics.

[Wong, D.W.S., 1992](http://dx.doi.org/10.1111/j.0033-0124.1992.00340.x). The Reliability of Using the Iterative Proportional Fitting Procedure. The Professional Geographer, 44(3), pp.340–348.

[Norman, P., 1999](http://eprints.whiterose.ac.uk/5029/1/99-3.pdf). Putting Iterative Proportional Fitting (IPF) on the Researcher’s Desk. School of Geography, University of Leeds.

[Ballas, D. et al., 2005](www.jrf.org.uk/sites/files/jrf/1859352669.pdf). Geography matters: simulating the local impacts of national social policies, Joseph Roundtree Foundation.

[Hermes, K. & Poulsen, M., 2012](http://linkinghub.elsevier.com/retrieve/pii/S0198971512000336). A review of current methods to generate synthetic spatial microdata using reweighting and future directions. Computers, Environment and Urban Systems, 36(4), pp.281–290.

[Pritchard, D.R. & Miller, E.J., 2012](http://www.springerlink.com/index/10.1007/s11116-011-9367-4). Advances in population synthesis: fitting many attributes per agent and fitting to household and person margins simultaneously. Transportation, 39(3), pp.685–704.





