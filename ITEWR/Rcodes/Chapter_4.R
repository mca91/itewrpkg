## library(AER)
## library(MASS)
library(knitr)
library(kableExtra)

frame <- data.frame(
  TestScore = c(680, 640, 670, 660, 630, 660, 635),
  STR = c(15, 17, 19, 20, 22, 23.5, 25)
         )
rownames(frame) <- 1:7

t(frame) %>% kable("latex", booktabs = T) %>%
kable_styling(latex_options = "striped")
library(knitr)
library(kableExtra)

frame <- data.frame(
  TestScore = c(680, 640, 670, 660, 630, 660, 635),
  STR = c(15, 17, 19, 20, 22, 23.5, 25)
         )
rownames(frame) <- 1:7

kable(t(frame), "latex", booktabs = T) %>% kable_styling(position = "center")
# Create sample data
STR <- c(15, 17, 19, 20, 22, 23.5, 25)
TestScore <- c(680, 640, 670, 660, 630, 660, 635) 

# Print out sample data
STR
TestScore
# create a scatterplot of the data
plot(TestScore ~ STR)

# add the systematic relationship to the plot
abline(a = 713, b = -3)
cat('
<div class = "keyconcept" id="KC4.1">
<h3 class = "right"> Key Concept 4.1 </h3>          
<h3 class = "left"> Terminology for the Linear Regression Model with a Single Regressor </h3>

<p> The linear regression model is 

$$Y_i = \\beta_0 + \\beta_1 X_1 + u_i$$

where

- the index $i$ runs over the observations, $i=1,\\dots,n$
- $Y_i$ is the *dependent variable*, the *regressand*, or simply the *left-hand variable*
- $X_i$ is the *independent variable*, the *regressor*, or simply the *right-hand variable*
- $Y = \\beta_0 + \\beta_1 X$ is the *population regression line* also called the *population regression function*
- $\\beta_0$ is the *intercept* of the population regression line
- $\\beta_1$ is the *slope* of the population regression line
- $u_i$ is the *error term*.
</p>
</div>
')
cat('\\begin{keyconcepts}[Terminology for the Linear Regression Model with a Single Regressor]{4.1}
The linear regression model is $$Y_i = \\beta_0 + \\beta_1 X_1 + u_i$$
where
\\begin{itemize}
\\item the index $i$ runs over the observations, $i=1,\\dots,n$
\\item $Y_i$ is the \\textit{dependent variable}, the \\textit{regressand}, or simply the \\textit{left-hand variable}
\\item $X_i$ is the \\textit{independent variable}, the \\textit{regressor}, or simply the \\textit{right-hand variable}
\\item $Y = \\beta_0 + \\beta_1 X$ is the \\textit{population regression line} also called the \\textit{population regression function}
\\item $\\beta_0$ is the \\textit{intercept} of the population regression line
\\item $\\beta_1$ is the \\textit{slope} of the population regression line
\\item $u_i$ is the \\textit{error term}.
\\end{itemize}
\\end{keyconcepts}
')
## # install the AER package (once)
## install.packages("AER")
## 
## # load the AER package
## library(AER)
## 
## # load the the data set in the workspace
## data(CASchools)
# load the AER package 
library(AER)   

# load the the data set in the workspace
data(CASchools) 
class(CASchools)
## Press <tt>ctrl + L</tt> to clear the console. This command deletes any code that has been typed in and executed by you or printed to the console by <tt>R</tt> functions. The good news is that anything else is left untouched. You neither loose defined variables etc. nor the code history. It is still possible to recall previously executed <tt>R</tt> commands using the up and down keys. If you are working in *RStudio*, press <tt>ctrl + Up</tt> on your keyboard (<tt>CMD + Up</tt> on a Mac) to review a list of previously entered commands.

head(CASchools)
# compute STR and append it to CASchools
CASchools$STR <- CASchools$students/CASchools$teachers 

# compute TestScore and append it to CASchools
CASchools$score <- (CASchools$read + CASchools$math)/2     
# compute sample averages of STR and score
avg_STR <- mean(CASchools$STR) 
avg_score <- mean(CASchools$score)

# compute sample standard deviations of STR and score
sd_STR <- sd(CASchools$STR) 
sd_score <- sd(CASchools$score)

# set up a vector of percentiles and compute the quantiles 
quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)
quant_STR <- quantile(CASchools$STR, quantiles)
quant_score <- quantile(CASchools$score, quantiles)

# gather everything in a data.frame 
DistributionSummary <- data.frame(Average = c(avg_STR, avg_score), 
                                  StandardDeviation = c(sd_STR, sd_score), 
                                  quantile = rbind(quant_STR, quant_score))

# print the summary to the console
DistributionSummary
plot(score ~ STR, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")
cor(CASchools$STR, CASchools$score)
cat('
<div class = "keyconcept" id="KC4.2">
<h3 class = "right"> Key Concept 4.2 </h3>          
<h3 class = "left"> The OLS Estimator, Predicted Values, and Residuals </h3>
<p> The OLS estimators of the slope $\\beta_1$ and the intercept $\\beta_0$ in the simple linear regression model are
\\begin{align}
  \\hat\\beta_1 & = \\frac{ \\sum_{i = 1}^n (X_i - \\overline{X})(Y_i - \\overline{Y}) } { \\sum_{i=1}^n (X_i - \\overline{X})^2},  \\\\
  \\\\
  \\hat\\beta_0 & =  \\overline{Y} - \\hat\\beta_1 \\overline{X}. 
\\end{align}
The OLS predicted values $\\widehat{Y}_i$ and residuals $\\hat{u}_i$ are
\\begin{align}
  \\widehat{Y}_i & =  \\hat\\beta_0 + \\hat\\beta_1 X_i,\\\\
  \\\\
  \\hat{u}_i & =  Y_i - \\widehat{Y}_i. 
\\end{align}

The estimated intercept $\\hat{\\beta}_0$, the slope parameter $\\hat{\\beta}_1$ and the residuals $\\left(\\hat{u}_i\\right)$ are computed from a sample of $n$ observations of $X_i$ and $Y_i$, $i$, $...$,  $n$. These are *estimates* of the unknown population intercept $\\left(\\beta_0 \\right)$, slope $\\left(\\beta_1\\right)$, and error term $(u_i)$.
</p>

The formulas presented above may not be very intuitive at first glance. The following interactive application aims to help you understand the mechanics of OLS. You can add observations by clicking into the coordinate system where the data are represented by points. Once two or more observations are available, the application computes a regression line using OLS and some statistics which are displayed in the right panel. The results are updated as you add further observations to the left panel. A double-click resets the application, i.e., all data are removed.

<iframe height="410" width="900" frameborder="0" scrolling="no" src="SimpleRegression.html"></iframe>

</div>')
cat('\\begin{keyconcepts}[The OLS Estimator, Predicted Values, and Residuals]{4.2}
The OLS estimators of the slope $\\beta_1$ and the intercept $\\beta_0$ in the simple linear regression model are
\\begin{align*}
  \\hat\\beta_1 & = \\frac{ \\sum_{i = 1}^n (X_i - \\overline{X})(Y_i - \\overline{Y}) } { \\sum_{i=1}^n (X_i - \\overline{X})^2},  \\\\
\\hat\\beta_0 & =  \\overline{Y} - \\hat\\beta_1 \\overline{X}. 
\\intertext{The OLS predicted values $\\widehat{Y}_i$ and residuals $\\hat{u}_i$ are}
  \\widehat{Y}_i & =  \\hat\\beta_0 + \\hat\\beta_1 X_i,\\\\
  \\hat{u}_i & =  Y_i - \\widehat{Y}_i. 
\\end{align*}

The estimated intercept $\\hat{\\beta}_0$, the slope parameter $\\hat{\\beta}_1$ and the residuals $\\left(\\hat{u}_i\\right)$ are computed from a sample of $n$ observations of $X_i$ and $Y_i$, $i$, $...$,  $n$. These are \\textit{estimates} of the unknown true population intercept $\\left(\\beta_0 \\right)$, slope $\\left(\\beta_1\\right)$, and error term $(u_i)$.
\\end{keyconcepts}
')
rm(STR)
attach(CASchools) # allows to use the variables contained in CASchools directly

# compute beta_1_hat
beta_1 <- sum((STR - mean(STR)) * (score - mean(score))) / sum((STR - mean(STR))^2)

# compute beta_0_hat
beta_0 <- mean(score) - beta_1 * mean(STR)

# print the results to the console
beta_1
beta_0
# estimate the model and assign the result to linear_model
linear_model <- lm(score ~ STR, data = CASchools)

# print the standard output of the estimated lm object to the console 
linear_model
# plot the data
plot(score ~ STR, 
     data = CASchools,
     main = "Scatterplot of TestScore and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)",
     xlim = c(10, 30),
     ylim = c(600, 720))

# add the regression line
abline(linear_model) 
mod_summary <- summary(linear_model)
mod_summary
# compute R^2 manually
SSR <- sum(mod_summary$residuals^2)
TSS <- sum((score - mean(score))^2)
R2 <- 1 - SSR/TSS

# print the value to the console
R2

# compute SER manually
n <- nrow(CASchools)
SER <- sqrt(SSR / (n-2))

# print the value to the console
SER
cat('
<div class = "keyconcept" id="KC4.3">
<h3 class = "right"> Key Concept 4.3 </h3>          
<h3 class = "left"> The Least Squares Assumptions </h3>
<p> 
$$Y_i = \\beta_0 + \\beta_1 X_i + u_i \\text{, } i = 1,\\dots,n$$
where

1. The error term $u_i$ has conditional mean zero given $X_i$: $E(u_i|X_i) = 0$.
2. $(X_i,Y_i), i = 1,\\dots,n$ are independent and identically distributed (i.i.d.) draws from their joint distribution.
3. Large outliers are unlikely: $X_i$ and $Y_i$ have nonzero finite fourth moments.
</p>
</div>
')
cat('\\begin{keyconcepts}[The Least Squares Assumptions]{4.3}
$$Y_i = \\beta_0 + \\beta_1 X_i + u_i \\text{, } i = 1,\\dots,n$$
where

\\begin{enumerate}
\\item The error term $u_i$ has conditional mean zero given $X_i$: $E(u_i|X_i) = 0$.
\\item $(X_i,Y_i), i = 1,\\dots,n$ are independent and identically distributed (i.i.d.) draws from their joint distribution.
\\item Large outliers are unlikely: $X_i$ and $Y_i$ have nonzero finite fourth moments.
\\end{enumerate}
\\end{keyconcepts}
')
# set a seed to make the results reproducible
set.seed(321)

# simulate the data 
X <- runif(50, min = -5, max = 5)
u <- rnorm(50, sd = 5)  

# the true relation  
Y <- X^2 + 2 * X + u                

# estimate a simple regression model 
mod_simple <- lm(Y ~ X)

# predict using a quadratic model 
prediction <- predict(lm(Y ~ X + I(X^2)), data.frame(X = sort(X)))

# plot the results
plot(Y ~ X)
abline(mod_simple, col = "red")
lines(sort(X), prediction)
# set seed
set.seed(123)

# generate a date vector
Date <- seq(as.Date("1951/1/1"), as.Date("2000/1/1"), "years")

# initialize the employment vector
X <- c(5000, rep(NA, length(Date)-1))

# generate time series observations with random influences
for (i in 2:length(Date)) {
  
    X[i] <- -50 + 0.98 * X[i-1] + rnorm(n = 1, sd = 200)
    
}

#plot the results
plot(x = Date, 
     y = X, 
     type = "l", 
     col = "steelblue", 
     ylab = "Workers", 
     xlab = "Time")
# set seed
set.seed(123)

# generate the data
X <- sort(runif(10, min = 30, max = 70))
Y <- rnorm(10 , mean = 200, sd = 50)
Y[9] <- 2000

# fit model with outlier
fit <- lm(Y ~ X)

# fit model without outlier
fitWithoutOutlier <- lm(Y[-9] ~ X[-9])

# plot the results
plot(Y ~ X)
abline(fit)
abline(fitWithoutOutlier, col = "red")
cat('
<div class = "keyconcept" id="KC4.4">
<h3 class = "right"> Key Concept 4.4 </h3>          
<h3 class = "left"> Large Sample Distribution of $\\hat\\beta_0$ and $\\hat\\beta_1$ </h3>

<p> If the least squares assumptions in Key Concept 4.3 hold, then in large samples $\\hat\\beta_0$ and $\\hat\\beta_1$ have a joint normal sampling distribution. The large sample normal distribution of $\\hat\\beta_1$ is $\\mathcal{N}(\\beta_1, \\sigma^2_{\\hat\\beta_1})$, where the variance of the distribution, $\\sigma^2_{\\hat\\beta_1}$, is 

\\begin{align}
\\sigma^2_{\\hat\\beta_1} = \\frac{1}{n} \\frac{Var \\left[ \\left(X_i - \\mu_X \\right) u_i  \\right]}  {\\left[  Var \\left(X_i \\right)  \\right]^2}. (\\#eq:olsvar1)
\\end{align}

The large sample normal distribution of $\\hat\\beta_0$ is $\\mathcal{N}(\\beta_0, \\sigma^2_{\\hat\\beta_0})$ with

\\begin{align}
\\sigma^2_{\\hat\\beta_0} =  \\frac{1}{n} \\frac{Var \\left( H_i u_i \\right)}{ \\left[  E \\left(H_i^2  \\right)  \\right]^2 } \\ , \\ \\text{where} \\ \\ H_i = 1 - \\left[ \\frac{\\mu_X} {E \\left( X_i^2\\right)} \\right] X_i. (\\#eq:olsvar2)
\\end{align}

The interactive simulation below continuously generates random samples $(X_i,Y_i)$ of $200$ observations where $E(Y\\vert X) = 100 + 3X$, estimates a simple regression model, stores the estimate of the slope $\\beta_1$ and visualizes the distribution of the $\\widehat{\\beta}_1$s observed so far using a histogram. The idea here is that for a large number of $\\widehat{\\beta}_1$s, the histogram gives a good approximation of the sampling distribution of the estimator. By decreasing the time between two sampling iterations, it becomes clear that the shape of the histogram approaches the characteristic bell shape of a normal distribution centered at the true slope of $3$.

<iframe height="470" width="700" frameborder="0" scrolling="no" src="SmallSampleDIstReg.html"></iframe>

</p>
</div>
')
cat('\\begin{keyconcepts}[Large Sample Distribution of $\\hat\\beta_0$ and $\\hat\\beta_1$]{4.4}
If the least squares assumptions in Key Concept 4.3 hold, then in large samples $\\hat\\beta_0$ and $\\hat\\beta_1$ have a joint normal sampling distribution. The large sample normal distribution of $\\hat\\beta_1$ is $\\mathcal{N}(\\beta_1, \\sigma^2_{\\hat\\beta_1})$, where the variance of the distribution, $\\sigma^2_{\\hat\\beta_1}$, is 

\\begin{equation}
\\sigma^2_{\\hat\\beta_1} = \\frac{1}{n} \\frac{Var \\left[ \\left(X_i - \\mu_X \\right) u_i  \\right]}  {\\left[  Var \\left(X_i \\right)  \\right]^2}.
\\end{equation}

The large sample normal distribution of $\\hat\\beta_0$ is $\\mathcal{N}(\\beta_0, \\sigma^2_{\\hat\\beta_0})$ with

\\begin{equation}
\\sigma^2_{\\hat\\beta_0} =  \\frac{1}{n} \\frac{Var \\left( H_i u_i \\right)}{ \\left[  E \\left(H_i^2  \\right)  \\right]^2 } \\ , \\ \\text{where} \\ \\ H_i = 1 - \\left[ \\frac{\\mu_X} {E \\left( X_i^2\\right)} \\right] X_i.
\\end{equation}

The interactive simulation below continuously generates random samples $(X_i,Y_i)$ of $200$ observations where $E(Y\\vert X) = 100 + 3X$, estimates a simple regression model, stores the estimate of the slope $\\beta_1$ and visualizes the distribution of the $\\widehat{\\beta}_1$s observed so far using a histogram. The idea here is that for a large number of $\\widehat{\\beta}_1$s, the histogram gives a good approximation of the sampling distribution of the estimator. By decreasing the time between two sampling iterations, it becomes clear that the shape of the histogram approaches the characteristic bell shape of a normal distribution centered at the true slope of $3$.\\vspace{0.5cm}

\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}

\\end{keyconcepts}
')
# simulate data
N <- 100000
X <- runif(N, min = 0, max = 20)
u <- rnorm(N, sd = 10)

# population regression
Y <- -2 + 3.5 * X + u
population <- data.frame(X, Y)
# set sample size
n <- 100

# compute the variance of beta_hat_0
H_i <- 1 - mean(X) / mean(X^2) * X
var_b0 <- var(H_i * u) / (n * mean(H_i^2)^2 )

# compute the variance of hat_beta_1
var_b1 <- var( ( X - mean(X) ) * u ) / (100 * var(X)^2)
# print variances to the console
var_b0
var_b1
# set repetitions and sample size
n <- 100
reps <- 10000

# initialize the matrix of outcomes
fit <- matrix(ncol = 2, nrow = reps)

# loop sampling and estimation of the coefficients
for (i in 1:reps){
  
 sample <- population[sample(1:N, n), ]
 fit[i, ] <- lm(Y ~ X, data = sample)$coefficients
 
}

# compute variance estimates using outcomes
var(fit[, 1])
var(fit[, 2])
# divide plotting area as 1-by-2 array
par(mfrow = c(1, 2))

# plot histograms of beta_0 estimates
hist(fit[, 1],
     cex.main = 1,
     main = bquote(The ~ Distribution  ~ of ~ 10000 ~ beta[0] ~ Estimates), 
     xlab = bquote(hat(beta)[0]), 
     freq = F)

# add true distribution to plot
curve(dnorm(x, 
            -2, 
            sqrt(var_b0)), 
      add = T, 
      col = "darkred")

# plot histograms of beta_hat_1 
hist(fit[, 2],
    cex.main = 1,
     main = bquote(The ~ Distribution  ~ of ~ 10000 ~ beta[1] ~ Estimates), 
     xlab = bquote(hat(beta)[1]), 
     freq = F)

# add true distribution to plot
curve(dnorm(x, 
            3.5, 
            sqrt(var_b1)), 
      add = T, 
      col = "darkred")
# set seed for reproducibility
set.seed(1)

# set repetitions and the vector of sample sizes
reps <- 1000
n <- c(100, 250, 1000, 3000)

# initialize the matrix of outcomes
fit <- matrix(ncol = 2, nrow = reps)

# divide the plot panel in a 2-by-2 array
par(mfrow = c(2, 2))

# loop sampling and plotting

# outer loop over n
for (j in 1:length(n)) {
  
  # inner loop: sampling and estimating of the coefficients
  for (i in 1:reps){
    
    sample <- population[sample(1:N, n[j]), ]
    fit[i, ] <- lm(Y ~ X, data = sample)$coefficients
    
  }
  
  # draw density estimates
  plot(density(fit[ ,2]), xlim=c(2.5, 4.5), 
       col = j, 
       main = paste("n=", n[j]), 
       xlab = bquote(hat(beta)[1]))
  
}
# load the MASS package
library(MASS)

# set seed for reproducibility
set.seed(4)

# simulate bivarite normal data
bvndata <- mvrnorm(100, 
                mu = c(5, 5), 
                Sigma = cbind(c(5, 4), c(4, 5))) 

# assign column names / convert to data.frame
colnames(bvndata) <- c("X", "Y")
bvndata <- as.data.frame(bvndata)

# subset the data
set1 <- subset(bvndata, abs(mean(X) - X) > 1)
set2 <- subset(bvndata, abs(mean(X) - X) <= 1)

# plot both data sets
plot(set1, 
     xlab = "X", 
     ylab = "Y", 
     pch = 19)

points(set2, 
       col = "steelblue", 
       pch = 19)
# estimate both regression lines
lm.set1 <- lm(Y ~ X, data = set1)
lm.set2 <- lm(Y ~ X, data = set2)

# plot observations
plot(set1, xlab = "X", ylab = "Y", pch = 19)
points(set2, col = "steelblue", pch = 19)

# add both lines to the plot
abline(lm.set1, col = "green")
abline(lm.set2, col = "red")
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 1. Class Sizes and Test Scores {-}

A researcher wants to analyze the relationship between class size (measured by the student-teacher ratio) and the average test score. Therefore he measures both variables in $10$ different classes and ends up with the following results.

<!--html_preserve-->

  <table>
      <tr>
        <td><b>Class Size</b></td>
        <td>23</td>
        <td>19</td>
        <td>30</td>
        <td>22</td>
        <td>23</td>
        <td>29</td>
        <td>35</td>
        <td>36</td>
        <td>33</td>
        <td>25</td>
      </tr>
      <tr>
        <td><b>Test Score</b></td>
        <td>430</td>
        <td>430</td>
        <td>333</td>
        <td>410</td>
        <td>390</td>
        <td>377</td>
        <td>325</td>
        <td>310</td>
        <td>328</td>
        <td>375</td>
      </tr>
    </table>

<!--/html_preserve-->

**Instructions:**

+ Create the vectors <tt>cs</tt> (the class size) and <tt>ts</tt> (the test score), containing the observations above.  

+ Draw a scatterplot of the results using <tt>plot()</tt>.

<iframe src="DCL/ex4_1.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>


</div>')
} else {
  cat('\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}')
}
if (my_output=='html') {
  cat('
<div  class = "DCexercise">

#### 2. Mean, Variance, Covariance and Correlation {-}

The vectors <tt>cs</tt> and <tt>ts</tt> are available in the working environment (you can check this: type their names into the console and press enter).

**Instructions:**

+ Compute the mean, the sample variance and the sample standard deviation of <tt>ts</tt>. 

+ Compute the covariance and the correlation coefficient for <tt>ts</tt> and <tt>cs</tt>.

<iframe src="DCL/ex4_2.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:** Use the <tt>R</tt> functions presented in this chapter: <tt>mean()</tt>, <tt>sd()</tt>, <tt>cov()</tt>, <tt>cor()</tt> and <tt>var()</tt>.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 3. Simple Linear Regression {-}

The vectors <tt>cs</tt> and <tt>ts</tt> are available in the working environment. 

**Instructions:**

+ The function <tt>lm()</tt> is part of the package <tt>AER</tt>. Attach the package using <tt>library()</tt>.

+ Use <tt>lm()</tt> to estimate the regression model $$TestScore_i = \\beta_0 + \\beta_1 STR_i + u_i.$$ Assign the result to <tt>mod</tt>.

+ Obtain a statistical summary of the model.

<iframe src="DCL/ex4_3.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 4. The Model Object {-}

Let us see how an object of class <tt>lm</tt> is structured. 

The vectors <tt>cs</tt> and <tt>ts</tt> as well as the model object <tt>mod</tt> from the previous exercise are available in your workspace.

**Instructions:**

+ Use <tt>class()</tt> to learn about the class of the object <tt>mod</tt>.
+ <tt>mod</tt> is an object of type <tt>list</tt> with named entries. Check this using the function <tt>is.list()</tt>.
+ See what information you can obtain from <tt>mod</tt> using <tt>names()</tt>. 
+ Read out an arbitrary entry of the object <tt>mod</tt> using the <tt>$</tt> operator.

<iframe src="DCL/ex4_4.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 5. Plotting the Regression Line {-}

You are provided with the code for the scatterplot in <tt>script.R</tt>

**Instructions:**

+ Add the regression line to the scatterplot from a few exercises before.

+ The object <tt>mod</tt> is available in your working environment.

<iframe src="DCL/ex4_5.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:** Use the function <tt>abline()</tt>.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 6. Summary of a Model Object {-}

Now read out and store some of the information that is contained in the output of <tt>summary()</tt>.

**Instructions:**

+ Assign the output of <tt>summary(mod)</tt> to the variable <tt>s</tt>.

+ Check entry names of the object <tt>s</tt>.

+ Create a new variable <tt>R2</tt> and assign the $R^2$ of the regression.

The object <tt>mod</tt> is available in your working environment.

<iframe src="DCL/ex4_6.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 7. Estimated Coefficients {-}

The function <tt>summary()</tt> also provides information on the statistical significance of the estimated coefficients.

**Instructions:**

Extract the named $2\\times4$ matrix with estimated coefficients, standard errors, $t$-statistics and corresponding $p$-values from the model summary <tt>s</tt>. Save this matrix in an object named <tt>coefs</tt>.

The objects <tt>mod</tt> and <tt>s</tt> are available in your working environment.

<iframe src="DCL/ex4_7.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 8. Dropping the Intercept {-}

So far, we have estimated regression models consisting of an intercept and a single regressor. In this exercise you will learn how to specify and how to estimate regression a model without intercept.

Note that excluding the intercept from a regression model might be a dodgy practice in some applications as this imposes the conditional expectation function of the dependent variable to be zero if the regressor is zero.

**Instructions:**

+ Figure out how the <tt>formula</tt> argument must be specified for a regression of <tt>ts</tt> solely on <tt>cs</tt>, i.e., a regression without intercept. Google is your friend!

+ Estimate the regression model without intercept and store the result in <tt>mod_ni</tt>.

The vectors <tt>cs</tt>, <tt>ts</tt> and the model object <tt>mod</tt> from previous exercises are available in the working environment.

<iframe src="DCL/ex4_8.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 9. Regression Output: No Constant Case {-}

In Exercise 8 you have estimated a model without intercept. The estimated regression function is

$$\\widehat{TestScore} = \\underset{(1.36)}{12.65} \\times STR.$$

**Instructions:**

Convince yourself that everything is as stated above: extract the coefficient matrix from the summary of <tt>mod_ni</tt> and store it in a variable named <tt>coef</tt>.

The vectors <tt>cs</tt>, <tt>ts</tt> as well as the model object <tt>mod_ni</tt> from the previous exercise are available in your working environment.

<iframe src="DCL/ex4_9.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:** An entry of a named list can be accessed using the <tt>$</tt> operator.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 10. Regression Output: No Constant Case --- Ctd. {-}

In Exercises 8 and 9 you have dealt with a model without intercept. The estimated regression function was

$$\\widehat{TestScore_i} = \\underset{(1.36)}{12.65} \\times STR_i.$$

The coefficient matrix <tt>coef</tt> from Exercise 9 contains the estimated coefficient on $STR$, its standard error, the $t$-statistic of the significance test and the corresponding $p$-value.

**Instructions:**

+ Print the contents of <tt>coef</tt> to the console.
+ Convince yourself that the reported $t$-statistic is correct: use the entries of <tt>coef</tt> to compute the $t$-statistic and save it to <tt>t_stat</tt>.

The matrix <tt>coef</tt> from the previous exercise is available in your working environment.

<iframe src="DCL/ex4_10.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:** 

+ <tt>X[a,b]</tt> returns the <tt>[a,b]</tt> element of the matrix <tt>X</tt>. 

+ The $t$-statistic for a test of the hypothesis $H_0: \\beta_1 = 0$ is computed as $$t = \\frac{\\hat{\\beta}_1}{SE(\\hat{\\beta}_1)}.$$

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 11. Two Regressions, One Plot {-}

The two estimated regression models from the previous exercises are

$$\\widehat{TestScore_i} = \\underset{(1.36)}{12.65} \\times STR_i$$

and

$$\\widehat{TestScore_i} = \\underset{(23.96)}{567.4272} \\underset{(0.85)}{-7.1501} \\times STR_i.$$

You are provided with the code line <tt>plot(cs, ts)</tt> which creates a scatterplot of <tt>ts</tt> and <tt>cs</tt>. Note that this line must be executed before calling <tt>abline()</tt>! You may color the regression lines by using, e.g., <tt>col = "red"</tt> or <tt>col = "blue"</tt> as an additional argument to <tt>abline()</tt> for better distinguishability.

The vectors <tt>cs</tt> and <tt>ts</tt> as well as the list objects <tt>mod</tt> and <tt>mod_ni</tt> from previous exercises are availabe in your working environment.

**Instructions:**

Generate a scatterplot of <tt>ts</tt> and <tt>cs</tt> and add the estimated regression lines of <tt>mod</tt> and <tt>mod_ni</tt>.

<iframe src="DCL/ex4_11.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 12. $TSS$ and $SSR$ {-}

If graphical inspection does not help, researchers resort to analytic techniques in order to detect if a model fits the data at hand good or better than another model.

Let us go back to the simple regression model including an intercept. The estimated regression line for <tt>mod</tt> was

$$\\widehat{TestScore_i} = 567.43 - 7.15 \\times STR_i, \\, R^2 = 0.8976, \\, SER=15.19.$$

You can check this as <tt>mod</tt> and the vectors <tt>cs</tt> and <tt>ts</tt> are available in your working environment.

**Instructions:**

+ Compute $SSR$, the sum of squared residuals, and save it to <tt>ssr</tt>.
+ Compute $TSS$, the total sum of squares, and save it to <tt>tss</tt>.

<iframe src="DCL/ex4_12.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 13. The $R^2$ of a Regression Model {-}

The $R^2$ of the regression saved in <tt>mod</tt> is $0.8976$. You can check this by executing <tt>summary(mod)$r.squared</tt> in the console below.

Remember the formula of $R^2$:

$$R^2 = \\frac{ESS}{TSS} = 1 - \\frac{SSR}{TSS}$$

The objects <tt>mod</tt>, <tt>tss</tt> and <tt>ssr</tt> from the previous exercise are available in your working environment.

**Instructions:**

+ Use <tt>ssr</tt> and <tt>tss</tt> to compute $R^2$ manually. *Round* the result to *four* decimal places and save it to <tt>R2</tt>.
+ Use the logical operator <tt>==</tt> to check whether your result matches the value mentioned above.

<iframe src="DCL/ex4_13.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

You may round numeric values using the function <tt>round()</tt>.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 14. The Standard Error of The Regression {-}

The standard error of the Regression in the simple regression model is $$SER = \\frac{1}{n-2} \\sum_{i=1}^n \\widehat{u}_i^2 =\\sqrt{\\frac{SSR}{n-2}}.$$ $SER$ measures the size of an average residual which is an estimate of the magnitude of a typical regression error.

The model object <tt>mod</tt> and the vectors <tt>cs</tt> and </tt>ts</tt> are available in your workspace.

**Instructions:**

+ Use <tt>summary()</tt> to obtain the $SER$ for the regression of <tt>ts</tt> on <tt>cs</tt> saved in the model object <tt>mod</tt>. Save the result in the variable <tt>SER</tt>.

+ Use <tt>SER</tt> to compute the $SSR$ and store it in <tt>SSR</tt>.

+ Check that <tt>SSR</tt> is indeed the $SSR$ by comparing <tt>SSR</tt> to the result of <tt>sum(mod$residuals^2)</tt>

<iframe src="DCL/ex4_14.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 15. The Estimated Covariance Matrix {-}

As has been discussed in Chapter \\@ref(tlsa), the OLS estimators $\\widehat{\\beta}_0$ and $\\widehat{\\beta}_1$ are functions of the random error term. Therefore, they are random variables themselves. For two or more random variables, their covariances and variances are summarized by a *variance-covariance matrix* (which is often simply called the *covariance matrix*). Taking the square root of the diagonal elements of the estimated covariance matrix obtains $SE(\\widehat\\beta_0)$ and $SE(\\widehat\\beta_1)$, the standard errors of $\\widehat{\\beta}_0$ and $\\widehat{\\beta}_1$. 

<tt>summary()</tt> computes an estimate of this matrix. The respective entry in the output of summary (remember that <tt>summary()</tt> produces a list) is called <tt>cov.unscaled</tt>. The model object <tt>mod</tt> is available in your workspace.

**Instructions:**

+ Use <tt>summary()</tt> to obtain the covariance matrix estimate for the regression of test scores on student-teacher ratios stored in the model object <tt>mod</tt>. Save the result to <tt>cov_matrix</tt>.

+ Obtain the diagonal elements of <tt>cov_matrix</tt>, compute their square root and assign the result to the variable <tt>SEs</tt>.

<iframe src="DCL/ex4_15.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:** <tt>diag(A)</tt> returns a vector containing the diagonal elements of the matrix <tt>A</tt>.

</div>')}
