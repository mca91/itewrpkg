## library(AER)
## library(scales)
cat('
<div class = "keyconcept" id="KC5.1">
<h3 class = "right"> Key Concept 5.1 </h3>
<h3 class = "left"> General Form of the $t$-Statistic </h3>
Remember from Chapter \\@ref(arosur) that a general $t$-statistic has the form
$$ t = \\frac{\\text{estimated value} - \\text{hypothesized value}}{\\text{standard error of the estimator}}.$$
</div>
')
cat('\\begin{keyconcepts}[General Form of the $t$-Statistic]{5.1}
Remember from Chapter \\ref{arosur} that a general $t$-statistic has the form
$$ t = \\frac{\\text{estimated value} - \\text{hypothesized value}}{\\text{standard error of the estimator}}.$$
\\end{keyconcepts}
')
cat('
<div class = "keyconcept" id="KC5.2">
<h3 class = "right"> Key Concept 5.2 </h3>
<h3 class = "left"> Testing Hypotheses regarding $\\beta_1$ </h3>

For testing the hypothesis $H_0: \\beta_1 = \\beta_{1,0}$, we need to perform the following steps:

1. Compute the standard error of $\\hat{\\beta}_1$, $SE(\\hat{\\beta}_1)$

\\[ SE(\\hat{\\beta}_1) = \\sqrt{ \\hat{\\sigma}^2_{\\hat{\\beta}_1} } \\ \\ , \\ \\ 
  \\hat{\\sigma}^2_{\\hat{\\beta}_1} = \\frac{1}{n} \\times \\frac{\\frac{1}{n-2} \\sum_{i=1}^n (X_i - \\overline{X})^2 \\hat{u_i}^2 }{ \\left[ \\frac{1}{n} \\sum_{i=1}^n (X_i - \\overline{X})^2 \\right]^2}.
\\]

2. Compute the $t$-statistic

\\[ t = \\frac{\\hat{\\beta}_1 - \\beta_{1,0}}{ SE(\\hat{\\beta}_1) }. \\]

3. Given a two sided alternative ($H_1:\\beta_1 \\neq \\beta_{1,0}$) we reject at the $5\\%$ level if $|t^{act}| > 1.96$ or, equivalently, if the $p$-value is less than $0.05$.<br>  
Recall the definition of the $p$-value:  

  \\begin{align*}
    p \\text{-value} =& \\, \\text{Pr}_{H_0} \\left[ \\left| \\frac{ \\hat{\\beta}_1 - \\beta_{1,0} }{ SE(\\hat{\\beta}_1) } \\right| > \\left|        \\frac{ \\hat{\\beta}_1^{act} - \\beta_{1,0} }{ SE(\\hat{\\beta}_1) } \\right| \\right] \\\\
    =& \\, \\text{Pr}_{H_0} (|t| > |t^{act}|) \\\\
    \\approx& \\, 2 \\cdot \\Phi(-|t^{act}|)
  \\end{align*}  

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; The last transformation is due to the normal approximation for large samples.

</div>
')
cat('\\begin{keyconcepts}[Testing Hypotheses regarding $\\beta_1$]{5.2}
For testing the hypothesis $H_0: \\beta_1 = \\beta_{1,0}$, we need to perform the following steps:\\newline

\\begin{enumerate}
\\item Compute the standard error of $\\hat{\\beta}_1$, $SE(\\hat{\\beta}_1)$

\\[ SE(\\hat{\\beta}_1) = \\sqrt{ \\hat{\\sigma}^2_{\\hat{\\beta}_1} } \\ \\ , \\ \\ 
  \\hat{\\sigma}^2_{\\hat{\\beta}_1} = \\frac{1}{n} \\times \\frac{\\frac{1}{n-2} \\sum_{i=1}^n (X_i - \\overline{X})^2 \\hat{u_i}^2 }{ \\left[ \\frac{1}{n} \\sum_{i=1}^n (X_i - \\overline{X})^2 \\right]^2}.
\\]

\\item Compute the $t$-statistic

\\[ t = \\frac{\\hat{\\beta}_1 - \\beta_{1,0}}{ SE(\\hat{\\beta}_1) }. \\]

\\item Given a two sided alternative ($H_1:\\beta_1 \\neq \\beta_{1,0}$) we reject at the $5\\%$ level if $|t^{act}| > 1.96$ or, equivalently, if the $p$-value is less than $0.05$.\\newline  
Recall the definition of the $p$-value:  

  \\begin{align*}
    p \\text{-value} =& \\, \\text{Pr}_{H_0} \\left[ \\left| \\frac{ \\hat{\\beta}_1 - \\beta_{1,0} }{ SE(\\hat{\\beta}_1) } \\right| > \\left|        \\frac{ \\hat{\\beta}_1^{act} - \\beta_{1,0} }{ SE(\\hat{\\beta}_1) } \\right| \\right] \\\\
    =& \\, \\text{Pr}_{H_0} (|t| > |t^{act}|) \\\\
    =& \\, 2 \\cdot \\Phi(-|t^{act}|)
  \\end{align*}  

The last transformation is due to the normal approximation for large samples.
\\end{enumerate}
\\end{keyconcepts}
')
library(AER)
data(CASchools)
CASchools$STR <- CASchools$students/CASchools$teachers     # class size
CASchools$score <- (CASchools$read + CASchools$math)/2     # average test-score
linear_model <- lm(score ~ STR, data = CASchools)          # estimate the model

# print the summary of the coefficients to the console
summary(linear_model)$coefficients
# determine residual degrees of freedom
linear_model$df.residual
2 * pt(-4.751327, df = 418)
2 * pnorm(-4.751327)

# Plot the standard normal on the support [-6,6]
t <- seq(-6, 6, 0.01)

plot(x = t, 
     y = dnorm(t, 0, 1), 
     type = "l", 
     col = "steelblue", 
     lwd = 2, 
     yaxs = "i", 
     axes = F, 
     ylab = "", 
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "=-0.47"), 
     cex.lab = 0.7,
     cex.main = 1)

tact <- -4.75

axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7)

# Shade the critical regions using polygon():

# critical region in left tail
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
        y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0), 
        col = 'orange')

# critical region in right tail

polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.96, 6, 0.01)), 0), 
        col = 'orange')

# Add arrows and texts indicating critical regions and the p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)

arrows(-5, 0.16, -4.75, 0, length = 0.1)
arrows(5, 0.16, 4.75, 0, length = 0.1)

text(-3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)

text(-5, 0.18, 
     labels = expression(paste("-|",t[act],"|")), 
     cex = 0.7)
text(5, 0.18, 
     labels = expression(paste("|",t[act],"|")), 
     cex = 0.7)

# Add ticks indicating critical values at the 0.05-level, t^act and -t^act 
rug(c(-1.96, 1.96), ticksize  = 0.145, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize  = -0.0451, lwd = 2, col = "darkgreen")
cat('
<div class = "keyconcept" id="KC5.3">
<h3 class = "right"> Key Concept 5.3 </h3>
<h3 class = "left"> A Confidence Interval for $\\beta_i$ </h3>

Imagine you could draw all possible random samples of given size. The interval that contains the true value $\\beta_i$ in $95\\%$ of all samples is given by the expression

\\[ \\text{CI}_{0.95}^{\\beta_i} = \\left[ \\hat{\\beta}_i - 1.96 \\times SE(\\hat{\\beta}_i) \\, , \\, \\hat{\\beta}_i + 1.96 \\times SE(\\hat{\\beta}_i) \\right]. \\]

Equivalently, this interval can be seen as the set of null hypotheses for which a $5\\%$ two-sided hypothesis test does not reject.
</div>
')
cat('\\begin{keyconcepts}[A Confidence Interval for $\\beta_i$]{5.3}
Imagine you could draw all possible random samples of given size. The interval that contains the true value $\\beta_i$ in $95\\%$ of all samples is given by the expression

\\[ \\text{CI}_{0.95}^{\\beta_i} = \\left[ \\hat{\\beta}_i - 1.96 \\times SE(\\hat{\\beta}_i) \\, , \\, \\hat{\\beta}_i + 1.96 \\times SE(\\hat{\\beta}_i) \\right]. \\]

Equivalently, this interval can be seen as the set of null hypotheses for which a $5\\%$ two-sided hypothesis test does not reject.
\\end{keyconcepts}
')
# set seed for reproducibility
set.seed(4)

# generate and plot the sample data
Y <- rnorm(n = 100, 
           mean = 5, 
           sd = 5)

plot(Y, 
     pch = 19, 
     col = "steelblue")
cbind(CIlower = mean(Y) - 1.96 * 5 / 10, CIupper = mean(Y) + 1.96 * 5 / 10)
# set seed
set.seed(1)

# initialize vectors of lower and upper interval boundaries
lower <- numeric(10000)
upper <- numeric(10000)

# loop sampling / estimation / CI
for(i in 1:10000) {
  
  Y <- rnorm(100, mean = 5, sd = 5)
  lower[i] <- mean(Y) - 1.96 * 5 / 10
  upper[i] <- mean(Y) + 1.96 * 5 / 10
  
}

# join vectors of interval bounds in a matrix
CIs <- cbind(lower, upper)
mean(CIs[, 1] <= 5 & 5 <= CIs[, 2])
# identify intervals not covering mu
# (4 intervals out of 100)
ID <- which(!(CIs[1:100, 1] <= 5 & 5 <= CIs[1:100, 2]))

# initialize the plot
plot(0, 
     xlim = c(3, 7), 
     ylim = c(1, 100), 
     ylab = "Sample", 
     xlab = expression(mu), 
     main = "Confidence Intervals")

# set up color vector
colors <- rep(gray(0.6), 100)
colors[ID] <- "red"

# draw reference line at mu=5
abline(v = 5, lty = 2)

# add horizontal bars representing the CIs
for(j in 1:100) {
  
  lines(c(CIs[j, 1], CIs[j, 2]), 
        c(j, j), 
        col = colors[j], 
        lwd = 2)
  
}
# compute 95% confidence interval for coefficients in 'linear_model'
confint(linear_model)
# compute 95% confidence interval for coefficients in 'linear_model' by hand
lm_summ <- summary(linear_model)

c("lower" = lm_summ$coef[2,1] - qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2],
  "upper" = lm_summ$coef[2,1] + qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2])
## # Create the dummy variable as defined above
## CASchools$D <- CASchools$STR < 20
## 
## # Plot the data
## plot(CASchools$D, CASchools$score,            # provide the data to be plotted
##      pch = 20,                                # use filled circles as plot symbols
##      cex = 0.5,                               # set size of plot symbols to 0.5
##      col = "Steelblue",                       # set the symbols' color to "Steelblue"
##      xlab = expression(D[i]),                 # Set title and axis names
##      ylab = "Test Score",
##      main = "Dummy Regression")
# Create the dummy variable as defined above
CASchools$D <- CASchools$STR < 20

# estimte the dummy regression
dummy_model <- lm(score ~ D, data = CASchools)

# Plot the data
plot(CASchools$D, CASchools$score, 
     pch = 20, cex = 0.5 , col = "Steelblue",
     xlab = expression(D[i]), ylab = "Test Score",
     main = "Dummy Regression")
#
points(CASchools$D, predict(dummy_model), col = "red", pch = 20)
# estimate the dummy regression model
dummy_model <- lm(score ~ D, data = CASchools)
summary(dummy_model)
## <tt>summary()</tt> reports the $p$-value of the test that the coefficient on <tt>(Intercept)</tt> is zero to to be <tt>< 2e-16</tt>. This scientific notation states that the $p$-value is smaller than $\frac{2}{10^{16}}$, so a very small number. The reason for this is that computers cannot handle arbitrary small numbers. In fact, $\frac{2}{10^{16}}$ is the smallest possble number <tt>R</tt> can work with.

## # add group specific predictions to the plot
## points(x = CASchools$D,
##        y = predict(dummy_model),
##        col = "red",
##        pch = 20)
# confidence intervals for coefficients in the dummy regression model
confint(dummy_model)
cat('
<div class = "keyconcept" id="KC5.4">
<h3 class = "right"> Key Concept 5.4 </h3>          
<h3 class = "left"> Heteroskedasticity and Homoskedasticity </h3>
- The error term of our regression model is homoskedastic if the variance of the conditional distribution of $u_i$ given $X_i$, $Var(u_i|X_i=x)$, is constant *for all* observations in our sample:
\\[ \\text{Var}(u_i|X_i=x) = \\sigma^2 \\ \\forall \\ i=1,\\dots,n. \\]

- If instead there is dependence of the conditional variance of $u_i$ on $X_i$, the error term is said to be heteroskedastic. We then write
\\[ \\text{Var}(u_i|X_i=x) = \\sigma_i^2 \\ \\forall \\ i=1,\\dots,n. \\]

- Homoskedasticity is a *special case* of heteroskedasticity.
</div>
')
cat('\\begin{keyconcepts}[Heteroskedasticity and Homoskedasticity]{5.4}
\\begin{itemize}
\\item The error term of our regression model is homoskedastic if the variance of the conditional distribution of $u_i$ given $X_i$, $Var(u_i|X_i=x)$, is constant \\textit{for all} observations in our sample:
\\[ \\text{Var}(u_i|X_i=x) = \\sigma^2 \\ \\forall \\ i=1,\\dots,n. \\]

\\item If instead there is dependence of the conditional variance of $u_i$ on $X_i$, the error term is said to be heteroskedastic. We then write
\\[ \\text{Var}(u_i|X_i=x) = \\sigma_i^2 \\ \\forall \\ i=1,\\dots,n. \\]

\\item Homoskedasticity is a \\textit{special case} of heteroskedasticity.
\\end{itemize}
\\end{keyconcepts}
')
# load scales package for adjusting color opacities
library(scales)

# generate some heteroskedastic data:

# set seed for reproducibility
set.seed(123) 

# set up vector of x coordinates
x <- rep(c(10, 15, 20, 25), each = 25)

# initialize vector of errors
e <- c()

# sample 100 errors such that the variance increases with x
e[1:25] <- rnorm(25, sd = 10)
e[26:50] <- rnorm(25, sd = 15)
e[51:75] <- rnorm(25, sd = 20)
e[76:100] <- rnorm(25, sd = 25)

# set up y
y <- 720 - 3.3 * x + e

# Estimate the model 
mod <- lm(y ~ x)

# Plot the data
plot(x = x, 
     y = y, 
     main = "An Example of Heteroskedasticity",
     xlab = "Student-Teacher Ratio",
     ylab = "Test Score",
     cex = 0.5, 
     pch = 19, 
     xlim = c(8, 27), 
     ylim = c(600, 710))

# Add the regression line to the plot
abline(mod, col = "darkred")

# Add boxplots to the plot
boxplot(formula = y ~ x, 
        add = TRUE, 
        at = c(10, 15, 20, 25), 
        col = alpha("gray", 0.4), 
        border = "black"
        )
# load package and attach data
library(AER)
data("CPSSWEducation")
attach(CPSSWEducation)

# get an overview
summary(CPSSWEducation)

# estimate a simple regression model
labor_model <- lm(earnings ~ education)

# plot observations and add the regression line
plot(education, 
     earnings, 
     ylim = c(0, 150))

abline(labor_model, 
       col = "steelblue", 
       lwd = 2)
# print the contents of labor_model to the console
labor_model
# compute a 95% confidence interval for the coefficients in the model
confint(labor_model)
# Store model summary in 'model'
model <- summary(linear_model)

# Extract the standard error of the regression from model summary
SER <- model$sigma

# Compute the variation in 'size'
V <- (nrow(CASchools)-1) * var(CASchools$STR)

# Compute the standard error of the slope parameter's estimator and print it
SE.beta_1.hat <- sqrt(SER^2/V)
SE.beta_1.hat

# Use logical operators to see if the value computed by hand matches the one provided 
# in mod$coefficients. Round estimates to four decimal places
round(model$coefficients[2, 2], 4) == round(SE.beta_1.hat, 4)
# compute heteroskedasticity-robust standard errors
vcov <- vcovHC(linear_model, type = "HC1")
vcov
## When we have k > 1 regressors, writing down the equations for a regression model becomes very messy. A more convinient way to denote and estimate so-called multiple regression models (see Chapter \@ref(rmwmr)) is by using matrix algebra. This is why functions like <tt>vcovHC()</tt> produce matrices. In the simple linear regression model, the variances and covariances of the estimators can be gathered in the symmetric variance-covariance matrix

# compute the square root of the diagonal elements in vcov
robust_se <- sqrt(diag(vcov))
robust_se
# we invoke the function `coeftest()` on our model
coeftest(linear_model, vcov. = vcov)
set.seed(905)

# generate heteroskedastic data 
X <- 1:500
Y <- rnorm(n = 500, mean = X, sd = 0.6 * X)

# estimate a simple regression model
reg <- lm(Y ~ X)
# plot the data
plot(x = X, y = Y, 
     pch = 19, 
     col = "steelblue", 
     cex = 0.8)

# add the regression line to the plot
abline(reg, 
       col = "darkred", 
       lwd = 1.5)
## <tt>linearHypothesis()</tt> computes a test statistic that follows an $F$-distribution under the null hypothesis. We will not loose too much words on the underlying theory. In general, the idea of the $F$-test is to compare the fit of different models. When testing a hypothesis about a *single* coefficient using an $F$-test, one can show that the test statistic is simply the square of the corresponding $t$-statistic:

# test hypthesis using the default standard error formula
linearHypothesis(reg, hypothesis.matrix = "X = 1")$'Pr(>F)'[2] < 0.05

# test hypothesis using the robust standard error formula
linearHypothesis(reg, hypothesis.matrix = "X = 1", white.adjust = "hc1")$'Pr(>F)'[2] < 0.05
set.seed(905)

# initialize vectors t and t.rob
t <- c()
t.rob <- c()

# loop sampling and estimation
for (i in 1:10000) {
  
  # sample data
  X <- 1:1000
  Y <- rnorm(n = 1000, mean = X, sd = 0.6*X)

  # estimate regression model
  reg <- lm(Y ~ X)

  # homoskedasdicity-only significance test
  t[i] <- linearHypothesis(reg, "X = 1")$'Pr(>F)'[2] < 0.05

  # robust significance test
  t.rob[i] <- linearHypothesis(reg, "X = 1", white.adjust = "hc1")$'Pr(>F)'[2] < 0.05

}

# compute the fraction of false rejections
round(cbind(t = mean(t), t.rob = mean(t.rob)), 3)
cat('
<div class = "keyconcept" id="KC5.5">
<h3 class = "right"> Key Concept 5.5 </h3>          
<h3 class = "left"> The Gauss-Markov Theorem for $\\hat{\\beta}_1$ </h3>
Suppose that the assumptions made in Key Concept 4.3 hold *and* that the errors are *homoskedastic*. The OLS estimator is the best (in the sense of smallest variance) linear conditionally unbiased estimator (BLUE) in this setting.

Let us have a closer look at what this means:

- Estimators of $\\beta_1$ that are linear functions of the $Y_1, \\dots, Y_n$ and that are unbiased conditionally on the regressor $X_1, \\dots, X_n$ can be written as \\[ \\overset{\\sim}{\\beta}_1 = \\sum_{i=1}^n a_i Y_i \\] where the $a_i$ are weights that are allowed to depend on the $X_i$ but *not* on the $Y_i$. 

- We already know that $\\overset{\\sim}{\\beta}_1$ has a sampling distribution: $\\overset{\\sim}{\\beta}_1$ is a linear function of the $Y_i$ which are random variables. If now \\[ E(\\overset{\\sim}{\\beta}_1 | X_1, \\dots, X_n) = \\beta_1, \\] $\\overset{\\sim}{\\beta}_1$ is a linear unbiased estimator of $\\beta_1$, conditionally on the $X_1, \\dots, X_n$.

- We may ask if $\\overset{\\sim}{\\beta}_1$ is also the *best* estimator in this class, i.e., the most efficient one of all linear conditionally unbiased estimators where "most efficient" means smallest variance. The weights $a_i$ play an important role here and it turns out that OLS uses just the right weights to have the BLUE property. 
</div>
')
cat('\\begin{keyconcepts}[The Gauss-Markov Theorem for $\\hat{\\beta}_1$]{5.5}
Suppose that the assumptions made in Key Concept 4.3 hold \\textit{and} that the errors are \\textit{homoskedastic}. The OLS estimator is the best (in the sense of smallest variance) linear conditionally unbiased estimator (BLUE) in this setting.\\newline

Let us have a closer look at what this means:\\newline

\\begin{itemize}
\\item Estimators of $\\beta_1$ that are linear functions of the $Y_1, \\dots, Y_n$ and that are unbiased conditionally on the regressor $X_1, \\dots, X_n$ can be written as \\[ \\overset{\\sim}{\\beta}_1 = \\sum_{i=1}^n a_i Y_i \\] where the $a_i$ are weights that are allowed to depend on the $X_i$ but \\textit{not} on the $Y_i$. 

\\item We already know that $\\overset{\\sim}{\\beta}_1$ has a sampling distribution: $\\overset{\\sim}{\\beta}_1$ is a linear function of the $Y_i$ which are random variables. If now \\[ E(\\overset{\\sim}{\\beta}_1 | X_1, \\dots, X_n) = \\beta_1, \\] $\\overset{\\sim}{\\beta}_1$ is a linear unbiased estimator of $\\beta_1$, conditionally on the $X_1, \\dots, X_n$.

\\item We may ask if $\\overset{\\sim}{\\beta}_1$ is also the \\textit{best} estimator in this class, i.e., the most efficient one of all linear conditionally unbiased estimators where most efficient means smallest variance. The weights $a_i$ play an important role here and it turns out that OLS uses just the right weights to have the BLUE property. 
\\end{itemize}
\\end{keyconcepts}
')
# set sample size and number of repetitions
n <- 100      
reps <- 1e5

# choose epsilon and create a vector of weights as defined above
epsilon <- 0.8
w <- c(rep((1 + epsilon) / n, n / 2), 
       rep((1 - epsilon) / n, n / 2) )

# draw a random sample y_1,...,y_n from the standard normal distribution, 
# use both estimators 1e5 times and store the result in the vectors 'ols' and 
# 'weightedestimator'

ols <- rep(NA, reps)
weightedestimator <- rep(NA, reps)

for (i in 1:reps) {
  
  y <- rnorm(n)
  ols[i] <- mean(y)
  weightedestimator[i] <- crossprod(w, y)
  
}

# plot kernel density estimates of the estimators' distributions: 

# OLS
plot(density(ols), 
     col = "purple", 
     lwd = 3, 
     main = "Density of OLS and Weighted Estimator",
     xlab = "Estimates")

# weighted
lines(density(weightedestimator), 
      col = "steelblue", 
      lwd = 3) 

# add a dashed line at 0 and add a legend to the plot
abline(v = 0, lty = 2)

legend('topright', 
       c("OLS", "Weighted"), 
       col = c("purple", "steelblue"), 
       lwd = 3)
# initialize two vectors
beta_0 <- c()
beta_1 <- c()

# loop sampling / estimation / t statistics
for (i in 1:10000) {
  
  X <- runif(20, 0, 20)
  Y <- rnorm(n = 20, mean = X)
  reg <- summary(lm(Y ~ X))
  beta_0[i] <- (reg$coefficients[1, 1] - 0)/(reg$coefficients[1, 2])
  beta_1[i] <- (reg$coefficients[2, 1] - 1)/(reg$coefficients[2, 2])
  
}

# plot the distributions and compare with t_18 density:

# divide plotting area
par(mfrow = c(1, 2))

# plot the simulated density of beta_0
plot(density(beta_0), 
     lwd = 2 , 
     main = expression(widehat(beta)[0]), 
     xlim = c(-4, 4))

# add the t_18 density to the plot
curve(dt(x, df = 18), 
      add = T, 
      col = "red", 
      lwd = 2, 
      lty = 2)

# plot the simulated density of beta_1
plot(density(beta_1), 
     lwd = 2, 
     main = expression(widehat(beta)[1]), xlim = c(-4, 4)
     )

# add the t_18 density to the plot
curve(dt(x, df = 18), 
      add = T, 
      col = "red", 
      lwd = 2, 
      lty = 2) 
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 1. Testing Two Null Hypotheses Separately {-}

Consider the estimated regression model

$$ \\widehat{TestScore} = \\underset{(23.96)}{567.43} - \\underset{(0.85)}{7.15} \\times STR, \\, R^2 = 0.8976, \\, SER=15.19 $$

with standard errors in parentheses.

**Instructions:**

+ Compute the $p$-value for a $t$-test of the hypothesis that the intercept is zero against the two-sided alternative that it is non-zero. Save the result to <tt>p_int</tt>
+ Compute the $p$-value for a $t$-test of the hypothesis that the coefficient of <tt>STR</tt> is zero against the two-sided alternative that it is non-zero. Save the result to <tt>p_STR</tt>

<iframe src="DCL/ex5_1.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:** 

Both hypotheses can be tested individually using a two-sided test. Use <tt>pnorm()</tt> to obtain cumulated probabilities for standard normally distributed outcomes.

</div>')
} else {
  cat('\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}')
}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 2. Two Null Hypotheses You Cannot Reject, Can You? {-}

Consider again the estimated regression model

$$\\widehat{TestScore} = \\underset{(23.96)}{567.43} - \\underset{(0.85)}{7.15} \\times STR, \\, R^2 = 0.8976, \\,SER=15.19$$

Can you reject the null hypotheses discussed in the previous code exercise using individual $t$-tests at the $5\\%$ significance level?

The variables <tt>t_int</tt> and <tt>t_STR</tt> are the $t$-statistics. Both are available in your working environment.

**Instructions:**

+ Gather <tt>t_int</tt> and <tt>t_STR</tt> in a vector <tt>test</tt> and use logical operators to check whether the corresponding rejection rule applies.

<iframe src="DCL/ex5_2.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

+ Both tests are two-sided $t$-tests. Key Concept 5.2 recaps how a two-sided $t$-test is conducted. 
+ Use <tt>qnorm()</tt> to obtain standard normal critical values.

</div>')}
if (my_output=="html") {
  cat('    
<div  class = "DCexercise">

#### 3. Confidence Intervals {-}

<tt>mod</tt>, the object of class <tt>lm</tt> which contains the estimated regression model $$\\widehat{TestScore} = \\underset{(23.96)}{567.43} - \\underset{(0.85)}{7.15} \\times STR, \\, R^2 = 0.8976, \\,SER=15.19$$ is available in your working environment.

**Instructions:**

Compute $90\\%$ confidence intervals for both coefficients. 

<iframe src="DCL/ex5_3.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:**

Use the function <tt>confint()</tt>, see <tt>?confint</tt>. The argument <tt>level</tt> sets the confidence level to be used.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 4. A Confidence Interval for the Mean I {-}

Consider the regression model $$Y_i = \\beta_1 + u_i$$ where $Y_i \\sim \\mathcal{N}(\\mu, \\sigma^2)$. Following the discussion preceding equation \\@ref(eq:KI), a $95\\%$ confidence interval for the mean of the $Y_i$ can be computed as

$$CI^{\\mu}_{0.95} = \\left[\\hat\\mu - 1.96 \\times \\frac{\\sigma}{\\sqrt{n}}; \\, \\hat\\mu + 1.96 \\times \\frac{\\sigma}{\\sqrt{n}} \\right].$$

**Instructions:**

+ Sample $n=100$ observations from a normal distribution with variance $100$ and mean $10$.
+ Use the sample to estimate $\\beta_1$. Save the estimate in <tt>mu_hat</tt>.
+ Assume that $\\sigma^2 = 100$ is known. Replace the <tt>NA</tt>s in the code below to obtain a $95\\%$ confidence interval for the mean of the $Y_i$.

<iframe src="DCL/ex5_4.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:**

Use the function <tt>confint()</tt>, see <tt>?confint</tt>. The argument <tt>level</tt> sets the confidence level.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 5. A Confidence Interval for the Mean II {-}

For historical reasons, some <tt>R</tt> functions which we use to obtain inference on model parameters, among them <tt>confint()</tt> and <tt>summary()</tt>, rely on the $t$-distribution instead of using the large-sample normal approximation. This is why for small sample sizes (and hence small degrees of freedom), $p$-values and confidence intervals reported by these functions deviate from those computed using critical values or cumulative probabilities of the standard normal distribution.

The $95\\%$ confidence interval for the mean in the previous exercise is $[9.13, 13.05]$.

**Instructions:**

100 observations sampled from a normal distribution with $\\mu=10$ and $\\sigma^2=100$ have been assigned to the vector <tt>s</tt> which is available in your environment.

Set up a suitable regression model to estimate the mean of the observations in <tt>s</tt>. Then use <tt>confint()</tt> to compute a $95\\%$ confidence interval for the mean.

(Check that the result is different from the interval reported above.)

<iframe src="DCL/ex5_5.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 6. Regression on a Dummy Variable I {-}

Chapter \\@ref(rwxiabv) discusses regression when $X$ is a dummy variable. We have used a <tt>for()</tt> loop to generate a binary variable indicating whether a schooling district in the <tt>CASchools</tt> data set has a student-teacher ratio below $20$. Though it is instructive to use a loop for this, there are alternate ways to achieve the same with fewer lines of code.

A <tt>data.frame</tt> <tt>DF</tt> with $100$ observations of a variable <tt>X</tt> is available in your working environment.

**Instructions:**

+ Use <tt>ifelse()</tt> to generate a binary vector <tt>dummy</tt> indicating whether the observations in <tt>X</tt> are *positive*.

+ Append <tt>dummy</tt> to the <tt>data.frame</tt> <tt>DF</tt>.

<iframe src="DCL/ex5_6.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 7. Regression on a Dummy Variable II {-}

A <tt>data.frame</tt> <tt>DF</tt> with 100 observations on <tt>Y</tt> and the binary variable <tt>D</tt> from the previous exercise is available in your working environment.

**Instructions:**

+ Compute the group-specific sample means of the observations in <tt>Y</tt>: save the mean of observations in <tt>Y</tt> where <tt>dummy == 1</tt> to <tt>mu_Y_D1</tt> and assign the mean of those observations with <tt>D == 0</tt> to <tt>mu_Y_D0</tt>. 

+ Use <tt>lm()</tt> to regress <tt>Y</tt> on <tt>D</tt>, i.e., estimate the coefficients in the model $$Y_i = \\beta_0 + \\beta_1 \\times D_i + u_i.$$

Also check that the estimates of the coefficients $\\beta_0$ and $\\beta_1$ reflect specific sample means. Can you tell which (no code submission needed)?

<iframe src="DCL/ex5_7.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 8. Regression on a Dummy Variable III {-}

In this exercise, you have to visualize some of the results from the dummy regression model $$\\widehat{Y}_i = -0.66 + 1.43 \\times D_i$$ estimated in the previous exercise.

A <tt>data.frame</tt> <tt>DF</tt> with 100 observations on <tt>X</tt> and the binary variable <tt>dummy</tt> as well as the model object <tt>dummy_mod</tt> from the previous exercise are available in your working environment.

**Instructions:**

+ Start by drawing a visually appealing plot of the observations on $Y$ and $D$ based on the code chunk provided in <tt>Script.R</tt>. Replace the <tt>???</tt> by the correct expressions!

+ Add the regression line to the plot.

<iframe src="DCL/ex5_8.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 9. Gender Wage Gap I {-}

The cross-section data set <tt>CPS1985</tt> is a subsample from the May 1985 *Current Population Survey* conducted by the *US Census Bureau* which contains observations on, among others things, wage and the gender of employees.

<tt>CPS1985</tt> is part of the package <tt>AER</tt>.

**Instructions:**

+ Attach the package <tt>AER</tt> and load the data set <tt>CPS1985</tt>.

+ Estimate the dummy regression model $$wage_i = \\beta_0 + \\beta_1 \\cdot female_i + u_i$$ where 
  
    \\begin{align*}
      female_i = 
      \\begin{cases}
        1, & \\text{if employee} \\, i \\, \\text{is female,} \\\\
        0, & \\text{if employee} \\, i \\,  \\text{is male.}
      \\end{cases}
    \\end{align*}
  
    Save the result in <tt>wage_mod</tt>.

<iframe src="DCL/ex5_9.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 10. Gender Wage Gap II {-}

The wage regression from the previous exercise yields $$\\widehat{wage}_i = 9.995 - 2.116 \\cdot female_i.$$

The model object <tt>dummy_mod</tt> is available in your working environment.

**Instructions:**

+ Test the hypothesis that the coefficient on $female_i$ is zero against the alternative that it is non-zero. The null hypothesis implies that there is no gender wage gap. Use the heteroskedasticity-robust estimator proposed by @white1980.

<iframe src="DCL/ex5_10.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

+ <tt>vcovHC()</tt> computes heteroskedasticity-robust estimates of the covariance matrix of the coefficient estimators for the model supplied. The estimator proposed by @white1980 is computed if you set <tt>type = "HC0"</tt>.

+ The function <tt>coeftest()</tt> performs significance tests for the coefficients in model objects. A covariance matrix can be supplied using the argument <tt>vcov.</tt>.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 11. Computation of Heteroskedasticity-Robust Standard Errors {-}

In the simple regression model, the covariance matrix of the coefficient estimators is denoted

\\begin{equation}
\\text{Var}
  \\begin{pmatrix}
    \\hat\\beta_0 \\
    \\hat\\beta_1
  \\end{pmatrix} = 
\\begin{pmatrix}
  \\text{Var}(\\hat\\beta_0) & \\text{Cov}(\\hat\\beta_0,\\hat\\beta_1) \\\\
\\text{Cov}(\\hat\\beta_0,\\hat\\beta_1) & \\text{Var}(\\hat\\beta_1)
\\end{pmatrix}
\\end{equation}

The function <tt>vcovHC</tt> can be used to obtain estimates of this matrix for a model object of interest.

<tt>dummy_mod</tt>, a model object containing the wage regression dealt with in Exercises 9 and 10 is available in your working environment.

**Instructions:**

+ Compute robust standard errors of the type <tt>HC1</tt> for the coefficients estimators in the model object <tt>dummy_mod</tt>. Store the standard errors in a vector named <tt>rob_SEs</tt>.

<iframe src="DCL/ex5_11.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints**

+ The standard errors we seek can be obtained by taking the square root of the diagonal elements of the estimated covariance matrix. 
+ <tt>diag(A)</tt> returns the diagonal elements of the matrix <tt>A</tt>.


</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 12. Robust Confidence Intervals {-}

The function <tt>confint()</tt> computes confidence intervals for regression models using homoskedasticity-only standard errors so this function is not an option when there is heteroskedasticity.

The function <tt>Rob_CI()</tt> in <tt>script.R</tt> is meant to compute and report heteroskedasticity-robust confidence intervals for both model coefficients in a simple regression model.

<tt>gender_mod</tt>, a model object containing the wage regression dealt with in the previous exercises is available in your working environment.

**Instructions:**

+ Complete the code of <tt>Rob_CI()</tt> given in <tt>Script.R</tt> such that lower and upper bounds of $95\\%$ robust confidence intervals are returned. Use standard errors of the type <tt>HC1</tt>.

+ Use the function <tt>Rob_CI()</tt> to obtain $95\\%$ confidence intervals for the model coefficients in <tt>dummy_mod</tt>.

<iframe src="DCL/ex5_12.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 13. A Small Simulation Study --- I {-}

Consider the data generating process (DGP)
\\begin{align}
  X_i \\sim& \\, \\mathcal{U}[2,10], \\notag \\\\
  e_i \\sim& \\, \\mathcal{N}(0, X_i), \\notag \\\\
  Y_i =& \\, \\beta_1 X_i + e_i, (\\#eq:asss)  
\\end{align}
where $\\mathcal{U}[2,10]$ denotes the uniform distribution on the interval $[2,10]$ and $\\beta_1=2$.

Notice that the errors $e_i$ are heteroskedastic since the variance of the $e_i$ is a function of $X_i$.

**Instructions:**

+ Write a function <tt>DGP_OLS</tt> that generates a sample $(X_i,Y_i)$, $i=1,…,100$ using the DGP above and returns the OLS estimate of $\\beta_1$ based on this sample.

<iframe src="DCL/ex5_13.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:**

<tt>runif()</tt> can be used to obtain random samples from a uniform distribution, see <tt>?runif</tt>.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 14. A Small Simulation Study --- II {-}

The function <tt>DGP_OLS()</tt> from the previous exercise is available in your working environment.

**Instructions:**

+ Use <tt>replicate()</tt> to generate a sample of $1000$ OLS estimates $\\widehat{\\beta}_1$ using the function <tt>DGP_OLS</tt>. Store the estimates in a vector named <tt>estimates</tt>.

+ Next, estimate the variance of $\\widehat{\\beta}_1$ in \\@ref(eq:asss): compute the sample variance of the $1000$ OLS estimates in <tt>estimates</tt>. Store the result in <tt>est_var_OLS</tt>.

<iframe src="DCL/ex5_14.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 15. A Small Simulation Study --- III {-}

According to the the Gauss-Markov theorem, the OLS estimator in linear regression models is no longer the most efficient estimator among the conditionally unbiased linear estimators when there is heteroskedasticity. In other words, the OLS estimator loses the BLUE property when the assumption of homoskedasticity is violated.

It turns out that OLS applied to the weighted observations $(w_i X_i, w_i Y_i)$ where $w_i=\\frac{1}{\\sigma_i}$ is the BLUE estimator under heteroskedasticity. This estimator is called the *weighted least squares* (WLS) estimator. Thus, when there is heteroskedasticity, the WLS estimator has lower variance than OLS.

The function <tt>DGP_OLS()</tt> and the estimated variance <tt>est_var_OLS</tt> from the previous exercises are available in your working environment.

**Instructions:**

+ Write a function <tt>DGP_WLS()</tt> that generates $100$ samples using the DGP introduced in Exercise 13 and returns the WLS estimate of $\\beta_1$. Treat $\\sigma_i$ as known, i.e., set $w_i=\\frac{1}{\\sqrt{X_i}}$.

+ Repeat exercise 14 using <tt>DGP_WLS()</tt>. Store the variance estimate in <tt>est_var_GLS</tt>.

+ Compare the estimated variances <tt>est_var_OLS</tt> and <tt>est_var_GLS</tt> using logical operators (<tt><</tt> or <tt>></tt>).

<iframe src="DCL/ex5_15.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

+ <tt>DGP_WLS()</tt> can be obtained using a modified code of <tt>DGP_OLS()</tt>.

+ Remember that functions are objects and you may print the code of a function to the console.

</div>')}
