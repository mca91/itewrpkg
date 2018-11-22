## library(AER)
## library(MASS)
cat('
<div class = "keyconcept" id="KC6.1">
<h3 class = "right"> Key Concept 6.1 </h3>          
<h3 class = "left"> Omitted Variable Bias in Regression with a Single Regressor </h3>

Omitted variable bias is the bias in the OLS estimator that arises when the regressor, $X$, is *correlated* with an omitted variable. For omitted variable bias to occur, two conditions must be fulfilled:

1. $X$ is correlated with the omitted variable. 
2. The omitted variable is a determinant of the dependent variable $Y$.

Together, 1. and 2. result in a violation of the first OLS assumption $E(u_i\\vert X_i) = 0$. Formally, the resulting bias can be expressed as

$$ \\hat\\beta_1 \\xrightarrow[]{p} \\beta_1 + \\rho_{Xu} \\frac{\\sigma_u}{\\sigma_X}. \\tag{6.1} $$
See Appendix 6.1 of the book for a detailed derivation. (<a href="#mjx-eqn-6.1">6.1</a>) states that OVB is a problem that cannot be solved by increasing the number of observations used to estimate $\\beta_1$, as $\\hat\\beta_1$ is inconsistent: OVB prevents the estimator from converging in probability to the true parameter value. Strength and direction of the bias are determined by $\\rho_{Xu}$, the correlation between the error term and the regressor.

</div>
')
cat('\\begin{keyconcepts}[Omitted Variable Bias in Regression with a Single Regressor]{6.1}
Omitted variable bias is the bias in the OLS estimator that arises when the regressor, $X$, is \\textit{correlated} with an omitted variable. For omitted variable bias to occur, two conditions must be fulfilled:\\newline

\\begin{enumerate}
\\item $X$ is correlated with the omitted variable. 
\\item The omitted variable is a determinant of the dependent variable $Y$.
\\end{enumerate}\\vspace{0.5cm}

Together, 1. and 2. result in a violation of the first OLS assumption $E(u_i\\vert X_i) = 0$. Formally, the resulting bias can be expressed as

\\begin{align}
\\hat\\beta_1 \\xrightarrow[]{p} \\beta_1 + \\rho_{Xu} \\frac{\\sigma_u}{\\sigma_X}.
\\end{align}

See Appendix 6.1 of the book for a detailed derivation. (6.1) states that OVB is a problem that cannot be solved by increasing the number of observations used to estimate $\\beta_1$, as $\\hat\\beta_1$ is inconsistent: OVB prevents the estimator from converging in probability to the true parameter value. Strength and direction of the bias are determined by $\\rho_{Xu}$, the correlation between the error term and the regressor.
\\end{keyconcepts}
')
# load the AER package
library(AER)

# load the data set
data(CASchools)   

# define variables
CASchools$STR <- CASchools$students/CASchools$teachers       
CASchools$score <- (CASchools$read + CASchools$math)/2

# compute correlations
cor(CASchools$STR, CASchools$score)
cor(CASchools$STR, CASchools$english)
# estimate both regression models
mod <- lm(score ~ STR, data = CASchools) 
mult.mod <- lm(score ~ STR + english, data = CASchools)

# print the results to the console
mod
mult.mod
cat('
<div class = "keyconcept" id="KC6.2">
<h3 class = "right"> Key Concept 6.2 </h3>          
<h3 class = "left"> The Multiple Regression Model </h3>

The multiple regression model is

$$ Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_2 X_{2i} + \\beta_3 X_{3i} + \\dots + \\beta_k X_{ki} + u_i \\ \\ , \\ \\ i=1,\\dots,n.  $$ 

The designations are similar to those in the simple regression model:

- $Y_i$ is the $i^{th}$ observation in the dependent variable. Observations on the $k$ regressors are denoted by $X_{1i},X_{2i},\\dots,X_{ki}$ and $u_i$ is the error term.
- The average relationship between $Y$ and the regressors is given by the population regression line
$$ E(Y_i\\vert X_{1i}=x_1, X_{2i}=x_2,  X_{3i}=x_3,\\dots, X_{ki}=x_k) = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + \\beta_3 x_3 + \\dots + \\beta_k x_k. $$
- $\\beta_0$ is the intercept; it is the expected value of $Y$ when all $X$s equal $0$. $\\beta_j \\ , \\ j=1,\\dots,k$ are the coefficients on $X_j \\ , \\ j=1,\\dots,k$. $\\beta_1$ measures the expected change in $Y_i$ that results from a one unit change in $X_{1i}$ while holding all other regressors constant. 
</div>
')
cat('\\begin{keyconcepts}[The Multiple Regression Model]{6.2}
\\begin{itemize}
\\item $Y_i$ is the $i^{th}$ observation in the dependent variable. Observations on the $k$ regressors are denoted by $X_{1i},X_{2i},\\dots,X_{ki}$ and $u_i$ is the error term.
\\item The average relationship between $Y$ and the regressors is given by the population regression line
$$ E(Y_i\\vert X_{1i}=x_1, X_{2i}=x_2,  X_{3i}=x_3,\\dots, X_{ki}=x_k) = \\beta_0 + \\beta_1 x_1 + \\beta_2 x_2 + \\beta_3 x_3 + \\dots + \\beta_k x_k. $$
\\item $\\beta_0$ is the intercept; it is the expected value of $Y$ when all $X$s equal $0$. $\\beta_j \\ , \\ j=1,\\dots,k$ are the coefficients on $X_j \\ , \\ j=1,\\dots,k$. $\\beta_1$ measures the expected change in $Y_i$ that results from a one unit change in $X_{1i}$ while holding all other regressors constant. 
\\end{itemize}
\\end{keyconcepts}
')
summary(mult.mod)$coef
if(knitr::opts_knit$get("rmarkdown.pandoc.to") == "html") {
library(plotly)

Sys.setenv("plotly_username"="mca_unidue")
Sys.setenv("plotly_api_key"="NKcv2lBiEbkOyiNdUXhk")

y <- CASchools$score
x1 <- CASchools$STR
x2 <- CASchools$english
df <- data.frame(y, x1, x2)

reg <- lm(y ~ x1 + x2)
cf.mod <- coef(reg)

x1.seq <- seq(min(x1),max(x1),length.out=25)
x2.seq <- seq(min(x2),max(x2),length.out=25)
z <- t(outer(x1.seq, x2.seq, function(x,y) cf.mod[1] + cf.mod[2]*x + cf.mod[3]*y))


rbPal <- colorRampPalette(c('red','blue'))
cols <- rbPal(10)[as.numeric(cut(abs(y-reg$fitted.values), breaks = 10))]

m <- list(
  t = 5
)

p <- plot_ly(x=~x1.seq, 
             y=~x2.seq, 
             z=~z,
             colors = "red",
             opacity = 0.9,
             name = "Reg.Plane",
             type="surface"
     ) %>%
  add_trace(data=df, name='CASchools', x=x1, y=x2, z=y, mode="markers", type="scatter3d",
            marker = list(color=cols, opacity=0.85, symbol=105, size=4)
  ) %>%
  hide_colorbar() %>%
  layout(
    margin = m,
    showlegend = FALSE,
    scene = list(
      aspectmode = "manual", aspectratio = list(x=1, y=1.3, z=1),
      xaxis = list(title = "STR"),
      yaxis = list(title = "PctEL"),
      zaxis = list(title = "TestScore"),
      camera = list(eye = list(x = -2,y = -0.1, z=0.05),
                    center = list(x = 0,
                                  y = 0,
                                  z = 0
                                  )
               )
    )
    
  )

p <- p %>% config(showLink = F, displayModeBar = F);p
}
cat('\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}')
summary(mult.mod)
# define the components
n <- nrow(CASchools)                            # number of observations (rows)
k <- 2                                          # number of regressors

y_mean <- mean(CASchools$score)                 # mean of avg. test-scores

SSR <- sum(residuals(mult.mod)^2)               # sum of squared residuals
TSS <- sum((CASchools$score - y_mean )^2)       # total sum of squares
ESS <- sum((fitted(mult.mod) - y_mean)^2)       # explained sum of squares

# compute the measures

SER <- sqrt(1/(n-k-1) * SSR)                    # standard error of the regression
Rsq <- 1 - (SSR / TSS)                          # R^2
adj_Rsq <- 1 - (n-1)/(n-k-1) * SSR/TSS          # adj. R^2

# print the measures to the console
c("SER" = SER, "R2" = Rsq, "Adj.R2" = adj_Rsq)
cat('
<div class = "keyconcept" id="KC6.4">
<h3 class = "right"> Key Concept 6.4 </h3>          
<h3 class = "left"> The Least Squares Assumptions in the Multiple Regression Model </h3>

The multiple regression model is given by

$$ Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_1 X_{2i} + \\dots + \\beta_k X_{ki} + u_i \\ , \\ i=1,\\dots,n. $$

The OLS assumptions in the multiple regression model are an extension of the ones made for the simple regression model:

1. Regressors $(X_{1i}, X_{2i}, \\dots, X_{ki}, Y_i) \\ , \\ i=1,\\dots,n$, are drawn such that the i.i.d. assumption holds. 
2. $u_i$ is an error term with conditional mean zero given the regressors, i.e.,
$$ E(u_i\\vert X_{1i}, X_{2i}, \\dots, X_{ki}) = 0. $$
3. Large outliers are unlikely, formally $X_{1i},\\dots,X_{ki}$ and $Y_i$ have finite fourth moments.
4. No perfect multicollinearity.

</div>
')
cat('\\begin{keyconcepts}[The Least Squares Assumptions in the Multiple Regression Model]{6.4}
The multiple regression model is given by

$$ Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_1 X_{2i} + \\dots + \\beta_k X_{ki} + u_i \\ , \\ i=1,\\dots,n. $$

The OLS assumptions in the multiple regression model are an extension of the ones made for the simple regression model:\\newline

\\begin{enumerate}
\\item Regressors $(X_{1i}, X_{2i}, \\dots, X_{ki}, Y_i) \\ , \\ i=1,\\dots,n$, are drawn such that the i.i.d. assumption holds. 
\\item $u_i$ is an error term with conditional mean zero given the regressors, i.e.,
$$ E(u_i\\vert X_{1i}, X_{2i}, \\dots, X_{ki}) = 0. $$
\\item Large outliers are unlikely, formally $X_{1i},\\dots,X_{ki}$ and $Y_i$ have finite fourth moments.
\\item No perfect multicollinearity.
\\end{enumerate}
\\end{keyconcepts}
')
# define the fraction of English learners        
CASchools$FracEL <- CASchools$english / 100

# estimate the model
mult.mod <- lm(score ~ STR + english + FracEL, data = CASchools) 

# obtain a summary of the model
summary(mult.mod)                                                 
# if STR smaller 12, NS = 0, else NS = 1
CASchools$NS <- ifelse(CASchools$STR < 12, 0, 1)

# estimate the model
mult.mod <- lm(score ~ computer + english + NS, data = CASchools)

# obtain a model summary
summary(mult.mod)                                                  
table(CASchools$NS)
# set seed for reproducibility
set.seed(1)

# generate artificial data on location
CASchools$direction <- sample(c("West", "North", "South", "East"), 
                              420, 
                              replace = T)

# estimate the model
mult.mod <- lm(score ~ STR + english + direction, data = CASchools)

# obtain a model summary
summary(mult.mod)                                                 
# Percentage of english speakers 
CASchools$PctES <- 100 - CASchools$english

# estimate the model
mult.mod <- lm(score ~ STR + english + PctES, data = CASchools)

# obtain a model summary
summary(mult.mod)                                                 
# load packages
library(MASS)
library(mvtnorm)

# set number of observations
n <- 50

# initialize vectors of coefficients
coefs1 <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))
coefs2 <- coefs1

# set seed
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
  
  # for cov(X_1,X_2) = 0.25
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))
  u <- rnorm(n, sd = 5)
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs1[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
  # for cov(X_1,X_2) = 0.85
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 8.5), c(8.5, 10)))
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs2[i, ] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
}

# obtain variance estimates
diag(var(coefs1))
diag(var(coefs2))
cat('
<div class = "keyconcept" id="KC6.5">
<h3 class = "right"> Key Concept 6.5 </h3>          
<h3 class = "left"> Large-sample distribution of $\\hat\\beta_0,\\hat\\beta_1,\\dots,\\hat\\beta_k$ </h3>

If the least squares assumptions in the multiple regression model (see Key Concept 6.4) hold, then, in large samples, the OLS estimators $\\hat\\beta_0,\\hat\\beta_1,\\dots,\\hat\\beta_k$ are jointly normally distributed. We also say that their joint distribution is *multivariate* normal. Further, each $\\hat\\beta_j$ is distributed as $\\mathcal{N}(\\beta_j,\\sigma_{\\beta_j}^2)$.

</div>
')
cat('\\begin{keyconcepts}[Large-sample distribution of $\\hat\\beta_0,\\hat\\beta_1,\\dots,\\hat\\beta_k$]{6.5}

If the least squares assumptions in the multiple regression model (see Key Concept 6.4) hold, then, in large samples, the OLS estimators $\\hat\\beta_0,\\hat\\beta_1,\\dots,\\hat\\beta_k$ are jointly normally distributed. We also say that their joint distribution is \\textit{multivariate} normal. Further, each $\\hat\\beta_j$ is distributed as $\\mathcal{N}(\\beta_j,\\sigma_{\\beta_j}^2)$.

\\end{keyconcepts}
')
# load packages
library(MASS)
library(mvtnorm)

# set sample size
n <- 50

# initialize vector of coefficients
coefs <- cbind("hat_beta_1" = numeric(10000), "hat_beta_2" = numeric(10000))

# set seed for reproducibility
set.seed(1)

# loop sampling and estimation
for (i in 1:10000) {
  
  X <- rmvnorm(n, c(50, 100), sigma = cbind(c(10, 2.5), c(2.5, 10)))
  u <- rnorm(n, sd = 5)
  Y <- 5 + 2.5 * X[, 1] + 3 * X[, 2] + u
  coefs[i,] <- lm(Y ~ X[, 1] + X[, 2])$coefficients[-1]
  
}

# compute density estimate
kde <- kde2d(coefs[, 1], coefs[, 2])

# plot density estimate
persp(kde, 
      theta = 310, 
      phi = 30, 
      xlab = "beta_1", 
      ylab = "beta_2", 
      zlab = "Est. Density")
# estimate the correlation between estimators
cor(coefs[, 1], coefs[, 2])
if(knitr::opts_knit$get("rmarkdown.pandoc.to") == "html") {
library(plotly)

kde <- kde2d(coefs[, 1], coefs[, 2], n = 100)

p <- plot_ly(x = kde$x, y = kde$y, z = kde$z, 
             type = "surface", showscale = FALSE)

p %>% layout(scene = list(zaxis = list(title = "Est. Density"
                                       ),
                          xaxis = list(title = "hat_beta_1"
                                       ),
                          yaxis = list(title = "hat_beta_2"
                                       )
                          )
             ) %>% 
  config(showLink = F, displayModeBar = F)
}
cat('\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}')
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 1. The Boston Housing Data Set {-}

For the course of this section, you will work with <tt>Boston</tt>, the Boston Housing data set which contains 506 observations on housing values in suburbs of Boston. <tt>Boston</tt> comes with the package <tt>MASS</tt> which is already installed for the interactive <tt>R</tt> exercises below. 

**Instructions:**

+ Load both the package and the data set.

+ Get yourself an overview over the data using function(s) known from the previous chapters.

+ Estimate a simple linear regression model that explains the median house value of districts (<tt>medv</tt>) by the percent of households with low socioeconomic status, <tt>lstat</tt>, and a constant. Save the model to <tt>bh_mod</tt>.

+ Print a coefficient summary to the console that reports robust standard errors.

<iframe src="DCL/ex6_1.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:**

You only need basic <tt>R</tt> functions here: <tt>library()</tt>, <tt>data()</tt>, <tt>lm()</tt> and <tt>coeftest()</tt>.


</div>') } else {
  cat('\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}')
}
cat('
<div  class = "DCexercise">

#### 2. A Multiple Regression Model of Housing Prices I {-}

Now, let us expand the approach from the previous exercise by adding additional regressors to the model and estimating it again.

As has been discussed in Chapter \\@ref(mofimr), adding regressors to the model improves the fit so the $SER$ decreases and the $R^2$ increases.  

The packages <tt>AER</tt> and <tt>MASS</tt> have been loaded. The model object <tt>bh_mod</tt> is available in the environment.

**Instructions:**

+ Regress the median housing value in a district, <tt>medv</tt>, on the average age of the buildings, <tt>age</tt>, the per-capita crime rate, <tt>crim</tt>, the percentage of individuals with low socioeconomic status, <tt>lstat</tt>, and a constant. Put differently, estimate the model $$medv_i = \\beta_0 + \\beta_1 lstat_i + \\beta_2 age_i + \\beta_3 crim_i + u_i.$$

+ Print a coefficient summary to the console that reports robust standard errors for the augmented model.

+ The $R^2$ of the simple regression model is stored in <tt>R2_res</tt>. Save the multiple regression models $R^2$ to <tt>R2_unres</tt> and check whether the augmented model yields a higher $R^2$. Use <tt><</tt> or <tt>></tt> for the comparison.

<iframe src="DCL/ex6_2.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')
cat('
<div  class = "DCexercise">

#### 3. A Multiple Regression Model of Housing Prices II {-}

The equation below describes estimated model from Exercise 2 (heteroskedasticity-robust standard errors in parentheses).

$$ \\widehat{medv}_i = \\underset{(0.74)}{32.828} \\underset{(0.08)}{-0.994} \\times lstat_i \\underset{(0.03)}{-0.083} \\times crim_i + \\underset{(0.02)}{0.038} \\times age_i$$

This model is saved in <tt>bh_mult_mod</tt> which is available in the working environment.

**Instructions:**

As has been stressed in Chapter \\@ref(mofimr), it is not meaningful to use $R^2$ when comparing regression models with a different number of regressors. Instead, the $\\bar{R}^2$ should be used. $\\bar{R}^2$ adjusts for the circumstance that the $SSR$ reduces when a regressor is added to the model.

+ Use the model object to compute the correction factor $CF = \\frac{n-1}{n-k-1}$ where $n$ is the number of observations and $k$ is the number of regressors, excluding the intercept. Save it to <tt>CF</tt>.

+ Use <tt>summary()</tt> to obtain $R^2$ and $\\bar{R}^2$ for <tt>bh_mult_mod</tt>. It is sufficient if you print both values to the console.

+ Check that $$\\bar{R}^2 = 1 - (1-R^2) \\cdot CF.$$ Use the <tt>==</tt> operator.

<iframe src="DCL/ex6_3.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')
cat('
<div  class = "DCexercise">

#### 4. A Fully-Fledged Model for Housing Values? {-}

Have a look at the <a href="https://stat.ethz.ch/R-manual/R-devel/library/MASS/html/Boston.html">description</a> of the variables contained in the <tt>Boston</tt> data set. 
Which variable would you expect to have the highest $p$-value in a multiple regression model which uses *all* remaining variables as regressors to explain <tt>medv</tt>?

**Instructions:**

+ Regress <tt>medv</tt> on all remaining variables that you find in the Boston data set.

+ Obtain a heteroskedasticity-robust summary of the coefficients.

+ The $\\bar{R}^2$ for the model in exercise 3 is $0.5533$. What can you say about the $\\bar{R}^2$ of the large regression model? Does this model improve on the previous one (no code submission needed)?

The packages <tt>AER</tt> and <tt>MASS</tt> as well as the data set <tt>Boston</tt> are loaded to the working environment.

<iframe src="DCL/ex6_4.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

+ For brevity, use the regression formula <tt>medv ~.</tt> in your call of <tt>lm()</tt>. This is a shortcut that specifies a regression of <tt>medv</tt> on all the remaining variables in the data set supplied to the argument <tt>data</tt>.

+ Use <tt>summary</tt> on both models for a comparison of both $\\bar{R}^2$s.

</div>')
cat('
<div  class = "DCexercise">

#### 5. Model Selection  {-}

Maybe we can improve the model by dropping a variable?

In this exercise, you have to estimate several models, each time dropping one of the explanatory variables used in the large regression model of Exercise 4 and compare the $\\bar{R}^2$.

The full regression model from the previous exercise, <tt>full_mod</tt>, is available in your environment.

**Instructions:**

+ You are completely free in solving this exercise. We recommend the following approach:

    1. Start by estimating a model <tt>mod_new</tt>, say, where, e.g., <tt>lstat</tt> is excluded from the explanatory variables.
Next, access the $\\bar{R}^2$ of this model. 

    2. Compare the $\\bar{R}^2$ of this model to the $\\bar{R}^2$ of the full model (this was about $0.7338$).

    3. Repeat Steps 1 and 2 for all explanatory variables used in the full regression model. Save the model with the highest improvement in $\\bar{R}^2$ to <tt>better_mod</tt>.

<iframe src="DCL/ex6_5.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')
