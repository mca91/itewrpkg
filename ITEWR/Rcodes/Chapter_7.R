## library(AER)
## library(stargazer)
cat('
<div class = "keyconcept" id="KC7.1">
<h3 class = "right"> Key Concept 7.1 </h3>          
<h3 class = "left"> Testing the Hypothesis $\\beta_j = \\beta_{j,0}$ <br>
                    Against the Alternative $\\beta_j \\neq \\beta_{j,0}$ </h3>
<p>
1. Compute the standard error of $\\hat{\\beta_j}$
2. Compute the $t$-statistic,
$$t^{act} = \\frac{\\hat{\\beta}_j - \\beta_{j,0}} {SE(\\hat{\\beta_j})}$$
3. Compute the $p$-value,
$$p\\text{-value} = 2 \\Phi(-|t^{act}|)$$

where $t^{act}$ is the value of the $t$-statistic actually computed. Reject the hypothesis at the $5\\%$ significance level if the $p$-value is less than $0.05$ or, equivalently, if $|t^{act}| > 1.96$. The standard error and (typically) the $t$-statistic and the corresponding $p$-value for testing $\\beta_j = 0$ are computed automatically by suitable <tt>R</tt> functions, e.g., by <tt>summary</tt>.
</p>
</div>
')
cat('\\begin{keyconcepts}[Testing the Hypothesis $\\beta_j = \\beta_{j,0}$
                    Against the Alternative $\\beta_j \\neq \\beta_{j,0}$]{7.1}
\\begin{enumerate}
\\item Compute the standard error of $\\hat{\\beta_j}$
\\item Compute the $t$-statistic,
$$t^{act} = \\frac{\\hat{\\beta}_j - \\beta_{j,0}} {SE(\\hat{\\beta_j})}$$
\\item Compute the $p$-value,
$$p\\text{-value} = 2 \\Phi(-|t^{act}|)$$
where $t^{act}$ is the value of the $t$-statistic actually computed. Reject the hypothesis at the $5\\%$ significance level if the $p$-value is less than $0.05$ or, equivalently, if $|t^{act}| > 1.96$. \\end{enumerate}\\vspace{0.5cm}

The standard error and (typically) the $t$-statistic and the corresponding $p$-value for testing $\\beta_j = 0$ are computed automatically by suitable \\texttt{R} functions, e.g., by \\texttt{summary()}.
\\end{keyconcepts}
')
library(AER)
data(CASchools)
CASchools$size <- CASchools$students/CASchools$teachers
CASchools$score <- (CASchools$read + CASchools$math)/2

model <- lm(score ~ size + english, data = CASchools)
coeftest(model, vcov. = vcovHC, type = "HC1")
# compute two-sided p-value
2 * (1 - pt(abs(coeftest(model, vcov. = vcovHC, type = "HC1")[2, 3]),
            df = model$df.residual))
cat('
<div class = "keyconcept" id="KC7.2">
<h3 class = "right"> Key Concept 7.2 </h3>          
<h3 class = "left"> Confidence Intervals for a Single Coefficient in Multiple Regression </h3>
<p>
A $95\\%$ two-sided confidence interval for the coefficient $\\beta_j$ is an interval that contains the true value of $\\beta_j$ with a $95 \\%$ probability; that is, it contains the true value of $\\beta_j$ in $95 \\%$ of all repeated samples. Equivalently, it is the set of values of $\\beta_j$ that cannot be rejected by a $5 \\%$ two-sided hypothesis test. When the sample size is large, the $95 \\%$ confidence interval for $\\beta_j$ is
$$\\left[\\hat{\\beta_j}- 1.96 \\times SE(\\hat{\\beta}_j), \\hat{\\beta_j} + 1.96 \\times SE(\\hat{\\beta_j})\\right].$$
</p>
</div>
')
cat('\\begin{keyconcepts}[Confidence Intervals for a Single Coefficient in Multiple Regression]{7.2}
A $95\\%$ two-sided confidence interval for the coefficient $\\beta_j$ is an interval that contains the true value of $\\beta_j$ with a $95 \\%$ probability; that is, it contains the true value of $\\beta_j$ in $95 \\%$ of repeated samples. Equivalently, it is the set of values of $\\beta_j$ that cannot be rejected by a $5 \\%$ two-sided hypothesis test. When the sample size is large, the $95 \\%$ confidence interval for $\\beta_j$ is
$$\\left[\\hat{\\beta_j}- 1.96 \\times SE(\\hat{\\beta}_j), \\hat{\\beta_j} + 1.96 \\times SE(\\hat{\\beta_j})\\right].$$
\\end{keyconcepts}
')
model <- lm(score ~ size + english, data = CASchools)
confint(model)
confint(model, level = 0.9)
# compute robust standard errors
rob_se <- diag(vcovHC(model, type = "HC1"))^0.5

# compute robust 95% confidence intervals
rbind("lower" = coef(model) - qnorm(0.975) * rob_se,
      "upper" = coef(model) + qnorm(0.975) * rob_se)

# compute robust 90% confidence intervals

rbind("lower" = coef(model) - qnorm(0.95) * rob_se,
      "upper" = coef(model) + qnorm(0.95) * rob_se)
library(AER)
data(CASchools)
CASchools$size <- CASchools$students/CASchools$teachers
CASchools$score <- (CASchools$read + CASchools$math)/2

# scale expenditure to thousands of dollars
CASchools$expenditure <- CASchools$expenditure/1000

# estimate the model
model <- lm(score ~ size + english + expenditure, data = CASchools)
coeftest(model, vcov. = vcovHC, type = "HC1")
# compute the sample correlation between 'size' and 'expenditure'
cor(CASchools$size, CASchools$expenditure)
# estimate the multiple regression model
model <- lm(score ~ size + english + expenditure, data = CASchools)

# execute the function on the model object and provide both linear restrictions 
# to be tested as strings
linearHypothesis(model, c("size=0", "expenditure=0"))
# heteroskedasticity-robust F-test
linearHypothesis(model, c("size=0", "expenditure=0"), white.adjust = "hc1")
# execute the function on the model object and provide the restrictions 
# to be tested as a character vector
linearHypothesis(model, c("size=0", "english=0", "expenditure=0"))

# Access the overall F-statistic from the model's summary
summary(model)$fstatistic
model <- lm(score ~ size + english + expenditure, data = CASchools)
# draw the 95% confidence set for coefficients on size and expenditure
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("size", "expenditure"),
                  main = "95% Confidence Set")
# draw the robust 95% confidence set for coefficients on size and expenditure 
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("size", "expenditure"),
                  main = "95% Confidence Sets",
                  vcov. = vcovHC(model, type = "HC1"),
                  col = "red")
                  
# draw the 95% confidence set for coefficients on size and expenditure
confidenceEllipse(model, 
                  fill = T,
                  lwd = 0,
                  which.coef = c("size", "expenditure"),
                  add = T)
cat('
<div class = "keyconcept" id="KC7.3">
<h3 class = "right"> Key Concept 7.3 </h3>          
<h3 class = "left"> Omitted Variable Bias in Multiple Regression</h3>
<p>
Omitted variable bias is the bias in the OLS estimator that arises when regressors correlate with an omitted variable. For omitted variable bias to arise, two things must be true: 

1. At least one of the included regressors must be correlated with the omitted variable. 
2. The omitted variable must be a determinant of the dependent variable, $Y$.
</p>
</div>
')
cat('\\begin{keyconcepts}[Omitted Variable Bias in Multiple Regression]{7.3}
Omitted variable bias is the bias in the OLS estimator that arises when regressors correlate with an omitted variable. For omitted variable bias to arise, two things must be true:\\newline

\\begin{enumerate}
\\item At least one of the included regressors must be correlated with the omitted variable. 
\\item The omitted variable must be a determinant of the dependent variable, $Y$.
\\end{enumerate}

\\end{keyconcepts}
')
library(AER)
data(CASchools)
CASchools$size <- CASchools$students/CASchools$teachers
CASchools$score <- (CASchools$read + CASchools$math)/2

# estimate the model and print the summary to console
model <- lm(score ~ size + english + lunch, data = CASchools)
coeftest(model, vcov. = vcovHC, type = "HC1")
cat('
<div class = "keyconcept" id="KC7.4">
<h3 class = "right"> Key Concept 7.4 </h3>          
<h3 class = "left"> $R^2$ and $\\bar{R}^2$: what they tell you --- and what they do not </h3>

<p>

The $R^2$ and $\\bar{R}^2$ tell you whether the regressors are good at explaining the variation of the independent variable in the sample. If the $R^2$ (or $\\bar{R}^2$) is nearly $1$, then the regressors produce a good prediction of the dependent variable in that sample, in the sense that the variance of OLS residuals is small compared to the variance of the dependent variable. If the $R^2$ (or $\\bar{R}^2$) is nearly $0$, the opposite is true.

The $R^2$ and $\\bar{R}^2$ do *not* tell you whether:

1. An included variable is statistically significant. 
2. The regressors are the true cause of the movements in the dependent variable.
3. There is omitted variable bias.
4. You have chosen the most appropriate set of regressors.

</p>
  
</div>
')
cat('\\begin{keyconcepts}[$R^2$ and $\\bar{R}^2$: What They Tell You --- and What They Do not]{7.4}
The $R^2$ and $\\bar{R}^2$ tell you whether the regressors are good at explaining the variation of the independent variable in the sample. If the $R^2$ (or $\\bar{R}^2$) is nearly $1$, then the regressors produce a good prediction of the dependent variable in that sample, in the sense that the variance of OLS residuals is small compared to the variance of the dependent variable. If the $R^2$ (or $\\bar{R}^2$) is nearly $0$, the opposite is true.\\newline

The $R^2$ and $\\bar{R}^2$ do \\textit{not} tell you whether:\\newline

\\begin{enumerate}
\\item An included variable is statistically significant. 
\\item The regressors are the true cause of the movements in the dependent variable.
\\item There is omitted variable bias.
\\item You have chosen the most appropriate set of regressors.
\\end{enumerate}
\\end{keyconcepts}
')
# set seed for reproducibility
set.seed(1)

# generate observations for parking lot space
CASchools$PLS <- c(22 * CASchools$income 
                   - 15 * CASchools$size 
                   + 0.2 * CASchools$expenditure
                   + rnorm(nrow(CASchools), sd = 80) + 3000)
# plot parking lot space against test score
plot(CASchools$PLS, 
     CASchools$score,
     xlab = "Parking Lot Space",
     ylab = "Test Score",
     pch = 20,
     col = "steelblue")

# regress test score on PLS
summary(lm(score ~ PLS, data = CASchools))
# estimate the correlation between 'calworks' and 'lunch'
cor(CASchools$calworks, CASchools$lunch)
# set up arrangement of plots
m <- rbind(c(1, 2), c(3, 0))
graphics::layout(mat = m)

# scatterplots
plot(score ~ english, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20, 
     xlim = c(0, 100),
     cex.main = 0.9,
     main = "Percentage of English language learners")

plot(score ~ lunch, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20,
     cex.main = 0.9,
     main = "Percentage qualifying for reduced price lunch")

plot(score ~ calworks, 
     data = CASchools, 
     col = "steelblue", 
     pch = 20, 
     xlim = c(0, 100),
     cex.main = 0.9,
     main = "Percentage qualifying for income assistance")
# estimate correlation between student characteristics and test scores
cor(CASchools$score, CASchools$english)
cor(CASchools$score, CASchools$lunch)
cor(CASchools$score, CASchools$calworks)
## # load the stargazer library
## library(stargazer)
## 
## # estimate different model specifications
## spec1 <- lm(score ~ size, data = CASchools)
## spec2 <- lm(score ~ size + english, data = CASchools)
## spec3 <- lm(score ~ size + english + lunch, data = CASchools)
## spec4 <- lm(score ~ size + english + calworks, data = CASchools)
## spec5 <- lm(score ~ size + english + lunch + calworks, data = CASchools)
## 
## # gather robust standard errors in a list
## rob_se <- list(sqrt(diag(vcovHC(spec1, type = "HC1"))),
##                sqrt(diag(vcovHC(spec2, type = "HC1"))),
##                sqrt(diag(vcovHC(spec3, type = "HC1"))),
##                sqrt(diag(vcovHC(spec4, type = "HC1"))),
##                sqrt(diag(vcovHC(spec5, type = "HC1"))))
## 
## # generate a LaTeX table using stargazer
## stargazer(spec1, spec2, spec3, spec4, spec5,
##           se = rob_se,
##           digits = 3,
##           header = F,
##           column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)"))
library(stargazer)
spec1 <- lm(score ~ size, data = CASchools)
spec2 <- lm(score ~ size + english, data = CASchools)
spec3 <- lm(score ~ size + english + lunch, data = CASchools)
spec4 <- lm(score ~ size + english + calworks, data = CASchools)
spec5 <- lm(score ~ size + english + lunch + calworks, data = CASchools)

# gather robust standard errors in a list
rob_se <- list(
  sqrt(diag(vcovHC(spec1, type = "HC1"))),
  sqrt(diag(vcovHC(spec2, type = "HC1"))),
  sqrt(diag(vcovHC(spec3, type = "HC1"))),
  sqrt(diag(vcovHC(spec4, type = "HC1"))),
  sqrt(diag(vcovHC(spec5, type = "HC1")))
)

stargazer(spec1, spec2, spec3, spec4, spec5, 
          type = "html",
          se = rob_se,
          header = F,
          digits = 3,
          float.env = "sidewaystable",
          object.names = TRUE,
          dep.var.caption = "Dependent Variable: Test Score",
          model.numbers = FALSE,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)")
          )

stargazer_html_title("Regressions of Test Scores on the Student-Teacher Ratio and Control Variables", "rotsostracv")
library(stargazer)
spec1 <- lm(score ~ size, data = CASchools)
spec2 <- lm(score ~ size + english, data = CASchools)
spec3 <- lm(score ~ size + english + lunch, data = CASchools)
spec4 <- lm(score ~ size + english + calworks, data = CASchools)
spec5 <- lm(score ~ size + english + lunch + calworks, data = CASchools)

# gather robust standard errors in a list
rob_se <- list(
  sqrt(diag(vcovHC(spec1, type = "HC1"))),
  sqrt(diag(vcovHC(spec2, type = "HC1"))),
  sqrt(diag(vcovHC(spec3, type = "HC1"))),
  sqrt(diag(vcovHC(spec4, type = "HC1"))),
  sqrt(diag(vcovHC(spec5, type = "HC1")))
)

stargazer(spec1, spec2, spec3, spec4, spec5, 
          type = "latex",
          title = "\\label{tab:rotsostracv} Regressions of Test Scores on the Student-Teacher Ratio and Control Variables",
          float.env = "sidewaystable",
          column.sep.width = "-10pt",
          se = rob_se,
          header = F,
          digits = 3,
          dep.var.caption = "Dependent Variable: Test Score",
          object.names = TRUE,
          model.numbers = FALSE,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)")
          )
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 1. Hypothesis Testing in a Multiple Regression Model --- $t$-statistics and $p$-values {-}

Reconsider the <tt>Boston</tt> data set and the following estimated model (homoscedasticity-only standard errors in parentheses) from the previous chapter:

$$\\widehat{medv}_i = \\underset{(0.75)}{32.828} -\\underset{(0.05)}{0.994} \\times lstat_i -\\underset{(0.04)}{0.083} \\times crim_i + \\underset{(0.01)}{0.038} \\times age_i.$$

Just as in the simple linear regression framework we can conduct hypothesis tests about the coefficients in multiple regression models. The most common hypothesis is $H_0:\\beta_j=0$ against the alternative $H_1:\\beta_j\\ne 0$ for some $j$ in $0,1,\\dots,k$.

The packages <tt>AER</tt> and <tt>MASS</tt> have been loaded. The coefficient estimates as well as the corresponding standard errors are available in <tt>coefs</tt> and <tt>SEs</tt>, respectively.

**Instructions:**

Use vector arithmetics to solve the following tasks:

+ Compute $t$-statistics for each coefficient by using the predefined objects <tt>coefs</tt> and <tt>SEs</tt>. Assign them to <tt>tstats</tt>.

+ Compute $p$-values for each coefficient and assign them to <tt>pval</tt>.

+ Check with the help of logical operators whether the hypotheses are rejected at the $1\\%$ significance level.

<iframe src="DCL/ex7_1.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

+ The $t$-statistic for each coefficient is defined as $t=\\frac{\\widehat{\\beta}_j-\\beta_{j,0}}{SE(\\widehat{\\beta}_j)}$.

+ The $p$-value for a two-sided test using is computed as $2\\cdot\\Phi(-|t^{act}|)$ where $t^{act}$ denotes the computed $t$-statistic.

</div>') } else {
  cat("\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}")
}
cat('
<div  class = "DCexercise">

#### 2. Hypothesis Testing in a Multiple Regression Model - Confidence Intervals {-}

Consider again the estimated model 

$$\\widehat{medv}_i = \\underset{(0.75)}{32.828} -\\underset{(0.05)}{0.994} \\times lstat_i -\\underset{(0.04)}{0.083} \\times crim_i + \\underset{(0.01)}{0.038} \\times age_i.$$

which is available as the object <tt>mod</tt> in your working environment. The packages <tt>AER</tt> and <tt>MASS</tt> have been loaded.

**Instructions:**

+ Construct $99\\%$ confidence intervals for all model coefficients. Use the intervals to decide whether the individual null hypotheses $H_0:\\beta_j=0$, $j=0,1,2,3,4$ are rejected at the $1\\%$ level.

<iframe src="DCL/ex7_2.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:**

+ You may use <tt>confint()</tt> to construct confidence intervals. The confidence level can be set via the argument <tt>level</tt>.

</div>')
cat('
<div  class = "DCexercise">

#### 3. Robust Hypothesis Testing in Multiple Regression Models {-}

The <tt>lm</tt> object <tt>mod</tt> from the previous exercises is available in your working environment. The packages <tt>AER</tt> and <tt>MASS</tt> have been loaded.

**Instructions:**

+ Print a coefficient summary that reports heteroscedasticity-robust standard errors.

+ Access entries of the matrix generated by <tt>coeftest()</tt> to check whether the hypotheses are rejected at a 1% significance level. Use logical operators <tt><,></tt>.

<iframe src="DCL/ex7_3.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

+ Using the argument <tt>vcov.</tt> in <tt>coeftest()</tt> forces the function to use robust standard errors.

+ The $p$-values are contained in the fourth column of the output generated by <tt>coeftest()</tt>. Use square brackets to subset the matrix accordingly.

</div>')
cat('
<div  class = "DCexercise">

#### 4. Joint Hypothesis Testing --- $F$-Test I {-}

Sometimes we are interested in testing joint hypotheses which impose restrictions on *multiple* regression coefficients. For example, in the model 

$$medv_i = \\beta_0 + \\beta_1\\times lstat_i + \\beta_2\\times crim_i + \\beta_3\\times age_i + u_i$$

we may test the null $H_0: \\beta_2=\\beta_3$ vs. the alternative $H_1: \\beta_2\\ne\\beta_3$ (which is a joint hypothesis as we impose a restriction on *two* regression coefficients).

The basic idea behind testing such a hypothesis is to conduct two regressions and to compare the outcomes: for one of the regressions we impose the restrictions of formalized by the null (we call this the restricted regression model), whereas for the other regression the restriction is left out (we call this the unrestricted model). From this starting point we construct a test-statistic which, under the null, follows a well known distribution, an $F$ distribution (see the next exercise).

However, in this exercise we start with the initial computations necessary to construct the test statistic.

The packages <tt>AER</tt> and <tt>MASS</tt> have been loaded.

**Instructions:**

+ Estimate the restricted model, that is, the model where the restriction formalized by $H_0: \\beta_2=\\beta_3$ is assumed to be true. Save the model in <tt>model_res</tt>.

+ Compute the $SSR$ of the restricted model and assign it to <tt>RSSR</tt>.

+ Estimate the unrestricted model, that is, the model where the restriction is assumed to be false. Save it in <tt>model_unres</tt>.

+ Compute the $SSR$ of the unrestricted model and assign it to <tt>USSR</tt>.

<iframe src="DCL/ex7_4.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

+ The restricted model can be written as $$medv_i = \\beta_0 + \\beta_1\\times lstat_i + \\beta_2\\times crim_i + \\beta_2\\times age_i + u_i$$ which, after rearranging, can be expressed as $$medv_i = \\beta_0 + \\beta_1\\times lstat_i + \\beta_2\\times(crim_i+age_i) + u_i.$$

+ The $SSR$ is defined as the sum of the squared residuals.

+ Note that the residuals of a regression model are available as <tt>residuals</tt> in the corresponding <tt>lm</tt> object. So you can access them as usual via the <tt>$</tt>-operator.

</div>

<div  class = "DCexercise">')
cat('
#### 5. Joint Hypothesis Testing --- F-Test II {-}

After estimating the models and computing the $SSR$s you now have to compute the test-statistic and conduct the $F$-test. As mentioned in the last exercise, the test-statistic follows an $F$ distribution. More precisely, we deal with the $F_{q,n-k-1}$ distribution where $q$ denotes the number of restrictions under the null and $k$ is the of regressors in the unrestricted model, excluding the intercept.

The packages <tt>AER</tt> and <tt>MASS</tt> have been loaded. Both models (<tt>model_res</tt> and <tt>model_unres</tt>) as well as their SSR (<tt>RSSR</tt> and <tt>USSR</tt>) are available in your working environment.

**Instructions:**

+ Compute the $F$-statistic and assign it to <tt>Fstat</tt>.

+ Compute the $p$-value and assign it to <tt>pval</tt>.

+ Check whether the null is rejected at the $1\\%$ level using logical operators.

+ Verify your result by using <tt>linearHypothesis()</tt> and printing the results. 

<iframe src="DCL/ex7_5.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

+ The $F$-statistic is defined as $\\frac{RSSR-USSR/q}{USSR/(n-k-1)}$.

+ The $p$-value can be computed as $1-F_{q,n-k-1}(F^{act})$ where $F_{q,n-k-1}$ denotes the CDF of the $F$-distribution (<tt>pf()</tt>) with degrees of freedom $q$ and $n-k-1$ and $F^{act}$ the computed $F$-statistic.

+ <tt>linearHypothesis()</tt> expects the unrestricted model as well as the null hypothesis as arguments.

</div>')
cat('
<div  class = "DCexercise">

#### 6. Joint Hypothesis Testing - Confidence Set {-}

As you know from previous chapters constructing a confidence set for a single regression coefficient results in a simple confidence interval on the real line. However if we consider $n$ regression coefficients jointly (as we do in a joint hypothesis testing setting) we move from $\\mathbb{R}$ to $\\mathbb{R}^n$ resulting in a n-dimensional confidence set. For the sake of illustration we then often choose $n=2$, so that we end up with a representable two-dimensional plane.

Recall the estimated model 

$$\\widehat{medv}_i = \\underset{(0.75)}{32.828} -\\underset{(0.05)}{0.994} \\times lstat_i -\\underset{(0.04)}{0.083} \\times crim_i + \\underset{(0.01)}{0.038} \\times age_i.$$

which is available as <tt>mod</tt> in your working environment. Assume you want to test the null $H_0: \\beta_2=\\beta_3=0$ vs. $H_1: \\beta_2\\ne 0$ or $\\beta_3\\ne 0$.

The packages <tt>AER</tt> and <tt>MASS</tt> have been loaded.

**Instructions:**

+ Construct a $99\\%$ confidence set for the coefficients of <tt>crim</tt> and <tt>lstat</tt>, that is a two-dimensional confidence set. Can you reject the null stated above?

+ Verify your visual inspection by conducting a corresponding $F$-test.

<iframe src="DCL/ex7_6.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

+ Use <tt>confidenceEllipse()</tt> to construct a two-dimensional confidence set. Besides the coefficients for which the confidence set shall be constructed (<tt>which.coef</tt>), you have to specify the confidence level (<tt>levels</tt>).

+ As usual you can use <tt>linearHypothesis()</tt> to conduct the $F$-test. Note that there are two restrictions now, hence you have to pass a vector containing both restrictions.

</div>')
