## library(AER)
## library(stargazer)
# prepare the data
library(AER)                                                     
data(CASchools)
CASchools$size <- CASchools$students/CASchools$teachers
CASchools$score <- (CASchools$read + CASchools$math) / 2       
cor(CASchools$income, CASchools$score)
# fit a simple linear model
linear_model<- lm(score ~ income, data = CASchools)

# plot the observations
plot(CASchools$income, CASchools$score,
     col = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)", 
     ylab = "Test Score",
     cex.main = 0.9,
     main = "Test Score vs. District Income and a Linear OLS Regression Function")

# add the regression line to the plot
abline(linear_model, 
       col = "red", 
       lwd = 2)
# fit the quadratic Model
quadratic_model <- lm(score ~ income + I(income^2), data = CASchools)

# obtain the model summary
coeftest(quadratic_model, vcov. = vcovHC, type = "HC1")
# draw a scatterplot of the observations for income and test score
plot(CASchools$income, CASchools$score,
     col  = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the plot
abline(linear_model, col = "black", lwd = 2)

# add quatratic function to the plot
order_id <- order(CASchools$income)

lines(x = CASchools$income[order_id], 
      y = fitted(quadratic_model)[order_id],
      col = "red", 
      lwd = 2) 
# estimate a cubic model
cubic_model <- lm(score ~ poly(income, degree = 3, raw = TRUE), data = CASchools)
# test the hypothesis of a linear model against quadratic or polynomial
# alternatives

# set up hypothesis matrix
R <- rbind(c(0, 0, 1, 0),
            c(0, 0, 0, 1))

# do the test
linearHypothesis(cubic_model,
                 hypothesis.matrix = R,
                 white.adj = "hc1")
summary(cubic_model)
# test the hypothesis using robust standard errors
coeftest(cubic_model, vcov. = vcovHC, type = "HC1")
# perform robust F-test 
linearHypothesis(cubic_model, 
                 hypothesis.matrix = R,
                 vcov. = vcovHC, type = "HC1")
cat('
<div class = "keyconcept" id="KC8.1">
<h3 class = "right"> Key Concept 8.1 </h3>          
<h3 class = "left"> The Expected Effect on $Y$ of a Change in $X_1$ in a Nonlinear Regression Model </h3>

Consider the nonlinear population regression model

$$ Y_i = f(X_{1i}, X_{2i}, \\dots, X_{ki}) + u_i \\ , \\ i=1,\\dots,n,$$

where $f(X_{1i}, X_{2i}, \\dots, X_{ki})$ is the population regression function and $u_i$ is the error term.

Denote by $\\Delta Y$ the expected change in $Y$ associated with $\\Delta X_1$, the change in $X_1$ while holding $X_2, \\cdots , X_k$ constant. That is, the expected change in $Y$ is the difference

$$\\Delta Y = f(X_1 + \\Delta X_1, X_2, \\cdots, X_k) - f(X_1, X_2, \\cdots, X_k).$$

The estimator of this unknown population difference is the difference between the predicted values for these two cases. Let $\\hat{f}(X_1, X_2, \\cdots, X_k)$ be the predicted value of of $Y$ based on the estimator $\\hat{f}$ of the population regression function. Then the predicted change in $Y$ is

$$\\Delta \\widehat{Y} = \\hat{f}(X_1 + \\Delta X_1, X_2, \\cdots, X_k) - \\hat{f}(X_1, X_2, \\cdots, X_k).$$
</p>
</div>
')
cat('\\begin{keyconcepts}[The Expected Effect on $Y$ of a Change in $X_1$ in a Nonlinear Regression Model]{8.1}
Consider the nonlinear population regression model

$$ Y_i = f(X_{1i}, X_{2i}, \\dots, X_{ki}) + u_i \\ , \\ i=1,\\dots,n,$$

where $f(X_{1i}, X_{2i}, \\dots, X_{ki})$ is the population regression function and $u_i$ is the error term.

Denote by $\\Delta Y$ the expected change in $Y$ associated with $\\Delta X_1$, the change in $X_1$ while holding $X_2, \\cdots , X_k$ constant. That is, the expected change in $Y$ is the difference

$$\\Delta Y = f(X_1 + \\Delta X_1, X_2, \\cdots, X_k) - f(X_1, X_2, \\cdots, X_k).$$

The estimator of this unknown population difference is the difference between the predicted values for these two cases. Let $\\hat{f}(X_1, X_2, \\cdots, X_k)$ be the predicted value of of $Y$ based on the estimator $\\hat{f}$ of the population regression function. Then the predicted change in $Y$ is

$$\\Delta \\widehat{Y} = \\hat{f}(X_1 + \\Delta X_1, X_2, \\cdots, X_k) - \\hat{f}(X_1, X_2, \\cdots, X_k).$$
\\end{keyconcepts}
')
# compute and assign the quadratic model
quadriatic_model <- lm(score ~ income + I(income^2), data = CASchools)

# set up data for prediction
new_data <- data.frame(income = c(10, 11))

# do the prediction
Y_hat <- predict(quadriatic_model, newdata = new_data)

# compute the difference
diff(Y_hat)
# set up data for prediction
new_data <- data.frame(income = c(40, 41))

# do the prediction
Y_hat <- predict(quadriatic_model, newdata = new_data)

# compute the difference
diff(Y_hat)
# estimate a level-log model
LinearLog_model <- lm(score ~ log(income), data = CASchools)

# compute robust summary
coeftest(LinearLog_model, 
         vcov = vcovHC, type = "HC1")
# draw a scatterplot
plot(score ~ income, 
     col = "steelblue",
     pch = 20,
     data = CASchools,
     main = "Linear-Log Regression Line")

# add the linear-log regression line
order_id  <- order(CASchools$income)

lines(CASchools$income[order_id],
      fitted(LinearLog_model)[order_id], 
      col = "red", 
      lwd = 2)
# set up new data
new_data <- data.frame(income = c(10, 11, 40, 41))

# predict the outcomes 
Y_hat <- predict(LinearLog_model, newdata = new_data)

# compute the expected difference
Y_hat_matrix <- matrix(Y_hat, nrow = 2, byrow = TRUE)
Y_hat_matrix[, 2] - Y_hat_matrix[, 1]
# estimate a log-linear model 
LogLinear_model <- lm(log(score) ~ income, data = CASchools)

# obtain a robust coefficient summary
coeftest(LogLinear_model, 
         vcov = vcovHC, type = "HC1")
# estimate the log-log model
LogLog_model <- lm(log(score) ~ log(income), data = CASchools)

# print robust coefficient summary to the console
coeftest(LogLog_model, 
         vcov = vcovHC, type = "HC1")
# generate a scatterplot
plot(log(score) ~ income, 
     col = "steelblue", 
     pch = 20, 
     data = CASchools,
     main = "Log-Linear Regression Function")

# add the log-linear regression line
order_id  <- order(CASchools$income)

lines(CASchools$income[order_id], 
      fitted(LogLinear_model)[order_id], 
      col = "red", 
      lwd = 2)

# add the log-log regression line
lines(sort(CASchools$income), 
      fitted(LogLog_model)[order(CASchools$income)], 
      col = "green", 
      lwd = 2)

# add a legend
legend("bottomright",
       legend = c("log-linear model", "log-log model"),
       lwd = 2,
       col = c("red", "green"))
cat('
<div class = "keyconcept" id="KC8.2">
<h3 class = "right"> Key Concept 8.2 </h3>          
<h3 class = "left"> Logarithms in Regression: Three Cases </h3>

<p> Logarithms can be used to transform the dependent variable $Y$ or the independent variable $X$, or both 
(the variable being transformed must be positive). The following table summarizes these three cases and the interpretation of the regression coefficient $\\beta_1$. In each case, $\\beta_1$, can be estimated by applying OLS after taking the logarithm(s) of the dependent and/or the independent variable. </p>

<table>
<thead>
<tr class="header">
<th align="left">Case</th>
<th align="left">Model Specification</th>
<th align="left">Interpretation of $\\beta_1$</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">$(I)$</td>
<td align="left">$Y_i = \\beta_0 + \\beta_1 \\ln(X_i) + u_i$</td>
<td align="left">A $1 \\%$ change in $X$ is associated with a change in $Y$ of $0.01 \\times \\beta_1$.</td>
</tr>
<tr class="even">
<td align="left">$(II)$</td>
<td align="left">$\\ln(Y_i) = \\beta_0 + \\beta_1 X_i + u_i$</td>
<td align="left">A change in $X$ by one unit ($\\Delta X = 1$) is associated with a $100 \\times \\beta_1 \\%$ change in $Y$.</td>
</tr>
<tr class="odd">
<td align="left">$(III)$</td>
<td align="left">$\\ln(Y_i) = \\beta_0 + \\beta_1 \\ln(X_i) + u_i$</td>
<td align="left">A $1\\%$ change in $X$ is associated with a $\\beta_1\\%$ change in $Y$, so $\\beta_1$ is the elasticity of $Y$ with respect to $X$.</td>
</tr>
</tbody>
</table>

</div>
')
cat('\\begin{keyconcepts}[Logarithms in Regression: Three Cases]{8.2}
    \\begin{tabularx}{\\textwidth}{llX}
    \\textbf{Case}  & \\textbf{Model Specification} & \\textbf{Interpretation of $\\beta_1$} \\\\
    $(I)$ & $Y_i = \\beta_0 + \\beta_1 \\ln(X_i) + u_i$ & A $1 \\%$ change in $X$ is associated with a change in $Y$ of \\newline $0.01 \\times \\beta_1$. \\\\
 $(II)$  & $\\ln(Y_i) = \\beta_0 + \\beta_1 X_i + u_i$ & A change in $X$ by one unit ($\\Delta X = 1$) is associated with a \\newline $100 \\times \\beta_1 \\%$ change in $Y$. \\\\
    $(III)$ & $\\ln(Y_i) = \\beta_0 + \\beta_1 \\ln(X_i) + u_i$ & A $1\\%$ change in $X$ is associated with a $\\beta_1 \\%$ change in $Y$, so \\newline $\\beta_1$ is the elasticity of $Y$ with respect to $X$. \\\\
    \\end{tabularx}
\\end{keyconcepts}
')
# estimate the polylog model
polyLog_model <- lm(score ~ log(income) + I(log(income)^2) + I(log(income)^3), 
                    data = CASchools)

# print robust summary to the console
coeftest(polyLog_model, 
         vcov = vcovHC, type = "HC1")
# compute the adj. R^2 for the nonlinear models
adj_R2 <-rbind("quadratic" = summary(quadratic_model)$adj.r.squared,
               "cubic" = summary(cubic_model)$adj.r.squared,
               "LinearLog" = summary(LinearLog_model)$adj.r.squared,
               "LogLinear" = summary(LogLinear_model)$adj.r.squared,
               "LogLog" = summary(LogLog_model)$adj.r.squared,
               "polyLog" = summary(polyLog_model)$adj.r.squared)

# assign column names
colnames(adj_R2) <- "adj_R2"

adj_R2
# generate a scatterplot
plot(score ~ income, 
     data = CASchools,
     col = "steelblue", 
     pch = 20,
     main = "Linear-Log and Cubic Regression Functions")

# add the linear-log regression line
order_id  <- order(CASchools$income)

lines(CASchools$income[order_id],
      fitted(LinearLog_model)[order_id], 
      col = "darkgreen", 
      lwd = 2)

# add the cubic regression line
lines(x = CASchools$income[order_id], 
      y = fitted(cubic_model)[order_id],
      col = "darkred", 
      lwd = 2) 
cat('
<div class = "keyconcept" id="KC8.3">
<h3 class = "right"> Key Concept 8.3 </h3>          
<h3 class = "left"> A Method for Interpreting Coefficients in Regression with Binary Variables </h3>

Compute expected values of $Y$ for each possible set described by the set of binary variables. Compare the expected values. 
The coefficients can be expressed either as expected values or as the difference between at least two expected values.

</div>
')
cat('\\begin{keyconcepts}[A Method for Interpreting Coefficients in Regression with Binary Variables]{8.3}
Compute expected values of $Y$ for each possible set described by the set of binary variables. Compare the expected values. 
The coefficients can be expressed either as expected values or as the difference between at least two expected values.
\\end{keyconcepts}
')
# append HiSTR to CASchools
CASchools$HiSTR <- as.numeric(CASchools$size >= 20)

# append HiEL to CASchools
CASchools$HiEL <- as.numeric(CASchools$english >= 10)
# estimate the model with a binary interaction term
bi_model <- lm(score ~ HiSTR * HiEL, data = CASchools)

# print a robust summary of the coefficients
coeftest(bi_model, vcov. = vcovHC, type = "HC1")
# estimate means for all combinations of HiSTR and HiEL

# 1.
predict(bi_model, newdata = data.frame("HiSTR" = 0, "HiEL" = 0))

# 2.
predict(bi_model, newdata = data.frame("HiSTR" = 0, "HiEL" = 1))

# 3.
predict(bi_model, newdata = data.frame("HiSTR" = 1, "HiEL" = 0))

# 4.
predict(bi_model, newdata = data.frame("HiSTR" = 1, "HiEL" = 1))
cat('
<div class = "keyconcept" id="KC8.4">
<h3 class = "right"> Key Concept 8.4 </h3>          
<h3 class = "left"> Interactions Between Binary and Continuous Variables </h3>

An interaction term like $X_i \\times D_i$ (where $X_i$ is continuous and $D_i$ is binary) allows for the slope to depend on the binary
variable $D_i$. There are three possibilities:

1. Different intercept and same slope:
$$ Y_i = \\beta_0 + \\beta_1 X_i + \\beta_2 D_i + u_i $$
2. Different intercept and different slope:
$$ Y_i = \\beta_0 + \\beta_1 X_i + \\beta_2 D_i + \\beta_3 \\times (X_i \\times D_i) + u_i $$

3. Same intercept and different slope:
$$ Y_i = \\beta_0 + \\beta_1 X_i + \\beta_2 (X_i \\times D_i) + u_i $$
</div>
')
cat('\\begin{keyconcepts}[Interactions Between Binary and Continuous Variables]{8.4}
An interaction term like $X_i \\times D_i$ (where $X_i$ is continuous and $D_i$ is binary) allows for the slope to depend on the binary
variable $D_i$. There are three possibilities:\\newline

\\begin{enumerate}
\\item Different intercept and same slope:
$$ Y_i = \\beta_0 + \\beta_1 X_i + \\beta_2 D_i + u_i $$
\\item Different intercept and different slope:
$$ Y_i = \\beta_0 + \\beta_1 X_i + \\beta_2 D_i + \\beta_3 \\times (X_i \\times D_i) + u_i $$
\\item Same intercept and different slope:
$$ Y_i = \\beta_0 + \\beta_1 X_i + \\beta_2 (X_i \\times D_i) + u_i $$
\\end{enumerate}

\\end{keyconcepts}
')
# generate artificial data
set.seed(1)

X <- runif(200,0, 15)
D <- sample(0:1, 200, replace = T)
Y <- 450 +  150 * X + 500 * D + 50 * (X * D) + rnorm(200, sd = 300)

# divide plotting area accordingly
m <- rbind(c(1, 2), c(3, 0))
graphics::layout(m)

# estimate the models and plot the regression lines

# 1. (baseline model)
plot(X, log(Y),
     pch = 20,
     col = "steelblue",
     main = "Different Intercepts, Same Slope")

mod1_coef <- lm(log(Y) ~ X + D)$coefficients

abline(coef = c(mod1_coef[1], mod1_coef[2]), 
       col = "red",
       lwd = 1.5)

abline(coef = c(mod1_coef[1] + mod1_coef[3], mod1_coef[2]), 
       col = "green",
       lwd = 1.5)
       
# 2. (baseline model + interaction term)
plot(X, log(Y),
     pch = 20,
     col = "steelblue",
     main = "Different Intercepts, Different Slopes")

mod2_coef <- lm(log(Y) ~ X + D + X:D)$coefficients

abline(coef = c(mod2_coef[1], mod2_coef[2]), 
       col = "red",
       lwd = 1.5)

abline(coef = c(mod2_coef[1] + mod2_coef[3], mod2_coef[2] + mod2_coef[4]), 
       col = "green",
       lwd = 1.5)

# 3. (omission of D as regressor + interaction term)
plot(X, log(Y),
     pch = 20,
     col = "steelblue",
     main = "Same Intercept, Different Slopes")

mod3_coef <- lm(log(Y) ~ X + X:D)$coefficients

abline(coef = c(mod3_coef[1], mod3_coef[2]), 
       col = "red",
       lwd = 1.5)

abline(coef = c(mod3_coef[1], mod3_coef[2] + mod3_coef[3]), 
       col = "green",
       lwd = 1.5)
# estimate the model
bci_model <- lm(score ~ size + HiEL + size * HiEL, data = CASchools)

# print robust summary of coefficients to the console
coeftest(bci_model, vcov. = vcovHC, type = "HC1")
# identify observations with PctEL >= 10
id <- CASchools$english >= 10

# plot observations with HiEL = 0 as red dots
plot(CASchools$size[!id], CASchools$score[!id],
     xlim = c(0, 27),
     ylim = c(600, 720),
     pch = 20,
     col = "red",
     main = "",
     xlab = "Class Size",
     ylab = "Test Score")

# plot observations with HiEL = 1 as green dots
points(CASchools$size[id], CASchools$score[id],
     pch = 20,
     col = "green")

# read out estimated coefficients of bci_model
coefs <- bci_model$coefficients

# draw the estimated regression line for HiEL = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# draw the estimated regression line for HiEL = 1
abline(coef = c(coefs[1] + coefs[3], coefs[2] + coefs[4]),
       col = "green", 
       lwd = 1.5 )

# add a legend to the plot
legend("topright", 
       pch = c(20, 20), 
       col = c("red", "green"), 
       legend = c("HiEL = 0", "HiEL = 1"))
cat('
<div class = "keyconcept" id="KC8.5">
<h3 class = "right"> Key Concept 8.5 </h3>          
<h3 class = "left"> Interactions in Multiple Regression </h3>

The interaction term between the two regressors $X_1$ and $X_2$ is given by their product $X_1 \\times X_2$. Adding this
interaction term as a regressor to the model $$ Y_i = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + u_i $$ allows the effect on $Y$ of a change in $X_2$ to depend on the value of $X_1$ and vice versa. Thus the coefficient $\\beta_3$ in the model $$ Y_i = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\beta_3 (X_1 \\times X_2) + u_i $$ measures the effect of a one-unit increase in both $X_1$ <it>and</it> $X_2$ above and beyond the sum of both individual effects. This holds for continuous *and* binary regressors.

</div>
')
cat('\\begin{keyconcepts}[Interactions in Multiple Regression]{8.5}
The interaction term between the two regressors $X_1$ and $X_2$ is given by their product $X_1 \\times X_2$. Adding this
interaction term as a regressor to the model $$ Y_i = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + u_i $$ allows the effect on $Y$ of a change in $X_2$ to depend on the value of $X_1$ and vice versa. Thus the coefficient $\\beta_3$ in the model $$ Y_i = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\beta_3 (X_1 \\times X_2) + u_i $$ measures the effect of a one-unit increase in both $X_1$ \\textit{and} $X_2$ above and beyond the sum of both individual effects. This holds for continuous \\textit{and} binary regressors.
\\end{keyconcepts}
')
# estimate regression model including the interaction between 'PctEL' and 'size'
cci_model <- lm(score ~ size + english + english * size, data = CASchools) 

# print a summary to the console
coeftest(cci_model, vcov. = vcovHC, type = "HC1")
summary(CASchools$english)
# load package and the data set
library(AER)
data("Journals")
# define and rename variables
Journals$PricePerCitation <- Journals$price/Journals$citations
Journals$Age <- 2000 - Journals$foundingyear
Journals$Characters <- Journals$charpp * Journals$pages/10^6
Journals$Subscriptions <- Journals$subs
# compute summary statistics for price per citation
summary(Journals$PricePerCitation)
# Estimate models (I) - (IV)
Journals_mod1 <- lm(log(Subscriptions) ~ log(PricePerCitation), 
                    data = Journals)

Journals_mod2 <- lm(log(Subscriptions) ~ log(PricePerCitation) 
                    + log(Age) + log(Characters), 
                    data = Journals)

Journals_mod3 <- lm(log(Subscriptions) ~ 
                    log(PricePerCitation) + I(log(PricePerCitation)^2) 
                    + I(log(PricePerCitation)^3) + log(Age) 
                    + log(Age):log(PricePerCitation) + log(Characters), 
                    data = Journals)

Journals_mod4 <- lm(log(Subscriptions) ~ 
                    log(PricePerCitation) + log(Age) 
                    + log(Age):log(PricePerCitation) + 
                    log(Characters), 
                    data = Journals)
# F-Test for significance of cubic terms
linearHypothesis(Journals_mod3, 
                 c("I(log(PricePerCitation)^2)=0", "I(log(PricePerCitation)^3)=0"),
                 vcov. = vcovHC, type = "HC1")
## # load the stargazer package
## library(stargazer)
## 
## # gather robust standard errors in a list
## rob_se <- list(sqrt(diag(vcovHC(Journals_mod1, type = "HC1"))),
##                sqrt(diag(vcovHC(Journals_mod2, type = "HC1"))),
##                sqrt(diag(vcovHC(Journals_mod3, type = "HC1"))),
##                sqrt(diag(vcovHC(Journals_mod4, type = "HC1"))))
## 
## # generate a Latex table using stargazer
## stargazer(Journals_mod1, Journals_mod2, Journals_mod3, Journals_mod4,
##           se = rob_se,
##           digits = 3,
##           column.labels = c("(I)", "(II)", "(III)", "(IV)"))
# load the stargazer package
library(stargazer)

# gather robust standard errors in a list
rob_se <- list(
  sqrt(diag(vcovHC(Journals_mod1, type = "HC1"))),
  sqrt(diag(vcovHC(Journals_mod2, type = "HC1"))),
  sqrt(diag(vcovHC(Journals_mod3, type = "HC1"))),
  sqrt(diag(vcovHC(Journals_mod4, type = "HC1")))
)

# generate a LaTeX table using stargazer
stargazer(Journals_mod1, Journals_mod2, Journals_mod3, Journals_mod4,
          type = "html", 
          model.numbers = FALSE,
          header = FALSE,
          dep.var.caption = "Dependent Variable: Logarithm of Subscriptions",
          se = rob_se,
          digits = 3,
          column.labels = c("(I)", "(II)", "(III)", "(IV)")
          )

stargazer_html_title("Nonlinear Regression Models of Journal Subscribtions", "nrmojs")
library(stargazer)

rob_se <- list(
  sqrt(diag(vcovHC(Journals_mod1, type = "HC1"))),
  sqrt(diag(vcovHC(Journals_mod2, type = "HC1"))),
  sqrt(diag(vcovHC(Journals_mod3, type = "HC1"))),
  sqrt(diag(vcovHC(Journals_mod4, type = "HC1")))
)

stargazer(Journals_mod1, Journals_mod2, Journals_mod3, Journals_mod4,
          type = "latex", 
          float.env = "sidewaystable",
          dep.var.caption = "Dependent Variable: Logarithm of Subscriptions",
          title = "\\label{tab:nrmojs} Nonlinear Regression Models of Journal Subscribtions",
          model.numbers = FALSE,
          header=FALSE,
          se = rob_se,
          digits = 3,
          column.labels = c("(I)", "(II)", "(III)", "(IV)")
          )
# divide plotting area
m <- rbind(c(1, 2), c(3, 0))
graphics::layout(m)

# scatterplot
plot(Journals$PricePerCitation, 
     Journals$Subscriptions, 
     pch = 20, 
     col = "steelblue",
     ylab = "Subscriptions",
     xlab = "ln(Price per ciation)",
     main = "(a)")

# log-log scatterplot and estimated regression line (I)
plot(log(Journals$PricePerCitation), 
     log(Journals$Subscriptions), 
     pch = 20, 
     col = "steelblue",
     ylab = "ln(Subscriptions)",
     xlab = "ln(Price per ciation)",
     main = "(b)")

abline(Journals_mod1,
       lwd = 1.5)

# log-log scatterplot and regression lines (IV) for Age = 5 and Age = 80
plot(log(Journals$PricePerCitation), 
     log(Journals$Subscriptions), 
     pch = 20, 
     col = "steelblue",
     ylab = "ln(Subscriptions)",
     xlab = "ln(Price per ciation)",
     main = "(c)")

JM4C <-Journals_mod4$coefficients

# Age = 80
abline(coef = c(JM4C[1] + JM4C[3] * log(80), 
                JM4C[2] + JM4C[5] * log(80)),
       col = "darkred",
       lwd = 1.5)

# Age = 5
abline(coef = c(JM4C[1] + JM4C[3] * log(5), 
                JM4C[2] + JM4C[5] * log(5)),
       col = "darkgreen",
       lwd = 1.5)
# estimate all models
TestScore_mod1 <- lm(score ~ size + english + lunch, data = CASchools)

TestScore_mod2 <- lm(score ~ size + english + lunch + log(income), data = CASchools)

TestScore_mod3 <- lm(score ~ size + HiEL + HiEL:size, data = CASchools)

TestScore_mod4 <- lm(score ~ size + HiEL + HiEL:size + lunch + log(income), data = CASchools)

TestScore_mod5 <- lm(score ~ size + I(size^2) + I(size^3) + HiEL + lunch + log(income), data = CASchools)

TestScore_mod6 <- lm(score ~ size + I(size^2) + I(size^3) + HiEL + HiEL:size + HiEL:I(size^2) + HiEL:I(size^3) + lunch + log(income), data = CASchools)

TestScore_mod7 <- lm(score ~ size + I(size^2) + I(size^3) + english + lunch + log(income), data = CASchools)
## # gather robust standard errors in a list
## rob_se <- list(sqrt(diag(vcovHC(TestScore_mod1, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_mod2, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_mod3, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_mod4, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_mod5, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_mod6, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_mod7, type = "HC1"))))
## 
## # generate a LaTeX table of regression outputs
## stargazer(TestScore_mod1,
##           TestScore_mod2,
##           TestScore_mod3,
##           TestScore_mod4,
##           TestScore_mod5,
##           TestScore_mod6,
##           TestScore_mod7,
##           digits = 3,
##           dep.var.caption = "Dependent Variable: Test Score",
##           se = rob_se,
##           column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"))
rob_se <- list(
  sqrt(diag(vcovHC(TestScore_mod1, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod2, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod3, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod4, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod5, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod6, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod7, type = "HC1")))
)

stargazer(TestScore_mod1, 
          TestScore_mod2, 
          TestScore_mod3, 
          TestScore_mod4, 
          TestScore_mod5, 
          TestScore_mod6, 
          TestScore_mod7,
          digits = 3,
          dep.var.caption = "Dependent Variable: Test Score",
          se = rob_se,
          type = "html", 
          model.numbers = FALSE,
          header=FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)")
          )

stargazer_html_title("Nonlinear Models of Test Scores", "nmots")
rob_se <- list(
  sqrt(diag(vcovHC(TestScore_mod1, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod2, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod3, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod4, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod5, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod6, type = "HC1"))),
  sqrt(diag(vcovHC(TestScore_mod7, type = "HC1")))
)

stargazer(TestScore_mod1, 
          TestScore_mod2, 
          TestScore_mod3, 
          TestScore_mod4, 
          TestScore_mod5, 
          TestScore_mod6, 
          TestScore_mod7,
          dep.var.caption = "Dependent Variable: Test Score",
          title = "\\label{tab:nmots} Nonlinear Models of Test Scores",
          digits = 3,
          se = rob_se,
          type = "latex", 
          float.env = "sidewaystable",
          model.numbers = FALSE,
          omit.stat = c("f","ser"),
          header=FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)")
          )
# check joint significance of the interaction terms
linearHypothesis(TestScore_mod6, 
                 c("size:HiEL=0", "I(size^2):HiEL=0", "I(size^3):HiEL=0"),
                 vcov. = vcovHC, type = "HC1")
# scatterplot
plot(CASchools$size, 
     CASchools$score, 
     xlim = c(12, 28),
     ylim = c(600, 740),
     pch = 20, 
     col = "gray", 
     xlab = "Student-Teacher Ratio", 
     ylab = "Test Score")

# add a legend
legend("top", 
       legend = c("Linear Regression (2)", 
                  "Cubic Regression (5)", 
                  "Cubic Regression (7)"),
       cex = 0.8,
       ncol = 3,
       lty = c(1, 1, 2),
       col = c("blue", "red", "black"))

# data for use with predict()
new_data <- data.frame("size" = seq(16, 24, 0.05), 
                       "english" = mean(CASchools$english),
                       "lunch" = mean(CASchools$lunch),
                       "income" = mean(CASchools$income),
                       "HiEL" = mean(CASchools$HiEL))

# add estimated regression function for model (2)
fitted <- predict(TestScore_mod2, newdata = new_data)

lines(new_data$size, 
      fitted,
      lwd = 1.5,
      col = "blue")

# add estimated regression function for model (5)
fitted <- predict(TestScore_mod5, newdata = new_data)

lines(new_data$size, 
      fitted, 
      lwd = 1.5,
      col = "red")

# add estimated regression function for model (7)
fitted <- predict(TestScore_mod7, newdata = new_data)

lines(new_data$size, 
      fitted, 
      col = "black",
      lwd = 1.5,
      lty = 2)
# draw scatterplot

# observations with HiEL = 0
plot(CASchools$size[CASchools$HiEL == 0], 
     CASchools$score[CASchools$HiEL == 0], 
     xlim = c(12, 28),
     ylim = c(600, 730),
     pch = 20, 
     col = "gray", 
     xlab = "Student-Teacher Ratio", 
     ylab = "Test Score")

# observations with HiEL = 1
points(CASchools$size[CASchools$HiEL == 1], 
       CASchools$score[CASchools$HiEL == 1],
       col = "steelblue",
       pch = 20)

# add a legend
legend("top", 
       legend = c("Regression (6) with HiEL=0", "Regression (6) with HiEL=1"),
       cex = 0.7,
       ncol = 2,
       lty = c(1, 1),
       col = c("green", "red"))

# data for use with 'predict()'
new_data <- data.frame("size" = seq(12, 28, 0.05), 
                       "english" = mean(CASchools$english),
                       "lunch" = mean(CASchools$lunch),
                       "income" = mean(CASchools$income),
                       "HiEL" = 0)

# add estimated regression function for model (6) with HiEL=0
fitted <- predict(TestScore_mod6, newdata = new_data)

lines(new_data$size, 
      fitted, 
      lwd = 1.5,
      col = "green")

# add estimated regression function for model (6) with HiEL=1
new_data$HiEL <- 1

fitted <- predict(TestScore_mod6, newdata = new_data)

lines(new_data$size, 
      fitted, 
      lwd = 1.5,
      col = "red")
if (my_output=="html"){
  cat('
<div  class = "DCexercise">

#### 1. Correlation and (Non)linearity I {-}
 
Consider the estimated simple linear regression model $$\\widehat{medv_i} = 34.554 - 0.95\\times lstat_i,$$

with <tt>medv</tt> (the median house value in the suburb) and <tt>lstat</tt> (the percent of households with low socioeconomic status in the suburb) being variables from the already known <tt>Boston</tt> dataset.

The <tt>lm()</tt>  object for the above model is available as <tt>mod</tt> in your working environment. The package <tt>MASS</tt> has been loaded.

**Instructions:**

  + Compute the correlation coefficient between <tt>medv</tt> and <tt>lstat</tt> and save it to <tt>corr</tt>.
  
  + Plot <tt>medv</tt> against <tt>lstat</tt> and add the regression line using the model object <tt>mod</tt>. What do you notice?

<iframe src="DCL/ex8_1.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>
        
**Hints:**
        
  + You can use <tt>cor()</tt> to compute the correlation between variables.
      
  + You may use <tt>plot()</tt> and <tt>abline()</tt> to visualize regression results.
      
</div>')
} else {
  cat('\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}')
}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 2. Correlation and (Non)linearity II {-}

In the previous exercise we saw an example where the correlation between the dependent variable <tt>medv</tt> and the regressor <tt>medv</tt> is not useful for choosing the functional form of the regression since correlation captures only the linear relationship. 

As an alternative, consider the nonlinear specification

$$medv_i = \\beta_0 + \\beta_1\\times\\log(lstat_i) + u_i.$$

The package <tt>MASS</tt> has been loaded.
      
**Instructions:**
        
  + Conduct the regression from above and assign the result to <tt>log_mod</tt>.
      
  + Visualize your results using a scatterplot and add the regression line. In comparison to the previous exercise, what do you notice now? 

<iframe src="DCL/ex8_2.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**
  
  + Use <tt>lm()</tt> to conduct the regression.

  + Use <tt>plot()</tt> and <tt>abline()</tt> to visualize regression results.
  
</div>')
}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 3. The Optimal Polynomial Order --- Sequential Testing {-}

Recall the following model from the previous exercise $$medv_i = \\beta_0 + \\beta_1\\times\\log(lstat_i) + u_i.$$

We saw that this model specification seems to be a reasonable choice. However, a higher order polynomial in $\\log(lstat_i)$ may be more suited for explaining $medv$.

The packages <tt>AER</tt> and <tt>MASS</tt> have been loaded.
      
**Instructions:**
        
  + Determine the optimal order of a polylog model using sequential testing. Use a maximum polynomial order of $r=4$ and the significance level $\\alpha=0.05$. We would like you to use a <tt>for()</tt> loop and recommend the following approach:

    1. Estimate a model, say <tt>mod</tt>, which starts with the highest polynomial order
    2. Save the $p$-value (use robust standard errors) of the relevant parameter and compare it to the significance level $\\alpha$
    3. If you cannot reject the null, repeat steps 1 and 2 for the next lowest polynomial order, otherwise stop the loop and print out the polynomial order

  + Compute the $R^2$ of the selected model and assign it to <tt>R2</tt>.

<iframe src="DCL/ex8_3.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + The index for the <tt>for()</tt> loop should start at 4 and end at 1.

  + Using <tt>poly()</tt> in the argument <tt>formula</tt> of <tt>lm()</tt> is a generic way to incorporate higher orders of a certain variable in the model. Besides the variable, you have to specify the degree of the polynomial via the argument <tt>degree</tt> and set <tt>raw = TRUE</tt>.

  + Use <tt>coeftest()</tt> together with the argument <tt>vcov.</tt> to obtain $p$-values (use robust standard errors!). Use the structure of the resulting object to extract the relevant $p$-value.

  + An <tt>if()</tt> statement may be useful to check whether the condition for acceptance of the null in step 3 is fulfilled. 

  + A <tt>for()</tt> loop is stopped using <tt>break</tt>.

  + Use <tt>summary()</tt> to obtain the $R^2$. You may extract it by appending <tt>$r.squared</tt> to the function call.
  
</div>') 
}

if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 4. The Estimated Effect of a Unit Change {-}

Reconsider the polylog model from the previous exercise that was selected by the sequential testing approach. As this model is logarithmic and of quadratic form, we cannot simply read off the estimated effect of a unit change (that is, one percent) in <tt>lstat</tt> from the coefficient summary because this effect depends on the level of <tt>lstat</tt>. We  may compute this manually.

The selected polylog model <tt>mod_pl</tt> is available in your working environment. The package <tt>MASS</tt> has been loaded.
      
**Instructions:**

Assume that we are interested in the effect on <tt>medv</tt> of an increase in <tt>lstat</tt> from $10\\%$ to $11\\%$.

  + Set up a <tt>data.frame</tt> with the relevant observations of <tt>lstat</tt>.

  + Use the new observations to predict the corresponding values of <tt>medv</tt>.

  + Compute the expected effect with the help of <tt>diff()</tt>.

<iframe src="DCL/ex8_4.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

- You may use <tt>predict()</tt> together with the new data to obtain the predicted values of <tt>medv</tt>. Note that the column names of the <tt>data.frame</tt> must match the names of the regressors when using <tt>predict()</tt>.

- <tt>diff()</tt> expects a vector. It computes the differences between all entries of this vector.
  
</div>') 
}

if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 5. Interactions between Independent Variables I {-}

Consider the following regression model

$$medv_i=\\beta_0+\\beta_1\\times chas_i+\\beta_2\\times old_i+\\beta_3\\times (chas_i\\cdot old_i)+u_i$$

where $chas_i$ and $old_i$ are dummy variables. The former takes the value $1$, if the Charles River (a short river in the proximity of Boston) passes through suburb $i$ and is $0$ otherwise. The latter indicates for a high proportion of old buildings and is constructed as

\\begin{align}
old_i = & \\,
    \\begin{cases}
      1 & \\text{if $age_i\\geq 95$},\\\\
      0 & \\text{else},
    \\end{cases}
\\end{align}

with $age_i$ being the proportion of owner-occupied units built prior to 1940 in suburb $i$.

The packages <tt>MASS</tt> and <tt>AER</tt> have been loaded.
      
**Instructions:**
        
  + Generate an append the binary variable <tt>old</tt> to the dataset <tt>Boston</tt>.

  + Conduct the regression stated above and assign the result to <tt>mod_bb</tt>.

  + Obtain a robust coefficient summary of the model. How do you interpret the results?

<iframe src="DCL/ex8_5.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + The operator <tt>>=</tt> can be used to generate a logical vector. Transform a logical vector to the numeric type via <tt>as.numeric()</tt>.

  + In <tt>lm()</tt> there are two ways to include interaction terms using the argument <tt>formula</tt>:

      1. <tt>Var1*Var2</tt> to add <tt>Var1</tt>, <tt>Var2</tt> and the corresponding interaction term at once

      2. <tt>Var1:Var2</tt> to manually add the interaction term (which of course requires you to add the remaining terms manually as well)

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 6. Interactions between Independent Variables II {-}

Now consider the regression model

$$medv_i=\\beta_0+\\beta_1\\times indus_i+\\beta_2\\times old_i+\\beta_3\\times (indus_i\\cdot old_i)+u_i$$

with $old_i$ defined as in the previous exercise and $indus_i$ being the proportion of non-retail business acres in suburb $i$.

The vector <tt>old</tt> from the previous exercise has been appended to the dataset. The package <tt>MASS</tt> has been loaded.
      
**Instructions:**
        
  + Estimate the above regression model and assign the result to <tt>mod_bc</tt>.

  + Extract the estimated coefficients of the model and assign them to <tt>params</tt>.

  + Plot <tt>medv</tt> against <tt>indus</tt> and add the regression lines for both states of the binary variable $old$.

<iframe src="DCL/ex8_6.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + Make use the structure of <tt>mod_bc</tt> the output generated by <tt>coef()</tt> to extract the estimated coefficients.

  + Apart from passing an <tt>lm()</tt> object to <tt>abline()</tt> one may also specify intercept and slope manually using the arguments <tt>a</tt> and <tt>b</tt>, respectively.
  
</div>') 
}
