
my_output <- knitr::opts_knit$get("rmarkdown.pandoc.to")

# function for escaping special LaTeX characters
escape_latex = function(x, newlines = FALSE, spaces = FALSE) {
	x = gsub('\\\\', '\\\\textbackslash', x)
	x = gsub('([#$%&_{}])', '\\\\\\1', x)
	x = gsub('\\\\textbackslash', '\\\\textbackslash{}', x)
	x = gsub('~', '\\\\textasciitilde{}', x)
	x = gsub('\\^', '\\\\textasciicircum{}', x)
	if (newlines) x = gsub('(?<!\n)\n(?!\n)', '\\\\\\\\', x, perl = TRUE)
	if (spaces) x = gsub('  ', '\\\\ \\\\ ', x)
	x
}

# function for output dependent inline code formatting
ttcode <- function(x, type = "tt") {
  outputFormat <- knitr:::pandoc_to()
  if (outputFormat %in% c('latex', 'beamer'))
    paste0("\\texttt{", escape_latex(x), "}")
  else if (outputFormat == 'html')
    paste0("<", type, ">", x, "</", type, ">")
  else
    x
}

# function to add html title and tag below stargazer table
stargazer_html_title <- function(title, tag) {
  cat(paste("<caption><p style='text-align:center'>(#tab:", tag, ")", " ", title, "</p></caption>", sep = ""))
}

# function that generates a html file from dc exercise code and includes an iframe
write_html <- function(html_body = NULL, label = NULL, write = T, ex = T, height = 320, playground = F) {
  
  #out_type <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  
  if (playground) {
    cat(paste("<iframe src='DCL/playground.html' frameborder='0' scrolling='no' style='width:100%;height:", height+20, "px'></iframe>", sep = ""))
  } 
  else if (write) {
    code <- paste(
      "<!DOCTYPE html>
      <head>
	      <meta charset='utf-8'/>
	      <link rel='stylesheet' href='exercise.css'/>
        <script async src='https://cdn.datacamp.com/dcl-react-dev.js.gz'></script>
      </head>
      <body>
      <div data-datacamp-exercise data-lang='r' data-height =", height, ">",
      html_body,
      "</div>
      </body>
      </html>", sep = "")
    if (ex == T) {
      writeLines(code, sep = "\n", paste("DCL/ex", label, ".html", sep = ""))
      cat(paste("<iframe src='DCL/ex", label, ".html' frameborder='0' scrolling='no' style='width:100%;height:", height+20, "px'></iframe>", sep = ""))
    }
    if (ex == F) {
      writeLines(code, sep = "\n", paste("DCL/", label, ".html", sep = ""))
      cat(paste("<iframe src='DCL/", label, ".html' frameborder='0' scrolling='no' style='width:100%;height:", height+20, "px'></iframe>", sep = ""))
    }
  } else {
    cat(paste("<iframe src='DCL/ex", label, ".html' frameborder='0' scrolling='no' style='width:100%;height:", height+20, "px'></iframe>", sep = ""))
  }
  }
knitr::opts_chunk$set(fig.width=5.6, fig.height=3.7, fig.align='center', fig.pos = 'h') 
knitr::opts_chunk$set(message=F, warning=F) 
# load the AER package 
library(AER)   

# load the the data set in the workspace
data(CASchools) 

# compute STR and append it to CASchools
CASchools$STR <- CASchools$students/CASchools$teachers 

# compute TestScore and append it to CASchools
CASchools$score <- (CASchools$read + CASchools$math)/2  

# Add HiSTR to CASchools
CASchools$HiSTR <- as.numeric(CASchools$STR >= 20)

# Add HiEL to CASchools
CASchools$HiEL <- as.numeric(CASchools$english >= 10)

# model (2) for California
TestScore_mod2 <- lm(score ~ STR + english + lunch + log(income), data = CASchools)
## library(AER)
## library(mvtnorm)
## library(stargazer)
cat("
<div class = 'keyconcept' id='KC9.1'>
<h3 class = 'right'> Key Concept 9.1 </h3>
<h3 class = 'left'> Internal and External Validity </h3>

A statistical analysis has *internal* validity if the statistical inference made about causal effects are valid for the considered population.

An analysis is said to have *external* validity if inferences and conclusion are valid for the studies' population and can be generalized to other populations and settings.

</div>
")
cat("\\begin{keyconcepts}[Internal and External Validity]{9.1}
A statistical analysis has \\textit{internal} validity if the statistical inference made about causal effects are valid for the considered population.\\newline

An analysis is said to have \\textit{external} validity if inferences and conclusion are valid for the studies' population and can be generalized to other populations and settings.
\\end{keyconcepts}
")
cat('
<div class = "keyconcept" id="KC9.2">
<h3 class = "right"> Key Concept 9.2 </h3>
<h3 class = "left"> Omitted Variable Bias: Should I include More Variables in My Regression? </h3>

Inclusion of additional variables reduces the risk of omitted variable bias but may increase the variance of the estimator of the coefficient of interest. 

We present some guidelines that help deciding whether to include an additional variable:

1. Specify the coefficient(s) of interest

2. Identify the most important potential sources of omitted variable bias by using knowledge available *before* estimating the model. You should end up with a baseline specification and a set of regressors that are questionable

3. Use different model specifications to test whether questionable regressors have coefficients different from zero

4. Use tables to provide full disclosure of your results, i.e., present different model specifications that both support your argument and enable the reader to see the effect of including questionable regressors

</div>
')
cat('\\begin{keyconcepts}[Omitted Variable Bias: Should I include More Variables in My Regression? ]{9.2}

Inclusion of additional variables reduces the risk of omitted variable bias but may increase the variance of the estimator of the coefficient of interest.\\newline 

We present some guidelines that help deciding whether to include an additional variable:\\newline

\\begin{itemize}
\\item Specify the coefficient(s) of interest
\\item Identify the most important potential sources of omitted variable bias by using knowledge available \\textit{before} estimating the model. You should end up with a base specification and a set of regressors that are questionable
\\item Use different model specifications to test whether questionable regressors have coefficients different from zero
\\item Use tables to provide full disclosure of your results, i.e., present different model specifications that both support your argument and enable the reader to see the effect of including questionable regressors
\\end{itemize}

\\end{keyconcepts}
')
cat('
<div class = "keyconcept" id="KC9.3">
<h3 class = "right"> Key Concept 9.3 </h3>
<h3 class = "left"> Functional Form Misspecification </h3>

A regression suffers from misspecification of the functional form when the functional form of the estimated regression model differs from the functional form of the population regression function. Functional form misspecification leads to biased and inconsistent coefficient estimators. A way to detect functional form misspecification is to plot the estimated regression function and the data. This may also be helpful to choose the correct functional form.

</div>
')
cat('\\begin{keyconcepts}[Functional Form Misspecification]{9.3}
We say a regression suffers from misspecification of the functional form when the functional form of the estimated regression model differs from the functional form of the population regression function. Functional form misspecification leads to biased and inconsistent coefficient estimators. A way to detect functional form misspecification is to plot the estimated regression function and the data. This may also be helpful to choose the correct functional form.
\\end{keyconcepts}
')
# set seed for reproducibility
set.seed(3)

# simulate data set
X <- runif(100, -5, 5)
Y <- X^2 + rnorm(100)

# estimate the regression function
ms_mod <- lm(Y ~ X)
ms_mod
# plot the data
plot(X, Y, 
     main = "Misspecification of Functional Form",
     pch = 20,
     col = "steelblue")

# plot the linear regression line
abline(ms_mod, 
       col = "darkred",
       lwd = 2)
cat('
<div class = "keyconcept" id="KC9.4">
<h3 class = "right"> Key Concept 9.4 </h3>
<h3 class = "left"> Errors-in-Variable Bias </h3>

When independent variables are measured imprecisely, we speak of errors-in-variables bias. This bias does not disappear if the sample size is large. If the measurement error has mean zero and is independent of the affected variable, the OLS estimator of the respective coefficient is biased towards zero.

</div>
')
cat('\\begin{keyconcepts}[Errors-in-Variable Bias]{9.4}
When independent variables are measured imprecisely, we speak of errors-in-variables bias. This bias does not disappear if the sample size is large. If the measurement error has mean zero and is independent of the affected variable, the OLS estimator of the respective coefficient is biased towards zero.
\\end{keyconcepts}
')
# set seed
set.seed(1)

# load the package 'mvtnorm' and simulate bivariate normal data
library(mvtnorm)
dat <- data.frame(
  rmvnorm(1000, c(50, 100), 
          sigma = cbind(c(10, 5), c(5, 10))))

# set columns names
colnames(dat) <- c("X", "Y")
# estimate the model (without measurement error)
noerror_mod <- lm(Y ~ X, data = dat)

# estimate the model (with measurement error in X)
dat$X <- dat$X + rnorm(n = 1000, sd = sqrt(10))
error_mod <- lm(Y ~ X, data = dat)

# print estimated coefficients to console
noerror_mod$coefficients
error_mod$coefficients
# plot sample data
plot(dat$X, dat$Y, 
     pch = 20, 
     col = "steelblue",
     xlab = "X",
     ylab = "Y")

# add population regression function
abline(coef = c(75, 0.5), 
       col = "darkgreen",
       lwd  = 1.5)

# add estimated regression functions
abline(noerror_mod, 
       col = "purple",
       lwd  = 1.5)

abline(error_mod, 
       col = "darkred",
       lwd  = 1.5)

# add legend
legend("topleft",
       bg = "transparent",
       cex = 0.8,
       lty = 1,
       col = c("darkgreen", "purple", "darkred"), 
       legend = c("Population", "No Errors", "Errors"))
cat('
<div class = "keyconcept" id="KC9.5">
<h3 class = "right"> Key Concept 9.5 </h3>
<h3 class = "left"> Sample Selection Bias </h3>

When the sampling process influences the availability of data and when there is a relation of this sampling process to the dependent variable that goes beyond the dependence on the regressors, we say that there is a sample selection bias. This bias is due to correlation between one or more regressors and the error term. Sample selection implies both bias and inconsistency of the OLS estimator.

</div>
')
cat('\\begin{keyconcepts}[Sample Selection Bias]{9.5}
When the sampling process influences the availability of data and when there is a relation of this sampling process to the dependent variable that goes beyond the dependence on the regressors, we say that there is a sample selection bias. This bias is due to correlation between one or more regressors and the error term. Sample selection implies both bias and inconsistency of the OLS estimator.
\\end{keyconcepts}
')
# set seed
set.seed(1)

# simulate data
dat <- data.frame(
  rmvnorm(1000, c(50, 100), 
          sigma = cbind(c(10, 5), c(5, 10))))

colnames(dat) <- c("X", "Y")

# mark 500 randomly selected observations
id <- sample(1:1000, size = 500)

plot(dat$X[-id], 
     dat$Y[-id], 
     col = "steelblue", 
     pch = 20,
     cex = 0.8,
     xlab = "X",
     ylab = "Y")

points(dat$X[id], 
       dat$Y[id],
       cex = 0.8,
       col = "gray", 
       pch = 20)

# add the population regression function
abline(coef = c(75, 0.5), 
       col = "darkgreen",
       lwd  = 1.5)

# add the estimated regression function for the full sample
abline(noerror_mod)

# estimate model case 1 and add the regression line
dat <- dat[-id, ]

c1_mod <- lm(dat$Y ~ dat$X, data = dat)
abline(c1_mod, col = "purple")

# add a legend
legend("topleft",
       lty = 1,
       bg = "transparent",
       cex = 0.8,
       col = c("darkgreen", "black", "purple"), 
       legend = c("Population", "Full sample", "500 obs. randomly selected"))
# set random seed
set.seed(1)

# simulate data
dat <- data.frame(
  rmvnorm(1000, c(50, 100), 
          sigma = cbind(c(10, 5), c(5, 10))))

colnames(dat) <- c("X", "Y")

# mark observations
id <- dat$X >= 45

plot(dat$X[-id], 
     dat$Y[-id], 
     col = "steelblue",
     cex = 0.8,
     pch = 20,
     xlab = "X",
     ylab = "Y")

points(dat$X[id], 
       dat$Y[id], 
       col = "gray",
       cex = 0.8,
       pch = 20)

# add population regression function
abline(coef = c(75, 0.5), 
       col = "darkgreen",
       lwd  = 1.5)

# add estimated regression function for full sample
abline(noerror_mod)

# estimate model case 1, add regression line
dat <- dat[-id, ]

c2_mod <- lm(dat$Y ~ dat$X, data = dat)
abline(c2_mod, col = "purple")

# add legend
legend("topleft",
       lty = 1,
       bg = "transparent",
       cex = 0.8,
       col = c("darkgreen", "black", "purple"), 
       legend = c("Population", "Full sample", "Obs. with X <= 45"))
# set random seed
set.seed(1)

# simulate data
dat <- data.frame(
  rmvnorm(1000, c(50,100), 
          sigma = cbind(c(10,5), c(5,10))))

colnames(dat) <- c("X","Y")

# mark observations
id <- which(dat$X <= 55 & dat$Y >= 100)

plot(dat$X[-id], 
       dat$Y[-id], 
       col = "gray",
       cex = 0.8,
       pch = 20,
       xlab = "X",
       ylab = "Y")

points(dat$X[id], 
     dat$Y[id], 
     col = "steelblue",
     cex = 0.8,
     pch = 20)

# add population regression function
abline(coef = c(75, 0.5), 
       col = "darkgreen",
       lwd  = 1.5)

# add estimated regression function for full sample
abline(noerror_mod)

# estimate model case 1, add regression line
dat <- dat[id, ]

c3_mod <- lm(dat$Y ~ dat$X, data = dat)
abline(c3_mod, col = "purple")

# add legend
legend("topleft",
       lty = 1,
       bg = "transparent",
       cex = 0.8,
       col = c("darkgreen", "black", "purple"), 
       legend = c("Population", "Full sample", "X <= 55 & Y >= 100"))
cat('
<div class = "keyconcept" id="KC9.6">
<h3 class = "right"> Key Concept 9.6 </h3>
<h3 class = "left"> Simultaneous Causality Bias </h3>
So far we have assumed that the changes in the independent variable $X$ are responsible for changes in the dependent variable $Y$. When the reverse is also true, we say that there is *simultaneous causality* between $X$ and $Y$. This reverse causality leads to correlation between $X$ and the error in the population regression of interest such that the coefficient on $X$ is estimated with bias.
</div>
')
cat('\\begin{keyconcepts}[Simultaneous Causality Bias]{9.6}
So far we have assumed that the changes in the independent variable $X$ are responsible for changes in the dependent variable $Y$. When the reverse is also true, we say that there is \\textit{simultaneous causality} between $X$ and $Y$. This reverse causality leads to correlation between $X$ and the error in the population regression of interest such that the coefficient on $X$ is estimated with a bias.
\\end{keyconcepts}
')
# load the data set
library(AER)
data("CigarettesSW")
c1995 <- subset(CigarettesSW, year == "1995")

# estimate the model
cigcon_mod <- lm(log(packs) ~ log(price), data = c1995)
cigcon_mod
# plot the estimated regression line and the data
plot(log(c1995$price), log(c1995$packs),
     xlab = "ln(Price)",
     ylab = "ln(Consumption)",
     main = "Demand for Cigarettes",
     pch = 20,
     col = "steelblue")

abline(cigcon_mod, 
       col = "darkred", 
       lwd = 1.5)
cat('
<div class = "keyconcept" id="KC9.7">
<h3 class = "right"> Key Concept 9.7 </h3>
<h3 class = "left"> Threats to Internal Validity of a Regression Study </h3>

The five primary threats to internal validity of a multiple regression study are:

1. Omitted variables

2. Misspecification of functional form

3. Errors in variables (measurement errors in the regressors)

4. Sample selection

5. Simultaneous causality

All these threats lead to failure of the first least squares assumption $$E(u_i\\vert X_{1i},\\dots ,X_{ki}) \\neq 0$$ so that the OLS estimator is biased *and* inconsistent. <br>

Furthermore, if one does not adjust for heteroskedasticity *and*/*or* serial correlation, incorrect standard errors may be a threat to internal validity of the study.

</div>
')
cat('\\begin{keyconcepts}[Threats to Internal Validity of a Regression Study ]{9.7}
The five primary threats to internal validity of a multiple regression study are:\\newline

\\begin{enumerate}
\\item Omitted variables
\\item Misspecification of functional form
\\item Errors in variables (measurement errors in the regressors)
\\item Sample selection
\\item Simultaneous causality
\\end{enumerate}\\vspace{0.5cm}

All these threats lead to failure of the first least squares assumption $$E(u_i\\vert X_{1i},\\dots ,X_{ki}) \\neq 0$$ so that the OLS estimator is biased \\textit{and} inconsistent.\\newline

Furthermore, if one does not adjust for heteroskedasticity \\textit{and}/\\textit{or} serial correlation, incorrect standard errors may be a threat to internal validity of the study.
\\end{keyconcepts}
')
linear_model <- lm(score ~ STR, data = CASchools)
linear_model
# attach the 'MASchools' dataset
data("MASchools")
summary(MASchools)
# Customized variables in MASchools
MASchools$score <- MASchools$score4 
MASchools$STR <- MASchools$stratio

# Reproduce Table 9.1 of the book
vars <- c("score", "STR", "english", "lunch", "income")

cbind(CA_mean = sapply(CASchools[, vars], mean),
      CA_sd   = sapply(CASchools[, vars], sd),
      MA_mean = sapply(MASchools[, vars], mean),
      MA_sd   = sapply(MASchools[, vars], sd))
# estimate linear model
Linear_model_MA <- lm(score ~ income, data = MASchools)
Linear_model_MA

# estimate linear-log model
Linearlog_model_MA <- lm(score ~ log(income), data = MASchools) 
Linearlog_model_MA

# estimate Cubic model
cubic_model_MA <- lm(score ~ I(income) + I(income^2) + I(income^3), data = MASchools)
cubic_model_MA
# plot data
plot(MASchools$income, MASchools$score,
     pch = 20,
     col = "steelblue",
     xlab = "District income",
     ylab = "Test score",
     xlim = c(0, 50),
     ylim = c(620, 780))

# add estimated regression line for the linear model
abline(Linear_model_MA, lwd = 2)

# add estimated regression function for Linear-log model
order_id  <- order(MASchools$income)

lines(MASchools$income[order_id],
      fitted(Linearlog_model_MA)[order_id], 
      col = "darkgreen", 
      lwd = 2)

# add estimated cubic regression function
lines(x = MASchools$income[order_id], 
      y = fitted(cubic_model_MA)[order_id],
      col = "orange", 
      lwd = 2) 

# add a legend
legend("topleft",
       legend = c("Linear", "Linear-Log", "Cubic"),
       lty = 1,
       col = c("Black", "darkgreen", "orange"))
## # add 'HiEL' to 'MASchools'
## MASchools$HiEL <- as.numeric(MASchools$english > median(MASchools$english))
## 
## # estimate the model specifications from Table 9.2 of the book
## TestScore_MA_mod1 <- lm(score ~ STR, data = MASchools)
## 
## TestScore_MA_mod2 <- lm(score ~ STR + english + lunch + log(income),
##                         data = MASchools)
## 
## TestScore_MA_mod3 <- lm(score ~ STR + english + lunch + income + I(income^2)
##                         + I(income^3), data = MASchools)
## 
## TestScore_MA_mod4 <- lm(score ~ STR + I(STR^2) + I(STR^3) + english + lunch + income
##                         + I(income^2) + I(income^3), data = MASchools)
## 
## TestScore_MA_mod5 <- lm(score ~ STR + I(income^2) + I(income^3) + HiEL:STR + lunch
##                         + income, data = MASchools)
## 
## TestScore_MA_mod6 <- lm(score ~ STR + I(income^2) + I(income^3) + HiEL + HiEL:STR + lunch
##                         + income, data = MASchools)
## 
## # gather robust standard errors
## rob_se <- list(sqrt(diag(vcovHC(TestScore_MA_mod1, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_MA_mod2, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_MA_mod3, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_MA_mod4, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_MA_mod5, type = "HC1"))),
##                sqrt(diag(vcovHC(TestScore_MA_mod6, type = "HC1"))))
## 
## # generate a table with 'stargazer()'
## library(stargazer)
## 
## stargazer(Linear_model_MA, TestScore_MA_mod2, TestScore_MA_mod3,
##           TestScore_MA_mod4, TestScore_MA_mod5, TestScore_MA_mod6,
##           title = "Regressions Using Massachusetts Test Score Data",
##           type = "latex",
##           digits = 3,
##           header = FALSE,
##           se = rob_se,
##           object.names = TRUE,
##           model.numbers = FALSE,
##           column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)"))
library(stargazer)
MASchools$HiEL <- as.numeric(MASchools$english > median(MASchools$english))
TestScore_MA_mod1 <- lm(score ~ STR, data = MASchools)
TestScore_MA_mod2 <- lm(score ~ STR + english + lunch + log(income), 
                        data = MASchools)
TestScore_MA_mod3 <- lm(score ~ STR + english + lunch + income + I(income^2) 
                        + I(income^3), data = MASchools)
TestScore_MA_mod4 <- lm(score ~ STR + I(STR^2) + I(STR^3) + english + lunch + income + I(income^2) 
                        + I(income^3), data = MASchools)
TestScore_MA_mod5 <- lm(score ~ STR + HiEL + HiEL:STR + lunch + income + I(income^2) 
                        + I(income^3), data = MASchools)
TestScore_MA_mod6 <- lm(score ~ STR + lunch + income + I(income^2) 
                        + I(income^3), data = MASchools)

rob_se <- list(
  sqrt(diag(vcovHC(TestScore_MA_mod1, type="HC1"))),
  sqrt(diag(vcovHC(TestScore_MA_mod2, type="HC1"))),
  sqrt(diag(vcovHC(TestScore_MA_mod3, type="HC1"))),
  sqrt(diag(vcovHC(TestScore_MA_mod4, type="HC1"))),
  sqrt(diag(vcovHC(TestScore_MA_mod5, type="HC1"))),
  sqrt(diag(vcovHC(TestScore_MA_mod6, type="HC1")))
)

stargazer(TestScore_MA_mod1, TestScore_MA_mod2, TestScore_MA_mod3, TestScore_MA_mod4, TestScore_MA_mod5, TestScore_MA_mod6, 
          se = rob_se,
          type = "html",
          header = FALSE,
          model.numbers = FALSE,
          dep.var.caption = "Dependent Variable: Score",
          column.sep.width = "1pt",
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)")
          )

stargazer_html_title("Regressions Using Massachusetts Test Score Data", "rumtsd")
library(stargazer)
MASchools$HiEL <- as.numeric(MASchools$english > median(MASchools$english))
TestScore_MA_mod1 <- lm(score ~ STR, data = MASchools)
TestScore_MA_mod2 <- lm(score ~ STR + english + lunch + log(income), 
                        data = MASchools)
TestScore_MA_mod3 <- lm(score ~ STR + english + lunch + income + I(income^2) 
                        + I(income^3), data = MASchools)
TestScore_MA_mod4 <- lm(score ~ STR + I(STR^2) + I(STR^3) + english + lunch + income + I(income^2) 
                        + I(income^3), data = MASchools)
TestScore_MA_mod5 <- lm(score ~ STR + HiEL + HiEL:STR + lunch + income + I(income^2) 
                        + I(income^3), data = MASchools)
TestScore_MA_mod6 <- lm(score ~ STR + lunch + income + I(income^2) 
                        + I(income^3), data = MASchools)

rob_se <- list(
  sqrt(diag(vcovHC(TestScore_MA_mod1, type="HC1"))),
  sqrt(diag(vcovHC(TestScore_MA_mod2, type="HC1"))),
  sqrt(diag(vcovHC(TestScore_MA_mod3, type="HC1"))),
  sqrt(diag(vcovHC(TestScore_MA_mod4, type="HC1"))),
  sqrt(diag(vcovHC(TestScore_MA_mod5, type="HC1"))),
  sqrt(diag(vcovHC(TestScore_MA_mod6, type="HC1")))
)

stargazer(TestScore_MA_mod1, TestScore_MA_mod2, TestScore_MA_mod3, TestScore_MA_mod4, TestScore_MA_mod5, TestScore_MA_mod6,
          digits = 3,
          title = "\\label{tab:rumtsd} Regressions Using Massachusetts Test Score Data",
          type = "latex",
          float.env = "sidewaystable",
          column.sep.width = "-7pt",
          se = rob_se,
          omit.stat = "f",
          model.numbers = FALSE,
          column.labels = c("(I)", "(II)", "(III)", "(IV)", "(V)", "(VI)")
          )
# F-test model (3)
linearHypothesis(TestScore_MA_mod3, 
                 c("I(income^2)=0", "I(income^3)=0"), 
                 vcov. = vcovHC, type = "HC1")

# F-tests model (4)
linearHypothesis(TestScore_MA_mod4, 
                 c("STR=0", "I(STR^2)=0", "I(STR^3)=0"), 
                 vcov. = vcovHC, type = "HC1")

linearHypothesis(TestScore_MA_mod4, 
                 c("I(STR^2)=0", "I(STR^3)=0"), 
                 vcov. = vcovHC, type = "HC1")

linearHypothesis(TestScore_MA_mod4, 
                 c("I(income^2)=0", "I(income^3)=0"), 
                 vcov. = vcovHC, type = "HC1")

# F-tests model (5)
linearHypothesis(TestScore_MA_mod5, 
                 c("STR=0", "STR:HiEL=0"), 
                 vcov. = vcovHC, type = "HC1")

linearHypothesis(TestScore_MA_mod5, 
                 c("I(income^2)=0", "I(income^3)=0"), 
                 vcov. = vcovHC, type = "HC1")

linearHypothesis(TestScore_MA_mod5, 
                 c("HiEL=0", "STR:HiEL=0"), 
                 vcov. = vcovHC, type = "HC1")

# F-test Model (6)
linearHypothesis(TestScore_MA_mod6, 
                 c("I(income^2)=0", "I(income^3)=0"), 
                 vcov. = vcovHC, type = "HC1")
TestScore_MA_mod3$coefficients[2] / sd(MASchools$score) * (-2)
TestScore_mod2$coefficients[2] / sd(CASchools$score) * (-2)
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 1. Simulation Study: Misspecification of Functional Form  {-}

As stated in Chapter \\@ref(ttivomra), misspecification of the regression function violates assumption 1 of Key Concept 6.3 so that the OLS estimator will be biased and inconsistent. We have illustrated the bias of $\\hat{\\beta}_0$ for the example of the quadratic population regression function
$$Y_i = X_i^2 $$
and the linear model $$Y_i = \\beta_0 + \\beta_1 X_i + u_i, \\, u_i \\sim \\mathcal{N}(0,1)$$ using 100 randomly generated observations. Strictly speaking, this finding could be just a coincidence because we consider just one estimate obtained using a single data set. 

In this exercise, you have to generate simulation evidence for the bias of $\\hat{\\beta}_0$ in the model $$Y_i = \\beta_0 + \\beta_1 X_i + u_i$$ if the population regression function is $$Y_i = X_i^2.$$

**Instructions:**

Make sure to use the definitions suggested in the skeleton code in <tt>script.R</tt> to complete the following tasks:

+ Generate 1000 OLS estimates of $\\beta_0$ in the model above using a <tt>for()</tt> loop where $X_i \\sim \\mathcal{U}[-5,5]$, $u_i \\sim \\mathcal{N}(0,1)$ using samples of size $100$. Save the estimates in <tt>beta_hats</tt>.

+ Compare the sample mean of the estimates to the true parameter using the <tt>==</tt> operator.

<iframe src="DCL/ex9_1.html" frameborder="0" scrolling="no" style="width:100%;height:360px"></iframe>

**Hint:**

You can generate random numbers from a uniform distribution using <tt>runif()</tt>.

</div>') } else {
  cat("\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}")
}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 2. Simulation Study: Errors-in-Variables Bias {-}

Consider again the application of the classical measurement error model introduced in Chapter \\@ref(ttivomra):  

The single regressor $X_i$ is measured with error so that $\\overset{\\sim}{X}_i$ is observed instead. Thus one estimates $\\beta_1$ in
\\begin{align*}
  Y_i =& \\, \\beta_0 + \\beta_1 \\overset{\\sim}{X}_i + \\underbrace{\\beta_1 (X_i -\\overset{\\sim}{X}_i) + u_i}_{=v_i} \\\\
  Y_i =& \\, \\beta_0 + \\beta_1 \\overset{\\sim}{X}_i + v_i
\\end{align*}
instead of $$Y_i = \\beta_0 + \\beta_1 X_i + u_i,$$

with the zero mean error $w_i$ being uncorrelated with $X_i$ and $u_i$. Then $\\beta_1$ is inconsistently estimated by OLS:
\\begin{equation}
  \\widehat{\\beta}_1 \\xrightarrow{p}{\\frac{\\sigma_{X}^2}{\\sigma_{X}^2 + \\sigma_{w}^2}} \\beta_1
\\end{equation}

Let $$(X, Y) \\sim \\mathcal{N}\\left[\\begin{pmatrix}50\\\\ 100\\end{pmatrix},\\begin{pmatrix}10 & 5 \\\\ 5 & 10 \\end{pmatrix}\\right].$$ Recall from \\@ref(eq:bnormexpfn) that $E(Y_i\\vert X_i) = 75 + 0.5 X_i$ in this case. Further Assume that $\\overset{\\sim}{X_i} = X_i + w_i$ with $w_i \\overset{i.i.d}{\\sim} \\mathcal{N}(0,10)$.

As mentioned in Exercise 1, Chapter \\@ref(ttivomra) discusses the consequences of the measurement error for the OLS estimator of $\\beta_1$ in this setting based on a *single* sample and and thus just one estimate. Strictly speaking, the conclusion made could be wrong because the oberseved bias may be due to random variation. A Monto Carlo simulation is more appropriate here.

**Instructions:**

Show that $\\beta_1$ is estimated with a bias using a simulation study. Make sure to use the definitions suggested in the skeleton code in <tt>script.R</tt> to complete the following tasks:

+ Generate 1000 estimates of $\\beta_1$ in the simple regression model $$Y_i = \\beta_0 + \\beta_1 X_i + u_i.$$ Use <tt>rmvnorm()</tt> to generate samples of 100 random observations from the bivariate normal distribution stated above.

+ Save the estimates in <tt>beta_hats</tt>.

+ Compute the sample mean of the estimates.

<iframe src="DCL/ex9_2.html" frameborder="0" scrolling="no" style="width:100%;height:400px"></iframe>

</div>')}
