
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
library(AER)
library(stargazer)
cat('
<div class = "keyconcept" id="KC11.1">
<h3 class = "right"> Key Concept 11.1 </h3>
<h3 class = "left"> The Linear Probability Model </h3>

The linear regression model 

$$Y_i = \\beta_0 + \\beta_1 + X_{1i} + \\beta_2 X_{2i} + \\dots + \\beta_k X_{ki} + u_i$$
with a binary dependent variable $Y_i$ is called the linear probability model. In the linear probability model we have $$E(Y\\vert X_1,X_2,\\dots,X_k) = P(Y=1\\vert X_1, X_2,\\dots, X_3)$$ where $$ P(Y = 1 \\vert X_1, X_2, \\dots, X_k) = \\beta_0 + \\beta_1 + X_{1i} + \\beta_2 X_{2i} + \\dots + \\beta_k X_{ki}.$$

Thus, $\\beta_j$ can be interpreted as the change in the probability that $Y_i=1$, holding constant the other $k-1$ regressors. Just as in common multiple regression, the $\\beta_j$ can be estimated using OLS and the robust standard error formulas can be used for hypothesis testing and computation of confidence intervals. 

In most linear probability models, $R^2$ has no meaningful interpretation since the regression line can never fit the data perfectly if the dependent variable is binary and the regressors are continuous. This can be seen in the application below.

It is *essential* to use robust standard errors since the $u_i$ in a linear probability model are always heteroskedastic.

Linear probability models are easily estimated in <tt>R</tt> using the function <tt>lm()</tt>.

</div>
')
cat('\\begin{keyconcepts}[The Linear Probability Model]{11.1}
The linear regression model 
$$Y_i = \\beta_0 + \\beta_1 + X_{1i} + \\beta_2 X_{2i} + \\dots + \\beta_k X_{ki} + u_i$$
with a binary dependent variable $Y_i$ is called the linear probability model. In the linear probability model we have $$E(Y\\vert X_1,X_2,\\dots,X_k) = P(Y=1\\vert X_1, X_2,\\dots, X_3)$$ where $$ P(Y = 1 \\vert X_1, X_2, \\dots, X_k) = \\beta_0 + \\beta_1 + X_{1i} + \\beta_2 X_{2i} + \\dots + \\beta_k X_{ki}.$$

Thus, $\\beta_j$ can be interpreted as the change in the probability that $Y_i=1$, holding constant the other $k-1$ regressors. Just as in common multiple regression, the $\\beta_j$ can be estimated using OLS and the robust standard error formulas can be used for hypothesis testing and computation of confidence intervals.\\newline 

In most linear probability models, $R^2$ has no meaningful interpretation since the regression line can never fit the data perfectly if the dependent variable is binary and the regressors are continuous. This can be seen in the application below.\\newline

It is \\textit{essential} to use robust standard errors since the $u_i$ in a linear probability model are always heteroskedastic.\\newline

Linear probability models are easily estimated in \\texttt{R} using the function \\texttt{lm()}.
\\end{keyconcepts}
')
# load `AER` package and attach the HMDA data
library(AER)
data(HMDA)
# inspect the data
head(HMDA)
summary(HMDA)
# convert 'deny' to numeric
HMDA$deny <- as.numeric(HMDA$deny) - 1

# estimate a simple linear probabilty model
denymod1 <- lm(deny ~ pirat, data = HMDA)
denymod1
# plot the data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Scatterplot Mortgage Application Denial and the Payment-to-Income Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add the estimated regression line
abline(denymod1, 
       lwd = 1.8, 
       col = "steelblue")
# print robust coefficient summary
coeftest(denymod1, vcov. = vcovHC, type = "HC1")
# rename the variable 'afam' for consistency
colnames(HMDA)[colnames(HMDA) == "afam"] <- "black"

# estimate the model
denymod2 <- lm(deny ~ pirat + black, data = HMDA)
coeftest(denymod2, vcov. = vcovHC)
cat('
<div class = "keyconcept" id="KC11.2">
<h3 class = "right"> Key Concept 11.2 </h3>
<h3 class = "left"> Probit Model, Predicted Probabilities and Estimated Effects</h3>

Assume that $Y$ is a binary variable. The model

$$ Y= \\beta_0 + \\beta_1 + X_1 + \\beta_2 X_2 + \\dots + \\beta_k X_k + u $$
with
$$P(Y = 1 \\vert X_1, X_2, \\dots ,X_k) = \\Phi(\\beta_0 + \\beta_1 + X_1 + \\beta_2 X_2 + \\dots + \\beta_k X_k)$$
is the population Probit model with multiple regressors $X_1, X_2, \\dots, X_k$ and $\\Phi(\\cdot)$ is the cumulative standard normal distribution function.

The predicted probability that $Y=1$ given $X_1, X_2, \\dots, X_k$ can be calculated in two steps:

1. Compute $z = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\dots + \\beta_k X_k$

2. Look up $\\Phi(z)$ by calling <tt>pnorm()</tt>.

$\\beta_j$ is the effect on $z$ of a one unit change in regressor $X_j$, holding constant all other $k-1$ regressors.

The effect on the predicted probability of a change in a regressor can be computed as in Key Concept 8.1.

In <tt>R</tt>, Probit models can be estimated using the function <tt>glm()</tt> from the package <tt>stats</tt>. Using the argument <tt>family</tt> we specify that we want to use a Probit link function.

</div>
')
cat('\\begin{keyconcepts}[Probit Model, Predicted Probabilities and Estimated Effects]{11.2}
Assume that $Y$ is a binary variable. The model

$$ Y= \\beta_0 + \\beta_1 + X_1 + \\beta_2 X_2 + \\dots + \\beta_k X_k + u $$
with
$$P(Y = 1 \\vert X_1, X_2, \\dots ,X_k) = \\Phi(\\beta_0 + \\beta_1 + X_1 + \\beta_2 X_2 + \\dots + \\beta_k X_k)$$
is the population Probit model with multiple regressors $X_1, X_2, \\dots, X_k$ and $\\Phi(\\cdot)$ is the cumulative standard normal distribution function.\\newline

The predicted probability that $Y=1$ given $X_1, X_2, \\dots, X_k$ can be calculated in two steps:\\newline

\\begin{enumerate}
\\item Compute $z = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\dots + \\beta_k X_k$
\\item Look up $\\Phi(z)$ by calling \\texttt{pnorm()}.
\\end{enumerate}\\vspace{0.5cm}

$\\beta_j$ is the effect on $z$ of a one unit change in regressor $X_j$, holding constant all other $k-1$ regressors.\\newline

The effect on the predicted probability of a change in a regressor can be computed as in Key Concept 8.1.\\newline

In \\texttt{R}, Probit models can be estimated using the function \\texttt{glm()} from the package \\texttt{stats}. Using the argument \\texttt{family} we specify that we want to use a Probit link function.
\\end{keyconcepts}
')
# estimate the simple probit model
denyprobit <- glm(deny ~ pirat, 
                  family = binomial(link = "probit"), 
                  data = HMDA)

coeftest(denyprobit, vcov. = vcovHC, type = "HC1")
# plot data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Probit Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.85)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line
x <- seq(0, 3, 0.01)
y <- predict(denyprobit, list(pirat = x), type = "response")

lines(x, y, lwd = 1.5, col = "steelblue")
# 1. compute predictions for P/I ratio = 0.3, 0.4
predictions <- predict(denyprobit, 
                       newdata = data.frame("pirat" = c(0.3, 0.4)),
                       type = "response")

# 2. Compute difference in probabilities
diff(predictions)
denyprobit2 <- glm(deny ~ pirat + black, 
                   family = binomial(link = "probit"), 
                   data = HMDA)

coeftest(denyprobit2, vcov. = vcovHC, type = "HC1")
# 1. compute predictions for P/I ratio = 0.3
predictions <- predict(denyprobit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.3, 0.3)),
                       type = "response")

# 2. compute difference in probabilities
diff(predictions)
cat('
<div class = "keyconcept" id="KC11.3">
<h3 class = "right"> Key Concept 11.3 </h3>
<h3 class = "left"> Logit Regression </h3>

The population Logit regression function is

\\begin{align*}
  P(Y=1\\vert X_1, X_2, \\dots, X_k) =& \\, F(\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\dots + \\beta_k X_k) \\\\
  =& \\, \\frac{1}{1+e^{-(\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\dots + \\beta_k X_k)}}.
\\end{align*}

The idea is similar to Probit regression except that a different CDF is used: $$F(x) = \\frac{1}{1+e^{-x}}$$ is the CDF of a standard logistically distributed random variable.

</div>
')
cat('\\begin{keyconcepts}[Logit Regression]{11.3}
The population Logit regression function is

\\begin{align*}
  P(Y=1\\vert X_1, X_2, \\dots, X_k) =& \\, F(\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\dots + \\beta_k X_k) \\\\
  =& \\, \\frac{1}{1+e^{-(\\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\dots + \\beta_k X_k)}}.
\\end{align*}

The idea is similar to Probit regression except that a different CDF is used: $$F(x) = \\frac{1}{1+e^{-x}}$$ is the CDF of a standard logistically distributed random variable.
\\end{keyconcepts}
')
denylogit <- glm(deny ~ pirat, 
                 family = binomial(link = "logit"), 
                 data = HMDA)

coeftest(denylogit, vcov. = vcovHC, type = "HC1")
# plot data
plot(x = HMDA$pirat, 
     y = HMDA$deny,
     main = "Probit and Logit Models Model of the Probability of Denial, Given P/I Ratio",
     xlab = "P/I ratio",
     ylab = "Deny",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.9)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line of Probit and Logit models
x <- seq(0, 3, 0.01)
y_probit <- predict(denyprobit, list(pirat = x), type = "response")
y_logit <- predict(denylogit, list(pirat = x), type = "response")

lines(x, y_probit, lwd = 1.5, col = "steelblue")
lines(x, y_logit, lwd = 1.5, col = "black", lty = 2)

# add a legend
legend("topleft",
       horiz = TRUE,
       legend = c("Probit", "Logit"),
       col = c("steelblue", "black"), 
       lty = c(1, 2))
# estimate a Logit regression with multiple regressors
denylogit2 <- glm(deny ~ pirat + black, 
                  family = binomial(link = "logit"), 
                  data = HMDA)

coeftest(denylogit2, vcov. = vcovHC, type = "HC1")
# 1. compute predictions for P/I ratio = 0.3
predictions <- predict(denylogit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "pirat" = c(0.3, 0.3)),
                       type = "response")

predictions

# 2. Compute difference in probabilities
diff(predictions)
# compute pseudo-R2 for the probit model of mortgage denial
pseudoR2 <- 1 - (denyprobit2$deviance) / (denyprobit2$null.deviance)
pseudoR2
# compute the null model
denyprobit_null <- glm(formula = deny ~ 1, 
                       family = binomial(link = "probit"), 
                       data = HMDA)

# compute the pseudo-R2 using 'logLik'
1 - logLik(denyprobit2)[1]/logLik(denyprobit_null)[1]
# Mean P/I ratio
mean(HMDA$pirat)

# inhouse expense-to-total-income ratio
mean(HMDA$hirat)

# loan-to-value ratio
mean(HMDA$lvrat)

# consumer credit score
mean(as.numeric(HMDA$chist))

# mortgage credit score
mean(as.numeric(HMDA$mhist))

# public bad credit record
mean(as.numeric(HMDA$phist)-1)

# denied mortgage insurance
prop.table(table(HMDA$insurance))

# self-employed
prop.table(table(HMDA$selfemp))

# single
prop.table(table(HMDA$single))

# high school diploma
prop.table(table(HMDA$hschool))

# unemployment rate
mean(HMDA$unemp)

# condominium
prop.table(table(HMDA$condomin))

# black
prop.table(table(HMDA$black))

# deny
prop.table(table(HMDA$deny))
# define low, medium and high loan-to-value ratio
HMDA$lvrat <- factor(
  ifelse(HMDA$lvrat < 0.8, "low",
  ifelse(HMDA$lvrat >= 0.8 & HMDA$lvrat <= 0.95, "medium", "high")),
  levels = c("low", "medium", "high"))

# convert credit scores to numeric
HMDA$mhist <- as.numeric(HMDA$mhist)
HMDA$chist <- as.numeric(HMDA$chist)
# estimate all 6 models for the denial probability
lpm_HMDA <- lm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
               + insurance + selfemp, data = HMDA)

logit_HMDA <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                  + insurance + selfemp, 
                  family = binomial(link = "logit"), 
                  data = HMDA)

probit_HMDA_1 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                     + insurance + selfemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_2 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist + phist 
                     + insurance + selfemp + single + hschool + unemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_3 <- glm(deny ~ black + pirat + hirat + lvrat + chist + mhist 
                     + phist + insurance + selfemp + single + hschool + unemp + condomin 
                     + I(mhist==3) + I(mhist==4) + I(chist==3) + I(chist==4) + I(chist==5) 
                     + I(chist==6), 
                     family = binomial(link = "probit"), 
                     data = HMDA)

probit_HMDA_4 <- glm(deny ~ black * (pirat + hirat) + lvrat + chist + mhist + phist 
                     + insurance + selfemp + single + hschool + unemp, 
                     family = binomial(link = "probit"), 
                     data = HMDA)
## rob_se <- list(sqrt(diag(vcovHC(lpm_HMDA, type = "HC1"))),
##                sqrt(diag(vcovHC(logit_HMDA, type = "HC1"))),
##                sqrt(diag(vcovHC(probit_HMDA_1, type = "HC1"))),
##                sqrt(diag(vcovHC(probit_HMDA_2, type = "HC1"))),
##                sqrt(diag(vcovHC(probit_HMDA_3, type = "HC1"))),
##                sqrt(diag(vcovHC(probit_HMDA_4, type = "HC1"))))
## 
## stargazer(lpm_HMDA, logit_HMDA, probit_HMDA_1,
##           probit_HMDA_2, probit_HMDA_3, probit_HMDA_4,
##           digits = 3,
##           type = "latex",
##           header = FALSE,
##           se = rob_se,
##           model.numbers = FALSE,
##           column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"))
library(stargazer)

rob_se <- list(
  sqrt(diag(vcovHC(lpm_HMDA, type = "HC1"))),
  sqrt(diag(vcovHC(logit_HMDA, type = "HC1"))),
  sqrt(diag(vcovHC(probit_HMDA_1, type = "HC1"))),
  sqrt(diag(vcovHC(probit_HMDA_2, type = "HC1"))),
  sqrt(diag(vcovHC(probit_HMDA_3, type = "HC1"))),
  sqrt(diag(vcovHC(probit_HMDA_4, type = "HC1")))
)

stargazer(lpm_HMDA, logit_HMDA, probit_HMDA_1, probit_HMDA_2, probit_HMDA_3, probit_HMDA_4, 
          digits = 3,
          type = "html", 
          se = rob_se,
          header = FALSE,          
          dep.var.caption = "Dependent Variable: Mortgage Application Denial",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)")
          )

stargazer_html_title("HMDA Data: LPM, Probit and Logit Models", "hmdad")
library(stargazer)

rob_se <- list(
  sqrt(diag(vcovHC(lpm_HMDA, type = "HC1"))),
  sqrt(diag(vcovHC(logit_HMDA, type = "HC1"))),
  sqrt(diag(vcovHC(probit_HMDA_1, type = "HC1"))),
  sqrt(diag(vcovHC(probit_HMDA_2, type = "HC1"))),
  sqrt(diag(vcovHC(probit_HMDA_3, type = "HC1"))),
  sqrt(diag(vcovHC(probit_HMDA_4, type = "HC1")))
)

stargazer(lpm_HMDA, logit_HMDA, probit_HMDA_1, probit_HMDA_2, probit_HMDA_3, probit_HMDA_4, 
          title = "\\label{tab:hmdad} HMDA Data: LPM, Probit and Logit Models",
          digits = 3,
          type = "latex",
          float.env = "sidewaystable",
          column.sep.width = "-5pt",
          no.space = T,
          single.row = T,
          header = FALSE,
          se = rob_se,
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)")
          )
# comppute regressor values for an average black person
new <- data.frame(
  "pirat" = mean(HMDA$pirat),
  "hirat" = mean(HMDA$hirat),
  "lvrat" = "low",
  "chist" = mean(HMDA$chist),
  "mhist" = mean(HMDA$mhist),
  "phist" = "no",
  "insurance" = "no",
  "selfemp" = "no",
  "black" = c("no", "yes"),
  "single" = "no",
  "hschool" = "yes",
  "unemp" = mean(HMDA$unemp),
  "condomin" = "no")

# differnce predicted by the LPM
predictions <- predict(lpm_HMDA, newdata = new)
diff(predictions)

# differnce predicted by the logit model
predictions <- predict(logit_HMDA, newdata = new, type = "response")
diff(predictions)

# difference predicted by probit model (3)
predictions <- predict(probit_HMDA_1, newdata = new, type = "response")
diff(predictions)

# difference predicted by probit model (4)
predictions <- predict(probit_HMDA_2, newdata = new, type = "response")
diff(predictions)

# difference predicted by probit model (5)
predictions <- predict(probit_HMDA_3, newdata = new, type = "response")
diff(predictions)

# difference predicted by probit model (6)
predictions <- predict(probit_HMDA_4, newdata = new, type = "response")
diff(predictions)
linearHypothesis(probit_HMDA_4,
                 test = "F",
                 c("blackyes:pirat=0", "blackyes:hirat=0"),
                 vcov = vcovHC, type = "HC1")
linearHypothesis(probit_HMDA_4,
                 test = "F",
                 c("blackyes=0", "blackyes:pirat=0", "blackyes:hirat=0"),
                 vcov = vcovHC, type = "HC1")
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 1. Titanic Survival Data {-}

Chapter \\@ref(palr) presented three approaches to model the conditional expectation function of a binary dependent variable: the linear probability model as well as Probit and Logit regression.

The exercises in this Chapter use data on the fate of the passengers of the ocean linear *Titanic*. We aim to explain survival, a binary variable, by socioeconomic variables using the above approaches. 

In this exercise we start with the aggregated data set <tt>Titanic</tt>. It is part of the package <tt>datasets</tt> which is part of base <tt>R</tt>. The following quote from the [description](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/Titanic.html) of the dataset motivates the attempt to predict the *probability* of survival:

*The sinking of the Titanic is a famous event, and new books are still being published about it. Many well-known facts — from the proportions of first-class passengers to the ‘women and children first’ policy, and the fact that that policy was not entirely successful in saving the women and children in the third class — are reflected in the survival rates for various classes of passenger.*

**Instructions:**

+ Assign the <tt>Titanic</tt> data to <tt>Titanic_1</tt> and get an overview.

+ Visualize the conditional survival rates for travel class (<tt>Class</tt>), gender (<tt>Sex</tt>) and age (<tt>Age</tt>) using <tt>mosaicplot()</tt>.

<iframe src="DCL/ex11_1.html" frameborder="0" scrolling="no" style="width:100%;height:360px"></iframe>

</div>') } else {
  cat("\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}")
}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 2. Titanic Survival Data --- Ctd. {-}

The <tt>Titanic</tt> data set from Exercise 1 is not useful for regression analysis because it is highly aggregated. In this exercise you will work with <tt>titanic.csv</tt> which is available under the URL https://stanford.io/2O9RUCF.

The columns of <tt>titanic.csv</tt> contain the following variables:

  <tt>Survived</tt> --- The survived indicator
    
  <tt>Pclass</tt> --- passenger class
    
  <tt>Name</tt> --- passenger\'s Name
    
  <tt>Sex</tt> --- passenger\'s gender
    
  <tt>Age</tt> --- passengers\'s age
    
  <tt>Siblings</tt> --- number of siblings aboard
    
  <tt>Parents.Children.Aboard</tt> --- number of parents and children aboard
    
  <tt>fare</tt> --- the fare paid in british pound

**Instructions:**

+ Import the data from <tt>titanic.csv</tt> using the function <tt>read.csv2()</tt>. Save it to <tt>Titanic_2</tt>.

+ Assign the following column names to <tt>Titanic_2</tt>: 
  
    <tt>Survived, Class, Name, Sex, Age, Siblings, Parents</tt> and <tt>Fare</tt>.

+ Get an overview over the data set. Drop the column <tt>Name</tt>.

+ Attach the packages <tt>corrplot</tt> and <tt>dplyr</tt>. Check whether there is multicollinearity in the data using <tt>corrplot()</tt>.

<iframe src="DCL/ex11_2.html" frameborder="0" scrolling="no" style="width:100%;height:560px"></iframe>

**Hints:**

+ <tt>read_csv()</tt> guesses the column specification as well as the seperators used in the <tt>.csv</tt> file. You should always check if the result is correct. 

+ You may use <tt>select_if()</tt> from the <tt>dplyr</tt> package to select all numeric columns from the data set.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 3. Titanic Survival Data --- Survival Rates {-}

Contingency tables similar to those provided by the data set <tt>Titanic</tt> from Exercise 1 may shed some light on the distribution of survival conditional and possible determinants thereof, e.g., the passenger class. Contingency tables are easily created using the base <tt>R</tt> function <tt>table</tt>. 

**Instructions:**

+ Generate a contingency table for <tt>Survived</tt> and <tt>Class</tt> using <tt>table()</tt>. Save the table to <tt>t_abs</tt>.

+ <tt>t_abs</tt> reports absolute frequencies. Transform <tt>t_abs</tt> into a table which reports relative frequencies (relative to the total number of observations). Save the result to <tt>t_rel</tt>.

+ Visualize the relative frequencies in <tt>t_rel</tt> using <tt>barplot()</tt>. Use different colors for better distinquishablitly among survival and non-survival rate (it does not matter which colors you use).

<iframe src="DCL/ex11_4_3.html" frameborder="0" scrolling="no" style="width:100%;height:320px"></iframe>

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 4. Titanic Survival Data --- Conditional Distributions of <tt>Age</tt> {-}

Contingency tables are useful for summarizing distribution of categorical variables like <tt>Survived</tt> and <tt>Class</tt> in Exercise 3. They are, however, not useful when the variable of interest takes on many different integers (and they are even impossible to generate when the variable is continuous).  

In this exercise you are asked to generate and visualize density estimates of the distribution of <tt>Age</tt> conditional on <tt>Survived</tt> to see whether there are indications how age relates to the chance of survival (despite that the data set reports integers, we treat <tt>Age</tt> as a continuous variable here). For example, it is interesting to see if the \'women and children first\' policy was effective.

The data set <tt>Titanic_2</tt> from the previous exercises is available in your working environment.

**Instructions:**

+ Obtain kernel density estimates of the distributions of <tt>Age</tt> for both the survivors and the deceased. 

+ Save the results to <tt>dens_age_surv</tt> (survived) and <tt>dens_age_died</tt> (died).

+ Plot both kernel density estimates (overlay them in a single plot!). Use different colors of your choive to make the estimates distinguishable.

<iframe src="DCL/ex11_5_4.html" frameborder="0" scrolling="no" style="width:100%;height:330px"></iframe>

**Hints:**

+ Kernel density estimates can be obtained using the functon <tt>density()</tt>.

+ Use <tt>plot()</tt> and <tt>lines()</tt> to plot the density estimates.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 5. Titanic Survival Data --- A Linear Probability Model for <tt>Survival</tt> I {-}

How do socio-economic characteristics of the passengers impact the probability of survival? In particular, are there systematic differences between the three passenger classes? Do the data reflect the \'children and women first\' policy? 

It is natural to start the analysis by estimating a simple linear probability model like (LMP) $$Survived_i = \\beta_0 + \\beta_1 Class2_i + \\beta_2 Class3_i + u_i$$ with dummy variables $Class2_i$ and $Class3_i$.

The data set <tt>Titanic_2</tt> from the previous exercises is available in your working environment.

**Instructions:**

+ Attach the <tt>AER</tt> package.

+ <tt>Class</tt> is of type <tt>int</tt> (integer), Convert <tt>Class</tt> to a factor variable.

+ Estimate the linear probability model and save the result to <tt>surv_mod</tt>.

+ Obtain a robust summary of the model coefficients.

+ Use <tt>surv_mod</tt> to predict the probability of survival for the three passenger classes.

<iframe src="DCL/ex11_lpm.html" frameborder="0" scrolling="no" style="width:100%;height:320px"></iframe>

**Hints:**

+ Linear probability models can be estimated using <tt>lm()</tt>.

+ Use <tt>predict()</tt> to obtain the predictions. Remember that a <tt>data.frame</tt> must be provided to the argument <tt>newdata</tt>.

</div>')}
if (my_output=="html") {
  cat('

<div  class = "DCexercise">

#### 6. Titanic Survival Data --- A Linear Probability Model for <tt>Survival</tt> II {-}

Consider again the outcome from Exercise 5:

$$\\widehat{Survived}_i = \\underset{(0.03)}{0.63} - \\underset{(0.05)}{0.16} Class2_i - \\underset{(0.04)}{0.39} Class3_i + u_i $$

(The estimated coefficients in this model are related to the class specific sample means of <tt>Survived</tt>. You are asked to compute them below.)

The highly significant coefficients indicate that the probability of survival decreases with the passenger class, that is, passengers from a less luxurious class are less likely to survive. 

This result could be affected by omitted variable bias arising from correlation of the passenger class with determinants of the probability of survival not included in the model. We therefore augment the model such that it includes all remaining variables as regressors.

The data set <tt>Titanic_2</tt> as well as the model <tt>surv_mod</tt> from the previous exercises are available in your working environment. The <tt>AER</tt> package is attached.

**Instructions:**

+ Use the model object <tt>surv_mod</tt> to obtain the class specific estimates for the probability of survival. Store them in <tt>surv_prob_c1</tt>, <tt>surv_prob_c2</tt> and <tt>surv_prob_c3</tt>.

+ Fit the augmented LMP and assign the result to the object <tt>LPM_mod</tt>.

+ Obtain a robust summary of the model coefficients.

<iframe src="DCL/ex11_lpm2.html" frameborder="0" scrolling="no" style="width:100%;height:320px"></iframe>

**Hint:**

+ Remember that the formula <tt>a ~ .</tt> specifies a regression of <tt>a</tt> on all other variables in the data set provided as the argument <tt>data</tt> in <tt>glm()</tt>.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 7. Titanic Survival Data --- Logistic Regression {-}

Chapter \\@ref(palr) introduces Logistic regression, also called Logit regression, which is a more suitable than the LPM for modelling the conditional probability function of a dichotomous outcome variable. Logit regression uses a nonlinear link function that restricts the fitted values to lie between $0$ and $1$: in Logit regression, the *log-odds* of the outcome are modeled as a linear combination of the predictors while the LPM assumes that the conditional probability function of outcome is linear.

The data set <tt>Titanic_2</tt> from Exercise 2 is available in your working environment. The package <tt>AER</tt> is attached.

**Instructions:**

+ Use <tt>glm()</tt> to estimate the model
\\begin{align*}
\\log\\left(\\frac{P(survived_i = 1)}{1-P(survived_i = 1)}\\right) =& \\, \\beta_0 + \\beta_1 Class2_i + \\beta_2 Ckass3_i + \\beta_3 Sex_i \\\\ +& \\, \\beta_4 Age_i + \\beta_5 Siblings_i + \\beta_6 Perents_i + \\beta_7 Fare_i + u_i.
\\end{align*}

+ Obtain a robust summary of the model coefficients.

+ The data frame <tt>passengers</tt> contains data on three hypothetical male passengers that differ only in their passenger class (the other variables are set to the respective sample average). Use <tt>Logit_mod</tt> to predict the probability of survival for these passengers.

<iframe src="DCL/ex11_3_6.html" frameborder="0" scrolling="no" style="width:100%;height:430px"></iframe>

**Hints:**

+ Remember that the formula <tt>a ~ .</tt> specifies a regression of <tt>a</tt> on all other variables in the data set provided as the argument <tt>data</tt> in <tt>glm()</tt>.

+ You need to specify the correct type of prediction in <tt>predict()</tt>.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 8. Titanic Survival Data --- Probit Regression {-}

Repeat Exercise 7 but this time estimate the Probit model
\\begin{align*}
P(Survived_i = 1\\vert Class2_i, Class3_i, \\dots, Fare_i) =& \\, \\Phi (\\beta_0 + \\beta_1 Class2_i + \\beta_2 Class3_i + \\beta_3 Sex_i \\\\ +& \\, \\beta_4 Age_i + \\beta_5 Siblings_i + \\beta_6 Parents_i + \\beta_7 Fare_i + u_i).
\\end{align*}

The data set <tt>Titanic_2</tt> from the previous exercises as well as the Logit model <tt>Logit_mod</tt> are available in your working environment. The package <tt>AER</tt> is attached.

**Instructions:**

+ Use <tt>glm()</tt> to estimate the above Probit model. Save the result to <tt>Probit_mod</tt>.

+ Obtain a robust summary of the model coefficients.

+ The data frame <tt>passengers</tt> contains data on three hypothetical male passengers that differ only in their passenger class (the other variables are set to the respective sample average). Use <tt>Probit_mod</tt> to predict the probability of survival for these passengers.

<iframe src="DCL/ex11_8.html" frameborder="0" scrolling="no" style="width:100%;height:430px"></iframe>

**Hints:**

+ Remember that the formula <tt>a ~ .</tt> specifies a regression of <tt>a</tt> on all other variables in the data set provided as the argument <tt>data</tt> in <tt>glm()</tt>.

+ You need to specify the correct type of prediction in <tt>predict()</tt>.

</div>')}
