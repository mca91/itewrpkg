
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
library(quantmod)
library(dynlm)
library(orcutt)
library(nlme)
library(stargazer)
# load the frozen orange juice data set
data("FrozenJuice")

# compute the price index for frozen concentrated juice
FOJCPI <- FrozenJuice[, "price"]/FrozenJuice[, "ppi"]
FOJC_pctc <- 100 * diff(log(FOJCPI))
FDD <- FrozenJuice[, "fdd"]
# convert series to xts objects
FOJCPI_xts <- as.xts(FOJCPI)
FDD_xts <- as.xts(FrozenJuice[, 3])

# Plot orange juice price index
plot(as.zoo(FOJCPI),
     col = "steelblue", 
     lwd = 2,
     xlab = "Date",
     ylab = "Price index", 
     main = "Frozen Concentrated Orange Juice")
# divide plotting area
par(mfrow = c(2, 1))

# Plot percentage changes in prices
plot(as.zoo(FOJC_pctc),
     col = "steelblue", 
     lwd = 2,
     xlab = "Date",
     ylab = "Percent",
     main = "Monthly Changes in the Price of Frozen Conentrated Orange Juice")

# plot freezing degree days
plot(as.zoo(FDD),
     col = "steelblue", 
     lwd = 2,
     xlab = "Date",
     ylab = "Freezing degree days",
     main = "Monthly Freezing Degree Days in Orlando, FL")
# simple regression of percentage changes on freezing degree days
orange_SR <- dynlm(FOJC_pctc ~ FDD)
coeftest(orange_SR, vcov. = vcovHAC)
# distributed lag model with 6 lags of freezing degree days
orange_DLM <- dynlm(FOJC_pctc ~ FDD + L(FDD, 1:6))
coeftest(orange_DLM, vcov. = vcovHAC)
cat('
<div class = "keyconcept" id="KC15.1">
<h3 class = "right"> Key Concept 15.1 </h3>
<h3 class = "left"> The Distributed Lag Model and Exogeneity </h3>
<p>
The general distributed lag model is
\\begin{align}
  Y_t = \\beta_0 + \\beta_1 X_t + \\beta_2 X_{t-1} + \\beta_3 X_{t-2} + \\dots + \\beta_{r+1} X_{t-r} + u_t, (\\#eq:dlm)
\\end{align}
where it is assumed that

1. $X$ is an exogenous variable, $$E(u_t\\vert X_t, X_{t-1}, X_{t-2},\\dots) = 0.$$

2.
     + a $X_t,Y_t$ have a stationary distribution.
     + b $(Y_t,X_t)$ and $(Y_{t-j},X_{t-j})$ become independently distributed as $j$ gets large.

3. Large outliers are unlikely. In particular, we need that all the variables have more than eight nonzero and finite moments --- a stronger assumption than before (four finite nonzero moments) that is required for computation of the HAC covariance matrix estimator.

4. There is no perfect multicollinearity.

 The distributed lag model may be extended to include contemporaneous and past values of additional regressors.

**On the assumption of exogeneity**

+ There is another form of exogeneity termed *strict exogeneity* which assumes $$E(u_t\\vert \\dots, X_{t+2},X_{t+1},X_t,X_{t-1},X_{t-2},\\dots)=0,$$ that is the error term has mean zero conditional on past, present and future values of $X$. Strict exogeneity implies exogeneity (as defined in 1. above) but not the other way around. From this point on we will therefore distinguish between exogeneity and strict exogeneity. 

+ Exogeneity as in 1. suffices for the OLS estimators of the coefficient in distributed lag models to be consistent. However, if the the assumption of strict exogeneity is valid, more efficient estimators can be applied.
</p>
</div>
')
cat('\\begin{keyconcepts}[The Distributed Lag Model and Exogeneity]{15.1}
The general distributed lag model is
\\begin{align}
  Y_t = \\beta_0 + \\beta_1 X_t + \\beta_2 X_{t-1} + \\beta_3 X_{t-2} + \\dots + \\beta_{r+1} X_{t-r} + u_t, \\label{eq:dlm}
\\end{align}
where it is assumed that \\newline

\\begin{enumerate}
\\item $X$ is an exogenous variable, $$E(u_t\\vert X_t, X_{t-1}, X_{t-2},\\dots) = 0.$$
\\item \\vspace{0.5cm}
\\begin{itemize}
\\item[(a)] $X_t,Y_t$ have a stationary distribution.
\\item[(b)] $(Y_t,X_t)$ and $(Y_{t-j},X_{t-j})$ become independently distributed as $j$ gets large.
\\end{itemize}\\vspace{0.5cm}
\\item Large outliers are unlikely. In particular, we need that all the variables have more than eight finite moments --- a stronger assumption than before (four finite moments) that is required for computation of the HAC covariance matrix estimator.
\\item There is no perfect multicollinearity.\\vspace{0.5cm}
\\end{enumerate}\\vspace{0.5cm}

The distributed lag model may be extended to include contemporaneous and past values of additional regressors.\\vspace{0.5cm}

\\textbf{On the assumption of exogeneity}\\vspace{0.5cm}

\\begin{itemize}
\\item There is another form of exogeneity termed \\textit{strict exogeneity} which assumes $$E(u_t\\vert \\dots, X_{t+2},X_{t+1},X_t,X_{t-1},X_{t-2},\\dots)=0,$$ that is the error term has mean zero conditional on past, present and future values of $X$. Strict exogeneity implies exogeneity (as defined in 1. above) but not the other way around. From this point on we will therefore distinguish between exogeneity and strict exogeneity.\\vspace{0.5cm} 
\\item Exogeneity as in 1. suffices for the OLS estimators of the coefficient in distributed lag models to be consistent. However, if the the assumption of strict exogeneity is valid, more efficient estimators can be applied.
\\end{itemize}
\\end{keyconcepts}
')
# compute cumulative multipliers
cum_mult <-cumsum(orange_DLM$coefficients[-1])

# rename entries
names(cum_mult) <- paste(0:6, sep = "-", "period CDM")

cum_mult
# estimate cumulative dynamic multipliers using the modified regression
cum_mult_reg <-dynlm(FOJC_pctc ~ d(FDD) + d(L(FDD,1:5)) + L(FDD,6))
coef(cum_mult_reg)[-1]
# obtain coefficient summary that reports HAC standard errors
coeftest(cum_mult_reg, vcov. = vcovHAC)
cat('
<div class = "keyconcept" id="KC15.2">
<h3 class = "right"> Key Concept 15.2 </h3>
<h3 class = "left"> HAC Standard errors </h3>
<p>
**Problem**:

If the error term $u_t$ in the distributed lag model \\@ref(eq:dlm) is serially correlated, statistical inference that rests on usual (heteroskedasticity-robust) standard errors can be strongly misleading.

**Solution**:

Heteroskedasticity- and autocorrelation-consistent (HAC) estimators of the variance-covariance matrix circumvent this issue. There are <tt>R</tt> functions like <tt>vcovHAC()</tt> from the package <tt>sandwich</tt> which are convenient for computation of such estimators.

The package <tt>sandwich</tt> also contains the function <tt>NeweyWest()</tt>, an implementation of the HAC variance-covariance estimator proposed by @newey1987.
</p>
</div>
')
cat('\\begin{keyconcepts}[HAC Standard errors]{15.2}
\\textbf{Problem}:\\newline

If the error term $u_t$ in the distributed lag model (\\ref{eq:dlm}) is serially correlated, statistical inference that rests on usual (heteroskedasticity-robust) standard errors can be strongly misleading.\\newline

\\textbf{Solution}:\\newline

Heteroskedasticity- and autocorrelation-consistent (HAC) estimators of the variance-covariance matrix circumvent this issue. There are \\texttt{R} functions like \\texttt{vcovHAC()} from the package \\texttt{sandwich} which are convenient for computation of such estimators.\\newline

The package \\texttt{sandwich} also contains the function \\texttt{NeweyWest()}, an implementation of the HAC variance-covariance estimator proposed by \\cite{newey1987}.
\\end{keyconcepts}
')
# function that computes rho tilde
acf_c <- function(x, j) {
  return(
    t(x[-c(1:j)]) %*% na.omit(Lag(x, j)) / t(x) %*% x
  )
}

# simulate time series with serially correlated errors
set.seed(1)

N <- 100

eps <- arima.sim(n = N, model = list(ma = 0.5))
X <- runif(N, 1, 10)
Y <- 0.5 * X + eps

# compute OLS residuals
res <- lm(Y ~ X)$res

# compute v
v <- (X - mean(X)) * res

# compute robust estimate of beta_1 variance
var_beta_hat <- 1/N * (1/(N-2) * sum((X - mean(X))^2 * res^2) ) / 
                        (1/N * sum((X - mean(X))^2))^2

# rule of thumb truncation parameter
m <- floor(0.75 * N^(1/3))

# compute correction factor
f_hat_T <- 1 + 2 * sum(
  (m - 1:(m-1))/m * sapply(1:(m - 1), function(i) acf_c(x = v, j = i))
  ) 

# compute Newey-West HAC estimate of the standard error 
sqrt(var_beta_hat * f_hat_T)
# Using NeweyWest():
NW_VCOV <- NeweyWest(lm(Y ~ X), 
              lag = m - 1, prewhite = F, 
              adjust = T)

# compute standard error
sqrt(diag(NW_VCOV))[2]
example_mod <- lm(Y ~ X)
coeftest(example_mod, vcov = NW_VCOV)
cat('
<div class = "keyconcept" id="KC15.4">
<h3 class = "right"> Key Concept 15.4 </h3>
<h3 class = "left"> Estimation of Dynamic Multipliers Under Strict Exogeneity </h3>
<p>
Consider the general distributed lag model with $r$ lags and errors following an AR($p$) process,
\\begin{align}
  Y_t =& \\, \\beta_0 + \\beta_1 X_t + \\beta_2 X_{t-1} + \\dots + \\beta_{r+1} X_{t-r} + u_t (\\#eq:dlmar) \\\\
  u_t =& \\, \\phi_1 u_{t-1} + \\phi u_{t-2} + \\dots + \\phi_p u_{t-p} + \\overset{\\sim}{u}_t. (\\#eq:dlmarerrors)
\\end{align}
Under strict exogeneity of $X_t$, one may rewrite the above model in the ADL specification
\\begin{align*}
  Y_t =& \\, \\alpha_0 + \\phi_1 Y_{t-1} + \\phi_2 Y_{t-2} + \\dots + \\phi_p Y_{t-p} \\\\
      &+ \\, \\delta_0 X_t + \\delta_1 X_{t-1} + \\dots + \\delta_q X_{t-q} + \\overset{\\sim}{u}_t
\\end{align*}
where $q=r+p$ and compute estimates of the dynamic multipliers $\\beta_1, \\beta_2, \\dots, \\beta_{r+1}$ using OLS estimates of $\\phi_1, \\phi_2, \\dots, \\phi_p, \\delta_0, \\delta_1, \\dots, \\delta_q$. 

An alternative is to estimate the dynamic multipliers using feasible GLS, that is to apply the OLS estimator to a quasi-difference specification of \\@ref(eq:dlmar). Under strict exogeneity, the feasible GLS approach is the BLUE estimator for the dynamic multipliers in large samples.

On the one hand, as demonstrated in Chapter 15.5 of the book, OLS estimation of the ADL representation can be beneficial for estimation of the dynamic multipliers in large distributed lag models because it allows for a more parsimonious model that may be a good approximation to the large model. On the other hand, the GLS approach is more efficient than the ADL estimator if the sample size is large.
</p>
</div>
')
cat('\\begin{keyconcepts}[Estimation of Dynamic Multipliers Under Strict Exogeneity]{15.4}
Consider the general distributed lag model with $r$ lags and errors following an AR($p$) process,
\\begin{align}
  Y_t =& \\, \\beta_0 + \\beta_1 X_t + \\beta_2 X_{t-1} + \\dots + \\beta_{r+1} X_{t-r} + u_t \\\\
  u_t =& \\, \\phi_1 u_{t-1} + \\phi u_{t-2} + \\dots + \\phi_p u_{t-p} + \\overset{\\sim}{u}_t. (\\#eq:dlmarerrors)
\\end{align}
Under strict exogeneity of $X_t$, one may rewrite the above model in the ADL specification
\\begin{align*}
  Y_t =& \\, \\alpha_0 + \\phi_1 Y_{t-1} + \\phi_2 Y_{t-2} + \\dots + \\phi_p Y_{t-p} \\\\
      &+ \\, \\delta_0 X_t + \\delta_1 X_{t-1} + \\dots + \\delta_q X_{t-q} + \\overset{\\sim}{u}_t
\\end{align*}
where $q=r+p$ and compute estimates of the dynamic multipliers $\\beta_1, \\beta_2, \\dots, \\beta_{r+1}$ using OLS estimates of $\\phi_1, \\phi_2, \\dots, \\phi_p, \\delta_0, \\delta_1, \\dots, \\delta_q$.\\newline 

An alternative is to estimate the dynamic multipliers using feasible GLS, that is to apply the OLS estimator to a quasi-difference specification of (\\ref{eq:dlmar}). Under strict exogeneity, the feasible GLS approach is the BLUE estimator for the dynamic multipliers in large samples.\\newline

On the one hand, as demonstrated in Chapter 15.5 of the book, OLS estimation of the ADL representation can be beneficial for estimation of the dynamic multipliers in large distributed lag models because it allows for a more parsimonious model that may be a good approximation to the large model. On the other hand, the GLS approach is more efficient than the ADL estimator if the sample size is large.
\\end{keyconcepts}
')
# set seed for reproducibility
set.seed(1)

# simulate a time series with serially correlated errors
obs <- 501
eps <- arima.sim(n = obs-1 , model = list(ar = 0.5))
X <- arima.sim(n = obs, model = list(ar = 0.25))
Y <- 0.1 * X[-1] + 0.25 * X[-obs] + eps
X <- ts(X[-1])

# estimate the distributed lag model
dlm <- dynlm(Y ~ X + L(X))
# check that the residuals are serially correlated
acf(residuals(dlm))
# coefficient summary using the Newey-West SE estimates
coeftest(dlm, vcov = NeweyWest, prewhite = F, adjust = T)
# estimate the ADL(2,1) representation of the distributed lag model
adl21_dynamic <- dynlm(Y ~ L(Y) + X + L(X, 1:2))

# plot the sample autocorrelaltions of residuals
acf(adl21_dynamic$residuals)
# compute estimated dynamic effects using coefficient restrictions
# in the ADL(2,1) representation
t <- adl21_dynamic$coefficients

c("hat_beta_1" = t[3],
  "hat_beta_2" = t[4] + t[3] * t[2])
# GLS: estimate quasi-differenced specification by OLS
iGLS_dynamic <- dynlm(I(Y- 0.5 * L(Y)) ~ I(X - 0.5 * L(X)) + I(L(X) - 0.5 * L(X, 2)))

summary(iGLS_dynamic)
X_t <- c(X[-1])
# create first lag
X_l1 <- c(X[-500])
Y_t <- c(Y[-1])

# iterated cochrane-orcutt procedure
summary(cochrane.orcutt(lm(Y_t ~ X_t + X_l1)))
# feasible GLS maximum likelihood estimation procedure
summary(gls(Y_t ~ X_t + X_l1, correlation = corAR1()))
# estimate distributed lag models of frozen orange juice price changes
FOJC_mod_DM <- dynlm(FOJC_pctc ~ L(FDD, 0:18))
FOJC_mod_CM1 <- dynlm(FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18))
FOJC_mod_CM2 <- dynlm(FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18) + season(FDD))
# set lag orders as regressor labels 
attr(FOJC_mod_DM$coefficients, "names")[1:20] <- c("(Intercept)", as.character(0:18))
attr(FOJC_mod_CM1$coefficients, "names")[1:20] <- c("(Intercept)", as.character(0:18))
attr(FOJC_mod_CM2$coefficients, "names")[1:20] <- c("(Intercept)", as.character(0:18))
length(FDD)
# gather HAC standard error errors in a list
SEs <- list(sqrt(diag(NeweyWest(FOJC_mod_DM, lag = 7, prewhite = F))), 
            sqrt(diag(NeweyWest(FOJC_mod_CM1, lag = 7, prewhite = F))), 
            sqrt(diag(NeweyWest(FOJC_mod_CM1, lag = 14, prewhite = F))),
            sqrt(diag(NeweyWest(FOJC_mod_CM2, lag = 7, prewhite = F))))
## stargazer(FOJC_mod_DM , FOJC_mod_CM1, FOJC_mod_CM1, FOJC_mod_CM2,
##   title = "Dynamic Effects of a Freezing Degree Day on the Price of Orange Juice",
##   header = FALSE,
##   digits = 3,
##   column.labels = c("Dynamic Multipliers", rep("Dynamic Cumulative Multipliers", 3)),
##   dep.var.caption  = "Dependent Variable: Monthly Percentage Change in Orange Juice Price",
##   dep.var.labels.include = FALSE,
##   covariate.labels = as.character(0:18),
##   omit = "season",
##   se = SEs,
##   no.space = T,
##   add.lines = list(c("Monthly indicators?","no", "no", "no", "yes"),
##                    c("HAC truncation","7", "7", "14", "7")),
##   omit.stat = c("rsq", "f","ser"))
stargazer(FOJC_mod_DM , FOJC_mod_CM1, FOJC_mod_CM1, FOJC_mod_CM2,
  header = FALSE, 
  type = "html",
  digits = 3, 
  column.labels = c("Dynamic Multipliers", rep("Dynamic Cumulative Multipliers", 3)),
  dep.var.caption  = "Dependent Variable: Monthly Percentage Change in Orange Juice Price",
  dep.var.labels.include = FALSE,
  covariate.labels = as.character(0:18),
  omit = "season",
  se = SEs,
  no.space = T,
  add.lines = list(
                   c("Monthly indicators?","no", "no", "no", "yes"),
                   c("HAC truncation","7", "7", "14", "7")
                   ),
  omit.stat = c("rsq", "f","ser")
  ) 

stargazer_html_title("Dynamic Effects of a Freezing Degree Day on the Price of Orange Juice", "deoafddotpooj")
stargazer(FOJC_mod_DM , FOJC_mod_CM1, FOJC_mod_CM1, FOJC_mod_CM2,
  title = "\\label{tab:deoafddotpooj} Dynamic Effects of a Freezing Degree Day on the Price of Orange Juice",
  header = FALSE, 
  type = "latex",
  column.sep.width = "20pt",
  float.env = "sidewaystable",
  single.row = T,
  digits = 3, 
  column.labels = c("Dyn. Mult.", rep("Dyn. Cum. Mult.", 3)),
  dep.var.caption  = "Dependent Variable: Monthly Percentage Change in Orange Juice Price",
  dep.var.labels.include = FALSE,
  covariate.labels = paste("lag",as.character(0:18)),
  omit = "season",
  se = SEs,
  no.space = T,
  add.lines = list(
                   c("Monthly indicators?","no", "no", "no", "yes"),
                   c("HAC truncation","7", "7", "14", "7")
                   ),
  omit.stat = c("rsq", "f","ser")
  ) 
# estimates on mothly dummies
FOJC_mod_CM2$coefficients[-c(1:20)]
# test if coefficients on monthly dummies are zero
unres_model <- dynlm(FOJC_pctc ~ L(d(FDD), 0:17) + L(FDD, 18) + season(FDD))

res_model <- update(unres_model, formula = . ~ . - season(FDD))

waldtest(unres_model, 
         res_model, 
         vcov = NeweyWest(unres_model, lag = 7, prewhite = F))
# 95% CI bounds
point_estimates <- FOJC_mod_DM$coefficients

CI_bounds <- cbind("lower" = point_estimates - 1.96 * SEs[[1]],
                   "upper" = point_estimates + 1.96 * SEs[[1]])[-1, ]

# plot the estimated dynamic multipliers
plot(0:18, point_estimates[-1], 
     type = "l", 
     lwd = 2, 
     col = "steelblue", 
     ylim = c(-0.4, 1),
     xlab = "Lag",
     ylab = "Dynamic multiplier",
     main = "Dynamic Effect of FDD on Orange Juice Price")

# add a dashed line at 0
abline(h = 0, lty = 2)

# add CI bounds
lines(0:18, CI_bounds[,1], col = "darkred")
lines(0:18, CI_bounds[,2], col = "darkred")
# 95% CI bounds
point_estimates <- FOJC_mod_CM1$coefficients

CI_bounds <- cbind("lower" = point_estimates - 1.96 * SEs[[2]],
                   "upper" = point_estimates + 1.96 * SEs[[2]])[-1,]


# plot estimated dynamic multipliers
plot(0:18, point_estimates[-1], 
     type = "l", 
     lwd = 2, 
     col = "steelblue", 
     ylim = c(-0.4, 1.6),
     xlab = "Lag",
     ylab = "Cumulative dynamic multiplier",
     main = "Cumulative Dynamic Effect of FDD on Orange Juice Price")

# add dashed line at 0
abline(h = 0, lty = 2)

# add CI bounds
lines(0:18, CI_bounds[, 1], col = "darkred")
lines(0:18, CI_bounds[, 2], col = "darkred")
# estimate cumulative multiplieres using different sample periods
FOJC_mod_CM1950 <- update(FOJC_mod_CM1, start = c(1950, 1), end = c(1966, 12))

FOJC_mod_CM1967 <- update(FOJC_mod_CM1, start = c(1967, 1), end = c(1983, 12))

FOJC_mod_CM1984 <- update(FOJC_mod_CM1, start = c(1984, 1), end = c(2000, 12))
# plot estimated dynamic cumulative multipliers (1950-1966)
plot(0:18, FOJC_mod_CM1950$coefficients[-1], 
     type = "l", 
     lwd = 2, 
     col = "steelblue",
     xlim = c(0, 20),
     ylim = c(-0.5, 2),
     xlab = "Lag",
     ylab = "Cumulative dynamic multiplier",
     main = "Cumulative Dynamic Effect for Different Sample Periods")

# plot estimated dynamic multipliers (1967-1983)
lines(0:18, FOJC_mod_CM1967$coefficients[-1], lwd = 2)

# plot estimated dynamic multipliers (1984-2000)
lines(0:18, FOJC_mod_CM1984$coefficients[-1], lwd = 2, col = "darkgreen")

# add dashed line at 0
abline(h = 0, lty = 2)

# add annotations
text(18, -0.24, "1984 - 2000")
text(18, 0.6, "1967 - 1983")
text(18, 1.2, "1950 - 1966")
# set up a range of possible break dates
tau <- c(window(time(FDD), 
                time(FDD)[round(612/100*15)], 
                time(FDD)[round(612/100*85)]))

# initialize the vector of F-statistics
Fstats <- numeric(length(tau))

# the restricted model
res_model <- dynlm(FOJC_pctc ~ L(FDD, 0:18))

# estimation, loop over break dates
for(i in 1:length(tau)) {
  
  # set up dummy variable
  D <- time(FOJC_pctc) > tau[i]
  
  # estimate DL model with intercations
  unres_model <- dynlm(FOJC_pctc ~ D * L(FDD, 0:18))
                 
  # compute and save F-statistic
  Fstats[i] <- waldtest(res_model, 
                        unres_model, 
                        vcov = NeweyWest(unres_model, lag = 7, prewhite = F))$F[2]
    
}
# QLR test statistic
max(Fstats)
