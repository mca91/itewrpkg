
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
library(readxl)
library(dynlm)
library(vars)
library(quantmod)
library(scales)
library(fGarch)
cat('
<div class = "keyconcept" id="KC16.1">
<h3 class = "right"> Key Concept <br> 16.1</h3>          
<h3 class = "left">Vector Autoregressions</h3>
<p>

The vector autoregression (VAR) model extends the idea of univariate autoregression to $k$ time series regressions, where the lagged values of *all* $k$ series appear as regressors. Put differently, in a VAR model we regress a *vector* of time series variables on lagged vectors of these variables. As for AR($p$) models, the lag order is denoted by $p$ so the VAR($p$) model of two variables $X_t$ and $Y_t$ ($k=2$) is given by the equations

\\begin{align*}
  Y_t =& \\, \\beta_{10} + \\beta_{11} Y_{t-1} + \\dots + \\beta_{1p} Y_{t-p} + \\gamma_{11} X_{t-1} + \\dots + \\gamma_{1p} X_{t-p} + u_{1t}, \\\\
  X_t =& \\, \\beta_{20} + \\beta_{21} Y_{t-1} + \\dots + \\beta_{2p} Y_{t-p} + \\gamma_{21} X_{t-1} + \\dots + \\gamma_{2p} X_{t-p} + u_{2t}.
\\end{align*}

The $\\beta$s and $\\gamma$s can be estimated using OLS on each equation. The assumptions for VARs are the time series assumptions presented in Key Concept 14.6 applied to each of the equations.

It is straightforward to estimate VAR models in <tt>R</tt>. A feasible approach is to simply use <tt>lm()</tt> for estimation of the individual equations. Furthermore, the <tt>R</tt>package <tt>vars</tt> provides standard tools for estimation, diagnostic testing and prediction using this type of models.  

</p>
</div>
')
cat('\\begin{keyconcepts}[Vector Autoregressions]{16.1}
The vector autoregression (VAR) model extends the idea of univariate autoregression to $k$ time series regressions, where the lagged values of \\textit{all} $k$ series appear as regressors. Put differently, in a VAR model we regress a \\textit{vector} of time series variables on lagged vectors of these variables. As for AR($p$) models, the lag order is denoted by $p$ so the VAR($p$) model of two variables $X_t$ and $Y_t$ ($k=2$) is given by the equations

\\begin{align*}
  Y_t =& \\, \\beta_{10} + \\beta_{11} Y_{t-1} + \\dots + \\beta_{1p} Y_{t-p} + \\gamma_{11} X_{t-1} + \\dots + \\gamma_{1p} X_{t-p} + u_{1t}, \\\\
  X_t =& \\, \\beta_{20} + \\beta_{21} Y_{t-1} + \\dots + \\beta_{2p} Y_{t-p} + \\gamma_{21} X_{t-1} + \\dots + \\gamma_{2p} X_{t-p} + u_{2t}.
\\end{align*}

The $\\beta$s and $\\gamma$s can be estimated using OLS on each equation. The assumptions for VARs are the time series assumptions presented in Key Concept 14.6 applied to each of the equations.\\newline

It is straightforward to estimate VAR models in \\texttt{R}. A feasible approach is to simply use \\texttt{lm()} for estimation of the individual equations. Furthermore, the \\texttt{R} package \\texttt{vars} provides standard tools for estimation, diagnostic testing and prediction using this type of models.  
\\end{keyconcepts}
')
## # load the U.S. macroeconomic data set
## USMacroSWQ <- read_xlsx("Data/us_macro_quarterly.xlsx",
##                          sheet = 1,
##                          col_types = c("text", rep("numeric", 9)))
## 
## # set the column names
## colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI", "GS10",
##                           "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")
## 
## # format the date column
## USMacroSWQ$Date <- as.yearqtr(USMacroSWQ$Date, format = "%Y:0%q")
## 
## # define GDP as ts object
## GDP <- ts(USMacroSWQ$GDPC96,
##           start = c(1957, 1),
##           end = c(2013, 4),
##           frequency = 4)
## 
## # define GDP growth as a ts object
## GDPGrowth <- ts(400*log(GDP[-1]/GDP[-length(GDP)]),
##                 start = c(1957, 2),
##                 end = c(2013, 4),
##                 frequency = 4)
## 
## # 3-months Treasury bill interest rate as a 'ts' object
## TB3MS <- ts(USMacroSWQ$TB3MS,
##             start = c(1957, 1),
##             end = c(2013, 4),
##             frequency = 4)
## 
## # 10-years Treasury bonds interest rate as a 'ts' object
## TB10YS <- ts(USMacroSWQ$GS10,
##               start = c(1957, 1),
##               end = c(2013, 4),
##               frequency = 4)
## 
## # generate the term spread series
## TSpread <- TB10YS - TB3MS
library(xts)
# load US macroeconomic data
USMacroSWQ <- read_xlsx("Data/us_macro_quarterly.xlsx",
                         sheet = 1,
                         col_types = c("text", rep("numeric", 9))
                        )

# set column names
colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI", "GS10", "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")

# formate date column
USMacroSWQ$Date <- as.yearqtr(USMacroSWQ$Date, format = "%Y:0%q")

# circumvent bug
GDP <- xts(USMacroSWQ$GDPC96, USMacroSWQ$Date)["1960::2013"]
GDPGrowth <- xts(400 * log(GDP/lag(GDP)))
GDP <- ts(GDP,
          start = c(1960, 1), 
          end = c(2013, 4), 
          frequency = 4)

GDPGrowth <- ts(GDPGrowth,
                start = c(1960, 1), 
                end = c(2013, 4), 
                frequency = 4)

# 3 months Treasury bill interest rate as ts object
TB3MS <- ts(USMacroSWQ$TB3MS,
            start = c(1957, 1), 
            end = c(2013, 4), 
            frequency = 4)

# 10 years Treasury bonds interest rate as ts object
TB10YS <- ts(USMacroSWQ$GS10, 
              start = c(1957, 1), 
              end = c(2013, 4), 
              frequency = 4)

# term spread series
TSpread <- TB10YS - TB3MS
# Estimate both equations using 'dynlm()'
VAR_EQ1 <- dynlm(GDPGrowth ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2), 
                 start = c(1981, 1), 
                 end = c(2012, 4))

VAR_EQ2 <- dynlm(TSpread ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2),
                 start = c(1981, 1),
                 end = c(2012, 4))

# rename regressors for better readability
names(VAR_EQ1$coefficients) <- c("Intercept","Growth_t-1", 
                                 "Growth_t-2", "TSpread_t-1", "TSpread_t-2")
names(VAR_EQ2$coefficients) <- names(VAR_EQ1$coefficients)

# robust coefficient summaries
coeftest(VAR_EQ1, vcov. = sandwich)
coeftest(VAR_EQ2, vcov. = sandwich)
# set up data for estimation using `VAR()`
VAR_data <- window(ts.union(GDPGrowth, TSpread), start = c(1980, 3), end = c(2012, 4))

# estimate model coefficients using `VAR()`
VAR_est <- VAR(y = VAR_data, p = 2)
VAR_est
# obtain the adj. R^2 from the output of 'VAR()'
summary(VAR_est$varresult$GDPGrowth)$adj.r.squared
summary(VAR_est$varresult$TSpread)$adj.r.squared
# Granger causality tests:

# test if term spread has no power in explaining GDP growth
linearHypothesis(VAR_EQ1, 
                 hypothesis.matrix = c("TSpread_t-1", "TSpread_t-2"),
                 vcov. = sandwich)

# test if GDP growth has no power in explaining term spread
linearHypothesis(VAR_EQ2, 
                 hypothesis.matrix = c("Growth_t-1", "Growth_t-2"),
                 vcov. = sandwich)
cat('
<div class = "keyconcept" id="KC16.2">
<h3 class = "right"> Key Concept <br> 16.2 </h3>          
<h3 class = "left"> Iterated Multiperiod Forecasts </h3>
<p>
The steps for an *iterated multiperiod AR forecast* are:

1. Estimate the AR($p$) model using OLS and compute the one-period-ahead forecast.

2. Use the one-period-ahead forecast to obtain the two-period-ahead forecast.

3. Continue by iterating to obtain forecasts farther into the future.

An *iterated multiperiod VAR forecast* is done as follows:

1. Estimate the VAR($p$) model using OLS per equation and compute the one-period-ahead forecast for *all* variables in the VAR.

2. Use the one-period-ahead forecasts to obtain the two-period-ahead forecasts.

3. Continue by iterating to obtain forecasts of all variables in the VAR farther into the future.

</p>
</div>
')
cat('\\begin{keyconcepts}[Iterated Multiperiod Forecasts]{16.2}
The steps for an \\textit{iterated multiperiod AR forecast} are:\\newline

\\begin{enumerate}
\\item Estimate the AR($p$) model using OLS and compute the one-period-ahead forecast.
\\item Use the one-period-ahead forecast to obtain the two-period-ahead forecast.
\\item Continue by iterating to obtain forecasts farther into the future.
\\end{enumerate}\\vspace{0.5cm}

An \\textit{iterated multiperiod VAR forecast} is done as follows:\\newline

\\begin{enumerate}
\\item Estimate the VAR($p$) model using OLS per equation and compute the one-period-ahead forecast for \\textit{all} variables in the VAR.
\\item Use the one-period-ahead forecasts to obtain the two-period-ahead forecasts.
\\item Continue by iterating to obtain forecasts of all variables in the VAR farther into the future.
\\end{enumerate}
\\end{keyconcepts}
')
# compute iterated forecasts for GDP growth and term spread for the next 10 quarters
forecasts <- predict(VAR_est)
forecasts
# visualize the iterated forecasts
plot(forecasts)
cat('
<div class = "keyconcept" id="KC16.3">
<h3 class = "right"> Key Concept <br> 16.3 </h3>          
<h3 class = "left"> Direct Multiperiod Forecasts </h3>
<p>
A *direct multiperiod forecast* that forecasts $h$ periods into the future using a model of $Y_t$ and an additional predictor $X_t$ with $p$ lags is done by first estimating

\\begin{align*}
  Y_t =& \\, \\delta_0 + \\delta_1 Y_{t-h} + \\dots + \\delta_{p} Y_{t-p-h+1} + \\delta_{p+1} X_{t-h} \\\\
  +& \\dots + \\delta_{2p} Y_{t-p-h+1} + u_t,  
\\end{align*}

which is then used to compute the forecast of $Y_{T+h}$ based on observations through period $T$.
</p>
</div>
')
cat('\\begin{keyconcepts}[Direct Multiperiod Forecasts]{16.3}
A \\textit{direct multiperiod forecast} that forecasts $h$ periods into the future using a model of $Y_t$ and an additional predictor $X_t$ with $p$ lags is done by first estimating
\\begin{align*}
  Y_t =& \\, \\delta_0 + \\delta_1 Y_{t-h} + \\dots + \\delta_{p} Y_{t-p-h+1} + \\delta_{p+1} X_{t-h} \\\\
  +& \\dots + \\delta_{2p} Y_{t-p-h+1} + u_t,  
\\end{align*}

which is then used to compute the forecast of $Y_{T+h}$ based on observations through period $T$.
\\end{keyconcepts}
')
# estimate models for direct two-quarter-ahead forecasts
VAR_EQ1_direct <- dynlm(GDPGrowth ~ L(GDPGrowth, 2:3) + L(TSpread, 2:3), 
                        start = c(1981, 1), end = c(2012, 4))

VAR_EQ2_direct <- dynlm(TSpread ~ L(GDPGrowth, 2:3) + L(TSpread, 2:3), 
                        start = c(1981, 1), end = c(2012, 4))

# compute direct two-quarter-ahead forecasts
coef(VAR_EQ1_direct) %*% c(1, # intercept
                           window(GDPGrowth, start = c(2012, 3), end = c(2012, 4)), 
                           window(TSpread, start = c(2012, 3), end = c(2012, 4)))

coef(VAR_EQ2_direct) %*% c(1, # intercept
                           window(GDPGrowth, start = c(2012, 3), end = c(2012, 4)), 
                           window(TSpread, start = c(2012, 3), end = c(2012, 4)))
cat('
<div class = "keyconcept" id="KC16.4">
<h3 class = "right"> Key Concept <br> 16.4 </h3>          
<h3 class = "left"> Orders of Integration, Differencing and Stationarity </h3>
<p>

+ When a time series $Y_t$ has a unit autoregressive root, $Y_t$ is integrated of order one. This is often denoted by $Y_t \\sim I(1)$. We simply say that $Y_t$ is $I(1)$. If $Y_t$ is $I(1)$, its first difference $\\Delta Y_t$ is stationary.

+ $Y_t$ is $I(2)$ when $Y_t$ needs to be differenced twice in order to obtain a stationary series. Using the notation introduced here, if $Y_t$ is $I(2)$, its first difference $\\Delta Y_t$ is $I(1)$ and its second difference $\\Delta^2 Y_t$ is stationary. $Y_t$ is $I(d)$ when $Y_t$ must be differenced $d$ times to obtain a stationary series.

+ When $Y_t$ is stationary, it is integrated of order $0$ so $Y_t$ is $I(0)$.

It is fairly easy to obtain differences of time series in <tt>R</tt>. For example, the function <tt>diff()</tt> returns suitably lagged and iterated differences of numeric vectors, matrices and time series objects of the class <tt>ts</tt>.

</p>
</div>
')
cat('\\begin{keyconcepts}[Orders of Integration, Differencing and Stationarity]{16.4}
\\begin{itemize}
\\item When a time series $Y_t$ has a unit autoregressive root, $Y_t$ is integrated of order one. This is often denoted by $Y_t \\sim I(1)$. We simply say that $Y_t$ is $I(1)$. If $Y_t$ is $I(1)$, its first difference $\\Delta Y_t$ is stationary.
\\item $Y_t$ is $I(2)$ when $Y_t$ needs to be differenced twice in order to obtain a stationary series. Using the notation introduced here, $Y_t$ is $I(2)$, its first difference $\\Delta Y_t$ is $I(1)$ and its second difference $\\Delta^2 Y_t$ is stationary. $Y_t$ is $I(d)$ when $Y_t$ must be differenced $d$ times to obtain a stationary series.
\\item When $Y_t$ is stationary, it is integrated of order $0$ so $Y_t$ is $I(0)$.
\\end{itemize}\\vspace{0.5cm}
It is fairly easy to obtain differences of time series in \\texttt{R}. For example, the function \\texttt{diff()} returns suitably lagged and iterated differences of numeric vectors, matrices and time series objects of the class \\texttt{ts}.
\\end{keyconcepts}
')
# define ts object of the U.S. PCE Price Index
PCECTPI <- ts(log(USMacroSWQ$PCECTPI), 
              start = c(1957, 1), 
              end = c(2012, 4), 
              freq = 4)

# plot logarithm of the PCE Price Index
plot(log(PCECTPI),
     main = "Log of United States PCE Price Index",
     ylab = "Logarithm",
     col = "steelblue", 
     lwd = 2)
# plot U.S. PCE price inflation
plot(400 * Delt(PCECTPI),
     main = "United States PCE Price Index",
     ylab = "Percent per annum",
     col = "steelblue", 
     lwd = 2)

# add a dashed line at y =  0 
abline(0, 0, lty = 2)
# DF-GLS test for unit root in GDP
summary(ur.ers(log(window(GDP, start = c(1962, 1), end = c(2012, 4))),
        model = "trend", 
        lag.max = 2))
cat('
<div class = "keyconcept" id="KC16.5">
<h3 class = "right"> Key Concept <br> 16.5 </h3>          
<h3 class = "left"> Cointegration </h3>
<p>
When $X_t$ and $Y_t$ are $I(1)$ and if there is a $\\theta$ such that $Y_t - \\theta X_t$ is $I(0)$, $X_t$ and $Y_t$ are cointegrated. Put differently, cointegration of $X_t$ and $Y_t$ means that $X_t$ and $Y_t$ have the same or a common stochastic trend and that this trend can be eliminated by taking a specific difference of the series such that the resulting series is stationary.

<tt>R</tt> functions for cointegration analysis are implemented in the package <tt>urca</tt>. 

</p>
</div>
')
cat('\\begin{keyconcepts}[Cointegration]{16.5}
When $X_t$ and $Y_t$ are $I(1)$ and if there is a $\\theta$ such that $Y_t - \\theta X_t$ is $I(0)$, $X_t$ and $Y_t$ are cointegrated. Put differently, cointegration of $X_t$ and $Y_t$ means that $X_t$ and $Y_t$ have the same or a common stochastic trend and that this trend can be eliminated by taking a specific difference of the series such that the resulting series is stationary.\\newline

\\texttt{R} functions for cointegration analysis are implemented in the package \\texttt{urca}. 
\\end{keyconcepts}
')
# reproduce Figure 16.2 of the book

# plot both interest series
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)), 
     plot.type = "single", 
     lty = c(2, 1),
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     ylim = c(-5, 17),
     main = "Interest Rates")

# add the term spread series
lines(as.zoo(TSpread), 
     col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Term Spread")

# shade the term spread
polygon(c(time(TB3MS), rev(time(TB3MS))), 
        c(TB10YS, rev(TB3MS)),
        col = alpha("steelblue", alpha = 0.3),
        border = NA)

# add horizontal line add 0
abline(0, 0)

# add a legend
legend("topright", 
       legend = c("TB3MS", "TB10YS", "Term Spread"),
       col = c("black", "black", "steelblue"),
       lwd = c(2, 2, 2),
       lty = c(2, 1, 1))
# test for nonstationarity of 3-month treasury bills using ADF test
ur.df(window(TB3MS, c(1962, 1), c(2012, 4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")

# test for nonstationarity of 10-years treasury bonds using ADF test
ur.df(window(TB10YS, c(1962, 1), c(2012, 4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")

# test for nonstationarity of 3-month treasury bills using DF-GLS test
ur.ers(window(TB3MS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)

# test for nonstationarity of 10-years treasury bonds using DF-GLS test
ur.ers(window(TB10YS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)
# test if term spread is stationairy (cointegration of interest rates) using ADF
ur.df(window(TB10YS, c(1962, 1), c(2012, 4)) - window(TB3MS, c(1962, 1), c(2012 ,4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")

# test if term spread is stationairy (cointegration of interest rates) using DF-GLS test
ur.ers(window(TB10YS, c(1962, 1), c(2012, 4)) - window(TB3MS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)
# estimate first-stage regression of EG-ADF test
FS_EGADF <- dynlm(window(TB10YS, c(1962, 1), c(2012, 4)) ~ window(TB3MS, c(1962, 1), c(2012, 4)))
FS_EGADF
# compute the residuals
z_hat <- resid(FS_EGADF)

# compute the ADF test statistic
ur.df(z_hat, lags = 6, type = "none", selectlags = "AIC")
TB10YS <- window(TB10YS, c(1962, 1), c(2012 ,4))
TB3MS <- window(TB3MS, c(1962, 1), c(2012, 4))

# set up error correction term
VECM_ECT <- TB10YS - TB3MS

# estimate both equations of the VECM using 'dynlm()'
VECM_EQ1 <- dynlm(d(TB10YS) ~ L(d(TB3MS), 1:2) + L(d(TB10YS), 1:2) + L(VECM_ECT))
VECM_EQ2 <- dynlm(d(TB3MS) ~ L(d(TB3MS), 1:2) + L(d(TB10YS), 1:2) + L(VECM_ECT))

# rename regressors for better readability
names(VECM_EQ1$coefficients) <- c("Intercept", "D_TB3MS_l1", "D_TB3MS_l2",
                                  "D_TB10YS_l1", "D_TB10YS_l2", "ect_l1")
names(VECM_EQ2$coefficients) <- names(VECM_EQ1$coefficients)

# coefficient summaries using HAC standard errors
coeftest(VECM_EQ1, vcov. = NeweyWest(VECM_EQ1, prewhite = F, adjust = T))
coeftest(VECM_EQ2, vcov. = NeweyWest(VECM_EQ2, prewhite = F, adjust = T))
# import data on the Wilshire 5000 index
W5000 <- read.csv2("data/Wilshire5000.csv", 
                   stringsAsFactors = F, 
                   header = T, 
                   sep = ",", 
                   na.strings = ".")

# transform the columns
W5000$DATE <- as.Date(W5000$DATE)
W5000$WILL5000INDFC <- as.numeric(W5000$WILL5000INDFC)

# remove NAs
W5000 <- na.omit(W5000)

# compute daily percentage changes
W5000_PC <- data.frame("Date" = W5000$DATE, 
                       "Value" = as.numeric(Delt(W5000$WILL5000INDFC) * 100))
W5000_PC <- na.omit(W5000_PC)
# plot percentage changes
plot(W5000_PC, 
     ylab = "Percent", 
     main = "Daily Percentage Changes",
     type="l", 
     col = "steelblue", 
     lwd = 0.5)

# add horizontal line at y = 0
abline(0, 0)
# plot sample autocorrelation of daily percentage changes
acf(W5000_PC$Value, main = "Wilshire 5000 Series")
# estimate GARCH(1,1) model of daily percentage changes
GARCH_Wilshire <- garchFit(data = W5000_PC$Value, trace = F)
# compute deviations of the percentage changes from their mean
dev_mean_W5000_PC <- W5000_PC$Value - GARCH_Wilshire@fit$coef[1]

# plot deviation of percentage changes from mean
plot(W5000_PC$Date, dev_mean_W5000_PC, 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "Estimated Bands of +- One Conditional Standard Deviation",
     lwd = 0.2)

# add horizontal line at y = 0
abline(0, 0)

# add GARCH(1,1) confidence bands (one standard deviation) to the plot
lines(W5000_PC$Date, 
      GARCH_Wilshire@fit$coef[1] + GARCH_Wilshire@sigma.t, 
      col = "darkred", 
      lwd = 0.5)

lines(W5000_PC$Date, 
      GARCH_Wilshire@fit$coef[1] - GARCH_Wilshire@sigma.t, 
      col = "darkred", 
      lwd = 0.5)
