library(dynlm)
library(stargazer)
library(scales)
library(readxl)
library(urca)

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
## library(AER)
## library(dynlm)
## library(forecast)
## library(readxl)
## library(stargazer)
## library(scales)
## library(quantmod)
## library(urca)
library(AER)
data(CASchools)   
CASchools$STR <- CASchools$students/CASchools$teachers       
CASchools$score <- (CASchools$read + CASchools$math)/2

mod <- lm(score ~ STR, data = CASchools)
mod
predict(mod, newdata = data.frame("STR" = 25))
# attach the package 'quantmod'
library(quantmod)
# load US macroeconomic data
USMacroSWQ <- read_xlsx("Data/us_macro_quarterly.xlsx",
                         sheet = 1,
                         col_types = c("text", rep("numeric", 9)))

# format date column
USMacroSWQ$X__1 <- as.yearqtr(USMacroSWQ$X__1, format = "%Y:0%q")

# adjust column names
colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI", 
                          "GS10", "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")
# GDP series as xts object
GDP <- xts(USMacroSWQ$GDPC96, USMacroSWQ$Date)["1960::2013"]

# GDP growth series as xts object
GDPGrowth <- xts(400 * log(GDP/lag(GDP)))
# reproduce Figure 14.1 (a) of the book
plot(log(as.zoo(GDP)),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "U.S. Quarterly Real GDP")
# reproduce Figure 14.1 (b) of the book
plot(as.zoo(GDPGrowth),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "U.S. Real GDP Growth Rates")
cat('
<div class = "keyconcept" id="KC14.1">
<h3 class = "right"> Key Concept 14.1 </h3>
<h3 class = "left"> Lags, First Differences, Logarithms and Growth Rates </h3>

- Previous values of a time series are called *lags*. The first lag of $Y_t$ is $Y_{t-1}$. The $j^{th}$ lag of $Y_t$ is $Y_{t-j}$. In <tt>r ttcode("R")</tt>, lags of univariate or multivariate time series objects are conveniently computed by <tt>lag()</tt>, see <tt>?lag</tt>.

- Sometimes we work with a differenced series. The first difference of a series is $\\Delta Y_{t} = Y_t - Y_{t-1}$, the difference between periods $t$ and $t-1$. If <tt>Y</tt> is a time series, the series of first differences is computed as <tt>diff(Y)</tt>.

- It may be convenient to work with the first difference in logarithms of a series. We denote this by $\\Delta \\log(Y_t) = \\log(Y_t) - \\log(Y_{t-1})$. For a time series <tt>Y</tt>, this is obtained using <tt>log(Y/lag(Y))</tt>. 

- $100 \\Delta \\log (Y_t)$ is an approximation for the percentage change between $Y_t$ and $Y_{t-1}$.

</div>
')
cat('\\begin{keyconcepts}[Lags, First Differences, Logarithms and Growth Rates]{14.1}
\\begin{itemize}
\\item Previous values of a time series are called \\textit{lags}. The first lag of $Y_t$ is $Y_{t-1}$. The $j^{th}$ lag of $Y_t$ is $Y_{t-j}$. In \\texttt{R}, lags of univariate or multivariate time series objects are conveniently computed by \\texttt{lag()}, see \\texttt{?lag}.
\\item Sometimes we work with differenced series. The first difference of a series is $\\Delta Y_{t} = Y_t - Y_{t-1}$, the difference between periods $t$ and $t-1$. If \\texttt{Y} is a time series, the series of first differences is computed as \\texttt{diff(Y)}.
\\item It may be convenient to work with the first difference in logarithms of a series. We denote this by $\\Delta \\log(Y_t) = \\log(Y_t) - \\log(Y_{t-1})$. For a time series \\texttt{Y}, this is obtained using \\texttt{log(Y/lag(Y))}. 
\\item $100 \\Delta \\log (Y_t)$ is an approximation for the percentage change between $Y_t$ and $Y_{t-1}$.
\\end{itemize}
\\end{keyconcepts}
')
# compute logarithms, annual growth rates and 1st lag of growth rates
quants <- function(series) {
  s <- series
  return(
    data.frame("Level" = s,
               "Logarithm" = log(s),
               "AnnualGrowthRate" = 400 * log(s / lag(s)),
               "1stLagAnnualGrowthRate" = lag(400 * log(s / lag(s))))
    )
}
# obtain a data.frame with level, logarithm, annual growth rate and its 1st lag of GDP
quants(GDP["2011-07::2013-01"])
cat('
<div class = "keyconcept" id="KC14.2">
<h3 class = "right"> Key Concept 14.2 </h3>
<h3 class = "left"> Autocorrelation and Autocovariance </h3>

The covariance between $Y_t$ and its $j^{th}$ lag, $Y_{t-j}$, is called the $j^{th}$ *autocovariance* of the series $Y_t$. The $j^{th}$ *autocorrelation coefficient*, also called the *serial correlation coefficient*, measures the correlation between $Y_t$ and $Y_{t-j}$.

We thus have
\\begin{align*}
  j^{th} \\text{autocovariance} =& \\, Cov(Y_t,Y_{t-j}), \\\\
  j^{th} \\text{autocorrelation} = \\rho_j =& \\, \\rho_{Y_t,Y_{t-j}} = \\frac{Cov(Y_t,Y_{t-j)}}{\\sqrt{Var(Y_t)Var(Y_{t-j})}}.
\\end{align*}

Population autocovariance and population autocorrelation can be estimated by $\\widehat{Cov(Y_t,Y_{t-j})}$, the sample autocovariance, and $\\widehat{\\rho}_j$, the sample autocorrelation:

\\begin{align*}
  \\widehat{Cov(Y_t,Y_{t-j})} =& \\, \\frac{1}{T} \\sum_{t=j+1}^T (Y_t - \\overline{Y}_{j+1:T})(Y_{t-j} - \\overline{Y}_{1:T-j}), \\\\
  \\widehat{\\rho}_j =& \\, \\frac{\\widehat{Cov(Y_t,Y_{t-j})}}{\\widehat{Var(Y_t)}}
\\end{align*}

$\\overline{Y}_{j+1:T}$ denotes the average of $Y_{j+1}, Y{j+2}, \\dots, Y_T$.

In <tt>R</tt> the function <tt>acf()</tt> from the package <tt>stats</tt> computes the sample autocovariance or the sample autocorrelation function.

</div>
')
cat('\\begin{keyconcepts}[Autocorrelation and Autocovariance]{14.2}
The covariance between $Y_t$ and its $j^{th}$ lag, $Y_{t-j}$, is called the $j^{th}$ \\textit{autocovariance} of the series $Y_t$. The $j^{th}$ \\textit{autocorrelation coefficient}, also called the \\textit{serial correlation coefficient}, measures the correlation between $Y_t$ and $Y_{t-j}$. We thus have
\\begin{align*}
  j^{th} \\text{autocovariance} =& \\, Cov(Y_t,Y_{t-j}), \\\\
  j^{th} \\text{autocorrelation} = \\rho_j =& \\, \\rho_{Y_t,Y_{t-j}} = \\frac{Cov(Y_t,Y_{t-j)}}{\\sqrt{Var(Y_t)Var(Y_{t-j})}}.
\\end{align*}

Population autocovariance and population autocorrelation can be estimated by $\\widehat{Cov(Y_t,Y_{t-j})}$, the sample autocovariance, and $\\widehat{\\rho}_j$, the sample autocorrelation:
\\begin{align*}
  \\widehat{Cov(Y_t,Y_{t-j})} =& \\, \\frac{1}{T} \\sum_{t=j+1}^T (Y_t - \\overline{Y}_{j+1:T})(Y_{t-j} - \\overline{Y}_{1:T-j}), \\\\
  \\widehat{\\rho}_j =& \\, \\frac{\\widehat{Cov(Y_t,Y_{t-j})}}{\\widehat{Var(Y_t)}}.
\\end{align*}\\vspace{0.5cm}

$\\overline{Y}_{j+1:T}$ denotes the average of $Y_{j+1}, Y{j+2}, \\dots, Y_T$.\\newline

In \\texttt{R} the function \\texttt{acf()} from the package \\texttt{stats} computes the sample autocovariance or the sample autocorrelation function.
\\end{keyconcepts}
')
acf(na.omit(GDPGrowth), lag.max = 4, plot = F)
# define series as xts objects
USUnemp <- xts(USMacroSWQ$UNRATE, USMacroSWQ$Date)["1960::2013"]

DollarPoundFX <- xts(USMacroSWQ$EXUSUK, USMacroSWQ$Date)["1960::2013"]
  
JPIndProd <- xts(log(USMacroSWQ$JAPAN_IP), USMacroSWQ$Date)["1960::2013"]

# attach NYSESW data
data("NYSESW")  
NYSESW <- xts(Delt(NYSESW))
# divide plotting area into 2x2 matrix
par(mfrow = c(2, 2))

# plot the series
plot(as.zoo(USUnemp),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent",
     xlab = "Date",
     main = "US Unemployment Rate",
     cex.main = 1)

plot(as.zoo(DollarPoundFX),
     col = "steelblue",
     lwd = 2,
     ylab = "Dollar per pound",
     xlab = "Date",
     main = "U.S. Dollar / B. Pound Exchange Rate",
     cex.main = 1)

plot(as.zoo(JPIndProd),
     col = "steelblue",
     lwd = 2,
     ylab = "Logarithm",
     xlab = "Date",
     main = "Japanese Industrial Production",
     cex.main = 1)

plot(as.zoo(NYSESW),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent per Day",
     xlab = "Date",
     main = "New York Stock Exchange Composite Index",
     cex.main = 1)
# compute sample autocorrelation for the NYSESW series
acf(na.omit(NYSESW), plot = F, lag.max = 10)
# plot sample autocorrelation for the NYSESW series
acf(na.omit(NYSESW), main = "Sample Autocorrelation for NYSESW Data")
# subset data
GDPGRSub <- GDPGrowth["1962::2012"]

# estimate the model
ar.ols(GDPGRSub, 
       order.max = 1, 
       demean = F, 
       intercept = T)
# length of data set
N <-length(GDPGRSub)

GDPGR_level <- as.numeric(GDPGRSub[-1])
GDPGR_lags <- as.numeric(GDPGRSub[-N])

# estimate the model
armod <- lm(GDPGR_level ~ GDPGR_lags)
armod
# robust summary
coeftest(armod, vcov. = vcovHC, type = "HC1")
library(forecast)

# assign GDP growth rate in 2012:Q4
new <- data.frame("GDPGR_lags" = GDPGR_level[N-1])

# forecast GDP growth rate in 2013:Q1
forecast(armod, newdata = new)
# compute the forecast error
forecast(armod, newdata = new)$mean - GDPGrowth["2013"][1]

# R^2
summary(armod)$r.squared

# SER
summary(armod)$sigma
cat('
<div class = "keyconcept" id="KC14.3">
<h3 class = "right"> Key Concept 14.3 </h3>
<h3 class = "left"> Autoregressions </h3>
<p>
An AR($p$) model assumes that a time series $Y_t$ can be modeld by a linear function of the first $p$ of its lagged values.
\\begin{align*}
  Y_t = \\beta_0 + \\beta_1 Y_{t-1} + \\beta_2 Y_{t-2} + \\dots + \\beta_p Y_{t-p} + u_t
\\end{align*}
is an autoregressive model of order $p$ where $E(u_t\\vert Y_{t-1}, Y_{t-2}, \\dots,Y_{t-p})=0$.
</p>
</div>
')
cat('\\begin{keyconcepts}[Autoregressions]{14.3}
An AR($p$) model assumes that a time series $Y_t$ can be modeled by a linear function of the first $p$ of its lagged values.
\\begin{align*}
  Y_t = \\beta_0 + \\beta_1 Y_{t-1} + \\beta_2 Y_{t-2} + \\dots + \\beta_p Y_{t-p} + u_t
\\end{align*}
is an autoregressive model of order $p$ where $E(u_t\\vert Y_{t-1}, Y_{t-2}, \\dots,Y_{t-p})=0$.
\\end{keyconcepts}
')
# estimate the AR(2) model
GDPGR_AR2 <- dynlm(ts(GDPGR_level) ~ L(ts(GDPGR_level)) + L(ts(GDPGR_level), 2))

coeftest(GDPGR_AR2, vcov. = sandwich)
# R^2
summary(GDPGR_AR2)$r.squared

# SER
summary(GDPGR_AR2)$sigma
# AR(2) forecast of GDP growth in 2013:Q1 
forecast <- c("2013:Q1" = coef(GDPGR_AR2) %*% c(1, GDPGR_level[N-1], GDPGR_level[N-2]))
# compute AR(2) forecast error 
GDPGrowth["2013"][1] - forecast
# read in data on stock returns
SReturns <- read_xlsx("Data/Stock_Returns_1931_2002.xlsx",
                      sheet = 1,
                      col_types = "numeric")
# convert to ts object
StockReturns <- ts(SReturns[, 3:4], 
                   start = c(1931, 1), 
                   end = c(2002, 12), 
                   frequency = 12)
# estimate AR models:

# AR(1)
SR_AR1 <- dynlm(ExReturn ~ L(ExReturn), 
      data = StockReturns, start = c(1960, 1), end = c(2002, 12))

# AR(2)
SR_AR2 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 2), 
      data = StockReturns, start = c(1960, 1), end = c(2002, 12))

# AR(4)
SR_AR4 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 1:4), 
      data = StockReturns, start = c(1960, 1), end = c(2002, 12))
# compute robust standard errors
rob_se <- list(sqrt(diag(sandwich(SR_AR1))),
               sqrt(diag(sandwich(SR_AR2))),
               sqrt(diag(sandwich(SR_AR4))))
## # generate table using 'stargazer()'
## stargazer(SR_AR1, SR_AR2, SR_AR4,
##   title = "Autoregressive Models of Monthly Excess Stock Returns",
##   header = FALSE,
##   model.numbers = F,
##   omit.table.layout = "n",
##   digits = 3,
##   column.labels = c("AR(1)", "AR(2)", "AR(4)"),
##   dep.var.caption  = "Dependent Variable: Excess Returns on the CSRP Value-Weighted Index",
##   dep.var.labels.include = FALSE,
##   covariate.labels = c("$excess return_{t-1}$", "$excess return_{t-2}$",
##                        "$excess return_{t-3}$", "$excess return_{t-4}$",
##                        "Intercept"),
##   se = rob_se,
##   omit.stat = "rsq")
stargazer(SR_AR1, SR_AR2, SR_AR4,
  header = FALSE, 
  type = "html",
  model.numbers = F,
  omit.table.layout = "n",
  digits = 3, 
  column.labels = c("AR(1)", "AR(2)", "AR(4)"),
  dep.var.caption  = "Dependent Variable: Excess returns on the CSRP Value-Weighted Index",
  dep.var.labels.include = FALSE,
  covariate.labels = c("$excess return_{t-1}$", "$excess return_{t-2}$", "$excess return_{t-3}$", "$excess return_{t-4}$", "Intercept"),
  se = rob_se,
  omit.stat = c("rsq")
  )

stargazer_html_title("Autoregressive Models of Monthly Excess Stock Returns", "amomesr")
stargazer(SR_AR1, SR_AR2, SR_AR4,
  title = "\\label{tab:amomesr} Autoregressive Models of Monthly Excess Stock Returns",
  header = FALSE, 
  type = "latex",
  model.numbers = F,
  omit.table.layout = "n",
  digits = 3, 
  column.labels = c("AR(1)", "AR(2)", "AR(4)"),
  dep.var.caption  = "Dependent Variable: Excess returns on the CSRP Value-Weighted Index",
  dep.var.labels.include = FALSE,
  covariate.labels = c("$excess return_{t-1}$", "$excess return_{t-2}$", "$excess return_{t-3}$", "$excess return_{t-4}$", "Intercept"),
  se = rob_se,
  omit.stat = c("rsq")
  ) 
cat('
<div class = "keyconcept" id="KC14.4">
<h3 class = "right"> Key Concept 14.4 </h3>
<h3 class = "left"> The Autoregressive Distributed Lag Model </h3>
<p>
An ADL($p$,$q$) model assumes that a time series $Y_t$ can be represented by a linear function of $p$ of its lagged values and $q$ lags of another time series $X_t$:
\\begin{align*}
  Y_t =& \\, \\beta_0 + \\beta_1 Y_{t-1} + \\beta_2 Y_{t-2} + \\dots + \\beta_p Y_{t-p} \\\\ 
      &+ \\, \\delta_1 X_{t-1} + \\delta_2 X_{t-2} + \\dots + \\delta_q X_{t-q} X_{t-q} + u_t.
\\end{align*}
is an *autoregressive distributed lag model* with $p$ lags of $Y_t$ and $q$ lags of $X_t$ where $$E(u_t\\vert Y_{t-1}, Y_{t-2}, \\dots, X_{t-1}, X_{t-2}, \\dots)=0.$$
</p>
</div>
')
cat('\\begin{keyconcepts}[The Autoregressive Distributed Lag Model]{14.4}
An ADL($p$,$q$) model assumes that a time series $Y_t$ can be represented by a linear function of $p$ of its lagged values and $q$ lags of another time series $X_t$:
\\begin{align*}
  Y_t =& \\, \\beta_0 + \\beta_1 Y_{t-1} + \\beta_2 Y_{t-2} + \\dots + \\beta_p Y_{t-p} \\\\ 
      &+ \\, \\delta_1 X_{t-1} + \\delta_2 X_{t-2} + \\dots + \\delta_q X_{t-q} X_{t-q} + u_t.
\\end{align*}
is an \\textit{autoregressive distributed lag model} with $p$ lags of $Y_t$ and $q$ lags of $X_t$ where $$E(u_t\\vert Y_{t-1}, Y_{t-2}, \\dots, X_{t-1}, X_{t-2}, \\dots)=0.$$
\\end{keyconcepts}
')
# 3-months Treasury bills interest rate
TB3MS <- xts(USMacroSWQ$TB3MS, USMacroSWQ$Date)["1960::2012"]

# 10-years Treasury bonds interest rate
TB10YS <- xts(USMacroSWQ$GS10, USMacroSWQ$Date)["1960::2012"]

# term spread
TSpread <- TB10YS - TB3MS
# reproduce Figure 14.2 (a) of the book
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)), 
     plot.type = "single", 
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Interest Rates")

# define function that transform years to class 'yearqtr'
YToYQTR <- function(years) {
  return(
      sort(as.yearqtr(sapply(years, paste, c("Q1", "Q2", "Q3", "Q4"))))
  )
}

# recessions
recessions <- YToYQTR(c(1961:1962, 1970, 1974:1975, 1980:1982, 1990:1991, 2001, 2007:2008))
          
# add color shading for recessions
xblocks(time(as.zoo(TB3MS)), 
        c(time(TB3MS) %in% recessions), 
        col = alpha("steelblue", alpha = 0.3))

# add a legend
legend("topright", 
       legend = c("TB3MS", "TB10YS"),
       col = c("darkred", "steelblue"),
       lwd = c(2, 2))

# reproduce Figure 14.2 (b) of the book
plot(as.zoo(TSpread), 
     col = "steelblue",
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     main = "Term Spread")

# add color shading for recessions
xblocks(time(as.zoo(TB3MS)), 
        c(time(TB3MS) %in% recessions), 
        col = alpha("steelblue", alpha = 0.3))
# convert growth and spread series to ts objects
GDPGrowth_ts <- ts(GDPGrowth, 
                  start = c(1960, 1), 
                  end = c(2013, 4), 
                  frequency = 4)

TSpread_ts <- ts(TSpread, 
                start = c(1960, 1), 
                end = c(2012, 4), 
                frequency = 4)

# join both ts objects
ADLdata <- ts.union(GDPGrowth_ts, TSpread_ts)
# estimate the ADL(2,1) model of GDP growth
GDPGR_ADL21 <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) + L(TSpread_ts), 
      start = c(1962, 1), end = c(2012, 4))

coeftest(GDPGR_ADL21, vcov. = sandwich)
# 2012:Q3 / 2012:Q4 data on GDP growth and term spread
subset <- window(ADLdata, c(2012, 3), c(2012, 4))

# ADL(2,1) GDP growth forecast for 2013:Q1
ADL21_forecast <- coef(GDPGR_ADL21) %*% c(1, subset[2, 1], subset[1, 1], subset[2, 2])
ADL21_forecast

# compute the forecast error
window(GDPGrowth_ts, c(2013, 1), c(2013, 1)) - ADL21_forecast
# estimate the ADL(2,2) model of GDP growth
GDPGR_ADL22 <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) 
                     + L(TSpread_ts) + L(TSpread_ts, 2), 
                     start = c(1962, 1), end = c(2012, 4))

coeftest(GDPGR_ADL22, vcov. = sandwich)
# ADL(2,2) GDP growth forecast for 2013:Q1
ADL22_forecast <- coef(GDPGR_ADL22) %*% c(1, subset[2, 1], subset[1, 1], subset[2, 2], subset[1, 2])
ADL22_forecast

# compute the forecast error
window(GDPGrowth_ts, c(2013, 1), c(2013, 1)) - ADL22_forecast
# compare adj. R2
c("Adj.R2 AR(2)" = summary(GDPGR_AR2)$r.squared,
  "Adj.R2 ADL(2,1)" = summary(GDPGR_ADL21)$r.squared,
  "Adj.R2 ADL(2,2)" = summary(GDPGR_ADL22)$r.squared)

# compare SER
c("SER AR(2)" = summary(GDPGR_AR2)$sigma,
  "SER ADL(2,1)" = summary(GDPGR_ADL21)$sigma,
  "SER ADL(2,2)" = summary(GDPGR_ADL22)$sigma)

# F-test on coefficients of term spread
linearHypothesis(GDPGR_ADL22, 
                 c("L(TSpread_ts)=0", "L(TSpread_ts, 2)=0"),
                 vcov. = sandwich)
cat('
<div class = "keyconcept" id="KC14.5">
<h3 class = "right"> Key Concept 14.5 </h3>
<h3 class = "left"> Stationarity </h3>
A time series $Y_t$ is stationary if its probability distribution is time independent, that is the joint distribution of $Y_{s+1}, Y_{s+2},\\dots,Y_{s+T}$ does not change as $s$ is varied, regardless of $T$.

Similarly, two time series $X_t$ and $Y_t$ are *jointly stationary* if the joint distribution of $(X_{s+1},Y_{s+1}, X_{s+2},Y_{s+2} \\dots, X_{s+T},Y_{s+T})$ does not depend on $s$, regardless of $T$.

Stationarity makes it easier to learn about the characteristics of past data.
</div>
')
cat('\\begin{keyconcepts}[Stationarity]{14.5}
A time series $Y_t$ is stationary if its probability distribution is time independent, that is the joint distribution of $Y_{s+1}, Y_{s+2},\\dots,Y_{s+T}$ does not change as $s$ is varied, regardless of $T$.\\newline

Similarly, two time series $X_t$ and $Y_t$ are \\textit{jointly stationary} if the joint distribution of $(X_{s+1},Y_{s+1}, X_{s+2},Y_{s+2} \\dots, X_{s+T}, Y_{s+T})$ does not depend on $s$, regardless of $T$.\\newline

In a probabilistic sense, stationarity means that information about how a time series evolves in the future is inherent to its past. If this is not the case, we cannot use the past of a series as a reliable guideline for its future.\\vspace{0.5cm}

Stationarity makes it easier to learn about the characteristics of past data.
\\end{keyconcepts}
')
cat('
<div class = "keyconcept" id="KC14.6">
<h3 class = "right"> Key Concept 14.6 </h3>
<h3 class = "left"> Time Series Regression with Multiple Predictors </h3>
The general time series regression model extends the ADL model such that multiple regressors and their lags are included. It uses $p$ lags of the dependent variable and $q_l$ lags of $l$ additional predictors where $l=1,\\dots,k$:

\\begin{equation}
  \\begin{aligned}
  Y_t =&  \\beta_0 + \\beta_1 Y_{t-1} + \\beta_2 Y_{t-2} + \\dots + \\beta_{p} Y_{t-p} \\\\
      &+  \\delta_{11} X_{1,t-1} + \\delta_{12} X_{1,t-2} + \\dots + \\delta_{1q} X_{1,t-q} \\\\
      &+  \\dots \\\\
      &+  \\delta_{k1} X_{k,t-1} + \\delta_{k2} X_{k,t-2} + \\dots + \\delta_{kq} X_{k,t-q} \\\\
      &+  u_t 
  \\end{aligned}
\\end{equation}

For estimation we make the following assumptions:

1. The error term $u_t$ has conditional mean zero given all regressors and their lags: $$E(u_t\\vert Y_{t-1}, Y_{t-2}, \\dots, X_{1,t-1}, X_{1,t-2} \\dots, X_{k,t-1}, X_{k,t-2}, \\dots)$$ This assumption is an extension of the conditional mean zero assumption used for AR and ADL models and guarantees that the general time series regression model stated above gives the best forecast of $Y_t$ given its lags, the additional regressors $X_{1,t},\\dots,X_{k,t}$ and their lags.  

2. The i.i.d. assumption for cross-sectional data is not (entirely) meaningful for time series data. We replace it by the following assumption witch consists of two parts:

    (a) The $(Y_{t}, X_{1,t}, \\dots, X_{k,t})$ have a stationary distribution (the "identically distributed" part of the i.i.d. assumption for cross-setional data). If this does not hold, forecasts *may* be biased and inference *can* be strongly misleading.    
  
    (b) $(Y_{t}, X_{1,t}, \\dots, X_{k,t})$ and $(Y_{t-j}, X_{1,t-j}, \\dots, X_{k,t-j})$ become independent as $j$ gets large (the "idependently" distributed part of the i.i.d. assumption for cross-sectional data). This assumption is also called *weak dependence*. It ensures that the WLLN and the CLT hold in large samples.
    
3. Large outliers are unlikely: $E(X_{1,t}^4), E(X_{2,t}^4), \\dots, E(X_{k,t}^4)$ and $E(Y_t^4)$ have nonzero, finite fourth moments.  

4. No perfect multicollinearity.

</div>
')
cat('\\begin{keyconcepts}[Time Series Regression with Multiple Predictors]{14.6}
The general time series regression model extends the ADL model such that multiple regressors and their lags are included. It uses $p$ lags of the dependent variable and $q_l$ lags of $l$ additional predictors where $l=1,\\dots,k$:
\\begin{equation}
  \\begin{aligned}
  Y_t =& \\, \\beta_0 + \\beta_1 Y_{t-1} + \\beta_2 Y_{t-2} + \\dots + \\beta_{p} Y_{t-p} \\\\
      +& \\, \\delta_{11} X_{1,t-1} + \\delta_{12} X_{1,t-2} + \\dots + \\delta_{1q} X_{1,t-q} \\\\
      +& \\, \\dots \\\\
      +& \\, \\delta_{k1} X_{k,t-1} + \\delta_{k2} X_{k,t-2} + \\dots + \\delta_{kq} X_{k,t-q} \\\\
      +& \\, u_t 
  \\end{aligned}
\\end{equation}

For estimation we make the following assumptions:\\newline

\\begin{enumerate}
\\item The error term $u_t$ has conditional mean zero given all regressors and their lags: $$E(u_t\\vert Y_{t-1}, Y_{t-2}, \\dots, X_{1,t-1}, X_{1,t-2} \\dots, X_{k,t-1}, X_{k,t-2}, \\dots)$$ This assumption is an extension of the conditional mean zero assumption used for AR and ADL models and guarantees that the general time series regression model stated above gives the best forecast of $Y_t$ given its lags, the additional regressors $X_{1,t},\\dots,X_{k,t}$ and their lags.  
\\item The i.i.d. assumption for cross-sectional data is not (entirely) meaningful for time series data. We replace it by the following assumption witch consists of two parts:\\newline
\\begin{itemize}
    \\item[(a)] The $(Y_{t}, X_{1,t}, \\dots, X_{k,t})$ have a stationary distribution (the "identically distributed" part of the i.i.d. assumption for cross-setional data). If this does not hold, forecasts may be biased and inference can be strongly misleading.    
  
    \\item[(b)] $(Y_{t}, X_{1,t}, \\dots, X_{k,t})$ and $(Y_{t-j}, X_{1,t-j}, \\dots, X_{k,t-j})$ become independent as $j$ gets large (the "idependently" distributed part of the i.i.d. assumption for cross-sectional data). This assumption is also called \\textit{weak dependence}. It ensures that the WLLN and the CLT hold in large samples.
\\end{itemize}
    
\\item Large outliers are unlikely: $E(X_{1,t}^4), E(X_{2,t}^4), \\dots, E(X_{k,t}^4)$ and $E(Y_t^4)$ have nonzero, finite fourth moments.  

\\item No perfect multicollinearity.
\\end{enumerate}
\\end{keyconcepts}
')
cat('
<div class = "keyconcept" id="KC14.7">
<h3 class = "right"> Key Concept 14.7 </h3>
<h3 class = "left"> Granger Causality Tests </h3>
The Granger causality test @granger1969 is an $F$ test of the null hypothesis that *all* lags of a variable $X$ included in a time series regression model do not have predictive power for $Y_t$. The Granger causality test does not test whether $X$ actually *causes* $Y$ but whether the included lags are informative in terms of predicting $Y$.
</div>
')
cat('\\begin{keyconcepts}[Granger Causality Tests]{14.7}
The Granger causality test \\citep{granger1969} is an $F$ test of the null hypothesis that \\textit{all} lags of a variable $X$ included in a time series regression model do not have predictive power for $Y_t$. The Granger causality test does not test whether $X$ actually \\textit{causes} $Y$ but whether the included lags are informative in terms of predicting $Y$.
\\end{keyconcepts}
')
# set seed
set.seed(1234)

# simulate the time series
Y <- arima.sim(list(order = c(2, 0, 0), ar = c(0.2, 0.2)),  n = 200)

# estimate an AR(2) model using 'arima()', see ?arima
model <- arima(Y, order = c(2, 0, 0))

# compute points forecasts and prediction intervals for the next 25 periods
fc <- forecast(model, h = 25, level = seq(5, 99, 10))

# plot a fan chart
plot(fc, 
     main = "Forecast Fan Chart for AR(2) Model of Simulated Data", 
     showgap = F, 
     fcol = "red",
     flty = 2)
# compute BIC for AR model objects of class 'dynlm'
BIC <- function(model) {
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  npar <- length(model$coef)
  
  return(
    round(c("p" = npar - 1,
          "BIC" = log(ssr/t) + npar * log(t)/t,
          "R2" = summary(model)$r.squared), 4)
  )
}
# apply the BIC() to an intercept-only model of GDP growth
BIC(dynlm(ts(GDPGR_level) ~ 1))

# loop BIC over models of different orders
order <- 1:6

BICs <- sapply(order, function(x) 
        "AR" = BIC(dynlm(ts(GDPGR_level) ~ L(ts(GDPGR_level), 1:x))))

BICs
# select the AR model with the smallest BIC
BICs[, which.min(BICs[2, ])]
# loop 'BIC()' over multiple ADL models 
order <- 1:12

BICs <- sapply(order, function(x) 
         BIC(dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts, 1:x) + L(TSpread_ts, 1:x), 
                   start = c(1962, 1), end = c(2012, 4))))

BICs
# select the ADL model with the smallest BIC
BICs[, which.min(BICs[2, ])]
# simulate and plot random walks starting at 0
set.seed(1)

RWs <- ts(replicate(n = 4, 
            arima.sim(model = list(order = c(0, 1 ,0)), n = 100)))

matplot(RWs, 
        type ="l", 
        col = c("steelblue", "darkgreen", "darkred", "orange"), 
        lty = 1, 
        lwd = 2,
        main = "Four Random Walks",
        xlab = "Time",
        ylab = "Value")
# simulate and plot random walks with drift
set.seed(1)

RWsd <- ts(replicate(n = 4, 
           arima.sim(model = list(order = c(0, 1, 0)), n = 100)))

matplot(RWsd, 
        type="l", 
        col = c("steelblue", "darkgreen", "darkred", "orange"), 
        lty = 1, 
        lwd = 2,
        main = "Four Random Walks with Drift",
        xlab = "Time",
        ylab = "Value")
# plot spurious relationship
matplot(RWs[, c(2, 3)], 
        lty = 1,
        lwd = 2,
        type = "l",
        col = c("darkgreen", "darkred"),
        xlab = "Time",
        ylab = "",
        main = "A Spurious Relationship")    
# estimate spurious AR model
summary(dynlm(RWs[, 2] ~ L(RWs[, 3])))$coefficients
# plot U.S. unemployment rate & Japanese industrial production
plot(merge(as.zoo(USUnemp), as.zoo(JPIndProd)), 
     plot.type = "single", 
     col = c("darkred", "steelblue"),
     lwd = 2,
     xlab = "Date",
     ylab = "",
     main = "Spurious Regression: Macroeconomic Time series")

# add a legend
legend("topleft", 
       legend = c("USUnemp", "JPIndProd"),
       col = c("darkred", "steelblue"),
       lwd = c(2, 2))
# estimate regression using data from 1962 to 1985
SR_Unemp1 <- dynlm(ts(USUnemp["1962::1985"]) ~ ts(JPIndProd["1962::1985"]))
coeftest(SR_Unemp1, vcov = sandwich)
# Estimate regression using data from 1986 to 2012
SR_Unemp2 <- dynlm(ts(USUnemp["1986::2012"]) ~ ts(JPIndProd["1986::2012"]))
coeftest(SR_Unemp2, vcov = sandwich)
cat('
<div class = "keyconcept" id="KC14.8">
<h3 class = "right"> Key Concept 14.8 </h3>
<h3 class = "left"> The ADF Test for a Unit Root </h3>
<p>
Consider the regression
\\begin{align}
  \\Delta Y_t = \\beta_0 + \\delta Y_{t-1} + \\gamma_1 \\Delta_1 Y_{t-1} + \\gamma_2 \\Delta Y_{t-2} + \\dots + \\gamma_p \\Delta Y_{t-p} + u_t. (\\#eq:ADFreg1)
\\end{align}

The ADF test for a unit autoregressive root tests the hypothesis $H_0: \\delta = 0$ (stochastic trend) against the one-sided alternative $H_1: \\delta < 0$ (stationarity) using the usual OLS $t$-statistic.

If it is assumed that $Y_t$ is stationary around a deterministic linear time trend, the model is augmented by the regressor $t$:
\\begin{align}
  \\Delta Y_t = \\beta_0 + at + \\delta Y_{t-1} + \\gamma_1 \\Delta_1 Y_{t-1} + \\gamma_2 \\Delta Y_{t-2} + \\dots + \\gamma_p \\Delta Y_{t-p} + u_t,  (\\#eq:ADFreg2)
\\end{align}
where again $H_0: \\delta = 0$ is tested against $H_1: \\delta < 0$.

The optimal lag length $p$ can be estimated using information criteria. In \\@ref(eq:ADFreg1), $p=0$ (no lags of $\\Delta Y_t$ are used as regressors) corresponds to a simple AR($1$).

Under the null, the $t$-statistic corresponding to $H_0: \\delta = 0$ does not have a normal distribution. The critical values can only be obtained from simulation and differ for regressions \\@ref(eq:ADFreg1) and \\@ref(eq:ADFreg2) since the distribution of the ADF test statistic is sensitive to the deterministic components included in the regression.
</p>
</div>
')
cat('\\begin{keyconcepts}[The ADF Test for a Unit Root]{14.8}
Consider the regression
\\begin{align}
  \\Delta Y_t = \\beta_0 + \\delta Y_{t-1} + \\gamma_1 \\Delta_1 Y_{t-1} + \\gamma_2 \\Delta Y_{t-2} + \\dots + \\gamma_p \\Delta Y_{t-p} + u_t. \\label{eq:ADFreg1}
\\end{align}

The ADF test for a unit autoregressive root tests the hypothesis $H_0: \\delta = 0$ (stochastic trend) against the one-sided alternative $H_1: \\delta < 0$ (stationarity) using the usual OLS $t$-statistic.\\newline

If it is assumed that $Y_t$ is stationary around a deterministic linear time trend, the model is augmented by the regressor $t$:
\\begin{align}
  \\Delta Y_t = \\beta_0 + at + \\delta Y_{t-1} + \\gamma_1 \\Delta_1 Y_{t-1} + \\gamma_2 \\Delta Y_{t-2} + \\dots + \\gamma_p \\Delta Y_{t-p} + u_t,  \\label{eq:ADFreg2}
\\end{align}
where again $H_0: \\delta = 0$ is tested against $H_1: \\delta < 0$.\\newline

The optimal lag length $p$ can be estimated using information criteria. In (\\ref{eq:ADFreg1}), $p=0$ (no lags of $\\Delta Y_t$ are used as regressors) corresponds to a simple AR($1$).\\newline

Under the null, the $t$-statistic corresponding to $H_0: \\delta = 0$ does not have a normal distribution. The critical values can only be obtained from simulation and differ for regressions (\\ref{eq:ADFreg1}) and (\\ref{eq:ADFreg2}) since the distribution of the ADF test statistic is sensitive to the deterministic components included in the regression.
\\end{keyconcepts}
')
# repetitions
N <- 1000

# observations
n <- 1000

# define drift and trend 
drift <- 0.5
trend <- 1:n

# simulate N random walks with drift 
RWD <- ts(replicate(n = N, 
            (n-1) * drift + arima.sim(model = list(order = c(0, 1, 0)),
                                      n = n - 1)))

# compute ADF test statistics and store them in 'ADFD'
ADFD <- numeric(N)

for(i in 1:ncol(RWD)) {
  ADFD[i] <- summary(
    dynlm(diff(RWD[, i], 1) ~ L(RWD[, i], 1)))$coef[2, 3]
}

# simulate N random walks with drift + trend
RWDT <- ts(replicate(n = N, 
                    trend + drift + arima.sim(model = list(order = c(0, 1, 0)), 
                                              n = n - 1)))

# compute ADF test statistics and store them in 'ADFDT'
ADFDT <- numeric(N)

for(i in 1:ncol(RWDT)) {
  ADFDT[i] <- summary(
    dynlm(diff(RWDT[, i], 1) ~ L(RWDT[, i], 1) + trend(RWDT[, i], scale = F))
  )$coef[2, 3]
}
# estimate quantiles for ADF regression with a drift
round(quantile(ADFD, c(0.1, 0.05, 0.01)), 2)

# estimate quantiles for ADF regression with drift and trend
round(quantile(ADFDT, c(0.1,0.05,0.01)),2)
# plot standard normal density
curve(dnorm(x), 
      from = -6, to = 3, 
      ylim = c(0, 0.6), 
      lty = 2,
      ylab = "Density",
      xlab = "t-Statistic",
      main = "Distributions of ADF Test Statistics",
      col = "darkred", 
      lwd = 2)

# plot density estimates of both Dickey-Fuller distributions
lines(density(ADFD), lwd = 2, col = "darkgreen")
lines(density(ADFDT), lwd = 2, col = "blue")

# add a legend
legend("topleft", 
       c("N(0,1)", "Drift", "Drift+Trend"),
       col = c("darkred", "darkgreen", "blue"),
       lty = c(2, 1, 1),
       lwd = 2)
# generate log GDP series
LogGDP <- ts(log(GDP["1962::2012"]))

# estimate the model
coeftest(
  dynlm(diff(LogGDP) ~ trend(LogGDP, scale = F) + L(LogGDP) 
                     + diff(L(LogGDP)) + diff(L(LogGDP), 2)))
# test for unit root in GDP using 'ur.df()' from the package 'urca'
summary(ur.df(LogGDP, 
              type = "trend", 
              lags = 2, 
              selectlags = "Fixed"))
cat('
<div class = "keyconcept" id="KC14.9">
<h3 class = "right"> Key Concept 14.9 </h3>
<h3 class = "left"> The QLR Test for Coefficient Stability </h3>
<p>
The QLR test can be used to test for a break in the population regression function if the date of the break is unknown. The QLR test statistic is the largest (Chow) $F(\\tau)$ statistic computed over a range of eligible break dates  $\\tau_0 \\leq \\tau \\leq \\tau_1$:
\\begin{align}
  QLR = \\max\\left[F(\\tau_0),F(\\tau_0 +1),\\dots,F(\\tau_1)\\right]. (\\#eq:QLRstatistic)
\\end{align}
</p>
The most important properties are:

+ The QLR test can be applied to test whether a subset of the coefficients in the population regression function breaks but the test also rejects if there is a slow evolution of the regression function. 

+ When there is a single discrete break in the population regression function that lying at a date within the range tested, the $QLR$ test statistic is $F(\\widehat{\\tau})$ and $\\widehat{\\tau}/T$ is a consistent estimator of fraction of the sample at which the break is.

+ The large-sample distribution of $QLR$ depends on $q$, the number of restrictions being tested and both ratios of end points to the sample size, $\\tau_0/T, \\tau_1/T$. 

+ Similar to the ADF test, the large-sample distribution of $QLR$ is nonstandard. Critical values are presented in Table 14.5 of the book.

</div>
')
cat('\\begin{keyconcepts}[The QLR Test for Coefficient Stability]{14.9}
The QLR test can be used to test for a break in the population regression function if the date of the break is unknown. The QLR test statistic is the largest (Chow) $F(\\tau)$ statistic computed over a range of eligible break dates  $\\tau_0 \\leq \\tau \\leq \\tau_1$:
\\begin{align}
  QLR = \\max\\left[F(\\tau_0),F(\\tau_0 +1),\\dots,F(\\tau_1)\\right]. \\label{eq:QLRstatistic}
\\end{align}\\vspace{0.5cm}
The most important properties are:
\\begin{itemize}
\\item The QLR test can be applied to test whether a subset of the coefficients in the population regression function breaks but the test also rejects if there is a slow evolution of the regression function. 
When there is a single discrete break in the population regression function that lying at a date within the range tested, the $QLR$ test statistic is $F(\\widehat{\\tau})$ and $\\widehat{\\tau}/T$ is a consistent estimator of fraction of the sample at which the break is.
\\item The large-sample distribution of $QLR$ depends on $q$, the number of restrictions being tested and both ratios of end points to the sample size, $\\tau_0/T, \\tau_1/T$. 
\\item Similar to the ADF test, the large-sample distribution of $QLR$ is nonstandard. Critical values are presented in Table 14.5 of the book.
\\end{itemize}
\\end{keyconcepts}
')
# set up a range of possible break dates
tau <- seq(1970, 2005, 0.25)

# initialize vector of F-statistics
Fstats <- numeric(length(tau))

# estimation loop over break dates
for(i in 1:length(tau)) {

  # set up dummy variable
  D <- time(GDPGrowth_ts) > tau[i]

  # estimate ADL(2,2) model with intercations
  test <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) + 
                D*L(TSpread_ts) + D*L(TSpread_ts, 2),
                start = c(1962, 1), 
                end = c(2012, 4))
  
  # compute and save the F-statistic
  Fstats[i] <- linearHypothesis(test, 
                                c("DTRUE=0", "DTRUE:L(TSpread_ts)", 
                                  "DTRUE:L(TSpread_ts, 2)"),
                                vcov. = sandwich)$F[2]

}
# identify QLR statistic
QLR <- max(Fstats)
QLR
# identify the time period where the QLR-statistic is observed
as.yearqtr(tau[which.max(Fstats)])
# series of F-statistics
Fstatsseries <- ts(Fstats, 
                   start = tau[1], 
                   end = tau[length(tau)], 
                   frequency = 4)

# plot the F-statistics 
plot(Fstatsseries, 
     xlim = c(1960, 2015),
     ylim = c(1, 7.5),
     lwd = 2,
     col = "steelblue",
     ylab = "F-Statistic",
     xlab = "Break Date",
     main = "Testing for a Break in GDP ADL(2,2) Regression at Different Dates")

# dashed horizontal lines for critical values and QLR statistic
abline(h = 4.71, lty = 2)
abline(h = 6.02, lty = 2)
segments(0, QLR, 1980.75, QLR, col = "darkred")
text(2010, 6.2, "1% Critical Value")
text(2010, 4.9, "5% Critical Value")
text(1980.75, QLR+0.2, "QLR Statistic")
cat('
<div class = "keyconcept" id="KC14.10">
<h3 class = "right"> Key Concept 14.10 </h3>
<h3 class = "left"> Pseudo Out-of-Sample Forecasting </h3>
1. Divide the sample data into $s=T-P$ and $P$ subsequent observations. The $P$ observations are used as pseudo-out-of-sample observations.

2. Estimate the model using the first $s$ observations. 

3. Compute the pseudo-forecast $\\overset{\\sim}{Y}_{s+1\\vert s}$.

4. Compute the pseudo-forecast-error $\\overset{\\sim}{u}_{s+1} = Y_{s+1} - \\overset{\\sim}{Y}_{s+1\\vert s}$.

5. Repeat steps 2 trough 4 for all remaining pseudo-out-of-sample dates.
</div>
')
cat('\\begin{keyconcepts}[Pseudo Out-of-Sample Forecasting]{14.10}
\\begin{enumerate}
\\item Divide the sample data into $s=T-P$ and $P$ subsequent observations. The $P$ observations are used as pseudo-out-of-sample observations.
\\item Estimate the model using the first $s$ observations. 
\\item Compute the pseudo-forecast $\\overset{\\sim}{Y}_{s+1\\vert s}$.
\\item Compute the pseudo-forecast-error $\\overset{\\sim}{u}_{s+1} = Y_{s+1} - \\overset{\\sim}{Y}_{s+1\\vert s}$.
\\item Repeat steps 2 trough 4 for all remaining pseudo-out-of-sample dates, i.e., reestimate the model at each date.
\\end{enumerate}
\\end{keyconcepts}
')
# end of sample dates
EndOfSample <- seq(2002.75, 2012.5, 0.25)

# initialize matrix forecasts
forecasts <- matrix(nrow = 1, 
                    ncol = length(EndOfSample))

# initialize vector SER
SER  <- numeric(length(EndOfSample))

# estimation loop over end of sample dates
for(i in 1:length(EndOfSample)) {

  # estimate ADL(2,2) model
  m <- dynlm(GDPGrowth_ts ~ L(GDPGrowth_ts) + L(GDPGrowth_ts, 2) 
                          + L(TSpread_ts) + L(TSpread_ts, 2), 
                start = c(1981, 1), 
                end = EndOfSample[i])
  
  SER[i] <- summary(m)$sigma
  
  # sample data for one-period ahead forecast
  s <- window(ADLdata, EndOfSample[i] - 0.25, EndOfSample[i])
  
  # compute forecast
  forecasts[i] <- coef(m) %*% c(1, s[1, 1], s[2, 1], s[1, 2], s[2, 2]) 
}
# compute psuedo-out-of-sample forecast errors
POOSFCE <- c(window(GDPGrowth_ts, c(2003, 1), c(2012, 4))) - forecasts
# series of pseudo-out-of-sample forecasts
PSOSSFc <- ts(c(forecasts), 
              start = 2003, 
              end = 2012.75, 
              frequency = 4)

# plot the GDP growth time series
plot(window(GDPGrowth_ts, c(2003, 1), c(2012, 4)),
     col = "steelblue",
     lwd = 2,
     ylab = "Percent",
     main = "Pseudo-Out-Of-Sample Forecasts of GDP Growth")

# add the series of pseudo-out-of-sample forecasts
lines(PSOSSFc, 
      lwd = 2, 
      lty = 2)

# shade area between curves (the pseudo forecast error)
polygon(c(time(PSOSSFc), rev(time(PSOSSFc))), 
        c(window(GDPGrowth_ts, c(2003, 1), c(2012, 4)), rev(PSOSSFc)),
        col = alpha("blue", alpha = 0.3),
        border = NA)

# add a legend
legend("bottomleft", 
       lty = c(1, 2, 1),
       lwd = c(2, 2, 10),
       col = c("steelblue", "black", alpha("blue", alpha = 0.3)), 
       legend = c("Actual GDP growth rate",
         "Forecasted GDP growth rate",
         "Pseudo forecast Error"))
# SER of ADL(2,2) mode using data from 1981:Q1 - 2002:Q4
SER[1]
# compute root mean squared forecast error
sd(POOSFCE)
# test if mean forecast error is zero
t.test(POOSFCE)
# plot logarithm of dividend yield series
plot(StockReturns[, 2], 
     col = "steelblue", 
     lwd = 2, 
     ylab = "Logarithm", 
     main = "Dividend Yield for CRSP Index")
# test for unit root in GDP using 'ur.df()' from the package 'urca'
summary(ur.df(window(StockReturns[, 2], 
                     c(1960,1), 
                     c(2002, 12)), 
              type = "drift", 
              lags = 0))
# ADL(1,1) (1st difference of log dividend yield)
CRSP_ADL_1 <- dynlm(ExReturn ~ L(ExReturn) + d(L(ln_DivYield)), 
                    data = StockReturns,
                    start = c(1960, 1), end = c(2002, 12))

# ADL(2,2) (1st & 2nd differences of log dividend yield)
CRSP_ADL_2 <- dynlm(ExReturn ~ L(ExReturn) + L(ExReturn, 2) 
                    + d(L(ln_DivYield)) + d(L(ln_DivYield, 2)), 
                    data = StockReturns,
                    start = c(1960, 1), end = c(2002, 12))

# ADL(1,1) (level of log dividend yield)
CRSP_ADL_3 <- dynlm(ExReturn ~ L(ExReturn) + L(ln_DivYield),
                    data = StockReturns,
                    start = c(1960, 1), end = c(1992, 12))
# gather robust standard errors
rob_se_CRSP_ADL <- list(sqrt(diag(sandwich(CRSP_ADL_1))),
                        sqrt(diag(sandwich(CRSP_ADL_2))),
                        sqrt(diag(sandwich(CRSP_ADL_3))))
## stargazer(CRSP_ADL_1, CRSP_ADL_2, CRSP_ADL_3,
##   title = "ADL Models of Monthly Excess Stock Returns",
##   header = FALSE,
##   type = "latex",
##   column.sep.width = "-5pt",
##   no.space = T,
##   digits = 3,
##   column.labels = c("ADL(1,1)", "ADL(2,2)", "ADL(1,1)"),
##   dep.var.caption  = "Dependent Variable: Excess returns on the CSRP value-weighted index",
##   dep.var.labels.include = FALSE,
##   covariate.labels = c("$excess return_{t-1}$",
##                        "$excess return_{t-2}$",
##                        "$1^{st} diff log(dividend yield_{t-1})$",
##                        "$1^{st} diff log(dividend yield_{t-2})$",
##                        "$log(dividend yield_{t-1})$",
##                        "Constant"),
##   se = rob_se_CRSP_ADL)
stargazer(CRSP_ADL_1, CRSP_ADL_2, CRSP_ADL_3,
  header = FALSE, 
  type = "html",
  digits = 3, 
  column.labels = c("ADL(1,1)", "ADL(2,2)", "ADL(1,1)"),
  dep.var.caption  = "Dependent Variable: Excess Returns on the CSRP Value-Weighted Index",
  dep.var.labels.include = FALSE,
  covariate.labels = c("$excess return_{t-1}$", "$excess return_{t-2}$", "$1^{st} diff log(dividend yield_{t-1})$", "$1^{st} diff log(dividend yield_{t-2})$", "$log(dividend yield_{t-1})$", "Constant"),
  se = rob_se_CRSP_ADL,
  no.space = T
  )

stargazer_html_title("ADL Models of Monthly Excess Stock Returns", "adlmomesr")
stargazer(CRSP_ADL_1, CRSP_ADL_2, CRSP_ADL_3,
  title = "\\label{tab:adlmomesr} ADL Models of Monthly Excess Stock Returns",
  header = FALSE, 
  type = "latex",
  column.sep.width = "-5pt",
  no.space = T,
  digits = 3, 
  column.labels = c("ADL(1,1)", "ADL(2,2)", "ADL(1,1)"),
  dep.var.caption  = "Dependent Variable: Excess returns on the CSRP Value-Weighted Index",
  dep.var.labels.include = FALSE,
  covariate.labels = c("$excess return_{t-1}$", "$excess return_{t-2}$", "$1^{st} diff log(dividend yield_{t-1})$", "$1^{st} diff log(dividend yield_{t-2})$", "$log(dividend yield_{t-1})$", "Constant"),
  se = rob_se_CRSP_ADL
  ) 
# end of sample dates
EndOfSample <- as.numeric(window(time(StockReturns), c(1992, 12), c(2002, 11)))

# initialize matrix  forecasts
forecasts <- matrix(nrow = 2, 
                    ncol = length(EndOfSample))

# estimation loop over end of sample dates
for(i in 1:length(EndOfSample)) {

  # estimate model (3)
  mod3 <- dynlm(ExReturn ~ L(ExReturn) + L(ln_DivYield), data = StockReturns, 
                start = c(1960, 1), 
                end = EndOfSample[i])
  
  # estimate intercept only model
  modconst <- dynlm(ExReturn ~ 1, data = StockReturns, 
                start = c(1960, 1), 
                end = EndOfSample[i])
  
  # sample data for one-period ahead forecast
  t <- window(StockReturns, EndOfSample[i], EndOfSample[i])
  
  # compute forecast
  forecasts[, i] <- c(coef(mod3) %*% c(1, t[1], t[2]), coef(modconst))
                     
}
# gather data
d <- cbind("Excess Returns" = c(window(StockReturns[,1], c(1993, 1), c(2002, 12))),
           "Model (3)" = forecasts[1,], 
           "Intercept Only" = forecasts[2,], 
           "Always Zero" =  0)

# Compute RMSFEs
c("ADL model (3)" = sd(d[, 1] - d[, 2]),
  "Intercept-only model" = sd(d[, 1] - d[, 3]),
  "Always zero" = sd(d[,1] - d[, 4]))
