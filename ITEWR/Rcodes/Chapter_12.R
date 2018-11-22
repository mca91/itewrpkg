
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
# load the data set and get an overview
library(AER)
data("CigarettesSW")
summary(CigarettesSW)
# compute real per capita prices
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

#  compute the sales tax
CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)

# check the correlation between sales tax and price
cor(CigarettesSW$salestax, CigarettesSW$price)

# generate a subset for the year 1995
c1995 <- subset(CigarettesSW, year == "1995")
# perform the first stage regression
cig_s1 <- lm(log(rprice) ~ salestax, data = c1995)

coeftest(cig_s1, vcov = vcovHC, type = "HC1")
# inspect the R^2 of the first stage regression
summary(cig_s1)$r.squared
# store the predicted values
lcigp_pred <- cig_s1$fitted.values
# run the stage 2 regression
cig_s2 <- lm(log(c1995$packs) ~ lcigp_pred)
coeftest(cig_s2, vcov = vcovHC)
# perform TSLS using 'ivreg()'
cig_ivreg <- ivreg(log(packs) ~ log(rprice) | salestax, data = c1995)

coeftest(cig_ivreg, vcov = vcovHC, type = "HC1")
cat('
<div class = "keyconcept" id="KC12.1">
<h3 class = "right"> Key Concept 12.1 </h3>
<h3 class = "left"> The General Instrumental Variables Regression Model and Terminology </h3>

\\begin{align}
  Y_i = \\beta_0 + \\beta_1 X_{1i} + \\dots + \\beta_k X_{ki} + \\beta_{k+1} W_{1i} + \\dots + \\beta_{k+r} W_{ri} + u_i, (\\#eq:givmodel)
\\end{align}

with $i=1,\\dots,n$ is the general instrumental variables regression model where

- $Y_i$ is the dependent variable

- $\\beta_0,\\dots,\\beta_{k+1}$ are $1+k+r$ unknown regression coefficients

- $X_{1i},\\dots,X_{ki}$ are $k$ endogenous regressors 

- $W_{1i},\\dots,W_{ri}$ are $r$ exogenous regressors which are uncorrelated with $u_i$

- $u_i$ is the error term

- $Z_{1i},\\dots,Z_{mi}$ are $m$ instrumental variables

The coefficients are overidentified if $m>k$. If $m<k$, the coefficients are underidentified and when $m=k$ they are exactly identified. For estimation of the IV regression model we require exact identification or overidentification.

</div>
')
cat('\\begin{keyconcepts}[The General Instrumental Variables Regression Model and Terminology]{12.1}
\\begin{align}
  Y_i = \\beta_0 + \\beta_1 X_{1i} + \\dots + \\beta_k X_{ki} + \\beta_{k+1} W_{1i} + \\dots + \\beta_{k+r} W_{ri} + u_i, \\label{eq:givmodel}
\\end{align}
with $i=1,\\dots,n$ is the general instrumental variables regression model where\\newline

\\begin{itemize}
\\item $Y_i$ is the dependent variable
\\item $\\beta_1,\\dots,\\beta_{k+r}$ are $1+k+r$ unknown regression coefficients
\\item $X_{1i},\\dots,X_{ki}$ are $k$ endogenous regressors 
\\item $W_{1i},\\dots,W_{ri}$ are $r$ exogenous regressors which are uncorrelated with $u_i$
\\item $u_i$ is the error term
\\item $Z_{1i},\\dots,Z_{mi}$ are $m$ instrumental variables
\\end{itemize}\\vspace{0.5cm}

The coefficients are overidentified if $m>k$. If $m<k$, the coefficients are underidentified and when $m=k$ they are exactly identified. For estimation of the IV regression model we require exact identification or overidentification.
\\end{keyconcepts}
')
cat('
<div class = "keyconcept" id="KC12.2">
<h3 class = "right"> Key Concept 12.2 </h3>
<h3 class = "left"> Two-Stage Least Squares </h3>

Similarly to the simple IV regression model, the general IV model \\@ref(eq:givmodel) can be estimated using the two-stage least squares estimator:

1. **First-stage regression(s)** 
    
    Run an OLS regression for each of the endogenous variables ($X_{1i},\\dots,X_{ki}$) on all instrumental variables ($Z_{1i},\\dots,Z_{mi}$), all exogenous variables ($W_{1i},\\dots,W_{ri}$) and an intercept. Compute the fitted values ($\\widehat{X}_{1i},\\dots,\\widehat{X}_{ki}$). 

2. **Second-stage regression** 

    Regress the dependent variable on the predicted values of all endogenous regressors, all exogenous variables and an intercept using OLS. This gives $\\widehat{\\beta}_{0}^{TSLS},\\dots,\\widehat{\\beta}_{k+r}^{TSLS}$, the TSLS estimates of the model coefficients.
</div>
')
cat('\\begin{keyconcepts}[Two-Stage Least Squares]{12.2}
Similarly to the simple IV regression model, the general IV model (\\ref{eq:givmodel}) can be estimated using the two-stage least squares estimator:\\newline

\\begin{itemize}
\\item \\textbf{First-stage regression(s)}\\newline Run an OLS regression for each of the endogenous variables ($X_{1i},\\dots,X_{ki}$) on all instrumental variables ($Z_{1i},\\dots,Z_{mi}$), all exogenous variables ($W_{1i},\\dots,W_{ri}$) and an intercept. Compute the fitted values ($\\widehat{X}_{1i},\\dots,\\widehat{X}_{ki}$).\\newline 
\\item \\textbf{Second-stage regression}\\newline Regress the dependent variable on the predicted values of all endogenous regressors, all exogenous variables and an intercept using OLS. This gives $\\widehat{\\beta}_{0}^{TSLS},\\dots,\\widehat{\\beta}_{k+r}^{TSLS}$, the TSLS estimates of the model coefficients.
\\end{itemize}
\\end{keyconcepts}
')
cat('
<div class = "keyconcept" id="KC12.3">
<h3 class = "right"> Key Concept 12.3 </h3>
<h3 class = "left"> Two Conditions for Valid Instruments </h3>

For $Z_{1i},\\dots,Z_{mi}$ to be a set of valid instruments, the following two conditions must be fulfilled:

1. **Instrument Relevance**: 

    if there are $k$ endogenous variables, $r$ exogenous variables and $m\\geq k$ instruments $Z$ and the $\\widehat{X}_{1i}^*,\\dots,\\widehat{X}_{ki}^*$ are the predicted values from the $k$ population first stage regressions, it must hold that $$(\\widehat{X}_{1i}^*,\\dots,\\widehat{X}_{ki}^*, W_{1i}, \\dots, W_{ri},1)$$ are not perfectly multicollinear. $1$ denotes the constant regressor which equals $1$ for all observations.

    *Note*: If there is only one endogenous regressor $X_i$, there must be at least one non-zero coefficient on the $Z$ and the $W$ in the population regression for this condition to be valid: if all of the coefficients are zero, all the $\\widehat{X}^*_i$ are just the mean of $X$ such that there is perfect multicollinearity.

2. **Instrument Exogeneity**:

    All $m$ instruments must be uncorrelated with the error term,

    $$\\rho_{Z_{1i},u_i} = 0,\\dots,\\rho_{Z_{mi},u_i} = 0.$$

</div>
')
cat('\\begin{keyconcepts}[Two Conditions for Valid Instruments]{12.3}
For $Z_{1i},\\dots,Z_{mi}$ to be a set of valid instruments, the following two conditions must be fulfilled:\\newline

\\begin{enumerate}
\\item \\textbf{Instrument Relevance}\\newline If there are $k$ endogenous variables, $r$ exogenous variables and $m\\geq k$ instruments $Z$ and the $\\widehat{X}_{1i}^*,\\dots,\\widehat{X}_{ki}^*$ are the predicted values from the $k$ population first stage regressions, it must hold that $$(\\widehat{X}_{1i}^*,\\dots,\\widehat{X}_{ki}^*, W_{1i}, \\dots, W_{ri},1)$$ are not perfectly multicollinear. $1$ denotes the constant regressor which equals $1$ for all observations.\\newline

\\textit{Note}: If there is only one endogenous regressor $X_i$, there must be at least one non-zero coefficient on the $Z$ and the $W$ in the population regression for this condition to be valid: if all of the coefficients are zero, all the $\\widehat{X}^*_i$ are just the mean of $X$ such that there is perfect multicollinearity.\\newline

\\item \\textbf{Instrument Exogeneity}\\newline
All $m$ instruments must be uncorrelated with the error term, $$\\rho_{Z_{1i},u_i} = 0,\\dots,\\rho_{Z_{mi},u_i} = 0.$$
\\end{enumerate}
\\end{keyconcepts}
')
cat('
<div class = "keyconcept" id="KC12.4">
<h3 class = "right"> Key Concept 12.4 </h3>
<h3 class = "left"> The IV Regression Assumptions </h3>

For the general IV regression model in Key Concept 12.1 we assume the following:

1. $E(u_i\\vert W_{1i}, \\dots, W_{ri}) = 0.$

2. $(X_{1i},\\dots,X_{ki},W_{1i},\\dots,W_{ri},Z_{1i},\\dots,Z_{mi})$ are i.i.d. draws from their joint distribution.

3. All variables have nonzero finite fourth moments, i.e., outliers are unlikely.

4. The $Z$s are valid instruments (see Key Concept 12.3).

</div>
')
cat('\\begin{keyconcepts}[The IV Regression Assumptions]{12.4}
For the general IV regression model in Key Concept 12.1 we assume the following:\\newline

\\begin{enumerate}
\\item $E(u_i\\vert W_{1i}, \\dots, W_{ri}) = 0.$
\\item $(X_{1i},\\dots,X_{ki},W_{1i},\\dots,W_{ri},Z_{1i},\\dots,Z_{mi})$ are i.i.d. draws from their joint distribution.
\\item All variables have nonzero finite fourth moments, i.e., outliers are unlikely.
\\item The $Z$s are valid instruments (see Key Concept 12.3).
\\end{enumerate}

\\end{keyconcepts}
')
# add rincome to the dataset
CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)

c1995 <- subset(CigarettesSW, year == "1995")
# estimate the model
cig_ivreg2 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | log(rincome) + 
                    salestax, data = c1995)

coeftest(cig_ivreg2, vcov = vcovHC, type = "HC1")
# add cigtax to the data set
CigarettesSW$cigtax <- with(CigarettesSW, tax/cpi)

c1995 <- subset(CigarettesSW, year == "1995")
# estimate the model
cig_ivreg3 <- ivreg(log(packs) ~ log(rprice) + log(rincome) | 
                    log(rincome) + salestax + cigtax, data = c1995)

coeftest(cig_ivreg3, vcov = vcovHC, type = "HC1")
cat('
<div class = "keyconcept" id="KC12.5">
<h3 class = "right"> Key Concept 12.5 </h3>
<h3 class = "left"> A Rule of Thumb for Checking for Weak Instruments </h3>

Consider the case of a single endogenous regressor $X$ and $m$ instruments $Z_1,\\dots,Z_m$. If the coefficients on all instruments in the population first-stage regression of a TSLS estimation are zero, the instruments do not explain any of the variation in the $X$ which clearly violates assumption 1 of Key Concept 12.2. Although the latter case is unlikely to be encountered in practice, we should ask ourselves "to what extent" the assumption of instrument relevance should be fulfilled. 

While this is hard to answer for general IV regression, in the case of a *single* endogenous regressor $X$ one may use the following rule of thumb:

Compute the $F$-statistic which corresponds to the hypothesis that the coefficients on $Z_1,\\dots,Z_m$ are all zero in the first-stage regression. If the $F$-statistic is less than $10$, the instruments are weak such that the TSLS estimate of the coefficient on $X$ is biased and no valid statistical inference about its true value can be made. See also Appendix 12.5 of the book.

</div>
')
cat('\\begin{keyconcepts}[A Rule of Thumb for Checking for Weak Instruments]{12.5}
Consider the case of a single endogenous regressor $X$ and $m$ instruments $Z_1,\\dots,Z_m$. If the coefficients on all instruments in the population first-stage regression of a TSLS estimation are zero, the instruments do not explain any of the variation in the $X$ which clearly violates assumption 1 of Key Concept 12.2. Although the latter case is unlikely to be encountered in practice, we should ask ourselves to what extent the assumption of instrument relevance should be fulfilled.\\newline 

While this is hard to answer for general IV regression, in the case of a \\textit{single} endogenous regressor $X$ one may use the following rule of thumb:\\newline

Compute the $F$-statistic which corresponds to the hypothesis that the coefficients on $Z_1,\\dots,Z_m$ are all zero in the first-stage regression. If the $F$-statistic is less than $10$, the instruments are weak such that the TSLS estimate of the coefficient on $X$ is biased and no valid statistical inference about its true value can be made. See also Appendix 12.5 of the book.
\\end{keyconcepts}
')
cat('
<div class = "keyconcept" id="KC12.6">
<h3 class = "right"> Key Concept 12.6 </h3>
<h3 class = "left"> $J$-Statistic / Overidentifying Restrictions Test </h3>

Take $\\widehat{u}_i^{TSLS} \\ , \\ i = 1,\\dots,n$, the residuals of the TSLS estimation of the general IV regression model \\@ref(eq:givmodel). Run the OLS regression

\\begin{align}
  \\widehat{u}_i^{TSLS} =& \\, \\delta_0 + \\delta_1 Z_{1i} + \\dots + \\delta_m Z_{mi} + \\delta_{m+1} W_{1i} + \\dots + \\delta_{m+r} W_{ri} + e_i (\\#eq:jstatreg)
\\end{align}

and test the joint hypothesis $$H_0: \\delta_1 = 0, \\dots, \\delta_{m} = 0$$ which states that all instruments are exogenous. This can be done using the corresponding $F$-statistic by computing $$J = mF.$$ This test is the overidentifying restrictions test and the statistic is called the $J$-statistic with $$J \\sim \\chi^2_{m-k}$$ in large samples under the null and the assumption of homoskedasticity. The degrees of freedom $m-k$ state the degree of overidentification since this is the number of instruments $m$ minus the number of endogenous regressors $k$.

</div>
')
cat('\\begin{keyconcepts}[$J$-Statistic / Overidentifying Restrictions Test]{12.6}
Take $\\widehat{u}_i^{TSLS} \\ , \\ i = 1,\\dots,n$, the residuals of the TSLS estimation of the general IV regression model \\ref{eq:givmodel}. Run the OLS regression

\\begin{align}
  \\widehat{u}_i^{TSLS} =& \\, \\delta_0 + \\delta_1 Z_{1i} + \\dots + \\delta_m Z_{mi} + \\delta_{m+1} W_{1i} + \\dots + \\delta_{m+r} W_{ri} + e_i \\label{eq:jstatreg}
\\end{align}

and test the joint hypothesis $$H_0: \\delta_1 = 0, \\dots, \\delta_{m} = 0$$ which states that all instruments are exogenous. This can be done using the corresponding $F$-statistic by computing $$J = m  F.$$ This test is the overidentifying restrictions test and the statistic is called the $J$-statistic with $$J \\sim \\chi^2_{m-k}$$ in large samples under the null and the assumption of homoskedasticity. The degrees of freedom $m-k$ state the degree of overidentification since this is the number of instruments $m$ minus the number of endogenous regressors $k$.
\\end{keyconcepts}
')
# subset data for year 1985
c1985 <- subset(CigarettesSW, year == "1985")

# define differences in variables
packsdiff <- log(c1995$packs) - log(c1985$packs)

pricediff <- log(c1995$price/c1995$cpi) - log(c1985$price/c1985$cpi)

incomediff <- log(c1995$income/c1995$population/c1995$cpi) -
log(c1985$income/c1985$population/c1985$cpi)

salestaxdiff <- (c1995$taxs - c1995$tax)/c1995$cpi - (c1985$taxs - c1985$tax)/c1985$cpi

cigtaxdiff <- c1995$tax/c1995$cpi - c1985$tax/c1985$cpi
# estimate the three models
cig_ivreg_diff1 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + 
                         salestaxdiff)

cig_ivreg_diff2 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + 
                         cigtaxdiff)

cig_ivreg_diff3 <- ivreg(packsdiff ~ pricediff + incomediff | incomediff + 
                         salestaxdiff + cigtaxdiff)
# robust coefficient summary for 1.
coeftest(cig_ivreg_diff1, vcov = vcovHC, type = "HC1")

# robust coefficient summary for 2.
coeftest(cig_ivreg_diff2, vcov = vcovHC, type = "HC1")

# robust coefficient summary for 3.
coeftest(cig_ivreg_diff3, vcov = vcovHC, type = "HC1")
## # gather robust standard errors in a list
## rob_se <- list(sqrt(diag(vcovHC(cig_ivreg_diff1, type = "HC1"))),
##                sqrt(diag(vcovHC(cig_ivreg_diff2, type = "HC1"))),
##                sqrt(diag(vcovHC(cig_ivreg_diff3, type = "HC1"))))
## 
## # generate table
## stargazer(cig_ivreg_diff1, cig_ivreg_diff2,cig_ivreg_diff3,
##   header = FALSE,
##   type = "html",
##   omit.table.layout = "n",
##   digits = 3,
##   column.labels = c("IV: salestax", "IV: cigtax", "IVs: salestax, cigtax"),
##   dep.var.labels.include = FALSE,
##   dep.var.caption = "Dependent Variable: 1985-1995 Difference in Log per Pack Price",
##   se = rob_se)
# gather robust standard errors in a list
rob_se <- list(sqrt(diag(vcovHC(cig_ivreg_diff1, type = "HC1"))),
               sqrt(diag(vcovHC(cig_ivreg_diff2, type = "HC1"))),
               sqrt(diag(vcovHC(cig_ivreg_diff3, type = "HC1"))))

stargazer(cig_ivreg_diff1, cig_ivreg_diff2,cig_ivreg_diff3,
  header = FALSE, 
  type = "html",
  digits = 3, 
  column.labels = c("IV: salestax", "IV: cigtax", "IVs: salestax, cigtax"),
  dep.var.labels.include = FALSE,
  dep.var.caption = "Dependent variable: 1985-1995 difference in log per pack price",
  se = rob_se)

stargazer_html_title("TSLS Estimates of the Long-Term Elasticity of the Demand for Cigarettes using Panel Data", "tslseotlteotdfcupd")
library(stargazer)

stargazer(cig_ivreg_diff1, cig_ivreg_diff2,cig_ivreg_diff3,
  title = "\\label{tab:tslseotlteotdfcupd} TSLS Estimates of the Long-Term Elasticity of the Demand for Cigarettes using Panel Data",
  header = F, 
  digits = 3,
  type = "latex",
  no.space = T,
  column.sep.width = "35pt",
  omit.table.layout = "n",
  column.labels = c("IV: salestax", "IV: cigtax", "IVs: salestax, cigtax"),
  dep.var.labels.include = FALSE,
  dep.var.caption = "Dependent variable: 1985-1995 difference in log per pack price",
  se = rob_se)
# first-stage regressions
mod_relevance1 <- lm(pricediff ~ salestaxdiff + incomediff)
mod_relevance2 <- lm(pricediff ~ cigtaxdiff + incomediff)
mod_relevance3 <- lm(pricediff ~ incomediff + salestaxdiff + cigtaxdiff)
# check instrument relevance for model (1)
linearHypothesis(mod_relevance1, 
                 "salestaxdiff = 0", 
                 vcov = vcovHC, type = "HC1")
# check instrument relevance for model (2)
linearHypothesis(mod_relevance2, 
                 "cigtaxdiff = 0", 
                 vcov = vcovHC, type = "HC1")
# check instrument relevance for model (3)
linearHypothesis(mod_relevance3, 
                 c("salestaxdiff = 0", "cigtaxdiff = 0"), 
                 vcov = vcovHC, type = "HC1")
# compute the J-statistic
cig_iv_OR <- lm(residuals(cig_ivreg_diff3) ~ incomediff + salestaxdiff + cigtaxdiff)

cig_OR_test <- linearHypothesis(cig_iv_OR, 
                               c("salestaxdiff = 0", "cigtaxdiff = 0"), 
                               test = "Chisq")
cig_OR_test
# compute correct p-value for J-statistic
pchisq(cig_OR_test[2, 5], df = 1, lower.tail = FALSE)
if (my_output == "html") {
  cat('
<div  class = "DCexercise">

#### 1. The College Distance Data {-}

There are many studies in labor economics which deal with the issue of estimating human capital earnings functions which state how wage income is determined by education and working experience. A prominent example is @card1993 who investigates the economic return to schooling and uses college proximity as an instrumental variable.

The exercises in this chapter deal with the dataset <tt>CollegeDistance</tt> which is similar to the data used by  @card1993. It stems from a survey of high school graduates with variables coded for wages, education, average tuition and a number of socio-economic measures. The data set also includes the distance from a college while the survey participants were in high school. <tt>CollegeDistance</tt> comes with the <tt>AER</tt> package.

**Instructions:**

+ Attach the <tt>AER</tt> package and load the <tt>CollegeDistance</tt> data.

+ Get an overview over the data set.

+ The variable <tt>distance</tt> (the distance to the closest 4-year college in 10 miles) will serve as an instrument in later exercises. Use a histogram to visualize the distribution of <tt>distance</tt>.

<iframe src="DCL/ex12_1.html" frameborder="0" scrolling="no" style="width:100%;height:330px"></iframe>

**Hints:**

+ Use <tt>data()</tt> to attach the data set.

+ The function <tt>hist()</tt> can be used to generate histograms.

</div>')}
if (my_output == "html") {
  cat('
<div  class = "DCexercise">

#### 2. The Selection Problem {-}

Regressing <tt>wage</tt> on <tt>education</tt> and control variables to estimate the human capital earnings function is problematic because education is not randomly assigned across the surveyed: individuals make their own education choices and so measured differences in earnings between individuals with different levels of education depend on how these choices are made. In the literature this is referred to as a *selection problem*. This selection problem implies that <tt>education</tt> is *endogenous* so the OLS estimate will be biased and we cannot make valid inference regarding the true coefficient. 

In this exercise you are asked to estimate two regressions which both do not yield trustworthy estimates of the coefficient on education due to the issue sketched above. Later you will compare the results to those obtained using the instrumental variables approach applied by @card1993.

The <tt>AER</tt> package has been attached. The data set <tt>CollegeDistance</tt> is available in your global environment.

**Instructions:**

+ Regress the *logarithm* of <tt>wage</tt> on <tt>education</tt>, that is, estimate the model $$\\log(wage_i) = \\beta_0 + \\beta_1 education_i + u_i$$ Save the result to <tt>wage_mod_1</tt>.

+ Augment the model by including the regressors <tt>unemp</tt>, <tt>hispanic</tt>, <tt>af-am</tt>, <tt>female</tt> and <tt>urban</tt>. Save the result to <tt>wage_mod_2</tt>

+ Obtain summaries on the estimated coefficients in both models.

<iframe src="DCL/ex12_2.html" frameborder="0" scrolling="no" style="width:100%;height:330px"></iframe>

</div>')}
if (my_output == "html") {
  cat('
<div  class = "DCexercise">

#### 3. Instrumental Variables Regression Approaches --- I  {-}

The above discussed selection problem renders the regression estimates in Exercise 2 implausible which is why @card1993 suggests instrumental variables regression that uses college distance as an instrument for education. 

Why use college distance as an instrument? The logic behind this is that distance from a college will be correlated to the decision to pursue a college degree (relevance) but may not predict wages apart from increased education (exogeneity) so college proximity could be considered a valid instrument (recall the definition of a valid instrument stated at the beginning of Chapter \\@ref(TIVEWASRAASI)).

The <tt>AER</tt> package has been attached. The data set <tt>CollegeDistance</tt> is available in your global environment.

**Instructions:**

+ Compute the correlations of the instrument <tt>distance</tt> with the edogenous regressor <tt>education</tt> and the dependent variable <tt>wage</tt>.

+ How much of the variation in <tt>education</tt> is explained by the *first-stage regression* which uses <tt>distance</tt> as a regressor? Save the result to <tt>R2</tt>.

+ Repeat Exercise 2 with IV regression, i.e., employ <tt>distance</tt> as an instrument for <tt>education</tt> in both regressions using <tt>ivreg()</tt>. Save the results to <tt>wage_mod_iv1</tt> and <tt>wage_mod_iv2</tt>. Obtain robust coefficient summaries for both models.

<iframe src="DCL/ex12_3.html" frameborder="0" scrolling="no" style="width:100%;height:410px"></iframe>

</div>')}
if (my_output == "html") {
  cat('
<div  class = "DCexercise">

#### 4. Instrumental Variables Regression Approaches --- II  {-}

Convince yourself that <tt>ivreg()</tt> works as expected by implementing the TSLS algorithm presented in Key Concept 12.2 for a single instrument, see Chapter \\@ref(TGIVRM).

**Instructions:**

+ Complete the function <tt>TSLS()</tt> such that it implements the TSLS estimator.

+ Use <tt>TSLS()</tt> to reproduce the coefficient estimates obtained using <tt>ivreg()</tt> for both models of Exercise 3.

<iframe src="DCL/ex12_4.html" frameborder="0" scrolling="no" style="width:100%;height:460px"></iframe>

**Hints:**

+ Completion of the function boils down to replacing the <tt>. . .</tt> by appropriate arguments.

+ Besides the data set (<tt>data</tt>), the function expects the dependent variable (<tt>Y</tt>), exogenous regressors  (<tt>W</tt>), the endogenous regressors (<tt>X</tt>) and an instrument (<tt>Z</tt>) as arguments. All of these should be of class <tt>character</tt>. 
+ Including <tt>W = NULL</tt> in the head of the function definition ensures that the set of exogenous variables is empty, by default.

</div>')}
if (my_output == "html") {
  cat('
<div  class = "DCexercise">

#### 5. Should we trust the Results? {-}

This is not a real code exercise (there are no submission correctness tests for checking your code). Instead we would like you to use the widget below to compare the results obtained using the OLS regressions of Exercise 2 with those of the IV regressions of Exercise 3.

The data set <tt>CollegeDistance</tt> and all model objects from Exercises 2 and 3 are available in the global environment.

**Instructions:**

Convince yourself of the following: 

1. It is likely that the bias of the estimated coefficient on <tt>education</tt> in the simple regression model <tt>wage_mod_1</tt> is subtantial because the regressor is endogenous due to omitting variables from the model which correlate with <tt>education</tt> and impact wage income. 

2. Due to the selection problem in described in Exercise 2, the estimate of the coefficient of interest is not trustworthy even in the multiple regression model <tt>wage_mod_2</tt> which includes several socio-economic control variables. The coeffiecient on <tt>education</tt> is not significant and its estimate is close to zero).

3. Instrumenting education by the college distance as done in <tt>wage_mod_iv1</tt> yields the IV estimate of the coefficient of interest. The result should, however, not be considered reliable because this simple model probably suffers from omitted variables bias just as the multiple regression model <tt>wage_mod_2</tt> from Exercise 2, see 1. Again,  the coeffiecient on <tt>education</tt> is not significant its estimate is quite small.  

4. <tt>wage_mod_iv2</tt>, the multiple regression model where we include demographic control variables and instrumend <tt>education</tt> by <tt>distance</tt> delivers the most reliable estimate of the impact of education on wage income among all the models considered. The coefficient is highly significant and the estimate is about $0.067$. Following Key Concept 8.2, the interpretation is that an additional year of schooling is expected to increases wage income by roughly $0.067 \\cdot 100\\% = 6.7\\%$.

5. Is the estimate of the coefficient on education reported by <tt>wage_mod_iv2</tt> trustworthy? This question is not easy to answer. In any case, we should bear in mind that using an instrumental variables approach is problematic when the instrument is *weak*. This could be the case here: Families with strong preference for education may move into neighborhoods close to colleges. Furthermore, neighborhoods close to colleges may have stronger job markets reflected by higher incomes. Such features would render the instrument invalid as they introduce unobserved variables which influence earnings but cannot be captured by years of schooling, our measure of education. 

<iframe src="DCL/ex12_5.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

</div>')}
