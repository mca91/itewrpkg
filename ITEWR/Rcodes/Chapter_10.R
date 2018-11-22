## library(AER)
## library(plm)
## library(stargazer)
cat('
<div class = "keyconcept" id="KC10.1">
<h3 class = "right"> Key Concept 10.1 </h3>
<h3 class = "left"> Notation for Panel Data </h3>
In contrast to cross-section data where we have observations on $n$ subjects (entities), panel data has observations on $n$ entities at $T\\geq2$ time periods. This is denoted

$$(X_{it},Y_{it}), \\ i=1,\\dots,n \\ \\ \\ \\text{and} \\ \\ \\ t=1,\\dots,T $$
where the index $i$ refers to the entity while $t$ refers to the time period.
</div>
')
cat('\\begin{keyconcepts}[Notation for Panel Data]{10.1}
In contrast to cross-section data where we have observations on $n$ subjects (entities), panel data has observations on $n$ entities at $T\\geq2$ time periods. This is denoted $$(X_{it},Y_{it}), \\ i=1,\\dots,n \\ \\ \\ \\text{and} \\ \\ \\ t=1,\\dots,T $$
where the index $i$ refers to the entity while $t$ refers to the time period.
\\end{keyconcepts}
')
# load the package and the dataset
library(AER)
data(Fatalities)
# obtain the dimension and inspect the structure
is.data.frame(Fatalities)
dim(Fatalities)
str(Fatalities)
# list the first few observations
head(Fatalities)
# summarize the variables 'state' and 'year'
summary(Fatalities[, c(1, 2)])
# define the fatality rate
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000

# subset the data
Fatalities1982 <- subset(Fatalities, year == "1982")
Fatalities1988 <- subset(Fatalities, year == "1988")
# estimate simple regression models using 1982 and 1988 data
fatal1982_mod <- lm(fatal_rate ~ beertax, data = Fatalities1982)
fatal1988_mod <- lm(fatal_rate ~ beertax, data = Fatalities1988)

coeftest(fatal1982_mod, vcov. = vcovHC, type = "HC1")
coeftest(fatal1988_mod, vcov. = vcovHC, type = "HC1")
# plot the observations and add the estimated regression line for 1982 data
plot(x = Fatalities1982$beertax, 
     y = Fatalities1982$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1982",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1982_mod, lwd = 1.5)

# plot observations and add estimated regression line for 1988 data
plot(x = Fatalities1988$beertax, 
     y = Fatalities1988$fatal_rate, 
     xlab = "Beer tax (in 1988 dollars)",
     ylab = "Fatality rate (fatalities per 10000)",
     main = "Traffic Fatality Rates and Beer Taxes in 1988",
     ylim = c(0, 4.5),
     pch = 20, 
     col = "steelblue")

abline(fatal1988_mod, lwd = 1.5)
# compute the differences 
diff_fatal_rate <- Fatalities1988$fatal_rate - Fatalities1982$fatal_rate
diff_beertax <- Fatalities1988$beertax - Fatalities1982$beertax

# estimate a regression using differenced data
fatal_diff_mod <- lm(diff_fatal_rate ~ diff_beertax)

coeftest(fatal_diff_mod, vcov = vcovHC, type = "HC1")
# plot the differenced data
plot(x = diff_beertax, 
     y = diff_fatal_rate, 
     xlab = "Change in beer tax (in 1988 dollars)",
     ylab = "Change in fatality rate (fatalities per 10000)",
     main = "Changes in Traffic Fatality Rates and Beer Taxes in 1982-1988",
     xlim = c(-0.6, 0.6),
     ylim = c(-1.5, 1),
     pch = 20, 
     col = "steelblue")

# add the regression line to plot
abline(fatal_diff_mod, lwd = 1.5)
# compute mean fatality rate over all states for all time periods
mean(Fatalities$fatal_rate)
cat('
<div class = "keyconcept" id="KC10.2">
<h3 class = "right"> Key Concept 10.2 </h3>
<h3 class = "left"> The Fixed Effects Regression Model </h3>

The fixed effects regression model is

\\begin{align}
Y_{it} = \\beta_1 X_{1,it} + \\cdots + \\beta_k X_{k,it} + \\alpha_i + u_{it} (\\#eq:gfemodel)
\\end{align}

with $i=1,\\dots,n$ and $t=1,\\dots,T$. The $\\alpha_i$ are entity-specific intercepts that capture heterogeneities across entities. An equivalent representation of this model is given by

\\begin{align}
Y_{it} = \\beta_0 + \\beta_1 X_{1,it} + \\cdots + \\beta_k X_{k,it} + \\gamma_2 D2_i + \\gamma_3 D3_i + \\cdots + \\gamma_n Dn_i  + u_{it} (\\#eq:gdrmodel)
\\end{align}

where the $D2_i,D3_i,\\dots,Dn_i$ are dummy variables.

</div>
')
cat('\\begin{keyconcepts}[The Fixed Effects Regression Model]{10.2}
The fixed effects regression model is
\\begin{align}
Y_{it} = \\beta_1 X_{1,it} + \\cdots + \\beta_k X_{k,it} + \\alpha_i + u_{it} \\label{eq:gfemodel}
\\end{align}
with $i=1,\\dots,n$ and $t=1,\\dots,T$. The $\\alpha_i$ are entity-specific intercepts that capture heterogeneities across entities. An equivalent representation of this model is given by
\\begin{align}
Y_{it} = \\beta_0 + \\beta_1 X_{1,it} + \\cdots + \\beta_k X_{k,it} + \\gamma_2 D2_i + \\gamma_3 D3_i + \\cdots + \\gamma_n Dn_i  + u_{it} \\label{eq:gdrmodel}
\\end{align}
where the $D2_i,D3_i,\\dots,Dn_i$ are dummy variables.
\\end{keyconcepts}
')
fatal_fe_lm_mod <- lm(fatal_rate ~ beertax + state - 1, data = Fatalities)
fatal_fe_lm_mod
## # obtain demeaned data
## Fatalities_demeaned <- with(Fatalities,
##             data.frame(fatal_rate = fatal_rate - ave(fatal_rate, state),
##             beertax = beertax - ave(beertax, state)))
## 
## # estimate the regression
## summary(lm(fatal_rate ~ beertax - 1, data = Fatalities_demeaned))
# install and load the 'plm' package
install.packages("plm")
library(plm)
# estimate the fixed effects regression with plm()
fatal_fe_mod <- plm(fatal_rate ~ beertax, 
                    data = Fatalities,
                    index = c("state", "year"), 
                    model = "within")

# print summary using robust standard errors
coeftest(fatal_fe_mod, vcov. = vcovHC, type = "HC1")
# estimate a combined time and entity fixed effects regression model

# via lm()
fatal_tefe_lm_mod <- lm(fatal_rate ~ beertax + state + year - 1, data = Fatalities)
fatal_tefe_lm_mod

# via plm()
fatal_tefe_mod <- plm(fatal_rate ~ beertax, 
                      data = Fatalities,
                      index = c("state", "year"), 
                      model = "within", 
                      effect = "twoways")

coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")
# check the class of 'state' and 'year'
class(Fatalities$state)
class(Fatalities$year)
cat('
<div class = "keyconcept" id="KC10.3">
<h3 class = "right"> Key Concept 10.3 </h3>
<h3 class = "left"> The Fixed Effects Regression Assumptions </h3>

In the fixed effects model $$ Y_{it} = \\beta_1 X_{it} + \\alpha_i + u_{it} \\ \\ , \\ \\ i=1,\\dots,n, \\ t=1,\\dots,T, $$ we assume the following:

1. The error term $u_{it}$ has conditional mean zero, that is, $E(u_{it}|X_{i1}, X_{i2},\\dots, X_{iT})$.

2. $(X_{i1}, X_{i2}, \\dots, X_{i3}, u_{i1}, \\dots, u_{iT})$, $i=1,\\dots,n$ are i.i.d. draws from their joint distribution.

3. Large outliers are unlikely, i.e., $(X_{it}, u_{it})$ have nonzero finite fourth moments.

4. There is no perfect multicollinearity.

When there are multiple regressors, $X_{it}$ is replaced by $X_{1,it}, X_{2,it}, \\dots, X_{k,it}$.

</div>
')
cat('\\begin{keyconcepts}[The Fixed Effects Regression Assumptions]{10.3}
In the fixed effects regression model $$ Y_{it} = \\beta_1 X_{it} + \\alpha_i + u_{it} \\ \\ , \\ \\ i=1,\\dots,n, \\ t=1,\\dots,T, $$ we assume the following:\\newline

\\begin{enumerate}
\\item The error term $u_{it}$ has conditional mean zero, that is, $E(u_{it}|X_{i1}, X_{i2},\\dots, X_{iT})$.
\\item $(X_{i1}, X_{i2}, \\dots, X_{i3}, u_{i1}, \\dots, u_{iT})$, $i=1,\\dots,n$ are i.i.d. draws from their joint distribution.
\\item Large outliers are unlikely, i.e., $(X_{it}, u_{it})$ have nonzero finite fourth moments.
\\item There is no perfect multicollinearity.
\\end{enumerate}\\vspace{0.5cm}

When there are multiple regressors, $X_{it}$ is replaced by $X_{1,it}, X_{2,it}, \\dots, X_{k,it}$.
\\end{keyconcepts}
')
# check class of the model object
class(fatal_tefe_lm_mod)

# obtain a summary based on heteroskedasticity-robust standard errors 
# (no adjustment for heteroskedasticity only)
coeftest(fatal_tefe_lm_mod, vcov = vcovHC, type = "HC1")[1, ]

# check class of the (plm) model object
class(fatal_tefe_mod)

# obtain a summary based on clusterd standard errors 
# (adjustment for autocorrelation + heteroskedasticity)
coeftest(fatal_tefe_mod, vcov = vcovHC, type = "HC1")
# discretize the minimum legal drinking age
Fatalities$drinkagec <- cut(Fatalities$drinkage,
                            breaks = 18:22, 
                            include.lowest = TRUE, 
                            right = FALSE)

# set minimum drinking age [21, 22] to be the baseline level
Fatalities$drinkagec <- relevel(Fatalities$drinkagec, "[21,22]")

# mandadory jail or community service?
Fatalities$punish <- with(Fatalities, factor(jail == "yes" | service == "yes", 
                                             labels = c("no", "yes")))

# the set of observations on all variables for 1982 and 1988
Fatalities_1982_1988 <- Fatalities[with(Fatalities, year == 1982 | year == 1988), ]
# estimate all seven models
fatalities_mod1 <- lm(fatal_rate ~ beertax, data = Fatalities)

fatalities_mod2 <- plm(fatal_rate ~ beertax + state, data = Fatalities)

fatalities_mod3 <- plm(fatal_rate ~ beertax + state + year,
                       index = c("state","year"),
                       model = "within",
                       effect = "twoways", 
                       data = Fatalities)

fatalities_mod4 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod5 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles,
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod6 <- plm(fatal_rate ~ beertax + year + drinkage 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities)

fatalities_mod7 <- plm(fatal_rate ~ beertax + state + year + drinkagec 
                       + punish + miles + unemp + log(income), 
                       index = c("state", "year"),
                       model = "within",
                       effect = "twoways",
                       data = Fatalities_1982_1988)
## library(stargazer)
## 
## # gather clustered standard errors in a list
## rob_se <- list(sqrt(diag(vcovHC(fatalities_mod1, type = "HC1"))),
##                sqrt(diag(vcovHC(fatalities_mod2, type = "HC1"))),
##                sqrt(diag(vcovHC(fatalities_mod3, type = "HC1"))),
##                sqrt(diag(vcovHC(fatalities_mod4, type = "HC1"))),
##                sqrt(diag(vcovHC(fatalities_mod5, type = "HC1"))),
##                sqrt(diag(vcovHC(fatalities_mod6, type = "HC1"))),
##                sqrt(diag(vcovHC(fatalities_mod7, type = "HC1"))))
## 
## # generate the table
## stargazer(fatalities_mod1, fatalities_mod2, fatalities_mod3,
##           fatalities_mod4, fatalities_mod5, fatalities_mod6, fatalities_mod7,
##           digits = 3,
##           header = FALSE,
##           type = "latex",
##           se = rob_se,
##           title = "Linear Panel Regression Models of Traffic Fatalities due to Drunk Driving",
##           model.numbers = FALSE,
##           column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)"))
library(stargazer)

rob_se <- list(
  sqrt(diag(vcovHC(fatalities_mod1, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod2, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod3, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod4, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod5, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod6, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod7, type="HC1")))
)

stargazer(fatalities_mod1, fatalities_mod2, fatalities_mod3, fatalities_mod4, fatalities_mod5, fatalities_mod6, fatalities_mod7, 
          digits = 3,
          type = "html",
          header = FALSE,
          se = rob_se,
          dep.var.caption = "Dependent Variable: Fatality Rate",
          model.numbers = FALSE,
          column.labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)")
          )

stargazer_html_title("Linear Panel Regression Models of Traffic Fatalities due to Drunk Driving", "lprmotfdtdd")
library(stargazer)

rob_se <- list(
  sqrt(diag(vcovHC(fatalities_mod1, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod2, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod3, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod4, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod5, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod6, type="HC1"))),
  sqrt(diag(vcovHC(fatalities_mod7, type="HC1")))
)

stargazer(fatalities_mod1, fatalities_mod2, fatalities_mod3, fatalities_mod4, fatalities_mod5, fatalities_mod6, fatalities_mod7, 
          digits = 3,
          type = "latex",
          float.env = "sidewaystable",
          column.sep.width = "-5pt",
          se = rob_se,
          header = FALSE,
          model.names = FALSE,
          column.labels = c('OLS','','','Linear Panel Regression'),
          omit.stat = "f",
          title = "\\label{tab:lprmotfdtdd} Linear Panel Regression Models of Traffic Fatalities due to Drunk Driving")
# test if legal drinking age has no explanatory power
linearHypothesis(fatalities_mod4,
                 test = "F",
                 c("drinkagec[18,19)=0", "drinkagec[19,20)=0", "drinkagec[20,21)"), 
                 vcov. = vcovHC, type = "HC1")
# test if economic indicators have no explanatory power
linearHypothesis(fatalities_mod4, 
                 test = "F",
                 c("log(income)", "unemp"), 
                 vcov. = vcovHC, type = "HC1")
if (my_output=="html"){
  cat('
For the course of this section, you will work with <tt>Guns</tt>, a balanced panel containing observations on criminal and demographic variables for all US states and the years 1977-1999. The data set comes with the package <tt>AER</tt> which is already installed for the interactive R exercises below.

<div  class = "DCexercise">

#### 1. The Guns Data Set {-}

**Instructions:**

+ Load both the package and the data set. 
  
+ Get yourself an overview over the data set using the <tt>summary()</tt> function. Use <tt>?Guns</tt> for detailed information on the variables.
  
+ Verify that <tt>Guns</tt> is a balanced panel. Do so by extracting the number of years and states from the data set and assign them to the predefined variables <tt>years</tt> and <tt>states</tt>, respectively. Afterwards use these variables for a logical comparison: check that the panel is balanced

<iframe src="DCL/ex10_1.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>
        
**Hints:**
  
  + Use <tt>library()</tt> and <tt>data()</tt> to attach the package and load the data set, respectively.
  + Use <tt>summary()</tt> to obtain a comprehensive overview of the dataset.
  + In order to be a balanced panel the number of entities times the number of years has to be equal to the total number of observations in the dataset. The basic functions <tt>levels()</tt>, <tt>length()</tt> and <tt>nrow()</tt> may be useful here to accomplish this task.
      
</div>')
} else {
  cat('\\begin{center}\\textit{This interactive part of URFITE is only available in the HTML version.}\\end{center}')
}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 2. Strict or Loose? Gun Laws and the Effect on Crime I {-}

There is a controversial debate on whether and to what extent the right to carry a gun influences crime. Proponents of so-called "Carrying a Concealed Weapon" (CCW) laws argue that the deterrent effect of guns prevents crime, whereas opponents argue that ... In the following exercises you will empirically investigate this topic.

To begin with consider the following estimated model $$\\widehat{{\\log(violent_i)}} = 6.135 - 0.443 \\times law_i$$, where <tt>violent</tt> and <tt>law</tt> denote the violent crime rate (incidents per $100000$ citizens) and a binary variable indicating the implementation of a CCW law (1 = yes, 0 = no), respectively.

The packages <tt>AER</tt> and <tt>plm</tt> have been loaded.
      
**Instructions:**
        
+ Is the proposed model a "good" model? In particular, are there variables that vary across states?

+ Print a summary of the model reporting robust standard errors. Analyze thr results.

<iframe src="DCL/ex10_2.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

+ As usual you can use <tt>coeftest()</tt> in conjunction with appropriate arguments to obtain a summary output with robust standard errors.
</div>')
}
