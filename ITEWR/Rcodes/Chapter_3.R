
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
## library(dplyr)
## library(MASS)
## library(readxl)
cat('
<div class = "keyconcept" id="KC3.1">
<h3 class = "right"> Key Concept 3.1 </h3>          
<h3 class = "left"> Estimators and Estimates </h3>

*Estimators* are functions of sample data drawn from an unknown population. *Estimates* are numeric values computed by estimators based on the sample data. Estimators are random variables because they are functions of *random* data. Estimates are nonrandom numbers.

</div>
')
cat('\\begin{keyconcepts}[Estimators and Estimates]{3.1}
\\textit{Estimators} are functions of sample data that are drawn randomly from an unknown population. \\textit{Estimates} are numeric values computed by estimators based on the sample data. Estimators are random variables because they are functions of \\textit{random} data. Estimates are nonrandom numbers.
\\end{keyconcepts}')
# plot the chi_12^2 distribution
curve(dchisq(x, df=12), 
      from = 0, 
      to = 40, 
      ylab = "density", 
      xlab = "hourly earnings in Euro")
# set seed for reproducibility
set.seed(1)

# sample from the chi_12^2 distribution, keep only the first observation
rchisq(n = 100, df = 12)[1]
cat('
<div class = "keyconcept" id="KC3.2">
<h3 class = "right"> Key Concept 3.2 </h3>          
<h3 class = "left"> Bias, Consistency and Efficiency </h3>

Desirable characteristics of an estimator include unbiasedness, consistency and efficiency.

**Unbiasedness:**  
If the mean of the sampling distribution of some estimator $\\hat\\mu_Y$ for the population mean $\\mu_Y$ equals $\\mu_Y$,
$$ E(\\hat\\mu_Y) = \\mu_Y, $$
the estimator is unbiased for $\\mu_Y$. The *bias* of $\\hat\\mu_Y$ then is $0$:

$$ E(\\hat\\mu_Y) - \\mu_Y = 0$$

**Consistency:**

We want the uncertainty of the estimator $\\mu_Y$ to decrease as the number of observations in the sample grows. More precisely, we want the probability that the estimate $\\hat\\mu_Y$ falls within a small interval around the true value $\\mu_Y$ to get increasingly closer to $1$ as $n$ grows. We write this as

$$ \\hat\\mu_Y \\xrightarrow{p} \\mu_Y. $$

**Variance and efficiency:**

We want the estimator to be efficient. Suppose we have two estimators, $\\hat\\mu_Y$ and $\\overset{\\sim}{\\mu}_Y$ and for some given sample size $n$ it holds that

$$ E(\\hat\\mu_Y) = E(\\overset{\\sim}{\\mu}_Y) = \\mu_Y $$
but
$$\\text{Var}(\\hat\\mu_Y) < \\text{Var}(\\overset{\\sim}{\\mu}_Y).$$

We then prefer to use $\\hat\\mu_Y$ as it has a lower variance than $\\overset{\\sim}{\\mu}_Y$, meaning that $\\hat\\mu_Y$ is more *efficient* in using the information provided by the observations in the sample.

</div>
')
cat('\\begin{keyconcepts}[Bias\\comma Consistency and Efficiency]{3.2}
Desirable characteristics of an estimator include unbiasedness, consistency and efficiency.\\newline

\\textbf{Unbiasedness:}

If the mean of the sampling distribution of some estimator $\\hat\\mu_Y$ for the population mean $\\mu_Y$ equals $\\mu_Y$,
$$ E(\\hat\\mu_Y) = \\mu_Ym, $$
the estimator is unbiased for $\\mu_Y$. The \\textit{bias} of $\\hat\\mu_Y$ then is $0$:

$$ E(\\hat\\mu_Y) - \\mu_Y = 0$$

\\textbf{Consistency:}

We want the uncertainty of the estimator $\\mu_Y$ to decrease as the number of observations in the sample grows. More precisely, we want the probability that the estimate $\\hat\\mu_Y$ falls within a small interval around the true value $\\mu_Y$ to get increasingly closer to $1$ as $n$ grows. We write this as

$$ \\hat\\mu_Y \\xrightarrow{p} \\mu_Y. $$

\\textbf{Variance and efficiency:}

We want the estimator to be efficient. Suppose we have two estimators, $\\hat\\mu_Y$ and $\\overset{\\sim}{\\mu}_Y$ and for some given sample size $n$ it holds that

$$ E(\\hat\\mu_Y) = E(\\overset{\\sim}{\\mu}_Y) = \\mu_Y $$
but
$$\\text{Var}(\\hat\\mu_Y) < \\text{Var}(\\overset{\\sim}{\\mu}_Y).$$

We then prefer to use $\\hat\\mu_Y$ as it has a lower variance than $\\overset{\\sim}{\\mu}_Y$, meaning that $\\hat\\mu_Y$ is more \\textit{efficient} in using the information provided by the observations in the sample.
\\end{keyconcepts}')
## A more precise way to express consistency of an estimator $\hat\mu$ for a parameter $\mu$ is

# generate a fictious population
pop <- rnorm(10000, 10, 1)

# sample from the population and estimate the mean
est1 <- replicate(expr = mean(sample(x = pop, size = 5)), n = 25000)

est2 <- replicate(expr = mean(sample(x = pop, size = 25)), n = 25000)

fo <- replicate(expr = sample(x = pop, size = 5)[1], n = 25000)
# check if object type is vector
is.vector(est1)
is.vector(est2)

# check length
length(est1)
length(est2)
# plot density estimate Y_1
plot(density(fo), 
      col = 'green', 
      lwd = 2,
      ylim = c(0, 2),
      xlab = 'estimates',
      main = 'Sampling Distributions of Unbiased Estimators')

# add density estimate for the distribution of the sample mean with n=5 to the plot
lines(density(est1), 
     col = 'steelblue', 
     lwd = 2, 
     bty = 'l')

# add density estimate for the distribution of the sample mean with n=25 to the plot
lines(density(est2), 
      col = 'red2', 
      lwd = 2)

# add a vertical line at the true parameter
abline(v = 10, lty = 2)

# add N(10,1) density to the plot
curve(dnorm(x, mean = 10), 
     lwd = 2,
     lty = 2,
     add = T)

# add a legend
legend("topleft",
       legend = c("N(10,1)",
                  expression(Y[1]),
                  expression(bar(Y) ~ n == 5),
                  expression(bar(Y) ~ n == 25)
                  ), 
       lty = c(2, 1, 1, 1), 
       col = c('black','green', 'steelblue', 'red2'),
       lwd = 2)
# define the function and vectorize it
sqm <- function(m) {
 sum((y-m)^2)
}
sqm <- Vectorize(sqm)

# draw random sample and compute the mean
y <- rnorm(100, 10, 1)
mean(y)
# plot the objective function
curve(sqm(x), 
      from = -50, 
      to = 70,
      xlab = "m",
      ylab = "sqm(m)")

# add vertical line at mean(y)
abline(v = mean(y), 
       lty = 2, 
       col = "darkred")

# add annotation at mean(y)
text(x = mean(y), 
     y = 0, 
     labels = paste(round(mean(y), 2)))
## Some <tt>R</tt> functions can only interact with functions that take a vector as an input and evaluate the function body on every entry of the vector, for example <tt>curve()</tt>. We call such functions vectorized functions and it is often a good idea to write vectorized functions yourself, although this is cumbersome in some cases. Having a vectorized function in <tt>R</tt> is never a drawback since these functions work on both single values and vectors.

# compute the population mean of pop
mean(pop)
# simulate outcomes for the sample mean when the i.i.d. assumption fails
est3 <-  replicate(n = 25000, 
                   expr = mean(sample(x = sort(pop), 
                                      size = 10, 
                                      prob = c(rep(4, 2500), rep(1, 7500)))))

# compute the sample mean of the outcomes
mean(est3)
# sampling distribution of sample mean, i.i.d. holds, n=25
plot(density(est2), 
      col = 'steelblue',
      lwd = 2,
      xlim = c(8, 11),
      xlab = 'Estimates',
      main = 'When the i.i.d. Assumption Fails')

# sampling distribution of sample mean, i.i.d. fails, n=25
lines(density(est3),
      col = 'red2',
      lwd = 2)

# add a legend
legend("topleft",
       legend = c(expression(bar(Y)[n == 25]~", i.i.d. fails"),
                  expression(bar(Y)[n == 25]~", i.i.d. holds")
                  ), 
       lty = c(1, 1), 
       col = c('red2', 'steelblue'),
       lwd = 2)
# plot the standard normal density on the interval [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Calculating a p-Value',
      yaxs = 'i',
      xlab = 'z',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-1.5, 0, 1.5), 
     padj = 0.75,
     labels = c(expression(-frac(bar(Y)^"act"~-~bar(mu)[Y,0], sigma[bar(Y)])),
                0,
                expression(frac(bar(Y)^"act"~-~bar(mu)[Y,0], sigma[bar(Y)]))))

# shade p-value/2 region in left tail
polygon(x = c(-6, seq(-6, -1.5, 0.01), -1.5),
        y = c(0, dnorm(seq(-6, -1.5, 0.01)),0), 
        col = 'steelblue')

# shade p-value/2 region in right tail
polygon(x = c(1.5, seq(1.5, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.5, 6, 0.01)), 0), 
        col = 'steelblue')
# vector of sample sizes
n <- c(10000, 5000, 2000, 1000, 500)

# sample observations, estimate using 'sd()' and plot the estimated distributions
sq_y <- replicate(n = 10000, expr = sd(rnorm(n[1], 10, 10)))
plot(density(sq_y),
     main = expression('Sampling Distributions of' ~ s[Y]),
     xlab = expression(s[y]),
     lwd = 2)

for (i in 2:length(n)) {
  sq_y <- replicate(n = 10000, expr = sd(rnorm(n[i], 10, 10)))
  lines(density(sq_y), 
        col = i, 
        lwd = 2)
}

# add a legend
legend("topleft",
       legend = c(expression(n == 10000),
                  expression(n == 5000),
                  expression(n == 2000),
                  expression(n == 1000),
                  expression(n == 500)), 
       col = 1:5,
       lwd = 2)
cat('
<div class = "keyconcept" id="KC3.4">
<h3 class = "right"> Key Concept 3.4 </h3>          
<h3 class = "left"> The Standard Error of $\\overline{Y}$ </h3>

Take an i.i.d. sample $Y_1, \\dots, Y_n$. The mean of $Y$ is consistently estimated by $\\overline{Y}$, the sample mean of the $Y_i$. Since $\\overline{Y}$ is a random variable, it has a sampling distribution with variance $\\frac{\\sigma_Y^2}{n}$.

The standard error of $\\overline{Y}$, denoted $SE(\\overline{Y})$ is an estimator of the standard deviation of $\\overline{Y}$:

$$ SE(\\overline{Y}) = \\hat\\sigma_{\\overline{Y}} = \\frac{s_Y}{\\sqrt{n}} $$

The caret (^) over $\\sigma$ indicates that $\\hat\\sigma_{\\overline{Y}}$ is an estimator for $\\sigma_{\\overline{Y}}$. 
</div>
')
cat('\\begin{keyconcepts}[The Standard Error of $\\overline{Y}$]{3.4}
Take an i.i.d. sample $Y_1, \\dots, Y_n$. The mean of $Y$ is consistently estimated by $\\overline{Y}$, the sample mean of the $Y_i$. Since $\\overline{Y}$ is a random variable, it has a sampling distribution with variance $\\frac{\\sigma_Y^2}{n}$.

The standard error of $\\overline{Y}$, denoted $SE(\\overline{Y})$ is an estimator of the standard deviation of $\\overline{Y}$:

$$ SE(\\overline{Y}) = \\hat\\sigma_{\\overline{Y}} = \\frac{s_Y}{\\sqrt{n}} $$

The caret (\\string^) over $\\sigma$ indicates that $\\hat\\sigma_{\\overline{Y}}$ is an estimator for $\\sigma_{\\overline{Y}}$. 
\\end{keyconcepts}
')
# draw 10000 samples of size 100 and estimate the mean of Y and
# estimate the standard error of the sample mean

mean_estimates <- numeric(10000)
se_estimates <- numeric(10000)

for (i in 1:10000) {
  
  s <- sample(0:1, 
              size = 100,  
              prob = c(0.9, 0.1),
              replace = T)
  
  mean_estimates[i] <- mean(s)
  se_estimates[i] <- sqrt(mean(s) * (1 - mean(s)) / 100)

}

mean(mean_estimates)
mean(se_estimates)
# sample and estimate, compute standard error
samplemean_act <- mean(
  sample(0:1, 
         prob = c(0.9, 0.1), 
         replace = T, 
         size = 100))

SE_samplemean <- sqrt(samplemean_act * (1 - samplemean_act) / 100)

# null hypothesis
mean_h0 <- 0.1

# compute the p-value
pvalue <- 2 * pnorm(- abs(samplemean_act - mean_h0) / SE_samplemean)
pvalue
# compute a t-statistic for the sample mean
tstatistic <- (samplemean_act - mean_h0) / SE_samplemean
tstatistic
# prepare empty vector for t-statistics
tstatistics <- numeric(10000)

# set sample size
n <- 300

# simulate 10000 t-statistics
for (i in 1:10000) {
  
  s <- sample(0:1, 
              size = n,  
              prob = c(0.9, 0.1),
              replace = T)
  
  tstatistics[i] <- (mean(s)-0.1)/sqrt(var(s)/n)
  
}
# plot density and compare to N(0,1) density
plot(density(tstatistics),
     xlab = 't-statistic',
     main = 'Estimated Distribution of the t-statistic when n=300',
     lwd = 2,
     xlim = c(-4, 4),
     col = 'steelblue')

# N(0,1) density (dashed)
curve(dnorm(x), 
      add = T, 
      lty = 2, 
      lwd = 2)
cat('
<div class = "keyconcept" id="KC3.5">
<h3 class = "right"> Key Concept 3.5 </h3>          
<h3 class = "left"> The Terminology of Hypothesis Testing </h3>

In hypothesis testing, two types of mistakes are possible:

1. The null hypothesis *is* rejected although it is true (type-I-error)  

2. The null hypothesis *is not* rejected although it is false (type-II-error) 

The **significance level** of the test is the probability to commit a type-I-error we are willing to accept in advance. E.g., using a prespecified significance level of $0.05$, we reject the null hypothesis if and only if the $p$-value is less than $0.05$. The significance level is chosen before the test is conducted.

An equivalent procedure is to reject the null hypothesis if the observed test statistic is, in absolute value terms, larger than the **critical value** of the test statistic. The critical value is determined by the significance level chosen and defines two disjoint sets of values which are called **acceptance region** and **rejection region**. The acceptance region contains all values of the test statistic for which the test does not reject while the rejection region contains all the values for which the test does reject.

The **$p$-value** is the probability that, in repeated sampling under the same conditions a test statistic is observed that provides just as much evidence against the null hypothesis as the test statistic actually observed.

The actual probability that the test rejects the true null hypothesis is called the **size of the test**. In an ideal setting, the size does equals the significance level.

The probability that the test correctly rejects a false null hypothesis is called **power**. 

</div>
')
cat('\\begin{keyconcepts}[The Terminology of Hypothesis Testing]{3.5}
In hypothesis testing, two types of mistakes are possible:\\newline

\\begin{enumerate}
\\item The null hypothesis \\textit{is} rejected although it is true (type-I-error)  
\\item The null hypothesis \\textit{is not} rejected although it is false (type-II-error) 
\\end{enumerate}\\vspace{0.5cm}

The \\textit{significance level} of the test is the probability to commit a type-I-error we are willing to accept in advance. E.g., using a prespecified significance level of $0.05$, we reject the null hypothesis if and only if the $p$-value is less than $0.05$. The significance level is chosen before the test is conducted.\\newline

An equivalent procedure is to reject the null hypothesis if the observed test statistic is, in absolute value terms, larger than the \\textit{critical value} of the test statistic. The critical value is determined by the significance level chosen and defines two disjoint sets of values which are called \\textit{acceptance region} and \\textit{rejection region}. The acceptance region contains all values of the test statistic for which the test does not reject while the rejection region contains all the values for which the test does reject.\\newline

The \\textit{$p$-value} is the probability that, in repeated sampling under the same conditions, a test statistic is observed that provides just as much evidence against the null hypothesis as the test statistic actually observed.\\newline

The actual probability that the test rejects the true null hypothesis is called the \\textit{size of the test}. In an ideal setting, the size equals the significance level.\\newline

The probability that the test correctly rejects a false null hypothesis is called \\textit{power}. 
\\end{keyconcepts}
')
# check whether p-value < 0.05
pvalue < 0.05
# check the critical value
qnorm(p = 0.975)

# check whether the null is rejected using the t-statistic computed further above
abs(tstatistic) > 1.96
cat('
<div class = "keyconcept" id="KC3.6">
<h3 class = "right"> Key Concept 3.6 </h3>          
<h3 class = "left"> Testing the Hypothesis $E(Y) = \\mu_{Y,0}$ Against the Alternative $E(Y) \\neq \\mu_{Y,0}$ </h3>

1. Estimate $\\mu_{Y}$ using $\\overline{Y}$ and compute the $SE(\\overline{Y})$, standard error of $\\overline{Y}$.

2. Compute the $t$-statistic.

3. Compute the $p$-value and reject the null hypothesis at the $5\\%$ level of significance if the $p$-value is smaller than $0.05$ or, equivalently, if
$$ \\left\\lvert t^{act} \\right\\rvert > 1.96. $$
</div>

')
cat('\\begin{keyconcepts}[Testing the Hypothesis $E(Y) = \\mu_{Y,0}$ Against the Alternative $E(Y) \\neq \\mu_{Y,0}$]{3.6}
\\begin{enumerate}
\\item Estimate $\\mu_{Y}$ using $\\overline{Y}$ and compute $SE(\\overline{Y})$, the standard error of $SE(\\overline{Y})$.
\\item Compute the $t$-statistic.
\\item Compute the $p$-value and reject the null hypothesis at the $5\\%$ level of significance if the $p$-value is smaller than $0.05$ or, equivalently, if $$\\left\\lvert t^{act} \\right\\rvert > 1.96.$$
\\end{enumerate}
\\end{keyconcepts}
')
# plot the standard normal density on the domain [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Rejection Region of a Right-Sided Test',
      yaxs = 'i',
      xlab = 't-statistic',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add the x-axis
axis(1, 
     at = c(-4, 0, 1.64, 4), 
     padj = 0.5,
     labels = c('', 0, expression(Phi^-1~(.95)==1.64), ''))

# shade the rejection region in the left tail
polygon(x = c(1.64, seq(1.64, 4, 0.01), 4),
        y = c(0, dnorm(seq(1.64, 4, 0.01)), 0), 
        col = 'darkred')
# plot the the standard normal density on the domain [-4,4]
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Rejection Region of a Left-Sided Test',
      yaxs = 'i',
      xlab = 't-statistic',
      ylab = '',
      lwd = 2,
      axes = 'F')

# add x-axis
axis(1, 
     at = c(-4, 0, -1.64, 4), 
     padj = 0.5,
     labels = c('', 0, expression(Phi^-1~(.05)==-1.64), ''))

# shade rejection region in right tail
polygon(x = c(-4, seq(-4, -1.64, 0.01), -1.64),
        y = c(0, dnorm(seq(-4, -1.64, 0.01)), 0), 
        col = 'darkred')
cat('
<div class = "keyconcept" id="KC3.7">
<h3 class = "right"> Key Concept 3.7 </h3>          
<h3 class = "left"> Confidence Intervals for the Population Mean </h3>

A $95\\%$ confidence interval for $\\mu_Y$ is a random variable that contains the true $\\mu_Y$ in $95\\%$ of all possible random samples. When $n$ is large we can use the normal approximation. Then, $99\\%$, $95\\%$, $90\\%$ confidence intervals are

\\begin{align}
&99\\%\\text{ confidence interval for } \\mu_Y = \\left[ \\overline{Y} \\pm 2.58 \\times SE(\\overline{Y}) \\right], \\\\
&95\\%\\text{ confidence interval for } \\mu_Y = \\left[\\overline{Y} \\pm 1.96 \\times SE(\\overline{Y}) \\right], \\\\
&90\\%\\text{ confidence interval for } \\mu_Y = \\left[ \\overline{Y} \\pm 1.64 \\times SE(\\overline{Y}) \\right].
\\end{align}

These confidence intervals are sets of null hypotheses we cannot reject in a two-sided hypothesis test at the given level of confidence.

Now consider the following statements.

1. In repeated sampling, the interval
$$ \\left[ \\overline{Y} \\pm 1.96 \\times SE(\\overline{Y}) \\right] $$
covers the true value of $\\mu_Y$ with a probability of $95\\%$.

2. We have computed $\\overline{Y} = 5.1$ and $SE(\\overline{Y})=2.5$ so the interval
$$ \\left[ 5.1  \\pm 1.96 \\times 2.5 \\right] = \\left[0.2,10\\right] $$ covers the true value of $\\mu_Y$ with a probability of $95\\%$.

While 1. is right (this is in line with the definition above), 2. is wrong and none of your lecturers wants to read such a sentence in a term paper, written exam or similar, believe us.
The difference is that, while 1. is the definition of a random variable, 2. is one possible *outcome* of this random variable so there is no meaning in making any probabilistic statement about it. Either the computed interval does cover $\\mu_Y$ *or* it does not!

</div>
')
cat('\\begin{keyconcepts}[Confidence Intervals for the Population Mean]{3.7}
A $95\\%$ confidence interval for $\\mu_Y$ is a \\texttt{random variable} that contains the true $\\mu_Y$ in $95\\%$ of all possible random samples. When $n$ is large we can use the normal approximation. Then, $99\\%$, $95\\%$, $90\\%$ confidence intervals are

\\begin{align}
&99\\%\\text{ confidence interval for } \\mu_Y = \\left[ \\overline{Y} \\pm 2.58 \\times SE(\\overline{Y}) \\right], \\\\
&95\\%\\text{ confidence interval for } \\mu_Y = \\left[\\overline{Y} \\pm 1.96 \\times SE(\\overline{Y}) \\right], \\\\
&90\\%\\text{ confidence interval for } \\mu_Y = \\left[ \\overline{Y} \\pm 1.64 \\times SE(\\overline{Y}) \\right].
\\end{align}

These confidence intervals are sets of null hypotheses we cannot reject in a two-sided hypothesis test at the given level of confidence.\\newline

Now consider the following statements.\\newline

\\begin{enumerate}
\\item In repeated sampling, the interval
$$ \\left[ \\overline{Y} \\pm 1.96 \\times SE(\\overline{Y}) \\right] $$
covers the true value of $\\mu_Y$ with a probability of $95\\%$.

\\item We have computed $\\overline{Y} = 5.1$ and $SE(\\overline{Y})=2.5$ so the interval
$$ \\left[5.1  \\pm 1.96 \\times 2.5 \\right] = \\left[0.2,10\\right] $$ covers the true value of $\\mu_Y$ with a probability of $95\\%$.
\\end{enumerate}\\vspace{0.5cm}

While 1. is right (this is in line with the definition above), 2. is wrong and none of your lecturers wants to read such a sentence in a term paper, written exam or similar, believe us.
The difference is that, while 1. is the definition of a random variable, 2. is one possible \\textit{outcome} of this random variable so there is no meaning in making any probabilistic statement about it. Either the computed interval \\textit{does cover} $\\mu_Y$ or it \\textit{does not}!
\\end{keyconcepts}
')
# set seed
set.seed(1)

# generate some sample data
sampledata <- rnorm(100, 10, 10)

# check the type of the outcome produced by t.test
typeof(t.test(sampledata))

# display the list elements produced by t.test
ls(t.test(sampledata))
t.test(sampledata)$"conf.int"
t.test(sampledata)
# set random seed
set.seed(1)

# draw data from two different populations with equal mean
sample_pop1 <- rnorm(100, 10, 10)
sample_pop2 <- rnorm(100, 10, 20)

# perform a two sample t-test
t.test(sample_pop1, sample_pop2)
# load the 'readxl' package
library(readxl)
# import the data into R
cps <- read_excel(path = 'data/cps_ch3.xlsx')
# load the 'dplyr' package
library(dplyr)
# get an overview of the data structure
head(cps)

# group data by gender and year and compute the mean, standard deviation
# and number of observations for each group
avgs <- cps %>% 
        group_by(a_sex, year) %>% 
        summarise(mean(ahe08), 
                  sd(ahe08), 
                  n())

# print the results to the console
print(avgs)
# split the dataset by gender
male <- avgs %>% filter(a_sex == 1) 
female <- avgs %>% filter(a_sex == 2)

# rename columns of both splits
colnames(male)   <- c("Sex", "Year", "Y_bar_m", "s_m", "n_m")
colnames(female) <- c("Sex", "Year", "Y_bar_f", "s_f", "n_f")

# estimate gender gaps, compute standard errors and confidence intervals for all dates
gap <- male$Y_bar_m - female$Y_bar_f

gap_se <- sqrt(male$s_m^2 / male$n_m + female$s_f^2 / female$n_f)

gap_ci_l <- gap - 1.96 * gap_se

gap_ci_u <- gap + 1.96 * gap_se

result <- cbind(male[,-1], female[,-(1:2)], gap, gap_se, gap_ci_l, gap_ci_u)

# print the results to the console
print(result, digits = 3)
# set random seed
set.seed(123)

# generate dataset
X <- runif(n = 100, 
           min = 18, 
           max = 70)

Y <- X + rnorm(n=100, 50, 15)

# plot observations
plot(X, 
     Y, 
     type = "p",
     main = "A Scatterplot of X and Y",
     xlab = "Age",
     ylab = "Earnings",
     col = "steelblue",
     pch = 19)
# compute sample covariance of X and Y
cov(X, Y)

# compute sample correlation between X and Y
cor(X, Y)

# an equivalent way to compute the sample correlation
cov(X, Y) / (sd(X) * sd(Y))
library(MASS)

# set random seed
set.seed(1)

# positive correlation (0.81)
example1 <- mvrnorm(100,
                    mu = c(0, 0), 
                    Sigma = matrix(c(2, 2, 2, 3), ncol = 2),
                    empirical = TRUE)

# negative correlation (-0.81)
example2 <- mvrnorm(100,
                    mu = c(0, 0), 
                    Sigma = matrix(c(2, -2, -2, 3), ncol = 2),
                    empirical = TRUE)

# no correlation 
example3 <- mvrnorm(100,
                    mu = c(0, 0), 
                    Sigma = matrix(c(1, 0, 0, 1), ncol = 2),
                    empirical = TRUE)

# no correlation (quadratic relationship)
X <- seq(-3, 3, 0.01)
Y <- - X^2 + rnorm(length(X))

example4 <- cbind(X, Y)

# divide plot area as 2-by-2 array
par(mfrow = c(2, 2))

# plot datasets
plot(example1, col = 'steelblue', pch = 20, xlab = 'X', ylab = 'Y', 
     main = "Correlation = 0.81")

plot(example2, col = 'steelblue', pch = 20, xlab = 'X', ylab = 'Y', 
     main = "Correlation = -0.81")

plot(example3, col = 'steelblue', pch = 20, xlab = 'X', ylab = 'Y', 
     main = "Correlation = 0")

plot(example4, col = 'steelblue', pch = 20, xlab = 'X', ylab = 'Y', 
     main = "Correlation = 0")
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 1. Biased ... {-}

Consider the following alternative estimator for $\\mu_Y$, the mean of the $Y_i$

$$\\widetilde{Y}=\\frac{1}{n-1}\\sum\\limits_{i=1}^n Y_i$$

In this exercise we will illustrate that this estimator is a biased estimator for $\\mu_Y$.

**Instructions:**

  + Define a function <tt>Y_tilde</tt> that implements the estimator above.
  
  + Randomly draw 5 observations from the $\\mathcal{N}(10, 25)$ distribution and compute an estimate using <tt>Y_tilde()</tt>. Repeat this procedure 10000 times and store the results in <tt>est_biased</tt>. 
  
  + Plot a histogram of <tt>est_biased</tt>.
  
  + Add a red vertical line at $\\mu=10$ using the function <tt>abline()</tt>.
  
<iframe src="DCL/ex3_1.html" frameborder="0" scrolling="no" style="width:100%;height:400px"></iframe>

**Hints:**

  + To compute the sum of a vector you can use <tt>sum()</tt>, to get the length of a vector you can use <tt>length()</tt>.
  
  + Use the function <tt>replicate()</tt> to compute repeatedly estimates of random samples. With the arguments <tt>expr</tt> and <tt>n</tt> you can specify the operation and how often it has to be replicated.
  
  + A histogram can be plotted with the function <tt>hist()</tt>.
  
  + The point on the x-axis as well as the color for the vertical line can be specified via the arguments <tt>v</tt> and <tt>col</tt>.

</div>')
} else {
  cat('\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}')
}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 2. ... but consistent estimator {-}

Consider again the estimator from the previous exercise. It is available in your environment as the function <tt>Y_tilde()</tt>. You are requested to do the same procedure as in the previous exercise. This time, however, increase the number of observations to draw from 5 to 1000. 

What do you notice? What can you say about this estimator?

**Instructions:**

  + Randomly draw 1000 observations from the $\\mathcal{N}(10, 25)$ distribution and compute an estimate of the mean using <tt>Y_tilde()</tt>. Repeat this procedure 10000 times and store the results in <tt>est_consistent</tt>. 
  
  + Plot a histogram of <tt>est_consistent</tt>.
  
  + Add a red vertical line at $\\mu=10$ using the function <tt>abline()</tt>.
  
<iframe src="DCL/ex3_2.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + Use the function <tt>replicate()</tt> to compute estimates of repeatedly drawn random samples. Using the arguments <tt>expr</tt> and <tt>n</tt> you may specify the operation and how often it will be replicated.
  
  + A histogram can be plotted with the function <tt>hist()</tt>.
  
  + The position on the x-axis as well as the color for the vertical line can be specified via the arguments <tt>v</tt> and <tt>col</tt>.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 3. Efficiency of an Estimator {-}

In this exercise we want to illustrate the result that the sample mean

$$\\hat{\\mu}_Y=\\sum\\limits_{i=1}^{n}a_iY_i$$ with the equal weighting scheme $a_i=\\frac{1}{n}$ for $i=1,...,n$ is the best linear unbiased estimator (BLUE) of $\\mu_Y$. 

As an alternative, consider the estimator

$$\\tilde{\\mu}_Y=\\sum\\limits_{i=1}^{n}b_iY_i$$

where $b_i$ gives the first $\\frac{n}{2}$ observations a higher weighting than the second $\\frac{n}{2}$ observations (we assume that $n$ is even for simplicity). 

The vector of weights <tt>w</tt> has been defined already and is available in your working environment.

**Instructions:**

  + Verify that $\\tilde{\\mu}$ is an unbiased estimator of $\\mu_Y$, the mean of the $Y_i$.
  
  + Implement the alternative estimator of $\\mu_Y$ as a function <tt>mu_tilde()</tt>.

  + Randomly draw 100 observations from the $\\mathcal{N}(5, 10)$ distribution and compute estimates with both estimators. Repeat this procedure 10000 times and store the results in <tt>est_bar</tt> and <tt>est_tilde</tt>.
  
  + Compute the sample variances of <tt>est_bar</tt> and <tt>est_tilde</tt>. What can you say about both estimators?
  
<iframe src="DCL/ex3_3.html" frameborder="0" scrolling="no" style="width:100%;height:420px"></iframe>

**Hints:**

  + In order for $\\tilde{\\mu}$ to be an unbiased estimator all weights have to sum up to 1.
  
  + Use the function <tt>replicate()</tt> to compute estimates of repeatedly drawn samples. With the arguments <tt>expr</tt> and <tt>n</tt> you can specify the operation and how often it is replicated.

  + You may use <tt>var()</tt> the compute the sample variance.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 4. Hypothesis Test --- $t$-statistic {-}

Consider the CPS dataset from Chapter \\@ref(aattggoe) again. The dataset <tt>cps</tt> is available in your working environment.

We suppose that the average hourly earnings (in prices of 2012) <tt>ahe12</tt> exceed 23.50 $\\$/h$ and wish to test this hypothesis at a significance level of $\\alpha=0.05$. Please do the following:

**Instructions:**

  + Compute the test statistic by hand and assign it to <tt>tstat</tt>.
  
  + Use <tt>tstat</tt> to accept or reject the null hypothesis. Please do so using the normal approximation. 
  
<iframe src="DCL/ex3_4.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + We test $H_0:\\mu_{Y_{ahe}}\\leq 23.5$ vs. $H_1:\\mu_{Y_{ahe}}>23.5$. That is, we conduct a right-sided test.
  
  + The $t$-statistic is defined as $\\frac{\\bar{Y}-\\mu_{Y,0}}{s_{Y}/\\sqrt{n}}$ where $s_Y$ denotes the sample variance.

  + To decide whether the null hypothesis is accepted or rejected you can compare the $t$-statistic with the respective quantile of the standard normal distribution. Use logical operators.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 5. Hypothesis Test --- $p$-value {-}

Reconsider the test situation from the previous exercise. The dataset <tt>cps</tt> as well as the vector <tt>tstat</tt> are available in your working environment.

Instead of using the $t$-statistic as decision criterion you may also use the $p$-value. Now please do the following:

**Instructions:**

  + Compute the $p$-value by hand and assign it to <tt>pval</tt>.
  
  + Use <tt>pval</tt> to accept or reject the null hypothesis.
  
<iframe src="DCL/ex3_5.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + The $p$-value for a right-sided test can be computed as $p=P(t>t^{act}|H_0)$.
  
  + We reject the null if $p<\\alpha$. Use logical operators to check for this.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 6. Hypothesis Test --- One Sample $t$-test {-}

In the last two exercises we discussed two ways of conducting a hypothesis test. These approaches are somewhat cumbersome to apply by hand which is why <tt>R</tt> provides the function <tt>t.test()</tt>. It does most of the work automatically. <tt>t.test()</tt> provides $t$-statistics, $p$-values and even confidence intervals (more on the latter in later exercises). Note that <tt>t.test()</tt> uses the $t$-distribution instead of the normal distribution which becomes important when the sample size is small. 

The dataset <tt>cps</tt> and the variable <tt>pval</tt> from Exercise 3.4 are available in your working environment.

**Instructions:**

  + Conduct the hypothesis test from previous exercises using the function <tt>t.test()</tt>.
  
  + Extract the $t$-statistic and the $p$-value from the list created by <tt>t.test()</tt>. Assign them to the variables <tt>tstat</tt> and <tt>pvalue</tt>.
  
  + Verify that using the normal approximation here is valid as well by computing the difference between both $p$-values.
  
<iframe src="DCL/ex3_6.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + The type of the test as well as the null hypothesis can be specified via the arguments <tt>alternative</tt> and <tt>mu</tt>.
  
  + The $t$-statistic and the $p$-value can be obtained via <tt>\\$statistic</tt> and <tt>\\$p.value</tt>, respectively.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 7. Hypothesis Test --- Two Sample $t$-test {-}

Consider the annual maximum sea levels at Port Pirie (Southern Australia) and Fremantle (Western Australia) for the last 30 years.

The observations are made available as vectors <tt>portpirie</tt> and <tt>fremantle</tt> in your working environment.

**Instructions:**

  + Test whether there is a significant difference in the annual maximum sea levels at a significance level of $\\alpha=0.05$.

<iframe src="DCL/ex3_7.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + We test $H_0:\\mu_{P}-\\mu_{F}=0$ vs. $H_1:\\mu_{P}-\\mu_{F}\\ne 0$. That is, we conduct a two sample $t$-test.
  
  + For a two sample $t$-test the function <tt>t.test()</tt> expects two vectors containing the data.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 8. Confidence Interval {-}

Reconsider the test situation concerning the annual maximum sea levels at Port Pirie and Fremantle.

The variables <tt>portpirie</tt> and <tt>fremantle</tt> are again available in your working environment.

**Instructions:**

  + Construct a $95\\%$-confidence interval for the difference in the sea levels using <tt>t.test()</tt>.

<iframe src="DCL/ex3_8.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:**

  + The function <tt>t.test()</tt> computes a $95\\%$ confidence interval by default. This is accessible via <tt>$conf.int</tt>.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 9. (Co)variance and Correlation I {-}

Consider a random sample $(X_i, Y_i)$ for $i=1,...,100$.

The respective vectors <tt>X</tt> and <tt>Y</tt> are available in your working environment.

**Instructions:**

  + Compute the variance of $X$ using the function <tt>cov()</tt>.
  
  + Compute the covariance of $X$ and $Y$.
  
  + Compute the correlation between $X$ and $Y$.

<iframe src="DCL/ex3_9.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + The variance is a special case of the covariance.
  
  + <tt>cov()</tt> as well as <tt>cor()</tt> expect a vector for each variable.

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 10. (Co)variance and Correlation II {-}

In this exercise we want to examine the limitations of the correlation as a dependency measure. 

Once the session has initialized you will see the plot of 100 realizations from two random variables $X$ and $Y$.

The respective observations are available in the vectors <tt>X</tt> and <tt>Y</tt> in your working environment.

**Instructions:**

  + Compute the correlation between $X$ and $Y$. Interpret your result critically.

<iframe src="DCL/ex3_10.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hint:**

  + <tt>cor()</tt> expects a vector for each variable.

</div>')}
