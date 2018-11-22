sample(1:6, 1) 
pdfdata <- rbind("Outcome"=as.character(1:6), "Probability"=c("1/6","1/6","1/6","1/6","1/6","1/6"), "Cumulative Probability"=c("1/6","2/6","3/6","4/6","5/6","1"))
knitr::kable(pdfdata, format = my_output, caption = "PDF and CDF of a Dice Roll")
# generate the vector of probabilities 
probability <- rep(1/6, 6) 

# plot the probabilites 
plot(probability, 
     main = "Probability Distribution",
     xlab = "outcomes") 
# generate the vector of cumulative probabilities 
cum_probability <- cumsum(probability) 

# plot the probabilites 
plot(cum_probability, 
     xlab = "outcomes", 
     main = "Cumulative Probability Distribution") 
sample(c("H", "T"), 1) 
dbinom(x = 5,
       size = 10,
       prob = 0.5) 
# compute P(4 <= k <= 7) using 'dbinom()'
sum(dbinom(x = 4:7, 
         size = 10, 
         prob = 0.5))
# compute P(4 <= k <= 7) using 'pbinom()'
pbinom(size = 10, prob = 0.5, q = 7) - pbinom(size = 10, prob = 0.5, q = 3) 
# set up vector of possible outcomes
k <- 0:10
# assign the probabilities
probability <- dbinom(x = k,
                      size = 10, 
                      prob = 0.5)

# plot the outcomes against their probabilities
plot(x = k, 
     y = probability,
     main = "Probability Distribution Function") 
# compute cumulative probabilities
prob <- dbinom(x = 0:10, 
               size = 10, 
               prob = 0.5)

# plot the cumulative probabilities
plot(x = k, 
     y = prob,
     main = "Cumulative Distribution Function") 
cat('
<div class = "keyconcept" <div class = "keyconcept" id="KC2.1"> 
<h3 class = "right"> Key Concept 2.1 </h3> 
<h3 class= "left"> Expected Value and the Mean </h3> 

<p> Suppose the random variable $Y$
takes on $k$ possible values, $y_1, \\dots, y_k$, where $y_1$ denotes the first
value, $y_2$ denotes the second value, and so forth, and that the probability
that $Y$ takes on $y_1$ is $p_1$, the probability that $Y$ takes on $y_2$ is
$p_2$ and so forth. The expected value of $Y$, $E(Y)$ is defined as

$$ E(Y) = y_1 p_1 + y_2 p_2 + \\cdots + y_k p_k = \\sum_{i=1}^k y_i p_i $$

where the notation $\\sum_{i=1}^k y_i p_i$ means "the sum of $y_i$ $p_i$ for $i$
running from $1$ to $k$". The expected value of $Y$ is also called the mean of $Y$
or the expectation of $Y$ and is denoted by $\\mu_Y$.
</p> 
</div>')
cat('\\begin{keyconcepts}[Expected Value and the Mean]{2.1}
Suppose the random variable $Y$
takes on $k$ possible values, $y_1, \\dots, y_k$, where $y_1$ denotes the first
value, $y_2$ denotes the second value, and so forth, and that the probability
that $Y$ takes on $y_1$ is $p_1$, the probability that $Y$ takes on $y_2$ is
$p_2$ and so forth. The expected value of $Y$, $E(Y)$ is defined as

$$ E(Y) = y_1 p_1 + y_2 p_2 + \\cdots + y_k p_k = \\sum_{i=1}^k y_i p_i $$

where the notation $\\sum_{i=1}^k y_i p_i$ means \\"the sum of $y_i$ $p_i$ for $i$
running from $1$ to $k$\\". The expected value of $Y$ is also called the mean of $Y$
or the expectation of $Y$ and is denoted by $\\mu_Y$.
\\end{keyconcepts}')
# compute mean of natural numbers from 1 to 6
mean(1:6)
# set seed for reproducibility
set.seed(1)

# rolling a dice three times in a row
sample(1:6, 3, replace = T)
# set seed for reproducibility
set.seed(1)

# compute the sample mean of 10000 dice rolls
mean(sample(1:6, 
           10000, 
           replace = T))
cat('<div class = "keyconcept" id="KC2.2">
<h3 class = "right"> Key Concept 2.2 </h3> 
<h3 class= "left"> Variance and Standard Deviation </h3> 

<p> 
The variance of the discrete random variable $Y$, denoted $\\sigma^2_Y$, is
$$ \\sigma^2_Y = \\text{Var}(Y) = E\\left[(Y-\\mu_y)^2\\right] = \\sum_{i=1}^k (y_i - \\mu_y)^2 p_i $$
The standard deviation of $Y$ is $\\sigma_Y$, the square root of the variance. The units of the standard deviation are the same as the units of $Y$.
</p> 
</div>')
cat('\\begin{keyconcepts}[Variance and Standard Deviation]{2.2}
The variance of the discrete \\textit{random variable} $Y$, denoted $\\sigma^2_Y$, is
$$ \\sigma^2_Y = \\text{Var}(Y) = E\\left[(Y-\\mu_Y)^2\\right] = \\sum_{i=1}^k (y_i - \\mu_Y)^2 p_i $$
The standard deviation of $Y$ is $\\sigma_Y$, the square root of the variance. The units of the standard deviation are the same as the units of $Y$.
\\end{keyconcepts}')
var(1:6)
write_html(playground = T)
cat('<div class = "keyconcept" id="KC2.3"> 
<h3 class = "right"> Key Concept 2.3 </h3> 
<h3 class= "left"> Probabilities, Expected Value and Variance of a Continuous Random Variable </h3> 

<p> 

Let $f_Y(y)$ denote the probability density function of $Y$. The probability that $Y$ falls between $a$ and $b$ where $a < b$ is 
$$ P(a \\leq Y \\leq b) = \\int_a^b f_Y(y) \\mathrm{d}y. $$
We further have that $P(-\\infty \\leq Y \\leq \\infty) = 1$ and therefore $\\int_{-\\infty}^{\\infty} f_Y(y) \\mathrm{d}y = 1$.

As for the discrete case, the expected value of $Y$ is the probability weighted average of its values. Due to continuity, we use integrals instead of sums. The expected value of $Y$ is defined as

$$ E(Y) =  \\mu_Y = \\int y f_Y(y) \\mathrm{d}y. $$

The variance is the expected value of $(Y - \\mu_Y)^2$. We thus have

$$\\text{Var}(Y) =  \\sigma_Y^2 = \\int (y - \\mu_Y)^2 f_Y(y) \\mathrm{d}y.$$ 
</p> 
</div>')
cat('
\\begin{keyconcepts}[Probabilities\\comma Expected Value and Variance of a Continuous Random Variable]{2.3}
Let $f_Y(y)$ denote the probability density function of $Y$. The Probability that $Y$ falls between $a$ and $b$ where $a < b$ is 
$$ P(a \\leq Y \\leq b) = \\int_a^b f_Y(y) \\mathrm{d}y. $$
We further have that $P(-\\infty \\leq Y \\leq \\infty) = 1$ and therefore $\\int_{-\\infty}^{\\infty} f_Y(y) \\mathrm{d}y = 1$.

As for the discrete case, the expected value of $Y$ is the probability weighted average of its values. Due to continuity, we use integrals instead of sums. The expected value of $Y$ is defined as

$$ E(Y) =  \\mu_Y = \\int y f_Y(y) \\mathrm{d}y. $$

The variance is the expected value of $(Y - \\mu_Y)^2$. We thus have

$$\\text{Var}(Y) =  \\sigma_Y^2 = \\int (y - \\mu_Y)^2 f_Y(y) \\mathrm{d}y.$$
\\end{keyconcepts}')
# define functions
f <- function(x) 3 / x^4
g <- function(x) x * f(x)
h <- function(x) x^2 * f(x)
# compute area under the density curve
area <- integrate(f, 
                 lower = 1, 
                 upper = Inf)$value
area 

# compute E(X)
EX <- integrate(g,
                lower = 1,
                upper = Inf)$value
EX

# compute Var(X)
VarX <- integrate(h,
                  lower = 1,
                  upper = Inf)$value - EX^2 
VarX
# draw a plot of the N(0,1) PDF
curve(dnorm(x),
      xlim = c(-3.5, 3.5),
      ylab = "Density", 
      main = "Standard Normal Density Function") 
# compute denstiy at x=-1.96, x=0 and x=1.96
dnorm(x = c(-1.96, 0, 1.96))
# plot the standard normal CDF
curve(pnorm(x), 
      xlim = c(-3.5, 3.5), 
      ylab = "Density", 
      main = "Standard Normal Cumulative Distribution Function")
# define the standard normal PDF as an R function
f <- function(x) {
  1/(sqrt(2 * pi)) * exp(-0.5 * x^2)
}
# define a vector of reals
quants <- c(-1.96, 0, 1.96)

# compute densities
f(quants)

# compare to the results produced by 'dnorm()'
f(quants) == dnorm(quants)
# integrate f()
integrate(f, 
          lower = -Inf, 
          upper = 1.337)
# compute the probability using pnorm()
pnorm(1.337)
# compute the probability
1 - 2 * (pnorm(-1.96)) 
cat('<div class = "keyconcept" id="KC2.4">
<h3 class = "right"> Key Concept 2.4 </h3> 
<h3 class = "left"> Computing Probabilities Involving Normal Random Variables </h3>

<p>

Suppose $Y$ is normally distributed with mean $\\mu$ and variance $\\sigma^2$: $$Y
\\sim \\mathcal{N}(\\mu, \\sigma^2)$$ Then $Y$ is standardized by subtracting its mean and
dividing by its standard deviation: $$ Z = \\frac{Y -\\mu}{\\sigma} $$ Let $c_1$
and $c_2$ denote two numbers whereby $c_1 < c_2$ and further $d_1 = (c_1 - \\mu)
/ \\sigma$ and $d_2 = (c_2 - \\mu)/\\sigma$. Then

\\begin{align*} 
P(Y \\leq c_2) =& \\, P(Z \\leq d_2) = \\Phi(d_2), \\\\ 
P(Y \\geq c_1) =& \\, P(Z \\geq d_1) = 1 - \\Phi(d_1), \\\\ 
P(c_1 \\leq Y \\leq c_2) =& \\, P(d_1 \\leq Z \\leq d_2) = \\Phi(d_2) - \\Phi(d_1). 
\\end{align*}

</p> 
</div>')
cat('\\begin{keyconcepts}[Computing Probabilities Involving Normal Random Variables]{2.4}

Suppose $Y$ is normally distributed with mean $\\mu$ and variance $\\sigma^2$: $$Y
\\sim \\mathcal{N}(\\mu, \\sigma^2)$$ Then $Y$ is standardized by subtracting its mean and
dividing by its standard deviation: $$ Z = \\frac{Y -\\mu}{\\sigma} $$ Let $c_1$
and $c_2$ denote two numbers whereby $c_1 < c_2$ and further $d_1 = (c_1 - \\mu)
/ \\sigma$ and $d_2 = (c_2 - \\mu)/\\sigma$. Then
\\begin{align*} 
P(Y \\leq c_2) =& \\, P(Z \\leq d_2) = \\Phi(d_2), \\\\ 
P(Y \\geq c_1) =& \\, P(Z \\geq d_1) = 1 - \\Phi(d_1), \\\\ 
P(c_1 \\leq Y \\leq c_2) =& \\, P(d_1 \\leq Z \\leq d_2) = \\Phi(d_2) - \\Phi(d_1). 
\\end{align*}
\\end{keyconcepts}')
pnorm(4, mean = 5, sd = 5) - pnorm(3, mean = 5, sd = 5) 
library("knitr")
library("devtools")
url<-"https://plot.ly/~mca_unidue/22.embed?width=550&height=550?showlink=false" 
plotly_iframe <- paste("<center><iframe scrolling='no' seamless='seamless' style='border:none' src='", url, 
    "/800/1200' width='600' height='400'></iframe></center>", sep = "")
if (my_output=="html"){
  cat('
<iframe height="880" width="770" frameborder="0" scrolling="no" src="bivariatenormalv4.html"></iframe>')
} else {
  cat("\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}")
}
# plot the PDF
curve(dchisq(x, df = 3), 
      xlim = c(0, 10), 
      ylim = c(0, 1), 
      col = "blue",
      ylab = "",
      main = "p.d.f. and c.d.f of Chi-Squared Distribution, M = 3")

# add the CDF to the plot
curve(pchisq(x, df = 3), 
      xlim = c(0, 10), 
      add = TRUE, 
      col = "red")

# add a legend to the plot
legend("topleft", 
       c("PDF", "CDF"), 
       col = c("blue", "red"), 
       lty = c(1, 1))
# plot the density for M=1
curve(dchisq(x, df = 1), 
      xlim = c(0, 15), 
      xlab = "x", 
      ylab = "Density", 
      main = "Chi-Square Distributed Random Variables")

# add densities for M=2,...,7 to the plot using a 'for()' loop 
for (M in 2:7) {
  curve(dchisq(x, df = M),
        xlim = c(0, 15), 
        add = T, 
        col = M)
}

# add a legend
legend("topright", 
       as.character(1:7), 
       col = 1:7 , 
       lty = 1, 
       title = "D.F.")
# plot the standard normal density
curve(dnorm(x), 
      xlim = c(-4, 4), 
      xlab = "x", 
      lty = 2, 
      ylab = "Density", 
      main = "Densities of t Distributions")

# plot the t density for M=2
curve(dt(x, df = 2), 
      xlim = c(-4, 4), 
      col = 2, 
      add = T)

# plot the t density for M=4
curve(dt(x, df = 4), 
      xlim = c(-4, 4), 
      col = 3, 
      add = T)

# plot the t density for M=25
curve(dt(x, df = 25), 
      xlim = c(-4, 4), 
      col = 4, 
      add = T)

# add a legend
legend("topright", 
       c("N(0, 1)", "M=2", "M=4", "M=25"), 
       col = 1:4, 
       lty = c(2, 1, 1, 1))
pf(2, 3, 13, lower.tail = F)
# define coordinate vectors for vertices of the polygon
x <- c(2, seq(2, 10, 0.01), 10)
y <- c(0, df(seq(2, 10, 0.01), 3, 14), 0)

# draw density of F_{3, 14}
curve(df(x ,3 ,14), 
      ylim = c(0, 0.8), 
      xlim = c(0, 10), 
      ylab = "Density",
      main = "Density Function")

# draw the polygon
polygon(x, y, col = "orange")
if (my_output=="html"){
  cat('
<iframe src="DCL/playground.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>
')
}  
cat('
<div class = "keyconcept" id="KC2.5">
<h3 class = "right"> Key Concept 2.5 </h3> 
<h3 class = "left"> Simple Random Sampling and i.i.d. Random Variables </h3>
<p>
In simple random sampling, $n$ objects are drawn at random from a population. Each object is equally likely to end up in the sample. We denote the value of the random variable $Y$ for the $i^{th}$ randomly drawn object as $Y_i$.  Since all objects are equally likely to be drawn and the distribution of $Y_i$ is the same for all $i$, the $Y_i, \\dots, Y_n$ are independently and identically distributed (i.i.d.). This means the distribution of $Y_i$ is the same for all $i=1,\\dots,n$ and $Y_1$ is distributed independently of $Y_2, \\dots, Y_n$ and $Y_2$ is distributed independently of $Y_1, Y_3, \\dots, Y_n$ and so forth.
</p> 
</div>')
cat('\\begin{keyconcepts}[Simple Random Sampling and i.i.d. Random Variables]{2.5}
In simple random sampling, $n$ objects are drawn at random from a population. Each object is equally likely to end up in the sample. We denote the value of the random variable $Y$ for the $i^{th}$ randomly drawn object as $Y_i$.  Since all objects are equally likely to be drawn and the distribution of $Y_i$ is the same for all $i$, the $Y_i, \\dots, Y_n$ are independently and identically distributed (i.i.d.). This means the distribution of $Y_i$ is the same for all $i=1,\\dots,n$ and $Y_1$ is distributed independently of $Y_2, \\dots, Y_n$ and $Y_2$ is distributed independently of $Y_1, Y_3, \\dots, Y_n$ and so forth.
\\end{keyconcepts}')
sum(sample(1:6, 2, replace = T))
# Vector of outcomes
S <- 2:12

# Vector of probabilities
PS <- c(1:6, 5:1) / 36

# Expectation of S
ES <- S %*% PS
ES

# Variance of S
VarS <- (S - c(ES))^2 %*% PS
VarS
# divide the plotting area into one row with two columns
par(mfrow = c(1, 2))

# plot the distribution of S
barplot(PS, 
        ylim = c(0, 0.2), 
        xlab = "S", 
        ylab = "Probability", 
        col = "steelblue", 
        space = 0, 
        main = "Sum of Two Dice Rolls")

# plot the distribution of D 
probability <- rep(1/6, 6)
names(probability) <- 1:6

barplot(probability, 
        ylim = c(0, 0.2), 
        xlab = "D", 
        col = "steelblue", 
        space = 0, 
        main = "Outcome of a Single Dice Roll")
# set sample size and number of samples
n <- 10
reps <- 10000

# perform random sampling
samples <- replicate(reps, rnorm(n)) # 10 x 10000 sample matrix

# compute sample means
sample.avgs <- colMeans(samples)
# check that 'sample.avgs' is a vector
is.vector(sample.avgs) 

# print the first few entries to the console
head(sample.avgs)
# Plot the density histogram
hist(sample.avgs, 
     ylim = c(0, 1.4), 
     col = "steelblue" , 
     freq = F, 
     breaks = 20)

# overlay the theoretical distribution of sample averages on top of the histogram
curve(dnorm(x, sd = 1/sqrt(n)), 
      col = "red", 
      lwd = "2", 
      add = T)
# number of repititions
reps <- 10000

# set degrees of freedom of a chi-Square Distribution
DF <- 3 

# sample 10000 column vectors à 3 N(0,1) R.V.S
Z <- replicate(reps, rnorm(DF)) 

# column sums of squares
X <- colSums(Z^2)

# histogram of column sums of squares
hist(X, 
     freq = F, 
     col = "steelblue", 
     breaks = 40, 
     ylab = "Density", 
     main = "")

# add theoretical density
curve(dchisq(x, df = DF), 
      type = 'l', 
      lwd = 2, 
      col = "red", 
      add = T)
cat('
<div class = "keyconcept" id="KC2.6">
<h3 class = "right"> Key Concept 2.6 </h3> 
<h3 class = "left"> Convergence in Probability, Consistency and the Law of Large Numbers </h3>
<p>
The sample average $\\overline{Y}$ converges in probability to $\\mu_Y$: $\\overline{Y}$ is *consistent* for $\\mu_Y$ if the probability that $\\overline{Y}$ is in the range $(\\mu_Y - \\epsilon)$ to $(\\mu_Y + \\epsilon)$ becomes arbitrary close to $1$ as $n$ increases for any constant $\\epsilon > 0$. We write this as

$$ P(\\mu_Y-\\epsilon \\leq \\overline{Y} \\leq \\mu_Y + \\epsilon) \\rightarrow 1, \\, \\epsilon > 0 \\text{ as } n\\rightarrow\\infty. $$

Consider the independently and identically distributed random variables $Y_i, i=1,\\dots,n$ with expectation $E(Y_i)=\\mu_Y$ and variance $\\text{Var}(Y_i)=\\sigma^2_Y$. Under the condition that $\\sigma^2_Y< \\infty$, that is, large outliers are unlikely, the law of large numbers thus states that

$$ \\overline{Y} \\xrightarrow[]{p} \\mu_Y. $$

The following application simulates a large number of coin tosses (you may set the number of trials using the slider) with a fair coin and computes the fraction of heads observed for each additional toss. The result is a random path that, as stated by the law of large numbers, shows a tendency to approach the value of $0.5$ as $n$ grows. 
<iframe height="570" width="800" frameborder="0" scrolling="no" src="CoinTossingWLLN.html"></iframe>
</p> 
</div>')
cat('\\begin{keyconcepts}[Convergence in Probability\\comma Consistency and the Law of Large Numbers]{2.6}
The sample average $\\overline{Y}$ converges in probability to $\\mu_Y$: $\\overline{Y}$ is \\textit{consistent} for $\\mu_Y$ if the probability that $\\overline{Y}$ is in the range $(\\mu_Y - \\epsilon)$ to $(\\mu_Y + \\epsilon)$ becomes arbitrary close to $1$ as $n$ increases for any constant $\\epsilon > 0$. We write this as

$$ P(\\mu_Y-\\epsilon \\leq \\overline{Y} \\leq \\mu_Y + \\epsilon) \\rightarrow 1, \\, \\epsilon > 0 \\text{ as } n\\rightarrow\\infty. $$

Consider the independently and identically distributed random variables $Y_i, i=1,\\dots,n$ with expectation $E(Y_i)=\\mu_Y$ and variance $\\text{Var}(Y_i)=\\sigma^2_Y$. Under the condition that $\\sigma^2_Y< \\infty$, that is, large outliers are unlikely, the law of large numbers states

$$ \\overline{Y} \\xrightarrow[]{p} \\mu_Y. $$

The following application simulates a large number of coin tosses (you may set the number of trials using the slider) with a fair coin and computes the fraction of heads observed for each additional toss. The result is a random path that, as stated by the law of large numbers, shows a tendency to approach the value of $0.5$ as $n$ grows.\\newline
\\begin{center}
\\textit{This interactive application is only available in the HTML version.}
\\end{center}
\\end{keyconcepts}')
# set seed
set.seed(1)

# set number of coin tosses and simulate
N <- 30000
Y <- sample(0:1, N, replace = T)

# Calculate R_n for 1:N
S <- cumsum(Y)
R <- S/(1:N)

# Plot the path.
plot(R, 
     ylim = c(0.3, 0.7), 
     type = "l", 
     col = "steelblue", 
     lwd = 2, 
     xlab = "n", 
     ylab = "R_n",
     main = "Converging Share of Heads in Repeated Coin Tossing")

# Add a dashed line for R_n = 0.5
lines(c(0, N), 
      c(0.5, 0.5), 
      col = "darkred", 
      lty = 2, 
      lwd = 1)
cat('
<div class = "keyconcept" id="KC2.7">
<h3 class = "right"> Key Concept 2.7 </h3> 
<h3 class = "left"> The Central Limit Theorem </h3>
<p>
Suppose that $Y_1,\\dots,Y_n$ are independently and identically distributed random variables with expectation $E(Y_i)=\\mu_Y$ and variance $\\text{Var}(Y_i)=\\sigma^2_Y$ where $0<\\sigma^2_Y<\\infty$.<br>
The Central Limit Theorem (CLT) states that, if the sample size $n$ goes to infinity, the distribution of the standardized sample average 
$$ \\frac{\\overline{Y} - \\mu_Y}{\\sigma_{\\overline{Y}}} = \\frac{\\overline{Y} - \\mu_Y}{\\sigma_Y/\\sqrt{n}} \\ $$
becomes arbitrarily well approximated by the standard normal distribution.

The application below demonstrates the CLT for the sample average of normally distributed random variables with mean $5$ and variance $25^2$. You may check the following properties:

+ The distribution of the sample average is normal.
+ As the sample size increases, the distribution of $\\overline{Y}$ tightens around the true mean of $5$.
+ The distribution of the standardized sample average is close to the standard normal distribution for large $n$.

<iframe height="620" width="800" frameborder="0" scrolling="no" src="normaldisthistWLLN.html"></iframe>
</p> 
</div>')
cat('\\begin{keyconcepts}[The Central Limit Theorem]{2.7}
Suppose that $Y_1,\\dots,Y_n$ are independently and identically distributed random variables with expectation $E(Y_i)=\\mu_Y$ and variance $\\text{Var}(Y_i)=\\sigma^2_Y$ where $0<\\sigma^2_Y<\\infty$. The Central Limit Theorem (CLT) states that, if the sample size $n$ goes to infinity, the distribution of the standardized sample average 
$$ \\frac{\\overline{Y} - \\mu_Y}{\\sigma_{\\overline{Y}}} = \\frac{\\overline{Y} - \\mu_Y}{\\sigma_Y/\\sqrt{n}} \\ $$
becomes arbitrarily well approximated by the standard normal distribution.

The application below demonstrates the CLT for the sample average of normally distributed random variables with mean $5$ and variance $25^2$. You may check the following properties:\\newline

\\begin{itemize}
\\item The distribution of the sample average is normal.
\\item As the sample size increases, the distribution of $\\overline{Y}$ tightens around the true mean of $5$.
\\item The distribution of the standardized sample average is close to the standard normal distribution for large $n$.
\\end{itemize}
\\vspace{0.5cm}
\\begin{center}
\\textit{This interactive application is only available in the HTML version.}
\\end{center}

\\end{keyconcepts}
')
# subdivide the plot panel into a 2-by-2 array
par(mfrow = c(2, 2))

# set the number of repetitions and the sample sizes
reps <- 10000
sample.sizes <- c(2, 10, 50, 100)

# outer loop (loop over the sample sizes)
  for (n in sample.sizes) {
    
    samplemean <- rep(0, reps) #initialize the vector of sample menas
    stdsamplemean <- rep(0, reps) #initialize the vector of standardized sample menas

# inner loop (loop over repetitions)   
    for (i in 1:reps) {
      x <- rbinom(n, 1, 0.5)
      samplemean[i] <- mean(x)
      stdsamplemean[i] <- sqrt(n)*(mean(x) - 0.5)/0.5
    }
    
# plot the histogram and overlay it with the N(0,1) density for every iteration    
    hist(stdsamplemean, 
         col = "steelblue", 
         breaks = 40, 
         freq = FALSE, 
         xlim = c(-3, 3), 
         ylim = c(0, 0.4), 
         xlab = paste("n =", n), 
         main = "")
    
    curve(dnorm(x), 
          lwd = 2, 
          col = "darkred", 
          add = TRUE)
  }  
write_html(playground = T)
if (my_output=="html"){
  cat('
<div  class = "DCexercise">

#### 1. Sampling {-}

Suppose you are the lottery fairy in a weekly lottery, where $6$ out of $49$ *unique* numbers are drawn.

**Instructions:**

  + Draw the winning numbers for this week.

<iframe src="DCL/ex2_1.html" frameborder="0" scrolling="no" style="width:100%;height:360px"></iframe>

**Hints:**

  + You may use the function <tt>sample()</tt> to draw random numbers, see `?sample`.

  + The set of elements to be sampled from here is $\\{1,...,49\\}$.

</div>')
} else {
  cat('\\begin{center}\\textit{This interactive part of the book is only available in the HTML version.}\\end{center}')
}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">

#### 2. Probability Density Function {-}

Consider a random variable $X$ with probability density function (PDF)

$$f_X(x)=\\frac{x}{4}e^{-x^2/8},\\quad x\\geq 0.$$

**Instructions:**

  + Define the PDF from above as a function <tt>f()</tt>. <tt>exp(a)</tt> computes $e^a$. 

  + Check whether the function you have defined is indeed a PDF.

<iframe src="DCL/ex2_2.html" frameborder="0" scrolling="no" style="width:100%;height:360px"></iframe>

**Hints:**

  + Use <tt>function(x) {...}</tt> to define a function which takes the argument <tt>x</tt>.

  + In order for <tt>f()</tt> to be a PDF, its integral over the whole domain has to equal 1: $\\int_0^\\infty f_X(x)\\mathrm{d}x=1$.

  + The function <tt>integrate()</tt> performs integration. You have to specify the function to be integrated as well as lower and upper limits of integration. These may be set to $[-\\infty,\\infty]$ by setting the corresponding arguments to <tt>-Inf</tt> and <tt>Inf</tt>. You can access the numerical value of the computed integral by appending <tt>$value</tt>. See `?integral for a detailed description of the function.

</div>
')}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">

#### 3. Expected Value and Variance {-}

In this exercise you have to compute the expected value and the variance of the random variable $X$ considered in the previous exercise. 

The PDF <tt>f()</tt> from the previous exercise is available in your working environment. 

**Instructions:**

  + Define a suitable function <tt>ex()</tt> which integrates to the expected value of $X$.

  + Compute the expected value of $X$. Store the result in <tt>expected_value</tt>.

  + Define a suitable function <tt>ex2()</tt> which integrates to the expected value of $X^2$.

  + Compute the variance of $X$. Store the result in <tt>variance</tt>.

<iframe src="DCL/ex2_3.html" frameborder="0"" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + The expected value of $X$ is defined as $E(X)=\\int_0^\\infty xf_X(x)dx$.
  
  + The value of an integral computed by <tt>integrate()</tt> can be obtained via <tt>\\$value</tt>.

  + The variance of $X$ is defined as $Var(X)=E(X^2)-E(X)^2$, where $E(X^2)=\\int_0^\\infty x^2f_X(x)\\mathrm{d}x$.
  
</div>')}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">
  
#### 4. Standard Normal Distribution I {-}

Let $Z\\sim\\mathcal{N}(0, 1)$.

**Instructions:**

  + Compute $\\phi(3)$, that is, the value of the standard normal density at $c=3$.

<iframe src="DCL/ex2_4.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>
  
**Hints:**

  + Values of $\\phi(\\cdot)$ can be computed using <tt>dnorm()</tt>. Note that by default <tt>dnorm()</tt> uses <tt>mean = 0</tt> and <tt>sd = 1</tt> so there is no need to set the corresponding arguments when you whish to obtain density values of the standard normal distribution.  

</div>')}
if (my_output=="html") {
  cat('
<div  class = "DCexercise">

#### 5. Standard Normal Distribution II {-}

Let $Z\\sim\\mathcal{N}(0, 1)$.

**Instructions:**

  + Compute $P(|Z|\\leq 1.64)$ by using the function <tt>pnorm()</tt>.

<iframe src="DCL/ex2_5.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + $P(|Z|\\leq z) = P(-z \\leq Z \\leq z)$.
  
  + Probabilities of the form $P(a \\leq Z \\leq b)$ can be computed as $P(Z\\leq b)-P(Z\\leq a)=F_Z(b)-F_Z(a)$ with $F_Z(\\cdot)$ the cumulative distribution function (CDF) of $Z$. Alternatively, you may exploit the symmetry of the standard normal distribution.
  
</div>')}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">

#### 6. Normal Distribution I {-}

Let $Y\\sim\\mathcal{N}(5, 25)$.

**Instructions:**

  + Compute the 99% quantile of the given distribution, i.e., find $y$ such that $\\Phi(\\frac{y-5}{5})=0.99$.

<iframe src="DCL/ex2_6.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + You can compute quantiles of the normal distribution by using the function <tt>qnorm()</tt>.

  + Besides the quantile to be computed you have to specify the mean and the standard deviation of the distribution. This is done via the arguments <tt>mean</tt> and <tt>sd</tt>. Note that <tt>sd</tt> sets the standard deviation, not the variance!
  
  + <tt>sqrt(a)</tt> returns the square root of the numeric argument <tt>a</tt>.

</div>')}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">

#### 7. Normal Distribution II {-}

Let $Y\\sim\\mathcal{N}(2, 12)$.

**Instructions:**

  + Generate $10$ random numbers from this distribution.

<iframe src="DCL/ex2_7.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + You can use <tt>rnorm()</tt> to draw random numbers from a normal distribution.

  + Besides the number of draws you have to specify the mean and the standard deviation of the distribution. This can be done via the arguments <tt>mean</tt> and <tt>sd</tt>. Note that <tt>sd</tt> requires the standard deviation, not the variance!

</div>')}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">
  
#### 8. Chi-squared Distribution I {-}

Let $W\\sim\\chi^2_{10}$.

**Instructions:**

  + Plot the corresponding PDF using <tt>curve()</tt>. Specify the range of x-values as $[0,25]$ via the argument <tt>xlim</tt>.
  
<iframe src="DCL/ex2_8.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + <tt>curve()</tt> expects a function and its parameters as arguments (here <tt>dchisq()</tt> and the degrees of freedom <tt>df</tt>).
  
  + The range of x-values in <tt>xlim</tt> can be passed as a vector of interval bounds.
  
</div>')}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">
  
#### 9. Chi-squared Distribution II {-}

Let $X_1$ and $X_2$ be two independent normally distributed random variables with $\\mu=0$ and $\\sigma^2=15$.

**Instructions:**

  + Compute $P(X_1^2+X_2^2>10)$.
  
<iframe src="DCL/ex2_9.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + Note that $X_1$ and $X_2$ are not $\\mathcal{N}(0,1)$ but $\\mathcal{N}(0,15)$ distributed. Hence you have to scale appropriately. Afterwards you can use <tt>pchisq()</tt> to compute the probability. 
  + The argument <tt>lower.tail</tt> may be helpful.
  
</div>')}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">
  
#### 10. Student t Distribution I {-}

Let $X\\sim t_{10000}$ and $Z\\sim\\mathcal{N}(0,1)$.

**Instructions:**

  + Compute the $95\\%$ quantile of both distributions. What do you notice?
  
<iframe src="DCL/ex2_10.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + You may use <tt>qt()</tt> and <tt>qnorm()</tt> to compute quantiles of the given distributions.
  
  + For the $t$ distribution you have to specify the degrees of freedom <tt>df</tt>.
  
</div>')}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">
  
#### 11. Student t Distribution II {-}

Let $X\\sim t_1$. Once the session has initialized you will see the plot of the corresponding probability density function (PDF).

**Instructions:**

  + Generate $1000$ random numbers from this distribution and assign them to the variable <tt>x</tt>.
  
  + Compute the sample mean of <tt>x</tt>. Can you explain the result?
  
<iframe src="DCL/ex2_11.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + You can use <tt>rt()</tt> to draw random numbers from a t distribution.
  
  + Note that the t distribution is fully determined through the degree(s) of freedom. Specify them via the argument <tt>df</tt>.

  + To compute the sample mean of a vector you can use the function <tt>mean()</tt>.
  
</div>')}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">

#### 12. F Distribution I {-}

Let $Y\\sim F(10, 4)$.

**Instructions:**

  + Plot the quantile function of the given distribution using the function <tt>curve()</tt>.
  
<iframe src="DCL/ex2_12.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + <tt>curve()</tt> expects the function with their respective parameters (here: degrees of freedom <tt>df1</tt> and <tt>df2</tt>) as an argument.
  
</div>')}
if (my_output=="html"){
  cat('
<div  class = "DCexercise">
  
#### 13. F Distribution II {-}

Let $Y\\sim F(4,5)$.

**Instructions:**

  + Compute $P(1<Y<10)$ by integration of the PDF.
  
<iframe src="DCL/ex2_13.html" frameborder="0" scrolling="no" style="width:100%;height:340px"></iframe>

**Hints:**

  + Besides providing the function to be integrated, you have to specify lower and upper bounds of integration.
  
  + The additional parameters of the distribution (here <tt>df1</tt> and <tt>df2</tt>) also have to be passed *inside* the call of <tt>integrate()</tt>.
  
  + The value of the integral can be obtained via <tt>\\$value</tt>.
  
</div>')}
