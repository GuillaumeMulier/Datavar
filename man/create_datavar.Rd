% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Deprecated.R
\name{create_datavar}
\alias{create_datavar}
\title{create_datavar}
\usage{
create_datavar(
  data,
  colonnes = colnames(data),
  default_datavar = options_datavar()
)
}
\arguments{
\item{data}{The dataset for which we want a datavar.}

\item{colonnes}{The columns of the dataset that you want to describe. By default, all the columns of the dataset are to be described.}

\item{default_datavar}{The default options for the creation. Made by the function \code{options_datavar}.}
}
\value{
A data.frame of 11 variables:
\itemize{
\item var: the name of each variable. All described variables should be in the datavar;
\item type: type of variable (quanti for a quantitative variable, quali for a categorial variable with more than 2 items and binary for a binary variable);
\item coefbin: for categorical and binary variables, TRUE if you want to display the confidence interval of the proportion computed with binomial distribution;
\item prec: number of decimals;
\item nomcateg: for binary variables, the name of the class in the data to be displayed;
\item label: for binary variables, the name displayed in the description table;
\item nomvariable: for categorical and quantitative variables, the name of the variable displayed in the description table;
\item ordonnee: for categorical variables, TRUE if you want to let the classes in order and FALSE if you want them by descending count;
\item mode: for quantitative variables, string indicating which statistics to be displayed (moy for mean, med for median, sd for standard error, iq for interquartile interval and rg for range);
\item test: string indicating the statistical test to perform (none for none, student for Student's test, wilcoxon for Wilcoxon's signed rank test, chisq for Chi-2 test and fisher for Fisher's test);
\item chif_pval: number of decimals for Pvalues.
}
}
\description{
Creation of a default datavar
}
\examples{
create_datavar(mtcars)
}
\seealso{
\code{\link{descr}}
}
