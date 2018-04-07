\name{cross_validate}
\alias{cross_validate}
\title{Cross Validation}
\usage{
cross_validate(df, tree, n_iter, split_ratio, method)
}
\arguments{
\item{df}{data frame on which cross-validation is being performed.}
\item{tree}{tree object generated from rpart on df}
\item{n_iter}{number of iterations for cross-validation}
\item{split_ratio}{train/test split ratio for cross-validation. split_ratio is supposed to be between 0 to 1.}
\item{method}{method for decision tree. The default value is 'class' for classification. Set method = 'anova' for regression.}
}
\description{
returns a list containing a data frame of accuracy values and other cross-validation statistics. In the data frame, accuracy_subset contains accuracy of cross-validation on user-specified features, while accuracy_all contains accuracy of cross-validation on all the available features in the data frame df. average_accuracy_subset is the average accuracy of n_iter iterations of cross-validation with user-specified features. average_acuracy_all is the average accuracy of n_iter iterations of cross-validation with all the available features. variance_accuracy_subset is the variance of accuracy of n_iter iterations of cross-validation with user-specified features. variance_accuracy_all is the variance of accuracy of n_iter iterations of cross-validation with all the available features.
}
\examples{
cross_validate(df, tree, n_iter = 10, split_ratio = 0.8, method = 'class')
}
