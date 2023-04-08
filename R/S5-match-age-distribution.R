#' 5. Match age distribution of survey participants
#'
#' The 1982 survey data come from the Control group of unaffected individuals, roughly age-matched to individuals who were seropositive for exposure to MPXV.  Because of the distribution of susceptibility at this time, the composition of the 1982 respondents are strongly skewed to children under 10 years of age.  In contrast, the 2007 survey includes individuals of all ages, who might exhibit quite different patterns of animal contact from children.  To control for the age of respondents, and hence to arrive at the most accurate estimate for the influence of changes in the wildlife community on the animal-human contact rates in this system, we subsampled the 2007 survey respondents to match the distribution of ages in the 1982 respondents.
#' @param mpx1b MPX 2007 survey dataset
#' @param data1980control Control responses from the 1980 MPX survey
#' @param method Controls how ages are matched or assembled from the 2000s survey. Default is 'matched' (also takes 'all<27' or 'unif<27')
#' @keywords responses
#' @return A vector of rows to sample to match the age distribution of the 1980s survey
#' @examples
#' age.match(mpx1b, data1980control, method = 'matched')

age.match <- function(mpx1b, data1980control, method) {

  age.rows.2000s = c()

  if (method == 'matched') { # Age-matched to the 1980s dataset
    for(ii in 1:max(data1980control$Age)) {
      age.rows.2000s = c(age.rows.2000s,
                         sample(which(mpx1b$approxAge == ii),
                                size = round(sum(data1980control$Age == ii)),
                                replace=TRUE))
    }
  } else if (method == 'all<27') { # Take all individuals <27 yr

    age.rows.2000s = which(mpx1b$approxAge < 27)

  } else if (method == 'unif<27') { # Minimum count <27 is 37. Age 0 is excluded because there's only 1.
    for(ii in 1:26) {
      age.rows.2000s = c(age.rows.2000s,
                         sample(which(mpx1b$approxAge == ii),
                                size = 37,
                                replace=TRUE))
    }
  } else (print('Incorrect method call.'))

  return(age.rows.2000s)
}

