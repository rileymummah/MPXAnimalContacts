#' Run all steps of 2000s analysis
#'
#' @param ii Set to 1 if running function independently. Used for function iteration in apply statement.
#' @param mpx1b MPX 2007 survey dataset
#' @param data1980control Control responses from the 1980 MPX survey
#' @param method Controls how ages are matched or assembled from the 2000s survey. Default is 'matched' (also takes 'all<27' or 'unif<27')
#' @param orig.animal.names The original animal names given in the 2007 survey
#' @param animal.groups, Non-unique animal groups to which the orig.animal.names are assigned
#' @param animal.group.names Unique animal groups to match the 1980 MPX survey
#' @param case Default = 'lower'. Also takes 'upper' or 'midpoint'
#' @param method Default = 'matched'. Also takes 'all<27' or 'unif<27'
#' @keywords responses
#' @return A vector of rows to sample to match the age distribution of the 1980s survey


run.MPXfxns.2000s <- function(ii, mpx1b, data1980control,
                              orig.animal.names, animal.group.names, animal.groups,
                              case = 'resample',
                              method = 'matched') {

  mean.vals.2000s <- c()

  ## 1. Convert 2007 survey responses to contact frequencies
  tmp <- sample.contact.freq(mpx1b)

  ## 2. Aggregate contact types
  tmp <- contact.type.max(tmp, orig.animal.names)

  ## 3. Aggregate species into groups, matching 1982 groups
  tmp <- species.groups.sum(mpx1b, animal.group.names, tmp)

  ## 4. Standardize how less-commonly contacted groups are included
  replicate.1980(case = case, tmp) -> total.contacts.top.three

  ## 5. Match age distribution of survey respondents
  age.rows.2000s = age.match(mpx1b, data1980control, method = method)

  mean.vals.2000s = rbind(mean.vals.2000s,
                          c(mean(total.contacts.top.three$rodent[age.rows.2000s], na.rm=TRUE),
                            mean(total.contacts.top.three$NHP[age.rows.2000s], na.rm=TRUE),
                            mean(total.contacts.top.three$antelope[age.rows.2000s], na.rm=TRUE),
                            mean(total.contacts.top.three$boar[age.rows.2000s], na.rm=TRUE)))
                            # mean(total.contacts.top.three$squirrel[age.rows.2000s], na.rm=TRUE)))

  print(ii)

  return(mean.vals.2000s)
}

