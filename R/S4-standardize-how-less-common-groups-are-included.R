#' 4. Standardize how less commonly contacted groups are included
#'
#' While the 2007 survey collected contact-frequency information for all species, Because the 1982 survey only collected data on the 3 most contacted groups. When full data were available, frequent contacts were often reported with more than the top three groups, and ignoring these missing values from the 1982 survey would result in biased contact estimates. To harmonize the 1982 and 2007 surveys, we assigned sampled values to animal groups that were not reported as the top three most-frequent contacts. Values were sampled from a uniform distribution from zero to the third-place contact rate.
#'
#' To bring data from the two surveys onto equivalent footing, we applied the following consistent set of actions: 1) For 1982 responses, use data on all 3 groups reported.  For all other groups, not reported explicitly, the contact rate was unifromtly sampled from zero to the third place contact rate for that respondent. 2) For 2007 responses, use data from the 3 groups with highest reported contact frequencies.  In the event of a tie for the 3rd spot, choose randomly which to include. For all other groups, below the top 3, we used the same procedure as for 1982 survey data, to maximize comparability.
#' @param case Determines how to summarize the 2000s data. Suitable values are: resample or lower.
#' @param total.contacts.by.group A group-level summary of contacts (generated via aggregate.species.groups())
#' @return A data frame of the top 3 contacts by each surveyed individual


replicate.1980 <- function(total.contacts.by.group, case = 'resample') {
  ## 8) Replicate 1980s sampling of only top 3 animals

  total.contacts.top.three = total.contacts.by.group

  # iterate through individuals in the survey
  for (ii in 1:length(total.contacts.by.group[,1])){

    if (sum(!is.na(total.contacts.by.group[ii,-c(1,2)]))>0){
      # which is the largest number - if there are ties, sample at random
      #  notice we ignore the first two columns, which are id values
      elig.col1 = which(total.contacts.by.group[ii,-c(1,2)]==max(total.contacts.by.group[ii,-c(1,2)], na.rm=TRUE))
      if(length(elig.col1)==1){
        max.col1 = elig.col1
      } else if(length(elig.col1)>1){
        max.col1 = sample(elig.col1,size=1)
      } else{
        warning(paste('error for individual ii=',ii))
      }

      # now take the second largest number (with the largest removed)
      elig.col2 = which(total.contacts.by.group[ii,-c(1,2)]==max(total.contacts.by.group[ii,-c(1,2, max.col1+2)], na.rm=TRUE))
      # make sure max.col1 is not included
      elig.col2 = elig.col2[which(elig.col2!=max.col1)]
      if(length(elig.col2)==1){
        max.col2 = elig.col2
      } else if(length(elig.col2)>1){
        max.col2 = sample(elig.col2,size=1)
      } else{
        warning(paste('error for individual ii=',ii))
      }

      # now take the third largest number (with the largest two removed)
      elig.col3 = which(total.contacts.by.group[ii,-c(1,2)]==max(total.contacts.by.group[ii,-c(1,2, max.col1+2, max.col2+2)], na.rm=TRUE))
      # make sure max.col1 and max.col2 are not included
      elig.col3 = elig.col3[intersect(which(elig.col3!=max.col1), which(elig.col3!=max.col2))]
      if(length(elig.col3)==1){
        max.col3 = elig.col3
      } else if(length(elig.col3)>1){
        max.col3 = sample(elig.col3,size=1)
      } else{
        warning(paste('error for individual ii=',ii))
      }

      # change contact rates depending on case
      if (case == 'resample') {
        # change all but the top three to uniformly sampled freq between 0 and 3rd place freq
        total.contacts.top.three[ii,3:(length(animal.group.names)+2)] = stats::runif(1, 0, total.contacts.by.group[ii,(max.col3+2)])
      } else if (case == 'lower') {
        # change all but the top three to the zero
        total.contacts.top.three[ii,3:(length(animal.group.names)+2)] = 0
      } else {print('Incorrect case call.')
        break
      }

      total.contacts.top.three[ii,(max.col1+2)] = total.contacts.by.group[ii,(max.col1+2)]
      total.contacts.top.three[ii,(max.col2+2)] = total.contacts.by.group[ii,(max.col2+2)]
      total.contacts.top.three[ii,(max.col3+2)] = total.contacts.by.group[ii,(max.col3+2)]

      if((total.contacts.by.group[ii,(max.col1+2)]>30) |
         (total.contacts.by.group[ii,(max.col2+2)]>30) |
         (total.contacts.by.group[ii,(max.col3+2)]>30)){
        print(ii)
        break
      }
    }

  }

  return(total.contacts.top.three)

}

