#' 2. Aggregate contact types by species
#'
#' The 2007 survey included separate questions on 10 different types of contacts (hunt, found dead, butcher, skin, cook, eat, eat raw, play with, scratched by, bitten by).  Because we aimed to make quantitative comparisons with the 1982 survey, which did not collect separate frequencies for different types of contact, we needed to aggregate the contact types to a single representative frequency.  As a conservative assumption, we assumed that different types of contact with a given animal species were likely to occur on the same day, so contact frequencies should not be summed.  Instead, we took the maximum frequency reported across all contact types within a species, and used this as the contact frequency for that species.
#' @param mpx1b MPX 2007 survey dataset
#' @param orig.animal.names The original animal names given in the 2007 survey
#' @return A data frame with the maximum contact value for each species for each surveyed individual
#' @examples
#' contact.type.max(mpx1b, orig.animal.names)


contact.type.max <- function(mpx1b, orig.animal.names) {
    ## 5) Take maximum contact frequency across all contact types within a species

    # for each original animal named, take the maximum number of contacts reported for any activity with that animal for each individual
    #   (within a species, take value for the type of contact that occurred most frequently)

    # data frame to store the maximum contact value for each species and each surveyed individual:
    max.contacts.by.species = data.frame('NetID' = mpx1b$NetID, 'FormID'=mpx1b$FormID)

    for (aa in 1:length(orig.animal.names)){
      cur.name = orig.animal.names[aa]
      ## which columns are associated with this animal?
      # only include those that have 'f_' - these indicate frequencies of contact rather than binary yes/no contact
      cur.cols = grep(paste('f_',cur.name,sep=''),names(mpx1b))

      # take out the relevant section of the data frame
      relevant.section = as.matrix(mpx1b[,cur.cols])

      # take the max over each row using apply
      max.contacts = suppressWarnings(apply(relevant.section, 1, max, na.rm=TRUE))
      # NOTE: this will give warnings because many lines don't have any non-NA values but that's okay
      # currently, NAs are represented as -Inf, so replace
      max.contacts[max.contacts==-Inf]=NA

      # insert these new values into the data frame
      max.contacts.by.species = cbind(max.contacts.by.species, max.contacts)

    }
    names(max.contacts.by.species) = c('NetID', 'FormID', orig.animal.names)

    return(max.contacts.by.species)
}
