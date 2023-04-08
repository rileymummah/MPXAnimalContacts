#' 3. Aggregate species into groups, matching 1982 groups
#'
#'    The 2007 survey included separate questions for 28 species of animal, including 12 species of non-human primates and 7 species of rodents.  To allow robust comparison with the coarser classifications used in the 1982 responses, we aggregated the species into four broad groups: antelope, rodent, NHP, and pig.  Because squirrels are the suspected reservoir of MPXV in the DRC, we conducted a separate analysis that focused on reported contacts with squirrels in particular.
#'
#'    Contact rates were summed for all species within a group.  Because 12 species of NHP were included in the 2007 questionnaire, whereas most responses to the 1982 survey simply said ‘singe’ or ‘XXX’, this led to inflation of the 2007 values.  There is not sufficient information in the two surveys to reconcile this difference, so we did not include NHP in our final analyses.
#' @param mpx1b MPX 2007 survey dataset
#' @param animal.group.names Animal groups to match the 1980 MPX survey
#' @param max.contacts.by.species The data frame output from contact.type.max
#' @return A data frame with the contact values summed across species group



species.groups.sum <- function(mpx1b, animal.group.names, max.contacts.by.species) {

  total.contacts.by.group = data.frame('NetID' = mpx1b$NetID, 'FormID' = mpx1b$FormID)

  for (gg in 1:length(animal.group.names)){
    cur.name = animal.group.names[gg]

    # which of the species fall in that group?
    cur.columns = which(animal.groups==cur.name) + 2 # the '+2' is for the first two id columns

    # sum across all these species
    sum.across.species = apply(as.matrix(max.contacts.by.species[, cur.columns]), 1, sum, na.rm=FALSE)

    # for entries with more than 30 contacts, replace with 30
    sum.across.species[sum.across.species>30] = 30

    # insert these new values into the data frame
    total.contacts.by.group = cbind(total.contacts.by.group, sum.across.species)
  }
  names(total.contacts.by.group) = c('NetID', 'FormID', animal.group.names)

  return(total.contacts.by.group)
}
