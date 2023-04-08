#' 1. Convert 2007 survey responses to contact frequencies
#'
#' The 2007 survey had five categorical options for frequency of contacts:
#'
#' | Code   | Frequency       |
#' | ------ | --------------- |
#' | 0      | Never           |
#' | 1      | Every day       |
#' | 2      | 1 time/month    |
#' | 3      | 2-3 times/month |
#' | 4      | >4 times/month  |
#'
#' To convert these into quantitative data that could be compared with data
#' from 1982, we sampled contact frequencies from the following triangular
#' distributions:
#'
#' | Code   | Frequency       | Sampling distribution                     |
#' | ------ | --------------- | ----------------------------------------- |
#' | 0      | Never           | 0                                         |
#' | 1      | Every day       | rtri(1, min = 20, max = 40, mode = 30)    |
#' | 2      | 1 time/month    | rtri(1, min = 0.5, max = 1.5, mode = 1)   |
#' | 3      | 2-3 times/month | rtri(1, min = 1.5, max = 3.5, mode = 2.5) |
#' | 4      | >4 times/month  | rtri(1, min = 4, max = 20, mode = 12)     |
#'
#' @param mpx1b MPX 2007 survey dataset
#' @keywords responses
#' @return A data frame with recoded survey values, which are sampled from triangular distributions
#' @examples
#' sample.contact.freq(mpx1b)


sample.contact.freq <- function(mpx1b) {
  ### relevant columns for replacement
  ## RM changed the range for jamie's data to 178:990 from 176:990
  # replace original numbers with new values

  mpx1bTemp = mpx1b[,c(3:815)] # Selects only contact columns

  ## EVERY INDIVIDUAL IS RESAMPLED
  for (j in (1:ncol(mpx1bTemp))) {
    code1 = which(mpx1bTemp[,j] == 1)
    if (length(code1) != 0){
      mpx1bTemp[code1, j] = EnvStats::rtri(length(code1), min = 20, max = 40, mode = 30)
    }

    code2 = which(mpx1bTemp[,j] == 2)
    if (length(code2) != 0) {
      mpx1bTemp[code2, j] = EnvStats::rtri(length(code2), min = 0.5, max = 1.5, mode = 1)
    }

    code3 = which(mpx1bTemp[,j] == 3)
    if (length(code3) != 0) {
      mpx1bTemp[code3, j] = EnvStats::rtri(length(code3), min = 1.5, max = 3.5, mode = 2.5)
    }

    code4 = which(mpx1bTemp[,j] == 4)
    if (length(code4) != 0) {
      mpx1bTemp[code4, j] = EnvStats::rtri(length(code4), min = 4, max = 20, mode = 12)
    }
  }


  ## EVERY INDIVIDUAL GETS THE SAME CODING
  # mpx1bTemp %>%
  #   mutate(row.names = row_number()) %>%
  #   pivot_longer(cols = 1:813, names_to = 'col.names', values_to = 'freq') %>%
  #   # rowwise() %>%
  #   mutate(freq = case_when(freq == 0 ~ 0,
  #                           freq == 1 ~ rtri(1, min = 20, max = 40, mode = 30),
  #                           freq == 2 ~ rtri(1, min = 0.5, max = 1.5, mode = 1),
  #                           freq == 3 ~ rtri(1, min = 1.5, max = 3.5, mode = 2.5),
  #                           freq == 4 ~ rtri(1, min = 4, max = 20, mode = 12))) %>%
  #   pivot_wider(id_cols = 'row.names', names_from = 'col.names', values_from = 'freq') %>%
  #   select(-row.names) %>%
  #   bind_cols(mpx1b[,1:2], .) -> mpx1b.sampled

  mpx1b.sampled = cbind(mpx1b[,1:2], mpx1bTemp)

  return(mpx1b.sampled)
}

