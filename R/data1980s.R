#' Analyze 1980 dataset
#'
#' @param i Set to 1 if running function independently. Used for replication of the function in apply statement.
#' @param data1980control Control responses from the 1980 MPX survey
#' @param case Defines how frequencies not included in the top three are analyzed. Default is 'resample', which dictates that non-top 3 frequencies are uniformly sampled from 0 to the #3 frequency. Also takes 'lower', which defines all non-top 3 frequencies as 0.
#' @param squirrel Indicates whether or not to return the raw squirrel frequency data. Default is FALSE.
#' @param antelope Indicates whether or not to return the raw antelope frequency data. Default is FALSE.
#' @param rodents Indicates whether or not to return the raw rodent frequency data. Default is FALSE.
#' @keywords responses
#' @return A list containing the mean and standard deviation values by species group (rodent, NHP, antelope, pig, squirrel) for the 1980s dataset



data1980.calc.contact <- function(i, data1980control, case = 'resample', squirrel = FALSE, antelope = FALSE, rodents = FALSE) {
  ## 2) Three approaches to calculating the mean, sd contact frequency for each animal

  animals.1980s.top.three.control = data.frame(data1980control$Serum.ID, data1980control$Age,
                                               data1980control$Anim1, data1980control$Freq1,
                                               data1980control$Anim2, data1980control$Freq2,
                                               data1980control$Anim3, data1980control$Freq3,
                                               data1980control$Squirrel)
  names(animals.1980s.top.three.control) = c('id', 'age', 'A1', 'F1',
                                             'A2', 'F2',  'A3', 'F3',
                                             'Squirrel')

  # try stacking data from each of the tree top animal contacts and seeing frequencies
  rodent.freqs = c()
  rodent.top3 = c()
  nhp.freqs = c()
  antelope.freqs = c()
  pig.freqs = c()
  squirrel.freqs = c()
  squirrel.top3 = c()

  if (case == 'lower') {
    ###  - Lower bound for non-listed animals - assign contact frequency 0 to any animal not listed in top three

    for (ii in 1:length(animals.1980s.top.three.control[,1])){
      #  check whether any of the top three are rodents
      if (animals.1980s.top.three.control$A1[ii]=='R' |
          animals.1980s.top.three.control$A2[ii]=='R' |
          animals.1980s.top.three.control$A3[ii]=='R') {
        animal.col = which(animals.1980s.top.three.control[ii,] == 'R')
        rodent.freqs = c(rodent.freqs, animals.1980s.top.three.control[ii,(animal.col+1)])
        rodent.top3 = c(rodent.top3, 1)
        # Add line just to calculate freq of squirrels
        if (!is.na(animals.1980s.top.three.control$Squirrel[ii])) {
          squirrel.freqs = c(squirrel.freqs, animals.1980s.top.three.control[ii,(animal.col+1)])
          squirrel.top3 = c(squirrel.top3, 1)
        } else {
          squirrel.freqs = c(squirrel.freqs, 0)
          squirrel.top3 = c(squirrel.top3, 0)
        }
      } else { # if it isn't listed in the top three, set frequency to zero
        rodent.freqs = c(rodent.freqs, 0)
        rodent.top3 = c(rodent.top3, 0)
      }

      #  check whether any of the top three are NHP
      if (animals.1980s.top.three.control$A1[ii]=='S' |
          animals.1980s.top.three.control$A2[ii]=='S' |
          animals.1980s.top.three.control$A3[ii]=='S' |
          animals.1980s.top.three.control$A1[ii]=='M' |
          animals.1980s.top.three.control$A2[ii]=='M' |
          animals.1980s.top.three.control$A3[ii]=='M') {
        animal.col = which(animals.1980s.top.three.control[ii,] == 'S')
        if (length(animal.col)==0) {
          animal.col = which(animals.1980s.top.three.control[ii,] == 'M')
        }
        # Add frequency to vector
        nhp.freqs = c(nhp.freqs, animals.1980s.top.three.control[ii,(animal.col+1)])
      } else { # if it isn't listed in the top three, set frequency to zero
        nhp.freqs = c(nhp.freqs, 0)
      }

      #  check whether any of the top three are antelope
      if (animals.1980s.top.three.control$A1[ii]=='A' |
          animals.1980s.top.three.control$A2[ii]=='A' |
          animals.1980s.top.three.control$A3[ii]=='A') {
        animal.col = which(animals.1980s.top.three.control[ii,] == 'A')[1]
        antelope.freqs = c(antelope.freqs, animals.1980s.top.three.control[ii,(animal.col+1)])
      } else { # if it isn't listed in the top three, set frequency to zero
        antelope.freqs = c(antelope.freqs, 0)
      }

      #  check whether any of the top three are pigs
      if (animals.1980s.top.three.control$A1[ii]=='WP' |
          animals.1980s.top.three.control$A2[ii]=='WP' |
          animals.1980s.top.three.control$A3[ii]=='WP') {
        animal.col = which(animals.1980s.top.three.control[ii,] == 'WP')
        pig.freqs = c(pig.freqs, animals.1980s.top.three.control[ii,(animal.col+1)])
      } else { # if it isn't listed in the top three, set frequency to zero
        pig.freqs = c(pig.freqs, 0)
      }
    }
  } else if (case == 'resample') {
    ###  - Upper bound for non-listed animals - assign contact frequency equal to the smallest listed frequency to any animal not listed in top three

    for (ii in 1:length(animals.1980s.top.three.control[,1])){
      #  check whether any of the top three are rodents
      if (animals.1980s.top.three.control$A1[ii]=='R' |
          animals.1980s.top.three.control$A2[ii]=='R' |
          animals.1980s.top.three.control$A3[ii]=='R') {
        animal.col = which(animals.1980s.top.three.control[ii,] == 'R')
        rodent.freqs = c(rodent.freqs, animals.1980s.top.three.control[ii,(animal.col+1)])
        rodent.top3 = c(rodent.top3, 1)
      } else { # if it isn't listed in the top three, use the smallest frequency
        rodent.freqs = c(rodent.freqs, stats::runif(1, 0, animals.1980s.top.three.control[ii,8]))
        rodent.top3 = c(rodent.top3, 0)
      }

      # calculate freq of squirrels
      if (!is.na(animals.1980s.top.three.control$Squirrel[ii])) {
        # if squirrel is listed then it takes the rodent frequency
        animal.col = which(animals.1980s.top.three.control[ii,] == 'R')
        squirrel.freqs = c(squirrel.freqs, animals.1980s.top.three.control[ii,(animal.col+1)])
        squirrel.top3 = c(squirrel.top3, 1)
      } else {
        squirrel.freqs = c(squirrel.freqs, stats::runif(1, 0, animals.1980s.top.three.control[ii,8]))
        squirrel.top3 = c(squirrel.top3, 0)
      }

      #  check whether any of the top three are NHP
      if (animals.1980s.top.three.control$A1[ii]=='S' |
          animals.1980s.top.three.control$A2[ii]=='S' |
          animals.1980s.top.three.control$A3[ii]=='S' |
          animals.1980s.top.three.control$A1[ii]=='M' |
          animals.1980s.top.three.control$A2[ii]=='M' |
          animals.1980s.top.three.control$A3[ii]=='M') {
        animal.col = which(animals.1980s.top.three.control[ii,] == 'S')
        if(length(animal.col)==0){
          animal.col = which(animals.1980s.top.three.control[ii,] == 'M')
        }
        nhp.freqs = c(nhp.freqs, animals.1980s.top.three.control[ii,(animal.col+1)])
      } else { # if it isn't listed in the top three, use the smallest frequency
        nhp.freqs = c(nhp.freqs, stats::runif(1, 0, animals.1980s.top.three.control[ii,8]))
      }

      #  check whether any of the top three are antelope
      if (animals.1980s.top.three.control$A1[ii]=='A' |
          animals.1980s.top.three.control$A2[ii]=='A' |
          animals.1980s.top.three.control$A3[ii]=='A') {
        animal.col = which(animals.1980s.top.three.control[ii,] == 'A')[1]
        antelope.freqs = c(antelope.freqs, animals.1980s.top.three.control[ii,(animal.col+1)])
      } else { # if it isn't listed in the top three, use the smallest frequency
        antelope.freqs = c(antelope.freqs, stats::runif(1, 0, animals.1980s.top.three.control[ii,8]))
      }

      #  check whether any of the top three are pigs
      if (animals.1980s.top.three.control$A1[ii]=='WP' |
          animals.1980s.top.three.control$A2[ii]=='WP' |
          animals.1980s.top.three.control$A3[ii]=='WP') {
        animal.col = which(animals.1980s.top.three.control[ii,] == 'WP')
        pig.freqs = c(pig.freqs, animals.1980s.top.three.control[ii,(animal.col+1)])
      } else { # if it isn't listed in the top three, use the smallest frequency
        pig.freqs = c(pig.freqs, stats::runif(1, 0, animals.1980s.top.three.control[ii,8]))
      }
    }
  } else {print('Incorrect case call.')}

  # Estimates for animals
  mean.vals.1980s = c(mean(as.numeric(rodent.freqs), na.rm=TRUE),
                      mean(as.numeric(nhp.freqs), na.rm=TRUE),
                      mean(as.numeric(antelope.freqs), na.rm=TRUE),
                      mean(as.numeric(pig.freqs), na.rm=TRUE),
                      mean(as.numeric(squirrel.freqs), na.rm=TRUE))

  if (squirrel) {
    return(data.frame(sq.freq = squirrel.freqs,
                      sq.top3 = squirrel.top3,
                      rod.freq = rodent.freqs,
                      rod.top3 = rodent.top3))
  } else if(antelope) {
    return(data.frame(freq = antelope.freqs))
  } else if(rodents) {
    return(data.frame(freq = rodent.freqs))
  } else {
    return(mean.vals.1980s)
  }


  # sd.vals.1980s = c(sd(as.numeric(rodent.freqs), na.rm=TRUE),
  #                   sd(as.numeric(nhp.freqs), na.rm=TRUE),
  #                   sd(as.numeric(antelope.freqs), na.rm=TRUE),
  #                   sd(as.numeric(pig.freqs), na.rm=TRUE),
  #                   sd(as.numeric(squirrel.freqs), na.rm = TRUE))


  # return(list(mean.vals.1980s = mean.vals.1980s,
  #             sd.vals.1980s = sd.vals.1980s))
}

