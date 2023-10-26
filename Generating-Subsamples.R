# Generate balanced subset of stimuli

# Load packages
library(tidyverse)

# Load summary data
DAO <- read_csv("https://raw.githubusercontent.com/alluttrell/DAO/main/Norm-Summaries.csv")

# Function to balance on a DV
balanced_sample <- function(data,
                            var = "att.M", #variable to balance on
                            total = 100, #total number of desired objects
                            sample.prop = c(1,2,3,3,2,1), #i.e., 1n objects with attitudes between 1-2; 2n objects with attitudes between 2-3, etc.
                            minmax = c(1,7) #range of var on which to sample
                            )
  {
  # Generate the number of stimuli to sample in each bin
  sample.ns <- round(total*(sample.prop/sum(sample.prop)),0) #target number of stimuli per bin
  sample.ns.final <- sample.ns #here we begin a process that ensures the total number of stimuli is reached
  random.bin <- sample(x = 1:length(sample.ns), size = 1) #randomly generate one of the bins to modify if needed
  sample.ns.final[random.bin] <- sample.ns[random.bin] + (total - sum(sample.ns)) #add or subtract to/from a random bin to reach the total number of stimuil

  # Translate bins established in sample.ns.final to individual ranges 
  # by establishing cut points along a continuum from min to max
  cutoffs <- seq(from = minmax[1], to = minmax[2], 
                 length.out = length(sample.ns.final)+1)
  
  # Sample from the DAO based on normative ratings in desired proportion
  out.list <- list() #set up empty list into which results will be saved
  errors <- list() #set up empty list into which errors may be saved
  for(i in 1:length(sample.prop)){
    min <- cutoffs[i] #establish minimum value for this bin
    max <- cutoffs[i+1] #establish maximum value for this bin
    range.text <- paste0(var, " from ", round(min, 2), " to ", round(max, 2))
    tmp.df <- tibble(obj = data$obj,
                     var = pull(data[var])) %>%  #create temporary df isolating desired variable
      filter(var > min & var <= max) #zero in on objects that are within range
    if(nrow(tmp.df) >= sample.ns.final[i]) {
      errors[[i]] <- NA
      out.list[[i]] <- tmp.df %>% sample_n(sample.ns.final[i]) %>% #randomly sample the appropriate number of objects
        mutate(level = i, range = range.text) %>% #create indices for this loop
        select(level, range, obj)
    } else {
      errors[[i]] <- paste0(var, " between ", min, " - ", max, " (total possible = ", nrow(tmp.df), "; total requested = ", sample.ns.final[i],")")
      out.list[[i]] <- tmp.df %>% #get all objects meeting criteria
        mutate(balanced.on = var,
               level = i, 
               range = range.text) %>% #create indices for this loop
        select(balanced.on, level, range, obj)
    }
  }
  
  # Combine all output into one data frame
  stim.balanced <- bind_rows(out.list)
  
  if(all(is.na(errors))==FALSE) {
    print(paste0("For the following ranges, the desired number of stimuli is greater than the total number of objects meeting the criteria. Output will include ALL stimuli meeting that condition."))
    print(unlist(errors)[!is.na(unlist(errors))])
  }

  return(stim.balanced)
}
