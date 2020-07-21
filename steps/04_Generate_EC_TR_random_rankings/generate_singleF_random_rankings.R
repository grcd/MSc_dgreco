
#  wrapper function

generate_singleF_random_rankings <- function(type, perms, R2_f, G, id_to_index='', 
                                             outputF='') {
  if (type=='EC') {
    source("steps/04_Generate_EC_TR_random_rankings/generate_singleF_EC_random_rankings.R")
    return(generate_singleF_EC_random_rankings(perms, R2_f, G, id_to_index, outputF))

  } else if (type == 'TR') {
    source("steps/04_Generate_EC_TR_random_rankings/generate_singleF_TR_random_rankings.R")
    return(generate_singleF_TR_random_rankings(perms, R2_f, G, id_to_index, outputF))
    
  } else {
    stop(sprintf("Unrecognized type: %s", type))
    # return(NULL)
  }
}
