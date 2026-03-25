#This R script contains all the functions needed to set up CV-folds by spatial blocking,
#adjust folds by presence point redistribution, and visually evaluate spatial folds


## Function to re-balance CV folds so that some of the presence points are randomly redistributed across all the folds
CV_redistribute <- function(i, k, cutoff){
  #Loop through each fold and randomly select 10% of the presence values in each fold to be redistributed systematically between the other folds 
  #set seed
  set.seed(342121)
  foldlist <- c(seq(1:1:k),seq(1:1:k),seq(1:1:k), seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k))
  for (j in 1:k){
    presnum <- nrow(dplyr::filter(i, fold == j & Occurrence == 1))
    if (presnum > cutoff){
      rnum <- as.numeric(round(0.6*presnum))
      count <- 0
      for (u in 1:nrow(i)){
        if (count >= rnum){
          break
        }
        if (i$Occurrence[u] == 1){
          if (i$fold[u] == j){
            count = count + 1
            i$fold[u] <- foldlist[j+count]
          }
        }
      }
    }
  }
  return(i)
}




###
CV_redistribute_new <- function(i, k, cutoff){
  # Loop through each fold and randomly select 10% of the presence values in each fold to be redistributed systematically between the other folds 
  # set seed
  set.seed(342121)
  foldlist <- rep(1:k, length.out = nrow(i))  # Create a repeating fold list with length equal to number of rows
  
  for (j in 1:k){
    # Count the number of presence values (Occurrence == 1) in the fold
    presnum <- nrow(dplyr::filter(i, fold == j & Occurrence == 1))
    
    if (presnum > cutoff){
      rnum <- as.numeric(round(0.6 * presnum))  # 60% of the presences to redistribute
      count <- 0
      
      for (u in 1:nrow(i)){
        if (count >= rnum){
          break
        }
        
        # Ensure i$fold[u] is not NA before checking
        if (i$Occurrence[u] == 1 && i$fold[u] == j){
          count = count + 1
          
          # Assign the fold from foldlist using a valid index
          new_fold_index <- (j + count - 1) %% k + 1  # Circular indexing within the bounds of k
          i$fold[u] <- foldlist[new_fold_index]
        }
      }
      
      # Check for NA values in the fold column after redistribution
      if (any(is.na(i$fold))){
        warning("There are NA values in the 'fold' column after redistribution!")
        print(i[is.na(i$fold), ])  # Print the rows where fold is NA for debugging
      }
    }
  }
  
  return(i)
}


###
## Function to re-balance CV folds so that some of the presence points are randomly redistributed across all the folds
CV_redistribute_old <- function(Kelp_Data, PA_num, k, cutoff){
  #Make a list of k and cutoff PA_num times
  klist <- list()
  cutlist <- list()
  for (i in 1:PA_num){
    klist[[i]] <- k+1
    cutlist[[i]] <- cutoff
  }
  #For each PA dataset loop through each fold and randomly select 10% of the presence values in each fold to be redistributed systematically between the other folds 
  New_Kelp_Data <- foreach(i = Kelp_Data, k = klist, cutoff = cutlist, .packages = c("tidyverse")) %dopar% {
    #set seed
    set.seed(342121)
    foldlist <- c(seq(1:1:k),seq(1:1:k),seq(1:1:k), seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k),seq(1:1:k))
    for (j in 1:k){
      presnum <- nrow(dplyr::filter(i, fold == j & Occurrence == 1))
      if (presnum > cutoff){
        rnum <- as.numeric(round(0.6*presnum))
        count <- 0
        for (u in 1:nrow(i)){
          if (count >= rnum){
            break
          }
          if (i$Occurrence[u] == 1){
            if (i$fold[u] == j){
              count = count + 1
              i$fold[u] <- foldlist[j+count]
            }
          }
        }
      }
    }
    return(i)
  }
  return(New_Kelp_Data)
}

###
CV_redistribute_ordinal <- function(input_dataset, number_folds, 
                                    cutoff_none, cutoff_few, cutoff_many, cutoff_abundant) {
  # Make a copy to work on
  df <- input_dataset
  
  # Define the modal categories and cutoffs
  modal_levels <- c("None", "Few", "Many", "Abundant")
  cutoffs <- c(cutoff_none, cutoff_few, cutoff_many, cutoff_abundant)
  names(cutoffs) <- modal_levels
  
  # Ensure 'density_modal' is a factor with correct levels
  df$density_modal <- factor(df$density_modal, levels = modal_levels)
  
  for (level in modal_levels) {
    level_cutoff <- cutoffs[level]
    
    # Count the number of samples per fold for this density level
    fold_counts <- table(df$fold[df$density_modal == level])
    
    # Find folds below and above the cutoff
    underrepresented_folds <- as.integer(names(fold_counts[fold_counts < level_cutoff]))
    overrepresented_folds <- as.integer(names(fold_counts[fold_counts > level_cutoff]))
    
    for (fold_under in underrepresented_folds) {
      deficit <- level_cutoff - fold_counts[as.character(fold_under)]
      
      for (fold_over in overrepresented_folds) {
        available <- fold_counts[as.character(fold_over)] - level_cutoff
        if (available <= 0) next
        
        transfer_n <- min(deficit, available)
        
        # Select rows to move
        rows_to_move <- which(df$fold == fold_over & df$density_modal == level)
        selected_rows <- sample(rows_to_move, transfer_n)
        
        # Reassign folds
        df$fold[selected_rows] <- fold_under
        
        # Update counts and deficit
        fold_counts[as.character(fold_under)] <- fold_counts[as.character(fold_under)] + transfer_n
        fold_counts[as.character(fold_over)] <- fold_counts[as.character(fold_over)] - transfer_n
        deficit <- deficit - transfer_n
        
        if (deficit <= 0) break
      }
    }
  }
  
  return(df)
}


##
## Function to re-balance CV folds so that some of the presence points are randomly redistributed across all the folds
CV_redistribute_continuous <- function(input_dataset, number_folds,
                                       cutoff_none, cutoff_few, cutoff_many, cutoff_abundant) {
  df <- input_dataset
  
  # Categorize 'mean' temporarily
  df$temp_density <- cut(df$mean,
                         breaks = c(-Inf, 0, 10, 20, Inf),
                         labels = c("None", "Few", "Many", "Abundant"),
                         right = TRUE)
  
  # Set up modal levels and corresponding cutoffs
  modal_levels <- c("None", "Few", "Many", "Abundant")
  cutoffs <- c(cutoff_none, cutoff_few, cutoff_many, cutoff_abundant)
  names(cutoffs) <- modal_levels
  
  for (level in modal_levels) {
    level_cutoff <- cutoffs[level]
    
    # Ensure all folds (1 to number_folds) are included
    fold_counts <- table(factor(df$fold[df$temp_density == level], levels = 1:number_folds))
    
    underrepresented_folds <- as.integer(names(fold_counts[fold_counts < level_cutoff]))
    overrepresented_folds <- as.integer(names(fold_counts[fold_counts > level_cutoff]))
    
    for (fold_under in underrepresented_folds) {
      deficit <- level_cutoff - fold_counts[as.character(fold_under)]
      
      for (fold_over in overrepresented_folds) {
        available <- fold_counts[as.character(fold_over)] - level_cutoff
        if (available <= 0 || deficit <= 0) next
        
        rows_to_move <- which(df$fold == fold_over & df$temp_density == level)
        
        if (length(rows_to_move) == 0) next
        
        transfer_n <- min(deficit, available, length(rows_to_move))
        selected_rows <- sample(rows_to_move, transfer_n)
        
        # Reassign fold
        df$fold[selected_rows] <- fold_under
        
        # Update counts
        fold_counts[as.character(fold_under)] <- fold_counts[as.character(fold_under)] + transfer_n
        fold_counts[as.character(fold_over)] <- fold_counts[as.character(fold_over)] - transfer_n
        deficit <- deficit - transfer_n
        
        if (deficit <= 0) break
      }
    }
  }
  
  df$temp_density <- NULL
  return(df)
}


## Function to re-balance CV folds so that some of the presence points are randomly redistributed across all the folds
  #i is training dataset w. folds
  #k is number of folds 
  #cutoff is?
CV_redistribute_presences <- function(Kelp_Data, PA_num, k, cutoff){
  
  #Set seed once
  set.seed(342121)
  
  foldlist <- rep(1:k, times = 40)
  
  for (j in 1:k){
    presnum <- nrow(dplyr::filter(Kelp_Data, fold == j & mean > 0))
    if (presnum > cutoff){
      rnum <- as.numeric(round(0.6 * presnum))
      count <- 0
      for (u in 1:nrow(Kelp_Data)){
        if (count >= rnum){
          break
        }
        if (Kelp_Data$mean[u] > 0 && Kelp_Data$fold[u] == j){
          count = count + 1
          Kelp_Data$fold[u] <- foldlist[j + count]
        }
      }
    }
  }
  
  return(Kelp_Data)
}


CV_redistribute_absences <- function(Kelp_Data, PA_num, k, cutoff){
  
  #Set seed once
  set.seed(342121)
  
  foldlist <- rep(1:k, times = 40)
  
  for (j in 1:k){
    presnum <- nrow(dplyr::filter(Kelp_Data, fold == j & mean == 0))
    if (presnum > cutoff){
      rnum <- as.numeric(round(0.6 * presnum))
      count <- 0
      for (u in 1:nrow(Kelp_Data)){
        if (count >= rnum){
          break
        }
        if (Kelp_Data$mean[u] == 0 && Kelp_Data$fold[u] == j){
          count = count + 1
          Kelp_Data$fold[u] <- foldlist[j + count]
        }
      }
    }
  }
  
  return(Kelp_Data)
}
