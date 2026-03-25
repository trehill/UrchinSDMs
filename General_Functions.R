#This R script contains general functions that will be used throughout the process
#of species distribution modeling for determing values in rows and columns of 
#dataframes and wrapping and unwrapping spatial data for passing to paralellized loops

## Function to find the maximum of a column in a dataframe
colMax <- function(data) sapply(data, max, na.rm = TRUE)

## Function to find the minimum of a column in a dataframe
colMin <- function(data) sapply(data, min, na.rm = TRUE)

## Function to find the maximum of a row in a dataframe
rowMax <- function(data) as.data.frame(sapply(as.data.frame(t(data)), max, na.rm = TRUE))

## Function to find the minimum of a row in a dataframe
rowMin <- function(data) as.data.frame(sapply(as.data.frame(t(data)), min, na.rm = TRUE))

## Function to wrap a list of terra objects
Wrapper <- function(Datalist){
  #Figure out the length of the list being passed
  listlen <-length(Datalist)
  #Set up a list to hold this
  Wraplist <- list()
  #Loop through the old list and wrap everything
  for (i in 1:listlen){
    Wraplist[[i]] <- terra::wrap(Datalist[[i]])
  }
  return(Wraplist)
}

## Function to unwrap a list of terra objects
Unwrapper <- function(Datalist){
  #Figure out the length of the list being passed
  listlen <-length(Datalist)
  #Set up a list to hold this
  Wraplist <- list()
  #Loop through the old list and wrap everything
  for (i in 1:listlen){
    Wraplist[[i]] <- terra::unwrap(Datalist[[i]])
  }
  return(Wraplist)
}

## Function for determining the weight of presence and absence points
Kelp_Weights <- function(Data){
  #Figure out how many presence points are in the dataset
  Presnum <- nrow(dplyr::filter(Data, Occurrence == 1))
  #Figure out how many absence points are in the dataset
  Absnum <- nrow(dplyr::filter(Data, Occurrence == 0))
  #Calculate total number of points
  Total <- Presnum + Absnum
  #Determine the weighting presence points should have for presence and absence data to be taken equally in model training
  Presweight <- Absnum/Total
  #Determine the weighting absence points should have for presence and absence data to be taken equally in model training
  Absweight <- Presnum/Total
  #Create a data frame of weights to return for this dataset
  Weights <- data.frame(seq(from = 0, to = 0, length = nrow(Data)))
  colnames(Weights) <- "weight"
  for (i in 1:nrow(Data)){
    if (Data$Occurrence[i] == 1){
      Weights$weight[i] <- Presweight
    }
    if (Data$Occurrence[i] == 0){
      Weights$weight[i] <- Absweight
    }
  }
  return(Weights)
}