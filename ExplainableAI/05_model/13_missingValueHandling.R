#' @author Felix Kleine Bösing
#' @description handles missing values by replacing with mean and sd
#' @param data
#' @return Nothing. writes files as fst
replaceMissingValuesWithMean <- function(data) {
  return(data)
}

#' @author Marc Schmieder
#' @description handles missing values by replacing mice
#' @param data
#' @return Nothing. writes files as fst
replaceMissingValuesMice = function(data){
  return(data)
  
}


#' @author Marc Schmieder
#' @description imputes missing values of selected external data columns by PLZ hierarchie
#' @param inputDT
#' @return inputDT imputed
imputeExternalData = function(inputDT, columns){
  data = inputDT
                       

  # adding help columns:PLZ with 1-3 digits less
  data[, ':='(PLZ4D = floor(PLZ/10),
               PLZ3D = floor(PLZ/100),
               PLZ2D = floor(PLZ/1000),
              PLZ1D = floor(PLZ/10000))]
  
  # do the imputation for every column seperately   
  for(imputeColumn in columns){
    imputeClass = class(data[, get(imputeColumn)])
    imputeFunction = ifelse(imputeClass %in% c("factor", "character") , function(x) Mode(x),
                            function(x) mean(x, na.rm = T))
    # compute vector of all PLZ where an imputation is needed 
    anyNAPLZ = data[, .(anyNA = any(is.na(get(imputeColumn)))), by = PLZ][anyNA == T & !is.na(PLZ), ]$PLZ
    print(paste0("imputation of column ", imputeColumn, " in progress. Number of NAs before Imputation = ",
                 sum(is.na(data[, get(imputeColumn)]))))
   
    # do imputation for every PLZ
    for(tmpPLZ in anyNAPLZ){
      firstTry = data[PLZ4D == floor(tmpPLZ/10),]
      secondTry = data[PLZ3D == floor(tmpPLZ/100),]
      thirdTry = data[PLZ2D == floor(tmpPLZ/1000),]
      fourthTry = data[PLZ1D == floor(tmpPLZ/10000),]
      
      replacementValue = ifelse(!all(is.na(firstTry[, get(imputeColumn)])), 
                                imputeFunction(firstTry[, get(imputeColumn)]),
                                ifelse(!all(is.na(secondTry[, get(imputeColumn)])), 
                                       imputeFunction(secondTry[, get(imputeColumn)]),
                                       ifelse(!all(is.na(thirdTry[, get(imputeColumn)])), 
                                              imputeFunction(thirdTry[, get(imputeColumn)]),
                                              ifelse(!all(is.na(fourthTry[, get(imputeColumn)])), 
                                                     imputeFunction(fourthTry[, get(imputeColumn)]),
                                              NA))))
      data[PLZ == tmpPLZ, c(imputeColumn) := list(replacementValue)]
      
    }
    print(paste0("imputation of column ", imputeColumn, " is done. Number of NAs after Imputation = ",
                 sum(is.na(data[, get(imputeColumn)]))))
  }
  # delete help columns
  data[, ':='(PLZ4D = NULL,
              PLZ3D = NULL,
              PLZ2D = NULL,
              PLZ1D = NULL)]
  return(data)
}

#' @author Marc Schmieder
#' @description imputes missing values of selected external data columns by PLZ hierarchie
#' @param inputDT
#' @return inputDT imputed
imputeInternalData = function(inputDT, columns, ratio = 0.05){
  data = inputDT
  
  for(imputeColumn in columns){
    
    imputeClass = class(data[, get(imputeColumn)])
    sumNA = sum(is.na(data[, get(imputeColumn)]))
    # skip this column of there are no missing Values
    if(sumNA == 0) next
    sumNARatio = sumNA/nrow(data)
    print(paste0("class of " , imputeColumn, " is " ,imputeClass, ". Ratio of NA is ", sumNARatio))
    
    if(imputeClass == "factor"){
      replacementValue = Mode(data[, get(imputeColumn)])
    }
    if(imputeClass == "character"){
      replacementValue = Mode(data[, get(imputeColumn)])
    }
    if(imputeClass == "integer"){
      replacementValue = median(data[, get(imputeColumn)], na.rm = T)
    }
    if(imputeClass == "numeric"){
      replacementValue = mean(data[, get(imputeColumn)], na.rm = T)
    }
    
    # when below 5 percent missings, impute, else throw warning message
    if(sumNARatio < ratio){
      inputDT[is.na(get(imputeColumn)), c(imputeColumn) := list(replacementValue)]
      print(paste0(imputeColumn,  " imputed "))
    } 
    
    if(sumNARatio >= ratio){
      inputDT[, c(imputeColumn) := NULL]
      print(paste0("too many NAs, " , imputeColumn,  " is deleted "))
    } 
    else{print(paste0(imputeColumn,  " not imputed, Ratio of missing Values is too high"))}
    
  }
  return(data)
}



Mode = function(x){
  x = x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
