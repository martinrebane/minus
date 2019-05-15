reorderMinusTech <- function(medsample) {

  # matrix to store monotonous system row order and respective row sums
  rowOrder <- matrix(data=NA,nrow=nrow(medsample), ncol = 2, dimnames = list(NULL, c("rowNum","sum")))
  # matrix to store monotonous system column order
  colOrder = matrix(data=NA,nrow=ncol(medsample), ncol = 2, dimnames = list(NULL, c("colNum","sum")))
  
  # function which replaces values with frequencies
  replaceValWithFreq <- function(col){
    keptRows = col[rowKeepFlag]
    t = as.data.frame(table(keptRows))
    t$Freq[match(col,t[,1])]
  }
  
  medTemp <- medsample
  rowKeepFlag <- rep(TRUE, dim(medTemp)[1])
  
  for (i in 1:dim(medTemp)[1]) {
    # replace all values with frequencies
    withFreq <- apply(medTemp,2,replaceValWithFreq)
    # calculate row sums
    rowSum <- rowSums(withFreq)
    # set already removed rows as max to avoid selecting them again
    rowSum[!rowKeepFlag]<-(max(rowSum)+1)
    # get single minimum row sum index and append it to rowOrder
    minRowIndex = which.min(rowSum)
    # store row order
    rowOrder[i,]<-c(minRowIndex, rowSum[minRowIndex])
    # mark row as removed
    rowKeepFlag[minRowIndex]<-FALSE
    #print(minRowIndex)
  }
  print("Rows ordered")
  # function which replaces values with frequencies
  replaceValWithFreqCol <- function(row){
    keptCols = row[colKeepFlag]
    t = as.data.frame(table(t(keptCols)))
    t$Freq[match(row,t[,1])]
  }
  
  colKeepFlag <- rep(TRUE, dim(medTemp)[2])
  
  for (i in 1:dim(medTemp)[2]) {
    # replace all values with frequencies
    withFreq <- apply(medTemp,1,replaceValWithFreqCol)
    # attach row sums as last column (no need, can keep separate)
    rowSum <- rowSums(withFreq)
    # set already removed rows as max to avoid selecting them again (need for same values)
    rowSum[!colKeepFlag]<-(max(rowSum)+1)
    # get single minimum row sum index and append it to rowOrder
    minRowIndex = which.min(rowSum)
    #print(minRowIndex)
    colOrder[i,]<-c(minRowIndex, rowSum[minRowIndex])
    colKeepFlag[minRowIndex]<-FALSE
  }
  print("Cols ordered")
  # reorder data by given order
  res<-medsample[rowOrder[,1],colOrder[,1]]
}

