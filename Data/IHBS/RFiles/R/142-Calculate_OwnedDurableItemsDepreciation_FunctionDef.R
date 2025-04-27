# Inputs:
#   DurableData_ExpDetail: Detailed expenditure data for durable items
#   DurableItems_OwningDetail: Ownership data for durable items
#   by: Grouping variables (default: "Item")
#   Decile: Optional income decile data
#   DurableItems: Metadata containing depreciation rates

Calculate_OwnedDurableItemsDepreciation <- function(DurableData_ExpDetail,
                                                    DurableItems_OwningDetail,
                                                    by="Item",
                                                    Decile=NULL,
                                                    DurableItems=NA){
  
  Ownsm <- melt(data = DurableItems_OwningDetail,id.vars = "HHID",
                measure.vars = names(DurableItems_OwningDetail)[-1],
                variable.name = "Item",value.name = "Owns")
  Ownsm <- Ownsm[Owns==1]  
  if(is.null(Decile)){
    by = setdiff(by,"Decile")
    DurableDepr <- data.table(Item=DurableItems$Item)
  }else{
    # Merge decile information with expenditure and ownership data
    DurableData_ExpDetail <- merge(DurableData_ExpDetail,Decile,by="HHID")
    Ownsm <- merge(Ownsm,Decile,by="HHID")
    
    DurableDepr <- data.table(expand.grid(Item=DurableItems$Item,Decile=factor(1:10)))
  }
  # if(is.na(DurableItems)){
  #   library(readxl)
  #   DurableItems <- data.table(read_excel(Settings$MetaDataFilePath,
  #                                         sheet=Settings$MDS_DurableItemsDepr))
  # }
  
  # Calculate average values by grouping variables
  DurableDepr <- merge(DurableDepr,DurableValues,by=by,all.x = TRUE)
  DurableDepr <- merge(DurableDepr,DurableItems,by="Item")

  # Polynomial regression function for decile-based value estimation
  f <- function(X){
    v <- X$Value
    d <- as.integer(as.character(X$Decile))
    d2 <- d^2
    d3 <- d^3
    mdl <- lm(v~d+d2+d3)
    vp <- predict(mdl,newdata = data.frame(d=d,d2=d2,d3=d3))
    return(vp)
  }
  # Calculate depreciation values
  if("Decile" %in% by){
    
    DurableDepr <- DurableDepr[order(Item,Decile)]
    DurableDepr[,estVal:=f(.SD),by=Item]
    
    for(i in unique(DurableDepr$Item))
      for(d in 9:1){
        ev <- DurableDepr[Item==i & as.integer(Decile)==d]$estVal
        evnext <- DurableDepr[Item==i & as.integer(Decile)==d+1]$estVal
        if(ev>evnext)
          DurableDepr[Item==i & as.integer(Decile)==d,estVal:=evnext]
      }
    DurableDepr[,DepreciationValue:=estVal*Depri/100]
  }else{
    DurableDepr[,DepreciationValue:=Value*Depri/100]
  }
  # Merge with ownership data and sum by household
  D <- merge(Ownsm,DurableDepr,by=by)
  
  OwnedDurableItemsDepreciation <- D[,.(OwnedDurableItemsDepreciation=sum(.SD$DepreciationValue)),by=HHID]
  
  return(OwnedDurableItemsDepreciation)
}
