#This function takes a spatial raster, and a spatial data frame of in situ points
#Then it will fill any NA value in the SpDF with the first-discovered non-NA values from r
lengthNONA=function(x){return(length(x[!is.na(x)]))}
pullindex=function(ind,values){return(values[ind])}
ExpandingExtract_Flex=function(Data,SurveyPts,Dists=c(0,500,1000,2000,4000,8000),Data_Col=NULL,REPORT=F){
  require(raster)
  require(spatial)
  require(sp)
  require(sf)
  
    # works with these ver
  # spatial_7.3-13  
  # raster_3.4-5 
  # sf_0.9-8       
  # sp_1.4-4  
  
  OutDF=data.frame(values=rep(NA,nrow(SurveyPts)),Dist=rep(NA,nrow(SurveyPts)),N=rep(NA,nrow(SurveyPts)))
  nDists=length(Dists)
  cnt=1
  NAi=which(is.na(OutDF$values))
  if(class(Data)=="raster"){
    NAsLeft=length(NAi)>0
    while(cnt<=nDists&NAsLeft){
      NAi=which(is.na(OutDF$values))
      pull=raster::extract(x=Data,y=SurveyPts[NAi,],
                           buffer=Dists[cnt],
                           small=TRUE,
                           na.rm=TRUE)
      Nper=unlist(lapply(pull,lengthNONA))
      OutDF$values[NAi]=unlist(lapply(pull,mean,na.rm=TRUE))
      OutDF$Dist[NAi]=Dists[cnt]
      OutDF$N[NAi]=Nper
      NAi=which(is.na(OutDF$values))
      NAsLeft=length(NAi)>0
      cnt=cnt+1
    }
  }else if(class(Data)=="SpatialPointsDataFrame"|class(Data)=="SpatialPoints"){
    #Prep the loop
    if(is.null(Data_Col)){
      print(paste0("You did not specific the parameter 'Data_Col' to direct which column in 'Data' you want to extract. Assuming you mean to extract the first numeric column."))
      SDD=sapply(Data@data, class)
      i_col=min(which(SDD=="numeric"),na.rm=T)
      Data_Col=names(Data@data)[i_col]
      print(paste0("AKA the ",i_col,"-th column, ",Data_Col))
      print("If you'd prefer otherwise, do it again with Data_Col properly specified.")
    }
    #Convert SurveyPoints in to Spatial Polygons (with buffer)
    SurveyPts.sf=st_as_sf(SurveyPts)
    Data.sf=st_as_sf(Data)
    #convert m to DD
    Dists_DD=Dists*1/111111
    print(paste0("*** Warning!!! If you _really_ care about fine-scale differences, be careful here. This extraction code is using the 1 Decimal Degree ~= 111.111 km generic assumption. There is potential for error here, but in our latitudes and across short distances, errors are small. ***"))
    NAsLeft=length(NAi)>0
    #Turn off warning just for the loop
    defaultW <- getOption("warn")
    options(warn = -1)
    while(cnt<=nDists&NAsLeft){
      NAi=which(is.na(OutDF$values))
      #Set Buffer Distance Around Survey Points
      if(Dists_DD[cnt]==0){Dists_DD[cnt]=.01*1/111111}#effective zero is 1 cm
      SurveyPts.Buffer=suppressMessages(st_buffer(x=SurveyPts.sf,dist=Dists_DD[cnt]))
      pull_i=suppressMessages(st_intersects(SurveyPts.Buffer[NAi,],Data.sf))
      valvec=Data@data%>% pull(Data_Col)
      pull=lapply(pull_i,pullindex,valvec)
      Nper=unlist(lapply(pull,lengthNONA))
      OutDF$values[NAi]=unlist(lapply(pull,mean,na.rm=TRUE))
      OutDF$Dist[NAi]=Dists[cnt]
      OutDF$N[NAi]=Nper
      NAi=which(is.na(OutDF$values))
      NAsLeft=length(NAi)>0
      cnt=cnt+1
      if(REPORT){print(paste("Extracted to ",Dists[cnt],"meters.",length(NAi),"unmatched points remain."))}
    }
    options(warn = defaultW)
  }else{stop("'Data' is neither RASTER or SPDF")}
  
  return(OutDF)
}

