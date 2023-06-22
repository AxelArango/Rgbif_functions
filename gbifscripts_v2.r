gbifdata<-function(sp.list,limit){
  require(rgbif)
  if (class(sp.list)=='data.frame'){
    datab<-sp.list
    if(is.null(datab$scientific_name)==F){
      datab<-datab$scientific_name}
    else if (is.null(datab$sp)==F){
      datab<-datab$sp
    }}else { datab<-as.character(sp.list)}
  splist.a<-as.vector(datab)
  splist.n<-length(splist.a)
  output<-list()
  for(i in 1:splist.n){
    print(paste(i,' of ',splist.n, ' (',splist.a[i],')',sep=''))
    vec<-occ_search(scientificName =splist.a[i],hasCoordinate=T,limit=limit)
    t2s<-data.frame(cbind(vec$data$species,vec$data$decimalLongitude,vec$data$decimalLatitude))
    if(ncol(t2s)!=3){print(paste(splist.a[i],' is null',sep = ''))} else{output[[i]]<-t2s}
  }
  df<-do.call('rbind',output)
  coordsdf<-as.matrix(df[,c(2,3)])
  coordsdfLong<-as.numeric(coordsdf[,1])
  coordsdfLat<-as.numeric(coordsdf[,2])
  coords.a<-cbind(coordsdfLong,coordsdfLat)
  presence<-data.frame(df,coords.a)
  names(presence)<-c('sp','longitude','latitude')
  match.a<-which(is.na(match(splist.a,presence$sp)))
  nulls<-splist.a[match.a]
  print(nulls)
  uniquepresences<-unique(presence[,1:3])
  
}

#################
gbifnull<-function(sp.list){
  require(rgbif)
  if (class(sp.list)=='data.frame'){
    datab<-sp.list
    if(is.null(datab$scientific_name)==F){
    datab<-datab$scientific_name}
    else if (is.null(datab$sp)==F){
      datab<-datab$sp
    }}else { datab<-as.character(sp.list)}
  splist.a<-as.vector(datab)
  splist.n<-length(splist.a)
  output<-list()
  for(i in 1:splist.n){
    print(paste(i,' of ',splist.n, ' (',splist.a[i],')',sep=''))
    vec<-name_backbone(splist.a[i])
    t2s<-data.frame(vec$canonicalName)
    if(ncol(t2s)!=1){print(paste(splist.a[i],' is null',sep = ''))} else{output[[i]]<-t2s}
  }
  df<-do.call('rbind',output)
  names.a<-data.frame(df)
  names(names.a)<-c('sp')
  match.a<-which(is.na(match(splist.a,names.a$sp)))
  nulls<-splist.a[match.a]
  nulls.b<-as.data.frame(nulls)
  if(nrow(nulls.b)==0){print('0 Nulls')} else{nulls.b}
}

######
gbifgeometry<-function(area.a,limit){
    require(rgbif)
    require(wicket)
    require(rgdal)
    require(rgeos)
    require(raster)
    if(class(area.a)!='SpatialPolygonsDataFrame'){print('Utiliza un poligono');stop} else{
        areawkt<-sp_convert(area.a)}
    bbx<-gbif_wkt2bbox(wkt = areawkt)
    clen<-occ_data(geometry=bbx,limit=limit,hasCoordinate = T)
    df<-data.frame(clen$data$species,clen$data$decimalLongitude,clen$data$decimalLatitude)
    names(df)<-c('sp','longitude','latitude')
    coordsdf<-as.matrix(df[,c(2,3)])
    coordsdfLong<-as.numeric(coordsdf[,1])
    coordsdfLat<-as.numeric(coordsdf[,2])
    coordsdf.longlat<-cbind(coordsdfLong,coordsdfLat)
    names(coordsdf.longlat)<-c('longitude','latitude')
    spdf<-SpatialPointsDataFrame(coords = coordsdf.longlat,data = data.frame(df$sp,coordsdf.longlat),proj4string =crs(area.a))
    int<-raster::intersect(spdf,area.a)
    dataint<-int@data
    dataint<-unique(dataint[,1:3)
}
#####
gbifsparea<-function(sp.list,area.a,limit){
    require(rgbif)
    require(wicket)
    require(rgdal)
    require(rgeos)
    require(raster)
    if (class(sp.list)=='data.frame'){
        datab<-sp.list
        if(is.null(datab$scientific_name)==F){
            datab<-datab$scientific_name}
        else if (is.null(datab$sp)==F){
            datab<-datab$sp
        }}else { datab<-as.character(sp.list)}
    splist.a<-as.vector(datab)
    splist.n<-length(splist.a)
    output<-list()
    if(class(area.a)!='SpatialPolygonsDataFrame'){print('Utiliza un poligono');stop} else{
        areawkt<-sp_convert(area.a)}
    bbx<-gbif_wkt2bbox(wkt = areawkt)
    for(i in 1:splist.n){
        print(paste(i,' of ',splist.n, ' (',splist.a[i],')',sep=''))
        vec<-occ_search(scientificName =splist.a[i],hasCoordinate=T,limit=limit,geometry=bbx,hasGeospatialIssue = F)
        t2s<-data.frame(cbind(vec$data$species,vec$data$decimalLongitude,vec$data$decimalLatitude))
        if(ncol(t2s)!=3){print(paste(splist.a[i],' is null',sep = ''))} else{output[[i]]<-t2s}
    }
    
    df<-do.call('rbind',output)
    names(df)<-c('sp','longitude','latitude')
    coordsdf<-as.matrix(df[,c(2,3)])
    coordsdfLong<-as.numeric(coordsdf[,1])
    coordsdfLat<-as.numeric(coordsdf[,2])
    coordsdf.longlat<-cbind(coordsdfLong,coordsdfLat)
    names(coordsdf.longlat)<-c('longitude','latitude')
    spdf<-SpatialPointsDataFrame(coords = coordsdf.longlat,data = data.frame(df$sp,coordsdf.longlat),proj4string =crs(area.a))
    int<-raster::intersect(spdf,area.a)
    dataint<-int@data
    dataint<-unique(dataint)
}
####
gbifspatial<-function(sp.list,area.a,limit){
  require(rgbif)
  require(wicket)
  require(rgdal)
  require(rgeos)
  require(raster)
  if (class(sp.list)=='data.frame'){
    datab<-sp.list
    if(is.null(datab$scientific_name)==F){
      datab<-datab$scientific_name}
    else if (is.null(datab$sp)==F){
      datab<-datab$sp
    }}else { datab<-as.character(sp.list)}
  splist.a<-as.vector(datab)
  splist.n<-length(splist.a)
  output<-list()
  if(class(area.a)!='SpatialPolygonsDataFrame'){print('Utiliza un poligono');stop} else{
    areawkt<-sp_convert(area.a)}
  bbx<-gbif_wkt2bbox(wkt = areawkt)
  for(i in 1:splist.n){
    print(paste(i,' of ',splist.n, ' (',splist.a[i],')',sep=''))
    vec<-occ_search(scientificName =splist.a[i],hasCoordinate=T,limit=limit,geometry=bbx,hasGeospatialIssue = F)
    t2s<-data.frame(cbind(vec$data$species,vec$data$decimalLongitude,vec$data$decimalLatitude))
    if(ncol(t2s)!=3){print(paste(splist.a[i],' is null',sep = ''))} else{output[[i]]<-t2s}
  }
  
  df<-do.call('rbind',output)
  names(df)<-c('sp','longitude','latitude')
  coordsdf<-as.matrix(df[,c(2,3)])
  coordsdfLong<-as.numeric(coordsdf[,1])
  coordsdfLat<-as.numeric(coordsdf[,2])
  coordsdf.longlat<-cbind(coordsdfLong,coordsdfLat)
  names(coordsdf.longlat)<-c('longitude','latitude')
  spdf<-SpatialPointsDataFrame(coords = coordsdf.longlat,data = data.frame(df$sp,coordsdf.longlat),proj4string =crs(area.a))
  int<-raster::intersect(spdf,area.a)
}
#######
gbifclade<-function(clado,area.a=NULL,limit){
  require(rgbif)
  require(wicket)
  require(rgdal)
  require(rgeos)
  require(raster)
  if(!is.null(area.a)){
    if(class(area.a)!='SpatialPolygonsDataFrame'){print('Utiliza un poligono');stop} else{
      areawkt<-sp_convert(area.a)}
    bbx<-gbif_wkt2bbox(wkt = areawkt)
    cla<-clado
    tax<-name_backbone(cla)
    TK<-tax$usageKey
    clen<-occ_search(taxonKey=TK,geometry=bbx,limit=limit,hasCoordinate = T)}else{cla<-clado
    tax<-name_backbone(cla)
    TK<-tax$usageKey
    clen<-occ_search(taxonKey=TK,limit=limit,hasCoordinate = T)}
  df<-data.frame(clen$data$species,clen$data$decimalLongitude,clen$data$decimalLatitude)
  names(df)<-c('sp','longitude','latitude')
  coordsdf<-as.matrix(df[,c(2,3)])
  coordsdfLong<-as.numeric(coordsdf[,1])
  coordsdfLat<-as.numeric(coordsdf[,2])
  coordsdf.longlat<-cbind(coordsdfLong,coordsdfLat)
  names(coordsdf.longlat)<-c('longitude','latitude')
  if(is.null(area.a)==T){presence<-data.frame(df$sp,coordsdf.longlat)
  names(presence)<-names(df)
  unique(presence)
  }else{spdf<-SpatialPointsDataFrame(coords = coordsdf.longlat,data = data.frame(df$sp,coordsdf.longlat),proj4string =crs(area.a))
  int<-raster::intersect(spdf,area.a)
  dataint<-int@data
  dataint<-unique(dataint)}
}

