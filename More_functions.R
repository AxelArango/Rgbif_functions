gbifdata_more<-function(sp.list,limit){
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
  nx<-c("institutionCode", "catalogNumber", "collectionCode", "country", "stateProvince", "year", "month", "decimalLatitude", "decimalLongitude", "basisOfRecord", "species", "family", "genus", "coordinateUncertaintyInMeters")
  output<-data.frame()
  for(i in 1:splist.n){
    print(paste(i,' of ',splist.n, ' (',splist.a[i],')',sep=''))
    vec<-occ_search(scientificName =splist.a[i],hasCoordinate=T,limit=limit)
    t2s<-data.frame(vec$data)
    t3s<-data.frame(t2s$institutionCode,t2s$catalogNumber,t2s$collectionCode,t2s$country,t2s$stateProvince,t2s$year,t2s$month,t2s$decimalLatitude,t2s$decimalLongitude,t2s$basisOfRecord,t2s$species,t2s$family,t2s$genus,t2s$coordinateUncertaintyInMeters)
    t3s<-data.frame(t3s)
    names(t3s)<-nx
    output<-rbind(output,t3s)
  }
  uniquepresences<-unique(output)
}






gbifdata_mel<-function(sp.list,limit,username){
  require(rgbif)
  require(elevatr)
  if (class(sp.list)=='data.frame'){
    datab<-sp.list
    if(is.null(datab$scientific_name)==F){
      datab<-datab$scientific_name}
    else if (is.null(datab$sp)==F){
      datab<-datab$sp
    }}else { datab<-as.character(sp.list)}
  splist.a<-as.vector(datab)
  splist.n<-length(splist.a)
  nx<-c("institutionCode", "catalogNumber", "collectionCode", "country", "stateProvince", "year", "month", "decimalLatitude", "decimalLongitude", "basisOfRecord", "species", "family", "genus", "coordinateUncertaintyInMeters")
  output<-data.frame()
  for(i in 1:splist.n){
    print(paste(i,' of ',splist.n, ' (',splist.a[i],')',sep=''))
    vec<-occ_search(scientificName =splist.a[i],hasCoordinate=T,limit=limit)
    t2s<-data.frame(vec$data)
    t3s<-data.frame(t2s$institutionCode,t2s$catalogNumber,t2s$collectionCode,t2s$country,t2s$stateProvince,t2s$year,t2s$month,t2s$decimalLatitude,t2s$decimalLongitude,t2s$basisOfRecord,t2s$species,t2s$family,t2s$genus,t2s$coordinateUncertaintyInMeters)
    t3s<-data.frame(t3s)
    names(t3s)<-nx
    elev<-elevation(latitude = t3s$decimalLatitude,longitude = t3s$decimalLongitude,username = username)
    t3s<-cbind(t3s,elev[3])
    output<-rbind(output,t3s)
  }
  uniquepresences<-unique(output)
 
}
