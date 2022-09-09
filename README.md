# Rgbif_functions
This repository contains customized functions for downloading occurrence data from gbif
  - gbifdata: downloads the occurrences of a species list
  - gbifnull: this function serves as a check list for species that might have a different name in gbif in relationship to a species list
  - gbifgeometry: this function gets all the occurrences in a specific area using a .shp
  - gbifsparea: this function gets the ocurrences of species from a species list in a specific area using a .shp
  - gbifspatial: this function gets spatial features from a species list in a specific area using a .shp
  - gbifclade: this function gets ocurrences from a specific clade (i.e genus, family, etc.)


All functions return a data.frame with species names and their coordinates in lon+lat

The more_functions functions include an expandend gbifdata function, that retrieves more fields that gbifdata. the gbif_mel function requires a username obtained from: www.geonames.org and enabling the webservices on https://www.geonames.org/enablefreewebservice
