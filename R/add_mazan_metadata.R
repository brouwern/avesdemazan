add_mazan_metadata <- function(spp_list, slopes){

  habitat_labels <- c("SHRUB", "NATIVE", "MIXED")

  ## vector names
  # names(habitat_labels) <- c("LLAV","MASE", "MAIN")
  names(habitat_labels) <- c("SHRUB","NATIVE", "MIXED")

  spp_trt <- spp_list

  spp_trt$Species_SciName <- gsub("[\\(\\)]", "", regmatches(spp_trt$Species, gregexpr("\\(.*?\\)", spp_trt$Species)))


  spp_trt$Species_SciName <- paste0(substr(spp_trt$Species_SciName,1,1),". ", sub("^\\S+\\s+", '', spp_trt$Species_SciName))


  i.col <- which(colnames(spp_trt) %in% c("Specie.Code", "Species_SciName"))

  spp_trt[,i.col[1]] <- as.character(spp_trt[,i.col[1]])


  # Merge metadata with slopes
  slopes.unique <- slopes[!duplicated(slopes$spp),]

  slopes.unique <- dplyr::left_join(slopes.unique,
                                    spp_trt[,i.col],
                                    by = c("spp" = "Specie.Code"))

  slopes_final <- dplyr::left_join(slopes, spp_trt[,i.col], by = c("spp" = "Specie.Code"))

  Species_SciName2_levels <- slopes.unique[order(slopes.unique$time_cts),]$Species_SciName
  slopes_final$Species_SciName2 <- ordered(slopes_final$Species_SciName,
                                           levels = Species_SciName2_levels)

  return(slopes_final)
}

