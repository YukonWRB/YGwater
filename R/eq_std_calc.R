# REVIEW make sure that all packages called are in the DESCRIPTION file (devtools::check() will tell you if any are missing). readxl is definitely not, but see note below about that problematic package.

#' Calculated standard processing sub-function required by EQ_fetch function
#'
#' Utility function to populate the std_calc_tmp table (product of EQ_fetch function) with calculated values, using averaged or calculated station parameters (pH, hardness, DOC, temp)
#'
#' @param sampledata  Assigned to "sampledata" data frame created during EQ_fetch function process containing sample data
#' @param calcs Assigned to "std_calc_tmp" data frame of calculated standards, populated by this function
#'
#' @return Populated std_calc_tmp table in EQfetch function
#'
#' @noRd
#' @keywords internal
#'
#' @details interim parameter values are assigned on an as-needs basis where required, such as standards where hardness is assigned a certain value if NA. These interim parameters are represented by the .x subscript (ie. pHx, hardx)
# REVIEW should this also be explained in eq_fetch?

eq_std_calc <- function(sampledata = sampledatafilt,
                        calcs = std_calc_tmp){

  #### Calculate input parameters from sampledata ####

  # Calculate pH with calculation order preference
  if(!all(is.na(sampledata$`pH-F (pH units)`))) {
    vec <- sampledata$`pH-F (pH units)`
    pH <- mean(vec[vec != 0 & !is.na(vec)])
  } else if(!all(is.na(sampledata$`pH-L (pH units)`))) {
    vec <- sampledata$`pH-L (pH units)`
    pH <- mean(vec[vec != 0 & !is.na(vec)])
  } else {pH <- NA}

  # Calculate hardness with calculation order preference
  if(!all(is.na(sampledata$`Hard-D (mg/L)`))) {
    vec <- sampledata$`Hard-D (mg/L)`
    hard <- mean(vec[vec != 0 & !is.na(vec)])
  } else if(!is.na(mean(na.omit(sampledata$`Ca-D (mg/L)`))*mean(na.omit(sampledata$`Mg-D (mg/L)`)))) {
    Ca <- sampledata$`Ca-D (mg/L)`
    Mg <- sampledata$`Mg-D (mg/L)`
    hard <- 2.497*mean(Ca[Ca != 0 & !is.na(Ca)]) + 4.118*mean(Mg[Mg != 0 & !is.na(Mg)])
  } else if(!all(is.na(sampledata$`Hard-T (mg/L)`))){
    vec <- sampledata$`Hard-T (mg/L)`
    hard <- mean(vec[vec != 0 & !is.na(vec)])
  } else if(!is.na(mean(na.omit(sampledata$`Ca-T (mg/L)`))*mean(na.omit(sampledata$`Mg-T (mg/L)`)))) {
    Ca <- sampledata$`Ca-T (mg/L)`
    Mg <- sampledata$`Mg-T (mg/L)`
    hard <- 2.497*mean(Ca[Ca != 0 & !is.na(Ca)]) + 4.118*mean(Mg[Mg != 0 & !is.na(Mg)])
  } else {
    hard <- NA}

  # Calculate DOC
  if(!all(is.na(sampledata$`C-DOC (mg/L)`))){
    vec <- sampledata$`C-DOC (mg/L)`
    DOC <- mean(vec[vec != 0 & !is.na(vec)])
  } else {
    DOC <- NA
  }

  # Calculate temp
  if(!all(is.na(sampledata$`Temp-F (C)`))) {
    temp <- plyr::round_any(mean(na.omit(sampledata$`Temp-F (C)`)), 5, f = floor)
  } else {
    temp <- NA
  }

  # Calculate Cl
  if(!all(is.na(sampledata$`Chlord (mg/L)`))) {
    vec <- sampledata$`Chlord (mg/L)`
    Cl <- mean(vec[vec != 0 & !is.na(vec)])
  } else {Cl <- NA}

  #### CCME_LT (T/D) ####

  # CCME_Al_lt
  if(!is.na(pH)){
    if(pH < 6.5){
      CCME_Al_lt <- 0.005}
    else if (pH >= 6.5){
      CCME_Al_lt <- 0.1}
  } else {
    CCME_Al_lt <- NA
  }
  if(is.element("CCME_Al_lt", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_Al_lt")] <- CCME_Al_lt
  }

  # CCME_Cd_lt
  if(hard < 17 | is.na(hard)){
    CCME_Cd_lt <- 0.04/1000
  } else if(hard >=17 & hard <=280){
    CCME_Cd_lt <- 10^(0.83*(log10(hard))-2.46)/1000
  } else if(hard > 280){
    CCME_Cd_lt <- 0.37/1000
  }
  if(is.element("CCME_Cd_lt", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_Cd_lt")] <- CCME_Cd_lt
  }

  # CCME_Cu_lt
  if(!is.na(hard)){
    if(hard < 82){
      CCME_Cu_lt <- 2/1000
    } else if(hard >= 82 & hard <= 180){
      CCME_Cu_lt <- 0.2*exp((0.8545*log(hard)-1.465))/1000
    } else if (hard > 180){
      CCME_Cu_lt <- 4/1000
    }
  } else {
    CCME_Cu_lt <- 2/1000
  }
  if(is.element("CCME_Cu_lt", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_Cu_lt")] <- CCME_Cu_lt
  }

  # CCME_Mn-D_lt
  if(is.na(pH)){
    pHx <- 7.5
  } else if(pH > 9){
    pHx <- 9
  } else if(pH < 5.5){
    pHx <- 5.5
  } else {
    pHx <- pH
  }
  pHx <- plyr::round_any(pHx, accuracy = 0.1, f = floor)

  if(is.na(hard)){
    hardx <- 50
  } else if(hard < 11){
    hardx <- 11
  } else if(hard > 670){
    hardx <- 670
  } else {
    hardx <- hard
  }
  hardx <- floor(hardx)

  lookup_mn <- YGwater:::data$eq_std_calc_CCME_Mn
  `CCME_Mn-D_lt` <- dplyr::pull(dplyr::filter(lookup_mn, hardx > Min & hardx <= Max)[which(colnames(lookup_mn) == as.character(pHx))])
  if(is.element("CCME_Mn-D_lt", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_Mn-D_lt")] <- `CCME_Mn-D_lt`
  }

  # CCME_NH4_lt
  if(is.na(pH)){
    pHx <- 7.5
  } else if(pH < 6){
    pHx <- 6
  }else {
    pHx <- pH
  }
  pHx <- plyr::round_any(pHx, accuracy = 0.5, f = floor)
  if(pHx <= 9.5 & pHx > 9){
    pHx <- 9
  } else if(pHx >9.5){
    pHx <- 10
  }
  if(is.na(temp)){
    tempx <- 30
  } else {
    tempx <- temp
  }

  lookup_nh4 <- YGwater:::data$eq_std_calc_CCME_NH4
  CCME_NH4_lt <- dplyr::pull(dplyr::filter(lookup_nh4, Temp == tempx)[which(colnames(lookup_nh4) == as.character(pHx))])
  if(is.element("CCME_NH4_lt", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_NH4_lt")] <- CCME_NH4_lt
  }

  # CCME_Ni_lt
  if(is.na(hard) | hard <= 60){
    CCME_Ni_lt <- 25/1000
  } else if(hard > 60 & hard <=180){
    CCME_Ni_lt <- exp((0.76*log(hard)+1.06))/1000
  } else if(hard > 180){
    CCME_Ni_lt <- 150/1000
  }
  if(is.element("CCME_Ni_lt", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_Ni_lt")] <- CCME_Ni_lt
  }

  # CCME_Pb_lt
  if(is.na(hard)){
    CCME_Pb_lt <- 1/1000
  } else if(hard <= 60){
    CCME_Pb_lt <- 1/1000
  } else if(hard > 60 & hard <=180){
    CCME_Pb_lt <- exp((1.273*log(hard)-4.705))/1000
  } else if(hard > 180){
    CCME_Pb_lt <- 7/1000
  }
  if(is.element("CCME_Pb_lt", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_Pb_lt")] <- CCME_Pb_lt
  }

  # CCME_Zn_lt
  if(any(is.na(c(DOC, pH, hard)))){
    CCME_Zn_lt <- NA
  } else if(pH < 6.5 | pH > 8.13 | hard < 23.4 | hard > 399 | DOC < 0.3 | DOC > 22.9){
    CCME_Zn_lt <- NA
  } else {
    CCME_Zn_lt <- exp((0.947*log(hard)) - (0.815*pH) + (0.398*log(DOC)) + 4.625)/1000
  }
  if(is.element("CCME_Zn_lt", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_Zn_lt")] <- CCME_Zn_lt
  }

  #### CCME_ST ####

  # CCME_Cd_st
  if(is.na(hard)){
    CCME_Cd_st <- NA
  } else if(hard < 5.3){
    CCME_Cd_st <- 0.11/1000
  } else if(hard >= 5.3 & hard <= 360){
    CCME_Cd_st <- 10^(1.016*(log10(hard))-1.71)/1000
  } else if(hard > 360){
    CCME_Cd_st <- 7.7/1000
  }
  if(is.element("CCME_Cd_st", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_Cd_st")] <- CCME_Cd_st
  }

  # CCME_Mn-D_st
  if(is.na(hard)){
    hardx <- 50
  } else {
    hardx <- hard
  }
  `CCME_Mn-D_st` <- exp((0.878*log(hardx)+4.76))/1000
  if(is.element("CCME_Mn-D_st", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_Mn-D_st")] <- `CCME_Mn-D_st`
  }

  # CCME_Zn_st
  if(any(is.na(c(DOC, pH, hard)))){
    CCME_Zn_st <- NA
  } else if(pH < 6.5 | pH > 8.13 | hard < 13.8 | hard > 250.5 | DOC < 0.3 | DOC > 17.3){
    CCME_Zn_st <- NA
  } else {
    CCME_Zn_st <- exp((0.833*log(hard)) + (0.240*log(DOC)) + 0.526)/1000
  }
  if(is.element("CCME_Zn_st", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "CCME_Zn_st")] <- CCME_Zn_st
  }

  #### CSR_S3_PAL ####

  # C3_AW_Ag
  if(is.na(hard)){
    C3_AW_Ag <- NA
  } else if(hard <= 100){
    C3_AW_Ag <- 0.5/1000
  } else if(hard > 100){
    C3_AW_Ag <- 15/1000
  }
  if(is.element("C3_AW_Ag", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3_AW_Ag")] <- C3_AW_Ag
  }

  # C3_AW_Cd
  if(is.na(hard)){
    C3_AW_Cd <- NA
  } else if(hard < 30){
    C3_AW_Cd <- 0.1/1000
  } else if(hard >= 30 & hard <90){
    C3_AW_Cd <- 0.3/1000
  } else if(hard >= 90 & hard <150){
    C3_AW_Cd <- 0.5/1000
  } else if(hard >= 150 & hard <210){
    C3_AW_Cd <- 0.6/1000
  } else if(hard >= 210){
    C3_AW_Cd <- NA
  }
  if(is.element("C3_AW_Cd", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3_AW_Cd")] <- C3_AW_Cd
  }

  # C3_AW_Cu
  if(is.na(hard)){
    C3_AW_Cu <- NA
  } else if(hard < 50){
    C3_AW_Cu <- 20/1000
  } else if(hard >= 50 & hard <75){
    C3_AW_Cu <- 30/1000
  } else if(hard >= 75 & hard <100){
    C3_AW_Cu <- 40/1000
  } else if(hard >= 100 & hard <125){
    C3_AW_Cu <- 50/1000
  } else if(hard >= 125 & hard <150){
    C3_AW_Cu <- 60/1000
  } else if(hard >= 150 & hard <175){
    C3_AW_Cu <- 70/1000
  } else if(hard >= 175 & hard <200){
    C3_AW_Cu <- 80/1000
  } else if(hard >= 200){
    C3_AW_Cu <- 90/1000
  }
  if(is.element("C3_AW_Cu", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3_AW_Cu")] <- C3_AW_Cu
  }

  # C3_AW_Flu
  if(is.na(hard)){
    C3_AW_Flu <- NA
  } else if(hard < 50){
    C3_AW_Flu <- 2000/1000
  } else if(hard >= 50){
    C3_AW_Flu <- 3000/1000
  }
  if(is.element("C3_AW_Flu", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3_AW_Flu")] <- C3_AW_Flu
  }

  # C3_AW_NH4
  if(is.na(pH)){
    C3_AW_NH4 <- NA
  } else if(pH < 7){
    C3_AW_NH4 <- 18400/1000
  } else if(pH >= 7 & pH < 7.5){
    C3_AW_NH4 <- 18500/1000
  } else if(pH >= 7.5 & pH < 8){
    C3_AW_NH4 <- 11300/1000
  } else if(pH >= 8 & pH < 8.5){
    C3_AW_NH4 <- 3700/1000
  } else if(pH >= 8.5){
    C3_AW_NH4 <- 1310/1000
  }
  if(is.element("C3_AW_NH4", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3_AW_NH4")] <- C3_AW_NH4
  }

  # C3_AW_Ni
  if(is.na(hard)){
    C3_AW_Ni <- NA
  } else if(hard < 60){
    C3_AW_Ni <- 250/1000
  } else if(hard >= 60 & hard <120){
    C3_AW_Ni <- 650/1000
  } else if(hard >= 120 & hard <180){
    C3_AW_Ni <- 1100/1000
  } else if(hard >= 180){
    C3_AW_Ni <- 1500/1000
  }
  if(is.element("C3_AW_Ni", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3_AW_Ni")] <- C3_AW_Ni
  }

  # C3_AW_NO2
  if(is.na(Cl)){
    C3_AW_NO2 <- NA
  } else if(Cl < 2){
    C3_AW_NO2 <- 200/1000
  } else if(Cl >= 2 & Cl < 4){
    C3_AW_NO2 <- 400/1000
  } else if(Cl >= 4 & Cl < 6){
    C3_AW_NO2 <- 600/1000
  } else if(Cl >= 6 & Cl < 8){
    C3_AW_NO2 <- 800/1000
  } else if(Cl >= 8 & Cl < 10){
    C3_AW_NO2 <- 1000/1000
  } else if(Cl >= 10){
    C3_AW_NO2 <- 2000/1000
  }
  if(is.element("C3_AW_NO2", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3_AW_NO2")] <- C3_AW_NO2
  }

  # C3_AW_Pb
  if(is.na(hard)){
    C3_AW_Pb <- NA
  } else if(hard < 50){
    C3_AW_Pb <- 40/1000
  } else if(hard >= 50 & hard <100){
    C3_AW_Pb <- 50/1000
  } else if(hard >= 100 & hard <200){
    C3_AW_Pb <- 60/1000
  } else if(hard >= 200 & hard <300){
    C3_AW_Pb <- 110/1000
  } else if(hard >= 300){
    C3_AW_Pb <- 160/1000
  }
  if(is.element("C3_AW_Pb", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3_AW_Pb")] <- C3_AW_Pb
  }

  # C3_AW_Zn
  if(is.na(hard)){
    C3_AW_Zn <- NA
  } else if(hard < 90){
    C3_AW_Zn <- 75/1000
  } else if(hard >= 90 & hard <100){
    C3_AW_Zn <- 150/1000
  } else if(hard >= 100 & hard <200){
    C3_AW_Zn <- 900/1000
  } else if(hard >= 200 & hard <300){
    C3_AW_Zn <- 1650/1000
  } else if(hard >= 300 & hard <400){
    C3_AW_Zn <- 2400/1000
  } else if(hard > 400){
    C3_AW_Zn <- NA
  }
  if(is.element("C3_AW_Zn", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3_AW_Zn")] <- C3_AW_Zn
  }

  #### CSR_S3_PAL_SW ####
  # Take CSR_s3_PAL standards, divide by 10

  # C3AWAgSW
  if(is.na(hard)){
    C3AWAgSW <- NA
  } else if(hard <= 100){
    C3AWAgSW <- 0.5/1000
  } else if(hard > 100){
    C3AWAgSW <- 15/1000
  }
  if(is.element("C3AWAgSW", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3AWAgSW")] <- C3AWAgSW/10
  }

  # C3AWCdSW
  if(is.na(hard)){
    C3AWCdSW <- NA
  } else if(hard < 30){
    C3AWCdSW <- 0.1/1000
  } else if(hard >= 30 & hard <90){
    C3AWCdSW <- 0.3/1000
  } else if(hard >= 90 & hard <150){
    C3AWCdSW <- 0.5/1000
  } else if(hard >= 150 & hard <210){
    C3AWCdSW <- 0.6/1000
  } else if(hard >= 210){
    C3AWCdSW <- NA
  }
  if(is.element("C3AWCdSW", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3AWCdSW")] <- C3AWCdSW/10
  }

  # C3AWCuSW
  if(is.na(hard)){
    C3AWCuSW <- NA
  } else if(hard < 50){
    C3AWCuSW <- 20/1000
  } else if(hard >= 50 & hard <75){
    C3AWCuSW <- 30/1000
  } else if(hard >= 75 & hard <100){
    C3AWCuSW <- 40/1000
  } else if(hard >= 100 & hard <125){
    C3AWCuSW <- 50/1000
  } else if(hard >= 125 & hard <150){
    C3AWCuSW <- 60/1000
  } else if(hard >= 150 & hard <175){
    C3AWCuSW <- 70/1000
  } else if(hard >= 175 & hard <200){
    C3AWCuSW <- 80/1000
  } else if(hard >= 200){
    C3AWCuSW <- 90/1000
  }
  if(is.element("C3AWCuSW", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3AWCuSW")] <- C3AWCuSW/10
  }

  # C3AWFluSW
  if(is.na(hard)){
    C3AWFluSW <- NA
  } else if(hard < 50){
    C3AWFluSW <- 2000/1000
  } else if(hard >= 50){
    C3AWFluSW <- 3000/1000
  }
  if(is.element("C3AWFluSW", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3AWFluSW")] <- C3AWFluSW/10
  }

  # C3AWNH4SW
  if(is.na(pH)){
    C3AWNH4SW <- NA
  } else if(pH < 7){
    C3AWNH4SW <- 18400/1000
  } else if(pH <= 7.5 & pH >7){
    C3AWNH4SW <- 18500/1000
  } else if(pH <= 8 & pH >7.5){
    C3AWNH4SW <- 11300/1000
  } else if(pH <= 8 & pH >8.5){
    C3AWNH4SW <- 3700/1000
  } else if(pH <= 8.5){
    C3AWNH4SW <- 1310/1000
  }
  if(is.element("C3AWNH4SW", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3AWNH4SW")] <- C3AWNH4SW/10
  }

  # C3AWNiSW
  if(is.na(hard)){
    C3AWNiSW <- NA
  } else if(hard < 60){
    C3AWNiSW <- 250/1000
  } else if(hard >= 60 & hard <120){
    C3AWNiSW <- 650/1000
  } else if(hard >= 120 & hard <180){
    C3AWNiSW <- 1100/1000
  } else if(hard >= 180){
    C3AWNiSW <- 1500/1000
  }
  if(is.element("C3AWNiSW", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3AWNiSW")] <- C3AWNiSW/10
  }

  # C3AWNO2SW
  if(is.na(Cl)){
    C3AWNO2SW <- NA
  } else if(Cl < 2){
    C3AWNO2SW <- 200/1000
  } else if(Cl >= 2 & Cl < 4){
    C3AWNO2SW <- 400/1000
  } else if(Cl >= 4 & Cl < 6){
    C3AWNO2SW <- 600/1000
  } else if(Cl >= 6 & Cl < 8){
    C3AWNO2SW <- 800/1000
  } else if(Cl >= 8 & Cl < 10){
    C3AWNO2SW <- 1000/1000
  } else if(Cl >= 10){
    C3AWNO2SW <- 2000/1000
  }
  if(is.element("C3AWNO2SW", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3AWNO2SW")] <- C3AWNO2SW/10
  }

  # C3AWPbSW
  if(is.na(hard)){
    C3AWPbSW <- NA
  } else if(hard < 50){
    C3AWPbSW <- 40/1000
  } else if(hard >= 50 & hard <100){
    C3AWPbSW <- 50/1000
  } else if(hard >= 100 & hard <200){
    C3AWPbSW <- 60/1000
  } else if(hard >= 200 & hard <300){
    C3AWPbSW <- 110/1000
  } else if(hard >= 300){
    C3AWPbSW <- 160/1000
  }
  if(is.element("C3AWPbSW", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3AWPbSW")] <- C3AWPbSW/10
  }

  # C3AWZnSW
  if(is.na(hard)){
    C3AWZnSW <- NA
  } else if(hard < 90){
    C3AWZnSW <- 75/1000
  } else if(hard >= 90 & hard <100){
    C3AWZnSW <- 150/1000
  } else if(hard >= 100 & hard <200){
    C3AWZnSW <- 900/1000
  } else if(hard >= 200 & hard <300){
    C3AWZnSW <- 1650/1000
  } else if(hard >= 300 & hard <400){
    C3AWZnSW <- 2400/1000
  } else if(hard > 400){
    C3AWZnSW <- NA
  }
  if(is.element("C3AWZnSW", calcs$MaxVal)){
    calcs$MaxVal[which(calcs$MaxVal == "C3AWZnSW")] <- C3AWZnSW/10
  }

  return(calcs)
}
