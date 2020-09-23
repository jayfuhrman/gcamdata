# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_LA1999.dac
#'
#' Sets up input, output, and IO coefficients for dac
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L1321.out_Mt_R_cement_Yh}, \code{L1321.IO_GJkg_R_cement_F_Yh}, \code{L1321.in_EJ_R_cement_F_Y}, \code{L1321.in_EJ_R_indenergy_F_Yh}. The corresponding file in the
#' original data system was \code{LA1321.cement.R} (energy level1).
#' @details This chunk generates input, output, and IO coefficients for the direct air capture sector. (these should be universally zero for historical periods as there is no dac in history)
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join mutate select semi_join summarise summarise_all
#' @importFrom tidyr complete gather nesting
#' @author JF Nov 2017
module_energy_LA1999.dac <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/iso_GCAM_regID",
             FILE = "emissions/A_PrimaryFuelCCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L1999.out_Mt_R_dac_Yh"))
  } else if(command == driver.MAKE) {

    # Silence global variable package check
    . <- Biomass <- Biomass_EJ <- Coal <- Coal_EJ <- Country <- GCAM_region_ID <- Gas <- Gas_EJ <-
    IEA_fuelshare_region <- IEA_intensity_region <- IOelec <- Oil <- Oil_EJ <- TPE_GJkg <-
    Worrell_region <- cement_prod_Mt <- country_name <- elec_EJ <- elec_GJkg <-
    emiss_ktC <- fuel <- heat_EJ <- heat_GJkg <- in.value <- ind.value <- iso <-
    old.year <- out.value <- process_emissions_MtC <- process_emissions_ktC <-
    prod_Mt <- prod_emiss_ratio <- reg_process_emissions <- region_GCAM3 <- sector <-
    share <- value <- cement <- year <- value.y <- value.x <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")


    # ===================================================
    # 2. Perform computations

    # Set constants used for this chunk
    # ---------------------------------

    # Extract carbon coefficient for atmospheric CO2 "fuel" from assumption file
    airCO2_CCOEF <- A_PrimaryFuelCCoef$PrimaryFuelCO2Coef[A_PrimaryFuelCCoef$PrimaryFuelCO2Coef.name == "airCO2"]
    # Determine historical years not available in data set (additional years) to copy values from final available year (final_CO2_year)
    ADDITIONAL_YEARS <- HISTORICAL_YEARS[!HISTORICAL_YEARS %in% energy.CDIAC_CO2_HISTORICAL_YEARS]
    FINAL_CO2_YEAR <- dplyr::last(energy.CDIAC_CO2_HISTORICAL_YEARS)






    return_data(L1999.out_Mt_R_dac_Yh)
  } else {
    stop("Unknown command")
  }
}
