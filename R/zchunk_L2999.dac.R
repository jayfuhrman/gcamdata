# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_L2999.dac
#'
#' Compute a variety of final energy keyword, sector, share weight, and technology information for dac-related GCAM inputs.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2999.SectorLogitTables[[ curr_table ]]$data}, \code{L2999.Supplysector_dac}, \code{L2999.FinalEnergyKeyword_dac},
#' \code{L2999.SubsectorLogitTables[[ curr_table ]]$data}, \code{L2999.SubsectorLogit_dac}, \code{L2999.SubsectorShrwtFllt_dac},
#' \code{L2999.SubsectorInterp_dac}, \code{L2999.StubTech_dac}, \code{L2999.GlobalTechShrwt_dac}, \code{L2999.GlobalTechCoef_dac},
#' \code{L2999.GlobalTechCost_dac}, \code{L2999.GlobalTechCapture_dac}, \code{L2999.StubTechProd_dac}, \code{L2999.StubTechCalInput_dac_heat},
#' \code{L2999.StubTechCoef_dac}, \code{L2999.PerCapitaBased_dac}, \code{L2999.BaseService_dac}, \code{L2999.PriceElasticity_dac},
#' \code{L2999.IncomeElasticity_dac_gcam3}, \code{L2999.IncomeElasticity_dac_gssp1}, \code{L2999.IncomeElasticity_dac_gssp2},
#' \code{L2999.IncomeElasticity_dac_gssp3}, \code{L2999.IncomeElasticity_dac_gssp4}, \code{L2999.IncomeElasticity_dac_gssp5},
#' \code{L2999.IncomeElasticity_dac_ssp1}, \code{L2999.IncomeElasticity_dac_ssp2}, \code{L2999.IncomeElasticity_dac_ssp3},
#' \code{L2999.IncomeElasticity_dac_ssp4}, \code{L2999.IncomeElasticity_dac_ssp5}, \code{object}. The corresponding file in the
#' original data system was \code{L2999.dac.R} (energy level2).
#' @details The chunk provides final energy keyword, supplysector/subsector information, supplysector/subsector interpolation information, global technology share weight, global technology efficiency, global technology coefficients, global technology cost, price elasticity, stub technology information, stub technology interpolation information, stub technology calibrated inputs, and etc for dac sector.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows distinct filter if_else group_by lag left_join mutate pull select
#' @importFrom tidyr complete nesting
#' @author JF October 2017
module_energy_L2999.dac <- function(command, ...) {



  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "energy/calibrated_techs",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "energy/A999.sector",
             FILE = "energy/A999.subsector_interp",
             FILE = "energy/A999.subsector_logit",
             FILE = "energy/A999.subsector_shrwt",
             FILE = "energy/A999.globaltech_coef",
             FILE = "energy/A999.globaltech_cost",
             FILE = "energy/A999.globaltech_shrwt",
             FILE = "energy/A999.globaltech_co2capture",
             FILE = "energy/A999.demand"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L2999.Supplysector_dac",
             "L2999.FinalEnergyKeyword_dac",
             "L2999.SubsectorLogit_dac",
             "L2999.SubsectorShrwtFllt_dac",
             "L2999.SubsectorInterp_dac",
             "L2999.GlobalTechCost_dac",
             "L2999.StubTech_dac",
             "L2999.GlobalTechShrwt_dac",
             "L2999.GlobalTechCoef_dac",
             "L2999.GlobalTechCapture_dac",
             "L2999.PerCapitaBased_dac",
             "L2999.PriceElasticity_dac"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    calibrated_techs <- get_data(all_data, "energy/calibrated_techs")
    A999.sector <- get_data(all_data, "energy/A999.sector", strip_attributes = TRUE)
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")
    A999.subsector_interp <- get_data(all_data, "energy/A999.subsector_interp", strip_attributes = TRUE)
    A999.subsector_logit <- get_data(all_data, "energy/A999.subsector_logit", strip_attributes = TRUE)
    A999.subsector_shrwt <- get_data(all_data, "energy/A999.subsector_shrwt", strip_attributes = TRUE)
    A999.globaltech_coef <- get_data(all_data, "energy/A999.globaltech_coef")
    A999.globaltech_cost <- get_data(all_data, "energy/A999.globaltech_cost")
    A999.globaltech_shrwt <- get_data(all_data, "energy/A999.globaltech_shrwt", strip_attributes = TRUE)
    A999.globaltech_co2capture <- get_data(all_data, "energy/A999.globaltech_co2capture")
    A999.demand <- get_data(all_data, "energy/A999.demand", strip_attributes = TRUE)


    # ===================================================
    # 0. Give binding for variable names used in pipeline
    year <- value <- GCAM_region_ID <- sector <- fuel <- year.fillout <- to.value <-
      technology <- supplysector <- subsector <- minicam.energy.input <- coefficient <-
      remove.fraction <- minicam.non.energy.input <- input.cost <- PrimaryFuelCO2Coef.name <-
      PrimaryFuelCO2Coef <- calibration <- calOutputValue <- subs.share.weight <- region <-
      calibrated.value <- . <- scenario <- temp_lag <- base.service <- energy.final.demand <-
      value.x <- value.y <- parameter <- NULL

    # ===================================================
    # 1. Perform computations
    # 1a. Supplysector information
    # L2999.Supplysector_dac: Supply sector information for dac sector
    A999.sector %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2999.Supplysector_dac

    # L2999.FinalEnergyKeyword_dac: Supply sector keywords for ces sector
    A999.sector %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["FinalEnergyKeyword"]], GCAM_region_names) %>%
      na.omit ->
      L2999.FinalEnergyKeyword_dac

    # 1b. Subsector information
    # L2999.SubsectorLogit_dac: Subsector logit exponents of dac sector
    A999.subsector_logit %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["SubsectorLogit"]], LOGIT_TYPE_COLNAME), GCAM_region_names) ->
      L2999.SubsectorLogit_dac

    # and L2999.SubsectorShrwtFllt_dac: Subsector shareweights of dac sector
    A999.subsector_shrwt %>%
      filter(!is.na(year.fillout)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorShrwtFllt"]], GCAM_region_names) ->
      L2999.SubsectorShrwtFllt_dac

    # L2999.SubsectorInterp_dac: Subsector shareweight interpolation of dac sector
    A999.subsector_interp %>%
      filter(is.na(to.value)) %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["SubsectorInterp"]], GCAM_region_names) ->
      L2999.SubsectorInterp_dac

    # 1c. Technology information
    # L2999.StubTech_dac: Identification of stub technologies of dac
    # Note: assuming that technology list in the shareweight table includes the full set (any others would default to a 0 shareweight)
    A999.globaltech_shrwt %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["Tech"]], GCAM_region_names) %>%
      rename(stub.technology = technology) ->
      L2999.StubTech_dac

    # L2999.GlobalTechShrwt_dac: Shareweights of global dac technologies
    A999.globaltech_shrwt %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(share.weight = approx_fun(year, value, rule = 1)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "share.weight") ->
      L2999.GlobalTechShrwt_dac

    # L2999.GlobalTechCoef_dac: Energy inputs and coefficients of dac technologies
    A999.globaltech_coef %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.energy.input) %>%
      mutate(coefficient = approx_fun(year, value, rule = 1),
             coefficient = round(coefficient, energy.DIGITS_COEFFICIENT)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCoef"]]) ->
      L2999.GlobalTechCoef_dac

    # Carbon capture rates for dac.
    # L2999.GlobalTechCapture_dac: defines CO2 capture fractions for dac (by definition 1, as all inputs are defined per tonne C removed from the atmosphere),
    # as well as a separately-defined process heat dac sector.
    # This allows separate consideration of the capture fraction of any combustion emissions resulting from the process heat input
    # No need to consider historical periods here
    A999.globaltech_co2capture %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology), year = c(year, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, year) %>%
      group_by(supplysector, subsector, technology) %>%
      mutate(remove.fraction = approx_fun(year, value, rule = 1),
             remove.fraction = round(remove.fraction, energy.DIGITS_REMOVE.FRACTION)) %>%
      ungroup %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechYr"]], "remove.fraction") %>%
      mutate(storage.market = energy.CO2.STORAGE.MARKET) ->
      L2999.GlobalTechCapture_dac

    # L2999.GlobalTechCost_dac: Non-energy costs of global dac manufacturing technologies
    A999.globaltech_cost %>%
      gather_years %>%
      complete(nesting(supplysector, subsector, technology, minicam.non.energy.input), year = c(year, MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      arrange(supplysector, subsector, technology, minicam.non.energy.input, year) %>%
      group_by(supplysector, subsector, technology, minicam.non.energy.input) %>%
      mutate(input.cost = approx_fun(year, value, rule = 1),
             input.cost = round(input.cost, energy.DIGITS_COST)) %>%
      ungroup %>%
      filter(year %in% c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      rename(sector.name = supplysector,
             subsector.name = subsector) %>%
      select(LEVEL2_DATA_NAMES[["GlobalTechCost"]]) ->
      L2999.GlobalTechCost_dac # intermediate tibble

    # Note: adjusting non-energy costs of technologies with CCS to include CO2 capture costs
    #       The additional CCS-related non-energy costs are not included in the global technology assessment.
    #       Calculate here in two steps:
    #       (1) calculate the additional CCS costs per unit of carbon produced in 1975$
    #       (2) calculate the quantity of CO2 produced per unit of dac produced (in kgC per kg dac)

#    dac_CCS_cost_total_1975USDtC <- energy.CEMENT_CCS_COST_2000USDTCO2 * gdp_deflator(1975, base_year = 2000) * emissions.CONV_C_CO2
#    CO2_storage_cost_1975USDtC <- energy.CO2_STORAGE_COST_1990_USDTC * gdp_deflator(1975, base_year = 1990)
#    dac_CCS_cost_1975USDtC <- dac_CCS_cost_total_1975USDtC - CO2_storage_cost_1975USDtC

    L2999.GlobalTechCapture_dac %>%
      pull(remove.fraction) %>%
      mean -> dac_CO2_capture_frac

    L2999.GlobalTechCoef_dac %>%
      filter(minicam.energy.input == "airCO2") %>%
      pull(coefficient) %>%
      mean ->
      coef_mean # temporary value

    A_PrimaryFuelCCoef %>%
      filter(PrimaryFuelCO2Coef.name == "airCO2") %>%
      pull(PrimaryFuelCO2Coef) %>%
      mean ->
      PrimaryFuelCO2Coef_mean # temporary value

#    CO2_IO_kgCkgdac <- coef_mean * PrimaryFuelCO2Coef_mean
#    CO2stored_IO_kgCkgdac <- CO2_IO_kgCkgdac * dac_CO2_capture_frac
#    dac_CCS_cost_75USD_tdac <- dac_CCS_cost_1975USDtC * CO2stored_IO_kgCkgdac / CONV_T_KG

    # Adjust the non-energy costs in the table for model input
    L2999.GlobalTechCost_dac %>%
      filter(technology %in% L2999.GlobalTechCapture_dac[["technology"]]) %>%
#      mutate(input.cost = input.cost) %>%
      bind_rows(filter(L2999.GlobalTechCost_dac, !(technology %in% L2999.GlobalTechCapture_dac[["technology"]]))) %>%
      mutate(input.cost = round(input.cost, energy.DIGITS_COST)) ->
      L2999.GlobalTechCost_dac

     #Calibration and region-specific data
    # L2999.StubTechProd_dac: calibrated ces production
    calibrated_techs %>%
      filter(calibration == "output") %>% # Only take the tech IDs where the calibration is identified as output
      select(sector, supplysector, subsector, technology) %>%
      distinct ->
      calibrated_techs_export # temporary tibble


    # L2999.StubTechCoef_dac: region-specific coefficients of dac production technologies
    # Take this as a given in all years for which data is available
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble


    # L2999.StubTechCalInput_dac_heat: calibrated dac production
    calibrated_techs %>%
      select(sector, fuel, supplysector, subsector, technology, minicam.energy.input) %>%
      distinct ->
      calibrated_techs_export # temporary tibble


    # L2999.PerCapitaBased_dac: per-capita based flag for dac exports final demand.  Note that this should be zero as the amount of DAC shouldn't be explicitly tied to population
    A999.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PerCapitaBased"]], GCAM_region_names) ->
      L2999.PerCapitaBased_dac


    # L2999.BaseService_dac: base-year service output of dac
#    L2999.StubTechProd_dac %>%
#      select(region, year, base.service = calOutputValue) %>%
#      mutate(energy.final.demand = A999.demand[["energy.final.demand"]]) ->
#      L2999.BaseService_dac

    # L2999.PriceElasticity_dac: price elasticity
    A999.demand %>%
      write_to_all_regions(LEVEL2_DATA_NAMES[["PriceElasticity"]][LEVEL2_DATA_NAMES[["PriceElasticity"]] != "year"], GCAM_region_names) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(LEVEL2_DATA_NAMES[["PriceElasticity"]]) ->
      L2999.PriceElasticity_dac


    # ===================================================
    # Produce outputs

    L2999.Supplysector_dac %>%
      add_title("Supply sector information for ces (climate engineering services) sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector information (output.unit, input.unit, price.unit, logit.year.fillout, logit.exponent) from A999.sector is expended into all GCAM regions") %>%
      add_legacy_name("L2999.Supplysector_dac") %>%
      add_precursors("energy/A999.sector", "common/GCAM_region_names") ->
      L2999.Supplysector_dac

    L2999.FinalEnergyKeyword_dac %>%
      add_title("Supply sector keywords for dac sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the supply sector final energy keywords from A999.sector are expended into all GCAM regions") %>%
      add_legacy_name("L2999.FinalEnergyKeyword_dac") %>%
      add_precursors("energy/A999.sector", "common/GCAM_region_names") ->
      L2999.FinalEnergyKeyword_dac

    L2999.SubsectorLogit_dac %>%
      add_title("Subsector logit exponents of dac sector") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the subsector logit exponents from A999.subsector_logit are expanded into all GCAM regions") %>%
      add_legacy_name("L2999.SubsectorLogit_dac") %>%
      add_precursors("energy/A999.subsector_logit", "common/GCAM_region_names") ->
      L2999.SubsectorLogit_dac

    L2999.SubsectorShrwtFllt_dac %>%
      add_title("Subsector shareweights of dac sector") %>%
      add_units("unitless") %>%
      add_comments("For dac sector, the subsector shareweights from A999.subsector_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2999.SubsectorShrwtFllt_dac") %>%
      add_precursors("energy/A999.subsector_shrwt", "common/GCAM_region_names") ->
      L2999.SubsectorShrwtFllt_dac

    L2999.SubsectorInterp_dac %>%
      add_title("Subsector shareweight interpolation of dac sector") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the subsector shareweight interpolation function infromation from A999.subsector_interp is expanded into all GCAM regions") %>%
      add_legacy_name("L2999.SubsectorInterp_dac") %>%
      add_precursors("energy/A999.subsector_interp", "common/GCAM_region_names") ->
      L2999.SubsectorInterp_dac

    L2999.StubTech_dac %>%
      add_title("Identification of stub technologies of dac") %>%
      add_units("NA") %>%
      add_comments("For dac sector, the stub technologies from A999.globaltech_shrwt are expanded into all GCAM regions") %>%
      add_legacy_name("L2999.StubTech_dac") %>%
      add_precursors("energy/A999.globaltech_shrwt", "common/GCAM_region_names") ->
      L2999.StubTech_dac

    L2999.GlobalTechShrwt_dac %>%
      add_title("Shareweights of global dac technologies") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the share weights from A999.globaltech_shrwt are interpolated into all base years and future years") %>%
      add_legacy_name("L2999.GlobalTechShrwt_dac") %>%
      add_precursors("energy/A999.globaltech_shrwt") ->
      L2999.GlobalTechShrwt_dac

    L2999.GlobalTechCoef_dac %>%
      add_title("Energy inputs and coefficients of dac technologies") %>%
      add_units("airCO2 input is unitless (Mt airCO2 per Mt dac); all others are GJ per kg (EJ of energy per Mt of dac)") %>%
      add_comments("For dac sector, the energy use coefficients from A999.globaltech_coef are interpolated into all model years") %>%
      add_legacy_name("L2999.GlobalTechCoef_dac") %>%
      add_precursors("energy/A999.globaltech_coef") ->
      L2999.GlobalTechCoef_dac

    L2999.GlobalTechCost_dac %>%
      add_title("Non-energy costs of global dac manufacturing technologies") %>%
      add_units("1975$/kg for supplysector dac; 1975$/GJ for supplysector process heat dac") %>%
      add_comments("For dac sector, the Non-energy costs of global dac manufacturing technologies are calculated then adjusted with CCS to include CO2 capture costs") %>%
      add_legacy_name("L2999.GlobalTechCost_dac") %>%
      add_precursors("energy/A999.globaltech_cost", "energy/A999.globaltech_co2capture", "energy/A999.globaltech_coef", "emissions/A_PrimaryFuelCCoef") ->
      L2999.GlobalTechCost_dac

    L2999.GlobalTechCapture_dac %>%
      add_title("CO2 capture fractions from global dac production technologies with CCS") %>%
      add_units("Unitless") %>%
      add_comments("For dac sector, the remove fractions from A999.globaltech_co2capture are interpolated into all model years") %>%
      add_legacy_name("L2999.GlobalTechCapture_dac") %>%
      add_precursors("energy/A999.globaltech_co2capture") ->
      L2999.GlobalTechCapture_dac


    L2999.PerCapitaBased_dac %>%
      add_title("per-capita based flag for dac exports final demand") %>%
      add_units("NA") %>%
      add_comments("Per-capita based flags for dac from A999.demand are expanded into all GCAM regions") %>%
      add_legacy_name("L2999.PerCapitaBased_dac") %>%
      add_precursors("energy/A999.demand", "common/GCAM_region_names") ->
      L2999.PerCapitaBased_dac


    L2999.PriceElasticity_dac %>%
      add_title("price elasticity for dac") %>%
      add_units("Unitless") %>%
      add_comments("The elasticity values from A999.demand are expanded into all GCAM_regions") %>%
      add_legacy_name("L2999.PriceElasticity_dac") %>%
      add_precursors("energy/A999.demand", "common/GCAM_region_names") ->
      L2999.PriceElasticity_dac

    return_data(L2999.Supplysector_dac, L2999.FinalEnergyKeyword_dac, L2999.SubsectorLogit_dac,
                L2999.SubsectorShrwtFllt_dac, L2999.SubsectorInterp_dac,L2999.GlobalTechCost_dac,
                L2999.StubTech_dac, L2999.GlobalTechShrwt_dac,
                L2999.GlobalTechCoef_dac, L2999.GlobalTechCapture_dac,
                L2999.PerCapitaBased_dac,
                L2999.PriceElasticity_dac)
  } else {
    stop("Unknown command")
  }
}
