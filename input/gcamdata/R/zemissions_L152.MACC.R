# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L152.MACC
#'
#' Create Marginal Abatement Cost Curves, in percent reduction by 1990 USD abatement costs from EPA cost curves.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L152.MAC_pct_R_S_Proc_EPA}. The corresponding file in the
#' original data system was \code{L152.MACC.R} (emissions level1).
#' @details Create Marginal abatement cost curves, in percent reduction by 1990 USD costs from EPA cost curves.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by left_join mutate select vars summarize_at
#' @importFrom tidyr gather spread
#' @author RMH May 2017 / YO Mar 2020

module_emissions_L152.MACC <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "emissions/EPA_2025/emis_baselines_by_gas_1990_2030",
             FILE = "emissions/EPA_2025/emis_baselines_by_gas_2031_2100",
             FILE = "emissions/EPA_2025/NonCO2_EPA_GCAM_Source_Mapping",
             FILE = "emissions/EPA_MACC_control_mapping",
             FILE = "emissions/EPA_MAC_missing_region",
             FILE = "emissions/EPA_2025/EPA_country_map_2025",
             FILE = "emissions/EPA_2025/macc_results_raw_AGRICULTURE",
             FILE = "emissions/EPA_2025/ISO_GCAM_region_map",
             FILE = "emissions/EPA_2025/macc_results_raw_ENERGY",
             FILE = "emissions/EPA_2025/macc_results_raw_INDUSTRIAL",
             FILE = "emissions/EPA_2025/macc_results_raw_WWR",
             FILE = "emissions/EPA_2025/macc_results_raw_LAN_1",
             FILE = "emissions/EPA_2025/macc_results_raw_LAN_2",
             FILE = "emissions/EPA_2025/macc_results_raw_LAN_3",
             FILE = "emissions/EPA_2025/macc_results_raw_LAN_4",
             FILE = "emissions/EPA_2025/macc_results_raw_LAN_5",
             FILE = "emissions/EPA_2025/macc_results_raw_LAN_6",
             FILE = "emissions/EPA_2025/macc_results_raw_LAN_7",
             FILE = "emissions/EPA_2025/macc_results_raw_LAN_8",
             FILE = "emissions/EPA_2025/macc_results_raw_LAN_9"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L152.MAC_pct_R_S_Proc_EPA"))
  } else if(command == driver.MAKE) {

    Process <- EPA_region <- cost_2010USD_tCO2e <- reduction_MtCO2e <- Sector <-
        EPA_region_code <- cost_1990USD_tCe <- year <- baseline_MtCO2e <-
            reduction_pct <- GCAM_region_ID_missing <- NULL       # silence package check.

    all_data <- list(...)[[1]]

    #silence packages
    cum_reduction_MtCO2e <- p <- value <- sector <- Sector <- GCAM_region_ID <-
      EPA_country <- iso <- EPA_sector <- GCAM_region_ID_missing <- NULL

    # Load required inputs
    EPA_baseline_master <- get_data(all_data, "emissions/EPA_2025/emis_baselines_by_gas_1990_2030")
    EPA_baseline_master2 <- get_data(all_data, "emissions/EPA_2025/emis_baselines_by_gas_2031_2100")
    EPA_MACC_Ag <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_AGRICULTURE")
    EPA_MACC_En <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_ENERGY")
    EPA_MACC_Ind <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_INDUSTRIAL")
    EPA_MACC_WWR <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_WWR")
    EPA_MACC_mapping <- get_data(all_data, "emissions/EPA_2025/NonCO2_EPA_GCAM_Source_Mapping")
    EPA_MACC_control_mapping <- get_data(all_data, "emissions/EPA_MACC_control_mapping")
    EPA_MAC_missing_region <- get_data(all_data, "emissions/EPA_MAC_missing_region")
    EPA_country_map <- get_data(all_data, "emissions/EPA_2025/EPA_country_map_2025")
    EPA_ISO_map <- get_data(all_data, "emissions/EPA_2025/ISO_GCAM_region_map")

    EPA_MACC_Lan_1 <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_LAN_1")
    EPA_MACC_Lan_2 <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_LAN_2")
    EPA_MACC_Lan_3 <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_LAN_3")
    EPA_MACC_Lan_4 <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_LAN_4")
    EPA_MACC_Lan_5 <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_LAN_5")
    EPA_MACC_Lan_6 <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_LAN_6")
    EPA_MACC_Lan_7 <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_LAN_7")
    EPA_MACC_Lan_8 <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_LAN_8")
    EPA_MACC_Lan_9 <- get_data(all_data, "emissions/EPA_2025/macc_results_raw_LAN_9")


    # baseline data
    EPA_baseline_master %>%
      rbind(EPA_baseline_master2) %>%
      left_join_error_no_match(EPA_country_map %>% select(-iso) %>% rename(country = EPA_country), by = "country") %>%
      left_join(EPA_MACC_mapping %>% select(-sector), by = c("source")) %>%
      filter(!is.na(Process)) %>%
      rename(Sector = sector) %>%
      group_by(GCAM_region_ID, Sector, Process, year) %>%
      summarise(value = sum(qbaseline)) %>%
      ungroup() %>%
      filter(year %in% emissions.EPA_MACC_YEAR) ->
      EPA_MACC_baselines_MtCO2e

    # mac data
    # Convert from 2010$/tCO2e to 1990$/tC

    EPA_MACC_Ag %>%
      rbind(EPA_MACC_Lan_1) %>%
      rbind(EPA_MACC_Lan_2) %>%
      rbind(EPA_MACC_Lan_3) %>%
      rbind(EPA_MACC_Lan_4) %>%
      rbind(EPA_MACC_Lan_5) %>%
      rbind(EPA_MACC_Lan_6) %>%
      rbind(EPA_MACC_Lan_7) %>%
      rbind(EPA_MACC_Lan_8) %>%
      rbind(EPA_MACC_Lan_9) %>%
      rbind(EPA_MACC_En) %>%
      rbind(EPA_MACC_Ind %>%
              filter(!country_code %in% c("TUV","NRU"))) %>%
      rbind(EPA_MACC_WWR) %>%
      left_join_error_no_match(EPA_ISO_map %>% mutate(country_code = toupper(country_code)), by = "country_code") %>%
      left_join_error_no_match(EPA_MACC_control_mapping, by = c("sector", "source")) %>%
      rename(cost_2024USD_tCO2e = p, reduction_MtCO2e = q_total) %>%
      select(GCAM_region_ID, Sector, Process, year, cost_2024USD_tCO2e, reduction_MtCO2e) %>%
      mutate(cost_2024USD_tCO2e = as.numeric(cost_2024USD_tCO2e),
             cost_1990USD_tCe = round(cost_2024USD_tCO2e * emissions.CONV_C_CO2 * gdp_deflator(1990, base_year = 2024), 0)) %>%
      select(-cost_2024USD_tCO2e) ->
      L152.EPA_MACC_MtCO2e_ungrouped

    # For in abatement and basebline data:
    # Combine aluminum and magnesium processes: define function, then call in both instances
    combine_Al_Mg <- function(x) {
      x %>%
        mutate(Process = sub("Primary Aluminum Production", "Aluminum and Magnesium Production", Process),
               Process = sub("Magnesium Manufacturing", "Aluminum and Magnesium Production", Process))
    }

    # Abatement data
    L152.EPA_MACC_MtCO2e_ungrouped %>%
      ungroup %>%
      combine_Al_Mg %>%
      group_by(Sector, Process, GCAM_region_ID, year, cost_1990USD_tCe) %>%
      summarize_at(vars(reduction_MtCO2e), sum) %>%
      ungroup() %>%
      group_by(Sector, Process, GCAM_region_ID, year) %>%
      mutate(cum_reduction_MtCO2e = cumsum(reduction_MtCO2e)) %>%
      ungroup() %>%
      replace_na(list(cum_reduction_MtCO2e = 0)) ->
      L152.EPA_MACC_MtCO2e

    # Baseline data
    # Also filter for only EPA MACC year
    EPA_MACC_baselines_MtCO2e %>%
      combine_Al_Mg %>%
      group_by(GCAM_region_ID, Sector, Process, year) %>%
      summarise(baseline_MtCO2e = sum(value)) %>%
      replace_na(list(baseline_MtCO2e = 0)) %>%
      ungroup() ->
      L152.EPA_MACC_baselines_MtCO2e

    # Match in the baseline emissions quantities to abatement tibble then calculate abatement percentages
    # Use left_join - there should be NAs (i.e., there are sectors where the baseline is zero) - then drop those NAs
    # (ie. MAC curves in regions where the sector/process does not exist - the baseline is zero)
    # emissions.MAC_HIGHESTREDUCTION is 0.95, defined in constant.R

    L152.EPA_MACC_MtCO2e %>%
      mutate(GCAM_region_ID = as.integer(GCAM_region_ID)) %>%
      left_join(L152.EPA_MACC_baselines_MtCO2e ,
                by = c("Sector", "Process", "GCAM_region_ID", "year")) %>%
      mutate(reduction_pct = cum_reduction_MtCO2e / baseline_MtCO2e,
             reduction_pct = if_else(is.na(reduction_pct) | is.infinite(reduction_pct), 0, reduction_pct),
             reduction_pct = if_else(reduction_pct >= 1, emissions.MAC_HIGHESTREDUCTION, reduction_pct)) %>%
      ungroup() %>%
      select(Sector, Process, GCAM_region_ID, year, cost_1990USD_tCe, reduction_pct) ->
      L152.EPA_MACC_percent_MtCO2e

    price_cut <- round(emissions.MAC_TAXES * emissions.CONV_C_CO2 * gdp_deflator(1990, base_year = 2010), 0)

    # create a template based on standarized price-cuts
    L152.EPA_MACC_percent_MtCO2e %>%
      select(Sector, Process, GCAM_region_ID, year) %>%
      distinct() %>%
      repeat_add_columns(tibble::tibble(cost_1990USD_tCe = price_cut)) ->
      L152.EPA_MACC_percent_MtCO2e_standardized

    # insert "standard MAC taxes" into the MAC table and complete the table
   suppressWarnings(L152.EPA_MACC_percent_MtCO2e %>%
      full_join(L152.EPA_MACC_percent_MtCO2e_standardized,
                by = c("Sector", "Process", "GCAM_region_ID", "year", "cost_1990USD_tCe")) %>%
      group_by(Sector, Process, GCAM_region_ID, year) %>%
      mutate(reduction_pct = approx_fun(cost_1990USD_tCe, reduction_pct)) %>%
      mutate(reduction_pct = map_dbl(cost_1990USD_tCe,~max(reduction_pct[cost_1990USD_tCe<=.x], na.rm = T))) %>%
      mutate(reduction_pct = ifelse(reduction_pct < 0,0,reduction_pct)) %>%
      ungroup() %>%
      filter(cost_1990USD_tCe %in% price_cut) %>%
      arrange(Sector, Process, GCAM_region_ID, year)) ->
      L152.EPA_MACC_percent_MtCO2e_complete



    # Select reduction percentage data for the given tax levels,
    # tax levels in emissions.MAC_TAXES are simply a range of costs in $1990 USD so we aren't retaining superfluous detail
    # create a new df with all rows for all costs for each unique Sector-Process-Region,
    # then add reduction percentages at those costs
    # keep MAC data for MODEL_FUTURE_YEARS

    L152.EPA_MACC_percent_MtCO2e_complete %>%
      filter(year %in% MODEL_FUTURE_YEARS) %>%
      rename(tax = cost_1990USD_tCe) %>%
      rename(mac.reduction = reduction_pct) %>%
      rename(mac.control = Process) ->
      L152.MAC_pct_R_S_Proc_EPA_missing

    # fill in missing MAC regions based on a mapping file - EPA_MAC_missing_region
    # currently just add Taiwan becuase EPA 2019 does not have it
    # assign all measures same as China (region 11) for data completeness

    if(!is.na(EPA_MAC_missing_region$GCAM_region_ID_missing)){
      L152.MAC_pct_R_S_Proc_EPA_missing %>%
        filter(GCAM_region_ID %in% EPA_MAC_missing_region$GCAM_region_ID_alternative) %>%
        left_join_error_no_match(EPA_MAC_missing_region, by = c("GCAM_region_ID" = "GCAM_region_ID_alternative")) %>%
        mutate(GCAM_region_ID = GCAM_region_ID_missing) %>%
        select(-GCAM_region_ID_missing) %>%
        bind_rows(L152.MAC_pct_R_S_Proc_EPA_missing) ->
        L152.MAC_pct_R_S_Proc_EPA
    } else {
      L152.MAC_pct_R_S_Proc_EPA_missing ->
        L152.MAC_pct_R_S_Proc_EPA
    }

    # ===================================================
    # Produce outputs
    L152.MAC_pct_R_S_Proc_EPA %>%
      add_title("Marginal abatement cost curves by GCAM region / EPA sector / process /year") %>%
      add_units("%") %>%
      add_comments("Marginal abatement cost curves, in percent reduction by 1990 USD abatement costs from EPA cost curves") %>%
      add_legacy_name("L152.MAC_pct_R_S_Proc_EPA") %>%
      add_precursors("emissions/EPA_2025/emis_baselines_by_gas_1990_2030",
                     "emissions/EPA_2025/emis_baselines_by_gas_2031_2100",
                     "emissions/EPA_2025/NonCO2_EPA_GCAM_Source_Mapping",
                     "emissions/EPA_MACC_control_mapping",
                     "emissions/EPA_MAC_missing_region",
                     "emissions/EPA_2025/EPA_country_map_2025",
                     "emissions/EPA_2025/macc_results_raw_AGRICULTURE",
                     "emissions/EPA_2025/ISO_GCAM_region_map",
                     "emissions/EPA_2025/macc_results_raw_ENERGY",
                     "emissions/EPA_2025/macc_results_raw_INDUSTRIAL",
                     "emissions/EPA_2025/macc_results_raw_WWR",
                     "emissions/EPA_2025/macc_results_raw_LAN_1",
                     "emissions/EPA_2025/macc_results_raw_LAN_2",
                     "emissions/EPA_2025/macc_results_raw_LAN_3",
                     "emissions/EPA_2025/macc_results_raw_LAN_4",
                     "emissions/EPA_2025/macc_results_raw_LAN_5",
                     "emissions/EPA_2025/macc_results_raw_LAN_6",
                     "emissions/EPA_2025/macc_results_raw_LAN_7",
                     "emissions/EPA_2025/macc_results_raw_LAN_8",
                     "emissions/EPA_2025/macc_results_raw_LAN_9") ->
      L152.MAC_pct_R_S_Proc_EPA

    return_data(L152.MAC_pct_R_S_Proc_EPA)
  } else {
    stop("Unknown command")
  }
}
