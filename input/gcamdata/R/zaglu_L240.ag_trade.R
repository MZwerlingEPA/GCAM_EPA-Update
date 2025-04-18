# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L240.ag_trade
#'
#' Model input for regional and (globally) traded agricultural crops and livestock commodities
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs, a vector of output names, or (if
#'   \code{command} is "MAKE") all the generated outputs: \code{L240.Supplysector_tra},
#'   \code{L240.SectorUseTrialMarket_tra}, \code{L240.SubsectorAll_tra}, \code{L240.TechShrwt_tra},
#'   \code{L240.TechCost_tra}, \code{L240.TechCoef_tra}, \code{L240.Production_tra}, \code{L240.Supplysector_reg},
#'   \code{L240.SubsectorAll_reg}, \code{L240.TechShrwt_reg}, \code{L240.TechCoef_reg}, \code{L240.Production_reg_imp},
#'   \code{L240.Production_reg_dom}.
#' @details Build datasets for ssp4 agricultural trade: food and nonfood trade coefficients, feed trade
#' coefficients, restricted agricultural trade, and trade regions.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter if_else left_join mutate rename select
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @author GPK February 2019  XZ March 2020
module_aglu_L240.ag_trade <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "aglu/A_agRegionalSector",
      FILE = "aglu/A_agRegionalSubsector",
      FILE = "aglu/A_agRegionalTechnology",
      FILE = "aglu/A_agTradedSector",
      FILE = "aglu/A_agTradedSubsector",
      FILE = "aglu/A_agTradedTechnology",
      FILE = "common/iso_GCAM_regID",
      "L109.ag_ALL_Mt_R_C_Y",
      "L109.an_ALL_Mt_R_C_Y",
      "L110.For_ALL_bm3_R_Y",
      "L100.FAO_For_Exp_m3")

  MODULE_OUTPUTS <-
    c("L240.Supplysector_tra",
      "L240.SectorUseTrialMarket_tra",
      "L240.SubsectorAll_tra",
      "L240.TechShrwt_tra",
      "L240.TechCost_tra",
      "L240.TechCoef_tra",
      "L240.Production_tra",
      "L240.Supplysector_reg",
      "L240.SubsectorAll_reg",
      "L240.TechShrwt_reg",
      "L240.TechCoef_reg",
      "L240.Production_reg_imp",
      "L240.Production_reg_dom")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- region <- supplysector <- subsector <- GCAM_commodity <- GrossExp_Mt <-
      calOutputValue <- subs.share.weight <- market.name <- minicam.energy.input <-
      GrossImp_Mt <- Prod_Mt <- GCAM_region_ID <- NetExp_Mt <- Prod_bm3 <-
      NetExp_bm3 <- value <- flow <- GrossExp <- NULL # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # 0: Bind crops, livestock, and forest for prod and net trade (netexp) ----
    L109.ag_an_for_ALL_Mt_R_C_Y <- L109.ag_ALL_Mt_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, Prod_Mt, NetExp_Mt) %>%
      bind_rows(L109.an_ALL_Mt_R_C_Y %>%
                  select(GCAM_region_ID, GCAM_commodity, year, Prod_Mt, NetExp_Mt)) %>%
      bind_rows(L110.For_ALL_bm3_R_Y %>%
                  select(GCAM_region_ID, GCAM_commodity, year,
                         Prod_Mt = Prod_bm3, NetExp_Mt = NetExp_bm3)) #note that physical unit for forest data is bm3

    #  and for gross trade
    L1091.GrossTrade_Mt_R_C_Y <- L109.ag_ALL_Mt_R_C_Y %>%
      select(GCAM_region_ID, GCAM_commodity, year, GrossExp_Mt, GrossImp_Mt) %>%
      bind_rows(L109.an_ALL_Mt_R_C_Y %>%
                  select(GCAM_region_ID, GCAM_commodity, year, GrossExp_Mt, GrossImp_Mt)) %>%
      bind_rows(L110.For_ALL_bm3_R_Y %>%
                  select(GCAM_region_ID, GCAM_commodity, year, GrossExp_Mt, GrossImp_Mt))


    # 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY") ----
    # L240.Supplysector_tra: generic supplysector info for traded ag commodities
    # By convention, traded commodity information is contained within the USA region (could be within any)
    A_agTradedSector$region <- gcam.USA_REGION

    # L240.Supplysector_tra: generic supplysector info for traded ag commodities
    L240.Supplysector_tra <- mutate(A_agTradedSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

    # L240.SectorUseTrialMarket_tra: Create solved markets for the traded sectors
    L240.SectorUseTrialMarket_tra <- select(A_agTradedSector, region, supplysector) %>%
      mutate(use.trial.market = 1)

    # L240.SubsectorAll_tra: generic subsector info for traded ag commodities
    # Traded commodities have the region set to USA and the subsector gets the region name pre-pended
    L240.SubsectorAll_tra <- write_to_all_regions(A_agTradedSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "logit.type"),
                                                  filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS),
                                                  has_traded = TRUE)

    # Base technology-level table for several tables to be written out")
    A_agTradedTechnology_R_Y <- repeat_add_columns(A_agTradedTechnology,
                                                   tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS)) %>%
      mutate(subsector = paste(region, subsector, sep = " "),
             technology = subsector,
             market.name = region,
             region = gcam.USA_REGION)

    # L240.TechShrwt_tra: Share-weights of traded technologies
    L240.TechShrwt_tra <- select(A_agTradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L240.TechCost_tra: Costs of traded technologies
    L240.TechCost_tra <- A_agTradedTechnology_R_Y %>%
      mutate(minicam.non.energy.input = "trade costs") %>%
      select(LEVEL2_DATA_NAMES[["TechCost"]])

    # L240.TechCoef_tra: Coefficient and market name of traded technologies
    L240.TechCoef_tra <- select(A_agTradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]]) %>%
      mutate(minicam.energy.input = if_else(minicam.energy.input %in% aglu.FOREST_COMMODITIES,paste0(minicam.energy.input, "_processing"),minicam.energy.input))


    # L240.Production_tra: Output (gross exports) of traded technologies
    L240.GrossExports_Mt_R_C_Y <- left_join_error_no_match(L1091.GrossTrade_Mt_R_C_Y,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, year, GrossExp_Mt)

    L240.Production_tra <- filter(A_agTradedTechnology_R_Y, year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(L240.GrossExports_Mt_R_C_Y,
                               by = c(market.name = "region", minicam.energy.input = "GCAM_commodity", "year")) %>%
      rename(calOutputValue = GrossExp_Mt) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      mutate(minicam.energy.input = if_else(minicam.energy.input %in% aglu.FOREST_COMMODITIES,paste0(minicam.energy.input, "_processing"),minicam.energy.input)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # 2: DOMESTIC SUPPLY SECTOR / SUBSECTOR / TECHNOLOGY") ----
    # L240.Supplysector_reg: generic supplysector info for regional ag commodities
    L240.Supplysector_reg <- mutate(A_agRegionalSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      write_to_all_regions(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"),
                           filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS))

    # L240.SubsectorAll_reg: generic subsector info for regional ag commodities (competing domestic prod vs intl imports)
    L240.SubsectorAll_reg <- write_to_all_regions(A_agRegionalSubsector,
                                                  c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "logit.type"),
                                                  filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS))

    # Base technology-level table for several tables to be written out")
    A_agRegionalTechnology_R_Y <- repeat_add_columns(A_agRegionalTechnology,
                                                     tibble(year = MODEL_YEARS)) %>%
      repeat_add_columns(filter(GCAM_region_names["region"], !region %in% aglu.NO_AGLU_REGIONS)) %>%
      mutate(market.name = if_else(market.name == "regional", region, market.name))

    # L240.TechShrwt_tra: Share-weights of traded technologies
    L240.TechShrwt_reg <- select(A_agRegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

    # L240.TechCoef_reg: Coefficient and market name of traded technologies
    L240.TechCoef_reg <- select(A_agRegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]]) %>%
                         mutate(minicam.energy.input= if_else(minicam.energy.input %in% aglu.FOREST_COMMODITIES, paste0(minicam.energy.input, "_processing"),minicam.energy.input))

    # L240.Production_reg_imp: Output (flow) of gross imports
    # Imports are equal to the gross imports calculated in L1091
    L240.GrossImports_Mt_R_C_Y <- left_join_error_no_match(L1091.GrossTrade_Mt_R_C_Y,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      left_join(select(A_agTradedTechnology, supplysector, minicam.energy.input),
                by = c(GCAM_commodity = "minicam.energy.input")) %>%
      select(region, supplysector, year, GrossImp_Mt)
    L240.Production_reg_imp <- A_agRegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "import", subsector)) %>%
      left_join_error_no_match(L240.GrossImports_Mt_R_C_Y,
                               by = c("region", minicam.energy.input = "supplysector", "year")) %>%
      rename(calOutputValue = GrossImp_Mt) %>%
      mutate(share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # L240.Production_reg_dom: Output (flow) of domestic
    # Domestic "output" is equal to production (Prod_Mt in L109) minus gross exports (calculated in L1091)

    #### DOMESTIC TECHNOLOGY OUTPUT = AG PRODUCTION - GROSS EXPORTS
    L240.GrossExports_Mt_R_C_Y <- left_join_error_no_match(L1091.GrossTrade_Mt_R_C_Y,
                                                           GCAM_region_names,
                                                           by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, year, GrossExp_Mt)

    L240.Prod_Mt_R_C_Y <- left_join_error_no_match(L109.ag_an_for_ALL_Mt_R_C_Y,
                                                   GCAM_region_names,
                                                   by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, year, Prod_Mt)

    L240.OpenStock_Mt_R_C_Y <-
      L109.ag_ALL_Mt_R_C_Y %>%
      gather(element, value, -GCAM_commodity, -year, -GCAM_region_ID) %>%
      bind_rows(
        L109.an_ALL_Mt_R_C_Y %>%
          gather(element, value, -GCAM_commodity, -year, -GCAM_region_ID)
      ) %>%
      # Keep relevant elements, storage comm., and base years only
      filter(element %in% c("Opening stocks")) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(region, GCAM_commodity, year, element, value) %>%
      spread(element, value)

    L240.Production_reg_dom <- A_agRegionalTechnology_R_Y %>%
      filter(year %in% MODEL_BASE_YEARS,
             grepl( "domestic", subsector)) %>%
      left_join_error_no_match(L240.GrossExports_Mt_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_commodity", "year")) %>%
      left_join_error_no_match(L240.Prod_Mt_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_commodity", "year")) %>%
      # using left_join here because forest had no storage placeholder added
      left_join(L240.OpenStock_Mt_R_C_Y,
                               by = c("region", minicam.energy.input = "GCAM_commodity", "year")) %>%
      replace_na(list(`Opening stocks` = 0)) %>%
      # storage enters domestic market as well
      mutate(calOutputValue = Prod_Mt - GrossExp_Mt + `Opening stocks`,
             share.weight.year = year,
             subs.share.weight = if_else(calOutputValue > 0, 1, 0),
             tech.share.weight = subs.share.weight) %>%
      mutate(minicam.energy.input = if_else(minicam.energy.input %in% aglu.FOREST_COMMODITIES,paste0(minicam.energy.input, "_processing"),minicam.energy.input)) %>%
      select(LEVEL2_DATA_NAMES[["Production"]])

    # Produce outputs
    L240.Supplysector_tra %>%
      add_title("Supplysector info for traded ag commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedSector") ->
      L240.Supplysector_tra

    L240.SectorUseTrialMarket_tra %>%
      add_title("Supplysector flag indicating to make trial markets") %>%
      add_units("None") %>%
      add_comments("This helps model solution when running with ag trade") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedSector") ->
      L240.SectorUseTrialMarket_tra

    L240.SubsectorAll_tra %>%
      add_title("Subsector info for traded ag commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedSubsector") ->
      L240.SubsectorAll_tra

    L240.TechShrwt_tra %>%
      add_title("Technology share-weights for traded ag commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedTechnology") ->
      L240.TechShrwt_tra

    L240.TechCost_tra %>%
      add_title("Technology costs for traded ag commodities") %>%
      add_units("1975$/kg") %>%
      add_comments("Exogenous cost to reflect shipping + handling of traded commodities") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedTechnology") ->
      L240.TechCost_tra

    L240.TechCoef_tra %>%
      add_title("Technology input-output coefficients for traded ag commodities") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedTechnology") ->
      L240.TechCoef_tra

    L240.Production_tra %>%
      add_title("Technology calibration for traded ag commodities") %>%
      add_units("Mt") %>%
      add_comments("Regional exports of commodities that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agTradedTechnology",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L100.FAO_For_Exp_m3",
                     "common/iso_GCAM_regID") ->
      L240.Production_tra

    L240.Supplysector_reg %>%
      add_title("Supplysector info for regional ag commodities") %>%
      add_units("None") %>%
      add_comments("These sectors are used for sharing between consumption of domestically produced crops versus imports") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalSector") ->
      L240.Supplysector_reg

    L240.SubsectorAll_reg %>%
      add_title("Subsector info for traded ag commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalSubsector") ->
      L240.SubsectorAll_reg

    L240.TechShrwt_reg %>%
      add_title("Technology share-weights for traded ag commodities") %>%
      add_units("None") %>%
      add_comments("We remove any regions for which agriculture and land use are not modeled.") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalTechnology") ->
      L240.TechShrwt_reg

    L240.TechCoef_reg %>%
      add_title("Technology input-output coefficients for regional ag commodities") %>%
      add_units("Unitless IO") %>%
      add_comments("Pass-through; 1 unless some portion is assumed lost/spoiled in shipping") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalTechnology") ->
      L240.TechCoef_reg

    L240.Production_reg_imp %>%
      add_title("Technology calibration for regional ag commodities: imports") %>%
      add_units("Mt") %>%
      add_comments("Consumption of commodities that are traded between GCAM regions") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalTechnology",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L240.Production_reg_imp

    L240.Production_reg_dom %>%
      add_title("Technology calibration for regional ag commodities: consumption of domestic production") %>%
      add_units("Mt") %>%
      add_comments("Consumption of commodities produced within-region") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agRegionalTechnology",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y",
                     "L110.For_ALL_bm3_R_Y",
                     "L100.FAO_For_Exp_m3",
                     "common/iso_GCAM_regID") ->
      L240.Production_reg_dom

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
