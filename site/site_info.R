#!/usr/bin/env Rscript

#' Site-specific information for data request execution.
#'
#' Please edit information as noted below.
#'
#' The information in this file describes site-specific practices for managing
#' data requests in general, such as connection information and organiation of
#' databases, and defaults for output handling.  These settings are not
#' typically request-specific, and will likely remain unchanged across multiple
#' requests targeting the same version of data.  As a result, you can often copy
#' this file from one request to another, or point multiple requests to a single
#' site_info.R file.
#'
#' @md
"site_info.R"

#' Your site's name.
#' For technical reasons, it should be lowercase.
#' @md
config('qry_site', 'site_name')

#' Code to establish a database connection at your site.
#'
#' The connection must be able to reach CDM data, the vocabularies,
#' and any result schemata needed.  The connection may be either a
#' dplyr-style src_foo() object or a DBI-style dbConnect()
#' object.
#'
#' A few notes:
#'
#' * You may find it convenient to use the
#'   (srcr)[https://cran.r-project.org/web/packages/srcr/index.html] package
#'   to abstract database connection information such as credentials and server
#'   names out of this file.
#' * If using Oracle, the following are required before loading ROracle, if
#'   these are not set in the global environment:
#'     * `Sys.setenv(TZ=Sys.timezone())`
#'     * `Sys.setenv(ORA_SDTZ=Sys.timezone())`
#'
#' @md
config('db_src', {
  require(srcr);
  default <- Sys.getenv('PEDSNET_DB_SRC_CONFIG_BASE', unset = NA)
  if (is.na(default) || nchar(default) == 0) default <- 'argos_pedsnet_current'
  srcr(default)
})

#' Name of the schema, if any, to be prepended to CDM fact table names.
#'
#' @details
#' If `NA`, no schema qualifier is added.
#' @md
# config('cdm_schema', paste0('ssdqa_', config('cohort')))
config('cdm_schema', 'weiss_jspa')

#' Name of the schema, if any, to be prepended to vocabulary tables.
#'
#' @details
#' If `NA`, no schema qualifier is added.
#' @md
config('vocabulary_schema', 'vocabulary')

#' Name of the schema in which to create intermediate and results tables
#'
#' This value determines whether a schema name is added to names of
#' tables holding intermediate or final results.  If it is `NA`, no
#' explicit schema is used, and tables are created wherever the DBMS
#' places them.  It can be overridden by the request-specific setting in `run.R`.
#' @md
config('default_results_schema', NA)

#' Whether or not to keep intermediate tables
#'
#' This Boolean value determines whether tables holding codesets
#' or intermediate steps are retained after execution completes.  If
#' `FALSE`, they are created as temporary tables.  It can be overridden by the
#' request-specific setting in `run.R`.
#' @md
config('default_retain_intermediates', FALSE)

#' Names of standard tables used in queries.
#'
#' This list defines a simple mapping between the names that will be used in the
#' code for the request (the left-hand side of each assignment) and the actual
#' names of the tables in the database.  It is intended to allow for different
#' alphabetic casing, use of version-specific names, etc.
#'
#' Please edit only the right-hand side of each assignment.
#' Table names on the left must be lower-case; those on the right
#' must reflect naming conventions in the database.
#' @md
# config('table_names',
#        list(adt_occurrence = 'cdm_adt_occurrence_stud_1279',
#             care_site = 'cdm_care_site_stud_1279',
#             care_site_spec = 'cdm_care_site_spec_stud_1279',
#             condition_era = 'condition_era',
#             condition_occurrence = 'cdm_condition_occurrence_stud_1279',
#             cohort_matched = 'cohort_matched_stud_1279',
#             cohort_pcc = 'cohort_pcc_stud_1279',
#             cohort_glom = 'cohort_glom_stud_1279',
#             death = 'cdm_death_stud_1279',
#             device_exposure = 'device_exposure',
#             dose_era = 'dose_era',
#             drug_era = 'drug_era',
#             drug_exposure = 'cdm_drug_exposure_stud_1279',
#             fact_relationship = 'fact_relationship',
#             immunization = 'cdm_immunization_stud_1279',
#             location = 'cdm_location_stud_1279',
#             measurement = 'measurement',
#             measurement_anthro = 'cdm_measurement_anthro_stud_1279',
#             measurement_labs = 'cdm_measurement_labs_stud_1279',
#             measurement_vitals = 'measurement',
#             measurement_organism = 'measurement_organism',
#             observation = 'observation',
#             observation_period = 'observation_period',
#             person = 'cdm_person_stud_1279',
#             procedure_occurrence = 'cdm_procedure_occurrence_stud_1279',
#             provider = 'cdm_provider_stud_1279',
#             provider_spec = 'cdm_provider_spec_stud_1279',
#             visit_occurrence = 'cdm_visit_occurrence_stud_1279',
#             visit_payer = 'cdm_visit_payer_stud_1279',
#             concept = 'concept',
#             concept_ancestor = 'concept_ancestor',
#             concept_relationship = 'concept_relationship'))

config('table_names',
       list(adt_occurrence = 'dcc_adt_occurrence_stud_1340',
            care_site = 'dcc_care_site_stud_1340',
            care_site_spec = 'dcc_care_site_spec_stud_1340',
            condition_era = 'condition_era',
            condition_occurrence = 'dcc_condition_occurrence_stud_1340',
            cohort_matched = 'cohort_matched_stud_1340',
            cohort_pcc = 'cohort_pcc_stud_1340',
            cohort_glom = 'cohort_glom_stud_1340',
            death = 'dcc_death_stud_1340',
            device_exposure = 'device_exposure',
            dose_era = 'dose_era',
            drug_era = 'drug_era',
            drug_exposure = 'dcc_drug_exposure_stud_1340',
            fact_relationship = 'fact_relationship',
            immunization = 'dcc_immunization_stud_1340',
            jia_pats = 'jia_pats_stud_1340',
            location = 'dcc_location_stud_1340',
            measurement = 'measurement',
            measurement_anthro = 'dcc_measurement_anthro_stud_1340',
            measurement_labs = 'dcc_measurement_labs_stud_1340',
            measurement_vitals = 'measurement',
            measurement_organism = 'measurement_organism',
            observation = 'observation',
            observation_period = 'observation_period',
            person = 'dcc_person_stud_1340',
            procedure_occurrence = 'dcc_procedure_occurrence_stud_1340',
            provider = 'dcc_provider_stud_1340',
            provider_spec = 'dcc_provider_spec_stud_1340',
            visit_occurrence = 'dcc_visit_occurrence_stud_1340',
            visit_payer = 'dcc_visit_payer_stud_1340',
            concept = 'concept',
            concept_ancestor = 'concept_ancestor',
            concept_relationship = 'concept_relationship'))


#> ##################### End of site-specific configuration
