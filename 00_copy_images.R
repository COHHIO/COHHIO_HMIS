# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

e <- environment()
purrr::walk(list.files("images", pattern = ".RData", full.names = TRUE), ~{
  load(.x, envir = e)
})

## to Rm:

 
.Rm <- list(
  APs = APs,
  bos_counties = bos_counties,
  Client = Client,
  covid19 = covid19,
  covid19_priority_plot = covid19_priority_plot,
  covid19_status_plot = covid19_status_plot,
  current_tay_hohs = current_tay_hohs,
  # FileEnd = FileEnd,
  goals = goals,
  hc_began_collecting_covid_data = hc_began_collecting_covid_data,        
  hc_check_dq_back_to = hc_check_dq_back_to,                   
  hc_data_goes_back_to = hc_data_goes_back_to,    
  hc_project_eval_start = hc_project_eval_start,
  hc_project_eval_end = hc_project_eval_end,
  hc_psh_started_collecting_move_in_date = hc_psh_started_collecting_move_in_date,
  meta_HUDCSV_Export_Date = meta_HUDCSV_Export_Date,               
  Mah_PIT = Mah_PIT,
  meta_HUDCSV_Export_End = meta_HUDCSV_Export_End,                
  meta_HUDCSV_Export_Start = meta_HUDCSV_Export_Start,              
  meta_Rmisc_last_run_date = meta_Rmisc_last_run_date,  
  note_bed_utilization = note_bed_utilization,
  note_calculation_utilization = note_calculation_utilization,
  note_qpr_dq_community_need = note_qpr_dq_community_need,
  note_qpr_housed_county = note_qpr_housed_county,
  note_qpr_served_county = note_qpr_served_county,
  note_unit_utilization = note_unit_utilization,
  Organization = Organization,
  pe_validation_summary = pe_validation_summary,
  project_type = project_type,
  qpr_benefits = qpr_benefits,
  qpr_income = qpr_income,
  qpr_leavers = qpr_leavers,
  qpr_rrh_enterers = qpr_rrh_enterers,
  qpr_spdats_county = qpr_spdats_county,
  qpr_spdats_project = qpr_spdats_project,
  qpr_spending = qpr_spending,
  regions = regions,
  ReportEnd = ReportEnd,
  ReportStart = ReportStart,
  Scores = Scores,
  Services = Services,
  spm_1b_loth_self_report = spm_1b_loth_self_report,
  spm_2_recurrence = spm_2_recurrence,
  spm_7b1_exits_lh = spm_7b1_exits_lh,
  spm_7b2_exits_ph = spm_7b2_exits_ph,
  spm_current_end_date = spm_current_end_date,
  spm_current_start_date = spm_current_start_date,
  spm_prior_end_date = spm_prior_end_date,
  spm_prior_start_date = spm_prior_start_date,
  summary_pe_final_scoring = summary_pe_final_scoring,
  # update_date = update_date,
  Users = Users,
  utilization = utilization,
  utilization_bed = utilization_bed,
  utilization_unit = utilization_unit,
  validation = validation,
  veteran_current_in_project = veteran_current_in_project
)

.Rme <- list(
  active_list = active_list,
  aps_no_referrals = aps_no_referrals,
  Beds = Beds,
  calc_2_yrs_prior_end = calc_2_yrs_prior_end,                  
  calc_2_yrs_prior_range = calc_2_yrs_prior_range,                
  calc_2_yrs_prior_start = calc_2_yrs_prior_start,                
  calc_data_goes_back_to = calc_data_goes_back_to,                
  calc_full_date_range = calc_full_date_range,                  
  Client = Client,
  dq_main = dq_main,
  dq_past_year = dq_past_year,
  dq_unsheltered = dq_unsheltered,
  data_APs = data_APs,
  dq_overlaps = dq_overlaps,
  detail_eligibility = detail_eligibility,
  dq_plot_eligibility = dq_plot_eligibility,
  dq_plot_errors = dq_plot_errors,
  dq_plot_hh_errors = dq_plot_hh_errors,
  dq_plot_hh_no_spdat = dq_plot_hh_no_spdat,
  dq_plot_outstanding_referrals = dq_plot_outstanding_referrals,
  dq_plot_projects_errors = dq_plot_projects_errors,
  dq_plot_projects_warnings = dq_plot_projects_warnings,
  dq_plot_unsheltered_high = dq_plot_unsheltered_high,
  dq_plot_warnings = dq_plot_warnings,
  dq_providers = dq_providers,
  # FileActualStart = FileActualStart,
  # FileEnd = FileEnd,
  # FileStart = FileStart,
  hc_began_collecting_covid_data = hc_began_collecting_covid_data,        
  hc_check_dq_back_to = hc_check_dq_back_to,                   
  hc_data_goes_back_to = hc_data_goes_back_to,  
  hc_project_eval_start = hc_project_eval_start,
  hc_project_eval_end = hc_project_eval_end,
  hc_psh_started_collecting_move_in_date = hc_psh_started_collecting_move_in_date,
  HUD_specs = HUD_specs,
  living_situation = living_situation,
  meta_HUDCSV_Export_Date = meta_HUDCSV_Export_Date,               
  meta_HUDCSV_Export_End = meta_HUDCSV_Export_End,                
  meta_HUDCSV_Export_Start = meta_HUDCSV_Export_Start,              
  meta_Rmisc_last_run_date = meta_Rmisc_last_run_date,  
  Organization = Organization,
  pe_increase_income = pe_increase_income,
  pe_exits_to_ph = pe_exits_to_ph,
  pe_homeless_history_index = pe_homeless_history_index,
  pe_length_of_stay = pe_length_of_stay,
  pe_benefits_at_exit = pe_benefits_at_exit,
  pe_entries_no_income = pe_entries_no_income,
  pe_long_term_homeless = pe_long_term_homeless,
  pe_res_prior = pe_res_prior,
  pe_own_housing = pe_own_housing,
  pe_validation_summary = pe_validation_summary,
  pe_scored_at_ph_entry = pe_scored_at_ph_entry,
  qpr_income = qpr_income,
  qpr_benefits = qpr_benefits,
  qpr_leavers = qpr_leavers,
  qpr_rrh_enterers = qpr_rrh_enterers,
  qpr_spending = qpr_spending,
  qpr_spdats_project = qpr_spdats_project,
  qpr_spdats_county = qpr_spdats_county,
  ReportEnd = ReportEnd,
  ReportStart = ReportStart,
  Referrals = Referrals,
  regions = regions,
  responsible_providers = responsible_providers,
  Scores = Scores,
  summary_pe_final_scoring = summary_pe_final_scoring,
  # update_date = update_date,
  unsheltered_by_month = unsheltered_by_month,
  unsh_overlaps = unsh_overlaps,
  Users = Users,
  utilizers_clients = utilizers_clients,
  utilization = utilization,
  utilization_bed = utilization_bed,
  validation = validation,
  veteran_active_list = veteran_active_list,  
   
)
directories <- c("../Rminor",
                 "../Rminor_elevated")


#' @title Send data to the respective app direcotry
#' @description Takes a list of data objects to store that the app will use. Saves `data.frame`s to `feather` files in the `data/db` directory and all other objects to an `RData` image in the `data/` directory.
#' @param objects \code{(list)} with all data objects to be stored for the app's operation
#' @param dir \code{(character)} file path to the application directory
#' @importFrom dplyr `%>%`
data_prep <- function (objects, dir) {
  # data directory
  .dir <- file.path(dir, "data")
  # db directory inside data directory
  .db <- file.path(.dir, "db")
  # make if not created
  purrr::walk(c(.dir, .db), ~{
    if (!dir.exists(.x)) {
      dir.create(.x)
    }
  })
  # Create an accessor fn
  .fn <- function(x = as.character(match.call()[[1]]), path = "data/db", ext = ".feather") feather::read_feather(file.path(path, paste0(x, ifelse(grepl("^\\.",ext), ext, paste0(".",ext)))))
  .is_df <- purrr::map_lgl(objects, is.data.frame)
  objects[.is_df] <- objects[.is_df] %>% 
    # Write the feather files
    purrr::imap(~ {
      feather::write_feather(.x, file.path(.db, paste0(.y,".feather")))
      .x
    }) %>% 
    # overwrite the DFs with an accessor function.
    # This reads the feather file with the same name as the function
    purrr::map(~.fn)  
  objects$df_nms <- names(objects)[.is_df]
  # Save the results
  save(
    list = names(objects),
    compress = "xz",
    envir = list2env(objects),
    file = file.path(.dir, paste0(basename(dir), ".Rdata"))
  )
}



data_prep(.Rm, directories[1])
data_prep(.Rme, directories[2])