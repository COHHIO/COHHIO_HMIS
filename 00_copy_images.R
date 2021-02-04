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
.rm <- "Rminor"
.rme <- "Rminor_elevated"

.files <- c(fs::path("..", .rm, "data", .rm , ext = "RData")
          , fs::path("..", .rme, "data", .rme , ext = "RData")
)

purrr::walk(.files, ~{
  .d <- dirname(.x)
  if (!dir.exists(.d)) {
    dir.create(.d)
  }
  if (file.exists(.x)) file.remove(.x)
})
e <- environment()
purrr::walk(list.files("images", pattern = ".RData", full.names = TRUE), ~{
  load(.x, envir = e)
})

## to Rm:

save(
  APs,
  bos_counties,
  BoS_PIT,
  calc_2_yrs_prior_end,                  
  calc_2_yrs_prior_range,                
  calc_2_yrs_prior_start,                
  calc_data_goes_back_to,                
  calc_full_date_range,                  
  Client,
  covid19,
  covid19_priority_plot,
  covid19_status_plot,
  current_tay_hohs,
  # FileEnd,
  goals,
  hc_began_collecting_covid_data,        
  hc_check_dq_back_to,                   
  hc_data_goes_back_to,    
  hc_project_eval_start,
  hc_project_eval_end,
  hc_psh_started_collecting_move_in_date,
  Mah_PIT,
  meta_HUDCSV_Export_Date,               
  meta_HUDCSV_Export_End,                
  meta_HUDCSV_Export_Start,              
  meta_Rmisc_last_run_date,  
  note_bed_utilization,
  note_calculation_utilization,
  note_qpr_dq_community_need,
  note_qpr_housed_county,
  note_qpr_served_county,
  note_unit_utilization,
  Organization,
  pe_validation_summary,
  project_type,
  qpr_benefits,
  qpr_income,
  qpr_leavers,
  qpr_rrh_enterers,
  qpr_spdats_county,
  qpr_spdats_project,
  qpr_spending,
  regions,
  ReportEnd,
  ReportStart,
  Scores,
  Services,
  spm_Metric_1b,
  spm_Metric_2,
  spm_Metric_7,
  spm_current_end_date,
  spm_current_start_date,
  spm_prior_end_date,
  spm_prior_start_date,
  summary_pe_final_scoring,
  # update_date,
  Users,
  utilization,
  utilization_bed,
  utilization_unit,
  validation,
  veteran_current_in_project,
  compress = "xz",
  envir = e,
  file = .files[1]
)

## to Rme:

save(
  active_list,
  aps_no_referrals,
  Beds,
  calc_2_yrs_prior_end,                  
  calc_2_yrs_prior_range,                
  calc_2_yrs_prior_start,                
  calc_data_goes_back_to,                
  calc_full_date_range,                  
  Client,
  dq_main,
  dq_past_year,
  dq_unsheltered,
  data_APs,
  dq_overlaps,
  detail_eligibility,
  dq_plot_eligibility,
  dq_plot_errors,
  dq_plot_hh_errors,
  dq_plot_hh_no_spdat,
  dq_plot_outstanding_referrals,
  dq_plot_projects_errors,
  dq_plot_projects_warnings,
  dq_plot_unsheltered_high,
  dq_plot_warnings,
  dq_providers,
  # FileActualStart,
  # FileEnd,
  # FileStart,
  hc_began_collecting_covid_data,        
  hc_check_dq_back_to,                   
  hc_data_goes_back_to,  
  hc_project_eval_start,
  hc_project_eval_end,
  hc_psh_started_collecting_move_in_date,
  HUD_specs,
  living_situation,
  meta_HUDCSV_Export_Date,               
  meta_HUDCSV_Export_End,                
  meta_HUDCSV_Export_Start,              
  meta_Rmisc_last_run_date,    
  Organization,
  pe_increase_income,
  pe_exits_to_ph,
  pe_homeless_history_index,
  pe_length_of_stay,
  pe_benefits_at_exit,
  pe_entries_no_income,
  pe_long_term_homeless,
  pe_res_prior,
  pe_own_housing,
  pe_validation_summary,
  pe_scored_at_ph_entry,
  qpr_income,
  qpr_benefits,
  qpr_leavers,
  qpr_rrh_enterers,
  qpr_spending,
  qpr_spdats_project,
  qpr_spdats_county,
  ReportEnd,
  ReportStart,
  Referrals,
  regions,
  responsible_providers,
  Scores,
  summary_pe_final_scoring,
  # update_date,
  unsheltered_by_month,
  unsh_overlaps,
  Users,
  utilizers_clients,
  utilization,
  utilization_bed,
  validation,
  veteran_active_list,  
  compress = "xz",
  envir = e,
  file = .files[2]
)

