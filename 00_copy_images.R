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

save(
  APs,
  bos_counties,
  Client,
  covid19,
  covid19_priority_plot,
  covid19_status_plot,
  current_tay_hohs,
  Exit,
  FileEnd,
  goals,
  note_bed_utilization,
  note_calculation_utilization,
  note_qpr_dq_community_need,
  note_qpr_housed_county,
  note_qpr_served_county,
  note_unit_utilization,
  Organization,
  pe_validation_summary,
  Project,
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
  spm_1b_loth_self_report,
  spm_2_recurrence,
  spm_7b1_exits_lh,
  spm_7b2_exits_ph,
  spm_current_end_date,
  spm_current_start_date,
  spm_prior_end_date,
  spm_prior_start_date,
  summary,
  summary_pe_final_scoring,
  tay,
  update_date,
  Users,
  utilization,
  utilization_bed,
  utilization_unit,
  validation,
  veteran_current_in_project,
  compress = FALSE,
  envir = e,
  file = .files[1]
)
save(
  active_list,
  Enrollment,
  Exit,
  Client,
  Referrals,
  Users,
  Project,
  Scores,
  HUD_specs,
  entered_between,
  exited_between,
  FileActualStart,
  FileEnd,
  FileStart,
  living_situation,
  Organization,
  regions,
  served_between,
  stayed_between,
  update_date,
  tay,
  summary,
  ReportEnd,
  ReportStart,
  dq_main,
  dq_past_year,
  dq_unsheltered,
  unsheltered_by_month,
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
  aps_no_referrals,
  data_APs,
  unsh_overlaps,
  pe_increase_income,
  pe_exits_to_ph,
  pe_homeless_history_index,
  pe_length_of_stay,
  pe_benefits_at_exit,
  pe_entries_no_income,
  pe_long_term_homeless,
  pe_res_prior,
  pe_scored_at_ph_entry,
  summary_pe_final_scoring,
  pe_own_housing,
  pe_validation_summary,
  qpr_leavers,
  validation,
  qpr_income,
  qpr_benefits,
  qpr_rrh_enterers,
  qpr_spending,
  qpr_spdats_project,
  qpr_spdats_county,
  utilizers_clients,
  utilization,
  utilization_bed,
  compress = FALSE,
  envir = e,
  file = .files[2]
)

