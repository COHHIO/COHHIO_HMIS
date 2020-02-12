


vispdats_entering_ph <- dq_2019 %>%
  right_join(pe_coc_funded, by = c("ProjectID", "ProjectType", "ProjectName")) %>%
  filter(Issue == "Non-Veteran Non-DV HoHs Entering PH without SPDAT") %>%
  select(ProjectName, PersonalID) %>% group_by(ProjectName) %>% summarise(n())


# dq_flags (11/93) have flags



