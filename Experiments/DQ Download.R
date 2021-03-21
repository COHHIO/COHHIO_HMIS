# scratch
high_priority <- dq_main %>%
  filter(Type == "High Priority") %>%
  select(ProjectName,
         PersonalID,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         UserCreating,
         Type,
         Issue, 
         Guidance)

errors <- dq_main %>%
  filter(Type == "Error") %>%
  select(ProjectName,
         PersonalID,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         UserCreating,
         Type,
         Issue, 
         Guidance)

warnings <- dq_main %>%
  filter(Type == "Warning") %>%
  select(ProjectName,
         PersonalID,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         UserCreating,
         Type,
         Issue, 
         Guidance) %>%
  mutate(PreviousProject = "N/A")



warnings <- rbind(warnings, dq_overlaps %>%
                    select(ProjectName,
                           PersonalID,
                           EntryDate,
                           MoveInDateAdjust,
                           ExitDate,
                           UserCreating,
                           Type,
                           Issue, 
                           Guidance,
                           PreviousProject))






write_xlsx(
  x = list(
    "High Priority" = high_priority,
    "Errors" = errors,
    "Warnings" = warnings
  ),
  "random_data/dq_data.xlsx"
)
           
           
           