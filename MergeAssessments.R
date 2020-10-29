# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
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

library(tidyverse)
library(readxl)
library(writexl)

# Demo -------------------------------------------------------------------

raw_assessments_demo <- read_xlsx("random_data/demo_assessments.xlsx",
                                sheet = 1)

raw_questions_demo <- read_xlsx("random_data/demo_assessments.xlsx",
                              sheet = 2)

raw_subs_demo <- read_xlsx("random_data/demo_assessments.xlsx",
                         sheet = 3)
raw_sub_questions_demo <- read_xlsx("random_data/demo_assessments.xlsx",
                                  sheet = 4)

raw_assessment_question_link_demo <-
  read_xlsx("random_data/demo_assessments.xlsx",
            sheet = 5)

raw_assessment_sub_question_link_demo <-
  read_xlsx("random_data/demo_assessments.xlsx",
            sheet = 6)

raw_picklists_demo <- read_xlsx("random_data/demo_assessments.xlsx",
                              sheet = 7)

# Balance of State --------------------------------------------------------

raw_assessments_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                                 sheet = 1)

raw_questions_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                               sheet = 2)

raw_subs_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                          sheet = 3)
raw_sub_questions_bos <-
  read_xlsx("random_data/bos_assessments.xlsx",
            sheet = 4)

raw_assessment_question_link_bos <-
  read_xlsx("random_data/bos_assessments.xlsx",
            sheet = 5)

raw_assessment_sub_question_link_bos <-
  read_xlsx("random_data/bos_assessments.xlsx",
            sheet = 6)

raw_picklists_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                               sheet = 7)

raw_picklists_yo <- read_xlsx("random_data/yo_assessments.xlsx",
                               sheet = 7)
# Question Differences ----------------------------------------------------

questionsysnames_not_on_bos <-
  data.frame(
    "QuestionComputerName" = setdiff(
      raw_questions_demo$QuestionComputerName,
      raw_questions_bos$QuestionComputerName
    )
  ) %>%
  left_join(raw_questions_demo, by = "QuestionComputerName")

# Picklist Differences ----------------------------------------------------

picklist_names_not_on_bos <-
  data.frame("PicklistName" = setdiff(
    raw_picklists_demo$PicklistName,
    raw_picklists_bos$PicklistName
  )) %>%
  left_join(raw_picklists_demo, by = "PicklistName")

picklist_names_y_yo_n_demo <-
  data.frame("PicklistName" = setdiff(
    raw_picklists_yo$PicklistName,
    raw_picklists_demo$PicklistName
  )) %>%
  left_join(raw_picklists_yo, by = "PicklistName")

picklist_values_y_yo_n_demo <- anti_join(
  raw_picklists_yo[, c(1, 5, 7)],
  raw_picklists_demo[, c(1, 5, 7)]
)

# Assessment Differences --------------------------------------------------

assessment_names_not_on_bos <-
  setdiff(
    raw_assessments_demo$AssessmentComputerName,
    raw_assessments_bos$AssessmentComputerName
  )

assessment_names_not_on_bos <-
  data.frame("AssessmentComputerName" = assessment_names_not_on_bos) %>%
  left_join(raw_assessments_demo, by = "AssessmentComputerName")

# Subassessments ----------------------------------------------------------
subs_not_on_bos <- data.frame("SubComputerName" = setdiff(raw_subs_demo$SubComputerName,
                                                          raw_subs_bos$SubComputerName))

subassessments_not_on_bos <-  subs_not_on_bos %>%
  left_join(raw_subs_demo, by = "SubComputerName")


# Sub Questions -----------------------------------------------------------

sub_qs_not_on_bos <- data.frame(
  "SubQuestionComputerName" = setdiff(
    raw_sub_questions_demo$SubQuestionComputerName,
    raw_sub_questions_bos$SubQuestionComputerName
  )
)

subassessment_qs_not_on_bos <-  sub_qs_not_on_bos %>%
  left_join(raw_sub_questions_demo, by = "SubQuestionComputerName")

# To Be Discussed ---------------------------------------------------------

datasets_to_examine <- list(
  "assessments" = assessment_names_not_on_bos,   
  "questions" = questionsysnames_not_on_bos,
  "subs" = subassessments_not_on_bos,
  "subqs" = subassessment_qs_not_on_bos,
  "picklists" = picklist_names_not_on_bos,
  "picklistvalues" = picklist_values_y_yo_n_demo
)

write_xlsx(datasets_to_examine, path = "DemoDifferences.xlsx")


