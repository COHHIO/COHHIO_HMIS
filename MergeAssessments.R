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

# Youngstown --------------------------------------------------------------

assessment_names_yo <- read_xlsx("random_data/yo_assessments.xlsx",
                                 sheet = 1)

questions_yo <- read_xlsx("random_data/yo_assessments.xlsx",
                          sheet = 2)

subs_yo <- read_xlsx("random_data/yo_assessments.xlsx",
                     sheet = 3)
sub_questions_yo <- read_xlsx("random_data/yo_assessments.xlsx",
                              sheet = 4)

assessment_question_link_yo <- read_xlsx("random_data/yo_assessments.xlsx",
                                         sheet = 5)

assessment_sub_question_link_yo <- read_xlsx("random_data/yo_assessments.xlsx",
                                             sheet = 6)

picklists_yo <- read_xlsx("random_data/yo_assessments.xlsx",
                          sheet = 7)

# Balance of State --------------------------------------------------------

assessment_names_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                                 sheet = 1)

questions_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                          sheet = 2)

subs_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                     sheet = 3)
sub_questions_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                              sheet = 4)

assessment_question_link_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                                         sheet = 5)

assessment_sub_question_link_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                                             sheet = 6)

picklists_bos <- read_xlsx("random_data/bos_assessments.xlsx",
                          sheet = 7)

# Question Differences ----------------------------------------------------

question_names_bos <- questions_bos$QuestionName 
  
question_names_yo <- questions_yo$QuestionName

questions_not_on_yo <- setdiff(question_names_bos, question_names_yo)

questions_not_on_bos <- setdiff(question_names_yo, question_names_bos)

questionsysnames_not_on_yo <- setdiff(questions_bos$QuestionComputerName, questions_yo$QuestionComputerName)

questionsysnames_not_on_bos <- setdiff(questions_yo$QuestionComputerName, questions_bos$QuestionComputerName)

same_questions <- intersect(questions_bos, questions_yo)






