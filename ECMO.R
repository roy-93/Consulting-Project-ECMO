############# Dataset ###############
#####################################

data_ecmo <- read.csv(file = "NorwoodECMO Data.csv", header = TRUE)
str(data_ecmo)
summary(data_ecmo)

############## Changing the variable label ###########
#####################################################

data_ecmo$ID <- as.character(data_ecmo$ID)
data_ecmo$Sex <- as.factor(data_ecmo$Sex)
data_ecmo$OpDate <- as.Date(data_ecmo$OpDate)
data_ecmo$DOD <- as.Date(data_ecmo$DOD)
data_ecmo$Dx <- as.factor(data_ecmo$Dx)
data_ecmo$SurgShuntT <- as.factor(data_ecmo$SurgShuntT)  
data_ecmo$ECA <- as.factor(data_ecmo$ECA)
data_ecmo$Hybrid <- as.factor(data_ecmo$Hybrid)
data_ecmo$MechVent <- as.factor(data_ecmo$MechVent)
data_ecmo$TR <- as.factor(data_ecmo$TR)
data_ecmo$VentDys <- as.factor(data_ecmo$VentDys)
data_ecmo$ECMO48 <- as.factor(data_ecmo$ECMO48)
data_ecmo$ECMO <- as.factor(data_ecmo$ECMO)
data_ecmo$SurvECMO <- as.factor(data_ecmo$SurvECMO)
data_ecmo$SurvSt2 <- as.factor(data_ecmo$SurvSt2)


#################### Descriptive Statistics #############
#####################################################
#library(devtools)
#install.packages("gtsummary")
#remotes::install_github("ddsjoberg/gtsummary")
library("gtsummary")


data_ecmo %>% select("Age", "Wgt", "Lgt", "SurgAoCT", "SurgAAoD", "SurgCircArrTot",
                   "SurgCPB", "SurgShuntS", "Cre", "ECMO48", "Sex", "Dx", "SurgShuntT" ,"ECA", 
                   "Hybrid","MechVent", "TR", "VentDys" ) %>% 
  
  tbl_summary(by = ECMO48, missing = "ifany",
              label = list(Wgt ~ "Weight",
                           Lgt ~ "Length",
                           SurgAoCT ~ "Aortic Clamp time",
                           SurgAAoD ~ "Aorta Diameter",
                           SurgCircArrTot ~ "Circulatory Arrest time",
                           SurgCPB ~ "Bypass time",
                           SurgShuntS ~ "Shunt Size",
                           Cre ~ "Pre-op creatinine",
                           Dx ~ "HLHS type",
                           SurgShuntT ~ "Shunt type",
                           ECA ~ "Extra cardiac anatomy",
                           MechVent ~ "Mechanically Ventilated",
                           TR ~ "Tricuspid rerug",
                           VentDys ~ "Ventricular dysfunction"),
              type = list(Age ~ "continuous2",
                          Wgt ~ "continuous2",
                          Lgt ~ "continuous2",
                          SurgAoCT ~ "continuous2",
                          SurgAAoD ~ "continuous2",
                          SurgCircArrTot ~ "continuous2",
                          SurgCPB ~ "continuous2",
                          SurgShuntS ~ "continuous2",
                          Cre ~ "continuous2",
                          Sex ~ "categorical",
                          Dx ~ "categorical",
                          SurgShuntT ~ "categorical",
                          ECA ~ "categorical",
                          Hybrid ~ "categorical",
                          MechVent ~ "categorical",
                          TR ~ "categorical",
                          VentDys ~ "categorical") ,
              statistic = list(all_continuous() ~ c("{N_nonmiss}", "{mean} ({sd})", "{min}, {max}"), 
                               all_categorical() ~ "{n} ({p}%)")) %>%
  add_p(test = list(Age ~ "t.test",
                    Wgt ~ "t.test",
                    Lgt ~ "t.test",
                    SurgAoCT ~ "wilcox.test",
                    SurgAAoD ~ "t.test",
                    SurgCircArrTot ~ "wilcox.test",
                    SurgCPB ~ "wilcox.test",
                    SurgShuntS ~ "t.test",
                    Cre ~ "t.test",
                    Sex ~ "fisher.test",
                    Dx ~ "fisher.test",
                    SurgShuntT ~ "chisq.test.no.correct",
                    ECA ~ "chisq.test.no.correct",
                    Hybrid ~ "fisher.test",
                    MechVent ~ "chisq.test.no.correct",
                    TR ~ "fisher.test",
                    VentDys ~ "fisher.test"),
        pvalue_fun = ~style_pvalue(., digits = 2)) %>% add_n() %>% 
  add_stat_label() %>%
  bold_labels() %>%
  modify_header(list(label ~ "**Variable**", all_stat_cols() ~ "**{level}**")) %>%
  modify_spanning_header(all_stat_cols() ~ "**ECMO within 48 hours**") %>%
  as_gt() %>%
  gt::tab_header(
    title = gt::md("**Table 1:Summary Statistics Stratified by response(ECMO48)**")
  ) %>%
  gt::tab_source_note("Data updated June 22, 2023")

# Summary Stat stratified by output (ECMO)

data_ecmo %>% select("Age", "Wgt", "Lgt", "SurgAoCT", "SurgAAoD", "SurgCircArrTot",
                     "SurgCPB", "SurgShuntS", "Cre", "ECMO", "Sex", "Dx", "SurgShuntT" ,"ECA", 
                     "Hybrid","MechVent", "TR", "VentDys" ) %>% 
  
  tbl_summary(by = ECMO, missing = "ifany",
              label = list(Wgt ~ "Weight",
                           Lgt ~ "Length",
                           SurgAoCT ~ "Aortic Clamp time",
                           SurgAAoD ~ "Aorta Diameter",
                           SurgCircArrTot ~ "Circulatory Arrest time",
                           SurgCPB ~ "Bypass time",
                           SurgShuntS ~ "Shunt Size",
                           Cre ~ "Pre-op creatinine",
                           Dx ~ "HLHS type",
                           SurgShuntT ~ "Shunt type",
                           ECA ~ "Extra cardiac anatomy",
                           MechVent ~ "Mechanically Ventilated",
                           TR ~ "Tricuspid rerug",
                           VentDys ~ "Ventricular dysfunction"),
              type = list(Age ~ "continuous2",
                          Wgt ~ "continuous2",
                          Lgt ~ "continuous2",
                          SurgAoCT ~ "continuous2",
                          SurgAAoD ~ "continuous2",
                          SurgCircArrTot ~ "continuous2",
                          SurgCPB ~ "continuous2",
                          SurgShuntS ~ "continuous2",
                          Cre ~ "continuous2",
                          Sex ~ "categorical",
                          Dx ~ "categorical",
                          SurgShuntT ~ "categorical",
                          ECA ~ "categorical",
                          Hybrid ~ "categorical",
                          MechVent ~ "categorical",
                          TR ~ "categorical",
                          VentDys ~ "categorical") ,
              statistic = list(all_continuous() ~ c("{N_nonmiss}", "{mean} ({sd})", "{min}, {max}"), 
                               all_categorical() ~ "{n} ({p}%)")) %>%
  add_p(test = list(Age ~ "t.test",
                    Wgt ~ "t.test",
                    Lgt ~ "t.test",
                    SurgAoCT ~ "wilcox.test",
                    SurgAAoD ~ "t.test",
                    SurgCircArrTot ~ "wilcox.test",
                    SurgCPB ~ "wilcox.test",
                    SurgShuntS ~ "t.test",
                    Cre ~ "t.test",
                    Sex ~ "fisher.test",
                    Dx ~ "fisher.test",
                    SurgShuntT ~ "chisq.test.no.correct",
                    ECA ~ "chisq.test.no.correct",
                    Hybrid ~ "fisher.test",
                    MechVent ~ "chisq.test.no.correct",
                    TR ~ "fisher.test",
                    VentDys ~ "fisher.test"),
        pvalue_fun = ~style_pvalue(., digits = 2)) %>% add_n() %>% 
  add_stat_label() %>%
  bold_labels() %>%
  modify_header(list(label ~ "**Variable**", all_stat_cols() ~ "**{level}**")) %>%
  modify_spanning_header(all_stat_cols() ~ "**ECMO Post Norwood**") %>%
  as_gt() %>%
  gt::tab_header(
    title = gt::md("**Table 2:Summary Statistics stratified by response(ECMO)**")
  ) %>%
  gt::tab_source_note("Data updated June 22, 2023")



# Summary Stat stratified by output (SurvECMO)

data_ecmo %>% select("Age", "Wgt", "Lgt", "SurgAoCT", "SurgAAoD", "SurgCircArrTot",
                     "SurgCPB", "SurgShuntS", "Cre", "SurvECMO", "Sex", "Dx", "SurgShuntT" ,"ECA", 
                     "Hybrid","MechVent", "TR", "VentDys" ) %>% 
  
  tbl_summary(by = SurvECMO, missing = "ifany",
              label = list(Wgt ~ "Weight",
                           Lgt ~ "Length",
                           SurgAoCT ~ "Aortic Clamp time",
                           SurgAAoD ~ "Aorta Diameter",
                           SurgCircArrTot ~ "Circulatory Arrest time",
                           SurgCPB ~ "Bypass time",
                           SurgShuntS ~ "Shunt Size",
                           Cre ~ "Pre-op creatinine",
                           Dx ~ "HLHS type",
                           SurgShuntT ~ "Shunt type",
                           ECA ~ "Extra cardiac anatomy",
                           MechVent ~ "Mechanically Ventilated",
                           TR ~ "Tricuspid rerug",
                           VentDys ~ "Ventricular dysfunction"),
              type = list(Age ~ "continuous2",
                          Wgt ~ "continuous2",
                          Lgt ~ "continuous2",
                          SurgAoCT ~ "continuous2",
                          SurgAAoD ~ "continuous2",
                          SurgCircArrTot ~ "continuous2",
                          SurgCPB ~ "continuous2",
                          SurgShuntS ~ "continuous2",
                          Cre ~ "continuous2",
                          Sex ~ "categorical",
                          Dx ~ "categorical",
                          SurgShuntT ~ "categorical",
                          ECA ~ "categorical",
                          Hybrid ~ "categorical",
                          MechVent ~ "categorical",
                          TR ~ "categorical",
                          VentDys ~ "categorical") ,
              statistic = list(all_continuous() ~ c("{N_nonmiss}", "{mean} ({sd})", "{min}, {max}"), 
                               all_categorical() ~ "{n} ({p}%)")) %>%
  add_p(test = list(Age ~ "t.test",
                    Wgt ~ "t.test",
                    Lgt ~ "t.test",
                    SurgAoCT ~ "wilcox.test",
                    SurgAAoD ~ "t.test",
                    SurgCircArrTot ~ "wilcox.test",
                    SurgCPB ~ "wilcox.test",
                    SurgShuntS ~ "t.test",
                    Cre ~ "t.test",
                    Sex ~ "fisher.test",
                    Dx ~ "fisher.test",
                    SurgShuntT ~ "chisq.test.no.correct",
                    ECA ~ "chisq.test.no.correct",
                    Hybrid ~ "fisher.test",
                    MechVent ~ "chisq.test.no.correct",
                    TR ~ "fisher.test",
                    VentDys ~ "fisher.test"),
        pvalue_fun = ~style_pvalue(., digits = 2)) %>% add_n() %>% 
  add_stat_label() %>%
  bold_labels() %>%
  modify_header(list(label ~ "**Variable**", all_stat_cols() ~ "**{level}**")) %>%
  modify_spanning_header(all_stat_cols() ~ "**Survival To ECMO**") %>%
  as_gt() %>%
  gt::tab_header(
    title = gt::md("**Table 3:Summary Statistics stratified by response(SurvECMO)**")
  ) %>%
  gt::tab_source_note("Data updated June 22, 2023")



# Summary Stat stratified by output (SurvSt2)

data_ecmo %>% select("Age", "Wgt", "Lgt", "SurgAoCT", "SurgAAoD", "SurgCircArrTot",
                     "SurgCPB", "SurgShuntS", "Cre", "SurvSt2", "Sex", "Dx", "SurgShuntT" ,"ECA", 
                     "Hybrid","MechVent", "TR", "VentDys" ) %>% 
  
  tbl_summary(by = SurvSt2, missing = "ifany",
              label = list(Wgt ~ "Weight",
                           Lgt ~ "Length",
                           SurgAoCT ~ "Aortic Clamp time",
                           SurgAAoD ~ "Aorta Diameter",
                           SurgCircArrTot ~ "Circulatory Arrest time",
                           SurgCPB ~ "Bypass time",
                           SurgShuntS ~ "Shunt Size",
                           Cre ~ "Pre-op creatinine",
                           Dx ~ "HLHS type",
                           SurgShuntT ~ "Shunt type",
                           ECA ~ "Extra cardiac anatomy",
                           MechVent ~ "Mechanically Ventilated",
                           TR ~ "Tricuspid rerug",
                           VentDys ~ "Ventricular dysfunction"),
              type = list(Age ~ "continuous2",
                          Wgt ~ "continuous2",
                          Lgt ~ "continuous2",
                          SurgAoCT ~ "continuous2",
                          SurgAAoD ~ "continuous2",
                          SurgCircArrTot ~ "continuous2",
                          SurgCPB ~ "continuous2",
                          SurgShuntS ~ "continuous2",
                          Cre ~ "continuous2",
                          Sex ~ "categorical",
                          Dx ~ "categorical",
                          SurgShuntT ~ "categorical",
                          ECA ~ "categorical",
                          Hybrid ~ "categorical",
                          MechVent ~ "categorical",
                          TR ~ "categorical",
                          VentDys ~ "categorical") ,
              statistic = list(all_continuous() ~ c("{N_nonmiss}", "{mean} ({sd})", "{min}, {max}"), 
                               all_categorical() ~ "{n} ({p}%)")) %>%
  add_p(test = list(Age ~ "t.test",
                    Wgt ~ "t.test",
                    Lgt ~ "t.test",
                    SurgAoCT ~ "wilcox.test",
                    SurgAAoD ~ "t.test",
                    SurgCircArrTot ~ "wilcox.test",
                    SurgCPB ~ "wilcox.test",
                    SurgShuntS ~ "t.test",
                    Cre ~ "t.test",
                    Sex ~ "fisher.test",
                    Dx ~ "fisher.test",
                    SurgShuntT ~ "chisq.test.no.correct",
                    ECA ~ "chisq.test.no.correct",
                    Hybrid ~ "fisher.test",
                    MechVent ~ "chisq.test.no.correct",
                    TR ~ "fisher.test",
                    VentDys ~ "fisher.test"),
        pvalue_fun = ~style_pvalue(., digits = 2)) %>% add_n() %>% 
  add_stat_label() %>%
  bold_labels() %>%
  modify_header(list(label ~ "**Variable**", all_stat_cols() ~ "**{level}**")) %>%
  modify_spanning_header(all_stat_cols() ~ "**Transplant Free Survival**") %>%
  as_gt() %>%
  gt::tab_header(
    title = gt::md("**Table 4:Summary Statistics stratified by response(SurvSt2)**")
  ) %>%
  gt::tab_source_note("Data updated June 22, 2023")





















# library("devtools")
# #install_github("emwozniak/Table1", force = TRUE)
# library("Table1")
# 
# ### Summary Statistics by ECMO48 ###
# ####################################
# 
# make.table(dat          = data_ecmo,
#            strat        = "ECMO48",
#            colnames     = c("Variable Name ", "No", "Yes", "Overall", "P-value"),
#            cat.varlist  = c("Sex", "Dx", "SurgShuntT" ,"ECA", "Hybrid","MechVent", "TR", "VentDys"),
#            cat.rmstat = list(c("row"), c("col")),
#            cat.ptype    = c("fisher", "chisq"),
#            cont.varlist = c("Age", "Wgt", "Lgt", "SurgAoCT", "SurgAAoD", "SurgCircArrTot",
#                             "SurgCPB", "SurgShuntS", "Cre", "DTE"),
#            cont.rmstat  = list(c("mediqr", "q1q3")),
#            cont.ptype   = c("ttest"), 
#            caption = "Table1: Summary Statistics by ECMO48",output = "plain")
# 
# 
# ### Summary Statistics by ECMO ###
# ##################################
# 
# make.table(dat          = data_ecmo,
#            strat        = "ECMO",
#            colnames     = c("Variable Name ", "No", "Yes", "Overall", "P-value"),
#            cat.varlist  = c("Sex", "Dx", "SurgShuntT" ,"ECA", "Hybrid","MechVent", "TR", "VentDys"),
#            cat.rmstat = list(c("row"), c("col")),
#            cat.ptype    = c("fisher", "chisq"),
#            cont.varlist = c("Age", "Wgt", "Lgt", "SurgAoCT", "SurgAAoD", "SurgCircArrTot",
#                             "SurgCPB", "SurgShuntS", "Cre", "DTE"),
#            cont.rmstat  = list(c("mediqr", "q1q3")),
#            cont.ptype   = c("ttest"), 
#            caption = "Table2: Summary Statistics by ECMO",output = "plain")
# 
# 
# ### Summary Statistics by SurvECMO###
# #####################################
# 
# make.table(dat          = data_ecmo,
#            strat        = "SurvECMO",
#            colnames     = c("Variable Name ", "No", "Yes", "Overall", "P-value"),
#            cat.varlist  = c("Sex", "Dx", "SurgShuntT" ,"ECA", "Hybrid","MechVent", "TR", "VentDys"),
#            cat.rmstat = list(c("row"), c("col")),
#            cat.ptype    = c("fisher", "chisq"),
#            cont.varlist = c("Age", "Wgt", "Lgt", "SurgAoCT", "SurgAAoD", "SurgCircArrTot",
#                             "SurgCPB", "SurgShuntS", "Cre", "DTE"),
#            cont.rmstat  = list(c("mediqr", "q1q3")),
#            cont.ptype   = c("ttest"), 
#            caption = "Table3: Summary Statistics by SurvECMO",output = "plain")
# 
# 
# ### Summary Statistics by SurvSt2 ####
# ######################################
# 
# make.table(dat          = data_ecmo,
#            strat        = "SurvSt2",
#            colnames     = c("Variable Name ", "No", "Yes", "Overall", "P-value"),
#            cat.varlist  = c("Sex", "Dx", "SurgShuntT" ,"ECA", "Hybrid","MechVent", "TR", "VentDys"),
#            cat.rmstat = list(c("row"), c("col")),
#            cat.ptype    = c("fisher", "chisq"),
#            cont.varlist = c("Age", "Wgt", "Lgt", "SurgAoCT", "SurgAAoD", "SurgCircArrTot",
#                             "SurgCPB", "SurgShuntS", "Cre", "DTE"),
#            cont.rmstat  = list(c("mediqr", "q1q3")),
#            cont.ptype   = c("ttest"), 
#            caption = "Table4: Summary Statistics by SurvSt2",output = "plain")
          


