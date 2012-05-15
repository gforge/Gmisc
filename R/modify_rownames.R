#' Change names of the columns
#' 
#' Used for switching the names for the variables to something more
#' sensible than the default, usually not so pretty names.
#' 
#' TODO: start using label
#' 
#' @param rn A vector with strings repreesenting row names 
#' @returnType List
#' @return Returns a list with all the names. This is due to that some names
#'   are changed into expressions, mostly to get the >= in a pretty way
#' 
#' @author max
#' @export
modify_rownames <- function(rn){
    # Create matrix with strings to be replaced
    var_matrix <- rbind(
      c("[*_] EMPTY ROW [_*]", "* NOT A NUMBER *"),
      c("Percent_compression", "Compression (%)"),
      c("Angle", "Gamma angle (increase 5 degrees)"),
      c("Asa_groups[=]*ASA 3 & 4", "3 & 4"),
      c("Time$", "Time (years)"),
      c("Sex[=]*Male", "Male"),
      c("Smoking[=]*Yes", "Yes"),
      c("Smoking[=]*Has quit > 6 months ago", "Ex-smoker"),
      c("Smoking[=]*Ex-smoker", "Ex-smoker"),
      c("Reposition[=]*Acceptable$", "Acceptable"),
      c("Reposition[=]*Acceptable or poor$", "Acceptable or poor"),
      c("Reposition[=]*Poor", "Poor"),
      c("Fracture_type_groups[=]*A2", "A2"),
      c("Lateral_protrusion", "Lateral protrusion (mm)"),
      c("Asa_groups=ASA 3 & 4", "3 & 4"),
      c("age_group.less.50", "Age < 50"),
      c("age_group.50.to.59", "Age 50-59"),
      c("age_group.more.75", "Age > 75"),
      c("^male_gender$", "Male"),
      c("^female_gender$", "Female"),
      c("^CHARKAT0A$", "Charnley A"),
      c("^CHARKAT0B$", "Charnley B"),
      c("^CHARKAT0C$", "Charnley C"),
      c("^NC06_code", "Antidepressants (Y vs N)"),
      c("^surgical_approachPosterior", "Posterior approach"),
      c("^surgical_approachAnterior", "Anterior approach"),
      c("^surgical_approachOther", "Other approach"),
      c("^reop_within_a_yearTRUE", "Reoperation within 1:st year"),
      c("^KLINGRP=County hospital", "County hospitals"),
      c("^KLINGRP=Rural hospital", "Rural hospitals"),
      c("^KLINGRP=Private hospital", "Private hospitals"),
      c("^PROTGRP=Uncemented$", "Uncemented"),
      c("^PROTGRP=Hybrid$", "Hybrid"),
      c("^PROTGRP=Rev. hybrid$", "Rev. hybrid"),
      c("^PROTGRP=Resurfacing$", "Resurfacing"),
      c("^(female_gender [*] PROTGRP=Uncemented|PROTGRP=Uncemented [*] female_gender)", "Female & uncemented"),
      c("^(female_gender [*] PROTGRP=Hybrid|PROTGRP=Hybrid [*] female_gender)", "Female & hybrid"),
      c("^(female_gender [*] PROTGRP=Rev. hybrid|PROTGRP=Rev. hybrid [*] female_gender)", "Female & rev. hybrid"),
      c("^(female_gender [*] PROTGRP=Resurfacing|PROTGRP=Resurfacing [*] female_gender)", "Female & resurfacing"),
      c("^OPNR$", "Second hip"),
      c("^OPNR=Second$", "Second hip"),
      c("[0-9]+yr_MI$", "Myocard. Inf."),
      c("[0-9]+yr_CHF$", "Congestive heart failure"),
      c("[0-9]+yr_DEM$", "Dementia"),
      c("[0-9]+yr_PVD$", "Periph. vasc. disease"),
      c("[0-9]+yr_COPD$", "Chronic pulmonary disease"),
      c("[0-9]+yr_LD$", "Liver disease"),
      c("([0-9]+yr_D(IAB|iab)|Combined_Diabetes)$", "Diabetes"),
      c("[0-9]+yr_D(IAB|iab)_C$", "Diabetes compl."),
      c("[0-9]+yr_D(IAB|iab)_UC$", "Diabetes uncompl."),
      c("[0-9]+yr_CEVD$", "Cerebrovascular disease"),
      c("[0-9]+yr_RD$", "Renal disease"),
      c("[0-9]+yr_Rheum$", "Rheumatic disease"),
      c("[0-9]+yr_CANCER$", "Any malignancy"),
      c("[0-9]+yr_METS$", "Metastatic cancer"),
      c("[0-9]+yr_PUD$", "Peptic ulcer disease"),
      c("Combined_LiverDisease$", "Liver disease"),
      c("[0-9]+yr_Arrhy$", "Caridiac arrhythmia"),
      c("[0-9]+yr_VD$", "Valvular disease"),
      c("[0-9]+yr_PCD$", "Pulmonary circ. disorders"),
      c("[0-9]+yr_HPTN_C$", "Hypertension uncompl."),
      c("[0-9]+yr_HPTN_UC$", "Hypertension compl."),
      c("[0-9]+yr_PARA$", "Paralysis"),
      c("[0-9]+yr_OthND$", "Other neurol. disorders"),
      c("[0-9]+yr_Hptothy$", "Hypothyroidism"),
      c("[0-9]+yr_RF$", "Renal Failure"),
      c("[0-9]+yr_PUD_NB$", "Peptic ulcer disease"),
      c("[0-9]+yr_Lymp$", "Lymphoma"),
      c("[0-9]+yr_Rheum_A$", "Rheumatic/collagen disease"),
      c("[0-9]+yr_Coag$", "Coagulopathy"),
      c("[0-9]+yr_Obesity$", "Obesity"),
      c("[0-9]+yr_WL$", "Weight loss"),
      c("[0-9]+yr_Fluid$", "Fluid and electrolyte disorders"),
      c("[0-9]+yr_BLA$", "Blood loss anemia"),
      c("[0-9]+yr_DA$", "Deficiency anemia"),
      c("[0-9]+yr_Alcohol$", "Alcohol abuse"),
      c("[0-9]+yr_Drug$", "Drug abuse"),
      c("[0-9]+yr_Psycho$", "Psychoses"),
      c("[0-9]+yr_Dep$", "Depression"),
      c("[0-9]+yr_Tumor$", "Solid tumor"),
    
      c("^(elixhausers_group.1|ElxCI_1yr_grouped[=]*1)$", "Elixhausers = 1"),
      c("^(elixhausers_group.2|ElxCI_1yr_grouped[=]*2)$", "Elixhausers = 2"),
      c("^(elixhausers_group.3|ElxCI_1yr_grouped[=]*3)$", "Elixhausers = 3"),
      c("^CCI_1yr_grouped_danish1-2$", "Charlsons = 1-2"),
      c("^CCI_1yr_grouped_danish=1-2$", "Charlsons = 1-2"),
      c("^charlsons_group.1$", "Charlsons = 1"),
      c("^charlsons_group.2$", "Charlsons = 2"),
      c("^rs_charlsons_group.1$", "RCS Charlsons = 1"),
      c("^rs_charlsons_group.2$", "RCS Charlsons = 2"),
      c("^RS_CCI_1yr_grouped_danish1-2$", "RCS Charlsons = 1-2"),
      c("^RS_CCI_1yr_grouped_danish=1-2$", "RCS Charlsons = 1-2"),
      
      c("^Denmark_ALDGRP=less 50", "Age < 50"),
      c("^Denmark_ALDGRP=50 to 59", "Age 50-59"),
      c("^Denmark_ALDGRP=60 to 69", "Age 60-69"),
      c("^Denmark_ALDGRP=70 to 79", "Age 60-69")
    )
        
    rn <- as.list(rn)
     
    index <- grep("^Denmark_ALDGRP=more 80$", rn)
    if (length(index) > 0 && is.na(index) == FALSE){
        rn[[index]] = expression(Age >= 80)
    }
     
    index <- grep("^CCI_1yr_grouped_danish[=]*>2$", rn)
    if (length(index) > 0 && is.na(index) == FALSE){
        rn[[index]] = expression(Charlsons >= 3)
    }
    index <- grep("^RS_CCI_1yr_grouped_danish[=]*>2$", rn)
    if (length(index) > 0 && is.na(index) == FALSE){
        rn[[index]] = expression("RCS Charlsons" >= 3)
    }
    index <- grep("^ElxCI_1yr_grouped[=]*>3$", rn)
    if (length(index) > 0 && is.na(index) == FALSE){
        rn[[index]] = expression(Elixhausers >= 4)
    }
    
    # Can't figure out a better way
    index <- grep("^charlsons_group..1$", rn)
    if (length(index) > 0 && is.na(index) == FALSE){
        rn[[index]] = expression(Charlsons >= 2)
    }
    index <- grep("^charlsons_group..[23]$", rn)
    if (length(index) > 0 && is.na(index) == FALSE){
        rn[[index]] = expression(Charlsons >= 3)
    }
    
    index <- grep("^rs_charlsons_group..1$", rn)
    if (length(index) > 0 && is.na(index) == FALSE){
        rn[[index]] = expression("RCS Charlsons" >= 2)
    }
    index <- grep("^rs_charlsons_group..2$", rn)
    if (length(index) > 0 && is.na(index) == FALSE){
        rn[[index]] = expression("RCS Charlsons" >= 2)
    }
    
    index <- grep("elixhausers_group..1$", rn)
    if (length(index) > 0 && is.na(index) == FALSE){
        rn[[index]] = expression(Elixhausers >= 2)
    }
    
    index <- grep("^elixhausers_group..[23]", rn)
    if (length(index) > 0 && is.na(index) == FALSE){
        rn[[index]] = expression(Elixhausers >= 3)
    }
    
    # A simpler way is possible fro the regular text
    # but it wont work with the expressions
    for (i in 1:NROW(var_matrix)){
        index <- grep(var_matrix[i,1], rn)
        if (length(index) > 0){
            if (var_matrix[i,2] == "* NOT A NUMBER *"){
                rn[index] = NA
            }else{
                rn[index] = var_matrix[i,2]           
            }
        }
    }
    
    return(list(rn))
}

