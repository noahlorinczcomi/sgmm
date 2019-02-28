#' A Function to Define Time Points
#'
#' This function is specific to my MSc dissertation, used to more easily define multiple timpoints.
#' @param useing_for enter if using function to call time values (e.g., month names), or dosage change points.
#' @param start_dose enter starting dosage point.
#' @param end_dose enter ending dosage point.
#' @keywords lcga
#' @export
#' @examples
#' def.times()

def.times <- function(using_for, start_dose, end_dose) {
  # months
  months <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", 		"m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", 			"m18", "m19", "m20", "m21", "m22", "m23", "m24", "m25", 		"m26", "m27", "m28", "m29", "m30", "m31", "m32", "m33", 		"m34", "m35", "m36", "m37", "m38", "m39", "m40",
              "m41", "m42", "m43", "m44", "m45", "46")
  # doses
  doses <- c("initial_dose", 
             "change_1_dose", "change_2_dose", "change_3_dose",
             "change_4_dose", "change_5_dose", "change_6_dose",
             "change_7_dose", "change_8_dose", "change_9_dose",
             "change_10_dose", "change_11_dose", "change_12_dose",
             "change_13_dose", "change_14_dose", "change_15_dose",
             "change_16_dose", "change_17_dose", "change_18_dose",
             "change_19_dose", "change_20_dose", "change_21_dose",
             "change_22_dose", "change_23_dose", "change_24_dose",
             "change_25_dose", "change_26_dose", "change_27_dose",
             "change_28_dose", "change_29_dose", "change_30_dose",
             "change_31_dose", "change_32_dose", "change_33_dose",
             "change_34_dose", "change_35_dose", "change_36_dose",
             "change_37_dose", "change_38_dose", "change_39_dose",
             "change_40_dose", "change_41_dose", "change_42_dose",
             "change_43_dose", "change_44_dose", "change_45_dose")
  # identifying chosen beginning index using match()
  matched_indices <- match(c(start_dose, end_dose), doses)
  list_length <- matched_indices[1]:matched_indices[2]
  # this now contains indices in the month values (indices should be
  # the same in both lists
  if(using_for == 'dosages') {
    return_list <- doses[matched_indices[1]:matched_indices[2]]
  } else if(using_for == 'times') {
    return_list <- months[matched_indices[1]:matched_indices[2]]
  }
  # end
  return(return_list)
}
