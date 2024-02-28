## Various ad-hoc encodings ---------------------------------------------------

# Interpretes RecordEnd as being right censored.
# Thus imputes using the maximal observed time, 
# and adds column indicating censoring.
po_RecEnd_rc<-po(
  "mutate",
  mutation = list(
    RecordEnd_num = as.numeric(ifelse(is.na(RecordEnd),
                                      max(RecordBeg),
                                      RecordEnd)),
    RecordEnd_censored = is.na(RecordEnd)
  ),
  delete_originals = TRUE
)

# Reencodes RecordBeg (POSIXct) to numeric
po_RecBeg_num<-po(
  "mutate",
  mutation = list(
    RecordBeg_num = as.numeric(RecordBeg)
  ),
  delete_originals = TRUE
)

# Reencodes SocioCateg as an integer using the numbering provided in the factor
po_SocCat_int<-po(
  "mutate",
  mutation = list(
    SocioCateg_int = as.integer(substr(SocioCateg,4,5))),
  delete_originals = TRUE
)

# Reencodes VehPrice as integer using the default ordering
po_VehPrice_int<-po(
  "mutate",
  mutation = list(
    SocioCateg_int = as.integer(VehPrice)),
  delete_originals = TRUE
)

# Reencodes VehAge as numeric using the "mean" of provided intervals
po_VehAge_num<-po(
  "mutate",
  mutation = list(
    VehAge_num = case_match(VehAge,
                            "6-7" ~ 6.5,
                            "8-9" ~ 8.5,
                            "10+" ~ 10,
                            .default = as.numeric(VehAge))),
  delete_originals = TRUE
)