## Various ad-hoc encodings ---------------------------------------------------

# Interpretes RecordEnd as being right censored.
# Thus imputes using the maximal observed time, 
# and adds column indicating censoring.
po_RecEnd_rc<-po(
  "mutate",
  mutation = list(
    RecordEnd = ~as.numeric(ifelse(is.na(RecordEnd),
                                      max(na.omit(RecordEnd)),
                                      RecordEnd)),
    RecordEnd_censored = ~is.na(RecordEnd)
  ),
  id = "RecEnd_rc"
)

# Reencodes RecordBeg (POSIXct) to numeric
po_RecBeg_num<-po(
  "mutate",
  mutation = list(
    RecordBeg = ~as.numeric(RecordBeg)
  ),
  id = "RecBeg_num"
)

# Reencodes SocioCateg as an integer using the numbering provided in the factor
po_SocCat_int<-po(
  "mutate",
  id = "SocioCateg_as_integer",
  mutation = list(
    SocioCateg = ~as.integer(substr(SocioCateg,4,5))),
)

# Reencodes VehPrice as integer using the default ordering
po_VehPrice_int<-po(
  "mutate",
  id = "VehPrice_as_integer",
  mutation = list(
    VehPrice = ~as.integer(VehPrice))
)

# Reencodes VehAge as numeric using the "mean" of provided intervals
po_VehAge_num<-po(
  "mutate",
  id = "VehAge_as_num",
  mutation = list(
    VehAge = ~case_match(VehAge,
                            "6-7" ~ 6.5,
                            "8-9" ~ 8.5,
                            "10+" ~ 10,
                            .default = as.numeric(VehAge))),
)