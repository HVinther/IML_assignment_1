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
    SocioCateg = ~as.integer(substr(SocioCateg,4,5)))
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
                            .default = as.numeric(VehAge)))
)


## Weights -----------------------------------

## Creates a weighting using exposure and sets it as the weigthing
po_add_weighting<-
  po("mutate",
     id = "create_weight",
     mutation =list(
       weights =~exp((Exposure-0.5)*12)/(1+exp((Exposure-0.5)*12))
       ))%>>%
  po("colroles",
     id = "set_weight",
     new_role = list(
       weights = "weight"
       ))


## po("mutate") does seem to be able to use the response as part of the feature creation.
## Frequency weight can be added using the add_weight function. The sigmoid function below
## is just a helper function for add_weight
.sigmoid <- function(x,k = 12, location = 0.5){
  inner <- exp((x-location)*k)
  return(inner/(1+inner))
}

## The add_weigth function can be called on a data.frame or a Task. It can fx. be used on 
## an unweighted Task inside a bencmark call ie.:
## benchmark(add_weight(task),learner,resampler)
add_weight<-function(dataset,
                     weighting = c("interest","frequency"),
                     case_values = c(213,1,21),
                     k = 12,
                     location = 0.5){
  stopifnot(any(weighting %in% c("interest","frequency")))
  if("data.frame" %in% class(dataset)){
    w<-rep(1,times = nrow(dataset))
    if("interest"%in% weighting){
      w<-w*case_match(sign(dataset$ClaimAmount),
                      -1 ~ case_values[1],
                      0 ~ case_values[2],
                      1 ~ case_values[3])
    }
    if("frequency" %in% weighting){
      w<-w*.sigmoid(dataset$Exposure,k = 12, location)
    }
    return(mutate(dataset,weights = w))
  } else if("Task" %in% class(dataset)){
    data <- dataset$data() |> as.data.frame()
    w<-rep(1,times = nrow(data))
    if("interest"%in% weighting){
      w<-w*case_match(sign(data$ClaimAmount),
                      -1 ~ case_values[1],
                      0 ~ case_values[2],
                      1 ~ case_values[3])
    }
    if("frequency" %in% weighting){
      w<-w*.sigmoid(data$Exposure,k = 12, location)
    }
    out<-as_task_regr(
      mutate(data,weights = w),
      target = dataset$col_roles$target)
    out$set_col_roles("weights",roles = "weight")
    out$id <- paste(c(dataset$id,"weight",paste(weighting,collapse = "_")),collapse = "_")
    return(out)
  }
}