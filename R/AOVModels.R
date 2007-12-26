listAOVModels <-
function (envir = .GlobalEnv, ...) 
{
  objects <- ls(envir = envir, ...)
  if (length(objects) == 0) 
    NULL
  else {
    aovs <- sapply(objects,
                   function(.x) {
                     obj <- get(.x, envir = envir)
                     "aov" %in% class(obj) |
                     ("lm" %in% class(obj) && length(obj$xlevels) > 0)
                   }
                   ) 
    objects[aovs]
  }
}

AOVModelsP <-
  function (n = 1) 
  length(listAOVModels()) >= n

is.AOVModel <-
function (object) 
{
    "aov" %in% class(object)
}

checkActiveAOVModel <-
function () 
{
    if (activeAOVModel() == FALSE) {
        tkfocus(CommanderWindow())
        FALSE
    }
    else TRUE
}

activeAOVModelP <-
function () 
!is.null(ActiveAOVModel())

activeAOVModel <-
function (model) 
{
    if (missing(model)) {
        .activeAOVModel <- ActiveAOVModel()
        if (is.null(.activeAOVModel)) {
            Message(message = gettextRcmdr("There is no activeAOV model."), 
                type = "error")
            return(FALSE)
        }
        else return(.activeAOVModel)
    }
    ActiveAOVModel(model)
    RcmdrTclSet("modelName", paste(" ", model, " "))
    if (!is.SciViews()) 
        tkconfigure(getRcmdr("modelLabel"), fg = "blue")
    else refreshStatus()
    activateMenus()
    model
}

ActiveAOVModel <-
function (name) 
{
    if (missing(name)) {
        temp <- getRcmdr(".activeAOVModel")
        if (is.null(temp)) 
            return(NULL)
        else if (!exists(temp) || !is.AOVModel(get(temp, envir = .GlobalEnv))) {
            Message(sprintf(gettextRcmdr("the model %s is no longer available"), 
                temp), type = "error")
            putRcmdr(".activeAOVModel", NULL)
            RcmdrTclSet("modelName", gettextRcmdr("<No activeAOV model>"))
            activateMenus()
            return(NULL)
        }
        else return(temp)
    }
    else putRcmdr(".activeAOVModel", name)
}


## source("~/HH-R.package/RcmdrPlugin.HH/R/AOVModels.R")
