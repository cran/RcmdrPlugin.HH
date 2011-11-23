listAllLikertCapable <- function (envir = .GlobalEnv, ...) 
{
    objects <- ls(envir = envir, ...)
    if (length(objects) == 0) 
        NULL
    else objects[sapply(objects, function(x) is.likertCapable(get(x)))]
}


PlotLikertDialog <- function() {
  initializeDialog(title=gettextRcmdr("Likert Plots"))
  allLikertCapables <- listAllLikertCapable()
  likertFrame <- tkframe(top)
  likertBox <- variableListBox(likertFrame, allLikertCapables,
                               title=gettextRcmdr("Likert Capable Objects (pick one)"),
                               selectmode="single",
                               initialSelection=0)

  mainVar <- tclVar("")
  mainEntry <- tkentry(likertFrame, width="30", textvariable=mainVar)
  LikertPlotNameVar <- tclVar("LikertPlot")
  LikertPlotNameEntry <- tkentry(likertFrame, width="16", textvariable=LikertPlotNameVar)
  boxWidthNumberVar <- tclVar("")
  boxWidthNumberEntry <- tkentry(likertFrame, width="16", textvariable=boxWidthNumberVar)
  boxWidthUnitVar <- tclVar("mm")
  boxWidthUnitEntry <- tkentry(likertFrame, width="16", textvariable=boxWidthUnitVar)
  layoutVar            <- tclVar("")
  layoutEntry            <- tkentry(likertFrame, width="16", textvariable=layoutVar)

  onOK <- function() {
                                        #on.exit(recover())
    table <- getSelection(likertBox)
    closeDialog()
    if (0 == length(table)) {
      errorCondition(recall=PlotLikertDialog,
                     message=gettextRcmdr("Exactly one Likert Capable Object must be selected."))
      return()
    }

    main  <- tclvalue(mainVar)
    LikertPlotName  <- tclvalue(LikertPlotNameVar)
    if (nchar(LikertPlotName)==0) LikertPlotName <- "LikertPlot"
    boxWidthValue  <- tclvalue(boxWidthNumberVar)
    boxWidthUnitValue  <- tclvalue(boxWidthUnitVar)
    layoutValue  <- tclvalue(layoutVar)

    command1 <- paste(LikertPlotName, " <- plot.likert(", table,
                      if (nchar(main) != 0) paste(", main='", main, "',", sep=""),
                      if (nchar(boxWidthValue) != 0) paste(
                                 ", box.width=unit(", boxWidthValue,
                                 ",'", boxWidthUnitValue, "')", sep=""),
                      if (nchar(layoutValue) != 0) paste(", layout=", layoutValue, sep=""),
                      ")", sep="")
    doItAndPrint(command1)
    doItAndPrint(LikertPlotName)
    activateMenus()
    tkfocus(CommanderWindow())
    justDoIt("bringToTop()")
  }
  OKCancelHelp(helpSubject="plot.likert")
  tkgrid(getFrame(likertBox),
         columnspan=1, sticky="w")
  tkgrid(tklabel(likertFrame, text=gettextRcmdr("Main title:")), mainEntry, sticky="w")
  tkgrid(tklabel(likertFrame, text=gettextRcmdr("LikertPlot Name:")), LikertPlotNameEntry, sticky="w")
  tkgrid(tklabel(likertFrame, text=gettextRcmdr("box.width:")), boxWidthNumberEntry, sticky="w")
  tkgrid(tklabel(likertFrame, text=gettextRcmdr("box.width unit:")), boxWidthUnitEntry, sticky="w")
  tkgrid(tklabel(likertFrame, text=gettextRcmdr("layout:")),            layoutEntry           , sticky="w")
  tkgrid(likertFrame, sticky="w")


  tkgrid(buttonsFrame, columnspan=2, sticky="w")
  dialogSuffix(rows=7, columns=2)
}

## source("c:/HOME/rmh/HH-R.package/RcmdrPlugin.HH/R/likert.R")
