Regr1Plot <- function() {
    require("graphics")
    initializeDialog(title=gettextRcmdr("Squared Residuals"))
    .numeric <- Numeric()
    variablesFrame <- tkframe(top)
    xBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("x-variable (pick one)"))
    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("y-variable (pick one)"))
    gBox <- variableListBox(variablesFrame, Factors(),
                            title=gettextRcmdr("group-variable (pick zero or one)"),
                            selectmode="single", initialSelection=-1)
    checkBoxes(frame="optionsFrame",
               boxes=c("", "jitterX", "useActiveModel", "", "", "points.yhat"),
               initialValues=c(0, 0, 0, 0, 1, 1),
               labels=gettextRcmdr(c("", "Jitter x-variable", "Use Active Model", "", "", "Display Y.hat points")))
    radioButtons(name="residuals",
                 buttons=c("none",
                   "square",
                   "line"),
                 values=c("FALSE", "'square'", "'line'"),
                 initialValue="'square'",
                 labels=gettextRcmdr(c
                   ("point estimate only",
                    "residuals---squares",
                    "residuals---straight line")),
                 title=gettextRcmdr("Residual Display"))
    radioButtons(name="model",
                 buttons=c("linear", "active"),
                 values=c("0", "1"),
                 initialValue="0",
                 labels=gettextRcmdr(c
                   ("default linear model",
                    "Active model")),
                 title=gettextRcmdr("Model"))
    subsetBox()    
    labelsFrame <- tkframe(top)
    xlabVar <- tclVar(gettextRcmdr("<auto>"))
    ylabVar <- tclVar(gettextRcmdr("<auto>"))
    xlabFrame <- tkframe(labelsFrame)
    xlabEntry <- tkentry(xlabFrame, width="25", textvariable=xlabVar)
    xlabScroll <- tkscrollbar(xlabFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(xlabEntry, ...))
    tkconfigure(xlabEntry, xscrollcommand=function(...) tkset(xlabScroll, ...))
    tkgrid(tklabel(xlabFrame, text=gettextRcmdr("x-axis label"), fg="blue"), sticky="w")
    tkgrid(xlabEntry, sticky="w")
    tkgrid(xlabScroll, sticky="ew")
    ylabFrame <- tkframe(labelsFrame)
    ylabEntry <- tkentry(ylabFrame, width="25", textvariable=ylabVar)
    ylabScroll <- tkscrollbar(ylabFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(ylabEntry, ...))
    tkconfigure(ylabEntry, xscrollcommand=function(...) tkset(ylabScroll, ...))
    tkgrid(tklabel(ylabFrame, text=gettextRcmdr("y-axis label"), fg="blue"), sticky="w")
    tkgrid(ylabEntry, sticky="w")
    tkgrid(ylabScroll, sticky="ew")
    tkgrid(xlabFrame, tklabel(labelsFrame, text="     "), ylabFrame, sticky="w")    
    parFrame <- tkframe(top) 

    xlimVar <- tclVar(gettextRcmdr("<auto>"))
    xlimFrame <- tkframe(top)
    xlimEntry <- tkentry(xlimFrame, width="25", textvariable=xlimVar)
    xlimScroll <- tkscrollbar(xlimFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(xlimEntry, ...))
    tkconfigure(xlimEntry, xscrollcommand=function(...) tkset(xlimScroll, ...))
    tkgrid(tklabel(xlimFrame, text=gettextRcmdr("x-limits"), fg="blue"), sticky="w")
    tkgrid(xlimEntry, sticky="w")
    tkgrid(xlimScroll, sticky="ew")

    ylimVar <- tclVar(gettextRcmdr("<auto>"))
    ylimFrame <- tkframe(top)
    ylimEntry <- tkentry(ylimFrame, width="25", textvariable=ylimVar)
    ylimScroll <- tkscrollbar(ylimFrame, orient="horizontal",
        repeatinterval=5, command=function(...) tkxview(ylimEntry, ...))
    tkconfigure(ylimEntry, xscrollcommand=function(...) tkset(ylimScroll, ...))
    tkgrid(tklabel(ylimFrame, text=gettextRcmdr("y-limits"), fg="blue"), sticky="w")
    tkgrid(ylimEntry, sticky="w")
    tkgrid(ylimScroll, sticky="ew")


    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        g <- getSelection(gBox)
        closeDialog()
        if (length(x) == 0 || length(y) == 0){
            errorCondition(recall=Regr1Plot, message=gettextRcmdr("You must select two variables"))
            return()
            }
        if (x == y) {
            errorCondition(recall=Regr1Plot, message=gettextRcmdr("x and y variables must be different"))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        Dx <- paste(.activeDataSet, "$", x, sep="")
        Dy <- paste(.activeDataSet, "$", y, sep="")
        Dg <- paste(.activeDataSet, "$", g, sep="")
        if ("1" == tclvalue(jitterXVariable)) Dx <- paste("jitter(", Dx, ")", sep="")
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" 
            else paste(", subset=", subset, sep="")

        xlab <- trim.blanks(tclvalue(xlabVar))
        if(xlab == gettextRcmdr("<auto>")) xlab <- x
        xlab <- paste(', xlab="', xlab, '"', sep="")
        ylab <- trim.blanks(tclvalue(ylabVar))
        if(ylab == gettextRcmdr("<auto>")) ylab <- y
        ylab <- paste(', ylab="', ylab, '"', sep="")

        xlim <- trim.blanks(tclvalue(xlimVar))
        xlim <- if(xlim == gettextRcmdr("<auto>")) ""
        else paste(', xlim=', xlim, sep="")
        ylim <- trim.blanks(tclvalue(ylimVar))
        ylim <- if(ylim == gettextRcmdr("<auto>")) ""
        else paste(', ylim=', ylim, sep="")

        model <- if (0==tclvalue(modelVariable)) ""
         else paste(', model=', ActiveModel(), sep="")

        main <- if (model == "")
          paste('Residuals from model: ', y, ' ~ ', x)
        else
          paste('Residuals from model: ', deparse(as.formula(get(ActiveModel()))))
        
        if (length(g) == 0) {
          doItAndPrint(paste("regr1.plot(", "x=", Dx, ", y=", Dy,
                             ", resid.plot=", tclvalue(residualsVariable),
                             xlab, ylab, ", cex=1.3",
                             xlim, ylim, model,
                             subset,
                             ", main='", main, "')", sep=""))
        }
        else {
          doItAndPrint(paste("regr1.plot(", "x=", Dx, ", y=", Dy,
                             ", resid.plot=", tclvalue(residualsVariable),
                             ", col=", "match(", Dg, ", levels(", Dg, "))+1",
                             xlab, ylab, ", cex=1.3",
                             xlim, ylim, model,
                             subset,
                             ", main='", main, "')", sep=""))
        }
        activateMenus()
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="regr1.plot")
    tkgrid(getFrame(xBox), getFrame(yBox),
           getFrame(gBox), columnspan=1, sticky="nw") 
    tkgrid(variablesFrame, sticky="w")   
    tkgrid(optionsFrame, sticky="w")
    tkgrid(residualsFrame, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(xlimFrame, sticky="w")
    tkgrid(ylimFrame, sticky="w")
##     tkgrid(modelFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(labelsFrame, sticky="w")
    tkgrid(tklabel(top, text=" "))    
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=8, columns=2)
    }

## source("~/HH-R.package/RcmdrPlugin.HH/R/Regr1Plot.R")
