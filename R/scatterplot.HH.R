scatterPlot.HH <- function(){
    require("car")
    initializeDialog(title=gettextRcmdr("Scatterplot.HH"))
    .numeric <- Numeric()
    variablesFrame <- tkframe(top)
    xBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("x-variable (pick one)"))
    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("y-variable (pick one)"))
    checkBoxes(frame="optionsFrame", boxes=c("identify", "jitterX", "jitterY", "boxplots", "lsLine", "smoothLine"),
        initialValues=c(0, 0, 0, 0, 1, 0), labels=gettextRcmdr(c("Identify points", "Jitter x-variable", "Jitter y-variable",
        "Marginal boxplots", "Least-squares line", "Smooth Line")))
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=5, orient="horizontal")
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
    pchVar <- tclVar(gettextRcmdr("<auto>"))
    pchEntry <- tkentry(parFrame, width=25, textvariable=pchVar)      
    cexValue <- tclVar("1")
    cex.axisValue <- tclVar("1")
    cex.labValue <- tclVar("1")    
    cexSlider <- tkscale(parFrame, from=0.5, to=2.5, showvalue=TRUE, variable=cexValue,
        resolution=0.1, orient="horizontal")
    cex.axisSlider <- tkscale(parFrame, from=0.5, to=2.5, showvalue=TRUE, variable=cex.axisValue,
        resolution=0.1, orient="horizontal")
    cex.labSlider <- tkscale(parFrame, from=0.5, to=2.5, showvalue=TRUE, variable=cex.labValue,
        resolution=0.1, orient="horizontal")
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        closeDialog()
        if (length(x) == 0 || length(y) == 0){
            errorCondition(recall=scatterPlot.HH, message=gettextRcmdr("You must select two variables"))
            return()
            }
        if (x == y) {
            errorCondition(recall=scatterPlot.HH, message=gettextRcmdr("x and y variables must be different"))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        jitter <- if ("1" == tclvalue(jitterXVariable) && "1" == tclvalue(jitterYVariable)) ", jitter=list(x=1, y=1)"
            else if ("1" == tclvalue(jitterXVariable)) ", jitter=list(x=1)"
            else if ("1" == tclvalue(jitterYVariable)) ", jitter=list(y=1)"
            else ""
        if("1" == tclvalue(identifyVariable)){
            RcmdrTkmessageBox(title="Identify Points",
                message=gettextRcmdr("Use left mouse button to identify points,\nright button to exit."),
                icon="info", type="ok")
            labels <- paste("rownames(", .activeDataSet, ")", sep="")
            }
        else labels <- "FALSE"
        box <- if ("1" == tclvalue(boxplotsVariable)) "'xy'" else "FALSE"
        line <- if("1" == tclvalue(lsLineVariable)) "lm" else "FALSE"
        smooth <- as.character("1" == tclvalue(smoothLineVariable))
        span <- as.numeric(tclvalue(sliderValue))
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" 
            else paste(", subset=", subset, sep="")
        xlab <- trim.blanks(tclvalue(xlabVar))
        xlab <- if(xlab == gettextRcmdr("<auto>")) "" else paste(', xlab="', xlab, '"', sep="")
        ylab <- trim.blanks(tclvalue(ylabVar))
        ylab <- if(ylab == gettextRcmdr("<auto>")) "" else paste(', ylab="', ylab, '"', sep="")
        cex <- as.numeric(tclvalue(cexValue))
        cex <- if(cex == 1) "" else paste(', cex=', cex, sep="")
        cex.axis <- as.numeric(tclvalue(cex.axisValue))
        cex.axis <- if(cex.axis == 1) "" else paste(', cex.axis=', cex.axis, sep="")
        cex.lab <- as.numeric(tclvalue(cex.labValue))
        cex.lab <- if(cex.lab == 1) "" else paste(', cex.lab=', cex.lab, sep="")        
        pch <- gsub(" ", ",", tclvalue(pchVar))
        if ("" == pch) {
            errorCondition(recall=scatterPlot.HH, message=gettextRcmdr("No plotting characters."))
            return()
            }
        pch <- if(trim.blanks(pch) == gettextRcmdr("<auto>")) {
          if (.groups == FALSE) ", pch=16" else ""
        }
        else paste(", pch=c(", pch, ")", sep="")
        if (.groups == FALSE) {
            doItAndPrint(paste("scatterplot(", y, "~", x,
                ", reg.line=", line, ", smooth=", smooth, ", labels=", labels,
                ", boxplots=", box, ", span=", span/100, jitter, xlab, ylab,
                cex, cex.axis, cex.lab, pch,
                ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else {
            doItAndPrint(paste("scatterplot(", y, "~", x," | ", .groups,
                ", reg.line=", line, ", smooth=", smooth, ", labels=", labels,
                ", boxplots=", box, ", span=", span/100, jitter, xlab, ylab,
                cex, cex.axis, cex.lab, pch,
                ", by.groups=", .linesByGroup,
                ", data=", .activeDataSet, subset, ")", sep=""))
            }
        activateMenus()
        tkfocus(CommanderWindow())
        }
    groupsBox(scatterPlot, plotLinesByGroup=TRUE)
    OKCancelHelp(helpSubject="scatterplot")
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw") 
    tkgrid(variablesFrame, sticky="w")   
    tkgrid(tklabel(optionsFrame, text=gettextRcmdr("Span for smooth")), slider, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsFrame, sticky="w")    
    tkgrid(labelsFrame, sticky="w")
    tkgrid(tklabel(top, text=" "))    
    tkgrid(tklabel(parFrame, text=gettextRcmdr("Plotting Parameters"), fg="blue"), sticky="w")
    tkgrid(tklabel(parFrame, text=gettextRcmdr("Plotting characters")), pchEntry, stick="w")
    tkgrid(tklabel(parFrame, text=gettextRcmdr("Point size")), cexSlider, sticky="w")
    tkgrid(tklabel(parFrame, text=gettextRcmdr("Axis text size")), cex.axisSlider, sticky="w")
    tkgrid(tklabel(parFrame, text=gettextRcmdr("Axis-labels text size")), cex.labSlider, sticky="w")
    tkgrid(parFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=8, columns=2)
    }

## source("~/HH-R.package/RcmdrPlugin.HH/R/scatterplot.HH.R")
