"scatterPlotMatrix.HH" <-
function(){
    require("car")
    initializeDialog(title=gettextRcmdr("Scatterplot Matrix"))
    variablesBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Select variables (three or more)"),
        selectmode="multiple", initialSelection=NULL)
    checkBoxes(frame="optionsFrame", boxes=c("lsLine", "smoothLine"), initialValues=rep(1,1),
        labels=gettextRcmdr(c("Least-squares lines", "Smooth lines")))
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=5, orient="horizontal")
    radioButtons(name="diagonal", buttons=c("density", "histogram", "boxplot", "oned", "qqplot", "none"),
        labels=gettextRcmdr(c("Density plots", "Histograms", "Boxplots", "One-dimensional scatterplots", "Normal QQ plots", "Nothing (empty)")),
        title=gettextRcmdr("On Diagonal"))
    radioButtons(name="diagonalDirection", buttons=c("SW.NE", "NW.SE"),
        labels=gettextRcmdr(c("SW-NE ' / '", "NW-SE ' \\ '")),
        title=gettextRcmdr("Diagonal Direction"))
    subsetBox()
    onOK <- function(){
        variables <- getSelection(variablesBox)
        closeDialog()
        if (length(variables) < 3) {
            errorCondition(recall=scatterPlotMatrix.HH, message=gettextRcmdr("Fewer than 3 variable selected."))
            return()
            }
        line <- if("1" == tclvalue(lsLineVariable)) "lm" else "FALSE"
        smooth <- as.character("1" == tclvalue(smoothLineVariable))
        span <- as.numeric(tclvalue(sliderValue))
        diag <- as.character(tclvalue(diagonalVariable))
        diagDir <- as.character(tclvalue(diagonalDirectionVariable))
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" 
            else paste(", subset=", subset, sep="")
        .activeDataSet <- ActiveDataSet()
        if (.groups == FALSE) {
          command <- paste("scatterplot.matrix(~", paste(variables, collapse="+"),
                           ", reg.line=", line, ", smooth=", smooth,
                           ", span=", span/100, ", diagonal = '", diag,
                           "', data=", .activeDataSet, subset,
                           ", row1attop=", diagDir=="NW.SE",
                           ")", sep="")
          logger(command)
          justDoIt(command)
        }
        else {
          command <- paste("scatterplot.matrix(~", paste(variables, collapse="+")," | ", .groups,
                           ", reg.line=", line, ", smooth=", smooth,
                           ", span=", span/100, ", diagonal= '", diag,
                           "', by.groups=", .linesByGroup,
                           ", data=", .activeDataSet, subset,
                           ", row1attop=", diagDir=="NW.SE",
                           ")", sep="")
          logger(command)
          justDoIt(command)
        }
        activateMenus()
        tkfocus(CommanderWindow())
        }
    groupsBox(scatterPlot, plotLinesByGroup=TRUE)
    OKCancelHelp(helpSubject="scatterplot.matrix")
    tkgrid(getFrame(variablesBox), sticky="nw")    
    tkgrid(optionsFrame, sticky="w")
    tkgrid(diagonalFrame, sticky="w")
    tkgrid(diagonalDirectionFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=6, columns=2)
    }

