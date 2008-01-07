## The following function by Richard Heiberger, with small modifications by J. Fox
## with more modifications by Richard Heiberger.
## 2008-01-03 added conditions, layout, and multiple colors
Xyplot.HH <- function() {
    require("lattice")
    initializeDialog(title=gettextRcmdr("XY Conditioning Plot"))
    predictorFrame <- tkframe(top)
    predictorBox <- variableListBox(predictorFrame, Numeric(), title=gettextRcmdr("Explanatory variables (pick one or more)"), selectmode="multiple")
    responseBox <- variableListBox(predictorFrame, Numeric(), title=gettextRcmdr("Response variables (pick one or more)"), selectmode="multiple")
    cgFrame <- tkframe(top)
    conditionsBox <- variableListBox(cgFrame, Factors(), title=gettextRcmdr("Conditions '|' (pick zero or more)"), selectmode="multiple", initialSelection=FALSE)
    groupsBox <- variableListBox(cgFrame, Factors(), title=gettextRcmdr("Groups 'groups=' (pick zero or more)"), selectmode="multiple", initialSelection=FALSE)
    checkBoxes(frame="optionsFrame",
               boxes=c("auto.key", "outer"),
               initialValues=c(1,0),
               labels=gettextRcmdr(c("Automatically draw key", 
                "Different panels for different y~x combinations")))
    relationFrame <- tkframe(top)
    radioButtons(window=relationFrame,
                 name="x.relation",
                 buttons=c("same", "free", "sliced"),
                 labels=gettextRcmdr(c("Identical", "Free", "Same range")),
                 title=gettextRcmdr("X-Axis Scales in Different Panels"))
    radioButtons(window=relationFrame,
                 name="y.relation",
                 buttons=c("same", "free", "sliced"),
                 labels=gettextRcmdr(c("Identical", "Free", "Same range")),
                 title=gettextRcmdr("Y-Axis Scales in Different Panels"))

    scalarsFrame <- tkframe(top)

    layoutColumnsVar <- tclVar("")
    layoutColumnsEntry <- tkentry(scalarsFrame, width="6", textvariable=layoutColumnsVar)
    layoutRowsVar <- tclVar("")
    layoutRowsEntry <- tkentry(scalarsFrame, width="6", textvariable=layoutRowsVar)

    onOK <- function() {
        predictor <- getSelection(predictorBox)
        response <- getSelection(responseBox)
        conditions <- getSelection(conditionsBox)
        groups <- getSelection(groupsBox)
        closeDialog()
        if (0 == length(response)) {
            errorCondition(recall=Xyplot.HH, message=gettextRcmdr("At least one response variable must be selected."))
            return()
            }
        if (0 == length(predictor)) {
            errorCondition(recall=Xyplot.HH, message=gettextRcmdr("At least one explanatory variable must be selected."))
            return()
            }
        auto.key <- ("1" == tclvalue(auto.keyVariable))
        outer    <- ("1" == tclvalue(outerVariable))
        x.relation <- as.character(tclvalue(x.relationVariable))
        y.relation <- as.character(tclvalue(y.relationVariable))

        layoutColumns  <- as.numeric(tclvalue(layoutColumnsVar))
        layoutRows     <- as.numeric(tclvalue(layoutRowsVar))
        layout.command <- ""
        number.na <- is.na(layoutColumns) + is.na(layoutRows)

        if (number.na==1) {
            errorCondition(recall=Xyplot.HH,
                           message=gettextRcmdr("Both or neither layout values must be numbers."))
            return()
          }
        if (number.na==0) layout.command <- deparse(c(layoutColumns, layoutRows))
        
        .activeDataSet <- ActiveDataSet()

        
        
        condtions.command <-
          if (length(conditions)==0) {
            if (outer) {
              if (layout.command=="")
                paste(", layout=c(",
                      length(predictor),
                      ",",
                      length(response),
                      ")")
              else
                paste(", layout=", layout.command, sep="")
            }
          }
          else {  ## (length(conditions)>0)
            if (outer) {
              condition.levels <- prod(sapply(conditions, d.f=get(.activeDataSet),
                                              function(g, d.f) length(levels(d.f[[g]]))))
              paste(", layout=c(",
                    condition.levels,
                    "*",
                    length(predictor),
                    ",",
                    length(response),
                    ")",
                    ## ", between=list(x=c(0,0, 1, 0,0), y=1)",
                    ", between=list(x=c(",
                    paste(rep(c(rep(0, condition.levels-1), 1),
                              length=condition.levels*length(predictor)-1),
                          collapse=","),
                    "), y=1)")
            }
          }
        
              groups.command <-
              if (length(groups)==1) paste(", groups=", groups, sep="")
              else ""
              
        xyplot.command <- paste("xyplot(",
                                paste(response, collapse=' + '),
                                " ~ ",
                                paste(predictor, collapse=' + '),
                                if (length(conditions) > 0)
                                   paste(" | ",
                                         paste(conditions, collapse=' + ')
                                         ) else "",
                                if (outer) ",\n outer=TRUE",
                                condtions.command,
                                groups.command,
                                ", pch=16",
                                if (auto.key) ",\n auto.key=list(border=TRUE), par.settings = simpleTheme(pch=16)" else "",
                                paste(", scales=list(x=list(relation='",
                                      x.relation,
                                      "'), y=list(relation='",
                                      y.relation,
                                      "'))", sep=""),
                                ",\n data=", .activeDataSet, ')', sep="")
        doItAndPrint(xyplot.command)
        activateMenus()
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="xyplot")
    tkgrid(getFrame(predictorBox), getFrame(responseBox),
           columnspan=1, sticky="w")
    tkgrid(predictorFrame, sticky="w")
    tkgrid(getFrame(conditionsBox),
           tklabel(cgFrame, text=gettextRcmdr("           ")),
           getFrame(groupsBox),
           columnspan=1, sticky="w")
    tkgrid(cgFrame, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Options"), fg="blue"), sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(x.relationFrame, y.relationFrame, columnspan=2, sticky="w")
    tkgrid(relationFrame, sticky="w")
    tkgrid(tklabel(top, text=gettextRcmdr("Layout"), fg="blue"), sticky="w")
    tkgrid(tklabel(scalarsFrame, text=gettextRcmdr("number of columns:")), layoutColumnsEntry, sticky="w")
    tkgrid(tklabel(scalarsFrame, text=gettextRcmdr("number of rows:")), layoutRowsEntry, sticky="w")
    tkgrid(scalarsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=6, columns=2)
    }
## source("~/HH-R.package/RcmdrPlugin.HH/R/Xyplot.HH.R")
