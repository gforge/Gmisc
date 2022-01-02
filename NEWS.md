NEWS for the Gmisc package

Changes for 3.0.0
-----------------
* **BREAKING:** The input parameters have changed for `getDescriptionStatsBy` and dropped old deprecated parameter conversion:
  * `show_missing_digits` -> `show_missing.digits`
  * `show_missing` -> `useNA`
  * `sig.limit` -> `statistics.sig_lim`
  * `two_dec.limit` -> `statistics.two_dec_lim`
Changes for 2.1.0
-----------------
* getSvdMostInfluential now returns the data needed for plotting
* added fontsize dependent padding to `boxGrob`

Changes for 2.0.2
-----------------
* Minor RCPP changes

Changes for 2.0.1
-----------------
* Minor CRAN check fixes

Changes for 2.0.0
-----------------
* BREAKING: The mergeList merges vectors into vectors instead of assuming that they should be bound as a matrix

Changes for 1.11.0
------------------
* Fix for adjustment in box justification, thanks Carl Suster
* Fix for incorrect boxPropGrob height calculation, bad handling of missing arguments and ignored gpar arguments (issue #46)
* Slightly changed color scheme for boxGrobs
* Added moveBox for moving boxGrob objects
* Added alignVertical and alignHorizontal for aligning boxGrob objects
* Added spreadVertival and spreadHorizontal for spreading boxGrob objects over the viewport
* Styling of htmlTable removed due to the 2.0 api change, nbow. the css.rgroup should be set using set htmlTable::setHtmlTableTheme
  or by passing the output thorugh htmlTable::addHtmlTableStyle.
* The boxes now accept language input, thanks Carl Suster

Changes for 1.10.0
------------------
* Added yamlDump for quick object review
* Added pathJoin for joining paths without worrying about trailing '/'
* Added time2spanTxt for printing time span descriptor

Changes for 1.9.1
-----------------
* Fixed new handling of data.frame stringsAsFactors in next R release

Changes for 1.9.0
-----------------
* Added id as a parameter to the Transtion-class
* Fixed bad no. of popViewport() not adequately leaving the transitionclass viewport tree
* Fixed bad viewport naming in Transition-class that was creating a conflict with multiple plots

Changes for 1.8.1
-----------------
* Fixed cran requirement where class(x) != class(y) could be of length > 1

Changes for 1.8
-----------------
* Added digits.nonzero for allowing values with extra digits where values are close to 0
* Changed sprintf to txtRound for many of the descriptors
* Fixed vignette names for CRAN
* Fixed bug in retrieve

Changes for 1.7
---------------
* Expressions are now allowed in boxGrob (Thanks Alan Haynes)
* Added lodash similar functions 'retrieve' and 'has'

Changes for 1.6.4
-----------------
* Removed changed constant for STRICT_R_HEADERS compliance

Changes for 1.6.3
-----------------
* Fixed minor issue related to new CPP error

Changes for 1.6.1
-----------------
* Removed test for CRAN compliance
* Changed abs to fabs in bezierArrowFn's c++ code

Changes for 1.6
-----------------
* Added skip_shadows to Transition-class

Changes for 1.4.3
-----------------
* Fixes non-existing values in different by-groups (issue #24)

Changes for 1.4.2
-----------------
* Added box functions between and coords
* Added more connectGrob options for arrows
* Added default options for connectGrob, boxGrob and boxGrobProp

Changes for 1.4.1
-----------------
* Fixed merge with master external descGetMissing (Thanks Peter)

Changes for 1.4
---------------
* Added grid grobs for creating boxes and connecting these with lines
* Added checkmate for argument checking

Changes for 1.3.2
-----------------
* Fixed bug when providing getDescriptionStatsBy with custom function
* Added ability add alternative column instead of the p-value column

Changes for 1.3.1
-----------------
* Fixed use_unit issue #16
* Fixed bug in vignette
* Added R-version dependency

Changes for 1.3
-----------------
* The Transition-class can now handle non-square matrix input
* The total column is now also passed through the txtInt function
* Fixed header_count bug in getDescriptionStatsBy - thanks rvlevitin (iss#13)

Changes for 1.2
---------------
* Fixed the naming of proportions in getDescriptionStatsBy
* If a variable has units set it can now be added into the rowname in getDescriptionStatsBy
* Merged with the Grmd-package allowing direct access to the docx_document()
* Prepared for stricter requirements for R 3.3

Changes for 1.1
-----------------
* Created the Transition class that can handle multiple transitions in contrast to transitionPlot
* The bezierArrowGradient has a simpler handling of the gradient proportion
* The bezier arrows have been rewritten with a > 20x speed increase
* The getDescriptionStatsBy function is now accompanied by mergeDesc that takes care of merging
  everything into one table
* Added ability to customize statistics for getDescriptionStatsBy
* transitionPlot: The color_bar_labels are now automatically extracted from the transition_flow input
* transitionPlot: Fixed bug so that the end viewport is the root
* transitionPlot: Fixed grid arrow bug
* transitionPlot: When using 3D matrices and two colors there should only be a colorbar when used with gradient arrows
* transitionPlot: If line width is smaller than the minimum width it is now set to 0

Changes for 1.0
---------------
* htmlTable and some of the formatters have now moved to the 'htmlTable' package
* forestplot2 has now moved to the 'forestplot' package
* added the mergeDesc in order to merge lists from getDescriptionStatsBy for
* All functions default to html = TRUE instead of FALSE
* show_missing is now useNA in order to match the table() call handling of missing
* Fixed a few bugs

Changes for 0.6.8
-----------------
* Applied the DRY-documentation principle

Changes for 0.6.7
-----------------
* Added an interactive() check before using the viewer
* Minor adjustments to the htmlTable handling of no-blank spaces (&nbsp;)
* htmlTable now remember the ... argument and passes it on to the print.htmlTable() and cat()
* Added figCapNo and associated caption function for numbering figure captions
* Improved documentation

Changes for 0.6.6
-----------------
* Improved the htmlTable viewer handling (thanks Adam Smith)

Changes for 0.6.5
-----------------
* Changed the getDescriptionsBy example to work with the htmlTable() instead of
  the Hmisc::latex()
* Fixed getDescriptionsBy bug - the Total column failed when specified to be
  last - thanks Alexandre Vivot
* Added example for side-by side plotting for forestplot2
* As of 0.98.932 the knitr package isn't loaded, instead the metadata or device
  options indicates that it is knitting
* htmlTable bug: The function can now handle matrices without column names
* htmlTable bug: The function deletes and warns when n.rgroup/n.cgroup < 1

Changes for 0.6.4
-------------------
* Bug fix for alt_col not respecting missing rgroup - added also a simple matrix
  output test
* Added visual tests for htmlTable and moved one of the examples there
* Added tableCSSclass option for htmlTable - thanks jphiplip

Changes for 0.6.3
-------------------
* Changed versioning to a 3-digit system as suggested by Yihui Xie
  http://yihui.name/en/2013/06/r-package-versioning/
* Added alt_col for coloring the rows of the htmlTable rgroups - thanks raredd

Changes for 0.6.2.3
-------------------
* Roxygen2 v 4.0.0 upgrade + other doc upgrades
* Percentages are now default without space between the number and the %-sign in
  order to better comply with English language standards

Changes for 0.6.2.2
-------------------
* transitionPlot - fixed arrow size and empty boxes
* outputInt now defaults to the ',' formatting according to the English
  language. It also has no-breaking space if html option is set. It can also
  handle complex vector/matrix input.
* describeMean now allows for custom plus minus sign
* describe* now use the outputInt with the default english settings
* Minor fix for the print.htmlTable

Changes for 0.6.2.1
-------------------
* bezierArrowSmplGradient changed name to just bezierArrowGradient
* Added visual tests and debugged the bezierArrowGradient/transitionPlots
* Externalized all internal functions of the transitionplot
* Added proportions for the transitionplot
* Managed thanks to Baptiste Auguié help on Stack Overflow I know now how to
  calculate the height of axisGrobs - useful both in the forestplot2 and the
  transitionPlot functions
* Fixed the adjustment of the xlabel, the lineheight argument, the viewports for
  the forestplot2 function
* Simplified the transitionPlot defaults
* Fixed a gradient bug in bezierArrowGradient

Changes for 0.6.2.0
-------------------
* Added the viewer for htmlTable allowing for direct visualisation in RStudio of
  the tables. Outside RStudio the utils::browseURL is used.
* htmlTable added table spanner functionality, some man-page update, removed the
  default NULL arguments in favor of the missing() funciton, and a few minor bugfixes.

Changes for 0.6.1.1
-------------------
* The bezierArrowSmpl now uses "mm" internally instead of the default "npc" -
  this improves the rendering as the "npc" is dependent on the axis. A few
  adjustments to arrow functions were also added.
* Bugfixes for the bezierArrowSmplGradient.
* Bugfixed missing data for getDescriptionStats and generated a test-file in
  order to check most combinations that they work
* Fixed the legend-bug for forestplot2 when using points

Changes for 0.6.1.0
--------------------
* Added legend markers option for the forestplot2 function

Changes for 0.6.0.2
--------------------
* Fixed ticks for forestplot2 - the prFpGetGraphTicksAndClips used the exp() for
  regular values.
* Cleaned up some of the code for the fpDrawXXXCI functions


Changes for 0.6.0.1
--------------------
* Changed depends and imports for the package

Changes for 0.6.0.0
--------------------
* For LibreOffice compatibility htmlTable now includes caption inside the table
  when compatibility='LibreOffice'
* Split the package to exclude regression functions, these are now in the Greg
  package
* Added title and label options to transitionPlot
* Added a size-autofit for the transitionPlot text

Changes for 0.5.8.2
--------------------
* htmlTable caused too large colspans when cgroup was set

Changes for 0.5.8.1
--------------------
* Bugfixes for printC&A and getDesc.
* Added a pvalueFormatter
* Fixed Intercept bug for get/printCrudeAndAdjusted - didn't include the
  intercept term

Changes for 0.5.8.0
--------------------
* Added to forestplot2 ability create customly drawn confidence intervals/boxes
* Generated diamond, circle and point custom draw functions for forestplot2
* Added forestplot2 compatibility for the summary
* Minor bugfixes behind the scenes
* Fixed issue with getCrudeAndAdjustedModelData mixing up rows for rms objects

Changes for 0.5.7.11
-------------------
* Added tfoot option to the htmlTable function
* Switched to the proper caption-align for htmlTable
* Added options for htmlTable() where you now can format the table counter text
  and change to Arabic numerals.
* Bug: changed just to align for printCrudeAndAdjustedModel()

Changes for 0.5.7.10
-------------------
* Switched the order in the multi-line forestplot for the legend

Changes for 0.5.7.9
-------------------
* Bugfix for getTics so that it can handle small spans < .5

Changes for 0.5.7.8
-------------------
* Bugfix for forestplot2 with headers

Changes for 0.5.7.7
-------------------
* Added title for forestplot2
* Fixed the margins for forestplot2

Changes for 0.5.7.6
-------------------
* Added more legend border and fill options to the forestplot2
* Added the legend.title option in the forestplot2
* Fixed the auto-sizing of the area for the forestplot2
* Rows/elements with NA are now not included in the zero-line as they are
  considered to be headers

Changes for 0.5.7.5
-------------------
* Added more legend options to the forestplot2
* Minor changes to simleRmsAnova

Changes for 0.5.7.4
-------------------
* Added some debugging info for the getDescriptionStatsBy with more detailed
  errors when unit/total columns don't match

Changes for 0.5.7.4
-------------------
* Externalized many of the forestplot2 functions and added the option of having
  a set line height
* Added new_page option for grid-based plots
* Updated the simpleRmsAnova function

Changes for 0.5.7.3
-------------------
* Removed the plot.new() from transitionplot, this is in order to allow
  splitting plots, adding it to a viewport etc.

Changes for 0.5.7.2
-------------------
* Added the lineheight option for forestplot2
* Fixed the R CMD Check --as-cran to run without any objections
* Updated dependencies

Changes for 0.5.7.1
-------------------
* Fixed rownames in printCrudeAndAdjusted
* Changed printCrudeAndAdjusted to return a matrix subclass instead of
  immediately printing
* Ran through som Pkg-check and hopefully cleared out some bugs

Changes for 0.5.7.0
-------------------
* getCrudeAndAdjusted is now an S3 method that has a default method and a method
  for rms objects. The rms relies on rms:::summaryrms and returns a matrix that
  is pruned and used for the situation.
* printCrudeAndAdjusted has externalized a lot of its functions and handling of
  the new rms getCrudeAndAdjusted output
* printCrudeAndAdjusted now ignores any interaction variables

Changes for 0.5.6.1
-------------------
* forestplot2 fix so that it properly handles data.frames as arguments
* printCrudeAndAdjustedModel has a better handling of the groups argument
* Improved import to importFrom

Changes for 0.5.6.0
-------------------
* The forestplot2 is updated - the widths are now relative to the window size,
  there is a margin option, and many of the inside functions have moved to
  helpers outside. Also fixed the alignment of the summary label to work better.
* Fixed a minor bug for cgroup without rowlabels
* Added default_ref for both getDescriptionStatsBy and describeProp function.
* The percentage sign in getDescriptionStatsBy, describProp and describeFactors
  is now optional.
* A minor bug fix for cgroup.just for the htmlTable function

Changes for 0.5.4.5
-------------------
* Minor fix to rownames in htmlTable... again.
* Had to remove the "terms" option from the plotHR rms:::cph() alternative as it
  did caused an error
** The error seems to stem from predictrms() where the Adjto function deletes the
   adjto output in:
** adjto <- if (int.pres)
**        model.matrix(Terms.ns, adjto)[, -1, drop = FALSE]
** I will have to look into it one day...

Changes for 0.5.4.4
-------------------
* Fixed a rowname problem for htmlTable
* Changed the rowlabel.pos to be by default at the bottom, seems more natural
  with multi-layered tables.

Changes for 0.5.4.3
-------------------
* Fixed a caption problem for htmlTable

Changes for 0.5.4.2
-------------------
* bezierArrowSmpl needed a color for the arrow line as it turned black by
  default. The line was added to fix the anti-aliasing issue.
* Changed CSS-defaults for groups when using the function printCrudeAndAdjusted

Changes for 0.5.4.2
-------------------
* Minor regular expression fix for printCrudeAndAdjusted for the addref. function
* Minor fix for cgroup.just as this was affected by the previous update
* Fixed the htmlTable cgroup.just issue
* The htmlTable is now rendering correct w3c-validated code

Changes for 0.5.4.1
-------------------
* Minor improvement to the htmlTable function with align now accepting vectors
* Added multiline cgroup alternative

Changes for 0.5.4.0
-------------------
* Added a robust alternative for rms ols regressions that now can use the
  sandwich package

Changes for 0.5.3.5
-------------------
* Added mergeLists() function for merging multiple lists

Changes for 0.5.3.4
-------------------
* New options for plotHR

Changes for 0.5.3.3
-------------------
* Minor fixes to the bezier arrows
* Update for transitionPlot to avoid overlapping background arrows

Changes for 0.5.3.2
-------------------
* Thank you Adam Van Iwaarden for fixing the htmlTable alignment and a few bugs!
* Bug fix for prGetModelVariables affecting model with functions as parameters
* Added poisson (link=log) to the getCrudeAndAdjusted function to ret exp() values

Changes for 0.5.3.1
-------------------
* Added transitionPlot ability to indicate proportions in the rectangles

Changes for 0.5.3.0
-------------------
* Added the bezier arrows to the transition plot
* Added a bridge option for the transition plot arrows
* Bugfix for the bezierSmplGradient

Changes for 0.5.2.0
-------------------
* Added the bezier arrows.

Changes for 0.5.1.1
-------------------
* Added lwd parameters to the transitionPlot lines
* Changed the transition matrix - the sums were not necessary as they easily can
  be calculated from the rowSums/colSums.

Changes for 0.5.1.0
-------------------
* Added the transitionPlot

Changes for 0.5.0.2
-------------------
* Bugfix for htmlTable - throws error if a one dimensional x is provided

Changes for 0.5.0.1
-------------------
* Bugfix for plotHR - there were issues with getting the model data
* Added option for having specific digits in printCrudeAndAdjustedModel
  in the model and the descriptive part

Changes for 0.5.0.0
-------------------
* Added the option of having descriptive data to the printCrudeAndAdjustedModel
* A completely new way to extract the model dataset

Changes for 0.4.9.1
-------------------
* Change the option for printCrudeAndAdjustedModel from just html to
  output alternatives html, latex or raw
* Altered the name of the describe_ functions to follow the camelCase
  style, i.e. describe_mean is now describeMean

Changes for 0.4.9.0
-------------------
* Added the getSvdMostInfluential function

Changes for 0.4.8.2
-------------------
* The simpleRmsAnovaLatex now has a better handling of the low p-values
* Fixed a minor but with printCrudeAndAdjusted with the automated reference

Changes for 0.4.8.2
-------------------
* Added the option of hadding a total column even in "vertical" mode
* Changed parameter names hrzl_prop.show_perc to total_col_show_perc
* Bugfix: The total column causes errors if the by contains missing values

Changes for 0.4.8.1
-------------------
* Fixed a few bugs to the forestplot2
