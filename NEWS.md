# pRolocGUI 2.1

## CHANGES IN VERSION 2.1.4
- fix #109 bug in pRolocVis when 'markers' is missing 
  from fvarLabels

## CHANGES IN VERSION 2.1.3
- added and renames buttons for downloading data
- fixed issue #104 that occurs when marker classes
  have only 1 member

## CHANGES IN VERSION 2.1.2
- re-write of code to fix the breaking changes
  introduced by shinydashboardPlus v2
- depend on shinydashboardPlus 2.0.0
- fixed bug in pRolocVis which fails when no fcol
  explicitly called markers is not present
- added argument classProfiles to compare and explore 
  apps so that users can choose to display class 
  specific profiles

## CHANGES IN VERSION 2.1.1
- fixed typos in vigentte 
- fixed typos in explore and compare apps UIs

# pRolocGUI 2.0

## CHANGES IN VERSION 2.0.0
- New version for Bioc release

# pRolocGUI 1.99

## CHANGES IN VERSION 1.99.2
- fixed warning in profiles plot
- fixed colour matching in factet plot

## CHANGES IN VERSION 1.99.1
- Apps updated to new shinydashboardPlus style
- Options to save plots as high res images 
- User set colourpicker for creating figures
- Support for data export to csv
- Support batch import searching
- PCA app has been replaced with explore app
- New aggregation app
- Classify app removed

# pRolocGUI 1.21

## CHANGES IN VERSION 1.21.3
- Depend on latest pRoloc

## CHANGES IN VERSION 1.21.2
- Keep only one maintainer

## CHANGES IN VERSION 1.21.1
- Update email

# pRolocGUI 1.15

## CHANGES IN VERSION 1.15.2
- Update NEWS and pkgdown site

## CHANGES IN VERSION 1.15.1
- Replace BiocInstaller biocLite mentions with BiocManager

## CHANGES IN VERSION 1.15.0
- New version for Bioc devel

# pRolocGUI 1.14

## CHANGES IN VERSION 1.14.0
- New version for Bioc release

# pRolocGUI 1.13

## CHANGES IN VERSION 1.13.2

- Catch unnecessary idDT error produced on exit <2018-04-23 Mon>

## CHANGES IN VERSION 1.13.1

- Update pRolocVis documentation do show how to use precomputed
  coordinates (see issue #92) <2018-03-16 Fri>

# pRolocGUI 1.12

## CHANGES IN VERSION 1.12.0
- Biocondutor release 3.6

# pRolocGUI 1.11

## CHANGES IN VERSION 1.11.3
- Rename the main app pca app <2017-08-21 Mon>

## CHANGES IN VERSION 1.11.2
- Fix links in vignette to point to new html pRoloc vignettes
  <2017-05-30 Tue>

## CHANGES IN VERSION 1.11.1
- Avoid computing dimensionality reduction at every reactive
  rendering, assuring that other, slower methods, in particular t-SNE,
  can be used <2017-05-20 Sat>


## CHANGES IN VERSION 1.11.0
- New version for Bioc devel 3.6

# pRolocGUI 1.10

## CHANGES IN VERSION 1.10.0
- New version for Bioc release 3.5

# pRolocGUI 1.9

## CHANGES IN VERSION 1.9.5

- Fix broken documentation files <2017-01-18 Wed>
- New aggregation app added <2017-01-22 Wed>

## CHANGES IN VERSION 1.9.4

- fixed remap=FALSE bug in compare app <2017-01-12 Thu>
- Added mirrorX and mirrorY to the compare app <2017-01-12 Thu>

## CHANGES IN VERSION 1.9.3

- Update vignette use latest BiocStyle::html_document2() with
   floating table-f content <2016-12-30 Fri>
- Change to NEWS.md <2016-12-30 Fri>
- mirrorX and mirrorY are now ignored in pRolocVis - see issue #84
  <2017-01-11 Wed>

## CHANGES IN VERSION 1.9.2

- Remove accidental merging left-over <2016-12-13 Tue>

## CHANGES IN VERSION 1.9.1

- Remove accidental call to browser

## CHANGES IN VERSION 1.9.0

- Bioc devel 3.5

# pRoloc 1.8

## CHANGES IN VERSION 1.8.0

- Bioc release 3.4

# pRoloc 1.7

## CHANGES IN VERSION 1.7.5

- Various bug fixes <2016-10-07 Fri>

## CHANGES IN VERSION 1.7.4

- Add DT version dependency (see issue #71) <2016-08-09 Tue>

## CHANGES IN VERSION 1.7.3

- Fixed bug with matrix-f markers in fcol. Now if-ne-f the
   columns in the feature data is a matrix, it is converted to
   a vector using mrkMatToVec. This helps clarity and reduces
   wasted table space-f 1's and 0's. <2016-07-29 Fri>
- Fixed bug in pRolocVis_compare application. Same issue as
   above with passing matrix as a fcol which manifested as an
   extra issue with zooming. Now fixed. <2016-07-29 Fri>

# pRoloc 1.5

## CHANGES IN VERSION 1.5.7

- Fixed bug in pRolocVis in converting GO ids <2016-05-26 Thu>

## CHANGES IN VERSION 1.5.6

- Removing plotMat2D app (closes issue #69) <2016-03-11 Fri>
- add package startup msg <2016-03-11 Fri>
- instruct users to install latest version from github

## CHANGES IN VERSION 1.5.5

- Depend-n DT >= 0.1.40

## CHANGES IN VERSION 1.5.4

- replace getLisacol by getStockcol (which are now Lisa's colours;
   since pRoloc version 1.11.9) <2016-02-16 Tue>

## CHANGES IN VERSION 1.5.3

- update refs in vignette <2016-02-09 Tue>

## CHANGES IN VERSION 1.5.2

- Updated pca app <2016-01-11 Mon>
- Updated vignette <2016-01-12 Tue>
- Fixed bugs, pca app renamed main app, removed
   profiles app <2016-01-14 Thu>
- new compare app <2016-01-30 Sat>
- updated vignette <2016-02-03 Wed>

## CHANGES IN VERSION 1.5.1

- New shiny apps <2015-10-12 Mon>
- New vignette <2015-10-29 Thu>
- Fixed bugs and updated examples in classify app
   <2015-11-09 Mon>

## CHANGES IN VERSION 1.5.0

- Bioc devel 3.3

# pRoloc 1.4

## CHANGES IN VERSION 1.4.0

- Bioc release 3.2

# pRoloc 1.3

## CHANGES IN VERSION 1.3.2

- Fix check warnings and errors <2015-07-20 Mon>

## CHANGES IN VERSION 1.3.1

- new plotMat2D function <2015-05-20 Wed>
- Fix query search in pRolocVis, contributed by pierremj
   <2015-05-27 Wed>
- pRolocVis has new method arg <2015-05-29 Fri>

## CHANGES IN VERSION 1.3.0

- Devel for Bioc 3.2

# pRoloc 1.2

## CHANGES IN VERSION 1.2.0

- Release for Bioc 3.1

# pRoloc 1.1

## CHANGES IN VERSION 1.1.5

- updating failing unit test [2015-04-03 Fri]

## CHANGES IN VERSION 1.1.4

- fix R_HOME error [2015-02-26 Thu]

## CHANGES IN VERSION 1.1.3

- don't require GNU make [2015-02-11 Wed]

## CHANGES IN VERSION 1.1.2

- Fix bug with FoIs and multiple data sets (reported by Harriet
   Parsons) [2015-02-06 Fri]

## CHANGES IN VERSION 1.1.1

- Update README with uptodate installation instructions
   [2014-10-14 Tue]
- handling and filtering missing value in input data [2014-10-21 Tue]

# pRoloc 1.0

## CHANGES IN VERSION 1.0.0

- Release for Bioc 3.0 [2014-10-14 Tue]

# pRoloc 0.99

## CHANGES IN VERSION 0.99.12

- added Video tag in DESCRIPTION [2014-10-07 Tue]

## CHANGES IN VERSION 0.99.11

- Add screenshot to README [2014-09-05 Fri]
- fix bug when features-f fois are not present
- move data tab to the end

## CHANGES IN VERSION 0.99.10

- Updated README [2014-09-04 Thu]
- Remove-ld R code file [2014-09-04]

## CHANGES IN VERSION 0.99.9

- fix vignette error-n Windows (Dan Tenenbaum) [2014-08-26 Tue]

## CHANGES IN VERSION 0.99.8

- selection and display-f multiple features-f interest in pRolocVis and
   pRolocComp [2014-08-13 Wed]

## CHANGES IN VERSION 0.99.7

- display feature meta-data instead-f protein name when hovering
   [2014-07-27 Tue]
- better feature highlighting [2014-07-27 Tue]
--nly 1 vignette [2014-07-27 Tue]

## CHANGES IN VERSION 0.99.6

- support mirroring-f PCA plots in pRolocComp [2014-07-22 Tue]

## CHANGES IN VERSION 0.99.5

- Update to latest knitcitations version and fix vignette
   [2014-07-15 Tue]

## CHANGES IN VERSION 0.99.4

- add function pRolocComp [2014-06-24 Tue]

## CHANGES IN VERSION 0.99.3

- multiple-bjects can be passed to pRolocVis by using a list [2014-06-03 Tue]
- improve query search (submit and select check box) [2014-06-02 Mon]
- add unit tests for helper functions and add manual unit test [2014-06-03 Tue]

## CHANGES IN VERSION 0.99.2

- change access and assignment to-bject pRolocGUI_SearchResults in
   .GlobalEnv [2014-05-30 Fri]

## CHANGES IN VERSION 0.99.1

- fix biocViews [2014-05-27 Tue]
- misc refactoring [2014-05-27 Tue]

## CHANGES IN VERSION 0.99.0

- Bioc submission
