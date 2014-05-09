<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{pRolocViz application}
%\VignetteKeywords{Visualisation, Organelle, Spatial, Proteomics, Shiny}
%\VignettePackage{pRolocGUI}
-->


pRolocGUI: An interactive tool for visualisation of organelle proteomics data
===========================================================

### Thomas Naake and Laurent Gatto*

#### *Computational Proteomics Unit, University of Cambridge


#### Foreword

This vignette describes the implemented functionality in the
`pRolocGUI` package. The package is based on the `MSnSet` class
defininitions of `MSnbase` [[1](#id1)] and on the functions defined
in the package `pRoloc` [[2](#id2)] and is especially meant for the
analyses of LOPIT [[3](#id3)] or PCP [[4](#id4)] experiments.  To
achieve reactivity and interactivity the function of pRolocGUI was
written in [`shiny`](http://www.rstudio.com/shiny/) notation.  The
implemented application facilitates a higher degree of interactivity
with the underlying data, though, it is limited by its deterministic
nature and will not offer the high flexibility given by the use of a
command based approach. Especially, pRolocGUI addresses to R novices
who have constraints of exploring their data by using the console.

pRolocGUI is under active development; current funtionality is evolving and 
new features will be added. This software is free and open-source. You are
invited to contact Thomas Naake in case you have any questions, suggestions 
or have found any bugs or typos. To reach a broader audience for more general
questions about proteomics analyses using R consider of writing to the 
Bioconductor list. 

-------------------
**Table of Contents**


1. [Introduction](#introduction)  
2. [Tabs of `pRolocVis`](#tabspRolocVis)    
  2.1. [Data](#tabspRolocVisData)  
  2.2. [PCA](#tabspRolocVisPCA)  
  2.3. [protein profiles](#tabspRolocVisPP)    
  2.4. [quantitation](#tabspRolocVisExprs)    
  2.5. [feature meta-data](#tabspRolocVisfData)   
  2.6. [sample meta-data](#tabspRolocVispData)     
  2.7. [search](#tabspRolocVisSearch)  
3. [*Display selection* widget](#display)    
  3.1. [PCA](#displayPCA)   
  3.2. [protein profiles](#displayPP)    
  3.3. [saved searches](#displaySaSe)    
  3.4. [query feature meta-data](#displayQuery)    
4. [References](#references)   
 


-------------------
<a id="introduction"></a> 
### 1. Introduction

Currently, you can start a `pRolocVis` application. 

Starting a `pRolocVis` application is easy as it gets. Just type 


```r
pRolocVis()
```


in your console window and press [Enter]. This will open a new tab in
your Internet browser and after a bit you will see the
application. You can also pass directly a `MSnSet` (e.g. `dunkley2006`
available from the `pRolocdata` experiment package) to `pRolocVis` by
entering


```r
pRolocVis(object = dunkley2006)
```


To stop the application from running press [Esc] or `Ctrl-C` in the
console (or use the "STOP" button when using RStudio) and close the
browser tab, where `pRolocVis` is running.

-------------------
<a id="tabspRolocVis"></a>
### 2. Tabs of `pRolocVis`
To optimize ease of use the interface of `pRolocVis` is subdivided in seven tabs:
* Data,  
* PCA,  
* protein profiles,     
* quantitation,   
* feature meta-data,   
* sample meta-data and  
* search.   

You browse through the tabs by simply clicking on them. Each tab
selected will have a different kind of appearance while some (PCA, protein 
profiles, quantitation and feature meta-data) share a common feature in the 
sidebar, the *Display selection* widget (see 3. *Display selection* widget for further details). 

<a id="tabspRolocVisData"></a>
#### 2.1. Data
The tab **_Data_** is the first tab you will see when you start `pRolocVis`. 
The appearance will differ depending on if you pass directly a `MSnSet` to 
`pRolocVis`. 

If you decide to start `pRolocVis` without assigning a `MSnSet` to the
argument object the tab **_data_** offers possibilities to switch to
an other `MSnSet`.  Currently you may choose between three example
`MSnSets` (derived from experiments of Breckels et al. (2013) [[5]](#id5), 
Dunkley et al. (2006) [3](#id3), Tan et al. (2009) [6](#id6) provided by the package `pRolocdata`) or
to use an `MSnSet` of your choice (own data).  Clicking on
**_Browse..._** will open a dialog window with which you can select
your `MSnSet` and load it to pRolovVis. `pRolocVis` will check if the
selected data file is of class `MSnSet` and if it is an `.Rdata` or
`.rda` file and will denote a notification if there are any conflicts.

If you pass an object to `pRolocVis` the tab data will show if the
`MSnSet` was accepted by `pRolocVis`.

<a id="tabspRolocVisPCA"></a>
#### 2.2. PCA

The tab **_PCA_** is characterized by its main panel which shows a PCA
plot for the selected `MSnSet` and its sidebar panel which is
compartimented into *Display selection* and *Plot*.

Let's forget about the compartiment *Display selection* here (see 3.
*Display selection* widget for further details) and let's turn towards
the *Plot* compartiment. Here we are able to adjust the appearance of
the PCA plot in the main panel. We are able to colour features
(proteins) in matters of common properties by changing the drop-down
list **_colour_**, e.g. if we select for the `MSnSet` originating from
Breckels 2013 the colour "markers", the features in the PCA plot
will be coloured according to their organelle affiliation.  As soon as
we select another colour than "none"
two (or three) new items will be added to the *Plot* widget:   

(1) an item to change the symbol type, **_symbol type_**,   
(2) an item to manipulate legend properties, **_legend_** and 
**_position of legend_** , and  
(3) according to the `MSnSet` used an item to change the point size, 
**_point size_**. 

By selecting one of the variable names in the drop-down list of
**_symbol type_** we will change the symbol type of the features in
the plot. By clicking on the check box to the left of **_legend_** a
legend is added to the plot and by choosing one of the items in the
drop-down list **_position of legend_** below we will change its
position. The third item which might appear is the drop-down list
**_point size_** to change the point size. `pRolocVis` will identify
numeric values in the feature variable labels and will list them in
the drop-down menu (this might be of interest if you have scores which
give information about the quality of a phenoDisco analysis).

By changing the drop-down lists of the items **_number of 1st
principal component_** and **_number of 2nd principal component_** the
x-values and y-values, respectively, will be rendered according to the
new principal components.

To zoom in and out change the drag and drop the little arrows of the slider
of the items **_zoom x-axis_** and **_zoom y-axis_**. This may be of great 
help when you want to identify points in dense clusters.

By clicking on **_Download Plot_** in the main panel below the PCA
plot will open a dialog window with an interface on showing or saving
the PCA plot as it is displayed in the main panel.

<a id="tabspRolocVisPP"></a>
#### 2.3. protein profiles (has to be revised)

The tab **_protein profiles_** shows the protein profiles in the main
panel (with an option of exporting the plot as it is shown in the main
panel by clicking on the button **_Download Plot_**) and the *Display
selection* widget and the *Plot* widget in the sidebar panel. Have a
look on section 3. *Display selection* widget if you want to retrieve
information about how to use the *Display selection* widget.

The *Plot* widget helps to manipulate the plots shown in the main
panel.  Let's assume we want to have a look upon the protein profiles
for the proteins from which we know that they belong to the organelles
endoplasmic reticulum, the golgi apparatus, mitochondrion and the
plasma membrane for the `MSnSet` originating from the experiments of
Breckels (2013).  We have four organelles to look at, so we select "4"
in the drop-down list **_number of plots to display_**. We will select
`markers` in the drop-down list and select `ER` (coding for
endoplasmic reticulum) in the next drow-down list and leave the other
two drop-down lists as they are. To plot the next plot we have to
change the slider **_Select number of plot to change_** to position 2
and for the shiny reactive expressions did not receive yet a new input
we have to change the drop-down lists in order a new plot will be
displayed, accordingly to our problem we will change the second
drop-down list to `Golgi` (coding for golgi apparatus). We proceed
with the two remaining organelles as described before by changing
firstly the slider to the next position and by changing the drop-down
lists addordingly to the organelles we want to display.

We can change the two lower drop-down lists if we want to see some
features compared to all features which are assigned to a variable
name. To see display results of the `phenoDisco` function we can select 
for the `MSnSet` from Breckels 2013 `pd.markers` in the first drop-down list
and `Golgi` as the organelle to look at.  As the comparison we will
choose in the third drop-down list `markers` and `Golgi` which will
display the features in a transparent manner.

<a id="tabspRolocVisExprs"></a>
#### 2.4. quantitation   
The tab **_quantitation_** displays the quantitation data for the proteins as a 
data table. 

In the main panel you can change the number of proteins shown per page and 
search both for proteins (or for the quantitation data). Also, you may sort 
the proteins by name or the quantitation data by clicking on the arrows 
on the top of the data table.

In the sidebar panel the *Display selection* widget is placed as well as radio buttons
to display all data or just selected features (see 3. *Display selection* widget for 
further details)

<a id="tabspRolocVisfData"></a>
#### 2.5. feature meta-data  
The tab **_feature meta-data_** displays the feature meta-data for the proteins
as a data table.

The layout of the tab is similar to the **_quantitation_** tab
and allows for sorting and searching in the feature meta-data of the `MSnSet` 
selected. 

The sidebar conprises the *Display selection* widget and radio buttons to 
show all or only selected features (see 3. *Display selection* widget for 
further details).

<a id="tabspRolocVispData"></a>
#### 2.6. sample meta-data    
The tab **_sample meta-data_** displays the sample meta-data for the 
experiment, the name of the isotopes used for tagging and the associated
fractions. 

<a id="tabspRolocVisSearch"></a>
#### 2.7. search  
`pRolocVis` allows to use past search results to display it in the PCA
plot, protein profiles and in the tabs **_quantitation_** and
**_feature meta-data_** (see 3. *Display selection* widget for further
details if this is your intention).  This ability require the object
`pRolocGUI_SearchResults` in the global environment which is of
class `FeaturesOfInterest` or `FoICollection` (enter
`?FeaturesOfInterest` in the console for further details).

In case this objects exists it will automatically be loaded to
`pRolocVis` and its content is displayed in the tab **_search_**. Use
the drop-down list in the sidebar panel to browse through the
different features of interest in case the object is of class
`FoICollection`.

If no object called `pRolocGUI_SearchResults` exists in the global
environment you can assign an object to the global environment which
is derived from the experiment of Breckels (2013) but will have no
features. To do this, click on **_Initialize saved searches_**, which
will create a new `FoICollection`.

To save features of interest to the object in the global environment you need
to select features and add these to the `FoICollection` by entering an 
appropriate description in the text field (on the sidebar panel, which will
be useful to trace back to the underlying features and doesn't yet exist in 
the `FoICollection`). By clicking on **_Create new features of interest_**.
This will add the selected features to the object `pRolocGUI_SearchResults`
in the global environment.

-------------------
<a id="display"></a>
### 3. *Display selection* widget
The *Display selection* widget is probably the most important implementation 
you can do this by selecting points in the PCA plot, clicking on features 
in the tab **_protein profiles_**, using past searches and/or querying for 
features in the `MSnSet` data.

As you may have already seen there are four check boxes in the
*Display selection* widget which represent the before mentioned ways
of searching features in the `MSnSet`.  To activate the search for one
specific method click on the check box left of its description. It is
also possible to select more than one at a time which allows for
greater flexibility with regard to information retrieval.

<a id="displayPCA"></a>
#### 3.1. PCA 

If you decide to identify proteins in the PCA plot, change to the tab
**_PCA_** and start clicking on features in the PCA plot (tip: the
zoom function may be of great expedient). The check box will be
checked when you start clicking in the PCA plot. As soon as you have
clicked on a feature it will be marked with a black circle around it.
If you have selected a feature by accident or want to deselect it,
just click again on the feature and it will be deselected.

There are two possibilities to deselect all selected features: If you
decide to remove all your features click on **_Clear
features_**. Please keep in mind that this step once carried out is
irreversible.  Besides that you are also able to simply blind out the
selected features by deselecting the check box left of PCA in the
*Display selection* widget.  Internally, the features are still
stored, i.e. by clicking again on the check box you will see the
selections again. Clicking on new proteins in the PCA plot will not
check the check box again, so you have to do this manually. The
features selected are shared between the different tabs. Click on the
tabs **_quantitation_** and **_feature meta-data_** to have a look
upon information about the selected features. For the case where you
see all features in the data table change the radio buttons settings
from **_all_** to **_selected_** at the lowermost widget in the
sidebar. Here again, you can compose the features from different
sources (PCA, protein profiles, saved searches and the text-based
query search).

If you display protein profiles in the tab **_protein profiles_**
selected features will be displayed by black lines with a greater line
width (depending on the features shown).

<a id="displayPP"></a>
#### 3.2. protein profiles

In principle the search for features in protein profiles is in
accordance with the search in the PCA plot. Though, bear in mind that
you are only able to select features when "1" is selected in the
drop-down list **_number of plots to display_**. Clicking on (or near)
the points in the plot will select, clicking another time will
deselect features.  The features will only be shown when the check box
left of **_protein profiles_** is activated.

<a id="displaySaSe"></a>
#### 3.3. saved searches

Clicking on the check box to the left of **_saved searches_** will
load the selected features of the class "FeaturesOfInterest". These
will be displayed in the PCA plot, in the plots for protein profiles
(depending on the displayed features) and will be available in the
tabs **_quantitation_** and **_feature meta-data_** for information
retrieval.  If there is an `FoICollection` loaded change the selected
search result in the drop-down list **_Search result_** in the tab
**_search_**; thus accordingly altering the selected features in the
*Display selection* widget context.

In `pRolocVis` there is no functionality implemented to remove features
from the object `pRolocGUI_SearchResults` in the global environment. The 
authors decided that it is not the task of a GUI to fulfill the requirements 
of this kind of data manipulation in a GUI, hence, the execution of removing
features of interests belongs to the field of the users responsibility.

<a id="displayQuery"></a>
#### 3.4. query feature-meta data

The *Display selection* widget offers the opportunity to query the feature meta-data
of the `MSnSet` for levels. The drop-down list consists of the item `protein`,
which is by definition the rowname of the feature-meta data and depending on 
the data accession number, protein ID, protein description, assigned markers
(varying on the underlying `MSnSet`). 

Let's assume we want to look in the `MSnSet` which was derived from
experiments of Breckels (2013) for all proteins which are assigned by
experimental evidence to the organelle `plasma membrane`. We ensure
ourselves that `Breckels 2013` is selected in the tab **_Data_**
and change to a tab where the *Display selection* widget is loaded. We
activate the check box to the left of **_query_** and select "marker"
in the upper drop-down list (for we are looking for the organelle). In
the next drop-down list below we select `PM` which codes for "plasma
membrane". Next, we click on **_Submit selection_**, which will
highlight all features which are assigned to "PM" for the variable
name `marker`. To remove the selected features from the internal
assignment we have to reset the search by clicking on **_Clear
features_**. Of course, we can also add other features: If we want to
add all features which are assigned to the Golgi apparatus we simply
select `Golgi` in the lower drop-down list and click on **_Submit
selection_** to save internally the selected features.

It is relatively easy to find levels when the drop-down list for these
levels is short (as it is for `markers` in the `MSnSet` originating
from the experiments of Breckels (2013)). But how should we proceed
when we want to look for a special protein, e.g. OCAD1? The drop-down
list for the variable name `protein` is very long and it is time
consuming to scroll through the whole list and look for our protein of
interest. Therefore, we can just enter OCAD1 in the text input field
**_Search for_** in between the two drop-down lists and we will get
the protein of interest (we are also able to query for protein names
which have the string `OC` in their name which will limit the
drop-down list to all proteins which have this specific string). By
clicking on **_Submit selection_** we save internally the
selected feature(s).

<a id="references"></a>
#### 4. References
Authors. Title. Journal, Volume (Issue): Pages. Year. doi: URL:


<a id="id1"></a>
[1] Laurent Gatto and Kathryn S. Lilley. MSnbase - an R/Bioconductor
package for isobaric tagged mass spectrometry data visualization,
processing and quantitation. Bioinformatics 28 (2): 288-289. 2011.
doi: 10.1093/bioinformatics/btr645.
http://www.ncbi.nlm.nih.gov/pubmed/22113085

<a id="id2"></a>
[2] Laurent Gatto, Lisa M. Breckels, Samuel Wieczorek, Thomas Burger, 
Kathryn S. Lilley. Mass-spectrometry- based spatial proteomics data 
analysis using pRoloc and pRolocdata. Bioinformatics, 30 (9): 1322-1324.
2014.
doi: 10.1093/bioinformatics/btu013 
URL http://bioinformatics.oxfordjournals.org/content/30/9/1322

<a id="id3"></a> 
[3] Tom P. J. Dunkley, Svenja Hester, Ian P. Shadforth, John Runions,
Thilo Weimar, Sally L. Hanton, Julian L. Griffin, Conrad Bessant, Federica
Brandizzi, Chris Hawes, Rod B. Watson, Paul Dupree, and Kathryn S. Lilley.
Mapping the arabidopsis organelle proteome. Proc Natl Acad Sci USA, 
103 (17): 6518-6523. 2006. 
doi: 10.1073/pnas.0506958103. 
URL http://dx.doi.org/10.1073/pnas.0506958103.

<a id="id4"></a> 
[4] Leonard J. Foster, Carmen L. de Hoog, Yanling Zhang, Yong Zhang,
Xiaohui Xie, Vamsi K. Mootha, and Matthias Mann. A mammalian organelle
map by protein correlation profiling. Cell, 125 (1): 187-199. 2006.
doi: 10.1016/j.cell.2006.03.022.
URL http://dx.doi.org/10.1016/j.cell.2006.03.022.

<a id="id5"></a>
[5] Breckels L.M., Gatto L., Christoforou A., Groen A.J., 
Lilley K.S. and Trotter M.W.B. The Effect of Organelle Discovery upon Sub-Cellular Protein Localisation. Journal of Proteomics, 88: 129-140. 2013. 
doi: 10.1016/j.jprot.2013.02.019.
URL http://www.ncbi.nlm.nih.gov/pubmed/23523639

<a id="id6"></a>
[6] Denise J. L. Tan, Heidi Dvinge, Andrew Christoforou, Paul Bertone, 
Alfonso Martinez Arias and Kathryn S. Lilley. Mapping organelle proteins 
and protein complexes in Drosophila melanogaster. J. Proteome Res., 
8 (6): 2667-2678. 2009.
doi: 10.1021/pr800866n
URL http://pubs.acs.org/doi/abs/10.1021/pr800866n