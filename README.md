# rockyIntertidal
<h3>Package for importing, joining and querying NETN rocky intertidal data</h3>

The first step to using this package is to either execute the <b>importData()</b> function, which loads and names all of the views in the rocky intertidal database into a new environment named ROCKY (default setting) or the global environment. 

Function names that start with "get" pull together data from the views and allow the user to filter by common factors, such as park, locations, years, species, etc. Function names that start with "sum" are higher level functions that summarize 
data and typically have group_by and summarize functions under the hood. Function names starting with "plot" are plotting functions that typically return a ggplot object.
