# elevate

### Overview
elevate is an R package for charting building elevations using permutations.

### Installation
```
# Install devtools
install.packages("devtools")
library(devtools)

# Install elevate from Github
devtools::install_github("lbuk/elevate")
library(elevate)
```

### Functions and Use
The functions of elevate include: 
* elevations. Type '??elevations' for the documentation.
* extension_elevations. Type '??extension_elevations' for the documentation.

```
# elevations
# elevations visualises building elevations based on the minimum and maximum storey. It also prints the number of combinations in the console. The buildings are shown along the x-axis and the storeys along the y-axis.
# Specify the number of buildings and the minimum and maximum permitted storey. 
elevations(n_buildings = 7, min_storey = 4, max_storey = 5)
```
![](https://github.com/lbuk/elevate/blob/master/img/elevate_elevations_chart_example.png)

```
# extension_elevations
# extension_elevations visualises rooftop extension elevations based on existing elevations and the maximum permitted storey. It additionally prints the number of combinations in the console. The x-axis depicts the buildings and the y-axis the storeys.
# Specify the existing elevations in a dataframe (see example) and the maximum permitted storey.
extension_elevations(df = data.frame(building_1 = 3, building_2 = 2, building_3 = 4, building_4 = 3, building_5 = 3, building_6 = 4, building_7 = 3), max_permitted_storey = 4)
```
![](https://github.com/lbuk/elevate/blob/master/img/elevate_extension_elevations_chart_example.png)
