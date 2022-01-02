# elevate

### Overview
elevate is an R package for charting building elevations based on permutations and planning policies.

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
# elevations visualises the possible elevations based on the minimum and maximum permitted storey. It also prints the number of combinations in the console.
# Specify the number of buildings along the x-axis and the minimum and maximum permitted storey along the y-axis. 
elevations(n_buildings = 7, min_storey = 4, max_storey = 5)
```
![](https://github.com/lbuk/elevate/blob/master/img/elevate_elevations_example.png)

```
# extension_elevations
# extension_elevations visualises building elevations and possible rooftop extensions that could be constructed based on the maximum permitted storey. It additionally prints the number of combinations in the console.
# Specify the existing elevations in a dataframe (see example) along the x-axis and the maximum permitted storey along the y-axis.
extension_elevations(df = data.frame(building_1 = 3, building_2 = 3, building_3 = 4, building_4 = 4), max_permitted_storey = 6)
```
![](https://github.com/lbuk/elevate/blob/master/img/elevate_extension_elevations_example.png)
