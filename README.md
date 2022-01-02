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
* elevations
* extension_elevations

```
# elevations
# Specify the number of buildings, the minimum and maximum permitted storey. It visualises the elevations and prints the number of combinations in the console.
elevations(n_buildings = 7, min_storey = 4, max_storey = 5)
```
![](https://github.com/lbuk/elevate/blob/master/img/elevations_example.png)

```
# extension_elevations
# Specify the existing elevations in a dataframe (see example) and the maximum permitted storey. It visualises the elevations, the rooftop extensions and prints the number of combinations in the console.
extension_elevations(df = data.frame(building_1 = 3, building_2 = 3, building_3 = 4, building_4 = 4), max_permitted_storey = 6)
```
![](https://github.com/lbuk/elevate/blob/master/img/extension_elevations_example.png)
