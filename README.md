# elevate

### Overview
elevate is an R package for visualising building elevations using permutations and charts.

### Installation
```
library(devtools)
install_github("lbuk/elevate")
```

### Use
```
library(elevate)

# Visualise elevations
elevations(nbuildings = 7, min_storey = 3, max_storey = 4, output = 'plot')
```
![](https://github.com/lbuk/elevate/blob/master/img/example_elevate_elevations_charts.png)

```
# Establish the number of storeys per building
storeys = c(4, 5, 4, 4)

# Visualise potential rooftop extension elevations based on the maximum number of storeys per building
extension_elevations(storeys = storeys, max_permitted_storey = 6, output = 'plot')
```
![](https://github.com/lbuk/elevate/blob/master/img/example_elevate_extension_elevations_charts.png)