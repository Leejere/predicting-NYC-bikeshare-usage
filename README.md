CitiBike is a dock-based bike-share system in New York. The successful operation of such a system relies not only on the density and ubiquitousness of stations, but also on the availability of bikes and opening docks in a station. A bike-share trip will not be possible, if there is no available bike at the origin or no opening dock at the destination. However, given the uneven distribution of the directions of bike-share trips at given times of the day, bikes and opening docks tend to be highly unbalanced in space.

To ensure better performance of the bike-share system, re-balancing is the practice of anticipating the demand for bikes and docks and manually redistribute bikes to ensure the availability of bikes and docks when needed.

In this project, I aim to develop an algorithm to predict the trip starts and trip ends across time and space. This could reveal potential bike-dock imbalances and inform the manual distribution of bikes. The re-balancing process consists of two mechanisms:

Giving incentives for riders to ride from certain stations to others. This process is relatively slow and should be allowed ample amounts of time. Predictions will be made two hours ahead of time for the allocation of incentives.

Using a fleet of trucks to move bikes between nearby complimentary stations. Predictions will be made one hour ahead of time to allow for the transportation of bikes.


Final file: Predicting NYC bikeshare usage.html
