## Optimize split used Watershed on image

***

Watershed transformation and watershed based object detection.

Usage
watershed(x, tolerance=1, ext=1)
Arguments
x
An Image object or an array.

tolerance
The minimum height of the object in the units of image intensity between its highest point (seed) and the point where it contacts another object (checked for every contact pixel). If the height is smaller than the tolerance, the object will be combined with one of its neighbors, which is the highest. Tolerance should be chosen according to the range of x. Default value is 1, which is a reasonable value if x comes from distmap.

ext
Radius of the neighborhood in pixels for the detection of neighboring objects. Higher value smoothes out small objects.
