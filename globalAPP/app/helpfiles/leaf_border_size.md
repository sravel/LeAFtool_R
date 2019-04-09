## Leaf border size

***

When zooming in on the edge of the leaf, you might observe shadows or other anomalies.

This parameter we applies a brush that removes pixels from the edge of the leaf.

Thus, this makes it possible to remove the shadow that might be classified as a lesion during an analysis.

Playing with this parameter (for example increasing it) can also help to identify the correct number of leaves for example when two leaves are very close and confused for  a single leaf .

Exemple with border size = 100px:
<img src="../www/exempleBorder.jpeg" alt="" class="img-responsive">

***
**Info**  The brush is a circle, so the values is rounded to an odd integer
