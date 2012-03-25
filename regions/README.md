Regions
=======

This library implements a binary tree of regions where each region
is a set of elements. Elements can diffuse from region to region
under certain conditions.

The goal is to end up with higher-scoring elements near the top of
the hierarchy and the lower-scoring elements near the bottom of the
hierarchy, with an even distribution across regions.

The rules are the following:

* Each element is initially added to an arbitrary region called its
  base region.
* An element can move up to its parent or down where it came from, but
  not lower than its base region.
* Periodically a certain fraction of each region moves to the parent,
  left child and right child.
* Each element has a score, used to rank the elements within a region
  and to decide which elements to relocate when the time comes.
  The top fraction of the region will move to the parent region.
  The bottom fraction coming from the left child will go back to the
  left child. Same for the right child.
