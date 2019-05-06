# Uniformity
 
## Logic

1.  Find all distinct combination of two node
2.  Find all pairs that are not symmetric, the resulting graph is a subset of the complete graph for all their nodes.
3.  Find all pairs that are symmetric but with both nodes existing in 2., the resulting graph is the difference between the complete graph and 2.
4.  Find the different components in 3.

## Uniform constraint

For a tessellation to be uniform, with only one symmetry, a stronger constraint must be added.

The tessellation is valid when:

 1.  all distinct combinations of two nodes
 2.  for each case at least one rotation must be super imposable
 
 ### Addition
 
 Adding a polygon requires these additional checks:
 
 1.  each added node must be paired with each existing node
 2.  for each case at least one rotation must be super imposable
 
 ### Subtraction
 
 Subtracting a polygon is always allowed
 
 



