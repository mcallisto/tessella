# Gonality
 
## How to assign gonality to perimeter nodes
  
1. if full vertices exist
   * if only one exists
     * if partial vertex is contained, assume same gonality, can be used for uniformity
     * if not question mark, cannot be used for uniformity
   * if more than one exist
     * if partial vertex is contained in one and not in the others, assume that gonal, can be used for uniformity
     * if partial vertex is contained in two or more, nothing can be assigned
     * if not question mark, cannot be used for uniformity
 
2. otherwise nothing

## Monogonal constraint

For a tessellation to be monogonal, with only one type of vertex, a stronger constraint must be added.

The tessellation is valid when:

 1. if full vertices exist
    * they should be all equal
       * necessary that all nodes not on the perimeter have the same degree
    * each partial (NOT full) vertex must be a subset of the full vertex
 2. otherwise, the combination of all partial vertices, must be:
    * full
    * or fillable, that is not more than 300° (full - triangle)
 
 ### Addition
 
 Adding a polygon requires these additional checks:
 
 1. if full vertices exist
    * the polygon must be an element of the full vertex
    * it will add to 2 or more vertices, and for each of them must
      * complete the full vertex
      * or add to a partial vertex being a subset of the full vertex
 2. otherwise (maybe better to go through main validity)
    * if a full vertex is completed ...
    * if two full vertices are completed ...
    * if two partial vertices ...
    
 
 ### Subtraction
 
 Subtracting a polygon is always allowed
 
 



