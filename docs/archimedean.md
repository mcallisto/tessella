# Archimedean tilings
 
There are only 11 combinations of regular p-gons at the full vertex being able to cover an infinite flat surface.

Called “Archimedean” tilings, they are tilings with only 1 type of vertex, that is they are monogonal.

**_Monogonal_**: every vertex, together with its incident edges, forms a figure congruent to that of any other vertex and its incident edges.
It simply means that vertices with its incident edges form one congruent class.

They can be divided into:

## Regular

There are only 3 regular tilings, they are monohedral.

**_Monohedral_**: every tile T<sub><i>i</i></sub> i in the tiling _`T`_ is congruent to one fixed set T, meaning all the
tiles are of the same shape and size.

1. > ![(▲⁶)](growth/(▲⁶).svg)
   >
   > `Mono.expandPattern(Full.s("(3*6)"), 30)`

2. > ![(■⁴)](growth/(■⁴).svg)
   >
   > `Mono.expandPattern(Full.s("(4*4)"), 30)`

3. > ![(⬣³)](growth/(⬣³).svg)
   >
   > `Mono.expandPattern(Full.s("(6*3)"), 30)`

## Semiregular

The other 8 tilings are 2-hedral or 3-hedral.

1. > ![(▲⁴.⬣)](growth/(▲⁴.⬣).svg)
   >
   > `Mono.expandPattern(Full.s("(3*4.6)"), 30)`

2. > ![(▲³.■²)](growth/(▲³.■²).svg)
   >
   > `Mono.expandPattern(Full.s("(3*3.4*2)"), 30)`

3. > ![(▲².■.▲.■)](growth/(▲².■.▲.■).svg)
   >
   > `Mono.expandPattern(Full.s("(3*2.4.3.4)"), 30)`

4. > ![(▲.■.⬣.■)](growth/(▲.■.⬣.■).svg)
   >
   > `Mono.expandPattern(Full.s("(3.4.6.4)"), 30)`

5. > ![(▲.⬣.▲.⬣)](growth/(▲.⬣.▲.⬣).svg)
   >
   > `Mono.expandPattern(Full.s("(3.6.3.6)"), 30)`

6. > ![(▲.12²)](growth/(▲.12²).svg)
   >
   > `Mono.expandPattern(Full.s("(3.12*2)"), 30)`

7. > ![(■.⬣.12)](growth/(■.⬣.12).svg)
   >
   > `Mono.expandPattern(Full.s("(4.6.12)"), 30)`

8. > ![(■.⯃²)](growth/(■.⯃²).svg)
   >
   > `Mono.expandPattern(Full.s("(4.8*2)"), 30)`
