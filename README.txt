
List of 44 symbols designating AMBER_ATOM_TYPE extracted from the
parameter files:

  br c c1 c2 c3 ca cc cd ce cf cg cl cp cx cy f h1 h2 h3 h4 h5 ha hc
  hn ho hs i n n1 n3 na nb nc nd nh no o oh os p5 s4 s6 sh ss

These appear  to be  the GAFF  atom types, see  the definition  of the
force-field   parameters   in  gaff.dat   from,   say  AmberTools   or
OpenBabel. There are 71 different GAFF atom types:

  br c c1 c2 c3 ca cc cd ce cf cg ch cl cp cq cu cv cx cy cz f h1 h2
  h3 h4 h5 ha hc hn ho hp hs hw hx i n n1 n2 n3 n4 na nb nc nd ne nf
  nh no o oh os ow p2 p3 p4 p5 pb pc pd pe pf px py s s2 s4 s6 sh ss
  sx sy

All of  the 44  used ones are  listed.  On  the other hand  Mol2 files
refer to only 18 unique SYBYL atom types:

  Br C.1 C.2 C.3 Cl F H I N.1 N.2 N.3 O.2 O.3 P.3 S.1 S.2 S.3 S.O2

The charges  specified in  Mol2 and Amber  parameter files  are mostly
consistent, except  for these three entries listed  here together with
the max deviation of site charges in natural units.

  ("hex_1_yne" 0.3276)
  ("methyl_p_nitrobenzoate" 0.3298)
  ("NN_dimethyl_p_nitrobenzamide" 0.7870999999999999)

For the first  one, "hex_1_yne", the reason might  be a permutation of
the sites.  Site name order in Mol2 and Amber files differ:

 MOL2:  (C2 C3 C4 C5 C6 H1 C1 H2 H3 H4 H5 H6 H7 H8 H9 H10)
 AMBER: (C1 C2 C3 C4 C5 C6 H1 H2 H3 H4 H5 H6 H7 H8 H9 H10)
 TYPES: (c1 c1 c3 c3 c3 c3 ha hc hc hc hc hc hc hc hc hc)

FIXME:  "2_methyl_but_2_ene"  and  "2_methylbut_2_ene" give  identical
results  in QM.   Both were  programmatically assigned  IUPAC  name of
2-methylbut-2-ene by the authors.

