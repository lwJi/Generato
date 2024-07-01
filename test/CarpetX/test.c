/* test.c */
/* Produced with Mathematica */


CCTK_REAL &rU1 = gf_rU.elts[0];
CCTK_REAL &rU2 = gf_rU.elts[1];
CCTK_REAL &rU3 = gf_rU.elts[2];
CCTK_REAL &uU1 = gf_uU.elts[0];
CCTK_REAL &uU2 = gf_uU.elts[1];
CCTK_REAL &uU3 = gf_uU.elts[2];
CCTK_REAL &MDD11 = gf_MDD.elts[0];
CCTK_REAL &MDD12 = gf_MDD.elts[1];
CCTK_REAL &MDD13 = gf_MDD.elts[2];
CCTK_REAL &MDD22 = gf_MDD.elts[3];
CCTK_REAL &MDD23 = gf_MDD.elts[4];
CCTK_REAL &MDD33 = gf_MDD.elts[5];

double vU1
=
MDD11*uU1 + MDD12*uU2 + MDD13*uU3
;

double vU2
=
MDD12*uU1 + MDD22*uU2 + MDD23*uU3
;

double vU3
=
MDD13*uU1 + MDD23*uU2 + MDD33*uU3
;


if(Msqr)
{
rU1
=
MDD11*vU1 + MDD12*vU2 + MDD13*vU3
;

rU2
=
MDD12*vU1 + MDD22*vU2 + MDD23*vU3
;

rU3
=
MDD13*vU1 + MDD23*vU2 + MDD33*vU3
;

}
else
{
rU1
=
vU1
;

rU2
=
vU2
;

rU3
=
vU3
;

}

/* test.c */
