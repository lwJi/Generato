/* test.c */
/* Produced with Mathematica */


vreal &rU1 = gf_rU(mask, index2).elts[0];
vreal &rU2 = gf_rU(mask, index2).elts[1];
vreal &rU3 = gf_rU(mask, index2).elts[2];
vreal &uU1 = gf_uU(mask, index5).elts[0];
vreal &uU2 = gf_uU(mask, index5).elts[1];
vreal &uU3 = gf_uU(mask, index5).elts[2];
vreal &MDD11 = gf_MDD(mask, index2).elts[0];
vreal &MDD12 = gf_MDD(mask, index2).elts[1];
vreal &MDD13 = gf_MDD(mask, index2).elts[2];
vreal &MDD22 = gf_MDD(mask, index2).elts[3];
vreal &MDD23 = gf_MDD(mask, index2).elts[4];
vreal &MDD33 = gf_MDD(mask, index2).elts[5];

vreal vU1
=
MDD11*uU1 + MDD12*uU2 + MDD13*uU3
;

vreal vU2
=
MDD12*uU1 + MDD22*uU2 + MDD23*uU3
;

vreal vU3
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
