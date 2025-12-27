/* test.hxx */
/* Produced with Generato */

#ifndef TEST_HXX
#define TEST_HXX


auto &rU1 = rU[0];
auto &rU2 = rU[1];
auto &rU3 = rU[2];
auto &uU1 = uU[0];
auto &uU2 = uU[1];
auto &uU3 = uU[2];
auto &MDD11 = MDD[0];
auto &MDD12 = MDD[1];
auto &MDD13 = MDD[2];
auto &MDD22 = MDD[3];
auto &MDD23 = MDD[4];
auto &MDD33 = MDD[5];

const Real
vU1
=
MDD11*uU1 + MDD12*uU2 + MDD13*uU3
;

const Real
vU2
=
MDD12*uU1 + MDD22*uU2 + MDD23*uU3
;

const Real
vU3
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

#endif // #ifndef TEST_HXX

/* test.hxx */
