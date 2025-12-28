/* test.hxx */
/* Produced with Generato */

#ifndef TEST_HXX
#define TEST_HXX


const auto &rU1 = gf_rU[0];
const auto &rU2 = gf_rU[1];
const auto &rU3 = gf_rU[2];
const auto &uU1 = gf_uU[0];
const auto &uU2 = gf_uU[1];
const auto &uU3 = gf_uU[2];
const auto &MDD11 = gf_MDD[0];
const auto &MDD12 = gf_MDD[1];
const auto &MDD13 = gf_MDD[2];
const auto &MDD22 = gf_MDD[3];
const auto &MDD23 = gf_MDD[4];
const auto &MDD33 = gf_MDD[5];

const vreal
vU1
=
MDD11(p.I)*uU1(p.I) + MDD12(p.I)*uU2(p.I) + MDD13(p.I)*uU3(p.I)
;

const vreal
vU2
=
MDD12(p.I)*uU1(p.I) + MDD22(p.I)*uU2(p.I) + MDD23(p.I)*uU3(p.I)
;

const vreal
vU3
=
MDD13(p.I)*uU1(p.I) + MDD23(p.I)*uU2(p.I) + MDD33(p.I)*uU3(p.I)
;


if(Msqr)
{
rU1(p.I)
=
vU1*MDD11(p.I) + vU2*MDD12(p.I) + vU3*MDD13(p.I)
;

rU2(p.I)
=
vU1*MDD12(p.I) + vU2*MDD22(p.I) + vU3*MDD23(p.I)
;

rU3(p.I)
=
vU1*MDD13(p.I) + vU2*MDD23(p.I) + vU3*MDD33(p.I)
;

}
else
{
rU1(p.I)
=
vU1
;

rU2(p.I)
=
vU2
;

rU3(p.I)
=
vU3
;

}

#endif // #ifndef TEST_HXX

/* test.hxx */
