/* testGPU.hxx */
/* Produced with Generato */

#ifndef TESTGPU_HXX
#define TESTGPU_HXX


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
MDD11*uU1 + MDD12*uU2 + MDD13*uU3
;

const vreal
vU2
=
MDD12*uU1 + MDD22*uU2 + MDD23*uU3
;

const vreal
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

#endif // #ifndef TESTGPU_HXX

/* testGPU.hxx */
