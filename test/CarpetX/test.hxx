/* test.hxx */
/* Produced with Mathematica */

#ifndef TEST_HXX
#define TEST_HXX


const GF3D2<CCTK_REAL> &local_rU1 = gf_rU(0);
const GF3D2<CCTK_REAL> &local_rU2 = gf_rU(1);
const GF3D2<CCTK_REAL> &local_rU3 = gf_rU(2);
const vreal uU1 = tmp_uU(0);
const vreal uU2 = tmp_uU(1);
const vreal uU3 = tmp_uU(2);
const vreal MDD11 = tmp_MDD(0,0);
const vreal MDD12 = tmp_MDD(0,1);
const vreal MDD13 = tmp_MDD(0,2);
const vreal MDD22 = tmp_MDD(1,1);
const vreal MDD23 = tmp_MDD(1,2);
const vreal MDD33 = tmp_MDD(2,2);

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
local_rU1.store(mask, index2, 
MDD11*vU1 + MDD12*vU2 + MDD13*vU3
);

local_rU2.store(mask, index2, 
MDD12*vU1 + MDD22*vU2 + MDD23*vU3
);

local_rU3.store(mask, index2, 
MDD13*vU1 + MDD23*vU2 + MDD33*vU3
);

}
else
{
local_rU1.store(mask, index2, 
vU1
);

local_rU2.store(mask, index2, 
vU2
);

local_rU3.store(mask, index2, 
vU3
);

}

#endif // #ifndef TEST_HXX

/* test.hxx */
