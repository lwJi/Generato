/* GHG.c */
/* Produced with Mathematica */

#include "nmesh.h"
#include "GHG.h"

#define Power(x,y) (pow((double) (x),(double) (y)))
#define Log(x) log((double) (x))
#define pow2(x) ((x)*(x))
#define pow2inv(x) (1.0/((x)*(x)))
#define Cal(x,y,z) ((x)?(y):(z))
#define Sqrt(x) sqrt(x)
#define Abs(x) fabs(x)

/* use globals from GHG */
extern tGHG GHG[1];


int GHG_rhs(tNode *node, tVarList *vlr, tVarList *vlu)
{
tMesh *mesh = node->pat->mesh;
int ialpha = Ind("ADM_alpha");
int ibetax = Ind("ADM_betax");
int iAdgtt = Ind("GHG_Adgtt");
int iAdPitt = Ind("GHG_AdPitt");
int iAdPhixtt = Ind("GHG_AdPhixtt");
int isrcSdHtt = Ind("GHG_srcSdHtt");
int fc[6];
int ijk;

double *dtg00 = Vard(node, Vind(vlr,GHG->i_gtt));
double *dtg01 = Vard(node, Vind(vlr,GHG->i_gtt+1));
double *dtg02 = Vard(node, Vind(vlr,GHG->i_gtt+2));
double *dtg03 = Vard(node, Vind(vlr,GHG->i_gtt+3));
double *dtg11 = Vard(node, Vind(vlr,GHG->i_gtt+4));
double *dtg12 = Vard(node, Vind(vlr,GHG->i_gtt+5));
double *dtg13 = Vard(node, Vind(vlr,GHG->i_gtt+6));
double *dtg22 = Vard(node, Vind(vlr,GHG->i_gtt+7));
double *dtg23 = Vard(node, Vind(vlr,GHG->i_gtt+8));
double *dtg33 = Vard(node, Vind(vlr,GHG->i_gtt+9));
double *dtPi00 = Vard(node, Vind(vlr,GHG->i_Pitt));
double *dtPi01 = Vard(node, Vind(vlr,GHG->i_Pitt+1));
double *dtPi02 = Vard(node, Vind(vlr,GHG->i_Pitt+2));
double *dtPi03 = Vard(node, Vind(vlr,GHG->i_Pitt+3));
double *dtPi11 = Vard(node, Vind(vlr,GHG->i_Pitt+4));
double *dtPi12 = Vard(node, Vind(vlr,GHG->i_Pitt+5));
double *dtPi13 = Vard(node, Vind(vlr,GHG->i_Pitt+6));
double *dtPi22 = Vard(node, Vind(vlr,GHG->i_Pitt+7));
double *dtPi23 = Vard(node, Vind(vlr,GHG->i_Pitt+8));
double *dtPi33 = Vard(node, Vind(vlr,GHG->i_Pitt+9));
double *dtPhi000 = Vard(node, Vind(vlr,GHG->i_Phixtt));
double *dtPhi001 = Vard(node, Vind(vlr,GHG->i_Phixtt+1));
double *dtPhi002 = Vard(node, Vind(vlr,GHG->i_Phixtt+2));
double *dtPhi003 = Vard(node, Vind(vlr,GHG->i_Phixtt+3));
double *dtPhi011 = Vard(node, Vind(vlr,GHG->i_Phixtt+4));
double *dtPhi012 = Vard(node, Vind(vlr,GHG->i_Phixtt+5));
double *dtPhi013 = Vard(node, Vind(vlr,GHG->i_Phixtt+6));
double *dtPhi022 = Vard(node, Vind(vlr,GHG->i_Phixtt+7));
double *dtPhi023 = Vard(node, Vind(vlr,GHG->i_Phixtt+8));
double *dtPhi033 = Vard(node, Vind(vlr,GHG->i_Phixtt+9));
double *dtPhi100 = Vard(node, Vind(vlr,GHG->i_Phixtt+10));
double *dtPhi101 = Vard(node, Vind(vlr,GHG->i_Phixtt+11));
double *dtPhi102 = Vard(node, Vind(vlr,GHG->i_Phixtt+12));
double *dtPhi103 = Vard(node, Vind(vlr,GHG->i_Phixtt+13));
double *dtPhi111 = Vard(node, Vind(vlr,GHG->i_Phixtt+14));
double *dtPhi112 = Vard(node, Vind(vlr,GHG->i_Phixtt+15));
double *dtPhi113 = Vard(node, Vind(vlr,GHG->i_Phixtt+16));
double *dtPhi122 = Vard(node, Vind(vlr,GHG->i_Phixtt+17));
double *dtPhi123 = Vard(node, Vind(vlr,GHG->i_Phixtt+18));
double *dtPhi133 = Vard(node, Vind(vlr,GHG->i_Phixtt+19));
double *dtPhi200 = Vard(node, Vind(vlr,GHG->i_Phixtt+20));
double *dtPhi201 = Vard(node, Vind(vlr,GHG->i_Phixtt+21));
double *dtPhi202 = Vard(node, Vind(vlr,GHG->i_Phixtt+22));
double *dtPhi203 = Vard(node, Vind(vlr,GHG->i_Phixtt+23));
double *dtPhi211 = Vard(node, Vind(vlr,GHG->i_Phixtt+24));
double *dtPhi212 = Vard(node, Vind(vlr,GHG->i_Phixtt+25));
double *dtPhi213 = Vard(node, Vind(vlr,GHG->i_Phixtt+26));
double *dtPhi222 = Vard(node, Vind(vlr,GHG->i_Phixtt+27));
double *dtPhi223 = Vard(node, Vind(vlr,GHG->i_Phixtt+28));
double *dtPhi233 = Vard(node, Vind(vlr,GHG->i_Phixtt+29));
double *dtPhi300 = Vard(node, Vind(vlr,GHG->i_Phixtt+30));
double *dtPhi301 = Vard(node, Vind(vlr,GHG->i_Phixtt+31));
double *dtPhi302 = Vard(node, Vind(vlr,GHG->i_Phixtt+32));
double *dtPhi303 = Vard(node, Vind(vlr,GHG->i_Phixtt+33));
double *dtPhi311 = Vard(node, Vind(vlr,GHG->i_Phixtt+34));
double *dtPhi312 = Vard(node, Vind(vlr,GHG->i_Phixtt+35));
double *dtPhi313 = Vard(node, Vind(vlr,GHG->i_Phixtt+36));
double *dtPhi322 = Vard(node, Vind(vlr,GHG->i_Phixtt+37));
double *dtPhi323 = Vard(node, Vind(vlr,GHG->i_Phixtt+38));
double *dtPhi333 = Vard(node, Vind(vlr,GHG->i_Phixtt+39));
double *g00 = Vard(node, Vind(vlu,GHG->i_gtt));
double *g01 = Vard(node, Vind(vlu,GHG->i_gtt+1));
double *g02 = Vard(node, Vind(vlu,GHG->i_gtt+2));
double *g03 = Vard(node, Vind(vlu,GHG->i_gtt+3));
double *g11 = Vard(node, Vind(vlu,GHG->i_gtt+4));
double *g12 = Vard(node, Vind(vlu,GHG->i_gtt+5));
double *g13 = Vard(node, Vind(vlu,GHG->i_gtt+6));
double *g22 = Vard(node, Vind(vlu,GHG->i_gtt+7));
double *g23 = Vard(node, Vind(vlu,GHG->i_gtt+8));
double *g33 = Vard(node, Vind(vlu,GHG->i_gtt+9));
double *Pi00 = Vard(node, Vind(vlu,GHG->i_Pitt));
double *Pi01 = Vard(node, Vind(vlu,GHG->i_Pitt+1));
double *Pi02 = Vard(node, Vind(vlu,GHG->i_Pitt+2));
double *Pi03 = Vard(node, Vind(vlu,GHG->i_Pitt+3));
double *Pi11 = Vard(node, Vind(vlu,GHG->i_Pitt+4));
double *Pi12 = Vard(node, Vind(vlu,GHG->i_Pitt+5));
double *Pi13 = Vard(node, Vind(vlu,GHG->i_Pitt+6));
double *Pi22 = Vard(node, Vind(vlu,GHG->i_Pitt+7));
double *Pi23 = Vard(node, Vind(vlu,GHG->i_Pitt+8));
double *Pi33 = Vard(node, Vind(vlu,GHG->i_Pitt+9));
double *Phi000 = Vard(node, Vind(vlu,GHG->i_Phixtt));
double *Phi001 = Vard(node, Vind(vlu,GHG->i_Phixtt+1));
double *Phi002 = Vard(node, Vind(vlu,GHG->i_Phixtt+2));
double *Phi003 = Vard(node, Vind(vlu,GHG->i_Phixtt+3));
double *Phi011 = Vard(node, Vind(vlu,GHG->i_Phixtt+4));
double *Phi012 = Vard(node, Vind(vlu,GHG->i_Phixtt+5));
double *Phi013 = Vard(node, Vind(vlu,GHG->i_Phixtt+6));
double *Phi022 = Vard(node, Vind(vlu,GHG->i_Phixtt+7));
double *Phi023 = Vard(node, Vind(vlu,GHG->i_Phixtt+8));
double *Phi033 = Vard(node, Vind(vlu,GHG->i_Phixtt+9));
double *Phi100 = Vard(node, Vind(vlu,GHG->i_Phixtt+10));
double *Phi101 = Vard(node, Vind(vlu,GHG->i_Phixtt+11));
double *Phi102 = Vard(node, Vind(vlu,GHG->i_Phixtt+12));
double *Phi103 = Vard(node, Vind(vlu,GHG->i_Phixtt+13));
double *Phi111 = Vard(node, Vind(vlu,GHG->i_Phixtt+14));
double *Phi112 = Vard(node, Vind(vlu,GHG->i_Phixtt+15));
double *Phi113 = Vard(node, Vind(vlu,GHG->i_Phixtt+16));
double *Phi122 = Vard(node, Vind(vlu,GHG->i_Phixtt+17));
double *Phi123 = Vard(node, Vind(vlu,GHG->i_Phixtt+18));
double *Phi133 = Vard(node, Vind(vlu,GHG->i_Phixtt+19));
double *Phi200 = Vard(node, Vind(vlu,GHG->i_Phixtt+20));
double *Phi201 = Vard(node, Vind(vlu,GHG->i_Phixtt+21));
double *Phi202 = Vard(node, Vind(vlu,GHG->i_Phixtt+22));
double *Phi203 = Vard(node, Vind(vlu,GHG->i_Phixtt+23));
double *Phi211 = Vard(node, Vind(vlu,GHG->i_Phixtt+24));
double *Phi212 = Vard(node, Vind(vlu,GHG->i_Phixtt+25));
double *Phi213 = Vard(node, Vind(vlu,GHG->i_Phixtt+26));
double *Phi222 = Vard(node, Vind(vlu,GHG->i_Phixtt+27));
double *Phi223 = Vard(node, Vind(vlu,GHG->i_Phixtt+28));
double *Phi233 = Vard(node, Vind(vlu,GHG->i_Phixtt+29));
double *Phi300 = Vard(node, Vind(vlu,GHG->i_Phixtt+30));
double *Phi301 = Vard(node, Vind(vlu,GHG->i_Phixtt+31));
double *Phi302 = Vard(node, Vind(vlu,GHG->i_Phixtt+32));
double *Phi303 = Vard(node, Vind(vlu,GHG->i_Phixtt+33));
double *Phi311 = Vard(node, Vind(vlu,GHG->i_Phixtt+34));
double *Phi312 = Vard(node, Vind(vlu,GHG->i_Phixtt+35));
double *Phi313 = Vard(node, Vind(vlu,GHG->i_Phixtt+36));
double *Phi322 = Vard(node, Vind(vlu,GHG->i_Phixtt+37));
double *Phi323 = Vard(node, Vind(vlu,GHG->i_Phixtt+38));
double *Phi333 = Vard(node, Vind(vlu,GHG->i_Phixtt+39));
double *H0 = Vard(node, Vind(vlu,GHG->i_Ht));
double *H1 = Vard(node, Vind(vlu,GHG->i_Ht+1));
double *H2 = Vard(node, Vind(vlu,GHG->i_Ht+2));
double *H3 = Vard(node, Vind(vlu,GHG->i_Ht+3));
double *Adg00 = Vard(node, iAdgtt);
double *Adg01 = Vard(node, iAdgtt+1);
double *Adg02 = Vard(node, iAdgtt+2);
double *Adg03 = Vard(node, iAdgtt+3);
double *Adg11 = Vard(node, iAdgtt+4);
double *Adg12 = Vard(node, iAdgtt+5);
double *Adg13 = Vard(node, iAdgtt+6);
double *Adg22 = Vard(node, iAdgtt+7);
double *Adg23 = Vard(node, iAdgtt+8);
double *Adg33 = Vard(node, iAdgtt+9);
double *AdPi00 = Vard(node, iAdPitt);
double *AdPi01 = Vard(node, iAdPitt+1);
double *AdPi02 = Vard(node, iAdPitt+2);
double *AdPi03 = Vard(node, iAdPitt+3);
double *AdPi11 = Vard(node, iAdPitt+4);
double *AdPi12 = Vard(node, iAdPitt+5);
double *AdPi13 = Vard(node, iAdPitt+6);
double *AdPi22 = Vard(node, iAdPitt+7);
double *AdPi23 = Vard(node, iAdPitt+8);
double *AdPi33 = Vard(node, iAdPitt+9);
double *AdPhi000 = Vard(node, iAdPhixtt);
double *AdPhi001 = Vard(node, iAdPhixtt+1);
double *AdPhi002 = Vard(node, iAdPhixtt+2);
double *AdPhi003 = Vard(node, iAdPhixtt+3);
double *AdPhi011 = Vard(node, iAdPhixtt+4);
double *AdPhi012 = Vard(node, iAdPhixtt+5);
double *AdPhi013 = Vard(node, iAdPhixtt+6);
double *AdPhi022 = Vard(node, iAdPhixtt+7);
double *AdPhi023 = Vard(node, iAdPhixtt+8);
double *AdPhi033 = Vard(node, iAdPhixtt+9);
double *AdPhi100 = Vard(node, iAdPhixtt+10);
double *AdPhi101 = Vard(node, iAdPhixtt+11);
double *AdPhi102 = Vard(node, iAdPhixtt+12);
double *AdPhi103 = Vard(node, iAdPhixtt+13);
double *AdPhi111 = Vard(node, iAdPhixtt+14);
double *AdPhi112 = Vard(node, iAdPhixtt+15);
double *AdPhi113 = Vard(node, iAdPhixtt+16);
double *AdPhi122 = Vard(node, iAdPhixtt+17);
double *AdPhi123 = Vard(node, iAdPhixtt+18);
double *AdPhi133 = Vard(node, iAdPhixtt+19);
double *AdPhi200 = Vard(node, iAdPhixtt+20);
double *AdPhi201 = Vard(node, iAdPhixtt+21);
double *AdPhi202 = Vard(node, iAdPhixtt+22);
double *AdPhi203 = Vard(node, iAdPhixtt+23);
double *AdPhi211 = Vard(node, iAdPhixtt+24);
double *AdPhi212 = Vard(node, iAdPhixtt+25);
double *AdPhi213 = Vard(node, iAdPhixtt+26);
double *AdPhi222 = Vard(node, iAdPhixtt+27);
double *AdPhi223 = Vard(node, iAdPhixtt+28);
double *AdPhi233 = Vard(node, iAdPhixtt+29);
double *AdPhi300 = Vard(node, iAdPhixtt+30);
double *AdPhi301 = Vard(node, iAdPhixtt+31);
double *AdPhi302 = Vard(node, iAdPhixtt+32);
double *AdPhi303 = Vard(node, iAdPhixtt+33);
double *AdPhi311 = Vard(node, iAdPhixtt+34);
double *AdPhi312 = Vard(node, iAdPhixtt+35);
double *AdPhi313 = Vard(node, iAdPhixtt+36);
double *AdPhi322 = Vard(node, iAdPhixtt+37);
double *AdPhi323 = Vard(node, iAdPhixtt+38);
double *AdPhi333 = Vard(node, iAdPhixtt+39);
double *alpha = Vard(node, ialpha);
double *beta0 = Vard(node, ibetax);
double *beta1 = Vard(node, ibetax+1);
double *beta2 = Vard(node, ibetax+2);
double *beta3 = Vard(node, ibetax+3);
double *srcSdH00 = Vard(node, isrcSdHtt);
double *srcSdH01 = Vard(node, isrcSdHtt+1);
double *srcSdH02 = Vard(node, isrcSdHtt+2);
double *srcSdH03 = Vard(node, isrcSdHtt+3);
double *srcSdH11 = Vard(node, isrcSdHtt+4);
double *srcSdH12 = Vard(node, isrcSdHtt+5);
double *srcSdH13 = Vard(node, isrcSdHtt+6);
double *srcSdH22 = Vard(node, isrcSdHtt+7);
double *srcSdH23 = Vard(node, isrcSdHtt+8);
double *srcSdH33 = Vard(node, isrcSdHtt+9);

TIMER_START;

/* compute */
forpoints(node, ijk)
{
double interior = !(ind_on_nodeface(node, ijk, fc));
double gammas[3], gamma0, gamma1, gamma2;
GHG_gammas(node, ijk, gammas);
gamma0 = gammas[0];
gamma1 = gammas[1];
gamma2 = gammas[2];

double detinvh
=
detinvh$RHS()
;

double invh00
=
invh$RHS(List(0,cart),List(0,cart))
;

double invh01
=
invh$RHS(List(0,cart),List(1,cart))
;

double invh02
=
invh$RHS(List(0,cart),List(2,cart))
;

double invh03
=
invh$RHS(List(0,cart),List(3,cart))
;

double invh11
=
invh$RHS(List(1,cart),List(1,cart))
;

double invh12
=
invh$RHS(List(1,cart),List(2,cart))
;

double invh13
=
invh$RHS(List(1,cart),List(3,cart))
;

double invh22
=
invh$RHS(List(2,cart),List(2,cart))
;

double invh23
=
invh$RHS(List(2,cart),List(3,cart))
;

double invh33
=
invh$RHS(List(3,cart),List(3,cart))
;

double nvec0
=
1/alpha[ijk]
;

double nvec1
=
-(beta1[ijk]/alpha[ijk])
;

double nvec2
=
-(beta2[ijk]/alpha[ijk])
;

double nvec3
=
-(beta3[ijk]/alpha[ijk])
;

double ndua0
=
-alpha[ijk]
;

double ndua1
=
0.
;

double ndua2
=
0.
;

double ndua3
=
0.
;

double invg00
=
invh00 - Power(nvec0,2)
;

double invg01
=
invh01 - nvec0*nvec1
;

double invg02
=
invh02 - nvec0*nvec2
;

double invg03
=
invh03 - nvec0*nvec3
;

double invg11
=
invh11 - Power(nvec1,2)
;

double invg12
=
invh12 - nvec1*nvec2
;

double invg13
=
invh13 - nvec1*nvec3
;

double invg22
=
invh22 - Power(nvec2,2)
;

double invg23
=
invh23 - nvec2*nvec3
;

double invg33
=
invh33 - Power(nvec3,2)
;

double dginFO000
=
beta0[ijk]*Phi000[ijk] + beta1[ijk]*Phi100[ijk] + beta2[ijk]*Phi200[ijk] + 
  beta3[ijk]*Phi300[ijk] - alpha[ijk]*Pi00[ijk]
;

double dginFO001
=
beta0[ijk]*Phi001[ijk] + beta1[ijk]*Phi101[ijk] + beta2[ijk]*Phi201[ijk] + 
  beta3[ijk]*Phi301[ijk] - alpha[ijk]*Pi01[ijk]
;

double dginFO002
=
beta0[ijk]*Phi002[ijk] + beta1[ijk]*Phi102[ijk] + beta2[ijk]*Phi202[ijk] + 
  beta3[ijk]*Phi302[ijk] - alpha[ijk]*Pi02[ijk]
;

double dginFO003
=
beta0[ijk]*Phi003[ijk] + beta1[ijk]*Phi103[ijk] + beta2[ijk]*Phi203[ijk] + 
  beta3[ijk]*Phi303[ijk] - alpha[ijk]*Pi03[ijk]
;

double dginFO011
=
beta0[ijk]*Phi011[ijk] + beta1[ijk]*Phi111[ijk] + beta2[ijk]*Phi211[ijk] + 
  beta3[ijk]*Phi311[ijk] - alpha[ijk]*Pi11[ijk]
;

double dginFO012
=
beta0[ijk]*Phi012[ijk] + beta1[ijk]*Phi112[ijk] + beta2[ijk]*Phi212[ijk] + 
  beta3[ijk]*Phi312[ijk] - alpha[ijk]*Pi12[ijk]
;

double dginFO013
=
beta0[ijk]*Phi013[ijk] + beta1[ijk]*Phi113[ijk] + beta2[ijk]*Phi213[ijk] + 
  beta3[ijk]*Phi313[ijk] - alpha[ijk]*Pi13[ijk]
;

double dginFO022
=
beta0[ijk]*Phi022[ijk] + beta1[ijk]*Phi122[ijk] + beta2[ijk]*Phi222[ijk] + 
  beta3[ijk]*Phi322[ijk] - alpha[ijk]*Pi22[ijk]
;

double dginFO023
=
beta0[ijk]*Phi023[ijk] + beta1[ijk]*Phi123[ijk] + beta2[ijk]*Phi223[ijk] + 
  beta3[ijk]*Phi323[ijk] - alpha[ijk]*Pi23[ijk]
;

double dginFO033
=
beta0[ijk]*Phi033[ijk] + beta1[ijk]*Phi133[ijk] + beta2[ijk]*Phi233[ijk] + 
  beta3[ijk]*Phi333[ijk] - alpha[ijk]*Pi33[ijk]
;

double dginFO100
=
Phi100[ijk]
;

double dginFO101
=
Phi101[ijk]
;

double dginFO102
=
Phi102[ijk]
;

double dginFO103
=
Phi103[ijk]
;

double dginFO111
=
Phi111[ijk]
;

double dginFO112
=
Phi112[ijk]
;

double dginFO113
=
Phi113[ijk]
;

double dginFO122
=
Phi122[ijk]
;

double dginFO123
=
Phi123[ijk]
;

double dginFO133
=
Phi133[ijk]
;

double dginFO200
=
Phi200[ijk]
;

double dginFO201
=
Phi201[ijk]
;

double dginFO202
=
Phi202[ijk]
;

double dginFO203
=
Phi203[ijk]
;

double dginFO211
=
Phi211[ijk]
;

double dginFO212
=
Phi212[ijk]
;

double dginFO213
=
Phi213[ijk]
;

double dginFO222
=
Phi222[ijk]
;

double dginFO223
=
Phi223[ijk]
;

double dginFO233
=
Phi233[ijk]
;

double dginFO300
=
Phi300[ijk]
;

double dginFO301
=
Phi301[ijk]
;

double dginFO302
=
Phi302[ijk]
;

double dginFO303
=
Phi303[ijk]
;

double dginFO311
=
Phi311[ijk]
;

double dginFO312
=
Phi312[ijk]
;

double dginFO313
=
Phi313[ijk]
;

double dginFO322
=
Phi322[ijk]
;

double dginFO323
=
Phi323[ijk]
;

double dginFO333
=
Phi333[ijk]
;

double Gam000
=
dginFO000/2.
;

double Gam001
=
dginFO100/2.
;

double Gam002
=
dginFO200/2.
;

double Gam003
=
dginFO300/2.
;

double Gam011
=
-0.5*dginFO011 + dginFO101
;

double Gam012
=
(-dginFO012 + dginFO102 + dginFO201)/2.
;

double Gam013
=
(-dginFO013 + dginFO103 + dginFO301)/2.
;

double Gam022
=
-0.5*dginFO022 + dginFO202
;

double Gam023
=
(-dginFO023 + dginFO203 + dginFO302)/2.
;

double Gam033
=
-0.5*dginFO033 + dginFO303
;

double Gam100
=
dginFO001 - dginFO100/2.
;

double Gam101
=
dginFO011/2.
;

double Gam102
=
(dginFO012 - dginFO102 + dginFO201)/2.
;

double Gam103
=
(dginFO013 - dginFO103 + dginFO301)/2.
;

double Gam111
=
dginFO111/2.
;

double Gam112
=
dginFO211/2.
;

double Gam113
=
dginFO311/2.
;

double Gam122
=
-0.5*dginFO122 + dginFO212
;

double Gam123
=
(-dginFO123 + dginFO213 + dginFO312)/2.
;

double Gam133
=
-0.5*dginFO133 + dginFO313
;

double Gam200
=
dginFO002 - dginFO200/2.
;

double Gam201
=
(dginFO012 + dginFO102 - dginFO201)/2.
;

double Gam202
=
dginFO022/2.
;

double Gam203
=
(dginFO023 - dginFO203 + dginFO302)/2.
;

double Gam211
=
dginFO112 - dginFO211/2.
;

double Gam212
=
dginFO122/2.
;

double Gam213
=
(dginFO123 - dginFO213 + dginFO312)/2.
;

double Gam222
=
dginFO222/2.
;

double Gam223
=
dginFO322/2.
;

double Gam233
=
-0.5*dginFO233 + dginFO323
;

double Gam300
=
dginFO003 - dginFO300/2.
;

double Gam301
=
(dginFO013 + dginFO103 - dginFO301)/2.
;

double Gam302
=
(dginFO023 + dginFO203 - dginFO302)/2.
;

double Gam303
=
dginFO033/2.
;

double Gam311
=
dginFO113 - dginFO311/2.
;

double Gam312
=
(dginFO123 + dginFO213 - dginFO312)/2.
;

double Gam313
=
dginFO133/2.
;

double Gam322
=
dginFO223 - dginFO322/2.
;

double Gam323
=
dginFO233/2.
;

double Gam333
=
dginFO333/2.
;

double trGam0
=
Gam000*invg00 + 2*Gam001*invg01 + 2*Gam002*invg02 + 2*Gam003*invg03 + 
  Gam011*invg11 + 2*Gam012*invg12 + 2*Gam013*invg13 + Gam022*invg22 + 
  2*Gam023*invg23 + Gam033*invg33
;

double trGam1
=
Gam100*invg00 + 2*Gam101*invg01 + 2*Gam102*invg02 + 2*Gam103*invg03 + 
  Gam111*invg11 + 2*Gam112*invg12 + 2*Gam113*invg13 + Gam122*invg22 + 
  2*Gam123*invg23 + Gam133*invg33
;

double trGam2
=
Gam200*invg00 + 2*Gam201*invg01 + 2*Gam202*invg02 + 2*Gam203*invg03 + 
  Gam211*invg11 + 2*Gam212*invg12 + 2*Gam213*invg13 + Gam222*invg22 + 
  2*Gam223*invg23 + Gam233*invg33
;

double trGam3
=
Gam300*invg00 + 2*Gam301*invg01 + 2*Gam302*invg02 + 2*Gam303*invg03 + 
  Gam311*invg11 + 2*Gam312*invg12 + 2*Gam313*invg13 + Gam322*invg22 + 
  2*Gam323*invg23 + Gam333*invg33
;


dtg00[ijk]
=
-(interior*Adg00[ijk]) - gamma1*beta0[ijk]*Phi000[ijk] - 
  gamma1*beta1[ijk]*Phi100[ijk] - gamma1*beta2[ijk]*Phi200[ijk] - 
  gamma1*beta3[ijk]*Phi300[ijk] - alpha[ijk]*Pi00[ijk]
;

dtg01[ijk]
=
-(interior*Adg01[ijk]) - gamma1*beta0[ijk]*Phi001[ijk] - 
  gamma1*beta1[ijk]*Phi101[ijk] - gamma1*beta2[ijk]*Phi201[ijk] - 
  gamma1*beta3[ijk]*Phi301[ijk] - alpha[ijk]*Pi01[ijk]
;

dtg02[ijk]
=
-(interior*Adg02[ijk]) - gamma1*beta0[ijk]*Phi002[ijk] - 
  gamma1*beta1[ijk]*Phi102[ijk] - gamma1*beta2[ijk]*Phi202[ijk] - 
  gamma1*beta3[ijk]*Phi302[ijk] - alpha[ijk]*Pi02[ijk]
;

dtg03[ijk]
=
-(interior*Adg03[ijk]) - gamma1*beta0[ijk]*Phi003[ijk] - 
  gamma1*beta1[ijk]*Phi103[ijk] - gamma1*beta2[ijk]*Phi203[ijk] - 
  gamma1*beta3[ijk]*Phi303[ijk] - alpha[ijk]*Pi03[ijk]
;

dtg11[ijk]
=
-(interior*Adg11[ijk]) - gamma1*beta0[ijk]*Phi011[ijk] - 
  gamma1*beta1[ijk]*Phi111[ijk] - gamma1*beta2[ijk]*Phi211[ijk] - 
  gamma1*beta3[ijk]*Phi311[ijk] - alpha[ijk]*Pi11[ijk]
;

dtg12[ijk]
=
-(interior*Adg12[ijk]) - gamma1*beta0[ijk]*Phi012[ijk] - 
  gamma1*beta1[ijk]*Phi112[ijk] - gamma1*beta2[ijk]*Phi212[ijk] - 
  gamma1*beta3[ijk]*Phi312[ijk] - alpha[ijk]*Pi12[ijk]
;

dtg13[ijk]
=
-(interior*Adg13[ijk]) - gamma1*beta0[ijk]*Phi013[ijk] - 
  gamma1*beta1[ijk]*Phi113[ijk] - gamma1*beta2[ijk]*Phi213[ijk] - 
  gamma1*beta3[ijk]*Phi313[ijk] - alpha[ijk]*Pi13[ijk]
;

dtg22[ijk]
=
-(interior*Adg22[ijk]) - gamma1*beta0[ijk]*Phi022[ijk] - 
  gamma1*beta1[ijk]*Phi122[ijk] - gamma1*beta2[ijk]*Phi222[ijk] - 
  gamma1*beta3[ijk]*Phi322[ijk] - alpha[ijk]*Pi22[ijk]
;

dtg23[ijk]
=
-(interior*Adg23[ijk]) - gamma1*beta0[ijk]*Phi023[ijk] - 
  gamma1*beta1[ijk]*Phi123[ijk] - gamma1*beta2[ijk]*Phi223[ijk] - 
  gamma1*beta3[ijk]*Phi323[ijk] - alpha[ijk]*Pi23[ijk]
;

dtg33[ijk]
=
-(interior*Adg33[ijk]) - gamma1*beta0[ijk]*Phi033[ijk] - 
  gamma1*beta1[ijk]*Phi133[ijk] - gamma1*beta2[ijk]*Phi233[ijk] - 
  gamma1*beta3[ijk]*Phi333[ijk] - alpha[ijk]*Pi33[ijk]
;

dtPi00[ijk]
=
-(interior*AdPi00[ijk]) - gamma1*gamma2*beta0[ijk]*Phi000[ijk] - 
  gamma1*gamma2*beta1[ijk]*Phi100[ijk] - 
  gamma1*gamma2*beta2[ijk]*Phi200[ijk] - 
  gamma1*gamma2*beta3[ijk]*Phi300[ijk] - 
  (alpha[ijk]*(4*Power(Gam000,2)*Power(invg00,2) + 
       16*Gam000*Gam001*invg00*invg01 + 
       8*Power(Gam001,2)*Power(invg01,2) + 
       8*Gam000*Gam011*Power(invg01,2) + 16*Gam000*Gam002*invg00*invg02 + 
       16*Gam001*Gam002*invg01*invg02 + 16*Gam000*Gam012*invg01*invg02 + 
       8*Power(Gam002,2)*Power(invg02,2) + 
       8*Gam000*Gam022*Power(invg02,2) + 16*Gam000*Gam003*invg00*invg03 + 
       16*Gam001*Gam003*invg01*invg03 + 16*Gam000*Gam013*invg01*invg03 + 
       16*Gam002*Gam003*invg02*invg03 + 16*Gam000*Gam023*invg02*invg03 + 
       8*Power(Gam003,2)*Power(invg03,2) + 
       8*Gam000*Gam033*Power(invg03,2) + 8*Power(Gam001,2)*invg00*invg11 + 
       16*Gam001*Gam011*invg01*invg11 + 16*Gam001*Gam012*invg02*invg11 + 
       16*Gam001*Gam013*invg03*invg11 + 
       4*Power(Gam011,2)*Power(invg11,2) + 
       16*Gam001*Gam002*invg00*invg12 + 16*Gam002*Gam011*invg01*invg12 + 
       16*Gam001*Gam012*invg01*invg12 + 16*Gam002*Gam012*invg02*invg12 + 
       16*Gam001*Gam022*invg02*invg12 + 16*Gam002*Gam013*invg03*invg12 + 
       16*Gam001*Gam023*invg03*invg12 + 16*Gam011*Gam012*invg11*invg12 + 
       8*Power(Gam012,2)*Power(invg12,2) + 
       8*Gam011*Gam022*Power(invg12,2) + 16*Gam001*Gam003*invg00*invg13 + 
       16*Gam003*Gam011*invg01*invg13 + 16*Gam001*Gam013*invg01*invg13 + 
       16*Gam003*Gam012*invg02*invg13 + 16*Gam001*Gam023*invg02*invg13 + 
       16*Gam003*Gam013*invg03*invg13 + 16*Gam001*Gam033*invg03*invg13 + 
       16*Gam011*Gam013*invg11*invg13 + 16*Gam012*Gam013*invg12*invg13 + 
       16*Gam011*Gam023*invg12*invg13 + 
       8*Power(Gam013,2)*Power(invg13,2) + 
       8*Gam011*Gam033*Power(invg13,2) + 8*Power(Gam002,2)*invg00*invg22 + 
       16*Gam002*Gam012*invg01*invg22 + 16*Gam002*Gam022*invg02*invg22 + 
       16*Gam002*Gam023*invg03*invg22 + 8*Power(Gam012,2)*invg11*invg22 + 
       16*Gam012*Gam022*invg12*invg22 + 16*Gam012*Gam023*invg13*invg22 + 
       4*Power(Gam022,2)*Power(invg22,2) + 
       16*Gam002*Gam003*invg00*invg23 + 16*Gam003*Gam012*invg01*invg23 + 
       16*Gam002*Gam013*invg01*invg23 + 16*Gam003*Gam022*invg02*invg23 + 
       16*Gam002*Gam023*invg02*invg23 + 16*Gam003*Gam023*invg03*invg23 + 
       16*Gam002*Gam033*invg03*invg23 + 16*Gam012*Gam013*invg11*invg23 + 
       16*Gam013*Gam022*invg12*invg23 + 16*Gam012*Gam023*invg12*invg23 + 
       16*Gam013*Gam023*invg13*invg23 + 16*Gam012*Gam033*invg13*invg23 + 
       16*Gam022*Gam023*invg22*invg23 + 
       8*Power(Gam023,2)*Power(invg23,2) + 
       8*Gam022*Gam033*Power(invg23,2) + 8*Power(Gam003,2)*invg00*invg33 + 
       16*Gam003*Gam013*invg01*invg33 + 16*Gam003*Gam023*invg02*invg33 + 
       16*Gam003*Gam033*invg03*invg33 + 8*Power(Gam013,2)*invg11*invg33 + 
       16*Gam013*Gam023*invg12*invg33 + 16*Gam013*Gam033*invg13*invg33 + 
       8*Power(Gam023,2)*invg22*invg33 + 16*Gam023*Gam033*invg23*invg33 + 
       4*Power(Gam033,2)*Power(invg33,2) - 4*gamma0*ndua0*trGam0 - 
       4*(Gam000*invg00 + Gam100*invg01 + Gam200*invg02 + 
          Gam300*invg03 + gamma0*ndua0)*H0[ijk] - 
       4*Gam000*invg01*H1[ijk] - 4*Gam100*invg11*H1[ijk] - 
       4*Gam200*invg12*H1[ijk] - 4*Gam300*invg13*H1[ijk] - 
       4*Gam000*invg02*H2[ijk] - 4*Gam100*invg12*H2[ijk] - 
       4*Gam200*invg22*H2[ijk] - 4*Gam300*invg23*H2[ijk] - 
       4*Gam000*invg03*H3[ijk] - 4*Gam100*invg13*H3[ijk] - 
       4*Gam200*invg23*H3[ijk] - 4*Gam300*invg33*H3[ijk] + 
       2*gamma0*g00[ijk]*(nvec0*trGam0 + nvec1*trGam1 + nvec2*trGam2 + 
          nvec3*trGam3 + nvec0*H0[ijk] + nvec1*H1[ijk] + nvec2*H2[ijk] + 
          nvec3*H3[ijk]) - 4*invg00*invh00*Power(Phi000[ijk],2) - 
       8*invg01*invh00*Phi000[ijk]*Phi001[ijk] - 
       4*invg11*invh00*Power(Phi001[ijk],2) - 
       8*invg02*invh00*Phi000[ijk]*Phi002[ijk] - 
       8*invg12*invh00*Phi001[ijk]*Phi002[ijk] - 
       4*invg22*invh00*Power(Phi002[ijk],2) - 
       8*invg03*invh00*Phi000[ijk]*Phi003[ijk] - 
       8*invg13*invh00*Phi001[ijk]*Phi003[ijk] - 
       8*invg23*invh00*Phi002[ijk]*Phi003[ijk] - 
       4*invg33*invh00*Power(Phi003[ijk],2) - 
       8*invg00*invh01*Phi000[ijk]*Phi100[ijk] - 
       8*invg01*invh01*Phi001[ijk]*Phi100[ijk] - 
       8*invg02*invh01*Phi002[ijk]*Phi100[ijk] - 
       8*invg03*invh01*Phi003[ijk]*Phi100[ijk] - 
       4*invg00*invh11*Power(Phi100[ijk],2) - 
       8*invg01*invh01*Phi000[ijk]*Phi101[ijk] - 
       8*invg11*invh01*Phi001[ijk]*Phi101[ijk] - 
       8*invg12*invh01*Phi002[ijk]*Phi101[ijk] - 
       8*invg13*invh01*Phi003[ijk]*Phi101[ijk] - 
       8*invg01*invh11*Phi100[ijk]*Phi101[ijk] - 
       4*invg11*invh11*Power(Phi101[ijk],2) - 
       8*invg02*invh01*Phi000[ijk]*Phi102[ijk] - 
       8*invg12*invh01*Phi001[ijk]*Phi102[ijk] - 
       8*invg22*invh01*Phi002[ijk]*Phi102[ijk] - 
       8*invg23*invh01*Phi003[ijk]*Phi102[ijk] - 
       8*invg02*invh11*Phi100[ijk]*Phi102[ijk] - 
       8*invg12*invh11*Phi101[ijk]*Phi102[ijk] - 
       4*invg22*invh11*Power(Phi102[ijk],2) - 
       8*invg03*invh01*Phi000[ijk]*Phi103[ijk] - 
       8*invg13*invh01*Phi001[ijk]*Phi103[ijk] - 
       8*invg23*invh01*Phi002[ijk]*Phi103[ijk] - 
       8*invg33*invh01*Phi003[ijk]*Phi103[ijk] - 
       8*invg03*invh11*Phi100[ijk]*Phi103[ijk] - 
       8*invg13*invh11*Phi101[ijk]*Phi103[ijk] - 
       8*invg23*invh11*Phi102[ijk]*Phi103[ijk] - 
       4*invg33*invh11*Power(Phi103[ijk],2) - 
       8*invg00*invh02*Phi000[ijk]*Phi200[ijk] - 
       8*invg01*invh02*Phi001[ijk]*Phi200[ijk] - 
       8*invg02*invh02*Phi002[ijk]*Phi200[ijk] - 
       8*invg03*invh02*Phi003[ijk]*Phi200[ijk] - 
       8*invg00*invh12*Phi100[ijk]*Phi200[ijk] - 
       8*invg01*invh12*Phi101[ijk]*Phi200[ijk] - 
       8*invg02*invh12*Phi102[ijk]*Phi200[ijk] - 
       8*invg03*invh12*Phi103[ijk]*Phi200[ijk] - 
       4*invg00*invh22*Power(Phi200[ijk],2) - 
       8*invg01*invh02*Phi000[ijk]*Phi201[ijk] - 
       8*invg11*invh02*Phi001[ijk]*Phi201[ijk] - 
       8*invg12*invh02*Phi002[ijk]*Phi201[ijk] - 
       8*invg13*invh02*Phi003[ijk]*Phi201[ijk] - 
       8*invg01*invh12*Phi100[ijk]*Phi201[ijk] - 
       8*invg11*invh12*Phi101[ijk]*Phi201[ijk] - 
       8*invg12*invh12*Phi102[ijk]*Phi201[ijk] - 
       8*invg13*invh12*Phi103[ijk]*Phi201[ijk] - 
       8*invg01*invh22*Phi200[ijk]*Phi201[ijk] - 
       4*invg11*invh22*Power(Phi201[ijk],2) - 
       8*invg02*invh02*Phi000[ijk]*Phi202[ijk] - 
       8*invg12*invh02*Phi001[ijk]*Phi202[ijk] - 
       8*invg22*invh02*Phi002[ijk]*Phi202[ijk] - 
       8*invg23*invh02*Phi003[ijk]*Phi202[ijk] - 
       8*invg02*invh12*Phi100[ijk]*Phi202[ijk] - 
       8*invg12*invh12*Phi101[ijk]*Phi202[ijk] - 
       8*invg22*invh12*Phi102[ijk]*Phi202[ijk] - 
       8*invg23*invh12*Phi103[ijk]*Phi202[ijk] - 
       8*invg02*invh22*Phi200[ijk]*Phi202[ijk] - 
       8*invg12*invh22*Phi201[ijk]*Phi202[ijk] - 
       4*invg22*invh22*Power(Phi202[ijk],2) - 
       8*invg03*invh02*Phi000[ijk]*Phi203[ijk] - 
       8*invg13*invh02*Phi001[ijk]*Phi203[ijk] - 
       8*invg23*invh02*Phi002[ijk]*Phi203[ijk] - 
       8*invg33*invh02*Phi003[ijk]*Phi203[ijk] - 
       8*invg03*invh12*Phi100[ijk]*Phi203[ijk] - 
       8*invg13*invh12*Phi101[ijk]*Phi203[ijk] - 
       8*invg23*invh12*Phi102[ijk]*Phi203[ijk] - 
       8*invg33*invh12*Phi103[ijk]*Phi203[ijk] - 
       8*invg03*invh22*Phi200[ijk]*Phi203[ijk] - 
       8*invg13*invh22*Phi201[ijk]*Phi203[ijk] - 
       8*invg23*invh22*Phi202[ijk]*Phi203[ijk] - 
       4*invg33*invh22*Power(Phi203[ijk],2) - 
       8*invg00*invh03*Phi000[ijk]*Phi300[ijk] - 
       8*invg01*invh03*Phi001[ijk]*Phi300[ijk] - 
       8*invg02*invh03*Phi002[ijk]*Phi300[ijk] - 
       8*invg03*invh03*Phi003[ijk]*Phi300[ijk] - 
       8*invg00*invh13*Phi100[ijk]*Phi300[ijk] - 
       8*invg01*invh13*Phi101[ijk]*Phi300[ijk] - 
       8*invg02*invh13*Phi102[ijk]*Phi300[ijk] - 
       8*invg03*invh13*Phi103[ijk]*Phi300[ijk] - 
       8*invg00*invh23*Phi200[ijk]*Phi300[ijk] - 
       8*invg01*invh23*Phi201[ijk]*Phi300[ijk] - 
       8*invg02*invh23*Phi202[ijk]*Phi300[ijk] - 
       8*invg03*invh23*Phi203[ijk]*Phi300[ijk] - 
       4*invg00*invh33*Power(Phi300[ijk],2) - 
       8*invg01*invh03*Phi000[ijk]*Phi301[ijk] - 
       8*invg11*invh03*Phi001[ijk]*Phi301[ijk] - 
       8*invg12*invh03*Phi002[ijk]*Phi301[ijk] - 
       8*invg13*invh03*Phi003[ijk]*Phi301[ijk] - 
       8*invg01*invh13*Phi100[ijk]*Phi301[ijk] - 
       8*invg11*invh13*Phi101[ijk]*Phi301[ijk] - 
       8*invg12*invh13*Phi102[ijk]*Phi301[ijk] - 
       8*invg13*invh13*Phi103[ijk]*Phi301[ijk] - 
       8*invg01*invh23*Phi200[ijk]*Phi301[ijk] - 
       8*invg11*invh23*Phi201[ijk]*Phi301[ijk] - 
       8*invg12*invh23*Phi202[ijk]*Phi301[ijk] - 
       8*invg13*invh23*Phi203[ijk]*Phi301[ijk] - 
       8*invg01*invh33*Phi300[ijk]*Phi301[ijk] - 
       4*invg11*invh33*Power(Phi301[ijk],2) - 
       8*invg02*invh03*Phi000[ijk]*Phi302[ijk] - 
       8*invg12*invh03*Phi001[ijk]*Phi302[ijk] - 
       8*invg22*invh03*Phi002[ijk]*Phi302[ijk] - 
       8*invg23*invh03*Phi003[ijk]*Phi302[ijk] - 
       8*invg02*invh13*Phi100[ijk]*Phi302[ijk] - 
       8*invg12*invh13*Phi101[ijk]*Phi302[ijk] - 
       8*invg22*invh13*Phi102[ijk]*Phi302[ijk] - 
       8*invg23*invh13*Phi103[ijk]*Phi302[ijk] - 
       8*invg02*invh23*Phi200[ijk]*Phi302[ijk] - 
       8*invg12*invh23*Phi201[ijk]*Phi302[ijk] - 
       8*invg22*invh23*Phi202[ijk]*Phi302[ijk] - 
       8*invg23*invh23*Phi203[ijk]*Phi302[ijk] - 
       8*invg02*invh33*Phi300[ijk]*Phi302[ijk] - 
       8*invg12*invh33*Phi301[ijk]*Phi302[ijk] - 
       4*invg22*invh33*Power(Phi302[ijk],2) - 
       8*invg03*invh03*Phi000[ijk]*Phi303[ijk] - 
       8*invg13*invh03*Phi001[ijk]*Phi303[ijk] - 
       8*invg23*invh03*Phi002[ijk]*Phi303[ijk] - 
       8*invg33*invh03*Phi003[ijk]*Phi303[ijk] - 
       8*invg03*invh13*Phi100[ijk]*Phi303[ijk] - 
       8*invg13*invh13*Phi101[ijk]*Phi303[ijk] - 
       8*invg23*invh13*Phi102[ijk]*Phi303[ijk] - 
       8*invg33*invh13*Phi103[ijk]*Phi303[ijk] - 
       8*invg03*invh23*Phi200[ijk]*Phi303[ijk] - 
       8*invg13*invh23*Phi201[ijk]*Phi303[ijk] - 
       8*invg23*invh23*Phi202[ijk]*Phi303[ijk] - 
       8*invg33*invh23*Phi203[ijk]*Phi303[ijk] - 
       8*invg03*invh33*Phi300[ijk]*Phi303[ijk] - 
       8*invg13*invh33*Phi301[ijk]*Phi303[ijk] - 
       8*invg23*invh33*Phi302[ijk]*Phi303[ijk] - 
       4*invg33*invh33*Power(Phi303[ijk],2) + 
       2*invh00*nvec0*Phi000[ijk]*Pi00[ijk] + 
       2*invh01*nvec0*Phi100[ijk]*Pi00[ijk] + 
       2*invh02*nvec0*Phi200[ijk]*Pi00[ijk] + 
       2*invh03*nvec0*Phi300[ijk]*Pi00[ijk] + 
       4*invg00*Power(Pi00[ijk],2) + Power(nvec0,2)*Power(Pi00[ijk],2) + 
       2*invh01*nvec0*Phi000[ijk]*Pi01[ijk] + 
       2*invh00*nvec1*Phi000[ijk]*Pi01[ijk] + 
       2*invh11*nvec0*Phi100[ijk]*Pi01[ijk] + 
       2*invh01*nvec1*Phi100[ijk]*Pi01[ijk] + 
       2*invh12*nvec0*Phi200[ijk]*Pi01[ijk] + 
       2*invh02*nvec1*Phi200[ijk]*Pi01[ijk] + 
       2*invh13*nvec0*Phi300[ijk]*Pi01[ijk] + 
       2*invh03*nvec1*Phi300[ijk]*Pi01[ijk] + 
       8*invg01*Pi00[ijk]*Pi01[ijk] + 2*nvec0*nvec1*Pi00[ijk]*Pi01[ijk] + 
       4*invg11*Power(Pi01[ijk],2) + 
       2*invh02*nvec0*Phi000[ijk]*Pi02[ijk] + 
       2*invh00*nvec2*Phi000[ijk]*Pi02[ijk] + 
       2*invh12*nvec0*Phi100[ijk]*Pi02[ijk] + 
       2*invh01*nvec2*Phi100[ijk]*Pi02[ijk] + 
       2*invh22*nvec0*Phi200[ijk]*Pi02[ijk] + 
       2*invh02*nvec2*Phi200[ijk]*Pi02[ijk] + 
       2*invh23*nvec0*Phi300[ijk]*Pi02[ijk] + 
       2*invh03*nvec2*Phi300[ijk]*Pi02[ijk] + 
       8*invg02*Pi00[ijk]*Pi02[ijk] + 2*nvec0*nvec2*Pi00[ijk]*Pi02[ijk] + 
       8*invg12*Pi01[ijk]*Pi02[ijk] + 4*invg22*Power(Pi02[ijk],2) + 
       2*invh03*nvec0*Phi000[ijk]*Pi03[ijk] + 
       2*invh00*nvec3*Phi000[ijk]*Pi03[ijk] + 
       2*invh13*nvec0*Phi100[ijk]*Pi03[ijk] + 
       2*invh01*nvec3*Phi100[ijk]*Pi03[ijk] + 
       2*invh23*nvec0*Phi200[ijk]*Pi03[ijk] + 
       2*invh02*nvec3*Phi200[ijk]*Pi03[ijk] + 
       2*invh33*nvec0*Phi300[ijk]*Pi03[ijk] + 
       2*invh03*nvec3*Phi300[ijk]*Pi03[ijk] + 
       8*invg03*Pi00[ijk]*Pi03[ijk] + 2*nvec0*nvec3*Pi00[ijk]*Pi03[ijk] + 
       8*invg13*Pi01[ijk]*Pi03[ijk] + 8*invg23*Pi02[ijk]*Pi03[ijk] + 
       4*invg33*Power(Pi03[ijk],2) + 
       2*invh01*nvec1*Phi000[ijk]*Pi11[ijk] + 
       2*invh11*nvec1*Phi100[ijk]*Pi11[ijk] + 
       2*invh12*nvec1*Phi200[ijk]*Pi11[ijk] + 
       2*invh13*nvec1*Phi300[ijk]*Pi11[ijk] + 
       Power(nvec1,2)*Pi00[ijk]*Pi11[ijk] + 
       2*invh02*nvec1*Phi000[ijk]*Pi12[ijk] + 
       2*invh01*nvec2*Phi000[ijk]*Pi12[ijk] + 
       2*invh12*nvec1*Phi100[ijk]*Pi12[ijk] + 
       2*invh11*nvec2*Phi100[ijk]*Pi12[ijk] + 
       2*invh22*nvec1*Phi200[ijk]*Pi12[ijk] + 
       2*invh12*nvec2*Phi200[ijk]*Pi12[ijk] + 
       2*invh23*nvec1*Phi300[ijk]*Pi12[ijk] + 
       2*invh13*nvec2*Phi300[ijk]*Pi12[ijk] + 
       2*nvec1*nvec2*Pi00[ijk]*Pi12[ijk] + 
       2*invh03*nvec1*Phi000[ijk]*Pi13[ijk] + 
       2*invh01*nvec3*Phi000[ijk]*Pi13[ijk] + 
       2*invh13*nvec1*Phi100[ijk]*Pi13[ijk] + 
       2*invh11*nvec3*Phi100[ijk]*Pi13[ijk] + 
       2*invh23*nvec1*Phi200[ijk]*Pi13[ijk] + 
       2*invh12*nvec3*Phi200[ijk]*Pi13[ijk] + 
       2*invh33*nvec1*Phi300[ijk]*Pi13[ijk] + 
       2*invh13*nvec3*Phi300[ijk]*Pi13[ijk] + 
       2*nvec1*nvec3*Pi00[ijk]*Pi13[ijk] + 
       2*invh02*nvec2*Phi000[ijk]*Pi22[ijk] + 
       2*invh12*nvec2*Phi100[ijk]*Pi22[ijk] + 
       2*invh22*nvec2*Phi200[ijk]*Pi22[ijk] + 
       2*invh23*nvec2*Phi300[ijk]*Pi22[ijk] + 
       Power(nvec2,2)*Pi00[ijk]*Pi22[ijk] + 
       2*invh03*nvec2*Phi000[ijk]*Pi23[ijk] + 
       2*invh02*nvec3*Phi000[ijk]*Pi23[ijk] + 
       2*invh13*nvec2*Phi100[ijk]*Pi23[ijk] + 
       2*invh12*nvec3*Phi100[ijk]*Pi23[ijk] + 
       2*invh23*nvec2*Phi200[ijk]*Pi23[ijk] + 
       2*invh22*nvec3*Phi200[ijk]*Pi23[ijk] + 
       2*invh33*nvec2*Phi300[ijk]*Pi23[ijk] + 
       2*invh23*nvec3*Phi300[ijk]*Pi23[ijk] + 
       2*nvec2*nvec3*Pi00[ijk]*Pi23[ijk] + 
       2*invh03*nvec3*Phi000[ijk]*Pi33[ijk] + 
       2*invh13*nvec3*Phi100[ijk]*Pi33[ijk] + 
       2*invh23*nvec3*Phi200[ijk]*Pi33[ijk] + 
       2*invh33*nvec3*Phi300[ijk]*Pi33[ijk] + 
       Power(nvec3,2)*Pi00[ijk]*Pi33[ijk]))/2. - srcSdH00[ijk]
;

dtPi01[ijk]
=
(-2*interior*AdPi01[ijk] + alpha[ijk]*
     (-4*Gam000*Gam100*Power(invg00,2) - 8*Gam001*Gam100*invg00*invg01 - 
       8*Gam000*Gam101*invg00*invg01 - 4*Gam011*Gam100*Power(invg01,2) - 
       8*Gam001*Gam101*Power(invg01,2) - 4*Gam000*Gam111*Power(invg01,2) - 
       8*Gam002*Gam100*invg00*invg02 - 8*Gam000*Gam102*invg00*invg02 - 
       8*Gam012*Gam100*invg01*invg02 - 8*Gam002*Gam101*invg01*invg02 - 
       8*Gam001*Gam102*invg01*invg02 - 8*Gam000*Gam112*invg01*invg02 - 
       4*Gam022*Gam100*Power(invg02,2) - 8*Gam002*Gam102*Power(invg02,2) - 
       4*Gam000*Gam122*Power(invg02,2) - 8*Gam003*Gam100*invg00*invg03 - 
       8*Gam000*Gam103*invg00*invg03 - 8*Gam013*Gam100*invg01*invg03 - 
       8*Gam003*Gam101*invg01*invg03 - 8*Gam001*Gam103*invg01*invg03 - 
       8*Gam000*Gam113*invg01*invg03 - 8*Gam023*Gam100*invg02*invg03 - 
       8*Gam003*Gam102*invg02*invg03 - 8*Gam002*Gam103*invg02*invg03 - 
       8*Gam000*Gam123*invg02*invg03 - 4*Gam033*Gam100*Power(invg03,2) - 
       8*Gam003*Gam103*Power(invg03,2) - 4*Gam000*Gam133*Power(invg03,2) - 
       8*Gam001*Gam101*invg00*invg11 - 8*Gam011*Gam101*invg01*invg11 - 
       8*Gam001*Gam111*invg01*invg11 - 8*Gam012*Gam101*invg02*invg11 - 
       8*Gam001*Gam112*invg02*invg11 - 8*Gam013*Gam101*invg03*invg11 - 
       8*Gam001*Gam113*invg03*invg11 - 4*Gam011*Gam111*Power(invg11,2) - 
       8*Gam002*Gam101*invg00*invg12 - 8*Gam001*Gam102*invg00*invg12 - 
       8*Gam012*Gam101*invg01*invg12 - 8*Gam011*Gam102*invg01*invg12 - 
       8*Gam002*Gam111*invg01*invg12 - 8*Gam001*Gam112*invg01*invg12 - 
       8*Gam022*Gam101*invg02*invg12 - 8*Gam012*Gam102*invg02*invg12 - 
       8*Gam002*Gam112*invg02*invg12 - 8*Gam001*Gam122*invg02*invg12 - 
       8*Gam023*Gam101*invg03*invg12 - 8*Gam013*Gam102*invg03*invg12 - 
       8*Gam002*Gam113*invg03*invg12 - 8*Gam001*Gam123*invg03*invg12 - 
       8*Gam012*Gam111*invg11*invg12 - 8*Gam011*Gam112*invg11*invg12 - 
       4*Gam022*Gam111*Power(invg12,2) - 8*Gam012*Gam112*Power(invg12,2) - 
       4*Gam011*Gam122*Power(invg12,2) - 8*Gam003*Gam101*invg00*invg13 - 
       8*Gam001*Gam103*invg00*invg13 - 8*Gam013*Gam101*invg01*invg13 - 
       8*Gam011*Gam103*invg01*invg13 - 8*Gam003*Gam111*invg01*invg13 - 
       8*Gam001*Gam113*invg01*invg13 - 8*Gam023*Gam101*invg02*invg13 - 
       8*Gam012*Gam103*invg02*invg13 - 8*Gam003*Gam112*invg02*invg13 - 
       8*Gam001*Gam123*invg02*invg13 - 8*Gam033*Gam101*invg03*invg13 - 
       8*Gam013*Gam103*invg03*invg13 - 8*Gam003*Gam113*invg03*invg13 - 
       8*Gam001*Gam133*invg03*invg13 - 8*Gam013*Gam111*invg11*invg13 - 
       8*Gam011*Gam113*invg11*invg13 - 8*Gam023*Gam111*invg12*invg13 - 
       8*Gam013*Gam112*invg12*invg13 - 8*Gam012*Gam113*invg12*invg13 - 
       8*Gam011*Gam123*invg12*invg13 - 4*Gam033*Gam111*Power(invg13,2) - 
       8*Gam013*Gam113*Power(invg13,2) - 4*Gam011*Gam133*Power(invg13,2) - 
       8*Gam002*Gam102*invg00*invg22 - 8*Gam012*Gam102*invg01*invg22 - 
       8*Gam002*Gam112*invg01*invg22 - 8*Gam022*Gam102*invg02*invg22 - 
       8*Gam002*Gam122*invg02*invg22 - 8*Gam023*Gam102*invg03*invg22 - 
       8*Gam002*Gam123*invg03*invg22 - 8*Gam012*Gam112*invg11*invg22 - 
       8*Gam022*Gam112*invg12*invg22 - 8*Gam012*Gam122*invg12*invg22 - 
       8*Gam023*Gam112*invg13*invg22 - 8*Gam012*Gam123*invg13*invg22 - 
       4*Gam022*Gam122*Power(invg22,2) - 8*Gam003*Gam102*invg00*invg23 - 
       8*Gam002*Gam103*invg00*invg23 - 8*Gam013*Gam102*invg01*invg23 - 
       8*Gam012*Gam103*invg01*invg23 - 8*Gam003*Gam112*invg01*invg23 - 
       8*Gam002*Gam113*invg01*invg23 - 8*Gam023*Gam102*invg02*invg23 - 
       8*Gam022*Gam103*invg02*invg23 - 8*Gam003*Gam122*invg02*invg23 - 
       8*Gam002*Gam123*invg02*invg23 - 8*Gam033*Gam102*invg03*invg23 - 
       8*Gam023*Gam103*invg03*invg23 - 8*Gam003*Gam123*invg03*invg23 - 
       8*Gam002*Gam133*invg03*invg23 - 8*Gam013*Gam112*invg11*invg23 - 
       8*Gam012*Gam113*invg11*invg23 - 8*Gam023*Gam112*invg12*invg23 - 
       8*Gam022*Gam113*invg12*invg23 - 8*Gam013*Gam122*invg12*invg23 - 
       8*Gam012*Gam123*invg12*invg23 - 8*Gam033*Gam112*invg13*invg23 - 
       8*Gam023*Gam113*invg13*invg23 - 8*Gam013*Gam123*invg13*invg23 - 
       8*Gam012*Gam133*invg13*invg23 - 8*Gam023*Gam122*invg22*invg23 - 
       8*Gam022*Gam123*invg22*invg23 - 4*Gam033*Gam122*Power(invg23,2) - 
       8*Gam023*Gam123*Power(invg23,2) - 4*Gam022*Gam133*Power(invg23,2) - 
       8*Gam003*Gam103*invg00*invg33 - 8*Gam013*Gam103*invg01*invg33 - 
       8*Gam003*Gam113*invg01*invg33 - 8*Gam023*Gam103*invg02*invg33 - 
       8*Gam003*Gam123*invg02*invg33 - 8*Gam033*Gam103*invg03*invg33 - 
       8*Gam003*Gam133*invg03*invg33 - 8*Gam013*Gam113*invg11*invg33 - 
       8*Gam023*Gam113*invg12*invg33 - 8*Gam013*Gam123*invg12*invg33 - 
       8*Gam033*Gam113*invg13*invg33 - 8*Gam013*Gam133*invg13*invg33 - 
       8*Gam023*Gam123*invg22*invg33 - 8*Gam033*Gam123*invg23*invg33 - 
       8*Gam023*Gam133*invg23*invg33 - 4*Gam033*Gam133*Power(invg33,2) + 
       2*gamma0*ndua1*trGam0 + 2*gamma0*ndua0*trGam1 + 
       2*(2*Gam001*invg00 + 2*Gam101*invg01 + 2*Gam201*invg02 + 
          2*Gam301*invg03 + gamma0*ndua1)*H0[ijk] + 
       4*Gam001*invg01*H1[ijk] + 4*Gam101*invg11*H1[ijk] + 
       4*Gam201*invg12*H1[ijk] + 4*Gam301*invg13*H1[ijk] + 
       2*gamma0*ndua0*H1[ijk] + 4*Gam001*invg02*H2[ijk] + 
       4*Gam101*invg12*H2[ijk] + 4*Gam201*invg22*H2[ijk] + 
       4*Gam301*invg23*H2[ijk] + 4*Gam001*invg03*H3[ijk] + 
       4*Gam101*invg13*H3[ijk] + 4*Gam201*invg23*H3[ijk] + 
       4*Gam301*invg33*H3[ijk] - 
       2*gamma0*g01[ijk]*(nvec0*trGam0 + nvec1*trGam1 + nvec2*trGam2 + 
          nvec3*trGam3 + nvec0*H0[ijk] + nvec1*H1[ijk] + nvec2*H2[ijk] + 
          nvec3*H3[ijk]) + 4*invg00*invh00*Phi000[ijk]*Phi001[ijk] + 
       4*invg01*invh00*Power(Phi001[ijk],2) + 
       4*invg02*invh00*Phi001[ijk]*Phi002[ijk] + 
       4*invg03*invh00*Phi001[ijk]*Phi003[ijk] + 
       4*invg01*invh00*Phi000[ijk]*Phi011[ijk] + 
       4*invg11*invh00*Phi001[ijk]*Phi011[ijk] + 
       4*invg12*invh00*Phi002[ijk]*Phi011[ijk] + 
       4*invg13*invh00*Phi003[ijk]*Phi011[ijk] + 
       4*invg02*invh00*Phi000[ijk]*Phi012[ijk] + 
       4*invg12*invh00*Phi001[ijk]*Phi012[ijk] + 
       4*invg22*invh00*Phi002[ijk]*Phi012[ijk] + 
       4*invg23*invh00*Phi003[ijk]*Phi012[ijk] + 
       4*invg03*invh00*Phi000[ijk]*Phi013[ijk] + 
       4*invg13*invh00*Phi001[ijk]*Phi013[ijk] + 
       4*invg23*invh00*Phi002[ijk]*Phi013[ijk] + 
       4*invg33*invh00*Phi003[ijk]*Phi013[ijk] + 
       4*invg00*invh01*Phi001[ijk]*Phi100[ijk] + 
       4*invg01*invh01*Phi011[ijk]*Phi100[ijk] + 
       4*invg02*invh01*Phi012[ijk]*Phi100[ijk] + 
       4*invg03*invh01*Phi013[ijk]*Phi100[ijk] + 
       4*invg00*invh01*Phi000[ijk]*Phi101[ijk] + 
       8*invg01*invh01*Phi001[ijk]*Phi101[ijk] + 
       4*invg02*invh01*Phi002[ijk]*Phi101[ijk] + 
       4*invg03*invh01*Phi003[ijk]*Phi101[ijk] + 
       4*invg11*invh01*Phi011[ijk]*Phi101[ijk] + 
       4*invg12*invh01*Phi012[ijk]*Phi101[ijk] + 
       4*invg13*invh01*Phi013[ijk]*Phi101[ijk] + 
       4*invg00*invh11*Phi100[ijk]*Phi101[ijk] + 
       4*invg01*invh11*Power(Phi101[ijk],2) + 
       4*invg02*invh01*Phi001[ijk]*Phi102[ijk] + 
       4*invg12*invh01*Phi011[ijk]*Phi102[ijk] + 
       4*invg22*invh01*Phi012[ijk]*Phi102[ijk] + 
       4*invg23*invh01*Phi013[ijk]*Phi102[ijk] + 
       4*invg02*invh11*Phi101[ijk]*Phi102[ijk] + 
       4*invg03*invh01*Phi001[ijk]*Phi103[ijk] + 
       4*invg13*invh01*Phi011[ijk]*Phi103[ijk] + 
       4*invg23*invh01*Phi012[ijk]*Phi103[ijk] + 
       4*invg33*invh01*Phi013[ijk]*Phi103[ijk] + 
       4*invg03*invh11*Phi101[ijk]*Phi103[ijk] + 
       4*invg01*invh01*Phi000[ijk]*Phi111[ijk] + 
       4*invg11*invh01*Phi001[ijk]*Phi111[ijk] + 
       4*invg12*invh01*Phi002[ijk]*Phi111[ijk] + 
       4*invg13*invh01*Phi003[ijk]*Phi111[ijk] + 
       4*invg01*invh11*Phi100[ijk]*Phi111[ijk] + 
       4*invg11*invh11*Phi101[ijk]*Phi111[ijk] + 
       4*invg12*invh11*Phi102[ijk]*Phi111[ijk] + 
       4*invg13*invh11*Phi103[ijk]*Phi111[ijk] + 
       4*invg02*invh01*Phi000[ijk]*Phi112[ijk] + 
       4*invg12*invh01*Phi001[ijk]*Phi112[ijk] + 
       4*invg22*invh01*Phi002[ijk]*Phi112[ijk] + 
       4*invg23*invh01*Phi003[ijk]*Phi112[ijk] + 
       4*invg02*invh11*Phi100[ijk]*Phi112[ijk] + 
       4*invg12*invh11*Phi101[ijk]*Phi112[ijk] + 
       4*invg22*invh11*Phi102[ijk]*Phi112[ijk] + 
       4*invg23*invh11*Phi103[ijk]*Phi112[ijk] + 
       4*invg03*invh01*Phi000[ijk]*Phi113[ijk] + 
       4*invg13*invh01*Phi001[ijk]*Phi113[ijk] + 
       4*invg23*invh01*Phi002[ijk]*Phi113[ijk] + 
       4*invg33*invh01*Phi003[ijk]*Phi113[ijk] + 
       4*invg03*invh11*Phi100[ijk]*Phi113[ijk] + 
       4*invg13*invh11*Phi101[ijk]*Phi113[ijk] + 
       4*invg23*invh11*Phi102[ijk]*Phi113[ijk] + 
       4*invg33*invh11*Phi103[ijk]*Phi113[ijk] + 
       4*invg00*invh02*Phi001[ijk]*Phi200[ijk] + 
       4*invg01*invh02*Phi011[ijk]*Phi200[ijk] + 
       4*invg02*invh02*Phi012[ijk]*Phi200[ijk] + 
       4*invg03*invh02*Phi013[ijk]*Phi200[ijk] + 
       4*invg00*invh12*Phi101[ijk]*Phi200[ijk] + 
       4*invg01*invh12*Phi111[ijk]*Phi200[ijk] + 
       4*invg02*invh12*Phi112[ijk]*Phi200[ijk] + 
       4*invg03*invh12*Phi113[ijk]*Phi200[ijk] + 
       4*invg00*invh02*Phi000[ijk]*Phi201[ijk] + 
       8*invg01*invh02*Phi001[ijk]*Phi201[ijk] + 
       4*invg02*invh02*Phi002[ijk]*Phi201[ijk] + 
       4*invg03*invh02*Phi003[ijk]*Phi201[ijk] + 
       4*invg11*invh02*Phi011[ijk]*Phi201[ijk] + 
       4*invg12*invh02*Phi012[ijk]*Phi201[ijk] + 
       4*invg13*invh02*Phi013[ijk]*Phi201[ijk] + 
       4*invg00*invh12*Phi100[ijk]*Phi201[ijk] + 
       8*invg01*invh12*Phi101[ijk]*Phi201[ijk] + 
       4*invg02*invh12*Phi102[ijk]*Phi201[ijk] + 
       4*invg03*invh12*Phi103[ijk]*Phi201[ijk] + 
       4*invg11*invh12*Phi111[ijk]*Phi201[ijk] + 
       4*invg12*invh12*Phi112[ijk]*Phi201[ijk] + 
       4*invg13*invh12*Phi113[ijk]*Phi201[ijk] + 
       4*invg00*invh22*Phi200[ijk]*Phi201[ijk] + 
       4*invg01*invh22*Power(Phi201[ijk],2) + 
       4*invg02*invh02*Phi001[ijk]*Phi202[ijk] + 
       4*invg12*invh02*Phi011[ijk]*Phi202[ijk] + 
       4*invg22*invh02*Phi012[ijk]*Phi202[ijk] + 
       4*invg23*invh02*Phi013[ijk]*Phi202[ijk] + 
       4*invg02*invh12*Phi101[ijk]*Phi202[ijk] + 
       4*invg12*invh12*Phi111[ijk]*Phi202[ijk] + 
       4*invg22*invh12*Phi112[ijk]*Phi202[ijk] + 
       4*invg23*invh12*Phi113[ijk]*Phi202[ijk] + 
       4*invg02*invh22*Phi201[ijk]*Phi202[ijk] + 
       4*invg03*invh02*Phi001[ijk]*Phi203[ijk] + 
       4*invg13*invh02*Phi011[ijk]*Phi203[ijk] + 
       4*invg23*invh02*Phi012[ijk]*Phi203[ijk] + 
       4*invg33*invh02*Phi013[ijk]*Phi203[ijk] + 
       4*invg03*invh12*Phi101[ijk]*Phi203[ijk] + 
       4*invg13*invh12*Phi111[ijk]*Phi203[ijk] + 
       4*invg23*invh12*Phi112[ijk]*Phi203[ijk] + 
       4*invg33*invh12*Phi113[ijk]*Phi203[ijk] + 
       4*invg03*invh22*Phi201[ijk]*Phi203[ijk] + 
       4*invg01*invh02*Phi000[ijk]*Phi211[ijk] + 
       4*invg11*invh02*Phi001[ijk]*Phi211[ijk] + 
       4*invg12*invh02*Phi002[ijk]*Phi211[ijk] + 
       4*invg13*invh02*Phi003[ijk]*Phi211[ijk] + 
       4*invg01*invh12*Phi100[ijk]*Phi211[ijk] + 
       4*invg11*invh12*Phi101[ijk]*Phi211[ijk] + 
       4*invg12*invh12*Phi102[ijk]*Phi211[ijk] + 
       4*invg13*invh12*Phi103[ijk]*Phi211[ijk] + 
       4*invg01*invh22*Phi200[ijk]*Phi211[ijk] + 
       4*invg11*invh22*Phi201[ijk]*Phi211[ijk] + 
       4*invg12*invh22*Phi202[ijk]*Phi211[ijk] + 
       4*invg13*invh22*Phi203[ijk]*Phi211[ijk] + 
       4*invg02*invh02*Phi000[ijk]*Phi212[ijk] + 
       4*invg12*invh02*Phi001[ijk]*Phi212[ijk] + 
       4*invg22*invh02*Phi002[ijk]*Phi212[ijk] + 
       4*invg23*invh02*Phi003[ijk]*Phi212[ijk] + 
       4*invg02*invh12*Phi100[ijk]*Phi212[ijk] + 
       4*invg12*invh12*Phi101[ijk]*Phi212[ijk] + 
       4*invg22*invh12*Phi102[ijk]*Phi212[ijk] + 
       4*invg23*invh12*Phi103[ijk]*Phi212[ijk] + 
       4*invg02*invh22*Phi200[ijk]*Phi212[ijk] + 
       4*invg12*invh22*Phi201[ijk]*Phi212[ijk] + 
       4*invg22*invh22*Phi202[ijk]*Phi212[ijk] + 
       4*invg23*invh22*Phi203[ijk]*Phi212[ijk] + 
       4*invg03*invh02*Phi000[ijk]*Phi213[ijk] + 
       4*invg13*invh02*Phi001[ijk]*Phi213[ijk] + 
       4*invg23*invh02*Phi002[ijk]*Phi213[ijk] + 
       4*invg33*invh02*Phi003[ijk]*Phi213[ijk] + 
       4*invg03*invh12*Phi100[ijk]*Phi213[ijk] + 
       4*invg13*invh12*Phi101[ijk]*Phi213[ijk] + 
       4*invg23*invh12*Phi102[ijk]*Phi213[ijk] + 
       4*invg33*invh12*Phi103[ijk]*Phi213[ijk] + 
       4*invg03*invh22*Phi200[ijk]*Phi213[ijk] + 
       4*invg13*invh22*Phi201[ijk]*Phi213[ijk] + 
       4*invg23*invh22*Phi202[ijk]*Phi213[ijk] + 
       4*invg33*invh22*Phi203[ijk]*Phi213[ijk] + 
       4*invg00*invh03*Phi001[ijk]*Phi300[ijk] + 
       4*invg01*invh03*Phi011[ijk]*Phi300[ijk] + 
       4*invg02*invh03*Phi012[ijk]*Phi300[ijk] + 
       4*invg03*invh03*Phi013[ijk]*Phi300[ijk] + 
       4*invg00*invh13*Phi101[ijk]*Phi300[ijk] + 
       4*invg01*invh13*Phi111[ijk]*Phi300[ijk] + 
       4*invg02*invh13*Phi112[ijk]*Phi300[ijk] + 
       4*invg03*invh13*Phi113[ijk]*Phi300[ijk] + 
       4*invg00*invh23*Phi201[ijk]*Phi300[ijk] + 
       4*invg01*invh23*Phi211[ijk]*Phi300[ijk] + 
       4*invg02*invh23*Phi212[ijk]*Phi300[ijk] + 
       4*invg03*invh23*Phi213[ijk]*Phi300[ijk] + 
       4*invg00*invh03*Phi000[ijk]*Phi301[ijk] + 
       8*invg01*invh03*Phi001[ijk]*Phi301[ijk] + 
       4*invg02*invh03*Phi002[ijk]*Phi301[ijk] + 
       4*invg03*invh03*Phi003[ijk]*Phi301[ijk] + 
       4*invg11*invh03*Phi011[ijk]*Phi301[ijk] + 
       4*invg12*invh03*Phi012[ijk]*Phi301[ijk] + 
       4*invg13*invh03*Phi013[ijk]*Phi301[ijk] + 
       4*invg00*invh13*Phi100[ijk]*Phi301[ijk] + 
       8*invg01*invh13*Phi101[ijk]*Phi301[ijk] + 
       4*invg02*invh13*Phi102[ijk]*Phi301[ijk] + 
       4*invg03*invh13*Phi103[ijk]*Phi301[ijk] + 
       4*invg11*invh13*Phi111[ijk]*Phi301[ijk] + 
       4*invg12*invh13*Phi112[ijk]*Phi301[ijk] + 
       4*invg13*invh13*Phi113[ijk]*Phi301[ijk] + 
       4*invg00*invh23*Phi200[ijk]*Phi301[ijk] + 
       8*invg01*invh23*Phi201[ijk]*Phi301[ijk] + 
       4*invg02*invh23*Phi202[ijk]*Phi301[ijk] + 
       4*invg03*invh23*Phi203[ijk]*Phi301[ijk] + 
       4*invg11*invh23*Phi211[ijk]*Phi301[ijk] + 
       4*invg12*invh23*Phi212[ijk]*Phi301[ijk] + 
       4*invg13*invh23*Phi213[ijk]*Phi301[ijk] + 
       4*invg00*invh33*Phi300[ijk]*Phi301[ijk] + 
       4*invg01*invh33*Power(Phi301[ijk],2) + 
       4*invg02*invh03*Phi001[ijk]*Phi302[ijk] + 
       4*invg12*invh03*Phi011[ijk]*Phi302[ijk] + 
       4*invg22*invh03*Phi012[ijk]*Phi302[ijk] + 
       4*invg23*invh03*Phi013[ijk]*Phi302[ijk] + 
       4*invg02*invh13*Phi101[ijk]*Phi302[ijk] + 
       4*invg12*invh13*Phi111[ijk]*Phi302[ijk] + 
       4*invg22*invh13*Phi112[ijk]*Phi302[ijk] + 
       4*invg23*invh13*Phi113[ijk]*Phi302[ijk] + 
       4*invg02*invh23*Phi201[ijk]*Phi302[ijk] + 
       4*invg12*invh23*Phi211[ijk]*Phi302[ijk] + 
       4*invg22*invh23*Phi212[ijk]*Phi302[ijk] + 
       4*invg23*invh23*Phi213[ijk]*Phi302[ijk] + 
       4*invg02*invh33*Phi301[ijk]*Phi302[ijk] + 
       4*invg03*invh03*Phi001[ijk]*Phi303[ijk] + 
       4*invg13*invh03*Phi011[ijk]*Phi303[ijk] + 
       4*invg23*invh03*Phi012[ijk]*Phi303[ijk] + 
       4*invg33*invh03*Phi013[ijk]*Phi303[ijk] + 
       4*invg03*invh13*Phi101[ijk]*Phi303[ijk] + 
       4*invg13*invh13*Phi111[ijk]*Phi303[ijk] + 
       4*invg23*invh13*Phi112[ijk]*Phi303[ijk] + 
       4*invg33*invh13*Phi113[ijk]*Phi303[ijk] + 
       4*invg03*invh23*Phi201[ijk]*Phi303[ijk] + 
       4*invg13*invh23*Phi211[ijk]*Phi303[ijk] + 
       4*invg23*invh23*Phi212[ijk]*Phi303[ijk] + 
       4*invg33*invh23*Phi213[ijk]*Phi303[ijk] + 
       4*invg03*invh33*Phi301[ijk]*Phi303[ijk] + 
       4*invg01*invh03*Phi000[ijk]*Phi311[ijk] + 
       4*invg11*invh03*Phi001[ijk]*Phi311[ijk] + 
       4*invg12*invh03*Phi002[ijk]*Phi311[ijk] + 
       4*invg13*invh03*Phi003[ijk]*Phi311[ijk] + 
       4*invg01*invh13*Phi100[ijk]*Phi311[ijk] + 
       4*invg11*invh13*Phi101[ijk]*Phi311[ijk] + 
       4*invg12*invh13*Phi102[ijk]*Phi311[ijk] + 
       4*invg13*invh13*Phi103[ijk]*Phi311[ijk] + 
       4*invg01*invh23*Phi200[ijk]*Phi311[ijk] + 
       4*invg11*invh23*Phi201[ijk]*Phi311[ijk] + 
       4*invg12*invh23*Phi202[ijk]*Phi311[ijk] + 
       4*invg13*invh23*Phi203[ijk]*Phi311[ijk] + 
       4*invg01*invh33*Phi300[ijk]*Phi311[ijk] + 
       4*invg11*invh33*Phi301[ijk]*Phi311[ijk] + 
       4*invg12*invh33*Phi302[ijk]*Phi311[ijk] + 
       4*invg13*invh33*Phi303[ijk]*Phi311[ijk] + 
       4*invg02*invh03*Phi000[ijk]*Phi312[ijk] + 
       4*invg12*invh03*Phi001[ijk]*Phi312[ijk] + 
       4*invg22*invh03*Phi002[ijk]*Phi312[ijk] + 
       4*invg23*invh03*Phi003[ijk]*Phi312[ijk] + 
       4*invg02*invh13*Phi100[ijk]*Phi312[ijk] + 
       4*invg12*invh13*Phi101[ijk]*Phi312[ijk] + 
       4*invg22*invh13*Phi102[ijk]*Phi312[ijk] + 
       4*invg23*invh13*Phi103[ijk]*Phi312[ijk] + 
       4*invg02*invh23*Phi200[ijk]*Phi312[ijk] + 
       4*invg12*invh23*Phi201[ijk]*Phi312[ijk] + 
       4*invg22*invh23*Phi202[ijk]*Phi312[ijk] + 
       4*invg23*invh23*Phi203[ijk]*Phi312[ijk] + 
       4*invg02*invh33*Phi300[ijk]*Phi312[ijk] + 
       4*invg12*invh33*Phi301[ijk]*Phi312[ijk] + 
       4*invg22*invh33*Phi302[ijk]*Phi312[ijk] + 
       4*invg23*invh33*Phi303[ijk]*Phi312[ijk] + 
       4*invg03*invh03*Phi000[ijk]*Phi313[ijk] + 
       4*invg13*invh03*Phi001[ijk]*Phi313[ijk] + 
       4*invg23*invh03*Phi002[ijk]*Phi313[ijk] + 
       4*invg33*invh03*Phi003[ijk]*Phi313[ijk] + 
       4*invg03*invh13*Phi100[ijk]*Phi313[ijk] + 
       4*invg13*invh13*Phi101[ijk]*Phi313[ijk] + 
       4*invg23*invh13*Phi102[ijk]*Phi313[ijk] + 
       4*invg33*invh13*Phi103[ijk]*Phi313[ijk] + 
       4*invg03*invh23*Phi200[ijk]*Phi313[ijk] + 
       4*invg13*invh23*Phi201[ijk]*Phi313[ijk] + 
       4*invg23*invh23*Phi202[ijk]*Phi313[ijk] + 
       4*invg33*invh23*Phi203[ijk]*Phi313[ijk] + 
       4*invg03*invh33*Phi300[ijk]*Phi313[ijk] + 
       4*invg13*invh33*Phi301[ijk]*Phi313[ijk] + 
       4*invg23*invh33*Phi302[ijk]*Phi313[ijk] + 
       4*invg33*invh33*Phi303[ijk]*Phi313[ijk] - 
       2*invh00*nvec0*Phi001[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi101[ijk]*Pi00[ijk] - 
       2*invh02*nvec0*Phi201[ijk]*Pi00[ijk] - 
       2*invh03*nvec0*Phi301[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi001[ijk]*Pi01[ijk] - 
       2*invh00*nvec1*Phi001[ijk]*Pi01[ijk] - 
       2*invh11*nvec0*Phi101[ijk]*Pi01[ijk] - 
       2*invh01*nvec1*Phi101[ijk]*Pi01[ijk] - 
       2*invh12*nvec0*Phi201[ijk]*Pi01[ijk] - 
       2*invh02*nvec1*Phi201[ijk]*Pi01[ijk] - 
       2*invh13*nvec0*Phi301[ijk]*Pi01[ijk] - 
       2*invh03*nvec1*Phi301[ijk]*Pi01[ijk] - 
       4*invg00*Pi00[ijk]*Pi01[ijk] - Power(nvec0,2)*Pi00[ijk]*Pi01[ijk] - 
       4*invg01*Power(Pi01[ijk],2) - 2*nvec0*nvec1*Power(Pi01[ijk],2) - 
       2*invh02*nvec0*Phi001[ijk]*Pi02[ijk] - 
       2*invh00*nvec2*Phi001[ijk]*Pi02[ijk] - 
       2*invh12*nvec0*Phi101[ijk]*Pi02[ijk] - 
       2*invh01*nvec2*Phi101[ijk]*Pi02[ijk] - 
       2*invh22*nvec0*Phi201[ijk]*Pi02[ijk] - 
       2*invh02*nvec2*Phi201[ijk]*Pi02[ijk] - 
       2*invh23*nvec0*Phi301[ijk]*Pi02[ijk] - 
       2*invh03*nvec2*Phi301[ijk]*Pi02[ijk] - 
       4*invg02*Pi01[ijk]*Pi02[ijk] - 2*nvec0*nvec2*Pi01[ijk]*Pi02[ijk] - 
       2*invh03*nvec0*Phi001[ijk]*Pi03[ijk] - 
       2*invh00*nvec3*Phi001[ijk]*Pi03[ijk] - 
       2*invh13*nvec0*Phi101[ijk]*Pi03[ijk] - 
       2*invh01*nvec3*Phi101[ijk]*Pi03[ijk] - 
       2*invh23*nvec0*Phi201[ijk]*Pi03[ijk] - 
       2*invh02*nvec3*Phi201[ijk]*Pi03[ijk] - 
       2*invh33*nvec0*Phi301[ijk]*Pi03[ijk] - 
       2*invh03*nvec3*Phi301[ijk]*Pi03[ijk] - 
       4*invg03*Pi01[ijk]*Pi03[ijk] - 2*nvec0*nvec3*Pi01[ijk]*Pi03[ijk] - 
       2*invh01*nvec1*Phi001[ijk]*Pi11[ijk] - 
       2*invh11*nvec1*Phi101[ijk]*Pi11[ijk] - 
       2*invh12*nvec1*Phi201[ijk]*Pi11[ijk] - 
       2*invh13*nvec1*Phi301[ijk]*Pi11[ijk] - 
       4*invg01*Pi00[ijk]*Pi11[ijk] - 4*invg11*Pi01[ijk]*Pi11[ijk] - 
       Power(nvec1,2)*Pi01[ijk]*Pi11[ijk] - 4*invg12*Pi02[ijk]*Pi11[ijk] - 
       4*invg13*Pi03[ijk]*Pi11[ijk] - 
       2*invh02*nvec1*Phi001[ijk]*Pi12[ijk] - 
       2*invh01*nvec2*Phi001[ijk]*Pi12[ijk] - 
       2*invh12*nvec1*Phi101[ijk]*Pi12[ijk] - 
       2*invh11*nvec2*Phi101[ijk]*Pi12[ijk] - 
       2*invh22*nvec1*Phi201[ijk]*Pi12[ijk] - 
       2*invh12*nvec2*Phi201[ijk]*Pi12[ijk] - 
       2*invh23*nvec1*Phi301[ijk]*Pi12[ijk] - 
       2*invh13*nvec2*Phi301[ijk]*Pi12[ijk] - 
       4*invg02*Pi00[ijk]*Pi12[ijk] - 4*invg12*Pi01[ijk]*Pi12[ijk] - 
       2*nvec1*nvec2*Pi01[ijk]*Pi12[ijk] - 4*invg22*Pi02[ijk]*Pi12[ijk] - 
       4*invg23*Pi03[ijk]*Pi12[ijk] - 
       2*invh03*nvec1*Phi001[ijk]*Pi13[ijk] - 
       2*invh01*nvec3*Phi001[ijk]*Pi13[ijk] - 
       2*invh13*nvec1*Phi101[ijk]*Pi13[ijk] - 
       2*invh11*nvec3*Phi101[ijk]*Pi13[ijk] - 
       2*invh23*nvec1*Phi201[ijk]*Pi13[ijk] - 
       2*invh12*nvec3*Phi201[ijk]*Pi13[ijk] - 
       2*invh33*nvec1*Phi301[ijk]*Pi13[ijk] - 
       2*invh13*nvec3*Phi301[ijk]*Pi13[ijk] - 
       4*invg03*Pi00[ijk]*Pi13[ijk] - 4*invg13*Pi01[ijk]*Pi13[ijk] - 
       2*nvec1*nvec3*Pi01[ijk]*Pi13[ijk] - 4*invg23*Pi02[ijk]*Pi13[ijk] - 
       4*invg33*Pi03[ijk]*Pi13[ijk] - 
       2*invh02*nvec2*Phi001[ijk]*Pi22[ijk] - 
       2*invh12*nvec2*Phi101[ijk]*Pi22[ijk] - 
       2*invh22*nvec2*Phi201[ijk]*Pi22[ijk] - 
       2*invh23*nvec2*Phi301[ijk]*Pi22[ijk] - 
       Power(nvec2,2)*Pi01[ijk]*Pi22[ijk] - 
       2*invh03*nvec2*Phi001[ijk]*Pi23[ijk] - 
       2*invh02*nvec3*Phi001[ijk]*Pi23[ijk] - 
       2*invh13*nvec2*Phi101[ijk]*Pi23[ijk] - 
       2*invh12*nvec3*Phi101[ijk]*Pi23[ijk] - 
       2*invh23*nvec2*Phi201[ijk]*Pi23[ijk] - 
       2*invh22*nvec3*Phi201[ijk]*Pi23[ijk] - 
       2*invh33*nvec2*Phi301[ijk]*Pi23[ijk] - 
       2*invh23*nvec3*Phi301[ijk]*Pi23[ijk] - 
       2*nvec2*nvec3*Pi01[ijk]*Pi23[ijk] - 
       2*invh03*nvec3*Phi001[ijk]*Pi33[ijk] - 
       2*invh13*nvec3*Phi101[ijk]*Pi33[ijk] - 
       2*invh23*nvec3*Phi201[ijk]*Pi33[ijk] - 
       2*invh33*nvec3*Phi301[ijk]*Pi33[ijk] - 
       Power(nvec3,2)*Pi01[ijk]*Pi33[ijk]) - 
    2*(gamma1*gamma2*beta0[ijk]*Phi001[ijk] + 
       gamma1*gamma2*beta1[ijk]*Phi101[ijk] + 
       gamma1*gamma2*beta2[ijk]*Phi201[ijk] + 
       gamma1*gamma2*beta3[ijk]*Phi301[ijk] + srcSdH01[ijk]))/2.
;

dtPi02[ijk]
=
(-2*interior*AdPi02[ijk] + alpha[ijk]*
     (-4*Gam000*Gam200*Power(invg00,2) - 8*Gam001*Gam200*invg00*invg01 - 
       8*Gam000*Gam201*invg00*invg01 - 4*Gam011*Gam200*Power(invg01,2) - 
       8*Gam001*Gam201*Power(invg01,2) - 4*Gam000*Gam211*Power(invg01,2) - 
       8*Gam002*Gam200*invg00*invg02 - 8*Gam000*Gam202*invg00*invg02 - 
       8*Gam012*Gam200*invg01*invg02 - 8*Gam002*Gam201*invg01*invg02 - 
       8*Gam001*Gam202*invg01*invg02 - 8*Gam000*Gam212*invg01*invg02 - 
       4*Gam022*Gam200*Power(invg02,2) - 8*Gam002*Gam202*Power(invg02,2) - 
       4*Gam000*Gam222*Power(invg02,2) - 8*Gam003*Gam200*invg00*invg03 - 
       8*Gam000*Gam203*invg00*invg03 - 8*Gam013*Gam200*invg01*invg03 - 
       8*Gam003*Gam201*invg01*invg03 - 8*Gam001*Gam203*invg01*invg03 - 
       8*Gam000*Gam213*invg01*invg03 - 8*Gam023*Gam200*invg02*invg03 - 
       8*Gam003*Gam202*invg02*invg03 - 8*Gam002*Gam203*invg02*invg03 - 
       8*Gam000*Gam223*invg02*invg03 - 4*Gam033*Gam200*Power(invg03,2) - 
       8*Gam003*Gam203*Power(invg03,2) - 4*Gam000*Gam233*Power(invg03,2) - 
       8*Gam001*Gam201*invg00*invg11 - 8*Gam011*Gam201*invg01*invg11 - 
       8*Gam001*Gam211*invg01*invg11 - 8*Gam012*Gam201*invg02*invg11 - 
       8*Gam001*Gam212*invg02*invg11 - 8*Gam013*Gam201*invg03*invg11 - 
       8*Gam001*Gam213*invg03*invg11 - 4*Gam011*Gam211*Power(invg11,2) - 
       8*Gam002*Gam201*invg00*invg12 - 8*Gam001*Gam202*invg00*invg12 - 
       8*Gam012*Gam201*invg01*invg12 - 8*Gam011*Gam202*invg01*invg12 - 
       8*Gam002*Gam211*invg01*invg12 - 8*Gam001*Gam212*invg01*invg12 - 
       8*Gam022*Gam201*invg02*invg12 - 8*Gam012*Gam202*invg02*invg12 - 
       8*Gam002*Gam212*invg02*invg12 - 8*Gam001*Gam222*invg02*invg12 - 
       8*Gam023*Gam201*invg03*invg12 - 8*Gam013*Gam202*invg03*invg12 - 
       8*Gam002*Gam213*invg03*invg12 - 8*Gam001*Gam223*invg03*invg12 - 
       8*Gam012*Gam211*invg11*invg12 - 8*Gam011*Gam212*invg11*invg12 - 
       4*Gam022*Gam211*Power(invg12,2) - 8*Gam012*Gam212*Power(invg12,2) - 
       4*Gam011*Gam222*Power(invg12,2) - 8*Gam003*Gam201*invg00*invg13 - 
       8*Gam001*Gam203*invg00*invg13 - 8*Gam013*Gam201*invg01*invg13 - 
       8*Gam011*Gam203*invg01*invg13 - 8*Gam003*Gam211*invg01*invg13 - 
       8*Gam001*Gam213*invg01*invg13 - 8*Gam023*Gam201*invg02*invg13 - 
       8*Gam012*Gam203*invg02*invg13 - 8*Gam003*Gam212*invg02*invg13 - 
       8*Gam001*Gam223*invg02*invg13 - 8*Gam033*Gam201*invg03*invg13 - 
       8*Gam013*Gam203*invg03*invg13 - 8*Gam003*Gam213*invg03*invg13 - 
       8*Gam001*Gam233*invg03*invg13 - 8*Gam013*Gam211*invg11*invg13 - 
       8*Gam011*Gam213*invg11*invg13 - 8*Gam023*Gam211*invg12*invg13 - 
       8*Gam013*Gam212*invg12*invg13 - 8*Gam012*Gam213*invg12*invg13 - 
       8*Gam011*Gam223*invg12*invg13 - 4*Gam033*Gam211*Power(invg13,2) - 
       8*Gam013*Gam213*Power(invg13,2) - 4*Gam011*Gam233*Power(invg13,2) - 
       8*Gam002*Gam202*invg00*invg22 - 8*Gam012*Gam202*invg01*invg22 - 
       8*Gam002*Gam212*invg01*invg22 - 8*Gam022*Gam202*invg02*invg22 - 
       8*Gam002*Gam222*invg02*invg22 - 8*Gam023*Gam202*invg03*invg22 - 
       8*Gam002*Gam223*invg03*invg22 - 8*Gam012*Gam212*invg11*invg22 - 
       8*Gam022*Gam212*invg12*invg22 - 8*Gam012*Gam222*invg12*invg22 - 
       8*Gam023*Gam212*invg13*invg22 - 8*Gam012*Gam223*invg13*invg22 - 
       4*Gam022*Gam222*Power(invg22,2) - 8*Gam003*Gam202*invg00*invg23 - 
       8*Gam002*Gam203*invg00*invg23 - 8*Gam013*Gam202*invg01*invg23 - 
       8*Gam012*Gam203*invg01*invg23 - 8*Gam003*Gam212*invg01*invg23 - 
       8*Gam002*Gam213*invg01*invg23 - 8*Gam023*Gam202*invg02*invg23 - 
       8*Gam022*Gam203*invg02*invg23 - 8*Gam003*Gam222*invg02*invg23 - 
       8*Gam002*Gam223*invg02*invg23 - 8*Gam033*Gam202*invg03*invg23 - 
       8*Gam023*Gam203*invg03*invg23 - 8*Gam003*Gam223*invg03*invg23 - 
       8*Gam002*Gam233*invg03*invg23 - 8*Gam013*Gam212*invg11*invg23 - 
       8*Gam012*Gam213*invg11*invg23 - 8*Gam023*Gam212*invg12*invg23 - 
       8*Gam022*Gam213*invg12*invg23 - 8*Gam013*Gam222*invg12*invg23 - 
       8*Gam012*Gam223*invg12*invg23 - 8*Gam033*Gam212*invg13*invg23 - 
       8*Gam023*Gam213*invg13*invg23 - 8*Gam013*Gam223*invg13*invg23 - 
       8*Gam012*Gam233*invg13*invg23 - 8*Gam023*Gam222*invg22*invg23 - 
       8*Gam022*Gam223*invg22*invg23 - 4*Gam033*Gam222*Power(invg23,2) - 
       8*Gam023*Gam223*Power(invg23,2) - 4*Gam022*Gam233*Power(invg23,2) - 
       8*Gam003*Gam203*invg00*invg33 - 8*Gam013*Gam203*invg01*invg33 - 
       8*Gam003*Gam213*invg01*invg33 - 8*Gam023*Gam203*invg02*invg33 - 
       8*Gam003*Gam223*invg02*invg33 - 8*Gam033*Gam203*invg03*invg33 - 
       8*Gam003*Gam233*invg03*invg33 - 8*Gam013*Gam213*invg11*invg33 - 
       8*Gam023*Gam213*invg12*invg33 - 8*Gam013*Gam223*invg12*invg33 - 
       8*Gam033*Gam213*invg13*invg33 - 8*Gam013*Gam233*invg13*invg33 - 
       8*Gam023*Gam223*invg22*invg33 - 8*Gam033*Gam223*invg23*invg33 - 
       8*Gam023*Gam233*invg23*invg33 - 4*Gam033*Gam233*Power(invg33,2) + 
       2*gamma0*ndua2*trGam0 + 2*gamma0*ndua0*trGam2 + 
       2*(2*Gam002*invg00 + 2*Gam102*invg01 + 2*Gam202*invg02 + 
          2*Gam302*invg03 + gamma0*ndua2)*H0[ijk] + 
       4*Gam002*invg01*H1[ijk] + 4*Gam102*invg11*H1[ijk] + 
       4*Gam202*invg12*H1[ijk] + 4*Gam302*invg13*H1[ijk] + 
       4*Gam002*invg02*H2[ijk] + 4*Gam102*invg12*H2[ijk] + 
       4*Gam202*invg22*H2[ijk] + 4*Gam302*invg23*H2[ijk] + 
       2*gamma0*ndua0*H2[ijk] + 4*Gam002*invg03*H3[ijk] + 
       4*Gam102*invg13*H3[ijk] + 4*Gam202*invg23*H3[ijk] + 
       4*Gam302*invg33*H3[ijk] - 
       2*gamma0*g02[ijk]*(nvec0*trGam0 + nvec1*trGam1 + nvec2*trGam2 + 
          nvec3*trGam3 + nvec0*H0[ijk] + nvec1*H1[ijk] + nvec2*H2[ijk] + 
          nvec3*H3[ijk]) + 4*invg00*invh00*Phi000[ijk]*Phi002[ijk] + 
       4*invg01*invh00*Phi001[ijk]*Phi002[ijk] + 
       4*invg02*invh00*Power(Phi002[ijk],2) + 
       4*invg03*invh00*Phi002[ijk]*Phi003[ijk] + 
       4*invg01*invh00*Phi000[ijk]*Phi012[ijk] + 
       4*invg11*invh00*Phi001[ijk]*Phi012[ijk] + 
       4*invg12*invh00*Phi002[ijk]*Phi012[ijk] + 
       4*invg13*invh00*Phi003[ijk]*Phi012[ijk] + 
       4*invg02*invh00*Phi000[ijk]*Phi022[ijk] + 
       4*invg12*invh00*Phi001[ijk]*Phi022[ijk] + 
       4*invg22*invh00*Phi002[ijk]*Phi022[ijk] + 
       4*invg23*invh00*Phi003[ijk]*Phi022[ijk] + 
       4*invg03*invh00*Phi000[ijk]*Phi023[ijk] + 
       4*invg13*invh00*Phi001[ijk]*Phi023[ijk] + 
       4*invg23*invh00*Phi002[ijk]*Phi023[ijk] + 
       4*invg33*invh00*Phi003[ijk]*Phi023[ijk] + 
       4*invg00*invh01*Phi002[ijk]*Phi100[ijk] + 
       4*invg01*invh01*Phi012[ijk]*Phi100[ijk] + 
       4*invg02*invh01*Phi022[ijk]*Phi100[ijk] + 
       4*invg03*invh01*Phi023[ijk]*Phi100[ijk] + 
       4*invg01*invh01*Phi002[ijk]*Phi101[ijk] + 
       4*invg11*invh01*Phi012[ijk]*Phi101[ijk] + 
       4*invg12*invh01*Phi022[ijk]*Phi101[ijk] + 
       4*invg13*invh01*Phi023[ijk]*Phi101[ijk] + 
       4*invg00*invh01*Phi000[ijk]*Phi102[ijk] + 
       4*invg01*invh01*Phi001[ijk]*Phi102[ijk] + 
       8*invg02*invh01*Phi002[ijk]*Phi102[ijk] + 
       4*invg03*invh01*Phi003[ijk]*Phi102[ijk] + 
       4*invg12*invh01*Phi012[ijk]*Phi102[ijk] + 
       4*invg22*invh01*Phi022[ijk]*Phi102[ijk] + 
       4*invg23*invh01*Phi023[ijk]*Phi102[ijk] + 
       4*invg00*invh11*Phi100[ijk]*Phi102[ijk] + 
       4*invg01*invh11*Phi101[ijk]*Phi102[ijk] + 
       4*invg02*invh11*Power(Phi102[ijk],2) + 
       4*invg03*invh01*Phi002[ijk]*Phi103[ijk] + 
       4*invg13*invh01*Phi012[ijk]*Phi103[ijk] + 
       4*invg23*invh01*Phi022[ijk]*Phi103[ijk] + 
       4*invg33*invh01*Phi023[ijk]*Phi103[ijk] + 
       4*invg03*invh11*Phi102[ijk]*Phi103[ijk] + 
       4*invg01*invh01*Phi000[ijk]*Phi112[ijk] + 
       4*invg11*invh01*Phi001[ijk]*Phi112[ijk] + 
       4*invg12*invh01*Phi002[ijk]*Phi112[ijk] + 
       4*invg13*invh01*Phi003[ijk]*Phi112[ijk] + 
       4*invg01*invh11*Phi100[ijk]*Phi112[ijk] + 
       4*invg11*invh11*Phi101[ijk]*Phi112[ijk] + 
       4*invg12*invh11*Phi102[ijk]*Phi112[ijk] + 
       4*invg13*invh11*Phi103[ijk]*Phi112[ijk] + 
       4*invg02*invh01*Phi000[ijk]*Phi122[ijk] + 
       4*invg12*invh01*Phi001[ijk]*Phi122[ijk] + 
       4*invg22*invh01*Phi002[ijk]*Phi122[ijk] + 
       4*invg23*invh01*Phi003[ijk]*Phi122[ijk] + 
       4*invg02*invh11*Phi100[ijk]*Phi122[ijk] + 
       4*invg12*invh11*Phi101[ijk]*Phi122[ijk] + 
       4*invg22*invh11*Phi102[ijk]*Phi122[ijk] + 
       4*invg23*invh11*Phi103[ijk]*Phi122[ijk] + 
       4*invg03*invh01*Phi000[ijk]*Phi123[ijk] + 
       4*invg13*invh01*Phi001[ijk]*Phi123[ijk] + 
       4*invg23*invh01*Phi002[ijk]*Phi123[ijk] + 
       4*invg33*invh01*Phi003[ijk]*Phi123[ijk] + 
       4*invg03*invh11*Phi100[ijk]*Phi123[ijk] + 
       4*invg13*invh11*Phi101[ijk]*Phi123[ijk] + 
       4*invg23*invh11*Phi102[ijk]*Phi123[ijk] + 
       4*invg33*invh11*Phi103[ijk]*Phi123[ijk] + 
       4*invg00*invh02*Phi002[ijk]*Phi200[ijk] + 
       4*invg01*invh02*Phi012[ijk]*Phi200[ijk] + 
       4*invg02*invh02*Phi022[ijk]*Phi200[ijk] + 
       4*invg03*invh02*Phi023[ijk]*Phi200[ijk] + 
       4*invg00*invh12*Phi102[ijk]*Phi200[ijk] + 
       4*invg01*invh12*Phi112[ijk]*Phi200[ijk] + 
       4*invg02*invh12*Phi122[ijk]*Phi200[ijk] + 
       4*invg03*invh12*Phi123[ijk]*Phi200[ijk] + 
       4*invg01*invh02*Phi002[ijk]*Phi201[ijk] + 
       4*invg11*invh02*Phi012[ijk]*Phi201[ijk] + 
       4*invg12*invh02*Phi022[ijk]*Phi201[ijk] + 
       4*invg13*invh02*Phi023[ijk]*Phi201[ijk] + 
       4*invg01*invh12*Phi102[ijk]*Phi201[ijk] + 
       4*invg11*invh12*Phi112[ijk]*Phi201[ijk] + 
       4*invg12*invh12*Phi122[ijk]*Phi201[ijk] + 
       4*invg13*invh12*Phi123[ijk]*Phi201[ijk] + 
       4*invg00*invh02*Phi000[ijk]*Phi202[ijk] + 
       4*invg01*invh02*Phi001[ijk]*Phi202[ijk] + 
       8*invg02*invh02*Phi002[ijk]*Phi202[ijk] + 
       4*invg03*invh02*Phi003[ijk]*Phi202[ijk] + 
       4*invg12*invh02*Phi012[ijk]*Phi202[ijk] + 
       4*invg22*invh02*Phi022[ijk]*Phi202[ijk] + 
       4*invg23*invh02*Phi023[ijk]*Phi202[ijk] + 
       4*invg00*invh12*Phi100[ijk]*Phi202[ijk] + 
       4*invg01*invh12*Phi101[ijk]*Phi202[ijk] + 
       8*invg02*invh12*Phi102[ijk]*Phi202[ijk] + 
       4*invg03*invh12*Phi103[ijk]*Phi202[ijk] + 
       4*invg12*invh12*Phi112[ijk]*Phi202[ijk] + 
       4*invg22*invh12*Phi122[ijk]*Phi202[ijk] + 
       4*invg23*invh12*Phi123[ijk]*Phi202[ijk] + 
       4*invg00*invh22*Phi200[ijk]*Phi202[ijk] + 
       4*invg01*invh22*Phi201[ijk]*Phi202[ijk] + 
       4*invg02*invh22*Power(Phi202[ijk],2) + 
       4*invg03*invh02*Phi002[ijk]*Phi203[ijk] + 
       4*invg13*invh02*Phi012[ijk]*Phi203[ijk] + 
       4*invg23*invh02*Phi022[ijk]*Phi203[ijk] + 
       4*invg33*invh02*Phi023[ijk]*Phi203[ijk] + 
       4*invg03*invh12*Phi102[ijk]*Phi203[ijk] + 
       4*invg13*invh12*Phi112[ijk]*Phi203[ijk] + 
       4*invg23*invh12*Phi122[ijk]*Phi203[ijk] + 
       4*invg33*invh12*Phi123[ijk]*Phi203[ijk] + 
       4*invg03*invh22*Phi202[ijk]*Phi203[ijk] + 
       4*invg01*invh02*Phi000[ijk]*Phi212[ijk] + 
       4*invg11*invh02*Phi001[ijk]*Phi212[ijk] + 
       4*invg12*invh02*Phi002[ijk]*Phi212[ijk] + 
       4*invg13*invh02*Phi003[ijk]*Phi212[ijk] + 
       4*invg01*invh12*Phi100[ijk]*Phi212[ijk] + 
       4*invg11*invh12*Phi101[ijk]*Phi212[ijk] + 
       4*invg12*invh12*Phi102[ijk]*Phi212[ijk] + 
       4*invg13*invh12*Phi103[ijk]*Phi212[ijk] + 
       4*invg01*invh22*Phi200[ijk]*Phi212[ijk] + 
       4*invg11*invh22*Phi201[ijk]*Phi212[ijk] + 
       4*invg12*invh22*Phi202[ijk]*Phi212[ijk] + 
       4*invg13*invh22*Phi203[ijk]*Phi212[ijk] + 
       4*invg02*invh02*Phi000[ijk]*Phi222[ijk] + 
       4*invg12*invh02*Phi001[ijk]*Phi222[ijk] + 
       4*invg22*invh02*Phi002[ijk]*Phi222[ijk] + 
       4*invg23*invh02*Phi003[ijk]*Phi222[ijk] + 
       4*invg02*invh12*Phi100[ijk]*Phi222[ijk] + 
       4*invg12*invh12*Phi101[ijk]*Phi222[ijk] + 
       4*invg22*invh12*Phi102[ijk]*Phi222[ijk] + 
       4*invg23*invh12*Phi103[ijk]*Phi222[ijk] + 
       4*invg02*invh22*Phi200[ijk]*Phi222[ijk] + 
       4*invg12*invh22*Phi201[ijk]*Phi222[ijk] + 
       4*invg22*invh22*Phi202[ijk]*Phi222[ijk] + 
       4*invg23*invh22*Phi203[ijk]*Phi222[ijk] + 
       4*invg03*invh02*Phi000[ijk]*Phi223[ijk] + 
       4*invg13*invh02*Phi001[ijk]*Phi223[ijk] + 
       4*invg23*invh02*Phi002[ijk]*Phi223[ijk] + 
       4*invg33*invh02*Phi003[ijk]*Phi223[ijk] + 
       4*invg03*invh12*Phi100[ijk]*Phi223[ijk] + 
       4*invg13*invh12*Phi101[ijk]*Phi223[ijk] + 
       4*invg23*invh12*Phi102[ijk]*Phi223[ijk] + 
       4*invg33*invh12*Phi103[ijk]*Phi223[ijk] + 
       4*invg03*invh22*Phi200[ijk]*Phi223[ijk] + 
       4*invg13*invh22*Phi201[ijk]*Phi223[ijk] + 
       4*invg23*invh22*Phi202[ijk]*Phi223[ijk] + 
       4*invg33*invh22*Phi203[ijk]*Phi223[ijk] + 
       4*invg00*invh03*Phi002[ijk]*Phi300[ijk] + 
       4*invg01*invh03*Phi012[ijk]*Phi300[ijk] + 
       4*invg02*invh03*Phi022[ijk]*Phi300[ijk] + 
       4*invg03*invh03*Phi023[ijk]*Phi300[ijk] + 
       4*invg00*invh13*Phi102[ijk]*Phi300[ijk] + 
       4*invg01*invh13*Phi112[ijk]*Phi300[ijk] + 
       4*invg02*invh13*Phi122[ijk]*Phi300[ijk] + 
       4*invg03*invh13*Phi123[ijk]*Phi300[ijk] + 
       4*invg00*invh23*Phi202[ijk]*Phi300[ijk] + 
       4*invg01*invh23*Phi212[ijk]*Phi300[ijk] + 
       4*invg02*invh23*Phi222[ijk]*Phi300[ijk] + 
       4*invg03*invh23*Phi223[ijk]*Phi300[ijk] + 
       4*invg01*invh03*Phi002[ijk]*Phi301[ijk] + 
       4*invg11*invh03*Phi012[ijk]*Phi301[ijk] + 
       4*invg12*invh03*Phi022[ijk]*Phi301[ijk] + 
       4*invg13*invh03*Phi023[ijk]*Phi301[ijk] + 
       4*invg01*invh13*Phi102[ijk]*Phi301[ijk] + 
       4*invg11*invh13*Phi112[ijk]*Phi301[ijk] + 
       4*invg12*invh13*Phi122[ijk]*Phi301[ijk] + 
       4*invg13*invh13*Phi123[ijk]*Phi301[ijk] + 
       4*invg01*invh23*Phi202[ijk]*Phi301[ijk] + 
       4*invg11*invh23*Phi212[ijk]*Phi301[ijk] + 
       4*invg12*invh23*Phi222[ijk]*Phi301[ijk] + 
       4*invg13*invh23*Phi223[ijk]*Phi301[ijk] + 
       4*invg00*invh03*Phi000[ijk]*Phi302[ijk] + 
       4*invg01*invh03*Phi001[ijk]*Phi302[ijk] + 
       8*invg02*invh03*Phi002[ijk]*Phi302[ijk] + 
       4*invg03*invh03*Phi003[ijk]*Phi302[ijk] + 
       4*invg12*invh03*Phi012[ijk]*Phi302[ijk] + 
       4*invg22*invh03*Phi022[ijk]*Phi302[ijk] + 
       4*invg23*invh03*Phi023[ijk]*Phi302[ijk] + 
       4*invg00*invh13*Phi100[ijk]*Phi302[ijk] + 
       4*invg01*invh13*Phi101[ijk]*Phi302[ijk] + 
       8*invg02*invh13*Phi102[ijk]*Phi302[ijk] + 
       4*invg03*invh13*Phi103[ijk]*Phi302[ijk] + 
       4*invg12*invh13*Phi112[ijk]*Phi302[ijk] + 
       4*invg22*invh13*Phi122[ijk]*Phi302[ijk] + 
       4*invg23*invh13*Phi123[ijk]*Phi302[ijk] + 
       4*invg00*invh23*Phi200[ijk]*Phi302[ijk] + 
       4*invg01*invh23*Phi201[ijk]*Phi302[ijk] + 
       8*invg02*invh23*Phi202[ijk]*Phi302[ijk] + 
       4*invg03*invh23*Phi203[ijk]*Phi302[ijk] + 
       4*invg12*invh23*Phi212[ijk]*Phi302[ijk] + 
       4*invg22*invh23*Phi222[ijk]*Phi302[ijk] + 
       4*invg23*invh23*Phi223[ijk]*Phi302[ijk] + 
       4*invg00*invh33*Phi300[ijk]*Phi302[ijk] + 
       4*invg01*invh33*Phi301[ijk]*Phi302[ijk] + 
       4*invg02*invh33*Power(Phi302[ijk],2) + 
       4*invg03*invh03*Phi002[ijk]*Phi303[ijk] + 
       4*invg13*invh03*Phi012[ijk]*Phi303[ijk] + 
       4*invg23*invh03*Phi022[ijk]*Phi303[ijk] + 
       4*invg33*invh03*Phi023[ijk]*Phi303[ijk] + 
       4*invg03*invh13*Phi102[ijk]*Phi303[ijk] + 
       4*invg13*invh13*Phi112[ijk]*Phi303[ijk] + 
       4*invg23*invh13*Phi122[ijk]*Phi303[ijk] + 
       4*invg33*invh13*Phi123[ijk]*Phi303[ijk] + 
       4*invg03*invh23*Phi202[ijk]*Phi303[ijk] + 
       4*invg13*invh23*Phi212[ijk]*Phi303[ijk] + 
       4*invg23*invh23*Phi222[ijk]*Phi303[ijk] + 
       4*invg33*invh23*Phi223[ijk]*Phi303[ijk] + 
       4*invg03*invh33*Phi302[ijk]*Phi303[ijk] + 
       4*invg01*invh03*Phi000[ijk]*Phi312[ijk] + 
       4*invg11*invh03*Phi001[ijk]*Phi312[ijk] + 
       4*invg12*invh03*Phi002[ijk]*Phi312[ijk] + 
       4*invg13*invh03*Phi003[ijk]*Phi312[ijk] + 
       4*invg01*invh13*Phi100[ijk]*Phi312[ijk] + 
       4*invg11*invh13*Phi101[ijk]*Phi312[ijk] + 
       4*invg12*invh13*Phi102[ijk]*Phi312[ijk] + 
       4*invg13*invh13*Phi103[ijk]*Phi312[ijk] + 
       4*invg01*invh23*Phi200[ijk]*Phi312[ijk] + 
       4*invg11*invh23*Phi201[ijk]*Phi312[ijk] + 
       4*invg12*invh23*Phi202[ijk]*Phi312[ijk] + 
       4*invg13*invh23*Phi203[ijk]*Phi312[ijk] + 
       4*invg01*invh33*Phi300[ijk]*Phi312[ijk] + 
       4*invg11*invh33*Phi301[ijk]*Phi312[ijk] + 
       4*invg12*invh33*Phi302[ijk]*Phi312[ijk] + 
       4*invg13*invh33*Phi303[ijk]*Phi312[ijk] + 
       4*invg02*invh03*Phi000[ijk]*Phi322[ijk] + 
       4*invg12*invh03*Phi001[ijk]*Phi322[ijk] + 
       4*invg22*invh03*Phi002[ijk]*Phi322[ijk] + 
       4*invg23*invh03*Phi003[ijk]*Phi322[ijk] + 
       4*invg02*invh13*Phi100[ijk]*Phi322[ijk] + 
       4*invg12*invh13*Phi101[ijk]*Phi322[ijk] + 
       4*invg22*invh13*Phi102[ijk]*Phi322[ijk] + 
       4*invg23*invh13*Phi103[ijk]*Phi322[ijk] + 
       4*invg02*invh23*Phi200[ijk]*Phi322[ijk] + 
       4*invg12*invh23*Phi201[ijk]*Phi322[ijk] + 
       4*invg22*invh23*Phi202[ijk]*Phi322[ijk] + 
       4*invg23*invh23*Phi203[ijk]*Phi322[ijk] + 
       4*invg02*invh33*Phi300[ijk]*Phi322[ijk] + 
       4*invg12*invh33*Phi301[ijk]*Phi322[ijk] + 
       4*invg22*invh33*Phi302[ijk]*Phi322[ijk] + 
       4*invg23*invh33*Phi303[ijk]*Phi322[ijk] + 
       4*invg03*invh03*Phi000[ijk]*Phi323[ijk] + 
       4*invg13*invh03*Phi001[ijk]*Phi323[ijk] + 
       4*invg23*invh03*Phi002[ijk]*Phi323[ijk] + 
       4*invg33*invh03*Phi003[ijk]*Phi323[ijk] + 
       4*invg03*invh13*Phi100[ijk]*Phi323[ijk] + 
       4*invg13*invh13*Phi101[ijk]*Phi323[ijk] + 
       4*invg23*invh13*Phi102[ijk]*Phi323[ijk] + 
       4*invg33*invh13*Phi103[ijk]*Phi323[ijk] + 
       4*invg03*invh23*Phi200[ijk]*Phi323[ijk] + 
       4*invg13*invh23*Phi201[ijk]*Phi323[ijk] + 
       4*invg23*invh23*Phi202[ijk]*Phi323[ijk] + 
       4*invg33*invh23*Phi203[ijk]*Phi323[ijk] + 
       4*invg03*invh33*Phi300[ijk]*Phi323[ijk] + 
       4*invg13*invh33*Phi301[ijk]*Phi323[ijk] + 
       4*invg23*invh33*Phi302[ijk]*Phi323[ijk] + 
       4*invg33*invh33*Phi303[ijk]*Phi323[ijk] - 
       2*invh00*nvec0*Phi002[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi102[ijk]*Pi00[ijk] - 
       2*invh02*nvec0*Phi202[ijk]*Pi00[ijk] - 
       2*invh03*nvec0*Phi302[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi002[ijk]*Pi01[ijk] - 
       2*invh00*nvec1*Phi002[ijk]*Pi01[ijk] - 
       2*invh11*nvec0*Phi102[ijk]*Pi01[ijk] - 
       2*invh01*nvec1*Phi102[ijk]*Pi01[ijk] - 
       2*invh12*nvec0*Phi202[ijk]*Pi01[ijk] - 
       2*invh02*nvec1*Phi202[ijk]*Pi01[ijk] - 
       2*invh13*nvec0*Phi302[ijk]*Pi01[ijk] - 
       2*invh03*nvec1*Phi302[ijk]*Pi01[ijk] - 
       2*invh02*nvec0*Phi002[ijk]*Pi02[ijk] - 
       2*invh00*nvec2*Phi002[ijk]*Pi02[ijk] - 
       2*invh12*nvec0*Phi102[ijk]*Pi02[ijk] - 
       2*invh01*nvec2*Phi102[ijk]*Pi02[ijk] - 
       2*invh22*nvec0*Phi202[ijk]*Pi02[ijk] - 
       2*invh02*nvec2*Phi202[ijk]*Pi02[ijk] - 
       2*invh23*nvec0*Phi302[ijk]*Pi02[ijk] - 
       2*invh03*nvec2*Phi302[ijk]*Pi02[ijk] - 
       4*invg00*Pi00[ijk]*Pi02[ijk] - Power(nvec0,2)*Pi00[ijk]*Pi02[ijk] - 
       4*invg01*Pi01[ijk]*Pi02[ijk] - 2*nvec0*nvec1*Pi01[ijk]*Pi02[ijk] - 
       4*invg02*Power(Pi02[ijk],2) - 2*nvec0*nvec2*Power(Pi02[ijk],2) - 
       2*invh03*nvec0*Phi002[ijk]*Pi03[ijk] - 
       2*invh00*nvec3*Phi002[ijk]*Pi03[ijk] - 
       2*invh13*nvec0*Phi102[ijk]*Pi03[ijk] - 
       2*invh01*nvec3*Phi102[ijk]*Pi03[ijk] - 
       2*invh23*nvec0*Phi202[ijk]*Pi03[ijk] - 
       2*invh02*nvec3*Phi202[ijk]*Pi03[ijk] - 
       2*invh33*nvec0*Phi302[ijk]*Pi03[ijk] - 
       2*invh03*nvec3*Phi302[ijk]*Pi03[ijk] - 
       4*invg03*Pi02[ijk]*Pi03[ijk] - 2*nvec0*nvec3*Pi02[ijk]*Pi03[ijk] - 
       2*invh01*nvec1*Phi002[ijk]*Pi11[ijk] - 
       2*invh11*nvec1*Phi102[ijk]*Pi11[ijk] - 
       2*invh12*nvec1*Phi202[ijk]*Pi11[ijk] - 
       2*invh13*nvec1*Phi302[ijk]*Pi11[ijk] - 
       Power(nvec1,2)*Pi02[ijk]*Pi11[ijk] - 
       2*invh02*nvec1*Phi002[ijk]*Pi12[ijk] - 
       2*invh01*nvec2*Phi002[ijk]*Pi12[ijk] - 
       2*invh12*nvec1*Phi102[ijk]*Pi12[ijk] - 
       2*invh11*nvec2*Phi102[ijk]*Pi12[ijk] - 
       2*invh22*nvec1*Phi202[ijk]*Pi12[ijk] - 
       2*invh12*nvec2*Phi202[ijk]*Pi12[ijk] - 
       2*invh23*nvec1*Phi302[ijk]*Pi12[ijk] - 
       2*invh13*nvec2*Phi302[ijk]*Pi12[ijk] - 
       4*invg01*Pi00[ijk]*Pi12[ijk] - 4*invg11*Pi01[ijk]*Pi12[ijk] - 
       4*invg12*Pi02[ijk]*Pi12[ijk] - 2*nvec1*nvec2*Pi02[ijk]*Pi12[ijk] - 
       4*invg13*Pi03[ijk]*Pi12[ijk] - 
       2*invh03*nvec1*Phi002[ijk]*Pi13[ijk] - 
       2*invh01*nvec3*Phi002[ijk]*Pi13[ijk] - 
       2*invh13*nvec1*Phi102[ijk]*Pi13[ijk] - 
       2*invh11*nvec3*Phi102[ijk]*Pi13[ijk] - 
       2*invh23*nvec1*Phi202[ijk]*Pi13[ijk] - 
       2*invh12*nvec3*Phi202[ijk]*Pi13[ijk] - 
       2*invh33*nvec1*Phi302[ijk]*Pi13[ijk] - 
       2*invh13*nvec3*Phi302[ijk]*Pi13[ijk] - 
       2*nvec1*nvec3*Pi02[ijk]*Pi13[ijk] - 
       2*invh02*nvec2*Phi002[ijk]*Pi22[ijk] - 
       2*invh12*nvec2*Phi102[ijk]*Pi22[ijk] - 
       2*invh22*nvec2*Phi202[ijk]*Pi22[ijk] - 
       2*invh23*nvec2*Phi302[ijk]*Pi22[ijk] - 
       4*invg02*Pi00[ijk]*Pi22[ijk] - 4*invg12*Pi01[ijk]*Pi22[ijk] - 
       4*invg22*Pi02[ijk]*Pi22[ijk] - Power(nvec2,2)*Pi02[ijk]*Pi22[ijk] - 
       4*invg23*Pi03[ijk]*Pi22[ijk] - 
       2*invh03*nvec2*Phi002[ijk]*Pi23[ijk] - 
       2*invh02*nvec3*Phi002[ijk]*Pi23[ijk] - 
       2*invh13*nvec2*Phi102[ijk]*Pi23[ijk] - 
       2*invh12*nvec3*Phi102[ijk]*Pi23[ijk] - 
       2*invh23*nvec2*Phi202[ijk]*Pi23[ijk] - 
       2*invh22*nvec3*Phi202[ijk]*Pi23[ijk] - 
       2*invh33*nvec2*Phi302[ijk]*Pi23[ijk] - 
       2*invh23*nvec3*Phi302[ijk]*Pi23[ijk] - 
       4*invg03*Pi00[ijk]*Pi23[ijk] - 4*invg13*Pi01[ijk]*Pi23[ijk] - 
       4*invg23*Pi02[ijk]*Pi23[ijk] - 2*nvec2*nvec3*Pi02[ijk]*Pi23[ijk] - 
       4*invg33*Pi03[ijk]*Pi23[ijk] - 
       2*invh03*nvec3*Phi002[ijk]*Pi33[ijk] - 
       2*invh13*nvec3*Phi102[ijk]*Pi33[ijk] - 
       2*invh23*nvec3*Phi202[ijk]*Pi33[ijk] - 
       2*invh33*nvec3*Phi302[ijk]*Pi33[ijk] - 
       Power(nvec3,2)*Pi02[ijk]*Pi33[ijk]) - 
    2*(gamma1*gamma2*beta0[ijk]*Phi002[ijk] + 
       gamma1*gamma2*beta1[ijk]*Phi102[ijk] + 
       gamma1*gamma2*beta2[ijk]*Phi202[ijk] + 
       gamma1*gamma2*beta3[ijk]*Phi302[ijk] + srcSdH02[ijk]))/2.
;

dtPi03[ijk]
=
(-2*interior*AdPi03[ijk] + alpha[ijk]*
     (-4*Gam000*Gam300*Power(invg00,2) - 8*Gam001*Gam300*invg00*invg01 - 
       8*Gam000*Gam301*invg00*invg01 - 4*Gam011*Gam300*Power(invg01,2) - 
       8*Gam001*Gam301*Power(invg01,2) - 4*Gam000*Gam311*Power(invg01,2) - 
       8*Gam002*Gam300*invg00*invg02 - 8*Gam000*Gam302*invg00*invg02 - 
       8*Gam012*Gam300*invg01*invg02 - 8*Gam002*Gam301*invg01*invg02 - 
       8*Gam001*Gam302*invg01*invg02 - 8*Gam000*Gam312*invg01*invg02 - 
       4*Gam022*Gam300*Power(invg02,2) - 8*Gam002*Gam302*Power(invg02,2) - 
       4*Gam000*Gam322*Power(invg02,2) - 8*Gam003*Gam300*invg00*invg03 - 
       8*Gam000*Gam303*invg00*invg03 - 8*Gam013*Gam300*invg01*invg03 - 
       8*Gam003*Gam301*invg01*invg03 - 8*Gam001*Gam303*invg01*invg03 - 
       8*Gam000*Gam313*invg01*invg03 - 8*Gam023*Gam300*invg02*invg03 - 
       8*Gam003*Gam302*invg02*invg03 - 8*Gam002*Gam303*invg02*invg03 - 
       8*Gam000*Gam323*invg02*invg03 - 4*Gam033*Gam300*Power(invg03,2) - 
       8*Gam003*Gam303*Power(invg03,2) - 4*Gam000*Gam333*Power(invg03,2) - 
       8*Gam001*Gam301*invg00*invg11 - 8*Gam011*Gam301*invg01*invg11 - 
       8*Gam001*Gam311*invg01*invg11 - 8*Gam012*Gam301*invg02*invg11 - 
       8*Gam001*Gam312*invg02*invg11 - 8*Gam013*Gam301*invg03*invg11 - 
       8*Gam001*Gam313*invg03*invg11 - 4*Gam011*Gam311*Power(invg11,2) - 
       8*Gam002*Gam301*invg00*invg12 - 8*Gam001*Gam302*invg00*invg12 - 
       8*Gam012*Gam301*invg01*invg12 - 8*Gam011*Gam302*invg01*invg12 - 
       8*Gam002*Gam311*invg01*invg12 - 8*Gam001*Gam312*invg01*invg12 - 
       8*Gam022*Gam301*invg02*invg12 - 8*Gam012*Gam302*invg02*invg12 - 
       8*Gam002*Gam312*invg02*invg12 - 8*Gam001*Gam322*invg02*invg12 - 
       8*Gam023*Gam301*invg03*invg12 - 8*Gam013*Gam302*invg03*invg12 - 
       8*Gam002*Gam313*invg03*invg12 - 8*Gam001*Gam323*invg03*invg12 - 
       8*Gam012*Gam311*invg11*invg12 - 8*Gam011*Gam312*invg11*invg12 - 
       4*Gam022*Gam311*Power(invg12,2) - 8*Gam012*Gam312*Power(invg12,2) - 
       4*Gam011*Gam322*Power(invg12,2) - 8*Gam003*Gam301*invg00*invg13 - 
       8*Gam001*Gam303*invg00*invg13 - 8*Gam013*Gam301*invg01*invg13 - 
       8*Gam011*Gam303*invg01*invg13 - 8*Gam003*Gam311*invg01*invg13 - 
       8*Gam001*Gam313*invg01*invg13 - 8*Gam023*Gam301*invg02*invg13 - 
       8*Gam012*Gam303*invg02*invg13 - 8*Gam003*Gam312*invg02*invg13 - 
       8*Gam001*Gam323*invg02*invg13 - 8*Gam033*Gam301*invg03*invg13 - 
       8*Gam013*Gam303*invg03*invg13 - 8*Gam003*Gam313*invg03*invg13 - 
       8*Gam001*Gam333*invg03*invg13 - 8*Gam013*Gam311*invg11*invg13 - 
       8*Gam011*Gam313*invg11*invg13 - 8*Gam023*Gam311*invg12*invg13 - 
       8*Gam013*Gam312*invg12*invg13 - 8*Gam012*Gam313*invg12*invg13 - 
       8*Gam011*Gam323*invg12*invg13 - 4*Gam033*Gam311*Power(invg13,2) - 
       8*Gam013*Gam313*Power(invg13,2) - 4*Gam011*Gam333*Power(invg13,2) - 
       8*Gam002*Gam302*invg00*invg22 - 8*Gam012*Gam302*invg01*invg22 - 
       8*Gam002*Gam312*invg01*invg22 - 8*Gam022*Gam302*invg02*invg22 - 
       8*Gam002*Gam322*invg02*invg22 - 8*Gam023*Gam302*invg03*invg22 - 
       8*Gam002*Gam323*invg03*invg22 - 8*Gam012*Gam312*invg11*invg22 - 
       8*Gam022*Gam312*invg12*invg22 - 8*Gam012*Gam322*invg12*invg22 - 
       8*Gam023*Gam312*invg13*invg22 - 8*Gam012*Gam323*invg13*invg22 - 
       4*Gam022*Gam322*Power(invg22,2) - 8*Gam003*Gam302*invg00*invg23 - 
       8*Gam002*Gam303*invg00*invg23 - 8*Gam013*Gam302*invg01*invg23 - 
       8*Gam012*Gam303*invg01*invg23 - 8*Gam003*Gam312*invg01*invg23 - 
       8*Gam002*Gam313*invg01*invg23 - 8*Gam023*Gam302*invg02*invg23 - 
       8*Gam022*Gam303*invg02*invg23 - 8*Gam003*Gam322*invg02*invg23 - 
       8*Gam002*Gam323*invg02*invg23 - 8*Gam033*Gam302*invg03*invg23 - 
       8*Gam023*Gam303*invg03*invg23 - 8*Gam003*Gam323*invg03*invg23 - 
       8*Gam002*Gam333*invg03*invg23 - 8*Gam013*Gam312*invg11*invg23 - 
       8*Gam012*Gam313*invg11*invg23 - 8*Gam023*Gam312*invg12*invg23 - 
       8*Gam022*Gam313*invg12*invg23 - 8*Gam013*Gam322*invg12*invg23 - 
       8*Gam012*Gam323*invg12*invg23 - 8*Gam033*Gam312*invg13*invg23 - 
       8*Gam023*Gam313*invg13*invg23 - 8*Gam013*Gam323*invg13*invg23 - 
       8*Gam012*Gam333*invg13*invg23 - 8*Gam023*Gam322*invg22*invg23 - 
       8*Gam022*Gam323*invg22*invg23 - 4*Gam033*Gam322*Power(invg23,2) - 
       8*Gam023*Gam323*Power(invg23,2) - 4*Gam022*Gam333*Power(invg23,2) - 
       8*Gam003*Gam303*invg00*invg33 - 8*Gam013*Gam303*invg01*invg33 - 
       8*Gam003*Gam313*invg01*invg33 - 8*Gam023*Gam303*invg02*invg33 - 
       8*Gam003*Gam323*invg02*invg33 - 8*Gam033*Gam303*invg03*invg33 - 
       8*Gam003*Gam333*invg03*invg33 - 8*Gam013*Gam313*invg11*invg33 - 
       8*Gam023*Gam313*invg12*invg33 - 8*Gam013*Gam323*invg12*invg33 - 
       8*Gam033*Gam313*invg13*invg33 - 8*Gam013*Gam333*invg13*invg33 - 
       8*Gam023*Gam323*invg22*invg33 - 8*Gam033*Gam323*invg23*invg33 - 
       8*Gam023*Gam333*invg23*invg33 - 4*Gam033*Gam333*Power(invg33,2) + 
       2*gamma0*ndua3*trGam0 + 2*gamma0*ndua0*trGam3 + 
       2*(2*Gam003*invg00 + 2*Gam103*invg01 + 2*Gam203*invg02 + 
          2*Gam303*invg03 + gamma0*ndua3)*H0[ijk] + 
       4*Gam003*invg01*H1[ijk] + 4*Gam103*invg11*H1[ijk] + 
       4*Gam203*invg12*H1[ijk] + 4*Gam303*invg13*H1[ijk] + 
       4*Gam003*invg02*H2[ijk] + 4*Gam103*invg12*H2[ijk] + 
       4*Gam203*invg22*H2[ijk] + 4*Gam303*invg23*H2[ijk] + 
       4*Gam003*invg03*H3[ijk] + 4*Gam103*invg13*H3[ijk] + 
       4*Gam203*invg23*H3[ijk] + 4*Gam303*invg33*H3[ijk] + 
       2*gamma0*ndua0*H3[ijk] - 
       2*gamma0*g03[ijk]*(nvec0*trGam0 + nvec1*trGam1 + nvec2*trGam2 + 
          nvec3*trGam3 + nvec0*H0[ijk] + nvec1*H1[ijk] + nvec2*H2[ijk] + 
          nvec3*H3[ijk]) + 4*invg00*invh00*Phi000[ijk]*Phi003[ijk] + 
       4*invg01*invh00*Phi001[ijk]*Phi003[ijk] + 
       4*invg02*invh00*Phi002[ijk]*Phi003[ijk] + 
       4*invg03*invh00*Power(Phi003[ijk],2) + 
       4*invg01*invh00*Phi000[ijk]*Phi013[ijk] + 
       4*invg11*invh00*Phi001[ijk]*Phi013[ijk] + 
       4*invg12*invh00*Phi002[ijk]*Phi013[ijk] + 
       4*invg13*invh00*Phi003[ijk]*Phi013[ijk] + 
       4*invg02*invh00*Phi000[ijk]*Phi023[ijk] + 
       4*invg12*invh00*Phi001[ijk]*Phi023[ijk] + 
       4*invg22*invh00*Phi002[ijk]*Phi023[ijk] + 
       4*invg23*invh00*Phi003[ijk]*Phi023[ijk] + 
       4*invg03*invh00*Phi000[ijk]*Phi033[ijk] + 
       4*invg13*invh00*Phi001[ijk]*Phi033[ijk] + 
       4*invg23*invh00*Phi002[ijk]*Phi033[ijk] + 
       4*invg33*invh00*Phi003[ijk]*Phi033[ijk] + 
       4*invg00*invh01*Phi003[ijk]*Phi100[ijk] + 
       4*invg01*invh01*Phi013[ijk]*Phi100[ijk] + 
       4*invg02*invh01*Phi023[ijk]*Phi100[ijk] + 
       4*invg03*invh01*Phi033[ijk]*Phi100[ijk] + 
       4*invg01*invh01*Phi003[ijk]*Phi101[ijk] + 
       4*invg11*invh01*Phi013[ijk]*Phi101[ijk] + 
       4*invg12*invh01*Phi023[ijk]*Phi101[ijk] + 
       4*invg13*invh01*Phi033[ijk]*Phi101[ijk] + 
       4*invg02*invh01*Phi003[ijk]*Phi102[ijk] + 
       4*invg12*invh01*Phi013[ijk]*Phi102[ijk] + 
       4*invg22*invh01*Phi023[ijk]*Phi102[ijk] + 
       4*invg23*invh01*Phi033[ijk]*Phi102[ijk] + 
       4*invg00*invh01*Phi000[ijk]*Phi103[ijk] + 
       4*invg01*invh01*Phi001[ijk]*Phi103[ijk] + 
       4*invg02*invh01*Phi002[ijk]*Phi103[ijk] + 
       8*invg03*invh01*Phi003[ijk]*Phi103[ijk] + 
       4*invg13*invh01*Phi013[ijk]*Phi103[ijk] + 
       4*invg23*invh01*Phi023[ijk]*Phi103[ijk] + 
       4*invg33*invh01*Phi033[ijk]*Phi103[ijk] + 
       4*invg00*invh11*Phi100[ijk]*Phi103[ijk] + 
       4*invg01*invh11*Phi101[ijk]*Phi103[ijk] + 
       4*invg02*invh11*Phi102[ijk]*Phi103[ijk] + 
       4*invg03*invh11*Power(Phi103[ijk],2) + 
       4*invg01*invh01*Phi000[ijk]*Phi113[ijk] + 
       4*invg11*invh01*Phi001[ijk]*Phi113[ijk] + 
       4*invg12*invh01*Phi002[ijk]*Phi113[ijk] + 
       4*invg13*invh01*Phi003[ijk]*Phi113[ijk] + 
       4*invg01*invh11*Phi100[ijk]*Phi113[ijk] + 
       4*invg11*invh11*Phi101[ijk]*Phi113[ijk] + 
       4*invg12*invh11*Phi102[ijk]*Phi113[ijk] + 
       4*invg13*invh11*Phi103[ijk]*Phi113[ijk] + 
       4*invg02*invh01*Phi000[ijk]*Phi123[ijk] + 
       4*invg12*invh01*Phi001[ijk]*Phi123[ijk] + 
       4*invg22*invh01*Phi002[ijk]*Phi123[ijk] + 
       4*invg23*invh01*Phi003[ijk]*Phi123[ijk] + 
       4*invg02*invh11*Phi100[ijk]*Phi123[ijk] + 
       4*invg12*invh11*Phi101[ijk]*Phi123[ijk] + 
       4*invg22*invh11*Phi102[ijk]*Phi123[ijk] + 
       4*invg23*invh11*Phi103[ijk]*Phi123[ijk] + 
       4*invg03*invh01*Phi000[ijk]*Phi133[ijk] + 
       4*invg13*invh01*Phi001[ijk]*Phi133[ijk] + 
       4*invg23*invh01*Phi002[ijk]*Phi133[ijk] + 
       4*invg33*invh01*Phi003[ijk]*Phi133[ijk] + 
       4*invg03*invh11*Phi100[ijk]*Phi133[ijk] + 
       4*invg13*invh11*Phi101[ijk]*Phi133[ijk] + 
       4*invg23*invh11*Phi102[ijk]*Phi133[ijk] + 
       4*invg33*invh11*Phi103[ijk]*Phi133[ijk] + 
       4*invg00*invh02*Phi003[ijk]*Phi200[ijk] + 
       4*invg01*invh02*Phi013[ijk]*Phi200[ijk] + 
       4*invg02*invh02*Phi023[ijk]*Phi200[ijk] + 
       4*invg03*invh02*Phi033[ijk]*Phi200[ijk] + 
       4*invg00*invh12*Phi103[ijk]*Phi200[ijk] + 
       4*invg01*invh12*Phi113[ijk]*Phi200[ijk] + 
       4*invg02*invh12*Phi123[ijk]*Phi200[ijk] + 
       4*invg03*invh12*Phi133[ijk]*Phi200[ijk] + 
       4*invg01*invh02*Phi003[ijk]*Phi201[ijk] + 
       4*invg11*invh02*Phi013[ijk]*Phi201[ijk] + 
       4*invg12*invh02*Phi023[ijk]*Phi201[ijk] + 
       4*invg13*invh02*Phi033[ijk]*Phi201[ijk] + 
       4*invg01*invh12*Phi103[ijk]*Phi201[ijk] + 
       4*invg11*invh12*Phi113[ijk]*Phi201[ijk] + 
       4*invg12*invh12*Phi123[ijk]*Phi201[ijk] + 
       4*invg13*invh12*Phi133[ijk]*Phi201[ijk] + 
       4*invg02*invh02*Phi003[ijk]*Phi202[ijk] + 
       4*invg12*invh02*Phi013[ijk]*Phi202[ijk] + 
       4*invg22*invh02*Phi023[ijk]*Phi202[ijk] + 
       4*invg23*invh02*Phi033[ijk]*Phi202[ijk] + 
       4*invg02*invh12*Phi103[ijk]*Phi202[ijk] + 
       4*invg12*invh12*Phi113[ijk]*Phi202[ijk] + 
       4*invg22*invh12*Phi123[ijk]*Phi202[ijk] + 
       4*invg23*invh12*Phi133[ijk]*Phi202[ijk] + 
       4*invg00*invh02*Phi000[ijk]*Phi203[ijk] + 
       4*invg01*invh02*Phi001[ijk]*Phi203[ijk] + 
       4*invg02*invh02*Phi002[ijk]*Phi203[ijk] + 
       8*invg03*invh02*Phi003[ijk]*Phi203[ijk] + 
       4*invg13*invh02*Phi013[ijk]*Phi203[ijk] + 
       4*invg23*invh02*Phi023[ijk]*Phi203[ijk] + 
       4*invg33*invh02*Phi033[ijk]*Phi203[ijk] + 
       4*invg00*invh12*Phi100[ijk]*Phi203[ijk] + 
       4*invg01*invh12*Phi101[ijk]*Phi203[ijk] + 
       4*invg02*invh12*Phi102[ijk]*Phi203[ijk] + 
       8*invg03*invh12*Phi103[ijk]*Phi203[ijk] + 
       4*invg13*invh12*Phi113[ijk]*Phi203[ijk] + 
       4*invg23*invh12*Phi123[ijk]*Phi203[ijk] + 
       4*invg33*invh12*Phi133[ijk]*Phi203[ijk] + 
       4*invg00*invh22*Phi200[ijk]*Phi203[ijk] + 
       4*invg01*invh22*Phi201[ijk]*Phi203[ijk] + 
       4*invg02*invh22*Phi202[ijk]*Phi203[ijk] + 
       4*invg03*invh22*Power(Phi203[ijk],2) + 
       4*invg01*invh02*Phi000[ijk]*Phi213[ijk] + 
       4*invg11*invh02*Phi001[ijk]*Phi213[ijk] + 
       4*invg12*invh02*Phi002[ijk]*Phi213[ijk] + 
       4*invg13*invh02*Phi003[ijk]*Phi213[ijk] + 
       4*invg01*invh12*Phi100[ijk]*Phi213[ijk] + 
       4*invg11*invh12*Phi101[ijk]*Phi213[ijk] + 
       4*invg12*invh12*Phi102[ijk]*Phi213[ijk] + 
       4*invg13*invh12*Phi103[ijk]*Phi213[ijk] + 
       4*invg01*invh22*Phi200[ijk]*Phi213[ijk] + 
       4*invg11*invh22*Phi201[ijk]*Phi213[ijk] + 
       4*invg12*invh22*Phi202[ijk]*Phi213[ijk] + 
       4*invg13*invh22*Phi203[ijk]*Phi213[ijk] + 
       4*invg02*invh02*Phi000[ijk]*Phi223[ijk] + 
       4*invg12*invh02*Phi001[ijk]*Phi223[ijk] + 
       4*invg22*invh02*Phi002[ijk]*Phi223[ijk] + 
       4*invg23*invh02*Phi003[ijk]*Phi223[ijk] + 
       4*invg02*invh12*Phi100[ijk]*Phi223[ijk] + 
       4*invg12*invh12*Phi101[ijk]*Phi223[ijk] + 
       4*invg22*invh12*Phi102[ijk]*Phi223[ijk] + 
       4*invg23*invh12*Phi103[ijk]*Phi223[ijk] + 
       4*invg02*invh22*Phi200[ijk]*Phi223[ijk] + 
       4*invg12*invh22*Phi201[ijk]*Phi223[ijk] + 
       4*invg22*invh22*Phi202[ijk]*Phi223[ijk] + 
       4*invg23*invh22*Phi203[ijk]*Phi223[ijk] + 
       4*invg03*invh02*Phi000[ijk]*Phi233[ijk] + 
       4*invg13*invh02*Phi001[ijk]*Phi233[ijk] + 
       4*invg23*invh02*Phi002[ijk]*Phi233[ijk] + 
       4*invg33*invh02*Phi003[ijk]*Phi233[ijk] + 
       4*invg03*invh12*Phi100[ijk]*Phi233[ijk] + 
       4*invg13*invh12*Phi101[ijk]*Phi233[ijk] + 
       4*invg23*invh12*Phi102[ijk]*Phi233[ijk] + 
       4*invg33*invh12*Phi103[ijk]*Phi233[ijk] + 
       4*invg03*invh22*Phi200[ijk]*Phi233[ijk] + 
       4*invg13*invh22*Phi201[ijk]*Phi233[ijk] + 
       4*invg23*invh22*Phi202[ijk]*Phi233[ijk] + 
       4*invg33*invh22*Phi203[ijk]*Phi233[ijk] + 
       4*invg00*invh03*Phi003[ijk]*Phi300[ijk] + 
       4*invg01*invh03*Phi013[ijk]*Phi300[ijk] + 
       4*invg02*invh03*Phi023[ijk]*Phi300[ijk] + 
       4*invg03*invh03*Phi033[ijk]*Phi300[ijk] + 
       4*invg00*invh13*Phi103[ijk]*Phi300[ijk] + 
       4*invg01*invh13*Phi113[ijk]*Phi300[ijk] + 
       4*invg02*invh13*Phi123[ijk]*Phi300[ijk] + 
       4*invg03*invh13*Phi133[ijk]*Phi300[ijk] + 
       4*invg00*invh23*Phi203[ijk]*Phi300[ijk] + 
       4*invg01*invh23*Phi213[ijk]*Phi300[ijk] + 
       4*invg02*invh23*Phi223[ijk]*Phi300[ijk] + 
       4*invg03*invh23*Phi233[ijk]*Phi300[ijk] + 
       4*invg01*invh03*Phi003[ijk]*Phi301[ijk] + 
       4*invg11*invh03*Phi013[ijk]*Phi301[ijk] + 
       4*invg12*invh03*Phi023[ijk]*Phi301[ijk] + 
       4*invg13*invh03*Phi033[ijk]*Phi301[ijk] + 
       4*invg01*invh13*Phi103[ijk]*Phi301[ijk] + 
       4*invg11*invh13*Phi113[ijk]*Phi301[ijk] + 
       4*invg12*invh13*Phi123[ijk]*Phi301[ijk] + 
       4*invg13*invh13*Phi133[ijk]*Phi301[ijk] + 
       4*invg01*invh23*Phi203[ijk]*Phi301[ijk] + 
       4*invg11*invh23*Phi213[ijk]*Phi301[ijk] + 
       4*invg12*invh23*Phi223[ijk]*Phi301[ijk] + 
       4*invg13*invh23*Phi233[ijk]*Phi301[ijk] + 
       4*invg02*invh03*Phi003[ijk]*Phi302[ijk] + 
       4*invg12*invh03*Phi013[ijk]*Phi302[ijk] + 
       4*invg22*invh03*Phi023[ijk]*Phi302[ijk] + 
       4*invg23*invh03*Phi033[ijk]*Phi302[ijk] + 
       4*invg02*invh13*Phi103[ijk]*Phi302[ijk] + 
       4*invg12*invh13*Phi113[ijk]*Phi302[ijk] + 
       4*invg22*invh13*Phi123[ijk]*Phi302[ijk] + 
       4*invg23*invh13*Phi133[ijk]*Phi302[ijk] + 
       4*invg02*invh23*Phi203[ijk]*Phi302[ijk] + 
       4*invg12*invh23*Phi213[ijk]*Phi302[ijk] + 
       4*invg22*invh23*Phi223[ijk]*Phi302[ijk] + 
       4*invg23*invh23*Phi233[ijk]*Phi302[ijk] + 
       4*invg00*invh03*Phi000[ijk]*Phi303[ijk] + 
       4*invg01*invh03*Phi001[ijk]*Phi303[ijk] + 
       4*invg02*invh03*Phi002[ijk]*Phi303[ijk] + 
       8*invg03*invh03*Phi003[ijk]*Phi303[ijk] + 
       4*invg13*invh03*Phi013[ijk]*Phi303[ijk] + 
       4*invg23*invh03*Phi023[ijk]*Phi303[ijk] + 
       4*invg33*invh03*Phi033[ijk]*Phi303[ijk] + 
       4*invg00*invh13*Phi100[ijk]*Phi303[ijk] + 
       4*invg01*invh13*Phi101[ijk]*Phi303[ijk] + 
       4*invg02*invh13*Phi102[ijk]*Phi303[ijk] + 
       8*invg03*invh13*Phi103[ijk]*Phi303[ijk] + 
       4*invg13*invh13*Phi113[ijk]*Phi303[ijk] + 
       4*invg23*invh13*Phi123[ijk]*Phi303[ijk] + 
       4*invg33*invh13*Phi133[ijk]*Phi303[ijk] + 
       4*invg00*invh23*Phi200[ijk]*Phi303[ijk] + 
       4*invg01*invh23*Phi201[ijk]*Phi303[ijk] + 
       4*invg02*invh23*Phi202[ijk]*Phi303[ijk] + 
       8*invg03*invh23*Phi203[ijk]*Phi303[ijk] + 
       4*invg13*invh23*Phi213[ijk]*Phi303[ijk] + 
       4*invg23*invh23*Phi223[ijk]*Phi303[ijk] + 
       4*invg33*invh23*Phi233[ijk]*Phi303[ijk] + 
       4*invg00*invh33*Phi300[ijk]*Phi303[ijk] + 
       4*invg01*invh33*Phi301[ijk]*Phi303[ijk] + 
       4*invg02*invh33*Phi302[ijk]*Phi303[ijk] + 
       4*invg03*invh33*Power(Phi303[ijk],2) + 
       4*invg01*invh03*Phi000[ijk]*Phi313[ijk] + 
       4*invg11*invh03*Phi001[ijk]*Phi313[ijk] + 
       4*invg12*invh03*Phi002[ijk]*Phi313[ijk] + 
       4*invg13*invh03*Phi003[ijk]*Phi313[ijk] + 
       4*invg01*invh13*Phi100[ijk]*Phi313[ijk] + 
       4*invg11*invh13*Phi101[ijk]*Phi313[ijk] + 
       4*invg12*invh13*Phi102[ijk]*Phi313[ijk] + 
       4*invg13*invh13*Phi103[ijk]*Phi313[ijk] + 
       4*invg01*invh23*Phi200[ijk]*Phi313[ijk] + 
       4*invg11*invh23*Phi201[ijk]*Phi313[ijk] + 
       4*invg12*invh23*Phi202[ijk]*Phi313[ijk] + 
       4*invg13*invh23*Phi203[ijk]*Phi313[ijk] + 
       4*invg01*invh33*Phi300[ijk]*Phi313[ijk] + 
       4*invg11*invh33*Phi301[ijk]*Phi313[ijk] + 
       4*invg12*invh33*Phi302[ijk]*Phi313[ijk] + 
       4*invg13*invh33*Phi303[ijk]*Phi313[ijk] + 
       4*invg02*invh03*Phi000[ijk]*Phi323[ijk] + 
       4*invg12*invh03*Phi001[ijk]*Phi323[ijk] + 
       4*invg22*invh03*Phi002[ijk]*Phi323[ijk] + 
       4*invg23*invh03*Phi003[ijk]*Phi323[ijk] + 
       4*invg02*invh13*Phi100[ijk]*Phi323[ijk] + 
       4*invg12*invh13*Phi101[ijk]*Phi323[ijk] + 
       4*invg22*invh13*Phi102[ijk]*Phi323[ijk] + 
       4*invg23*invh13*Phi103[ijk]*Phi323[ijk] + 
       4*invg02*invh23*Phi200[ijk]*Phi323[ijk] + 
       4*invg12*invh23*Phi201[ijk]*Phi323[ijk] + 
       4*invg22*invh23*Phi202[ijk]*Phi323[ijk] + 
       4*invg23*invh23*Phi203[ijk]*Phi323[ijk] + 
       4*invg02*invh33*Phi300[ijk]*Phi323[ijk] + 
       4*invg12*invh33*Phi301[ijk]*Phi323[ijk] + 
       4*invg22*invh33*Phi302[ijk]*Phi323[ijk] + 
       4*invg23*invh33*Phi303[ijk]*Phi323[ijk] + 
       4*invg03*invh03*Phi000[ijk]*Phi333[ijk] + 
       4*invg13*invh03*Phi001[ijk]*Phi333[ijk] + 
       4*invg23*invh03*Phi002[ijk]*Phi333[ijk] + 
       4*invg33*invh03*Phi003[ijk]*Phi333[ijk] + 
       4*invg03*invh13*Phi100[ijk]*Phi333[ijk] + 
       4*invg13*invh13*Phi101[ijk]*Phi333[ijk] + 
       4*invg23*invh13*Phi102[ijk]*Phi333[ijk] + 
       4*invg33*invh13*Phi103[ijk]*Phi333[ijk] + 
       4*invg03*invh23*Phi200[ijk]*Phi333[ijk] + 
       4*invg13*invh23*Phi201[ijk]*Phi333[ijk] + 
       4*invg23*invh23*Phi202[ijk]*Phi333[ijk] + 
       4*invg33*invh23*Phi203[ijk]*Phi333[ijk] + 
       4*invg03*invh33*Phi300[ijk]*Phi333[ijk] + 
       4*invg13*invh33*Phi301[ijk]*Phi333[ijk] + 
       4*invg23*invh33*Phi302[ijk]*Phi333[ijk] + 
       4*invg33*invh33*Phi303[ijk]*Phi333[ijk] - 
       2*invh00*nvec0*Phi003[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi103[ijk]*Pi00[ijk] - 
       2*invh02*nvec0*Phi203[ijk]*Pi00[ijk] - 
       2*invh03*nvec0*Phi303[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi003[ijk]*Pi01[ijk] - 
       2*invh00*nvec1*Phi003[ijk]*Pi01[ijk] - 
       2*invh11*nvec0*Phi103[ijk]*Pi01[ijk] - 
       2*invh01*nvec1*Phi103[ijk]*Pi01[ijk] - 
       2*invh12*nvec0*Phi203[ijk]*Pi01[ijk] - 
       2*invh02*nvec1*Phi203[ijk]*Pi01[ijk] - 
       2*invh13*nvec0*Phi303[ijk]*Pi01[ijk] - 
       2*invh03*nvec1*Phi303[ijk]*Pi01[ijk] - 
       2*invh02*nvec0*Phi003[ijk]*Pi02[ijk] - 
       2*invh00*nvec2*Phi003[ijk]*Pi02[ijk] - 
       2*invh12*nvec0*Phi103[ijk]*Pi02[ijk] - 
       2*invh01*nvec2*Phi103[ijk]*Pi02[ijk] - 
       2*invh22*nvec0*Phi203[ijk]*Pi02[ijk] - 
       2*invh02*nvec2*Phi203[ijk]*Pi02[ijk] - 
       2*invh23*nvec0*Phi303[ijk]*Pi02[ijk] - 
       2*invh03*nvec2*Phi303[ijk]*Pi02[ijk] - 
       2*invh03*nvec0*Phi003[ijk]*Pi03[ijk] - 
       2*invh00*nvec3*Phi003[ijk]*Pi03[ijk] - 
       2*invh13*nvec0*Phi103[ijk]*Pi03[ijk] - 
       2*invh01*nvec3*Phi103[ijk]*Pi03[ijk] - 
       2*invh23*nvec0*Phi203[ijk]*Pi03[ijk] - 
       2*invh02*nvec3*Phi203[ijk]*Pi03[ijk] - 
       2*invh33*nvec0*Phi303[ijk]*Pi03[ijk] - 
       2*invh03*nvec3*Phi303[ijk]*Pi03[ijk] - 
       4*invg00*Pi00[ijk]*Pi03[ijk] - Power(nvec0,2)*Pi00[ijk]*Pi03[ijk] - 
       4*invg01*Pi01[ijk]*Pi03[ijk] - 2*nvec0*nvec1*Pi01[ijk]*Pi03[ijk] - 
       4*invg02*Pi02[ijk]*Pi03[ijk] - 2*nvec0*nvec2*Pi02[ijk]*Pi03[ijk] - 
       4*invg03*Power(Pi03[ijk],2) - 2*nvec0*nvec3*Power(Pi03[ijk],2) - 
       2*invh01*nvec1*Phi003[ijk]*Pi11[ijk] - 
       2*invh11*nvec1*Phi103[ijk]*Pi11[ijk] - 
       2*invh12*nvec1*Phi203[ijk]*Pi11[ijk] - 
       2*invh13*nvec1*Phi303[ijk]*Pi11[ijk] - 
       Power(nvec1,2)*Pi03[ijk]*Pi11[ijk] - 
       2*invh02*nvec1*Phi003[ijk]*Pi12[ijk] - 
       2*invh01*nvec2*Phi003[ijk]*Pi12[ijk] - 
       2*invh12*nvec1*Phi103[ijk]*Pi12[ijk] - 
       2*invh11*nvec2*Phi103[ijk]*Pi12[ijk] - 
       2*invh22*nvec1*Phi203[ijk]*Pi12[ijk] - 
       2*invh12*nvec2*Phi203[ijk]*Pi12[ijk] - 
       2*invh23*nvec1*Phi303[ijk]*Pi12[ijk] - 
       2*invh13*nvec2*Phi303[ijk]*Pi12[ijk] - 
       2*nvec1*nvec2*Pi03[ijk]*Pi12[ijk] - 
       2*invh03*nvec1*Phi003[ijk]*Pi13[ijk] - 
       2*invh01*nvec3*Phi003[ijk]*Pi13[ijk] - 
       2*invh13*nvec1*Phi103[ijk]*Pi13[ijk] - 
       2*invh11*nvec3*Phi103[ijk]*Pi13[ijk] - 
       2*invh23*nvec1*Phi203[ijk]*Pi13[ijk] - 
       2*invh12*nvec3*Phi203[ijk]*Pi13[ijk] - 
       2*invh33*nvec1*Phi303[ijk]*Pi13[ijk] - 
       2*invh13*nvec3*Phi303[ijk]*Pi13[ijk] - 
       4*invg01*Pi00[ijk]*Pi13[ijk] - 4*invg11*Pi01[ijk]*Pi13[ijk] - 
       4*invg12*Pi02[ijk]*Pi13[ijk] - 4*invg13*Pi03[ijk]*Pi13[ijk] - 
       2*nvec1*nvec3*Pi03[ijk]*Pi13[ijk] - 
       2*invh02*nvec2*Phi003[ijk]*Pi22[ijk] - 
       2*invh12*nvec2*Phi103[ijk]*Pi22[ijk] - 
       2*invh22*nvec2*Phi203[ijk]*Pi22[ijk] - 
       2*invh23*nvec2*Phi303[ijk]*Pi22[ijk] - 
       Power(nvec2,2)*Pi03[ijk]*Pi22[ijk] - 
       2*invh03*nvec2*Phi003[ijk]*Pi23[ijk] - 
       2*invh02*nvec3*Phi003[ijk]*Pi23[ijk] - 
       2*invh13*nvec2*Phi103[ijk]*Pi23[ijk] - 
       2*invh12*nvec3*Phi103[ijk]*Pi23[ijk] - 
       2*invh23*nvec2*Phi203[ijk]*Pi23[ijk] - 
       2*invh22*nvec3*Phi203[ijk]*Pi23[ijk] - 
       2*invh33*nvec2*Phi303[ijk]*Pi23[ijk] - 
       2*invh23*nvec3*Phi303[ijk]*Pi23[ijk] - 
       4*invg02*Pi00[ijk]*Pi23[ijk] - 4*invg12*Pi01[ijk]*Pi23[ijk] - 
       4*invg22*Pi02[ijk]*Pi23[ijk] - 4*invg23*Pi03[ijk]*Pi23[ijk] - 
       2*nvec2*nvec3*Pi03[ijk]*Pi23[ijk] - 
       2*invh03*nvec3*Phi003[ijk]*Pi33[ijk] - 
       2*invh13*nvec3*Phi103[ijk]*Pi33[ijk] - 
       2*invh23*nvec3*Phi203[ijk]*Pi33[ijk] - 
       2*invh33*nvec3*Phi303[ijk]*Pi33[ijk] - 
       4*invg03*Pi00[ijk]*Pi33[ijk] - 4*invg13*Pi01[ijk]*Pi33[ijk] - 
       4*invg23*Pi02[ijk]*Pi33[ijk] - 4*invg33*Pi03[ijk]*Pi33[ijk] - 
       Power(nvec3,2)*Pi03[ijk]*Pi33[ijk]) - 
    2*(gamma1*gamma2*beta0[ijk]*Phi003[ijk] + 
       gamma1*gamma2*beta1[ijk]*Phi103[ijk] + 
       gamma1*gamma2*beta2[ijk]*Phi203[ijk] + 
       gamma1*gamma2*beta3[ijk]*Phi303[ijk] + srcSdH03[ijk]))/2.
;

dtPi11[ijk]
=
-(interior*AdPi11[ijk]) - gamma1*gamma2*beta0[ijk]*Phi011[ijk] - 
  gamma1*gamma2*beta1[ijk]*Phi111[ijk] - 
  gamma1*gamma2*beta2[ijk]*Phi211[ijk] - 
  gamma1*gamma2*beta3[ijk]*Phi311[ijk] - 
  (alpha[ijk]*(4*Power(Gam100,2)*Power(invg00,2) + 
       16*Gam100*Gam101*invg00*invg01 + 
       8*Power(Gam101,2)*Power(invg01,2) + 
       8*Gam100*Gam111*Power(invg01,2) + 16*Gam100*Gam102*invg00*invg02 + 
       16*Gam101*Gam102*invg01*invg02 + 16*Gam100*Gam112*invg01*invg02 + 
       8*Power(Gam102,2)*Power(invg02,2) + 
       8*Gam100*Gam122*Power(invg02,2) + 16*Gam100*Gam103*invg00*invg03 + 
       16*Gam101*Gam103*invg01*invg03 + 16*Gam100*Gam113*invg01*invg03 + 
       16*Gam102*Gam103*invg02*invg03 + 16*Gam100*Gam123*invg02*invg03 + 
       8*Power(Gam103,2)*Power(invg03,2) + 
       8*Gam100*Gam133*Power(invg03,2) + 8*Power(Gam101,2)*invg00*invg11 + 
       16*Gam101*Gam111*invg01*invg11 + 16*Gam101*Gam112*invg02*invg11 + 
       16*Gam101*Gam113*invg03*invg11 + 
       4*Power(Gam111,2)*Power(invg11,2) + 
       16*Gam101*Gam102*invg00*invg12 + 16*Gam102*Gam111*invg01*invg12 + 
       16*Gam101*Gam112*invg01*invg12 + 16*Gam102*Gam112*invg02*invg12 + 
       16*Gam101*Gam122*invg02*invg12 + 16*Gam102*Gam113*invg03*invg12 + 
       16*Gam101*Gam123*invg03*invg12 + 16*Gam111*Gam112*invg11*invg12 + 
       8*Power(Gam112,2)*Power(invg12,2) + 
       8*Gam111*Gam122*Power(invg12,2) + 16*Gam101*Gam103*invg00*invg13 + 
       16*Gam103*Gam111*invg01*invg13 + 16*Gam101*Gam113*invg01*invg13 + 
       16*Gam103*Gam112*invg02*invg13 + 16*Gam101*Gam123*invg02*invg13 + 
       16*Gam103*Gam113*invg03*invg13 + 16*Gam101*Gam133*invg03*invg13 + 
       16*Gam111*Gam113*invg11*invg13 + 16*Gam112*Gam113*invg12*invg13 + 
       16*Gam111*Gam123*invg12*invg13 + 
       8*Power(Gam113,2)*Power(invg13,2) + 
       8*Gam111*Gam133*Power(invg13,2) + 8*Power(Gam102,2)*invg00*invg22 + 
       16*Gam102*Gam112*invg01*invg22 + 16*Gam102*Gam122*invg02*invg22 + 
       16*Gam102*Gam123*invg03*invg22 + 8*Power(Gam112,2)*invg11*invg22 + 
       16*Gam112*Gam122*invg12*invg22 + 16*Gam112*Gam123*invg13*invg22 + 
       4*Power(Gam122,2)*Power(invg22,2) + 
       16*Gam102*Gam103*invg00*invg23 + 16*Gam103*Gam112*invg01*invg23 + 
       16*Gam102*Gam113*invg01*invg23 + 16*Gam103*Gam122*invg02*invg23 + 
       16*Gam102*Gam123*invg02*invg23 + 16*Gam103*Gam123*invg03*invg23 + 
       16*Gam102*Gam133*invg03*invg23 + 16*Gam112*Gam113*invg11*invg23 + 
       16*Gam113*Gam122*invg12*invg23 + 16*Gam112*Gam123*invg12*invg23 + 
       16*Gam113*Gam123*invg13*invg23 + 16*Gam112*Gam133*invg13*invg23 + 
       16*Gam122*Gam123*invg22*invg23 + 
       8*Power(Gam123,2)*Power(invg23,2) + 
       8*Gam122*Gam133*Power(invg23,2) + 8*Power(Gam103,2)*invg00*invg33 + 
       16*Gam103*Gam113*invg01*invg33 + 16*Gam103*Gam123*invg02*invg33 + 
       16*Gam103*Gam133*invg03*invg33 + 8*Power(Gam113,2)*invg11*invg33 + 
       16*Gam113*Gam123*invg12*invg33 + 16*Gam113*Gam133*invg13*invg33 + 
       8*Power(Gam123,2)*invg22*invg33 + 16*Gam123*Gam133*invg23*invg33 + 
       4*Power(Gam133,2)*Power(invg33,2) - 4*gamma0*ndua1*trGam1 - 
       4*(Gam011*invg00 + Gam111*invg01 + Gam211*invg02 + Gam311*invg03)*
        H0[ijk] - 4*Gam011*invg01*H1[ijk] - 4*Gam111*invg11*H1[ijk] - 
       4*Gam211*invg12*H1[ijk] - 4*Gam311*invg13*H1[ijk] - 
       4*gamma0*ndua1*H1[ijk] - 4*Gam011*invg02*H2[ijk] - 
       4*Gam111*invg12*H2[ijk] - 4*Gam211*invg22*H2[ijk] - 
       4*Gam311*invg23*H2[ijk] - 4*Gam011*invg03*H3[ijk] - 
       4*Gam111*invg13*H3[ijk] - 4*Gam211*invg23*H3[ijk] - 
       4*Gam311*invg33*H3[ijk] + 
       2*gamma0*g11[ijk]*(nvec0*trGam0 + nvec1*trGam1 + nvec2*trGam2 + 
          nvec3*trGam3 + nvec0*H0[ijk] + nvec1*H1[ijk] + nvec2*H2[ijk] + 
          nvec3*H3[ijk]) - 4*invg00*invh00*Power(Phi001[ijk],2) - 
       8*invg01*invh00*Phi001[ijk]*Phi011[ijk] - 
       4*invg11*invh00*Power(Phi011[ijk],2) - 
       8*invg02*invh00*Phi001[ijk]*Phi012[ijk] - 
       8*invg12*invh00*Phi011[ijk]*Phi012[ijk] - 
       4*invg22*invh00*Power(Phi012[ijk],2) - 
       8*invg03*invh00*Phi001[ijk]*Phi013[ijk] - 
       8*invg13*invh00*Phi011[ijk]*Phi013[ijk] - 
       8*invg23*invh00*Phi012[ijk]*Phi013[ijk] - 
       4*invg33*invh00*Power(Phi013[ijk],2) - 
       8*invg00*invh01*Phi001[ijk]*Phi101[ijk] - 
       8*invg01*invh01*Phi011[ijk]*Phi101[ijk] - 
       8*invg02*invh01*Phi012[ijk]*Phi101[ijk] - 
       8*invg03*invh01*Phi013[ijk]*Phi101[ijk] - 
       4*invg00*invh11*Power(Phi101[ijk],2) - 
       8*invg01*invh01*Phi001[ijk]*Phi111[ijk] - 
       8*invg11*invh01*Phi011[ijk]*Phi111[ijk] - 
       8*invg12*invh01*Phi012[ijk]*Phi111[ijk] - 
       8*invg13*invh01*Phi013[ijk]*Phi111[ijk] - 
       8*invg01*invh11*Phi101[ijk]*Phi111[ijk] - 
       4*invg11*invh11*Power(Phi111[ijk],2) - 
       8*invg02*invh01*Phi001[ijk]*Phi112[ijk] - 
       8*invg12*invh01*Phi011[ijk]*Phi112[ijk] - 
       8*invg22*invh01*Phi012[ijk]*Phi112[ijk] - 
       8*invg23*invh01*Phi013[ijk]*Phi112[ijk] - 
       8*invg02*invh11*Phi101[ijk]*Phi112[ijk] - 
       8*invg12*invh11*Phi111[ijk]*Phi112[ijk] - 
       4*invg22*invh11*Power(Phi112[ijk],2) - 
       8*invg03*invh01*Phi001[ijk]*Phi113[ijk] - 
       8*invg13*invh01*Phi011[ijk]*Phi113[ijk] - 
       8*invg23*invh01*Phi012[ijk]*Phi113[ijk] - 
       8*invg33*invh01*Phi013[ijk]*Phi113[ijk] - 
       8*invg03*invh11*Phi101[ijk]*Phi113[ijk] - 
       8*invg13*invh11*Phi111[ijk]*Phi113[ijk] - 
       8*invg23*invh11*Phi112[ijk]*Phi113[ijk] - 
       4*invg33*invh11*Power(Phi113[ijk],2) - 
       8*invg00*invh02*Phi001[ijk]*Phi201[ijk] - 
       8*invg01*invh02*Phi011[ijk]*Phi201[ijk] - 
       8*invg02*invh02*Phi012[ijk]*Phi201[ijk] - 
       8*invg03*invh02*Phi013[ijk]*Phi201[ijk] - 
       8*invg00*invh12*Phi101[ijk]*Phi201[ijk] - 
       8*invg01*invh12*Phi111[ijk]*Phi201[ijk] - 
       8*invg02*invh12*Phi112[ijk]*Phi201[ijk] - 
       8*invg03*invh12*Phi113[ijk]*Phi201[ijk] - 
       4*invg00*invh22*Power(Phi201[ijk],2) - 
       8*invg01*invh02*Phi001[ijk]*Phi211[ijk] - 
       8*invg11*invh02*Phi011[ijk]*Phi211[ijk] - 
       8*invg12*invh02*Phi012[ijk]*Phi211[ijk] - 
       8*invg13*invh02*Phi013[ijk]*Phi211[ijk] - 
       8*invg01*invh12*Phi101[ijk]*Phi211[ijk] - 
       8*invg11*invh12*Phi111[ijk]*Phi211[ijk] - 
       8*invg12*invh12*Phi112[ijk]*Phi211[ijk] - 
       8*invg13*invh12*Phi113[ijk]*Phi211[ijk] - 
       8*invg01*invh22*Phi201[ijk]*Phi211[ijk] - 
       4*invg11*invh22*Power(Phi211[ijk],2) - 
       8*invg02*invh02*Phi001[ijk]*Phi212[ijk] - 
       8*invg12*invh02*Phi011[ijk]*Phi212[ijk] - 
       8*invg22*invh02*Phi012[ijk]*Phi212[ijk] - 
       8*invg23*invh02*Phi013[ijk]*Phi212[ijk] - 
       8*invg02*invh12*Phi101[ijk]*Phi212[ijk] - 
       8*invg12*invh12*Phi111[ijk]*Phi212[ijk] - 
       8*invg22*invh12*Phi112[ijk]*Phi212[ijk] - 
       8*invg23*invh12*Phi113[ijk]*Phi212[ijk] - 
       8*invg02*invh22*Phi201[ijk]*Phi212[ijk] - 
       8*invg12*invh22*Phi211[ijk]*Phi212[ijk] - 
       4*invg22*invh22*Power(Phi212[ijk],2) - 
       8*invg03*invh02*Phi001[ijk]*Phi213[ijk] - 
       8*invg13*invh02*Phi011[ijk]*Phi213[ijk] - 
       8*invg23*invh02*Phi012[ijk]*Phi213[ijk] - 
       8*invg33*invh02*Phi013[ijk]*Phi213[ijk] - 
       8*invg03*invh12*Phi101[ijk]*Phi213[ijk] - 
       8*invg13*invh12*Phi111[ijk]*Phi213[ijk] - 
       8*invg23*invh12*Phi112[ijk]*Phi213[ijk] - 
       8*invg33*invh12*Phi113[ijk]*Phi213[ijk] - 
       8*invg03*invh22*Phi201[ijk]*Phi213[ijk] - 
       8*invg13*invh22*Phi211[ijk]*Phi213[ijk] - 
       8*invg23*invh22*Phi212[ijk]*Phi213[ijk] - 
       4*invg33*invh22*Power(Phi213[ijk],2) - 
       8*invg00*invh03*Phi001[ijk]*Phi301[ijk] - 
       8*invg01*invh03*Phi011[ijk]*Phi301[ijk] - 
       8*invg02*invh03*Phi012[ijk]*Phi301[ijk] - 
       8*invg03*invh03*Phi013[ijk]*Phi301[ijk] - 
       8*invg00*invh13*Phi101[ijk]*Phi301[ijk] - 
       8*invg01*invh13*Phi111[ijk]*Phi301[ijk] - 
       8*invg02*invh13*Phi112[ijk]*Phi301[ijk] - 
       8*invg03*invh13*Phi113[ijk]*Phi301[ijk] - 
       8*invg00*invh23*Phi201[ijk]*Phi301[ijk] - 
       8*invg01*invh23*Phi211[ijk]*Phi301[ijk] - 
       8*invg02*invh23*Phi212[ijk]*Phi301[ijk] - 
       8*invg03*invh23*Phi213[ijk]*Phi301[ijk] - 
       4*invg00*invh33*Power(Phi301[ijk],2) - 
       8*invg01*invh03*Phi001[ijk]*Phi311[ijk] - 
       8*invg11*invh03*Phi011[ijk]*Phi311[ijk] - 
       8*invg12*invh03*Phi012[ijk]*Phi311[ijk] - 
       8*invg13*invh03*Phi013[ijk]*Phi311[ijk] - 
       8*invg01*invh13*Phi101[ijk]*Phi311[ijk] - 
       8*invg11*invh13*Phi111[ijk]*Phi311[ijk] - 
       8*invg12*invh13*Phi112[ijk]*Phi311[ijk] - 
       8*invg13*invh13*Phi113[ijk]*Phi311[ijk] - 
       8*invg01*invh23*Phi201[ijk]*Phi311[ijk] - 
       8*invg11*invh23*Phi211[ijk]*Phi311[ijk] - 
       8*invg12*invh23*Phi212[ijk]*Phi311[ijk] - 
       8*invg13*invh23*Phi213[ijk]*Phi311[ijk] - 
       8*invg01*invh33*Phi301[ijk]*Phi311[ijk] - 
       4*invg11*invh33*Power(Phi311[ijk],2) - 
       8*invg02*invh03*Phi001[ijk]*Phi312[ijk] - 
       8*invg12*invh03*Phi011[ijk]*Phi312[ijk] - 
       8*invg22*invh03*Phi012[ijk]*Phi312[ijk] - 
       8*invg23*invh03*Phi013[ijk]*Phi312[ijk] - 
       8*invg02*invh13*Phi101[ijk]*Phi312[ijk] - 
       8*invg12*invh13*Phi111[ijk]*Phi312[ijk] - 
       8*invg22*invh13*Phi112[ijk]*Phi312[ijk] - 
       8*invg23*invh13*Phi113[ijk]*Phi312[ijk] - 
       8*invg02*invh23*Phi201[ijk]*Phi312[ijk] - 
       8*invg12*invh23*Phi211[ijk]*Phi312[ijk] - 
       8*invg22*invh23*Phi212[ijk]*Phi312[ijk] - 
       8*invg23*invh23*Phi213[ijk]*Phi312[ijk] - 
       8*invg02*invh33*Phi301[ijk]*Phi312[ijk] - 
       8*invg12*invh33*Phi311[ijk]*Phi312[ijk] - 
       4*invg22*invh33*Power(Phi312[ijk],2) - 
       8*invg03*invh03*Phi001[ijk]*Phi313[ijk] - 
       8*invg13*invh03*Phi011[ijk]*Phi313[ijk] - 
       8*invg23*invh03*Phi012[ijk]*Phi313[ijk] - 
       8*invg33*invh03*Phi013[ijk]*Phi313[ijk] - 
       8*invg03*invh13*Phi101[ijk]*Phi313[ijk] - 
       8*invg13*invh13*Phi111[ijk]*Phi313[ijk] - 
       8*invg23*invh13*Phi112[ijk]*Phi313[ijk] - 
       8*invg33*invh13*Phi113[ijk]*Phi313[ijk] - 
       8*invg03*invh23*Phi201[ijk]*Phi313[ijk] - 
       8*invg13*invh23*Phi211[ijk]*Phi313[ijk] - 
       8*invg23*invh23*Phi212[ijk]*Phi313[ijk] - 
       8*invg33*invh23*Phi213[ijk]*Phi313[ijk] - 
       8*invg03*invh33*Phi301[ijk]*Phi313[ijk] - 
       8*invg13*invh33*Phi311[ijk]*Phi313[ijk] - 
       8*invg23*invh33*Phi312[ijk]*Phi313[ijk] - 
       4*invg33*invh33*Power(Phi313[ijk],2) + 
       2*invh00*nvec0*Phi011[ijk]*Pi00[ijk] + 
       2*invh01*nvec0*Phi111[ijk]*Pi00[ijk] + 
       2*invh02*nvec0*Phi211[ijk]*Pi00[ijk] + 
       2*invh03*nvec0*Phi311[ijk]*Pi00[ijk] + 
       2*invh01*nvec0*Phi011[ijk]*Pi01[ijk] + 
       2*invh00*nvec1*Phi011[ijk]*Pi01[ijk] + 
       2*invh11*nvec0*Phi111[ijk]*Pi01[ijk] + 
       2*invh01*nvec1*Phi111[ijk]*Pi01[ijk] + 
       2*invh12*nvec0*Phi211[ijk]*Pi01[ijk] + 
       2*invh02*nvec1*Phi211[ijk]*Pi01[ijk] + 
       2*invh13*nvec0*Phi311[ijk]*Pi01[ijk] + 
       2*invh03*nvec1*Phi311[ijk]*Pi01[ijk] + 
       4*invg00*Power(Pi01[ijk],2) + 
       2*invh02*nvec0*Phi011[ijk]*Pi02[ijk] + 
       2*invh00*nvec2*Phi011[ijk]*Pi02[ijk] + 
       2*invh12*nvec0*Phi111[ijk]*Pi02[ijk] + 
       2*invh01*nvec2*Phi111[ijk]*Pi02[ijk] + 
       2*invh22*nvec0*Phi211[ijk]*Pi02[ijk] + 
       2*invh02*nvec2*Phi211[ijk]*Pi02[ijk] + 
       2*invh23*nvec0*Phi311[ijk]*Pi02[ijk] + 
       2*invh03*nvec2*Phi311[ijk]*Pi02[ijk] + 
       2*invh03*nvec0*Phi011[ijk]*Pi03[ijk] + 
       2*invh00*nvec3*Phi011[ijk]*Pi03[ijk] + 
       2*invh13*nvec0*Phi111[ijk]*Pi03[ijk] + 
       2*invh01*nvec3*Phi111[ijk]*Pi03[ijk] + 
       2*invh23*nvec0*Phi211[ijk]*Pi03[ijk] + 
       2*invh02*nvec3*Phi211[ijk]*Pi03[ijk] + 
       2*invh33*nvec0*Phi311[ijk]*Pi03[ijk] + 
       2*invh03*nvec3*Phi311[ijk]*Pi03[ijk] + 
       2*invh01*nvec1*Phi011[ijk]*Pi11[ijk] + 
       2*invh11*nvec1*Phi111[ijk]*Pi11[ijk] + 
       2*invh12*nvec1*Phi211[ijk]*Pi11[ijk] + 
       2*invh13*nvec1*Phi311[ijk]*Pi11[ijk] + 
       Power(nvec0,2)*Pi00[ijk]*Pi11[ijk] + 8*invg01*Pi01[ijk]*Pi11[ijk] + 
       2*nvec0*nvec1*Pi01[ijk]*Pi11[ijk] + 
       2*nvec0*nvec2*Pi02[ijk]*Pi11[ijk] + 
       2*nvec0*nvec3*Pi03[ijk]*Pi11[ijk] + 4*invg11*Power(Pi11[ijk],2) + 
       Power(nvec1,2)*Power(Pi11[ijk],2) + 
       2*invh02*nvec1*Phi011[ijk]*Pi12[ijk] + 
       2*invh01*nvec2*Phi011[ijk]*Pi12[ijk] + 
       2*invh12*nvec1*Phi111[ijk]*Pi12[ijk] + 
       2*invh11*nvec2*Phi111[ijk]*Pi12[ijk] + 
       2*invh22*nvec1*Phi211[ijk]*Pi12[ijk] + 
       2*invh12*nvec2*Phi211[ijk]*Pi12[ijk] + 
       2*invh23*nvec1*Phi311[ijk]*Pi12[ijk] + 
       2*invh13*nvec2*Phi311[ijk]*Pi12[ijk] + 
       8*invg02*Pi01[ijk]*Pi12[ijk] + 8*invg12*Pi11[ijk]*Pi12[ijk] + 
       2*nvec1*nvec2*Pi11[ijk]*Pi12[ijk] + 4*invg22*Power(Pi12[ijk],2) + 
       2*invh03*nvec1*Phi011[ijk]*Pi13[ijk] + 
       2*invh01*nvec3*Phi011[ijk]*Pi13[ijk] + 
       2*invh13*nvec1*Phi111[ijk]*Pi13[ijk] + 
       2*invh11*nvec3*Phi111[ijk]*Pi13[ijk] + 
       2*invh23*nvec1*Phi211[ijk]*Pi13[ijk] + 
       2*invh12*nvec3*Phi211[ijk]*Pi13[ijk] + 
       2*invh33*nvec1*Phi311[ijk]*Pi13[ijk] + 
       2*invh13*nvec3*Phi311[ijk]*Pi13[ijk] + 
       8*invg03*Pi01[ijk]*Pi13[ijk] + 8*invg13*Pi11[ijk]*Pi13[ijk] + 
       2*nvec1*nvec3*Pi11[ijk]*Pi13[ijk] + 8*invg23*Pi12[ijk]*Pi13[ijk] + 
       4*invg33*Power(Pi13[ijk],2) + 
       2*invh02*nvec2*Phi011[ijk]*Pi22[ijk] + 
       2*invh12*nvec2*Phi111[ijk]*Pi22[ijk] + 
       2*invh22*nvec2*Phi211[ijk]*Pi22[ijk] + 
       2*invh23*nvec2*Phi311[ijk]*Pi22[ijk] + 
       Power(nvec2,2)*Pi11[ijk]*Pi22[ijk] + 
       2*invh03*nvec2*Phi011[ijk]*Pi23[ijk] + 
       2*invh02*nvec3*Phi011[ijk]*Pi23[ijk] + 
       2*invh13*nvec2*Phi111[ijk]*Pi23[ijk] + 
       2*invh12*nvec3*Phi111[ijk]*Pi23[ijk] + 
       2*invh23*nvec2*Phi211[ijk]*Pi23[ijk] + 
       2*invh22*nvec3*Phi211[ijk]*Pi23[ijk] + 
       2*invh33*nvec2*Phi311[ijk]*Pi23[ijk] + 
       2*invh23*nvec3*Phi311[ijk]*Pi23[ijk] + 
       2*nvec2*nvec3*Pi11[ijk]*Pi23[ijk] + 
       2*invh03*nvec3*Phi011[ijk]*Pi33[ijk] + 
       2*invh13*nvec3*Phi111[ijk]*Pi33[ijk] + 
       2*invh23*nvec3*Phi211[ijk]*Pi33[ijk] + 
       2*invh33*nvec3*Phi311[ijk]*Pi33[ijk] + 
       Power(nvec3,2)*Pi11[ijk]*Pi33[ijk]))/2. - srcSdH11[ijk]
;

dtPi12[ijk]
=
(-2*interior*AdPi12[ijk] + alpha[ijk]*
     (-4*Gam100*Gam200*Power(invg00,2) - 8*Gam101*Gam200*invg00*invg01 - 
       8*Gam100*Gam201*invg00*invg01 - 4*Gam111*Gam200*Power(invg01,2) - 
       8*Gam101*Gam201*Power(invg01,2) - 4*Gam100*Gam211*Power(invg01,2) - 
       8*Gam102*Gam200*invg00*invg02 - 8*Gam100*Gam202*invg00*invg02 - 
       8*Gam112*Gam200*invg01*invg02 - 8*Gam102*Gam201*invg01*invg02 - 
       8*Gam101*Gam202*invg01*invg02 - 8*Gam100*Gam212*invg01*invg02 - 
       4*Gam122*Gam200*Power(invg02,2) - 8*Gam102*Gam202*Power(invg02,2) - 
       4*Gam100*Gam222*Power(invg02,2) - 8*Gam103*Gam200*invg00*invg03 - 
       8*Gam100*Gam203*invg00*invg03 - 8*Gam113*Gam200*invg01*invg03 - 
       8*Gam103*Gam201*invg01*invg03 - 8*Gam101*Gam203*invg01*invg03 - 
       8*Gam100*Gam213*invg01*invg03 - 8*Gam123*Gam200*invg02*invg03 - 
       8*Gam103*Gam202*invg02*invg03 - 8*Gam102*Gam203*invg02*invg03 - 
       8*Gam100*Gam223*invg02*invg03 - 4*Gam133*Gam200*Power(invg03,2) - 
       8*Gam103*Gam203*Power(invg03,2) - 4*Gam100*Gam233*Power(invg03,2) - 
       8*Gam101*Gam201*invg00*invg11 - 8*Gam111*Gam201*invg01*invg11 - 
       8*Gam101*Gam211*invg01*invg11 - 8*Gam112*Gam201*invg02*invg11 - 
       8*Gam101*Gam212*invg02*invg11 - 8*Gam113*Gam201*invg03*invg11 - 
       8*Gam101*Gam213*invg03*invg11 - 4*Gam111*Gam211*Power(invg11,2) - 
       8*Gam102*Gam201*invg00*invg12 - 8*Gam101*Gam202*invg00*invg12 - 
       8*Gam112*Gam201*invg01*invg12 - 8*Gam111*Gam202*invg01*invg12 - 
       8*Gam102*Gam211*invg01*invg12 - 8*Gam101*Gam212*invg01*invg12 - 
       8*Gam122*Gam201*invg02*invg12 - 8*Gam112*Gam202*invg02*invg12 - 
       8*Gam102*Gam212*invg02*invg12 - 8*Gam101*Gam222*invg02*invg12 - 
       8*Gam123*Gam201*invg03*invg12 - 8*Gam113*Gam202*invg03*invg12 - 
       8*Gam102*Gam213*invg03*invg12 - 8*Gam101*Gam223*invg03*invg12 - 
       8*Gam112*Gam211*invg11*invg12 - 8*Gam111*Gam212*invg11*invg12 - 
       4*Gam122*Gam211*Power(invg12,2) - 8*Gam112*Gam212*Power(invg12,2) - 
       4*Gam111*Gam222*Power(invg12,2) - 8*Gam103*Gam201*invg00*invg13 - 
       8*Gam101*Gam203*invg00*invg13 - 8*Gam113*Gam201*invg01*invg13 - 
       8*Gam111*Gam203*invg01*invg13 - 8*Gam103*Gam211*invg01*invg13 - 
       8*Gam101*Gam213*invg01*invg13 - 8*Gam123*Gam201*invg02*invg13 - 
       8*Gam112*Gam203*invg02*invg13 - 8*Gam103*Gam212*invg02*invg13 - 
       8*Gam101*Gam223*invg02*invg13 - 8*Gam133*Gam201*invg03*invg13 - 
       8*Gam113*Gam203*invg03*invg13 - 8*Gam103*Gam213*invg03*invg13 - 
       8*Gam101*Gam233*invg03*invg13 - 8*Gam113*Gam211*invg11*invg13 - 
       8*Gam111*Gam213*invg11*invg13 - 8*Gam123*Gam211*invg12*invg13 - 
       8*Gam113*Gam212*invg12*invg13 - 8*Gam112*Gam213*invg12*invg13 - 
       8*Gam111*Gam223*invg12*invg13 - 4*Gam133*Gam211*Power(invg13,2) - 
       8*Gam113*Gam213*Power(invg13,2) - 4*Gam111*Gam233*Power(invg13,2) - 
       8*Gam102*Gam202*invg00*invg22 - 8*Gam112*Gam202*invg01*invg22 - 
       8*Gam102*Gam212*invg01*invg22 - 8*Gam122*Gam202*invg02*invg22 - 
       8*Gam102*Gam222*invg02*invg22 - 8*Gam123*Gam202*invg03*invg22 - 
       8*Gam102*Gam223*invg03*invg22 - 8*Gam112*Gam212*invg11*invg22 - 
       8*Gam122*Gam212*invg12*invg22 - 8*Gam112*Gam222*invg12*invg22 - 
       8*Gam123*Gam212*invg13*invg22 - 8*Gam112*Gam223*invg13*invg22 - 
       4*Gam122*Gam222*Power(invg22,2) - 8*Gam103*Gam202*invg00*invg23 - 
       8*Gam102*Gam203*invg00*invg23 - 8*Gam113*Gam202*invg01*invg23 - 
       8*Gam112*Gam203*invg01*invg23 - 8*Gam103*Gam212*invg01*invg23 - 
       8*Gam102*Gam213*invg01*invg23 - 8*Gam123*Gam202*invg02*invg23 - 
       8*Gam122*Gam203*invg02*invg23 - 8*Gam103*Gam222*invg02*invg23 - 
       8*Gam102*Gam223*invg02*invg23 - 8*Gam133*Gam202*invg03*invg23 - 
       8*Gam123*Gam203*invg03*invg23 - 8*Gam103*Gam223*invg03*invg23 - 
       8*Gam102*Gam233*invg03*invg23 - 8*Gam113*Gam212*invg11*invg23 - 
       8*Gam112*Gam213*invg11*invg23 - 8*Gam123*Gam212*invg12*invg23 - 
       8*Gam122*Gam213*invg12*invg23 - 8*Gam113*Gam222*invg12*invg23 - 
       8*Gam112*Gam223*invg12*invg23 - 8*Gam133*Gam212*invg13*invg23 - 
       8*Gam123*Gam213*invg13*invg23 - 8*Gam113*Gam223*invg13*invg23 - 
       8*Gam112*Gam233*invg13*invg23 - 8*Gam123*Gam222*invg22*invg23 - 
       8*Gam122*Gam223*invg22*invg23 - 4*Gam133*Gam222*Power(invg23,2) - 
       8*Gam123*Gam223*Power(invg23,2) - 4*Gam122*Gam233*Power(invg23,2) - 
       8*Gam103*Gam203*invg00*invg33 - 8*Gam113*Gam203*invg01*invg33 - 
       8*Gam103*Gam213*invg01*invg33 - 8*Gam123*Gam203*invg02*invg33 - 
       8*Gam103*Gam223*invg02*invg33 - 8*Gam133*Gam203*invg03*invg33 - 
       8*Gam103*Gam233*invg03*invg33 - 8*Gam113*Gam213*invg11*invg33 - 
       8*Gam123*Gam213*invg12*invg33 - 8*Gam113*Gam223*invg12*invg33 - 
       8*Gam133*Gam213*invg13*invg33 - 8*Gam113*Gam233*invg13*invg33 - 
       8*Gam123*Gam223*invg22*invg33 - 8*Gam133*Gam223*invg23*invg33 - 
       8*Gam123*Gam233*invg23*invg33 - 4*Gam133*Gam233*Power(invg33,2) + 
       2*gamma0*ndua2*trGam1 + 2*gamma0*ndua1*trGam2 + 
       4*(Gam012*invg00 + Gam112*invg01 + Gam212*invg02 + Gam312*invg03)*
        H0[ijk] + 4*Gam012*invg01*H1[ijk] + 4*Gam112*invg11*H1[ijk] + 
       4*Gam212*invg12*H1[ijk] + 4*Gam312*invg13*H1[ijk] + 
       2*gamma0*ndua2*H1[ijk] + 4*Gam012*invg02*H2[ijk] + 
       4*Gam112*invg12*H2[ijk] + 4*Gam212*invg22*H2[ijk] + 
       4*Gam312*invg23*H2[ijk] + 2*gamma0*ndua1*H2[ijk] + 
       4*Gam012*invg03*H3[ijk] + 4*Gam112*invg13*H3[ijk] + 
       4*Gam212*invg23*H3[ijk] + 4*Gam312*invg33*H3[ijk] - 
       2*gamma0*g12[ijk]*(nvec0*trGam0 + nvec1*trGam1 + nvec2*trGam2 + 
          nvec3*trGam3 + nvec0*H0[ijk] + nvec1*H1[ijk] + nvec2*H2[ijk] + 
          nvec3*H3[ijk]) + 4*invg00*invh00*Phi001[ijk]*Phi002[ijk] + 
       4*invg01*invh00*Phi002[ijk]*Phi011[ijk] + 
       4*invg01*invh00*Phi001[ijk]*Phi012[ijk] + 
       4*invg02*invh00*Phi002[ijk]*Phi012[ijk] + 
       4*invg11*invh00*Phi011[ijk]*Phi012[ijk] + 
       4*invg12*invh00*Power(Phi012[ijk],2) + 
       4*invg03*invh00*Phi002[ijk]*Phi013[ijk] + 
       4*invg13*invh00*Phi012[ijk]*Phi013[ijk] + 
       4*invg02*invh00*Phi001[ijk]*Phi022[ijk] + 
       4*invg12*invh00*Phi011[ijk]*Phi022[ijk] + 
       4*invg22*invh00*Phi012[ijk]*Phi022[ijk] + 
       4*invg23*invh00*Phi013[ijk]*Phi022[ijk] + 
       4*invg03*invh00*Phi001[ijk]*Phi023[ijk] + 
       4*invg13*invh00*Phi011[ijk]*Phi023[ijk] + 
       4*invg23*invh00*Phi012[ijk]*Phi023[ijk] + 
       4*invg33*invh00*Phi013[ijk]*Phi023[ijk] + 
       4*invg00*invh01*Phi002[ijk]*Phi101[ijk] + 
       4*invg01*invh01*Phi012[ijk]*Phi101[ijk] + 
       4*invg02*invh01*Phi022[ijk]*Phi101[ijk] + 
       4*invg03*invh01*Phi023[ijk]*Phi101[ijk] + 
       4*invg00*invh01*Phi001[ijk]*Phi102[ijk] + 
       4*invg01*invh01*Phi011[ijk]*Phi102[ijk] + 
       4*invg02*invh01*Phi012[ijk]*Phi102[ijk] + 
       4*invg03*invh01*Phi013[ijk]*Phi102[ijk] + 
       4*invg00*invh11*Phi101[ijk]*Phi102[ijk] + 
       4*invg01*invh01*Phi002[ijk]*Phi111[ijk] + 
       4*invg11*invh01*Phi012[ijk]*Phi111[ijk] + 
       4*invg12*invh01*Phi022[ijk]*Phi111[ijk] + 
       4*invg13*invh01*Phi023[ijk]*Phi111[ijk] + 
       4*invg01*invh11*Phi102[ijk]*Phi111[ijk] + 
       4*invg01*invh01*Phi001[ijk]*Phi112[ijk] + 
       4*invg02*invh01*Phi002[ijk]*Phi112[ijk] + 
       4*invg11*invh01*Phi011[ijk]*Phi112[ijk] + 
       8*invg12*invh01*Phi012[ijk]*Phi112[ijk] + 
       4*invg13*invh01*Phi013[ijk]*Phi112[ijk] + 
       4*invg22*invh01*Phi022[ijk]*Phi112[ijk] + 
       4*invg23*invh01*Phi023[ijk]*Phi112[ijk] + 
       4*invg01*invh11*Phi101[ijk]*Phi112[ijk] + 
       4*invg02*invh11*Phi102[ijk]*Phi112[ijk] + 
       4*invg11*invh11*Phi111[ijk]*Phi112[ijk] + 
       4*invg12*invh11*Power(Phi112[ijk],2) + 
       4*invg03*invh01*Phi002[ijk]*Phi113[ijk] + 
       4*invg13*invh01*Phi012[ijk]*Phi113[ijk] + 
       4*invg23*invh01*Phi022[ijk]*Phi113[ijk] + 
       4*invg33*invh01*Phi023[ijk]*Phi113[ijk] + 
       4*invg03*invh11*Phi102[ijk]*Phi113[ijk] + 
       4*invg13*invh11*Phi112[ijk]*Phi113[ijk] + 
       4*invg02*invh01*Phi001[ijk]*Phi122[ijk] + 
       4*invg12*invh01*Phi011[ijk]*Phi122[ijk] + 
       4*invg22*invh01*Phi012[ijk]*Phi122[ijk] + 
       4*invg23*invh01*Phi013[ijk]*Phi122[ijk] + 
       4*invg02*invh11*Phi101[ijk]*Phi122[ijk] + 
       4*invg12*invh11*Phi111[ijk]*Phi122[ijk] + 
       4*invg22*invh11*Phi112[ijk]*Phi122[ijk] + 
       4*invg23*invh11*Phi113[ijk]*Phi122[ijk] + 
       4*invg03*invh01*Phi001[ijk]*Phi123[ijk] + 
       4*invg13*invh01*Phi011[ijk]*Phi123[ijk] + 
       4*invg23*invh01*Phi012[ijk]*Phi123[ijk] + 
       4*invg33*invh01*Phi013[ijk]*Phi123[ijk] + 
       4*invg03*invh11*Phi101[ijk]*Phi123[ijk] + 
       4*invg13*invh11*Phi111[ijk]*Phi123[ijk] + 
       4*invg23*invh11*Phi112[ijk]*Phi123[ijk] + 
       4*invg33*invh11*Phi113[ijk]*Phi123[ijk] + 
       4*invg00*invh02*Phi002[ijk]*Phi201[ijk] + 
       4*invg01*invh02*Phi012[ijk]*Phi201[ijk] + 
       4*invg02*invh02*Phi022[ijk]*Phi201[ijk] + 
       4*invg03*invh02*Phi023[ijk]*Phi201[ijk] + 
       4*invg00*invh12*Phi102[ijk]*Phi201[ijk] + 
       4*invg01*invh12*Phi112[ijk]*Phi201[ijk] + 
       4*invg02*invh12*Phi122[ijk]*Phi201[ijk] + 
       4*invg03*invh12*Phi123[ijk]*Phi201[ijk] + 
       4*invg00*invh02*Phi001[ijk]*Phi202[ijk] + 
       4*invg01*invh02*Phi011[ijk]*Phi202[ijk] + 
       4*invg02*invh02*Phi012[ijk]*Phi202[ijk] + 
       4*invg03*invh02*Phi013[ijk]*Phi202[ijk] + 
       4*invg00*invh12*Phi101[ijk]*Phi202[ijk] + 
       4*invg01*invh12*Phi111[ijk]*Phi202[ijk] + 
       4*invg02*invh12*Phi112[ijk]*Phi202[ijk] + 
       4*invg03*invh12*Phi113[ijk]*Phi202[ijk] + 
       4*invg00*invh22*Phi201[ijk]*Phi202[ijk] + 
       4*invg01*invh02*Phi002[ijk]*Phi211[ijk] + 
       4*invg11*invh02*Phi012[ijk]*Phi211[ijk] + 
       4*invg12*invh02*Phi022[ijk]*Phi211[ijk] + 
       4*invg13*invh02*Phi023[ijk]*Phi211[ijk] + 
       4*invg01*invh12*Phi102[ijk]*Phi211[ijk] + 
       4*invg11*invh12*Phi112[ijk]*Phi211[ijk] + 
       4*invg12*invh12*Phi122[ijk]*Phi211[ijk] + 
       4*invg13*invh12*Phi123[ijk]*Phi211[ijk] + 
       4*invg01*invh22*Phi202[ijk]*Phi211[ijk] + 
       4*invg01*invh02*Phi001[ijk]*Phi212[ijk] + 
       4*invg02*invh02*Phi002[ijk]*Phi212[ijk] + 
       4*invg11*invh02*Phi011[ijk]*Phi212[ijk] + 
       8*invg12*invh02*Phi012[ijk]*Phi212[ijk] + 
       4*invg13*invh02*Phi013[ijk]*Phi212[ijk] + 
       4*invg22*invh02*Phi022[ijk]*Phi212[ijk] + 
       4*invg23*invh02*Phi023[ijk]*Phi212[ijk] + 
       4*invg01*invh12*Phi101[ijk]*Phi212[ijk] + 
       4*invg02*invh12*Phi102[ijk]*Phi212[ijk] + 
       4*invg11*invh12*Phi111[ijk]*Phi212[ijk] + 
       8*invg12*invh12*Phi112[ijk]*Phi212[ijk] + 
       4*invg13*invh12*Phi113[ijk]*Phi212[ijk] + 
       4*invg22*invh12*Phi122[ijk]*Phi212[ijk] + 
       4*invg23*invh12*Phi123[ijk]*Phi212[ijk] + 
       4*invg01*invh22*Phi201[ijk]*Phi212[ijk] + 
       4*invg02*invh22*Phi202[ijk]*Phi212[ijk] + 
       4*invg11*invh22*Phi211[ijk]*Phi212[ijk] + 
       4*invg12*invh22*Power(Phi212[ijk],2) + 
       4*invg03*invh02*Phi002[ijk]*Phi213[ijk] + 
       4*invg13*invh02*Phi012[ijk]*Phi213[ijk] + 
       4*invg23*invh02*Phi022[ijk]*Phi213[ijk] + 
       4*invg33*invh02*Phi023[ijk]*Phi213[ijk] + 
       4*invg03*invh12*Phi102[ijk]*Phi213[ijk] + 
       4*invg13*invh12*Phi112[ijk]*Phi213[ijk] + 
       4*invg23*invh12*Phi122[ijk]*Phi213[ijk] + 
       4*invg33*invh12*Phi123[ijk]*Phi213[ijk] + 
       4*invg03*invh22*Phi202[ijk]*Phi213[ijk] + 
       4*invg13*invh22*Phi212[ijk]*Phi213[ijk] + 
       4*invg02*invh02*Phi001[ijk]*Phi222[ijk] + 
       4*invg12*invh02*Phi011[ijk]*Phi222[ijk] + 
       4*invg22*invh02*Phi012[ijk]*Phi222[ijk] + 
       4*invg23*invh02*Phi013[ijk]*Phi222[ijk] + 
       4*invg02*invh12*Phi101[ijk]*Phi222[ijk] + 
       4*invg12*invh12*Phi111[ijk]*Phi222[ijk] + 
       4*invg22*invh12*Phi112[ijk]*Phi222[ijk] + 
       4*invg23*invh12*Phi113[ijk]*Phi222[ijk] + 
       4*invg02*invh22*Phi201[ijk]*Phi222[ijk] + 
       4*invg12*invh22*Phi211[ijk]*Phi222[ijk] + 
       4*invg22*invh22*Phi212[ijk]*Phi222[ijk] + 
       4*invg23*invh22*Phi213[ijk]*Phi222[ijk] + 
       4*invg03*invh02*Phi001[ijk]*Phi223[ijk] + 
       4*invg13*invh02*Phi011[ijk]*Phi223[ijk] + 
       4*invg23*invh02*Phi012[ijk]*Phi223[ijk] + 
       4*invg33*invh02*Phi013[ijk]*Phi223[ijk] + 
       4*invg03*invh12*Phi101[ijk]*Phi223[ijk] + 
       4*invg13*invh12*Phi111[ijk]*Phi223[ijk] + 
       4*invg23*invh12*Phi112[ijk]*Phi223[ijk] + 
       4*invg33*invh12*Phi113[ijk]*Phi223[ijk] + 
       4*invg03*invh22*Phi201[ijk]*Phi223[ijk] + 
       4*invg13*invh22*Phi211[ijk]*Phi223[ijk] + 
       4*invg23*invh22*Phi212[ijk]*Phi223[ijk] + 
       4*invg33*invh22*Phi213[ijk]*Phi223[ijk] + 
       4*invg00*invh03*Phi002[ijk]*Phi301[ijk] + 
       4*invg01*invh03*Phi012[ijk]*Phi301[ijk] + 
       4*invg02*invh03*Phi022[ijk]*Phi301[ijk] + 
       4*invg03*invh03*Phi023[ijk]*Phi301[ijk] + 
       4*invg00*invh13*Phi102[ijk]*Phi301[ijk] + 
       4*invg01*invh13*Phi112[ijk]*Phi301[ijk] + 
       4*invg02*invh13*Phi122[ijk]*Phi301[ijk] + 
       4*invg03*invh13*Phi123[ijk]*Phi301[ijk] + 
       4*invg00*invh23*Phi202[ijk]*Phi301[ijk] + 
       4*invg01*invh23*Phi212[ijk]*Phi301[ijk] + 
       4*invg02*invh23*Phi222[ijk]*Phi301[ijk] + 
       4*invg03*invh23*Phi223[ijk]*Phi301[ijk] + 
       4*invg00*invh03*Phi001[ijk]*Phi302[ijk] + 
       4*invg01*invh03*Phi011[ijk]*Phi302[ijk] + 
       4*invg02*invh03*Phi012[ijk]*Phi302[ijk] + 
       4*invg03*invh03*Phi013[ijk]*Phi302[ijk] + 
       4*invg00*invh13*Phi101[ijk]*Phi302[ijk] + 
       4*invg01*invh13*Phi111[ijk]*Phi302[ijk] + 
       4*invg02*invh13*Phi112[ijk]*Phi302[ijk] + 
       4*invg03*invh13*Phi113[ijk]*Phi302[ijk] + 
       4*invg00*invh23*Phi201[ijk]*Phi302[ijk] + 
       4*invg01*invh23*Phi211[ijk]*Phi302[ijk] + 
       4*invg02*invh23*Phi212[ijk]*Phi302[ijk] + 
       4*invg03*invh23*Phi213[ijk]*Phi302[ijk] + 
       4*invg00*invh33*Phi301[ijk]*Phi302[ijk] + 
       4*invg01*invh03*Phi002[ijk]*Phi311[ijk] + 
       4*invg11*invh03*Phi012[ijk]*Phi311[ijk] + 
       4*invg12*invh03*Phi022[ijk]*Phi311[ijk] + 
       4*invg13*invh03*Phi023[ijk]*Phi311[ijk] + 
       4*invg01*invh13*Phi102[ijk]*Phi311[ijk] + 
       4*invg11*invh13*Phi112[ijk]*Phi311[ijk] + 
       4*invg12*invh13*Phi122[ijk]*Phi311[ijk] + 
       4*invg13*invh13*Phi123[ijk]*Phi311[ijk] + 
       4*invg01*invh23*Phi202[ijk]*Phi311[ijk] + 
       4*invg11*invh23*Phi212[ijk]*Phi311[ijk] + 
       4*invg12*invh23*Phi222[ijk]*Phi311[ijk] + 
       4*invg13*invh23*Phi223[ijk]*Phi311[ijk] + 
       4*invg01*invh33*Phi302[ijk]*Phi311[ijk] + 
       4*invg01*invh03*Phi001[ijk]*Phi312[ijk] + 
       4*invg02*invh03*Phi002[ijk]*Phi312[ijk] + 
       4*invg11*invh03*Phi011[ijk]*Phi312[ijk] + 
       8*invg12*invh03*Phi012[ijk]*Phi312[ijk] + 
       4*invg13*invh03*Phi013[ijk]*Phi312[ijk] + 
       4*invg22*invh03*Phi022[ijk]*Phi312[ijk] + 
       4*invg23*invh03*Phi023[ijk]*Phi312[ijk] + 
       4*invg01*invh13*Phi101[ijk]*Phi312[ijk] + 
       4*invg02*invh13*Phi102[ijk]*Phi312[ijk] + 
       4*invg11*invh13*Phi111[ijk]*Phi312[ijk] + 
       8*invg12*invh13*Phi112[ijk]*Phi312[ijk] + 
       4*invg13*invh13*Phi113[ijk]*Phi312[ijk] + 
       4*invg22*invh13*Phi122[ijk]*Phi312[ijk] + 
       4*invg23*invh13*Phi123[ijk]*Phi312[ijk] + 
       4*invg01*invh23*Phi201[ijk]*Phi312[ijk] + 
       4*invg02*invh23*Phi202[ijk]*Phi312[ijk] + 
       4*invg11*invh23*Phi211[ijk]*Phi312[ijk] + 
       8*invg12*invh23*Phi212[ijk]*Phi312[ijk] + 
       4*invg13*invh23*Phi213[ijk]*Phi312[ijk] + 
       4*invg22*invh23*Phi222[ijk]*Phi312[ijk] + 
       4*invg23*invh23*Phi223[ijk]*Phi312[ijk] + 
       4*invg01*invh33*Phi301[ijk]*Phi312[ijk] + 
       4*invg02*invh33*Phi302[ijk]*Phi312[ijk] + 
       4*invg11*invh33*Phi311[ijk]*Phi312[ijk] + 
       4*invg12*invh33*Power(Phi312[ijk],2) + 
       4*invg03*invh03*Phi002[ijk]*Phi313[ijk] + 
       4*invg13*invh03*Phi012[ijk]*Phi313[ijk] + 
       4*invg23*invh03*Phi022[ijk]*Phi313[ijk] + 
       4*invg33*invh03*Phi023[ijk]*Phi313[ijk] + 
       4*invg03*invh13*Phi102[ijk]*Phi313[ijk] + 
       4*invg13*invh13*Phi112[ijk]*Phi313[ijk] + 
       4*invg23*invh13*Phi122[ijk]*Phi313[ijk] + 
       4*invg33*invh13*Phi123[ijk]*Phi313[ijk] + 
       4*invg03*invh23*Phi202[ijk]*Phi313[ijk] + 
       4*invg13*invh23*Phi212[ijk]*Phi313[ijk] + 
       4*invg23*invh23*Phi222[ijk]*Phi313[ijk] + 
       4*invg33*invh23*Phi223[ijk]*Phi313[ijk] + 
       4*invg03*invh33*Phi302[ijk]*Phi313[ijk] + 
       4*invg13*invh33*Phi312[ijk]*Phi313[ijk] + 
       4*invg02*invh03*Phi001[ijk]*Phi322[ijk] + 
       4*invg12*invh03*Phi011[ijk]*Phi322[ijk] + 
       4*invg22*invh03*Phi012[ijk]*Phi322[ijk] + 
       4*invg23*invh03*Phi013[ijk]*Phi322[ijk] + 
       4*invg02*invh13*Phi101[ijk]*Phi322[ijk] + 
       4*invg12*invh13*Phi111[ijk]*Phi322[ijk] + 
       4*invg22*invh13*Phi112[ijk]*Phi322[ijk] + 
       4*invg23*invh13*Phi113[ijk]*Phi322[ijk] + 
       4*invg02*invh23*Phi201[ijk]*Phi322[ijk] + 
       4*invg12*invh23*Phi211[ijk]*Phi322[ijk] + 
       4*invg22*invh23*Phi212[ijk]*Phi322[ijk] + 
       4*invg23*invh23*Phi213[ijk]*Phi322[ijk] + 
       4*invg02*invh33*Phi301[ijk]*Phi322[ijk] + 
       4*invg12*invh33*Phi311[ijk]*Phi322[ijk] + 
       4*invg22*invh33*Phi312[ijk]*Phi322[ijk] + 
       4*invg23*invh33*Phi313[ijk]*Phi322[ijk] + 
       4*invg03*invh03*Phi001[ijk]*Phi323[ijk] + 
       4*invg13*invh03*Phi011[ijk]*Phi323[ijk] + 
       4*invg23*invh03*Phi012[ijk]*Phi323[ijk] + 
       4*invg33*invh03*Phi013[ijk]*Phi323[ijk] + 
       4*invg03*invh13*Phi101[ijk]*Phi323[ijk] + 
       4*invg13*invh13*Phi111[ijk]*Phi323[ijk] + 
       4*invg23*invh13*Phi112[ijk]*Phi323[ijk] + 
       4*invg33*invh13*Phi113[ijk]*Phi323[ijk] + 
       4*invg03*invh23*Phi201[ijk]*Phi323[ijk] + 
       4*invg13*invh23*Phi211[ijk]*Phi323[ijk] + 
       4*invg23*invh23*Phi212[ijk]*Phi323[ijk] + 
       4*invg33*invh23*Phi213[ijk]*Phi323[ijk] + 
       4*invg03*invh33*Phi301[ijk]*Phi323[ijk] + 
       4*invg13*invh33*Phi311[ijk]*Phi323[ijk] + 
       4*invg23*invh33*Phi312[ijk]*Phi323[ijk] + 
       4*invg33*invh33*Phi313[ijk]*Phi323[ijk] - 
       2*invh00*nvec0*Phi012[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi112[ijk]*Pi00[ijk] - 
       2*invh02*nvec0*Phi212[ijk]*Pi00[ijk] - 
       2*invh03*nvec0*Phi312[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi012[ijk]*Pi01[ijk] - 
       2*invh00*nvec1*Phi012[ijk]*Pi01[ijk] - 
       2*invh11*nvec0*Phi112[ijk]*Pi01[ijk] - 
       2*invh01*nvec1*Phi112[ijk]*Pi01[ijk] - 
       2*invh12*nvec0*Phi212[ijk]*Pi01[ijk] - 
       2*invh02*nvec1*Phi212[ijk]*Pi01[ijk] - 
       2*invh13*nvec0*Phi312[ijk]*Pi01[ijk] - 
       2*invh03*nvec1*Phi312[ijk]*Pi01[ijk] - 
       2*invh02*nvec0*Phi012[ijk]*Pi02[ijk] - 
       2*invh00*nvec2*Phi012[ijk]*Pi02[ijk] - 
       2*invh12*nvec0*Phi112[ijk]*Pi02[ijk] - 
       2*invh01*nvec2*Phi112[ijk]*Pi02[ijk] - 
       2*invh22*nvec0*Phi212[ijk]*Pi02[ijk] - 
       2*invh02*nvec2*Phi212[ijk]*Pi02[ijk] - 
       2*invh23*nvec0*Phi312[ijk]*Pi02[ijk] - 
       2*invh03*nvec2*Phi312[ijk]*Pi02[ijk] - 
       4*invg00*Pi01[ijk]*Pi02[ijk] - 
       2*invh03*nvec0*Phi012[ijk]*Pi03[ijk] - 
       2*invh00*nvec3*Phi012[ijk]*Pi03[ijk] - 
       2*invh13*nvec0*Phi112[ijk]*Pi03[ijk] - 
       2*invh01*nvec3*Phi112[ijk]*Pi03[ijk] - 
       2*invh23*nvec0*Phi212[ijk]*Pi03[ijk] - 
       2*invh02*nvec3*Phi212[ijk]*Pi03[ijk] - 
       2*invh33*nvec0*Phi312[ijk]*Pi03[ijk] - 
       2*invh03*nvec3*Phi312[ijk]*Pi03[ijk] - 
       2*invh01*nvec1*Phi012[ijk]*Pi11[ijk] - 
       2*invh11*nvec1*Phi112[ijk]*Pi11[ijk] - 
       2*invh12*nvec1*Phi212[ijk]*Pi11[ijk] - 
       2*invh13*nvec1*Phi312[ijk]*Pi11[ijk] - 
       4*invg01*Pi02[ijk]*Pi11[ijk] - 
       2*invh02*nvec1*Phi012[ijk]*Pi12[ijk] - 
       2*invh01*nvec2*Phi012[ijk]*Pi12[ijk] - 
       2*invh12*nvec1*Phi112[ijk]*Pi12[ijk] - 
       2*invh11*nvec2*Phi112[ijk]*Pi12[ijk] - 
       2*invh22*nvec1*Phi212[ijk]*Pi12[ijk] - 
       2*invh12*nvec2*Phi212[ijk]*Pi12[ijk] - 
       2*invh23*nvec1*Phi312[ijk]*Pi12[ijk] - 
       2*invh13*nvec2*Phi312[ijk]*Pi12[ijk] - 
       Power(nvec0,2)*Pi00[ijk]*Pi12[ijk] - 4*invg01*Pi01[ijk]*Pi12[ijk] - 
       2*nvec0*nvec1*Pi01[ijk]*Pi12[ijk] - 4*invg02*Pi02[ijk]*Pi12[ijk] - 
       2*nvec0*nvec2*Pi02[ijk]*Pi12[ijk] - 
       2*nvec0*nvec3*Pi03[ijk]*Pi12[ijk] - 4*invg11*Pi11[ijk]*Pi12[ijk] - 
       Power(nvec1,2)*Pi11[ijk]*Pi12[ijk] - 4*invg12*Power(Pi12[ijk],2) - 
       2*nvec1*nvec2*Power(Pi12[ijk],2) - 
       2*invh03*nvec1*Phi012[ijk]*Pi13[ijk] - 
       2*invh01*nvec3*Phi012[ijk]*Pi13[ijk] - 
       2*invh13*nvec1*Phi112[ijk]*Pi13[ijk] - 
       2*invh11*nvec3*Phi112[ijk]*Pi13[ijk] - 
       2*invh23*nvec1*Phi212[ijk]*Pi13[ijk] - 
       2*invh12*nvec3*Phi212[ijk]*Pi13[ijk] - 
       2*invh33*nvec1*Phi312[ijk]*Pi13[ijk] - 
       2*invh13*nvec3*Phi312[ijk]*Pi13[ijk] - 
       4*invg03*Pi02[ijk]*Pi13[ijk] - 4*invg13*Pi12[ijk]*Pi13[ijk] - 
       2*nvec1*nvec3*Pi12[ijk]*Pi13[ijk] - 
       2*invh02*nvec2*Phi012[ijk]*Pi22[ijk] - 
       2*invh12*nvec2*Phi112[ijk]*Pi22[ijk] - 
       2*invh22*nvec2*Phi212[ijk]*Pi22[ijk] - 
       2*invh23*nvec2*Phi312[ijk]*Pi22[ijk] - 
       4*invg02*Pi01[ijk]*Pi22[ijk] - 4*invg12*Pi11[ijk]*Pi22[ijk] - 
       4*invg22*Pi12[ijk]*Pi22[ijk] - Power(nvec2,2)*Pi12[ijk]*Pi22[ijk] - 
       4*invg23*Pi13[ijk]*Pi22[ijk] - 
       2*invh03*nvec2*Phi012[ijk]*Pi23[ijk] - 
       2*invh02*nvec3*Phi012[ijk]*Pi23[ijk] - 
       2*invh13*nvec2*Phi112[ijk]*Pi23[ijk] - 
       2*invh12*nvec3*Phi112[ijk]*Pi23[ijk] - 
       2*invh23*nvec2*Phi212[ijk]*Pi23[ijk] - 
       2*invh22*nvec3*Phi212[ijk]*Pi23[ijk] - 
       2*invh33*nvec2*Phi312[ijk]*Pi23[ijk] - 
       2*invh23*nvec3*Phi312[ijk]*Pi23[ijk] - 
       4*invg03*Pi01[ijk]*Pi23[ijk] - 4*invg13*Pi11[ijk]*Pi23[ijk] - 
       4*invg23*Pi12[ijk]*Pi23[ijk] - 2*nvec2*nvec3*Pi12[ijk]*Pi23[ijk] - 
       4*invg33*Pi13[ijk]*Pi23[ijk] - 
       2*invh03*nvec3*Phi012[ijk]*Pi33[ijk] - 
       2*invh13*nvec3*Phi112[ijk]*Pi33[ijk] - 
       2*invh23*nvec3*Phi212[ijk]*Pi33[ijk] - 
       2*invh33*nvec3*Phi312[ijk]*Pi33[ijk] - 
       Power(nvec3,2)*Pi12[ijk]*Pi33[ijk]) - 
    2*(gamma1*gamma2*beta0[ijk]*Phi012[ijk] + 
       gamma1*gamma2*beta1[ijk]*Phi112[ijk] + 
       gamma1*gamma2*beta2[ijk]*Phi212[ijk] + 
       gamma1*gamma2*beta3[ijk]*Phi312[ijk] + srcSdH12[ijk]))/2.
;

dtPi13[ijk]
=
(-2*interior*AdPi13[ijk] + alpha[ijk]*
     (-4*Gam100*Gam300*Power(invg00,2) - 8*Gam101*Gam300*invg00*invg01 - 
       8*Gam100*Gam301*invg00*invg01 - 4*Gam111*Gam300*Power(invg01,2) - 
       8*Gam101*Gam301*Power(invg01,2) - 4*Gam100*Gam311*Power(invg01,2) - 
       8*Gam102*Gam300*invg00*invg02 - 8*Gam100*Gam302*invg00*invg02 - 
       8*Gam112*Gam300*invg01*invg02 - 8*Gam102*Gam301*invg01*invg02 - 
       8*Gam101*Gam302*invg01*invg02 - 8*Gam100*Gam312*invg01*invg02 - 
       4*Gam122*Gam300*Power(invg02,2) - 8*Gam102*Gam302*Power(invg02,2) - 
       4*Gam100*Gam322*Power(invg02,2) - 8*Gam103*Gam300*invg00*invg03 - 
       8*Gam100*Gam303*invg00*invg03 - 8*Gam113*Gam300*invg01*invg03 - 
       8*Gam103*Gam301*invg01*invg03 - 8*Gam101*Gam303*invg01*invg03 - 
       8*Gam100*Gam313*invg01*invg03 - 8*Gam123*Gam300*invg02*invg03 - 
       8*Gam103*Gam302*invg02*invg03 - 8*Gam102*Gam303*invg02*invg03 - 
       8*Gam100*Gam323*invg02*invg03 - 4*Gam133*Gam300*Power(invg03,2) - 
       8*Gam103*Gam303*Power(invg03,2) - 4*Gam100*Gam333*Power(invg03,2) - 
       8*Gam101*Gam301*invg00*invg11 - 8*Gam111*Gam301*invg01*invg11 - 
       8*Gam101*Gam311*invg01*invg11 - 8*Gam112*Gam301*invg02*invg11 - 
       8*Gam101*Gam312*invg02*invg11 - 8*Gam113*Gam301*invg03*invg11 - 
       8*Gam101*Gam313*invg03*invg11 - 4*Gam111*Gam311*Power(invg11,2) - 
       8*Gam102*Gam301*invg00*invg12 - 8*Gam101*Gam302*invg00*invg12 - 
       8*Gam112*Gam301*invg01*invg12 - 8*Gam111*Gam302*invg01*invg12 - 
       8*Gam102*Gam311*invg01*invg12 - 8*Gam101*Gam312*invg01*invg12 - 
       8*Gam122*Gam301*invg02*invg12 - 8*Gam112*Gam302*invg02*invg12 - 
       8*Gam102*Gam312*invg02*invg12 - 8*Gam101*Gam322*invg02*invg12 - 
       8*Gam123*Gam301*invg03*invg12 - 8*Gam113*Gam302*invg03*invg12 - 
       8*Gam102*Gam313*invg03*invg12 - 8*Gam101*Gam323*invg03*invg12 - 
       8*Gam112*Gam311*invg11*invg12 - 8*Gam111*Gam312*invg11*invg12 - 
       4*Gam122*Gam311*Power(invg12,2) - 8*Gam112*Gam312*Power(invg12,2) - 
       4*Gam111*Gam322*Power(invg12,2) - 8*Gam103*Gam301*invg00*invg13 - 
       8*Gam101*Gam303*invg00*invg13 - 8*Gam113*Gam301*invg01*invg13 - 
       8*Gam111*Gam303*invg01*invg13 - 8*Gam103*Gam311*invg01*invg13 - 
       8*Gam101*Gam313*invg01*invg13 - 8*Gam123*Gam301*invg02*invg13 - 
       8*Gam112*Gam303*invg02*invg13 - 8*Gam103*Gam312*invg02*invg13 - 
       8*Gam101*Gam323*invg02*invg13 - 8*Gam133*Gam301*invg03*invg13 - 
       8*Gam113*Gam303*invg03*invg13 - 8*Gam103*Gam313*invg03*invg13 - 
       8*Gam101*Gam333*invg03*invg13 - 8*Gam113*Gam311*invg11*invg13 - 
       8*Gam111*Gam313*invg11*invg13 - 8*Gam123*Gam311*invg12*invg13 - 
       8*Gam113*Gam312*invg12*invg13 - 8*Gam112*Gam313*invg12*invg13 - 
       8*Gam111*Gam323*invg12*invg13 - 4*Gam133*Gam311*Power(invg13,2) - 
       8*Gam113*Gam313*Power(invg13,2) - 4*Gam111*Gam333*Power(invg13,2) - 
       8*Gam102*Gam302*invg00*invg22 - 8*Gam112*Gam302*invg01*invg22 - 
       8*Gam102*Gam312*invg01*invg22 - 8*Gam122*Gam302*invg02*invg22 - 
       8*Gam102*Gam322*invg02*invg22 - 8*Gam123*Gam302*invg03*invg22 - 
       8*Gam102*Gam323*invg03*invg22 - 8*Gam112*Gam312*invg11*invg22 - 
       8*Gam122*Gam312*invg12*invg22 - 8*Gam112*Gam322*invg12*invg22 - 
       8*Gam123*Gam312*invg13*invg22 - 8*Gam112*Gam323*invg13*invg22 - 
       4*Gam122*Gam322*Power(invg22,2) - 8*Gam103*Gam302*invg00*invg23 - 
       8*Gam102*Gam303*invg00*invg23 - 8*Gam113*Gam302*invg01*invg23 - 
       8*Gam112*Gam303*invg01*invg23 - 8*Gam103*Gam312*invg01*invg23 - 
       8*Gam102*Gam313*invg01*invg23 - 8*Gam123*Gam302*invg02*invg23 - 
       8*Gam122*Gam303*invg02*invg23 - 8*Gam103*Gam322*invg02*invg23 - 
       8*Gam102*Gam323*invg02*invg23 - 8*Gam133*Gam302*invg03*invg23 - 
       8*Gam123*Gam303*invg03*invg23 - 8*Gam103*Gam323*invg03*invg23 - 
       8*Gam102*Gam333*invg03*invg23 - 8*Gam113*Gam312*invg11*invg23 - 
       8*Gam112*Gam313*invg11*invg23 - 8*Gam123*Gam312*invg12*invg23 - 
       8*Gam122*Gam313*invg12*invg23 - 8*Gam113*Gam322*invg12*invg23 - 
       8*Gam112*Gam323*invg12*invg23 - 8*Gam133*Gam312*invg13*invg23 - 
       8*Gam123*Gam313*invg13*invg23 - 8*Gam113*Gam323*invg13*invg23 - 
       8*Gam112*Gam333*invg13*invg23 - 8*Gam123*Gam322*invg22*invg23 - 
       8*Gam122*Gam323*invg22*invg23 - 4*Gam133*Gam322*Power(invg23,2) - 
       8*Gam123*Gam323*Power(invg23,2) - 4*Gam122*Gam333*Power(invg23,2) - 
       8*Gam103*Gam303*invg00*invg33 - 8*Gam113*Gam303*invg01*invg33 - 
       8*Gam103*Gam313*invg01*invg33 - 8*Gam123*Gam303*invg02*invg33 - 
       8*Gam103*Gam323*invg02*invg33 - 8*Gam133*Gam303*invg03*invg33 - 
       8*Gam103*Gam333*invg03*invg33 - 8*Gam113*Gam313*invg11*invg33 - 
       8*Gam123*Gam313*invg12*invg33 - 8*Gam113*Gam323*invg12*invg33 - 
       8*Gam133*Gam313*invg13*invg33 - 8*Gam113*Gam333*invg13*invg33 - 
       8*Gam123*Gam323*invg22*invg33 - 8*Gam133*Gam323*invg23*invg33 - 
       8*Gam123*Gam333*invg23*invg33 - 4*Gam133*Gam333*Power(invg33,2) + 
       2*gamma0*ndua3*trGam1 + 2*gamma0*ndua1*trGam3 + 
       4*(Gam013*invg00 + Gam113*invg01 + Gam213*invg02 + Gam313*invg03)*
        H0[ijk] + 4*Gam013*invg01*H1[ijk] + 4*Gam113*invg11*H1[ijk] + 
       4*Gam213*invg12*H1[ijk] + 4*Gam313*invg13*H1[ijk] + 
       2*gamma0*ndua3*H1[ijk] + 4*Gam013*invg02*H2[ijk] + 
       4*Gam113*invg12*H2[ijk] + 4*Gam213*invg22*H2[ijk] + 
       4*Gam313*invg23*H2[ijk] + 4*Gam013*invg03*H3[ijk] + 
       4*Gam113*invg13*H3[ijk] + 4*Gam213*invg23*H3[ijk] + 
       4*Gam313*invg33*H3[ijk] + 2*gamma0*ndua1*H3[ijk] - 
       2*gamma0*g13[ijk]*(nvec0*trGam0 + nvec1*trGam1 + nvec2*trGam2 + 
          nvec3*trGam3 + nvec0*H0[ijk] + nvec1*H1[ijk] + nvec2*H2[ijk] + 
          nvec3*H3[ijk]) + 4*invg00*invh00*Phi001[ijk]*Phi003[ijk] + 
       4*invg01*invh00*Phi003[ijk]*Phi011[ijk] + 
       4*invg02*invh00*Phi003[ijk]*Phi012[ijk] + 
       4*invg01*invh00*Phi001[ijk]*Phi013[ijk] + 
       4*invg03*invh00*Phi003[ijk]*Phi013[ijk] + 
       4*invg11*invh00*Phi011[ijk]*Phi013[ijk] + 
       4*invg12*invh00*Phi012[ijk]*Phi013[ijk] + 
       4*invg13*invh00*Power(Phi013[ijk],2) + 
       4*invg02*invh00*Phi001[ijk]*Phi023[ijk] + 
       4*invg12*invh00*Phi011[ijk]*Phi023[ijk] + 
       4*invg22*invh00*Phi012[ijk]*Phi023[ijk] + 
       4*invg23*invh00*Phi013[ijk]*Phi023[ijk] + 
       4*invg03*invh00*Phi001[ijk]*Phi033[ijk] + 
       4*invg13*invh00*Phi011[ijk]*Phi033[ijk] + 
       4*invg23*invh00*Phi012[ijk]*Phi033[ijk] + 
       4*invg33*invh00*Phi013[ijk]*Phi033[ijk] + 
       4*invg00*invh01*Phi003[ijk]*Phi101[ijk] + 
       4*invg01*invh01*Phi013[ijk]*Phi101[ijk] + 
       4*invg02*invh01*Phi023[ijk]*Phi101[ijk] + 
       4*invg03*invh01*Phi033[ijk]*Phi101[ijk] + 
       4*invg00*invh01*Phi001[ijk]*Phi103[ijk] + 
       4*invg01*invh01*Phi011[ijk]*Phi103[ijk] + 
       4*invg02*invh01*Phi012[ijk]*Phi103[ijk] + 
       4*invg03*invh01*Phi013[ijk]*Phi103[ijk] + 
       4*invg00*invh11*Phi101[ijk]*Phi103[ijk] + 
       4*invg01*invh01*Phi003[ijk]*Phi111[ijk] + 
       4*invg11*invh01*Phi013[ijk]*Phi111[ijk] + 
       4*invg12*invh01*Phi023[ijk]*Phi111[ijk] + 
       4*invg13*invh01*Phi033[ijk]*Phi111[ijk] + 
       4*invg01*invh11*Phi103[ijk]*Phi111[ijk] + 
       4*invg02*invh01*Phi003[ijk]*Phi112[ijk] + 
       4*invg12*invh01*Phi013[ijk]*Phi112[ijk] + 
       4*invg22*invh01*Phi023[ijk]*Phi112[ijk] + 
       4*invg23*invh01*Phi033[ijk]*Phi112[ijk] + 
       4*invg02*invh11*Phi103[ijk]*Phi112[ijk] + 
       4*invg01*invh01*Phi001[ijk]*Phi113[ijk] + 
       4*invg03*invh01*Phi003[ijk]*Phi113[ijk] + 
       4*invg11*invh01*Phi011[ijk]*Phi113[ijk] + 
       4*invg12*invh01*Phi012[ijk]*Phi113[ijk] + 
       8*invg13*invh01*Phi013[ijk]*Phi113[ijk] + 
       4*invg23*invh01*Phi023[ijk]*Phi113[ijk] + 
       4*invg33*invh01*Phi033[ijk]*Phi113[ijk] + 
       4*invg01*invh11*Phi101[ijk]*Phi113[ijk] + 
       4*invg03*invh11*Phi103[ijk]*Phi113[ijk] + 
       4*invg11*invh11*Phi111[ijk]*Phi113[ijk] + 
       4*invg12*invh11*Phi112[ijk]*Phi113[ijk] + 
       4*invg13*invh11*Power(Phi113[ijk],2) + 
       4*invg02*invh01*Phi001[ijk]*Phi123[ijk] + 
       4*invg12*invh01*Phi011[ijk]*Phi123[ijk] + 
       4*invg22*invh01*Phi012[ijk]*Phi123[ijk] + 
       4*invg23*invh01*Phi013[ijk]*Phi123[ijk] + 
       4*invg02*invh11*Phi101[ijk]*Phi123[ijk] + 
       4*invg12*invh11*Phi111[ijk]*Phi123[ijk] + 
       4*invg22*invh11*Phi112[ijk]*Phi123[ijk] + 
       4*invg23*invh11*Phi113[ijk]*Phi123[ijk] + 
       4*invg03*invh01*Phi001[ijk]*Phi133[ijk] + 
       4*invg13*invh01*Phi011[ijk]*Phi133[ijk] + 
       4*invg23*invh01*Phi012[ijk]*Phi133[ijk] + 
       4*invg33*invh01*Phi013[ijk]*Phi133[ijk] + 
       4*invg03*invh11*Phi101[ijk]*Phi133[ijk] + 
       4*invg13*invh11*Phi111[ijk]*Phi133[ijk] + 
       4*invg23*invh11*Phi112[ijk]*Phi133[ijk] + 
       4*invg33*invh11*Phi113[ijk]*Phi133[ijk] + 
       4*invg00*invh02*Phi003[ijk]*Phi201[ijk] + 
       4*invg01*invh02*Phi013[ijk]*Phi201[ijk] + 
       4*invg02*invh02*Phi023[ijk]*Phi201[ijk] + 
       4*invg03*invh02*Phi033[ijk]*Phi201[ijk] + 
       4*invg00*invh12*Phi103[ijk]*Phi201[ijk] + 
       4*invg01*invh12*Phi113[ijk]*Phi201[ijk] + 
       4*invg02*invh12*Phi123[ijk]*Phi201[ijk] + 
       4*invg03*invh12*Phi133[ijk]*Phi201[ijk] + 
       4*invg00*invh02*Phi001[ijk]*Phi203[ijk] + 
       4*invg01*invh02*Phi011[ijk]*Phi203[ijk] + 
       4*invg02*invh02*Phi012[ijk]*Phi203[ijk] + 
       4*invg03*invh02*Phi013[ijk]*Phi203[ijk] + 
       4*invg00*invh12*Phi101[ijk]*Phi203[ijk] + 
       4*invg01*invh12*Phi111[ijk]*Phi203[ijk] + 
       4*invg02*invh12*Phi112[ijk]*Phi203[ijk] + 
       4*invg03*invh12*Phi113[ijk]*Phi203[ijk] + 
       4*invg00*invh22*Phi201[ijk]*Phi203[ijk] + 
       4*invg01*invh02*Phi003[ijk]*Phi211[ijk] + 
       4*invg11*invh02*Phi013[ijk]*Phi211[ijk] + 
       4*invg12*invh02*Phi023[ijk]*Phi211[ijk] + 
       4*invg13*invh02*Phi033[ijk]*Phi211[ijk] + 
       4*invg01*invh12*Phi103[ijk]*Phi211[ijk] + 
       4*invg11*invh12*Phi113[ijk]*Phi211[ijk] + 
       4*invg12*invh12*Phi123[ijk]*Phi211[ijk] + 
       4*invg13*invh12*Phi133[ijk]*Phi211[ijk] + 
       4*invg01*invh22*Phi203[ijk]*Phi211[ijk] + 
       4*invg02*invh02*Phi003[ijk]*Phi212[ijk] + 
       4*invg12*invh02*Phi013[ijk]*Phi212[ijk] + 
       4*invg22*invh02*Phi023[ijk]*Phi212[ijk] + 
       4*invg23*invh02*Phi033[ijk]*Phi212[ijk] + 
       4*invg02*invh12*Phi103[ijk]*Phi212[ijk] + 
       4*invg12*invh12*Phi113[ijk]*Phi212[ijk] + 
       4*invg22*invh12*Phi123[ijk]*Phi212[ijk] + 
       4*invg23*invh12*Phi133[ijk]*Phi212[ijk] + 
       4*invg02*invh22*Phi203[ijk]*Phi212[ijk] + 
       4*invg01*invh02*Phi001[ijk]*Phi213[ijk] + 
       4*invg03*invh02*Phi003[ijk]*Phi213[ijk] + 
       4*invg11*invh02*Phi011[ijk]*Phi213[ijk] + 
       4*invg12*invh02*Phi012[ijk]*Phi213[ijk] + 
       8*invg13*invh02*Phi013[ijk]*Phi213[ijk] + 
       4*invg23*invh02*Phi023[ijk]*Phi213[ijk] + 
       4*invg33*invh02*Phi033[ijk]*Phi213[ijk] + 
       4*invg01*invh12*Phi101[ijk]*Phi213[ijk] + 
       4*invg03*invh12*Phi103[ijk]*Phi213[ijk] + 
       4*invg11*invh12*Phi111[ijk]*Phi213[ijk] + 
       4*invg12*invh12*Phi112[ijk]*Phi213[ijk] + 
       8*invg13*invh12*Phi113[ijk]*Phi213[ijk] + 
       4*invg23*invh12*Phi123[ijk]*Phi213[ijk] + 
       4*invg33*invh12*Phi133[ijk]*Phi213[ijk] + 
       4*invg01*invh22*Phi201[ijk]*Phi213[ijk] + 
       4*invg03*invh22*Phi203[ijk]*Phi213[ijk] + 
       4*invg11*invh22*Phi211[ijk]*Phi213[ijk] + 
       4*invg12*invh22*Phi212[ijk]*Phi213[ijk] + 
       4*invg13*invh22*Power(Phi213[ijk],2) + 
       4*invg02*invh02*Phi001[ijk]*Phi223[ijk] + 
       4*invg12*invh02*Phi011[ijk]*Phi223[ijk] + 
       4*invg22*invh02*Phi012[ijk]*Phi223[ijk] + 
       4*invg23*invh02*Phi013[ijk]*Phi223[ijk] + 
       4*invg02*invh12*Phi101[ijk]*Phi223[ijk] + 
       4*invg12*invh12*Phi111[ijk]*Phi223[ijk] + 
       4*invg22*invh12*Phi112[ijk]*Phi223[ijk] + 
       4*invg23*invh12*Phi113[ijk]*Phi223[ijk] + 
       4*invg02*invh22*Phi201[ijk]*Phi223[ijk] + 
       4*invg12*invh22*Phi211[ijk]*Phi223[ijk] + 
       4*invg22*invh22*Phi212[ijk]*Phi223[ijk] + 
       4*invg23*invh22*Phi213[ijk]*Phi223[ijk] + 
       4*invg03*invh02*Phi001[ijk]*Phi233[ijk] + 
       4*invg13*invh02*Phi011[ijk]*Phi233[ijk] + 
       4*invg23*invh02*Phi012[ijk]*Phi233[ijk] + 
       4*invg33*invh02*Phi013[ijk]*Phi233[ijk] + 
       4*invg03*invh12*Phi101[ijk]*Phi233[ijk] + 
       4*invg13*invh12*Phi111[ijk]*Phi233[ijk] + 
       4*invg23*invh12*Phi112[ijk]*Phi233[ijk] + 
       4*invg33*invh12*Phi113[ijk]*Phi233[ijk] + 
       4*invg03*invh22*Phi201[ijk]*Phi233[ijk] + 
       4*invg13*invh22*Phi211[ijk]*Phi233[ijk] + 
       4*invg23*invh22*Phi212[ijk]*Phi233[ijk] + 
       4*invg33*invh22*Phi213[ijk]*Phi233[ijk] + 
       4*invg00*invh03*Phi003[ijk]*Phi301[ijk] + 
       4*invg01*invh03*Phi013[ijk]*Phi301[ijk] + 
       4*invg02*invh03*Phi023[ijk]*Phi301[ijk] + 
       4*invg03*invh03*Phi033[ijk]*Phi301[ijk] + 
       4*invg00*invh13*Phi103[ijk]*Phi301[ijk] + 
       4*invg01*invh13*Phi113[ijk]*Phi301[ijk] + 
       4*invg02*invh13*Phi123[ijk]*Phi301[ijk] + 
       4*invg03*invh13*Phi133[ijk]*Phi301[ijk] + 
       4*invg00*invh23*Phi203[ijk]*Phi301[ijk] + 
       4*invg01*invh23*Phi213[ijk]*Phi301[ijk] + 
       4*invg02*invh23*Phi223[ijk]*Phi301[ijk] + 
       4*invg03*invh23*Phi233[ijk]*Phi301[ijk] + 
       4*invg00*invh03*Phi001[ijk]*Phi303[ijk] + 
       4*invg01*invh03*Phi011[ijk]*Phi303[ijk] + 
       4*invg02*invh03*Phi012[ijk]*Phi303[ijk] + 
       4*invg03*invh03*Phi013[ijk]*Phi303[ijk] + 
       4*invg00*invh13*Phi101[ijk]*Phi303[ijk] + 
       4*invg01*invh13*Phi111[ijk]*Phi303[ijk] + 
       4*invg02*invh13*Phi112[ijk]*Phi303[ijk] + 
       4*invg03*invh13*Phi113[ijk]*Phi303[ijk] + 
       4*invg00*invh23*Phi201[ijk]*Phi303[ijk] + 
       4*invg01*invh23*Phi211[ijk]*Phi303[ijk] + 
       4*invg02*invh23*Phi212[ijk]*Phi303[ijk] + 
       4*invg03*invh23*Phi213[ijk]*Phi303[ijk] + 
       4*invg00*invh33*Phi301[ijk]*Phi303[ijk] + 
       4*invg01*invh03*Phi003[ijk]*Phi311[ijk] + 
       4*invg11*invh03*Phi013[ijk]*Phi311[ijk] + 
       4*invg12*invh03*Phi023[ijk]*Phi311[ijk] + 
       4*invg13*invh03*Phi033[ijk]*Phi311[ijk] + 
       4*invg01*invh13*Phi103[ijk]*Phi311[ijk] + 
       4*invg11*invh13*Phi113[ijk]*Phi311[ijk] + 
       4*invg12*invh13*Phi123[ijk]*Phi311[ijk] + 
       4*invg13*invh13*Phi133[ijk]*Phi311[ijk] + 
       4*invg01*invh23*Phi203[ijk]*Phi311[ijk] + 
       4*invg11*invh23*Phi213[ijk]*Phi311[ijk] + 
       4*invg12*invh23*Phi223[ijk]*Phi311[ijk] + 
       4*invg13*invh23*Phi233[ijk]*Phi311[ijk] + 
       4*invg01*invh33*Phi303[ijk]*Phi311[ijk] + 
       4*invg02*invh03*Phi003[ijk]*Phi312[ijk] + 
       4*invg12*invh03*Phi013[ijk]*Phi312[ijk] + 
       4*invg22*invh03*Phi023[ijk]*Phi312[ijk] + 
       4*invg23*invh03*Phi033[ijk]*Phi312[ijk] + 
       4*invg02*invh13*Phi103[ijk]*Phi312[ijk] + 
       4*invg12*invh13*Phi113[ijk]*Phi312[ijk] + 
       4*invg22*invh13*Phi123[ijk]*Phi312[ijk] + 
       4*invg23*invh13*Phi133[ijk]*Phi312[ijk] + 
       4*invg02*invh23*Phi203[ijk]*Phi312[ijk] + 
       4*invg12*invh23*Phi213[ijk]*Phi312[ijk] + 
       4*invg22*invh23*Phi223[ijk]*Phi312[ijk] + 
       4*invg23*invh23*Phi233[ijk]*Phi312[ijk] + 
       4*invg02*invh33*Phi303[ijk]*Phi312[ijk] + 
       4*invg01*invh03*Phi001[ijk]*Phi313[ijk] + 
       4*invg03*invh03*Phi003[ijk]*Phi313[ijk] + 
       4*invg11*invh03*Phi011[ijk]*Phi313[ijk] + 
       4*invg12*invh03*Phi012[ijk]*Phi313[ijk] + 
       8*invg13*invh03*Phi013[ijk]*Phi313[ijk] + 
       4*invg23*invh03*Phi023[ijk]*Phi313[ijk] + 
       4*invg33*invh03*Phi033[ijk]*Phi313[ijk] + 
       4*invg01*invh13*Phi101[ijk]*Phi313[ijk] + 
       4*invg03*invh13*Phi103[ijk]*Phi313[ijk] + 
       4*invg11*invh13*Phi111[ijk]*Phi313[ijk] + 
       4*invg12*invh13*Phi112[ijk]*Phi313[ijk] + 
       8*invg13*invh13*Phi113[ijk]*Phi313[ijk] + 
       4*invg23*invh13*Phi123[ijk]*Phi313[ijk] + 
       4*invg33*invh13*Phi133[ijk]*Phi313[ijk] + 
       4*invg01*invh23*Phi201[ijk]*Phi313[ijk] + 
       4*invg03*invh23*Phi203[ijk]*Phi313[ijk] + 
       4*invg11*invh23*Phi211[ijk]*Phi313[ijk] + 
       4*invg12*invh23*Phi212[ijk]*Phi313[ijk] + 
       8*invg13*invh23*Phi213[ijk]*Phi313[ijk] + 
       4*invg23*invh23*Phi223[ijk]*Phi313[ijk] + 
       4*invg33*invh23*Phi233[ijk]*Phi313[ijk] + 
       4*invg01*invh33*Phi301[ijk]*Phi313[ijk] + 
       4*invg03*invh33*Phi303[ijk]*Phi313[ijk] + 
       4*invg11*invh33*Phi311[ijk]*Phi313[ijk] + 
       4*invg12*invh33*Phi312[ijk]*Phi313[ijk] + 
       4*invg13*invh33*Power(Phi313[ijk],2) + 
       4*invg02*invh03*Phi001[ijk]*Phi323[ijk] + 
       4*invg12*invh03*Phi011[ijk]*Phi323[ijk] + 
       4*invg22*invh03*Phi012[ijk]*Phi323[ijk] + 
       4*invg23*invh03*Phi013[ijk]*Phi323[ijk] + 
       4*invg02*invh13*Phi101[ijk]*Phi323[ijk] + 
       4*invg12*invh13*Phi111[ijk]*Phi323[ijk] + 
       4*invg22*invh13*Phi112[ijk]*Phi323[ijk] + 
       4*invg23*invh13*Phi113[ijk]*Phi323[ijk] + 
       4*invg02*invh23*Phi201[ijk]*Phi323[ijk] + 
       4*invg12*invh23*Phi211[ijk]*Phi323[ijk] + 
       4*invg22*invh23*Phi212[ijk]*Phi323[ijk] + 
       4*invg23*invh23*Phi213[ijk]*Phi323[ijk] + 
       4*invg02*invh33*Phi301[ijk]*Phi323[ijk] + 
       4*invg12*invh33*Phi311[ijk]*Phi323[ijk] + 
       4*invg22*invh33*Phi312[ijk]*Phi323[ijk] + 
       4*invg23*invh33*Phi313[ijk]*Phi323[ijk] + 
       4*invg03*invh03*Phi001[ijk]*Phi333[ijk] + 
       4*invg13*invh03*Phi011[ijk]*Phi333[ijk] + 
       4*invg23*invh03*Phi012[ijk]*Phi333[ijk] + 
       4*invg33*invh03*Phi013[ijk]*Phi333[ijk] + 
       4*invg03*invh13*Phi101[ijk]*Phi333[ijk] + 
       4*invg13*invh13*Phi111[ijk]*Phi333[ijk] + 
       4*invg23*invh13*Phi112[ijk]*Phi333[ijk] + 
       4*invg33*invh13*Phi113[ijk]*Phi333[ijk] + 
       4*invg03*invh23*Phi201[ijk]*Phi333[ijk] + 
       4*invg13*invh23*Phi211[ijk]*Phi333[ijk] + 
       4*invg23*invh23*Phi212[ijk]*Phi333[ijk] + 
       4*invg33*invh23*Phi213[ijk]*Phi333[ijk] + 
       4*invg03*invh33*Phi301[ijk]*Phi333[ijk] + 
       4*invg13*invh33*Phi311[ijk]*Phi333[ijk] + 
       4*invg23*invh33*Phi312[ijk]*Phi333[ijk] + 
       4*invg33*invh33*Phi313[ijk]*Phi333[ijk] - 
       2*invh00*nvec0*Phi013[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi113[ijk]*Pi00[ijk] - 
       2*invh02*nvec0*Phi213[ijk]*Pi00[ijk] - 
       2*invh03*nvec0*Phi313[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi013[ijk]*Pi01[ijk] - 
       2*invh00*nvec1*Phi013[ijk]*Pi01[ijk] - 
       2*invh11*nvec0*Phi113[ijk]*Pi01[ijk] - 
       2*invh01*nvec1*Phi113[ijk]*Pi01[ijk] - 
       2*invh12*nvec0*Phi213[ijk]*Pi01[ijk] - 
       2*invh02*nvec1*Phi213[ijk]*Pi01[ijk] - 
       2*invh13*nvec0*Phi313[ijk]*Pi01[ijk] - 
       2*invh03*nvec1*Phi313[ijk]*Pi01[ijk] - 
       2*invh02*nvec0*Phi013[ijk]*Pi02[ijk] - 
       2*invh00*nvec2*Phi013[ijk]*Pi02[ijk] - 
       2*invh12*nvec0*Phi113[ijk]*Pi02[ijk] - 
       2*invh01*nvec2*Phi113[ijk]*Pi02[ijk] - 
       2*invh22*nvec0*Phi213[ijk]*Pi02[ijk] - 
       2*invh02*nvec2*Phi213[ijk]*Pi02[ijk] - 
       2*invh23*nvec0*Phi313[ijk]*Pi02[ijk] - 
       2*invh03*nvec2*Phi313[ijk]*Pi02[ijk] - 
       2*invh03*nvec0*Phi013[ijk]*Pi03[ijk] - 
       2*invh00*nvec3*Phi013[ijk]*Pi03[ijk] - 
       2*invh13*nvec0*Phi113[ijk]*Pi03[ijk] - 
       2*invh01*nvec3*Phi113[ijk]*Pi03[ijk] - 
       2*invh23*nvec0*Phi213[ijk]*Pi03[ijk] - 
       2*invh02*nvec3*Phi213[ijk]*Pi03[ijk] - 
       2*invh33*nvec0*Phi313[ijk]*Pi03[ijk] - 
       2*invh03*nvec3*Phi313[ijk]*Pi03[ijk] - 
       4*invg00*Pi01[ijk]*Pi03[ijk] - 
       2*invh01*nvec1*Phi013[ijk]*Pi11[ijk] - 
       2*invh11*nvec1*Phi113[ijk]*Pi11[ijk] - 
       2*invh12*nvec1*Phi213[ijk]*Pi11[ijk] - 
       2*invh13*nvec1*Phi313[ijk]*Pi11[ijk] - 
       4*invg01*Pi03[ijk]*Pi11[ijk] - 
       2*invh02*nvec1*Phi013[ijk]*Pi12[ijk] - 
       2*invh01*nvec2*Phi013[ijk]*Pi12[ijk] - 
       2*invh12*nvec1*Phi113[ijk]*Pi12[ijk] - 
       2*invh11*nvec2*Phi113[ijk]*Pi12[ijk] - 
       2*invh22*nvec1*Phi213[ijk]*Pi12[ijk] - 
       2*invh12*nvec2*Phi213[ijk]*Pi12[ijk] - 
       2*invh23*nvec1*Phi313[ijk]*Pi12[ijk] - 
       2*invh13*nvec2*Phi313[ijk]*Pi12[ijk] - 
       4*invg02*Pi03[ijk]*Pi12[ijk] - 
       2*invh03*nvec1*Phi013[ijk]*Pi13[ijk] - 
       2*invh01*nvec3*Phi013[ijk]*Pi13[ijk] - 
       2*invh13*nvec1*Phi113[ijk]*Pi13[ijk] - 
       2*invh11*nvec3*Phi113[ijk]*Pi13[ijk] - 
       2*invh23*nvec1*Phi213[ijk]*Pi13[ijk] - 
       2*invh12*nvec3*Phi213[ijk]*Pi13[ijk] - 
       2*invh33*nvec1*Phi313[ijk]*Pi13[ijk] - 
       2*invh13*nvec3*Phi313[ijk]*Pi13[ijk] - 
       Power(nvec0,2)*Pi00[ijk]*Pi13[ijk] - 4*invg01*Pi01[ijk]*Pi13[ijk] - 
       2*nvec0*nvec1*Pi01[ijk]*Pi13[ijk] - 
       2*nvec0*nvec2*Pi02[ijk]*Pi13[ijk] - 4*invg03*Pi03[ijk]*Pi13[ijk] - 
       2*nvec0*nvec3*Pi03[ijk]*Pi13[ijk] - 4*invg11*Pi11[ijk]*Pi13[ijk] - 
       Power(nvec1,2)*Pi11[ijk]*Pi13[ijk] - 4*invg12*Pi12[ijk]*Pi13[ijk] - 
       2*nvec1*nvec2*Pi12[ijk]*Pi13[ijk] - 4*invg13*Power(Pi13[ijk],2) - 
       2*nvec1*nvec3*Power(Pi13[ijk],2) - 
       2*invh02*nvec2*Phi013[ijk]*Pi22[ijk] - 
       2*invh12*nvec2*Phi113[ijk]*Pi22[ijk] - 
       2*invh22*nvec2*Phi213[ijk]*Pi22[ijk] - 
       2*invh23*nvec2*Phi313[ijk]*Pi22[ijk] - 
       Power(nvec2,2)*Pi13[ijk]*Pi22[ijk] - 
       2*invh03*nvec2*Phi013[ijk]*Pi23[ijk] - 
       2*invh02*nvec3*Phi013[ijk]*Pi23[ijk] - 
       2*invh13*nvec2*Phi113[ijk]*Pi23[ijk] - 
       2*invh12*nvec3*Phi113[ijk]*Pi23[ijk] - 
       2*invh23*nvec2*Phi213[ijk]*Pi23[ijk] - 
       2*invh22*nvec3*Phi213[ijk]*Pi23[ijk] - 
       2*invh33*nvec2*Phi313[ijk]*Pi23[ijk] - 
       2*invh23*nvec3*Phi313[ijk]*Pi23[ijk] - 
       4*invg02*Pi01[ijk]*Pi23[ijk] - 4*invg12*Pi11[ijk]*Pi23[ijk] - 
       4*invg22*Pi12[ijk]*Pi23[ijk] - 4*invg23*Pi13[ijk]*Pi23[ijk] - 
       2*nvec2*nvec3*Pi13[ijk]*Pi23[ijk] - 
       2*invh03*nvec3*Phi013[ijk]*Pi33[ijk] - 
       2*invh13*nvec3*Phi113[ijk]*Pi33[ijk] - 
       2*invh23*nvec3*Phi213[ijk]*Pi33[ijk] - 
       2*invh33*nvec3*Phi313[ijk]*Pi33[ijk] - 
       4*invg03*Pi01[ijk]*Pi33[ijk] - 4*invg13*Pi11[ijk]*Pi33[ijk] - 
       4*invg23*Pi12[ijk]*Pi33[ijk] - 4*invg33*Pi13[ijk]*Pi33[ijk] - 
       Power(nvec3,2)*Pi13[ijk]*Pi33[ijk]) - 
    2*(gamma1*gamma2*beta0[ijk]*Phi013[ijk] + 
       gamma1*gamma2*beta1[ijk]*Phi113[ijk] + 
       gamma1*gamma2*beta2[ijk]*Phi213[ijk] + 
       gamma1*gamma2*beta3[ijk]*Phi313[ijk] + srcSdH13[ijk]))/2.
;

dtPi22[ijk]
=
-(interior*AdPi22[ijk]) - gamma1*gamma2*beta0[ijk]*Phi022[ijk] - 
  gamma1*gamma2*beta1[ijk]*Phi122[ijk] - 
  gamma1*gamma2*beta2[ijk]*Phi222[ijk] - 
  gamma1*gamma2*beta3[ijk]*Phi322[ijk] - 
  (alpha[ijk]*(4*Power(Gam200,2)*Power(invg00,2) + 
       16*Gam200*Gam201*invg00*invg01 + 
       8*Power(Gam201,2)*Power(invg01,2) + 
       8*Gam200*Gam211*Power(invg01,2) + 16*Gam200*Gam202*invg00*invg02 + 
       16*Gam201*Gam202*invg01*invg02 + 16*Gam200*Gam212*invg01*invg02 + 
       8*Power(Gam202,2)*Power(invg02,2) + 
       8*Gam200*Gam222*Power(invg02,2) + 16*Gam200*Gam203*invg00*invg03 + 
       16*Gam201*Gam203*invg01*invg03 + 16*Gam200*Gam213*invg01*invg03 + 
       16*Gam202*Gam203*invg02*invg03 + 16*Gam200*Gam223*invg02*invg03 + 
       8*Power(Gam203,2)*Power(invg03,2) + 
       8*Gam200*Gam233*Power(invg03,2) + 8*Power(Gam201,2)*invg00*invg11 + 
       16*Gam201*Gam211*invg01*invg11 + 16*Gam201*Gam212*invg02*invg11 + 
       16*Gam201*Gam213*invg03*invg11 + 
       4*Power(Gam211,2)*Power(invg11,2) + 
       16*Gam201*Gam202*invg00*invg12 + 16*Gam202*Gam211*invg01*invg12 + 
       16*Gam201*Gam212*invg01*invg12 + 16*Gam202*Gam212*invg02*invg12 + 
       16*Gam201*Gam222*invg02*invg12 + 16*Gam202*Gam213*invg03*invg12 + 
       16*Gam201*Gam223*invg03*invg12 + 16*Gam211*Gam212*invg11*invg12 + 
       8*Power(Gam212,2)*Power(invg12,2) + 
       8*Gam211*Gam222*Power(invg12,2) + 16*Gam201*Gam203*invg00*invg13 + 
       16*Gam203*Gam211*invg01*invg13 + 16*Gam201*Gam213*invg01*invg13 + 
       16*Gam203*Gam212*invg02*invg13 + 16*Gam201*Gam223*invg02*invg13 + 
       16*Gam203*Gam213*invg03*invg13 + 16*Gam201*Gam233*invg03*invg13 + 
       16*Gam211*Gam213*invg11*invg13 + 16*Gam212*Gam213*invg12*invg13 + 
       16*Gam211*Gam223*invg12*invg13 + 
       8*Power(Gam213,2)*Power(invg13,2) + 
       8*Gam211*Gam233*Power(invg13,2) + 8*Power(Gam202,2)*invg00*invg22 + 
       16*Gam202*Gam212*invg01*invg22 + 16*Gam202*Gam222*invg02*invg22 + 
       16*Gam202*Gam223*invg03*invg22 + 8*Power(Gam212,2)*invg11*invg22 + 
       16*Gam212*Gam222*invg12*invg22 + 16*Gam212*Gam223*invg13*invg22 + 
       4*Power(Gam222,2)*Power(invg22,2) + 
       16*Gam202*Gam203*invg00*invg23 + 16*Gam203*Gam212*invg01*invg23 + 
       16*Gam202*Gam213*invg01*invg23 + 16*Gam203*Gam222*invg02*invg23 + 
       16*Gam202*Gam223*invg02*invg23 + 16*Gam203*Gam223*invg03*invg23 + 
       16*Gam202*Gam233*invg03*invg23 + 16*Gam212*Gam213*invg11*invg23 + 
       16*Gam213*Gam222*invg12*invg23 + 16*Gam212*Gam223*invg12*invg23 + 
       16*Gam213*Gam223*invg13*invg23 + 16*Gam212*Gam233*invg13*invg23 + 
       16*Gam222*Gam223*invg22*invg23 + 
       8*Power(Gam223,2)*Power(invg23,2) + 
       8*Gam222*Gam233*Power(invg23,2) + 8*Power(Gam203,2)*invg00*invg33 + 
       16*Gam203*Gam213*invg01*invg33 + 16*Gam203*Gam223*invg02*invg33 + 
       16*Gam203*Gam233*invg03*invg33 + 8*Power(Gam213,2)*invg11*invg33 + 
       16*Gam213*Gam223*invg12*invg33 + 16*Gam213*Gam233*invg13*invg33 + 
       8*Power(Gam223,2)*invg22*invg33 + 16*Gam223*Gam233*invg23*invg33 + 
       4*Power(Gam233,2)*Power(invg33,2) - 4*gamma0*ndua2*trGam2 - 
       4*(Gam022*invg00 + Gam122*invg01 + Gam222*invg02 + Gam322*invg03)*
        H0[ijk] - 4*Gam022*invg01*H1[ijk] - 4*Gam122*invg11*H1[ijk] - 
       4*Gam222*invg12*H1[ijk] - 4*Gam322*invg13*H1[ijk] - 
       4*Gam022*invg02*H2[ijk] - 4*Gam122*invg12*H2[ijk] - 
       4*Gam222*invg22*H2[ijk] - 4*Gam322*invg23*H2[ijk] - 
       4*gamma0*ndua2*H2[ijk] - 4*Gam022*invg03*H3[ijk] - 
       4*Gam122*invg13*H3[ijk] - 4*Gam222*invg23*H3[ijk] - 
       4*Gam322*invg33*H3[ijk] + 
       2*gamma0*g22[ijk]*(nvec0*trGam0 + nvec1*trGam1 + nvec2*trGam2 + 
          nvec3*trGam3 + nvec0*H0[ijk] + nvec1*H1[ijk] + nvec2*H2[ijk] + 
          nvec3*H3[ijk]) - 4*invg00*invh00*Power(Phi002[ijk],2) - 
       8*invg01*invh00*Phi002[ijk]*Phi012[ijk] - 
       4*invg11*invh00*Power(Phi012[ijk],2) - 
       8*invg02*invh00*Phi002[ijk]*Phi022[ijk] - 
       8*invg12*invh00*Phi012[ijk]*Phi022[ijk] - 
       4*invg22*invh00*Power(Phi022[ijk],2) - 
       8*invg03*invh00*Phi002[ijk]*Phi023[ijk] - 
       8*invg13*invh00*Phi012[ijk]*Phi023[ijk] - 
       8*invg23*invh00*Phi022[ijk]*Phi023[ijk] - 
       4*invg33*invh00*Power(Phi023[ijk],2) - 
       8*invg00*invh01*Phi002[ijk]*Phi102[ijk] - 
       8*invg01*invh01*Phi012[ijk]*Phi102[ijk] - 
       8*invg02*invh01*Phi022[ijk]*Phi102[ijk] - 
       8*invg03*invh01*Phi023[ijk]*Phi102[ijk] - 
       4*invg00*invh11*Power(Phi102[ijk],2) - 
       8*invg01*invh01*Phi002[ijk]*Phi112[ijk] - 
       8*invg11*invh01*Phi012[ijk]*Phi112[ijk] - 
       8*invg12*invh01*Phi022[ijk]*Phi112[ijk] - 
       8*invg13*invh01*Phi023[ijk]*Phi112[ijk] - 
       8*invg01*invh11*Phi102[ijk]*Phi112[ijk] - 
       4*invg11*invh11*Power(Phi112[ijk],2) - 
       8*invg02*invh01*Phi002[ijk]*Phi122[ijk] - 
       8*invg12*invh01*Phi012[ijk]*Phi122[ijk] - 
       8*invg22*invh01*Phi022[ijk]*Phi122[ijk] - 
       8*invg23*invh01*Phi023[ijk]*Phi122[ijk] - 
       8*invg02*invh11*Phi102[ijk]*Phi122[ijk] - 
       8*invg12*invh11*Phi112[ijk]*Phi122[ijk] - 
       4*invg22*invh11*Power(Phi122[ijk],2) - 
       8*invg03*invh01*Phi002[ijk]*Phi123[ijk] - 
       8*invg13*invh01*Phi012[ijk]*Phi123[ijk] - 
       8*invg23*invh01*Phi022[ijk]*Phi123[ijk] - 
       8*invg33*invh01*Phi023[ijk]*Phi123[ijk] - 
       8*invg03*invh11*Phi102[ijk]*Phi123[ijk] - 
       8*invg13*invh11*Phi112[ijk]*Phi123[ijk] - 
       8*invg23*invh11*Phi122[ijk]*Phi123[ijk] - 
       4*invg33*invh11*Power(Phi123[ijk],2) - 
       8*invg00*invh02*Phi002[ijk]*Phi202[ijk] - 
       8*invg01*invh02*Phi012[ijk]*Phi202[ijk] - 
       8*invg02*invh02*Phi022[ijk]*Phi202[ijk] - 
       8*invg03*invh02*Phi023[ijk]*Phi202[ijk] - 
       8*invg00*invh12*Phi102[ijk]*Phi202[ijk] - 
       8*invg01*invh12*Phi112[ijk]*Phi202[ijk] - 
       8*invg02*invh12*Phi122[ijk]*Phi202[ijk] - 
       8*invg03*invh12*Phi123[ijk]*Phi202[ijk] - 
       4*invg00*invh22*Power(Phi202[ijk],2) - 
       8*invg01*invh02*Phi002[ijk]*Phi212[ijk] - 
       8*invg11*invh02*Phi012[ijk]*Phi212[ijk] - 
       8*invg12*invh02*Phi022[ijk]*Phi212[ijk] - 
       8*invg13*invh02*Phi023[ijk]*Phi212[ijk] - 
       8*invg01*invh12*Phi102[ijk]*Phi212[ijk] - 
       8*invg11*invh12*Phi112[ijk]*Phi212[ijk] - 
       8*invg12*invh12*Phi122[ijk]*Phi212[ijk] - 
       8*invg13*invh12*Phi123[ijk]*Phi212[ijk] - 
       8*invg01*invh22*Phi202[ijk]*Phi212[ijk] - 
       4*invg11*invh22*Power(Phi212[ijk],2) - 
       8*invg02*invh02*Phi002[ijk]*Phi222[ijk] - 
       8*invg12*invh02*Phi012[ijk]*Phi222[ijk] - 
       8*invg22*invh02*Phi022[ijk]*Phi222[ijk] - 
       8*invg23*invh02*Phi023[ijk]*Phi222[ijk] - 
       8*invg02*invh12*Phi102[ijk]*Phi222[ijk] - 
       8*invg12*invh12*Phi112[ijk]*Phi222[ijk] - 
       8*invg22*invh12*Phi122[ijk]*Phi222[ijk] - 
       8*invg23*invh12*Phi123[ijk]*Phi222[ijk] - 
       8*invg02*invh22*Phi202[ijk]*Phi222[ijk] - 
       8*invg12*invh22*Phi212[ijk]*Phi222[ijk] - 
       4*invg22*invh22*Power(Phi222[ijk],2) - 
       8*invg03*invh02*Phi002[ijk]*Phi223[ijk] - 
       8*invg13*invh02*Phi012[ijk]*Phi223[ijk] - 
       8*invg23*invh02*Phi022[ijk]*Phi223[ijk] - 
       8*invg33*invh02*Phi023[ijk]*Phi223[ijk] - 
       8*invg03*invh12*Phi102[ijk]*Phi223[ijk] - 
       8*invg13*invh12*Phi112[ijk]*Phi223[ijk] - 
       8*invg23*invh12*Phi122[ijk]*Phi223[ijk] - 
       8*invg33*invh12*Phi123[ijk]*Phi223[ijk] - 
       8*invg03*invh22*Phi202[ijk]*Phi223[ijk] - 
       8*invg13*invh22*Phi212[ijk]*Phi223[ijk] - 
       8*invg23*invh22*Phi222[ijk]*Phi223[ijk] - 
       4*invg33*invh22*Power(Phi223[ijk],2) - 
       8*invg00*invh03*Phi002[ijk]*Phi302[ijk] - 
       8*invg01*invh03*Phi012[ijk]*Phi302[ijk] - 
       8*invg02*invh03*Phi022[ijk]*Phi302[ijk] - 
       8*invg03*invh03*Phi023[ijk]*Phi302[ijk] - 
       8*invg00*invh13*Phi102[ijk]*Phi302[ijk] - 
       8*invg01*invh13*Phi112[ijk]*Phi302[ijk] - 
       8*invg02*invh13*Phi122[ijk]*Phi302[ijk] - 
       8*invg03*invh13*Phi123[ijk]*Phi302[ijk] - 
       8*invg00*invh23*Phi202[ijk]*Phi302[ijk] - 
       8*invg01*invh23*Phi212[ijk]*Phi302[ijk] - 
       8*invg02*invh23*Phi222[ijk]*Phi302[ijk] - 
       8*invg03*invh23*Phi223[ijk]*Phi302[ijk] - 
       4*invg00*invh33*Power(Phi302[ijk],2) - 
       8*invg01*invh03*Phi002[ijk]*Phi312[ijk] - 
       8*invg11*invh03*Phi012[ijk]*Phi312[ijk] - 
       8*invg12*invh03*Phi022[ijk]*Phi312[ijk] - 
       8*invg13*invh03*Phi023[ijk]*Phi312[ijk] - 
       8*invg01*invh13*Phi102[ijk]*Phi312[ijk] - 
       8*invg11*invh13*Phi112[ijk]*Phi312[ijk] - 
       8*invg12*invh13*Phi122[ijk]*Phi312[ijk] - 
       8*invg13*invh13*Phi123[ijk]*Phi312[ijk] - 
       8*invg01*invh23*Phi202[ijk]*Phi312[ijk] - 
       8*invg11*invh23*Phi212[ijk]*Phi312[ijk] - 
       8*invg12*invh23*Phi222[ijk]*Phi312[ijk] - 
       8*invg13*invh23*Phi223[ijk]*Phi312[ijk] - 
       8*invg01*invh33*Phi302[ijk]*Phi312[ijk] - 
       4*invg11*invh33*Power(Phi312[ijk],2) - 
       8*invg02*invh03*Phi002[ijk]*Phi322[ijk] - 
       8*invg12*invh03*Phi012[ijk]*Phi322[ijk] - 
       8*invg22*invh03*Phi022[ijk]*Phi322[ijk] - 
       8*invg23*invh03*Phi023[ijk]*Phi322[ijk] - 
       8*invg02*invh13*Phi102[ijk]*Phi322[ijk] - 
       8*invg12*invh13*Phi112[ijk]*Phi322[ijk] - 
       8*invg22*invh13*Phi122[ijk]*Phi322[ijk] - 
       8*invg23*invh13*Phi123[ijk]*Phi322[ijk] - 
       8*invg02*invh23*Phi202[ijk]*Phi322[ijk] - 
       8*invg12*invh23*Phi212[ijk]*Phi322[ijk] - 
       8*invg22*invh23*Phi222[ijk]*Phi322[ijk] - 
       8*invg23*invh23*Phi223[ijk]*Phi322[ijk] - 
       8*invg02*invh33*Phi302[ijk]*Phi322[ijk] - 
       8*invg12*invh33*Phi312[ijk]*Phi322[ijk] - 
       4*invg22*invh33*Power(Phi322[ijk],2) - 
       8*invg03*invh03*Phi002[ijk]*Phi323[ijk] - 
       8*invg13*invh03*Phi012[ijk]*Phi323[ijk] - 
       8*invg23*invh03*Phi022[ijk]*Phi323[ijk] - 
       8*invg33*invh03*Phi023[ijk]*Phi323[ijk] - 
       8*invg03*invh13*Phi102[ijk]*Phi323[ijk] - 
       8*invg13*invh13*Phi112[ijk]*Phi323[ijk] - 
       8*invg23*invh13*Phi122[ijk]*Phi323[ijk] - 
       8*invg33*invh13*Phi123[ijk]*Phi323[ijk] - 
       8*invg03*invh23*Phi202[ijk]*Phi323[ijk] - 
       8*invg13*invh23*Phi212[ijk]*Phi323[ijk] - 
       8*invg23*invh23*Phi222[ijk]*Phi323[ijk] - 
       8*invg33*invh23*Phi223[ijk]*Phi323[ijk] - 
       8*invg03*invh33*Phi302[ijk]*Phi323[ijk] - 
       8*invg13*invh33*Phi312[ijk]*Phi323[ijk] - 
       8*invg23*invh33*Phi322[ijk]*Phi323[ijk] - 
       4*invg33*invh33*Power(Phi323[ijk],2) + 
       2*invh00*nvec0*Phi022[ijk]*Pi00[ijk] + 
       2*invh01*nvec0*Phi122[ijk]*Pi00[ijk] + 
       2*invh02*nvec0*Phi222[ijk]*Pi00[ijk] + 
       2*invh03*nvec0*Phi322[ijk]*Pi00[ijk] + 
       2*invh01*nvec0*Phi022[ijk]*Pi01[ijk] + 
       2*invh00*nvec1*Phi022[ijk]*Pi01[ijk] + 
       2*invh11*nvec0*Phi122[ijk]*Pi01[ijk] + 
       2*invh01*nvec1*Phi122[ijk]*Pi01[ijk] + 
       2*invh12*nvec0*Phi222[ijk]*Pi01[ijk] + 
       2*invh02*nvec1*Phi222[ijk]*Pi01[ijk] + 
       2*invh13*nvec0*Phi322[ijk]*Pi01[ijk] + 
       2*invh03*nvec1*Phi322[ijk]*Pi01[ijk] + 
       2*invh02*nvec0*Phi022[ijk]*Pi02[ijk] + 
       2*invh00*nvec2*Phi022[ijk]*Pi02[ijk] + 
       2*invh12*nvec0*Phi122[ijk]*Pi02[ijk] + 
       2*invh01*nvec2*Phi122[ijk]*Pi02[ijk] + 
       2*invh22*nvec0*Phi222[ijk]*Pi02[ijk] + 
       2*invh02*nvec2*Phi222[ijk]*Pi02[ijk] + 
       2*invh23*nvec0*Phi322[ijk]*Pi02[ijk] + 
       2*invh03*nvec2*Phi322[ijk]*Pi02[ijk] + 
       4*invg00*Power(Pi02[ijk],2) + 
       2*invh03*nvec0*Phi022[ijk]*Pi03[ijk] + 
       2*invh00*nvec3*Phi022[ijk]*Pi03[ijk] + 
       2*invh13*nvec0*Phi122[ijk]*Pi03[ijk] + 
       2*invh01*nvec3*Phi122[ijk]*Pi03[ijk] + 
       2*invh23*nvec0*Phi222[ijk]*Pi03[ijk] + 
       2*invh02*nvec3*Phi222[ijk]*Pi03[ijk] + 
       2*invh33*nvec0*Phi322[ijk]*Pi03[ijk] + 
       2*invh03*nvec3*Phi322[ijk]*Pi03[ijk] + 
       2*invh01*nvec1*Phi022[ijk]*Pi11[ijk] + 
       2*invh11*nvec1*Phi122[ijk]*Pi11[ijk] + 
       2*invh12*nvec1*Phi222[ijk]*Pi11[ijk] + 
       2*invh13*nvec1*Phi322[ijk]*Pi11[ijk] + 
       2*invh02*nvec1*Phi022[ijk]*Pi12[ijk] + 
       2*invh01*nvec2*Phi022[ijk]*Pi12[ijk] + 
       2*invh12*nvec1*Phi122[ijk]*Pi12[ijk] + 
       2*invh11*nvec2*Phi122[ijk]*Pi12[ijk] + 
       2*invh22*nvec1*Phi222[ijk]*Pi12[ijk] + 
       2*invh12*nvec2*Phi222[ijk]*Pi12[ijk] + 
       2*invh23*nvec1*Phi322[ijk]*Pi12[ijk] + 
       2*invh13*nvec2*Phi322[ijk]*Pi12[ijk] + 
       8*invg01*Pi02[ijk]*Pi12[ijk] + 4*invg11*Power(Pi12[ijk],2) + 
       2*invh03*nvec1*Phi022[ijk]*Pi13[ijk] + 
       2*invh01*nvec3*Phi022[ijk]*Pi13[ijk] + 
       2*invh13*nvec1*Phi122[ijk]*Pi13[ijk] + 
       2*invh11*nvec3*Phi122[ijk]*Pi13[ijk] + 
       2*invh23*nvec1*Phi222[ijk]*Pi13[ijk] + 
       2*invh12*nvec3*Phi222[ijk]*Pi13[ijk] + 
       2*invh33*nvec1*Phi322[ijk]*Pi13[ijk] + 
       2*invh13*nvec3*Phi322[ijk]*Pi13[ijk] + 
       2*invh02*nvec2*Phi022[ijk]*Pi22[ijk] + 
       2*invh12*nvec2*Phi122[ijk]*Pi22[ijk] + 
       2*invh22*nvec2*Phi222[ijk]*Pi22[ijk] + 
       2*invh23*nvec2*Phi322[ijk]*Pi22[ijk] + 
       Power(nvec0,2)*Pi00[ijk]*Pi22[ijk] + 
       2*nvec0*nvec1*Pi01[ijk]*Pi22[ijk] + 8*invg02*Pi02[ijk]*Pi22[ijk] + 
       2*nvec0*nvec2*Pi02[ijk]*Pi22[ijk] + 
       2*nvec0*nvec3*Pi03[ijk]*Pi22[ijk] + 
       Power(nvec1,2)*Pi11[ijk]*Pi22[ijk] + 8*invg12*Pi12[ijk]*Pi22[ijk] + 
       2*nvec1*nvec2*Pi12[ijk]*Pi22[ijk] + 
       2*nvec1*nvec3*Pi13[ijk]*Pi22[ijk] + 4*invg22*Power(Pi22[ijk],2) + 
       Power(nvec2,2)*Power(Pi22[ijk],2) + 
       2*invh03*nvec2*Phi022[ijk]*Pi23[ijk] + 
       2*invh02*nvec3*Phi022[ijk]*Pi23[ijk] + 
       2*invh13*nvec2*Phi122[ijk]*Pi23[ijk] + 
       2*invh12*nvec3*Phi122[ijk]*Pi23[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Pi23[ijk] + 
       2*invh22*nvec3*Phi222[ijk]*Pi23[ijk] + 
       2*invh33*nvec2*Phi322[ijk]*Pi23[ijk] + 
       2*invh23*nvec3*Phi322[ijk]*Pi23[ijk] + 
       8*invg03*Pi02[ijk]*Pi23[ijk] + 8*invg13*Pi12[ijk]*Pi23[ijk] + 
       8*invg23*Pi22[ijk]*Pi23[ijk] + 2*nvec2*nvec3*Pi22[ijk]*Pi23[ijk] + 
       4*invg33*Power(Pi23[ijk],2) + 
       2*invh03*nvec3*Phi022[ijk]*Pi33[ijk] + 
       2*invh13*nvec3*Phi122[ijk]*Pi33[ijk] + 
       2*invh23*nvec3*Phi222[ijk]*Pi33[ijk] + 
       2*invh33*nvec3*Phi322[ijk]*Pi33[ijk] + 
       Power(nvec3,2)*Pi22[ijk]*Pi33[ijk]))/2. - srcSdH22[ijk]
;

dtPi23[ijk]
=
(-2*interior*AdPi23[ijk] + alpha[ijk]*
     (-4*Gam200*Gam300*Power(invg00,2) - 8*Gam201*Gam300*invg00*invg01 - 
       8*Gam200*Gam301*invg00*invg01 - 4*Gam211*Gam300*Power(invg01,2) - 
       8*Gam201*Gam301*Power(invg01,2) - 4*Gam200*Gam311*Power(invg01,2) - 
       8*Gam202*Gam300*invg00*invg02 - 8*Gam200*Gam302*invg00*invg02 - 
       8*Gam212*Gam300*invg01*invg02 - 8*Gam202*Gam301*invg01*invg02 - 
       8*Gam201*Gam302*invg01*invg02 - 8*Gam200*Gam312*invg01*invg02 - 
       4*Gam222*Gam300*Power(invg02,2) - 8*Gam202*Gam302*Power(invg02,2) - 
       4*Gam200*Gam322*Power(invg02,2) - 8*Gam203*Gam300*invg00*invg03 - 
       8*Gam200*Gam303*invg00*invg03 - 8*Gam213*Gam300*invg01*invg03 - 
       8*Gam203*Gam301*invg01*invg03 - 8*Gam201*Gam303*invg01*invg03 - 
       8*Gam200*Gam313*invg01*invg03 - 8*Gam223*Gam300*invg02*invg03 - 
       8*Gam203*Gam302*invg02*invg03 - 8*Gam202*Gam303*invg02*invg03 - 
       8*Gam200*Gam323*invg02*invg03 - 4*Gam233*Gam300*Power(invg03,2) - 
       8*Gam203*Gam303*Power(invg03,2) - 4*Gam200*Gam333*Power(invg03,2) - 
       8*Gam201*Gam301*invg00*invg11 - 8*Gam211*Gam301*invg01*invg11 - 
       8*Gam201*Gam311*invg01*invg11 - 8*Gam212*Gam301*invg02*invg11 - 
       8*Gam201*Gam312*invg02*invg11 - 8*Gam213*Gam301*invg03*invg11 - 
       8*Gam201*Gam313*invg03*invg11 - 4*Gam211*Gam311*Power(invg11,2) - 
       8*Gam202*Gam301*invg00*invg12 - 8*Gam201*Gam302*invg00*invg12 - 
       8*Gam212*Gam301*invg01*invg12 - 8*Gam211*Gam302*invg01*invg12 - 
       8*Gam202*Gam311*invg01*invg12 - 8*Gam201*Gam312*invg01*invg12 - 
       8*Gam222*Gam301*invg02*invg12 - 8*Gam212*Gam302*invg02*invg12 - 
       8*Gam202*Gam312*invg02*invg12 - 8*Gam201*Gam322*invg02*invg12 - 
       8*Gam223*Gam301*invg03*invg12 - 8*Gam213*Gam302*invg03*invg12 - 
       8*Gam202*Gam313*invg03*invg12 - 8*Gam201*Gam323*invg03*invg12 - 
       8*Gam212*Gam311*invg11*invg12 - 8*Gam211*Gam312*invg11*invg12 - 
       4*Gam222*Gam311*Power(invg12,2) - 8*Gam212*Gam312*Power(invg12,2) - 
       4*Gam211*Gam322*Power(invg12,2) - 8*Gam203*Gam301*invg00*invg13 - 
       8*Gam201*Gam303*invg00*invg13 - 8*Gam213*Gam301*invg01*invg13 - 
       8*Gam211*Gam303*invg01*invg13 - 8*Gam203*Gam311*invg01*invg13 - 
       8*Gam201*Gam313*invg01*invg13 - 8*Gam223*Gam301*invg02*invg13 - 
       8*Gam212*Gam303*invg02*invg13 - 8*Gam203*Gam312*invg02*invg13 - 
       8*Gam201*Gam323*invg02*invg13 - 8*Gam233*Gam301*invg03*invg13 - 
       8*Gam213*Gam303*invg03*invg13 - 8*Gam203*Gam313*invg03*invg13 - 
       8*Gam201*Gam333*invg03*invg13 - 8*Gam213*Gam311*invg11*invg13 - 
       8*Gam211*Gam313*invg11*invg13 - 8*Gam223*Gam311*invg12*invg13 - 
       8*Gam213*Gam312*invg12*invg13 - 8*Gam212*Gam313*invg12*invg13 - 
       8*Gam211*Gam323*invg12*invg13 - 4*Gam233*Gam311*Power(invg13,2) - 
       8*Gam213*Gam313*Power(invg13,2) - 4*Gam211*Gam333*Power(invg13,2) - 
       8*Gam202*Gam302*invg00*invg22 - 8*Gam212*Gam302*invg01*invg22 - 
       8*Gam202*Gam312*invg01*invg22 - 8*Gam222*Gam302*invg02*invg22 - 
       8*Gam202*Gam322*invg02*invg22 - 8*Gam223*Gam302*invg03*invg22 - 
       8*Gam202*Gam323*invg03*invg22 - 8*Gam212*Gam312*invg11*invg22 - 
       8*Gam222*Gam312*invg12*invg22 - 8*Gam212*Gam322*invg12*invg22 - 
       8*Gam223*Gam312*invg13*invg22 - 8*Gam212*Gam323*invg13*invg22 - 
       4*Gam222*Gam322*Power(invg22,2) - 8*Gam203*Gam302*invg00*invg23 - 
       8*Gam202*Gam303*invg00*invg23 - 8*Gam213*Gam302*invg01*invg23 - 
       8*Gam212*Gam303*invg01*invg23 - 8*Gam203*Gam312*invg01*invg23 - 
       8*Gam202*Gam313*invg01*invg23 - 8*Gam223*Gam302*invg02*invg23 - 
       8*Gam222*Gam303*invg02*invg23 - 8*Gam203*Gam322*invg02*invg23 - 
       8*Gam202*Gam323*invg02*invg23 - 8*Gam233*Gam302*invg03*invg23 - 
       8*Gam223*Gam303*invg03*invg23 - 8*Gam203*Gam323*invg03*invg23 - 
       8*Gam202*Gam333*invg03*invg23 - 8*Gam213*Gam312*invg11*invg23 - 
       8*Gam212*Gam313*invg11*invg23 - 8*Gam223*Gam312*invg12*invg23 - 
       8*Gam222*Gam313*invg12*invg23 - 8*Gam213*Gam322*invg12*invg23 - 
       8*Gam212*Gam323*invg12*invg23 - 8*Gam233*Gam312*invg13*invg23 - 
       8*Gam223*Gam313*invg13*invg23 - 8*Gam213*Gam323*invg13*invg23 - 
       8*Gam212*Gam333*invg13*invg23 - 8*Gam223*Gam322*invg22*invg23 - 
       8*Gam222*Gam323*invg22*invg23 - 4*Gam233*Gam322*Power(invg23,2) - 
       8*Gam223*Gam323*Power(invg23,2) - 4*Gam222*Gam333*Power(invg23,2) - 
       8*Gam203*Gam303*invg00*invg33 - 8*Gam213*Gam303*invg01*invg33 - 
       8*Gam203*Gam313*invg01*invg33 - 8*Gam223*Gam303*invg02*invg33 - 
       8*Gam203*Gam323*invg02*invg33 - 8*Gam233*Gam303*invg03*invg33 - 
       8*Gam203*Gam333*invg03*invg33 - 8*Gam213*Gam313*invg11*invg33 - 
       8*Gam223*Gam313*invg12*invg33 - 8*Gam213*Gam323*invg12*invg33 - 
       8*Gam233*Gam313*invg13*invg33 - 8*Gam213*Gam333*invg13*invg33 - 
       8*Gam223*Gam323*invg22*invg33 - 8*Gam233*Gam323*invg23*invg33 - 
       8*Gam223*Gam333*invg23*invg33 - 4*Gam233*Gam333*Power(invg33,2) + 
       2*gamma0*ndua3*trGam2 + 2*gamma0*ndua2*trGam3 + 
       4*(Gam023*invg00 + Gam123*invg01 + Gam223*invg02 + Gam323*invg03)*
        H0[ijk] + 4*Gam023*invg01*H1[ijk] + 4*Gam123*invg11*H1[ijk] + 
       4*Gam223*invg12*H1[ijk] + 4*Gam323*invg13*H1[ijk] + 
       4*Gam023*invg02*H2[ijk] + 4*Gam123*invg12*H2[ijk] + 
       4*Gam223*invg22*H2[ijk] + 4*Gam323*invg23*H2[ijk] + 
       2*gamma0*ndua3*H2[ijk] + 4*Gam023*invg03*H3[ijk] + 
       4*Gam123*invg13*H3[ijk] + 4*Gam223*invg23*H3[ijk] + 
       4*Gam323*invg33*H3[ijk] + 2*gamma0*ndua2*H3[ijk] - 
       2*gamma0*g23[ijk]*(nvec0*trGam0 + nvec1*trGam1 + nvec2*trGam2 + 
          nvec3*trGam3 + nvec0*H0[ijk] + nvec1*H1[ijk] + nvec2*H2[ijk] + 
          nvec3*H3[ijk]) + 4*invg00*invh00*Phi002[ijk]*Phi003[ijk] + 
       4*invg01*invh00*Phi003[ijk]*Phi012[ijk] + 
       4*invg01*invh00*Phi002[ijk]*Phi013[ijk] + 
       4*invg11*invh00*Phi012[ijk]*Phi013[ijk] + 
       4*invg02*invh00*Phi003[ijk]*Phi022[ijk] + 
       4*invg12*invh00*Phi013[ijk]*Phi022[ijk] + 
       4*invg02*invh00*Phi002[ijk]*Phi023[ijk] + 
       4*invg03*invh00*Phi003[ijk]*Phi023[ijk] + 
       4*invg12*invh00*Phi012[ijk]*Phi023[ijk] + 
       4*invg13*invh00*Phi013[ijk]*Phi023[ijk] + 
       4*invg22*invh00*Phi022[ijk]*Phi023[ijk] + 
       4*invg23*invh00*Power(Phi023[ijk],2) + 
       4*invg03*invh00*Phi002[ijk]*Phi033[ijk] + 
       4*invg13*invh00*Phi012[ijk]*Phi033[ijk] + 
       4*invg23*invh00*Phi022[ijk]*Phi033[ijk] + 
       4*invg33*invh00*Phi023[ijk]*Phi033[ijk] + 
       4*invg00*invh01*Phi003[ijk]*Phi102[ijk] + 
       4*invg01*invh01*Phi013[ijk]*Phi102[ijk] + 
       4*invg02*invh01*Phi023[ijk]*Phi102[ijk] + 
       4*invg03*invh01*Phi033[ijk]*Phi102[ijk] + 
       4*invg00*invh01*Phi002[ijk]*Phi103[ijk] + 
       4*invg01*invh01*Phi012[ijk]*Phi103[ijk] + 
       4*invg02*invh01*Phi022[ijk]*Phi103[ijk] + 
       4*invg03*invh01*Phi023[ijk]*Phi103[ijk] + 
       4*invg00*invh11*Phi102[ijk]*Phi103[ijk] + 
       4*invg01*invh01*Phi003[ijk]*Phi112[ijk] + 
       4*invg11*invh01*Phi013[ijk]*Phi112[ijk] + 
       4*invg12*invh01*Phi023[ijk]*Phi112[ijk] + 
       4*invg13*invh01*Phi033[ijk]*Phi112[ijk] + 
       4*invg01*invh11*Phi103[ijk]*Phi112[ijk] + 
       4*invg01*invh01*Phi002[ijk]*Phi113[ijk] + 
       4*invg11*invh01*Phi012[ijk]*Phi113[ijk] + 
       4*invg12*invh01*Phi022[ijk]*Phi113[ijk] + 
       4*invg13*invh01*Phi023[ijk]*Phi113[ijk] + 
       4*invg01*invh11*Phi102[ijk]*Phi113[ijk] + 
       4*invg11*invh11*Phi112[ijk]*Phi113[ijk] + 
       4*invg02*invh01*Phi003[ijk]*Phi122[ijk] + 
       4*invg12*invh01*Phi013[ijk]*Phi122[ijk] + 
       4*invg22*invh01*Phi023[ijk]*Phi122[ijk] + 
       4*invg23*invh01*Phi033[ijk]*Phi122[ijk] + 
       4*invg02*invh11*Phi103[ijk]*Phi122[ijk] + 
       4*invg12*invh11*Phi113[ijk]*Phi122[ijk] + 
       4*invg02*invh01*Phi002[ijk]*Phi123[ijk] + 
       4*invg03*invh01*Phi003[ijk]*Phi123[ijk] + 
       4*invg12*invh01*Phi012[ijk]*Phi123[ijk] + 
       4*invg13*invh01*Phi013[ijk]*Phi123[ijk] + 
       4*invg22*invh01*Phi022[ijk]*Phi123[ijk] + 
       8*invg23*invh01*Phi023[ijk]*Phi123[ijk] + 
       4*invg33*invh01*Phi033[ijk]*Phi123[ijk] + 
       4*invg02*invh11*Phi102[ijk]*Phi123[ijk] + 
       4*invg03*invh11*Phi103[ijk]*Phi123[ijk] + 
       4*invg12*invh11*Phi112[ijk]*Phi123[ijk] + 
       4*invg13*invh11*Phi113[ijk]*Phi123[ijk] + 
       4*invg22*invh11*Phi122[ijk]*Phi123[ijk] + 
       4*invg23*invh11*Power(Phi123[ijk],2) + 
       4*invg03*invh01*Phi002[ijk]*Phi133[ijk] + 
       4*invg13*invh01*Phi012[ijk]*Phi133[ijk] + 
       4*invg23*invh01*Phi022[ijk]*Phi133[ijk] + 
       4*invg33*invh01*Phi023[ijk]*Phi133[ijk] + 
       4*invg03*invh11*Phi102[ijk]*Phi133[ijk] + 
       4*invg13*invh11*Phi112[ijk]*Phi133[ijk] + 
       4*invg23*invh11*Phi122[ijk]*Phi133[ijk] + 
       4*invg33*invh11*Phi123[ijk]*Phi133[ijk] + 
       4*invg00*invh02*Phi003[ijk]*Phi202[ijk] + 
       4*invg01*invh02*Phi013[ijk]*Phi202[ijk] + 
       4*invg02*invh02*Phi023[ijk]*Phi202[ijk] + 
       4*invg03*invh02*Phi033[ijk]*Phi202[ijk] + 
       4*invg00*invh12*Phi103[ijk]*Phi202[ijk] + 
       4*invg01*invh12*Phi113[ijk]*Phi202[ijk] + 
       4*invg02*invh12*Phi123[ijk]*Phi202[ijk] + 
       4*invg03*invh12*Phi133[ijk]*Phi202[ijk] + 
       4*invg00*invh02*Phi002[ijk]*Phi203[ijk] + 
       4*invg01*invh02*Phi012[ijk]*Phi203[ijk] + 
       4*invg02*invh02*Phi022[ijk]*Phi203[ijk] + 
       4*invg03*invh02*Phi023[ijk]*Phi203[ijk] + 
       4*invg00*invh12*Phi102[ijk]*Phi203[ijk] + 
       4*invg01*invh12*Phi112[ijk]*Phi203[ijk] + 
       4*invg02*invh12*Phi122[ijk]*Phi203[ijk] + 
       4*invg03*invh12*Phi123[ijk]*Phi203[ijk] + 
       4*invg00*invh22*Phi202[ijk]*Phi203[ijk] + 
       4*invg01*invh02*Phi003[ijk]*Phi212[ijk] + 
       4*invg11*invh02*Phi013[ijk]*Phi212[ijk] + 
       4*invg12*invh02*Phi023[ijk]*Phi212[ijk] + 
       4*invg13*invh02*Phi033[ijk]*Phi212[ijk] + 
       4*invg01*invh12*Phi103[ijk]*Phi212[ijk] + 
       4*invg11*invh12*Phi113[ijk]*Phi212[ijk] + 
       4*invg12*invh12*Phi123[ijk]*Phi212[ijk] + 
       4*invg13*invh12*Phi133[ijk]*Phi212[ijk] + 
       4*invg01*invh22*Phi203[ijk]*Phi212[ijk] + 
       4*invg01*invh02*Phi002[ijk]*Phi213[ijk] + 
       4*invg11*invh02*Phi012[ijk]*Phi213[ijk] + 
       4*invg12*invh02*Phi022[ijk]*Phi213[ijk] + 
       4*invg13*invh02*Phi023[ijk]*Phi213[ijk] + 
       4*invg01*invh12*Phi102[ijk]*Phi213[ijk] + 
       4*invg11*invh12*Phi112[ijk]*Phi213[ijk] + 
       4*invg12*invh12*Phi122[ijk]*Phi213[ijk] + 
       4*invg13*invh12*Phi123[ijk]*Phi213[ijk] + 
       4*invg01*invh22*Phi202[ijk]*Phi213[ijk] + 
       4*invg11*invh22*Phi212[ijk]*Phi213[ijk] + 
       4*invg02*invh02*Phi003[ijk]*Phi222[ijk] + 
       4*invg12*invh02*Phi013[ijk]*Phi222[ijk] + 
       4*invg22*invh02*Phi023[ijk]*Phi222[ijk] + 
       4*invg23*invh02*Phi033[ijk]*Phi222[ijk] + 
       4*invg02*invh12*Phi103[ijk]*Phi222[ijk] + 
       4*invg12*invh12*Phi113[ijk]*Phi222[ijk] + 
       4*invg22*invh12*Phi123[ijk]*Phi222[ijk] + 
       4*invg23*invh12*Phi133[ijk]*Phi222[ijk] + 
       4*invg02*invh22*Phi203[ijk]*Phi222[ijk] + 
       4*invg12*invh22*Phi213[ijk]*Phi222[ijk] + 
       4*invg02*invh02*Phi002[ijk]*Phi223[ijk] + 
       4*invg03*invh02*Phi003[ijk]*Phi223[ijk] + 
       4*invg12*invh02*Phi012[ijk]*Phi223[ijk] + 
       4*invg13*invh02*Phi013[ijk]*Phi223[ijk] + 
       4*invg22*invh02*Phi022[ijk]*Phi223[ijk] + 
       8*invg23*invh02*Phi023[ijk]*Phi223[ijk] + 
       4*invg33*invh02*Phi033[ijk]*Phi223[ijk] + 
       4*invg02*invh12*Phi102[ijk]*Phi223[ijk] + 
       4*invg03*invh12*Phi103[ijk]*Phi223[ijk] + 
       4*invg12*invh12*Phi112[ijk]*Phi223[ijk] + 
       4*invg13*invh12*Phi113[ijk]*Phi223[ijk] + 
       4*invg22*invh12*Phi122[ijk]*Phi223[ijk] + 
       8*invg23*invh12*Phi123[ijk]*Phi223[ijk] + 
       4*invg33*invh12*Phi133[ijk]*Phi223[ijk] + 
       4*invg02*invh22*Phi202[ijk]*Phi223[ijk] + 
       4*invg03*invh22*Phi203[ijk]*Phi223[ijk] + 
       4*invg12*invh22*Phi212[ijk]*Phi223[ijk] + 
       4*invg13*invh22*Phi213[ijk]*Phi223[ijk] + 
       4*invg22*invh22*Phi222[ijk]*Phi223[ijk] + 
       4*invg23*invh22*Power(Phi223[ijk],2) + 
       4*invg03*invh02*Phi002[ijk]*Phi233[ijk] + 
       4*invg13*invh02*Phi012[ijk]*Phi233[ijk] + 
       4*invg23*invh02*Phi022[ijk]*Phi233[ijk] + 
       4*invg33*invh02*Phi023[ijk]*Phi233[ijk] + 
       4*invg03*invh12*Phi102[ijk]*Phi233[ijk] + 
       4*invg13*invh12*Phi112[ijk]*Phi233[ijk] + 
       4*invg23*invh12*Phi122[ijk]*Phi233[ijk] + 
       4*invg33*invh12*Phi123[ijk]*Phi233[ijk] + 
       4*invg03*invh22*Phi202[ijk]*Phi233[ijk] + 
       4*invg13*invh22*Phi212[ijk]*Phi233[ijk] + 
       4*invg23*invh22*Phi222[ijk]*Phi233[ijk] + 
       4*invg33*invh22*Phi223[ijk]*Phi233[ijk] + 
       4*invg00*invh03*Phi003[ijk]*Phi302[ijk] + 
       4*invg01*invh03*Phi013[ijk]*Phi302[ijk] + 
       4*invg02*invh03*Phi023[ijk]*Phi302[ijk] + 
       4*invg03*invh03*Phi033[ijk]*Phi302[ijk] + 
       4*invg00*invh13*Phi103[ijk]*Phi302[ijk] + 
       4*invg01*invh13*Phi113[ijk]*Phi302[ijk] + 
       4*invg02*invh13*Phi123[ijk]*Phi302[ijk] + 
       4*invg03*invh13*Phi133[ijk]*Phi302[ijk] + 
       4*invg00*invh23*Phi203[ijk]*Phi302[ijk] + 
       4*invg01*invh23*Phi213[ijk]*Phi302[ijk] + 
       4*invg02*invh23*Phi223[ijk]*Phi302[ijk] + 
       4*invg03*invh23*Phi233[ijk]*Phi302[ijk] + 
       4*invg00*invh03*Phi002[ijk]*Phi303[ijk] + 
       4*invg01*invh03*Phi012[ijk]*Phi303[ijk] + 
       4*invg02*invh03*Phi022[ijk]*Phi303[ijk] + 
       4*invg03*invh03*Phi023[ijk]*Phi303[ijk] + 
       4*invg00*invh13*Phi102[ijk]*Phi303[ijk] + 
       4*invg01*invh13*Phi112[ijk]*Phi303[ijk] + 
       4*invg02*invh13*Phi122[ijk]*Phi303[ijk] + 
       4*invg03*invh13*Phi123[ijk]*Phi303[ijk] + 
       4*invg00*invh23*Phi202[ijk]*Phi303[ijk] + 
       4*invg01*invh23*Phi212[ijk]*Phi303[ijk] + 
       4*invg02*invh23*Phi222[ijk]*Phi303[ijk] + 
       4*invg03*invh23*Phi223[ijk]*Phi303[ijk] + 
       4*invg00*invh33*Phi302[ijk]*Phi303[ijk] + 
       4*invg01*invh03*Phi003[ijk]*Phi312[ijk] + 
       4*invg11*invh03*Phi013[ijk]*Phi312[ijk] + 
       4*invg12*invh03*Phi023[ijk]*Phi312[ijk] + 
       4*invg13*invh03*Phi033[ijk]*Phi312[ijk] + 
       4*invg01*invh13*Phi103[ijk]*Phi312[ijk] + 
       4*invg11*invh13*Phi113[ijk]*Phi312[ijk] + 
       4*invg12*invh13*Phi123[ijk]*Phi312[ijk] + 
       4*invg13*invh13*Phi133[ijk]*Phi312[ijk] + 
       4*invg01*invh23*Phi203[ijk]*Phi312[ijk] + 
       4*invg11*invh23*Phi213[ijk]*Phi312[ijk] + 
       4*invg12*invh23*Phi223[ijk]*Phi312[ijk] + 
       4*invg13*invh23*Phi233[ijk]*Phi312[ijk] + 
       4*invg01*invh33*Phi303[ijk]*Phi312[ijk] + 
       4*invg01*invh03*Phi002[ijk]*Phi313[ijk] + 
       4*invg11*invh03*Phi012[ijk]*Phi313[ijk] + 
       4*invg12*invh03*Phi022[ijk]*Phi313[ijk] + 
       4*invg13*invh03*Phi023[ijk]*Phi313[ijk] + 
       4*invg01*invh13*Phi102[ijk]*Phi313[ijk] + 
       4*invg11*invh13*Phi112[ijk]*Phi313[ijk] + 
       4*invg12*invh13*Phi122[ijk]*Phi313[ijk] + 
       4*invg13*invh13*Phi123[ijk]*Phi313[ijk] + 
       4*invg01*invh23*Phi202[ijk]*Phi313[ijk] + 
       4*invg11*invh23*Phi212[ijk]*Phi313[ijk] + 
       4*invg12*invh23*Phi222[ijk]*Phi313[ijk] + 
       4*invg13*invh23*Phi223[ijk]*Phi313[ijk] + 
       4*invg01*invh33*Phi302[ijk]*Phi313[ijk] + 
       4*invg11*invh33*Phi312[ijk]*Phi313[ijk] + 
       4*invg02*invh03*Phi003[ijk]*Phi322[ijk] + 
       4*invg12*invh03*Phi013[ijk]*Phi322[ijk] + 
       4*invg22*invh03*Phi023[ijk]*Phi322[ijk] + 
       4*invg23*invh03*Phi033[ijk]*Phi322[ijk] + 
       4*invg02*invh13*Phi103[ijk]*Phi322[ijk] + 
       4*invg12*invh13*Phi113[ijk]*Phi322[ijk] + 
       4*invg22*invh13*Phi123[ijk]*Phi322[ijk] + 
       4*invg23*invh13*Phi133[ijk]*Phi322[ijk] + 
       4*invg02*invh23*Phi203[ijk]*Phi322[ijk] + 
       4*invg12*invh23*Phi213[ijk]*Phi322[ijk] + 
       4*invg22*invh23*Phi223[ijk]*Phi322[ijk] + 
       4*invg23*invh23*Phi233[ijk]*Phi322[ijk] + 
       4*invg02*invh33*Phi303[ijk]*Phi322[ijk] + 
       4*invg12*invh33*Phi313[ijk]*Phi322[ijk] + 
       4*invg02*invh03*Phi002[ijk]*Phi323[ijk] + 
       4*invg03*invh03*Phi003[ijk]*Phi323[ijk] + 
       4*invg12*invh03*Phi012[ijk]*Phi323[ijk] + 
       4*invg13*invh03*Phi013[ijk]*Phi323[ijk] + 
       4*invg22*invh03*Phi022[ijk]*Phi323[ijk] + 
       8*invg23*invh03*Phi023[ijk]*Phi323[ijk] + 
       4*invg33*invh03*Phi033[ijk]*Phi323[ijk] + 
       4*invg02*invh13*Phi102[ijk]*Phi323[ijk] + 
       4*invg03*invh13*Phi103[ijk]*Phi323[ijk] + 
       4*invg12*invh13*Phi112[ijk]*Phi323[ijk] + 
       4*invg13*invh13*Phi113[ijk]*Phi323[ijk] + 
       4*invg22*invh13*Phi122[ijk]*Phi323[ijk] + 
       8*invg23*invh13*Phi123[ijk]*Phi323[ijk] + 
       4*invg33*invh13*Phi133[ijk]*Phi323[ijk] + 
       4*invg02*invh23*Phi202[ijk]*Phi323[ijk] + 
       4*invg03*invh23*Phi203[ijk]*Phi323[ijk] + 
       4*invg12*invh23*Phi212[ijk]*Phi323[ijk] + 
       4*invg13*invh23*Phi213[ijk]*Phi323[ijk] + 
       4*invg22*invh23*Phi222[ijk]*Phi323[ijk] + 
       8*invg23*invh23*Phi223[ijk]*Phi323[ijk] + 
       4*invg33*invh23*Phi233[ijk]*Phi323[ijk] + 
       4*invg02*invh33*Phi302[ijk]*Phi323[ijk] + 
       4*invg03*invh33*Phi303[ijk]*Phi323[ijk] + 
       4*invg12*invh33*Phi312[ijk]*Phi323[ijk] + 
       4*invg13*invh33*Phi313[ijk]*Phi323[ijk] + 
       4*invg22*invh33*Phi322[ijk]*Phi323[ijk] + 
       4*invg23*invh33*Power(Phi323[ijk],2) + 
       4*invg03*invh03*Phi002[ijk]*Phi333[ijk] + 
       4*invg13*invh03*Phi012[ijk]*Phi333[ijk] + 
       4*invg23*invh03*Phi022[ijk]*Phi333[ijk] + 
       4*invg33*invh03*Phi023[ijk]*Phi333[ijk] + 
       4*invg03*invh13*Phi102[ijk]*Phi333[ijk] + 
       4*invg13*invh13*Phi112[ijk]*Phi333[ijk] + 
       4*invg23*invh13*Phi122[ijk]*Phi333[ijk] + 
       4*invg33*invh13*Phi123[ijk]*Phi333[ijk] + 
       4*invg03*invh23*Phi202[ijk]*Phi333[ijk] + 
       4*invg13*invh23*Phi212[ijk]*Phi333[ijk] + 
       4*invg23*invh23*Phi222[ijk]*Phi333[ijk] + 
       4*invg33*invh23*Phi223[ijk]*Phi333[ijk] + 
       4*invg03*invh33*Phi302[ijk]*Phi333[ijk] + 
       4*invg13*invh33*Phi312[ijk]*Phi333[ijk] + 
       4*invg23*invh33*Phi322[ijk]*Phi333[ijk] + 
       4*invg33*invh33*Phi323[ijk]*Phi333[ijk] - 
       2*invh00*nvec0*Phi023[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi123[ijk]*Pi00[ijk] - 
       2*invh02*nvec0*Phi223[ijk]*Pi00[ijk] - 
       2*invh03*nvec0*Phi323[ijk]*Pi00[ijk] - 
       2*invh01*nvec0*Phi023[ijk]*Pi01[ijk] - 
       2*invh00*nvec1*Phi023[ijk]*Pi01[ijk] - 
       2*invh11*nvec0*Phi123[ijk]*Pi01[ijk] - 
       2*invh01*nvec1*Phi123[ijk]*Pi01[ijk] - 
       2*invh12*nvec0*Phi223[ijk]*Pi01[ijk] - 
       2*invh02*nvec1*Phi223[ijk]*Pi01[ijk] - 
       2*invh13*nvec0*Phi323[ijk]*Pi01[ijk] - 
       2*invh03*nvec1*Phi323[ijk]*Pi01[ijk] - 
       2*invh02*nvec0*Phi023[ijk]*Pi02[ijk] - 
       2*invh00*nvec2*Phi023[ijk]*Pi02[ijk] - 
       2*invh12*nvec0*Phi123[ijk]*Pi02[ijk] - 
       2*invh01*nvec2*Phi123[ijk]*Pi02[ijk] - 
       2*invh22*nvec0*Phi223[ijk]*Pi02[ijk] - 
       2*invh02*nvec2*Phi223[ijk]*Pi02[ijk] - 
       2*invh23*nvec0*Phi323[ijk]*Pi02[ijk] - 
       2*invh03*nvec2*Phi323[ijk]*Pi02[ijk] - 
       2*invh03*nvec0*Phi023[ijk]*Pi03[ijk] - 
       2*invh00*nvec3*Phi023[ijk]*Pi03[ijk] - 
       2*invh13*nvec0*Phi123[ijk]*Pi03[ijk] - 
       2*invh01*nvec3*Phi123[ijk]*Pi03[ijk] - 
       2*invh23*nvec0*Phi223[ijk]*Pi03[ijk] - 
       2*invh02*nvec3*Phi223[ijk]*Pi03[ijk] - 
       2*invh33*nvec0*Phi323[ijk]*Pi03[ijk] - 
       2*invh03*nvec3*Phi323[ijk]*Pi03[ijk] - 
       4*invg00*Pi02[ijk]*Pi03[ijk] - 
       2*invh01*nvec1*Phi023[ijk]*Pi11[ijk] - 
       2*invh11*nvec1*Phi123[ijk]*Pi11[ijk] - 
       2*invh12*nvec1*Phi223[ijk]*Pi11[ijk] - 
       2*invh13*nvec1*Phi323[ijk]*Pi11[ijk] - 
       2*invh02*nvec1*Phi023[ijk]*Pi12[ijk] - 
       2*invh01*nvec2*Phi023[ijk]*Pi12[ijk] - 
       2*invh12*nvec1*Phi123[ijk]*Pi12[ijk] - 
       2*invh11*nvec2*Phi123[ijk]*Pi12[ijk] - 
       2*invh22*nvec1*Phi223[ijk]*Pi12[ijk] - 
       2*invh12*nvec2*Phi223[ijk]*Pi12[ijk] - 
       2*invh23*nvec1*Phi323[ijk]*Pi12[ijk] - 
       2*invh13*nvec2*Phi323[ijk]*Pi12[ijk] - 
       4*invg01*Pi03[ijk]*Pi12[ijk] - 
       2*invh03*nvec1*Phi023[ijk]*Pi13[ijk] - 
       2*invh01*nvec3*Phi023[ijk]*Pi13[ijk] - 
       2*invh13*nvec1*Phi123[ijk]*Pi13[ijk] - 
       2*invh11*nvec3*Phi123[ijk]*Pi13[ijk] - 
       2*invh23*nvec1*Phi223[ijk]*Pi13[ijk] - 
       2*invh12*nvec3*Phi223[ijk]*Pi13[ijk] - 
       2*invh33*nvec1*Phi323[ijk]*Pi13[ijk] - 
       2*invh13*nvec3*Phi323[ijk]*Pi13[ijk] - 
       4*invg01*Pi02[ijk]*Pi13[ijk] - 4*invg11*Pi12[ijk]*Pi13[ijk] - 
       2*invh02*nvec2*Phi023[ijk]*Pi22[ijk] - 
       2*invh12*nvec2*Phi123[ijk]*Pi22[ijk] - 
       2*invh22*nvec2*Phi223[ijk]*Pi22[ijk] - 
       2*invh23*nvec2*Phi323[ijk]*Pi22[ijk] - 
       4*invg02*Pi03[ijk]*Pi22[ijk] - 4*invg12*Pi13[ijk]*Pi22[ijk] - 
       2*invh03*nvec2*Phi023[ijk]*Pi23[ijk] - 
       2*invh02*nvec3*Phi023[ijk]*Pi23[ijk] - 
       2*invh13*nvec2*Phi123[ijk]*Pi23[ijk] - 
       2*invh12*nvec3*Phi123[ijk]*Pi23[ijk] - 
       2*invh23*nvec2*Phi223[ijk]*Pi23[ijk] - 
       2*invh22*nvec3*Phi223[ijk]*Pi23[ijk] - 
       2*invh33*nvec2*Phi323[ijk]*Pi23[ijk] - 
       2*invh23*nvec3*Phi323[ijk]*Pi23[ijk] - 
       Power(nvec0,2)*Pi00[ijk]*Pi23[ijk] - 
       2*nvec0*nvec1*Pi01[ijk]*Pi23[ijk] - 4*invg02*Pi02[ijk]*Pi23[ijk] - 
       2*nvec0*nvec2*Pi02[ijk]*Pi23[ijk] - 4*invg03*Pi03[ijk]*Pi23[ijk] - 
       2*nvec0*nvec3*Pi03[ijk]*Pi23[ijk] - 
       Power(nvec1,2)*Pi11[ijk]*Pi23[ijk] - 4*invg12*Pi12[ijk]*Pi23[ijk] - 
       2*nvec1*nvec2*Pi12[ijk]*Pi23[ijk] - 4*invg13*Pi13[ijk]*Pi23[ijk] - 
       2*nvec1*nvec3*Pi13[ijk]*Pi23[ijk] - 4*invg22*Pi22[ijk]*Pi23[ijk] - 
       Power(nvec2,2)*Pi22[ijk]*Pi23[ijk] - 4*invg23*Power(Pi23[ijk],2) - 
       2*nvec2*nvec3*Power(Pi23[ijk],2) - 
       2*invh03*nvec3*Phi023[ijk]*Pi33[ijk] - 
       2*invh13*nvec3*Phi123[ijk]*Pi33[ijk] - 
       2*invh23*nvec3*Phi223[ijk]*Pi33[ijk] - 
       2*invh33*nvec3*Phi323[ijk]*Pi33[ijk] - 
       4*invg03*Pi02[ijk]*Pi33[ijk] - 4*invg13*Pi12[ijk]*Pi33[ijk] - 
       4*invg23*Pi22[ijk]*Pi33[ijk] - 4*invg33*Pi23[ijk]*Pi33[ijk] - 
       Power(nvec3,2)*Pi23[ijk]*Pi33[ijk]) - 
    2*(gamma1*gamma2*beta0[ijk]*Phi023[ijk] + 
       gamma1*gamma2*beta1[ijk]*Phi123[ijk] + 
       gamma1*gamma2*beta2[ijk]*Phi223[ijk] + 
       gamma1*gamma2*beta3[ijk]*Phi323[ijk] + srcSdH23[ijk]))/2.
;

dtPi33[ijk]
=
-(interior*AdPi33[ijk]) - gamma1*gamma2*beta0[ijk]*Phi033[ijk] - 
  gamma1*gamma2*beta1[ijk]*Phi133[ijk] - 
  gamma1*gamma2*beta2[ijk]*Phi233[ijk] - 
  gamma1*gamma2*beta3[ijk]*Phi333[ijk] - 
  (alpha[ijk]*(4*Power(Gam300,2)*Power(invg00,2) + 
       16*Gam300*Gam301*invg00*invg01 + 
       8*Power(Gam301,2)*Power(invg01,2) + 
       8*Gam300*Gam311*Power(invg01,2) + 16*Gam300*Gam302*invg00*invg02 + 
       16*Gam301*Gam302*invg01*invg02 + 16*Gam300*Gam312*invg01*invg02 + 
       8*Power(Gam302,2)*Power(invg02,2) + 
       8*Gam300*Gam322*Power(invg02,2) + 16*Gam300*Gam303*invg00*invg03 + 
       16*Gam301*Gam303*invg01*invg03 + 16*Gam300*Gam313*invg01*invg03 + 
       16*Gam302*Gam303*invg02*invg03 + 16*Gam300*Gam323*invg02*invg03 + 
       8*Power(Gam303,2)*Power(invg03,2) + 
       8*Gam300*Gam333*Power(invg03,2) + 8*Power(Gam301,2)*invg00*invg11 + 
       16*Gam301*Gam311*invg01*invg11 + 16*Gam301*Gam312*invg02*invg11 + 
       16*Gam301*Gam313*invg03*invg11 + 
       4*Power(Gam311,2)*Power(invg11,2) + 
       16*Gam301*Gam302*invg00*invg12 + 16*Gam302*Gam311*invg01*invg12 + 
       16*Gam301*Gam312*invg01*invg12 + 16*Gam302*Gam312*invg02*invg12 + 
       16*Gam301*Gam322*invg02*invg12 + 16*Gam302*Gam313*invg03*invg12 + 
       16*Gam301*Gam323*invg03*invg12 + 16*Gam311*Gam312*invg11*invg12 + 
       8*Power(Gam312,2)*Power(invg12,2) + 
       8*Gam311*Gam322*Power(invg12,2) + 16*Gam301*Gam303*invg00*invg13 + 
       16*Gam303*Gam311*invg01*invg13 + 16*Gam301*Gam313*invg01*invg13 + 
       16*Gam303*Gam312*invg02*invg13 + 16*Gam301*Gam323*invg02*invg13 + 
       16*Gam303*Gam313*invg03*invg13 + 16*Gam301*Gam333*invg03*invg13 + 
       16*Gam311*Gam313*invg11*invg13 + 16*Gam312*Gam313*invg12*invg13 + 
       16*Gam311*Gam323*invg12*invg13 + 
       8*Power(Gam313,2)*Power(invg13,2) + 
       8*Gam311*Gam333*Power(invg13,2) + 8*Power(Gam302,2)*invg00*invg22 + 
       16*Gam302*Gam312*invg01*invg22 + 16*Gam302*Gam322*invg02*invg22 + 
       16*Gam302*Gam323*invg03*invg22 + 8*Power(Gam312,2)*invg11*invg22 + 
       16*Gam312*Gam322*invg12*invg22 + 16*Gam312*Gam323*invg13*invg22 + 
       4*Power(Gam322,2)*Power(invg22,2) + 
       16*Gam302*Gam303*invg00*invg23 + 16*Gam303*Gam312*invg01*invg23 + 
       16*Gam302*Gam313*invg01*invg23 + 16*Gam303*Gam322*invg02*invg23 + 
       16*Gam302*Gam323*invg02*invg23 + 16*Gam303*Gam323*invg03*invg23 + 
       16*Gam302*Gam333*invg03*invg23 + 16*Gam312*Gam313*invg11*invg23 + 
       16*Gam313*Gam322*invg12*invg23 + 16*Gam312*Gam323*invg12*invg23 + 
       16*Gam313*Gam323*invg13*invg23 + 16*Gam312*Gam333*invg13*invg23 + 
       16*Gam322*Gam323*invg22*invg23 + 
       8*Power(Gam323,2)*Power(invg23,2) + 
       8*Gam322*Gam333*Power(invg23,2) + 8*Power(Gam303,2)*invg00*invg33 + 
       16*Gam303*Gam313*invg01*invg33 + 16*Gam303*Gam323*invg02*invg33 + 
       16*Gam303*Gam333*invg03*invg33 + 8*Power(Gam313,2)*invg11*invg33 + 
       16*Gam313*Gam323*invg12*invg33 + 16*Gam313*Gam333*invg13*invg33 + 
       8*Power(Gam323,2)*invg22*invg33 + 16*Gam323*Gam333*invg23*invg33 + 
       4*Power(Gam333,2)*Power(invg33,2) - 4*gamma0*ndua3*trGam3 - 
       4*(Gam033*invg00 + Gam133*invg01 + Gam233*invg02 + Gam333*invg03)*
        H0[ijk] - 4*Gam033*invg01*H1[ijk] - 4*Gam133*invg11*H1[ijk] - 
       4*Gam233*invg12*H1[ijk] - 4*Gam333*invg13*H1[ijk] - 
       4*Gam033*invg02*H2[ijk] - 4*Gam133*invg12*H2[ijk] - 
       4*Gam233*invg22*H2[ijk] - 4*Gam333*invg23*H2[ijk] - 
       4*Gam033*invg03*H3[ijk] - 4*Gam133*invg13*H3[ijk] - 
       4*Gam233*invg23*H3[ijk] - 4*Gam333*invg33*H3[ijk] - 
       4*gamma0*ndua3*H3[ijk] + 
       2*gamma0*g33[ijk]*(nvec0*trGam0 + nvec1*trGam1 + nvec2*trGam2 + 
          nvec3*trGam3 + nvec0*H0[ijk] + nvec1*H1[ijk] + nvec2*H2[ijk] + 
          nvec3*H3[ijk]) - 4*invg00*invh00*Power(Phi003[ijk],2) - 
       8*invg01*invh00*Phi003[ijk]*Phi013[ijk] - 
       4*invg11*invh00*Power(Phi013[ijk],2) - 
       8*invg02*invh00*Phi003[ijk]*Phi023[ijk] - 
       8*invg12*invh00*Phi013[ijk]*Phi023[ijk] - 
       4*invg22*invh00*Power(Phi023[ijk],2) - 
       8*invg03*invh00*Phi003[ijk]*Phi033[ijk] - 
       8*invg13*invh00*Phi013[ijk]*Phi033[ijk] - 
       8*invg23*invh00*Phi023[ijk]*Phi033[ijk] - 
       4*invg33*invh00*Power(Phi033[ijk],2) - 
       8*invg00*invh01*Phi003[ijk]*Phi103[ijk] - 
       8*invg01*invh01*Phi013[ijk]*Phi103[ijk] - 
       8*invg02*invh01*Phi023[ijk]*Phi103[ijk] - 
       8*invg03*invh01*Phi033[ijk]*Phi103[ijk] - 
       4*invg00*invh11*Power(Phi103[ijk],2) - 
       8*invg01*invh01*Phi003[ijk]*Phi113[ijk] - 
       8*invg11*invh01*Phi013[ijk]*Phi113[ijk] - 
       8*invg12*invh01*Phi023[ijk]*Phi113[ijk] - 
       8*invg13*invh01*Phi033[ijk]*Phi113[ijk] - 
       8*invg01*invh11*Phi103[ijk]*Phi113[ijk] - 
       4*invg11*invh11*Power(Phi113[ijk],2) - 
       8*invg02*invh01*Phi003[ijk]*Phi123[ijk] - 
       8*invg12*invh01*Phi013[ijk]*Phi123[ijk] - 
       8*invg22*invh01*Phi023[ijk]*Phi123[ijk] - 
       8*invg23*invh01*Phi033[ijk]*Phi123[ijk] - 
       8*invg02*invh11*Phi103[ijk]*Phi123[ijk] - 
       8*invg12*invh11*Phi113[ijk]*Phi123[ijk] - 
       4*invg22*invh11*Power(Phi123[ijk],2) - 
       8*invg03*invh01*Phi003[ijk]*Phi133[ijk] - 
       8*invg13*invh01*Phi013[ijk]*Phi133[ijk] - 
       8*invg23*invh01*Phi023[ijk]*Phi133[ijk] - 
       8*invg33*invh01*Phi033[ijk]*Phi133[ijk] - 
       8*invg03*invh11*Phi103[ijk]*Phi133[ijk] - 
       8*invg13*invh11*Phi113[ijk]*Phi133[ijk] - 
       8*invg23*invh11*Phi123[ijk]*Phi133[ijk] - 
       4*invg33*invh11*Power(Phi133[ijk],2) - 
       8*invg00*invh02*Phi003[ijk]*Phi203[ijk] - 
       8*invg01*invh02*Phi013[ijk]*Phi203[ijk] - 
       8*invg02*invh02*Phi023[ijk]*Phi203[ijk] - 
       8*invg03*invh02*Phi033[ijk]*Phi203[ijk] - 
       8*invg00*invh12*Phi103[ijk]*Phi203[ijk] - 
       8*invg01*invh12*Phi113[ijk]*Phi203[ijk] - 
       8*invg02*invh12*Phi123[ijk]*Phi203[ijk] - 
       8*invg03*invh12*Phi133[ijk]*Phi203[ijk] - 
       4*invg00*invh22*Power(Phi203[ijk],2) - 
       8*invg01*invh02*Phi003[ijk]*Phi213[ijk] - 
       8*invg11*invh02*Phi013[ijk]*Phi213[ijk] - 
       8*invg12*invh02*Phi023[ijk]*Phi213[ijk] - 
       8*invg13*invh02*Phi033[ijk]*Phi213[ijk] - 
       8*invg01*invh12*Phi103[ijk]*Phi213[ijk] - 
       8*invg11*invh12*Phi113[ijk]*Phi213[ijk] - 
       8*invg12*invh12*Phi123[ijk]*Phi213[ijk] - 
       8*invg13*invh12*Phi133[ijk]*Phi213[ijk] - 
       8*invg01*invh22*Phi203[ijk]*Phi213[ijk] - 
       4*invg11*invh22*Power(Phi213[ijk],2) - 
       8*invg02*invh02*Phi003[ijk]*Phi223[ijk] - 
       8*invg12*invh02*Phi013[ijk]*Phi223[ijk] - 
       8*invg22*invh02*Phi023[ijk]*Phi223[ijk] - 
       8*invg23*invh02*Phi033[ijk]*Phi223[ijk] - 
       8*invg02*invh12*Phi103[ijk]*Phi223[ijk] - 
       8*invg12*invh12*Phi113[ijk]*Phi223[ijk] - 
       8*invg22*invh12*Phi123[ijk]*Phi223[ijk] - 
       8*invg23*invh12*Phi133[ijk]*Phi223[ijk] - 
       8*invg02*invh22*Phi203[ijk]*Phi223[ijk] - 
       8*invg12*invh22*Phi213[ijk]*Phi223[ijk] - 
       4*invg22*invh22*Power(Phi223[ijk],2) - 
       8*invg03*invh02*Phi003[ijk]*Phi233[ijk] - 
       8*invg13*invh02*Phi013[ijk]*Phi233[ijk] - 
       8*invg23*invh02*Phi023[ijk]*Phi233[ijk] - 
       8*invg33*invh02*Phi033[ijk]*Phi233[ijk] - 
       8*invg03*invh12*Phi103[ijk]*Phi233[ijk] - 
       8*invg13*invh12*Phi113[ijk]*Phi233[ijk] - 
       8*invg23*invh12*Phi123[ijk]*Phi233[ijk] - 
       8*invg33*invh12*Phi133[ijk]*Phi233[ijk] - 
       8*invg03*invh22*Phi203[ijk]*Phi233[ijk] - 
       8*invg13*invh22*Phi213[ijk]*Phi233[ijk] - 
       8*invg23*invh22*Phi223[ijk]*Phi233[ijk] - 
       4*invg33*invh22*Power(Phi233[ijk],2) - 
       8*invg00*invh03*Phi003[ijk]*Phi303[ijk] - 
       8*invg01*invh03*Phi013[ijk]*Phi303[ijk] - 
       8*invg02*invh03*Phi023[ijk]*Phi303[ijk] - 
       8*invg03*invh03*Phi033[ijk]*Phi303[ijk] - 
       8*invg00*invh13*Phi103[ijk]*Phi303[ijk] - 
       8*invg01*invh13*Phi113[ijk]*Phi303[ijk] - 
       8*invg02*invh13*Phi123[ijk]*Phi303[ijk] - 
       8*invg03*invh13*Phi133[ijk]*Phi303[ijk] - 
       8*invg00*invh23*Phi203[ijk]*Phi303[ijk] - 
       8*invg01*invh23*Phi213[ijk]*Phi303[ijk] - 
       8*invg02*invh23*Phi223[ijk]*Phi303[ijk] - 
       8*invg03*invh23*Phi233[ijk]*Phi303[ijk] - 
       4*invg00*invh33*Power(Phi303[ijk],2) - 
       8*invg01*invh03*Phi003[ijk]*Phi313[ijk] - 
       8*invg11*invh03*Phi013[ijk]*Phi313[ijk] - 
       8*invg12*invh03*Phi023[ijk]*Phi313[ijk] - 
       8*invg13*invh03*Phi033[ijk]*Phi313[ijk] - 
       8*invg01*invh13*Phi103[ijk]*Phi313[ijk] - 
       8*invg11*invh13*Phi113[ijk]*Phi313[ijk] - 
       8*invg12*invh13*Phi123[ijk]*Phi313[ijk] - 
       8*invg13*invh13*Phi133[ijk]*Phi313[ijk] - 
       8*invg01*invh23*Phi203[ijk]*Phi313[ijk] - 
       8*invg11*invh23*Phi213[ijk]*Phi313[ijk] - 
       8*invg12*invh23*Phi223[ijk]*Phi313[ijk] - 
       8*invg13*invh23*Phi233[ijk]*Phi313[ijk] - 
       8*invg01*invh33*Phi303[ijk]*Phi313[ijk] - 
       4*invg11*invh33*Power(Phi313[ijk],2) - 
       8*invg02*invh03*Phi003[ijk]*Phi323[ijk] - 
       8*invg12*invh03*Phi013[ijk]*Phi323[ijk] - 
       8*invg22*invh03*Phi023[ijk]*Phi323[ijk] - 
       8*invg23*invh03*Phi033[ijk]*Phi323[ijk] - 
       8*invg02*invh13*Phi103[ijk]*Phi323[ijk] - 
       8*invg12*invh13*Phi113[ijk]*Phi323[ijk] - 
       8*invg22*invh13*Phi123[ijk]*Phi323[ijk] - 
       8*invg23*invh13*Phi133[ijk]*Phi323[ijk] - 
       8*invg02*invh23*Phi203[ijk]*Phi323[ijk] - 
       8*invg12*invh23*Phi213[ijk]*Phi323[ijk] - 
       8*invg22*invh23*Phi223[ijk]*Phi323[ijk] - 
       8*invg23*invh23*Phi233[ijk]*Phi323[ijk] - 
       8*invg02*invh33*Phi303[ijk]*Phi323[ijk] - 
       8*invg12*invh33*Phi313[ijk]*Phi323[ijk] - 
       4*invg22*invh33*Power(Phi323[ijk],2) - 
       8*invg03*invh03*Phi003[ijk]*Phi333[ijk] - 
       8*invg13*invh03*Phi013[ijk]*Phi333[ijk] - 
       8*invg23*invh03*Phi023[ijk]*Phi333[ijk] - 
       8*invg33*invh03*Phi033[ijk]*Phi333[ijk] - 
       8*invg03*invh13*Phi103[ijk]*Phi333[ijk] - 
       8*invg13*invh13*Phi113[ijk]*Phi333[ijk] - 
       8*invg23*invh13*Phi123[ijk]*Phi333[ijk] - 
       8*invg33*invh13*Phi133[ijk]*Phi333[ijk] - 
       8*invg03*invh23*Phi203[ijk]*Phi333[ijk] - 
       8*invg13*invh23*Phi213[ijk]*Phi333[ijk] - 
       8*invg23*invh23*Phi223[ijk]*Phi333[ijk] - 
       8*invg33*invh23*Phi233[ijk]*Phi333[ijk] - 
       8*invg03*invh33*Phi303[ijk]*Phi333[ijk] - 
       8*invg13*invh33*Phi313[ijk]*Phi333[ijk] - 
       8*invg23*invh33*Phi323[ijk]*Phi333[ijk] - 
       4*invg33*invh33*Power(Phi333[ijk],2) + 
       2*invh00*nvec0*Phi033[ijk]*Pi00[ijk] + 
       2*invh01*nvec0*Phi133[ijk]*Pi00[ijk] + 
       2*invh02*nvec0*Phi233[ijk]*Pi00[ijk] + 
       2*invh03*nvec0*Phi333[ijk]*Pi00[ijk] + 
       2*invh01*nvec0*Phi033[ijk]*Pi01[ijk] + 
       2*invh00*nvec1*Phi033[ijk]*Pi01[ijk] + 
       2*invh11*nvec0*Phi133[ijk]*Pi01[ijk] + 
       2*invh01*nvec1*Phi133[ijk]*Pi01[ijk] + 
       2*invh12*nvec0*Phi233[ijk]*Pi01[ijk] + 
       2*invh02*nvec1*Phi233[ijk]*Pi01[ijk] + 
       2*invh13*nvec0*Phi333[ijk]*Pi01[ijk] + 
       2*invh03*nvec1*Phi333[ijk]*Pi01[ijk] + 
       2*invh02*nvec0*Phi033[ijk]*Pi02[ijk] + 
       2*invh00*nvec2*Phi033[ijk]*Pi02[ijk] + 
       2*invh12*nvec0*Phi133[ijk]*Pi02[ijk] + 
       2*invh01*nvec2*Phi133[ijk]*Pi02[ijk] + 
       2*invh22*nvec0*Phi233[ijk]*Pi02[ijk] + 
       2*invh02*nvec2*Phi233[ijk]*Pi02[ijk] + 
       2*invh23*nvec0*Phi333[ijk]*Pi02[ijk] + 
       2*invh03*nvec2*Phi333[ijk]*Pi02[ijk] + 
       2*invh03*nvec0*Phi033[ijk]*Pi03[ijk] + 
       2*invh00*nvec3*Phi033[ijk]*Pi03[ijk] + 
       2*invh13*nvec0*Phi133[ijk]*Pi03[ijk] + 
       2*invh01*nvec3*Phi133[ijk]*Pi03[ijk] + 
       2*invh23*nvec0*Phi233[ijk]*Pi03[ijk] + 
       2*invh02*nvec3*Phi233[ijk]*Pi03[ijk] + 
       2*invh33*nvec0*Phi333[ijk]*Pi03[ijk] + 
       2*invh03*nvec3*Phi333[ijk]*Pi03[ijk] + 
       4*invg00*Power(Pi03[ijk],2) + 
       2*invh01*nvec1*Phi033[ijk]*Pi11[ijk] + 
       2*invh11*nvec1*Phi133[ijk]*Pi11[ijk] + 
       2*invh12*nvec1*Phi233[ijk]*Pi11[ijk] + 
       2*invh13*nvec1*Phi333[ijk]*Pi11[ijk] + 
       2*invh02*nvec1*Phi033[ijk]*Pi12[ijk] + 
       2*invh01*nvec2*Phi033[ijk]*Pi12[ijk] + 
       2*invh12*nvec1*Phi133[ijk]*Pi12[ijk] + 
       2*invh11*nvec2*Phi133[ijk]*Pi12[ijk] + 
       2*invh22*nvec1*Phi233[ijk]*Pi12[ijk] + 
       2*invh12*nvec2*Phi233[ijk]*Pi12[ijk] + 
       2*invh23*nvec1*Phi333[ijk]*Pi12[ijk] + 
       2*invh13*nvec2*Phi333[ijk]*Pi12[ijk] + 
       2*invh03*nvec1*Phi033[ijk]*Pi13[ijk] + 
       2*invh01*nvec3*Phi033[ijk]*Pi13[ijk] + 
       2*invh13*nvec1*Phi133[ijk]*Pi13[ijk] + 
       2*invh11*nvec3*Phi133[ijk]*Pi13[ijk] + 
       2*invh23*nvec1*Phi233[ijk]*Pi13[ijk] + 
       2*invh12*nvec3*Phi233[ijk]*Pi13[ijk] + 
       2*invh33*nvec1*Phi333[ijk]*Pi13[ijk] + 
       2*invh13*nvec3*Phi333[ijk]*Pi13[ijk] + 
       8*invg01*Pi03[ijk]*Pi13[ijk] + 4*invg11*Power(Pi13[ijk],2) + 
       2*invh02*nvec2*Phi033[ijk]*Pi22[ijk] + 
       2*invh12*nvec2*Phi133[ijk]*Pi22[ijk] + 
       2*invh22*nvec2*Phi233[ijk]*Pi22[ijk] + 
       2*invh23*nvec2*Phi333[ijk]*Pi22[ijk] + 
       2*invh03*nvec2*Phi033[ijk]*Pi23[ijk] + 
       2*invh02*nvec3*Phi033[ijk]*Pi23[ijk] + 
       2*invh13*nvec2*Phi133[ijk]*Pi23[ijk] + 
       2*invh12*nvec3*Phi133[ijk]*Pi23[ijk] + 
       2*invh23*nvec2*Phi233[ijk]*Pi23[ijk] + 
       2*invh22*nvec3*Phi233[ijk]*Pi23[ijk] + 
       2*invh33*nvec2*Phi333[ijk]*Pi23[ijk] + 
       2*invh23*nvec3*Phi333[ijk]*Pi23[ijk] + 
       8*invg02*Pi03[ijk]*Pi23[ijk] + 8*invg12*Pi13[ijk]*Pi23[ijk] + 
       4*invg22*Power(Pi23[ijk],2) + 
       2*invh03*nvec3*Phi033[ijk]*Pi33[ijk] + 
       2*invh13*nvec3*Phi133[ijk]*Pi33[ijk] + 
       2*invh23*nvec3*Phi233[ijk]*Pi33[ijk] + 
       2*invh33*nvec3*Phi333[ijk]*Pi33[ijk] + 
       Power(nvec0,2)*Pi00[ijk]*Pi33[ijk] + 
       2*nvec0*nvec1*Pi01[ijk]*Pi33[ijk] + 
       2*nvec0*nvec2*Pi02[ijk]*Pi33[ijk] + 8*invg03*Pi03[ijk]*Pi33[ijk] + 
       2*nvec0*nvec3*Pi03[ijk]*Pi33[ijk] + 
       Power(nvec1,2)*Pi11[ijk]*Pi33[ijk] + 
       2*nvec1*nvec2*Pi12[ijk]*Pi33[ijk] + 8*invg13*Pi13[ijk]*Pi33[ijk] + 
       2*nvec1*nvec3*Pi13[ijk]*Pi33[ijk] + 
       Power(nvec2,2)*Pi22[ijk]*Pi33[ijk] + 8*invg23*Pi23[ijk]*Pi33[ijk] + 
       2*nvec2*nvec3*Pi23[ijk]*Pi33[ijk] + 4*invg33*Power(Pi33[ijk],2) + 
       Power(nvec3,2)*Power(Pi33[ijk],2)))/2. - srcSdH33[ijk]
;

dtPhi000[ijk]
=
-(interior*AdPhi000[ijk]) + (alpha[ijk]*
     (2*invh00*nvec0*Power(Phi000[ijk],2) + 
       2*invh12*nvec0*Phi002[ijk]*Phi100[ijk] + 
       2*invh01*nvec2*Phi002[ijk]*Phi100[ijk] + 
       2*invh13*nvec0*Phi003[ijk]*Phi100[ijk] + 
       2*invh01*nvec3*Phi003[ijk]*Phi100[ijk] + 
       2*invh11*nvec1*Phi011[ijk]*Phi100[ijk] + 
       2*invh12*nvec1*Phi012[ijk]*Phi100[ijk] + 
       2*invh11*nvec2*Phi012[ijk]*Phi100[ijk] + 
       2*invh13*nvec1*Phi013[ijk]*Phi100[ijk] + 
       2*invh11*nvec3*Phi013[ijk]*Phi100[ijk] + 
       2*invh12*nvec2*Phi022[ijk]*Phi100[ijk] + 
       2*invh13*nvec2*Phi023[ijk]*Phi100[ijk] + 
       2*invh12*nvec3*Phi023[ijk]*Phi100[ijk] + 
       2*invh13*nvec3*Phi033[ijk]*Phi100[ijk] + 
       2*invh22*nvec0*Phi002[ijk]*Phi200[ijk] + 
       2*invh02*nvec2*Phi002[ijk]*Phi200[ijk] + 
       2*invh23*nvec0*Phi003[ijk]*Phi200[ijk] + 
       2*invh02*nvec3*Phi003[ijk]*Phi200[ijk] + 
       2*invh12*nvec1*Phi011[ijk]*Phi200[ijk] + 
       2*invh22*nvec1*Phi012[ijk]*Phi200[ijk] + 
       2*invh12*nvec2*Phi012[ijk]*Phi200[ijk] + 
       2*invh23*nvec1*Phi013[ijk]*Phi200[ijk] + 
       2*invh12*nvec3*Phi013[ijk]*Phi200[ijk] + 
       2*invh22*nvec2*Phi022[ijk]*Phi200[ijk] + 
       2*invh23*nvec2*Phi023[ijk]*Phi200[ijk] + 
       2*invh22*nvec3*Phi023[ijk]*Phi200[ijk] + 
       2*invh23*nvec3*Phi033[ijk]*Phi200[ijk] + 
       2*invh23*nvec0*Phi002[ijk]*Phi300[ijk] + 
       2*invh03*nvec2*Phi002[ijk]*Phi300[ijk] + 
       2*invh33*nvec0*Phi003[ijk]*Phi300[ijk] + 
       2*invh03*nvec3*Phi003[ijk]*Phi300[ijk] + 
       2*invh13*nvec1*Phi011[ijk]*Phi300[ijk] + 
       2*invh23*nvec1*Phi012[ijk]*Phi300[ijk] + 
       2*invh13*nvec2*Phi012[ijk]*Phi300[ijk] + 
       2*invh33*nvec1*Phi013[ijk]*Phi300[ijk] + 
       2*invh13*nvec3*Phi013[ijk]*Phi300[ijk] + 
       2*invh23*nvec2*Phi022[ijk]*Phi300[ijk] + 
       2*invh33*nvec2*Phi023[ijk]*Phi300[ijk] + 
       2*invh23*nvec3*Phi023[ijk]*Phi300[ijk] + 
       2*invh33*nvec3*Phi033[ijk]*Phi300[ijk] + 
       2*nvec0*nvec2*Phi002[ijk]*Pi00[ijk] + 
       2*nvec0*nvec3*Phi003[ijk]*Pi00[ijk] + 
       Power(nvec1,2)*Phi011[ijk]*Pi00[ijk] + 
       2*nvec1*nvec2*Phi012[ijk]*Pi00[ijk] + 
       2*nvec1*nvec3*Phi013[ijk]*Pi00[ijk] + 
       Power(nvec2,2)*Phi022[ijk]*Pi00[ijk] + 
       2*nvec2*nvec3*Phi023[ijk]*Pi00[ijk] + 
       Power(nvec3,2)*Phi033[ijk]*Pi00[ijk] + 
       Phi000[ijk]*(-2*gamma2 + 
          2*(invh01*nvec0 + invh00*nvec1)*Phi001[ijk] + 
          2*(invh02*nvec0 + invh00*nvec2)*Phi002[ijk] + 
          2*invh03*nvec0*Phi003[ijk] + 2*invh00*nvec3*Phi003[ijk] + 
          2*invh01*nvec1*Phi011[ijk] + 2*invh02*nvec1*Phi012[ijk] + 
          2*invh01*nvec2*Phi012[ijk] + 2*invh03*nvec1*Phi013[ijk] + 
          2*invh01*nvec3*Phi013[ijk] + 2*invh02*nvec2*Phi022[ijk] + 
          2*invh03*nvec2*Phi023[ijk] + 2*invh02*nvec3*Phi023[ijk] + 
          2*invh03*nvec3*Phi033[ijk] + 2*invh01*nvec0*Phi100[ijk] + 
          2*invh02*nvec0*Phi200[ijk] + 2*invh03*nvec0*Phi300[ijk] + 
          Power(nvec0,2)*Pi00[ijk]) + 
       2*Phi001[ijk]*((invh11*nvec0 + invh01*nvec1)*Phi100[ijk] + 
          (invh12*nvec0 + invh02*nvec1)*Phi200[ijk] + 
          invh13*nvec0*Phi300[ijk] + invh03*nvec1*Phi300[ijk] + 
          nvec0*nvec1*Pi00[ijk])))/2.
;

dtPhi001[ijk]
=
-(interior*AdPhi001[ijk]) + (alpha[ijk]*
     (2*(invh01*nvec0 + invh00*nvec1)*Power(Phi001[ijk],2) + 
       2*invh12*nvec0*Phi002[ijk]*Phi101[ijk] + 
       2*invh01*nvec2*Phi002[ijk]*Phi101[ijk] + 
       2*invh13*nvec0*Phi003[ijk]*Phi101[ijk] + 
       2*invh01*nvec3*Phi003[ijk]*Phi101[ijk] + 
       2*invh11*nvec1*Phi011[ijk]*Phi101[ijk] + 
       2*invh12*nvec1*Phi012[ijk]*Phi101[ijk] + 
       2*invh11*nvec2*Phi012[ijk]*Phi101[ijk] + 
       2*invh13*nvec1*Phi013[ijk]*Phi101[ijk] + 
       2*invh11*nvec3*Phi013[ijk]*Phi101[ijk] + 
       2*invh12*nvec2*Phi022[ijk]*Phi101[ijk] + 
       2*invh13*nvec2*Phi023[ijk]*Phi101[ijk] + 
       2*invh12*nvec3*Phi023[ijk]*Phi101[ijk] + 
       2*invh13*nvec3*Phi033[ijk]*Phi101[ijk] + 
       2*invh22*nvec0*Phi002[ijk]*Phi201[ijk] + 
       2*invh02*nvec2*Phi002[ijk]*Phi201[ijk] + 
       2*invh23*nvec0*Phi003[ijk]*Phi201[ijk] + 
       2*invh02*nvec3*Phi003[ijk]*Phi201[ijk] + 
       2*invh12*nvec1*Phi011[ijk]*Phi201[ijk] + 
       2*invh22*nvec1*Phi012[ijk]*Phi201[ijk] + 
       2*invh12*nvec2*Phi012[ijk]*Phi201[ijk] + 
       2*invh23*nvec1*Phi013[ijk]*Phi201[ijk] + 
       2*invh12*nvec3*Phi013[ijk]*Phi201[ijk] + 
       2*invh22*nvec2*Phi022[ijk]*Phi201[ijk] + 
       2*invh23*nvec2*Phi023[ijk]*Phi201[ijk] + 
       2*invh22*nvec3*Phi023[ijk]*Phi201[ijk] + 
       2*invh23*nvec3*Phi033[ijk]*Phi201[ijk] + 
       2*invh23*nvec0*Phi002[ijk]*Phi301[ijk] + 
       2*invh03*nvec2*Phi002[ijk]*Phi301[ijk] + 
       2*invh33*nvec0*Phi003[ijk]*Phi301[ijk] + 
       2*invh03*nvec3*Phi003[ijk]*Phi301[ijk] + 
       2*invh13*nvec1*Phi011[ijk]*Phi301[ijk] + 
       2*invh23*nvec1*Phi012[ijk]*Phi301[ijk] + 
       2*invh13*nvec2*Phi012[ijk]*Phi301[ijk] + 
       2*invh33*nvec1*Phi013[ijk]*Phi301[ijk] + 
       2*invh13*nvec3*Phi013[ijk]*Phi301[ijk] + 
       2*invh23*nvec2*Phi022[ijk]*Phi301[ijk] + 
       2*invh33*nvec2*Phi023[ijk]*Phi301[ijk] + 
       2*invh23*nvec3*Phi023[ijk]*Phi301[ijk] + 
       2*invh33*nvec3*Phi033[ijk]*Phi301[ijk] + 
       2*nvec0*nvec2*Phi002[ijk]*Pi01[ijk] + 
       2*nvec0*nvec3*Phi003[ijk]*Pi01[ijk] + 
       Power(nvec1,2)*Phi011[ijk]*Pi01[ijk] + 
       2*nvec1*nvec2*Phi012[ijk]*Pi01[ijk] + 
       2*nvec1*nvec3*Phi013[ijk]*Pi01[ijk] + 
       Power(nvec2,2)*Phi022[ijk]*Pi01[ijk] + 
       2*nvec2*nvec3*Phi023[ijk]*Pi01[ijk] + 
       Power(nvec3,2)*Phi033[ijk]*Pi01[ijk] + 
       nvec0*Phi000[ijk]*(2*invh01*Phi101[ijk] + 2*invh02*Phi201[ijk] + 
          2*invh03*Phi301[ijk] + nvec0*Pi01[ijk]) + 
       2*Phi001[ijk]*(-gamma2 + invh00*nvec0*Phi000[ijk] + 
          (invh02*nvec0 + invh00*nvec2)*Phi002[ijk] + 
          invh03*nvec0*Phi003[ijk] + invh00*nvec3*Phi003[ijk] + 
          invh01*nvec1*Phi011[ijk] + invh02*nvec1*Phi012[ijk] + 
          invh01*nvec2*Phi012[ijk] + invh03*nvec1*Phi013[ijk] + 
          invh01*nvec3*Phi013[ijk] + invh02*nvec2*Phi022[ijk] + 
          invh03*nvec2*Phi023[ijk] + invh02*nvec3*Phi023[ijk] + 
          invh03*nvec3*Phi033[ijk] + invh11*nvec0*Phi101[ijk] + 
          invh01*nvec1*Phi101[ijk] + invh12*nvec0*Phi201[ijk] + 
          invh02*nvec1*Phi201[ijk] + invh13*nvec0*Phi301[ijk] + 
          invh03*nvec1*Phi301[ijk] + nvec0*nvec1*Pi01[ijk])))/2.
;

dtPhi002[ijk]
=
-(interior*AdPhi002[ijk]) + (alpha[ijk]*
     (2*(invh02*nvec0 + invh00*nvec2)*Power(Phi002[ijk],2) + 
       2*invh11*nvec0*Phi001[ijk]*Phi102[ijk] + 
       2*invh01*nvec1*Phi001[ijk]*Phi102[ijk] + 
       2*invh13*nvec0*Phi003[ijk]*Phi102[ijk] + 
       2*invh01*nvec3*Phi003[ijk]*Phi102[ijk] + 
       2*invh11*nvec1*Phi011[ijk]*Phi102[ijk] + 
       2*invh12*nvec1*Phi012[ijk]*Phi102[ijk] + 
       2*invh11*nvec2*Phi012[ijk]*Phi102[ijk] + 
       2*invh13*nvec1*Phi013[ijk]*Phi102[ijk] + 
       2*invh11*nvec3*Phi013[ijk]*Phi102[ijk] + 
       2*invh12*nvec2*Phi022[ijk]*Phi102[ijk] + 
       2*invh13*nvec2*Phi023[ijk]*Phi102[ijk] + 
       2*invh12*nvec3*Phi023[ijk]*Phi102[ijk] + 
       2*invh13*nvec3*Phi033[ijk]*Phi102[ijk] + 
       2*invh12*nvec0*Phi001[ijk]*Phi202[ijk] + 
       2*invh02*nvec1*Phi001[ijk]*Phi202[ijk] + 
       2*invh23*nvec0*Phi003[ijk]*Phi202[ijk] + 
       2*invh02*nvec3*Phi003[ijk]*Phi202[ijk] + 
       2*invh12*nvec1*Phi011[ijk]*Phi202[ijk] + 
       2*invh22*nvec1*Phi012[ijk]*Phi202[ijk] + 
       2*invh12*nvec2*Phi012[ijk]*Phi202[ijk] + 
       2*invh23*nvec1*Phi013[ijk]*Phi202[ijk] + 
       2*invh12*nvec3*Phi013[ijk]*Phi202[ijk] + 
       2*invh22*nvec2*Phi022[ijk]*Phi202[ijk] + 
       2*invh23*nvec2*Phi023[ijk]*Phi202[ijk] + 
       2*invh22*nvec3*Phi023[ijk]*Phi202[ijk] + 
       2*invh23*nvec3*Phi033[ijk]*Phi202[ijk] + 
       2*invh13*nvec0*Phi001[ijk]*Phi302[ijk] + 
       2*invh03*nvec1*Phi001[ijk]*Phi302[ijk] + 
       2*invh33*nvec0*Phi003[ijk]*Phi302[ijk] + 
       2*invh03*nvec3*Phi003[ijk]*Phi302[ijk] + 
       2*invh13*nvec1*Phi011[ijk]*Phi302[ijk] + 
       2*invh23*nvec1*Phi012[ijk]*Phi302[ijk] + 
       2*invh13*nvec2*Phi012[ijk]*Phi302[ijk] + 
       2*invh33*nvec1*Phi013[ijk]*Phi302[ijk] + 
       2*invh13*nvec3*Phi013[ijk]*Phi302[ijk] + 
       2*invh23*nvec2*Phi022[ijk]*Phi302[ijk] + 
       2*invh33*nvec2*Phi023[ijk]*Phi302[ijk] + 
       2*invh23*nvec3*Phi023[ijk]*Phi302[ijk] + 
       2*invh33*nvec3*Phi033[ijk]*Phi302[ijk] + 
       2*nvec0*nvec1*Phi001[ijk]*Pi02[ijk] + 
       2*nvec0*nvec3*Phi003[ijk]*Pi02[ijk] + 
       Power(nvec1,2)*Phi011[ijk]*Pi02[ijk] + 
       2*nvec1*nvec2*Phi012[ijk]*Pi02[ijk] + 
       2*nvec1*nvec3*Phi013[ijk]*Pi02[ijk] + 
       Power(nvec2,2)*Phi022[ijk]*Pi02[ijk] + 
       2*nvec2*nvec3*Phi023[ijk]*Pi02[ijk] + 
       Power(nvec3,2)*Phi033[ijk]*Pi02[ijk] + 
       nvec0*Phi000[ijk]*(2*invh01*Phi102[ijk] + 2*invh02*Phi202[ijk] + 
          2*invh03*Phi302[ijk] + nvec0*Pi02[ijk]) + 
       2*Phi002[ijk]*(-gamma2 + invh00*nvec0*Phi000[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi001[ijk] + 
          invh03*nvec0*Phi003[ijk] + invh00*nvec3*Phi003[ijk] + 
          invh01*nvec1*Phi011[ijk] + invh02*nvec1*Phi012[ijk] + 
          invh01*nvec2*Phi012[ijk] + invh03*nvec1*Phi013[ijk] + 
          invh01*nvec3*Phi013[ijk] + invh02*nvec2*Phi022[ijk] + 
          invh03*nvec2*Phi023[ijk] + invh02*nvec3*Phi023[ijk] + 
          invh03*nvec3*Phi033[ijk] + invh12*nvec0*Phi102[ijk] + 
          invh01*nvec2*Phi102[ijk] + invh22*nvec0*Phi202[ijk] + 
          invh02*nvec2*Phi202[ijk] + invh23*nvec0*Phi302[ijk] + 
          invh03*nvec2*Phi302[ijk] + nvec0*nvec2*Pi02[ijk])))/2.
;

dtPhi003[ijk]
=
-(interior*AdPhi003[ijk]) + (alpha[ijk]*
     (2*(invh03*nvec0 + invh00*nvec3)*Power(Phi003[ijk],2) + 
       2*invh11*nvec0*Phi001[ijk]*Phi103[ijk] + 
       2*invh01*nvec1*Phi001[ijk]*Phi103[ijk] + 
       2*invh12*nvec0*Phi002[ijk]*Phi103[ijk] + 
       2*invh01*nvec2*Phi002[ijk]*Phi103[ijk] + 
       2*invh11*nvec1*Phi011[ijk]*Phi103[ijk] + 
       2*invh12*nvec1*Phi012[ijk]*Phi103[ijk] + 
       2*invh11*nvec2*Phi012[ijk]*Phi103[ijk] + 
       2*invh13*nvec1*Phi013[ijk]*Phi103[ijk] + 
       2*invh11*nvec3*Phi013[ijk]*Phi103[ijk] + 
       2*invh12*nvec2*Phi022[ijk]*Phi103[ijk] + 
       2*invh13*nvec2*Phi023[ijk]*Phi103[ijk] + 
       2*invh12*nvec3*Phi023[ijk]*Phi103[ijk] + 
       2*invh13*nvec3*Phi033[ijk]*Phi103[ijk] + 
       2*invh12*nvec0*Phi001[ijk]*Phi203[ijk] + 
       2*invh02*nvec1*Phi001[ijk]*Phi203[ijk] + 
       2*invh22*nvec0*Phi002[ijk]*Phi203[ijk] + 
       2*invh02*nvec2*Phi002[ijk]*Phi203[ijk] + 
       2*invh12*nvec1*Phi011[ijk]*Phi203[ijk] + 
       2*invh22*nvec1*Phi012[ijk]*Phi203[ijk] + 
       2*invh12*nvec2*Phi012[ijk]*Phi203[ijk] + 
       2*invh23*nvec1*Phi013[ijk]*Phi203[ijk] + 
       2*invh12*nvec3*Phi013[ijk]*Phi203[ijk] + 
       2*invh22*nvec2*Phi022[ijk]*Phi203[ijk] + 
       2*invh23*nvec2*Phi023[ijk]*Phi203[ijk] + 
       2*invh22*nvec3*Phi023[ijk]*Phi203[ijk] + 
       2*invh23*nvec3*Phi033[ijk]*Phi203[ijk] + 
       2*invh13*nvec0*Phi001[ijk]*Phi303[ijk] + 
       2*invh03*nvec1*Phi001[ijk]*Phi303[ijk] + 
       2*invh23*nvec0*Phi002[ijk]*Phi303[ijk] + 
       2*invh03*nvec2*Phi002[ijk]*Phi303[ijk] + 
       2*invh13*nvec1*Phi011[ijk]*Phi303[ijk] + 
       2*invh23*nvec1*Phi012[ijk]*Phi303[ijk] + 
       2*invh13*nvec2*Phi012[ijk]*Phi303[ijk] + 
       2*invh33*nvec1*Phi013[ijk]*Phi303[ijk] + 
       2*invh13*nvec3*Phi013[ijk]*Phi303[ijk] + 
       2*invh23*nvec2*Phi022[ijk]*Phi303[ijk] + 
       2*invh33*nvec2*Phi023[ijk]*Phi303[ijk] + 
       2*invh23*nvec3*Phi023[ijk]*Phi303[ijk] + 
       2*invh33*nvec3*Phi033[ijk]*Phi303[ijk] + 
       2*nvec0*nvec1*Phi001[ijk]*Pi03[ijk] + 
       2*nvec0*nvec2*Phi002[ijk]*Pi03[ijk] + 
       Power(nvec1,2)*Phi011[ijk]*Pi03[ijk] + 
       2*nvec1*nvec2*Phi012[ijk]*Pi03[ijk] + 
       2*nvec1*nvec3*Phi013[ijk]*Pi03[ijk] + 
       Power(nvec2,2)*Phi022[ijk]*Pi03[ijk] + 
       2*nvec2*nvec3*Phi023[ijk]*Pi03[ijk] + 
       Power(nvec3,2)*Phi033[ijk]*Pi03[ijk] + 
       nvec0*Phi000[ijk]*(2*invh01*Phi103[ijk] + 2*invh02*Phi203[ijk] + 
          2*invh03*Phi303[ijk] + nvec0*Pi03[ijk]) + 
       2*Phi003[ijk]*(-gamma2 + invh00*nvec0*Phi000[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi001[ijk] + 
          invh02*nvec0*Phi002[ijk] + invh00*nvec2*Phi002[ijk] + 
          invh01*nvec1*Phi011[ijk] + invh02*nvec1*Phi012[ijk] + 
          invh01*nvec2*Phi012[ijk] + invh03*nvec1*Phi013[ijk] + 
          invh01*nvec3*Phi013[ijk] + invh02*nvec2*Phi022[ijk] + 
          invh03*nvec2*Phi023[ijk] + invh02*nvec3*Phi023[ijk] + 
          invh03*nvec3*Phi033[ijk] + invh13*nvec0*Phi103[ijk] + 
          invh01*nvec3*Phi103[ijk] + invh23*nvec0*Phi203[ijk] + 
          invh02*nvec3*Phi203[ijk] + invh33*nvec0*Phi303[ijk] + 
          invh03*nvec3*Phi303[ijk] + nvec0*nvec3*Pi03[ijk])))/2.
;

dtPhi011[ijk]
=
-(interior*AdPhi011[ijk]) + (alpha[ijk]*
     (2*invh01*nvec1*Power(Phi011[ijk],2) + 
       2*invh11*nvec0*Phi001[ijk]*Phi111[ijk] + 
       2*invh01*nvec1*Phi001[ijk]*Phi111[ijk] + 
       2*invh12*nvec0*Phi002[ijk]*Phi111[ijk] + 
       2*invh01*nvec2*Phi002[ijk]*Phi111[ijk] + 
       2*invh13*nvec0*Phi003[ijk]*Phi111[ijk] + 
       2*invh01*nvec3*Phi003[ijk]*Phi111[ijk] + 
       2*invh12*nvec1*Phi012[ijk]*Phi111[ijk] + 
       2*invh11*nvec2*Phi012[ijk]*Phi111[ijk] + 
       2*invh13*nvec1*Phi013[ijk]*Phi111[ijk] + 
       2*invh11*nvec3*Phi013[ijk]*Phi111[ijk] + 
       2*invh12*nvec2*Phi022[ijk]*Phi111[ijk] + 
       2*invh13*nvec2*Phi023[ijk]*Phi111[ijk] + 
       2*invh12*nvec3*Phi023[ijk]*Phi111[ijk] + 
       2*invh13*nvec3*Phi033[ijk]*Phi111[ijk] + 
       2*invh12*nvec0*Phi001[ijk]*Phi211[ijk] + 
       2*invh02*nvec1*Phi001[ijk]*Phi211[ijk] + 
       2*invh22*nvec0*Phi002[ijk]*Phi211[ijk] + 
       2*invh02*nvec2*Phi002[ijk]*Phi211[ijk] + 
       2*invh23*nvec0*Phi003[ijk]*Phi211[ijk] + 
       2*invh02*nvec3*Phi003[ijk]*Phi211[ijk] + 
       2*invh22*nvec1*Phi012[ijk]*Phi211[ijk] + 
       2*invh12*nvec2*Phi012[ijk]*Phi211[ijk] + 
       2*invh23*nvec1*Phi013[ijk]*Phi211[ijk] + 
       2*invh12*nvec3*Phi013[ijk]*Phi211[ijk] + 
       2*invh22*nvec2*Phi022[ijk]*Phi211[ijk] + 
       2*invh23*nvec2*Phi023[ijk]*Phi211[ijk] + 
       2*invh22*nvec3*Phi023[ijk]*Phi211[ijk] + 
       2*invh23*nvec3*Phi033[ijk]*Phi211[ijk] + 
       2*invh13*nvec0*Phi001[ijk]*Phi311[ijk] + 
       2*invh03*nvec1*Phi001[ijk]*Phi311[ijk] + 
       2*invh23*nvec0*Phi002[ijk]*Phi311[ijk] + 
       2*invh03*nvec2*Phi002[ijk]*Phi311[ijk] + 
       2*invh33*nvec0*Phi003[ijk]*Phi311[ijk] + 
       2*invh03*nvec3*Phi003[ijk]*Phi311[ijk] + 
       2*invh23*nvec1*Phi012[ijk]*Phi311[ijk] + 
       2*invh13*nvec2*Phi012[ijk]*Phi311[ijk] + 
       2*invh33*nvec1*Phi013[ijk]*Phi311[ijk] + 
       2*invh13*nvec3*Phi013[ijk]*Phi311[ijk] + 
       2*invh23*nvec2*Phi022[ijk]*Phi311[ijk] + 
       2*invh33*nvec2*Phi023[ijk]*Phi311[ijk] + 
       2*invh23*nvec3*Phi023[ijk]*Phi311[ijk] + 
       2*invh33*nvec3*Phi033[ijk]*Phi311[ijk] + 
       2*nvec0*nvec1*Phi001[ijk]*Pi11[ijk] + 
       2*nvec0*nvec2*Phi002[ijk]*Pi11[ijk] + 
       2*nvec0*nvec3*Phi003[ijk]*Pi11[ijk] + 
       2*nvec1*nvec2*Phi012[ijk]*Pi11[ijk] + 
       2*nvec1*nvec3*Phi013[ijk]*Pi11[ijk] + 
       Power(nvec2,2)*Phi022[ijk]*Pi11[ijk] + 
       2*nvec2*nvec3*Phi023[ijk]*Pi11[ijk] + 
       Power(nvec3,2)*Phi033[ijk]*Pi11[ijk] + 
       nvec0*Phi000[ijk]*(2*invh01*Phi111[ijk] + 2*invh02*Phi211[ijk] + 
          2*invh03*Phi311[ijk] + nvec0*Pi11[ijk]) + 
       Phi011[ijk]*(-2*gamma2 + 2*invh00*nvec0*Phi000[ijk] + 
          2*(invh01*nvec0 + invh00*nvec1)*Phi001[ijk] + 
          2*invh02*nvec0*Phi002[ijk] + 2*invh00*nvec2*Phi002[ijk] + 
          2*invh03*nvec0*Phi003[ijk] + 2*invh00*nvec3*Phi003[ijk] + 
          2*invh02*nvec1*Phi012[ijk] + 2*invh01*nvec2*Phi012[ijk] + 
          2*invh03*nvec1*Phi013[ijk] + 2*invh01*nvec3*Phi013[ijk] + 
          2*invh02*nvec2*Phi022[ijk] + 2*invh03*nvec2*Phi023[ijk] + 
          2*invh02*nvec3*Phi023[ijk] + 2*invh03*nvec3*Phi033[ijk] + 
          2*invh11*nvec1*Phi111[ijk] + 2*invh12*nvec1*Phi211[ijk] + 
          2*invh13*nvec1*Phi311[ijk] + Power(nvec1,2)*Pi11[ijk])))/2.
;

dtPhi012[ijk]
=
-(interior*AdPhi012[ijk]) + (alpha[ijk]*
     (2*(invh02*nvec1 + invh01*nvec2)*Power(Phi012[ijk],2) + 
       2*invh11*nvec0*Phi001[ijk]*Phi112[ijk] + 
       2*invh01*nvec1*Phi001[ijk]*Phi112[ijk] + 
       2*invh12*nvec0*Phi002[ijk]*Phi112[ijk] + 
       2*invh01*nvec2*Phi002[ijk]*Phi112[ijk] + 
       2*invh13*nvec0*Phi003[ijk]*Phi112[ijk] + 
       2*invh01*nvec3*Phi003[ijk]*Phi112[ijk] + 
       2*invh11*nvec1*Phi011[ijk]*Phi112[ijk] + 
       2*invh13*nvec1*Phi013[ijk]*Phi112[ijk] + 
       2*invh11*nvec3*Phi013[ijk]*Phi112[ijk] + 
       2*invh12*nvec2*Phi022[ijk]*Phi112[ijk] + 
       2*invh13*nvec2*Phi023[ijk]*Phi112[ijk] + 
       2*invh12*nvec3*Phi023[ijk]*Phi112[ijk] + 
       2*invh13*nvec3*Phi033[ijk]*Phi112[ijk] + 
       2*invh12*nvec0*Phi001[ijk]*Phi212[ijk] + 
       2*invh02*nvec1*Phi001[ijk]*Phi212[ijk] + 
       2*invh22*nvec0*Phi002[ijk]*Phi212[ijk] + 
       2*invh02*nvec2*Phi002[ijk]*Phi212[ijk] + 
       2*invh23*nvec0*Phi003[ijk]*Phi212[ijk] + 
       2*invh02*nvec3*Phi003[ijk]*Phi212[ijk] + 
       2*invh12*nvec1*Phi011[ijk]*Phi212[ijk] + 
       2*invh23*nvec1*Phi013[ijk]*Phi212[ijk] + 
       2*invh12*nvec3*Phi013[ijk]*Phi212[ijk] + 
       2*invh22*nvec2*Phi022[ijk]*Phi212[ijk] + 
       2*invh23*nvec2*Phi023[ijk]*Phi212[ijk] + 
       2*invh22*nvec3*Phi023[ijk]*Phi212[ijk] + 
       2*invh23*nvec3*Phi033[ijk]*Phi212[ijk] + 
       2*invh13*nvec0*Phi001[ijk]*Phi312[ijk] + 
       2*invh03*nvec1*Phi001[ijk]*Phi312[ijk] + 
       2*invh23*nvec0*Phi002[ijk]*Phi312[ijk] + 
       2*invh03*nvec2*Phi002[ijk]*Phi312[ijk] + 
       2*invh33*nvec0*Phi003[ijk]*Phi312[ijk] + 
       2*invh03*nvec3*Phi003[ijk]*Phi312[ijk] + 
       2*invh13*nvec1*Phi011[ijk]*Phi312[ijk] + 
       2*invh33*nvec1*Phi013[ijk]*Phi312[ijk] + 
       2*invh13*nvec3*Phi013[ijk]*Phi312[ijk] + 
       2*invh23*nvec2*Phi022[ijk]*Phi312[ijk] + 
       2*invh33*nvec2*Phi023[ijk]*Phi312[ijk] + 
       2*invh23*nvec3*Phi023[ijk]*Phi312[ijk] + 
       2*invh33*nvec3*Phi033[ijk]*Phi312[ijk] + 
       2*nvec0*nvec1*Phi001[ijk]*Pi12[ijk] + 
       2*nvec0*nvec2*Phi002[ijk]*Pi12[ijk] + 
       2*nvec0*nvec3*Phi003[ijk]*Pi12[ijk] + 
       Power(nvec1,2)*Phi011[ijk]*Pi12[ijk] + 
       2*nvec1*nvec3*Phi013[ijk]*Pi12[ijk] + 
       Power(nvec2,2)*Phi022[ijk]*Pi12[ijk] + 
       2*nvec2*nvec3*Phi023[ijk]*Pi12[ijk] + 
       Power(nvec3,2)*Phi033[ijk]*Pi12[ijk] + 
       nvec0*Phi000[ijk]*(2*invh01*Phi112[ijk] + 2*invh02*Phi212[ijk] + 
          2*invh03*Phi312[ijk] + nvec0*Pi12[ijk]) + 
       2*Phi012[ijk]*(-gamma2 + invh00*nvec0*Phi000[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi001[ijk] + 
          invh02*nvec0*Phi002[ijk] + invh00*nvec2*Phi002[ijk] + 
          invh03*nvec0*Phi003[ijk] + invh00*nvec3*Phi003[ijk] + 
          invh01*nvec1*Phi011[ijk] + invh03*nvec1*Phi013[ijk] + 
          invh01*nvec3*Phi013[ijk] + invh02*nvec2*Phi022[ijk] + 
          invh03*nvec2*Phi023[ijk] + invh02*nvec3*Phi023[ijk] + 
          invh03*nvec3*Phi033[ijk] + invh12*nvec1*Phi112[ijk] + 
          invh11*nvec2*Phi112[ijk] + invh22*nvec1*Phi212[ijk] + 
          invh12*nvec2*Phi212[ijk] + invh23*nvec1*Phi312[ijk] + 
          invh13*nvec2*Phi312[ijk] + nvec1*nvec2*Pi12[ijk])))/2.
;

dtPhi013[ijk]
=
-(interior*AdPhi013[ijk]) + (alpha[ijk]*
     (2*(invh03*nvec1 + invh01*nvec3)*Power(Phi013[ijk],2) + 
       2*invh11*nvec0*Phi001[ijk]*Phi113[ijk] + 
       2*invh01*nvec1*Phi001[ijk]*Phi113[ijk] + 
       2*invh12*nvec0*Phi002[ijk]*Phi113[ijk] + 
       2*invh01*nvec2*Phi002[ijk]*Phi113[ijk] + 
       2*invh13*nvec0*Phi003[ijk]*Phi113[ijk] + 
       2*invh01*nvec3*Phi003[ijk]*Phi113[ijk] + 
       2*invh11*nvec1*Phi011[ijk]*Phi113[ijk] + 
       2*invh12*nvec1*Phi012[ijk]*Phi113[ijk] + 
       2*invh11*nvec2*Phi012[ijk]*Phi113[ijk] + 
       2*invh12*nvec2*Phi022[ijk]*Phi113[ijk] + 
       2*invh13*nvec2*Phi023[ijk]*Phi113[ijk] + 
       2*invh12*nvec3*Phi023[ijk]*Phi113[ijk] + 
       2*invh13*nvec3*Phi033[ijk]*Phi113[ijk] + 
       2*invh12*nvec0*Phi001[ijk]*Phi213[ijk] + 
       2*invh02*nvec1*Phi001[ijk]*Phi213[ijk] + 
       2*invh22*nvec0*Phi002[ijk]*Phi213[ijk] + 
       2*invh02*nvec2*Phi002[ijk]*Phi213[ijk] + 
       2*invh23*nvec0*Phi003[ijk]*Phi213[ijk] + 
       2*invh02*nvec3*Phi003[ijk]*Phi213[ijk] + 
       2*invh12*nvec1*Phi011[ijk]*Phi213[ijk] + 
       2*invh22*nvec1*Phi012[ijk]*Phi213[ijk] + 
       2*invh12*nvec2*Phi012[ijk]*Phi213[ijk] + 
       2*invh22*nvec2*Phi022[ijk]*Phi213[ijk] + 
       2*invh23*nvec2*Phi023[ijk]*Phi213[ijk] + 
       2*invh22*nvec3*Phi023[ijk]*Phi213[ijk] + 
       2*invh23*nvec3*Phi033[ijk]*Phi213[ijk] + 
       2*invh13*nvec0*Phi001[ijk]*Phi313[ijk] + 
       2*invh03*nvec1*Phi001[ijk]*Phi313[ijk] + 
       2*invh23*nvec0*Phi002[ijk]*Phi313[ijk] + 
       2*invh03*nvec2*Phi002[ijk]*Phi313[ijk] + 
       2*invh33*nvec0*Phi003[ijk]*Phi313[ijk] + 
       2*invh03*nvec3*Phi003[ijk]*Phi313[ijk] + 
       2*invh13*nvec1*Phi011[ijk]*Phi313[ijk] + 
       2*invh23*nvec1*Phi012[ijk]*Phi313[ijk] + 
       2*invh13*nvec2*Phi012[ijk]*Phi313[ijk] + 
       2*invh23*nvec2*Phi022[ijk]*Phi313[ijk] + 
       2*invh33*nvec2*Phi023[ijk]*Phi313[ijk] + 
       2*invh23*nvec3*Phi023[ijk]*Phi313[ijk] + 
       2*invh33*nvec3*Phi033[ijk]*Phi313[ijk] + 
       2*nvec0*nvec1*Phi001[ijk]*Pi13[ijk] + 
       2*nvec0*nvec2*Phi002[ijk]*Pi13[ijk] + 
       2*nvec0*nvec3*Phi003[ijk]*Pi13[ijk] + 
       Power(nvec1,2)*Phi011[ijk]*Pi13[ijk] + 
       2*nvec1*nvec2*Phi012[ijk]*Pi13[ijk] + 
       Power(nvec2,2)*Phi022[ijk]*Pi13[ijk] + 
       2*nvec2*nvec3*Phi023[ijk]*Pi13[ijk] + 
       Power(nvec3,2)*Phi033[ijk]*Pi13[ijk] + 
       nvec0*Phi000[ijk]*(2*invh01*Phi113[ijk] + 2*invh02*Phi213[ijk] + 
          2*invh03*Phi313[ijk] + nvec0*Pi13[ijk]) + 
       2*Phi013[ijk]*(-gamma2 + invh00*nvec0*Phi000[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi001[ijk] + 
          invh02*nvec0*Phi002[ijk] + invh00*nvec2*Phi002[ijk] + 
          invh03*nvec0*Phi003[ijk] + invh00*nvec3*Phi003[ijk] + 
          invh01*nvec1*Phi011[ijk] + invh02*nvec1*Phi012[ijk] + 
          invh01*nvec2*Phi012[ijk] + invh02*nvec2*Phi022[ijk] + 
          invh03*nvec2*Phi023[ijk] + invh02*nvec3*Phi023[ijk] + 
          invh03*nvec3*Phi033[ijk] + invh13*nvec1*Phi113[ijk] + 
          invh11*nvec3*Phi113[ijk] + invh23*nvec1*Phi213[ijk] + 
          invh12*nvec3*Phi213[ijk] + invh33*nvec1*Phi313[ijk] + 
          invh13*nvec3*Phi313[ijk] + nvec1*nvec3*Pi13[ijk])))/2.
;

dtPhi022[ijk]
=
-(interior*AdPhi022[ijk]) + (alpha[ijk]*
     (2*invh02*nvec2*Power(Phi022[ijk],2) + 
       2*invh11*nvec0*Phi001[ijk]*Phi122[ijk] + 
       2*invh01*nvec1*Phi001[ijk]*Phi122[ijk] + 
       2*invh12*nvec0*Phi002[ijk]*Phi122[ijk] + 
       2*invh01*nvec2*Phi002[ijk]*Phi122[ijk] + 
       2*invh13*nvec0*Phi003[ijk]*Phi122[ijk] + 
       2*invh01*nvec3*Phi003[ijk]*Phi122[ijk] + 
       2*invh11*nvec1*Phi011[ijk]*Phi122[ijk] + 
       2*invh12*nvec1*Phi012[ijk]*Phi122[ijk] + 
       2*invh11*nvec2*Phi012[ijk]*Phi122[ijk] + 
       2*invh13*nvec1*Phi013[ijk]*Phi122[ijk] + 
       2*invh11*nvec3*Phi013[ijk]*Phi122[ijk] + 
       2*invh13*nvec2*Phi023[ijk]*Phi122[ijk] + 
       2*invh12*nvec3*Phi023[ijk]*Phi122[ijk] + 
       2*invh13*nvec3*Phi033[ijk]*Phi122[ijk] + 
       2*invh12*nvec0*Phi001[ijk]*Phi222[ijk] + 
       2*invh02*nvec1*Phi001[ijk]*Phi222[ijk] + 
       2*invh22*nvec0*Phi002[ijk]*Phi222[ijk] + 
       2*invh02*nvec2*Phi002[ijk]*Phi222[ijk] + 
       2*invh23*nvec0*Phi003[ijk]*Phi222[ijk] + 
       2*invh02*nvec3*Phi003[ijk]*Phi222[ijk] + 
       2*invh12*nvec1*Phi011[ijk]*Phi222[ijk] + 
       2*invh22*nvec1*Phi012[ijk]*Phi222[ijk] + 
       2*invh12*nvec2*Phi012[ijk]*Phi222[ijk] + 
       2*invh23*nvec1*Phi013[ijk]*Phi222[ijk] + 
       2*invh12*nvec3*Phi013[ijk]*Phi222[ijk] + 
       2*invh23*nvec2*Phi023[ijk]*Phi222[ijk] + 
       2*invh22*nvec3*Phi023[ijk]*Phi222[ijk] + 
       2*invh23*nvec3*Phi033[ijk]*Phi222[ijk] + 
       2*invh13*nvec0*Phi001[ijk]*Phi322[ijk] + 
       2*invh03*nvec1*Phi001[ijk]*Phi322[ijk] + 
       2*invh23*nvec0*Phi002[ijk]*Phi322[ijk] + 
       2*invh03*nvec2*Phi002[ijk]*Phi322[ijk] + 
       2*invh33*nvec0*Phi003[ijk]*Phi322[ijk] + 
       2*invh03*nvec3*Phi003[ijk]*Phi322[ijk] + 
       2*invh13*nvec1*Phi011[ijk]*Phi322[ijk] + 
       2*invh23*nvec1*Phi012[ijk]*Phi322[ijk] + 
       2*invh13*nvec2*Phi012[ijk]*Phi322[ijk] + 
       2*invh33*nvec1*Phi013[ijk]*Phi322[ijk] + 
       2*invh13*nvec3*Phi013[ijk]*Phi322[ijk] + 
       2*invh33*nvec2*Phi023[ijk]*Phi322[ijk] + 
       2*invh23*nvec3*Phi023[ijk]*Phi322[ijk] + 
       2*invh33*nvec3*Phi033[ijk]*Phi322[ijk] + 
       2*nvec0*nvec1*Phi001[ijk]*Pi22[ijk] + 
       2*nvec0*nvec2*Phi002[ijk]*Pi22[ijk] + 
       2*nvec0*nvec3*Phi003[ijk]*Pi22[ijk] + 
       Power(nvec1,2)*Phi011[ijk]*Pi22[ijk] + 
       2*nvec1*nvec2*Phi012[ijk]*Pi22[ijk] + 
       2*nvec1*nvec3*Phi013[ijk]*Pi22[ijk] + 
       2*nvec2*nvec3*Phi023[ijk]*Pi22[ijk] + 
       Power(nvec3,2)*Phi033[ijk]*Pi22[ijk] + 
       nvec0*Phi000[ijk]*(2*invh01*Phi122[ijk] + 2*invh02*Phi222[ijk] + 
          2*invh03*Phi322[ijk] + nvec0*Pi22[ijk]) + 
       Phi022[ijk]*(-2*gamma2 + 2*invh00*nvec0*Phi000[ijk] + 
          2*(invh01*nvec0 + invh00*nvec1)*Phi001[ijk] + 
          2*invh02*nvec0*Phi002[ijk] + 2*invh00*nvec2*Phi002[ijk] + 
          2*invh03*nvec0*Phi003[ijk] + 2*invh00*nvec3*Phi003[ijk] + 
          2*invh01*nvec1*Phi011[ijk] + 2*invh02*nvec1*Phi012[ijk] + 
          2*invh01*nvec2*Phi012[ijk] + 2*invh03*nvec1*Phi013[ijk] + 
          2*invh01*nvec3*Phi013[ijk] + 2*invh03*nvec2*Phi023[ijk] + 
          2*invh02*nvec3*Phi023[ijk] + 2*invh03*nvec3*Phi033[ijk] + 
          2*invh12*nvec2*Phi122[ijk] + 2*invh22*nvec2*Phi222[ijk] + 
          2*invh23*nvec2*Phi322[ijk] + Power(nvec2,2)*Pi22[ijk])))/2.
;

dtPhi023[ijk]
=
-(interior*AdPhi023[ijk]) + (alpha[ijk]*
     (2*(invh03*nvec2 + invh02*nvec3)*Power(Phi023[ijk],2) + 
       2*invh11*nvec0*Phi001[ijk]*Phi123[ijk] + 
       2*invh01*nvec1*Phi001[ijk]*Phi123[ijk] + 
       2*invh12*nvec0*Phi002[ijk]*Phi123[ijk] + 
       2*invh01*nvec2*Phi002[ijk]*Phi123[ijk] + 
       2*invh13*nvec0*Phi003[ijk]*Phi123[ijk] + 
       2*invh01*nvec3*Phi003[ijk]*Phi123[ijk] + 
       2*invh11*nvec1*Phi011[ijk]*Phi123[ijk] + 
       2*invh12*nvec1*Phi012[ijk]*Phi123[ijk] + 
       2*invh11*nvec2*Phi012[ijk]*Phi123[ijk] + 
       2*invh13*nvec1*Phi013[ijk]*Phi123[ijk] + 
       2*invh11*nvec3*Phi013[ijk]*Phi123[ijk] + 
       2*invh12*nvec2*Phi022[ijk]*Phi123[ijk] + 
       2*invh13*nvec3*Phi033[ijk]*Phi123[ijk] + 
       2*invh12*nvec0*Phi001[ijk]*Phi223[ijk] + 
       2*invh02*nvec1*Phi001[ijk]*Phi223[ijk] + 
       2*invh22*nvec0*Phi002[ijk]*Phi223[ijk] + 
       2*invh02*nvec2*Phi002[ijk]*Phi223[ijk] + 
       2*invh23*nvec0*Phi003[ijk]*Phi223[ijk] + 
       2*invh02*nvec3*Phi003[ijk]*Phi223[ijk] + 
       2*invh12*nvec1*Phi011[ijk]*Phi223[ijk] + 
       2*invh22*nvec1*Phi012[ijk]*Phi223[ijk] + 
       2*invh12*nvec2*Phi012[ijk]*Phi223[ijk] + 
       2*invh23*nvec1*Phi013[ijk]*Phi223[ijk] + 
       2*invh12*nvec3*Phi013[ijk]*Phi223[ijk] + 
       2*invh22*nvec2*Phi022[ijk]*Phi223[ijk] + 
       2*invh23*nvec3*Phi033[ijk]*Phi223[ijk] + 
       2*invh13*nvec0*Phi001[ijk]*Phi323[ijk] + 
       2*invh03*nvec1*Phi001[ijk]*Phi323[ijk] + 
       2*invh23*nvec0*Phi002[ijk]*Phi323[ijk] + 
       2*invh03*nvec2*Phi002[ijk]*Phi323[ijk] + 
       2*invh33*nvec0*Phi003[ijk]*Phi323[ijk] + 
       2*invh03*nvec3*Phi003[ijk]*Phi323[ijk] + 
       2*invh13*nvec1*Phi011[ijk]*Phi323[ijk] + 
       2*invh23*nvec1*Phi012[ijk]*Phi323[ijk] + 
       2*invh13*nvec2*Phi012[ijk]*Phi323[ijk] + 
       2*invh33*nvec1*Phi013[ijk]*Phi323[ijk] + 
       2*invh13*nvec3*Phi013[ijk]*Phi323[ijk] + 
       2*invh23*nvec2*Phi022[ijk]*Phi323[ijk] + 
       2*invh33*nvec3*Phi033[ijk]*Phi323[ijk] + 
       2*nvec0*nvec1*Phi001[ijk]*Pi23[ijk] + 
       2*nvec0*nvec2*Phi002[ijk]*Pi23[ijk] + 
       2*nvec0*nvec3*Phi003[ijk]*Pi23[ijk] + 
       Power(nvec1,2)*Phi011[ijk]*Pi23[ijk] + 
       2*nvec1*nvec2*Phi012[ijk]*Pi23[ijk] + 
       2*nvec1*nvec3*Phi013[ijk]*Pi23[ijk] + 
       Power(nvec2,2)*Phi022[ijk]*Pi23[ijk] + 
       Power(nvec3,2)*Phi033[ijk]*Pi23[ijk] + 
       nvec0*Phi000[ijk]*(2*invh01*Phi123[ijk] + 2*invh02*Phi223[ijk] + 
          2*invh03*Phi323[ijk] + nvec0*Pi23[ijk]) + 
       2*Phi023[ijk]*(-gamma2 + invh00*nvec0*Phi000[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi001[ijk] + 
          invh02*nvec0*Phi002[ijk] + invh00*nvec2*Phi002[ijk] + 
          invh03*nvec0*Phi003[ijk] + invh00*nvec3*Phi003[ijk] + 
          invh01*nvec1*Phi011[ijk] + invh02*nvec1*Phi012[ijk] + 
          invh01*nvec2*Phi012[ijk] + invh03*nvec1*Phi013[ijk] + 
          invh01*nvec3*Phi013[ijk] + invh02*nvec2*Phi022[ijk] + 
          invh03*nvec3*Phi033[ijk] + invh13*nvec2*Phi123[ijk] + 
          invh12*nvec3*Phi123[ijk] + invh23*nvec2*Phi223[ijk] + 
          invh22*nvec3*Phi223[ijk] + invh33*nvec2*Phi323[ijk] + 
          invh23*nvec3*Phi323[ijk] + nvec2*nvec3*Pi23[ijk])))/2.
;

dtPhi033[ijk]
=
-(interior*AdPhi033[ijk]) + (alpha[ijk]*
     (2*invh03*nvec3*Power(Phi033[ijk],2) + 
       2*invh11*nvec0*Phi001[ijk]*Phi133[ijk] + 
       2*invh01*nvec1*Phi001[ijk]*Phi133[ijk] + 
       2*invh12*nvec0*Phi002[ijk]*Phi133[ijk] + 
       2*invh01*nvec2*Phi002[ijk]*Phi133[ijk] + 
       2*invh13*nvec0*Phi003[ijk]*Phi133[ijk] + 
       2*invh01*nvec3*Phi003[ijk]*Phi133[ijk] + 
       2*invh11*nvec1*Phi011[ijk]*Phi133[ijk] + 
       2*invh12*nvec1*Phi012[ijk]*Phi133[ijk] + 
       2*invh11*nvec2*Phi012[ijk]*Phi133[ijk] + 
       2*invh13*nvec1*Phi013[ijk]*Phi133[ijk] + 
       2*invh11*nvec3*Phi013[ijk]*Phi133[ijk] + 
       2*invh12*nvec2*Phi022[ijk]*Phi133[ijk] + 
       2*invh13*nvec2*Phi023[ijk]*Phi133[ijk] + 
       2*invh12*nvec3*Phi023[ijk]*Phi133[ijk] + 
       2*invh12*nvec0*Phi001[ijk]*Phi233[ijk] + 
       2*invh02*nvec1*Phi001[ijk]*Phi233[ijk] + 
       2*invh22*nvec0*Phi002[ijk]*Phi233[ijk] + 
       2*invh02*nvec2*Phi002[ijk]*Phi233[ijk] + 
       2*invh23*nvec0*Phi003[ijk]*Phi233[ijk] + 
       2*invh02*nvec3*Phi003[ijk]*Phi233[ijk] + 
       2*invh12*nvec1*Phi011[ijk]*Phi233[ijk] + 
       2*invh22*nvec1*Phi012[ijk]*Phi233[ijk] + 
       2*invh12*nvec2*Phi012[ijk]*Phi233[ijk] + 
       2*invh23*nvec1*Phi013[ijk]*Phi233[ijk] + 
       2*invh12*nvec3*Phi013[ijk]*Phi233[ijk] + 
       2*invh22*nvec2*Phi022[ijk]*Phi233[ijk] + 
       2*invh23*nvec2*Phi023[ijk]*Phi233[ijk] + 
       2*invh22*nvec3*Phi023[ijk]*Phi233[ijk] + 
       2*invh13*nvec0*Phi001[ijk]*Phi333[ijk] + 
       2*invh03*nvec1*Phi001[ijk]*Phi333[ijk] + 
       2*invh23*nvec0*Phi002[ijk]*Phi333[ijk] + 
       2*invh03*nvec2*Phi002[ijk]*Phi333[ijk] + 
       2*invh33*nvec0*Phi003[ijk]*Phi333[ijk] + 
       2*invh03*nvec3*Phi003[ijk]*Phi333[ijk] + 
       2*invh13*nvec1*Phi011[ijk]*Phi333[ijk] + 
       2*invh23*nvec1*Phi012[ijk]*Phi333[ijk] + 
       2*invh13*nvec2*Phi012[ijk]*Phi333[ijk] + 
       2*invh33*nvec1*Phi013[ijk]*Phi333[ijk] + 
       2*invh13*nvec3*Phi013[ijk]*Phi333[ijk] + 
       2*invh23*nvec2*Phi022[ijk]*Phi333[ijk] + 
       2*invh33*nvec2*Phi023[ijk]*Phi333[ijk] + 
       2*invh23*nvec3*Phi023[ijk]*Phi333[ijk] + 
       2*nvec0*nvec1*Phi001[ijk]*Pi33[ijk] + 
       2*nvec0*nvec2*Phi002[ijk]*Pi33[ijk] + 
       2*nvec0*nvec3*Phi003[ijk]*Pi33[ijk] + 
       Power(nvec1,2)*Phi011[ijk]*Pi33[ijk] + 
       2*nvec1*nvec2*Phi012[ijk]*Pi33[ijk] + 
       2*nvec1*nvec3*Phi013[ijk]*Pi33[ijk] + 
       Power(nvec2,2)*Phi022[ijk]*Pi33[ijk] + 
       2*nvec2*nvec3*Phi023[ijk]*Pi33[ijk] + 
       nvec0*Phi000[ijk]*(2*invh01*Phi133[ijk] + 2*invh02*Phi233[ijk] + 
          2*invh03*Phi333[ijk] + nvec0*Pi33[ijk]) + 
       Phi033[ijk]*(-2*gamma2 + 2*invh00*nvec0*Phi000[ijk] + 
          2*(invh01*nvec0 + invh00*nvec1)*Phi001[ijk] + 
          2*invh02*nvec0*Phi002[ijk] + 2*invh00*nvec2*Phi002[ijk] + 
          2*invh03*nvec0*Phi003[ijk] + 2*invh00*nvec3*Phi003[ijk] + 
          2*invh01*nvec1*Phi011[ijk] + 2*invh02*nvec1*Phi012[ijk] + 
          2*invh01*nvec2*Phi012[ijk] + 2*invh03*nvec1*Phi013[ijk] + 
          2*invh01*nvec3*Phi013[ijk] + 2*invh02*nvec2*Phi022[ijk] + 
          2*invh03*nvec2*Phi023[ijk] + 2*invh02*nvec3*Phi023[ijk] + 
          2*invh13*nvec3*Phi133[ijk] + 2*invh23*nvec3*Phi233[ijk] + 
          2*invh33*nvec3*Phi333[ijk] + Power(nvec3,2)*Pi33[ijk])))/2.
;

dtPhi100[ijk]
=
-(interior*AdPhi100[ijk]) + (alpha[ijk]*
     (2*invh01*nvec0*Power(Phi100[ijk],2) + 
       2*Phi000[ijk]*((invh01*nvec0 + invh00*nvec1)*Phi101[ijk] + 
          (invh02*nvec0 + invh00*nvec2)*Phi102[ijk] + 
          invh03*nvec0*Phi103[ijk] + invh00*nvec3*Phi103[ijk] + 
          invh01*nvec1*Phi111[ijk] + invh02*nvec1*Phi112[ijk] + 
          invh01*nvec2*Phi112[ijk] + invh03*nvec1*Phi113[ijk] + 
          invh01*nvec3*Phi113[ijk] + invh02*nvec2*Phi122[ijk] + 
          invh03*nvec2*Phi123[ijk] + invh02*nvec3*Phi123[ijk] + 
          invh03*nvec3*Phi133[ijk]) + 
       2*invh12*nvec0*Phi101[ijk]*Phi200[ijk] + 
       2*invh02*nvec1*Phi101[ijk]*Phi200[ijk] + 
       2*invh22*nvec0*Phi102[ijk]*Phi200[ijk] + 
       2*invh02*nvec2*Phi102[ijk]*Phi200[ijk] + 
       2*invh23*nvec0*Phi103[ijk]*Phi200[ijk] + 
       2*invh02*nvec3*Phi103[ijk]*Phi200[ijk] + 
       2*invh12*nvec1*Phi111[ijk]*Phi200[ijk] + 
       2*invh22*nvec1*Phi112[ijk]*Phi200[ijk] + 
       2*invh12*nvec2*Phi112[ijk]*Phi200[ijk] + 
       2*invh23*nvec1*Phi113[ijk]*Phi200[ijk] + 
       2*invh12*nvec3*Phi113[ijk]*Phi200[ijk] + 
       2*invh22*nvec2*Phi122[ijk]*Phi200[ijk] + 
       2*invh23*nvec2*Phi123[ijk]*Phi200[ijk] + 
       2*invh22*nvec3*Phi123[ijk]*Phi200[ijk] + 
       2*invh23*nvec3*Phi133[ijk]*Phi200[ijk] + 
       2*invh13*nvec0*Phi101[ijk]*Phi300[ijk] + 
       2*invh03*nvec1*Phi101[ijk]*Phi300[ijk] + 
       2*invh23*nvec0*Phi102[ijk]*Phi300[ijk] + 
       2*invh03*nvec2*Phi102[ijk]*Phi300[ijk] + 
       2*invh33*nvec0*Phi103[ijk]*Phi300[ijk] + 
       2*invh03*nvec3*Phi103[ijk]*Phi300[ijk] + 
       2*invh13*nvec1*Phi111[ijk]*Phi300[ijk] + 
       2*invh23*nvec1*Phi112[ijk]*Phi300[ijk] + 
       2*invh13*nvec2*Phi112[ijk]*Phi300[ijk] + 
       2*invh33*nvec1*Phi113[ijk]*Phi300[ijk] + 
       2*invh13*nvec3*Phi113[ijk]*Phi300[ijk] + 
       2*invh23*nvec2*Phi122[ijk]*Phi300[ijk] + 
       2*invh33*nvec2*Phi123[ijk]*Phi300[ijk] + 
       2*invh23*nvec3*Phi123[ijk]*Phi300[ijk] + 
       2*invh33*nvec3*Phi133[ijk]*Phi300[ijk] + 
       2*nvec0*nvec1*Phi101[ijk]*Pi00[ijk] + 
       2*nvec0*nvec2*Phi102[ijk]*Pi00[ijk] + 
       2*nvec0*nvec3*Phi103[ijk]*Pi00[ijk] + 
       Power(nvec1,2)*Phi111[ijk]*Pi00[ijk] + 
       2*nvec1*nvec2*Phi112[ijk]*Pi00[ijk] + 
       2*nvec1*nvec3*Phi113[ijk]*Pi00[ijk] + 
       Power(nvec2,2)*Phi122[ijk]*Pi00[ijk] + 
       2*nvec2*nvec3*Phi123[ijk]*Pi00[ijk] + 
       Power(nvec3,2)*Phi133[ijk]*Pi00[ijk] + 
       Phi100[ijk]*(-2*gamma2 + 2*invh00*nvec0*Phi000[ijk] + 
          2*(invh11*nvec0 + invh01*nvec1)*Phi101[ijk] + 
          2*invh12*nvec0*Phi102[ijk] + 2*invh01*nvec2*Phi102[ijk] + 
          2*invh13*nvec0*Phi103[ijk] + 2*invh01*nvec3*Phi103[ijk] + 
          2*invh11*nvec1*Phi111[ijk] + 2*invh12*nvec1*Phi112[ijk] + 
          2*invh11*nvec2*Phi112[ijk] + 2*invh13*nvec1*Phi113[ijk] + 
          2*invh11*nvec3*Phi113[ijk] + 2*invh12*nvec2*Phi122[ijk] + 
          2*invh13*nvec2*Phi123[ijk] + 2*invh12*nvec3*Phi123[ijk] + 
          2*invh13*nvec3*Phi133[ijk] + 2*invh02*nvec0*Phi200[ijk] + 
          2*invh03*nvec0*Phi300[ijk] + Power(nvec0,2)*Pi00[ijk])))/2.
;

dtPhi101[ijk]
=
-(interior*AdPhi101[ijk]) + (alpha[ijk]*
     (2*(invh11*nvec0 + invh01*nvec1)*Power(Phi101[ijk],2) + 
       2*Phi001[ijk]*(invh00*nvec0*Phi100[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi101[ijk] + 
          invh02*nvec0*Phi102[ijk] + invh00*nvec2*Phi102[ijk] + 
          invh03*nvec0*Phi103[ijk] + invh00*nvec3*Phi103[ijk] + 
          invh01*nvec1*Phi111[ijk] + invh02*nvec1*Phi112[ijk] + 
          invh01*nvec2*Phi112[ijk] + invh03*nvec1*Phi113[ijk] + 
          invh01*nvec3*Phi113[ijk] + invh02*nvec2*Phi122[ijk] + 
          invh03*nvec2*Phi123[ijk] + invh02*nvec3*Phi123[ijk] + 
          invh03*nvec3*Phi133[ijk]) + 
       2*invh02*nvec0*Phi100[ijk]*Phi201[ijk] + 
       2*invh22*nvec0*Phi102[ijk]*Phi201[ijk] + 
       2*invh02*nvec2*Phi102[ijk]*Phi201[ijk] + 
       2*invh23*nvec0*Phi103[ijk]*Phi201[ijk] + 
       2*invh02*nvec3*Phi103[ijk]*Phi201[ijk] + 
       2*invh12*nvec1*Phi111[ijk]*Phi201[ijk] + 
       2*invh22*nvec1*Phi112[ijk]*Phi201[ijk] + 
       2*invh12*nvec2*Phi112[ijk]*Phi201[ijk] + 
       2*invh23*nvec1*Phi113[ijk]*Phi201[ijk] + 
       2*invh12*nvec3*Phi113[ijk]*Phi201[ijk] + 
       2*invh22*nvec2*Phi122[ijk]*Phi201[ijk] + 
       2*invh23*nvec2*Phi123[ijk]*Phi201[ijk] + 
       2*invh22*nvec3*Phi123[ijk]*Phi201[ijk] + 
       2*invh23*nvec3*Phi133[ijk]*Phi201[ijk] + 
       2*invh03*nvec0*Phi100[ijk]*Phi301[ijk] + 
       2*invh23*nvec0*Phi102[ijk]*Phi301[ijk] + 
       2*invh03*nvec2*Phi102[ijk]*Phi301[ijk] + 
       2*invh33*nvec0*Phi103[ijk]*Phi301[ijk] + 
       2*invh03*nvec3*Phi103[ijk]*Phi301[ijk] + 
       2*invh13*nvec1*Phi111[ijk]*Phi301[ijk] + 
       2*invh23*nvec1*Phi112[ijk]*Phi301[ijk] + 
       2*invh13*nvec2*Phi112[ijk]*Phi301[ijk] + 
       2*invh33*nvec1*Phi113[ijk]*Phi301[ijk] + 
       2*invh13*nvec3*Phi113[ijk]*Phi301[ijk] + 
       2*invh23*nvec2*Phi122[ijk]*Phi301[ijk] + 
       2*invh33*nvec2*Phi123[ijk]*Phi301[ijk] + 
       2*invh23*nvec3*Phi123[ijk]*Phi301[ijk] + 
       2*invh33*nvec3*Phi133[ijk]*Phi301[ijk] + 
       Power(nvec0,2)*Phi100[ijk]*Pi01[ijk] + 
       2*nvec0*nvec2*Phi102[ijk]*Pi01[ijk] + 
       2*nvec0*nvec3*Phi103[ijk]*Pi01[ijk] + 
       Power(nvec1,2)*Phi111[ijk]*Pi01[ijk] + 
       2*nvec1*nvec2*Phi112[ijk]*Pi01[ijk] + 
       2*nvec1*nvec3*Phi113[ijk]*Pi01[ijk] + 
       Power(nvec2,2)*Phi122[ijk]*Pi01[ijk] + 
       2*nvec2*nvec3*Phi123[ijk]*Pi01[ijk] + 
       Power(nvec3,2)*Phi133[ijk]*Pi01[ijk] + 
       2*Phi101[ijk]*(-gamma2 + invh01*nvec0*Phi100[ijk] + 
          (invh12*nvec0 + invh01*nvec2)*Phi102[ijk] + 
          invh13*nvec0*Phi103[ijk] + invh01*nvec3*Phi103[ijk] + 
          invh11*nvec1*Phi111[ijk] + invh12*nvec1*Phi112[ijk] + 
          invh11*nvec2*Phi112[ijk] + invh13*nvec1*Phi113[ijk] + 
          invh11*nvec3*Phi113[ijk] + invh12*nvec2*Phi122[ijk] + 
          invh13*nvec2*Phi123[ijk] + invh12*nvec3*Phi123[ijk] + 
          invh13*nvec3*Phi133[ijk] + invh12*nvec0*Phi201[ijk] + 
          invh02*nvec1*Phi201[ijk] + invh13*nvec0*Phi301[ijk] + 
          invh03*nvec1*Phi301[ijk] + nvec0*nvec1*Pi01[ijk])))/2.
;

dtPhi102[ijk]
=
-(interior*AdPhi102[ijk]) + (alpha[ijk]*
     (2*(invh12*nvec0 + invh01*nvec2)*Power(Phi102[ijk],2) + 
       2*Phi002[ijk]*(invh00*nvec0*Phi100[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi101[ijk] + 
          invh02*nvec0*Phi102[ijk] + invh00*nvec2*Phi102[ijk] + 
          invh03*nvec0*Phi103[ijk] + invh00*nvec3*Phi103[ijk] + 
          invh01*nvec1*Phi111[ijk] + invh02*nvec1*Phi112[ijk] + 
          invh01*nvec2*Phi112[ijk] + invh03*nvec1*Phi113[ijk] + 
          invh01*nvec3*Phi113[ijk] + invh02*nvec2*Phi122[ijk] + 
          invh03*nvec2*Phi123[ijk] + invh02*nvec3*Phi123[ijk] + 
          invh03*nvec3*Phi133[ijk]) + 
       2*invh02*nvec0*Phi100[ijk]*Phi202[ijk] + 
       2*invh12*nvec0*Phi101[ijk]*Phi202[ijk] + 
       2*invh02*nvec1*Phi101[ijk]*Phi202[ijk] + 
       2*invh23*nvec0*Phi103[ijk]*Phi202[ijk] + 
       2*invh02*nvec3*Phi103[ijk]*Phi202[ijk] + 
       2*invh12*nvec1*Phi111[ijk]*Phi202[ijk] + 
       2*invh22*nvec1*Phi112[ijk]*Phi202[ijk] + 
       2*invh12*nvec2*Phi112[ijk]*Phi202[ijk] + 
       2*invh23*nvec1*Phi113[ijk]*Phi202[ijk] + 
       2*invh12*nvec3*Phi113[ijk]*Phi202[ijk] + 
       2*invh22*nvec2*Phi122[ijk]*Phi202[ijk] + 
       2*invh23*nvec2*Phi123[ijk]*Phi202[ijk] + 
       2*invh22*nvec3*Phi123[ijk]*Phi202[ijk] + 
       2*invh23*nvec3*Phi133[ijk]*Phi202[ijk] + 
       2*invh03*nvec0*Phi100[ijk]*Phi302[ijk] + 
       2*invh13*nvec0*Phi101[ijk]*Phi302[ijk] + 
       2*invh03*nvec1*Phi101[ijk]*Phi302[ijk] + 
       2*invh33*nvec0*Phi103[ijk]*Phi302[ijk] + 
       2*invh03*nvec3*Phi103[ijk]*Phi302[ijk] + 
       2*invh13*nvec1*Phi111[ijk]*Phi302[ijk] + 
       2*invh23*nvec1*Phi112[ijk]*Phi302[ijk] + 
       2*invh13*nvec2*Phi112[ijk]*Phi302[ijk] + 
       2*invh33*nvec1*Phi113[ijk]*Phi302[ijk] + 
       2*invh13*nvec3*Phi113[ijk]*Phi302[ijk] + 
       2*invh23*nvec2*Phi122[ijk]*Phi302[ijk] + 
       2*invh33*nvec2*Phi123[ijk]*Phi302[ijk] + 
       2*invh23*nvec3*Phi123[ijk]*Phi302[ijk] + 
       2*invh33*nvec3*Phi133[ijk]*Phi302[ijk] + 
       Power(nvec0,2)*Phi100[ijk]*Pi02[ijk] + 
       2*nvec0*nvec1*Phi101[ijk]*Pi02[ijk] + 
       2*nvec0*nvec3*Phi103[ijk]*Pi02[ijk] + 
       Power(nvec1,2)*Phi111[ijk]*Pi02[ijk] + 
       2*nvec1*nvec2*Phi112[ijk]*Pi02[ijk] + 
       2*nvec1*nvec3*Phi113[ijk]*Pi02[ijk] + 
       Power(nvec2,2)*Phi122[ijk]*Pi02[ijk] + 
       2*nvec2*nvec3*Phi123[ijk]*Pi02[ijk] + 
       Power(nvec3,2)*Phi133[ijk]*Pi02[ijk] + 
       2*Phi102[ijk]*(-gamma2 + invh01*nvec0*Phi100[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi101[ijk] + 
          invh13*nvec0*Phi103[ijk] + invh01*nvec3*Phi103[ijk] + 
          invh11*nvec1*Phi111[ijk] + invh12*nvec1*Phi112[ijk] + 
          invh11*nvec2*Phi112[ijk] + invh13*nvec1*Phi113[ijk] + 
          invh11*nvec3*Phi113[ijk] + invh12*nvec2*Phi122[ijk] + 
          invh13*nvec2*Phi123[ijk] + invh12*nvec3*Phi123[ijk] + 
          invh13*nvec3*Phi133[ijk] + invh22*nvec0*Phi202[ijk] + 
          invh02*nvec2*Phi202[ijk] + invh23*nvec0*Phi302[ijk] + 
          invh03*nvec2*Phi302[ijk] + nvec0*nvec2*Pi02[ijk])))/2.
;

dtPhi103[ijk]
=
-(interior*AdPhi103[ijk]) + (alpha[ijk]*
     (2*(invh13*nvec0 + invh01*nvec3)*Power(Phi103[ijk],2) + 
       2*Phi003[ijk]*(invh00*nvec0*Phi100[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi101[ijk] + 
          invh02*nvec0*Phi102[ijk] + invh00*nvec2*Phi102[ijk] + 
          invh03*nvec0*Phi103[ijk] + invh00*nvec3*Phi103[ijk] + 
          invh01*nvec1*Phi111[ijk] + invh02*nvec1*Phi112[ijk] + 
          invh01*nvec2*Phi112[ijk] + invh03*nvec1*Phi113[ijk] + 
          invh01*nvec3*Phi113[ijk] + invh02*nvec2*Phi122[ijk] + 
          invh03*nvec2*Phi123[ijk] + invh02*nvec3*Phi123[ijk] + 
          invh03*nvec3*Phi133[ijk]) + 
       2*invh02*nvec0*Phi100[ijk]*Phi203[ijk] + 
       2*invh12*nvec0*Phi101[ijk]*Phi203[ijk] + 
       2*invh02*nvec1*Phi101[ijk]*Phi203[ijk] + 
       2*invh22*nvec0*Phi102[ijk]*Phi203[ijk] + 
       2*invh02*nvec2*Phi102[ijk]*Phi203[ijk] + 
       2*invh12*nvec1*Phi111[ijk]*Phi203[ijk] + 
       2*invh22*nvec1*Phi112[ijk]*Phi203[ijk] + 
       2*invh12*nvec2*Phi112[ijk]*Phi203[ijk] + 
       2*invh23*nvec1*Phi113[ijk]*Phi203[ijk] + 
       2*invh12*nvec3*Phi113[ijk]*Phi203[ijk] + 
       2*invh22*nvec2*Phi122[ijk]*Phi203[ijk] + 
       2*invh23*nvec2*Phi123[ijk]*Phi203[ijk] + 
       2*invh22*nvec3*Phi123[ijk]*Phi203[ijk] + 
       2*invh23*nvec3*Phi133[ijk]*Phi203[ijk] + 
       2*invh03*nvec0*Phi100[ijk]*Phi303[ijk] + 
       2*invh13*nvec0*Phi101[ijk]*Phi303[ijk] + 
       2*invh03*nvec1*Phi101[ijk]*Phi303[ijk] + 
       2*invh23*nvec0*Phi102[ijk]*Phi303[ijk] + 
       2*invh03*nvec2*Phi102[ijk]*Phi303[ijk] + 
       2*invh13*nvec1*Phi111[ijk]*Phi303[ijk] + 
       2*invh23*nvec1*Phi112[ijk]*Phi303[ijk] + 
       2*invh13*nvec2*Phi112[ijk]*Phi303[ijk] + 
       2*invh33*nvec1*Phi113[ijk]*Phi303[ijk] + 
       2*invh13*nvec3*Phi113[ijk]*Phi303[ijk] + 
       2*invh23*nvec2*Phi122[ijk]*Phi303[ijk] + 
       2*invh33*nvec2*Phi123[ijk]*Phi303[ijk] + 
       2*invh23*nvec3*Phi123[ijk]*Phi303[ijk] + 
       2*invh33*nvec3*Phi133[ijk]*Phi303[ijk] + 
       Power(nvec0,2)*Phi100[ijk]*Pi03[ijk] + 
       2*nvec0*nvec1*Phi101[ijk]*Pi03[ijk] + 
       2*nvec0*nvec2*Phi102[ijk]*Pi03[ijk] + 
       Power(nvec1,2)*Phi111[ijk]*Pi03[ijk] + 
       2*nvec1*nvec2*Phi112[ijk]*Pi03[ijk] + 
       2*nvec1*nvec3*Phi113[ijk]*Pi03[ijk] + 
       Power(nvec2,2)*Phi122[ijk]*Pi03[ijk] + 
       2*nvec2*nvec3*Phi123[ijk]*Pi03[ijk] + 
       Power(nvec3,2)*Phi133[ijk]*Pi03[ijk] + 
       2*Phi103[ijk]*(-gamma2 + invh01*nvec0*Phi100[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi101[ijk] + 
          invh12*nvec0*Phi102[ijk] + invh01*nvec2*Phi102[ijk] + 
          invh11*nvec1*Phi111[ijk] + invh12*nvec1*Phi112[ijk] + 
          invh11*nvec2*Phi112[ijk] + invh13*nvec1*Phi113[ijk] + 
          invh11*nvec3*Phi113[ijk] + invh12*nvec2*Phi122[ijk] + 
          invh13*nvec2*Phi123[ijk] + invh12*nvec3*Phi123[ijk] + 
          invh13*nvec3*Phi133[ijk] + invh23*nvec0*Phi203[ijk] + 
          invh02*nvec3*Phi203[ijk] + invh33*nvec0*Phi303[ijk] + 
          invh03*nvec3*Phi303[ijk] + nvec0*nvec3*Pi03[ijk])))/2.
;

dtPhi111[ijk]
=
-(interior*AdPhi111[ijk]) + (alpha[ijk]*
     (2*invh11*nvec1*Power(Phi111[ijk],2) + 
       2*Phi011[ijk]*(invh00*nvec0*Phi100[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi101[ijk] + 
          invh02*nvec0*Phi102[ijk] + invh00*nvec2*Phi102[ijk] + 
          invh03*nvec0*Phi103[ijk] + invh00*nvec3*Phi103[ijk] + 
          invh01*nvec1*Phi111[ijk] + invh02*nvec1*Phi112[ijk] + 
          invh01*nvec2*Phi112[ijk] + invh03*nvec1*Phi113[ijk] + 
          invh01*nvec3*Phi113[ijk] + invh02*nvec2*Phi122[ijk] + 
          invh03*nvec2*Phi123[ijk] + invh02*nvec3*Phi123[ijk] + 
          invh03*nvec3*Phi133[ijk]) + 
       2*invh02*nvec0*Phi100[ijk]*Phi211[ijk] + 
       2*invh12*nvec0*Phi101[ijk]*Phi211[ijk] + 
       2*invh02*nvec1*Phi101[ijk]*Phi211[ijk] + 
       2*invh22*nvec0*Phi102[ijk]*Phi211[ijk] + 
       2*invh02*nvec2*Phi102[ijk]*Phi211[ijk] + 
       2*invh23*nvec0*Phi103[ijk]*Phi211[ijk] + 
       2*invh02*nvec3*Phi103[ijk]*Phi211[ijk] + 
       2*invh22*nvec1*Phi112[ijk]*Phi211[ijk] + 
       2*invh12*nvec2*Phi112[ijk]*Phi211[ijk] + 
       2*invh23*nvec1*Phi113[ijk]*Phi211[ijk] + 
       2*invh12*nvec3*Phi113[ijk]*Phi211[ijk] + 
       2*invh22*nvec2*Phi122[ijk]*Phi211[ijk] + 
       2*invh23*nvec2*Phi123[ijk]*Phi211[ijk] + 
       2*invh22*nvec3*Phi123[ijk]*Phi211[ijk] + 
       2*invh23*nvec3*Phi133[ijk]*Phi211[ijk] + 
       2*invh03*nvec0*Phi100[ijk]*Phi311[ijk] + 
       2*invh13*nvec0*Phi101[ijk]*Phi311[ijk] + 
       2*invh03*nvec1*Phi101[ijk]*Phi311[ijk] + 
       2*invh23*nvec0*Phi102[ijk]*Phi311[ijk] + 
       2*invh03*nvec2*Phi102[ijk]*Phi311[ijk] + 
       2*invh33*nvec0*Phi103[ijk]*Phi311[ijk] + 
       2*invh03*nvec3*Phi103[ijk]*Phi311[ijk] + 
       2*invh23*nvec1*Phi112[ijk]*Phi311[ijk] + 
       2*invh13*nvec2*Phi112[ijk]*Phi311[ijk] + 
       2*invh33*nvec1*Phi113[ijk]*Phi311[ijk] + 
       2*invh13*nvec3*Phi113[ijk]*Phi311[ijk] + 
       2*invh23*nvec2*Phi122[ijk]*Phi311[ijk] + 
       2*invh33*nvec2*Phi123[ijk]*Phi311[ijk] + 
       2*invh23*nvec3*Phi123[ijk]*Phi311[ijk] + 
       2*invh33*nvec3*Phi133[ijk]*Phi311[ijk] + 
       Power(nvec0,2)*Phi100[ijk]*Pi11[ijk] + 
       2*nvec0*nvec1*Phi101[ijk]*Pi11[ijk] + 
       2*nvec0*nvec2*Phi102[ijk]*Pi11[ijk] + 
       2*nvec0*nvec3*Phi103[ijk]*Pi11[ijk] + 
       2*nvec1*nvec2*Phi112[ijk]*Pi11[ijk] + 
       2*nvec1*nvec3*Phi113[ijk]*Pi11[ijk] + 
       Power(nvec2,2)*Phi122[ijk]*Pi11[ijk] + 
       2*nvec2*nvec3*Phi123[ijk]*Pi11[ijk] + 
       Power(nvec3,2)*Phi133[ijk]*Pi11[ijk] + 
       Phi111[ijk]*(-2*gamma2 + 2*invh01*nvec0*Phi100[ijk] + 
          2*(invh11*nvec0 + invh01*nvec1)*Phi101[ijk] + 
          2*invh12*nvec0*Phi102[ijk] + 2*invh01*nvec2*Phi102[ijk] + 
          2*invh13*nvec0*Phi103[ijk] + 2*invh01*nvec3*Phi103[ijk] + 
          2*invh12*nvec1*Phi112[ijk] + 2*invh11*nvec2*Phi112[ijk] + 
          2*invh13*nvec1*Phi113[ijk] + 2*invh11*nvec3*Phi113[ijk] + 
          2*invh12*nvec2*Phi122[ijk] + 2*invh13*nvec2*Phi123[ijk] + 
          2*invh12*nvec3*Phi123[ijk] + 2*invh13*nvec3*Phi133[ijk] + 
          2*invh12*nvec1*Phi211[ijk] + 2*invh13*nvec1*Phi311[ijk] + 
          Power(nvec1,2)*Pi11[ijk])))/2.
;

dtPhi112[ijk]
=
-(interior*AdPhi112[ijk]) + (alpha[ijk]*
     (2*(invh12*nvec1 + invh11*nvec2)*Power(Phi112[ijk],2) + 
       2*Phi012[ijk]*(invh00*nvec0*Phi100[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi101[ijk] + 
          invh02*nvec0*Phi102[ijk] + invh00*nvec2*Phi102[ijk] + 
          invh03*nvec0*Phi103[ijk] + invh00*nvec3*Phi103[ijk] + 
          invh01*nvec1*Phi111[ijk] + invh02*nvec1*Phi112[ijk] + 
          invh01*nvec2*Phi112[ijk] + invh03*nvec1*Phi113[ijk] + 
          invh01*nvec3*Phi113[ijk] + invh02*nvec2*Phi122[ijk] + 
          invh03*nvec2*Phi123[ijk] + invh02*nvec3*Phi123[ijk] + 
          invh03*nvec3*Phi133[ijk]) + 
       2*invh02*nvec0*Phi100[ijk]*Phi212[ijk] + 
       2*invh12*nvec0*Phi101[ijk]*Phi212[ijk] + 
       2*invh02*nvec1*Phi101[ijk]*Phi212[ijk] + 
       2*invh22*nvec0*Phi102[ijk]*Phi212[ijk] + 
       2*invh02*nvec2*Phi102[ijk]*Phi212[ijk] + 
       2*invh23*nvec0*Phi103[ijk]*Phi212[ijk] + 
       2*invh02*nvec3*Phi103[ijk]*Phi212[ijk] + 
       2*invh12*nvec1*Phi111[ijk]*Phi212[ijk] + 
       2*invh23*nvec1*Phi113[ijk]*Phi212[ijk] + 
       2*invh12*nvec3*Phi113[ijk]*Phi212[ijk] + 
       2*invh22*nvec2*Phi122[ijk]*Phi212[ijk] + 
       2*invh23*nvec2*Phi123[ijk]*Phi212[ijk] + 
       2*invh22*nvec3*Phi123[ijk]*Phi212[ijk] + 
       2*invh23*nvec3*Phi133[ijk]*Phi212[ijk] + 
       2*invh03*nvec0*Phi100[ijk]*Phi312[ijk] + 
       2*invh13*nvec0*Phi101[ijk]*Phi312[ijk] + 
       2*invh03*nvec1*Phi101[ijk]*Phi312[ijk] + 
       2*invh23*nvec0*Phi102[ijk]*Phi312[ijk] + 
       2*invh03*nvec2*Phi102[ijk]*Phi312[ijk] + 
       2*invh33*nvec0*Phi103[ijk]*Phi312[ijk] + 
       2*invh03*nvec3*Phi103[ijk]*Phi312[ijk] + 
       2*invh13*nvec1*Phi111[ijk]*Phi312[ijk] + 
       2*invh33*nvec1*Phi113[ijk]*Phi312[ijk] + 
       2*invh13*nvec3*Phi113[ijk]*Phi312[ijk] + 
       2*invh23*nvec2*Phi122[ijk]*Phi312[ijk] + 
       2*invh33*nvec2*Phi123[ijk]*Phi312[ijk] + 
       2*invh23*nvec3*Phi123[ijk]*Phi312[ijk] + 
       2*invh33*nvec3*Phi133[ijk]*Phi312[ijk] + 
       Power(nvec0,2)*Phi100[ijk]*Pi12[ijk] + 
       2*nvec0*nvec1*Phi101[ijk]*Pi12[ijk] + 
       2*nvec0*nvec2*Phi102[ijk]*Pi12[ijk] + 
       2*nvec0*nvec3*Phi103[ijk]*Pi12[ijk] + 
       Power(nvec1,2)*Phi111[ijk]*Pi12[ijk] + 
       2*nvec1*nvec3*Phi113[ijk]*Pi12[ijk] + 
       Power(nvec2,2)*Phi122[ijk]*Pi12[ijk] + 
       2*nvec2*nvec3*Phi123[ijk]*Pi12[ijk] + 
       Power(nvec3,2)*Phi133[ijk]*Pi12[ijk] + 
       2*Phi112[ijk]*(-gamma2 + invh01*nvec0*Phi100[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi101[ijk] + 
          invh12*nvec0*Phi102[ijk] + invh01*nvec2*Phi102[ijk] + 
          invh13*nvec0*Phi103[ijk] + invh01*nvec3*Phi103[ijk] + 
          invh11*nvec1*Phi111[ijk] + invh13*nvec1*Phi113[ijk] + 
          invh11*nvec3*Phi113[ijk] + invh12*nvec2*Phi122[ijk] + 
          invh13*nvec2*Phi123[ijk] + invh12*nvec3*Phi123[ijk] + 
          invh13*nvec3*Phi133[ijk] + invh22*nvec1*Phi212[ijk] + 
          invh12*nvec2*Phi212[ijk] + invh23*nvec1*Phi312[ijk] + 
          invh13*nvec2*Phi312[ijk] + nvec1*nvec2*Pi12[ijk])))/2.
;

dtPhi113[ijk]
=
-(interior*AdPhi113[ijk]) + (alpha[ijk]*
     (2*(invh13*nvec1 + invh11*nvec3)*Power(Phi113[ijk],2) + 
       2*Phi013[ijk]*(invh00*nvec0*Phi100[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi101[ijk] + 
          invh02*nvec0*Phi102[ijk] + invh00*nvec2*Phi102[ijk] + 
          invh03*nvec0*Phi103[ijk] + invh00*nvec3*Phi103[ijk] + 
          invh01*nvec1*Phi111[ijk] + invh02*nvec1*Phi112[ijk] + 
          invh01*nvec2*Phi112[ijk] + invh03*nvec1*Phi113[ijk] + 
          invh01*nvec3*Phi113[ijk] + invh02*nvec2*Phi122[ijk] + 
          invh03*nvec2*Phi123[ijk] + invh02*nvec3*Phi123[ijk] + 
          invh03*nvec3*Phi133[ijk]) + 
       2*invh02*nvec0*Phi100[ijk]*Phi213[ijk] + 
       2*invh12*nvec0*Phi101[ijk]*Phi213[ijk] + 
       2*invh02*nvec1*Phi101[ijk]*Phi213[ijk] + 
       2*invh22*nvec0*Phi102[ijk]*Phi213[ijk] + 
       2*invh02*nvec2*Phi102[ijk]*Phi213[ijk] + 
       2*invh23*nvec0*Phi103[ijk]*Phi213[ijk] + 
       2*invh02*nvec3*Phi103[ijk]*Phi213[ijk] + 
       2*invh12*nvec1*Phi111[ijk]*Phi213[ijk] + 
       2*invh22*nvec1*Phi112[ijk]*Phi213[ijk] + 
       2*invh12*nvec2*Phi112[ijk]*Phi213[ijk] + 
       2*invh22*nvec2*Phi122[ijk]*Phi213[ijk] + 
       2*invh23*nvec2*Phi123[ijk]*Phi213[ijk] + 
       2*invh22*nvec3*Phi123[ijk]*Phi213[ijk] + 
       2*invh23*nvec3*Phi133[ijk]*Phi213[ijk] + 
       2*invh03*nvec0*Phi100[ijk]*Phi313[ijk] + 
       2*invh13*nvec0*Phi101[ijk]*Phi313[ijk] + 
       2*invh03*nvec1*Phi101[ijk]*Phi313[ijk] + 
       2*invh23*nvec0*Phi102[ijk]*Phi313[ijk] + 
       2*invh03*nvec2*Phi102[ijk]*Phi313[ijk] + 
       2*invh33*nvec0*Phi103[ijk]*Phi313[ijk] + 
       2*invh03*nvec3*Phi103[ijk]*Phi313[ijk] + 
       2*invh13*nvec1*Phi111[ijk]*Phi313[ijk] + 
       2*invh23*nvec1*Phi112[ijk]*Phi313[ijk] + 
       2*invh13*nvec2*Phi112[ijk]*Phi313[ijk] + 
       2*invh23*nvec2*Phi122[ijk]*Phi313[ijk] + 
       2*invh33*nvec2*Phi123[ijk]*Phi313[ijk] + 
       2*invh23*nvec3*Phi123[ijk]*Phi313[ijk] + 
       2*invh33*nvec3*Phi133[ijk]*Phi313[ijk] + 
       Power(nvec0,2)*Phi100[ijk]*Pi13[ijk] + 
       2*nvec0*nvec1*Phi101[ijk]*Pi13[ijk] + 
       2*nvec0*nvec2*Phi102[ijk]*Pi13[ijk] + 
       2*nvec0*nvec3*Phi103[ijk]*Pi13[ijk] + 
       Power(nvec1,2)*Phi111[ijk]*Pi13[ijk] + 
       2*nvec1*nvec2*Phi112[ijk]*Pi13[ijk] + 
       Power(nvec2,2)*Phi122[ijk]*Pi13[ijk] + 
       2*nvec2*nvec3*Phi123[ijk]*Pi13[ijk] + 
       Power(nvec3,2)*Phi133[ijk]*Pi13[ijk] + 
       2*Phi113[ijk]*(-gamma2 + invh01*nvec0*Phi100[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi101[ijk] + 
          invh12*nvec0*Phi102[ijk] + invh01*nvec2*Phi102[ijk] + 
          invh13*nvec0*Phi103[ijk] + invh01*nvec3*Phi103[ijk] + 
          invh11*nvec1*Phi111[ijk] + invh12*nvec1*Phi112[ijk] + 
          invh11*nvec2*Phi112[ijk] + invh12*nvec2*Phi122[ijk] + 
          invh13*nvec2*Phi123[ijk] + invh12*nvec3*Phi123[ijk] + 
          invh13*nvec3*Phi133[ijk] + invh23*nvec1*Phi213[ijk] + 
          invh12*nvec3*Phi213[ijk] + invh33*nvec1*Phi313[ijk] + 
          invh13*nvec3*Phi313[ijk] + nvec1*nvec3*Pi13[ijk])))/2.
;

dtPhi122[ijk]
=
-(interior*AdPhi122[ijk]) + (alpha[ijk]*
     (2*invh12*nvec2*Power(Phi122[ijk],2) + 
       2*Phi022[ijk]*(invh00*nvec0*Phi100[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi101[ijk] + 
          invh02*nvec0*Phi102[ijk] + invh00*nvec2*Phi102[ijk] + 
          invh03*nvec0*Phi103[ijk] + invh00*nvec3*Phi103[ijk] + 
          invh01*nvec1*Phi111[ijk] + invh02*nvec1*Phi112[ijk] + 
          invh01*nvec2*Phi112[ijk] + invh03*nvec1*Phi113[ijk] + 
          invh01*nvec3*Phi113[ijk] + invh02*nvec2*Phi122[ijk] + 
          invh03*nvec2*Phi123[ijk] + invh02*nvec3*Phi123[ijk] + 
          invh03*nvec3*Phi133[ijk]) + 
       2*invh02*nvec0*Phi100[ijk]*Phi222[ijk] + 
       2*invh12*nvec0*Phi101[ijk]*Phi222[ijk] + 
       2*invh02*nvec1*Phi101[ijk]*Phi222[ijk] + 
       2*invh22*nvec0*Phi102[ijk]*Phi222[ijk] + 
       2*invh02*nvec2*Phi102[ijk]*Phi222[ijk] + 
       2*invh23*nvec0*Phi103[ijk]*Phi222[ijk] + 
       2*invh02*nvec3*Phi103[ijk]*Phi222[ijk] + 
       2*invh12*nvec1*Phi111[ijk]*Phi222[ijk] + 
       2*invh22*nvec1*Phi112[ijk]*Phi222[ijk] + 
       2*invh12*nvec2*Phi112[ijk]*Phi222[ijk] + 
       2*invh23*nvec1*Phi113[ijk]*Phi222[ijk] + 
       2*invh12*nvec3*Phi113[ijk]*Phi222[ijk] + 
       2*invh23*nvec2*Phi123[ijk]*Phi222[ijk] + 
       2*invh22*nvec3*Phi123[ijk]*Phi222[ijk] + 
       2*invh23*nvec3*Phi133[ijk]*Phi222[ijk] + 
       2*invh03*nvec0*Phi100[ijk]*Phi322[ijk] + 
       2*invh13*nvec0*Phi101[ijk]*Phi322[ijk] + 
       2*invh03*nvec1*Phi101[ijk]*Phi322[ijk] + 
       2*invh23*nvec0*Phi102[ijk]*Phi322[ijk] + 
       2*invh03*nvec2*Phi102[ijk]*Phi322[ijk] + 
       2*invh33*nvec0*Phi103[ijk]*Phi322[ijk] + 
       2*invh03*nvec3*Phi103[ijk]*Phi322[ijk] + 
       2*invh13*nvec1*Phi111[ijk]*Phi322[ijk] + 
       2*invh23*nvec1*Phi112[ijk]*Phi322[ijk] + 
       2*invh13*nvec2*Phi112[ijk]*Phi322[ijk] + 
       2*invh33*nvec1*Phi113[ijk]*Phi322[ijk] + 
       2*invh13*nvec3*Phi113[ijk]*Phi322[ijk] + 
       2*invh33*nvec2*Phi123[ijk]*Phi322[ijk] + 
       2*invh23*nvec3*Phi123[ijk]*Phi322[ijk] + 
       2*invh33*nvec3*Phi133[ijk]*Phi322[ijk] + 
       Power(nvec0,2)*Phi100[ijk]*Pi22[ijk] + 
       2*nvec0*nvec1*Phi101[ijk]*Pi22[ijk] + 
       2*nvec0*nvec2*Phi102[ijk]*Pi22[ijk] + 
       2*nvec0*nvec3*Phi103[ijk]*Pi22[ijk] + 
       Power(nvec1,2)*Phi111[ijk]*Pi22[ijk] + 
       2*nvec1*nvec2*Phi112[ijk]*Pi22[ijk] + 
       2*nvec1*nvec3*Phi113[ijk]*Pi22[ijk] + 
       2*nvec2*nvec3*Phi123[ijk]*Pi22[ijk] + 
       Power(nvec3,2)*Phi133[ijk]*Pi22[ijk] + 
       Phi122[ijk]*(-2*gamma2 + 2*invh01*nvec0*Phi100[ijk] + 
          2*(invh11*nvec0 + invh01*nvec1)*Phi101[ijk] + 
          2*invh12*nvec0*Phi102[ijk] + 2*invh01*nvec2*Phi102[ijk] + 
          2*invh13*nvec0*Phi103[ijk] + 2*invh01*nvec3*Phi103[ijk] + 
          2*invh11*nvec1*Phi111[ijk] + 2*invh12*nvec1*Phi112[ijk] + 
          2*invh11*nvec2*Phi112[ijk] + 2*invh13*nvec1*Phi113[ijk] + 
          2*invh11*nvec3*Phi113[ijk] + 2*invh13*nvec2*Phi123[ijk] + 
          2*invh12*nvec3*Phi123[ijk] + 2*invh13*nvec3*Phi133[ijk] + 
          2*invh22*nvec2*Phi222[ijk] + 2*invh23*nvec2*Phi322[ijk] + 
          Power(nvec2,2)*Pi22[ijk])))/2.
;

dtPhi123[ijk]
=
-(interior*AdPhi123[ijk]) + (alpha[ijk]*
     (2*(invh13*nvec2 + invh12*nvec3)*Power(Phi123[ijk],2) + 
       2*Phi023[ijk]*(invh00*nvec0*Phi100[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi101[ijk] + 
          invh02*nvec0*Phi102[ijk] + invh00*nvec2*Phi102[ijk] + 
          invh03*nvec0*Phi103[ijk] + invh00*nvec3*Phi103[ijk] + 
          invh01*nvec1*Phi111[ijk] + invh02*nvec1*Phi112[ijk] + 
          invh01*nvec2*Phi112[ijk] + invh03*nvec1*Phi113[ijk] + 
          invh01*nvec3*Phi113[ijk] + invh02*nvec2*Phi122[ijk] + 
          invh03*nvec2*Phi123[ijk] + invh02*nvec3*Phi123[ijk] + 
          invh03*nvec3*Phi133[ijk]) + 
       2*invh02*nvec0*Phi100[ijk]*Phi223[ijk] + 
       2*invh12*nvec0*Phi101[ijk]*Phi223[ijk] + 
       2*invh02*nvec1*Phi101[ijk]*Phi223[ijk] + 
       2*invh22*nvec0*Phi102[ijk]*Phi223[ijk] + 
       2*invh02*nvec2*Phi102[ijk]*Phi223[ijk] + 
       2*invh23*nvec0*Phi103[ijk]*Phi223[ijk] + 
       2*invh02*nvec3*Phi103[ijk]*Phi223[ijk] + 
       2*invh12*nvec1*Phi111[ijk]*Phi223[ijk] + 
       2*invh22*nvec1*Phi112[ijk]*Phi223[ijk] + 
       2*invh12*nvec2*Phi112[ijk]*Phi223[ijk] + 
       2*invh23*nvec1*Phi113[ijk]*Phi223[ijk] + 
       2*invh12*nvec3*Phi113[ijk]*Phi223[ijk] + 
       2*invh22*nvec2*Phi122[ijk]*Phi223[ijk] + 
       2*invh23*nvec3*Phi133[ijk]*Phi223[ijk] + 
       2*invh03*nvec0*Phi100[ijk]*Phi323[ijk] + 
       2*invh13*nvec0*Phi101[ijk]*Phi323[ijk] + 
       2*invh03*nvec1*Phi101[ijk]*Phi323[ijk] + 
       2*invh23*nvec0*Phi102[ijk]*Phi323[ijk] + 
       2*invh03*nvec2*Phi102[ijk]*Phi323[ijk] + 
       2*invh33*nvec0*Phi103[ijk]*Phi323[ijk] + 
       2*invh03*nvec3*Phi103[ijk]*Phi323[ijk] + 
       2*invh13*nvec1*Phi111[ijk]*Phi323[ijk] + 
       2*invh23*nvec1*Phi112[ijk]*Phi323[ijk] + 
       2*invh13*nvec2*Phi112[ijk]*Phi323[ijk] + 
       2*invh33*nvec1*Phi113[ijk]*Phi323[ijk] + 
       2*invh13*nvec3*Phi113[ijk]*Phi323[ijk] + 
       2*invh23*nvec2*Phi122[ijk]*Phi323[ijk] + 
       2*invh33*nvec3*Phi133[ijk]*Phi323[ijk] + 
       Power(nvec0,2)*Phi100[ijk]*Pi23[ijk] + 
       2*nvec0*nvec1*Phi101[ijk]*Pi23[ijk] + 
       2*nvec0*nvec2*Phi102[ijk]*Pi23[ijk] + 
       2*nvec0*nvec3*Phi103[ijk]*Pi23[ijk] + 
       Power(nvec1,2)*Phi111[ijk]*Pi23[ijk] + 
       2*nvec1*nvec2*Phi112[ijk]*Pi23[ijk] + 
       2*nvec1*nvec3*Phi113[ijk]*Pi23[ijk] + 
       Power(nvec2,2)*Phi122[ijk]*Pi23[ijk] + 
       Power(nvec3,2)*Phi133[ijk]*Pi23[ijk] + 
       2*Phi123[ijk]*(-gamma2 + invh01*nvec0*Phi100[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi101[ijk] + 
          invh12*nvec0*Phi102[ijk] + invh01*nvec2*Phi102[ijk] + 
          invh13*nvec0*Phi103[ijk] + invh01*nvec3*Phi103[ijk] + 
          invh11*nvec1*Phi111[ijk] + invh12*nvec1*Phi112[ijk] + 
          invh11*nvec2*Phi112[ijk] + invh13*nvec1*Phi113[ijk] + 
          invh11*nvec3*Phi113[ijk] + invh12*nvec2*Phi122[ijk] + 
          invh13*nvec3*Phi133[ijk] + invh23*nvec2*Phi223[ijk] + 
          invh22*nvec3*Phi223[ijk] + invh33*nvec2*Phi323[ijk] + 
          invh23*nvec3*Phi323[ijk] + nvec2*nvec3*Pi23[ijk])))/2.
;

dtPhi133[ijk]
=
-(interior*AdPhi133[ijk]) + (alpha[ijk]*
     (2*invh13*nvec3*Power(Phi133[ijk],2) + 
       2*Phi033[ijk]*(invh00*nvec0*Phi100[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi101[ijk] + 
          invh02*nvec0*Phi102[ijk] + invh00*nvec2*Phi102[ijk] + 
          invh03*nvec0*Phi103[ijk] + invh00*nvec3*Phi103[ijk] + 
          invh01*nvec1*Phi111[ijk] + invh02*nvec1*Phi112[ijk] + 
          invh01*nvec2*Phi112[ijk] + invh03*nvec1*Phi113[ijk] + 
          invh01*nvec3*Phi113[ijk] + invh02*nvec2*Phi122[ijk] + 
          invh03*nvec2*Phi123[ijk] + invh02*nvec3*Phi123[ijk] + 
          invh03*nvec3*Phi133[ijk]) + 
       2*invh02*nvec0*Phi100[ijk]*Phi233[ijk] + 
       2*invh12*nvec0*Phi101[ijk]*Phi233[ijk] + 
       2*invh02*nvec1*Phi101[ijk]*Phi233[ijk] + 
       2*invh22*nvec0*Phi102[ijk]*Phi233[ijk] + 
       2*invh02*nvec2*Phi102[ijk]*Phi233[ijk] + 
       2*invh23*nvec0*Phi103[ijk]*Phi233[ijk] + 
       2*invh02*nvec3*Phi103[ijk]*Phi233[ijk] + 
       2*invh12*nvec1*Phi111[ijk]*Phi233[ijk] + 
       2*invh22*nvec1*Phi112[ijk]*Phi233[ijk] + 
       2*invh12*nvec2*Phi112[ijk]*Phi233[ijk] + 
       2*invh23*nvec1*Phi113[ijk]*Phi233[ijk] + 
       2*invh12*nvec3*Phi113[ijk]*Phi233[ijk] + 
       2*invh22*nvec2*Phi122[ijk]*Phi233[ijk] + 
       2*invh23*nvec2*Phi123[ijk]*Phi233[ijk] + 
       2*invh22*nvec3*Phi123[ijk]*Phi233[ijk] + 
       2*invh03*nvec0*Phi100[ijk]*Phi333[ijk] + 
       2*invh13*nvec0*Phi101[ijk]*Phi333[ijk] + 
       2*invh03*nvec1*Phi101[ijk]*Phi333[ijk] + 
       2*invh23*nvec0*Phi102[ijk]*Phi333[ijk] + 
       2*invh03*nvec2*Phi102[ijk]*Phi333[ijk] + 
       2*invh33*nvec0*Phi103[ijk]*Phi333[ijk] + 
       2*invh03*nvec3*Phi103[ijk]*Phi333[ijk] + 
       2*invh13*nvec1*Phi111[ijk]*Phi333[ijk] + 
       2*invh23*nvec1*Phi112[ijk]*Phi333[ijk] + 
       2*invh13*nvec2*Phi112[ijk]*Phi333[ijk] + 
       2*invh33*nvec1*Phi113[ijk]*Phi333[ijk] + 
       2*invh13*nvec3*Phi113[ijk]*Phi333[ijk] + 
       2*invh23*nvec2*Phi122[ijk]*Phi333[ijk] + 
       2*invh33*nvec2*Phi123[ijk]*Phi333[ijk] + 
       2*invh23*nvec3*Phi123[ijk]*Phi333[ijk] + 
       Power(nvec0,2)*Phi100[ijk]*Pi33[ijk] + 
       2*nvec0*nvec1*Phi101[ijk]*Pi33[ijk] + 
       2*nvec0*nvec2*Phi102[ijk]*Pi33[ijk] + 
       2*nvec0*nvec3*Phi103[ijk]*Pi33[ijk] + 
       Power(nvec1,2)*Phi111[ijk]*Pi33[ijk] + 
       2*nvec1*nvec2*Phi112[ijk]*Pi33[ijk] + 
       2*nvec1*nvec3*Phi113[ijk]*Pi33[ijk] + 
       Power(nvec2,2)*Phi122[ijk]*Pi33[ijk] + 
       2*nvec2*nvec3*Phi123[ijk]*Pi33[ijk] + 
       Phi133[ijk]*(-2*gamma2 + 2*invh01*nvec0*Phi100[ijk] + 
          2*(invh11*nvec0 + invh01*nvec1)*Phi101[ijk] + 
          2*invh12*nvec0*Phi102[ijk] + 2*invh01*nvec2*Phi102[ijk] + 
          2*invh13*nvec0*Phi103[ijk] + 2*invh01*nvec3*Phi103[ijk] + 
          2*invh11*nvec1*Phi111[ijk] + 2*invh12*nvec1*Phi112[ijk] + 
          2*invh11*nvec2*Phi112[ijk] + 2*invh13*nvec1*Phi113[ijk] + 
          2*invh11*nvec3*Phi113[ijk] + 2*invh12*nvec2*Phi122[ijk] + 
          2*invh13*nvec2*Phi123[ijk] + 2*invh12*nvec3*Phi123[ijk] + 
          2*invh23*nvec3*Phi233[ijk] + 2*invh33*nvec3*Phi333[ijk] + 
          Power(nvec3,2)*Pi33[ijk])))/2.
;

dtPhi200[ijk]
=
-(interior*AdPhi200[ijk]) + (alpha[ijk]*
     (2*invh02*nvec0*Power(Phi200[ijk],2) + 
       2*invh11*nvec0*Phi100[ijk]*Phi201[ijk] + 
       2*invh01*nvec1*Phi100[ijk]*Phi201[ijk] + 
       2*invh12*nvec0*Phi100[ijk]*Phi202[ijk] + 
       2*invh01*nvec2*Phi100[ijk]*Phi202[ijk] + 
       2*invh13*nvec0*Phi100[ijk]*Phi203[ijk] + 
       2*invh01*nvec3*Phi100[ijk]*Phi203[ijk] + 
       2*invh11*nvec1*Phi100[ijk]*Phi211[ijk] + 
       2*invh12*nvec1*Phi100[ijk]*Phi212[ijk] + 
       2*invh11*nvec2*Phi100[ijk]*Phi212[ijk] + 
       2*invh13*nvec1*Phi100[ijk]*Phi213[ijk] + 
       2*invh11*nvec3*Phi100[ijk]*Phi213[ijk] + 
       2*invh12*nvec2*Phi100[ijk]*Phi222[ijk] + 
       2*invh13*nvec2*Phi100[ijk]*Phi223[ijk] + 
       2*invh12*nvec3*Phi100[ijk]*Phi223[ijk] + 
       2*invh13*nvec3*Phi100[ijk]*Phi233[ijk] + 
       2*Phi000[ijk]*((invh01*nvec0 + invh00*nvec1)*Phi201[ijk] + 
          (invh02*nvec0 + invh00*nvec2)*Phi202[ijk] + 
          invh03*nvec0*Phi203[ijk] + invh00*nvec3*Phi203[ijk] + 
          invh01*nvec1*Phi211[ijk] + invh02*nvec1*Phi212[ijk] + 
          invh01*nvec2*Phi212[ijk] + invh03*nvec1*Phi213[ijk] + 
          invh01*nvec3*Phi213[ijk] + invh02*nvec2*Phi222[ijk] + 
          invh03*nvec2*Phi223[ijk] + invh02*nvec3*Phi223[ijk] + 
          invh03*nvec3*Phi233[ijk]) + 
       2*invh13*nvec0*Phi201[ijk]*Phi300[ijk] + 
       2*invh03*nvec1*Phi201[ijk]*Phi300[ijk] + 
       2*invh23*nvec0*Phi202[ijk]*Phi300[ijk] + 
       2*invh03*nvec2*Phi202[ijk]*Phi300[ijk] + 
       2*invh33*nvec0*Phi203[ijk]*Phi300[ijk] + 
       2*invh03*nvec3*Phi203[ijk]*Phi300[ijk] + 
       2*invh13*nvec1*Phi211[ijk]*Phi300[ijk] + 
       2*invh23*nvec1*Phi212[ijk]*Phi300[ijk] + 
       2*invh13*nvec2*Phi212[ijk]*Phi300[ijk] + 
       2*invh33*nvec1*Phi213[ijk]*Phi300[ijk] + 
       2*invh13*nvec3*Phi213[ijk]*Phi300[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Phi300[ijk] + 
       2*invh33*nvec2*Phi223[ijk]*Phi300[ijk] + 
       2*invh23*nvec3*Phi223[ijk]*Phi300[ijk] + 
       2*invh33*nvec3*Phi233[ijk]*Phi300[ijk] + 
       2*nvec0*nvec1*Phi201[ijk]*Pi00[ijk] + 
       2*nvec0*nvec2*Phi202[ijk]*Pi00[ijk] + 
       2*nvec0*nvec3*Phi203[ijk]*Pi00[ijk] + 
       Power(nvec1,2)*Phi211[ijk]*Pi00[ijk] + 
       2*nvec1*nvec2*Phi212[ijk]*Pi00[ijk] + 
       2*nvec1*nvec3*Phi213[ijk]*Pi00[ijk] + 
       Power(nvec2,2)*Phi222[ijk]*Pi00[ijk] + 
       2*nvec2*nvec3*Phi223[ijk]*Pi00[ijk] + 
       Power(nvec3,2)*Phi233[ijk]*Pi00[ijk] + 
       Phi200[ijk]*(-2*gamma2 + 2*invh00*nvec0*Phi000[ijk] + 
          2*invh01*nvec0*Phi100[ijk] + 2*invh12*nvec0*Phi201[ijk] + 
          2*invh02*nvec1*Phi201[ijk] + 2*invh22*nvec0*Phi202[ijk] + 
          2*invh02*nvec2*Phi202[ijk] + 2*invh23*nvec0*Phi203[ijk] + 
          2*invh02*nvec3*Phi203[ijk] + 2*invh12*nvec1*Phi211[ijk] + 
          2*invh22*nvec1*Phi212[ijk] + 2*invh12*nvec2*Phi212[ijk] + 
          2*invh23*nvec1*Phi213[ijk] + 2*invh12*nvec3*Phi213[ijk] + 
          2*invh22*nvec2*Phi222[ijk] + 2*invh23*nvec2*Phi223[ijk] + 
          2*invh22*nvec3*Phi223[ijk] + 2*invh23*nvec3*Phi233[ijk] + 
          2*invh03*nvec0*Phi300[ijk] + Power(nvec0,2)*Pi00[ijk])))/2.
;

dtPhi201[ijk]
=
-(interior*AdPhi201[ijk]) + (alpha[ijk]*
     (-2*gamma2*Phi201[ijk] + 2*invh02*nvec0*Phi200[ijk]*Phi201[ijk] + 
       2*invh12*nvec0*Power(Phi201[ijk],2) + 
       2*invh02*nvec1*Power(Phi201[ijk],2) + 
       2*invh22*nvec0*Phi201[ijk]*Phi202[ijk] + 
       2*invh02*nvec2*Phi201[ijk]*Phi202[ijk] + 
       2*invh23*nvec0*Phi201[ijk]*Phi203[ijk] + 
       2*invh02*nvec3*Phi201[ijk]*Phi203[ijk] + 
       2*invh12*nvec1*Phi201[ijk]*Phi211[ijk] + 
       2*invh22*nvec1*Phi201[ijk]*Phi212[ijk] + 
       2*invh12*nvec2*Phi201[ijk]*Phi212[ijk] + 
       2*invh23*nvec1*Phi201[ijk]*Phi213[ijk] + 
       2*invh12*nvec3*Phi201[ijk]*Phi213[ijk] + 
       2*invh22*nvec2*Phi201[ijk]*Phi222[ijk] + 
       2*invh23*nvec2*Phi201[ijk]*Phi223[ijk] + 
       2*invh22*nvec3*Phi201[ijk]*Phi223[ijk] + 
       2*invh23*nvec3*Phi201[ijk]*Phi233[ijk] + 
       2*Phi001[ijk]*(invh00*nvec0*Phi200[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi201[ijk] + 
          invh02*nvec0*Phi202[ijk] + invh00*nvec2*Phi202[ijk] + 
          invh03*nvec0*Phi203[ijk] + invh00*nvec3*Phi203[ijk] + 
          invh01*nvec1*Phi211[ijk] + invh02*nvec1*Phi212[ijk] + 
          invh01*nvec2*Phi212[ijk] + invh03*nvec1*Phi213[ijk] + 
          invh01*nvec3*Phi213[ijk] + invh02*nvec2*Phi222[ijk] + 
          invh03*nvec2*Phi223[ijk] + invh02*nvec3*Phi223[ijk] + 
          invh03*nvec3*Phi233[ijk]) + 
       2*Phi101[ijk]*(invh01*nvec0*Phi200[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi201[ijk] + 
          invh12*nvec0*Phi202[ijk] + invh01*nvec2*Phi202[ijk] + 
          invh13*nvec0*Phi203[ijk] + invh01*nvec3*Phi203[ijk] + 
          invh11*nvec1*Phi211[ijk] + invh12*nvec1*Phi212[ijk] + 
          invh11*nvec2*Phi212[ijk] + invh13*nvec1*Phi213[ijk] + 
          invh11*nvec3*Phi213[ijk] + invh12*nvec2*Phi222[ijk] + 
          invh13*nvec2*Phi223[ijk] + invh12*nvec3*Phi223[ijk] + 
          invh13*nvec3*Phi233[ijk]) + 
       2*invh03*nvec0*Phi200[ijk]*Phi301[ijk] + 
       2*invh13*nvec0*Phi201[ijk]*Phi301[ijk] + 
       2*invh03*nvec1*Phi201[ijk]*Phi301[ijk] + 
       2*invh23*nvec0*Phi202[ijk]*Phi301[ijk] + 
       2*invh03*nvec2*Phi202[ijk]*Phi301[ijk] + 
       2*invh33*nvec0*Phi203[ijk]*Phi301[ijk] + 
       2*invh03*nvec3*Phi203[ijk]*Phi301[ijk] + 
       2*invh13*nvec1*Phi211[ijk]*Phi301[ijk] + 
       2*invh23*nvec1*Phi212[ijk]*Phi301[ijk] + 
       2*invh13*nvec2*Phi212[ijk]*Phi301[ijk] + 
       2*invh33*nvec1*Phi213[ijk]*Phi301[ijk] + 
       2*invh13*nvec3*Phi213[ijk]*Phi301[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Phi301[ijk] + 
       2*invh33*nvec2*Phi223[ijk]*Phi301[ijk] + 
       2*invh23*nvec3*Phi223[ijk]*Phi301[ijk] + 
       2*invh33*nvec3*Phi233[ijk]*Phi301[ijk] + 
       Power(nvec0,2)*Phi200[ijk]*Pi01[ijk] + 
       2*nvec0*nvec1*Phi201[ijk]*Pi01[ijk] + 
       2*nvec0*nvec2*Phi202[ijk]*Pi01[ijk] + 
       2*nvec0*nvec3*Phi203[ijk]*Pi01[ijk] + 
       Power(nvec1,2)*Phi211[ijk]*Pi01[ijk] + 
       2*nvec1*nvec2*Phi212[ijk]*Pi01[ijk] + 
       2*nvec1*nvec3*Phi213[ijk]*Pi01[ijk] + 
       Power(nvec2,2)*Phi222[ijk]*Pi01[ijk] + 
       2*nvec2*nvec3*Phi223[ijk]*Pi01[ijk] + 
       Power(nvec3,2)*Phi233[ijk]*Pi01[ijk]))/2.
;

dtPhi202[ijk]
=
-(interior*AdPhi202[ijk]) + (alpha[ijk]*
     (-2*gamma2*Phi202[ijk] + 2*invh02*nvec0*Phi200[ijk]*Phi202[ijk] + 
       2*invh12*nvec0*Phi201[ijk]*Phi202[ijk] + 
       2*invh02*nvec1*Phi201[ijk]*Phi202[ijk] + 
       2*invh22*nvec0*Power(Phi202[ijk],2) + 
       2*invh02*nvec2*Power(Phi202[ijk],2) + 
       2*invh23*nvec0*Phi202[ijk]*Phi203[ijk] + 
       2*invh02*nvec3*Phi202[ijk]*Phi203[ijk] + 
       2*invh12*nvec1*Phi202[ijk]*Phi211[ijk] + 
       2*invh22*nvec1*Phi202[ijk]*Phi212[ijk] + 
       2*invh12*nvec2*Phi202[ijk]*Phi212[ijk] + 
       2*invh23*nvec1*Phi202[ijk]*Phi213[ijk] + 
       2*invh12*nvec3*Phi202[ijk]*Phi213[ijk] + 
       2*invh22*nvec2*Phi202[ijk]*Phi222[ijk] + 
       2*invh23*nvec2*Phi202[ijk]*Phi223[ijk] + 
       2*invh22*nvec3*Phi202[ijk]*Phi223[ijk] + 
       2*invh23*nvec3*Phi202[ijk]*Phi233[ijk] + 
       2*Phi002[ijk]*(invh00*nvec0*Phi200[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi201[ijk] + 
          invh02*nvec0*Phi202[ijk] + invh00*nvec2*Phi202[ijk] + 
          invh03*nvec0*Phi203[ijk] + invh00*nvec3*Phi203[ijk] + 
          invh01*nvec1*Phi211[ijk] + invh02*nvec1*Phi212[ijk] + 
          invh01*nvec2*Phi212[ijk] + invh03*nvec1*Phi213[ijk] + 
          invh01*nvec3*Phi213[ijk] + invh02*nvec2*Phi222[ijk] + 
          invh03*nvec2*Phi223[ijk] + invh02*nvec3*Phi223[ijk] + 
          invh03*nvec3*Phi233[ijk]) + 
       2*Phi102[ijk]*(invh01*nvec0*Phi200[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi201[ijk] + 
          invh12*nvec0*Phi202[ijk] + invh01*nvec2*Phi202[ijk] + 
          invh13*nvec0*Phi203[ijk] + invh01*nvec3*Phi203[ijk] + 
          invh11*nvec1*Phi211[ijk] + invh12*nvec1*Phi212[ijk] + 
          invh11*nvec2*Phi212[ijk] + invh13*nvec1*Phi213[ijk] + 
          invh11*nvec3*Phi213[ijk] + invh12*nvec2*Phi222[ijk] + 
          invh13*nvec2*Phi223[ijk] + invh12*nvec3*Phi223[ijk] + 
          invh13*nvec3*Phi233[ijk]) + 
       2*invh03*nvec0*Phi200[ijk]*Phi302[ijk] + 
       2*invh13*nvec0*Phi201[ijk]*Phi302[ijk] + 
       2*invh03*nvec1*Phi201[ijk]*Phi302[ijk] + 
       2*invh23*nvec0*Phi202[ijk]*Phi302[ijk] + 
       2*invh03*nvec2*Phi202[ijk]*Phi302[ijk] + 
       2*invh33*nvec0*Phi203[ijk]*Phi302[ijk] + 
       2*invh03*nvec3*Phi203[ijk]*Phi302[ijk] + 
       2*invh13*nvec1*Phi211[ijk]*Phi302[ijk] + 
       2*invh23*nvec1*Phi212[ijk]*Phi302[ijk] + 
       2*invh13*nvec2*Phi212[ijk]*Phi302[ijk] + 
       2*invh33*nvec1*Phi213[ijk]*Phi302[ijk] + 
       2*invh13*nvec3*Phi213[ijk]*Phi302[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Phi302[ijk] + 
       2*invh33*nvec2*Phi223[ijk]*Phi302[ijk] + 
       2*invh23*nvec3*Phi223[ijk]*Phi302[ijk] + 
       2*invh33*nvec3*Phi233[ijk]*Phi302[ijk] + 
       Power(nvec0,2)*Phi200[ijk]*Pi02[ijk] + 
       2*nvec0*nvec1*Phi201[ijk]*Pi02[ijk] + 
       2*nvec0*nvec2*Phi202[ijk]*Pi02[ijk] + 
       2*nvec0*nvec3*Phi203[ijk]*Pi02[ijk] + 
       Power(nvec1,2)*Phi211[ijk]*Pi02[ijk] + 
       2*nvec1*nvec2*Phi212[ijk]*Pi02[ijk] + 
       2*nvec1*nvec3*Phi213[ijk]*Pi02[ijk] + 
       Power(nvec2,2)*Phi222[ijk]*Pi02[ijk] + 
       2*nvec2*nvec3*Phi223[ijk]*Pi02[ijk] + 
       Power(nvec3,2)*Phi233[ijk]*Pi02[ijk]))/2.
;

dtPhi203[ijk]
=
-(interior*AdPhi203[ijk]) + (alpha[ijk]*
     (-2*gamma2*Phi203[ijk] + 2*invh02*nvec0*Phi200[ijk]*Phi203[ijk] + 
       2*invh12*nvec0*Phi201[ijk]*Phi203[ijk] + 
       2*invh02*nvec1*Phi201[ijk]*Phi203[ijk] + 
       2*invh22*nvec0*Phi202[ijk]*Phi203[ijk] + 
       2*invh02*nvec2*Phi202[ijk]*Phi203[ijk] + 
       2*invh23*nvec0*Power(Phi203[ijk],2) + 
       2*invh02*nvec3*Power(Phi203[ijk],2) + 
       2*invh12*nvec1*Phi203[ijk]*Phi211[ijk] + 
       2*invh22*nvec1*Phi203[ijk]*Phi212[ijk] + 
       2*invh12*nvec2*Phi203[ijk]*Phi212[ijk] + 
       2*invh23*nvec1*Phi203[ijk]*Phi213[ijk] + 
       2*invh12*nvec3*Phi203[ijk]*Phi213[ijk] + 
       2*invh22*nvec2*Phi203[ijk]*Phi222[ijk] + 
       2*invh23*nvec2*Phi203[ijk]*Phi223[ijk] + 
       2*invh22*nvec3*Phi203[ijk]*Phi223[ijk] + 
       2*invh23*nvec3*Phi203[ijk]*Phi233[ijk] + 
       2*Phi003[ijk]*(invh00*nvec0*Phi200[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi201[ijk] + 
          invh02*nvec0*Phi202[ijk] + invh00*nvec2*Phi202[ijk] + 
          invh03*nvec0*Phi203[ijk] + invh00*nvec3*Phi203[ijk] + 
          invh01*nvec1*Phi211[ijk] + invh02*nvec1*Phi212[ijk] + 
          invh01*nvec2*Phi212[ijk] + invh03*nvec1*Phi213[ijk] + 
          invh01*nvec3*Phi213[ijk] + invh02*nvec2*Phi222[ijk] + 
          invh03*nvec2*Phi223[ijk] + invh02*nvec3*Phi223[ijk] + 
          invh03*nvec3*Phi233[ijk]) + 
       2*Phi103[ijk]*(invh01*nvec0*Phi200[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi201[ijk] + 
          invh12*nvec0*Phi202[ijk] + invh01*nvec2*Phi202[ijk] + 
          invh13*nvec0*Phi203[ijk] + invh01*nvec3*Phi203[ijk] + 
          invh11*nvec1*Phi211[ijk] + invh12*nvec1*Phi212[ijk] + 
          invh11*nvec2*Phi212[ijk] + invh13*nvec1*Phi213[ijk] + 
          invh11*nvec3*Phi213[ijk] + invh12*nvec2*Phi222[ijk] + 
          invh13*nvec2*Phi223[ijk] + invh12*nvec3*Phi223[ijk] + 
          invh13*nvec3*Phi233[ijk]) + 
       2*invh03*nvec0*Phi200[ijk]*Phi303[ijk] + 
       2*invh13*nvec0*Phi201[ijk]*Phi303[ijk] + 
       2*invh03*nvec1*Phi201[ijk]*Phi303[ijk] + 
       2*invh23*nvec0*Phi202[ijk]*Phi303[ijk] + 
       2*invh03*nvec2*Phi202[ijk]*Phi303[ijk] + 
       2*invh33*nvec0*Phi203[ijk]*Phi303[ijk] + 
       2*invh03*nvec3*Phi203[ijk]*Phi303[ijk] + 
       2*invh13*nvec1*Phi211[ijk]*Phi303[ijk] + 
       2*invh23*nvec1*Phi212[ijk]*Phi303[ijk] + 
       2*invh13*nvec2*Phi212[ijk]*Phi303[ijk] + 
       2*invh33*nvec1*Phi213[ijk]*Phi303[ijk] + 
       2*invh13*nvec3*Phi213[ijk]*Phi303[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Phi303[ijk] + 
       2*invh33*nvec2*Phi223[ijk]*Phi303[ijk] + 
       2*invh23*nvec3*Phi223[ijk]*Phi303[ijk] + 
       2*invh33*nvec3*Phi233[ijk]*Phi303[ijk] + 
       Power(nvec0,2)*Phi200[ijk]*Pi03[ijk] + 
       2*nvec0*nvec1*Phi201[ijk]*Pi03[ijk] + 
       2*nvec0*nvec2*Phi202[ijk]*Pi03[ijk] + 
       2*nvec0*nvec3*Phi203[ijk]*Pi03[ijk] + 
       Power(nvec1,2)*Phi211[ijk]*Pi03[ijk] + 
       2*nvec1*nvec2*Phi212[ijk]*Pi03[ijk] + 
       2*nvec1*nvec3*Phi213[ijk]*Pi03[ijk] + 
       Power(nvec2,2)*Phi222[ijk]*Pi03[ijk] + 
       2*nvec2*nvec3*Phi223[ijk]*Pi03[ijk] + 
       Power(nvec3,2)*Phi233[ijk]*Pi03[ijk]))/2.
;

dtPhi211[ijk]
=
-(interior*AdPhi211[ijk]) + (alpha[ijk]*
     (-2*gamma2*Phi211[ijk] + 2*invh02*nvec0*Phi200[ijk]*Phi211[ijk] + 
       2*invh12*nvec0*Phi201[ijk]*Phi211[ijk] + 
       2*invh02*nvec1*Phi201[ijk]*Phi211[ijk] + 
       2*invh22*nvec0*Phi202[ijk]*Phi211[ijk] + 
       2*invh02*nvec2*Phi202[ijk]*Phi211[ijk] + 
       2*invh23*nvec0*Phi203[ijk]*Phi211[ijk] + 
       2*invh02*nvec3*Phi203[ijk]*Phi211[ijk] + 
       2*invh12*nvec1*Power(Phi211[ijk],2) + 
       2*invh22*nvec1*Phi211[ijk]*Phi212[ijk] + 
       2*invh12*nvec2*Phi211[ijk]*Phi212[ijk] + 
       2*invh23*nvec1*Phi211[ijk]*Phi213[ijk] + 
       2*invh12*nvec3*Phi211[ijk]*Phi213[ijk] + 
       2*invh22*nvec2*Phi211[ijk]*Phi222[ijk] + 
       2*invh23*nvec2*Phi211[ijk]*Phi223[ijk] + 
       2*invh22*nvec3*Phi211[ijk]*Phi223[ijk] + 
       2*invh23*nvec3*Phi211[ijk]*Phi233[ijk] + 
       2*Phi011[ijk]*(invh00*nvec0*Phi200[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi201[ijk] + 
          invh02*nvec0*Phi202[ijk] + invh00*nvec2*Phi202[ijk] + 
          invh03*nvec0*Phi203[ijk] + invh00*nvec3*Phi203[ijk] + 
          invh01*nvec1*Phi211[ijk] + invh02*nvec1*Phi212[ijk] + 
          invh01*nvec2*Phi212[ijk] + invh03*nvec1*Phi213[ijk] + 
          invh01*nvec3*Phi213[ijk] + invh02*nvec2*Phi222[ijk] + 
          invh03*nvec2*Phi223[ijk] + invh02*nvec3*Phi223[ijk] + 
          invh03*nvec3*Phi233[ijk]) + 
       2*Phi111[ijk]*(invh01*nvec0*Phi200[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi201[ijk] + 
          invh12*nvec0*Phi202[ijk] + invh01*nvec2*Phi202[ijk] + 
          invh13*nvec0*Phi203[ijk] + invh01*nvec3*Phi203[ijk] + 
          invh11*nvec1*Phi211[ijk] + invh12*nvec1*Phi212[ijk] + 
          invh11*nvec2*Phi212[ijk] + invh13*nvec1*Phi213[ijk] + 
          invh11*nvec3*Phi213[ijk] + invh12*nvec2*Phi222[ijk] + 
          invh13*nvec2*Phi223[ijk] + invh12*nvec3*Phi223[ijk] + 
          invh13*nvec3*Phi233[ijk]) + 
       2*invh03*nvec0*Phi200[ijk]*Phi311[ijk] + 
       2*invh13*nvec0*Phi201[ijk]*Phi311[ijk] + 
       2*invh03*nvec1*Phi201[ijk]*Phi311[ijk] + 
       2*invh23*nvec0*Phi202[ijk]*Phi311[ijk] + 
       2*invh03*nvec2*Phi202[ijk]*Phi311[ijk] + 
       2*invh33*nvec0*Phi203[ijk]*Phi311[ijk] + 
       2*invh03*nvec3*Phi203[ijk]*Phi311[ijk] + 
       2*invh13*nvec1*Phi211[ijk]*Phi311[ijk] + 
       2*invh23*nvec1*Phi212[ijk]*Phi311[ijk] + 
       2*invh13*nvec2*Phi212[ijk]*Phi311[ijk] + 
       2*invh33*nvec1*Phi213[ijk]*Phi311[ijk] + 
       2*invh13*nvec3*Phi213[ijk]*Phi311[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Phi311[ijk] + 
       2*invh33*nvec2*Phi223[ijk]*Phi311[ijk] + 
       2*invh23*nvec3*Phi223[ijk]*Phi311[ijk] + 
       2*invh33*nvec3*Phi233[ijk]*Phi311[ijk] + 
       Power(nvec0,2)*Phi200[ijk]*Pi11[ijk] + 
       2*nvec0*nvec1*Phi201[ijk]*Pi11[ijk] + 
       2*nvec0*nvec2*Phi202[ijk]*Pi11[ijk] + 
       2*nvec0*nvec3*Phi203[ijk]*Pi11[ijk] + 
       Power(nvec1,2)*Phi211[ijk]*Pi11[ijk] + 
       2*nvec1*nvec2*Phi212[ijk]*Pi11[ijk] + 
       2*nvec1*nvec3*Phi213[ijk]*Pi11[ijk] + 
       Power(nvec2,2)*Phi222[ijk]*Pi11[ijk] + 
       2*nvec2*nvec3*Phi223[ijk]*Pi11[ijk] + 
       Power(nvec3,2)*Phi233[ijk]*Pi11[ijk]))/2.
;

dtPhi212[ijk]
=
-(interior*AdPhi212[ijk]) + (alpha[ijk]*
     (-2*gamma2*Phi212[ijk] + 2*invh02*nvec0*Phi200[ijk]*Phi212[ijk] + 
       2*invh12*nvec0*Phi201[ijk]*Phi212[ijk] + 
       2*invh02*nvec1*Phi201[ijk]*Phi212[ijk] + 
       2*invh22*nvec0*Phi202[ijk]*Phi212[ijk] + 
       2*invh02*nvec2*Phi202[ijk]*Phi212[ijk] + 
       2*invh23*nvec0*Phi203[ijk]*Phi212[ijk] + 
       2*invh02*nvec3*Phi203[ijk]*Phi212[ijk] + 
       2*invh12*nvec1*Phi211[ijk]*Phi212[ijk] + 
       2*invh22*nvec1*Power(Phi212[ijk],2) + 
       2*invh12*nvec2*Power(Phi212[ijk],2) + 
       2*invh23*nvec1*Phi212[ijk]*Phi213[ijk] + 
       2*invh12*nvec3*Phi212[ijk]*Phi213[ijk] + 
       2*invh22*nvec2*Phi212[ijk]*Phi222[ijk] + 
       2*invh23*nvec2*Phi212[ijk]*Phi223[ijk] + 
       2*invh22*nvec3*Phi212[ijk]*Phi223[ijk] + 
       2*invh23*nvec3*Phi212[ijk]*Phi233[ijk] + 
       2*Phi012[ijk]*(invh00*nvec0*Phi200[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi201[ijk] + 
          invh02*nvec0*Phi202[ijk] + invh00*nvec2*Phi202[ijk] + 
          invh03*nvec0*Phi203[ijk] + invh00*nvec3*Phi203[ijk] + 
          invh01*nvec1*Phi211[ijk] + invh02*nvec1*Phi212[ijk] + 
          invh01*nvec2*Phi212[ijk] + invh03*nvec1*Phi213[ijk] + 
          invh01*nvec3*Phi213[ijk] + invh02*nvec2*Phi222[ijk] + 
          invh03*nvec2*Phi223[ijk] + invh02*nvec3*Phi223[ijk] + 
          invh03*nvec3*Phi233[ijk]) + 
       2*Phi112[ijk]*(invh01*nvec0*Phi200[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi201[ijk] + 
          invh12*nvec0*Phi202[ijk] + invh01*nvec2*Phi202[ijk] + 
          invh13*nvec0*Phi203[ijk] + invh01*nvec3*Phi203[ijk] + 
          invh11*nvec1*Phi211[ijk] + invh12*nvec1*Phi212[ijk] + 
          invh11*nvec2*Phi212[ijk] + invh13*nvec1*Phi213[ijk] + 
          invh11*nvec3*Phi213[ijk] + invh12*nvec2*Phi222[ijk] + 
          invh13*nvec2*Phi223[ijk] + invh12*nvec3*Phi223[ijk] + 
          invh13*nvec3*Phi233[ijk]) + 
       2*invh03*nvec0*Phi200[ijk]*Phi312[ijk] + 
       2*invh13*nvec0*Phi201[ijk]*Phi312[ijk] + 
       2*invh03*nvec1*Phi201[ijk]*Phi312[ijk] + 
       2*invh23*nvec0*Phi202[ijk]*Phi312[ijk] + 
       2*invh03*nvec2*Phi202[ijk]*Phi312[ijk] + 
       2*invh33*nvec0*Phi203[ijk]*Phi312[ijk] + 
       2*invh03*nvec3*Phi203[ijk]*Phi312[ijk] + 
       2*invh13*nvec1*Phi211[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi212[ijk]*Phi312[ijk] + 
       2*invh13*nvec2*Phi212[ijk]*Phi312[ijk] + 
       2*invh33*nvec1*Phi213[ijk]*Phi312[ijk] + 
       2*invh13*nvec3*Phi213[ijk]*Phi312[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Phi312[ijk] + 
       2*invh33*nvec2*Phi223[ijk]*Phi312[ijk] + 
       2*invh23*nvec3*Phi223[ijk]*Phi312[ijk] + 
       2*invh33*nvec3*Phi233[ijk]*Phi312[ijk] + 
       Power(nvec0,2)*Phi200[ijk]*Pi12[ijk] + 
       2*nvec0*nvec1*Phi201[ijk]*Pi12[ijk] + 
       2*nvec0*nvec2*Phi202[ijk]*Pi12[ijk] + 
       2*nvec0*nvec3*Phi203[ijk]*Pi12[ijk] + 
       Power(nvec1,2)*Phi211[ijk]*Pi12[ijk] + 
       2*nvec1*nvec2*Phi212[ijk]*Pi12[ijk] + 
       2*nvec1*nvec3*Phi213[ijk]*Pi12[ijk] + 
       Power(nvec2,2)*Phi222[ijk]*Pi12[ijk] + 
       2*nvec2*nvec3*Phi223[ijk]*Pi12[ijk] + 
       Power(nvec3,2)*Phi233[ijk]*Pi12[ijk]))/2.
;

dtPhi213[ijk]
=
-(interior*AdPhi213[ijk]) + (alpha[ijk]*
     (-2*gamma2*Phi213[ijk] + 2*invh02*nvec0*Phi200[ijk]*Phi213[ijk] + 
       2*invh12*nvec0*Phi201[ijk]*Phi213[ijk] + 
       2*invh02*nvec1*Phi201[ijk]*Phi213[ijk] + 
       2*invh22*nvec0*Phi202[ijk]*Phi213[ijk] + 
       2*invh02*nvec2*Phi202[ijk]*Phi213[ijk] + 
       2*invh23*nvec0*Phi203[ijk]*Phi213[ijk] + 
       2*invh02*nvec3*Phi203[ijk]*Phi213[ijk] + 
       2*invh12*nvec1*Phi211[ijk]*Phi213[ijk] + 
       2*invh22*nvec1*Phi212[ijk]*Phi213[ijk] + 
       2*invh12*nvec2*Phi212[ijk]*Phi213[ijk] + 
       2*invh23*nvec1*Power(Phi213[ijk],2) + 
       2*invh12*nvec3*Power(Phi213[ijk],2) + 
       2*invh22*nvec2*Phi213[ijk]*Phi222[ijk] + 
       2*invh23*nvec2*Phi213[ijk]*Phi223[ijk] + 
       2*invh22*nvec3*Phi213[ijk]*Phi223[ijk] + 
       2*invh23*nvec3*Phi213[ijk]*Phi233[ijk] + 
       2*Phi013[ijk]*(invh00*nvec0*Phi200[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi201[ijk] + 
          invh02*nvec0*Phi202[ijk] + invh00*nvec2*Phi202[ijk] + 
          invh03*nvec0*Phi203[ijk] + invh00*nvec3*Phi203[ijk] + 
          invh01*nvec1*Phi211[ijk] + invh02*nvec1*Phi212[ijk] + 
          invh01*nvec2*Phi212[ijk] + invh03*nvec1*Phi213[ijk] + 
          invh01*nvec3*Phi213[ijk] + invh02*nvec2*Phi222[ijk] + 
          invh03*nvec2*Phi223[ijk] + invh02*nvec3*Phi223[ijk] + 
          invh03*nvec3*Phi233[ijk]) + 
       2*Phi113[ijk]*(invh01*nvec0*Phi200[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi201[ijk] + 
          invh12*nvec0*Phi202[ijk] + invh01*nvec2*Phi202[ijk] + 
          invh13*nvec0*Phi203[ijk] + invh01*nvec3*Phi203[ijk] + 
          invh11*nvec1*Phi211[ijk] + invh12*nvec1*Phi212[ijk] + 
          invh11*nvec2*Phi212[ijk] + invh13*nvec1*Phi213[ijk] + 
          invh11*nvec3*Phi213[ijk] + invh12*nvec2*Phi222[ijk] + 
          invh13*nvec2*Phi223[ijk] + invh12*nvec3*Phi223[ijk] + 
          invh13*nvec3*Phi233[ijk]) + 
       2*invh03*nvec0*Phi200[ijk]*Phi313[ijk] + 
       2*invh13*nvec0*Phi201[ijk]*Phi313[ijk] + 
       2*invh03*nvec1*Phi201[ijk]*Phi313[ijk] + 
       2*invh23*nvec0*Phi202[ijk]*Phi313[ijk] + 
       2*invh03*nvec2*Phi202[ijk]*Phi313[ijk] + 
       2*invh33*nvec0*Phi203[ijk]*Phi313[ijk] + 
       2*invh03*nvec3*Phi203[ijk]*Phi313[ijk] + 
       2*invh13*nvec1*Phi211[ijk]*Phi313[ijk] + 
       2*invh23*nvec1*Phi212[ijk]*Phi313[ijk] + 
       2*invh13*nvec2*Phi212[ijk]*Phi313[ijk] + 
       2*invh33*nvec1*Phi213[ijk]*Phi313[ijk] + 
       2*invh13*nvec3*Phi213[ijk]*Phi313[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Phi313[ijk] + 
       2*invh33*nvec2*Phi223[ijk]*Phi313[ijk] + 
       2*invh23*nvec3*Phi223[ijk]*Phi313[ijk] + 
       2*invh33*nvec3*Phi233[ijk]*Phi313[ijk] + 
       Power(nvec0,2)*Phi200[ijk]*Pi13[ijk] + 
       2*nvec0*nvec1*Phi201[ijk]*Pi13[ijk] + 
       2*nvec0*nvec2*Phi202[ijk]*Pi13[ijk] + 
       2*nvec0*nvec3*Phi203[ijk]*Pi13[ijk] + 
       Power(nvec1,2)*Phi211[ijk]*Pi13[ijk] + 
       2*nvec1*nvec2*Phi212[ijk]*Pi13[ijk] + 
       2*nvec1*nvec3*Phi213[ijk]*Pi13[ijk] + 
       Power(nvec2,2)*Phi222[ijk]*Pi13[ijk] + 
       2*nvec2*nvec3*Phi223[ijk]*Pi13[ijk] + 
       Power(nvec3,2)*Phi233[ijk]*Pi13[ijk]))/2.
;

dtPhi222[ijk]
=
-(interior*AdPhi222[ijk]) + (alpha[ijk]*
     (-2*gamma2*Phi222[ijk] + 2*invh02*nvec0*Phi200[ijk]*Phi222[ijk] + 
       2*invh12*nvec0*Phi201[ijk]*Phi222[ijk] + 
       2*invh02*nvec1*Phi201[ijk]*Phi222[ijk] + 
       2*invh22*nvec0*Phi202[ijk]*Phi222[ijk] + 
       2*invh02*nvec2*Phi202[ijk]*Phi222[ijk] + 
       2*invh23*nvec0*Phi203[ijk]*Phi222[ijk] + 
       2*invh02*nvec3*Phi203[ijk]*Phi222[ijk] + 
       2*invh12*nvec1*Phi211[ijk]*Phi222[ijk] + 
       2*invh22*nvec1*Phi212[ijk]*Phi222[ijk] + 
       2*invh12*nvec2*Phi212[ijk]*Phi222[ijk] + 
       2*invh23*nvec1*Phi213[ijk]*Phi222[ijk] + 
       2*invh12*nvec3*Phi213[ijk]*Phi222[ijk] + 
       2*invh22*nvec2*Power(Phi222[ijk],2) + 
       2*invh23*nvec2*Phi222[ijk]*Phi223[ijk] + 
       2*invh22*nvec3*Phi222[ijk]*Phi223[ijk] + 
       2*invh23*nvec3*Phi222[ijk]*Phi233[ijk] + 
       2*Phi022[ijk]*(invh00*nvec0*Phi200[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi201[ijk] + 
          invh02*nvec0*Phi202[ijk] + invh00*nvec2*Phi202[ijk] + 
          invh03*nvec0*Phi203[ijk] + invh00*nvec3*Phi203[ijk] + 
          invh01*nvec1*Phi211[ijk] + invh02*nvec1*Phi212[ijk] + 
          invh01*nvec2*Phi212[ijk] + invh03*nvec1*Phi213[ijk] + 
          invh01*nvec3*Phi213[ijk] + invh02*nvec2*Phi222[ijk] + 
          invh03*nvec2*Phi223[ijk] + invh02*nvec3*Phi223[ijk] + 
          invh03*nvec3*Phi233[ijk]) + 
       2*Phi122[ijk]*(invh01*nvec0*Phi200[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi201[ijk] + 
          invh12*nvec0*Phi202[ijk] + invh01*nvec2*Phi202[ijk] + 
          invh13*nvec0*Phi203[ijk] + invh01*nvec3*Phi203[ijk] + 
          invh11*nvec1*Phi211[ijk] + invh12*nvec1*Phi212[ijk] + 
          invh11*nvec2*Phi212[ijk] + invh13*nvec1*Phi213[ijk] + 
          invh11*nvec3*Phi213[ijk] + invh12*nvec2*Phi222[ijk] + 
          invh13*nvec2*Phi223[ijk] + invh12*nvec3*Phi223[ijk] + 
          invh13*nvec3*Phi233[ijk]) + 
       2*invh03*nvec0*Phi200[ijk]*Phi322[ijk] + 
       2*invh13*nvec0*Phi201[ijk]*Phi322[ijk] + 
       2*invh03*nvec1*Phi201[ijk]*Phi322[ijk] + 
       2*invh23*nvec0*Phi202[ijk]*Phi322[ijk] + 
       2*invh03*nvec2*Phi202[ijk]*Phi322[ijk] + 
       2*invh33*nvec0*Phi203[ijk]*Phi322[ijk] + 
       2*invh03*nvec3*Phi203[ijk]*Phi322[ijk] + 
       2*invh13*nvec1*Phi211[ijk]*Phi322[ijk] + 
       2*invh23*nvec1*Phi212[ijk]*Phi322[ijk] + 
       2*invh13*nvec2*Phi212[ijk]*Phi322[ijk] + 
       2*invh33*nvec1*Phi213[ijk]*Phi322[ijk] + 
       2*invh13*nvec3*Phi213[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Phi322[ijk] + 
       2*invh33*nvec2*Phi223[ijk]*Phi322[ijk] + 
       2*invh23*nvec3*Phi223[ijk]*Phi322[ijk] + 
       2*invh33*nvec3*Phi233[ijk]*Phi322[ijk] + 
       Power(nvec0,2)*Phi200[ijk]*Pi22[ijk] + 
       2*nvec0*nvec1*Phi201[ijk]*Pi22[ijk] + 
       2*nvec0*nvec2*Phi202[ijk]*Pi22[ijk] + 
       2*nvec0*nvec3*Phi203[ijk]*Pi22[ijk] + 
       Power(nvec1,2)*Phi211[ijk]*Pi22[ijk] + 
       2*nvec1*nvec2*Phi212[ijk]*Pi22[ijk] + 
       2*nvec1*nvec3*Phi213[ijk]*Pi22[ijk] + 
       Power(nvec2,2)*Phi222[ijk]*Pi22[ijk] + 
       2*nvec2*nvec3*Phi223[ijk]*Pi22[ijk] + 
       Power(nvec3,2)*Phi233[ijk]*Pi22[ijk]))/2.
;

dtPhi223[ijk]
=
-(interior*AdPhi223[ijk]) + (alpha[ijk]*
     (-2*gamma2*Phi223[ijk] + 2*invh02*nvec0*Phi200[ijk]*Phi223[ijk] + 
       2*invh12*nvec0*Phi201[ijk]*Phi223[ijk] + 
       2*invh02*nvec1*Phi201[ijk]*Phi223[ijk] + 
       2*invh22*nvec0*Phi202[ijk]*Phi223[ijk] + 
       2*invh02*nvec2*Phi202[ijk]*Phi223[ijk] + 
       2*invh23*nvec0*Phi203[ijk]*Phi223[ijk] + 
       2*invh02*nvec3*Phi203[ijk]*Phi223[ijk] + 
       2*invh12*nvec1*Phi211[ijk]*Phi223[ijk] + 
       2*invh22*nvec1*Phi212[ijk]*Phi223[ijk] + 
       2*invh12*nvec2*Phi212[ijk]*Phi223[ijk] + 
       2*invh23*nvec1*Phi213[ijk]*Phi223[ijk] + 
       2*invh12*nvec3*Phi213[ijk]*Phi223[ijk] + 
       2*invh22*nvec2*Phi222[ijk]*Phi223[ijk] + 
       2*invh23*nvec2*Power(Phi223[ijk],2) + 
       2*invh22*nvec3*Power(Phi223[ijk],2) + 
       2*invh23*nvec3*Phi223[ijk]*Phi233[ijk] + 
       2*Phi023[ijk]*(invh00*nvec0*Phi200[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi201[ijk] + 
          invh02*nvec0*Phi202[ijk] + invh00*nvec2*Phi202[ijk] + 
          invh03*nvec0*Phi203[ijk] + invh00*nvec3*Phi203[ijk] + 
          invh01*nvec1*Phi211[ijk] + invh02*nvec1*Phi212[ijk] + 
          invh01*nvec2*Phi212[ijk] + invh03*nvec1*Phi213[ijk] + 
          invh01*nvec3*Phi213[ijk] + invh02*nvec2*Phi222[ijk] + 
          invh03*nvec2*Phi223[ijk] + invh02*nvec3*Phi223[ijk] + 
          invh03*nvec3*Phi233[ijk]) + 
       2*Phi123[ijk]*(invh01*nvec0*Phi200[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi201[ijk] + 
          invh12*nvec0*Phi202[ijk] + invh01*nvec2*Phi202[ijk] + 
          invh13*nvec0*Phi203[ijk] + invh01*nvec3*Phi203[ijk] + 
          invh11*nvec1*Phi211[ijk] + invh12*nvec1*Phi212[ijk] + 
          invh11*nvec2*Phi212[ijk] + invh13*nvec1*Phi213[ijk] + 
          invh11*nvec3*Phi213[ijk] + invh12*nvec2*Phi222[ijk] + 
          invh13*nvec2*Phi223[ijk] + invh12*nvec3*Phi223[ijk] + 
          invh13*nvec3*Phi233[ijk]) + 
       2*invh03*nvec0*Phi200[ijk]*Phi323[ijk] + 
       2*invh13*nvec0*Phi201[ijk]*Phi323[ijk] + 
       2*invh03*nvec1*Phi201[ijk]*Phi323[ijk] + 
       2*invh23*nvec0*Phi202[ijk]*Phi323[ijk] + 
       2*invh03*nvec2*Phi202[ijk]*Phi323[ijk] + 
       2*invh33*nvec0*Phi203[ijk]*Phi323[ijk] + 
       2*invh03*nvec3*Phi203[ijk]*Phi323[ijk] + 
       2*invh13*nvec1*Phi211[ijk]*Phi323[ijk] + 
       2*invh23*nvec1*Phi212[ijk]*Phi323[ijk] + 
       2*invh13*nvec2*Phi212[ijk]*Phi323[ijk] + 
       2*invh33*nvec1*Phi213[ijk]*Phi323[ijk] + 
       2*invh13*nvec3*Phi213[ijk]*Phi323[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Phi323[ijk] + 
       2*invh33*nvec2*Phi223[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi223[ijk]*Phi323[ijk] + 
       2*invh33*nvec3*Phi233[ijk]*Phi323[ijk] + 
       Power(nvec0,2)*Phi200[ijk]*Pi23[ijk] + 
       2*nvec0*nvec1*Phi201[ijk]*Pi23[ijk] + 
       2*nvec0*nvec2*Phi202[ijk]*Pi23[ijk] + 
       2*nvec0*nvec3*Phi203[ijk]*Pi23[ijk] + 
       Power(nvec1,2)*Phi211[ijk]*Pi23[ijk] + 
       2*nvec1*nvec2*Phi212[ijk]*Pi23[ijk] + 
       2*nvec1*nvec3*Phi213[ijk]*Pi23[ijk] + 
       Power(nvec2,2)*Phi222[ijk]*Pi23[ijk] + 
       2*nvec2*nvec3*Phi223[ijk]*Pi23[ijk] + 
       Power(nvec3,2)*Phi233[ijk]*Pi23[ijk]))/2.
;

dtPhi233[ijk]
=
-(interior*AdPhi233[ijk]) + (alpha[ijk]*
     (-2*gamma2*Phi233[ijk] + 2*invh02*nvec0*Phi200[ijk]*Phi233[ijk] + 
       2*invh12*nvec0*Phi201[ijk]*Phi233[ijk] + 
       2*invh02*nvec1*Phi201[ijk]*Phi233[ijk] + 
       2*invh22*nvec0*Phi202[ijk]*Phi233[ijk] + 
       2*invh02*nvec2*Phi202[ijk]*Phi233[ijk] + 
       2*invh23*nvec0*Phi203[ijk]*Phi233[ijk] + 
       2*invh02*nvec3*Phi203[ijk]*Phi233[ijk] + 
       2*invh12*nvec1*Phi211[ijk]*Phi233[ijk] + 
       2*invh22*nvec1*Phi212[ijk]*Phi233[ijk] + 
       2*invh12*nvec2*Phi212[ijk]*Phi233[ijk] + 
       2*invh23*nvec1*Phi213[ijk]*Phi233[ijk] + 
       2*invh12*nvec3*Phi213[ijk]*Phi233[ijk] + 
       2*invh22*nvec2*Phi222[ijk]*Phi233[ijk] + 
       2*invh23*nvec2*Phi223[ijk]*Phi233[ijk] + 
       2*invh22*nvec3*Phi223[ijk]*Phi233[ijk] + 
       2*invh23*nvec3*Power(Phi233[ijk],2) + 
       2*Phi033[ijk]*(invh00*nvec0*Phi200[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi201[ijk] + 
          invh02*nvec0*Phi202[ijk] + invh00*nvec2*Phi202[ijk] + 
          invh03*nvec0*Phi203[ijk] + invh00*nvec3*Phi203[ijk] + 
          invh01*nvec1*Phi211[ijk] + invh02*nvec1*Phi212[ijk] + 
          invh01*nvec2*Phi212[ijk] + invh03*nvec1*Phi213[ijk] + 
          invh01*nvec3*Phi213[ijk] + invh02*nvec2*Phi222[ijk] + 
          invh03*nvec2*Phi223[ijk] + invh02*nvec3*Phi223[ijk] + 
          invh03*nvec3*Phi233[ijk]) + 
       2*Phi133[ijk]*(invh01*nvec0*Phi200[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi201[ijk] + 
          invh12*nvec0*Phi202[ijk] + invh01*nvec2*Phi202[ijk] + 
          invh13*nvec0*Phi203[ijk] + invh01*nvec3*Phi203[ijk] + 
          invh11*nvec1*Phi211[ijk] + invh12*nvec1*Phi212[ijk] + 
          invh11*nvec2*Phi212[ijk] + invh13*nvec1*Phi213[ijk] + 
          invh11*nvec3*Phi213[ijk] + invh12*nvec2*Phi222[ijk] + 
          invh13*nvec2*Phi223[ijk] + invh12*nvec3*Phi223[ijk] + 
          invh13*nvec3*Phi233[ijk]) + 
       2*invh03*nvec0*Phi200[ijk]*Phi333[ijk] + 
       2*invh13*nvec0*Phi201[ijk]*Phi333[ijk] + 
       2*invh03*nvec1*Phi201[ijk]*Phi333[ijk] + 
       2*invh23*nvec0*Phi202[ijk]*Phi333[ijk] + 
       2*invh03*nvec2*Phi202[ijk]*Phi333[ijk] + 
       2*invh33*nvec0*Phi203[ijk]*Phi333[ijk] + 
       2*invh03*nvec3*Phi203[ijk]*Phi333[ijk] + 
       2*invh13*nvec1*Phi211[ijk]*Phi333[ijk] + 
       2*invh23*nvec1*Phi212[ijk]*Phi333[ijk] + 
       2*invh13*nvec2*Phi212[ijk]*Phi333[ijk] + 
       2*invh33*nvec1*Phi213[ijk]*Phi333[ijk] + 
       2*invh13*nvec3*Phi213[ijk]*Phi333[ijk] + 
       2*invh23*nvec2*Phi222[ijk]*Phi333[ijk] + 
       2*invh33*nvec2*Phi223[ijk]*Phi333[ijk] + 
       2*invh23*nvec3*Phi223[ijk]*Phi333[ijk] + 
       2*invh33*nvec3*Phi233[ijk]*Phi333[ijk] + 
       Power(nvec0,2)*Phi200[ijk]*Pi33[ijk] + 
       2*nvec0*nvec1*Phi201[ijk]*Pi33[ijk] + 
       2*nvec0*nvec2*Phi202[ijk]*Pi33[ijk] + 
       2*nvec0*nvec3*Phi203[ijk]*Pi33[ijk] + 
       Power(nvec1,2)*Phi211[ijk]*Pi33[ijk] + 
       2*nvec1*nvec2*Phi212[ijk]*Pi33[ijk] + 
       2*nvec1*nvec3*Phi213[ijk]*Pi33[ijk] + 
       Power(nvec2,2)*Phi222[ijk]*Pi33[ijk] + 
       2*nvec2*nvec3*Phi223[ijk]*Pi33[ijk] + 
       Power(nvec3,2)*Phi233[ijk]*Pi33[ijk]))/2.
;

dtPhi300[ijk]
=
-(interior*AdPhi300[ijk]) + (alpha[ijk]*
     (2*invh03*nvec0*Power(Phi300[ijk],2) + 
       2*invh11*nvec0*Phi100[ijk]*Phi301[ijk] + 
       2*invh01*nvec1*Phi100[ijk]*Phi301[ijk] + 
       2*invh12*nvec0*Phi200[ijk]*Phi301[ijk] + 
       2*invh02*nvec1*Phi200[ijk]*Phi301[ijk] + 
       2*invh12*nvec0*Phi100[ijk]*Phi302[ijk] + 
       2*invh01*nvec2*Phi100[ijk]*Phi302[ijk] + 
       2*invh22*nvec0*Phi200[ijk]*Phi302[ijk] + 
       2*invh02*nvec2*Phi200[ijk]*Phi302[ijk] + 
       2*invh13*nvec0*Phi100[ijk]*Phi303[ijk] + 
       2*invh01*nvec3*Phi100[ijk]*Phi303[ijk] + 
       2*invh23*nvec0*Phi200[ijk]*Phi303[ijk] + 
       2*invh02*nvec3*Phi200[ijk]*Phi303[ijk] + 
       2*invh11*nvec1*Phi100[ijk]*Phi311[ijk] + 
       2*invh12*nvec1*Phi200[ijk]*Phi311[ijk] + 
       2*invh12*nvec1*Phi100[ijk]*Phi312[ijk] + 
       2*invh11*nvec2*Phi100[ijk]*Phi312[ijk] + 
       2*invh22*nvec1*Phi200[ijk]*Phi312[ijk] + 
       2*invh12*nvec2*Phi200[ijk]*Phi312[ijk] + 
       2*invh13*nvec1*Phi100[ijk]*Phi313[ijk] + 
       2*invh11*nvec3*Phi100[ijk]*Phi313[ijk] + 
       2*invh23*nvec1*Phi200[ijk]*Phi313[ijk] + 
       2*invh12*nvec3*Phi200[ijk]*Phi313[ijk] + 
       2*invh12*nvec2*Phi100[ijk]*Phi322[ijk] + 
       2*invh22*nvec2*Phi200[ijk]*Phi322[ijk] + 
       2*invh13*nvec2*Phi100[ijk]*Phi323[ijk] + 
       2*invh12*nvec3*Phi100[ijk]*Phi323[ijk] + 
       2*invh23*nvec2*Phi200[ijk]*Phi323[ijk] + 
       2*invh22*nvec3*Phi200[ijk]*Phi323[ijk] + 
       2*invh13*nvec3*Phi100[ijk]*Phi333[ijk] + 
       2*invh23*nvec3*Phi200[ijk]*Phi333[ijk] + 
       2*Phi000[ijk]*((invh01*nvec0 + invh00*nvec1)*Phi301[ijk] + 
          (invh02*nvec0 + invh00*nvec2)*Phi302[ijk] + 
          invh03*nvec0*Phi303[ijk] + invh00*nvec3*Phi303[ijk] + 
          invh01*nvec1*Phi311[ijk] + invh02*nvec1*Phi312[ijk] + 
          invh01*nvec2*Phi312[ijk] + invh03*nvec1*Phi313[ijk] + 
          invh01*nvec3*Phi313[ijk] + invh02*nvec2*Phi322[ijk] + 
          invh03*nvec2*Phi323[ijk] + invh02*nvec3*Phi323[ijk] + 
          invh03*nvec3*Phi333[ijk]) + 2*nvec0*nvec1*Phi301[ijk]*Pi00[ijk] + 
       2*nvec0*nvec2*Phi302[ijk]*Pi00[ijk] + 
       2*nvec0*nvec3*Phi303[ijk]*Pi00[ijk] + 
       Power(nvec1,2)*Phi311[ijk]*Pi00[ijk] + 
       2*nvec1*nvec2*Phi312[ijk]*Pi00[ijk] + 
       2*nvec1*nvec3*Phi313[ijk]*Pi00[ijk] + 
       Power(nvec2,2)*Phi322[ijk]*Pi00[ijk] + 
       2*nvec2*nvec3*Phi323[ijk]*Pi00[ijk] + 
       Power(nvec3,2)*Phi333[ijk]*Pi00[ijk] + 
       Phi300[ijk]*(-2*gamma2 + 2*invh00*nvec0*Phi000[ijk] + 
          2*invh01*nvec0*Phi100[ijk] + 2*invh02*nvec0*Phi200[ijk] + 
          2*invh13*nvec0*Phi301[ijk] + 2*invh03*nvec1*Phi301[ijk] + 
          2*invh23*nvec0*Phi302[ijk] + 2*invh03*nvec2*Phi302[ijk] + 
          2*invh33*nvec0*Phi303[ijk] + 2*invh03*nvec3*Phi303[ijk] + 
          2*invh13*nvec1*Phi311[ijk] + 2*invh23*nvec1*Phi312[ijk] + 
          2*invh13*nvec2*Phi312[ijk] + 2*invh33*nvec1*Phi313[ijk] + 
          2*invh13*nvec3*Phi313[ijk] + 2*invh23*nvec2*Phi322[ijk] + 
          2*invh33*nvec2*Phi323[ijk] + 2*invh23*nvec3*Phi323[ijk] + 
          2*invh33*nvec3*Phi333[ijk] + Power(nvec0,2)*Pi00[ijk])))/2.
;

dtPhi301[ijk]
=
-(interior*AdPhi301[ijk]) + (alpha[ijk]*
     (2*invh02*nvec0*Phi201[ijk]*Phi300[ijk] - 2*gamma2*Phi301[ijk] + 
       2*invh12*nvec0*Phi201[ijk]*Phi301[ijk] + 
       2*invh02*nvec1*Phi201[ijk]*Phi301[ijk] + 
       2*invh03*nvec0*Phi300[ijk]*Phi301[ijk] + 
       2*invh13*nvec0*Power(Phi301[ijk],2) + 
       2*invh03*nvec1*Power(Phi301[ijk],2) + 
       2*invh22*nvec0*Phi201[ijk]*Phi302[ijk] + 
       2*invh02*nvec2*Phi201[ijk]*Phi302[ijk] + 
       2*invh23*nvec0*Phi301[ijk]*Phi302[ijk] + 
       2*invh03*nvec2*Phi301[ijk]*Phi302[ijk] + 
       2*invh23*nvec0*Phi201[ijk]*Phi303[ijk] + 
       2*invh02*nvec3*Phi201[ijk]*Phi303[ijk] + 
       2*invh33*nvec0*Phi301[ijk]*Phi303[ijk] + 
       2*invh03*nvec3*Phi301[ijk]*Phi303[ijk] + 
       2*invh12*nvec1*Phi201[ijk]*Phi311[ijk] + 
       2*invh13*nvec1*Phi301[ijk]*Phi311[ijk] + 
       2*invh22*nvec1*Phi201[ijk]*Phi312[ijk] + 
       2*invh12*nvec2*Phi201[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi301[ijk]*Phi312[ijk] + 
       2*invh13*nvec2*Phi301[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi201[ijk]*Phi313[ijk] + 
       2*invh12*nvec3*Phi201[ijk]*Phi313[ijk] + 
       2*invh33*nvec1*Phi301[ijk]*Phi313[ijk] + 
       2*invh13*nvec3*Phi301[ijk]*Phi313[ijk] + 
       2*invh22*nvec2*Phi201[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi301[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi201[ijk]*Phi323[ijk] + 
       2*invh22*nvec3*Phi201[ijk]*Phi323[ijk] + 
       2*invh33*nvec2*Phi301[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi301[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi201[ijk]*Phi333[ijk] + 
       2*invh33*nvec3*Phi301[ijk]*Phi333[ijk] + 
       2*Phi001[ijk]*(invh00*nvec0*Phi300[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi301[ijk] + 
          invh02*nvec0*Phi302[ijk] + invh00*nvec2*Phi302[ijk] + 
          invh03*nvec0*Phi303[ijk] + invh00*nvec3*Phi303[ijk] + 
          invh01*nvec1*Phi311[ijk] + invh02*nvec1*Phi312[ijk] + 
          invh01*nvec2*Phi312[ijk] + invh03*nvec1*Phi313[ijk] + 
          invh01*nvec3*Phi313[ijk] + invh02*nvec2*Phi322[ijk] + 
          invh03*nvec2*Phi323[ijk] + invh02*nvec3*Phi323[ijk] + 
          invh03*nvec3*Phi333[ijk]) + 
       2*Phi101[ijk]*(invh01*nvec0*Phi300[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi301[ijk] + 
          invh12*nvec0*Phi302[ijk] + invh01*nvec2*Phi302[ijk] + 
          invh13*nvec0*Phi303[ijk] + invh01*nvec3*Phi303[ijk] + 
          invh11*nvec1*Phi311[ijk] + invh12*nvec1*Phi312[ijk] + 
          invh11*nvec2*Phi312[ijk] + invh13*nvec1*Phi313[ijk] + 
          invh11*nvec3*Phi313[ijk] + invh12*nvec2*Phi322[ijk] + 
          invh13*nvec2*Phi323[ijk] + invh12*nvec3*Phi323[ijk] + 
          invh13*nvec3*Phi333[ijk]) + 
       Power(nvec0,2)*Phi300[ijk]*Pi01[ijk] + 
       2*nvec0*nvec1*Phi301[ijk]*Pi01[ijk] + 
       2*nvec0*nvec2*Phi302[ijk]*Pi01[ijk] + 
       2*nvec0*nvec3*Phi303[ijk]*Pi01[ijk] + 
       Power(nvec1,2)*Phi311[ijk]*Pi01[ijk] + 
       2*nvec1*nvec2*Phi312[ijk]*Pi01[ijk] + 
       2*nvec1*nvec3*Phi313[ijk]*Pi01[ijk] + 
       Power(nvec2,2)*Phi322[ijk]*Pi01[ijk] + 
       2*nvec2*nvec3*Phi323[ijk]*Pi01[ijk] + 
       Power(nvec3,2)*Phi333[ijk]*Pi01[ijk]))/2.
;

dtPhi302[ijk]
=
-(interior*AdPhi302[ijk]) + (alpha[ijk]*
     (2*invh02*nvec0*Phi202[ijk]*Phi300[ijk] + 
       2*invh12*nvec0*Phi202[ijk]*Phi301[ijk] + 
       2*invh02*nvec1*Phi202[ijk]*Phi301[ijk] - 2*gamma2*Phi302[ijk] + 
       2*invh22*nvec0*Phi202[ijk]*Phi302[ijk] + 
       2*invh02*nvec2*Phi202[ijk]*Phi302[ijk] + 
       2*invh03*nvec0*Phi300[ijk]*Phi302[ijk] + 
       2*invh13*nvec0*Phi301[ijk]*Phi302[ijk] + 
       2*invh03*nvec1*Phi301[ijk]*Phi302[ijk] + 
       2*invh23*nvec0*Power(Phi302[ijk],2) + 
       2*invh03*nvec2*Power(Phi302[ijk],2) + 
       2*invh23*nvec0*Phi202[ijk]*Phi303[ijk] + 
       2*invh02*nvec3*Phi202[ijk]*Phi303[ijk] + 
       2*invh33*nvec0*Phi302[ijk]*Phi303[ijk] + 
       2*invh03*nvec3*Phi302[ijk]*Phi303[ijk] + 
       2*invh12*nvec1*Phi202[ijk]*Phi311[ijk] + 
       2*invh13*nvec1*Phi302[ijk]*Phi311[ijk] + 
       2*invh22*nvec1*Phi202[ijk]*Phi312[ijk] + 
       2*invh12*nvec2*Phi202[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi302[ijk]*Phi312[ijk] + 
       2*invh13*nvec2*Phi302[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi202[ijk]*Phi313[ijk] + 
       2*invh12*nvec3*Phi202[ijk]*Phi313[ijk] + 
       2*invh33*nvec1*Phi302[ijk]*Phi313[ijk] + 
       2*invh13*nvec3*Phi302[ijk]*Phi313[ijk] + 
       2*invh22*nvec2*Phi202[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi302[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi202[ijk]*Phi323[ijk] + 
       2*invh22*nvec3*Phi202[ijk]*Phi323[ijk] + 
       2*invh33*nvec2*Phi302[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi302[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi202[ijk]*Phi333[ijk] + 
       2*invh33*nvec3*Phi302[ijk]*Phi333[ijk] + 
       2*Phi002[ijk]*(invh00*nvec0*Phi300[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi301[ijk] + 
          invh02*nvec0*Phi302[ijk] + invh00*nvec2*Phi302[ijk] + 
          invh03*nvec0*Phi303[ijk] + invh00*nvec3*Phi303[ijk] + 
          invh01*nvec1*Phi311[ijk] + invh02*nvec1*Phi312[ijk] + 
          invh01*nvec2*Phi312[ijk] + invh03*nvec1*Phi313[ijk] + 
          invh01*nvec3*Phi313[ijk] + invh02*nvec2*Phi322[ijk] + 
          invh03*nvec2*Phi323[ijk] + invh02*nvec3*Phi323[ijk] + 
          invh03*nvec3*Phi333[ijk]) + 
       2*Phi102[ijk]*(invh01*nvec0*Phi300[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi301[ijk] + 
          invh12*nvec0*Phi302[ijk] + invh01*nvec2*Phi302[ijk] + 
          invh13*nvec0*Phi303[ijk] + invh01*nvec3*Phi303[ijk] + 
          invh11*nvec1*Phi311[ijk] + invh12*nvec1*Phi312[ijk] + 
          invh11*nvec2*Phi312[ijk] + invh13*nvec1*Phi313[ijk] + 
          invh11*nvec3*Phi313[ijk] + invh12*nvec2*Phi322[ijk] + 
          invh13*nvec2*Phi323[ijk] + invh12*nvec3*Phi323[ijk] + 
          invh13*nvec3*Phi333[ijk]) + 
       Power(nvec0,2)*Phi300[ijk]*Pi02[ijk] + 
       2*nvec0*nvec1*Phi301[ijk]*Pi02[ijk] + 
       2*nvec0*nvec2*Phi302[ijk]*Pi02[ijk] + 
       2*nvec0*nvec3*Phi303[ijk]*Pi02[ijk] + 
       Power(nvec1,2)*Phi311[ijk]*Pi02[ijk] + 
       2*nvec1*nvec2*Phi312[ijk]*Pi02[ijk] + 
       2*nvec1*nvec3*Phi313[ijk]*Pi02[ijk] + 
       Power(nvec2,2)*Phi322[ijk]*Pi02[ijk] + 
       2*nvec2*nvec3*Phi323[ijk]*Pi02[ijk] + 
       Power(nvec3,2)*Phi333[ijk]*Pi02[ijk]))/2.
;

dtPhi303[ijk]
=
-(interior*AdPhi303[ijk]) + (alpha[ijk]*
     (2*invh02*nvec0*Phi203[ijk]*Phi300[ijk] + 
       2*invh12*nvec0*Phi203[ijk]*Phi301[ijk] + 
       2*invh02*nvec1*Phi203[ijk]*Phi301[ijk] + 
       2*invh22*nvec0*Phi203[ijk]*Phi302[ijk] + 
       2*invh02*nvec2*Phi203[ijk]*Phi302[ijk] - 2*gamma2*Phi303[ijk] + 
       2*invh23*nvec0*Phi203[ijk]*Phi303[ijk] + 
       2*invh02*nvec3*Phi203[ijk]*Phi303[ijk] + 
       2*invh03*nvec0*Phi300[ijk]*Phi303[ijk] + 
       2*invh13*nvec0*Phi301[ijk]*Phi303[ijk] + 
       2*invh03*nvec1*Phi301[ijk]*Phi303[ijk] + 
       2*invh23*nvec0*Phi302[ijk]*Phi303[ijk] + 
       2*invh03*nvec2*Phi302[ijk]*Phi303[ijk] + 
       2*invh33*nvec0*Power(Phi303[ijk],2) + 
       2*invh03*nvec3*Power(Phi303[ijk],2) + 
       2*invh12*nvec1*Phi203[ijk]*Phi311[ijk] + 
       2*invh13*nvec1*Phi303[ijk]*Phi311[ijk] + 
       2*invh22*nvec1*Phi203[ijk]*Phi312[ijk] + 
       2*invh12*nvec2*Phi203[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi303[ijk]*Phi312[ijk] + 
       2*invh13*nvec2*Phi303[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi203[ijk]*Phi313[ijk] + 
       2*invh12*nvec3*Phi203[ijk]*Phi313[ijk] + 
       2*invh33*nvec1*Phi303[ijk]*Phi313[ijk] + 
       2*invh13*nvec3*Phi303[ijk]*Phi313[ijk] + 
       2*invh22*nvec2*Phi203[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi303[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi203[ijk]*Phi323[ijk] + 
       2*invh22*nvec3*Phi203[ijk]*Phi323[ijk] + 
       2*invh33*nvec2*Phi303[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi303[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi203[ijk]*Phi333[ijk] + 
       2*invh33*nvec3*Phi303[ijk]*Phi333[ijk] + 
       2*Phi003[ijk]*(invh00*nvec0*Phi300[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi301[ijk] + 
          invh02*nvec0*Phi302[ijk] + invh00*nvec2*Phi302[ijk] + 
          invh03*nvec0*Phi303[ijk] + invh00*nvec3*Phi303[ijk] + 
          invh01*nvec1*Phi311[ijk] + invh02*nvec1*Phi312[ijk] + 
          invh01*nvec2*Phi312[ijk] + invh03*nvec1*Phi313[ijk] + 
          invh01*nvec3*Phi313[ijk] + invh02*nvec2*Phi322[ijk] + 
          invh03*nvec2*Phi323[ijk] + invh02*nvec3*Phi323[ijk] + 
          invh03*nvec3*Phi333[ijk]) + 
       2*Phi103[ijk]*(invh01*nvec0*Phi300[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi301[ijk] + 
          invh12*nvec0*Phi302[ijk] + invh01*nvec2*Phi302[ijk] + 
          invh13*nvec0*Phi303[ijk] + invh01*nvec3*Phi303[ijk] + 
          invh11*nvec1*Phi311[ijk] + invh12*nvec1*Phi312[ijk] + 
          invh11*nvec2*Phi312[ijk] + invh13*nvec1*Phi313[ijk] + 
          invh11*nvec3*Phi313[ijk] + invh12*nvec2*Phi322[ijk] + 
          invh13*nvec2*Phi323[ijk] + invh12*nvec3*Phi323[ijk] + 
          invh13*nvec3*Phi333[ijk]) + 
       Power(nvec0,2)*Phi300[ijk]*Pi03[ijk] + 
       2*nvec0*nvec1*Phi301[ijk]*Pi03[ijk] + 
       2*nvec0*nvec2*Phi302[ijk]*Pi03[ijk] + 
       2*nvec0*nvec3*Phi303[ijk]*Pi03[ijk] + 
       Power(nvec1,2)*Phi311[ijk]*Pi03[ijk] + 
       2*nvec1*nvec2*Phi312[ijk]*Pi03[ijk] + 
       2*nvec1*nvec3*Phi313[ijk]*Pi03[ijk] + 
       Power(nvec2,2)*Phi322[ijk]*Pi03[ijk] + 
       2*nvec2*nvec3*Phi323[ijk]*Pi03[ijk] + 
       Power(nvec3,2)*Phi333[ijk]*Pi03[ijk]))/2.
;

dtPhi311[ijk]
=
-(interior*AdPhi311[ijk]) + (alpha[ijk]*
     (2*invh02*nvec0*Phi211[ijk]*Phi300[ijk] + 
       2*invh12*nvec0*Phi211[ijk]*Phi301[ijk] + 
       2*invh02*nvec1*Phi211[ijk]*Phi301[ijk] + 
       2*invh22*nvec0*Phi211[ijk]*Phi302[ijk] + 
       2*invh02*nvec2*Phi211[ijk]*Phi302[ijk] + 
       2*invh23*nvec0*Phi211[ijk]*Phi303[ijk] + 
       2*invh02*nvec3*Phi211[ijk]*Phi303[ijk] - 2*gamma2*Phi311[ijk] + 
       2*invh12*nvec1*Phi211[ijk]*Phi311[ijk] + 
       2*invh03*nvec0*Phi300[ijk]*Phi311[ijk] + 
       2*invh13*nvec0*Phi301[ijk]*Phi311[ijk] + 
       2*invh03*nvec1*Phi301[ijk]*Phi311[ijk] + 
       2*invh23*nvec0*Phi302[ijk]*Phi311[ijk] + 
       2*invh03*nvec2*Phi302[ijk]*Phi311[ijk] + 
       2*invh33*nvec0*Phi303[ijk]*Phi311[ijk] + 
       2*invh03*nvec3*Phi303[ijk]*Phi311[ijk] + 
       2*invh13*nvec1*Power(Phi311[ijk],2) + 
       2*invh22*nvec1*Phi211[ijk]*Phi312[ijk] + 
       2*invh12*nvec2*Phi211[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi311[ijk]*Phi312[ijk] + 
       2*invh13*nvec2*Phi311[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi211[ijk]*Phi313[ijk] + 
       2*invh12*nvec3*Phi211[ijk]*Phi313[ijk] + 
       2*invh33*nvec1*Phi311[ijk]*Phi313[ijk] + 
       2*invh13*nvec3*Phi311[ijk]*Phi313[ijk] + 
       2*invh22*nvec2*Phi211[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi311[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi211[ijk]*Phi323[ijk] + 
       2*invh22*nvec3*Phi211[ijk]*Phi323[ijk] + 
       2*invh33*nvec2*Phi311[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi311[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi211[ijk]*Phi333[ijk] + 
       2*invh33*nvec3*Phi311[ijk]*Phi333[ijk] + 
       2*Phi011[ijk]*(invh00*nvec0*Phi300[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi301[ijk] + 
          invh02*nvec0*Phi302[ijk] + invh00*nvec2*Phi302[ijk] + 
          invh03*nvec0*Phi303[ijk] + invh00*nvec3*Phi303[ijk] + 
          invh01*nvec1*Phi311[ijk] + invh02*nvec1*Phi312[ijk] + 
          invh01*nvec2*Phi312[ijk] + invh03*nvec1*Phi313[ijk] + 
          invh01*nvec3*Phi313[ijk] + invh02*nvec2*Phi322[ijk] + 
          invh03*nvec2*Phi323[ijk] + invh02*nvec3*Phi323[ijk] + 
          invh03*nvec3*Phi333[ijk]) + 
       2*Phi111[ijk]*(invh01*nvec0*Phi300[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi301[ijk] + 
          invh12*nvec0*Phi302[ijk] + invh01*nvec2*Phi302[ijk] + 
          invh13*nvec0*Phi303[ijk] + invh01*nvec3*Phi303[ijk] + 
          invh11*nvec1*Phi311[ijk] + invh12*nvec1*Phi312[ijk] + 
          invh11*nvec2*Phi312[ijk] + invh13*nvec1*Phi313[ijk] + 
          invh11*nvec3*Phi313[ijk] + invh12*nvec2*Phi322[ijk] + 
          invh13*nvec2*Phi323[ijk] + invh12*nvec3*Phi323[ijk] + 
          invh13*nvec3*Phi333[ijk]) + 
       Power(nvec0,2)*Phi300[ijk]*Pi11[ijk] + 
       2*nvec0*nvec1*Phi301[ijk]*Pi11[ijk] + 
       2*nvec0*nvec2*Phi302[ijk]*Pi11[ijk] + 
       2*nvec0*nvec3*Phi303[ijk]*Pi11[ijk] + 
       Power(nvec1,2)*Phi311[ijk]*Pi11[ijk] + 
       2*nvec1*nvec2*Phi312[ijk]*Pi11[ijk] + 
       2*nvec1*nvec3*Phi313[ijk]*Pi11[ijk] + 
       Power(nvec2,2)*Phi322[ijk]*Pi11[ijk] + 
       2*nvec2*nvec3*Phi323[ijk]*Pi11[ijk] + 
       Power(nvec3,2)*Phi333[ijk]*Pi11[ijk]))/2.
;

dtPhi312[ijk]
=
-(interior*AdPhi312[ijk]) + (alpha[ijk]*
     (2*invh02*nvec0*Phi212[ijk]*Phi300[ijk] + 
       2*invh12*nvec0*Phi212[ijk]*Phi301[ijk] + 
       2*invh02*nvec1*Phi212[ijk]*Phi301[ijk] + 
       2*invh22*nvec0*Phi212[ijk]*Phi302[ijk] + 
       2*invh02*nvec2*Phi212[ijk]*Phi302[ijk] + 
       2*invh23*nvec0*Phi212[ijk]*Phi303[ijk] + 
       2*invh02*nvec3*Phi212[ijk]*Phi303[ijk] + 
       2*invh12*nvec1*Phi212[ijk]*Phi311[ijk] - 2*gamma2*Phi312[ijk] + 
       2*invh22*nvec1*Phi212[ijk]*Phi312[ijk] + 
       2*invh12*nvec2*Phi212[ijk]*Phi312[ijk] + 
       2*invh03*nvec0*Phi300[ijk]*Phi312[ijk] + 
       2*invh13*nvec0*Phi301[ijk]*Phi312[ijk] + 
       2*invh03*nvec1*Phi301[ijk]*Phi312[ijk] + 
       2*invh23*nvec0*Phi302[ijk]*Phi312[ijk] + 
       2*invh03*nvec2*Phi302[ijk]*Phi312[ijk] + 
       2*invh33*nvec0*Phi303[ijk]*Phi312[ijk] + 
       2*invh03*nvec3*Phi303[ijk]*Phi312[ijk] + 
       2*invh13*nvec1*Phi311[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Power(Phi312[ijk],2) + 
       2*invh13*nvec2*Power(Phi312[ijk],2) + 
       2*invh23*nvec1*Phi212[ijk]*Phi313[ijk] + 
       2*invh12*nvec3*Phi212[ijk]*Phi313[ijk] + 
       2*invh33*nvec1*Phi312[ijk]*Phi313[ijk] + 
       2*invh13*nvec3*Phi312[ijk]*Phi313[ijk] + 
       2*invh22*nvec2*Phi212[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi312[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi212[ijk]*Phi323[ijk] + 
       2*invh22*nvec3*Phi212[ijk]*Phi323[ijk] + 
       2*invh33*nvec2*Phi312[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi312[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi212[ijk]*Phi333[ijk] + 
       2*invh33*nvec3*Phi312[ijk]*Phi333[ijk] + 
       2*Phi012[ijk]*(invh00*nvec0*Phi300[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi301[ijk] + 
          invh02*nvec0*Phi302[ijk] + invh00*nvec2*Phi302[ijk] + 
          invh03*nvec0*Phi303[ijk] + invh00*nvec3*Phi303[ijk] + 
          invh01*nvec1*Phi311[ijk] + invh02*nvec1*Phi312[ijk] + 
          invh01*nvec2*Phi312[ijk] + invh03*nvec1*Phi313[ijk] + 
          invh01*nvec3*Phi313[ijk] + invh02*nvec2*Phi322[ijk] + 
          invh03*nvec2*Phi323[ijk] + invh02*nvec3*Phi323[ijk] + 
          invh03*nvec3*Phi333[ijk]) + 
       2*Phi112[ijk]*(invh01*nvec0*Phi300[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi301[ijk] + 
          invh12*nvec0*Phi302[ijk] + invh01*nvec2*Phi302[ijk] + 
          invh13*nvec0*Phi303[ijk] + invh01*nvec3*Phi303[ijk] + 
          invh11*nvec1*Phi311[ijk] + invh12*nvec1*Phi312[ijk] + 
          invh11*nvec2*Phi312[ijk] + invh13*nvec1*Phi313[ijk] + 
          invh11*nvec3*Phi313[ijk] + invh12*nvec2*Phi322[ijk] + 
          invh13*nvec2*Phi323[ijk] + invh12*nvec3*Phi323[ijk] + 
          invh13*nvec3*Phi333[ijk]) + 
       Power(nvec0,2)*Phi300[ijk]*Pi12[ijk] + 
       2*nvec0*nvec1*Phi301[ijk]*Pi12[ijk] + 
       2*nvec0*nvec2*Phi302[ijk]*Pi12[ijk] + 
       2*nvec0*nvec3*Phi303[ijk]*Pi12[ijk] + 
       Power(nvec1,2)*Phi311[ijk]*Pi12[ijk] + 
       2*nvec1*nvec2*Phi312[ijk]*Pi12[ijk] + 
       2*nvec1*nvec3*Phi313[ijk]*Pi12[ijk] + 
       Power(nvec2,2)*Phi322[ijk]*Pi12[ijk] + 
       2*nvec2*nvec3*Phi323[ijk]*Pi12[ijk] + 
       Power(nvec3,2)*Phi333[ijk]*Pi12[ijk]))/2.
;

dtPhi313[ijk]
=
-(interior*AdPhi313[ijk]) + (alpha[ijk]*
     (2*invh02*nvec0*Phi213[ijk]*Phi300[ijk] + 
       2*invh12*nvec0*Phi213[ijk]*Phi301[ijk] + 
       2*invh02*nvec1*Phi213[ijk]*Phi301[ijk] + 
       2*invh22*nvec0*Phi213[ijk]*Phi302[ijk] + 
       2*invh02*nvec2*Phi213[ijk]*Phi302[ijk] + 
       2*invh23*nvec0*Phi213[ijk]*Phi303[ijk] + 
       2*invh02*nvec3*Phi213[ijk]*Phi303[ijk] + 
       2*invh12*nvec1*Phi213[ijk]*Phi311[ijk] + 
       2*invh22*nvec1*Phi213[ijk]*Phi312[ijk] + 
       2*invh12*nvec2*Phi213[ijk]*Phi312[ijk] - 2*gamma2*Phi313[ijk] + 
       2*invh23*nvec1*Phi213[ijk]*Phi313[ijk] + 
       2*invh12*nvec3*Phi213[ijk]*Phi313[ijk] + 
       2*invh03*nvec0*Phi300[ijk]*Phi313[ijk] + 
       2*invh13*nvec0*Phi301[ijk]*Phi313[ijk] + 
       2*invh03*nvec1*Phi301[ijk]*Phi313[ijk] + 
       2*invh23*nvec0*Phi302[ijk]*Phi313[ijk] + 
       2*invh03*nvec2*Phi302[ijk]*Phi313[ijk] + 
       2*invh33*nvec0*Phi303[ijk]*Phi313[ijk] + 
       2*invh03*nvec3*Phi303[ijk]*Phi313[ijk] + 
       2*invh13*nvec1*Phi311[ijk]*Phi313[ijk] + 
       2*invh23*nvec1*Phi312[ijk]*Phi313[ijk] + 
       2*invh13*nvec2*Phi312[ijk]*Phi313[ijk] + 
       2*invh33*nvec1*Power(Phi313[ijk],2) + 
       2*invh13*nvec3*Power(Phi313[ijk],2) + 
       2*invh22*nvec2*Phi213[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi313[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi213[ijk]*Phi323[ijk] + 
       2*invh22*nvec3*Phi213[ijk]*Phi323[ijk] + 
       2*invh33*nvec2*Phi313[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi313[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi213[ijk]*Phi333[ijk] + 
       2*invh33*nvec3*Phi313[ijk]*Phi333[ijk] + 
       2*Phi013[ijk]*(invh00*nvec0*Phi300[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi301[ijk] + 
          invh02*nvec0*Phi302[ijk] + invh00*nvec2*Phi302[ijk] + 
          invh03*nvec0*Phi303[ijk] + invh00*nvec3*Phi303[ijk] + 
          invh01*nvec1*Phi311[ijk] + invh02*nvec1*Phi312[ijk] + 
          invh01*nvec2*Phi312[ijk] + invh03*nvec1*Phi313[ijk] + 
          invh01*nvec3*Phi313[ijk] + invh02*nvec2*Phi322[ijk] + 
          invh03*nvec2*Phi323[ijk] + invh02*nvec3*Phi323[ijk] + 
          invh03*nvec3*Phi333[ijk]) + 
       2*Phi113[ijk]*(invh01*nvec0*Phi300[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi301[ijk] + 
          invh12*nvec0*Phi302[ijk] + invh01*nvec2*Phi302[ijk] + 
          invh13*nvec0*Phi303[ijk] + invh01*nvec3*Phi303[ijk] + 
          invh11*nvec1*Phi311[ijk] + invh12*nvec1*Phi312[ijk] + 
          invh11*nvec2*Phi312[ijk] + invh13*nvec1*Phi313[ijk] + 
          invh11*nvec3*Phi313[ijk] + invh12*nvec2*Phi322[ijk] + 
          invh13*nvec2*Phi323[ijk] + invh12*nvec3*Phi323[ijk] + 
          invh13*nvec3*Phi333[ijk]) + 
       Power(nvec0,2)*Phi300[ijk]*Pi13[ijk] + 
       2*nvec0*nvec1*Phi301[ijk]*Pi13[ijk] + 
       2*nvec0*nvec2*Phi302[ijk]*Pi13[ijk] + 
       2*nvec0*nvec3*Phi303[ijk]*Pi13[ijk] + 
       Power(nvec1,2)*Phi311[ijk]*Pi13[ijk] + 
       2*nvec1*nvec2*Phi312[ijk]*Pi13[ijk] + 
       2*nvec1*nvec3*Phi313[ijk]*Pi13[ijk] + 
       Power(nvec2,2)*Phi322[ijk]*Pi13[ijk] + 
       2*nvec2*nvec3*Phi323[ijk]*Pi13[ijk] + 
       Power(nvec3,2)*Phi333[ijk]*Pi13[ijk]))/2.
;

dtPhi322[ijk]
=
-(interior*AdPhi322[ijk]) + (alpha[ijk]*
     (2*invh02*nvec0*Phi222[ijk]*Phi300[ijk] + 
       2*invh12*nvec0*Phi222[ijk]*Phi301[ijk] + 
       2*invh02*nvec1*Phi222[ijk]*Phi301[ijk] + 
       2*invh22*nvec0*Phi222[ijk]*Phi302[ijk] + 
       2*invh02*nvec2*Phi222[ijk]*Phi302[ijk] + 
       2*invh23*nvec0*Phi222[ijk]*Phi303[ijk] + 
       2*invh02*nvec3*Phi222[ijk]*Phi303[ijk] + 
       2*invh12*nvec1*Phi222[ijk]*Phi311[ijk] + 
       2*invh22*nvec1*Phi222[ijk]*Phi312[ijk] + 
       2*invh12*nvec2*Phi222[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi222[ijk]*Phi313[ijk] + 
       2*invh12*nvec3*Phi222[ijk]*Phi313[ijk] - 2*gamma2*Phi322[ijk] + 
       2*invh22*nvec2*Phi222[ijk]*Phi322[ijk] + 
       2*invh03*nvec0*Phi300[ijk]*Phi322[ijk] + 
       2*invh13*nvec0*Phi301[ijk]*Phi322[ijk] + 
       2*invh03*nvec1*Phi301[ijk]*Phi322[ijk] + 
       2*invh23*nvec0*Phi302[ijk]*Phi322[ijk] + 
       2*invh03*nvec2*Phi302[ijk]*Phi322[ijk] + 
       2*invh33*nvec0*Phi303[ijk]*Phi322[ijk] + 
       2*invh03*nvec3*Phi303[ijk]*Phi322[ijk] + 
       2*invh13*nvec1*Phi311[ijk]*Phi322[ijk] + 
       2*invh23*nvec1*Phi312[ijk]*Phi322[ijk] + 
       2*invh13*nvec2*Phi312[ijk]*Phi322[ijk] + 
       2*invh33*nvec1*Phi313[ijk]*Phi322[ijk] + 
       2*invh13*nvec3*Phi313[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Power(Phi322[ijk],2) + 
       2*invh23*nvec2*Phi222[ijk]*Phi323[ijk] + 
       2*invh22*nvec3*Phi222[ijk]*Phi323[ijk] + 
       2*invh33*nvec2*Phi322[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi322[ijk]*Phi323[ijk] + 
       2*invh23*nvec3*Phi222[ijk]*Phi333[ijk] + 
       2*invh33*nvec3*Phi322[ijk]*Phi333[ijk] + 
       2*Phi022[ijk]*(invh00*nvec0*Phi300[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi301[ijk] + 
          invh02*nvec0*Phi302[ijk] + invh00*nvec2*Phi302[ijk] + 
          invh03*nvec0*Phi303[ijk] + invh00*nvec3*Phi303[ijk] + 
          invh01*nvec1*Phi311[ijk] + invh02*nvec1*Phi312[ijk] + 
          invh01*nvec2*Phi312[ijk] + invh03*nvec1*Phi313[ijk] + 
          invh01*nvec3*Phi313[ijk] + invh02*nvec2*Phi322[ijk] + 
          invh03*nvec2*Phi323[ijk] + invh02*nvec3*Phi323[ijk] + 
          invh03*nvec3*Phi333[ijk]) + 
       2*Phi122[ijk]*(invh01*nvec0*Phi300[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi301[ijk] + 
          invh12*nvec0*Phi302[ijk] + invh01*nvec2*Phi302[ijk] + 
          invh13*nvec0*Phi303[ijk] + invh01*nvec3*Phi303[ijk] + 
          invh11*nvec1*Phi311[ijk] + invh12*nvec1*Phi312[ijk] + 
          invh11*nvec2*Phi312[ijk] + invh13*nvec1*Phi313[ijk] + 
          invh11*nvec3*Phi313[ijk] + invh12*nvec2*Phi322[ijk] + 
          invh13*nvec2*Phi323[ijk] + invh12*nvec3*Phi323[ijk] + 
          invh13*nvec3*Phi333[ijk]) + 
       Power(nvec0,2)*Phi300[ijk]*Pi22[ijk] + 
       2*nvec0*nvec1*Phi301[ijk]*Pi22[ijk] + 
       2*nvec0*nvec2*Phi302[ijk]*Pi22[ijk] + 
       2*nvec0*nvec3*Phi303[ijk]*Pi22[ijk] + 
       Power(nvec1,2)*Phi311[ijk]*Pi22[ijk] + 
       2*nvec1*nvec2*Phi312[ijk]*Pi22[ijk] + 
       2*nvec1*nvec3*Phi313[ijk]*Pi22[ijk] + 
       Power(nvec2,2)*Phi322[ijk]*Pi22[ijk] + 
       2*nvec2*nvec3*Phi323[ijk]*Pi22[ijk] + 
       Power(nvec3,2)*Phi333[ijk]*Pi22[ijk]))/2.
;

dtPhi323[ijk]
=
-(interior*AdPhi323[ijk]) + (alpha[ijk]*
     (2*invh02*nvec0*Phi223[ijk]*Phi300[ijk] + 
       2*invh12*nvec0*Phi223[ijk]*Phi301[ijk] + 
       2*invh02*nvec1*Phi223[ijk]*Phi301[ijk] + 
       2*invh22*nvec0*Phi223[ijk]*Phi302[ijk] + 
       2*invh02*nvec2*Phi223[ijk]*Phi302[ijk] + 
       2*invh23*nvec0*Phi223[ijk]*Phi303[ijk] + 
       2*invh02*nvec3*Phi223[ijk]*Phi303[ijk] + 
       2*invh12*nvec1*Phi223[ijk]*Phi311[ijk] + 
       2*invh22*nvec1*Phi223[ijk]*Phi312[ijk] + 
       2*invh12*nvec2*Phi223[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi223[ijk]*Phi313[ijk] + 
       2*invh12*nvec3*Phi223[ijk]*Phi313[ijk] + 
       2*invh22*nvec2*Phi223[ijk]*Phi322[ijk] - 2*gamma2*Phi323[ijk] + 
       2*invh23*nvec2*Phi223[ijk]*Phi323[ijk] + 
       2*invh22*nvec3*Phi223[ijk]*Phi323[ijk] + 
       2*invh03*nvec0*Phi300[ijk]*Phi323[ijk] + 
       2*invh13*nvec0*Phi301[ijk]*Phi323[ijk] + 
       2*invh03*nvec1*Phi301[ijk]*Phi323[ijk] + 
       2*invh23*nvec0*Phi302[ijk]*Phi323[ijk] + 
       2*invh03*nvec2*Phi302[ijk]*Phi323[ijk] + 
       2*invh33*nvec0*Phi303[ijk]*Phi323[ijk] + 
       2*invh03*nvec3*Phi303[ijk]*Phi323[ijk] + 
       2*invh13*nvec1*Phi311[ijk]*Phi323[ijk] + 
       2*invh23*nvec1*Phi312[ijk]*Phi323[ijk] + 
       2*invh13*nvec2*Phi312[ijk]*Phi323[ijk] + 
       2*invh33*nvec1*Phi313[ijk]*Phi323[ijk] + 
       2*invh13*nvec3*Phi313[ijk]*Phi323[ijk] + 
       2*invh23*nvec2*Phi322[ijk]*Phi323[ijk] + 
       2*invh33*nvec2*Power(Phi323[ijk],2) + 
       2*invh23*nvec3*Power(Phi323[ijk],2) + 
       2*invh23*nvec3*Phi223[ijk]*Phi333[ijk] + 
       2*invh33*nvec3*Phi323[ijk]*Phi333[ijk] + 
       2*Phi023[ijk]*(invh00*nvec0*Phi300[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi301[ijk] + 
          invh02*nvec0*Phi302[ijk] + invh00*nvec2*Phi302[ijk] + 
          invh03*nvec0*Phi303[ijk] + invh00*nvec3*Phi303[ijk] + 
          invh01*nvec1*Phi311[ijk] + invh02*nvec1*Phi312[ijk] + 
          invh01*nvec2*Phi312[ijk] + invh03*nvec1*Phi313[ijk] + 
          invh01*nvec3*Phi313[ijk] + invh02*nvec2*Phi322[ijk] + 
          invh03*nvec2*Phi323[ijk] + invh02*nvec3*Phi323[ijk] + 
          invh03*nvec3*Phi333[ijk]) + 
       2*Phi123[ijk]*(invh01*nvec0*Phi300[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi301[ijk] + 
          invh12*nvec0*Phi302[ijk] + invh01*nvec2*Phi302[ijk] + 
          invh13*nvec0*Phi303[ijk] + invh01*nvec3*Phi303[ijk] + 
          invh11*nvec1*Phi311[ijk] + invh12*nvec1*Phi312[ijk] + 
          invh11*nvec2*Phi312[ijk] + invh13*nvec1*Phi313[ijk] + 
          invh11*nvec3*Phi313[ijk] + invh12*nvec2*Phi322[ijk] + 
          invh13*nvec2*Phi323[ijk] + invh12*nvec3*Phi323[ijk] + 
          invh13*nvec3*Phi333[ijk]) + 
       Power(nvec0,2)*Phi300[ijk]*Pi23[ijk] + 
       2*nvec0*nvec1*Phi301[ijk]*Pi23[ijk] + 
       2*nvec0*nvec2*Phi302[ijk]*Pi23[ijk] + 
       2*nvec0*nvec3*Phi303[ijk]*Pi23[ijk] + 
       Power(nvec1,2)*Phi311[ijk]*Pi23[ijk] + 
       2*nvec1*nvec2*Phi312[ijk]*Pi23[ijk] + 
       2*nvec1*nvec3*Phi313[ijk]*Pi23[ijk] + 
       Power(nvec2,2)*Phi322[ijk]*Pi23[ijk] + 
       2*nvec2*nvec3*Phi323[ijk]*Pi23[ijk] + 
       Power(nvec3,2)*Phi333[ijk]*Pi23[ijk]))/2.
;

dtPhi333[ijk]
=
-(interior*AdPhi333[ijk]) + (alpha[ijk]*
     (2*invh02*nvec0*Phi233[ijk]*Phi300[ijk] + 
       2*invh12*nvec0*Phi233[ijk]*Phi301[ijk] + 
       2*invh02*nvec1*Phi233[ijk]*Phi301[ijk] + 
       2*invh22*nvec0*Phi233[ijk]*Phi302[ijk] + 
       2*invh02*nvec2*Phi233[ijk]*Phi302[ijk] + 
       2*invh23*nvec0*Phi233[ijk]*Phi303[ijk] + 
       2*invh02*nvec3*Phi233[ijk]*Phi303[ijk] + 
       2*invh12*nvec1*Phi233[ijk]*Phi311[ijk] + 
       2*invh22*nvec1*Phi233[ijk]*Phi312[ijk] + 
       2*invh12*nvec2*Phi233[ijk]*Phi312[ijk] + 
       2*invh23*nvec1*Phi233[ijk]*Phi313[ijk] + 
       2*invh12*nvec3*Phi233[ijk]*Phi313[ijk] + 
       2*invh22*nvec2*Phi233[ijk]*Phi322[ijk] + 
       2*invh23*nvec2*Phi233[ijk]*Phi323[ijk] + 
       2*invh22*nvec3*Phi233[ijk]*Phi323[ijk] - 2*gamma2*Phi333[ijk] + 
       2*invh23*nvec3*Phi233[ijk]*Phi333[ijk] + 
       2*invh03*nvec0*Phi300[ijk]*Phi333[ijk] + 
       2*invh13*nvec0*Phi301[ijk]*Phi333[ijk] + 
       2*invh03*nvec1*Phi301[ijk]*Phi333[ijk] + 
       2*invh23*nvec0*Phi302[ijk]*Phi333[ijk] + 
       2*invh03*nvec2*Phi302[ijk]*Phi333[ijk] + 
       2*invh33*nvec0*Phi303[ijk]*Phi333[ijk] + 
       2*invh03*nvec3*Phi303[ijk]*Phi333[ijk] + 
       2*invh13*nvec1*Phi311[ijk]*Phi333[ijk] + 
       2*invh23*nvec1*Phi312[ijk]*Phi333[ijk] + 
       2*invh13*nvec2*Phi312[ijk]*Phi333[ijk] + 
       2*invh33*nvec1*Phi313[ijk]*Phi333[ijk] + 
       2*invh13*nvec3*Phi313[ijk]*Phi333[ijk] + 
       2*invh23*nvec2*Phi322[ijk]*Phi333[ijk] + 
       2*invh33*nvec2*Phi323[ijk]*Phi333[ijk] + 
       2*invh23*nvec3*Phi323[ijk]*Phi333[ijk] + 
       2*invh33*nvec3*Power(Phi333[ijk],2) + 
       2*Phi033[ijk]*(invh00*nvec0*Phi300[ijk] + 
          (invh01*nvec0 + invh00*nvec1)*Phi301[ijk] + 
          invh02*nvec0*Phi302[ijk] + invh00*nvec2*Phi302[ijk] + 
          invh03*nvec0*Phi303[ijk] + invh00*nvec3*Phi303[ijk] + 
          invh01*nvec1*Phi311[ijk] + invh02*nvec1*Phi312[ijk] + 
          invh01*nvec2*Phi312[ijk] + invh03*nvec1*Phi313[ijk] + 
          invh01*nvec3*Phi313[ijk] + invh02*nvec2*Phi322[ijk] + 
          invh03*nvec2*Phi323[ijk] + invh02*nvec3*Phi323[ijk] + 
          invh03*nvec3*Phi333[ijk]) + 
       2*Phi133[ijk]*(invh01*nvec0*Phi300[ijk] + 
          (invh11*nvec0 + invh01*nvec1)*Phi301[ijk] + 
          invh12*nvec0*Phi302[ijk] + invh01*nvec2*Phi302[ijk] + 
          invh13*nvec0*Phi303[ijk] + invh01*nvec3*Phi303[ijk] + 
          invh11*nvec1*Phi311[ijk] + invh12*nvec1*Phi312[ijk] + 
          invh11*nvec2*Phi312[ijk] + invh13*nvec1*Phi313[ijk] + 
          invh11*nvec3*Phi313[ijk] + invh12*nvec2*Phi322[ijk] + 
          invh13*nvec2*Phi323[ijk] + invh12*nvec3*Phi323[ijk] + 
          invh13*nvec3*Phi333[ijk]) + 
       Power(nvec0,2)*Phi300[ijk]*Pi33[ijk] + 
       2*nvec0*nvec1*Phi301[ijk]*Pi33[ijk] + 
       2*nvec0*nvec2*Phi302[ijk]*Pi33[ijk] + 
       2*nvec0*nvec3*Phi303[ijk]*Pi33[ijk] + 
       Power(nvec1,2)*Phi311[ijk]*Pi33[ijk] + 
       2*nvec1*nvec2*Phi312[ijk]*Pi33[ijk] + 
       2*nvec1*nvec3*Phi313[ijk]*Pi33[ijk] + 
       Power(nvec2,2)*Phi322[ijk]*Pi33[ijk] + 
       2*nvec2*nvec3*Phi323[ijk]*Pi33[ijk] + 
       Power(nvec3,2)*Phi333[ijk]*Pi33[ijk]))/2.
;

} /* end of points */

TIMER_STOP;

return 0;
} /* end of function */

/* GHG_rhs.c */
