/* test.c */
/* Produced with Generato */

#include "nmesh.h"
#include "C3GH.h"

/* use globals from C3GH */
extern tC3GH C3GH[1];


void test(tVarList *vlu, tVarList *vlr)
{
tMesh *mesh = u->mesh;

int Msqr = GetvLax(Par("ADM_ConstraintNorm"), "Msqr ");

formylnodes(mesh)
{
tNode *node = MyLnode;
int ijk;

forpoints(node, ijk)
{
int iMDD = Ind("ADM_gxx");

double *rU1 = Vard(node, Vind(vlr,C3GH->i_rUx));
double *rU2 = Vard(node, Vind(vlr,C3GH->i_rUx+1));
double *rU3 = Vard(node, Vind(vlr,C3GH->i_rUx+2));
double *uU1 = Vard(node, Vind(vlu,C3GH->i_uUx));
double *uU2 = Vard(node, Vind(vlu,C3GH->i_uUx+1));
double *uU3 = Vard(node, Vind(vlu,C3GH->i_uUx+2));
double *MDD11 = Vard(node, iMDDxx);
double *MDD12 = Vard(node, iMDDxx+1);
double *MDD13 = Vard(node, iMDDxx+2);
double *MDD22 = Vard(node, iMDDxx+3);
double *MDD23 = Vard(node, iMDDxx+4);
double *MDD33 = Vard(node, iMDDxx+5);

double vU1
=
MDD11[ijk]*uU1[ijk] + MDD12[ijk]*uU2[ijk] + MDD13[ijk]*uU3[ijk]
;

double vU2
=
MDD12[ijk]*uU1[ijk] + MDD22[ijk]*uU2[ijk] + MDD23[ijk]*uU3[ijk]
;

double vU3
=
MDD13[ijk]*uU1[ijk] + MDD23[ijk]*uU2[ijk] + MDD33[ijk]*uU3[ijk]
;


if(Msqr)
{
rU1[ijk]
=
vU1*MDD11[ijk] + vU2*MDD12[ijk] + vU3*MDD13[ijk]
;

rU2[ijk]
=
vU1*MDD12[ijk] + vU2*MDD22[ijk] + vU3*MDD23[ijk]
;

rU3[ijk]
=
vU1*MDD13[ijk] + vU2*MDD23[ijk] + vU3*MDD33[ijk]
;

}
else
{
rU1[ijk]
=
vU1
;

rU2[ijk]
=
vU2
;

rU3[ijk]
=
vU3
;

}

} /* end of points */
} /* end of nodes */
}

/* test.c */
