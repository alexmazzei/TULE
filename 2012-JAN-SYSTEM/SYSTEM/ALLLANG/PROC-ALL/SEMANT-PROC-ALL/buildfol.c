/*	Compiler: ECL 12.7.1                                          */
/*	Date: 2012/9/20 14:57 (yyyy/mm/dd)                            */
/*	Machine: Darwin 10.8.0 x86_64                                 */
/*	Source: /Users/mazzei/lavori/Projects/ATLAS/softExt/tup/2012-JAN-SYSTEM/SYSTEM/ALLLANG/PROC-ALL/SEMANT-PROC-ALL/buildfol */
#include <ecl/ecl-cmp.h>
#include "/Users/mazzei/lavori/Projects/ATLAS/softExt/tup/2012-JAN-SYSTEM/SYSTEM/ALLLANG/PROC-ALL/SEMANT-PROC-ALL/buildfol.eclh"
/*	function definition for SEM-TO-FOL-TRANSLATION                */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L1sem_to_fol_translation(cl_object V1)
{ VT2 VLEX2 CLSR2 STCK2
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  GOAL-PART       */
	cl_object V3;                             /*  RESTR-PART      */
	cl_object V4;                             /*  PAR-RESULT      */
	cl_object V5;                             /*  BODY            */
	cl_object V6;                             /*  VARS            */
	V2= ECL_NIL;
	V3= ECL_NIL;
	V4= ECL_NIL;
	V5= ECL_NIL;
	V6= ECL_NIL;
	T0= ecl_car(V1);
	if(!((T0)==(VV[2]))){
	goto L8;}
	(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[3]) /*  BREAK */;
	T0= ecl_cadr(V1);
	T1= ecl_cadddr(V1);
	V2= 