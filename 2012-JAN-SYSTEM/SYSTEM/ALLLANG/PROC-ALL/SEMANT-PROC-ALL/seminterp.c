/*	Compiler: ECL 12.7.1                                          */
/*	Date: 2012/9/20 14:57 (yyyy/mm/dd)                            */
/*	Machine: Darwin 10.8.0 x86_64                                 */
/*	Source: /Users/mazzei/lavori/Projects/ATLAS/softExt/tup/2012-JAN-SYSTEM/SYSTEM/ALLLANG/PROC-ALL/SEMANT-PROC-ALL/seminterp */
#include <ecl/ecl-cmp.h>
#include "/Users/mazzei/lavori/Projects/ATLAS/softExt/tup/2012-JAN-SYSTEM/SYSTEM/ALLLANG/PROC-ALL/SEMANT-PROC-ALL/seminterp.eclh"
/*	function definition for SEMANTIC-INTERPRETATION               */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L1semantic_interpretation(cl_narg narg, cl_object V1, ...)
{ VT2 VLEX2 CLSR2 STCK2
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	if (ecl_unlikely(narg<1)) FEwrong_num_arguments_anonym();
	if (ecl_unlikely(narg>2)) FEwrong_num_arguments_anonym();
	{
	cl_object V2;
	va_list args; va_start(args,V1);
	{int i=1;
	if (i >= narg) {
	V2= ECL_NIL;
	} else {
	i++;
	V2= va_arg(args,cl_object);
	}}
	va_end(args);
	(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[0]) /*  BREAK */;
	if(!(ECL_STRINGP(V1))){
	goto L4;}
	value0=L3seminterp_from_string(V1)        /*  SEMINTERP-FROM-STRING */;
	return value0;
L4:;
	if(!((V1)==(VV[1]))){
	goto L7;}
	T0= ecl_function_dispatch(cl_env_copy,VV[265])(2,V2,VV[2]) /*  CHANGE-EXTENS */;
	T1= ecl_function_dispatch(cl_env_copy,VV[265])(2,V2,VV[3]) /*  CHANGE-EXTENS */;
	T2= ecl_function_dispatch(cl_env_copy,VV[265])(2,V2,VV[4]) /*  CHANGE-EXTENS */;
	T3= ecl_function_dispatch(cl_env_copy,VV[265])(2,V2,VV[5]) /*  CHANGE-EXTENS */;
	T4= ecl_function_dispatch(cl_env_copy,VV[265])(2,V2,VV[6]) /*  CHANGE-EXTENS */;
	value0=L2file_sem_interp(T0,T1,T2,T3,T4)  /*  FILE-SEM-INTERP */;
	return value0;
L7:;
	{cl_object V3;
	cl_object V4;
	V3= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V1))) FEtype_error_list(V1);
	V4= V1;
	{cl_object V5;
	cl_object V6;
	V5= ecl_list1(ECL_NIL);
	V6= V5;
L15:;
	if(!(ecl_endp(V4))){
	goto L19;}
	goto L16;
L19:;
	goto L17;
L17:;
	V3= _ecl_car(V4);
	{cl_object V7;
	V7= _ecl_cdr(V4);
	if (ecl_unlikely(!ECL_LISTP(V7))) FEtype_error_list(V7);
	V4= V7;
	}
	if (ecl_unlikely(ECL_ATOM(V6))) FEtype_error_cons(V6);
	T0= V6;
	T1= L4singsent_sem_interp(1,V3)           /*  SINGSENT-SEM-INTERP */;
	V6= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V6,T0);
	goto L15;
L16:;
	value0=ecl_cdr(V5); cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for FILE-SEM-INTERP                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L2file_sem_interp(cl_object volatile V1, cl_object volatile V2, cl_object volatile V3, cl_object volatile V4, cl_object volatile V5)
{ VT3 VLEX3 CLSR3 STCK3
	const cl_env_ptr cl_env_copy = ecl_process_env();
	volatile cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{volatile cl_object V6;                   /*  NXTTREE         */
	volatile cl_object V7;                    /*  ONTOREPR        */
	volatile cl_object V8;                    /*  ONTOSHORT       */
	volatile cl_object V9;                    /*  FOL-REPR        */
	V6= ECL_NIL;
	V7= ECL_NIL;
	V8= ECL_NIL;
	V9= ECL_NIL;
	ecl_bds_bind(cl_env_copy,VV[12],ECL_NIL); /*  *FULL-TREE*     */
	{volatile cl_object V10;                  /*  AVMPORT         */
	V10= cl_open(7,V1,ECL_SYM("DIRECTION",1219),ECL_SYM("INPUT",1255),ECL_SYM("EXTERNAL-FORMAT",1237),VV[13],ECL_SYM("IF-DOES-NOT-EXIST",1245),ECL_SYM("ERROR",1230)) /*  OPEN */;
	{
	volatile bool unwinding = FALSE;
	cl_index V11=ECL_STACK_INDEX(cl_env_copy),V12;
	ecl_frame_ptr next_fr;
	if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {
	  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;
	} else {
	{ struct ecl_stack_frame _ecl_inner_frame_aux;
	volatile cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);
	{volatile cl_object V13;
	V13= _ecl_inner_frame;
	{volatile cl_object V14;                  /*  SIMPLEAVMPORT   */
	V14= cl_open(9,V2,ECL_SYM("DIRECTION",1219),ECL_SYM("OUTPUT",1283),ECL_SYM("IF-EXISTS",1246),ECL_SYM("OVERWRITE",1284),ECL_SYM("IF-DOES-NOT-EXIST",1245),ECL_SYM("CREATE",1214),ECL_SYM("EXTERNAL-FORMAT",1237),VV[13]) /*  OPEN */;
	{
	volatile bool unwinding = FALSE;
	cl_index V15=ECL_STACK_INDEX(cl_env_copy),V16;
	ecl_frame_ptr next_fr;
	if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {
	  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;
	} else {
	{ struct ecl_stack_frame _ecl_inner_frame_aux;
	volatile cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);
	{volatile cl_object V17;
	V17= _ecl_inner_frame;
	{volatile cl_object V18;                  /*  SEMPORT         */
	V18= cl_open(9,V3,ECL_SYM("DIRECTION",1219),ECL_SYM("OUTPUT",1283),ECL_SYM("IF-EXISTS",1246),ECL_SYM("OVERWRITE",1284),ECL_SYM("IF-DOES-NOT-EXIST",1245),ECL_SYM("CREATE",1214),ECL_SYM("EXTERNAL-FORMAT",1237),VV[13]) /*  OPEN */;
	{
	volatile bool unwinding = FALSE;
	cl_index V19=ECL_STACK_INDEX(cl_env_copy),V20;
	ecl_frame_ptr next_fr;
	if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {
	  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;
	} else {
	{ struct ecl_stack_frame _ecl_inner_frame_aux;
	volatile cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);
	{volatile cl_object V21;
	V21= _ecl_inner_frame;
	{volatile cl_object V22;                  /*  SEMSHPORT       */
	V22= cl_open(9,V4,ECL_SYM("DIRECTION",1219),ECL_SYM("OUTPUT",1283),ECL_SYM("IF-EXISTS",1246),ECL_SYM("OVERWRITE",1284),ECL_SYM("IF-DOES-NOT-EXIST",1245),ECL_SYM("CREATE",1214),ECL_SYM("EXTERNAL-FORMAT",1237),VV[13]) /*  OPEN */;
	{
	volatile bool unwinding = FALSE;
	cl_index V23=ECL_STACK_INDEX(cl_env_copy),V24;
	ecl_frame_ptr next_fr;
	if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {
	  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;
	} else {
	{ struct ecl_stack_frame _ecl_inner_frame_aux;
	volatile cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);
	{volatile cl_object V25;
	V25= _ecl_inner_frame;
	{volatile cl_object V26;                  /*  FOLPORT         */
	V26= cl_open(9,V5,ECL_SYM("DIRECTION",1219),ECL_SYM("OUTPUT",1283),ECL_SYM("IF-EXISTS",1246),ECL_SYM("OVERWRITE",1284),ECL_SYM("IF-DOES-NOT-EXIST",1245),ECL_SYM("CREATE",1214),ECL_SYM("EXTERNAL-FORMAT",1237),VV[13]) /*  OPEN */;
	{
	volatile bool unwinding = FALSE;
	cl_index V27=ECL_STACK_INDEX(cl_env_copy),V28;
	ecl_frame_ptr next_fr;
	if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {
	  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;
	} else {
	{ struct ecl_stack_frame _ecl_inner_frame_aux;
	volatile cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);
	{cl_object V29;
	V29= _ecl_inner_frame;
	{cl_object V30;                           /*  NXTHEADING      */
	V30= cl_read_line(3,V10,ECL_NIL,CODE_CHAR(27)) /*  READ-LINE  */;
	goto L39;
L38:;
	T0= cl_string_trim(VV[14],V30)            /*  STRING-TRIM     */;
	if((cl_stringE(2,T0,VV[15])               /*  STRING=         */)==ECL_NIL){
	goto L43;}
	cl_format(3,V18,VV[16],V30)               /*  FORMAT          */;
	cl_format(3,V22,VV[16],V30)               /*  FORMAT          */;
	cl_format(3,V26,VV[16],V30)               /*  FORMAT          */;
	goto L41;
L43:;
	if((ecl_function_dispatch(cl_env_copy,VV[267])(2,V30,ECL_NIL) /*  IS-SENTENCE-HEADING */)==ECL_NIL){
	goto L48;}
	cl_format(3,V18,VV[16],V30)               /*  FORMAT          */;
	cl_format(3,V22,VV[16],V30)               /*  FORMAT          */;
	cl_format(3,V26,VV[16],V30)               /*  FORMAT          */;
	V6= cl_read(1,V10)                        /*  READ            */;
	cl_set(VV[12],L5annotate(V6)              /*  ANNOTATE        */);
	ecl_function_dispatch(cl_env_copy,VV[268])(2,ecl_symbol_value(VV[12]),V14) /*  PRINT-ACTAVM-READABLE */;
	V7= ecl_function_dispatch(cl_env_copy,VV[269])(1,ecl_symbol_value(VV[12])) /*  BUILD-SEM-QUERY */;
	cl_format(3,V18,VV[16],V7)                /*  FORMAT          */;
	V8= L51simplify_onto_repr(V7)             /*  SIMPLIFY-ONTO-REPR */;
	cl_format(3,V22,VV[16],V8)                /*  FORMAT          */;
	V9= ecl_function_dispatch(cl_env_copy,VV[270])(1,V8) /*  SEM-TO-FOL-TRANSLATION */;
	ecl_function_dispatch(cl_env_copy,VV[271])(2,V9,V26) /*  FOL-PRINT-PRETTY */;
	goto L41;
L48:;
	ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[18]) /*  EXCEPTION */;
	goto L41;
L41:;
	V30= cl_read_line(3,V10,ECL_NIL,CODE_CHAR(27)) /*  READ-LINE  */;
L39:;
	if(ecl_equal(V30,CODE_CHAR(27))){
	goto L70;}
	goto L38;
L70:;
	goto L68;
L68:;
	goto L36;
L36:;
	cl_env_copy->values[0]=ECL_NIL; cl_env_copy->nvalues=1;
	}
	ecl_stack_frame_push_values(V29);
	if((V26)==ECL_NIL){
	goto L74;}
	cl_close(1,V26)                           /*  CLOSE           */;
	goto L72;
L74:;
	goto L72;
L72:;cl_env_copy->values[0]=ecl_stack_frame_pop_values(V29);
	}
	ecl_stack_frame_close(_ecl_inner_frame);}
	}
	ecl_frs_pop(cl_env_copy);
	V28=ecl_stack_push_values(cl_env_copy);
	if((V26)==ECL_NIL){
	goto L78;}
	cl_close(3,V26,ECL_SYM("ABORT",1196),ECL_T) /*  CLOSE         */;
	goto L76;
L78:;
	goto L76;
L76:;
	ecl_stack_pop_values(cl_env_copy,V28);
	if (unwinding) ecl_unwind(cl_env_copy,next_fr);
	ECL_STACK_SET_INDEX(cl_env_copy,V27);}
	}
	ecl_stack_frame_push_values(V25);
	if((V22)==ECL_NIL){
	goto L82;}
	cl_close(1,V22)                           /*  CLOSE           */;
	goto L80;
L82:;
	goto L80;
L80:;cl_env_copy->values[0]=ecl_stack_frame_pop_values(V25);
	}
	ecl_stack_frame_close(_ecl_inner_frame);}
	}
	ecl_frs_pop(cl_env_copy);
	V24=ecl_stack_push_values(cl_env_copy);
	if((V22)==ECL_NIL){
	goto L86;}
	cl_close(3,V22,ECL_SYM("ABORT",1196),ECL_T) /*  CLOSE         */;
	goto L84;
L86:;
	goto L84;
L84:;
	ecl_stack_pop_values(cl_env_copy,V24);
	if (unwinding) ecl_unwind(cl_env_copy,next_fr);
	ECL_STACK_SET_INDEX(cl_env_copy,V23);}
	}
	ecl_stack_frame_push_values(V21);
	if((V18)==ECL_NIL){
	goto L90;}
	cl_close(1,V18)                           /*  CLOSE           */;
	goto L88;
L90:;
	goto L88;
L88:;cl_env_copy->values[0]=ecl_stack_frame_pop_values(V21);
	}
	ecl_stack_frame_close(_ecl_inner_frame);}
	}
	ecl_frs_pop(cl_env_copy);
	V20=ecl_stack_push_values(cl_env_copy);
	if((V18)==ECL_NIL){
	goto L94;}
	cl_close(3,V18,ECL_SYM("ABORT",1196),ECL_T) /*  CLOSE         */;
	goto L92;
L94:;
	goto L92;
L92:;
	ecl_stack_pop_values(cl_env_copy,V20);
	if (unwinding) ecl_unwind(cl_env_copy,next_fr);
	ECL_STACK_SET_INDEX(cl_env_copy,V19);}
	}
	ecl_stack_frame_push_values(V17);
	if((V14)==ECL_NIL){
	goto L98;}
	cl_close(1,V14)                           /*  CLOSE           */;
	goto L96;
L98:;
	goto L96;
L96:;cl_env_copy->values[0]=ecl_stack_frame_pop_values(V17);
	}
	ecl_stack_frame_close(_ecl_inner_frame);}
	}
	ecl_frs_pop(cl_env_copy);
	V16=ecl_stack_push_values(cl_env_copy);
	if((V14)==ECL_NIL){
	goto L102;}
	cl_close(3,V14,ECL_SYM("ABORT",1196),ECL_T) /*  CLOSE         */;
	goto L100;
L102:;
	goto L100;
L100:;
	ecl_stack_pop_values(cl_env_copy,V16);
	if (unwinding) ecl_unwind(cl_env_copy,next_fr);
	ECL_STACK_SET_INDEX(cl_env_copy,V15);}
	}
	ecl_stack_frame_push_values(V13);
	if((V10)==ECL_NIL){
	goto L106;}
	cl_close(1,V10)                           /*  CLOSE           */;
	goto L104;
L106:;
	goto L104;
L104:;cl_env_copy->values[0]=ecl_stack_frame_pop_values(V13);
	}
	ecl_stack_frame_close(_ecl_inner_frame);}
	}
	ecl_frs_pop(cl_env_copy);
	V12=ecl_stack_push_values(cl_env_copy);
	if((V10)==ECL_NIL){
	goto L110;}
	cl_close(3,V10,ECL_SYM("ABORT",1196),ECL_T) /*  CLOSE         */;
	goto L108;
L110:;
	goto L108;
L108:;
	ecl_stack_pop_values(cl_env_copy,V12);
	if (unwinding) ecl_unwind(cl_env_copy,next_fr);
	ECL_STACK_SET_INDEX(cl_env_copy,V11);
	ecl_bds_unwind1(cl_env_copy);
	return cl_env_copy->values[0];}
	}
	}
}}
/*	function definition for SEMINTERP-FROM-STRING                 */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L3seminterp_from_string(cl_object V1)
{ VT4 VLEX4 CLSR4 STCK4
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	cl_set(VV[8],ecl_make_fixnum(0));
	{cl_object V2;                            /*  LET3366         */
	cl_object V3;                             /*  FOL-FORMS       */
	V2= cl_read_from_string(1,V1)             /*  READ-FROM-STRING */;
	V3= ECL_NIL;
	{cl_object V4;                            /*  LET3367         */
	cl_object V5;                             /*  LET3368         */
	cl_object V6;                             /*  NXTTREE         */
	cl_object V7;                             /*  SYNT-TREES      */
	V4= ecl_car(V2);
	V5= ecl_cdr(V2);
	V6= V4;
	V7= V5;
	goto L12;
L11:;
	T0= L4singsent_sem_interp(1,V6)           /*  SINGSENT-SEM-INTERP */;
	V3= ecl_function_dispatch(cl_env_copy,VV[274])(2,V3,T0) /*  APPEND1 */;
	V6= ecl_car(V7);
	V7= ecl_cdr(V7);
L12:;
	if(V6==ECL_NIL){
	goto L21;}
	goto L11;
L21:;
	goto L19;
L19:;
	goto L9;
L9:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for SINGSENT-SEM-INTERP                   */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L4singsent_sem_interp(volatile cl_narg narg, cl_object volatile V1, ...)
{ VT5 VLEX5 CLSR5 STCK5
	const cl_env_ptr cl_env_copy = ecl_process_env();
	volatile cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	if (ecl_unlikely(narg<1)) FEwrong_num_arguments_anonym();
	if (ecl_unlikely(narg>2)) FEwrong_num_arguments_anonym();
	{
	volatile cl_object V2;
	va_list args; va_start(args,V1);
	{int i=1;
	if (i >= narg) {
	V2= ECL_NIL;
	} else {
	i++;
	V2= va_arg(args,cl_object);
	}}
	va_end(args);
	{volatile cl_object V3;                   /*  ONTOREPR        */
	volatile cl_object V4;                    /*  ONTOSHORT       */
	volatile cl_object V5;                    /*  FOL-REPR        */
	ecl_bds_bind(cl_env_copy,VV[12],ECL_NIL); /*  *FULL-TREE*     */
	V3= ECL_NIL;
	V4= ECL_NIL;
	V5= ECL_NIL;
	cl_set(VV[12],L5annotate(V1)              /*  ANNOTATE        */);
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,ecl_symbol_value(VV[10]),VV[21]) /*  MEMQ */)==ECL_NIL){
	goto L9;}
	if(!(ecl_number_compare(ecl_symbol_value(VV[8]),ecl_make_fixnum(3))>=0)){
	goto L9;}
	ecl_function_dispatch(cl_env_copy,VV[268])(2,ecl_symbol_value(VV[12]),ECL_T) /*  PRINT-ACTAVM-READABLE */;
	goto L7;
L9:;
	goto L7;
L7:;
	if(V2==ECL_NIL){
	goto L14;}
	{volatile cl_object V6;                   /*  OUTAVMPORT      */
	T0= ecl_function_dispatch(cl_env_copy,VV[265])(2,V2,VV[3]) /*  CHANGE-EXTENS */;
	T1= ecl_function_dispatch(cl_env_copy,VV[277])(1,T0) /*  BUILD-FILE-NAME */;
	V6= cl_open(7,T1,ECL_SYM("DIRECTION",1219),ECL_SYM("OUTPUT",1283),ECL_SYM("IF-EXISTS",1246),ECL_SYM("OVERWRITE",1284),ECL_SYM("IF-DOES-NOT-EXIST",1245),ECL_SYM("CREATE",1214)) /*  OPEN */;
	{
	volatile bool unwinding = FALSE;
	cl_index V7=ECL_STACK_INDEX(cl_env_copy),V8;
	ecl_frame_ptr next_fr;
	if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {
	  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;
	} else {
	{ struct ecl_stack_frame _ecl_inner_frame_aux;
	volatile cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);
	{cl_object V9;
	V9= _ecl_inner_frame;
	cl_env_copy->values[0]=ecl_function_dispatch(cl_env_copy,VV[268])(2,ecl_symbol_value(VV[12]),V6) /*  PRINT-ACTAVM-READABLE */;
	ecl_stack_frame_push_values(V9);
	if((V6)==ECL_NIL){
	goto L24;}
	cl_close(1,V6)                            /*  CLOSE           */;
	goto L22;
L24:;
	goto L22;
L22:;cl_env_copy->values[0]=ecl_stack_frame_pop_values(V9);
	}
	ecl_stack_frame_close(_ecl_inner_frame);}
	}
	ecl_frs_pop(cl_env_copy);
	V8=ecl_stack_push_values(cl_env_copy);
	if((V6)==ECL_NIL){
	goto L28;}
	cl_close(3,V6,ECL_SYM("ABORT",1196),ECL_T) /*  CLOSE          */;
	goto L26;
L28:;
	goto L26;
L26:;
	ecl_stack_pop_values(cl_env_copy,V8);
	if (unwinding) ecl_unwind(cl_env_copy,next_fr);
	ECL_STACK_SET_INDEX(cl_env_copy,V7);
	goto L12;}
	}
L14:;
	goto L12;
L12:;
	V3= ecl_function_dispatch(cl_env_copy,VV[269])(1,ecl_symbol_value(VV[12])) /*  BUILD-SEM-QUERY */;
	V4= L51simplify_onto_repr(V3)             /*  SIMPLIFY-ONTO-REPR */;
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,ecl_symbol_value(VV[10]),VV[21]) /*  MEMQ */)==ECL_NIL){
	goto L36;}
	if(!(ecl_number_compare(ecl_symbol_value(VV[8]),ecl_make_fixnum(2))>=0)){
	goto L36;}
	cl_format(3,ECL_T,VV[16],V4)              /*  FORMAT          */;
	goto L34;
L36:;
	goto L34;
L34:;
	if(V2==ECL_NIL){
	goto L41;}
	{volatile cl_object V6;                   /*  OUTSEMPORT      */
	T0= ecl_function_dispatch(cl_env_copy,VV[265])(2,V2,VV[4]) /*  CHANGE-EXTENS */;
	T1= ecl_function_dispatch(cl_env_copy,VV[277])(1,T0) /*  BUILD-FILE-NAME */;
	V6= cl_open(7,T1,ECL_SYM("DIRECTION",1219),ECL_SYM("OUTPUT",1283),ECL_SYM("IF-EXISTS",1246),ECL_SYM("OVERWRITE",1284),ECL_SYM("IF-DOES-NOT-EXIST",1245),ECL_SYM("CREATE",1214)) /*  OPEN */;
	{
	volatile bool unwinding = FALSE;
	cl_index V7=ECL_STACK_INDEX(cl_env_copy),V8;
	ecl_frame_ptr next_fr;
	if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {
	  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;
	} else {
	{ struct ecl_stack_frame _ecl_inner_frame_aux;
	volatile cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);
	{cl_object V9;
	V9= _ecl_inner_frame;
	cl_env_copy->values[0]=cl_format(3,V6,VV[16],V3) /*  FORMAT   */;
	ecl_stack_frame_push_values(V9);
	if((V6)==ECL_NIL){
	goto L52;}
	cl_close(1,V6)                            /*  CLOSE           */;
	goto L50;
L52:;
	goto L50;
L50:;cl_env_copy->values[0]=ecl_stack_frame_pop_values(V9);
	}
	ecl_stack_frame_close(_ecl_inner_frame);}
	}
	ecl_frs_pop(cl_env_copy);
	V8=ecl_stack_push_values(cl_env_copy);
	if((V6)==ECL_NIL){
	goto L56;}
	cl_close(3,V6,ECL_SYM("ABORT",1196),ECL_T) /*  CLOSE          */;
	goto L54;
L56:;
	goto L54;
L54:;
	ecl_stack_pop_values(cl_env_copy,V8);
	if (unwinding) ecl_unwind(cl_env_copy,next_fr);
	ECL_STACK_SET_INDEX(cl_env_copy,V7);}
	}
	{volatile cl_object V6;                   /*  OUTSSHPORT      */
	T0= ecl_function_dispatch(cl_env_copy,VV[265])(2,V2,VV[5]) /*  CHANGE-EXTENS */;
	T1= ecl_function_dispatch(cl_env_copy,VV[277])(1,T0) /*  BUILD-FILE-NAME */;
	V6= cl_open(7,T1,ECL_SYM("DIRECTION",1219),ECL_SYM("OUTPUT",1283),ECL_SYM("IF-EXISTS",1246),ECL_SYM("OVERWRITE",1284),ECL_SYM("IF-DOES-NOT-EXIST",1245),ECL_SYM("CREATE",1214)) /*  OPEN */;
	{
	volatile bool unwinding = FALSE;
	cl_index V7=ECL_STACK_INDEX(cl_env_copy),V8;
	ecl_frame_ptr next_fr;
	if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {
	  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;
	} else {
	{ struct ecl_stack_frame _ecl_inner_frame_aux;
	volatile cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);
	{cl_object V9;
	V9= _ecl_inner_frame;
	cl_env_copy->values[0]=cl_format(3,V6,VV[16],V4) /*  FORMAT   */;
	ecl_stack_frame_push_values(V9);
	if((V6)==ECL_NIL){
	goto L66;}
	cl_close(1,V6)                            /*  CLOSE           */;
	goto L64;
L66:;
	goto L64;
L64:;cl_env_copy->values[0]=ecl_stack_frame_pop_values(V9);
	}
	ecl_stack_frame_close(_ecl_inner_frame);}
	}
	ecl_frs_pop(cl_env_copy);
	V8=ecl_stack_push_values(cl_env_copy);
	if((V6)==ECL_NIL){
	goto L70;}
	cl_close(3,V6,ECL_SYM("ABORT",1196),ECL_T) /*  CLOSE          */;
	goto L68;
L70:;
	goto L68;
L68:;
	ecl_stack_pop_values(cl_env_copy,V8);
	if (unwinding) ecl_unwind(cl_env_copy,next_fr);
	ECL_STACK_SET_INDEX(cl_env_copy,V7);
	goto L39;}
	}
L41:;
	goto L39;
L39:;
	V5= ecl_function_dispatch(cl_env_copy,VV[270])(1,V4) /*  SEM-TO-FOL-TRANSLATION */;
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,ecl_symbol_value(VV[10]),VV[21]) /*  MEMQ */)==ECL_NIL){
	goto L76;}
	if(!(ecl_number_compare(ecl_symbol_value(VV[8]),ecl_make_fixnum(1))>=0)){
	goto L76;}
	ecl_function_dispatch(cl_env_copy,VV[271])(2,V5,ECL_T) /*  FOL-PRINT-PRETTY */;
	goto L74;
L76:;
	goto L74;
L74:;
	if(V2==ECL_NIL){
	goto L81;}
	{volatile cl_object V6;                   /*  OUTFOLPORT      */
	T0= ecl_function_dispatch(cl_env_copy,VV[265])(2,V2,VV[6]) /*  CHANGE-EXTENS */;
	T1= ecl_function_dispatch(cl_env_copy,VV[277])(1,T0) /*  BUILD-FILE-NAME */;
	V6= cl_open(7,T1,ECL_SYM("DIRECTION",1219),ECL_SYM("OUTPUT",1283),ECL_SYM("IF-EXISTS",1246),ECL_SYM("OVERWRITE",1284),ECL_SYM("IF-DOES-NOT-EXIST",1245),ECL_SYM("CREATE",1214)) /*  OPEN */;
	{
	volatile bool unwinding = FALSE;
	cl_index V7=ECL_STACK_INDEX(cl_env_copy),V8;
	ecl_frame_ptr next_fr;
	if (ecl_frs_push(cl_env_copy,ECL_PROTECT_TAG)) {
	  unwinding = TRUE; next_fr=cl_env_copy->nlj_fr;
	} else {
	{ struct ecl_stack_frame _ecl_inner_frame_aux;
	volatile cl_object _ecl_inner_frame = ecl_stack_frame_open(cl_env_copy,(cl_object)&_ecl_inner_frame_aux,0);
	{cl_object V9;
	V9= _ecl_inner_frame;
	cl_env_copy->values[0]=ecl_function_dispatch(cl_env_copy,VV[271])(2,V5,V6) /*  FOL-PRINT-PRETTY */;
	ecl_stack_frame_push_values(V9);
	if((V6)==ECL_NIL){
	goto L91;}
	cl_close(1,V6)                            /*  CLOSE           */;
	goto L89;
L91:;
	goto L89;
L89:;cl_env_copy->values[0]=ecl_stack_frame_pop_values(V9);
	}
	ecl_stack_frame_close(_ecl_inner_frame);}
	}
	ecl_frs_pop(cl_env_copy);
	V8=ecl_stack_push_values(cl_env_copy);
	if((V6)==ECL_NIL){
	goto L95;}
	cl_close(3,V6,ECL_SYM("ABORT",1196),ECL_T) /*  CLOSE          */;
	goto L93;
L95:;
	goto L93;
L93:;
	ecl_stack_pop_values(cl_env_copy,V8);
	if (unwinding) ecl_unwind(cl_env_copy,next_fr);
	ECL_STACK_SET_INDEX(cl_env_copy,V7);
	goto L79;}
	}
L81:;
	goto L79;
L79:;
	T0= cl_list(3,VV[22],ecl_symbol_value(VV[12]),V3) /*  LIST    */;
	cl_set(VV[9],CONS(T0,ecl_symbol_value(VV[9])));
	value0=V5; cl_env_copy->nvalues=1;
	ecl_bds_unwind1(cl_env_copy);
	return value0;
	}
}}
/*	function definition for ANNOTATE                              */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L5annotate(cl_object V1)
{ VT6 VLEX6 CLSR6 STCK6
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  TEMP-ANNOT-TREE */
	cl_object V3;                             /*  ANNOTATED-TREE  */
	cl_object V4;                             /*  HEADCATEG       */
	cl_object V5;                             /*  HEADMEANING     */
	cl_object V6;                             /*  OBJ-SENTOBJ     */
	V2= L25add_actavm_headmeaning(V1,ECL_NIL) /*  ADD-ACTAVM-HEADMEANING */;
	V3= L36solve_coreferences(V2,V2,ECL_NIL)  /*  SOLVE-COREFERENCES */;
	V4= ecl_function_dispatch(cl_env_copy,VV[279])(1,V3) /*  GET-ACTAVM-HEADCATEG */;
	V5= ecl_function_dispatch(cl_env_copy,VV[280])(1,V3) /*  GET-ACTAVM-HEADLEXMEAN */;
	V6= ECL_NIL;
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,V4,VV[26]) /*  MEMQ */)==ECL_NIL){
	goto L7;}
	value0=L11add_givinfo(V3,ecl_symbol_value(VV[11])) /*  ADD-GIVINFO */;
	return value0;
L7:;
	if(!((VV[27])==(V4))){
	goto L10;}
	if(!((VV[28])==(V5))){
	goto L13;}
	value0=L15add_want_obj(V3)                /*  ADD-WANT-OBJ    */;
	return value0;
L13:;
	if(!((ecl_symbol_value(VV[10]))==(VV[29]))){
	goto L16;}
	value0=L12add_getinfo(V3)                 /*  ADD-GETINFO     */;
	return value0;
L16:;
	value0=L11add_givinfo(V3,ecl_symbol_value(VV[11])) /*  ADD-GIVINFO */;
	return value0;
L10:;
	if(!((VV[30])==(V4))){
	goto L19;}
	{cl_object V7;                            /*  LET3370         */
	cl_object V8;                             /*  LET3371         */
	V7= L42find_verb_person_or_number(V3,VV[31]) /*  FIND-VERB-PERSON-OR-NUMBER */;
	V8= L42find_verb_person_or_number(V3,ECL_SYM("NUMBER",606)) /*  FIND-VERB-PERSON-OR-NUMBER */;
	if(!((ecl_make_fixnum(1))==(V7))){
	goto L24;}
	if(!((ecl_symbol_value(VV[11]))==(VV[22]))){
	goto L27;}
	if(!((VV[32])==(V5))){
	goto L30;}
	{cl_object V9;                            /*  LET3372         */
	cl_object V10;                            /*  ADJOIN-LABEL-UP */
	cl_object V11;                            /*  ADJOIN-LABEL-DOWN */
	cl_object V12;                            /*  ADJOIN-PATH     */
	cl_object V13;                            /*  ATTACH-PATH     */
	V9= L6get_sentential_object(V3)           /*  GET-SENTENTIAL-OBJECT */;
	V10= ECL_NIL;
	V11= ECL_NIL;
	V12= ECL_NIL;
	V13= ECL_NIL;
	if(!(V9==ECL_NIL)){
	goto L38;}
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L38:;
	T0= ecl_function_dispatch(cl_env_copy,VV[280])(1,V9) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[33])==(T0))){
	goto L41;}
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L41:;
	T0= ecl_function_dispatch(cl_env_copy,VV[281])(1,V9) /*  GET-ACTAVM-EXT-HEADLEXMEAN */;
	if(!((VV[34])==(T0))){
	goto L44;}
	V6= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[35],V9) /*  FIND-ACTAVM-DEP */;
	if(!(V6==ECL_NIL)){
	goto L49;}
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[36]) /*  EXCEPTION */;
	return value0;
L49:;
	T0= ecl_function_dispatch(cl_env_copy,VV[281])(1,V6) /*  GET-ACTAVM-EXT-HEADLEXMEAN */;
	if(!((VV[28])==(T0))){
	goto L52;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L52:;
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[37]) /*  EXCEPTION */;
	return value0;
L44:;
	T0= ecl_function_dispatch(cl_env_copy,VV[283])(1,V3) /*  GET-ACTAVM-HEADTYPE */;
	if(!((ECL_SYM("MOD",560))==(T0))){
	goto L56;}
	V10= VV[38];
	V11= VV[35];
	V12= VV[39];
	V13= VV[40];
	goto L54;
L56:;
	V10= VV[41];
	V11= VV[35];
	V12= VV[42];
	V13= VV[43];
	goto L54;
L54:;
	T0= cl_list(3,VV[33],V10,VV[44])          /*  LIST            */;
	T1= cl_list(3,V12,T0,V11)                 /*  LIST            */;
	V3= L16adjoin_subtree(V3,T1)              /*  ADJOIN-SUBTREE  */;
	T0= cl_list(2,V13,VV[45])                 /*  LIST            */;
	value0=L20attach_subtree(V3,T0)           /*  ATTACH-SUBTREE  */;
	return value0;
	}
L30:;
	if(!((VV[46])==(V5))){
	goto L75;}
	{cl_object V14;                           /*  SENTOBJ         */
	V14= L6get_sentential_object(V3)          /*  GET-SENTENTIAL-OBJECT */;
	T0= ecl_function_dispatch(cl_env_copy,VV[281])(1,V14) /*  GET-ACTAVM-EXT-HEADLEXMEAN */;
	if(!((VV[47])==(T0))){
	goto L79;}
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L79:;
	value0=L13add_want_know(V3)               /*  ADD-WANT-KNOW   */;
	return value0;
	}
L75:;
	if(!((VV[33])==(V5))){
	goto L82;}
	value0=L14add_want(V3)                    /*  ADD-WANT        */;
	return value0;
L82:;
	if(!((VV[48])==(V5))){
	goto L85;}
	{cl_object V15;                           /*  LET3373         */
	cl_object V16;                            /*  PREDCOMPL       */
	cl_object V17;                            /*  PREDMEAN        */
	V15= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[49],V3) /*  FIND-ACTAVM-DEP */;
	V16= V15;
	V17= ECL_NIL;
	if(!(V16==ECL_NIL)){
	goto L91;}
	V16= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[50],V3) /*  FIND-ACTAVM-DEP */;
	if(!(V16==ECL_NIL)){
	goto L96;}
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L96:;
	V17= ecl_function_dispatch(cl_env_copy,VV[280])(1,V16) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((V17)==(VV[51]))){
	goto L106;}
	goto L103;
	goto L104;
L106:;
	goto L104;
L104:;
	if(!((V17)==(VV[53]))){
	goto L101;}
	goto L102;
L103:;
L102:;
	value0=L11add_givinfo(V3,ecl_symbol_value(VV[11])) /*  ADD-GIVINFO */;
	return value0;
L101:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L91:;
	T0= ecl_function_dispatch(cl_env_copy,VV[280])(1,V16) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[55])==(T0))){
	goto L110;}
	{cl_object V18;                           /*  PREPOSIT        */
	V18= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[56],V16) /*  FIND-ACTAVM-DEP */;
	if(V18==ECL_NIL){
	goto L114;}
	value0=L11add_givinfo(V18,ecl_symbol_value(VV[11])) /*  ADD-GIVINFO */;
	return value0;
L114:;
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[57]) /*  EXCEPTION */;
	return value0;
	}
L110:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
	}
L85:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L27:;
	if(!((ecl_symbol_value(VV[11]))==(VV[58]))){
	goto L117;}
	if(!((V8)==(VV[59]))){
	goto L120;}
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L120:;
	if(!((V8)==(VV[60]))){
	goto L123;}
	if(!((V5)==(VV[61]))){
	goto L126;}
	value0=L11add_givinfo(V3,ecl_symbol_value(VV[11])) /*  ADD-GIVINFO */;
	return value0;
L126:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L123:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L117:;
	value0=(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[62]) /*  BREAK */;
	return value0;
L24:;
	if(!((ecl_make_fixnum(2))==(V7))){
	goto L129;}
	T0= ecl_function_dispatch(cl_env_copy,VV[284])(1,V3) /*  GET-ACTAVM-HEADMOOD */;
	if(!((VV[63])==(T0))){
	goto L132;}
	if((ecl_function_dispatch(cl_env_copy,VV[285])(2,V5,VV[64]) /*  ONE-IS-SUBCLASS-OF */)==ECL_NIL){
	goto L132;}
	value0=L14add_want(V3)                    /*  ADD-WANT        */;
	return value0;
L132:;
	if(!((VV[46])==(V5))){
	goto L136;}
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L136:;
	if((L46is_sem_interrogative(V3)           /*  IS-SEM-INTERROGATIVE */)==ECL_NIL){
	goto L139;}
	value0=L13add_want_know(V3)               /*  ADD-WANT-KNOW   */;
	return value0;
L139:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L129:;
	if(!((ecl_make_fixnum(3))==(V7))){
	goto L142;}
	if(!((VV[46])==(V5))){
	goto L145;}
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L145:;
	if(!((VV[32])==(V5))){
	goto L148;}
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L148:;
	if((L46is_sem_interrogative(V3)           /*  IS-SEM-INTERROGATIVE */)==ECL_NIL){
	goto L151;}
	value0=L13add_want_know(V3)               /*  ADD-WANT-KNOW   */;
	return value0;
L151:;
	value0=L11add_givinfo(V3,ecl_symbol_value(VV[11])) /*  ADD-GIVINFO */;
	return value0;
L142:;
	T0= ecl_function_dispatch(cl_env_copy,VV[284])(1,V3) /*  GET-ACTAVM-HEADMOOD */;
	if(!((VV[65])==(T0))){
	goto L154;}
	if((ecl_function_dispatch(cl_env_copy,VV[285])(2,V5,VV[64]) /*  ONE-IS-SUBCLASS-OF */)==ECL_NIL){
	goto L154;}
	V3= L14add_want(V3)                       /*  ADD-WANT        */;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L154:;
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[66]) /*  EXCEPTION */;
	return value0;
	}
L19:;
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,V4,VV[67]) /*  MEMQ */)==ECL_NIL){
	goto L159;}
	value0=L11add_givinfo(V3,ecl_symbol_value(VV[11])) /*  ADD-GIVINFO */;
	return value0;
L159:;
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(3,VV[17],VV[68],V4) /*  EXCEPTION */;
	return value0;
	}
}}
/*	function definition for GET-SENTENTIAL-OBJECT                 */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L6get_sentential_object(cl_object V1)
{ VT7 VLEX7 CLSR7 STCK7
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	V1= L45skip_question_tense_marker(V1)     /*  SKIP-QUESTION-TENSE-MARKER */;
	T0= ecl_function_dispatch(cl_env_copy,VV[283])(1,V1) /*  GET-ACTAVM-HEADTYPE */;
	if((ECL_SYM("MOD",560))==(T0)){
	goto L6;}
	T0= ecl_function_dispatch(cl_env_copy,VV[287])(1,V1) /*  GET-ACTAVM-HEADSYN */;
	if(!(T0==ECL_NIL)){
	goto L4;}
	goto L5;
L6:;
L5:;
	value0=ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[38],V1) /*  FIND-ACTAVM-DEP */;
	return value0;
L4:;
	{cl_object V2;                            /*  VERB-OBJ        */
	cl_object V3;                             /*  VERB-OBJ-CAT    */
	V2= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[35],V1) /*  FIND-ACTAVM-DEP */;
	V3= ecl_function_dispatch(cl_env_copy,VV[279])(1,V2) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[70])==(V3))){
	goto L11;}
	{cl_object V4;                            /*  PREP-ARG        */
	V4= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[41],V2) /*  FIND-ACTAVM-DEP */;
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V4) /*  GET-ACTAVM-HEADCATEG */;
	if((VV[30])==(T0)){
	goto L17;}
	T0= ecl_function_dispatch(cl_env_copy,VV[287])(1,V4) /*  GET-ACTAVM-HEADSYN */;
	if(!(T0==ECL_NIL)){
	goto L15;}
	goto L16;
L17:;
L16:;
	value0=V4; cl_env_copy->nvalues=1;
	return value0;
L15:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
	}
L11:;
	if(!((VV[71])==(V3))){
	goto L20;}
	{cl_object V5;                            /*  CONJ-ARG        */
	V5= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[72],V2) /*  FIND-ACTAVM-DEP */;
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V5) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[30])==(T0))){
	goto L24;}
	value0=V5; cl_env_copy->nvalues=1;
	return value0;
L24:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
	}
L20:;
	if(!((VV[30])==(V3))){
	goto L27;}
	value0=V2; cl_env_copy->nvalues=1;
	return value0;
L27:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for IS-A-ACTAVM-NOUN-COMPLEX              */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L7is_a_actavm_noun_complex(cl_object V1)
{ VT8 VLEX8 CLSR8 STCK8
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  LET3374         */
	cl_object V3;                             /*  LET3375         */
	cl_object V4;                             /*  LABEL           */
	cl_object V5;                             /*  DOWNTREE        */
	V2= ecl_function_dispatch(cl_env_copy,VV[279])(1,V1) /*  GET-ACTAVM-HEADCATEG */;
	V3= ecl_function_dispatch(cl_env_copy,VV[283])(1,V1) /*  GET-ACTAVM-HEADTYPE */;
	V4= ECL_NIL;
	V5= ECL_NIL;
	if(!((V2)==(VV[74]))){
	goto L6;}
	value0=ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[75],V1) /*  FIND-ACTAVM-DEP */;
	return value0;
L6:;
	if(!((V2)==(VV[76]))){
	goto L9;}
	if(!((V3)==(VV[77]))){
	goto L14;}
	V4= VV[78];
	goto L12;
L14:;
	if(!((V3)==(VV[79]))){
	goto L17;}
	V4= VV[80];
	goto L12;
L17:;
	V4= ECL_NIL;
	goto L12;
L12:;
	V5= ecl_function_dispatch(cl_env_copy,VV[282])(2,V4,V1) /*  FIND-ACTAVM-DEP */;
	if(!(V5==ECL_NIL)){
	goto L22;}
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,ECL_SYM("PARSE-ERROR",627),VV[81]) /*  EXCEPTION */;
	return value0;
L22:;
	value0=V5; cl_env_copy->nvalues=1;
	return value0;
L9:;
	if(!((V2)==(VV[82]))){
	goto L25;}
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,V3,VV[83]) /*  MEMQ */)==ECL_NIL){
	goto L25;}
	if(!((V3)==(VV[84]))){
	goto L31;}
	V4= VV[80];
	goto L29;
L31:;
	if(!((V3)==(VV[79]))){
	goto L34;}
	V4= VV[75];
	goto L29;
L34:;
	if(!((V3)==(VV[85]))){
	goto L37;}
	V4= VV[78];
	goto L29;
L37:;
	if(!((V3)==(VV[86]))){
	goto L40;}
	V4= VV[78];
	goto L29;
L40:;
	if(!((V3)==(VV[87]))){
	goto L43;}
	V4= VV[88];
	goto L29;
L43:;
	V4= ECL_NIL;
	goto L29;
L29:;
	V5= ecl_function_dispatch(cl_env_copy,VV[282])(2,V4,V1) /*  FIND-ACTAVM-DEP */;
	if(!(V5==ECL_NIL)){
	goto L48;}
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,ECL_SYM("PARSE-ERROR",627),VV[89]) /*  EXCEPTION */;
	return value0;
L48:;
	value0=V5; cl_env_copy->nvalues=1;
	return value0;
L25:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for GET-STANDARD-OBJECT                   */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L8get_standard_object(cl_object V1)
{ VT9 VLEX9 CLSR9 STCK9
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  LET3376         */
	cl_object V3;                             /*  LOCUT           */
	cl_object V4;                             /*  LOCUT-ARG       */
	V2= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[35],V1) /*  FIND-ACTAVM-DEP */;
	V3= ECL_NIL;
	V4= ECL_NIL;
	if(V2==ECL_NIL){
	goto L5;}
	value0=V2; cl_env_copy->nvalues=1;
	return value0;
L5:;
	V3= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[91],V1) /*  FIND-ACTAVM-DEP */;
	if(!(V3==ECL_NIL)){
	goto L10;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L10:;
	V4= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[92],V3) /*  FIND-ACTAVM-DEP */;
	value0=ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[41],V4) /*  FIND-ACTAVM-DEP */;
	return value0;
	}
}}
/*	function definition for GET-PREPOSITION-ARG                   */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L9get_preposition_arg(cl_object V1)
{ VT10 VLEX10 CLSR10 STCK10
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  LET3377         */
	cl_object V3;                             /*  DOWN-PREP       */
	V2= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[41],V1) /*  FIND-ACTAVM-DEP */;
	V3= ECL_NIL;
	if(V2==ECL_NIL){
	goto L4;}
	value0=V2; cl_env_copy->nvalues=1;
	return value0;
L4:;
	V3= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[94],V1) /*  FIND-ACTAVM-DEP */;
	if(!(V3==ECL_NIL)){
	goto L9;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L9:;
	value0=ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[41],V3) /*  FIND-ACTAVM-DEP */;
	return value0;
	}
}}
/*	function definition for MAKE-LEXMEAN                          */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L10make_lexmean(cl_object V1)
{ VT11 VLEX11 CLSR11 STCK11
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	T0= cl_list(2,VV[96],V1)                  /*  LIST            */;
	value0=ecl_list1(T0); cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for ADD-GIVINFO                           */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L11add_givinfo(cl_object V1, cl_object V2)
{ VT12 VLEX12 CLSR12 STCK12
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	T0= CONS(V2,VV[101]);
	T1= cl_list(3,VV[100],T0,VV[102])         /*  LIST            */;
	T2= cl_list(3,VV[98],VV[99],T1)           /*  LIST            */;
	T3= cl_list(3,ECL_NIL,T2,VV[41])          /*  LIST            */;
	value0=L16adjoin_subtree(V1,T3)           /*  ADJOIN-SUBTREE  */;
	return value0;
}}
/*	function definition for ADD-GETINFO                           */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L12add_getinfo(cl_object V1)
{ VT13 VLEX13 CLSR13 STCK13
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	value0=L16adjoin_subtree(V1,VV[104])      /*  ADJOIN-SUBTREE  */;
	return value0;
}}
/*	function definition for ADD-WANT-KNOW                         */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L13add_want_know(cl_object V1)
{ VT14 VLEX14 CLSR14 STCK14
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	value0=L16adjoin_subtree(V1,VV[106])      /*  ADJOIN-SUBTREE  */;
	return value0;
}}
/*	function definition for ADD-WANT                              */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L14add_want(cl_object V1)
{ VT15 VLEX15 CLSR15 STCK15
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	value0=L16adjoin_subtree(V1,VV[108])      /*  ADJOIN-SUBTREE  */;
	return value0;
}}
/*	function definition for ADD-WANT-OBJ                          */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L15add_want_obj(cl_object V1)
{ VT16 VLEX16 CLSR16 STCK16
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	value0=L16adjoin_subtree(V1,VV[110])      /*  ADJOIN-SUBTREE  */;
	return value0;
}}
/*	function definition for ADJOIN-SUBTREE                        */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L16adjoin_subtree(cl_object V1, cl_object V2)
{ VT17 VLEX17 CLSR17 STCK17
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  LET3378         */
	cl_object V4;                             /*  LET3379         */
	cl_object V5;                             /*  LET3380         */
	V3= ecl_car(V2);
	V4= ecl_cadr(V2);
	V5= ecl_caddr(V2);
	if(!(V3==ECL_NIL)){
	goto L5;}
	T0= ecl_list1(V4);
	T1= L18expand_adjoin_tree(T0,V1,V5,ecl_make_fixnum(0),ecl_make_fixnum(9)) /*  EXPAND-ADJOIN-TREE */;
	value0=ecl_car(T1); cl_env_copy->nvalues=1;
	return value0;
L5:;
	T0= ecl_function_dispatch(cl_env_copy,VV[298])(1,V1) /*  GET-ACTAVM-HEAD */;
	T1= cl_list(2,VV[112],T0)                 /*  LIST            */;
	T2= ecl_function_dispatch(cl_env_copy,VV[299])(1,V1) /*  GET-ACTAVM-DEPENDENTS */;
	T3= ecl_function_dispatch(cl_env_copy,VV[300])(1,V1) /*  GET-ACTAVM-HEADNUMB */;
	T4= L17replace_dependent(T2,V3,V4,V5,T3,ecl_make_fixnum(9)) /*  REPLACE-DEPENDENT */;
	T5= cl_list(2,VV[113],T4)                 /*  LIST            */;
	value0=cl_list(2,T1,T5)                   /*  LIST            */;
	return value0;
	}
}}
/*	function definition for REPLACE-DEPENDENT                     */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L17replace_dependent(cl_object V1, cl_object V2, cl_object V3, cl_object V4, cl_object V5, cl_object V6)
{ VT18 VLEX18 CLSR18 STCK18
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[115]) /*  EXCEPTION */;
	return value0;
L2:;
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	if((T1)==(CODE_CHAR(35))){
	goto L7;}
	T0= ecl_car(V2);
	T1= ecl_car(V1);
	T2= ecl_function_dispatch(cl_env_copy,VV[302])(1,T1) /*  GET-ACTAVM-HEADLINK */;
	if((ecl_function_dispatch(cl_env_copy,VV[303])(2,T0,T2) /*  NEQ */)==ECL_NIL){
	goto L5;}
	goto L6;
L7:;
L6:;
	T0= ecl_car(V1);
	T1= ecl_cdr(V1);
	T3= ecl_car(V1);
	if((ecl_function_dispatch(cl_env_copy,VV[304])(1,T3) /*  IS-A-ACTAVM-TRACE? */)==ECL_NIL){
	goto L11;}
	T3= ecl_car(V1);
	T4= ecl_function_dispatch(cl_env_copy,VV[300])(1,T3) /*  GET-ACTAVM-HEADNUMB */;
	T2= ecl_cadr(T4);
	goto L9;
L11:;
	T2= ecl_make_fixnum(9);
	goto L9;
L9:;
	T3= L17replace_dependent(T1,V2,V3,V4,V5,T2) /*  REPLACE-DEPENDENT */;
	value0=CONS(T0,T3); cl_env_copy->nvalues=1;
	return value0;
L5:;
	T0= ecl_cdr(V2);
	if(!(T0==ECL_NIL)){
	goto L14;}
	T0= ecl_list1(V3);
	T1= ecl_car(V1);
	T2= L18expand_adjoin_tree(T0,T1,V4,V5,V6) /*  EXPAND-ADJOIN-TREE */;
	T3= ecl_cdr(V1);
	value0=ecl_append(T2,T3); cl_env_copy->nvalues=1;
	return value0;
L14:;
	T0= ecl_car(V1);
	T1= ecl_function_dispatch(cl_env_copy,VV[298])(1,T0) /*  GET-ACTAVM-HEAD */;
	T2= cl_list(2,VV[112],T1)                 /*  LIST            */;
	T3= ecl_car(V1);
	T4= ecl_function_dispatch(cl_env_copy,VV[299])(1,T3) /*  GET-ACTAVM-DEPENDENTS */;
	T5= ecl_cdr(V2);
	T6= L17replace_dependent(T4,T5,V3,V4,V5,ecl_make_fixnum(9)) /*  REPLACE-DEPENDENT */;
	T7= cl_list(2,VV[113],T6)                 /*  LIST            */;
	T8= cl_list(2,T2,T7)                      /*  LIST            */;
	T9= ecl_cdr(V1);
	value0=CONS(T8,T9); cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for EXPAND-ADJOIN-TREE                    */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L18expand_adjoin_tree(cl_object V1, cl_object V2, cl_object V3, cl_object V4, cl_object V5)
{ VT19 VLEX19 CLSR19 STCK19
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
	ecl_bds_bind(cl_env_copy,VV[117],V5);     /*  LASTTRACEIND    */
TTL:
	if(!(ecl_numberp(V4))){
	goto L2;}
	value0=L19int_expand_adjoin_tree(V1,V2,V3,V4) /*  INT-EXPAND-ADJOIN-TREE */;
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L2:;
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(3,VV[17],VV[118],V4) /*  EXCEPTION */;
	ecl_bds_unwind1(cl_env_copy);
	return value0;
}}
/*	function definition for INT-EXPAND-ADJOIN-TREE                */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L19int_expand_adjoin_tree(cl_object V1, cl_object V2, cl_object V3, cl_object V4)
{ VT20 VLEX20 CLSR20 STCK20
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L2:;
	{cl_object V5;                            /*  FIRSTDEP        */
	V5= ecl_car(V1);
	if(!(ecl_equal(V5,VV[100]))){
	goto L6;}
	T0= ecl_cdr(V1);
	T1= L19int_expand_adjoin_tree(T0,V2,V3,V4) /*  INT-EXPAND-ADJOIN-TREE */;
	value0=CONS(V5,T1); cl_env_copy->nvalues=1;
	return value0;
L6:;
	if(!(ecl_equal(V5,VV[120]))){
	goto L9;}
	T0= ecl_function_dispatch(cl_env_copy,VV[298])(1,V2) /*  GET-ACTAVM-HEAD */;
	T1= L40subst_head_val(T0,VV[121],V3)      /*  SUBST-HEAD-VAL  */;
	T2= cl_list(2,VV[112],T1)                 /*  LIST            */;
	T3= ecl_function_dispatch(cl_env_copy,VV[299])(1,V2) /*  GET-ACTAVM-DEPENDENTS */;
	T4= cl_list(2,VV[113],T3)                 /*  LIST            */;
	T5= cl_list(2,T2,T4)                      /*  LIST            */;
	T6= ecl_cdr(V1);
	T7= L19int_expand_adjoin_tree(T6,V2,V3,V4) /*  INT-EXPAND-ADJOIN-TREE */;
	value0=CONS(T5,T7); cl_env_copy->nvalues=1;
	return value0;
L9:;
	cl_set(VV[117],ecl_one_plus(ecl_symbol_value(VV[117])));
	T0= ecl_car(V5);
	T1= cl_list(2,VV[96],T0)                  /*  LIST            */;
	T2= ecl_list1(T1);
	T3= cl_list(2,VV[124],T2)                 /*  LIST            */;
	T4= ecl_cadr(V5);
	T5= cl_list(2,VV[121],T4)                 /*  LIST            */;
	T6= cl_list(2,V4,ecl_symbol_value(VV[117])) /*  LIST          */;
	T7= cl_list(2,ECL_SYM("POSITION",644),T6) /*  LIST            */;
	T8= cl_list(5,VV[122],VV[123],T3,T5,T7)   /*  LIST            */;
	T9= cl_list(2,VV[112],T8)                 /*  LIST            */;
	T10= ecl_caddr(V5);
	T11= L19int_expand_adjoin_tree(T10,V2,V3,V4) /*  INT-EXPAND-ADJOIN-TREE */;
	T12= cl_list(2,VV[113],T11)               /*  LIST            */;
	T13= cl_list(2,T9,T12)                    /*  LIST            */;
	T14= ecl_cdr(V1);
	T15= L19int_expand_adjoin_tree(T14,V2,V3,V4) /*  INT-EXPAND-ADJOIN-TREE */;
	value0=CONS(T13,T15); cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for ATTACH-SUBTREE                        */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L20attach_subtree(cl_object V1, cl_object V2)
{ VT21 VLEX21 CLSR21 STCK21
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  LET3381         */
	cl_object V4;                             /*  LET3382         */
	V3= ecl_car(V2);
	V4= ecl_cadr(V2);
	if(!(V3==ECL_NIL)){
	goto L4;}
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[126]) /*  EXCEPTION */;
	return value0;
L4:;
	T0= ecl_function_dispatch(cl_env_copy,VV[298])(1,V1) /*  GET-ACTAVM-HEAD */;
	T1= cl_list(2,VV[112],T0)                 /*  LIST            */;
	T2= ecl_function_dispatch(cl_env_copy,VV[299])(1,V1) /*  GET-ACTAVM-DEPENDENTS */;
	T3= ecl_function_dispatch(cl_env_copy,VV[300])(1,V1) /*  GET-ACTAVM-HEADNUMB */;
	T4= L21attach_dependent(T2,V3,V4,T3)      /*  ATTACH-DEPENDENT */;
	T5= cl_list(2,VV[113],T4)                 /*  LIST            */;
	value0=cl_list(2,T1,T5)                   /*  LIST            */;
	return value0;
	}
}}
/*	function definition for ATTACH-DEPENDENT                      */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L21attach_dependent(cl_object V1, cl_object V2, cl_object V3, cl_object V4)
{ VT22 VLEX22 CLSR22 STCK22
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[128]) /*  EXCEPTION */;
	return value0;
L2:;
	{cl_object V5;                            /*  FIRSTDEP        */
	V5= ecl_car(V1);
	if(!(V2==ECL_NIL)){
	goto L6;}
	T0= ecl_car(V5);
	if(!((T0)==(CODE_CHAR(35)))){
	goto L9;}
	T0= ecl_cdr(V1);
	T1= L22skip_traces_and_expand(T0,V3,V4,ecl_make_fixnum(9)) /*  SKIP-TRACES-AND-EXPAND */;
	value0=CONS(V5,T1); cl_env_copy->nvalues=1;
	return value0;
L9:;
	T0= ecl_cdr(V1);
	T1= L21attach_dependent(T0,ECL_NIL,V3,V4) /*  ATTACH-DEPENDENT */;
	value0=CONS(V5,T1); cl_env_copy->nvalues=1;
	return value0;
L6:;
	T0= ecl_car(V5);
	if((T0)==(CODE_CHAR(35))){
	goto L14;}
	T0= ecl_car(V2);
	T1= ecl_function_dispatch(cl_env_copy,VV[302])(1,V5) /*  GET-ACTAVM-HEADLINK */;
	if((ecl_function_dispatch(cl_env_copy,VV[303])(2,T0,T1) /*  NEQ */)==ECL_NIL){
	goto L12;}
	goto L13;
L14:;
L13:;
	T0= ecl_cdr(V1);
	T1= L21attach_dependent(T0,V2,V3,V4)      /*  ATTACH-DEPENDENT */;
	value0=CONS(V5,T1); cl_env_copy->nvalues=1;
	return value0;
L12:;
	T0= ecl_function_dispatch(cl_env_copy,VV[298])(1,V5) /*  GET-ACTAVM-HEAD */;
	T1= cl_list(2,VV[112],T0)                 /*  LIST            */;
	T2= ecl_function_dispatch(cl_env_copy,VV[299])(1,V5) /*  GET-ACTAVM-DEPENDENTS */;
	T3= ecl_cdr(V2);
	T4= ecl_function_dispatch(cl_env_copy,VV[300])(1,V5) /*  GET-ACTAVM-HEADNUMB */;
	T5= L21attach_dependent(T2,T3,V3,T4)      /*  ATTACH-DEPENDENT */;
	T6= cl_list(2,VV[113],T5)                 /*  LIST            */;
	T7= cl_list(2,T1,T6)                      /*  LIST            */;
	T8= ecl_cdr(V1);
	value0=CONS(T7,T8); cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for SKIP-TRACES-AND-EXPAND                */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L22skip_traces_and_expand(cl_object V1, cl_object V2, cl_object V3, cl_object V4)
{ VT23 VLEX23 CLSR23 STCK23
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	T0= ecl_list1(V2);
	value0=L23expand_tree(T0,V3,V4)           /*  EXPAND-TREE     */;
	return value0;
L2:;
	T0= ecl_car(V1);
	if((ecl_function_dispatch(cl_env_copy,VV[304])(1,T0) /*  IS-A-ACTAVM-TRACE? */)==ECL_NIL){
	goto L5;}
	T0= ecl_car(V1);
	T1= ecl_cdr(V1);
	T2= ecl_car(V1);
	T3= ecl_function_dispatch(cl_env_copy,VV[300])(1,T2) /*  GET-ACTAVM-HEADNUMB */;
	T4= ecl_cadr(T3);
	T5= L22skip_traces_and_expand(T1,V2,V3,T4) /*  SKIP-TRACES-AND-EXPAND */;
	value0=CONS(T0,T5); cl_env_copy->nvalues=1;
	return value0;
L5:;
	T0= L23expand_tree(V2,V3,V4)              /*  EXPAND-TREE     */;
	value0=CONS(T0,V1); cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for EXPAND-TREE                           */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L23expand_tree(cl_object V1, cl_object V2, cl_object V3)
{ VT24 VLEX24 CLSR24 STCK24
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
	ecl_bds_bind(cl_env_copy,VV[117],V3);     /*  LASTTRACEIND    */
TTL:
	if(!(ecl_numberp(V2))){
	goto L2;}
	T0= ecl_list1(V1);
	T1= L24int_expand_tree(T0,V2)             /*  INT-EXPAND-TREE */;
	value0=ecl_car(T1); cl_env_copy->nvalues=1;
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L2:;
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(3,VV[17],VV[131],V2) /*  EXCEPTION */;
	ecl_bds_unwind1(cl_env_copy);
	return value0;
}}
/*	function definition for INT-EXPAND-TREE                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L24int_expand_tree(cl_object V1, cl_object V2)
{ VT25 VLEX25 CLSR25 STCK25
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L2:;
	{cl_object V3;                            /*  FIRSTDEP        */
	V3= ecl_car(V1);
	if(!(ecl_equal(V3,VV[100]))){
	goto L6;}
	T0= ecl_cdr(V1);
	T1= L24int_expand_tree(T0,V2)             /*  INT-EXPAND-TREE */;
	value0=CONS(V3,T1); cl_env_copy->nvalues=1;
	return value0;
L6:;
	cl_set(VV[117],ecl_one_plus(ecl_symbol_value(VV[117])));
	T0= ecl_car(V3);
	T1= cl_list(2,VV[96],T0)                  /*  LIST            */;
	T2= ecl_list1(T1);
	T3= cl_list(2,VV[124],T2)                 /*  LIST            */;
	T4= ecl_cadr(V3);
	T5= cl_list(2,VV[121],T4)                 /*  LIST            */;
	T6= cl_list(2,V2,ecl_symbol_value(VV[117])) /*  LIST          */;
	T7= cl_list(2,ECL_SYM("POSITION",644),T6) /*  LIST            */;
	T8= cl_list(5,VV[122],VV[123],T3,T5,T7)   /*  LIST            */;
	T9= cl_list(2,VV[112],T8)                 /*  LIST            */;
	T10= ecl_caddr(V3);
	T11= L24int_expand_tree(T10,V2)           /*  INT-EXPAND-TREE */;
	T12= cl_list(2,VV[113],T11)               /*  LIST            */;
	T13= cl_list(2,T9,T12)                    /*  LIST            */;
	T14= ecl_cdr(V1);
	T15= L24int_expand_tree(T14,V2)           /*  INT-EXPAND-TREE */;
	value0=CONS(T13,T15); cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for ADD-ACTAVM-HEADMEANING                */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L25add_actavm_headmeaning(cl_object V1, cl_object V2)
{ VT26 VLEX26 CLSR26 STCK26
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(ecl_equal(V1,VV[100]))){
	goto L2;}
	value0=V1; cl_env_copy->nvalues=1;
	return value0;
L2:;
	{cl_object V3;                            /*  NEWHEAD         */
	cl_object V4;                             /*  NEWTREE         */
	T0= ecl_function_dispatch(cl_env_copy,VV[313])(1,V1) /*  GET-ACTAVM-HEADLEMMA */;
	T1= ecl_function_dispatch(cl_env_copy,VV[302])(1,V1) /*  GET-ACTAVM-HEADLINK */;
	V3= L27add_head_wordmeaning(V1,T0,T1,V2)  /*  ADD-HEAD-WORDMEANING */;
	T0= ecl_function_dispatch(cl_env_copy,VV[299])(1,V1) /*  GET-ACTAVM-DEPENDENTS */;
	T1= cl_list(2,VV[113],T0)                 /*  LIST            */;
	V4= ecl_function_dispatch(cl_env_copy,VV[274])(2,V3,T1) /*  APPEND1 */;
	{cl_object V5;                            /*  LET3384         */
	cl_object V6;
	cl_object V7;
	V5= ecl_function_dispatch(cl_env_copy,VV[299])(1,V1) /*  GET-ACTAVM-DEPENDENTS */;
	V6= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V5))) FEtype_error_list(V5);
	V7= V5;
	{cl_object V8;
	cl_object V9;
	V8= ecl_list1(ECL_NIL);
	V9= V8;
L14:;
	if(!(ecl_endp(V7))){
	goto L18;}
	goto L15;
L18:;
	goto L16;
L16:;
	V6= _ecl_car(V7);
	{cl_object V10;
	V10= _ecl_cdr(V7);
	if (ecl_unlikely(!ECL_LISTP(V10))) FEtype_error_list(V10);
	V7= V10;
	}
	if (ecl_unlikely(ECL_ATOM(V9))) FEtype_error_cons(V9);
	T1= V9;
	T2= L25add_actavm_headmeaning(V6,V4)      /*  ADD-ACTAVM-HEADMEANING */;
	V9= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V9,T1);
	goto L14;
L15:;
	T0= ecl_cdr(V8);
	goto L6;
	}
	}
L6:;
	T1= cl_list(2,VV[113],T0)                 /*  LIST            */;
	T2= ecl_list1(T1);
	value0=ecl_append(V3,T2); cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for ENCLOSED-IN-QUOTES                    */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L26enclosed_in_quotes(cl_object V1)
{ VT27 VLEX27 CLSR27 STCK27
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  DEPS            */
	V2= ecl_function_dispatch(cl_env_copy,VV[299])(1,V1) /*  GET-ACTAVM-DEPENDENTS */;
	T0= ecl_car(V2);
	if(ecl_equal(T0,VV[100])){
	goto L3;}
	{cl_object V3;
	T0= ecl_car(V2);
	V3= ecl_function_dispatch(cl_env_copy,VV[313])(1,T0) /*  GET-ACTAVM-HEADLEMMA */;
	if(!(((V3)==ECL_CODE_CHAR((ecl_character)(34))))){
	goto L10;}
	goto L7;
	goto L8;
L10:;
	goto L8;
L8:;
	if(!(((V3)==ECL_CODE_CHAR((ecl_character)(39))))){
	goto L3;}
	goto L5;
L7:;
	}
L5:;
	T0= ecl_function_dispatch(cl_env_copy,VV[315])(1,V2) /*  ULT  */;
	if(ecl_equal(T0,VV[100])){
	goto L3;}
	{cl_object V3;
	T0= ecl_function_dispatch(cl_env_copy,VV[315])(1,V2) /*  ULT  */;
	V3= ecl_function_dispatch(cl_env_copy,VV[313])(1,T0) /*  GET-ACTAVM-HEADLEMMA */;
	if(!(((V3)==ECL_CODE_CHAR((ecl_character)(34))))){
	goto L19;}
	value0=VV[135];
	goto L17;
L19:;
	value0=ECL_NIL;
	goto L17;
L17:;
	if((value0)!=ECL_NIL){
	goto L16;}
	if(!(((V3)==ECL_CODE_CHAR((ecl_character)(39))))){
	goto L22;}
	value0=VV[136]; cl_env_copy->nvalues=1;
	return value0;
L22:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L16:;
	cl_env_copy->nvalues=1;
	return value0;
	}
L3:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for ADD-HEAD-WORDMEANING                  */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L27add_head_wordmeaning(cl_object V1, cl_object V2, cl_object V3, cl_object V4)
{ VT28 VLEX28 CLSR28 STCK28
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V5;                            /*  HEAD            */
	cl_object V6;                             /*  ROLE            */
	cl_object V7;                             /*  CATEG           */
	cl_object V8;                             /*  TYPE            */
	cl_object V9;                             /*  HPERSON         */
	cl_object V10;                            /*  HNUMBER         */
	cl_object V11;                            /*  UP-CATEG        */
	cl_object V12;                            /*  UP-PERSON       */
	cl_object V13;                            /*  NEWSEM          */
	cl_object V14;                            /*  COREF-IDENT     */
	cl_object V15;                            /*  ANAPH           */
	cl_object V16;                            /*  NEWNUMB         */
	cl_object V17;                            /*  INST-CLASS      */
	cl_object V18;                            /*  VTYPE           */
	cl_object V19;                            /*  WMEAN           */
	V5= ecl_function_dispatch(cl_env_copy,VV[298])(1,V1) /*  GET-ACTAVM-HEAD */;
	V6= ecl_function_dispatch(cl_env_copy,VV[302])(1,V1) /*  GET-ACTAVM-HEADLINK */;
	V7= ecl_function_dispatch(cl_env_copy,VV[279])(1,V1) /*  GET-ACTAVM-HEADCATEG */;
	V8= ecl_function_dispatch(cl_env_copy,VV[283])(1,V1) /*  GET-ACTAVM-HEADTYPE */;
	V9= ecl_function_dispatch(cl_env_copy,VV[317])(1,V1) /*  GET-ACTAVM-HEADPERSON */;
	V10= ecl_function_dispatch(cl_env_copy,VV[318])(1,V1) /*  GET-ACTAVM-HEADNUMBER */;
	V11= ecl_function_dispatch(cl_env_copy,VV[279])(1,V4) /*  GET-ACTAVM-HEADCATEG */;
	if(!((V11)==(VV[30]))){
	goto L10;}
	V12= L42find_verb_person_or_number(V4,VV[31]) /*  FIND-VERB-PERSON-OR-NUMBER */;
	goto L8;
L10:;
	V12= ECL_NIL;
	goto L8;
L8:;
	V13= ECL_NIL;
	V14= ECL_NIL;
	V15= ECL_NIL;
	V16= ECL_NIL;
	V17= ECL_NIL;
	V18= ECL_NIL;
	V19= L28get_word_meaning(4,V2,V7,V8,V1)   /*  GET-WORD-MEANING */;
	if((L26enclosed_in_quotes(V1)             /*  ENCLOSED-IN-QUOTES */)==ECL_NIL){
	goto L21;}
	{cl_object V20;                           /*  INST-CLASS      */
	T0= ecl_car(V19);
	V20= ecl_function_dispatch(cl_env_copy,VV[319])(1,T0) /*  GET-INSTANCE-CLASS */;
	if(!(V20==ECL_NIL)){
	goto L25;}
	T0= L48read_actavm_sent(V1,ecl_make_fixnum(0),ECL_T) /*  READ-ACTAVM-SENT */;
	T1= cl_list(2,VV[140],T0)                 /*  LIST            */;
	V13= cl_list(2,VV[139],T1)                /*  LIST            */;
	goto L19;
L25:;
	T0= cl_list(2,VV[96],V20)                 /*  LIST            */;
	T1= ecl_car(V19);
	T2= cl_list(2,VV[140],T1)                 /*  LIST            */;
	V13= cl_list(2,T0,T2)                     /*  LIST            */;
	goto L19;
	}
L21:;
	if(!((V7)==(VV[141]))){
	goto L30;}
	if(!((V3)==(VV[142]))){
	goto L33;}
	T0= ecl_function_dispatch(cl_env_copy,VV[283])(1,V4) /*  GET-ACTAVM-HEADTYPE */;
	if(!((T0)==(ECL_SYM("MOD",560)))){
	goto L33;}
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,V9,VV[143]) /*  MEMQ */)!=ECL_NIL){
	goto L33;}
	if(!((V9)==(VV[144]))){
	goto L39;}
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,V12,VV[143]) /*  MEMQ */)!=ECL_NIL){
	goto L33;}
	goto L34;
L39:;
	goto L34;
L34:;
	V13= VV[145];
	goto L19;
L33:;
	T0= ecl_car(V19);
	if(!((T0)==(VV[146]))){
	goto L43;}
	V13= L10make_lexmean(VV[147])             /*  MAKE-LEXMEAN    */;
	goto L19;
L43:;
	if(!((V9)==(ecl_make_fixnum(1)))){
	goto L47;}
	if(!((V10)==(VV[59]))){
	goto L50;}
	V13= L10make_lexmean(VV[22])              /*  MAKE-LEXMEAN    */;
	goto L19;
L50:;
	V13= L10make_lexmean(VV[148])             /*  MAKE-LEXMEAN    */;
	goto L19;
L47:;
	if(!((V9)==(ecl_make_fixnum(2)))){
	goto L55;}
	V13= L10make_lexmean(VV[58])              /*  MAKE-LEXMEAN    */;
	goto L19;
L55:;
	if(!((V9)==(VV[144]))){
	goto L59;}
	T0= ecl_function_dispatch(cl_env_copy,VV[320])(1,V1) /*  GET-ACTAVM-HEADFORM */;
	if(!((T0)==(CODE_CHAR(116)))){
	goto L62;}
	V14= ecl_function_dispatch(cl_env_copy,VV[321])(1,V1) /*  GET-ACTAVM-HEADCOREF */;
	if(!((V14)==(VV[149]))){
	goto L67;}
	if(!(V4==ECL_NIL)){
	goto L70;}
	V13= VV[145];
	goto L19;
L70:;
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V4) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[30])==(T0))){
	goto L74;}
	if(!((V3)==(VV[142]))){
	goto L77;}
	if(!((ecl_make_fixnum(1))==(V12))){
	goto L80;}
	if(!((VV[60])==(V10))){
	goto L83;}
	V13= L10make_lexmean(VV[148])             /*  MAKE-LEXMEAN    */;
	goto L19;
L83:;
	V13= L10make_lexmean(VV[22])              /*  MAKE-LEXMEAN    */;
	goto L19;
L80:;
	if(!((ecl_make_fixnum(2))==(V12))){
	goto L88;}
	V13= L10make_lexmean(VV[58])              /*  MAKE-LEXMEAN    */;
	goto L19;
L88:;
	if(!((ecl_make_fixnum(3))==(V12))){
	goto L92;}
	if(!((VV[60])==(V10))){
	goto L92;}
	V13= L10make_lexmean(VV[150])             /*  MAKE-LEXMEAN    */;
	goto L19;
L92:;
	T0= ecl_function_dispatch(cl_env_copy,VV[284])(1,V4) /*  GET-ACTAVM-HEADMOOD */;
	if(!((VV[65])==(T0))){
	goto L97;}
	V13= L10make_lexmean(VV[22])              /*  MAKE-LEXMEAN    */;
	goto L19;
L97:;
	V15= L33get_last_anaph_ref(V4,V6)         /*  GET-LAST-ANAPH-REF */;
	V13= ecl_car(V15);
	V16= ecl_cadr(V15);
	goto L19;
L77:;
	if(!((V3)==(VV[151]))){
	goto L106;}
	T0= ecl_function_dispatch(cl_env_copy,VV[313])(1,V1) /*  GET-ACTAVM-HEADLEMMA */;
	if(!((VV[152])==(T0))){
	goto L109;}
	V13= L10make_lexmean(VV[150])             /*  MAKE-LEXMEAN    */;
	goto L19;
L109:;
	V13= VV[145];
	goto L19;
L106:;
	V13= VV[145];
	goto L19;
L74:;
	V13= VV[145];
	goto L19;
L67:;
	V13= VV[145];
	goto L19;
L62:;
	ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[153]) /*  EXCEPTION */;
	goto L19;
L59:;
	if(!((V2)==(VV[152]))){
	goto L117;}
	if(!(V9==ECL_NIL)){
	goto L117;}
	V13= L10make_lexmean(VV[147])             /*  MAKE-LEXMEAN    */;
	goto L19;
L117:;
	T0= ecl_car(V19);
	T1= cl_list(2,VV[96],T0)                  /*  LIST            */;
	V13= ecl_list1(T1);
	goto L19;
L30:;
	if(!((V7)==(VV[74]))){
	goto L123;}
	{cl_object V21;                           /*  MONTH           */
	V21= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[154],V1) /*  FIND-ACTAVM-DEP */;
	if(!(V21==ECL_NIL)){
	goto L128;}
	{cl_object V22;                           /*  PREP-DEP        */
	V22= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[92],V1) /*  FIND-ACTAVM-DEP */;
	if(V22==ECL_NIL){
	goto L132;}
	V21= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[41],V22) /*  FIND-ACTAVM-DEP */;
	if(V21==ECL_NIL){
	goto L137;}
	T0= ecl_function_dispatch(cl_env_copy,VV[313])(1,V21) /*  GET-ACTAVM-HEADLEMMA */;
	T1= ecl_function_dispatch(cl_env_copy,VV[279])(1,V21) /*  GET-ACTAVM-HEADCATEG */;
	T2= ecl_function_dispatch(cl_env_copy,VV[283])(1,V21) /*  GET-ACTAVM-HEADTYPE */;
	T3= L28get_word_meaning(4,T0,T1,T2,V1)    /*  GET-WORD-MEANING */;
	T4= ecl_car(T3);
	T5= ecl_function_dispatch(cl_env_copy,VV[319])(1,T4) /*  GET-INSTANCE-CLASS */;
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,VV[155],T5) /*  MEMQ */)!=ECL_NIL){
	goto L137;}
	V21= ECL_NIL;
	goto L126;
L137:;
	goto L126;
L132:;
	goto L126;
	}
L128:;
	goto L126;
L126:;
	if(!(V21==ECL_NIL)){
	goto L143;}
	if(!((ecl_symbol_value(VV[138]))==(VV[156]))){
	goto L143;}
	V21= ecl_make_bool((V3)==(VV[157]));
	goto L141;
L143:;
	goto L141;
L141:;
	if(!(V21==ECL_NIL)){
	goto L148;}
	T0= ecl_function_dispatch(cl_env_copy,VV[322])(1,V1) /*  GET-ACTAVM-HEADVALUE */;
	T1= cl_list(2,VV[96],T0)                  /*  LIST            */;
	V13= ecl_list1(T1);
	goto L19;
L148:;
	V13= L10make_lexmean(VV[158])             /*  MAKE-LEXMEAN    */;
	goto L19;
	}
L123:;
	if(!((V7)==(VV[82]))){
	goto L153;}
	T0= ecl_function_dispatch(cl_env_copy,VV[283])(1,V1) /*  GET-ACTAVM-HEADTYPE */;
	if(!((T0)==(VV[159]))){
	goto L153;}
	if(!((ecl_symbol_value(VV[138]))==(VV[156]))){
	goto L153;}
	T0= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[154],V1) /*  FIND-ACTAVM-DEP */;
	if(T0==ECL_NIL){
	goto L153;}
	V13= L10make_lexmean(VV[158])             /*  MAKE-LEXMEAN    */;
	goto L19;
L153:;
	if(!((V7)==(VV[27]))){
	goto L160;}
	T0= ecl_function_dispatch(cl_env_copy,VV[302])(1,V1) /*  GET-ACTAVM-HEADLINK */;
	if(!((VV[91])==(T0))){
	goto L160;}
	V13= VV[160];
	goto L19;
L160:;
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,V7,VV[161]) /*  MEMQ */)==ECL_NIL){
	goto L165;}
	V18= ecl_function_dispatch(cl_env_copy,VV[283])(1,V1) /*  GET-ACTAVM-HEADTYPE */;
	if(!((V7)==(VV[30]))){
	goto L173;}
	if((V18)==(VV[162])){
	goto L170;}
	goto L171;
L173:;
	goto L171;
L171:;
	T0= ecl_car(V19);
	if(!(ECL_LISTP(T0))){
	goto L176;}
	T0= ecl_car(V19);
	T1= ecl_cadr(T0);
	if(!(ECL_LISTP(T1))){
	goto L176;}
	T0= ecl_car(V19);
	T1= ecl_cadr(T0);
	T2= ecl_car(T1);
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,T2,VV[163]) /*  MEMQ */)==ECL_NIL){
	goto L176;}
	T0= ecl_car(V19);
	T1= ecl_car(T0);
	T2= cl_list(2,VV[96],T1)                  /*  LIST            */;
	T3= ecl_car(V19);
	T4= ecl_cadr(T3);
	T5= ecl_car(T4);
	T6= ecl_car(V19);
	T7= ecl_cadr(T6);
	T8= ecl_cdr(T7);
	T9= cl_list(2,T5,T8)                      /*  LIST            */;
	V13= cl_list(2,T2,T9)                     /*  LIST            */;
	goto L19;
L176:;
	T0= ecl_car(V19);
	if(!(ECL_LISTP(T0))){
	goto L182;}
	T0= ecl_car(V19);
	T1= ecl_car(T0);
	if(!(ECL_LISTP(T1))){
	goto L182;}
	T0= ecl_car(V19);
	V13= L10make_lexmean(T0)                  /*  MAKE-LEXMEAN    */;
	goto L19;
L182:;
	T0= ecl_cdr(V19);
	if(!(T0==ECL_NIL)){
	goto L187;}
	T0= ecl_car(V19);
	if(!(ECL_ATOM(T0))){
	goto L190;}
	T0= ecl_car(V19);
	V17= ecl_function_dispatch(cl_env_copy,VV[319])(1,T0) /*  GET-INSTANCE-CLASS */;
	if(!(V17==ECL_NIL)){
	goto L195;}
	T0= ecl_car(V19);
	T1= cl_list(2,VV[96],T0)                  /*  LIST            */;
	V13= ecl_list1(T1);
	goto L19;
L195:;
	T0= cl_list(2,VV[96],V17)                 /*  LIST            */;
	T1= ecl_car(V19);
	T2= cl_list(2,VV[140],T1)                 /*  LIST            */;
	V13= cl_list(2,T0,T2)                     /*  LIST            */;
	goto L19;
L190:;
	(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[164]) /*  BREAK */;
	goto L19;
L187:;
	T0= ecl_function_dispatch(cl_env_copy,VV[323])(2,V19,V7) /*  LEGGI */;
	V19= ecl_car(T0);
	T0= ecl_car(V19);
	V13= L10make_lexmean(T0)                  /*  MAKE-LEXMEAN    */;
	goto L19;
L170:;
	V13= VV[145];
	goto L19;
L165:;
	if(!((V7)==(VV[71]))){
	goto L204;}
	T0= ecl_car(V19);
	T1= cl_list(2,VV[96],T0)                  /*  LIST            */;
	V13= ecl_list1(T1);
	goto L19;
L204:;
	T0= ecl_cdr(V19);
	if(!(T0==ECL_NIL)){
	goto L208;}
	T0= ecl_car(V19);
	if(!(ECL_ATOM(T0))){
	goto L211;}
	{cl_object V22;                           /*  INST-CLASS      */
	T0= ecl_car(V19);
	V22= ecl_function_dispatch(cl_env_copy,VV[319])(1,T0) /*  GET-INSTANCE-CLASS */;
	if(!(V22==ECL_NIL)){
	goto L215;}
	T0= ecl_car(V19);
	T1= cl_list(2,VV[96],T0)                  /*  LIST            */;
	V13= ecl_list1(T1);
	goto L19;
L215:;
	T0= cl_list(2,VV[96],V22)                 /*  LIST            */;
	T1= ecl_car(V19);
	T2= cl_list(2,VV[140],T1)                 /*  LIST            */;
	V13= cl_list(2,T0,T2)                     /*  LIST            */;
	goto L19;
	}
L211:;
	(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[164]) /*  BREAK */;
	goto L19;
L208:;
	T0= ecl_function_dispatch(cl_env_copy,VV[323])(2,V19,V7) /*  LEGGI */;
	V19= ecl_car(T0);
	T0= cl_list(2,VV[96],V19)                 /*  LIST            */;
	V13= ecl_list1(T0);
	goto L19;
L19:;
	T0= ecl_function_dispatch(cl_env_copy,VV[324])(2,V5,VV[165]) /*  GET-FLATAVM-FEAT-VAL */;
	if(!(T0==ECL_NIL)){
	goto L223;}
	T0= ecl_assql(VV[166],V5);
	T1= ecl_assql(ECL_SYM("POSITION",644),V5);
	if(!(V16==ECL_NIL)){
	goto L229;}
	T2= ecl_assql(VV[167],V5);
	goto L227;
L229:;
	T3= ecl_assql(VV[167],V5);
	T4= ecl_cadr(T3);
	T5= L39subst_syn_val(T4,ECL_SYM("NUMBER",606),V16) /*  SUBST-SYN-VAL */;
	T2= cl_list(2,VV[167],T5)                 /*  LIST            */;
	goto L227;
L227:;
	T3= ecl_assql(VV[121],V5);
	T4= ecl_function_dispatch(cl_env_copy,VV[323])(2,V5,VV[124]) /*  LEGGI */;
	T5= ecl_car(T4);
	T6= ecl_append(T5,V13);
	T7= cl_list(2,VV[124],T6)                 /*  LIST            */;
	T8= cl_list(5,T0,T1,T2,T3,T7)             /*  LIST            */;
	T9= cl_list(2,VV[112],T8)                 /*  LIST            */;
	value0=ecl_list1(T9); cl_env_copy->nvalues=1;
	return value0;
L223:;
	T0= ecl_assql(VV[166],V5);
	T1= ecl_assql(ECL_SYM("POSITION",644),V5);
	if(!(V16==ECL_NIL)){
	goto L237;}
	T2= ecl_assql(VV[167],V5);
	goto L235;
L237:;
	T3= ecl_assql(VV[167],V5);
	T4= ecl_cadr(T3);
	T5= L39subst_syn_val(T4,ECL_SYM("NUMBER",606),V16) /*  SUBST-SYN-VAL */;
	T2= cl_list(2,VV[167],T5)                 /*  LIST            */;
	goto L235;
L235:;
	T3= ecl_assql(VV[165],V5);
	T4= ecl_assql(VV[121],V5);
	T5= ecl_function_dispatch(cl_env_copy,VV[323])(2,V5,VV[124]) /*  LEGGI */;
	T6= ecl_car(T5);
	T7= ecl_append(T6,V13);
	T8= cl_list(2,VV[124],T7)                 /*  LIST            */;
	T9= cl_list(6,T0,T1,T2,T3,T4,T8)          /*  LIST            */;
	T10= cl_list(2,VV[112],T9)                /*  LIST            */;
	value0=ecl_list1(T10); cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for GET-WORD-MEANING                      */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L28get_word_meaning(cl_narg narg, cl_object V1, cl_object V2, cl_object V3, cl_object V4, ...)
{ VT29 VLEX29 CLSR29 STCK29
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	if (ecl_unlikely(narg<4)) FEwrong_num_arguments_anonym();
	if (ecl_unlikely(narg>5)) FEwrong_num_arguments_anonym();
	{
	cl_object V5;
	va_list args; va_start(args,V4);
	{int i=4;
	if (i >= narg) {
	V5= ECL_NIL;
	} else {
	i++;
	V5= va_arg(args,cl_object);
	}}
	va_end(args);
	{cl_object V6;                            /*  LET3385         */
	cl_object V7;                             /*  MEANING         */
	cl_object V8;                             /*  DEPS            */
	cl_object V9;                             /*  FOUND           */
	cl_object V10;                            /*  NEWWORD         */
	cl_object V11;                            /*  FMEAN           */
	V6= ecl_function_dispatch(cl_env_copy,VV[326])(2,ecl_symbol_value(VV[169]),V1) /*  ALL-ACCENT-LEGGI */;
	V7= V6;
	V8= ECL_NIL;
	V9= ECL_NIL;
	V10= ECL_NIL;
	V11= ECL_NIL;
	V11= ecl_car(V7);
	if(!(V7==ECL_NIL)){
	goto L11;}
	if(!((V2)==(VV[30]))){
	goto L14;}
	T0= ecl_function_dispatch(cl_env_copy,VV[299])(1,V4) /*  GET-ACTAVM-DEPENDENTS */;
	V8= ecl_function_dispatch(cl_env_copy,VV[327])(1,T0) /*  REMOVE-HEAD-MARKER */;
	{cl_object V12;                           /*  LET3386         */
	cl_object V13;                            /*  LET3387         */
	cl_object V14;                            /*  NXTDEP          */
	cl_object V15;                            /*  DEPS            */
	V12= ecl_car(V8);
	V13= ecl_cdr(V8);
	V14= V12;
	V15= V13;
	goto L25;
L24:;
	T0= ecl_function_dispatch(cl_env_copy,VV[302])(1,V14) /*  GET-ACTAVM-HEADLINK */;
	T1= ecl_function_dispatch(cl_env_copy,VV[328])(1,T0) /*  EXPLODE */;
	T2= ecl_function_dispatch(cl_env_copy,VV[329])(2,T1,CODE_CHAR(42)) /*  EXPL+CATS */;
	if((ecl_memql(VV[170],T2))==ECL_NIL){
	goto L29;}
	V9= V14;
	goto L27;
L29:;
	goto L27;
L27:;
	V14= ecl_car(V15);
	V15= ecl_cdr(V15);
L25:;
	if(V15==ECL_NIL){
	goto L37;}
	if((V9)!=ECL_NIL){
	goto L37;}
	goto L24;
L37:;
	goto L35;
L35:;
	goto L22;
L22:;
	if((V9)==ECL_NIL){
	goto L41;}
	T0= ecl_function_dispatch(cl_env_copy,VV[328])(1,V1) /*  EXPLODE */;
	T1= ecl_function_dispatch(cl_env_copy,VV[313])(1,V9) /*  GET-ACTAVM-HEADLEMMA */;
	T2= ecl_function_dispatch(cl_env_copy,VV[328])(1,T1) /*  EXPLODE */;
	T3= cl_append(3,T0,VV[171],T2)            /*  APPEND          */;
	V10= ecl_function_dispatch(cl_env_copy,VV[330])(1,T3) /*  IMPLODE */;
	V7= ecl_function_dispatch(cl_env_copy,VV[326])(2,ecl_symbol_value(VV[169]),V10) /*  ALL-ACCENT-LEGGI */;
	value0=V7; cl_env_copy->nvalues=1;
	return value0;
L41:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
	}
L14:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L11:;
	if(!(ECL_ATOM(V11))){
	goto L47;}
	T0= ecl_cadr(V7);
	if(!(T0==ECL_NIL)){
	goto L50;}
	value0=V7; cl_env_copy->nvalues=1;
	return value0;
L50:;
	{cl_object V16;                           /*  DEP             */
	T0= ecl_cadr(V7);
	T1= ecl_list1(T0);
	T2= ecl_function_dispatch(cl_env_copy,VV[299])(1,V4) /*  GET-ACTAVM-DEPENDENTS */;
	V16= ecl_function_dispatch(cl_env_copy,VV[331])(3,VV[172],T1,T2) /*  FIND-ACTAVM-DESCENDANT */;
	if(!(V16==ECL_NIL)){
	goto L54;}
	T0= ecl_car(V7);
	value0=ecl_list1(T0); cl_env_copy->nvalues=1;
	return value0;
L54:;
	T0= ecl_caddr(V7);
	value0=ecl_list1(T0); cl_env_copy->nvalues=1;
	return value0;
	}
L47:;
	T0= ecl_car(V11);
	if(!(ECL_ATOM(T0))){
	goto L57;}
	T0= ecl_cadr(V11);
	if(!(ECL_LISTP(T0))){
	goto L60;}
	T0= ecl_cadr(V11);
	T1= ecl_car(T0);
	if(!((VV[173])==(T1))){
	goto L63;}
	if(!(V4==ECL_NIL)){
	goto L66;}
	value0=V7; cl_env_copy->nvalues=1;
	return value0;
L66:;
	T0= L29choose_best_grid(V7,V4)            /*  CHOOSE-BEST-GRID */;
	value0=ecl_list1(T0); cl_env_copy->nvalues=1;
	return value0;
L63:;
	T0= ecl_cadr(V11);
	T1= ecl_car(T0);
	if(!((VV[174])==(T1))){
	goto L69;}
	value0=V7; cl_env_copy->nvalues=1;
	return value0;
L69:;
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[175]) /*  EXCEPTION */;
	return value0;
L60:;
	T0= ecl_function_dispatch(cl_env_copy,VV[323])(2,V7,V2) /*  LEGGI */;
	value0=ecl_car(T0); cl_env_copy->nvalues=1;
	return value0;
L57:;
	T0= ecl_cadr(V11);
	if(T0==ECL_NIL){
	goto L72;}
	T0= ecl_car(V11);
	T1= ecl_cadr(T0);
	if(!(ECL_ATOM(T1))){
	goto L75;}
	T0= cl_list(2,V2,V3)                      /*  LIST            */;
	value0=ecl_function_dispatch(cl_env_copy,VV[332])(2,V7,T0) /*  LIST-LEGGI */;
	return value0;
L75:;
	value0=L32feature_based_disamb(V7,V2,V4,V5) /*  FEATURE-BASED-DISAMB */;
	return value0;
L72:;
	value0=ecl_car(V11); cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for CHOOSE-BEST-GRID                      */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L29choose_best_grid(cl_object V1, cl_object V2)
{ VT30 VLEX30 CLSR30 STCK30
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  DEPS            */
	cl_object V4;                             /*  BEST-GRID       */
	cl_object V5;                             /*  BEST-MATCH      */
	T0= ecl_function_dispatch(cl_env_copy,VV[299])(1,V2) /*  GET-ACTAVM-DEPENDENTS */;
	V3= ecl_function_dispatch(cl_env_copy,VV[327])(1,T0) /*  REMOVE-HEAD-MARKER */;
	V4= ecl_car(V1);
	T0= ecl_cadr(V4);
	T1= ecl_cdr(T0);
	V5= L30them_grid_match(T1,V3)             /*  THEM-GRID-MATCH */;
	{cl_object V6;                            /*  LET3388         */
	cl_object V7;                             /*  LET3389         */
	cl_object V8;                             /*  NXTGRID         */
	cl_object V9;                             /*  THEM-GRIDS      */
	V6= ecl_cadr(V1);
	T0= ecl_cdr(V1);
	V7= ecl_cdr(T0);
	V8= V6;
	V9= V7;
	goto L11;
L10:;
	T0= ecl_cadr(V8);
	T1= ecl_cdr(T0);
	T2= L30them_grid_match(T1,V3)             /*  THEM-GRID-MATCH */;
	if(!(ecl_number_compare(T2,V5)<0)){
	goto L15;}
	V4= V8;
	goto L13;
L15:;
	goto L13;
L13:;
	V8= ecl_car(V9);
	V9= ecl_cdr(V9);
L11:;
	if(V8==ECL_NIL){
	goto L23;}
	goto L10;
L23:;
	goto L21;
L21:;
	goto L8;
L8:;
	value0=V4; cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for THEM-GRID-MATCH                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L30them_grid_match(cl_object V1, cl_object V2)
{ VT31 VLEX31 CLSR31 STCK31
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  LET3390         */
	cl_object V4;                             /*  NUM-MISSING     */
	cl_object V5;                             /*  REMDEPS         */
	{cl_object V6;
	cl_object V7;
	V6= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V1))) FEtype_error_list(V1);
	V7= V1;
	{cl_object V8;
	cl_object V9;
	V8= ecl_list1(ECL_NIL);
	V9= V8;
L8:;
	if(!(ecl_endp(V7))){
	goto L12;}
	goto L9;
L12:;
	goto L10;
L10:;
	V6= _ecl_car(V7);
	{cl_object V10;
	V10= _ecl_cdr(V7);
	if (ecl_unlikely(!ECL_LISTP(V10))) FEtype_error_list(V10);
	V7= V10;
	}
	if (ecl_unlikely(ECL_ATOM(V9))) FEtype_error_cons(V9);
	T0= V9;
	T1= ecl_car(V6);
	V9= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V9,T0);
	goto L8;
L9:;
	V3= ecl_cdr(V8);
	goto L1;
	}
	}
L1:;
	V4= ecl_make_fixnum(0);
	V5= V2;
	{cl_object V6;                            /*  LET3395         */
	cl_object V7;                             /*  LET3396         */
	cl_object V8;                             /*  NXTLAB          */
	cl_object V9;                             /*  GRIDLAB         */
	V6= ecl_car(V3);
	V7= ecl_cdr(V3);
	V8= V6;
	V9= V7;
	goto L35;
L34:;
	cl_env_copy->values[0]=L31search_grid_element(V8,V4,V5) /*  SEARCH-GRID-ELEMENT */;
	{int V10=cl_env_copy->nvalues-0;
	if (V10--<=0) goto L39;
	V4= cl_env_copy->values[0];
	if (V10--<=0) goto L40;
	V5= cl_env_copy->values[1];
	goto L41;}
L39:;
	V4= ECL_NIL;
L40:;
	V5= ECL_NIL;
L41:;
	V8= ecl_car(V9);
	V9= ecl_cdr(V9);
L35:;
	if(V8==ECL_NIL){
	goto L47;}
	goto L34;
L47:;
	goto L45;
L45:;
	goto L32;
L32:;
	{cl_fixnum V10;
	V10= ecl_length(V5);
	T0= ecl_times(ecl_make_fixnum(3),ecl_make_fixnum(V10));
	value0=ecl_plus(V4,T0); cl_env_copy->nvalues=1;
	return value0;}
	}
	}
}}
/*	function definition for SEARCH-GRID-ELEMENT                   */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L31search_grid_element(cl_object V1, cl_object V2, cl_object V3)
{ VT32 VLEX32 CLSR32 STCK32
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V4;                            /*  PREVDEPS        */
	cl_object V5;                             /*  NXTDEP          */
	cl_object V6;                             /*  REMDEPS         */
	V4= ECL_NIL;
	V5= ecl_car(V3);
	V6= ecl_cdr(V3);
	goto L7;
L6:;
	V4= CONS(V5,V4);
	V5= ecl_car(V6);
	V6= ecl_cdr(V6);
L7:;
	T0= ecl_function_dispatch(cl_env_copy,VV[302])(1,V5) /*  GET-ACTAVM-HEADLINK */;
	if((V1)==(T0)){
	goto L17;}
	if(V5==ECL_NIL){
	goto L17;}
	goto L6;
L17:;
	goto L15;
L15:;
	goto L4;
L4:;
	T0= ecl_function_dispatch(cl_env_copy,VV[302])(1,V5) /*  GET-ACTAVM-HEADLINK */;
	if(!((V1)==(T0))){
	goto L21;}
	T0= ecl_append(V4,V6);
	cl_env_copy->nvalues=2;
	cl_env_copy->values[1]=T0;
	cl_env_copy->values[0]=V2;
	return cl_env_copy->values[0];
L21:;
	T0= ecl_one_plus(V2);
	cl_env_copy->nvalues=2;
	cl_env_copy->values[1]=V4;
	cl_env_copy->values[0]=T0;
	return cl_env_copy->values[0];
	}
}}
/*	function definition for FEATURE-BASED-DISAMB                  */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L32feature_based_disamb(cl_object V1, cl_object V2, cl_object V3, cl_object V4)
{ VT33 VLEX33 CLSR33 STCK33
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L2:;
	{cl_object V5;                            /*  FIRSTCASE       */
	cl_object V6;                             /*  SELECTOR        */
	cl_object V7;                             /*  CONCEPT         */
	cl_object V8;                             /*  SELCATEG        */
	cl_object V9;                             /*  SELFEAT         */
	cl_object V10;                            /*  FEATNAME        */
	cl_object V11;                            /*  FEATVAL         */
	cl_object V12;                            /*  TREEFEATURE     */
	V5= ecl_car(V1);
	V6= ecl_car(V5);
	V7= ecl_cdr(V5);
	V8= ecl_car(V6);
	V9= ecl_cadr(V6);
	V10= ecl_car(V9);
	V11= ecl_cadr(V9);
	if(!(V3==ECL_NIL)){
	goto L13;}
	T0= ecl_function_dispatch(cl_env_copy,VV[323])(2,V4,V10) /*  LEGGI */;
	V12= ecl_car(T0);
	goto L11;
L13:;
	V12= ecl_function_dispatch(cl_env_copy,VV[337])(2,V10,V3) /*  GET-ACTAVM-FEATVAL */;
	goto L11;
L11:;
	if(!((V2)==(V8))){
	goto L16;}
	if(!((V11)==(V12))){
	goto L16;}
	value0=V7; cl_env_copy->nvalues=1;
	return value0;
L16:;
	V1= ecl_cdr(V1);
	goto TTL;
	}
}}
/*	function definition for GET-LAST-ANAPH-REF                    */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L33get_last_anaph_ref(cl_object V1, cl_object V2)
{ VT34 VLEX34 CLSR34 STCK34
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  FOUND           */
	V3= ECL_NIL;
	{cl_object V4;                            /*  LET3397         */
	cl_object V5;                             /*  LET3398         */
	cl_object V6;                             /*  NEXTTURN        */
	cl_object V7;                             /*  REMTURNS        */
	V4= ecl_car(ecl_symbol_value(VV[9]));
	V5= ecl_cdr(ecl_symbol_value(VV[9]));
	V6= V4;
	V7= V5;
	goto L9;
L8:;
	V3= L34check_anaph_last_turn(V6,V1,V2)    /*  CHECK-ANAPH-LAST-TURN */;
	V6= ecl_car(V7);
	V7= ecl_cdr(V7);
L9:;
	if(V6==ECL_NIL){
	goto L18;}
	if((V3)!=ECL_NIL){
	goto L18;}
	goto L8;
L18:;
	goto L16;
L16:;
	goto L6;
L6:;
	if(!(V6==ECL_NIL)){
	goto L22;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L22:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for CHECK-ANAPH-LAST-TURN                 */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L34check_anaph_last_turn(cl_object V1, cl_object V2, cl_object V3)
{ VT35 VLEX35 CLSR35 STCK35
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	T0= ecl_car(V1);
	if(!((T0)==(VV[58]))){
	goto L2;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L2:;
	T0= ecl_cadr(V1);
	T1= L45skip_question_tense_marker(V2)     /*  SKIP-QUESTION-TENSE-MARKER */;
	value0=L35find_anaph_in_tree(T0,T1,V3,ECL_NIL) /*  FIND-ANAPH-IN-TREE */;
	return value0;
}}
/*	function definition for FIND-ANAPH-IN-TREE                    */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L35find_anaph_in_tree(cl_object V1, cl_object V2, cl_object V3, cl_object V4)
{ VT36 VLEX36 CLSR36 STCK36
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V5;                            /*  FOUND           */
	V5= ECL_NIL;
	if(ecl_equal(V1,VV[100])){
	goto L3;}
	{cl_object V6;                            /*  LET3399         */
	cl_object V7;                             /*  LET3400         */
	V6= ecl_function_dispatch(cl_env_copy,VV[279])(1,V1) /*  GET-ACTAVM-HEADCATEG */;
	T0= ecl_function_dispatch(cl_env_copy,VV[299])(1,V1) /*  GET-ACTAVM-DEPENDENTS */;
	V7= cl_reverse(T0)                        /*  REVERSE         */;
	{cl_object V8;                            /*  LET3401         */
	cl_object V9;                             /*  LET3402         */
	cl_object V10;                            /*  NXTDEP          */
	cl_object V11;                            /*  DEPS            */
	V8= ecl_car(V7);
	V9= ecl_cdr(V7);
	V10= V8;
	V11= V9;
	goto L14;
L13:;
	if(!(ecl_equal(V10,VV[100]))){
	goto L18;}
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V1) /*  GET-ACTAVM-HEADCATEG */;
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,T0,VV[183]) /*  MEMQ */)==ECL_NIL){
	goto L21;}
	if(!((VV[27])==(V4))){
	goto L24;}
	goto L16;
L24:;
	if(V2==ECL_NIL){
	goto L29;}
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V2) /*  GET-ACTAVM-HEADCATEG */;
	if((ecl_function_dispatch(cl_env_copy,VV[303])(2,VV[30],T0) /*  NEQ */)==ECL_NIL){
	goto L27;}
	goto L28;
L29:;
L28:;
	T0= ecl_function_dispatch(cl_env_copy,VV[341])(1,V1) /*  GET-ACTAVM-HEADSEM */;
	T1= ecl_function_dispatch(cl_env_copy,VV[318])(1,V1) /*  GET-ACTAVM-HEADNUMBER */;
	V5= cl_list(2,T0,T1)                      /*  LIST            */;
	goto L16;
L27:;
	T0= ecl_function_dispatch(cl_env_copy,VV[341])(1,V1) /*  GET-ACTAVM-HEADSEM */;
	T1= ecl_function_dispatch(cl_env_copy,VV[341])(1,V2) /*  GET-ACTAVM-HEADSEM */;
	if((ecl_function_dispatch(cl_env_copy,VV[342])(3,T0,T1,V3) /*  CHECK-COMPLEM-COMPATIB */)==ECL_NIL){
	goto L33;}
	T0= ecl_function_dispatch(cl_env_copy,VV[341])(1,V1) /*  GET-ACTAVM-HEADSEM */;
	T1= ecl_function_dispatch(cl_env_copy,VV[318])(1,V1) /*  GET-ACTAVM-HEADNUMBER */;
	V5= cl_list(2,T0,T1)                      /*  LIST            */;
	goto L16;
L33:;
	goto L16;
L21:;
	goto L16;
L18:;
	V5= L35find_anaph_in_tree(V10,V2,V3,V6)   /*  FIND-ANAPH-IN-TREE */;
	goto L16;
L16:;
	V10= ecl_car(V11);
	V11= ecl_cdr(V11);
L14:;
	if(V10==ECL_NIL){
	goto L42;}
	if((V5)!=ECL_NIL){
	goto L42;}
	goto L13;
L42:;
	goto L40;
L40:;
	goto L11;
L11:;
	value0=V5; cl_env_copy->nvalues=1;
	return value0;
	}
	}
L3:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for SOLVE-COREFERENCES                    */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L36solve_coreferences(cl_object V1, cl_object V2, cl_object V3)
{ VT37 VLEX37 CLSR37 STCK37
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(ecl_equal(V1,VV[100]))){
	goto L2;}
	value0=V1; cl_env_copy->nvalues=1;
	return value0;
L2:;
	{cl_object V4;                            /*  THEAD           */
	V4= ecl_function_dispatch(cl_env_copy,VV[298])(1,V1) /*  GET-ACTAVM-HEAD */;
	if((ecl_function_dispatch(cl_env_copy,VV[344])(1,V1) /*  IS-A-COREF-TRACE? */)==ECL_NIL){
	goto L6;}
	T0= ecl_function_dispatch(cl_env_copy,VV[280])(1,V1) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!(T0==ECL_NIL)){
	goto L6;}
	T0= ecl_function_dispatch(cl_env_copy,VV[345])(1,V1) /*  GET-ACTAVM-HEADCOREFTYPE */;
	if(!((CODE_CHAR(112))==(T0))){
	goto L10;}
	T0= ecl_assql(VV[166],V4);
	T1= ecl_assql(ECL_SYM("POSITION",644),V4);
	T2= ecl_assql(VV[167],V4);
	T3= ecl_assql(VV[165],V4);
	T4= ecl_assql(VV[121],V4);
	T5= ecl_function_dispatch(cl_env_copy,VV[323])(2,V4,VV[124]) /*  LEGGI */;
	T6= ecl_car(T5);
	T7= ecl_function_dispatch(cl_env_copy,VV[346])(1,V1) /*  GET-ACTAVM-HEADCOREFLINE */;
	T8= ecl_list1(V2);
	T9= L37find_coreferent(T7,T8)             /*  FIND-COREFERENT */;
	T10= ecl_function_dispatch(cl_env_copy,VV[280])(1,T9) /*  GET-ACTAVM-HEADLEXMEAN */;
	T11= L38subst_sem_val(T6,T10)             /*  SUBST-SEM-VAL   */;
	T12= cl_list(2,VV[124],T11)               /*  LIST            */;
	T13= cl_list(6,T0,T1,T2,T3,T4,T12)        /*  LIST            */;
	T14= cl_list(2,VV[112],T13)               /*  LIST            */;
	value0=cl_list(2,T14,VV[185])             /*  LIST            */;
	return value0;
L10:;
	T0= ecl_function_dispatch(cl_env_copy,VV[345])(1,V1) /*  GET-ACTAVM-HEADCOREFTYPE */;
	if(!((CODE_CHAR(102))==(T0))){
	goto L18;}
	{cl_object V5;                            /*  COREFERENT      */
	T0= ecl_function_dispatch(cl_env_copy,VV[346])(1,V1) /*  GET-ACTAVM-HEADCOREFLINE */;
	T1= ecl_list1(V2);
	V5= L37find_coreferent(T0,T1)             /*  FIND-COREFERENT */;
	T0= ecl_assql(VV[166],V4);
	T1= ecl_assql(ECL_SYM("POSITION",644),V4);
	T2= ecl_assql(VV[167],V4);
	T3= ecl_assql(VV[165],V4);
	T4= ecl_assql(VV[121],V4);
	T5= ecl_function_dispatch(cl_env_copy,VV[323])(2,V4,VV[124]) /*  LEGGI */;
	T6= ecl_car(T5);
	T7= ecl_function_dispatch(cl_env_copy,VV[280])(1,V5) /*  GET-ACTAVM-HEADLEXMEAN */;
	T8= L38subst_sem_val(T6,T7)               /*  SUBST-SEM-VAL   */;
	T9= cl_list(2,VV[124],T8)                 /*  LIST            */;
	T10= cl_list(6,T0,T1,T2,T3,T4,T9)         /*  LIST            */;
	T11= cl_list(2,VV[112],T10)               /*  LIST            */;
	T12= ecl_function_dispatch(cl_env_copy,VV[299])(1,V5) /*  GET-ACTAVM-DEPENDENTS */;
	T13= cl_list(2,VV[113],T12)               /*  LIST            */;
	value0=cl_list(2,T11,T13)                 /*  LIST            */;
	return value0;
	}
L18:;
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[186]) /*  EXCEPTION */;
	return value0;
L6:;
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V1) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[141])==(T0))){
	goto L27;}
	T0= ecl_function_dispatch(cl_env_copy,VV[283])(1,V1) /*  GET-ACTAVM-HEADTYPE */;
	if(!((VV[187])==(T0))){
	goto L27;}
	{cl_object V6;                            /*  PRON-UP         */
	cl_object V7;                             /*  PRON-UP-UP      */
	cl_object V8;                             /*  RELAT-SEM       */
	T0= ecl_list1(V2);
	V6= ecl_function_dispatch(cl_env_copy,VV[347])(2,V1,T0) /*  FIND-ACTAVM-PARENT */;
	T0= ecl_list1(V2);
	V7= ecl_function_dispatch(cl_env_copy,VV[347])(2,V6,T0) /*  FIND-ACTAVM-PARENT */;
	V8= ECL_NIL;
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V6) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[30])==(T0))){
	goto L35;}
	V8= ecl_function_dispatch(cl_env_copy,VV[280])(1,V7) /*  GET-ACTAVM-HEADLEXMEAN */;
	goto L33;
L35:;
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V6) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[70])==(T0))){
	goto L39;}
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V7) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[30])==(T0))){
	goto L42;}
	T0= ecl_list1(V2);
	T1= ecl_function_dispatch(cl_env_copy,VV[347])(2,V7,T0) /*  FIND-ACTAVM-PARENT */;
	V8= ecl_function_dispatch(cl_env_copy,VV[280])(1,T1) /*  GET-ACTAVM-HEADLEXMEAN */;
	goto L33;
L42:;
	ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[188]) /*  EXCEPTION */;
	goto L33;
L39:;
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V6) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[76])==(T0))){
	goto L46;}
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V7) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[30])==(T0))){
	goto L49;}
	V8= ecl_function_dispatch(cl_env_copy,VV[280])(1,V7) /*  GET-ACTAVM-HEADLEXMEAN */;
	goto L33;
L49:;
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V7) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[70])==(T0))){
	goto L53;}
	{cl_object V9;                            /*  PRON-UP-UP-UP   */
	T0= ecl_list1(V2);
	V9= ecl_function_dispatch(cl_env_copy,VV[347])(2,V7,T0) /*  FIND-ACTAVM-PARENT */;
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V9) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[30])==(T0))){
	goto L57;}
	T0= ecl_list1(V2);
	T1= ecl_function_dispatch(cl_env_copy,VV[347])(2,V9,T0) /*  FIND-ACTAVM-PARENT */;
	V8= ecl_function_dispatch(cl_env_copy,VV[280])(1,T1) /*  GET-ACTAVM-HEADLEXMEAN */;
	goto L33;
L57:;
	ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[189]) /*  EXCEPTION */;
	goto L33;
	}
L53:;
	ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[190]) /*  EXCEPTION */;
	goto L33;
L46:;
	ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[191]) /*  EXCEPTION */;
	goto L33;
L33:;
	T0= ecl_assql(VV[166],V4);
	T1= ecl_assql(ECL_SYM("POSITION",644),V4);
	T2= ecl_assql(VV[167],V4);
	T3= ecl_assql(VV[165],V4);
	T4= ecl_assql(VV[121],V4);
	T5= ecl_function_dispatch(cl_env_copy,VV[323])(2,V4,VV[124]) /*  LEGGI */;
	T6= ecl_car(T5);
	T7= L38subst_sem_val(T6,V8)               /*  SUBST-SEM-VAL   */;
	T8= cl_list(2,VV[124],T7)                 /*  LIST            */;
	T9= cl_list(6,T0,T1,T2,T3,T4,T8)          /*  LIST            */;
	T10= cl_list(2,VV[112],T9)                /*  LIST            */;
	value0=cl_list(2,T10,VV[185])             /*  LIST            */;
	return value0;
	}
L27:;
	{cl_object V9;                            /*  TREEHEAD        */
	V9= ecl_function_dispatch(cl_env_copy,VV[298])(1,V1) /*  GET-ACTAVM-HEAD */;
	T0= cl_list(2,VV[112],V9)                 /*  LIST            */;
	{cl_object V10;                           /*  LET3404         */
	cl_object V11;
	cl_object V12;
	V10= ecl_function_dispatch(cl_env_copy,VV[348])(1,V1) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	V11= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V10))) FEtype_error_list(V10);
	V12= V10;
	{cl_object V13;
	cl_object V14;
	V13= ecl_list1(ECL_NIL);
	V14= V13;
L74:;
	if(!(ecl_endp(V12))){
	goto L78;}
	goto L75;
L78:;
	goto L76;
L76:;
	V11= _ecl_car(V12);
	{cl_object V15;
	V15= _ecl_cdr(V12);
	if (ecl_unlikely(!ECL_LISTP(V15))) FEtype_error_list(V15);
	V12= V15;
	}
	if (ecl_unlikely(ECL_ATOM(V14))) FEtype_error_cons(V14);
	T2= V14;
	T3= L36solve_coreferences(V11,V2,V9)      /*  SOLVE-COREFERENCES */;
	V14= ecl_list1(T3);
	(ECL_CONS_CDR(T2)=V14,T2);
	goto L74;
L75:;
	T1= ecl_cdr(V13);
	goto L66;
	}
	}
L66:;
	T2= cl_list(2,VV[113],T1)                 /*  LIST            */;
	value0=cl_list(2,T0,T2)                   /*  LIST            */;
	return value0;
	}
	}
}}
/*	function definition for FIND-COREFERENT                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L37find_coreferent(cl_object V1, cl_object V2)
{ VT38 VLEX38 CLSR38 STCK38
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V2==ECL_NIL)){
	goto L2;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L2:;
	T0= ecl_car(V2);
	if(!(ecl_equal(T0,VV[100]))){
	goto L5;}
	V2= ecl_cdr(V2);
	goto TTL;
L5:;
	T0= ecl_car(V2);
	T1= ecl_function_dispatch(cl_env_copy,VV[300])(1,T0) /*  GET-ACTAVM-HEADNUMB */;
	if(!(ecl_equal(T1,V1))){
	goto L11;}
	value0=ecl_car(V2); cl_env_copy->nvalues=1;
	return value0;
L11:;
	{cl_object V3;                            /*  FIRSTCOREF      */
	T0= ecl_car(V2);
	T1= ecl_function_dispatch(cl_env_copy,VV[348])(1,T0) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	V3= L37find_coreferent(V1,T1)             /*  FIND-COREFERENT */;
	if(!(V3==ECL_NIL)){
	goto L15;}
	V2= ecl_cdr(V2);
	goto TTL;
L15:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for SUBST-SEM-VAL                         */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L38subst_sem_val(cl_object V1, cl_object V2)
{ VT39 VLEX39 CLSR39 STCK39
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	T0= cl_list(2,VV[96],V2)                  /*  LIST            */;
	value0=ecl_list1(T0); cl_env_copy->nvalues=1;
	return value0;
L2:;
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	if(!((T1)==(VV[96]))){
	goto L5;}
	T0= cl_list(2,VV[96],V2)                  /*  LIST            */;
	T1= ecl_cdr(V1);
	value0=CONS(T0,T1); cl_env_copy->nvalues=1;
	return value0;
L5:;
	T0= ecl_car(V1);
	T1= ecl_cdr(V1);
	T2= L38subst_sem_val(T1,V2)               /*  SUBST-SEM-VAL   */;
	value0=CONS(T0,T2); cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for SUBST-SYN-VAL                         */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L39subst_syn_val(cl_object V1, cl_object V2, cl_object V3)
{ VT40 VLEX40 CLSR40 STCK40
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	T0= cl_list(2,V2,V3)                      /*  LIST            */;
	value0=ecl_list1(T0); cl_env_copy->nvalues=1;
	return value0;
L2:;
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	if(!((T1)==(V2))){
	goto L5;}
	T0= cl_list(2,V2,V3)                      /*  LIST            */;
	T1= ecl_cdr(V1);
	value0=CONS(T0,T1); cl_env_copy->nvalues=1;
	return value0;
L5:;
	T0= ecl_car(V1);
	T1= ecl_cdr(V1);
	T2= L39subst_syn_val(T1,V2,V3)            /*  SUBST-SYN-VAL   */;
	value0=CONS(T0,T2); cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for SUBST-HEAD-VAL                        */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L40subst_head_val(cl_object V1, cl_object V2, cl_object V3)
{ VT41 VLEX41 CLSR41 STCK41
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	T0= cl_list(2,V2,V3)                      /*  LIST            */;
	value0=ecl_list1(T0); cl_env_copy->nvalues=1;
	return value0;
L2:;
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	if(!((T1)==(V2))){
	goto L5;}
	T0= cl_list(2,V2,V3)                      /*  LIST            */;
	T1= ecl_cdr(V1);
	value0=CONS(T0,T1); cl_env_copy->nvalues=1;
	return value0;
L5:;
	T0= ecl_car(V1);
	T1= ecl_cdr(V1);
	T2= L40subst_head_val(T1,V2,V3)           /*  SUBST-HEAD-VAL  */;
	value0=CONS(T0,T2); cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for GET-LEFTMOST-ITEM                     */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L41get_leftmost_item(cl_object V1)
{ VT42 VLEX42 CLSR42 STCK42
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  DEPS            */
	V2= ecl_function_dispatch(cl_env_copy,VV[299])(1,V1) /*  GET-ACTAVM-DEPENDENTS */;
	T0= ecl_function_dispatch(cl_env_copy,VV[315])(1,V2) /*  ULT  */;
	if(!(ecl_equal(T0,VV[100]))){
	goto L3;}
	value0=V1; cl_env_copy->nvalues=1;
	return value0;
L3:;
	V1= ecl_function_dispatch(cl_env_copy,VV[315])(1,V2) /*  ULT  */;
	goto TTL;
	}
}}
/*	function definition for FIND-VERB-PERSON-OR-NUMBER            */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L42find_verb_person_or_number(cl_object V1, cl_object V2)
{ VT43 VLEX43 CLSR43 STCK43
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  FEATUREVAL      */
	if(!((V2)==(VV[31]))){
	goto L3;}
	V3= ecl_function_dispatch(cl_env_copy,VV[317])(1,V1) /*  GET-ACTAVM-HEADPERSON */;
	goto L1;
L3:;
	if(!((V2)==(ECL_SYM("NUMBER",606)))){
	goto L6;}
	V3= ecl_function_dispatch(cl_env_copy,VV[318])(1,V1) /*  GET-ACTAVM-HEADNUMBER */;
	goto L1;
L6:;
	V3= ECL_NIL;
	goto L1;
L1:;
	if(!(V3==ECL_NIL)){
	goto L9;}
	{cl_object V4;                            /*  HEADMOOD        */
	V4= ecl_function_dispatch(cl_env_copy,VV[284])(1,V1) /*  GET-ACTAVM-HEADMOOD */;
	if(!((V4)==(VV[65]))){
	goto L13;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L13:;
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,V4,VV[198]) /*  MEMQ */)==ECL_NIL){
	goto L16;}
	{cl_object V5;                            /*  AUXILIARY       */
	V5= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[162],V1) /*  FIND-ACTAVM-DEP */;
	if(!(V5==ECL_NIL)){
	goto L20;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L20:;
	{cl_object V6;                            /*  LET3405         */
	cl_object V7;                             /*  VSUBJ           */
	if(!((V2)==(VV[31]))){
	goto L24;}
	V6= ecl_function_dispatch(cl_env_copy,VV[317])(1,V5) /*  GET-ACTAVM-HEADPERSON */;
	goto L22;
L24:;
	if(!((V2)==(ECL_SYM("NUMBER",606)))){
	goto L27;}
	V6= ecl_function_dispatch(cl_env_copy,VV[318])(1,V5) /*  GET-ACTAVM-HEADNUMBER */;
	goto L22;
L27:;
	V6= ECL_NIL;
	goto L22;
L22:;
	V7= ECL_NIL;
	if(!(V6==ECL_NIL)){
	goto L31;}
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(3,VV[17],VV[199],V6) /*  EXCEPTION */;
	return value0;
L31:;
	if(!((V6)==(VV[144]))){
	goto L34;}
	V7= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[142],V1) /*  FIND-ACTAVM-DEP */;
	if(V7==ECL_NIL){
	goto L39;}
	if(!((V2)==(ECL_SYM("NUMBER",606)))){
	goto L42;}
	value0=ecl_function_dispatch(cl_env_copy,VV[318])(1,V7) /*  GET-ACTAVM-HEADNUMBER */;
	return value0;
L42:;
	if(!((V2)==(VV[31]))){
	goto L45;}
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V7) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[141])==(T0))){
	goto L48;}
	value0=ecl_function_dispatch(cl_env_copy,VV[317])(1,V7) /*  GET-ACTAVM-HEADPERSON */;
	return value0;
L48:;
	value0=ecl_make_fixnum(3); cl_env_copy->nvalues=1;
	return value0;
L45:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L39:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L34:;
	value0=V6; cl_env_copy->nvalues=1;
	return value0;
	}
	}
L16:;
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[200]) /*  EXCEPTION */;
	return value0;
	}
L9:;
	if(!((V3)==(VV[144]))){
	goto L51;}
	{cl_object V8;                            /*  VSUBJ           */
	V8= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[142],V1) /*  FIND-ACTAVM-DEP */;
	if(!(V8==ECL_NIL)){
	goto L55;}
	value0=ecl_function_dispatch(cl_env_copy,VV[272])(2,VV[17],VV[201]) /*  EXCEPTION */;
	return value0;
L55:;
	if(!((V2)==(ECL_SYM("NUMBER",606)))){
	goto L58;}
	value0=ecl_function_dispatch(cl_env_copy,VV[318])(1,V8) /*  GET-ACTAVM-HEADNUMBER */;
	return value0;
L58:;
	if(!((V2)==(VV[31]))){
	goto L61;}
	T0= ecl_function_dispatch(cl_env_copy,VV[279])(1,V8) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[141])==(T0))){
	goto L64;}
	value0=ecl_function_dispatch(cl_env_copy,VV[317])(1,V8) /*  GET-ACTAVM-HEADPERSON */;
	return value0;
L64:;
	value0=ecl_make_fixnum(3); cl_env_copy->nvalues=1;
	return value0;
L61:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
	}
L51:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for SKIP-DETERMINER                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L43skip_determiner(cl_narg narg, cl_object V1, ...)
{ VT44 VLEX44 CLSR44 STCK44
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	if (ecl_unlikely(narg<1)) FEwrong_num_arguments_anonym();
	if (ecl_unlikely(narg>2)) FEwrong_num_arguments_anonym();
	{
	cl_object V2;
	va_list args; va_start(args,V1);
	{int i=1;
	if (i >= narg) {
	V2= ECL_NIL;
	} else {
	i++;
	V2= va_arg(args,cl_object);
	}}
	va_end(args);
	if((V2)==ECL_NIL){
	goto L3;}
	{cl_object V3;
	cl_object V4;
	V3= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V1))) FEtype_error_list(V1);
	V4= V1;
	{cl_object V5;
	cl_object V6;
	V5= ecl_list1(ECL_NIL);
	V6= V5;
L11:;
	if(!(ecl_endp(V4))){
	goto L15;}
	goto L12;
L15:;
	goto L13;
L13:;
	V3= _ecl_car(V4);
	{cl_object V7;
	V7= _ecl_cdr(V4);
	if (ecl_unlikely(!ECL_LISTP(V7))) FEtype_error_list(V7);
	V4= V7;
	}
	if (ecl_unlikely(ECL_ATOM(V6))) FEtype_error_cons(V6);
	T0= V6;
	T1= L44single_skip_determiner(V3)         /*  SINGLE-SKIP-DETERMINER */;
	V6= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V6,T0);
	goto L11;
L12:;
	value0=ecl_cdr(V5); cl_env_copy->nvalues=1;
	return value0;
	}
	}
L3:;
	value0=L44single_skip_determiner(V1)      /*  SINGLE-SKIP-DETERMINER */;
	return value0;
}}
/*	function definition for SINGLE-SKIP-DETERMINER                */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L44single_skip_determiner(cl_object V1)
{ VT45 VLEX45 CLSR45 STCK45
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  DETERM-HEAD     */
	V2= L7is_a_actavm_noun_complex(V1)        /*  IS-A-ACTAVM-NOUN-COMPLEX */;
	if(V2==ECL_NIL){
	goto L3;}
	value0=V2; cl_env_copy->nvalues=1;
	return value0;
L3:;
	value0=V1; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for SKIP-QUESTION-TENSE-MARKER            */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L45skip_question_tense_marker(cl_object V1)
{ VT46 VLEX46 CLSR46 STCK46
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  HEADMEAN        */
	V2= ecl_function_dispatch(cl_env_copy,VV[280])(1,V1) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((V2)==(VV[205]))){
	goto L3;}
	T0= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[38],V1) /*  FIND-ACTAVM-DEP */;
	cl_env_copy->nvalues=2;
	cl_env_copy->values[1]=VV[206];
	cl_env_copy->values[0]=T0;
	return cl_env_copy->values[0];
L3:;
	if(!((V2)==(VV[207]))){
	goto L6;}
	T0= ecl_function_dispatch(cl_env_copy,VV[282])(2,VV[38],V1) /*  FIND-ACTAVM-DEP */;
	cl_env_copy->nvalues=2;
	cl_env_copy->values[1]=VV[208];
	cl_env_copy->values[0]=T0;
	return cl_env_copy->values[0];
L6:;
	value0=V1; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for IS-SEM-INTERROGATIVE                  */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L46is_sem_interrogative(cl_object V1)
{ VT47 VLEX47 CLSR47 STCK47
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  TREE-DEPS       */
	V2= ecl_function_dispatch(cl_env_copy,VV[299])(1,V1) /*  GET-ACTAVM-DEPENDENTS */;
	T0= L41get_leftmost_item(V1)              /*  GET-LEFTMOST-ITEM */;
	T1= ecl_function_dispatch(cl_env_copy,VV[313])(1,T0) /*  GET-ACTAVM-HEADLEMMA */;
	value0=ecl_make_bool((T1)==(CODE_CHAR(63)));
	if((value0)!=ECL_NIL){
	goto L3;}
	T0= ecl_function_dispatch(cl_env_copy,VV[280])(1,V1) /*  GET-ACTAVM-HEADLEXMEAN */;
	value0=ecl_make_bool((VV[205])==(T0));
	if((value0)!=ECL_NIL){
	goto L3;}
	value0=ecl_function_dispatch(cl_env_copy,VV[359])(1,V2) /*  FIND-INTERR-ADV */;
	if((value0)!=ECL_NIL){
	goto L3;}
	value0=ecl_function_dispatch(cl_env_copy,VV[360])(1,V2) /*  FIND-INTERR-PRON */;
	if((value0)!=ECL_NIL){
	goto L3;}
	value0=ecl_function_dispatch(cl_env_copy,VV[361])(1,V2) /*  FIND-INTERR-ADJEC */;
	if((value0)!=ECL_NIL){
	goto L3;}
	if(!((ecl_symbol_value(VV[138]))==(VV[156]))){
	goto L10;}
	value0=L47there_inversion(V2)             /*  THERE-INVERSION */;
	return value0;
L10:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L3:;
	cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for THERE-INVERSION                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L47there_inversion(cl_object V1)
{ VT48 VLEX48 CLSR48 STCK48
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  FOUND-HEAD      */
	cl_object V3;                             /*  RESULT          */
	V2= ECL_NIL;
	V3= ECL_NIL;
	{cl_object V4;
	V4= V1;
	goto L7;
L6:;
	{cl_object V5;                            /*  DEP             */
	V5= ecl_car(V4);
	if(!(ecl_equal(V5,VV[100]))){
	goto L13;}
	V2= ECL_T;
	goto L11;
L13:;
	if((V2)==ECL_NIL){
	goto L17;}
	T0= ecl_function_dispatch(cl_env_copy,VV[302])(1,V5) /*  GET-ACTAVM-HEADLINK */;
	if(!((T0)==(VV[211]))){
	goto L17;}
	V3= ECL_T;
	goto L11;
L17:;
	goto L11;
L11:;
	}
	V4= ecl_cdr(V4);
L7:;
	if((V4)==ECL_NIL){
	goto L25;}
	goto L6;
L25:;
	goto L23;
L23:;
	goto L4;
L4:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for READ-ACTAVM-SENT                      */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L48read_actavm_sent(cl_object V1, cl_object V2, cl_object V3)
{ VT49 VLEX49 CLSR49 STCK49
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	T0= L49read_t_s_int(V1,V2,V3)             /*  READ-T-S-INT    */;
	T1= ecl_function_dispatch(cl_env_copy,VV[328])(1,T0) /*  EXPLODE */;
	T2= ecl_cdr(T1);
	T3= ecl_function_dispatch(cl_env_copy,VV[330])(1,T2) /*  IMPLODE */;
	value0=cl_string(T3)                      /*  STRING          */;
	return value0;
}}
/*	function definition for READ-T-S-INT                          */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L49read_t_s_int(cl_object V1, cl_object V2, cl_object V3)
{ VT50 VLEX50 CLSR50 STCK50
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V4;                            /*  LET3408         */
	cl_object V5;                             /*  LET3409         */
	cl_object V6;                             /*  LET3410         */
	cl_object V7;                             /*  SENTENCE        */
	V4= ecl_function_dispatch(cl_env_copy,VV[299])(1,V1) /*  GET-ACTAVM-DEPENDENTS */;
	V5= ecl_function_dispatch(cl_env_copy,VV[320])(1,V1) /*  GET-ACTAVM-HEADFORM */;
	T0= ecl_function_dispatch(cl_env_copy,VV[300])(1,V1) /*  GET-ACTAVM-HEADNUMB */;
	{bool V8;
	V8= ecl_numberp(T0);
	V6= (V8)?ECL_NIL:ECL_T;}
	V7= VV[15];
	if((V3)==ECL_NIL){
	goto L6;}
	if(!(ecl_number_equalp(V2,ecl_make_fixnum(1)))){
	goto L6;}
	if(!((V5)==(VV[214]))){
	goto L6;}
	value0=VV[15]; cl_env_copy->nvalues=1;
	return value0;
L6:;
	{cl_object V8;
	V8= V4;
	goto L14;
L13:;
	{cl_object V9;                            /*  NXTDEP          */
	V9= ecl_car(V8);
	if(!(ecl_equal(V9,VV[100]))){
	goto L20;}
	if((V6)!=ECL_NIL){
	goto L23;}
	T0= L50get_printable_tree_form(V5)        /*  GET-PRINTABLE-TREE-FORM */;
	V7= cl_concatenate(4,ECL_SYM("STRING",805),V7,VV[215],T0) /*  CONCATENATE */;
	goto L18;
L23:;
	goto L18;
L20:;
	T0= ecl_one_plus(V2);
	T1= L49read_t_s_int(V9,T0,V3)             /*  READ-T-S-INT    */;
	V7= cl_concatenate(3,ECL_SYM("STRING",805),V7,T1) /*  CONCATENATE */;
	goto L18;
L18:;
	}
	V8= ecl_cdr(V8);
L14:;
	if((V8)==ECL_NIL){
	goto L31;}
	goto L13;
L31:;
	goto L29;
L29:;
	goto L11;
L11:;
	value0=V7; cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for GET-PRINTABLE-TREE-FORM               */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L50get_printable_tree_form(cl_object V1)
{ VT51 VLEX51 CLSR51 STCK51
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!((V1)==(CODE_CHAR(116)))){
	goto L2;}
	value0=VV[15]; cl_env_copy->nvalues=1;
	return value0;
L2:;
	T0= ecl_symbol_name(V1);
	value0=si_coerce_to_vector(T0,ECL_SYM("CHARACTER",222),ECL_SYM("*",18),ECL_NIL) /*  COERCE-TO-VECTOR */;
	return value0;
}}
/*	function definition for SIMPLIFY-ONTO-REPR                    */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L51simplify_onto_repr(cl_object V1)
{ VT52 VLEX52 CLSR52 STCK52
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  INPFORM         */
	cl_object V3;                             /*  SIMPLRESULT     */
	V2= ECL_NIL;
	V3= ECL_NIL;
	T0= ecl_car(V1);
	if(!((T0)==(VV[218]))){
	goto L5;}
	T0= ecl_cddddr(V1);
	V2= ecl_cadr(T0);
	T0= ecl_function_dispatch(cl_env_copy,VV[367])(2,ecl_make_fixnum(5),V1) /*  FIRST-N */;
	T1= L53act_simplify_onto(V2)              /*  ACT-SIMPLIFY-ONTO */;
	V3= ecl_function_dispatch(cl_env_copy,VV[274])(2,T0,T1) /*  APPEND1 */;
	goto L3;
L5:;
	T0= ecl_car(V1);
	if(!((T0)==(VV[219]))){
	goto L11;}
	V2= ecl_cadddr(V1);
	T0= ecl_function_dispatch(cl_env_copy,VV[367])(2,ecl_make_fixnum(3),V1) /*  FIRST-N */;
	T1= L53act_simplify_onto(V2)              /*  ACT-SIMPLIFY-ONTO */;
	V3= ecl_function_dispatch(cl_env_copy,VV[274])(2,T0,T1) /*  APPEND1 */;
	goto L3;
L11:;
	(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[220]) /*  BREAK */;
	goto L3;
L3:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for IS-SIMPLE-ONTO-ITEM                   */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L52is_simple_onto_item(cl_object V1)
{ VT53 VLEX53 CLSR53 STCK53
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	value0=ecl_make_bool(ECL_ATOM(V1));
	if((value0)!=ECL_NIL){
	goto L2;}
	T0= ecl_car(V1);
	value0=ecl_make_bool((VV[222])==(T0)); cl_env_copy->nvalues=1;
	return value0;
L2:;
	cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for ACT-SIMPLIFY-ONTO                     */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L53act_simplify_onto(cl_object V1)
{ VT54 VLEX54 CLSR54 STCK54
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  FLATTENED       */
	cl_object V3;                             /*  NOUPPER         */
	cl_object V4;                             /*  FINRES          */
	V2= ECL_NIL;
	V3= ECL_NIL;
	V4= ECL_NIL;
	V2= L64flatten_all_ands(V1,ECL_NIL)       /*  FLATTEN-ALL-ANDS */;
	{cl_object V5;
	cl_object V6;
	V5= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V2))) FEtype_error_list(V2);
	V6= V2;
	{cl_object V7;
	cl_object V8;
	V7= ecl_list1(ECL_NIL);
	V8= V7;
L14:;
	if(!(ecl_endp(V6))){
	goto L18;}
	goto L15;
L18:;
	goto L16;
L16:;
	V5= _ecl_car(V6);
	{cl_object V9;
	V9= _ecl_cdr(V6);
	if (ecl_unlikely(!ECL_LISTP(V9))) FEtype_error_list(V9);
	V6= V9;
	}
	if (ecl_unlikely(ECL_ATOM(V8))) FEtype_error_cons(V8);
	T0= V8;
	T1= L61remove_upper_classes(V5)           /*  REMOVE-UPPER-CLASSES */;
	V8= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V8,T0);
	goto L14;
L15:;
	V3= ecl_cdr(V7);
	goto L7;
	}
	}
L7:;
	V4= L54compose_and_repr(V3)               /*  COMPOSE-AND-REPR */;
	value0=V4; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for COMPOSE-AND-REPR                      */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L54compose_and_repr(cl_object V1)
{ VT55 VLEX55 CLSR55 STCK55
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  FIRSTP          */
	V2= ECL_NIL;
	if((ecl_function_dispatch(cl_env_copy,VV[371])(1,V1) /*  ALL-NULL */)==ECL_NIL){
	goto L3;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L3:;
	{cl_fixnum V3;
	V3= ecl_length(V1);
	if(!((1)==(V3))){
	goto L6;}}
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	T2= ecl_function_dispatch(cl_env_copy,VV[372])(1,T1) /*  REMOVE-SYNT-POINTER */;
	if(!(ECL_LISTP(T2))){
	goto L6;}
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	T2= ecl_car(T1);
	if((ecl_function_dispatch(cl_env_copy,VV[303])(2,ECL_SYM("EQ",333),T2) /*  NEQ */)==ECL_NIL){
	goto L6;}
	V2= ecl_car(V1);
	T0= ecl_car(V2);
	T1= ecl_car(T0);
	if(!((VV[225])==(T1))){
	goto L13;}
	{cl_object V3;                            /*  LET3415         */
	cl_object V4;
	cl_object V5;
	T1= ecl_car(V2);
	V3= ecl_cadr(T1);
	V4= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V3))) FEtype_error_list(V3);
	V5= V3;
	{cl_object V6;
	cl_object V7;
	V6= ecl_list1(ECL_NIL);
	V7= V6;
L23:;
	if(!(ecl_endp(V5))){
	goto L27;}
	goto L24;
L27:;
	goto L25;
L25:;
	V4= _ecl_car(V5);
	{cl_object V8;
	V8= _ecl_cdr(V5);
	if (ecl_unlikely(!ECL_LISTP(V8))) FEtype_error_list(V8);
	V5= V8;
	}
	if (ecl_unlikely(ECL_ATOM(V7))) FEtype_error_cons(V7);
	T1= V7;
	T2= L54compose_and_repr(V4)               /*  COMPOSE-AND-REPR */;
	V7= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V7,T1);
	goto L23;
L24:;
	T0= ecl_cdr(V6);
	goto L15;
	}
	}
L15:;
	T1= cl_list(2,VV[225],T0)                 /*  LIST            */;
	value0=ecl_list1(T1); cl_env_copy->nvalues=1;
	return value0;
L13:;
	value0=(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[226]) /*  BREAK */;
	return value0;
L6:;
	{cl_object V3;                            /*  ALLSTARTS       */
	cl_object V4;                             /*  GROUPED         */
	cl_object V5;                             /*  STARTS          */
	cl_object V6;                             /*  EQUIVALENCES    */
	{cl_object V7;
	cl_object V8;
	V7= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V1))) FEtype_error_list(V1);
	V8= V1;
	{cl_object V9;
	cl_object V10;
	V9= ecl_list1(ECL_NIL);
	V10= V9;
L48:;
	if(!(ecl_endp(V8))){
	goto L52;}
	goto L49;
L52:;
	goto L50;
L50:;
	V7= _ecl_car(V8);
	{cl_object V11;
	V11= _ecl_cdr(V8);
	if (ecl_unlikely(!ECL_LISTP(V11))) FEtype_error_list(V11);
	V8= V11;
	}
	if (ecl_unlikely(ECL_ATOM(V10))) FEtype_error_cons(V10);
	T0= V10;
	T1= ecl_car(V7);
	V10= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V10,T0);
	goto L48;
L49:;
	V3= ecl_cdr(V9);
	goto L41;
	}
	}
L41:;
	V4= ECL_NIL;
	V5= ECL_NIL;
	V6= ECL_NIL;
	{cl_object V7;
	cl_object V8;
	V7= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V3))) FEtype_error_list(V3);
	V8= V3;
	{cl_object V9;
	cl_object V10;
	V9= ecl_list1(ECL_NIL);
	V10= V9;
L78:;
	if(!(ecl_endp(V8))){
	goto L82;}
	goto L79;
L82:;
	goto L80;
L80:;
	V7= _ecl_car(V8);
	{cl_object V11;
	V11= _ecl_cdr(V8);
	if (ecl_unlikely(!ECL_LISTP(V11))) FEtype_error_list(V11);
	V8= V11;
	}
	if (ecl_unlikely(ECL_ATOM(V10))) FEtype_error_cons(V10);
	T1= V10;
	T2= ecl_function_dispatch(cl_env_copy,VV[372])(1,V7) /*  REMOVE-SYNT-POINTER */;
	V10= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V10,T1);
	goto L78;
L79:;
	T0= ecl_cdr(V9);
	goto L71;
	}
	}
L71:;
	cl_env_copy->values[0]=L55merge_all_starts(T0) /*  MERGE-ALL-STARTS */;
	{int V7=cl_env_copy->nvalues-0;
	if (V7--<=0) goto L96;
	V5= cl_env_copy->values[0];
	if (V7--<=0) goto L97;
	V6= cl_env_copy->values[1];
	goto L98;}
L96:;
	V5= ECL_NIL;
L97:;
	V6= ECL_NIL;
L98:;
	{cl_fixnum V7;
	V7= ecl_length(V5);
	if(!((V7)==(1))){
	goto L100;}}
	T0= L57find_best_start(V3,V6)             /*  FIND-BEST-START */;
	{cl_object V7;
	cl_object V8;
	V7= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V1))) FEtype_error_list(V1);
	V8= V1;
	{cl_object V9;
	cl_object V10;
	V9= ecl_list1(ECL_NIL);
	V10= V9;
L109:;
	if(!(ecl_endp(V8))){
	goto L113;}
	goto L110;
L113:;
	goto L111;
L111:;
	V7= _ecl_car(V8);
	{cl_object V11;
	V11= _ecl_cdr(V8);
	if (ecl_unlikely(!ECL_LISTP(V11))) FEtype_error_list(V11);
	V8= V11;
	}
	if (ecl_unlikely(ECL_ATOM(V10))) FEtype_error_cons(V10);
	T2= V10;
	T3= ecl_cdr(V7);
	V10= ecl_list1(T3);
	(ECL_CONS_CDR(T2)=V10,T2);
	goto L109;
L110:;
	T1= ecl_cdr(V9);
	goto L102;
	}
	}
L102:;
	T2= L54compose_and_repr(T1)               /*  COMPOSE-AND-REPR */;
	value0=CONS(T0,T2); cl_env_copy->nvalues=1;
	return value0;
L100:;
	T0= L59group_same_start(V5,V1,V6)         /*  GROUP-SAME-START */;
	V4= ecl_function_dispatch(cl_env_copy,VV[373])(1,T0) /*  DROPNIL2 */;
	{cl_fixnum V7;
	V7= ecl_length(V4);
	if(!((1)==(V7))){
	goto L130;}}
	V1= ecl_car(V4);
	goto TTL;
L130:;
	{cl_object V7;
	cl_object V8;
	V7= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V4))) FEtype_error_list(V4);
	V8= V4;
	{cl_object V9;
	cl_object V10;
	V9= ecl_list1(ECL_NIL);
	V10= V9;
L141:;
	if(!(ecl_endp(V8))){
	goto L145;}
	goto L142;
L145:;
	goto L143;
L143:;
	V7= _ecl_car(V8);
	{cl_object V11;
	V11= _ecl_cdr(V8);
	if (ecl_unlikely(!ECL_LISTP(V11))) FEtype_error_list(V11);
	V8= V11;
	}
	if (ecl_unlikely(ECL_ATOM(V10))) FEtype_error_cons(V10);
	T1= V10;
	T2= L54compose_and_repr(V7)               /*  COMPOSE-AND-REPR */;
	V10= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V10,T1);
	goto L141;
L142:;
	T0= ecl_cdr(V9);
	goto L134;
	}
	}
L134:;
	T1= cl_list(2,ECL_SYM("AND",87),T0)       /*  LIST            */;
	value0=ecl_list1(T1); cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for MERGE-ALL-STARTS                      */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L55merge_all_starts(cl_object V1)
{ VT56 VLEX56 CLSR56 STCK56
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  RESULT          */
	cl_object V3;                             /*  EQUIV           */
	cl_object V4;                             /*  EQUIVS          */
	V2= ECL_NIL;
	V3= ECL_NIL;
	V4= ECL_NIL;
	{cl_object V5;                            /*  LET3424         */
	cl_object V6;                             /*  LET3425         */
	cl_object V7;                             /*  NXTCONC         */
	cl_object V8;                             /*  CONCEPTS        */
	V5= ecl_car(V1);
	V6= ecl_cdr(V1);
	V7= V5;
	V8= V6;
	goto L11;
L10:;
	cl_env_copy->values[0]=L56insert_no_dupl_with_subsumption(V7,V2) /*  INSERT-NO-DUPL-WITH-SUBSUMPTION */;
	{int V9=cl_env_copy->nvalues-0;
	if (V9--<=0) goto L15;
	V2= cl_env_copy->values[0];
	if (V9--<=0) goto L16;
	V3= cl_env_copy->values[1];
	goto L17;}
L15:;
	V2= ECL_NIL;
L16:;
	V3= ECL_NIL;
L17:;
	if(V3==ECL_NIL){
	goto L23;}
	if((ecl_member(V3,V4))==ECL_NIL){
	goto L21;}
	goto L22;
L23:;
L22:;
	goto L19;
L21:;
	V4= CONS(V3,V4);
	goto L19;
L19:;
	V7= ecl_car(V8);
	V8= ecl_cdr(V8);
L11:;
	if(V7==ECL_NIL){
	goto L30;}
	goto L10;
L30:;
	goto L28;
L28:;
	goto L8;
L8:;
	T0= cl_reverse(V2)                        /*  REVERSE         */;
	cl_env_copy->nvalues=2;
	cl_env_copy->values[1]=V4;
	cl_env_copy->values[0]=T0;
	return cl_env_copy->values[0];
	}
	}
}}
/*	function definition for INSERT-NO-DUPL-WITH-SUBSUMPTION       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L56insert_no_dupl_with_subsumption(cl_object V1, cl_object V2)
{ VT57 VLEX57 CLSR57 STCK57
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  REPL            */
	cl_object V4;                             /*  RES             */
	cl_object V5;                             /*  FOUND           */
	V3= ECL_NIL;
	V4= ECL_NIL;
	V5= ECL_NIL;
	{cl_object V6;                            /*  LET3426         */
	cl_object V7;                             /*  LET3427         */
	cl_object V8;                             /*  NXTITEM         */
	cl_object V9;                             /*  LIS             */
	V6= ecl_car(V2);
	V7= ecl_cdr(V2);
	V8= V6;
	V9= V7;
	goto L11;
L10:;
	if(!((V8)==(V1))){
	goto L15;}
	V5= ECL_T;
	V4= CONS(V8,V4);
	goto L13;
L15:;
	if((ecl_function_dispatch(cl_env_copy,VV[376])(2,V8,V1) /*  IS-SUBCLASS-OF */)==ECL_NIL){
	goto L21;}
	V5= ECL_T;
	V3= cl_list(2,V8,V1)                      /*  LIST            */;
	V4= CONS(V8,V4);
	goto L13;
L21:;
	if((ecl_function_dispatch(cl_env_copy,VV[376])(2,V1,V8) /*  IS-SUBCLASS-OF */)==ECL_NIL){
	goto L29;}
	V5= ECL_T;
	V3= cl_list(2,V1,V8)                      /*  LIST            */;
	V4= CONS(V1,V4);
	goto L13;
L29:;
	V4= CONS(V8,V4);
	goto L13;
L13:;
	V8= ecl_car(V9);
	V9= ecl_cdr(V9);
L11:;
	if(V8==ECL_NIL){
	goto L42;}
	if((V5)!=ECL_NIL){
	goto L42;}
	goto L10;
L42:;
	goto L40;
L40:;
	goto L8;
L8:;
	if((V5)==ECL_NIL){
	goto L46;}
	T0= ecl_list1(V8);
	T1= cl_append(3,V4,T0,V9)                 /*  APPEND          */;
	T2= ecl_function_dispatch(cl_env_copy,VV[377])(1,T1) /*  DROPNIL */;
	cl_env_copy->nvalues=2;
	cl_env_copy->values[1]=V3;
	cl_env_copy->values[0]=T2;
	return cl_env_copy->values[0];
L46:;
	T0= ecl_list1(V8);
	T1= cl_append(3,V4,T0,V9)                 /*  APPEND          */;
	T2= ecl_function_dispatch(cl_env_copy,VV[377])(1,T1) /*  DROPNIL */;
	T3= CONS(V1,T2);
	cl_env_copy->nvalues=2;
	cl_env_copy->values[1]=ECL_NIL;
	cl_env_copy->values[0]=T3;
	return cl_env_copy->values[0];
	}
	}
}}
/*	function definition for FIND-BEST-START                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L57find_best_start(cl_object V1, cl_object V2)
{ VT58 VLEX58 CLSR58 STCK58
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  BEGSTART        */
	V3= ecl_car(V1);
	{cl_object V4;                            /*  LET3428         */
	cl_object V5;                             /*  LET3429         */
	cl_object V6;                             /*  NXTSTART        */
	cl_object V7;                             /*  STARTS          */
	V4= ecl_car(V1);
	V5= ecl_cdr(V1);
	V6= V4;
	V7= V5;
	goto L9;
L8:;
	V6= ecl_car(V7);
	V7= ecl_cdr(V7);
L9:;
	if(V6==ECL_NIL){
	goto L16;}
	if(!(ECL_LISTP(V6))){
	goto L20;}
	T0= ecl_car(V6);
	if((VV[222])==(T0)){
	goto L16;}
	goto L17;
L20:;
	goto L17;
L17:;
	goto L8;
L16:;
	goto L14;
L14:;
	goto L6;
L6:;
	if(!(V6==ECL_NIL)){
	goto L23;}
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
L23:;
	value0=L58find_start_equiv(V6,V2)         /*  FIND-START-EQUIV */;
	return value0;
	}
	}
}}
/*	function definition for FIND-START-EQUIV                      */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L58find_start_equiv(cl_object V1, cl_object V2)
{ VT59 VLEX59 CLSR59 STCK59
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  LET3430         */
	cl_object V4;                             /*  LET3431         */
	cl_object V5;                             /*  NXTEQUIV        */
	cl_object V6;                             /*  EQUIVS          */
	V3= ecl_car(V2);
	V4= ecl_cdr(V2);
	V5= V3;
	V6= V4;
	goto L8;
L7:;
	T0= ecl_caddr(V1);
	T1= ecl_cadr(V5);
	if(!((T0)==(T1))){
	goto L12;}
	T0= ecl_cadr(V1);
	T1= ecl_car(V5);
	V1= cl_list(3,VV[222],T0,T1)              /*  LIST            */;
	goto L10;
L12:;
	goto L10;
L10:;
	V5= ecl_car(V6);
	V6= ecl_car(V6);
L8:;
	if(V5==ECL_NIL){
	goto L20;}
	goto L7;
L20:;
	goto L18;
L18:;
	goto L5;
L5:;
	value0=V1; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for GROUP-SAME-START                      */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L59group_same_start(cl_object V1, cl_object V2, cl_object V3)
{ VT60 VLEX60 CLSR60 STCK60
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L2:;
	{cl_object V4;                            /*  SIMPLSTART      */
	T0= ecl_car(V1);
	V4= ecl_function_dispatch(cl_env_copy,VV[372])(1,T0) /*  REMOVE-SYNT-POINTER */;
	{cl_object V5;
	cl_object V6;
	V5= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V2))) FEtype_error_list(V2);
	V6= V2;
	{cl_object V7;
	cl_object V8;
	V7= ecl_list1(ECL_NIL);
	V8= V7;
L12:;
	if(!(ecl_endp(V6))){
	goto L16;}
	goto L13;
L16:;
	goto L14;
L14:;
	V5= _ecl_car(V6);
	{cl_object V9;
	V9= _ecl_cdr(V6);
	if (ecl_unlikely(!ECL_LISTP(V9))) FEtype_error_list(V9);
	V6= V9;
	}
	if (ecl_unlikely(ECL_ATOM(V8))) FEtype_error_cons(V8);
	T1= V8;
	{cl_object V9;                            /*  SIMPLX          */
	T3= ecl_car(V5);
	V9= ecl_function_dispatch(cl_env_copy,VV[372])(1,T3) /*  REMOVE-SYNT-POINTER */;
	if((L60equiv_equal(V4,V9,V3)              /*  EQUIV-EQUAL     */)==ECL_NIL){
	goto L31;}
	T3= ecl_car(V1);
	T4= ecl_car(V5);
	T5= ecl_function_dispatch(cl_env_copy,VV[381])(2,T3,T4) /*  BEST-SYNT-CONC */;
	T6= ecl_cdr(V5);
	T2= CONS(T5,T6);
	goto L28;
L31:;
	T2= ECL_NIL;
	goto L28;
	}
L28:;
	V8= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V8,T1);
	goto L12;
L13:;
	T0= ecl_cdr(V7);
	goto L5;
	}
	}
L5:;
	T1= ecl_function_dispatch(cl_env_copy,VV[373])(1,T0) /*  DROPNIL2 */;
	T2= ecl_cdr(V1);
	T3= L59group_same_start(T2,V2,V3)         /*  GROUP-SAME-START */;
	value0=CONS(T1,T3); cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for EQUIV-EQUAL                           */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L60equiv_equal(cl_object V1, cl_object V2, cl_object V3)
{ VT61 VLEX61 CLSR61 STCK61
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	value0=ecl_make_bool((V1)==(V2));
	if((value0)!=ECL_NIL){
	goto L2;}
	T0= cl_list(2,V1,V2)                      /*  LIST            */;
	value0=ecl_member(T0,V3);
	if((value0)!=ECL_NIL){
	goto L2;}
	T0= cl_list(2,V2,V1)                      /*  LIST            */;
	value0=ecl_member(T0,V3); cl_env_copy->nvalues=1;
	return value0;
L2:;
	cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for REMOVE-UPPER-CLASSES                  */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L61remove_upper_classes(cl_object V1)
{ VT62 VLEX62 CLSR62 STCK62
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L2:;
	{cl_object V2;                            /*  LET3434         */
	cl_object V3;                             /*  LET3435         */
	cl_object V4;                             /*  LET3436         */
	V2= ecl_car(V1);
	V3= ecl_cadr(V1);
	V4= ecl_cadddr(V1);
	if((L52is_simple_onto_item(V2)            /*  IS-SIMPLE-ONTO-ITEM */)!=ECL_NIL){
	goto L10;}
	T0= ecl_car(V2);
	if(!((ECL_SYM("EQ",333))==(T0))){
	goto L8;}
	goto L9;
L10:;
L9:;
	if(!(V3==ECL_NIL)){
	goto L13;}
	value0=ecl_list1(V2); cl_env_copy->nvalues=1;
	return value0;
L13:;
	if((ecl_function_dispatch(cl_env_copy,VV[384])(1,V2) /*  IS-A-STRUCTURAL-ITEM */)==ECL_NIL){
	goto L16;}
	value0=(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[234]) /*  BREAK */;
	return value0;
L16:;
	if(!((V3)==(VV[235]))){
	goto L19;}
	if(!((V4)==(VV[236]))){
	goto L22;}
	T0= ecl_cddddr(V1);
	T1= ecl_car(T0);
	if((ecl_function_dispatch(cl_env_copy,VV[303])(2,V2,T1) /*  NEQ */)==ECL_NIL){
	goto L22;}
	T0= ecl_function_dispatch(cl_env_copy,VV[367])(2,ecl_make_fixnum(4),V1) /*  FIRST-N */;
	T1= ecl_cddddr(V1);
	T2= L61remove_upper_classes(T1)           /*  REMOVE-UPPER-CLASSES */;
	value0=ecl_append(T0,T2); cl_env_copy->nvalues=1;
	return value0;
L22:;
	T0= ecl_cdr(V1);
	T1= ecl_cdr(T0);
	T2= ecl_cdr(T1);
	V1= CONS(V2,T2);
	goto TTL;
L19:;
	if(!((V3)==(VV[236]))){
	goto L28;}
	T0= ecl_cdr(V1);
	V1= ecl_cdr(T0);
	goto TTL;
L28:;
	T0= ecl_cdr(V1);
	T1= ecl_cdr(T0);
	T2= L61remove_upper_classes(T1)           /*  REMOVE-UPPER-CLASSES */;
	value0=cl_listX(3,V2,V3,T2)               /*  LIST*           */;
	return value0;
L8:;
	T0= ecl_car(V2);
	if(!((VV[225])==(T0))){
	goto L33;}
	{cl_object V5;                            /*  LET3438         */
	cl_object V6;
	cl_object V7;
	V5= ecl_cadr(V2);
	V6= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V5))) FEtype_error_list(V5);
	V7= V5;
	{cl_object V8;
	cl_object V9;
	V8= ecl_list1(ECL_NIL);
	V9= V8;
L43:;
	if(!(ecl_endp(V7))){
	goto L47;}
	goto L44;
L47:;
	goto L45;
L45:;
	V6= _ecl_car(V7);
	{cl_object V10;
	V10= _ecl_cdr(V7);
	if (ecl_unlikely(!ECL_LISTP(V10))) FEtype_error_list(V10);
	V7= V10;
	}
	if (ecl_unlikely(ECL_ATOM(V9))) FEtype_error_cons(V9);
	T1= V9;
	{cl_object V10;
	cl_object V11;
	V10= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V6))) FEtype_error_list(V6);
	V11= V6;
	{cl_object V12;
	cl_object V13;
	V12= ecl_list1(ECL_NIL);
	V13= V12;
L66:;
	if(!(ecl_endp(V11))){
	goto L70;}
	goto L67;
L70:;
	goto L68;
L68:;
	V10= _ecl_car(V11);
	{cl_object V14;
	V14= _ecl_cdr(V11);
	if (ecl_unlikely(!ECL_LISTP(V14))) FEtype_error_list(V14);
	V11= V14;
	}
	if (ecl_unlikely(ECL_ATOM(V13))) FEtype_error_cons(V13);
	T3= V13;
	T4= L61remove_upper_classes(V10)          /*  REMOVE-UPPER-CLASSES */;
	V13= ecl_list1(T4);
	(ECL_CONS_CDR(T3)=V13,T3);
	goto L66;
L67:;
	T2= ecl_cdr(V12);
	goto L59;
	}
	}
L59:;
	V9= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V9,T1);
	goto L43;
L44:;
	T0= ecl_cdr(V8);
	goto L35;
	}
	}
L35:;
	T1= cl_list(2,VV[225],T0)                 /*  LIST            */;
	value0=ecl_list1(T1); cl_env_copy->nvalues=1;
	return value0;
L33:;
	value0=(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[237]) /*  BREAK */;
	return value0;
	}
}}
/*	function definition for OLD-REMOVE-UPPER-CLASSES              */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L62old_remove_upper_classes(cl_object V1, cl_object V2)
{ VT63 VLEX63 CLSR63 STCK63
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  ONTOREPR        */
	cl_object V4;                             /*  FIRSTITEM       */
	cl_object V5;                             /*  SECONDITEM      */
	cl_object V6;                             /*  THIRDITEM       */
	cl_object V7;                             /*  ANDLIST         */
	cl_object V8;                             /*  ANDRESULT       */
	cl_object V9;                             /*  NEWSECOPERANDS  */
	cl_object V10;                            /*  TEMPRESULT      */
	cl_object V11;                            /*  RESULT          */
	{cl_object V12;
	cl_object V13;
	V12= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V1))) FEtype_error_list(V1);
	V13= V1;
	{cl_object V14;
	cl_object V15;
	V14= ecl_list1(ECL_NIL);
	V15= V14;
L8:;
	if(!(ecl_endp(V13))){
	goto L12;}
	goto L9;
L12:;
	goto L10;
L10:;
	V12= _ecl_car(V13);
	{cl_object V16;
	V16= _ecl_cdr(V13);
	if (ecl_unlikely(!ECL_LISTP(V16))) FEtype_error_list(V16);
	V13= V16;
	}
	if (ecl_unlikely(ECL_ATOM(V15))) FEtype_error_cons(V15);
	T0= V15;
	T1= L63shorten_loops(V12,ECL_NIL,ECL_NIL) /*  SHORTEN-LOOPS   */;
	V15= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V15,T0);
	goto L8;
L9:;
	V3= ecl_cdr(V14);
	goto L1;
	}
	}
L1:;
	V4= ecl_car(V3);
	V5= ecl_cadr(V3);
	V6= ecl_caddr(V3);
	V7= ECL_NIL;
	V8= ECL_NIL;
	V9= ECL_NIL;
	V10= ECL_NIL;
	V11= ECL_NIL;
	if(!(V3==ECL_NIL)){
	goto L38;}
	V11= ECL_NIL;
	goto L36;
L38:;
	if((L52is_simple_onto_item(V4)            /*  IS-SIMPLE-ONTO-ITEM */)==ECL_NIL){
	goto L41;}
	if(!(V5==ECL_NIL)){
	goto L44;}
	V11= ecl_list1(V4);
	goto L36;
L44:;
	if((ecl_function_dispatch(cl_env_copy,VV[384])(1,V4) /*  IS-A-STRUCTURAL-ITEM */)==ECL_NIL){
	goto L47;}
	T0= ecl_cdr(V3);
	T1= L62old_remove_upper_classes(T0,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	V11= CONS(V4,T1);
	goto L36;
L47:;
	if((L52is_simple_onto_item(V5)            /*  IS-SIMPLE-ONTO-ITEM */)==ECL_NIL){
	goto L50;}
	if(!((VV[235])==(V5))){
	goto L53;}
	if((L52is_simple_onto_item(V6)            /*  IS-SIMPLE-ONTO-ITEM */)==ECL_NIL){
	goto L56;}
	T0= ecl_cadddr(V3);
	if(!((T0)==(VV[236]))){
	goto L59;}
	T0= ecl_function_dispatch(cl_env_copy,VV[367])(2,ecl_make_fixnum(4),V3) /*  FIRST-N */;
	T1= ecl_cddddr(V3);
	T2= L62old_remove_upper_classes(T1,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	V11= ecl_append(T0,T2);
	goto L36;
L59:;
	T0= ecl_cdddr(V3);
	T1= CONS(V4,T0);
	V11= L62old_remove_upper_classes(T1,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	goto L36;
L56:;
	T0= ecl_car(V6);
	if(!((ECL_SYM("AND",87))==(T0))){
	goto L62;}
	V7= ecl_cadr(V6);
	{cl_object V12;                           /*  LET3443         */
	cl_object V13;                            /*  LET3444         */
	cl_object V14;                            /*  NXTOPERAND      */
	cl_object V15;                            /*  ANDLIST         */
	V12= ecl_car(V7);
	V13= ecl_cdr(V7);
	V14= V12;
	V15= V13;
	goto L73;
L72:;
	T0= ecl_car(V14);
	if((L52is_simple_onto_item(T0)            /*  IS-SIMPLE-ONTO-ITEM */)==ECL_NIL){
	goto L77;}
	T0= ecl_car(V14);
	if((ecl_function_dispatch(cl_env_copy,VV[384])(1,T0) /*  IS-A-STRUCTURAL-ITEM */)==ECL_NIL){
	goto L80;}
	(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[239]) /*  BREAK */;
	goto L75;
L80:;
	T0= ecl_cadr(V14);
	if((L52is_simple_onto_item(T0)            /*  IS-SIMPLE-ONTO-ITEM */)==ECL_NIL){
	goto L83;}
	T0= ecl_cdr(V14);
	T1= L62old_remove_upper_classes(T0,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	V8= ecl_function_dispatch(cl_env_copy,VV[274])(2,V8,T1) /*  APPEND1 */;
	goto L75;
L83:;
	T0= ecl_cadr(V14);
	T1= ecl_car(T0);
	if(!((ECL_SYM("AND",87))==(T1))){
	goto L87;}
	T0= ecl_car(V14);
	T1= ecl_cadr(V14);
	T2= ecl_cadr(T1);
	V9= L74flatten_second_and(T0,T2)          /*  FLATTEN-SECOND-AND */;
	V15= ecl_append(V9,V15);
	goto L75;
L87:;
	T0= ecl_car(V14);
	T1= ecl_cdr(V14);
	T2= L62old_remove_upper_classes(T1,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	T3= CONS(T0,T2);
	V8= ecl_function_dispatch(cl_env_copy,VV[274])(2,V8,T3) /*  APPEND1 */;
	goto L75;
L77:;
	T0= ecl_car(V14);
	T1= ecl_car(T0);
	if(!((ECL_SYM("AND",87))==(T1))){
	goto L94;}
	(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[240]) /*  BREAK */;
	goto L75;
L94:;
	T0= ecl_car(V14);
	T1= ecl_cadr(V14);
	T2= ecl_cdr(V14);
	T3= ecl_cdr(T2);
	T4= L62old_remove_upper_classes(T3,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	T5= cl_listX(3,T0,T1,T4)                  /*  LIST*           */;
	V8= ecl_function_dispatch(cl_env_copy,VV[274])(2,V8,T5) /*  APPEND1 */;
	goto L75;
L75:;
	V14= ecl_car(V15);
	V15= ecl_cdr(V15);
L73:;
	if(V14==ECL_NIL){
	goto L102;}
	goto L72;
L102:;
	goto L100;
L100:;
	goto L70;
L70:;
	V10= ecl_function_dispatch(cl_env_copy,VV[373])(1,V8) /*  DROPNIL2 */;
	{cl_fixnum V16;
	V16= ecl_length(V10);
	if(!((V16)>(1))){
	goto L107;}}
	T0= cl_list(2,ECL_SYM("AND",87),V10)      /*  LIST            */;
	T1= ecl_cdddr(V3);
	T2= L62old_remove_upper_classes(T1,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	V11= cl_listX(3,V4,T0,T2)                 /*  LIST*           */;
	goto L36;
L107:;
	T0= ecl_car(V10);
	T1= ecl_cdddr(V3);
	T2= L62old_remove_upper_classes(T1,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	V11= ecl_append(T0,T2);
	goto L36;
	}
L62:;
	V11= (cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[240]) /*  BREAK */;
	goto L36;
L53:;
	if(!((VV[236])==(V5))){
	goto L110;}
	if((L52is_simple_onto_item(V6)            /*  IS-SIMPLE-ONTO-ITEM */)==ECL_NIL){
	goto L113;}
	T0= ecl_cdr(V3);
	T1= ecl_cdr(T0);
	V11= L62old_remove_upper_classes(T1,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	goto L36;
L113:;
	T0= ecl_cdr(V3);
	T1= ecl_cdr(T0);
	V11= L62old_remove_upper_classes(T1,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	goto L36;
L110:;
	T0= ecl_cdr(V3);
	T1= ecl_cdr(T0);
	T2= L62old_remove_upper_classes(T1,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	V11= cl_listX(3,V4,V5,T2)                 /*  LIST*           */;
	goto L36;
L50:;
	T0= ecl_car(V5);
	if(!((ECL_SYM("AND",87))==(T0))){
	goto L116;}
	T0= ecl_cadr(V5);
	V10= L74flatten_second_and(V4,T0)         /*  FLATTEN-SECOND-AND */;
	{cl_object V16;
	cl_object V17;
	V16= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V10))) FEtype_error_list(V10);
	V17= V10;
	{cl_object V18;
	cl_object V19;
	V18= ecl_list1(ECL_NIL);
	V19= V18;
L129:;
	if(!(ecl_endp(V17))){
	goto L133;}
	goto L130;
L133:;
	goto L131;
L131:;
	V16= _ecl_car(V17);
	{cl_object V20;
	V20= _ecl_cdr(V17);
	if (ecl_unlikely(!ECL_LISTP(V20))) FEtype_error_list(V20);
	V17= V20;
	}
	if (ecl_unlikely(ECL_ATOM(V19))) FEtype_error_cons(V19);
	T1= V19;
	T2= L75non_single_rest(V16)               /*  NON-SINGLE-REST */;
	V19= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V19,T1);
	goto L129;
L130:;
	T0= ecl_cdr(V18);
	goto L122;
	}
	}
L122:;
	T1= L77remove_inner_ands(T0)              /*  REMOVE-INNER-ANDS */;
	V10= ecl_function_dispatch(cl_env_copy,VV[373])(1,T1) /*  DROPNIL2 */;
	{cl_fixnum V16;
	V16= ecl_length(V10);
	if(!((V16)>(1))){
	goto L148;}}
	V11= L76list_non_single(V4,V10)           /*  LIST-NON-SINGLE */;
	goto L36;
L148:;
	T0= ecl_car(V10);
	V11= CONS(V4,T0);
	goto L36;
L116:;
	T0= ecl_cdr(V3);
	T1= ecl_cdr(T0);
	T2= L62old_remove_upper_classes(T1,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	V11= cl_listX(3,V4,V5,T2)                 /*  LIST*           */;
	goto L36;
L41:;
	T0= ecl_car(V4);
	if(!((ECL_SYM("AND",87))==(T0))){
	goto L151;}
	V11= ECL_NIL;
	goto L36;
L151:;
	if(!(V5==ECL_NIL)){
	goto L154;}
	V11= ecl_list1(V4);
	goto L36;
L154:;
	T0= ecl_cdr(V3);
	T1= ecl_cdr(T0);
	T2= L62old_remove_upper_classes(T1,ECL_NIL) /*  OLD-REMOVE-UPPER-CLASSES */;
	V11= cl_listX(3,V4,V5,T2)                 /*  LIST*           */;
	goto L36;
L36:;
	value0=V11; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for SHORTEN-LOOPS                         */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L63shorten_loops(cl_object V1, cl_object V2, cl_object V3)
{ VT64 VLEX64 CLSR64 STCK64
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V4;                            /*  RESULT          */
	cl_object V5;                             /*  NEWSHORT        */
	cl_object V6;                             /*  RESTPATH        */
	cl_object V7;                             /*  NEWBUFF1        */
	cl_object V8;                             /*  NEWBUFF2        */
	V4= ECL_NIL;
	V5= ECL_NIL;
	V6= ECL_NIL;
	V7= ECL_NIL;
	V8= ECL_NIL;
	if(!(V1==ECL_NIL)){
	goto L7;}
	{cl_object V9;                            /*  LET3448         */
	cl_object V10;
	cl_object V11;
	V9= ecl_append(V2,V3);
	V10= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V9))) FEtype_error_list(V9);
	V11= V9;
	{cl_object V12;
	cl_object V13;
	V12= ecl_list1(ECL_NIL);
	V13= V12;
L18:;
	if(!(ecl_endp(V11))){
	goto L22;}
	goto L19;
L22:;
	goto L20;
L20:;
	V10= _ecl_car(V11);
	{cl_object V14;
	V14= _ecl_cdr(V11);
	if (ecl_unlikely(!ECL_LISTP(V14))) FEtype_error_list(V14);
	V11= V14;
	}
	if (ecl_unlikely(ECL_ATOM(V13))) FEtype_error_cons(V13);
	T0= V13;
	T1= cl_reverse(V10)                       /*  REVERSE         */;
	V13= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V13,T0);
	goto L18;
L19:;
	V4= ecl_cdr(V12);
	goto L10;
	}
	}
L10:;
	value0=V4; cl_env_copy->nvalues=1;
	return value0;
L7:;
	T0= ecl_car(V1);
	if(ECL_ATOM(T0)){
	goto L39;}
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	if((ecl_function_dispatch(cl_env_copy,VV[276])(2,T1,VV[242]) /*  MEMQ */)!=ECL_NIL){
	goto L37;}
	goto L38;
L39:;
L38:;
	cl_env_copy->values[0]=L68check_revpath(V1) /*  CHECK-REVPATH */;
	{int V9=cl_env_copy->nvalues-0;
	if (V9--<=0) goto L43;
	V5= cl_env_copy->values[0];
	if (V9--<=0) goto L44;
	V6= cl_env_copy->values[1];
	goto L45;}
L43:;
	V5= ECL_NIL;
L44:;
	V6= ECL_NIL;
L45:;
	if(!(V5==ECL_NIL)){
	goto L47;}
	{cl_object V9;
	V9= ecl_cdr(V1);
	T0= ecl_car(V1);
	V2= L66flat_cons(T0,V2)                   /*  FLAT-CONS       */;
	V1= V9;}
	goto TTL;
L47:;
	T0= ecl_car(V5);
	V7= L66flat_cons(T0,V2)                   /*  FLAT-CONS       */;
	if(!(V2==ECL_NIL)){
	goto L58;}
	T0= cl_reverse(V5)                        /*  REVERSE         */;
	T1= ecl_list1(T0);
	V8= ecl_list1(T1);
	goto L56;
L58:;
	{cl_object V9;
	cl_object V10;
	V9= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V2))) FEtype_error_list(V2);
	V10= V2;
	{cl_object V11;
	cl_object V12;
	V11= ecl_list1(ECL_NIL);
	V12= V11;
L67:;
	if(!(ecl_endp(V10))){
	goto L71;}
	goto L68;
L71:;
	goto L69;
L69:;
	V9= _ecl_car(V10);
	{cl_object V13;
	V13= _ecl_cdr(V10);
	if (ecl_unlikely(!ECL_LISTP(V13))) FEtype_error_list(V13);
	V10= V13;
	}
	if (ecl_unlikely(ECL_ATOM(V12))) FEtype_error_cons(V12);
	T1= V12;
	T3= cl_reverse(V5)                        /*  REVERSE         */;
	T2= ecl_append(T3,V9);
	V12= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V12,T1);
	goto L67;
L68:;
	T0= ecl_cdr(V11);
	goto L60;
	}
	}
L60:;
	V8= ecl_append(T0,V3);
	goto L56;
L56:;
	V1= V6;
	V2= V7;
	V3= V8;
	goto TTL;
L37:;
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	if(!((T1)==(VV[225]))){
	goto L91;}
	{cl_object V9;
	cl_object V10;
	V9= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V2))) FEtype_error_list(V2);
	V10= V2;
	{cl_object V11;
	cl_object V12;
	V11= ecl_list1(ECL_NIL);
	V12= V11;
L100:;
	if(!(ecl_endp(V10))){
	goto L104;}
	goto L101;
L104:;
	goto L102;
L102:;
	V9= _ecl_car(V10);
	{cl_object V13;
	V13= _ecl_cdr(V10);
	if (ecl_unlikely(!ECL_LISTP(V13))) FEtype_error_list(V13);
	V10= V13;
	}
	if (ecl_unlikely(ECL_ATOM(V12))) FEtype_error_cons(V12);
	T1= V12;
	T2= cl_reverse(V9)                        /*  REVERSE         */;
	V12= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V12,T1);
	goto L100;
L101:;
	T0= ecl_cdr(V11);
	goto L93;
	}
	}
L93:;
	{cl_object V9;                            /*  LET3454         */
	cl_object V10;
	cl_object V11;
	T2= ecl_car(V1);
	V9= ecl_cadr(T2);
	V10= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V9))) FEtype_error_list(V9);
	V11= V9;
	{cl_object V12;
	cl_object V13;
	V12= ecl_list1(ECL_NIL);
	V13= V12;
L126:;
	if(!(ecl_endp(V11))){
	goto L130;}
	goto L127;
L130:;
	goto L128;
L128:;
	V10= _ecl_car(V11);
	{cl_object V14;
	V14= _ecl_cdr(V11);
	if (ecl_unlikely(!ECL_LISTP(V14))) FEtype_error_list(V14);
	V11= V14;
	}
	if (ecl_unlikely(ECL_ATOM(V13))) FEtype_error_cons(V13);
	T2= V13;
	{cl_object V14;
	cl_object V15;
	V14= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V10))) FEtype_error_list(V10);
	V15= V10;
	{cl_object V16;
	cl_object V17;
	V16= ecl_list1(ECL_NIL);
	V17= V16;
L149:;
	if(!(ecl_endp(V15))){
	goto L153;}
	goto L150;
L153:;
	goto L151;
L151:;
	V14= _ecl_car(V15);
	{cl_object V18;
	V18= _ecl_cdr(V15);
	if (ecl_unlikely(!ECL_LISTP(V18))) FEtype_error_list(V18);
	V15= V18;
	}
	if (ecl_unlikely(ECL_ATOM(V17))) FEtype_error_cons(V17);
	T4= V17;
	T6= L63shorten_loops(V14,ECL_NIL,ECL_NIL) /*  SHORTEN-LOOPS   */;
	T5= ecl_function_dispatch(cl_env_copy,VV[387])(1,T6) /*  FLATTEN */;
	V17= ecl_list1(T5);
	(ECL_CONS_CDR(T4)=V17,T4);
	goto L149;
L150:;
	T3= ecl_cdr(V16);
	goto L142;
	}
	}
L142:;
	V13= ecl_list1(T3);
	(ECL_CONS_CDR(T2)=V13,T2);
	goto L126;
L127:;
	T1= ecl_cdr(V12);
	goto L118;
	}
	}
L118:;
	T2= cl_list(2,VV[225],T1)                 /*  LIST            */;
	value0=L65flat_append1(T0,T2)             /*  FLAT-APPEND1    */;
	return value0;
L91:;
	value0=(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[243]) /*  BREAK */;
	return value0;
	}
}}
/*	function definition for FLATTEN-ALL-ANDS                      */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L64flatten_all_ands(cl_object V1, cl_object V2)
{ VT65 VLEX65 CLSR65 STCK65
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	{cl_object V3;
	cl_object V4;
	V3= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V2))) FEtype_error_list(V2);
	V4= V2;
	{cl_object V5;
	cl_object V6;
	V5= ecl_list1(ECL_NIL);
	V6= V5;
L10:;
	if(!(ecl_endp(V4))){
	goto L14;}
	goto L11;
L14:;
	goto L12;
L12:;
	V3= _ecl_car(V4);
	{cl_object V7;
	V7= _ecl_cdr(V4);
	if (ecl_unlikely(!ECL_LISTP(V7))) FEtype_error_list(V7);
	V4= V7;
	}
	if (ecl_unlikely(ECL_ATOM(V6))) FEtype_error_cons(V6);
	T0= V6;
	T1= cl_reverse(V3)                        /*  REVERSE         */;
	V6= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V6,T0);
	goto L10;
L11:;
	value0=ecl_cdr(V5); cl_env_copy->nvalues=1;
	return value0;
	}
	}
L2:;
	T0= ecl_car(V1);
	if((L52is_simple_onto_item(T0)            /*  IS-SIMPLE-ONTO-ITEM */)!=ECL_NIL){
	goto L31;}
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	if(!((ECL_SYM("EQ",333))==(T1))){
	goto L29;}
	goto L30;
L31:;
L30:;
	{cl_object V7;
	V7= ecl_cdr(V1);
	T0= ecl_car(V1);
	V2= L66flat_cons(T0,V2)                   /*  FLAT-CONS       */;
	V1= V7;}
	goto TTL;
L29:;
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	if(!((ECL_SYM("AND",87))==(T1))){
	goto L37;}
	{cl_object V7;                            /*  LET3460         */
	cl_object V8;
	cl_object V9;
	T1= ecl_car(V1);
	V7= ecl_cadr(T1);
	V8= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V7))) FEtype_error_list(V7);
	V9= V7;
	{cl_object V10;
	cl_object V11;
	V10= ecl_list1(ECL_NIL);
	V11= V10;
L47:;
	if(!(ecl_endp(V9))){
	goto L51;}
	goto L48;
L51:;
	goto L49;
L49:;
	V8= _ecl_car(V9);
	{cl_object V12;
	V12= _ecl_cdr(V9);
	if (ecl_unlikely(!ECL_LISTP(V12))) FEtype_error_list(V12);
	V9= V12;
	}
	if (ecl_unlikely(ECL_ATOM(V11))) FEtype_error_cons(V11);
	T1= V11;
	T2= L64flatten_all_ands(V8,V2)            /*  FLATTEN-ALL-ANDS */;
	V11= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V11,T1);
	goto L47;
L48:;
	T0= ecl_cdr(V10);
	goto L39;
	}
	}
L39:;
	value0=L67int_fl_all_ands(T0)             /*  INT-FL-ALL-ANDS */;
	return value0;
L37:;
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	if(!((VV[225])==(T1))){
	goto L67;}
	{cl_object V7;
	cl_object V8;
	V7= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V2))) FEtype_error_list(V2);
	V8= V2;
	{cl_object V9;
	cl_object V10;
	V9= ecl_list1(ECL_NIL);
	V10= V9;
L76:;
	if(!(ecl_endp(V8))){
	goto L80;}
	goto L77;
L80:;
	goto L78;
L78:;
	V7= _ecl_car(V8);
	{cl_object V11;
	V11= _ecl_cdr(V8);
	if (ecl_unlikely(!ECL_LISTP(V11))) FEtype_error_list(V11);
	V8= V11;
	}
	if (ecl_unlikely(ECL_ATOM(V10))) FEtype_error_cons(V10);
	T1= V10;
	T2= cl_reverse(V7)                        /*  REVERSE         */;
	V10= ecl_list1(T2);
	(ECL_CONS_CDR(T1)=V10,T1);
	goto L76;
L77:;
	T0= ecl_cdr(V9);
	goto L69;
	}
	}
L69:;
	{cl_object V7;                            /*  LET3464         */
	cl_object V8;
	cl_object V9;
	T2= ecl_car(V1);
	V7= ecl_cadr(T2);
	V8= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V7))) FEtype_error_list(V7);
	V9= V7;
	{cl_object V10;
	cl_object V11;
	V10= ecl_list1(ECL_NIL);
	V11= V10;
L102:;
	if(!(ecl_endp(V9))){
	goto L106;}
	goto L103;
L106:;
	goto L104;
L104:;
	V8= _ecl_car(V9);
	{cl_object V12;
	V12= _ecl_cdr(V9);
	if (ecl_unlikely(!ECL_LISTP(V12))) FEtype_error_list(V12);
	V9= V12;
	}
	if (ecl_unlikely(ECL_ATOM(V11))) FEtype_error_cons(V11);
	T2= V11;
	T3= L64flatten_all_ands(V8,ECL_NIL)       /*  FLATTEN-ALL-ANDS */;
	V11= ecl_list1(T3);
	(ECL_CONS_CDR(T2)=V11,T2);
	goto L102;
L103:;
	T1= ecl_cdr(V10);
	goto L94;
	}
	}
L94:;
	T2= cl_list(2,VV[225],T1)                 /*  LIST            */;
	value0=L65flat_append1(T0,T2)             /*  FLAT-APPEND1    */;
	return value0;
L67:;
	value0=(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[245]) /*  BREAK */;
	return value0;
}}
/*	function definition for FLAT-APPEND1                          */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L65flat_append1(cl_object V1, cl_object V2)
{ VT66 VLEX66 CLSR66 STCK66
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	T0= ecl_list1(V2);
	value0=ecl_list1(T0); cl_env_copy->nvalues=1;
	return value0;
L2:;
	{cl_object V3;
	cl_object V4;
	V3= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V1))) FEtype_error_list(V1);
	V4= V1;
	{cl_object V5;
	cl_object V6;
	V5= ecl_list1(ECL_NIL);
	V6= V5;
L10:;
	if(!(ecl_endp(V4))){
	goto L14;}
	goto L11;
L14:;
	goto L12;
L12:;
	V3= _ecl_car(V4);
	{cl_object V7;
	V7= _ecl_cdr(V4);
	if (ecl_unlikely(!ECL_LISTP(V7))) FEtype_error_list(V7);
	V4= V7;
	}
	if (ecl_unlikely(ECL_ATOM(V6))) FEtype_error_cons(V6);
	T0= V6;
	T1= ecl_function_dispatch(cl_env_copy,VV[274])(2,V3,V2) /*  APPEND1 */;
	V6= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V6,T0);
	goto L10;
L11:;
	value0=ecl_cdr(V5); cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for FLAT-CONS                             */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L66flat_cons(cl_object V1, cl_object V2)
{ VT67 VLEX67 CLSR67 STCK67
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V2==ECL_NIL)){
	goto L2;}
	T0= ecl_list1(V1);
	value0=ecl_list1(T0); cl_env_copy->nvalues=1;
	return value0;
L2:;
	{cl_object V3;
	cl_object V4;
	V3= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V2))) FEtype_error_list(V2);
	V4= V2;
	{cl_object V5;
	cl_object V6;
	V5= ecl_list1(ECL_NIL);
	V6= V5;
L10:;
	if(!(ecl_endp(V4))){
	goto L14;}
	goto L11;
L14:;
	goto L12;
L12:;
	V3= _ecl_car(V4);
	{cl_object V7;
	V7= _ecl_cdr(V4);
	if (ecl_unlikely(!ECL_LISTP(V7))) FEtype_error_list(V7);
	V4= V7;
	}
	if (ecl_unlikely(ECL_ATOM(V6))) FEtype_error_cons(V6);
	T0= V6;
	T1= CONS(V1,V3);
	V6= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V6,T0);
	goto L10;
L11:;
	value0=ecl_cdr(V5); cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for INT-FL-ALL-ANDS                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L67int_fl_all_ands(cl_object V1)
{ VT68 VLEX68 CLSR68 STCK68
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L2:;
	T0= ecl_car(V1);
	T1= ecl_cdr(V1);
	T2= L67int_fl_all_ands(T1)                /*  INT-FL-ALL-ANDS */;
	value0=ecl_append(T0,T2); cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for CHECK-REVPATH                         */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L68check_revpath(cl_object V1)
{ VT69 VLEX69 CLSR69 STCK69
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  LET3469         */
	cl_object V3;                             /*  PLENGTH         */
	cl_object V4;                             /*  HALFLENG        */
	cl_object V5;                             /*  REM             */
	cl_object V6;                             /*  FOUND           */
	cl_object V7;                             /*  PREVPATH        */
	T0= ecl_car(V1);
	V2= L72find_same_beg_start(V1,T0,ECL_NIL,ECL_NIL) /*  FIND-SAME-BEG-START */;
	V3= ECL_NIL;
	V4= ECL_NIL;
	V5= ECL_NIL;
	V6= ECL_NIL;
	V7= ECL_NIL;
	{cl_object V8;                            /*  LET3470         */
	cl_object V9;                             /*  LET3471         */
	cl_object V10;                            /*  NXTSTART        */
	cl_object V11;                            /*  AFTERSTART      */
	V8= ecl_car(V2);
	V9= ecl_cdr(V2);
	V10= V8;
	V11= V9;
	goto L14;
L13:;
	V3= ecl_make_fixnum(ecl_length(V10));
	cl_env_copy->values[0]=ecl_floor2(V3,ecl_make_fixnum(2));
	V4= cl_env_copy->values[0];
	V5= cl_env_copy->values[1];
	if(!(ecl_number_equalp(V5,ecl_make_fixnum(1)))){
	goto L22;}
	V6= L70pathsymmetric(V10,V4,V3)           /*  PATHSYMMETRIC   */;
	V7= V10;
	goto L20;
L22:;
	goto L20;
L20:;
	V10= ecl_car(V11);
	V11= ecl_cdr(V11);
L14:;
	if(V10==ECL_NIL){
	goto L32;}
	if((V6)!=ECL_NIL){
	goto L32;}
	goto L13;
L32:;
	goto L30;
L30:;
	goto L11;
L11:;
	if((V6)==ECL_NIL){
	goto L36;}
	T0= L69remove_rev_path(V1,V7)             /*  REMOVE-REV-PATH */;
	cl_env_copy->nvalues=2;
	cl_env_copy->values[1]=T0;
	cl_env_copy->values[0]=V6;
	return cl_env_copy->values[0];
L36:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for REMOVE-REV-PATH                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L69remove_rev_path(cl_object V1, cl_object V2)
{ VT70 VLEX70 CLSR70 STCK70
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V2==ECL_NIL)){
	goto L2;}
	value0=V1; cl_env_copy->nvalues=1;
	return value0;
L2:;
	T0= ecl_car(V2);
	T1= ecl_car(V1);
	if(!((T0)==(T1))){
	goto L5;}
	V1= ecl_cdr(V1);
	V2= ecl_cdr(V2);
	goto TTL;
L5:;
	value0=(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[251]) /*  BREAK */;
	return value0;
}}
/*	function definition for PATHSYMMETRIC                         */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L70pathsymmetric(cl_object V1, cl_object V2, cl_object V3)
{ VT71 VLEX71 CLSR71 STCK71
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V4;                            /*  BEGPATH         */
	V4= ecl_function_dispatch(cl_env_copy,VV[367])(2,V3,V1) /*  FIRST-N */;
	if((L71all_has_subclass(V4)               /*  ALL-HAS-SUBCLASS */)!=ECL_NIL){
	goto L3;}
	T0= cl_reverse(V4)                        /*  REVERSE         */;
	T1= ecl_function_dispatch(cl_env_copy,VV[395])(1,T0) /*  INV-RANGE-DOM */;
	T2= ecl_function_dispatch(cl_env_copy,VV[367])(2,V2,T1) /*  FIRST-N */;
	T3= ecl_function_dispatch(cl_env_copy,VV[367])(2,V2,V4) /*  FIRST-N */;
	if(!(ecl_equal(T2,T3))){
	goto L3;}
	T0= ecl_one_plus(V2);
	value0=ecl_function_dispatch(cl_env_copy,VV[367])(2,T0,V4) /*  FIRST-N */;
	return value0;
L3:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for ALL-HAS-SUBCLASS                      */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L71all_has_subclass(cl_object V1)
{ VT72 VLEX72 CLSR72 STCK72
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	value0=ECL_T; cl_env_copy->nvalues=1;
	return value0;
L2:;
	T0= ecl_car(V1);
	T1= ecl_function_dispatch(cl_env_copy,VV[372])(1,T0) /*  REMOVE-SYNT-POINTER */;
	if(!(ECL_ATOM(T1))){
	goto L5;}
	T0= ecl_car(V1);
	if((ecl_function_dispatch(cl_env_copy,VV[384])(1,T0) /*  IS-A-STRUCTURAL-ITEM */)==ECL_NIL){
	goto L8;}
	T0= ecl_car(V1);
	if((ecl_function_dispatch(cl_env_copy,VV[303])(2,T0,VV[236]) /*  NEQ */)==ECL_NIL){
	goto L8;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L8:;
	V1= ecl_cdr(V1);
	goto TTL;
L5:;
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for FIND-SAME-BEG-START                   */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L72find_same_beg_start(cl_object V1, cl_object V2, cl_object V3, cl_object V4)
{ VT73 VLEX73 CLSR73 STCK73
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	{cl_object V5;                            /*  LET3473         */
	cl_object V6;
	cl_object V7;
	V5= ecl_butlast(V4,1);
	V6= ECL_NIL;
	V7= V5;
	{cl_object V8;
	cl_object V9;
	V8= ecl_list1(ECL_NIL);
	V9= V8;
L10:;
	if(!(ecl_endp(V7))){
	goto L14;}
	goto L11;
L14:;
	goto L12;
L12:;
	V6= _ecl_car(V7);
	{cl_object V10;
	V10= _ecl_cdr(V7);
	if (ecl_unlikely(!ECL_LISTP(V10))) FEtype_error_list(V10);
	V7= V10;
	}
	if (ecl_unlikely(ECL_ATOM(V9))) FEtype_error_cons(V9);
	T0= V9;
	T1= cl_reverse(V6)                        /*  REVERSE         */;
	V9= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V9,T0);
	goto L10;
L11:;
	value0=ecl_cdr(V8); cl_env_copy->nvalues=1;
	return value0;
	}
	}
L2:;
	T0= ecl_car(V1);
	T1= ecl_function_dispatch(cl_env_copy,VV[372])(1,T0) /*  REMOVE-SYNT-POINTER */;
	if(!((T1)==(V2))){
	goto L29;}
	{cl_object V10;
	V10= ecl_cdr(V1);
	{cl_object V11;
	V11= V2;
	{cl_object V12;
	T0= ecl_car(V1);
	V12= CONS(T0,V3);
	T0= ecl_car(V1);
	T1= ecl_function_dispatch(cl_env_copy,VV[381])(2,V2,T0) /*  BEST-SYNT-CONC */;
	T2= CONS(T1,V3);
	V4= CONS(T2,V4);
	V3= V12;
	V2= V11;
	V1= V10;}}}
	goto TTL;
L29:;
	{cl_object V10;
	V10= ecl_cdr(V1);
	T0= ecl_car(V1);
	V3= CONS(T0,V3);
	V1= V10;}
	goto TTL;
}}
/*	function definition for INCLUDE-NON-RELATIONAL                */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L73include_non_relational(cl_object V1)
{ VT74 VLEX74 CLSR74 STCK74
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  FIRSTP          */
	V2= ECL_NIL;
	if(!(V1==ECL_NIL)){
	goto L3;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L3:;
	T0= ecl_car(V1);
	V2= ecl_function_dispatch(cl_env_copy,VV[372])(1,T0) /*  REMOVE-SYNT-POINTER */;
	if(!(ECL_ATOM(V2))){
	goto L8;}
	if((ecl_function_dispatch(cl_env_copy,VV[376])(2,V2,VV[256]) /*  IS-SUBCLASS-OF */)==ECL_NIL){
	goto L8;}
	value0=ECL_T; cl_env_copy->nvalues=1;
	return value0;
L8:;
	V1= ecl_cdr(V1);
	goto TTL;
	}
}}
/*	function definition for FLATTEN-SECOND-AND                    */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L74flatten_second_and(cl_object V1, cl_object V2)
{ VT75 VLEX75 CLSR75 STCK75
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;
	cl_object V4;
	V3= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V2))) FEtype_error_list(V2);
	V4= V2;
	{cl_object V5;
	cl_object V6;
	V5= ecl_list1(ECL_NIL);
	V6= V5;
L7:;
	if(!(ecl_endp(V4))){
	goto L11;}
	goto L8;
L11:;
	goto L9;
L9:;
	V3= _ecl_car(V4);
	{cl_object V7;
	V7= _ecl_cdr(V4);
	if (ecl_unlikely(!ECL_LISTP(V7))) FEtype_error_list(V7);
	V4= V7;
	}
	if (ecl_unlikely(ECL_ATOM(V6))) FEtype_error_cons(V6);
	T0= V6;
	T1= CONS(V1,V3);
	V6= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V6,T0);
	goto L7;
L8:;
	value0=ecl_cdr(V5); cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for NON-SINGLE-REST                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L75non_single_rest(cl_object V1)
{ VT76 VLEX76 CLSR76 STCK76
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_fixnum V2;
	V2= ecl_length(V1);
	if(!((1)==(V2))){
	goto L2;}}
	value0=V1; cl_env_copy->nvalues=1;
	return value0;
L2:;
	value0=ecl_cdr(V1); cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for LIST-NON-SINGLE                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L76list_non_single(cl_object V1, cl_object V2)
{ VT77 VLEX77 CLSR77 STCK77
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;                            /*  FOUND           */
	cl_object V4;                             /*  REMPATHS        */
	V3= ECL_NIL;
	V4= ECL_NIL;
	{cl_object V5;                            /*  LET3476         */
	cl_object V6;                             /*  LET3477         */
	cl_object V7;                             /*  NXTPATH         */
	cl_object V8;                             /*  FOLLPATHS       */
	V5= ecl_car(V2);
	V6= ecl_cdr(V2);
	V7= V5;
	V8= V6;
	goto L10;
L9:;
	{cl_fixnum V9;
	V9= ecl_length(V7);
	if(!((1)==(V9))){
	goto L14;}}
	V3= ecl_car(V7);
	goto L12;
L14:;
	V4= ecl_function_dispatch(cl_env_copy,VV[274])(2,V4,V7) /*  APPEND1 */;
	goto L12;
L12:;
	V7= ecl_car(V8);
	V8= ecl_cdr(V8);
L10:;
	if(V7==ECL_NIL){
	goto L23;}
	goto L9;
L23:;
	goto L21;
L21:;
	goto L7;
L7:;
	if((V3)==ECL_NIL){
	goto L26;}
	{cl_fixnum V9;
	V9= ecl_length(V4);
	if(!((1)==(V9))){
	goto L29;}}
	T0= ecl_car(V4);
	value0=CONS(V3,T0); cl_env_copy->nvalues=1;
	return value0;
L29:;
	T0= cl_list(2,ECL_SYM("AND",87),V4)       /*  LIST            */;
	value0=cl_list(2,V3,T0)                   /*  LIST            */;
	return value0;
L26:;
	T0= cl_list(2,ECL_SYM("AND",87),V4)       /*  LIST            */;
	value0=cl_list(2,V1,T0)                   /*  LIST            */;
	return value0;
	}
	}
}}
/*	function definition for REMOVE-INNER-ANDS                     */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L77remove_inner_ands(cl_object V1)
{ VT78 VLEX78 CLSR78 STCK78
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	if(!(V1==ECL_NIL)){
	goto L2;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L2:;
	T0= ecl_car(V1);
	if(ECL_ATOM(T0)){
	goto L7;}
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	if(!(ECL_ATOM(T1))){
	goto L5;}
	goto L6;
L7:;
L6:;
	T0= ecl_car(V1);
	T1= ecl_cdr(V1);
	T2= L77remove_inner_ands(T1)              /*  REMOVE-INNER-ANDS */;
	value0=CONS(T0,T2); cl_env_copy->nvalues=1;
	return value0;
L5:;
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	T2= ecl_car(T1);
	if(!((ECL_SYM("AND",87))==(T2))){
	goto L10;}
	T0= ecl_car(V1);
	T1= ecl_car(T0);
	T2= ecl_cadr(T1);
	T3= ecl_cdr(V1);
	T4= L77remove_inner_ands(T3)              /*  REMOVE-INNER-ANDS */;
	value0=ecl_append(T2,T4); cl_env_copy->nvalues=1;
	return value0;
L10:;
	T0= ecl_car(V1);
	T1= ecl_cdr(V1);
	T2= L77remove_inner_ands(T1)              /*  REMOVE-INNER-ANDS */;
	value0=CONS(T0,T2); cl_env_copy->nvalues=1;
	return value0;
}}
/*	function definition for REMOVE-UP-DOWN                        */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L78remove_up_down(cl_object V1)
{ VT79 VLEX79 CLSR79 STCK79
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  LET3478         */
	cl_object V3;                             /*  LET3479         */
	cl_object V4;                             /*  LET3480         */
	cl_object V5;                             /*  LET3481         */
	cl_object V6;                             /*  ANDLIST         */
	cl_object V7;                             /*  ANDRESULT       */
	V2= ecl_car(V1);
	V3= ecl_cadr(V1);
	V4= ecl_caddr(V1);
	V5= ecl_cadddr(V1);
	V6= ECL_NIL;
	V7= ECL_NIL;
	if(!(V1==ECL_NIL)){
	goto L8;}
	value0=ECL_NIL; cl_env_copy->nvalues=1;
	return value0;
L8:;
	if(!(ECL_ATOM(V2))){
	goto L11;}
	if((ecl_function_dispatch(cl_env_copy,VV[384])(1,V2) /*  IS-A-STRUCTURAL-ITEM */)==ECL_NIL){
	goto L14;}
	T0= ecl_cdr(V1);
	T1= L78remove_up_down(T0)                 /*  REMOVE-UP-DOWN  */;
	value0=CONS(V2,T1); cl_env_copy->nvalues=1;
	return value0;
L14:;
	T0= ecl_function_dispatch(cl_env_copy,VV[372])(1,V3) /*  REMOVE-SYNT-POINTER */;
	if(!(ECL_ATOM(T0))){
	goto L17;}
	if(!((VV[235])==(V3))){
	goto L20;}
	T0= ecl_function_dispatch(cl_env_copy,VV[372])(1,V4) /*  REMOVE-SYNT-POINTER */;
	if(!(ECL_ATOM(T0))){
	goto L23;}
	T0= ecl_function_dispatch(cl_env_copy,VV[372])(1,V5) /*  REMOVE-SYNT-POINTER */;
	if(!(ECL_ATOM(T0))){
	goto L26;}
	if((