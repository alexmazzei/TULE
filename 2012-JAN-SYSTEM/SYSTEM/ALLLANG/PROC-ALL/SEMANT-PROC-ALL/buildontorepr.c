/*	Compiler: ECL 12.7.1                                          */
/*	Date: 2012/9/20 14:57 (yyyy/mm/dd)                            */
/*	Machine: Darwin 10.8.0 x86_64                                 */
/*	Source: /Users/mazzei/lavori/Projects/ATLAS/softExt/tup/2012-JAN-SYSTEM/SYSTEM/ALLLANG/PROC-ALL/SEMANT-PROC-ALL/buildontorepr */
#include <ecl/ecl-cmp.h>
#include "/Users/mazzei/lavori/Projects/ATLAS/softExt/tup/2012-JAN-SYSTEM/SYSTEM/ALLLANG/PROC-ALL/SEMANT-PROC-ALL/buildontorepr.eclh"
/*	function definition for BUILD-SEM-QUERY                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L1build_sem_query(cl_narg narg, cl_object V1, ...)
{ VT2 VLEX2 CLSR2 STCK2
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	if (ecl_unlikely(narg<1)) FEwrong_num_arguments_anonym();
	if (ecl_unlikely(narg>2)) FEwrong_num_arguments_anonym();
	{
	cl_object V2;
	va_list args; va_start(args,V1);
	ecl_bds_bind(cl_env_copy,VV[0],V1);       /*  ANNOTATED-TREE  */
	{int i=1;
	if (i >= narg) {
	V2= ECL_NIL;
	} else {
	i++;
	V2= va_arg(args,cl_object);
	}}
	va_end(args);
	{cl_object V3;                            /*  LET3486         */
	cl_object V4;                             /*  THEME-SUBTREE   */
	cl_object V5;                             /*  OBJ-SUBTREE     */
	cl_object V6;                             /*  TOPIC-SUBTREE   */
	cl_object V7;                             /*  FULL-TOPIC-SUBTREE */
	cl_object V8;                             /*  ACT-TOPIC-SUBTREE */
	cl_object V9;                             /*  TEMP-SUBTREE    */
	cl_object V10;                            /*  TOPICS          */
	cl_object V11;                            /*  MULT-DEFAULT-INFOS */
	cl_object V12;                            /*  RESTRICTIONS    */
	cl_object V13;                            /*  SEMRESTR        */
	cl_object V14;                            /*  HEAD-IDENT      */
	cl_object V15;                            /*  CURR-CONTEXT    */
	cl_object V16;                            /*  QTENSE-MARKER   */
	cl_object V17;                            /*  DET-MEAN        */
	cl_object V18;                            /*  OBJ-DET-MEAN    */
	V3= ecl_symbol_value(VV[0]);
	V4= ECL_NIL;
	V5= ECL_NIL;
	V6= ECL_NIL;
	V7= ECL_NIL;
	V8= ECL_NIL;
	ecl_bds_bind(cl_env_copy,VV[5],V3);       /*  +FULL-TREE+     */
	V9= ECL_NIL;
	V10= ECL_NIL;
	V11= ECL_NIL;
	V12= ECL_NIL;
	V13= ECL_NIL;
	V14= ECL_NIL;
	V15= ECL_NIL;
	ecl_bds_bind(cl_env_copy,VV[6],ECL_NIL);  /*  TOPIC-CHANGES   */
	V16= ECL_NIL;
	V17= ECL_NIL;
	V18= ECL_NIL;
	if(!((ecl_symbol_value(VV[4]))==(VV[7]))){
	goto L20;}
	V15= ecl_function_dispatch(cl_env_copy,VV[483])(1,ecl_symbol_value(VV[1])) /*  GET-TOP-BLOCK-NAME */;
	goto L18;
L20:;
	if(!((ecl_symbol_value(VV[4]))==(VV[8]))){
	goto L24;}
	V15= VV[9];
	goto L18;
L24:;
	goto L18;
L18:;
	cl_env_copy->values[0]=ecl_function_dispatch(cl_env_copy,VV[484])(1,ecl_symbol_value(VV[0])) /*  SKIP-QUESTION-TENSE-MARKER */;
	{int V19=cl_env_copy->nvalues-0;
	if (V19--<=0) goto L29;
	cl_set(VV[0],cl_env_copy->values[0]);
	if (V19--<=0) goto L30;
	V16= cl_env_copy->values[1];
	goto L31;}
L29:;
	cl_set(VV[0],ECL_NIL);
L30:;
	V16= ECL_NIL;
L31:;
	{cl_object V19;                           /*  LET3487         */
	cl_object V20;                            /*  RESULT-QUERY    */
	cl_object V21;                            /*  OBJ-SUBTREE-MEAN */
	cl_object V22;                            /*  TO-BE-SEARCH    */
	V19= ecl_function_dispatch(cl_env_copy,VV[485])(1,ecl_symbol_value(VV[0])) /*  GET-ACTAVM-HEADLEXMEAN */;
	V20= ECL_NIL;
	V21= ECL_NIL;
	V22= ECL_NIL;
	if((ecl_function_dispatch(cl_env_copy,VV[486])(2,ecl_symbol_value(VV[4]),VV[10]) /*  MEMQ */)!=ECL_NIL){
	goto L39;}
	if((V2)!=ECL_NIL){
	goto L39;}
	T0= CONS(VV[11],ecl_symbol_value(VV[2]));
	if((ecl_function_dispatch(cl_env_copy,VV[486])(2,V15,T0) /*  MEMQ */)==ECL_NIL){
	goto L37;}
	goto L38;
L39:;
L38:;
	if(!((VV[12])==(V19))){
	goto L44;}
	V4= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[13],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	if(!(V4==ECL_NIL)){
	goto L50;}
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	cl_env_copy->values[0]=ecl_function_dispatch(cl_env_copy,VV[488])(1,T0) /*  SKIP-DETERMINER */;
	{int V23=cl_env_copy->nvalues-0;
	if (V23--<=0) goto L53;
	V4= cl_env_copy->values[0];
	if (V23--<=0) goto L54;
	V17= cl_env_copy->values[1];
	goto L55;}
L53:;
	V4= ECL_NIL;
L54:;
	V17= ECL_NIL;
L55:;
	goto L48;
L50:;
	goto L48;
L48:;
	if(!(V4==ECL_NIL)){
	goto L58;}
	T0= ecl_function_dispatch(cl_env_copy,VV[489])(1,ecl_symbol_value(VV[0])) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	V4= ecl_function_dispatch(cl_env_copy,VV[490])(3,VV[15],VV[16],T0) /*  FIND-ACTAVM-DESCENDANT */;
	goto L56;
L58:;
	goto L56;
L56:;
	if(!(V4==ECL_NIL)){
	goto L62;}
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[18]) /*  EXCEPTION */;
	goto L42;
L62:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V4) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[19])==(T0))){
	goto L65;}
	V4= L4find_information_topic(V4)          /*  FIND-INFORMATION-TOPIC */;
	goto L42;
L65:;
	goto L42;
L44:;
	if(!((VV[20])==(V19))){
	goto L69;}
	V5= ecl_function_dispatch(cl_env_copy,VV[492])(1,ecl_symbol_value(VV[0])) /*  GET-SENTENTIAL-OBJECT */;
	if(!(V5==ECL_NIL)){
	goto L75;}
	T0= ecl_function_dispatch(cl_env_copy,VV[493])(1,ecl_symbol_value(VV[0])) /*  GET-STANDARD-OBJECT */;
	cl_env_copy->values[0]=ecl_function_dispatch(cl_env_copy,VV[488])(1,T0) /*  SKIP-DETERMINER */;
	{int V23=cl_env_copy->nvalues-0;
	if (V23--<=0) goto L79;
	V4= cl_env_copy->values[0];
	if (V23--<=0) goto L80;
	V17= cl_env_copy->values[1];
	goto L81;}
L79:;
	V4= ECL_NIL;
L80:;
	V17= ECL_NIL;
L81:;
	T0= ecl_function_dispatch(cl_env_copy,VV[494])(1,V4) /*  GET-ACTAVM-HEADCATEG */;
	if(!((VV[21])==(T0))){
	goto L84;}
	T0= ecl_function_dispatch(cl_env_copy,VV[495])(1,V4) /*  GET-ACTAVM-HEADPERSON */;
	if(!((ecl_make_fixnum(1))==(T0))){
	goto L84;}
	V4= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[22],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	goto L82;
L84:;
	goto L82;
L82:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V4) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[19])==(T0))){
	goto L89;}
	V4= L4find_information_topic(V4)          /*  FIND-INFORMATION-TOPIC */;
	goto L73;
L89:;
	goto L73;
L75:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V5) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[23])==(T0))){
	goto L93;}
	V4= L3find_know_theme(V5)                 /*  FIND-KNOW-THEME */;
	goto L73;
L93:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V5) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[24])==(T0))){
	goto L97;}
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],V5) /*  FIND-ACTAVM-DEP */;
	cl_env_copy->values[0]=ecl_function_dispatch(cl_env_copy,VV[488])(1,T0) /*  SKIP-DETERMINER */;
	{int V23=cl_env_copy->nvalues-0;
	if (V23--<=0) goto L101;
	V4= cl_env_copy->values[0];
	if (V23--<=0) goto L102;
	V18= cl_env_copy->values[1];
	goto L103;}
L101:;
	V4= ECL_NIL;
L102:;
	V18= ECL_NIL;
L103:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V4) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[19])==(T0))){
	goto L105;}
	V4= L4find_information_topic(V4)          /*  FIND-INFORMATION-TOPIC */;
	goto L73;
L105:;
	goto L73;
L97:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V5) /*  GET-ACTAVM-HEADLEXMEAN */;
	if((ecl_function_dispatch(cl_env_copy,VV[496])(2,T0,VV[25]) /*  ONE-IS-SUBCLASS-OF */)!=ECL_NIL){
	goto L111;}
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V5) /*  GET-ACTAVM-HEADLEXMEAN */;
	if((ecl_function_dispatch(cl_env_copy,VV[496])(2,T0,VV[26]) /*  ONE-IS-SUBCLASS-OF */)==ECL_NIL){
	goto L109;}
	goto L110;
L111:;
L110:;
	V4= V5;
	goto L73;
L109:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V5) /*  GET-ACTAVM-HEADLEXMEAN */;
	ecl_function_dispatch(cl_env_copy,VV[491])(3,VV[17],VV[27],T0) /*  EXCEPTION */;
	goto L73;
L73:;
	if(!(V4==ECL_NIL)){
	goto L115;}
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[28]) /*  EXCEPTION */;
	goto L42;
L115:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V4) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[29])==(T0))){
	goto L118;}
	V9= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],V4) /*  FIND-ACTAVM-DEP */;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V9) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[19])==(T0))){
	goto L123;}
	V4= L4find_information_topic(V9)          /*  FIND-INFORMATION-TOPIC */;
	goto L42;
L123:;
	goto L42;
L118:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V4) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[30])==(T0))){
	goto L127;}
	V4= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[31],V4) /*  FIND-ACTAVM-DEP */;
	goto L42;
L127:;
	goto L42;
L69:;
	if(!((VV[30])==(V19))){
	goto L131;}
	V5= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[31],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	V21= ecl_function_dispatch(cl_env_copy,VV[485])(1,V5) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((V21)==(VV[32]))){
	goto L139;}
	V4= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],V5) /*  FIND-ACTAVM-DEP */;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V4) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((T0)==(VV[30]))){
	goto L144;}
	V4= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[31],V4) /*  FIND-ACTAVM-DEP */;
	goto L137;
L144:;
	goto L137;
L139:;
	if(!((V21)==(VV[33]))){
	goto L148;}
	V9= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],V5) /*  FIND-ACTAVM-DEP */;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V9) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[19])==(T0))){
	goto L153;}
	V4= L4find_information_topic(V9)          /*  FIND-INFORMATION-TOPIC */;
	goto L137;
L153:;
	goto L137;
L148:;
	if(!((V21)==(VV[12]))){
	goto L157;}
	V20= ecl_function_dispatch(cl_env_copy,VV[497])(2,V5,V2) /*  INT-BUILD-SEM-QUERY */;
	goto L137;
L157:;
	if((ecl_function_dispatch(cl_env_copy,VV[496])(2,V21,VV[25]) /*  ONE-IS-SUBCLASS-OF */)!=ECL_NIL){
	goto L163;}
	if((ecl_function_dispatch(cl_env_copy,VV[496])(2,V21,VV[26]) /*  ONE-IS-SUBCLASS-OF */)==ECL_NIL){
	goto L161;}
	goto L162;
L163:;
L162:;
	V4= V5;
	goto L137;
L161:;
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[34]) /*  EXCEPTION */;
	goto L137;
L137:;
	if(!(V4==ECL_NIL)){
	goto L167;}
	if(!(V20==ECL_NIL)){
	goto L167;}
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[35]) /*  EXCEPTION */;
	goto L42;
L167:;
	goto L42;
L131:;
	if(!((V19)==(VV[36]))){
	goto L171;}
	{cl_object V23;
	V23= ecl_make_bool((V15)==(VV[9]));
	if((V23)==ECL_NIL){
	goto L175;}
	goto L42;
L175:;
	{cl_object V24;                           /*  PREP-CASE       */
	T0= ecl_function_dispatch(cl_env_copy,VV[489])(1,ecl_symbol_value(VV[0])) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	V24= ecl_function_dispatch(cl_env_copy,VV[490])(3,VV[15],VV[37],T0) /*  FIND-ACTAVM-DESCENDANT */;
	if(!(V24==ECL_NIL)){
	goto L179;}
	V4= ecl_symbol_value(VV[0]);
	goto L42;
L179:;
	V22= ECL_T;
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[38],V24) /*  FIND-ACTAVM-DEP */;
	V4= ecl_function_dispatch(cl_env_copy,VV[488])(1,T0) /*  SKIP-DETERMINER */;
	goto L42;
	}
	}
L171:;
	if(!((V19)==(VV[39]))){
	goto L186;}
	V4= ecl_symbol_value(VV[0]);
	goto L42;
L186:;
	goto L42;
L42:;
	if(V20==ECL_NIL){
	goto L191;}
	goto L189;
L191:;
	if(!((V19)==(VV[40]))){
	goto L194;}
	V5= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[13],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	if(!(V5==ECL_NIL)){
	goto L199;}
	V5= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	goto L189;
L199:;
	goto L189;
L194:;
	if(!((V19)==(VV[41]))){
	goto L203;}
	V5= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	if(!(V5==ECL_NIL)){
	goto L208;}
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[42]) /*  EXCEPTION */;
	goto L189;
L208:;
	V20= ecl_function_dispatch(cl_env_copy,VV[497])(1,V5) /*  INT-BUILD-SEM-QUERY */;
	goto L189;
L203:;
	if((ecl_function_dispatch(cl_env_copy,VV[486])(2,V19,VV[43]) /*  MEMQ */)==ECL_NIL){
	goto L212;}
	cl_env_copy->values[0]=L5find_topic_subtree(V4) /*  FIND-TOPIC-SUBTREE */;
	{int V23=cl_env_copy->nvalues-0;
	if (V23--<=0) goto L216;
	V6= cl_env_copy->values[0];
	if (V23--<=0) goto L217;
	V7= cl_env_copy->values[1];
	if (V23--<=0) goto L218;
	V10= cl_env_copy->values[2];
	if (V23--<=0) goto L219;
	V12= cl_env_copy->values[3];
	goto L220;}
L216:;
	V6= ECL_NIL;
L217:;
	V7= ECL_NIL;
L218:;
	V10= ECL_NIL;
L219:;
	V12= ECL_NIL;
L220:;
	{cl_object V23;
	cl_object V24;
	V23= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V12))) FEtype_error_list(V12);
	V24= V12;
	{cl_object V25;
	cl_object V26;
	V25= ecl_list1(ECL_NIL);
	V26= V25;
L229:;
	if(!(ecl_endp(V24))){
	goto L233;}
	goto L230;
L233:;
	goto L231;
L231:;
	V23= _ecl_car(V24);
	{cl_object V27;
	V27= _ecl_cdr(V24);
	if (ecl_unlikely(!ECL_LISTP(V27))) FEtype_error_list(V27);
	V24= V27;
	}
	if (ecl_unlikely(ECL_ATOM(V26))) FEtype_error_cons(V26);
	T0= V26;
	T2= ecl_list1(V23);
	T1= cl_list(2,T2,VV[44])                  /*  LIST            */;
	V26= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V26,T0);
	goto L229;
L230:;
	V12= ecl_cdr(V25);
	goto L222;
	}
	}
L222:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V4) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((T0)==(VV[36]))){
	goto L250;}
	T0= L123get_verb_restrictions(2,V4,V7)    /*  GET-VERB-RESTRICTIONS */;
	V12= ecl_append(V12,T0);
	(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[45]) /*  BREAK */;
	goto L248;
L250:;
	goto L248;
L248:;
	if((V19)==(VV[12])){
	goto L257;}
	if(!((V19)==(VV[36]))){
	goto L261;}
	if((V22)!=ECL_NIL){
	goto L257;}
	goto L259;
L261:;
	goto L259;
L259:;
	if(!((V19)==(VV[20]))){
	goto L255;}
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	T1= ecl_function_dispatch(cl_env_copy,VV[485])(1,T0) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[19])==(T1))){
	goto L255;}
	goto L256;
L257:;
L256:;
	T0= L123get_verb_restrictions(2,ecl_symbol_value(VV[0]),V7) /*  GET-VERB-RESTRICTIONS */;
	V12= ecl_append(V12,T0);
	(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[46]) /*  BREAK */;
	goto L189;
L255:;
	goto L189;
L212:;
	if((ecl_function_dispatch(cl_env_copy,VV[496])(2,V19,VV[25]) /*  ONE-IS-SUBCLASS-OF */)==ECL_NIL){
	goto L267;}
	cl_env_copy->values[0]=L5find_topic_subtree(ecl_symbol_value(VV[0])) /*  FIND-TOPIC-SUBTREE */;
	{int V23=cl_env_copy->nvalues-0;
	if (V23--<=0) goto L271;
	V6= cl_env_copy->values[0];
	if (V23--<=0) goto L272;
	V7= cl_env_copy->values[1];
	if (V23--<=0) goto L273;
	V10= cl_env_copy->values[2];
	if (V23--<=0) goto L274;
	V12= cl_env_copy->values[3];
	goto L275;}
L271:;
	V6= ECL_NIL;
L272:;
	V7= ECL_NIL;
L273:;
	V10= ECL_NIL;
L274:;
	V12= ECL_NIL;
L275:;
	{cl_object V23;
	cl_object V24;
	V23= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V12))) FEtype_error_list(V12);
	V24= V12;
	{cl_object V25;
	cl_object V26;
	V25= ecl_list1(ECL_NIL);
	V26= V25;
L283:;
	if(!(ecl_endp(V24))){
	goto L287;}
	goto L284;
L287:;
	goto L285;
L285:;
	V23= _ecl_car(V24);
	{cl_object V27;
	V27= _ecl_cdr(V24);
	if (ecl_unlikely(!ECL_LISTP(V27))) FEtype_error_list(V27);
	V24= V27;
	}
	if (ecl_unlikely(ECL_ATOM(V26))) FEtype_error_cons(V26);
	T0= V26;
	T2= ecl_list1(V23);
	T1= cl_list(2,T2,VV[44])                  /*  LIST            */;
	V26= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V26,T0);
	goto L283;
L284:;
	V12= ecl_cdr(V25);
	goto L276;
	}
	}
L276:;
	goto L189;
L267:;
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[47]) /*  EXCEPTION */;
	goto L189;
L189:;
	if(V20==ECL_NIL){
	goto L303;}
	value0=V20; cl_env_copy->nvalues=1;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L303:;
	if(!((VV[40])==(V19))){
	goto L306;}
	T0= ecl_function_dispatch(cl_env_copy,VV[498])(1,V5) /*  FIND-OBJ-ALL-CONJUNCTS */;
	T1= cl_list(2,T0,VV[44])                  /*  LIST            */;
	T2= ecl_list1(T1);
	T3= L10build_restr_sem(7,VV[44],VV[48],ecl_make_fixnum(0),ECL_NIL,T2,ECL_NIL,ECL_T) /*  BUILD-RESTR-SEM */;
	V13= ecl_car(T3);
	value0=L96final_build_givinfo(VV[48],V13) /*  FINAL-BUILD-GIVINFO */;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L306:;
	if(V4==ECL_NIL){
	goto L311;}
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V4) /*  GET-ACTAVM-HEADLEXMEAN */;
	if((ecl_function_dispatch(cl_env_copy,VV[496])(2,T0,VV[25]) /*  ONE-IS-SUBCLASS-OF */)!=ECL_NIL){
	goto L314;}
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V4) /*  GET-ACTAVM-HEADLEXMEAN */;
	if((ecl_function_dispatch(cl_env_copy,VV[496])(2,T0,VV[26]) /*  ONE-IS-SUBCLASS-OF */)==ECL_NIL){
	goto L311;}
	goto L312;
L314:;
L312:;
	T0= ecl_list1(V4);
	T1= cl_list(2,T0,VV[44])                  /*  LIST            */;
	T2= ecl_list1(T1);
	T3= L10build_restr_sem(5,VV[44],VV[48],ecl_make_fixnum(0),ECL_NIL,T2) /*  BUILD-RESTR-SEM */;
	V13= ecl_car(T3);
	value0=L96final_build_givinfo(VV[48],V13) /*  FINAL-BUILD-GIVINFO */;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L311:;
	if(!(V6==ECL_NIL)){
	goto L319;}
	value0=ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[49]) /*  EXCEPTION */;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L319:;
	V14= ecl_function_dispatch(cl_env_copy,VV[499])(1,V6) /*  GET-ACTAVM-HEADLEXIDENT */;
	if(V14==ECL_NIL){
	goto L325;}
	T0= ecl_list1(V6);
	T1= cl_list(2,T0,VV[44])                  /*  LIST            */;
	V12= CONS(T1,V12);
	goto L323;
L325:;
	goto L323;
L323:;
	T0= ecl_function_dispatch(cl_env_copy,VV[500])(1,V6) /*  GET-ACTAVM-HEADNUMB */;
	T1= L10build_restr_sem(5,VV[44],V10,T0,V6,V12) /*  BUILD-RESTR-SEM */;
	V13= ecl_car(T1);
	{cl_object V23;
	cl_object V24;
	V23= ECL_NIL;
	if (ecl_unlikely(!ECL_LISTP(V10))) FEtype_error_list(V10);
	V24= V10;
	{cl_object V25;
	cl_object V26;
	V25= ecl_list1(ECL_NIL);
	V26= V25;
L338:;
	if(!(ecl_endp(V24))){
	goto L342;}
	goto L339;
L342:;
	goto L340;
L340:;
	V23= _ecl_car(V24);
	{cl_object V27;
	V27= _ecl_cdr(V24);
	if (ecl_unlikely(!ECL_LISTP(V27))) FEtype_error_list(V27);
	V24= V27;
	}
	if (ecl_unlikely(ECL_ATOM(V26))) FEtype_error_cons(V26);
	T0= V26;
	T1= L119get_default_infos(V23)            /*  GET-DEFAULT-INFOS */;
	V26= ecl_list1(T1);
	(ECL_CONS_CDR(T0)=V26,T0);
	goto L338;
L339:;
	V11= ecl_cdr(V25);
	goto L331;
	}
	}
L331:;
	value0=L84final_build_query(V11,V13)      /*  FINAL-BUILD-QUERY */;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L37:;
	if(!((ecl_symbol_value(VV[4]))==(VV[7]))){
	goto L357;}
	if(!((V15)==(VV[50]))){
	goto L357;}
	if(!((VV[12])==(V19))){
	goto L361;}
	V6= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[13],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	if(!(V6==ECL_NIL)){
	goto L367;}
	V8= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	goto L365;
L367:;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V6) /*  GET-ACTAVM-HEADLEXMEAN */;
	if((ecl_function_dispatch(cl_env_copy,VV[486])(2,T0,VV[51]) /*  MEMQ */)==ECL_NIL){
	goto L371;}
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[38],V6) /*  FIND-ACTAVM-DEP */;
	V8= ecl_function_dispatch(cl_env_copy,VV[488])(1,T0) /*  SKIP-DETERMINER */;
	goto L365;
L371:;
	T0= ecl_function_dispatch(cl_env_copy,VV[501])(1,V6) /*  GET-ACTAVM-HEADLEMMA */;
	ecl_function_dispatch(cl_env_copy,VV[491])(3,VV[17],VV[52],T0) /*  EXCEPTION */;
	goto L365;
L365:;
	V14= ecl_function_dispatch(cl_env_copy,VV[499])(1,V8) /*  GET-ACTAVM-HEADLEXIDENT */;
	T0= L123get_verb_restrictions(2,ecl_symbol_value(VV[0]),V8) /*  GET-VERB-RESTRICTIONS */;
	V12= CONS(V8,T0);
	(cl_env_copy->function=(ECL_SYM("BREAK",158)->symbol.gfdef))->cfun.entry(1,VV[53]) /*  BREAK */;
	T0= L10build_restr_sem(5,VV[44],VV[54],ecl_make_fixnum(0),ECL_NIL,V12) /*  BUILD-RESTR-SEM */;
	V13= ecl_car(T0);
	T0= ecl_list1(VV[48]);
	value0=L96final_build_givinfo(T0,V13)     /*  FINAL-BUILD-GIVINFO */;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L361:;
	if(!((VV[41])==(V19))){
	goto L382;}
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],ecl_symbol_value(VV[0])) /*  FIND-ACTAVM-DEP */;
	V5= ecl_function_dispatch(cl_env_copy,VV[488])(1,T0) /*  SKIP-DETERMINER */;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V5) /*  GET-ACTAVM-HEADLEXMEAN */;
	if((ecl_function_dispatch(cl_env_copy,VV[502])(2,T0,VV[55]) /*  IS-SUBCLASS-OF */)==ECL_NIL){
	goto L388;}
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[56],V5) /*  FIND-ACTAVM-DEP */;
	V4= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[38],T0) /*  FIND-ACTAVM-DEP */;
	goto L386;
L388:;
	V4= V5;
	goto L386;
L386:;
	if(!(V4==ECL_NIL)){
	goto L394;}
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[57]) /*  EXCEPTION */;
	goto L392;
L394:;
	goto L392;
L392:;
	T0= ecl_list1(V4);
	V12= ecl_list1(T0);
	T0= L10build_restr_sem(5,VV[44],VV[48],ecl_make_fixnum(0),ECL_NIL,V12) /*  BUILD-RESTR-SEM */;
	V13= ecl_car(T0);
	value0=L96final_build_givinfo(VV[48],V13) /*  FINAL-BUILD-GIVINFO */;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L382:;
	value0=ecl_function_dispatch(cl_env_copy,VV[497])(2,ecl_symbol_value(VV[0]),ECL_T) /*  INT-BUILD-SEM-QUERY */;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L357:;
	if(!((ecl_symbol_value(VV[4]))==(VV[7]))){
	goto L401;}
	if(!((V15)==(VV[58]))){
	goto L401;}
	if(!((VV[40])==(V19))){
	goto L405;}
	T0= ecl_function_dispatch(cl_env_copy,VV[489])(1,ecl_symbol_value(VV[0])) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	V5= ecl_function_dispatch(cl_env_copy,VV[490])(3,VV[59],VV[60],T0) /*  FIND-ACTAVM-DESCENDANT */;
	if(!(V5==ECL_NIL)){
	goto L411;}
	T0= ecl_function_dispatch(cl_env_copy,VV[489])(1,ecl_symbol_value(VV[0])) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	V5= ecl_function_dispatch(cl_env_copy,VV[490])(3,VV[59],VV[61],T0) /*  FIND-ACTAVM-DESCENDANT */;
	goto L409;
L411:;
	goto L409;
L409:;
	V14= ecl_function_dispatch(cl_env_copy,VV[499])(1,V5) /*  GET-ACTAVM-HEADLEXIDENT */;
	if(V14==ECL_NIL){
	goto L418;}
	T0= ecl_list1(V5);
	V12= ecl_list1(T0);
	goto L416;
L418:;
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[62]) /*  EXCEPTION */;
	goto L416;
L416:;
	T0= L10build_restr_sem(5,VV[44],VV[48],ecl_make_fixnum(0),ECL_NIL,V12) /*  BUILD-RESTR-SEM */;
	V13= ecl_car(T0);
	if(!(ECL_LISTP(V13))){
	goto L425;}
	{cl_fixnum V23;
	V23= ecl_length(V13);
	if(!((1)==(V23))){
	goto L425;}}
	V13= ecl_car(V13);
	goto L423;
L425:;
	goto L423;
L423:;
	value0=L96final_build_givinfo(VV[63],V13) /*  FINAL-BUILD-GIVINFO */;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L405:;
	value0=ecl_function_dispatch(cl_env_copy,VV[497])(2,ecl_symbol_value(VV[0]),ECL_T) /*  INT-BUILD-SEM-QUERY */;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
L401:;
	value0=ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[64]) /*  EXCEPTION */;
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	ecl_bds_unwind1(cl_env_copy);
	return value0;
	}
	}
}}
/*	function definition for APPLY-TOPIC-CHANGES                   */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L2apply_topic_changes(cl_object V1, cl_object V2)
{ VT3 VLEX3 CLSR3 STCK3
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V3;
	V3= V2;
	goto L5;
L4:;
	{cl_object V4;                            /*  NXTCHANGE       */
	V4= ecl_car(V3);
	T0= ecl_cadr(V4);
	T1= ecl_car(V4);
	V1= cl_substitute(3,T0,T1,V1)             /*  SUBSTITUTE      */;
	}
	V3= ecl_cdr(V3);
L5:;
	if((V3)==ECL_NIL){
	goto L15;}
	goto L4;
L15:;
	goto L13;
L13:;
	goto L2;
L2:;
	value0=V1; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for FIND-KNOW-THEME                       */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L3find_know_theme(cl_object V1)
{ VT4 VLEX4 CLSR4 STCK4
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  THEME           */
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],V1) /*  FIND-ACTAVM-DEP */;
	V2= ecl_function_dispatch(cl_env_copy,VV[484])(1,T0) /*  SKIP-QUESTION-TENSE-MARKER */;
	if(!(V2==ECL_NIL)){
	goto L4;}
	T0= ecl_function_dispatch(cl_env_copy,VV[489])(1,V1) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	V2= ecl_function_dispatch(cl_env_copy,VV[490])(3,VV[59],VV[67],T0) /*  FIND-ACTAVM-DESCENDANT */;
	if(V2==ECL_NIL){
	goto L9;}
	T0= ecl_function_dispatch(cl_env_copy,VV[489])(1,V2) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	T1= ecl_function_dispatch(cl_env_copy,VV[490])(3,VV[59],VV[68],T0) /*  FIND-ACTAVM-DESCENDANT */;
	V2= ecl_function_dispatch(cl_env_copy,VV[484])(1,T1) /*  SKIP-QUESTION-TENSE-MARKER */;
	goto L2;
L9:;
	goto L2;
L4:;
	goto L2;
L2:;
	value0=V2; cl_env_copy->nvalues=1;
	return value0;
	}
}}
/*	function definition for FIND-INFORMATION-TOPIC                */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L4find_information_topic(cl_object V1)
{ VT5 VLEX5 CLSR5 STCK5
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  LET3494         */
	cl_object V3;                             /*  INFO-TOPIC      */
	V2= ecl_function_dispatch(cl_env_copy,VV[489])(1,V1) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	V3= ECL_NIL;
	{cl_object V4;                            /*  LET3495         */
	cl_object V5;                             /*  LET3496         */
	cl_object V6;                             /*  NXTDEP          */
	cl_object V7;                             /*  INFO-DEPENDENTS */
	V4= ecl_car(V2);
	V5= ecl_cdr(V2);
	V6= V4;
	V7= V5;
	goto L10;
L9:;
	if(!(ecl_equal(V6,VV[70]))){
	goto L14;}
	goto L12;
L14:;
	T0= ecl_function_dispatch(cl_env_copy,VV[506])(1,V6) /*  GET-ACTAVM-HEADLINK */;
	if(!((T0)==(VV[56]))){
	goto L17;}
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V6) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((T0)==(VV[71]))){
	goto L17;}
	V3= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[38],V6) /*  FIND-ACTAVM-DEP */;
	goto L12;
L17:;
	T0= ecl_function_dispatch(cl_env_copy,VV[506])(1,V6) /*  GET-ACTAVM-HEADLINK */;
	if(!((T0)==(VV[72]))){
	goto L22;}
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V6) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((T0)==(VV[73]))){
	goto L22;}
	V3= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[74],V6) /*  FIND-ACTAVM-DEP */;
	goto L12;
L22:;
	goto L12;
L12:;
	V6= ecl_car(V7);
	V7= ecl_cdr(V7);
L10:;
	if(V6==ECL_NIL){
	goto L31;}
	if(!(V3==ECL_NIL)){
	goto L31;}
	goto L9;
L31:;
	goto L29;
L29:;
	goto L7;
L7:;
	value0=V3; cl_env_copy->nvalues=1;
	return value0;
	}
	}
}}
/*	function definition for FIND-TOPIC-SUBTREE                    */
/*	optimize speed 3, debug 0, space 0, safety 2                  */
static cl_object L5find_topic_subtree(cl_object V1)
{ VT6 VLEX6 CLSR6 STCK6
	const cl_env_ptr cl_env_copy = ecl_process_env();
	cl_object value0;
	ecl_cs_check(cl_env_copy,value0);
	{
TTL:
	{cl_object V2;                            /*  LET3497         */
	cl_object V3;                             /*  LET3498         */
	cl_object V4;                             /*  LET3499         */
	cl_object V5;                             /*  TOPIC-SUBTREE   */
	cl_object V6;                             /*  RESTRICTIONS    */
	cl_object V7;                             /*  ACT-TOPIC-SUBTREE */
	cl_object V8;                             /*  TEMP-SUBTREE    */
	cl_object V9;                             /*  TEMP-DEPS       */
	cl_object V10;                            /*  TOPICS          */
	cl_object V11;                            /*  PREP-SUBTREE    */
	cl_object V12;                            /*  PREP-MEAN       */
	cl_object V13;                            /*  OTHER-TOPIC-SUBTREE */
	cl_object V14;                            /*  TOPIC-CATEG     */
	V2= ecl_function_dispatch(cl_env_copy,VV[485])(1,V1) /*  GET-ACTAVM-HEADLEXMEAN */;
	V3= ecl_function_dispatch(cl_env_copy,VV[494])(1,V1) /*  GET-ACTAVM-HEADCATEG */;
	V4= ecl_function_dispatch(cl_env_copy,VV[489])(1,V1) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	V5= ECL_NIL;
	V6= ECL_NIL;
	V7= ECL_NIL;
	V8= ECL_NIL;
	V9= ECL_NIL;
	V10= ECL_NIL;
	V11= ECL_NIL;
	V12= ECL_NIL;
	V13= ECL_NIL;
	V14= ECL_NIL;
	if(!((VV[77])==(V3))){
	goto L15;}
	if(!((VV[30])==(V2))){
	goto L15;}
	V1= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[31],V1) /*  FIND-ACTAVM-DEP */;
	goto TTL;
L15:;
	if(!((VV[77])==(V3))){
	goto L21;}
	if(!((VV[36])==(V2))){
	goto L21;}
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[79],V1) /*  FIND-ACTAVM-DEP */;
	T1= ecl_function_dispatch(cl_env_copy,VV[485])(1,T0) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[78])==(T1))){
	goto L21;}
	V1= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[22],V1) /*  FIND-ACTAVM-DEP */;
	goto TTL;
L21:;
	if(!((VV[77])==(V3))){
	goto L28;}
	if(!((VV[80])==(V2))){
	goto L28;}
	V1= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],V1) /*  FIND-ACTAVM-DEP */;
	goto TTL;
L28:;
	if(!((VV[81])==(V3))){
	goto L34;}
	if((ecl_function_dispatch(cl_env_copy,VV[486])(2,V2,VV[82]) /*  MEMQ */)==ECL_NIL){
	goto L34;}
	T0= ecl_function_dispatch(cl_env_copy,VV[508])(1,V1) /*  GET-PREPOSITION-ARG */;
	V5= ecl_function_dispatch(cl_env_copy,VV[488])(1,T0) /*  SKIP-DETERMINER */;
	value0=V5; cl_env_copy->nvalues=1;
	return value0;
L34:;
	if(!((VV[83])==(V3))){
	goto L39;}
	if(!((VV[84])==(V2))){
	goto L39;}
	V5= V1;
	value0=V5; cl_env_copy->nvalues=1;
	return value0;
L39:;
	V5= L6find_interr_adv(V4)                 /*  FIND-INTERR-ADV */;
	if(V5==ECL_NIL){
	goto L47;}
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V5) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[85])==(T0))){
	goto L47;}
	V5= V1;
	goto L45;
L47:;
	goto L45;
L45:;
	if(!(V5==ECL_NIL)){
	goto L53;}
	V5= L7find_interr_pron(V4)                /*  FIND-INTERR-PRON */;
	goto L51;
L53:;
	goto L51;
L51:;
	if(V5==ECL_NIL){
	goto L58;}
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V5) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((VV[86])==(T0))){
	goto L58;}
	if(!((VV[36])==(V2))){
	goto L58;}
	V13= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[22],V1) /*  FIND-ACTAVM-DEP */;
	if(V13==ECL_NIL){
	goto L67;}
	if(!(ecl_equal(V5,V13))){
	goto L65;}
	goto L66;
L67:;
L66:;
	V5= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[79],V1) /*  FIND-ACTAVM-DEP */;
	goto L56;
L65:;
	V5= V13;
	goto L56;
L58:;
	goto L56;
L56:;
	if(!(V5==ECL_NIL)){
	goto L73;}
	V5= L9find_interr_adj_adv(V4)             /*  FIND-INTERR-ADJ+ADV */;
	goto L71;
L73:;
	goto L71;
L71:;
	if(!(V5==ECL_NIL)){
	goto L78;}
	V5= L8find_interr_adjec(V4)               /*  FIND-INTERR-ADJEC */;
	goto L76;
L78:;
	goto L76;
L76:;
	if(!(V5==ECL_NIL)){
	goto L83;}
	if((ecl_function_dispatch(cl_env_copy,VV[486])(2,V3,VV[87]) /*  MEMQ */)==ECL_NIL){
	goto L86;}
	V5= V1;
	goto L81;
L86:;
	if((ecl_function_dispatch(cl_env_copy,VV[486])(2,ecl_symbol_value(VV[76]),VV[88]) /*  MEMQ */)==ECL_NIL){
	goto L95;}
	if(!((V2)==(VV[36]))){
	goto L95;}
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[89],V1) /*  FIND-ACTAVM-DEP */;
	if(!(T0==ECL_NIL)){
	goto L92;}
	goto L93;
L95:;
	goto L93;
L93:;
	if(!((ecl_symbol_value(VV[76]))==(VV[90]))){
	goto L100;}
	if((ecl_function_dispatch(cl_env_copy,VV[486])(2,V2,VV[91]) /*  MEMQ */)==ECL_NIL){
	goto L100;}
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[89],V1) /*  FIND-ACTAVM-DEP */;
	if(!(T0==ECL_NIL)){
	goto L92;}
	goto L98;
L100:;
	goto L98;
L98:;
	if(!((ecl_symbol_value(VV[76]))==(VV[92]))){
	goto L90;}
	if((ecl_function_dispatch(cl_env_copy,VV[486])(2,V2,VV[91]) /*  MEMQ */)==ECL_NIL){
	goto L90;}
	goto L91;
L92:;
L91:;
	V5= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[22],V1) /*  FIND-ACTAVM-DEP */;
	goto L81;
L90:;
	if(!((VV[32])==(V2))){
	goto L106;}
	{cl_object V15;
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[22],V1) /*  FIND-ACTAVM-DEP */;
	V15= ecl_function_dispatch(cl_env_copy,VV[485])(1,T0) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((V15)==(VV[93]))){
	goto L113;}
	goto L110;
	goto L111;
L113:;
	goto L111;
L111:;
	if(!((V15)==(ECL_NIL))){
	goto L106;}
	goto L107;
L110:;
	}
L107:;
	V5= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],V1) /*  FIND-ACTAVM-DEP */;
	goto L81;
L106:;
	if(!((VV[23])==(V2))){
	goto L118;}
	{cl_object V15;
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[22],V1) /*  FIND-ACTAVM-DEP */;
	V15= ecl_function_dispatch(cl_env_copy,VV[485])(1,T0) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((V15)==(VV[93]))){
	goto L125;}
	goto L122;
	goto L123;
L125:;
	goto L123;
L123:;
	if(!((V15)==(ECL_NIL))){
	goto L118;}
	goto L119;
L122:;
	}
L119:;
	V5= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],V1) /*  FIND-ACTAVM-DEP */;
	goto L81;
L118:;
	if(!((VV[96])==(V2))){
	goto L130;}
	{cl_object V15;
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[22],V1) /*  FIND-ACTAVM-DEP */;
	V15= ecl_function_dispatch(cl_env_copy,VV[485])(1,T0) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((V15)==(VV[93]))){
	goto L137;}
	goto L134;
	goto L135;
L137:;
	goto L135;
L135:;
	if(!((V15)==(ECL_NIL))){
	goto L130;}
	goto L131;
L134:;
	}
L131:;
	T0= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[14],V1) /*  FIND-ACTAVM-DEP */;
	V8= ecl_function_dispatch(cl_env_copy,VV[488])(1,T0) /*  SKIP-DETERMINER */;
	T0= ecl_function_dispatch(cl_env_copy,VV[485])(1,V8) /*  GET-ACTAVM-HEADLEXMEAN */;
	if(!((T0)==(VV[19]))){
	goto L143;}
	T0= ecl_function_dispatch(cl_env_copy,VV[489])(1,V8) /*  GET-ACTAVM-DEPS-WITH-TRACES */;
	V9= cl_remove(2,VV[70],T0)                /*  REMOVE          */;
	if(!(V9==ECL_NIL)){
	goto L148;}
	V8= ecl_function_dispatch(cl_env_copy,VV[487])(2,VV[97],V1) /*  FIND-ACTAVM-DEP */;
	goto L81;
L148:;
	V11= L4find_information_topic(V8)         /*  FIND-INFORMATION-TOPIC */;
	if(V11==ECL_NIL){
	goto L154;}
	V12= ecl_function_dispatch(cl_env_copy,VV[485])(1,V11) /*  GET-ACTAVM-HEADLEXMEAN */;
	if((ecl_function_dispatch(cl_env_copy,VV[486])(2,V12,VV[98]) /*  MEMQ */)==ECL_NIL){
	goto L159;}
	V5= V11;
	goto L81;
L159:;
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[99]) /*  EXCEPTION */;
	goto L81;
L154:;
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[100]) /*  EXCEPTION */;
	goto L81;
L143:;
	ecl_function_dispatch(cl_env_copy,VV[491])(2,VV[17],VV[101]) /*  EXCEPTION */;
	goto L81;
L130:;
	goto L81;
L83:;
	goto L81;
L81:;
	V14= ecl_function_dispatch(cl_env_copy,VV[494])(1,V5) /*  GET-ACTAVM-HEADCATEG */;
	if(!((V14)==(VV[81]))){
	goto L166;}
	T0= ecl_function_dispatch(cl_env_copy,VV[508])(1,V5) /*  GET-PREPOSITION-ARG */;
	V7= ecl_function_dispatch(cl_env_copy,VV[488])(1,T0) /*  SKIP-DETERMINER */;
	cl_env_copy->values[0]=