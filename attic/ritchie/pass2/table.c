#include "c1.h"
#define rjump &optab[0]

static char L1[]="jp\301\n";
static char L2[]="GBjp\250hl)\n";
#define rcall &optab[3]

static char L3[]="call\315A\n";
static char L4[]="GBcall\352phl\n";
static char L5[]="GAcall\352phl\n";
#define rname &optab[7]

static char L6[]="ld\311,0\n";
static char L7[]="call\346clr\n";
static char L8[]="ld\341,(A)\nld\354,a\nld\350,0\n";
static char L9[]="Q\n";
static char L10[]="call\346ldA\n";
static char L11[]="GBld\341,(hl)\nld\354,a\nld\350,0\n";
static char L12[]="GBld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\n";
static char L13[]="GBcall\346ldi\n";
static char L14[]="\5\nld\345,(hl)\ninc\350l\nld\344,(hl)\ninc\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\nex\344e,hl\n";
static char L15[]="GBcall\354ld\n";
static char L16[]="GA";
#define rpostinc &optab[23]

static char L17[]="ld\311,(A)\nM'\250A)\n";
static char L18[]="ld\311,(A)\nld\344e,B\npush\350l\nQ\nM\n\3\npop\350l\n";
static char L19[]="ld\341,(A)\nld\354,a\nld\350,0\npush\350l\nM'\341\nld\250A),a\npop\350l\n";
static char L20[]="GJpush\344e\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\250sp),hl\nld\341,(hl)\nM'\341\nld\250hl),a\ninc\350l\nld\341,(hl)\nV\341\nld\250hl),a\npop\350l\n";
static char L21[]="GBpush\350l\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\250sp),hl\nld\341,(hl)\nM'\341\nld\250hl),a\ninc\350l\nld\341,(hl)\nV\341\nld\250hl),a\nex\344e,hl\n";
static char L22[]="GJpush\344e\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nld\344e,B\nex\250sp),hl\npush\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\nM\nex\344e,hl\npop\350l\nld\250hl),e\ninc\350l\nld\250hl),d\npop\350l\n";
static char L23[]="GJpush\344e\nld\341,(de)\nld\354,a\nld\350,0\nex\250sp),hl\nld\341,(hl)\nM'\341\nld\250hl),a\npop\350l\n";
static char L24[]="GBpush\350l\nld\345,(hl)\ninc\350l\nld\344,(hl)\npush\344e\nld\344e,B\nld\341,(hl)\ndec\350l\nld\354,(hl)\nld\350,a\nM\nex\344e,hl\npop\350l\nld\250hl),e\ninc\350l\nld\250hl),d\npop\350l\n";
static char L25[]="GBpush\350l\nld\341,(hl)\npush\341f\nM'\341\nld\250hl),a\npop\341f\nld\354,a\nld\350,0\npop\344e\n";
static char L26[]="GAQ\ninc\350l\ninc\350l\nld\341,(hl)\nM'\341\nld\250hl),a\ninc\350l\nld\341,(hl)\nV\341\nld\250hl),a\ndec\350l\nld\341,(hl)\ndec\350l\nld\350,(hl)\nld\354,a\n";
static char L27[]="GJpush\344e\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\ninc\344e\nld\341,(de)\ninc\344e\npush\341f\nld\341,(de)\npop\342c\nld\343,a\npush\342c\npush\350l\npop\344e\npop\350l\nex\250sp),hl\ninc\350l\ninc\350l\nld\341,(hl)\nM'\341\nld\250hl),a\ninc\350l\nld\341,(hl)\nV\341\nld\250hl),a\npop\350l\n";
static char L28[]="GBpush\350l\ncall\354ld\npush\350l\npush\344e\nex\250sp),hl\ninc\350l\ninc\350l\nld\341,(hl)\nM'\341\nld\250hl),a\ninc\350l\nld\341,(hl)\nV\341\nld\250hl),a\npop\344e\npop\350l\n";
#define runary &optab[39]

static char L29[]="GAM\n";
static char L30[]="GAcall\315\n";
#define rassign &optab[44]

static char L31[]="KAld\250A),I\n";
static char L32[]="KAld\341,l\nld\250A),a\n";
static char L33[]="KAcall\346stA\n";
static char L34[]="GBKAcall\346sti\n";
static char L35[]="GBld\344e,B\nld\250hl),e\ninc\350l\nld\250hl),d\nex\344e,hl\n";
static char L36[]="GBKAcall\346sti\n";
static char L37[]="GBKIld\250hl),e\ninc\350l\nld\250hl),d\nex\344e,hl\n";
static char L38[]="GBKIld\341,e\nld\250hl),a\nld\354,a\nld\350,0\n";
static char L39[]="KAGJcall\346stde\n";
static char L40[]="KAGJcall\346stde\n";
static char L41[]="GDKApop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
static char L42[]="GDKApop\344e\nld\341,l\nld\250de),a\n";
static char L43[]="GDKApop\344e\ncall\346stde\n";
static char L44[]="KA\3\nQ+2\nld\250hl),e\ninc\350l\nld\250hl),d\n";
static char L45[]="KAGJex\344e,hl\nld\250hl),e\ninc\350l\nld\250hl),d\ninc\350l\npop\344e\nld\250hl),e\ninc\350l\nld\250hl),d\npop\350l\n";
static char L46[]="GDKApop\342c\nld\250bc),l\ninc\342c\nld\341,h\nld\250bc),a\ninc\342c\nld\341,e\nld\250bc),a\ninc\342c\nld\341,d\nld\250bc),a\n";
#define rfield &optab[72]

static char L47[]="KAld\341,(A)\nand\332\nor\354\nld\250A),a\n";
#define L48 fas1

static char L49[]="KCGBld\341,(hl)\npop\344e\nand\332\nor\345\nld\250hl),a\nld\354,e\nld\350,0\n";
#define radd &optab[76]

static char L50[]="GA";
static char L51[]="GAM'\n";
#define add1 L52

static char L52[]="GAld\344e,B\nM\n";
#define add2 L53

static char L53[]="GAKJpush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\nM\n";
#define add3 L54

static char L54[]="GAKIM\n";
#define add4 L55

static char L55[]="KDGApop\344e\npush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\nM\n";
#define add5 L56

static char L56[]="KCGApop\344e\nM\n";
static char L57[]="GAld\342c,B\nadd\350l,bc\nex\344e,hl\nld\350l,0\nV\nex\344e,hl\n";
static char L58[]="GAKIadd\350l,de\nex\344e,hl\nld\350l,0\nV\nex\344e,hl\n";
static char L59[]="GApush\344e\nw\nadd\350l,de\npop\344e\npush\350l\nld\350l,(B+2)\nex\344e,hl\nV\nex\344e,hl\npop\350l\n";
#define addl1 L60

static char L60[]="GAKIM\n";
#define addl2 L61

static char L61[]="KCGAcall\315\n";
#define rxor &optab[107]

#define L62 add3

static char L63[]="GCKApop\344e\ncall\370or16\n";
#define L64 addl1

static char L65[]="KCGAcall\354xor\n";
#define rrsh &optab[118]

static char L66[]="GAsra\350\nrr\354\n";
#define rmul &optab[120]

#define L67 add1

#define L68 add2

#define L69 add3

#define L70 add5

#define rdiv &optab[129]

static char L71[]="GAld\344e,B\ncall\315\n";
static char L72[]="GAKJpush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\ncall\315\n";
static char L73[]="GAKIcall\315\n";
static char L74[]="KCGApop\344e\ncall\315\n";
#define L75 add1

#define L76 add2

#define L77 add3

#define L78 add5

#define rptoi &optab[138]

static char L79[]="GA!ld\344e,B\ncall\344iv16\n";
#define rasadd &optab[141]

#define addq1 L80

static char L80[]="Q\nld\344e,B\nM\n\3\n";
#define addq20 L81

static char L81[]="ld\341,(A)\nld\354,a\nld\350,0\nld\344e,B\nM\nld\341,l\nld\250A),a\n";
#define addq1a L82

static char L82[]="ld\311,(A)\nld\344e,B\nM\nld\250A),I\n";
#define addq2 L83

static char L83[]="KBpush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\nQ\nM\n\3\npop\344e\n";
#define addq3 L84

static char L84[]="KAex\344e,hl\nQ\nM\n\3\n";
#define addq21 L85

static char L85[]="KCld\341,(A)\nld\354,a\nld\350,0\npop\344e\nM\nld\341,l\nld\250A),a\n";
#define addq4 L86

static char L86[]="KBGJpush\344e\npush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\npop\344e\npush\344e\npush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\nM\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\npop\344e\n";
#define addq4a L87

static char L87[]="call\346ldA\nKIM\ncall\346stA\n";
#define addq5 L88

static char L88[]="KCld\311,(A)\npop\344e\nM\nld\250A),I\n";
#define addq6 L89

static char L89[]="KCcall\346ldA\npop\344e\nM\ncall\346stA\n";
#define addq7 L90

static char L90[]="KAGJpush\350l\npush\344e\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\nM\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
#define addq8 L91

static char L91[]="KCGBpush\350l\npop\344e\npush\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\npop\342c\nex\250sp),hl\nex\344e,hl\nM\npush\342c\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
#define addq9 L92

static char L92[]="GDKCpop\350l\npush\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\npop\344e\nex\250sp),hl\nex\344e,hl\nM\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
#define addq22 L93

static char L93[]="GDKCpop\350l\nld\341,(hl)\npush\350l\nld\354,a\nld\350,0\npop\344e\nex\250sp),hl\nex\344e,hl\nM\npop\344e\nld\341,l\nld\250de),a\n";
#define addq9a L94

static char L94[]="KCGBpush\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\npop\344e\nex\250sp),hl\nex\344e,hl\nM\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
#define addq10 L95

static char L95[]="KCGBpush\350l\ncall\346ldi\npop\344e\nex\250sp),hl\nex\344e,hl\nM\npop\344e\ncall\346stde\n";
#define addq11 L96

static char L96[]="ld\350l,(A+2)\nld\344e,B\nM\nld\250A+2),hl\nV\250A)\nQ\nld\344e,(A+2)\n";
#define addq12 L97

static char L97[]="ld\350l,(A+2)\nld\344e,(B+2)\nM\nld\250A+2),hl\nQ\nw\nV\n\3\nld\344e,(A+2)\n";
#define addq13 L98

static char L98[]="KApush\350l\npush\344e\nld\350l,(A+2)\nex\250sp),hl\nM\nld\250A+2),hl\npop\350l\nq\nV\n\3\nld\344e,(A+2)\n";
#define addq14 L99

static char L99[]="GBpush\350l\ninc\350l\ninc\350l\nld\344e,B\npush\344e\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\npop\344e\nM\npop\344e\npush\350l\ninc\344e\ninc\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\ndec\344e\ndec\344e\ndec\344e\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nV\ndec\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\npop\344e\n";
#define addq15 L100

static char L100[]="GBpush\350l\ninc\350l\ninc\350l\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nld\344e,(B+2)\nM\npop\342c\npush\350l\npush\342c\ninc\342c\ninc\342c\nld\250bc),l\ninc\342c\nld\341,h\nld\250bc),a\npop\350l\npush\350l\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nw\nV\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\npop\344e\n";
#define addq16 L101

static char L101[]="KCGBpush\350l\ncall\354ld\ncall\354add\npop\344e\ncall\354stde\n";
#define rasmul &optab[180]

#define L102 addq1a

#define L103 addq4a

#define L104 addq5

#define L105 addq6

#define L106 addq20

#define L107 addq21

#define L108 addq9

#define L109 addq22

#define L110 addq9a

#define L111 addq10

#define rasdiv &optab[193]

static char L112[]="ld\311,(A)\nld\344e,B\ncall\315\nld\250A),I\n";
static char L113[]="KCld\311,(A)\npop\344e\ncall\315\nld\250A),I\n";
static char L114[]="KCld\341,(A)\nld\354,a\nld\350,0\npop\344e\ncall\315\nld\341,l\nld\250A),a\n";
static char L115[]="KCGJpush\344e\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\npop\344e\nex\250sp),hl\nex\344e,hl\ncall\315\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
static char L116[]="GDKCpop\350l\npush\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\npop\344e\nex\250sp),hl\nex\344e,hl\ncall\315\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
static char L117[]="GDKCpop\350l\nld\341,(hl)\npush\350l\nld\354,a\nld\350,0\npop\344e\nex\250sp),hl\nex\344e,hl\ncall\315\npop\344e\nld\341,l\nld\250de),a\n";
#define L118 addq1a

#define L119 addq4a

#define L120 addq5

#define L121 addq6

#define L122 addq9a

#define L123 addq10

#define rasmod &optab[206]

static char L124[]="ld\311,(A)\nld\344e,B\ncall\315\nld\250A),I\n";
static char L125[]="KCld\311,(A)\npop\344e\ncall\315\nld\250A),I\n";
static char L126[]="KCld\341,(A)\nld\354,a\nld\350,0\npop\344e\ncall\315\nld\341,l\nld\250A),a\n";
static char L127[]="KCGJpush\344e\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\npop\344e\nex\250sp),hl\nex\344e,hl\ncall\315\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
static char L128[]="GDKCpop\350l\npush\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\npop\344e\nex\250sp),hl\nex\344e,hl\ncall\315\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
static char L129[]="GDKCpop\350l\nld\341,(hl)\npush\350l\nld\354,a\nld\350,0\npop\344e\nex\250sp),hl\nex\344e,hl\ncall\315\npop\344e\nld\341,l\nld\250de),a\n";
#define rasxor &optab[213]

#define L130 addq3

static char L131[]="KCld\341,(A)\nld\354,a\nld\350,0\npop\344e\ncall\370or16\nld\341,l\nld\250A),a\n";
static char L132[]="KCld\341,(A)\nld\354,a\nld\350,0\npop\344e\ncall\370or16\nld\341,l\nld\250A),a\n";
static char L133[]="GDpop\350l\npush\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\nKApop\344e\ncall\370or16\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
static char L134[]="GDpop\350l\nld\341,(hl)\npush\350l\nld\354,a\nld\350,0\nKApop\344e\ncall\370or16\npop\344e\nld\341,l\nld\250de),a\n";
#define rasrsh &optab[219]

static char L135[]="Q\nsra\350\nrr\354\n\3\n";
static char L136[]="GBld\345,(hl)\ninc\350l\nld\344,(hl)\nsra\344\nrr\345\nld\250hl),d\ndec\350l\nld\250hl),e\nex\344e,hl\n";
#define rasor &optab[222]

#define L137 addq1

static char L138[]="ld\341,(A)\nM\302\nld\250A),a\nld\354,a\nld\350,0\n";
#define L139 addq1a

#define L140 addq2

#define L141 addq3

static char L142[]="KCld\341,(A)\nld\354,a\nld\350,0\npop\344e\nM\nld\341,l\nld\250A),a\n";
#define L143 addq4

#define L144 addq4a

#define L145 addq5

#define L146 addq6

#define L147 addq7

#define L148 addq8

#define L149 addq9

static char L150[]="GDKCpop\350l\nld\341,(hl)\npush\350l\npop\344e\nex\250sp),hl\nM\341\npop\350l\nld\250hl),a\nld\354,a\nld\350,0\n";
#define L151 addq9a

#define L152 addq10

#define L153 addq11

#define L154 addq12

#define L155 addq13

#define L156 addq14

#define L157 addq15

#define L158 addq16

#define rlshl &optab[261]

#define L159 add1

#define L160 add2

#define L161 add3

#define L162 add4

#define L163 add5

#define rlushr &optab[272]

static char L164[]="KCGApop\344e\nld\341,e\ncall\354ushr\n";
#define ralushr &optab[275]

static char L165[]="KCGCpop\344e\nld\341,e\ncall\354ushr\npop\344e\n";
#define ritof &optab[277]

static char L166[]="Q\ncall\351tof\n";
static char L167[]="GBld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\ncall\351tof\n";
static char L168[]="GAcall\351tof\n";
#define rftoi &optab[281]

static char L169[]="GAcall\346toi\n";
#define rftol &optab[283]

static char L170[]="GAcall\346tol\n";
#define rltof &optab[285]

static char L171[]="Q\nld\344e,(A+2)\ncall\354tof\n";
static char L172[]="GBcall\354ld\ncall\354tof\n";
static char L173[]="GCcall\354tof\n";
#define rultof &optab[289]

static char L174[]="Q\nld\344e,(A+2)\ncall\365ltof\n";
static char L175[]="GBcall\354ld\ncall\365ltof\n";
static char L176[]="GCcall\365ltof\n";
#define ritol &optab[293]

static char L177[]="GI!ld\344e,0\n";
static char L178[]="GAex\344e,hl\nld\350l,0\n";
static char L179[]="GI!ld\341,d\nrla\nsbc\341,a\nld\344,a\nld\345,a\n";
static char L180[]="GAex\344e,hl\nld\341,d\nrla\nsbc\341,a\nld\350,a\nld\354,a\n";
#define rltoi &optab[298]

static char L181[]="Q\n";
static char L182[]="GBld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\n";
#define rlmul &optab[303]

#define l82 L183

static char L183[]="KCGCcall\315\n";
#define rulmul &optab[308]

#define L184 l82

#define rulasmul &optab[312]

#define L185 l86

#define rlasmul &optab[317]

#define l86 L186

static char L186[]="KCGCcall\315\n";
#define ritoc &optab[320]

static char L187[]="GAld\341,l\nrla\nsbc\341,a\nld\350,a\n";
#define rudiv &optab[322]

static char L188[]="GA!KI!call\315\n";
static char L189[]="KCGA!pop\344e\ncall\315\n";
#define ruasdiv &optab[325]

static char L190[]="Q\nKI!call\315\n\3\n";
static char L191[]="ld\341,(A)\nld\354,a\nld\350,0\nKI!call\315\nld\341,l\nld\250A),a\n";
static char L192[]="KCQ\npop\344e\ncall\315\n\3\n";
static char L193[]="GDKA!ex\344e,hl\npop\350l\npush\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\ncall\315\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
static char L194[]="KCld\341,(A)\nld\354,a\nld\350,0\npop\344e\ncall\315\nld\341,l\nld\250A),a\n";
static char L195[]="GDKA!ex\344e,hl\npop\350l\nld\341,(hl)\npush\350l\nld\354,a\nld\350,0\ncall\315\npop\344e\nld\341,l\nld\250de),a\n";
#define rptrdif &optab[335]

static char L196[]="GA?sra\350\nrr\354\n";
#define eassign &optab[337]

#define move1 L197

static char L197[]="ld\250A),0\n";
#define move2 L198

static char L198[]="GBld\250hl),0\ninc\350l\nld\250hl),0\n";
#define move3 L199

static char L199[]="ld\350l,B\n\3\n";
#define move4 L200

static char L200[]="KBex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\n\3\n";
#define move5 L201

static char L201[]="KA\3\n";
#define move6 L202

static char L202[]="GBld\344e,B\nld\250hl),e\ninc\350l\nld\250hl),d\n";
#define move7 L203

static char L203[]="GBKJpush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\nld\250hl),e\ninc\350l\nld\250hl),d\n";
#define move8 L204

static char L204[]="GBKIld\250hl),e\ninc\350l\nld\250hl),d\n";
#define move9 L205

static char L205[]="KBGJpush\344e\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\nex\344e,hl\npop\350l\nld\250hl),e\ninc\350l\nld\250hl),d\n";
#define move10 L206

static char L206[]="KAGJex\344e,hl\nld\250hl),e\ninc\350l\nld\250hl),d\n";
#define move11 L207

static char L207[]="GDKBpush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\nld\250hl),e\ninc\350l\nld\250hl),d\n";
#define move12 L208

static char L208[]="GDKApop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
static char L209[]="KAcall\346toi\n\3\n";
static char L210[]="KAGJcall\346toi\nex\344e,hl\nld\250hl),e\ninc\350l\nld\250hl),d\n";
static char L211[]="ld\350l,0\n\3\nld\250A+2),hl\n";
static char L212[]="GBld\250hl),0\ninc\350l\nld\250hl),0\ninc\350l\nld\250hl),0\ninc\350l\nld\250hl),0\n";
#define move13a L213

static char L213[]="ld\350l,B\n\3\nld\341,h\nrla\nsbc\341,a\nld\350,a\nld\354,a\nld\250A+2),hl\n";
static char L214[]="KBex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\n\3\nld\341,h\nrla\nsbc\341,a\nld\350,a\nld\354,a\nld\250A+2),hl\n";
static char L215[]="KA\3\nld\341,h\nrla\nsbc\341,a\nld\350,a\nld\354,a\nld\250A+2),hl\n";
static char L216[]="KAcall\346tol\n\3\nld\250A+2),de\n";
static char L217[]="KAGJcall\346tol\nex\344e,hl\nld\250hl),e\ninc\350l\nld\250hl),d\ninc\350l\npop\344e\nld\250hl),e\ninc\350l\nld\250hl),d\n";
#define move13 L218

static char L218[]="W\n\3\nld\350l,(B+2)\nld\250A+2),hl\n";
#define move14 L219

static char L219[]="KBcall\354ldde\n\3\nld\250A+2),de\n";
#define move15 L220

static char L220[]="KA\3\nld\250A+2),de\n";
#define move14a L221

static char L221[]="GBld\344e,B\nld\250hl),e\ninc\350l\nld\250hl),d\ninc\350l\nld\341,d\nrla\nsbc\341,a\nld\250hl),a\ninc\350l\nld\250hl),a\n";
#define move16a L222

static char L222[]="GBw\nld\250hl),e\ninc\350l\nld\250hl),d\ninc\350l\nld\344e,(B+2)\nld\250hl),e\ninc\350l\nld\250hl),d\n";
#define move16 L223

static char L223[]="KAGJex\344e,hl\nld\250hl),e\ninc\350l\nld\250hl),d\ninc\350l\npop\344e\nld\250hl),e\ninc\350l\nld\250hl),d\n";
static char L224[]="KCGBpop\344e\nld\250hl),e\ninc\350l\nld\250hl),d\ninc\350l\nld\341,d\nrla\nsbc\341,a\nld\250hl),a\ninc\350l\nld\250hl),a\n";
#define move17 L225

static char L225[]="KCGBpop\344e\nld\250hl),e\ninc\350l\nld\250hl),d\ninc\350l\npop\344e\nld\250hl),e\ninc\350l\nld\250hl),d\n";
#define easor &optab[416]

#define L226 move3

static char L227[]="KAld\341,l\nM\250A)\nld\250A),a\n";
#define L228 move5

#define L229 move6

#define L230 move7

#define L231 move8

#define L232 move9

#define L233 move10

#define L234 move11

#define L235 move12

#define L236 move13a

#define L237 move13

#define L238 move14

#define L239 move15

#define L240 move14a

#define L241 move16a

#define L242 move16

#define L243 move17

#define easxor &optab[473]

#define L244 move15

#define L245 move16

static char L246[]="GDKApop\342c\nld\341,(bc)\nxor\354\nld\250bc),a\ninc\342c\nld\341,(bc)\nxor\350\nld\250bc),a\ninc\342c\nld\341,(bc)\nxor\345\nld\250bc),a\ninc\342c\nld\341,(bc)\nxor\344\nld\250bc),a\n";
#define easadd &optab[486]

static char L247[]="M'\250A)\n";
#define L248 move3

#define L249 move4

#define L250 move5

#define L251 move2

#define L252 move9

static char L253[]="KBpush\344e\nQ\nld\341,(de)\ninc\344e\nld\344,(de)\nld\345,a\nM\n\3\npop\344e\n";
static char L254[]="KAex\344e,hl\nQ\nM\n\3\n";
static char L255[]="KAld\341,(A)\nM\354\nld\250A),a\n";
#define L256 move10

#define L257 move12

static char L258[]="KCGBpush\350l\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\npop\344e\nex\250sp),hl\nex\344e,hl\nM\npop\344e\nld\250de),l\ninc\344e\nld\341,h\nld\250de),a\n";
static char L259[]="KCGBld\341,(hl)\npush\350l\npush\341f\npop\350l\nld\350,0\npop\344e\nex\250sp),hl\nex\344e,hl\nM\npop\344e\nld\341,l\nld\250de),a\n";
#define L260 move13a

#define L261 move13

#define L262 move14

#define L263 move15

#define L264 move14a

#define L265 move16a

#define L266 move16

#define L267 move17

#define easrsh &optab[537]

static char L268[]="Q\nsra\350\nrr\354\n\3\n";
static char L269[]="ld\341,(A)\nsrl\341\nld\250A),a\n";
static char L270[]="GBld\345,(hl)\ninc\350l\nld\344,(hl)\nsra\344\nrr\345\nld\250hl),d\ndec\350l\nld\250hl),e\n";
static char L271[]="GBld\341,(hl)\nsrl\341\nld\250hl),a\n";
#define easlsh &optab[542]

static char L272[]="Q\nadd\350l,hl\n\3\n";
static char L273[]="GBld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nadd\350l,hl\nex\344e,hl\nld\250hl),d\ndec\350l\nld\250hl),e\n";
static char L274[]="Q\nld\341,B\ncall\363hl16\n";
static char L275[]="KBld\341,(de)\nQ\ncall\363hl16\n\3\n";
static char L276[]="KAld\341,l\nQ\ncall\363hl16\n\3\n";
#define rlaslsh &optab[550]

static char L277[]="Q\nld\344e,(A+2)\nld\341,B\ncall\354shl\n\3\nld\250A+2),de\n";
static char L278[]="KCQ\nld\344e,(A+2)\npop\342c\nld\341,c\ncall\354shl\n\3\nld\250A+2),de\n";
static char L279[]="GDKCpop\350l\npush\350l\ncall\354ld\npop\342c\nld\341,c\ncall\354shl\npop\344e\ncall\354stde\n";
#define efield &optab[557]

static char L280[]="ld\341,(A)\nand\332\nld\350l,B\nor\354\nld\250A),a\n";
static char L281[]="KAld\341,(A)\nand\332\nor\354\nld\250A),a\n";
static char L282[]="GBld\341,(hl)\nand\332\nld\344e,B\nor\345\nld\250hl),a\n";
#define fas1 L283

static char L283[]="KAGJld\341,(de)\nand\332\nor\354\nld\250de),a\n";
static char L284[]="GBKIld\341,(hl)\nand\332\nor\345\nld\250hl),a\n";
static char L285[]="KCGBpop\344e\nld\341,(hl)\nand\332\nor\345\nld\250hl),a\n";
#define ccmp &optab[564]

static char L286[]="Q\nld\341,h\nor\354\n";
static char L287[]="call\346ldA\ncall\346tst\n";
static char L288[]="GBld\341,(hl)\ninc\350l\nor\250hl)\n";
static char L289[]="GBcall\346ldi\ncall\346tst\n";
static char L290[]="GE";
static char L291[]="Q\nw\nor\341\nsbc\350l,de\n";
static char L292[]="GBld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\nld\344e,B\nor\341\nsbc\350l,de\n";
#define L293 add1

static char L294[]="GBKJpush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\nor\341\nsbc\350l,de\n";
static char L295[]="GBKIld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\nor\341\nsbc\350l,de\n";
#define L296 add2

#define L297 add3

static char L298[]="GDKBpush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\nld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\nor\341\nsbc\350l,de\n";
static char L299[]="GDKApop\344e\nld\341,(de)\ninc\344e\npush\341f\nld\341,(de)\npop\344e\nld\344,a\nor\341\nsbc\350l,de\n";
#define L300 add5

static char L301[]="Q\nld\341,h\nor\354\nX0ld\350l,(A+2)\nld\341,h\nor\354\nX1";
static char L302[]="Q\nld\341,h\nor\354\nX0ld\350l,(A+2)\nld\344e,B\nor\341\nsbc\350l,de\nX1";
#define lcmp1 L303

static char L303[]="Q\nw\nM\nX0ld\350l,(A+2)\nld\344e,(B+2)\nM\nX1";
static char L304[]="GBld\341,(hl)\ninc\350l\nor\250hl)\ninc\350l\nX0ld\341,(hl)\ninc\350l\nor\250hl)\nX1";
static char L305[]="GBld\341,(hl)\ninc\350l\nor\250hl)\ninc\350l\nX0ld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\nld\344e,B\nor\341\nsbc\350l,de\nX1";
#define lcmp2 L306

static char L306[]="GBpush\350l\nld\345,(hl)\ninc\350l\nld\344,(hl)\nW\nex\344e,hl\nM\nX0pop\350l\ninc\350l\ninc\350l\nld\345,(hl)\ninc\350l\nld\344,(hl)\nld\350l,(B+2)\nex\344e,hl\nM\nX1";
static char L307[]="GAld\341,h\nor\354\nX0ld\341,d\nor\345\nX1";
static char L308[]="GAld\341,h\nor\354\nX0ld\350l,B\nex\344e,hl\nor\341\nsbc\350l,de\nX1";
#define lcmp3 L309

static char L309[]="GApush\344e\nw\nM\nX0pop\350l\nld\344e,(B+2)\nM\nX1";
#define lcmp4 L310

static char L310[]="GBKJpush\350l\npush\344e\nld\345,(hl)\ninc\350l\nld\344,(hl)\npop\350l\npush\344e\nld\345,(hl)\ninc\350l\nld\344,(hl)\npop\350l\nM\nX0pop\350l\npop\344e\ninc\350l\ninc\350l\ninc\344e\ninc\344e\npush\350l\npush\344e\nld\345,(hl)\ninc\350l\nld\344,(hl)\npop\350l\npush\344e\nld\345,(hl)\ninc\350l\nld\344,(hl)\npop\350l\nM\nX1";
#define lcmp5 L311

static char L311[]="GAKJpush\350l\npush\344e\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\nM\nX0pop\350l\ninc\344e\ninc\344e\npush\344e\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\344e\nM\nX1";
#define lcmp6 L312

static char L312[]="GCKApop\344e\nM\nX0pop\344e\nM\nX1";
#define candtst &optab[640]

static char L313[]="Q\nld\344e,B\nld\341,l\nand\345\nld\354,a\nld\341,h\nand\344\nor\354\n";
static char L314[]="KAq\nld\341,l\nand\345\nld\354,a\nld\341,h\nand\344\nor\354\n";
static char L315[]="GBld\344e,B\nld\341,(hl)\nand\345\nld\345,a\ninc\350l\nld\341,(hl)\nand\344\nor\345\n";
#define L316 add1

#define L317 add3

#define L318 add5

static char L319[]="ld\350l,(A+2)\nld\344e,B\nld\341,l\nand\345\nld\354,a\nld\341,h\nand\344\nor\354\nX1";
static char L320[]="GBinc\350l\ninc\350l\nld\344e,B\nld\341,(hl)\nand\345\nld\345,a\ninc\350l\nld\341,(hl)\nand\344\nor\345\nX1";
#define L321 lcmp1

#define L322 lcmp2

#define L323 lcmp3

#define L324 lcmp4

#define L325 lcmp5

#define L326 lcmp6

static char L327[]="GAld\350l,B\nld\341,l\nand\345\nld\354,a\nld\341,h\nand\344\nor\354\nX1";
#define rest &optab[694]

static char L328[]="HA";
#define sname &optab[697]

static char L329[]="ld\350l,0\npush\350l\n";
static char L330[]="Q\npush\350l\n";
static char L331[]="ld\341,(A)\nld\354,a\nld\350,0\npush\350l\n";
static char L332[]="GBld\341,(hl)\ninc\350l\nld\350,(hl)\nld\354,a\npush\350l\n";
static char L333[]="ld\350l,(A+2)\npush\350l\nQ\npush\350l\n";
#define sadd &optab[705]

static char L334[]="GCM'\250sp)\n";
static char L335[]="GCld\344e,B\npop\350l\nM\npush\350l\n";
static char L336[]="GCKBpush\350l\nex\344e,hl\nld\345,(hl)\ninc\350l\nld\344,(hl)\nex\344e,hl\nex\344e,hl\npop\350l\nex\250sp),hl\nM\npush\350l\n";
static char L337[]="GCKApop\344e\nex\250sp),hl\nM\npush\350l\n";
#define sitol &optab[710]

static char L338[]="GCld\350l,0\npush\350l\n";
static char L339[]="Q\nld\341,h\nrla\nsbc\341,a\nld\344,a\nld\345,a\npush\344e\npush\350l\n";
#define sftol &optab[713]

static char L340[]="GAcall\346tol\npush\344e\npush\350l\n";
#define estrasg &optab[715]

static char L341[]="GA!KI!";
static char L342[]="KCGA!pop\344e\n";

/*
 * =============================================================================
 * rjump: GOTO - Unconditional jump
 * =============================================================================
 * Generates: jp target
 *
 * Used for: goto label;
 */
/* rjump */
/*
 * %a,n - Direct jump to addressible label
 * Example: goto label;
 * Output: jp _label
 */

struct optab optab[]={
	{16,0,63,0,L1},	/* 0 */
/*
 * %n*,n - Indirect jump through pointer
 * Example: goto *ptr;  (computed goto, GCC extension)
 * First evaluate the pointer expression, then jump through HL.
 * F* = evaluate First subtree and dereference (loads address into HL)
 * Output: jp (hl)
 */
	{127,0,63,0,L2},	/* 1 */
/*
 * =============================================================================
 * rcall: CALL - Function call
 * =============================================================================
 * Generates: call target
 *
 * Used for: func() or (*fptr)()
 * Arguments are already pushed to stack before this pattern runs.
 * Return value ends up in HL (or HLDE for long/float).
 */
	{0},
/* rcall */
/*
 * %a,n - Direct call to addressible function
 * Example: printf("hello");
 * I = instruction prefix (underscore for C symbols)
 * A1 = function name
 * Output: call _printf
 */
	{16,0,63,0,L3},	/* 3 */
/*
 * %n*,n - Indirect call through function pointer (pointer expression)
 * Example: (*fptr)(args);
 * F* = evaluate pointer expression and dereference (loads func addr into HL)
 * jphl = helper routine that does jp (hl) - needed because Z80's
 *        call (hl) doesn't exist, so we call a trampoline.
 * Output:
 *   [code to compute function address into HL]
 *   call jphl
 */
	{127,0,63,0,L4},	/* 4 */
/*
 * %n,n - Indirect call through function pointer (already in register)
 * Example: fptr(args); where fptr is a function pointer variable
 * F = evaluate expression (loads func addr into HL)
 * Output:
 *   [code to load function address into HL]
 *   call jphl
 */
	{63,0,63,0,L5},	/* 5 */
/*
 * =============================================================================
 * rname: NAME - Load value into register
 * =============================================================================
 * This is the fundamental "load" operation. It takes any addressible value
 * and loads it into the primary register (HL for 16-bit, HLDE for 32-bit).
 *
 * This rule handles:
 *   - Constants (including zero)
 *   - Global/static variables
 *   - Stack locals (via IY+offset)
 *   - Dereferencing pointers
 *   - Register variables
 */
	{0},
/* rname */
/*
 * %z,n - Load zero constant
 * Example: x = 0;
 * R = HL (primary register)
 * Output: ld hl,0
 */
	{4,0,63,0,L6},	/* 7 */
/*
 * %zf,n - Load float zero
 * Example: float f = 0.0;
 * Uses helper because floats are 32-bit in HLDE/HL' pair.
 * Output: call fclr  (clears float accumulator)
 */
	{4,4,63,0,L7},	/* 8 */
/*
 * %aub,n - Load addressible unsigned byte
 * Example: unsigned char c; ... use c ...
 * Must zero-extend to 16-bit word in HL.
 * Output:
 *   ld a,(_c)     ; load byte into A
 *   ld l,a        ; move to L
 *   ld h,0        ; zero-extend H
 */
	{16,10,63,0,L8},	/* 9 */
/*
 * %a,n / %ad,n - Load addressible word or double
 * Example: int x; ... use x ...
 * Q1 = smart word load macro - handles all addressing modes:
 *      - Static: ld hl,(_x)
 *      - Stack:  ld l,(iy+n); ld h,(iy+n+1)
 *      - Register: (already in reg, maybe copy)
 * Output: [Q1 expansion for operand 1]
 */
	{16,0,63,0,L9},	/* 10 */
	{16,5,63,0,L9},	/* 11 */
/*
 * %af,n - Load addressible float
 * Example: float f; ... use f ...
 * Uses helper to load 32-bit float from memory.
 * A1 = address of float variable
 * Output: call fldA1  (loads float from address into HLDE)
 */
	{16,4,63,0,L10},	/* 12 */
/*
 * %nub*,n - Load through pointer to unsigned byte
 * Example: unsigned char *p; ... *p ...
 * F* = evaluate pointer expression, result is address in HL
 * Then load byte and zero-extend.
 * Output:
 *   [code to get pointer into HL]
 *   ld a,(hl)     ; load byte through pointer
 *   ld l,a        ; move to L
 *   ld h,0        ; zero-extend
 */
	{127,10,63,0,L11},	/* 13 */
/*
 * %n*,n / %nd*,n - Load word through pointer
 * Example: int *p; ... *p ...
 * F* = evaluate pointer, address now in HL
 * Load 16-bit value byte-by-byte (Z80 little-endian).
 * Output:
 *   [code to get pointer into HL]
 *   ld a,(hl)     ; load low byte
 *   inc hl        ; point to high byte
 *   ld h,(hl)     ; load high byte into H
 *   ld l,a        ; restore low byte to L
 * Result: 16-bit value now in HL
 */
	{127,0,63,0,L12},	/* 14 */
	{127,5,63,0,L12},	/* 15 */
/*
 * %nf*,n - Load float through pointer
 * Example: float *p; ... *p ...
 * F* = evaluate pointer (address in HL)
 * fldi = "float load indirect" helper
 * Output:
 *   [code to get pointer into HL]
 *   call fldi     ; load 32-bit float from (HL) into HLDE
 */
	{127,4,63,0,L13},	/* 16 */
/*
 * %al,n / %aul,n - Load addressible long (32-bit)
 * Example: long x; ... use x ...
 * LA1 = load address of operand 1 into HL
 * Then manually load 4 bytes into HLDE (HL=low, DE=high).
 * Output:
 *   [LA1 - loads address of x into HL]
 *   ld e,(hl)     ; byte 0 -> E (low byte of low word)
 *   inc hl
 *   ld d,(hl)     ; byte 1 -> D (high byte of low word)
 *   inc hl
 *   ld a,(hl)     ; byte 2 -> A (will go to L)
 *   inc hl
 *   ld h,(hl)     ; byte 3 -> H (high byte of high word)
 *   ld l,a        ; byte 2 -> L (low byte of high word)
 *   ex de,hl      ; swap so HL=low word, DE=high word
 * Result: 32-bit value in HLDE (HL=low 16 bits, DE=high 16 bits)
 */
	{16,8,63,0,L14},	/* 17 */
	{16,11,63,0,L14},	/* 18 */
/*
 * %nl*,n / %nul*,n - Load long through pointer
 * Example: long *p; ... *p ...
 * F* = evaluate pointer (address in HL)
 * lld = "long load" helper - loads 32-bit from (HL) into HLDE
 * Output:
 *   [code to get pointer into HL]
 *   call lld      ; load 32-bit value from (HL)
 */
	{127,8,63,0,L15},	/* 19 */
	{127,11,63,0,L15},	/* 20 */
/*
 * %n,n - Fallback: evaluate any expression
 * Example: complex expression that's not addressible
 * F = evaluate First subtree (result in HL)
 * This is the catch-all that triggers recursive code generation.
 */
	{63,0,63,0,L16},	/* 21 */
/*
 * =============================================================================
 * rpostinc: POSTINC/POSTDEC (x++, x--)
 * =============================================================================
 * Postfix increment/decrement: return OLD value, then modify.
 * This is tricky because we must:
 *   1. Save the original value (to return)
 *   2. Compute new value
 *   3. Store new value back
 *   4. Return original value
 *
 * I' = inc or dec instruction depending on operator (++ or --)
 * V = propagate carry/borrow for multi-byte arithmetic
 */
	{0},
/* rpostinc */
/*
 * %a,1 - Simple addressible word, increment/decrement by 1
 * Example: x++ where x is a simple variable
 * This is the fast path - Z80 has inc/dec (addr) instructions.
 * Output:
 *   ld hl,(_x)    ; load original value (return this)
 *   inc (_x)      ; or dec (_x) - modify in place
 */
	{16,0,5,0,L17},	/* 23 */
/*
 * %aw,n - Addressible word, increment by arbitrary amount
 * Example: x += 5 (treated as postfix form)
 * Output:
 *   ld hl,(_x)    ; load original
 *   ld de,5       ; load increment amount
 *   push hl       ; save original for return
 *   [Q1]          ; reload x into HL
 *   add hl,de     ; I = add/sub
 *   [P1]          ; store result back to x
 *   pop hl        ; restore original as return value
 */
	{16,1,63,0,L18},	/* 24 */
/*
 * %aub,n - Addressible unsigned byte
 * Example: char c; c++;
 * Output:
 *   ld a,(_c)     ; load original byte
 *   ld l,a        ; zero-extend to HL
 *   ld h,0
 *   push hl       ; save original
 *   inc a         ; I' = inc or dec
 *   ld (_c),a     ; store new value
 *   pop hl        ; restore original as return
 */
	{16,10,63,0,L19},	/* 25 */
/*
 * %e*,1 - Expression pointer to word, inc/dec by 1
 * Example: (*p)++ where p is already computed (in DE)
 * F1* = evaluate pointer with op1 context into DE
 * V = adc a,0 or sbc a,0 to propagate carry to high byte
 * Output:
 *   [F1* - get pointer into DE]
 *   push de               ; save pointer
 *   ex de,hl              ; HL = pointer
 *   ld e,(hl); inc hl; ld d,(hl)  ; load word into DE
 *   ex de,hl              ; HL = original value
 *   ex (sp),hl            ; stack=orig, HL=pointer
 *   ld a,(hl); inc a; ld (hl),a   ; inc low byte
 *   inc hl
 *   ld a,(hl); adc a,0; ld (hl),a ; propagate carry to high
 *   pop hl                ; return original value
 */
	{84,0,5,0,L20},	/* 26 */
/*
 * %n*,1 - Any pointer expression, inc/dec by 1
 * Example: (*expr)++
 * F* = evaluate pointer expression into HL
 * Output:
 *   [F* - get pointer into HL]
 *   push hl               ; save pointer
 *   ld e,(hl); inc hl; ld d,(hl)  ; load original into DE
 *   ex (sp),hl            ; stack=orig_value, HL=pointer
 *   ld a,(hl); inc a; ld (hl),a   ; inc low byte
 *   inc hl
 *   ld a,(hl); adc a,0; ld (hl),a ; propagate carry
 *   ex de,hl              ; return original in HL
 */
	{127,0,5,0,L21},	/* 27 */
/*
 * %ew*,n - Expression pointer to word, inc/dec by n
 * Example: (*p) += 5 as postfix
 * Complex: need to save ptr, load value, save orig, add, store, return orig
 */
	{84,1,63,0,L22},	/* 28 */
/*
 * %eub*,n - Expression pointer to unsigned byte
 * Example: (*p)++ where p points to char
 */
	{84,10,63,0,L23},	/* 29 */
/*
 * %nw*,n - Any pointer to word, inc/dec by n
 * Example: (*expr) += n as postfix
 */
	{127,1,63,0,L24},	/* 30 */
/*
 * %nub*,n - Any pointer to unsigned byte
 * Example: (*expr)++ where expr points to char
 */
	{127,10,63,0,L25},	/* 31 */
/*
 * %al,1 / %aul,1 - Addressible long, inc/dec by 1
 * Example: long x; x++;
 * Must propagate carry through all 4 bytes.
 * Output:
 *   [F]           ; ?
 *   [Q1]          ; load address into HL
 *   inc hl; inc hl ; point to high word
 *   [inc byte 2, propagate to byte 3]
 *   [load low word as return value]
 */
	{16,8,5,0,L26},	/* 32 */
	{16,11,5,0,L26},	/* 33 */
/*
 * %el*,1 / %eul*,1 - Expression pointer to long, inc/dec by 1
 * Example: (*lp)++ where lp is long*
 * Very complex - must handle 32-bit value and carry propagation.
 */
	{84,8,5,0,L27},	/* 34 */
	{84,11,5,0,L27},	/* 35 */
/*
 * %nl*,1 / %nul*,1 - Any pointer to long, inc/dec by 1
 * Example: (*expr)++ where expr evaluates to long*
 */
	{127,8,5,0,L28},	/* 36 */
	{127,11,5,0,L28},	/* 37 */
/*
 * =============================================================================
 * runary: NEG (-) and COMPL (~) - Unary operators
 * =============================================================================
 * Unary minus: -x = 0 - x = negate
 * Bitwise complement: ~x = invert all bits
 *
 * I = the appropriate instruction (call neg16 or call com16 for words,
 *     call lneg or call lcom for longs)
 */
	{0},
/* runary */
/*
 * %n,n / %nf,n - Negate or complement word/float
 * Example: -x or ~x
 * F = evaluate operand into HL
 * I = neg16 (negate) or com16 (complement) helper call
 * Output:
 *   [code to load x into HL]
 *   call neg16    ; or call com16, or inline for simple cases
 */
	{63,0,63,0,L29},	/* 39 */
	{63,4,63,0,L29},	/* 40 */
/*
 * %nl,n / %nul,n - Negate or complement long
 * Example: -longval or ~longval
 * F = evaluate into HLDE (32-bit)
 * I = lneg or lcom helper function
 * Output:
 *   [code to load longval into HLDE]
 *   call lneg     ; or call lcom
 */
	{63,8,63,0,L30},	/* 41 */
	{63,11,63,0,L30},	/* 42 */
/*
 * =============================================================================
 * rassign: ASSIGN (=) - Simple assignment
 * =============================================================================
 * Evaluate RHS, store to LHS, return the stored value.
 * The assignment operator returns the value assigned (for chaining: a = b = c).
 *
 * S = evaluate Second (right) operand - the value to assign
 * F* = evaluate First (left) operand as address for storing
 */
	{0},
/* rassign */
/*
 * %a,n / %ad,nf - Assign to addressible word or double
 * Example: x = expr;
 * S = evaluate RHS into HL
 * Then store HL to the addressible location.
 * Output:
 *   [code to evaluate RHS into HL]
 *   ld (_x),hl    ; store to variable
 * Result: HL still contains value (for chained assignments)
 */
	{16,0,63,0,L31},	/* 44 */
	{16,5,63,4,L31},	/* 45 */
/*
 * %aub,n - Assign to addressible unsigned byte
 * Example: char c = expr;
 * S = evaluate RHS into HL (only L is meaningful)
 * Store low byte only.
 * Output:
 *   [code to evaluate RHS into HL]
 *   ld a,l        ; get low byte
 *   ld (_c),a     ; store byte
 */
	{16,10,63,0,L32},	/* 46 */
/*
 * %af,nf - Assign to addressible float
 * Example: float f = expr;
 * S = evaluate RHS into float registers
 * fstA1 = store float to address A1
 */
	{16,4,63,4,L33},	/* 47 */
/*
 * %nd*,af - Assign to pointer-to-double from addressible float
 * Example: *dp = floatvar;
 * F* = evaluate destination pointer
 * S = evaluate source float
 * fsti = float store indirect (store to address in HL)
 */
	{127,5,16,4,L34},	/* 48 */
/*
 * %n*,aw - Assign addressible word to pointer location
 * Example: *p = intvar;
 * F* = evaluate destination pointer into HL
 * A2 = load source value directly
 * Store word byte-by-byte (Z80 little-endian).
 * Output:
 *   [code to get dest pointer into HL]
 *   ld de,(_var)  ; load source value
 *   ld (hl),e     ; store low byte
 *   inc hl
 *   ld (hl),d     ; store high byte
 *   ex de,hl      ; return value in HL
 */
	{127,0,16,1,L35},	/* 49 */
/*
 * %nf*,af - Assign float through pointer
 * Example: *fp = floatvar;
 */
	{127,4,16,4,L36},	/* 50 */
/*
 * %n*,e - Assign expression (in DE) through pointer
 * Example: *p = expr; where expr is already evaluated
 * F* = evaluate dest pointer into HL
 * S1 = evaluate source into DE
 * Store DE through HL.
 */
	{127,0,20,0,L37},	/* 51 */
/*
 * %nub*,e - Assign byte expression through pointer
 * Example: *cp = charexpr;
 * Store only low byte, zero-extend result.
 */
	{127,10,20,0,L38},	/* 52 */
/*
 * %ed*,nf / %ef*,nf - Assign float to expression pointer
 * Example: *ep = floatexpr; where ep is pointer expression
 * S = evaluate RHS (float) into float regs
 * F1* = evaluate dest pointer into DE
 * fstde = store float to address in DE
 */
	{84,5,63,4,L39},	/* 53 */
	{84,4,63,4,L40},	/* 54 */
/*
 * %n*,n / %nd*,nf - General pointer assignment
 * Example: *p = expr;
 * FS* = evaluate dest pointer, push it
 * S = evaluate source into HL
 * pop dest address, store HL there.
 * Output:
 *   [code to push dest address]
 *   [code to evaluate source into HL]
 *   pop de        ; get dest address
 *   ld (de),l     ; store low byte
 *   inc de
 *   ld a,h        ; can't do ld (de),h directly
 *   ld (de),a     ; store high byte
 */
	{127,0,63,0,L41},	/* 55 */
	{127,5,63,4,L41},	/* 56 */
/*
 * %nub*,n - Assign byte through general pointer
 */
	{127,10,63,0,L42},	/* 57 */
/*
 * %nf*,nf - Assign float through general pointer
 */
	{127,4,63,4,L43},	/* 58 */
/*
 * %al,nl ... - Assign long to addressible long
 * Example: long x = longexpr;
 * S = evaluate RHS into HLDE (32-bit)
 * P1 = store low word (HL) to x
 * Q1+2 = load address of x+2 (high word location)
 * Store high word (DE) there.
 */
	{16,8,63,8,L44},	/* 59 */
	{16,8,63,11,L44},	/* 60 */
	{16,11,63,8,L44},	/* 61 */
	{16,11,63,11,L44},	/* 62 */
/*
 * %el*,nl ... - Assign long through expression pointer
 * Example: *lp = longexpr; where lp is expression
 * S = evaluate RHS (32-bit)
 * F1* = evaluate dest pointer into DE
 * Store all 4 bytes.
 */
	{84,8,63,8,L45},	/* 63 */
	{84,8,63,11,L45},	/* 64 */
	{84,11,63,8,L45},	/* 65 */
	{84,11,63,11,L45},	/* 66 */
/*
 * %nl*,nl ... - Assign long through general pointer
 * Example: *p = longexpr;
 * FS* = push dest pointer
 * S = evaluate RHS (32-bit)
 * Use BC as temp for dest pointer to store all 4 bytes.
 */
	{127,8,63,8,L46},	/* 67 */
	{127,8,63,11,L46},	/* 68 */
	{127,11,63,8,L46},	/* 69 */
	{127,11,63,11,L46},	/* 70 */
/*
 * =============================================================================
 * rfield: FSEL - Bitfield assignment
 * =============================================================================
 * Bitfields require read-modify-write: read current value, mask off the
 * field bits, OR in the new value, write back.
 *
 * Z = field mask (inverted) - the bits to preserve
 * The new value is already shifted to the correct position by the compiler.
 */
	{0},
/* rfield */
/*
 * %a,n - Assign to addressible bitfield
 * Example: struct.field = val;
 * S = evaluate new value into HL (L has the shifted bits)
 * Read byte, clear field bits with AND, set new bits with OR, write back.
 * Output:
 *   [code to evaluate new value into HL]
 *   ld a,(_struct)  ; read current byte
 *   and 0xF0        ; Z = mask to preserve other bits
 *   or l            ; merge in new field value
 *   ld (_struct),a  ; write back
 */
	{16,0,63,0,L47},	/* 72 */
/*
 * %e*,n - Bitfield through expression pointer
 * Uses shared pattern [fas1] defined in efftab section.
 */
	{84,0,63,0,L48},	/* 73 */


/*
 * %n*,n - Bitfield through general pointer
 * SS = push new value
 * F* = get pointer into HL
 * Do read-modify-write through pointer.
 */
	{127,0,63,0,L49},	/* 74 */
/*
 * =============================================================================
 * radd: Binary arithmetic operators (+, -, |, &, <<)
 * =============================================================================
 * These operators share similar code patterns:
 *   - Evaluate left operand into HL
 *   - Evaluate right operand into DE
 *   - Apply operator (add hl,de / or a; sbc hl,de / etc.)
 *   - Result in HL
 *
 * I = the instruction for this operator:
 *     + : add hl,de
 *     - : or a; sbc hl,de
 *     | : call or16
 *     & : call and16
 *     <<: add hl,hl (for shift by 1) or call shl16
 *
 * I' = inc/dec variant for +1/-1 optimization
 */
	{0},
/* radd */
/*
 * %n,z - Add/sub zero is identity
 * Example: x + 0, x - 0
 * Just evaluate the operand, result is unchanged.
 */
	{63,0,4,0,L50},	/* 76 */
/*
 * %n,1 - Add/sub by 1, use inc/dec
 * Example: x + 1, x - 1
 * F = evaluate into HL
 * I' = inc hl or dec hl
 * Output:
 *   [code to load x into HL]
 *   inc hl        ; or dec hl
 */
	{63,0,5,0,L51},	/* 77 */
/*
 * %[add1:] - NAMED PATTERN: operand + addressible word
 * %n,aw / %nf,ad - Add word from addressible location
 * Example: x + y where y is a simple variable
 * F = evaluate left operand into HL
 * Load right operand directly into DE.
 * I = add/sub/or/and instruction.
 * Output:
 *   [code to load x into HL]
 *   ld de,(_y)    ; load right operand
 *   add hl,de     ; I = the operation
 */
	{63,0,16,1,L52},	/* 78 */
	{63,4,16,5,L52},	/* 79 */
/*
 * %[add2:] - operand + expression-pointer-to-word
 * %n,ew* / %nf,ed* - Right operand is dereferenced expression pointer
 * Example: x + *p where p is an expression
 * F = evaluate left operand into HL
 * S1* = evaluate right pointer expression into DE, dereference
 * Load word through pointer, then operate.
 */
	{63,0,84,1,L53},	/* 80 */
	{63,4,84,5,L53},	/* 81 */
/*
 * %[add3:] - operand + expression (already in register)
 * %n,e / %nf,ef - Right operand already evaluated
 * Example: x + expr where expr is already computed
 * F = evaluate left operand into HL
 * S1 = right operand already in DE
 * Just do the operation.
 */
	{63,0,20,0,L54},	/* 82 */
	{63,4,20,4,L54},	/* 83 */
/*
 * %[add4:] - operand + pointer-to-word (needs deref)
 * %n,nw* / %nf,nd* - Right operand is any pointer needing dereference
 * Example: x + *ptr
 * SS* = evaluate right pointer, push it
 * F = evaluate left operand into HL
 * Load through the saved pointer.
 */
	{63,0,127,1,L55},	/* 84 */
	{63,4,127,5,L55},	/* 85 */
/*
 * %[add5:] - General case: any + any
 * %n,n / %nf,nf - Both operands need full evaluation
 * Example: expr1 + expr2
 * SS = evaluate right operand, push it
 * F = evaluate left operand into HL
 * Pop right operand into DE, operate.
 * Output:
 *   [code to evaluate expr2, push it]
 *   [code to evaluate expr1 into HL]
 *   pop de        ; get right operand
 *   add hl,de     ; I = the operation
 */
	{63,0,63,0,L56},	/* 86 */
	{63,4,63,4,L56},	/* 87 */
/*
 * Long addition/subtraction - special handling for 32-bit
 * These patterns handle long + constant, long + unsigned, etc.
 * V = adc/sbc to propagate carry between low and high words.
 */

/*
 * %nl,c / %nul,c / %nl,au / %nul,au - Long + small constant
 * Example: long_val + 5
 * Add to low word, propagate carry to high word.
 * Output:
 *   [code to load long into HLDE]
 *   ld bc,5       ; constant
 *   add hl,bc     ; add to low word
 *   ex de,hl
 *   ld hl,0       ; no carry-in to high word const
 *   adc hl,de     ; V = propagate carry
 *   ex de,hl      ; result in HLDE
 */
	{63,8,8,0,L57},	/* 88 */
	{63,8,16,9,L57},	/* 89 */
	{63,11,8,0,L57},	/* 90 */
	{63,11,16,9,L57},	/* 91 */
/*
 * %nl,eu / %nul,eu - Long + unsigned expression
 * Example: long_val + unsigned_expr
 */
	{63,8,20,9,L58},	/* 92 */
	{63,11,20,9,L58},	/* 93 */
/*
 * %nl,al ... - Long + addressible long
 * Example: long1 + long2 where both are variables
 * Add low words, then high words with carry.
 */
	{63,8,16,8,L59},	/* 94 */
	{63,8,16,11,L59},	/* 95 */
	{63,11,16,8,L59},	/* 96 */
	{63,11,16,11,L59},	/* 97 */
/*
 * %[addl1:] - Long + expression long (already in registers)
 * The helper function handles the 32-bit operation.
 */
	{63,8,20,8,L60},	/* 98 */
	{63,8,20,11,L60},	/* 99 */
	{63,11,20,8,L60},	/* 100 */
	{63,11,20,11,L60},	/* 101 */
/*
 * %[addl2:] - Long + any long (general case)
 * Must push one operand, evaluate other, call helper.
 * I = ladd (long add), lsub (long sub), lor, land, etc.
 */
	{63,8,63,8,L61},	/* 102 */
	{63,8,63,11,L61},	/* 103 */
	{63,11,63,8,L61},	/* 104 */
	{63,11,63,11,L61},	/* 105 */
/*
 * =============================================================================
 * rxor: XOR (^) - Bitwise exclusive OR
 * =============================================================================
 * XOR needs its own section because Z80 has no 16-bit XOR instruction.
 * Must call helper function xor16.
 */
	{0},
/* rxor */
/*
 * %n,e - XOR with expression (already in register)
 * Uses shared [add3] pattern - works the same way.
 */
	{63,0,20,0,L62},	/* 107 */


/*
 * %n,n - General XOR
 * Example: a ^ b
 * FS = evaluate left (but this looks wrong - should be SS?)
 * S = evaluate right into HL
 * Call helper since Z80 can't XOR HL and DE directly.
 */
	{63,0,63,0,L63},	/* 108 */
/*
 * Long XOR - 32-bit exclusive OR
 */
	{63,8,20,8,L64},	/* 109 */
	{63,8,20,11,L64},	/* 110 */
	{63,11,20,8,L64},	/* 111 */
	{63,11,20,11,L64},	/* 112 */


	{63,8,63,8,L65},	/* 113 */
	{63,8,63,11,L65},	/* 114 */
	{63,11,63,8,L65},	/* 115 */
	{63,11,63,11,L65},	/* 116 */
/*
 * =============================================================================
 * rrsh: RSHIFT (>>) - Signed right shift
 * =============================================================================
 * Arithmetic right shift preserves sign bit.
 * Z80's SRA instruction does arithmetic shift.
 */
	{0},
/* rrsh */
/*
 * %n,1 - Shift right by 1
 * Example: x >> 1
 * F = load value into HL
 * SRA H shifts H right, bit 7 preserved, bit 0 -> carry
 * RR L rotates L right through carry (brings in bit from H)
 * Output:
 *   [code to load x into HL]
 *   sra h         ; arithmetic shift H right
 *   rr l          ; rotate L right through carry
 * Result: HL = x >> 1 with sign preserved
 */
	{63,0,5,0,L66},	/* 118 */
/*
 * =============================================================================
 * rmul: TIMES (*) - Multiplication
 * =============================================================================
 * Z80 has no multiply instruction. All multiplication calls helper functions.
 * Uses same patterns as addition since operand handling is identical.
 *
 * I = mul16 (16-bit multiply) or fmul (float multiply)
 */
	{0},
/* rmul */
/*
 * %n,aw / %nf,ad - Multiply by addressible operand
 * Uses [add1] pattern - load operands, call mul16.
 */
	{63,0,16,1,L67},	/* 120 */
	{63,4,16,5,L67},	/* 121 */


	{63,0,84,1,L68},	/* 122 */
	{63,4,84,5,L68},	/* 123 */


	{63,0,20,0,L69},	/* 124 */
	{63,4,20,4,L69},	/* 125 */


	{63,0,63,0,L70},	/* 126 */
	{63,4,63,4,L70},	/* 127 */


/*
 * =============================================================================
 * rdiv: DIVIDE (/) and MOD (%) - Division operations
 * =============================================================================
 * Z80 has no divide instruction. All division calls helper functions.
 *
 * For integers: I = div16 (signed divide) or mod16 (modulo)
 * For floats: I = fdiv
 *
 * Unlike multiply, divide has specific patterns because:
 * - Division is not commutative (order matters)
 * - Need to call helper functions explicitly
 */
	{0},
/* rdiv */
/*
 * %n,aw - Divide by addressible word
 * Example: x / y where y is a variable
 * F = evaluate dividend into HL
 * Load divisor into DE
 * Call divide helper.
 * Output:
 *   [code to load x into HL]
 *   ld de,(_y)
 *   call div16    ; HL = HL / DE, remainder in BC (for mod)
 */
	{63,0,16,1,L71},	/* 129 */
/*
 * %n,ew* - Divide by value through expression pointer
 * Example: x / *p where p is an expression
 */
	{63,0,84,1,L72},	/* 130 */
/*
 * %n,e - Divide by expression (already in register)
 */
	{63,0,20,0,L73},	/* 131 */
/*
 * %n,n - General divide
 * Example: expr1 / expr2
 */
	{63,0,63,0,L74},	/* 132 */
/*
 * Float division - uses same patterns as addition
 */
	{63,4,16,5,L75},	/* 133 */


	{63,4,84,5,L76},	/* 134 */


	{63,4,20,4,L77},	/* 135 */


	{63,4,63,4,L78},	/* 136 */


/*
 * =============================================================================
 * rptoi: PTOI - Pointer difference divided by element size
 * =============================================================================
 * When subtracting two pointers of the same type, C requires the result
 * to be divided by sizeof(element) to give the number of elements between.
 *
 * Example: int *p, *q; ... (p - q) gives element count, not byte count.
 *
 * The subtraction is already done; this just does the division.
 * F! = evaluate with some special flag (difference already computed?)
 */
	{0},
/* rptoi */
	{63,8,16,0,L79},	/* 138 */
	{63,11,16,0,L79},	/* 139 */
/*
 * =============================================================================
 * rasadd: Compound Assignment (+=, -=, |=, &=)
 * =============================================================================
 * Compound assignment operators: x op= y is equivalent to x = x op y
 * but x is only evaluated once (important for side effects).
 *
 * These patterns must:
 *   1. Evaluate RHS into a register
 *   2. Load current LHS value
 *   3. Perform the operation
 *   4. Store result back to LHS
 *   5. Return the new value
 *
 * I = the operation instruction (add hl,de / sbc hl,de / or16 / and16)
 *
 * Named patterns [addqN:] are referenced by other compound operators
 * (*=, /=, etc.) that share the same operand handling.
 */
	{0},
/* rasadd */
/*
 * %[addq1:] - Simplest case: addressible word op= addressible word
 * %aw,aw
 * Example: x += y; where both are simple variables
 * Q1 = load x into HL
 * Load y into DE
 * I = operation (add hl,de)
 * P1 = store result back to x
 * Output:
 *   ld hl,(_x)    ; Q1: load LHS
 *   ld de,(_y)    ; load RHS
 *   add hl,de     ; I: operation
 *   ld (_x),hl    ; P1: store result
 * Result: HL = new value
 */
	{16,1,16,1,L80},	/* 141 */
/*
 * %[addq20:] - Unsigned byte op= addressible word
 * %aub,aw
 * Example: unsigned char c; c += x;
 * Must zero-extend byte to word, operate, store byte back.
 */
	{16,10,16,1,L81},	/* 142 */
/*
 * %[addq1a:] - Generic addressible op= addressible
 * %a,aw / %ad,ad
 * Example: x += y;
 * Direct load/operate/store.
 */
	{16,0,16,1,L82},	/* 143 */
	{16,5,16,5,L82},	/* 144 */
/*
 * %[addq2:] - Addressible word op= pointer-to-word
 * %aw,nw*
 * Example: x += *p;
 * S* = evaluate RHS pointer, dereference
 * Need to save dereferenced value, load LHS, operate, store.
 */
	{16,1,127,1,L83},	/* 145 */
/*
 * %[addq3:] - Addressible word op= any expression
 * %aw,n
 * Example: x += expr;
 * S = evaluate RHS into HL
 * Move to DE, load LHS, operate, store.
 */
	{16,1,63,0,L84},	/* 146 */
/*
 * %[addq21:] - Unsigned byte op= any expression
 * %aub,n
 * Example: unsigned char c; c += expr;
 */
	{16,10,63,0,L85},	/* 147 */
/*
 * %[addq4:] - Expression-pointer-to-word op= pointer-to-word
 * %ew*,nw*
 * Example: *p += *q; where p is an expression
 * Very complex: must save both pointers, load values, operate, store.
 */
	{84,1,127,1,L86},	/* 148 */
/*
 * %[addq4a:] - Addressible double/float op= expression float
 * %ad,ef
 * Example: double d; d += floatexpr;
 * Use float helper functions.
 */
	{16,5,20,4,L87},	/* 149 */
/*
 * %[addq5:] - Generic addressible op= any expression
 * %a,n / %ad,nf
 * Example: x += expr;
 * SS = push RHS
 * Load LHS, pop RHS into DE, operate, store.
 */
	{16,0,63,0,L88},	/* 150 */
	{16,5,63,4,L88},	/* 151 */
/*
 * %[addq6:] - Addressible float op= float expression
 * %af,nf
 * Example: float f; f += floatexpr;
 */
	{16,4,63,4,L89},	/* 152 */
/*
 * %[addq7:] - Expression-pointer-to-word op= any expression
 * %ew*,n
 * Example: *p += expr; where p is expression
 * S = evaluate RHS
 * F1* = evaluate LHS pointer into DE
 * Load through pointer, operate, store back.
 */
	{84,1,63,0,L90},	/* 153 */
/*
 * %[addq8:] - Any-pointer-to-word op= any expression
 * %nw*,n
 * Example: *ptr += expr;
 * SS = push RHS
 * F* = evaluate LHS pointer
 * Complex register juggling to load, operate, store.
 */
	{127,1,63,0,L91},	/* 154 */
/*
 * %[addq9:] - General pointer op= any expression
 * %n*,n
 * Example: *complexexpr += expr;
 * FS* = push LHS pointer
 * SS = push RHS
 * Load through pointer, operate, store back.
 */
	{127,0,63,0,L92},	/* 155 */
/*
 * %[addq22:] - Pointer to unsigned byte op= any expression
 * %nub*,n
 * Example: *charptr += expr;
 * Byte load/store instead of word.
 */
	{127,10,63,0,L93},	/* 156 */
/*
 * %[addq9a:] - Pointer to double op= float expression
 * %nd*,nf
 * Example: *doubleptr += floatexpr;
 */
	{127,5,63,4,L94},	/* 157 */
	{127,4,63,4,L95},	/* 158 */
	{16,8,8,0,L96},	/* 159 */
	{16,11,8,0,L96},	/* 160 */
	{16,8,16,8,L97},	/* 161 */
	{16,8,16,11,L97},	/* 162 */
	{16,11,16,8,L97},	/* 163 */
	{16,11,16,11,L97},	/* 164 */
	{16,8,63,8,L98},	/* 165 */
	{16,8,63,11,L98},	/* 166 */
	{16,11,63,8,L98},	/* 167 */
	{16,11,63,11,L98},	/* 168 */
	{127,8,8,0,L99},	/* 169 */
	{127,11,8,0,L99},	/* 170 */
	{127,8,16,8,L100},	/* 171 */
	{127,8,16,11,L100},	/* 172 */
	{127,11,16,8,L100},	/* 173 */
	{127,11,16,11,L100},	/* 174 */
	{127,8,63,8,L101},	/* 175 */
	{127,8,63,11,L101},	/* 176 */
	{127,11,63,8,L101},	/* 177 */
	{127,11,63,11,L101},	/* 178 */
/* *= */
	{0},
/* rasmul */
	{16,0,16,1,L102},	/* 180 */
	{16,5,16,5,L102},	/* 181 */


	{16,5,20,4,L103},	/* 182 */


	{16,0,63,0,L104},	/* 183 */
	{16,5,63,4,L104},	/* 184 */


	{16,4,63,4,L105},	/* 185 */


	{16,10,16,1,L106},	/* 186 */


	{16,10,63,0,L107},	/* 187 */


	{127,0,63,0,L108},	/* 188 */


	{127,10,63,0,L109},	/* 189 */


	{127,5,63,4,L110},	/* 190 */


	{127,4,63,4,L111},	/* 191 */


/* /= */
	{0},
/* rasdiv */
	{16,0,16,1,L112},	/* 193 */
	{16,0,63,0,L113},	/* 194 */
	{16,10,63,0,L114},	/* 195 */
	{84,0,63,0,L115},	/* 196 */
	{127,0,63,0,L116},	/* 197 */
	{127,10,63,0,L117},	/* 198 */
	{16,5,16,5,L118},	/* 199 */


	{16,5,20,4,L119},	/* 200 */


	{16,5,63,4,L120},	/* 201 */


	{16,4,63,4,L121},	/* 202 */


	{127,5,63,4,L122},	/* 203 */


	{127,4,63,4,L123},	/* 204 */


/* %= and >>= */
	{0},
/* rasmod */
	{16,0,16,1,L124},	/* 206 */
	{16,0,63,0,L125},	/* 207 */
	{16,10,63,0,L126},	/* 208 */
	{84,0,63,0,L127},	/* 209 */
	{127,0,63,0,L128},	/* 210 */
	{127,10,63,0,L129},	/* 211 */
/* ^= */
	{0},
/* rasxor */
	{16,1,63,0,L130},	/* 213 */


	{16,3,63,0,L131},	/* 214 */
	{16,10,63,0,L132},	/* 215 */
	{127,0,63,0,L133},	/* 216 */
	{127,10,63,0,L134},	/* 217 */
/* >>= (simple cases) */
	{0},
/* rasrsh */
	{16,0,5,0,L135},	/* 219 */
	{127,0,5,0,L136},	/* 220 */
/* |=, &~= */
	{0},
/* rasor */
	{16,1,16,1,L137},	/* 222 */


	{16,10,16,0,L138},	/* 223 */
	{16,0,16,1,L139},	/* 224 */
	{16,5,16,5,L139},	/* 225 */


	{16,1,127,1,L140},	/* 226 */


	{16,1,63,0,L141},	/* 227 */


	{16,10,63,0,L142},	/* 228 */
	{84,1,127,1,L143},	/* 229 */


	{16,5,20,4,L144},	/* 230 */


	{16,0,63,0,L145},	/* 231 */
	{16,5,63,4,L145},	/* 232 */


	{16,4,63,4,L146},	/* 233 */


	{84,1,63,0,L147},	/* 234 */


	{127,1,63,0,L148},	/* 235 */


	{127,0,63,0,L149},	/* 236 */


	{127,10,63,0,L150},	/* 237 */
	{127,5,63,4,L151},	/* 238 */


	{127,4,63,4,L152},	/* 239 */


	{16,8,8,0,L153},	/* 240 */
	{16,11,8,0,L153},	/* 241 */


	{16,8,16,8,L154},	/* 242 */
	{16,8,16,11,L154},	/* 243 */
	{16,11,16,8,L154},	/* 244 */
	{16,11,16,11,L154},	/* 245 */


	{16,8,63,8,L155},	/* 246 */
	{16,8,63,11,L155},	/* 247 */
	{16,11,63,8,L155},	/* 248 */
	{16,11,63,11,L155},	/* 249 */


	{127,8,8,0,L156},	/* 250 */
	{127,11,8,0,L156},	/* 251 */


	{127,8,16,8,L157},	/* 252 */
	{127,8,16,11,L157},	/* 253 */
	{127,11,16,8,L157},	/* 254 */
	{127,11,16,11,L157},	/* 255 */


	{127,8,63,8,L158},	/* 256 */
	{127,8,63,11,L158},	/* 257 */
	{127,11,63,8,L158},	/* 258 */
	{127,11,63,11,L158},	/* 259 */


/* << for longs */
	{0},
/* rlshl */
	{63,8,16,1,L159},	/* 261 */
	{63,11,16,1,L159},	/* 262 */


	{63,8,84,1,L160},	/* 263 */
	{63,11,84,1,L160},	/* 264 */


	{63,8,20,0,L161},	/* 265 */
	{63,11,20,0,L161},	/* 266 */


	{63,8,127,1,L162},	/* 267 */
	{63,11,127,1,L162},	/* 268 */


	{63,8,63,0,L163},	/* 269 */
	{63,11,63,0,L163},	/* 270 */


/* >> for unsigned long */
	{0},
/* rlushr */
	{63,8,63,0,L164},	/* 272 */
	{63,11,63,0,L164},	/* 273 */
/* >>= for unsigned long */
	{0},
/* ralushr */
	{63,0,63,0,L165},	/* 275 */
/* int -> float */
	{0},
/* ritof */
	{16,1,63,0,L166},	/* 277 */
	{127,1,63,0,L167},	/* 278 */
	{63,0,63,0,L168},	/* 279 */
/* float -> int */
	{0},
/* rftoi */
	{63,4,63,0,L169},	/* 281 */
/* float to long */
	{0},
/* rftol */
	{63,4,63,0,L170},	/* 283 */
/* long to float */
	{0},
/* rltof */
	{16,8,63,0,L171},	/* 285 */
	{127,8,63,0,L172},	/* 286 */
	{63,8,63,0,L173},	/* 287 */
/* unsigned long to float */
	{0},
/* rultof */
	{16,11,63,0,L174},	/* 289 */
	{127,11,63,0,L175},	/* 290 */
	{63,11,63,0,L176},	/* 291 */
/* integer to long */
	{0},
/* ritol */
	{20,9,63,0,L177},	/* 293 */
	{63,9,63,0,L178},	/* 294 */
	{20,0,63,0,L179},	/* 295 */
	{63,0,63,0,L180},	/* 296 */
/* long to integer */
	{0},
/* rltoi */
	{16,8,63,0,L181},	/* 298 */
	{16,11,63,0,L181},	/* 299 */
	{127,8,63,0,L182},	/* 300 */
	{127,11,63,0,L182},	/* 301 */
/* *, /, % for longs */
	{0},
/* rlmul */
	{63,8,63,8,L183},	/* 303 */
	{63,8,63,11,L183},	/* 304 */
	{63,11,63,8,L183},	/* 305 */
	{63,11,63,11,L183},	/* 306 */
/* *, /, % for unsigned long */
	{0},
/* rulmul */
	{63,11,63,8,L184},	/* 308 */
	{63,8,63,11,L184},	/* 309 */
	{63,11,63,11,L184},	/* 310 */


/* *=, /=, %= for unsigned long */
	{0},
/* rulasmul */
	{63,0,63,8,L185},	/* 312 */
	{63,0,63,11,L185},	/* 313 */
	{63,8,63,0,L185},	/* 314 */
	{63,11,63,0,L185},	/* 315 */


/* *=, /=, %= for longs */
	{0},
/* rlasmul */
	{63,0,63,8,L186},	/* 317 */
	{63,0,63,11,L186},	/* 318 */
/* convert integer to character (sign extend) */
	{0},
/* ritoc */
	{63,0,63,0,L187},	/* 320 */
/* divide, mod for unsigned */
	{0},
/* rudiv */
	{63,0,20,0,L188},	/* 322 */
	{63,0,63,0,L189},	/* 323 */
/* /= %= for unsigned */
	{0},
/* ruasdiv */
	{16,1,20,0,L190},	/* 325 */
	{16,3,20,0,L190},	/* 326 */
	{16,10,20,0,L191},	/* 327 */
	{16,1,63,0,L192},	/* 328 */
	{16,3,63,0,L192},	/* 329 */
	{127,1,63,0,L193},	/* 330 */
	{127,3,63,0,L193},	/* 331 */
	{16,10,63,0,L194},	/* 332 */
	{127,10,63,0,L195},	/* 333 */
/* (int *) - (int *) */
	{0},
/* rptrdif */
	{63,0,63,0,L196},	/* 335 */


/*
 * =============================================================================
 * eassign: Assignment for effect (= where result is discarded)
 * =============================================================================
 * Simpler than rassign because we don't need to leave the value in HL.
 * Many [moveN:] named patterns are defined here and shared by other
 * effect operators.
 */
	{0},
/* eassign */
	{16,0,4,0,L197},	/* 337 */
	{16,5,4,4,L197},	/* 338 */
	{16,10,4,0,L197},	/* 339 */
	{127,0,4,0,L198},	/* 340 */
	{127,5,4,4,L198},	/* 341 */
	{127,10,4,0,L198},	/* 342 */
	{16,0,16,1,L199},	/* 343 */
	{16,3,16,0,L199},	/* 344 */
	{16,3,16,10,L199},	/* 345 */
	{16,10,16,0,L199},	/* 346 */
	{16,10,16,3,L199},	/* 347 */
	{16,3,127,0,L200},	/* 348 */
	{16,0,127,1,L200},	/* 349 */
	{16,10,127,0,L200},	/* 350 */
	{16,0,63,0,L201},	/* 351 */
	{16,10,63,0,L201},	/* 352 */
	{127,0,16,1,L202},	/* 353 */
	{127,3,16,0,L202},	/* 354 */
	{127,10,16,0,L202},	/* 355 */
	{127,0,84,1,L203},	/* 356 */
	{127,3,84,0,L203},	/* 357 */
	{127,10,84,0,L203},	/* 358 */
	{127,0,20,0,L204},	/* 359 */
	{127,10,20,0,L204},	/* 360 */
	{84,0,127,1,L205},	/* 361 */
	{84,3,127,0,L205},	/* 362 */
	{84,10,127,0,L205},	/* 363 */
	{84,0,63,0,L206},	/* 364 */
	{84,10,63,0,L206},	/* 365 */
	{127,0,127,1,L207},	/* 366 */
	{127,3,127,0,L207},	/* 367 */
	{127,10,127,0,L207},	/* 368 */
	{127,0,63,0,L208},	/* 369 */
	{127,10,63,0,L208},	/* 370 */
	{16,1,63,4,L209},	/* 371 */
	{84,1,63,4,L210},	/* 372 */
	{16,8,4,0,L211},	/* 373 */
	{16,11,4,0,L211},	/* 374 */
	{127,8,4,0,L212},	/* 375 */
	{127,11,4,0,L212},	/* 376 */
	{16,8,16,1,L213},	/* 377 */
	{16,11,16,1,L213},	/* 378 */
	{16,8,127,1,L214},	/* 379 */
	{16,11,127,1,L214},	/* 380 */
	{16,8,63,0,L215},	/* 381 */
	{16,11,63,0,L215},	/* 382 */
	{16,8,63,4,L216},	/* 383 */
	{16,11,63,4,L216},	/* 384 */
	{84,8,63,4,L217},	/* 385 */
	{84,11,63,4,L217},	/* 386 */
	{16,8,16,8,L218},	/* 387 */
	{16,8,16,11,L218},	/* 388 */
	{16,11,16,8,L218},	/* 389 */
	{16,11,16,11,L218},	/* 390 */
	{16,8,127,8,L219},	/* 391 */
	{16,8,127,11,L219},	/* 392 */
	{16,11,127,8,L219},	/* 393 */
	{16,11,127,11,L219},	/* 394 */
	{16,8,63,8,L220},	/* 395 */
	{16,8,63,11,L220},	/* 396 */
	{16,11,63,8,L220},	/* 397 */
	{16,11,63,11,L220},	/* 398 */
	{127,8,16,1,L221},	/* 399 */
	{127,11,16,1,L221},	/* 400 */
	{127,8,16,8,L222},	/* 401 */
	{127,8,16,11,L222},	/* 402 */
	{127,11,16,8,L222},	/* 403 */
	{127,11,16,11,L222},	/* 404 */
	{84,8,63,8,L223},	/* 405 */
	{84,8,63,11,L223},	/* 406 */
	{84,11,63,8,L223},	/* 407 */
	{84,11,63,11,L223},	/* 408 */
	{127,8,63,0,L224},	/* 409 */
	{127,11,63,0,L224},	/* 410 */
	{127,8,63,8,L225},	/* 411 */
	{127,8,63,11,L225},	/* 412 */
	{127,11,63,8,L225},	/* 413 */
	{127,11,63,11,L225},	/* 414 */
/* =| and =&~ */
	{0},
/* easor */
	{16,0,16,0,L226},	/* 416 */
	{16,0,16,3,L226},	/* 417 */
	{16,0,16,10,L226},	/* 418 */
	{16,3,16,0,L226},	/* 419 */
	{16,3,16,3,L226},	/* 420 */
	{16,3,16,10,L226},	/* 421 */
	{16,10,16,0,L226},	/* 422 */
	{16,10,16,3,L226},	/* 423 */
	{16,10,16,10,L226},	/* 424 */


	{16,10,63,0,L227},	/* 425 */
	{16,0,63,0,L228},	/* 426 */


	{127,0,16,1,L229},	/* 427 */
	{127,3,16,0,L229},	/* 428 */
	{127,10,16,0,L229},	/* 429 */


	{127,0,84,1,L230},	/* 430 */
	{127,3,84,0,L230},	/* 431 */
	{127,10,84,0,L230},	/* 432 */


	{127,0,20,0,L231},	/* 433 */


	{84,0,127,1,L232},	/* 434 */
	{84,3,127,0,L232},	/* 435 */
	{84,10,127,0,L232},	/* 436 */


	{84,0,63,0,L233},	/* 437 */


	{127,0,127,1,L234},	/* 438 */
	{127,3,127,0,L234},	/* 439 */
	{127,10,127,0,L234},	/* 440 */


	{127,0,63,0,L235},	/* 441 */


	{16,8,8,0,L236},	/* 442 */
	{16,8,16,9,L236},	/* 443 */
	{16,11,8,0,L236},	/* 444 */
	{16,11,16,9,L236},	/* 445 */


	{16,8,16,8,L237},	/* 446 */
	{16,8,16,11,L237},	/* 447 */
	{16,11,16,8,L237},	/* 448 */
	{16,11,16,11,L237},	/* 449 */


	{16,8,127,8,L238},	/* 450 */
	{16,8,127,11,L238},	/* 451 */
	{16,11,127,8,L238},	/* 452 */
	{16,11,127,11,L238},	/* 453 */


	{16,8,63,8,L239},	/* 454 */
	{16,8,63,11,L239},	/* 455 */
	{16,11,63,8,L239},	/* 456 */
	{16,11,63,11,L239},	/* 457 */


	{127,8,8,0,L240},	/* 458 */
	{127,11,8,0,L240},	/* 459 */


	{127,8,16,8,L241},	/* 460 */
	{127,8,16,11,L241},	/* 461 */
	{127,11,16,8,L241},	/* 462 */
	{127,11,16,11,L241},	/* 463 */


	{84,8,63,8,L242},	/* 464 */
	{84,8,63,11,L242},	/* 465 */
	{84,11,63,8,L242},	/* 466 */
	{84,11,63,11,L242},	/* 467 */


	{127,8,63,8,L243},	/* 468 */
	{127,8,63,11,L243},	/* 469 */
	{127,11,63,8,L243},	/* 470 */
	{127,11,63,11,L243},	/* 471 */


/* =^ */
	{0},
/* easxor */
	{16,8,63,8,L244},	/* 473 */
	{16,8,63,11,L244},	/* 474 */
	{16,11,63,8,L244},	/* 475 */
	{16,11,63,11,L244},	/* 476 */


	{84,8,63,8,L245},	/* 477 */
	{84,8,63,11,L245},	/* 478 */
	{84,11,63,8,L245},	/* 479 */
	{84,11,63,11,L245},	/* 480 */


	{127,8,63,8,L246},	/* 481 */
	{127,8,63,11,L246},	/* 482 */
	{127,11,63,8,L246},	/* 483 */
	{127,11,63,11,L246},	/* 484 */
/* =+ for effect */
	{0},
/* easadd */
	{127,0,4,0,L247},	/* 486 */
	{16,0,4,0,L247},	/* 487 */
	{16,3,5,0,L247},	/* 488 */
	{16,10,5,0,L247},	/* 489 */
	{16,0,5,0,L247},	/* 490 */
	{16,1,16,1,L248},	/* 491 */


	{16,1,127,1,L249},	/* 492 */


	{16,1,63,0,L250},	/* 493 */


	{127,0,5,0,L251},	/* 494 */
	{127,10,5,0,L251},	/* 495 */


	{84,1,127,1,L252},	/* 496 */


	{16,0,84,1,L253},	/* 497 */
	{16,0,63,0,L254},	/* 498 */
	{16,10,63,0,L255},	/* 499 */
	{84,1,63,0,L256},	/* 500 */


	{127,1,63,0,L257},	/* 501 */


	{127,0,63,0,L258},	/* 502 */
	{127,10,63,0,L259},	/* 503 */
	{16,8,8,0,L260},	/* 504 */
	{16,8,16,9,L260},	/* 505 */
	{16,11,16,9,L260},	/* 506 */
	{16,11,8,0,L260},	/* 507 */


	{16,8,16,8,L261},	/* 508 */
	{16,8,16,11,L261},	/* 509 */
	{16,11,16,8,L261},	/* 510 */
	{16,11,16,11,L261},	/* 511 */


	{16,8,127,8,L262},	/* 512 */
	{16,8,127,11,L262},	/* 513 */
	{16,11,127,8,L262},	/* 514 */
	{16,11,127,11,L262},	/* 515 */


	{16,8,63,8,L263},	/* 516 */
	{16,8,63,11,L263},	/* 517 */
	{16,11,63,8,L263},	/* 518 */
	{16,11,63,11,L263},	/* 519 */


	{127,8,8,0,L264},	/* 520 */
	{127,8,16,9,L264},	/* 521 */
	{127,11,8,0,L264},	/* 522 */
	{127,11,16,9,L264},	/* 523 */


	{127,8,16,8,L265},	/* 524 */
	{127,8,16,11,L265},	/* 525 */
	{127,11,16,8,L265},	/* 526 */
	{127,11,16,11,L265},	/* 527 */


	{84,8,63,8,L266},	/* 528 */
	{84,8,63,11,L266},	/* 529 */
	{84,11,63,8,L266},	/* 530 */
	{84,11,63,11,L266},	/* 531 */


	{127,8,63,8,L267},	/* 532 */
	{127,8,63,11,L267},	/* 533 */
	{127,11,63,8,L267},	/* 534 */
	{127,11,63,11,L267},	/* 535 */


/* >>= (simple) */
	{0},
/* easrsh */
	{16,0,5,0,L268},	/* 537 */
	{16,10,5,0,L269},	/* 538 */
	{127,0,5,0,L270},	/* 539 */
	{127,10,5,0,L271},	/* 540 */
/* <<= */
	{0},
/* easlsh */
	{16,0,5,0,L272},	/* 542 */
	{16,10,5,0,L272},	/* 543 */
	{127,0,5,0,L273},	/* 544 */
	{127,10,5,0,L273},	/* 545 */
	{9,0,16,1,L274},	/* 546 */
	{9,0,127,1,L275},	/* 547 */
	{9,0,63,0,L276},	/* 548 */
/* <<= for longs */
	{0},
/* rlaslsh */
	{16,8,16,1,L277},	/* 550 */
	{16,11,16,1,L277},	/* 551 */
	{16,8,63,0,L278},	/* 552 */
	{16,11,63,0,L278},	/* 553 */
	{127,8,63,0,L279},	/* 554 */
	{127,11,63,0,L279},	/* 555 */
/* field = ... */
	{0},
/* efield */
	{16,0,16,0,L280},	/* 557 */
	{16,0,63,0,L281},	/* 558 */
	{127,0,16,0,L282},	/* 559 */
	{84,0,63,0,L283},	/* 560 */
	{127,0,20,0,L284},	/* 561 */
	{127,0,63,0,L285},	/* 562 */


/*
 * =============================================================================
 * ccmp: Comparison and relational operators for condition codes
 * =============================================================================
 * Generate code that sets Z80 flags for conditional branching.
 * These patterns compare two values and leave flags set for jp z/nz/c/nc/m/p.
 */
	{0},
/* ccmp */
	{16,0,4,0,L286},	/* 564 */
	{16,5,4,4,L286},	/* 565 */
	{16,10,4,0,L286},	/* 566 */
	{16,4,4,0,L287},	/* 567 */
	{127,0,4,0,L288},	/* 568 */
	{127,5,4,4,L288},	/* 569 */
	{127,10,4,0,L288},	/* 570 */
	{127,4,4,0,L289},	/* 571 */
	{63,0,4,0,L290},	/* 572 */
	{63,4,4,4,L290},	/* 573 */
	{16,1,16,1,L291},	/* 574 */
	{16,3,16,3,L291},	/* 575 */
	{16,10,16,0,L291},	/* 576 */
	{16,10,16,10,L291},	/* 577 */
	{127,1,16,1,L292},	/* 578 */
	{127,3,16,3,L292},	/* 579 */
	{127,10,16,10,L292},	/* 580 */
	{63,0,16,1,L293},	/* 581 */
	{63,4,16,5,L293},	/* 582 */


	{127,1,84,1,L294},	/* 583 */
	{127,3,84,3,L294},	/* 584 */
	{127,10,84,10,L294},	/* 585 */
	{127,1,20,0,L295},	/* 586 */
	{63,0,84,1,L296},	/* 587 */
	{63,4,84,5,L296},	/* 588 */


	{63,0,20,0,L297},	/* 589 */
	{63,4,20,4,L297},	/* 590 */


	{127,1,127,1,L298},	/* 591 */
	{127,3,127,3,L298},	/* 592 */
	{127,10,127,10,L298},	/* 593 */
	{127,1,63,0,L299},	/* 594 */
	{63,0,63,0,L300},	/* 595 */
	{63,4,63,4,L300},	/* 596 */


	{16,8,4,0,L301},	/* 597 */
	{16,11,4,0,L301},	/* 598 */
	{16,8,8,0,L302},	/* 599 */
	{16,8,16,9,L302},	/* 600 */
	{16,11,8,0,L302},	/* 601 */
	{16,11,16,9,L302},	/* 602 */
	{16,8,16,8,L303},	/* 603 */
	{16,8,16,11,L303},	/* 604 */
	{16,11,16,8,L303},	/* 605 */
	{16,11,16,11,L303},	/* 606 */
	{127,8,4,0,L304},	/* 607 */
	{127,11,4,0,L304},	/* 608 */
	{127,8,8,0,L305},	/* 609 */
	{127,11,8,0,L305},	/* 610 */
	{127,8,16,9,L305},	/* 611 */
	{127,11,16,9,L305},	/* 612 */
	{127,8,16,8,L306},	/* 613 */
	{127,8,16,11,L306},	/* 614 */
	{127,11,16,8,L306},	/* 615 */
	{127,8,16,11,L306},	/* 616 */
	{63,8,4,0,L307},	/* 617 */
	{63,11,4,0,L307},	/* 618 */
	{63,8,8,0,L308},	/* 619 */
	{63,11,8,0,L308},	/* 620 */
	{63,8,16,9,L308},	/* 621 */
	{63,11,16,9,L308},	/* 622 */
	{63,8,16,8,L309},	/* 623 */
	{63,8,16,11,L309},	/* 624 */
	{63,11,16,8,L309},	/* 625 */
	{63,11,16,11,L309},	/* 626 */
	{127,8,84,8,L310},	/* 627 */
	{127,8,84,11,L310},	/* 628 */
	{127,11,84,8,L310},	/* 629 */
	{127,11,84,11,L310},	/* 630 */
	{63,8,84,8,L311},	/* 631 */
	{63,8,84,11,L311},	/* 632 */
	{63,11,84,8,L311},	/* 633 */
	{63,11,84,11,L311},	/* 634 */
	{63,8,63,8,L312},	/* 635 */
	{63,8,63,11,L312},	/* 636 */
	{63,11,63,8,L312},	/* 637 */
	{63,11,63,11,L312},	/* 638 */
/* & as in "if ((a&b)==0)" */
	{0},
/* candtst */
	{16,0,16,0,L313},	/* 640 */
	{16,0,16,3,L313},	/* 641 */
	{16,0,16,10,L313},	/* 642 */
	{16,3,16,0,L313},	/* 643 */
	{16,3,16,3,L313},	/* 644 */
	{16,3,16,10,L313},	/* 645 */
	{16,10,16,0,L313},	/* 646 */
	{16,10,16,3,L313},	/* 647 */
	{16,10,16,10,L313},	/* 648 */
	{16,0,20,0,L314},	/* 649 */
	{16,10,20,0,L314},	/* 650 */
	{127,0,16,0,L315},	/* 651 */
	{127,9,16,0,L315},	/* 652 */
	{127,10,16,0,L315},	/* 653 */
	{63,0,16,0,L316},	/* 654 */


	{63,0,20,0,L317},	/* 655 */


	{63,0,63,0,L318},	/* 656 */


	{16,8,8,0,L319},	/* 657 */
	{16,11,8,0,L319},	/* 658 */
	{16,8,16,9,L319},	/* 659 */
	{16,11,16,9,L319},	/* 660 */
	{127,8,8,0,L320},	/* 661 */
	{127,11,8,0,L320},	/* 662 */
	{127,8,16,9,L320},	/* 663 */
	{127,11,16,9,L320},	/* 664 */
	{16,8,16,8,L321},	/* 665 */
	{16,8,16,11,L321},	/* 666 */
	{16,11,16,8,L321},	/* 667 */
	{16,11,16,11,L321},	/* 668 */


	{127,8,16,8,L322},	/* 669 */
	{127,8,16,11,L322},	/* 670 */
	{127,11,16,8,L322},	/* 671 */
	{127,11,16,11,L322},	/* 672 */


	{63,8,16,8,L323},	/* 673 */
	{63,8,16,11,L323},	/* 674 */
	{63,11,16,8,L323},	/* 675 */
	{63,11,16,11,L323},	/* 676 */


	{127,8,84,8,L324},	/* 677 */
	{127,8,84,11,L324},	/* 678 */
	{127,11,84,8,L324},	/* 679 */
	{127,11,84,11,L324},	/* 680 */


	{63,8,84,8,L325},	/* 681 */
	{63,8,84,11,L325},	/* 682 */
	{63,11,84,8,L325},	/* 683 */
	{63,11,84,11,L325},	/* 684 */


	{63,8,63,8,L326},	/* 685 */
	{63,8,63,11,L326},	/* 686 */
	{63,11,63,8,L326},	/* 687 */
	{63,11,63,11,L326},	/* 688 */


	{63,8,8,0,L327},	/* 689 */
	{63,11,8,0,L327},	/* 690 */
	{63,8,16,9,L327},	/* 691 */
	{63,11,16,9,L327},	/* 692 */
/* set codes right */
	{0},
/* rest */
	{63,0,63,0,L328},	/* 694 */
	{63,4,63,4,L328},	/* 695 */


/*
 * -----------------------------------------------------------------------------
 * sname - Push Named Value to Stack
 * -----------------------------------------------------------------------------
 * Handle NAME nodes (variables, constants) by loading value and pushing.
 * Used for function arguments and compound operations needing stack storage.
 *
 * Pattern types:
 *   %z,n    - Zero constant: push 0
 *   %aw,n   - Addressable word: load and push
 *   %aub,n  - Addressable unsigned byte: zero-extend and push as word
 *   %nw*,n  - Word through pointer: deref pointer, load word, push
 *   %al,n   - Addressable long: push high word, then low word (4 bytes total)
 * -----------------------------------------------------------------------------
 */
	{0},
/* sname */
/* Zero constant (int or float) - push 0 */
	{4,0,63,0,L329},	/* 697 */
	{4,4,63,0,L329},	/* 698 */
/* Addressable word variable - smart load and push */
	{16,1,63,0,L330},	/* 699 */
/* Addressable unsigned byte - zero-extend to word and push */
/* Result on stack is 16-bit with high byte = 0 */
	{16,10,63,0,L331},	/* 700 */
/* Word through pointer - eval pointer in HL, deref, push word */
/* Pattern: F* gives us pointer in HL, then we load word at (HL) */
	{127,1,63,0,L332},	/* 701 */
/* Addressable long (signed or unsigned) - push as 32-bit (2 words) */
/* Push order: high word first, then low word */
/* Stack layout after: [low_word] <- SP, [high_word] <- SP+2 */
	{16,8,63,0,L333},	/* 702 */
	{16,11,63,0,L333},	/* 703 */
/*
 * -----------------------------------------------------------------------------
 * sadd - Binary Arithmetic to Stack
 * -----------------------------------------------------------------------------
 * Evaluate binary operation (+, -, |, &~) and push result to stack.
 * Used when function argument is a computed expression, not just a variable.
 *
 * Opcode reuse: opcodes 40 (ADD), 41 (SUB), 55 (OR), 48 (AND_NOT) all route
 * here because the pattern is the same - only the 'I' instruction differs.
 *
 * Key optimization: %a,1 pattern uses I' (sp) to operate directly on stack
 * top without popping, very efficient for increment/decrement of arg.
 * -----------------------------------------------------------------------------
 */
	{0},
/* sadd */
/* Any + constant 1: use in-place increment on stack top */
/* FS pushes left operand, then I' operates on (sp) directly */
	{16,0,5,0,L334},	/* 705 */
/* Any + addressable word: push left, load right into DE, pop left, operate */
	{16,0,16,1,L335},	/* 706 */
/* Any + word through pointer: complex sequence to get both operands */
/* Push left, eval right pointer, push it, deref to get word, operate */
	{16,0,127,1,L336},	/* 707 */
/* General case: push left, eval right, pop left into DE, operate, push */
/* Uses ex (sp),hl to swap HL with stack top efficiently */
	{16,0,63,0,L337},	/* 708 */
/*
 * -----------------------------------------------------------------------------
 * sitol - Integer to Long, Push to Stack
 * -----------------------------------------------------------------------------
 * Convert 16-bit integer to 32-bit long and push both words to stack.
 * Used when passing int to function expecting long argument.
 *
 * Sign extension for signed:
 *   - RLA rotates sign bit into carry
 *   - SBC A,A produces 0x00 (positive) or 0xFF (negative)
 *   - High word is sign-extended copy
 *
 * Stack layout after: [low_word] <- SP, [high_word] <- SP+2
 * -----------------------------------------------------------------------------
 */
	{0},
/* sitol */
/* Unsigned integer: push already on stack, just add zero high word */
	{63,9,63,0,L338},	/* 710 */
/* Signed word: load, sign-extend to 32 bits, push both words */
/* RLA gets sign bit into carry, SBC A,A gives 0x00 or 0xFF */
	{16,1,63,0,L339},	/* 711 */
/*
 * -----------------------------------------------------------------------------
 * sftol - Float to Long, Push to Stack
 * -----------------------------------------------------------------------------
 * Convert 32-bit float to 32-bit long and push result.
 * Uses ftol helper function which returns result in DE:HL (high:low).
 * Float is already 32 bits, so this is really a type conversion.
 * -----------------------------------------------------------------------------
 */
	{0},
/* sftol */
/* Float expression: evaluate, call ftol helper, push 32-bit result */
	{63,4,63,0,L340},	/* 713 */
/*
 * -----------------------------------------------------------------------------
 * estrasg - Structure Assignment Setup
 * -----------------------------------------------------------------------------
 * Prepare for structure assignment by loading source address.
 * The F! output code loads the first operand's ADDRESS (not value).
 * Used before block copy operations like LDIR.
 *
 * Note: This is in efftab (effect table) despite the 's' prefix label,
 * because structure assignment is done for side effect, not value.
 * The label name 'estrasg' reflects its purpose (structure assignment).
 * -----------------------------------------------------------------------------
 */
	{0},
/* estrasg */
/* Left is source struct, right is constant size: load addresses for LDIR */
/* F! loads source address, S1! loads dest address and size */
	{63,0,20,0,L341},	/* 715 */
/* Left is source, right is expression (computed size): push dest, load source */
/* SS pushes right operand (size or dest), F! loads source address */
	{63,0,63,0,L342},	/* 716 */
	{0},
};

/*
 * =============================================================================
 * OPTABLE - Z80 Code Generation Tables for Ritchie C Compiler Pass2
 * =============================================================================
 *
 * This file defines pattern-matching rules that drive code generation.
 * cvopt.c processes this file into table.c with encoded patterns.
 *
 * REGISTER MODEL:
 * ---------------
 *   R  (HL)  - Primary accumulator. All expression results end up here.
 *              Also used for function return values (16-bit).
 *   R1 (DE)  - Secondary accumulator. Used for right operand of binops,
 *              or high word of 32-bit values.
 *   R+ (DE)  - High word of long. Longs are stored as HLDE pair:
 *              HL = low 16 bits, DE = high 16 bits.
 *   IY       - Frame pointer. Points to saved IY on stack.
 *              Locals at negative offsets (iy-2, iy-4, ...),
 *              Args at positive offsets (iy+4, iy+6, ...).
 *   BC, IX   - Register variables. Compiler allocates frequently-used
 *              locals to these registers for speed.
 *   SP       - Stack pointer. Grows downward.
 *   A        - Byte accumulator for 8-bit operations.
 *
 * PATTERN SYNTAX - MATCH PREDICATES:
 * ----------------------------------
 * Each rule starts with %<op1>,<op2> defining what operand types match.
 *
 * Operand class specifiers:
 *   z  - Zero constant (literal 0)
 *   c  - Constant (any compile-time constant)
 *   1  - Literal constant 1 (for inc/dec optimization)
 *   a  - Addressible (symbol, stack var, static - can be accessed directly)
 *   e  - Expression in register (already evaluated, result in HL or DE)
 *   r  - Register variable (BC or IX)
 *   n  - aNy (matches everything - wildcard)
 *
 * Type modifiers (follow class specifier):
 *   w  - Word (16-bit int)
 *   b  - Byte (8-bit signed char)
 *   ub - Unsigned byte
 *   l  - Long (32-bit signed)
 *   ul - Unsigned long
 *   f  - Float (32-bit IEEE 754)
 *   d  - Double (same as float in this compiler)
 *   u  - Unsigned (16-bit)
 *   p  - Pointer (adds 16 to type code)
 *
 * Indirection:
 *   *  - Pointer that needs dereferencing. The operand is a pointer,
 *        and we need to load through it to get the actual value.
 *
 * Examples:
 *   %a,n     - Addressible op1, any op2
 *   %aw,aw   - Addressible word op1, addressible word op2
 *   %n*,z    - Any pointer needing deref op1, zero op2
 *   %nub*,n  - Any unsigned-byte pointer needing deref, any op2
 *   %al,nl   - Addressible long, any long
 *
 * PATTERN SYNTAX - OUTPUT CODES:
 * ------------------------------
 * After the match line, indented lines are the output pattern.
 *
 * Operand substitution:
 *   A1  - Emit addressified form of operand 1 (symbol name, (iy+n), etc)
 *   A2  - Emit addressified form of operand 2
 *   R   - Primary register name "hl"
 *   R1  - Secondary register name "de"
 *   I   - Emit instruction from instab (the opcode for this operator)
 *   I'  - Emit instruction with ' variant (e.g., inc vs dec)
 *   Z   - Emit field mask (for bitfield operations)
 *
 * Subtree evaluation:
 *   F   - Evaluate First (left) subtree, result in HL
 *   S   - Evaluate Second (right) subtree, result in HL
 *   F*  - Evaluate First subtree (pointer), then dereference
 *   S*  - Evaluate Second subtree (pointer), then dereference
 *   FS  - Evaluate First subtree for Store (push address for later store)
 *   FS* - Evaluate First subtree pointer, push for later store
 *   SS  - Evaluate Second subtree, push result, continue
 *   F1  - Evaluate First subtree with op1 context
 *   S1  - Evaluate Second subtree with op1 context, result in DE
 *   F1* - Evaluate First subtree pointer with op1 context
 *   S1* - Evaluate Second subtree pointer with op1 context
 *   FC  - Evaluate First subtree for Condition codes
 *   H   - Evaluate for condition codes (cctab handler)
 *
 * Z80-specific smart macros (expanded by c10.c):
 *   Q1  - Smart word load from op1 to HL (handles IY+n, statics, etc)
 *   Q2  - Smart word load from op2 to HL
 *   QD1 - Smart word load from op1 to DE
 *   QD2 - Smart word load from op2 to DE
 *   P1  - Smart word store from HL to op1
 *   P2  - Smart word store from HL to op2
 *   PD1 - Smart word store from DE to op1
 *   PD2 - Smart word store from DE to op2
 *   LA1 - Load Address of op1 into HL
 *   LA2 - Load Address of op2 into HL
 *
 * Branch/jump handling:
 *   X0  - First part of comparison (may short-circuit)
 *   X1  - Second part of comparison
 *   V   - Propagate carry for multi-word arithmetic
 *
 * Labels:
 *   %[name:] - Define a named pattern that can be referenced
 *   %[name]  - Reference a previously defined pattern
 *   name:    - Define a dispatch target label
 *
 * IMPORTANT: Labels must use LOWERCASE letters only!
 * Uppercase letters (A,B,C,F,H,I,L,P,Q,R,S,X,Y,T) are interpreted as
 * output codes by cvopt, so they get consumed instead of added to the
 * label name. Use: rname, radd, rasadd (not rNAME, rADD, rASADD)
 *
 * TABLES:
 * -------
 * This file defines four tables:
 *   regtab  - Generate code for value (result in HL)
 *   efftab  - Generate code for side effect only (result discarded)
 *   cctab   - Generate code and set condition codes for branching
 *   sptab   - Generate code and push result to stack
 *
 * OPERATOR NUMBERS (from c1.h):
 * -----------------------------
 *   14  PTOI    - Pointer difference divided by size
 *   16  FSEL    - Field select (bitfield assignment)
 *   17  DIVIDE  - Integer division
 *   18  MOD     - Integer modulo
 *   28  QUEST   - Ternary operator ?:
 *   30  PREINC  - Prefix ++
 *   31  PREDEC  - Prefix --
 *   32  POSTINC - Postfix ++
 *   33  POSTDEC - Postfix --
 *   34  EXCLA   - Logical NOT !
 *   35  AMPER   - Address-of &
 *   36  STAR    - Dereference *
 *   37  NEG     - Unary minus -
 *   38  COMPL   - Bitwise complement ~
 *   40  PLUS    - Addition +
 *   41  MINUS   - Subtraction -
 *   42  TIMES   - Multiplication *
 *   43  DIVIDE  - Division /
 *   44  MOD     - Modulo %
 *   45  RSHIFT  - Right shift >>
 *   46  LSHIFT  - Left shift <<
 *   48  AND     - Bitwise AND &
 *   49  XOR     - Bitwise XOR ^
 *   51  ITOF    - Int to float conversion
 *   52  FTOI    - Float to int conversion
 *   55  OR      - Bitwise OR |
 *   56  FTOL    - Float to long conversion
 *   57  LTOF    - Long to float conversion
 *   58  ITOL    - Int to long conversion
 *   59  LTOI    - Long to int conversion
 *   60-69       - Comparison operators (==, !=, <, <=, >, >=, signed/unsigned)
 *   70  ASPLUS  - Compound assignment +=
 *   71  ASMINUS - Compound assignment -=
 *   72  ASTIMES - Compound assignment *=
 *   73  ASDIV   - Compound assignment /=
 *   74  ASMOD   - Compound assignment %=
 *   75  ASRSH   - Compound assignment >>=
 *   76  ASLSH   - Compound assignment <<=
 *   78  ASOR    - Compound assignment |=
 *   79  ASXOR   - Compound assignment ^=
 *   80  ASSIGN  - Simple assignment =
 *   81  ANDAND  - Logical AND && (for condition code testing)
 *   82-84       - Long multiply, divide, mod
 *   85  ASAND   - Compound assignment &=
 *   86-88       - Long compound assignment *=, /=, %=
 *   91  LSHIFT (long) - Long left shift
 *   92  ASLSH (long)  - Long compound <<=
 *   98  MCALL   - Member function call
 *   99  CALL    - Function call
 *   102 JUMP    - Goto
 *   106 NAME    - Load name/constant into register
 *   107 PTRDIFF - Pointer difference (int*)-(int*) -> int
 *   109 ITOC    - Int to char conversion (sign extend)
 *   116 STRASG  - Structure assignment setup
 *   117-118     - Unsigned divide/mod
 *   119-120     - Unsigned compound /=, %=
 *   121-126     - Unsigned long *, /, %, *=, /=, %=
 *   127 ULTOF   - Unsigned long to float
 *   128 LUSHR   - Unsigned long right shift
 *   129 ALUSHR  - Unsigned long compound >>=
 */

/*
 * REGTAB - Generate expression value into HL register
 * ----------------------------------------------------
 * This is the main code generation table. Each entry maps an operator
 * number to a code generation rule. The result ends up in HL (or HLDE
 * for 32-bit values).
 */
struct table regtab[] = {
	{LOAD,rname},	/* NAME - load value into register */
	{INCBEF,rasadd},	/* PREINC ++ - uses compound assignment patterns */
	{DECBEF,rasadd},	/* PREDEC -- - uses compound assignment patterns */
	{INCAFT,rpostinc},	/* POSTINC ++ - return old value, then increment */
	{DECAFT,rpostinc},	/* POSTDEC -- - return old value, then decrement */
	{NEG,runary},	/* NEG - unary minus */
	{COMPL,runary},	/* COMPL ~ - bitwise complement */
	{CALL1,rcall},	/* MCALL - member function call */
	{CALL2,rcall},	/* CALL - function call */
	{ASSIGN,rassign},	/* ASSIGN = - simple assignment */
	{PLUS,radd},	/* PLUS + - addition */
	{MINUS,radd},	/* MINUS - - subtraction (uses same patterns as +) */
	{TIMES,rmul},	/* TIMES * - multiplication */
	{DIVIDE,rdiv},	/* DIVIDE / - signed division */
	{PTOI,rptoi},	/* PTOI - pointer difference / sizeof */
	{MOD,rdiv},	/* MOD % - modulo (uses divide patterns) */
	{RSHIFT,rrsh},	/* RSHIFT >> - signed right shift */
	{LSHIFT,radd},	/* LSHIFT << - left shift (uses add patterns) */
	{ANDN,radd},	/* OR | - bitwise or (uses add patterns) */
	{OR,radd},	/* AND & - bitwise and (uses add patterns) */
	{EXOR,rxor},	/* XOR ^ - bitwise xor */
	{ASPLUS,rasadd},	/* ASPLUS += - compound add assignment */
	{ASMINUS,rasadd},	/* ASMINUS -= - compound subtract assignment */
	{ASTIMES,rasmul},	/* ASTIMES *= - compound multiply assignment */
	{ASDIV,rasdiv},	/* ASDIV /= - compound divide assignment */
	{ASMOD,rasmod},	/* ASMOD %= - compound modulo assignment */
	{ASRSH,rasrsh},	/* ASRSH >>= - compound right shift assignment */
	{ASLSH,rasmul},	/* ASLSH <<= - compound left shift assignment */
	{ASOR,rasor},	/* ASOR |= - compound or assignment */
	{ASANDN,rasor},	/* ASAND &= - compound and assignment */
	{ASXOR,rasxor},	/* ASXOR ^= - compound xor assignment */
	{JUMP,rjump},	/* JUMP - goto statement */
	{ITOF,ritof},	/* ITOF - int to float conversion */
	{FTOI,rftoi},	/* FTOI - float to int conversion */
	{FTOL,rftol},	/* FTOL - float to long conversion */
	{LTOF,rltof},	/* LTOF - long to float conversion */
	{ITOL,ritol},	/* ITOL - int to long conversion */
	{LTOI,rltoi},	/* LTOI - long to int conversion */
	{LLSHIFT,rlshl},	/* LSHIFT (long) - 32-bit left shift */
	{LTIMES,rlmul},	/* LTIMES - long multiply */
	{LDIV,rlmul},	/* LDIV - long divide */
	{LMOD,rlmul},	/* LMOD - long modulo */
	{LASTIMES,rlasmul},	/* LASTIMES - long compound *= */
	{LASDIV,rlasmul},	/* LASDIV - long compound /= */
	{LASMOD,rlasmul},	/* LASMOD - long compound %= */
	{FSELA,rfield},	/* FSEL - bitfield assignment */
	{ASLSHL,rlaslsh},	/* ASLSH (long) - long compound <<= */
	{ULSH,rdiv},	/* DIVIDE - uses same as rdiv */
	{ASULSH,rasmod},	/* MOD - uses same as rasmod */
	{ITOC,ritoc},	/* ITOC - int to char (sign extend low byte) */
	{UDIV,rudiv},	/* UDIV - unsigned divide */
	{UMOD,rudiv},	/* UMOD - unsigned modulo */
	{ASUDIV,ruasdiv},	/* UASDIV - unsigned compound /= */
	{ASUMOD,ruasdiv},	/* UASMOD - unsigned compound %= */
	{PTOI1,rptrdif},	/* PTRDIFF - (int*)-(int*) for int pointers */
	{ULTIMES,rulmul},	/* ULTIMES - unsigned long multiply */
	{ULDIV,rulmul},	/* ULDIV - unsigned long divide */
	{ULMOD,rulmul},	/* ULMOD - unsigned long modulo */
	{ULASTIMES,rulasmul},	/* ULASTIMES - unsigned long compound *= */
	{ULASDIV,rulasmul},	/* ULASDIV - unsigned long compound /= */
	{ULASMOD,rulasmul},	/* ULASMOD - unsigned long compound %= */
	{ULTOF,rultof},	/* ULTOF - unsigned long to float */
	{ULLSHIFT,rlushr},	/* LUSHR - unsigned long right shift */
	{UASLSHL,ralushr},	/* ALUSHR - unsigned long compound >>= */
	{0}		/* End of table marker */
};

/*
 * =============================================================================
 * EFFTAB - Generate code for SIDE EFFECTS only (result discarded)
 * =============================================================================
 * When an expression appears in a statement context where its value is
 * discarded (e.g., "x = 5;" as a statement, not "y = (x = 5)"), we can
 * generate simpler code that doesn't bother leaving a result in HL.
 *
 * Examples:
 *   x++;           // just increment, don't need result
 *   x = y;         // just store, don't need to return value
 *   x += y;        // just add and store
 *
 * These patterns are often simpler than their regtab counterparts because
 * they don't need to preserve the result.
 *
 * Label naming convention:
 *   eXXX  = "effect" version of operator XXX
 *   [moveN:] = named patterns for different assignment scenarios
 */
struct table efftab[] = {
	{INCBEF,easadd},	/* PREINC ++ */
	{DECBEF,easadd},	/* PREDEC -- */
	{INCAFT,easadd},	/* POSTINC ++ */
	{DECAFT,easadd},	/* POSTDEC -- */
	{ASSIGN,eassign},	/* ASSIGN = */
	{ASPLUS,easadd},	/* ASPLUS += */
	{ASMINUS,easadd},	/* ASMINUS -= */
	{ASOR,easor},	/* ASOR |= */
	{ASXOR,easxor},	/* ASXOR ^= */
	{ASANDN,easor},	/* ASAND &= */
	{ASRSH,easrsh},	/* ASRSH >>= */
	{ASLSH,easlsh},	/* ASLSH <<= */
	{FSELA,efield},	/* FSEL - bitfield */
	{STRSET,estrasg},	/* STRASG - struct assignment */
	{0}
};

/*
 * =============================================================================
 * CCTAB - Generate code that SETS CONDITION CODES for branching
 * =============================================================================
 * Used in conditional contexts: if(), while(), for(), ?:
 *
 * The generated code must set the Z80 flags appropriately:
 *   - Zero flag (Z): set if result is zero
 *   - Sign flag (S): set if result is negative
 *   - Carry flag (C): set for unsigned comparisons
 *
 * After these patterns execute, the caller uses conditional jumps:
 *   jp z,label   - jump if zero/equal
 *   jp nz,label  - jump if not zero/not equal
 *   jp c,label   - jump if carry (unsigned less than)
 *   jp m,label   - jump if minus (signed negative)
 *
 * For comparisons (==, !=, <, <=, >, >=), the pattern does:
 *   or a        ; clear carry
 *   sbc hl,de   ; subtract, setting flags
 *
 * Label naming convention:
 *   cXXX = "condition code" version of operator XXX
 *   rest = fallback - just evaluate expression and set codes
 *
 * Long comparisons use X0/X1 for short-circuit evaluation:
 *   Compare low words first (X0), if equal compare high words (X1).
 */
struct table cctab[] = {
	{LOAD,ccmp},	/* NAME - compare/test value */
	{AUTOD,rest},	/* QUEST ?: - evaluate for condition */
	{ANDN,rest},	/* OR | - evaluate for condition */
	{EXCLA,rest},	/* EXCLA ! - evaluate for condition */
	{AMPER,rest},	/* AMPER & - evaluate for condition */
	{STAR,rest},	/* STAR * - evaluate for condition */
	{NEG,rest},	/* NEG - - evaluate for condition */
	{PLUS,rest},	/* PLUS + - evaluate for condition */
	{MINUS,rest},	/* MINUS - - evaluate for condition */
	{DIVIDE,rest},	/* DIVIDE / - evaluate for condition */
	{TAND,candtst},	/* ANDAND && - bitwise AND test (a&b)==0 */
	{OR,rest},	/* AND & - evaluate for condition */
	{EQUAL,ccmp},	/* EQ == */
	{NEQUAL,ccmp},	/* NE != */
	{LESSEQ,ccmp},	/* LT < (signed) */
	{LESS,ccmp},	/* LE <= (signed) */
	{GREATEQ,ccmp},	/* GT > (signed) */
	{GREAT,ccmp},	/* GE >= (signed) */
	{LESSEQP,ccmp},	/* LO < (unsigned) */
	{LESSP,ccmp},	/* LS <= (unsigned) */
	{GREATQP,ccmp},	/* HI > (unsigned) */
	{GREATP,ccmp},	/* HS >= (unsigned) */
	{ASTIMES,rest},	/* ASTIMES *= - evaluate for condition */
	{ASDIV,rest},	/* ASDIV /= - evaluate for condition */
	{ASXOR,rest},	/* ASXOR ^= - evaluate for condition */
	{0}
};

/*
 * =============================================================================
 * SPTAB - STACK PUSH TABLES
 * =============================================================================
 *
 * Purpose: Generate code to evaluate an expression and push the result onto
 * the stack. This is used for:
 *   - Function arguments (pushed right-to-left before CALL)
 *   - Compound operations that need to save intermediate values
 *   - Long (32-bit) operations that use stack-based helpers
 *
 * How sptab differs from regtab:
 *   - regtab: evaluate expression, leave result in HL (or A for bytes)
 *   - sptab: evaluate expression, result ends up on stack (via PUSH HL)
 *
 * Stack Growth:
 *   Z80 stack grows downward (SP decreases on PUSH)
 *   PUSH HL: SP -= 2, then store HL at (SP)
 *   For 32-bit values: push high word first, then low word
 *   Result: low word at lower address (little-endian in memory)
 *
 * Dispatch Table Opcodes:
 *   106 = NAME     - Push named variable or constant
 *   40  = ADD      - Push result of addition (also -, |, &~)
 *   41  = SUB      - Push result of subtraction
 *   55  = OR       - Push result of bitwise OR
 *   48  = AND_NOT  - Push result of AND-NOT
 *   58  = ITOL     - Convert int to long, push 32-bit result
 *   56  = FTOL     - Convert float to long, push 32-bit result
 *
 * Label Naming:
 *   sname  - Push named value (variable, constant)
 *   sadd   - Push binary arithmetic result
 *   sitol  - Push int converted to long (4 bytes)
 *   sftol  - Push float as long (reinterpret bits)
 *
 * Common Patterns:
 *   Q1; push hl           - Load word, push it
 *   ld hl,(A1+2); push hl; Q1; push hl  - Load 32-bit, push both words
 *   FS; I' (sp)           - Eval both, apply op to stack top
 *
 * Note: Most expressions go through regtab with explicit PUSH added by caller.
 * sptab provides optimized patterns for common "evaluate and push" sequences.
 * =============================================================================
 */

struct table sptab[] = {
	{LOAD,sname},
	{PLUS,sadd},
	{MINUS,sadd},
	{ANDN,sadd},
	{OR,sadd},
	{ITOL,sitol},
	{FTOL,sftol},
	{0}
};
