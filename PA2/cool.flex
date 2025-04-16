/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
char k_unterminated_string_constant[] = "Unterminated string constant";
char k_string_contain_null_character[] = "String contains null character";
char k_string_constant_too_long[] = "String constant too long";
char k_eof_in_string_constant[] = "EOF in string constant";
char k_eof_in_comment[] = "EOF in comment";
char k_unmatched_close_comment[] = "Unmatched *)";

bool is_string_error = false;

#define CHECK_STRING_SPACE(n) do { \
			if (string_buf_ptr - string_buf + n > MAX_STR_CONST) { \
				cool_yylval.error_msg = k_string_constant_too_long; \
				return ERROR;										\
			}														\
		} while(0)

%}

/*
 * Define names for regular expressions here.
 */

DARROW      =>
LE			<=
ASSIGN		<-
BITNOT		~
LT			<
EQUAL		=
AT			@
LCURL		"{"
RCURL		"}"
LPARE		"("
RPARE		")"
DOT			"."
COMMA		,
COLON		:
SEMICOLON	;
ADD			"+"
SUB			"-"
MUL			"*"
DIV			"/"

DIGIT		[0-9]
LETTER		[a-zA-Z]
US			_
WS			[ \f\r\t\v]
LF			\n

NUMBER		{DIGIT}+
OBJ_ID		[a-z]({LETTER}|{US}|{DIGIT})*
TYPE_ID		[A-Z]({LETTER}|{US}|{DIGIT})*

SELF_ID		self
SELF_TYPE_ID	SELF_TYPE

 /*keywords*/
CLASS		(?i:class)
IF			(?i:if)
ELSE		(?i:else)
FI			(?i:fi)
IN			(?i:in)
INHERITS	(?i:inherits)
LET			(?i:let)
LOOP		(?i:loop)
POOL		(?i:pool)
THEN		(?i:then)
WHILE		(?i:while)
CASE		(?i:case)
ESAC		(?i:esac)
OF			(?i:of)
NEW			(?i:new)
ISVOID		(?i:isvoid)
NOT			(?i:not)



	/*STR_CONST, INT_CONST, BOOL_CONST, TYPEID, OBJECTID, ERROR, LET_STMT*/

%x	STRING COMMENT1	COMMENT2
%option stack noyywrap

%%


 /*
  *  Nested comments
  */

"--"		{BEGIN(COMMENT1);}

<INITIAL>"(*"		{yy_push_state(COMMENT2);}

<COMMENT1>{
[^\n]*
\n		{BEGIN(INITIAL); curr_lineno++;}
<<EOF>>	{BEGIN(INITIAL); return 0;}
}

<COMMENT2>{
[^(*\n]*			
\n				{curr_lineno++;}
"("+[^(*\n]*	
"("+"*"			{yy_push_state(COMMENT2);}
"*"+[^*)\n]*
"*"+")" 		{yy_pop_state();}
<<EOF>> 		{
					cool_yylval.error_msg = k_eof_in_comment;
					yy_pop_state();
					return ERROR;
				}
}


 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }
{LE}			{ return (LE); }
{ASSIGN}		{ return (ASSIGN); }

{LT}		{ return '<';}
{EQUAL}		{ return '=';}
{AT}		{ return '@';}
{LCURL}		{ return '{';}
{RCURL}		{ return '}';}
{LPARE}		{ return '(';}
{RPARE}		{ return ')';}
{DOT}		{ return '.';}
{COMMA}		{ return ',';}
{COLON}		{ return ':';}
{SEMICOLON}	{ return ';';}
{ADD}		{ return '+';}
{SUB}		{ return '-';}
{MUL}		{ return '*';}
{DIV}		{ return '/';}
{BITNOT}	{ return '~';}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{CLASS}		{return CLASS;}
{IF}		{return IF;}
{ELSE}		{return ELSE;}
{FI}		{return FI;}
{IN}		{return IN;}
{INHERITS}	{return INHERITS;}
{LET}		{return LET;}
{LOOP}		{return LOOP;}
{POOL}		{return POOL;}
{THEN}		{return THEN;}
{WHILE}		{return WHILE;}
{CASE}		{return CASE;}
{ESAC}		{return ESAC;}
{OF}		{return OF;}
{NEW}		{return NEW;}
{ISVOID}	{return ISVOID;}
{NOT}		{return NOT;}


t(?i:rue)	{cool_yylval.boolean = 1; return BOOL_CONST;}
f(?i:alse)	{cool_yylval.boolean = 0; return BOOL_CONST;}

{SELF_ID}		{cool_yylval.symbol = idtable.add_string(yytext); return OBJECTID;}
{SELF_TYPE_ID}	{cool_yylval.symbol = idtable.add_string(yytext); return TYPEID;}

{NUMBER}	{cool_yylval.symbol = inttable.add_string(yytext);	return INT_CONST;}
{TYPE_ID}	{cool_yylval.symbol = idtable.add_string(yytext);	return TYPEID;}
{OBJ_ID}	{cool_yylval.symbol = idtable.add_string(yytext);	return OBJECTID;}

{WS}+		
{LF}+		{curr_lineno = curr_lineno + yyleng;}
 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
  
\"		{
			string_buf_ptr = string_buf; 
			is_string_error = false;
			BEGIN(STRING);
		}

<STRING>{
 /* Regular cases */ 
[^"\\\n\0] {
			CHECK_STRING_SPACE(yyleng);

            char *yptr = yytext;
            while (*yptr) {
                *string_buf_ptr++ = *yptr++;
            }
        }

\"      {   
            BEGIN(INITIAL);
			CHECK_STRING_SPACE(1);
			*string_buf_ptr++ = '\0';

			cool_yylval.symbol = stringtable.add_string(string_buf);
			return is_string_error ? ERROR : STR_CONST; 
        }   
\n      {   
			cool_yylval.error_msg = k_unterminated_string_constant;
            BEGIN(INITIAL);
            return ERROR;
        }   

\0		{
			*string_buf_ptr++ = '\0';
			cool_yylval.error_msg = k_string_contain_null_character;
			is_string_error = true;
		}

"\\n"    { CHECK_STRING_SPACE(1); *string_buf_ptr++ = '\n'; }
"\\t"     { CHECK_STRING_SPACE(1); *string_buf_ptr++ = '\t'; }
"\\b"     { CHECK_STRING_SPACE(1); *string_buf_ptr++ = '\b'; }
"\\f"     { CHECK_STRING_SPACE(1); *string_buf_ptr++ = '\f'; }
"\\\n"	{ CHECK_STRING_SPACE(1); *string_buf_ptr++ = yytext[1]; curr_lineno++; }
"\\\0"	{ 
			*string_buf_ptr++ = '\0';
			cool_yylval.error_msg = k_string_contain_null_character;
			is_string_error = true;
		}
\\[^\n\0] { CHECK_STRING_SPACE(1); *string_buf_ptr++ = yytext[1]; }



<<EOF>>	{
			cool_yylval.error_msg = k_eof_in_string_constant;
            BEGIN(INITIAL);
			return ERROR;
		}

}

"*)"	{
			cool_yylval.error_msg = k_unmatched_close_comment;
			return ERROR;
		}
	
.		{	
			cool_yylval.error_msg = yytext;
			return ERROR;
		}

%%
