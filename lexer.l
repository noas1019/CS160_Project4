%option yylineno
%pointer

%{
    #include <stdlib.h>
    #include <stdio.h>
    #include <string.h>
    #include <errno.h>
    #include <limits.h>
    #include "ast.hpp"
    #include "parser.hpp"
    void yyerror(const char *);
%}

%x COMMENT
%%

"/*"              BEGIN(COMMENT);
<COMMENT>[^<<EOF>>]  
<COMMENT><<EOF>>  { yyerror("dangling comment"); }
<COMMENT>"*/"     BEGIN(INITIAL); 

  /* WRITEME: Write your Flex token rules here */
"->"              { return T_ARROW; }
";"               { return T_SEMICOLON; }
"."               { return T_DOT; }
"{"               { return T_OPENBRACKET; }
"}"               { return T_CLOSEBRACKET; }
"("               { return T_OPENPAREN; }
")"               { return T_CLOSEPAREN; }
","               { return T_COMMA; }
"not"             { return T_NOT; }
"*"               { return T_MULT; }
"/"               { return T_DIV; }
"+"               { return T_PLUS; }
"-"               { return T_MINUS; }
">="              { return T_GREATEREQUAL; }
">"               { return T_GREATER; }
"="               { return T_EQUAL; }
"and"             { return T_AND; }
"or"              { return T_OR; }
"print"           { return T_PRINT; }
"return"          { return T_RETURN; }
"if"              { return T_IF; }
"else"            { return T_ELSE; }
"while"           { return T_WHILE; }
"new"             { return T_NEW; }
"integer"         { return T_INTEGER; }
"boolean"         { return T_BOOLEAN; }
"none"            { return T_NONE; }
"equals"          { return T_EQUALS; }
"true"            { yylval.base_int = 1; return T_TRUE; }
"false"           { yylval.base_int = 0; return T_FALSE; }
"extends"         { return T_EXTENDS; }
"do"              { return T_DO; }
[a-zA-Z][a-zA-Z0-9]* {  yylval.base_char_ptr = new char(); strcpy(yylval.base_char_ptr, yytext); return T_IDENTIFIER; }
[0-9]+\.?[0-9]+   { return T_LITERAL; }
[1-9][0-9]*|0     { yylval.base_int = std::atoi(yytext); return T_LITERAL; }
[\t\n]            ;
" "               ;
.                 { yyerror("invalid character"); }
%%

int yywrap(void) {
  return 1;
}
