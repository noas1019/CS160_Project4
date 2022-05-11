%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <iostream>

    #include "ast.hpp"
    
    #define YYDEBUG 1
    #define YYINITDEPTH 10000
    
    int yylex(void);
    void yyerror(const char *);
    
    extern ASTNode* astRoot;
%}

%error-verbose

/* WRITEME: List all your tokens here */
%token T_IDENTIFIER T_LITERAL
%token T_ARROW T_SEMICOLON T_DOT T_OPENBRACKET T_CLOSEBRACKET T_OPENPAREN T_CLOSEPAREN T_COMMA T_NOT T_MULT T_DIV T_PLUS T_MINUS T_GREATER T_GREATEREQUAL T_EQUAL
%token T_AND T_OR T_PRINT T_RETURN T_IF T_ELSE T_WHILE T_NEW T_INTEGER T_BOOLEAN T_NONE T_EQUALS T_TRUE T_FALSE T_EXTENDS T_DO
%token T_UNARY

/* WRITEME: Specify precedence here */
%right T_NOT T_UNARY
%left T_MULT T_DIV
%left T_PLUS T_MINUS
%left T_GREATER T_GREATEREQUAL T_EQUALS
%left T_AND
%left T_OR

%type <program_ptr> Start
%type <> ClassName
%type <> ClassBody
%type <type_ptr> Type
%type <> Members
%type <> MembersPrime
%type <> Methods
%type <> MethodsPrime
%type <parameter_ptr> Parameters
%type <parameter_list_ptr> ParametersPrime
%type <> Body
%type <returnstatement_ptr> Return
%type <> Declarations
%type <> DeclarationsPrime
%type <> DeclarationsList
%type <> StatementsList
%type <> Statements
%type <> Blocks
%type <> BlocksPrime
%type <expression_ptr> Expression
%type <methodcall_ptr> MethodCall
%type <> Arguments
%type <> ArgumentsPrime
%type <> T_LITERAL
%type <identifier_ptr> T_IDENTIFIER

%%

/* WRITEME: This rule is a placeholder, since Bison requires
            at least one rule to run successfully. Replace
            this with your appropriate start rules. */
Start : ClassName { $$ = $1; astRoot = $$; }
      ;

/* WRITME: Write your Bison grammar specification here */
ClassName : ClassName T_IDENTIFIER T_EXTENDS T_IDENTIFIER T_OPENBRACKET ClassBody T_CLOSEBRACKET
          | ClassName T_IDENTIFIER T_OPENBRACKET ClassBody T_CLOSEBRACKET
          | 
          %empty
          ;

ClassBody : Members MembersPrime
          | Methods MethodsPrime
          | Members MembersPrime Methods MethodsPrime
          |
          %empty
          ;

Type : T_INTEGER
     | T_BOOLEAN
     | T_IDENTIFIER
     | T_NONE
     ;

Members : Type T_IDENTIFIER T_SEMICOLON
        ;

MembersPrime : MembersPrime Type T_IDENTIFIER T_SEMICOLON
             |
             %empty
             ;

Methods : T_IDENTIFIER T_OPENPAREN Parameters T_CLOSEPAREN T_ARROW Type T_OPENBRACKET Body T_CLOSEBRACKET
        ;

MethodsPrime : MethodsPrime T_IDENTIFIER T_OPENPAREN Parameters T_CLOSEPAREN T_ARROW Type T_OPENBRACKET Body T_CLOSEBRACKET
             |
             %empty
             ;

Parameters : Type T_IDENTIFIER ParametersPrime
           | 
           %empty
           ;

ParametersPrime : ParametersPrime T_COMMA Type T_IDENTIFIER
                |
                %empty
                ;

Body : Return
     | Declarations DeclarationsList Return
     | Statements StatementsList Return
     | Declarations DeclarationsList Statements StatementsList Return
     ;

Return : T_RETURN Expression T_SEMICOLON
       |
       %empty
       ;

Declarations : Type T_IDENTIFIER DeclarationsPrime
             ;

DeclarationsPrime : T_COMMA T_IDENTIFIER DeclarationsPrime
                  | T_SEMICOLON
                  ;

DeclarationsList : DeclarationsList Declarations
                 |
                 %empty
                 ;

StatementsList : StatementsList Statements
               | 
               %empty
               ;

Statements : T_IDENTIFIER T_EQUAL Expression T_SEMICOLON
           | T_IDENTIFIER T_DOT T_IDENTIFIER T_EQUAL Expression T_SEMICOLON
           | MethodCall T_SEMICOLON
           | T_IF Expression T_OPENBRACKET Blocks T_CLOSEBRACKET
           | T_IF Expression T_OPENBRACKET Blocks T_CLOSEBRACKET T_ELSE T_OPENBRACKET Blocks T_CLOSEBRACKET
           | T_WHILE Expression T_OPENBRACKET Blocks T_CLOSEBRACKET
           | T_DO T_OPENBRACKET Blocks T_CLOSEBRACKET T_WHILE T_OPENPAREN Expression T_CLOSEPAREN T_SEMICOLON
           | T_PRINT Expression T_SEMICOLON

Blocks : Statements BlocksPrime
       ;

BlocksPrime : BlocksPrime Statements
            |
            %empty
            ;

Expression : Expression T_PLUS Expression { $$ = new PlusNode($1, $3); }
           | Expression T_MINUS Expression { $$ = new MinusNode($1, $3); }
           | Expression T_MULT Expression { $$ = new TimesNode($1, $3); }
           | Expression T_DIV Expression { $$ = new DivideNode($1, $3); }
           | Expression T_GREATER Expression { $$ = new GreaterNode($1, $3); }
           | Expression T_GREATEREQUAL Expression { $$ = new GreaterEqualNode($1, $3); }
           | Expression T_EQUALS Expression { $$ = new EqualNode($1, $3); }
           | Expression T_AND Expression { $$ = new AndNode($1, $3); }
           | Expression T_OR Expression { $$ = new OrNode($1, $3); }
           | T_NOT Expression { $$ = new NotNode($2); }
           | T_MINUS Expression %prec T_UNARY { $$ = new NegationNode($2); }
           | T_IDENTIFIER { $$ = new IntegerLiteralNode(new IntegerNode($1)); }
           | T_IDENTIFIER T_DOT T_IDENTIFIER { $$ = new MemberAccessNode($1, $3); }
           | T_IDENTIFIER T_ARROW T_IDENTIFIER { $$ = new MemberAccessNode($1, $3); }
           | MethodCall { $$ = new CallNode($1); }
           | T_OPENPAREN Expression T_CLOSEPAREN
           | T_LITERAL
           | T_TRUE { $$ = new BooleanLiteralNode(new IntegerNode(1)); }
           | T_FALSE { $$ = new BooleanLiteralNode(new IntegerNode(0)); }
           | T_NEW T_IDENTIFIER
           | T_NEW T_IDENTIFIER T_OPENPAREN Arguments T_CLOSEPAREN
           ;

MethodCall : T_IDENTIFIER T_OPENPAREN Arguments T_CLOSEPAREN { $$ = new MethodCallNode($1, nullptr, ) }
           | T_IDENTIFIER T_DOT T_IDENTIFIER T_OPENPAREN Arguments T_CLOSEPAREN
           ;

Arguments  : ArgumentsPrime
           | 
           %empty
           ;

ArgumentsPrime : ArgumentsPrime T_COMMA Expression
               | Expression
               ;
%%

extern int yylineno;

void yyerror(const char *s) {
  fprintf(stderr, "%s at line %d\n", s, yylineno);
  exit(0);
}
