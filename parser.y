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
%glr-parser

/* WRITEME: List all your tokens here */
%token T_IDENTIFIER T_LITERAL
%token T_ARROW T_SEMICOLON T_DOT T_OPENBRACKET T_CLOSEBRACKET T_OPENPAREN T_CLOSEPAREN T_COMMA T_NOT T_MULT T_DIV T_PLUS T_MINUS T_GREATER T_GREATEREQUAL T_EQUAL
%token T_AND T_OR T_PRINT T_RETURN T_IF T_ELSE T_WHILE T_NEW T_INTEGER T_BOOLEAN T_NONE T_EQUALS T_TRUE T_FALSE T_EXTENDS T_DO
%token T_UNARY

/* WRITEME: Specify precedence here */
%left T_OR
%left T_AND
%left T_GREATER T_GREATEREQUAL T_EQUALS
%left T_PLUS T_MINUS
%left T_MULT T_DIV
%right T_NOT T_UNARY

%type <program_ptr> Start
%type <class_list_ptr> ClassList
%type <class_ptr> Class
%type <type_ptr> Type
%type <declaration_list_ptr> Members
%type <declaration_ptr> Member
%type <method_list_ptr> Methods
%type <method_ptr> Method
%type <parameter_list_ptr> Parameters
%type <methodbody_ptr> Body
%type <returnstatement_ptr> Return
%type <declaration_ptr> Declaration
%type <identifier_list_ptr> Declarations
%type <declaration_list_ptr> DeclarationsList
%type <statement_list_ptr> Statements
%type <statement_ptr> Statement
%type <statement_list_ptr> Blocks
%type <statement_list_ptr> BlocksPrime
%type <expression_ptr> Expression
%type <methodcall_ptr> MethodCall
%type <expression_list_ptr> Arguments
%type <expression_list_ptr> ArgumentsPrime
%type <base_int> T_LITERAL
%type <base_char_ptr> T_IDENTIFIER

%%

/* WRITEME: This rule is a placeholder, since Bison requires
            at least one rule to run successfully. Replace
            this with your appropriate start rules. */
Start : ClassList { $$ = new ProgramNode($1); astRoot = $$; }
      ;

/* WRITME: Write your Bison grammar specification here */
ClassList : ClassList Class { $$ = $1; $$->push_back($2); }
          | Class { $$ = new std::list<ClassNode*>(); $$->push_back($1); }
          ;

Class : T_IDENTIFIER T_EXTENDS T_IDENTIFIER T_OPENBRACKET Member Members T_CLOSEBRACKET { $6->push_front($5); $$ = new ClassNode(new IdentifierNode($1), new IdentifierNode($3), $6, nullptr); }
      | T_IDENTIFIER T_EXTENDS T_IDENTIFIER T_OPENBRACKET Method Methods T_CLOSEBRACKET { $6->push_front($5); $$ = new ClassNode(new IdentifierNode($1), new IdentifierNode($3), nullptr, $6); }
      | T_IDENTIFIER T_EXTENDS T_IDENTIFIER T_OPENBRACKET Member Members Method Methods T_CLOSEBRACKET { $6->push_front($5); $8->push_front($7); $$ = new ClassNode(new IdentifierNode($1), new IdentifierNode($3), $6, $8); }
      | T_IDENTIFIER T_EXTENDS T_IDENTIFIER T_OPENBRACKET T_CLOSEBRACKET { $$ = new ClassNode(new IdentifierNode($1), new IdentifierNode($3), nullptr, nullptr); }
      | T_IDENTIFIER T_OPENBRACKET Member Members T_CLOSEBRACKET { $4->push_front($3); $$ = new ClassNode(new IdentifierNode($1), nullptr, $4, nullptr); }
      | T_IDENTIFIER T_OPENBRACKET Method Methods T_CLOSEBRACKET { $4->push_front($3); $$ = new ClassNode(new IdentifierNode($1), nullptr, nullptr, $4); }
      | T_IDENTIFIER T_OPENBRACKET Member Members Method Methods T_CLOSEBRACKET { $4->push_front($3); $6->push_front($5); $$ = new ClassNode(new IdentifierNode($1), nullptr, $4, $6); }
      | T_IDENTIFIER T_OPENBRACKET T_CLOSEBRACKET { $$ = new ClassNode(new IdentifierNode($1), nullptr, nullptr, nullptr); }
      ;

Members : Members Member { $$ = $1; $$->push_back($2); }
        | %empty { $$ = new std::list<DeclarationNode*>(); }
        ;

Member : Type T_IDENTIFIER T_SEMICOLON { $$ = new DeclarationNode($1, new std::list<IdentifierNode*>{new IdentifierNode($2)}); }
       ;

Methods : Methods Method { $$ = $1; $$->push_back($2); }
        | %empty { $$ = new std::list<MethodNode*>(); }
        ;

Method : T_IDENTIFIER T_OPENPAREN Parameters T_CLOSEPAREN T_ARROW Type T_OPENBRACKET Body T_CLOSEBRACKET { $$ = new MethodNode(new IdentifierNode($1), $3, $6, $8); }
       ;

Parameters: Parameters T_COMMA Type T_IDENTIFIER { $$ = $1; $$->push_back(new ParameterNode($3, new IdentifierNode($4))); }
          | Type T_IDENTIFIER { $$ = new std::list<ParameterNode*>(); $$->push_back(new ParameterNode($1, new IdentifierNode($2))); }
          | %empty { $$ = new std::list<ParameterNode*>(); }
          ;

Type : T_INTEGER { $$ = new IntegerTypeNode(); }
     | T_BOOLEAN{ $$ = new BooleanTypeNode(); }
     | T_IDENTIFIER { $$ = new ObjectTypeNode(new IdentifierNode($1)); }
     | T_NONE { $$ = new NoneNode(); }
     ;

Body : Return { $$ = new MethodBodyNode(nullptr, nullptr, $1); }
     | Declaration DeclarationsList Return { $2->push_front($1); $$ = new MethodBodyNode($2, nullptr, $3); }
     | Statement Statements Return { $2->push_front($1); $$ = new MethodBodyNode(nullptr, $2, $3); }
     | Declaration DeclarationsList Statement Statements Return { $2->push_front($1); $4->push_front($3); $$ = new MethodBodyNode($2, $4, $5); }
     ;

Return : T_RETURN Expression T_SEMICOLON { $$ = new ReturnStatementNode($2); }
       | %empty { $$ = nullptr; }
       ;

Declaration : Type Declarations T_IDENTIFIER T_SEMICOLON { $2->push_back(new IdentifierNode($3)); $$ = new DeclarationNode($1, $2); }
            ;

Declarations : Declarations T_IDENTIFIER T_COMMA { $$ = $1; $$->push_back(new IdentifierNode($2)); }
             | %empty { $$ = new std::list<IdentifierNode*>(); }
             ;

DeclarationsList : DeclarationsList Declaration { $$ = $1; $$->push_back($2); }
                 | %empty { $$ = new std::list<DeclarationNode*>(); }
                 ;

Statements : Statements Statement { $$ = $1; $$->push_back($2); }
           | %empty { $$ = new std::list<StatementNode*>(); }
           ;

Statement : T_IDENTIFIER T_EQUAL Expression T_SEMICOLON { $$ = new AssignmentNode(new IdentifierNode($1), nullptr, $3); }
          | T_IDENTIFIER T_DOT T_IDENTIFIER T_EQUAL Expression T_SEMICOLON { $$ = new AssignmentNode(new IdentifierNode($1), new IdentifierNode($3), $5); }
          | T_IDENTIFIER T_ARROW T_IDENTIFIER T_EQUAL Expression T_SEMICOLON { $$ = new AssignmentNode(new IdentifierNode($1), new IdentifierNode($3), $5); }
          | MethodCall T_SEMICOLON { $$ = new CallNode($1); }
          | T_IF Expression T_OPENBRACKET Blocks T_CLOSEBRACKET { $$ = new IfElseNode($2, $4, nullptr); }
          | T_IF Expression T_OPENBRACKET Blocks T_CLOSEBRACKET T_ELSE T_OPENBRACKET Blocks T_CLOSEBRACKET { $$ = new IfElseNode($2, $4, $8); }
          | T_WHILE Expression T_OPENBRACKET Blocks T_CLOSEBRACKET { $$ = new WhileNode($2, $4); }
          | T_DO T_OPENBRACKET Blocks T_CLOSEBRACKET T_WHILE T_OPENPAREN Expression T_CLOSEPAREN T_SEMICOLON { $$ = new DoWhileNode($3, $7); }
          | T_PRINT Expression T_SEMICOLON { $$ = new PrintNode($2); }

Blocks : Statement BlocksPrime { $2->push_front($1); $$ = $2; }
       ;

BlocksPrime : BlocksPrime Statement { $$ = $1; $$->push_back($2); }
            | %empty { $$ = new std::list<StatementNode*>(); }
            ;

Expression : Expression T_PLUS Expression { $$ = new PlusNode($1, $3); }
           | Expression T_MINUS Expression { $$ = new MinusNode($1, $3); }
           | Expression T_MULT Expression { $$ = new TimesNode($1, $3); }
           | Expression T_DIV Expression { $$ = new DivideNode($1, $3); }
           | Expression T_GREATER Expression { $$ = new GreaterNode($1, $3); }
           | Expression T_GREATEREQUAL Expression { $$ = new GreaterEqualNode($1, $3); }
           | Expression T_EQUALS Expression { $$ = new EqualNode($1, $3); }
           | Expression T_AND Expression %prec T_AND { $$ = new AndNode($1, $3); }
           | Expression T_OR Expression { $$ = new OrNode($1, $3); }
           | T_NOT Expression { $$ = new NotNode($2); }
           | T_MINUS Expression %prec T_UNARY { $$ = new NegationNode($2); }
           | T_IDENTIFIER { $$ = new VariableNode(new IdentifierNode($1)); }
           | T_IDENTIFIER T_DOT T_IDENTIFIER { $$ = new MemberAccessNode(new IdentifierNode($1), new IdentifierNode($3)); }
           | T_IDENTIFIER T_ARROW T_IDENTIFIER { $$ = new MemberAccessNode(new IdentifierNode($1), new IdentifierNode($3)); }
           | MethodCall { $$ = $1; }
           | T_OPENPAREN Expression T_CLOSEPAREN { $$ = $2; }
           | T_LITERAL { $$ = new IntegerLiteralNode(new IntegerNode($1)); }
           | T_TRUE { $$ = new BooleanLiteralNode(new IntegerNode(1)); }
           | T_FALSE { $$ = new BooleanLiteralNode(new IntegerNode(0)); }
           | T_NEW T_IDENTIFIER { $$ = new NewNode(new IdentifierNode($2), nullptr); }
           | T_NEW T_IDENTIFIER T_OPENPAREN Arguments T_CLOSEPAREN { $$ = new NewNode(new IdentifierNode($2), $4); }
           ;

MethodCall : T_IDENTIFIER T_OPENPAREN Arguments T_CLOSEPAREN { $$ = new MethodCallNode(new IdentifierNode($1), nullptr, $3); }
           | T_IDENTIFIER T_DOT T_IDENTIFIER T_OPENPAREN Arguments T_CLOSEPAREN { $$ = new MethodCallNode(new IdentifierNode($1), new IdentifierNode($3), $5); }
           | T_IDENTIFIER T_ARROW T_IDENTIFIER T_OPENPAREN Arguments T_CLOSEPAREN { $$ = new MethodCallNode(new IdentifierNode($1), new IdentifierNode($3), $5); }
           ;

Arguments  : ArgumentsPrime { $$ = $1; }
           | %empty { $$ = new std::list<ExpressionNode*>(); }
           ;

ArgumentsPrime : ArgumentsPrime T_COMMA Expression { $$ = $1; $$->push_back($3); }
               | Expression { $$ = new std::list<ExpressionNode*>(); $$->push_back($1); }
               ;
%%

extern int yylineno;

void yyerror(const char *s) {
  fprintf(stderr, "%s at line %d\n", s, yylineno);
  exit(0);
}
