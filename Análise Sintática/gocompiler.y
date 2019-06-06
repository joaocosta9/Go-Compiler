%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "gocompiler.h"
int yylex(void);
void yyerror (char *s);
node* head= NULL;
node* aux=NULL;
node* aux2=NULL;
node* aux3=NULL;
node* auxtype;
extern int errors;
char * n_type;
char * id;
int brother = 0;

node* create(char* type,char* id){
    node* aux = (node*)malloc(sizeof(node));
    aux->type = (char*)strdup(type);
    if(id != NULL){
        aux->id = (char*)strdup(id);
    }
    else{
        aux->id = NULL;
    }
    aux->son = NULL;
    aux->brother = NULL;


    return aux;
}
void createSon(node *father,node *son){
    if(father == NULL || son == NULL){
        return;
    }
    father->son = son;
}
void createBrother(node *origBro,node *newBro){
    if(origBro == NULL || newBro == NULL){
        return;
    }
    node* aux= origBro;
    while(aux->brother != NULL){
        aux = aux->brother;
    }
    aux->brother = newBro;

}

void printTree(node* curr,int no){
        
        if(curr == NULL ){
                return;
        }
        else if(strcmp(curr->type, "NULL") == 0 ){
        }
        else{   
                for(int i = 0; i < no; i++){
                        printf("..");
                }
                if(curr->id == NULL)
                        printf("%s\n",curr->type);
                if(curr->id != NULL){
                        printf("%s(%s)\n",curr->type,curr->id);
                }
        }
        if(curr->son!=NULL){
              printTree(curr->son,no+1);  
        }
        if(curr->brother!=NULL){
             printTree(curr->brother,no);  
        }

       
}

void createTypes(node * no){
        n_type = no -> son -> type;
        no = no -> brother;
        while(strcmp(no->type,"VarDecl")==0){
                aux2=create(n_type,NULL);
                if (no->son->brother == NULL){  
                      aux3=create(no ->son ->type,no -> son -> id);
                      createSon(no, aux2);
                      createBrother(aux2,aux3);
                 }
                 no=no->brother;
        }  
        brother=0;               

}




%}
%union{
 int i;
 char string[100];
 char* strToken;
 struct node *node;
}

%left COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NE LT LE GT GE
%left PLUS MINUS
%left STAR DIV MOD
%right NOT
%left LPAR RPAR LSQ RSQ
%nonassoc bec

%token<strToken> ID INTLIT REALLIT STRLIT
%token<node> NUMBER SEMICOLON BLANKID PACKAGE AND ASSIGN STAR COMMA DIV EQ GE GT 
LBRACE LE LPAR LSQ LT MINUS MOD NE NOT OR PLUS RBRACE RPAR RSQ ELSE FOR IF VAR INT 
FLOAT32 BOOL STRING PRINT PARSEINT FUNC CMDARGS RESERVED RETURN


%type<node>  Program Declarations VarDeclaration VarSpec Type Parameters FuncBody
auxParameters VarsAndStatements auxVarSpec FuncDeclaration Statement auxStatement
Expr FuncInvocation ParseArgs auxFuncInvocation 
%%

Program: PACKAGE ID SEMICOLON  Declarations        {head = create("Program",NULL); createSon(head,$4);}
;
Declarations:%empty                                     {$$ = create("NULL",NULL);}
        |VarDeclaration SEMICOLON Declarations          {$$ = $1; createBrother($1,$3);}     
        |FuncDeclaration SEMICOLON Declarations         {$$ = $1; createBrother($1,$3);} 
        ;

VarDeclaration:VAR VarSpec                             {$$ = $2;}
        |VAR LPAR VarSpec SEMICOLON RPAR               {$$ = $3;}
        ;

VarSpec:ID auxVarSpec Type                             {$$ = create("VarDecl",NULL);
                                                       
                                                        aux = create("Id",$1);
                                                        createSon($$,$3);
                                                        createBrother($3,aux);
                                                        createBrother($$,$2);  
                                                        if (brother == 1)
                                                                createTypes($$);   
                                                        brother=0;                 
                                                        }
        ;

auxVarSpec: %empty                                      {$$ = create("NULL",NULL);}

        | COMMA ID auxVarSpec                           {$$ = create("VarDecl",NULL);
                                                        aux = create("Id",$2);
                                                        createSon($$,aux);
                                                        createBrother($$,$3);
                                                        brother=1;                                              
                                                        }
        ;

Type: INT                                               {$$=create("Int",NULL);}    
     | FLOAT32                                          {$$=create("Float32",NULL);}  
     | BOOL                                             {$$=create("Bool",NULL);}  
     | STRING                                           {$$=create("String",NULL);}  
     ;

FuncDeclaration:
                FUNC ID LPAR Parameters  RPAR FuncBody                {$$=create("FuncDecl", NULL);aux=create("FuncHeader",NULL);createSon($$,aux);aux2 = create("Id",$2);createSon(aux,aux2);createBrother(aux2,$4);createBrother(aux,$6);}
                |FUNC ID LPAR Parameters RPAR Type FuncBody           {$$=create("FuncDecl", NULL);aux=create("FuncHeader",NULL);createSon($$,aux);aux2 = create("Id",$2);createSon(aux,aux2);createBrother(aux2,$6);createBrother($6,$4);createBrother(aux,$7);;} 
             ;
            

        
Parameters: %empty                                                      {$$=create("FuncParams",NULL);}
        |ID Type auxParameters                                          {$$=create("FuncParams",NULL);aux = create("ParamDecl",NULL);createSon($$,aux);aux2 = create("Id",$1);createSon(aux,$2);createBrother($2,aux2);createBrother(aux,$3);}
        ;

auxParameters: %empty                                                   {$$=create("NULL",NULL);}
        |COMMA ID Type auxParameters                            {
                                                                        $$=create("ParamDecl",NULL);
                                                                        createSon($$,$3);
                                                                        aux = create("Id",$2);
                                                                        createBrother($3,aux);
                                                                        createBrother($$,$4);
                                                                }
        ;


FuncBody:LBRACE VarsAndStatements RBRACE                                {$$=create("FuncBody",NULL);createSon($$,$2);}                   
        ;

VarsAndStatements:
        %empty                                                          {$$=create("NULL",NULL);}
        |SEMICOLON VarsAndStatements                                    {$$=$2;}
        |VarDeclaration SEMICOLON  VarsAndStatements                    {$$=$1;createBrother($1,$3);}
        |Statement SEMICOLON VarsAndStatements                          {$$=$1;createBrother($1,$3);} 
      
 ;   

Statement:ID ASSIGN Expr                                                                {$$=create("Assign",NULL);aux = create("Id",$1);createSon($$,aux);createBrother(aux,$3);}
        |LBRACE auxStatement RBRACE                                                     {if($2 != NULL && $2->brother != NULL && strcmp($2->brother->type,"NULL")!=0){$$=create("Block",NULL);createSon($$,$2);}else $$ = $2;}
        |IF Expr LBRACE auxStatement RBRACE                                             {$$=create("If",NULL);createSon($$,$2);aux = create("Block",NULL);createBrother($2,aux);createSon(aux,$4);aux2 = create("Block",NULL);createBrother(aux,aux2);}
        |IF Expr LBRACE auxStatement RBRACE ELSE LBRACE auxStatement RBRACE             {$$=create("If",NULL);createSon($$,$2);aux = create("Block",NULL);createBrother($2,aux);createSon(aux,$4);aux2 = create("Block",NULL);createBrother(aux,aux2);createSon(aux2,$8);}
        |FOR LBRACE auxStatement RBRACE                                                 {$$=create("For",NULL);aux = create("Block",NULL);createSon($$,aux);createSon(aux,$3);}  
        |FOR Expr LBRACE auxStatement RBRACE                                            {$$=create("For",NULL);createSon($$,$2);aux = create("Block",NULL);createBrother($2,aux);createSon(aux,$4);}
        |RETURN                                                                         {$$=create("Return",NULL);}
        |RETURN Expr                                                                    {$$=create("Return",NULL);createSon($$,$2);}  
        |FuncInvocation                                                                 {$$=$1;}
        |ParseArgs                                                                      {$$=$1;}     
        |PRINT LPAR Expr RPAR                                                           {$$=create("Print",NULL);createSon($$,$3);} 
        |PRINT LPAR STRLIT RPAR                                                         {$$=create("Print",NULL);aux = create("StrLit",$3);createSon($$,aux);}
        |error                                                                          {$$=create("NULL", NULL);}


;   
auxStatement:%empty                                                                      {$$=create("NULL", NULL);}
            |Statement SEMICOLON auxStatement                                            {$$ = $1; createBrother($1,$3);} 
 
            ;

ParseArgs:ID COMMA BLANKID ASSIGN PARSEINT LPAR CMDARGS LSQ Expr RSQ RPAR       {$$=create("ParseArgs", NULL);aux = create("Id",$1);createSon($$,aux); createBrother(aux,$9);}
         |ID COMMA BLANKID ASSIGN PARSEINT LPAR error RPAR                      {$$=create("NULL", NULL);}

         ;

FuncInvocation:ID LPAR RPAR                                             {$$=create("Call",NULL);aux = create("Id",$1);createSon($$,aux);}     
              |ID LPAR Expr auxFuncInvocation RPAR                      {$$=create("Call",NULL);aux = create("Id",$1);createSon($$,aux);createBrother(aux,$3);createBrother($3,$4);}    
              |ID LPAR error RPAR                                       {$$=create("NULL", NULL);}
              ;

auxFuncInvocation:%empty                                                {$$=create("NULL", NULL);}
                 |COMMA Expr auxFuncInvocation                          {$$ = $2; createBrother($2,$3);}     
                ;

Expr:  Expr OR Expr                                             {$$=create("Or", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr AND Expr                                            {$$=create("And", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr LT Expr                                             {$$=create("Lt", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr GT Expr                                             {$$=create("Gt", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr EQ Expr                                             {$$=create("Eq", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr NE Expr                                             {$$=create("Ne", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr LE Expr                                             {$$=create("Le", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr GE Expr                                             {$$=create("Ge", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr PLUS Expr                                           {$$=create("Add", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr MINUS Expr                                          {$$=create("Sub", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr STAR Expr                                           {$$=create("Mul", NULL);createSon($$,$1);createBrother($1,$3);}
      |Expr DIV Expr                                            {$$=create("Div", NULL);createSon($$,$1);createBrother($1,$3);} 
      |Expr MOD Expr                                            {$$=create("Mod", NULL);createSon($$,$1);createBrother($1,$3);}
      |NOT Expr       %prec bec                                 {$$=create("Not", NULL);createSon($$,$2);}
      |MINUS Expr       %prec bec                               {$$=create("Minus", NULL);createSon($$,$2);}
      |PLUS Expr         %prec bec                              {$$=create("Plus", NULL);createSon($$,$2);}
      |INTLIT                                                   {$$ = create("IntLit",$1);}
      |REALLIT                                                  {$$ = create("RealLit",$1);}
      |ID                                                       {$$ = create("Id",$1);}
      |FuncInvocation                                           {$$=$1;}
      |LPAR Expr RPAR                                           {$$=$2;}
      |LPAR error RPAR                                          {$$=create("NULL", NULL);}
      ;     


%%

