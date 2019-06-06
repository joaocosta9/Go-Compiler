%x COMMENTS
%option yylineno

%{
 #include <stdio.h>
 #include <string.h>
int column = 1;
int flag;
int ident = 0;
int tempColumn, tempLine;
%}
escape             (\\f|\\t|\\\\|\\\"|\\n|\\r)
letter             [a-zA-Z]|"_"
integer			       [0-9]+
identifier         ({letter})({letter}|{integer})*
intlit             ("0"?{integer}|"0"("x"|"X")([a-fA-F]|{integer})+)
strlit             \"(([^\\\n\r\"])|({escape}))*\"
invalidescape      \"(\\.|[^\\\n\r\"])*\"
unterstrlit        \"((\\.|[^\\\n\r\"])*)
reallit            (({integer}"."{integer}?)|("."{integer}))(("e"|"E")("+"|"-")?({integer}))?|{integer}("e"|"E")("+"|"-")?({integer})
reserved	         "--"|"++"|"break"|"case"|"chan"|"const"|"continue"|"default"|"defer"|"else"|"for"|"fallthrough"|"func"|"go"|"goto"|"if"|"import"|"interface"|"map"|"package"|"range"|"return"|"select"|"struct"|"switch"|"type"|"var"

%%

"\t"|" "	         {column+=yyleng;}
"\n"               {if(flag ==1){if(ident == 1){printf("SEMICOLON\n"); column = 1; ident = 0;}column=1;}}
"\r"               {column=1;}
"//".*             {column += yyleng;}
"/*"               {BEGIN COMMENTS; tempColumn = column; tempLine = yylineno;  column+=yyleng;}
<COMMENTS>\n       {column = 1;}
<COMMENTS>.        {column+=yyleng;}
<COMMENTS>"*/"     {column+=yyleng; BEGIN 0;}
<COMMENTS><<EOF>>  {printf("Line %d, column %d: unterminated comment\n", tempLine, tempColumn); column+=yyleng; return 0;}

";"                {column += yyleng; ident = 0;if(flag==1) printf("SEMICOLON\n");}
"_"                {column += yyleng; ident = 0;if(flag==1) printf("BLANKID\n");}  
"package"          {column += yyleng; ident = 0;if(flag==1) printf("PACKAGE\n");} 
"return"           {column += yyleng; ident = 1;if(flag==1) printf("RETURN\n");} 
"&&"               {column += yyleng; ident = 0;if(flag==1) printf("AND\n");} 
"="                {column += yyleng; ident = 0;if(flag==1) printf("ASSIGN\n");} 
"*"                {column += yyleng; ident = 0;if(flag==1) printf("STAR\n");}
","                {column += yyleng; ident = 0;if(flag==1) printf("COMMA\n");}
"/"                {column += yyleng; ident = 0;if(flag==1) printf("DIV\n");}
"=="               {column += yyleng; ident = 0;if(flag==1) printf("EQ\n");}
">="               {column += yyleng; ident = 0;if(flag==1) printf("GE\n");}
">"                {column += yyleng; ident = 0;if(flag==1) printf("GT\n");}
"{"                {column += yyleng; ident = 0;if(flag==1) printf("LBRACE\n");}
"<="               {column += yyleng; ident = 0;if(flag==1) printf("LE\n");}
"("                {column += yyleng; ident = 0;if(flag==1) printf("LPAR\n");}
"["                {column += yyleng; ident = 0;if(flag==1) printf("LSQ\n");}
"<"                {column += yyleng; ident = 0;if(flag==1) printf("LT\n");}
"-"                {column += yyleng; ident = 0;if(flag==1) printf("MINUS\n");}
"%"                {column += yyleng; ident = 0;if(flag==1) printf("MOD\n");}
"!="               {column += yyleng; ident = 0;if(flag==1) printf("NE\n");}
"!"                {column += yyleng; ident = 0;if(flag==1) printf("NOT\n");}
"||"               {column += yyleng; ident = 0;if(flag==1) printf("OR\n");}
"+"                {column += yyleng; ident = 0;if(flag==1) printf("PLUS\n");}
"}"                {column += yyleng; ident = 1;if(flag==1) printf("RBRACE\n");}
")"                {column += yyleng; ident = 1;if(flag==1) printf("RPAR\n");}
"]"                {column += yyleng; ident = 1;if(flag==1) printf("RSQ\n");}
"else"             {column += yyleng; ident = 0;if(flag==1) printf("ELSE\n");}
"for"              {column += yyleng; ident = 0;if(flag==1) printf("FOR\n");}
"if"               {column += yyleng; ident = 0;if(flag==1) printf("IF\n");}
"var"              {column += yyleng; ident = 0;if(flag==1) printf("VAR\n");}
"int"              {column += yyleng; ident = 0;if(flag==1) printf("INT\n");}
"float32"          {column += yyleng; ident = 0;if(flag==1) printf("FLOAT32\n");}
"bool"             {column += yyleng; ident = 0;if(flag==1) printf("BOOL\n");}
"string"           {column += yyleng; ident = 0;if(flag==1) printf("STRING\n");}
"fmt.Println"      {column += yyleng; ident = 0;if(flag==1) printf("PRINT\n");}
"strconv.Atoi"     {column += yyleng; ident = 0;if(flag==1) printf("PARSEINT\n");}
"func"             {column += yyleng; ident = 0;if(flag==1) printf("FUNC\n");}
"os.Args"          {column += yyleng; ident = 0;if(flag==1) printf("CMDARGS\n");}
{reserved}         {column += yyleng; ident = 0;if(flag==1) printf("RESERVED(%s)\n", yytext);}
{strlit}           {column += yyleng; ident = 1;if(flag==1) printf("STRLIT(%s)\n",yytext);}
{invalidescape}	   {
                    ident = 0;
                    for(int i=0;i<yyleng;i++){
                      if(yytext[i]== '\\' ){
                        if(yytext[i+1]!='f' && yytext[i+1]!='n' && yytext[i+1]!='r' && yytext[i+1]!='"' && yytext[i+1]!='t' && yytext[i+1]!='\\'){
                          ident = 0;
                          printf("Line %d, column %d: invalid escape sequence (\\%c)\n", yylineno, column+i, yytext[i+1]); 
                        }  
                         else if(yytext[i+1] == '\\'){
                         i++;
                        }
                      }
                    }
                    column+=yyleng;
}
{unterstrlit}	     { for(int i=0;i<yyleng;i++){
                      if(yytext[i]== '\\' ){
                        if(yytext[i+1]!='f' && yytext[i+1]!='n' && yytext[i+1]!='r' && yytext[i+1]!='"' && yytext[i+1]!='t' && yytext[i+1]!='\\'){
                          ident = 0;
                          printf("Line %d, column %d: invalid escape sequence (\\%c)\n", yylineno, column+i, yytext[i+1]); 
                        }  
                         else if(yytext[i+1] == '\\'){
                         i++;
                      } 
                    }
                  } 
                      printf("Line %d, column %d: unterminated string literal\n", yylineno, column);ident=0; column+=yyleng;}
{identifier}       {column += yyleng; ident = 1;if(flag==1) printf("ID(%s)\n", yytext);}
{intlit}           {column += yyleng; ident = 1;if(flag==1) printf("INTLIT(%s)\n", yytext);}
{reallit}          {column += yyleng; ident = 1;if(flag==1) printf("REALLIT(%s)\n", yytext);}
.		               {printf("Line %d, column %d: illegal character (%s)\n", yylineno, column, yytext); column+=yyleng;}

%%

int main(int argc, char * argv[])
{
    if(argc>1 && strcmp(argv[1],"-l")==0){
      flag = 1;
    }
yylex();
return 0;
}

int yywrap()
{
return 1;
}