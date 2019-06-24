/* description: Parses end evaluates mathematical expressions. */
/* lexical grammar */
%lex
%%
\s+                                                                                             {/* skip whitespace */}
'"'("\\"["]|[^"])*'"'                                                                           {return 'STRING';}
"'"('\\'[']|[^'])*"'"                                                                           {return 'STRING';}
"IF"                                                                                            {return 'IF';}
[A-Za-z]{1,}[A-Za-z_0-9\.]+(?=[(])                                                              {return 'FUNCTION';}
'#'[A-Z0-9\/]+('!'|'?')?                                                                        {return 'ERROR';}
'$'[A-Za-z]+'$'[0-9]+                                                                           {return 'ABSOLUTE_CELL';}
'$'[A-Za-z]+[0-9]+                                                                              {return 'MIXED_CELL';}
[A-Za-z]+'$'[0-9]+                                                                              {return 'MIXED_CELL';}
[A-Za-z]+[0-9]+                                                                                 {return 'RELATIVE_CELL';}
[A-Za-z\.]+(?=[(])                                                                              {return 'FUNCTION';}
[A-Za-z]{1,}[A-Za-z_0-9]+                                                                       {return 'VARIABLE';}
[A-Za-z_]+                                                                                      {return 'VARIABLE';}
[0-9]+                                                                                          {return 'NUMBER';}
'['(.*)?']'                                                                                     {return 'ARRAY';}
"&"                                                                                             {return '&';}
" "                                                                                             {return ' ';}
[.]                                                                                             {return 'DECIMAL';}
":"                                                                                             {return ':';}
";"                                                                                             {return ';';}
","                                                                                             {return ',';}
"*"                                                                                             {return '*';}
"/"                                                                                             {return '/';}
"-"                                                                                             {return '-';}
"+"                                                                                             {return '+';}
"^"                                                                                             {return '^';}
"("                                                                                             {return '(';}
")"                                                                                             {return ')';}
">"                                                                                             {return '>';}
"<"                                                                                             {return '<';}
"NOT"                                                                                           {return 'NOT';}
'"'                                                                                             {return '"';}
"'"                                                                                             {return "'";}
"!"                                                                                             {return "!";}
"="                                                                                             {return '=';}
"%"                                                                                             {return '%';}
[#]                                                                                             {return '#';}
<<EOF>>                                                                                         {return 'EOF';}
/lex

/* operator associations and precedence (low-top, high-bottom) */
%left '='
%left '<=' '>=' '<>' 'NOT' '||'
%left '>' '<'
%left '+' '-'
%left '*' '/'
%left '^'
%left '&'
%left '%'
%left UMINUS

%start expressions

%% /* language grammar */

expressions
  : expression EOF {
      return $1;
    }
;

expression
  : variableSequence {
      $$ = checkError($1[0]) || yy.callVariable($1[0]);
    }
  | number {
      $$ = checkError($1) || yy.toNumber($1);
    }
  | STRING {
      $$ = checkError($1) || yy.trimEdges($1);
    }
  | expression '&' expression {
      $$ = checkError([$1, $3]) || yy.evaluateByOperator('&', [$1, $3]);
    }
  | expression '=' expression {
      $$ = checkError([$1, $3]) || yy.evaluateByOperator('=', [$1, $3]);
    }
  | expression '+' expression {
      $$ = checkError([$1, $3]) || yy.evaluateByOperator('+', [$1, $3]);
    }
  | '(' expression ')' {
      $$ = checkError($2) || $2;
    }
  | expression '<' '=' expression {
      $$ = checkError([$1, $4]) || yy.evaluateByOperator('<=', [$1, $4]);
    }
  | expression '>' '=' expression {
      $$ = checkError([$1, $4]) || yy.evaluateByOperator('>=', [$1, $4]);
    }
  | expression '<' '>' expression {
      $$ = checkError([$1, $4]) || yy.evaluateByOperator('<>', [$1, $4]);
    }
  | expression NOT expression {
      $$ = checkError([$1, $3]) || yy.evaluateByOperator('NOT', [$1, $3]);
    }
  | expression '>' expression {
      $$ = checkError([$1, $3]) || yy.evaluateByOperator('>', [$1, $3]);
    }
  | expression '<' expression {
      $$ = checkError([$1, $3]) || yy.evaluateByOperator('<', [$1, $3]);
    }
  | expression '-' expression {
      $$ = checkError([$1, $3]) || yy.evaluateByOperator('-', [$1, $3]);
    }
  | expression '*' expression {
      $$ = checkError([$1, $3]) || yy.evaluateByOperator('*', [$1, $3]);
    }
  | expression '/' expression {
      $$ = checkError([$1, $3]) || yy.evaluateByOperator('/', [$1, $3]);
    }
  | expression '^' expression {
      $$ = checkError([$1, $3]) || yy.evaluateByOperator('^', [$1, $3]);
    }
  | '-' expression {
      var error = checkError($2);
      if (error){
        $$ = error;
        return;
      }
      var n1 = yy.invertNumber($2);

      $$ = n1;

      if (isNaN($$)) {
          $$ = 0;
      }
    }
  | '+' expression {
      var error = checkError($2);
      if (error){
        $$ = error;
        return;
      }
      var n1 = yy.toNumber($2);

      $$ = n1;

      if (isNaN($$)) {
          $$ = 0;
      }
    }
  | FUNCTION '(' ')' {
      $$ = yy.callFunction($1);
    }
  | IF '(' ')' {
      $$ = yy.callFunction($1);
    }
  | IF '(' expseq ')' {
        var result = $3[0] === true ? $3[1] : $3[2];
        var error = checkError(result);
        if (error){
          $$ = error;
          return;
        }
        $$ = yy.callFunction($1, $3);
    }
  | FUNCTION '(' expseq ')' {
      $$ = checkError($3) || yy.callFunction($1, $3);
    }
  | cell
  | error
  | error error
;

cell
   : ABSOLUTE_CELL {
      $$ = yy.cellValue($1);
    }
  | RELATIVE_CELL {
      $$ = yy.cellValue($1);
    }
  | MIXED_CELL {
      $$ = yy.cellValue($1);
    }
  | ABSOLUTE_CELL ':' ABSOLUTE_CELL {
      $$ = yy.rangeValue($1, $3);
    }
  | ABSOLUTE_CELL ':' RELATIVE_CELL {
      $$ = yy.rangeValue($1, $3);
    }
  | ABSOLUTE_CELL ':' MIXED_CELL {
      $$ = yy.rangeValue($1, $3);
    }
  | RELATIVE_CELL ':' ABSOLUTE_CELL {
      $$ = yy.rangeValue($1, $3);
    }
  | RELATIVE_CELL ':' RELATIVE_CELL {
      $$ = yy.rangeValue($1, $3);
    }
  | RELATIVE_CELL ':' MIXED_CELL {
      $$ = yy.rangeValue($1, $3);
    }
  | MIXED_CELL ':' ABSOLUTE_CELL {
      $$ = yy.rangeValue($1, $3);
    }
  | MIXED_CELL ':' RELATIVE_CELL {
      $$ = yy.rangeValue($1, $3);
    }
  | MIXED_CELL ':' MIXED_CELL {
      $$ = yy.rangeValue($1, $3);
    }
;

expseq
  : expression {
      $$ = [$1];
    }
  | ARRAY {
      $$ = yy.trimEdges(yytext).split(',');
    }
  | expseq ';' expression {
      $1.push($3);
      $$ = $1;
    }
  | expseq ',' expression {
      $1.push($3);
      $$ = $1;
    }
;

variableSequence
  : VARIABLE {
      $$ = [$1];
    }
  | variableSequence DECIMAL VARIABLE {
      $$ = (Array.isArray($1) ? $1 : [$1]);
      $$.push($3);
    }
;

number
  : NUMBER {
      $$ = $1;
    }
  | NUMBER DECIMAL NUMBER {
      $$ = ($1 + '.' + $3) * 1;
    }
  | number '%' {
      $$ = $1 * 0.01;
    }
;

error
  : ERROR {
      $$ = yy.throwError($1);
    }
;

%%

function checkError(args){
    if (args instanceof Error){
        return args;
    } else if (Array.isArray(args)){
        for (var i = 0; i < args.length; i++){
            if (args[i] instanceof Error){
                return args[i];
            }
        }
    }
    return false;
}