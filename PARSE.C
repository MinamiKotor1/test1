/****************************************************/
/* File: parse.c                                    */
/* The parser implementation for the TINY compiler  */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

static TokenType token; /* holds current token */

/* function prototypes for recursive calls 
每一个非终结符都对应一个递归函数,11个*/
static TreeNode * stmt_sequence(void);
static TreeNode * statement(void);
static TreeNode * if_stmt(void);
static TreeNode * repeat_stmt(void);
static TreeNode * assign_stmt(void);
static TreeNode * read_stmt(void);
static TreeNode * write_stmt(void);
static TreeNode * exp(void);
static TreeNode * simple_exp(void);
static TreeNode * term(void);
static TreeNode * factor(void);
//语法错误处理，将出错信息打印到列表文件listing中
static void syntaxError(char * message)
{ fprintf(listing,"\n>>> ");
  fprintf(listing,"Syntax error at line %d: %s",lineno,message);
  Error = TRUE;
}

static void match(TokenType expected)
{ if (token == expected) token = getToken();
  else {
    syntaxError("unexpected token -> ");
    //函数printToken在util.h中声明
    printToken(token,tokenString);
    fprintf(listing,"      ");
  }
}
// 对应于产生式：stmt-sequence-〉statement{;statement}
//其中终结符有：；
TreeNode * stmt_sequence(void)
{ TreeNode * t = statement();
  TreeNode * p = t;
  /*如果token的值是ENDFILE\...则语法分析完毕；
    那么调用函数if_stmt(void)和repeat_stmt(void)
    会接着做相应的处理，
    如果token的值是分号或其它的单词，则进行下面的处理。
  */
  while ((token!=ENDFILE) && (token!=END) &&
         (token!=ELSE) && (token!=UNTIL))
  { TreeNode * q;
    match(SEMI);
    q = statement();
    if (q!=NULL) {
      if (t==NULL) t = p = q;
      else /* now p cannot be NULL either */
      { p->sibling = q;
        p = q;
      }
    }
  }
  return t;
}
/* 对应于产生式：statement-〉if-stmt|
  repeat-stmt|assign-stmt|read-stmt
  |write-stmt*/
TreeNode * statement(void)
{TreeNode * t = NULL;
 switch (token) {
   case IF : t = if_stmt(); break;
   case REPEAT : t = repeat_stmt(); break;
   case ID : t = assign_stmt(); break;
   case READ : t = read_stmt(); break;
   case WRITE : t = write_stmt(); break;
   default : syntaxError("unexpected token -> ");
           printToken(token,tokenString);
           token = getToken();
           break;
  } /* end case */
  return t;
}
// if-stmt-〉if exp then stmt-sequence[else stmt-sequence]end
//其中终结符有：if  then  else  end
//repeat-stmt-〉repeat stmt-sequence until exp
TreeNode * if_stmt(void)
{ TreeNode * t = newStmtNode(IfK);
  match(IF);
  if (t!=NULL) t->child[0] = exp();
  match(THEN);
  if (t!=NULL) t->child[1] = stmt_sequence();
  if (token==ELSE) {
    match(ELSE);
    if (t!=NULL) t->child[2] = stmt_sequence();
  }
  match(END);
  return t;
}
// 对应于产生式：
//repeat-stmt-〉repeat stmt-sequence until exp
//其中终结符有：repeat  until
TreeNode * repeat_stmt(void)

{ 
  match(REPEAT);
  stmt_sequence();
  match(UNTIL);
  exp();

  TreeNode * t = newStmtNode(RepeatK);
  match(REPEAT);//
  if (t!=NULL) t->child[0] = stmt_sequence();//  
  match(UNTIL);
  if (t!=NULL) t->child[1] = exp();
  return t;
}
// 对应于产生式：assign_stmt-〉identifier:=exp
//其中终结符有：identifier
TreeNode * assign_stmt(void)
{ TreeNode * t = newStmtNode(AssignK);

  if ((t!=NULL) && (token==ID))
    t->attr.name = copyString(tokenString);
  
  match(ID);
  match(ASSIGN);
  if (t!=NULL) t->child[0] = exp();
  return t;
}
// 对应于产生式：read_stmt-〉read identifier
//其中终结符有：read identifier
TreeNode * read_stmt(void)
{ TreeNode * t = newStmtNode(ReadK);
  match(READ);
  if ((t!=NULL) && (token==ID))
    t->attr.name = copyString(tokenString);
  match(ID);
  return t;
}
// 对应于产生式：write_stmt-〉write exp
//其中终结符有：write
TreeNode * write_stmt(void)
{ TreeNode * t = newStmtNode(WriteK);
  match(WRITE);
  if (t!=NULL) t->child[0] = exp();
  return t;
}
// 对应于产生式：exp-〉simple-exp[<|=simple-exp]
TreeNode * exp(void)
{ TreeNode * t = simple_exp();
  if ((token==LT)||(token==EQ)) {
    TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
    }
    match(token);
    if (t!=NULL)
      t->child[1] = simple_exp();
  }
  return t;
}
// 对应于产生式：simple_exp-〉term{+|-term}
TreeNode * simple_exp(void)
{ TreeNode * t = term();
  while ((token==PLUS)||(token==MINUS))
  { TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
      match(token);
      t->child[1] = term();
    }
  }
  return t;
}
// 对应于产生式：term-〉factor{*|/ factor}
TreeNode * term(void)
{ TreeNode * t = factor();
  while ((token==TIMES)||(token==OVER))
  { TreeNode * p = newExpNode(OpK);
    if (p!=NULL) {
      p->child[0] = t;
      p->attr.op = token;
      t = p;
      match(token);
      p->child[1] = factor();     
    }
  }
  return t;
}
// 对应于产生式：factor-〉(exp)|number|identifier
//其中终结符有：( 或 number或 identifier
TreeNode * factor(void)
{ TreeNode * t = NULL;
  switch (token) {
    case NUM :
      t = newExpNode(ConstK);
      if ((t!=NULL) && (token==NUM))
        t->attr.val = atoi(tokenString);      
      match(NUM);
      break;
    case ID :
      t = newExpNode(IdK);
      if ((t!=NULL) && (token==ID))
        t->attr.name = copyString(tokenString);
      match(ID);
      break;
    case LPAREN :
      match(LPAREN);
      t = exp();
      match(RPAREN);
      break;
    default:
      syntaxError("unexpected token -> ");
      printToken(token,tokenString);
      token = getToken();
      break;
    }
  return t;
}

/****************************************/
/* the primary function of the parser   */
/****************************************/
/* Function parse returns the newly 
 * constructed syntax tree
 */
TreeNode * parse(void)
{ TreeNode * t;
  token = getToken(); 
   t = stmt_sequence(); 
  if (token!=ENDFILE)
    syntaxError("Code ends before file\n");
  return t;
}
