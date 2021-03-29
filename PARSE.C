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
ÿһ�����ս������Ӧһ���ݹ麯��,11��*/
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
//�﷨����������������Ϣ��ӡ���б��ļ�listing��
static void syntaxError(char * message)
{ fprintf(listing,"\n>>> ");
  fprintf(listing,"Syntax error at line %d: %s",lineno,message);
  Error = TRUE;
}

static void match(TokenType expected)
{ if (token == expected) token = getToken();
  else {
    syntaxError("unexpected token -> ");
    //����printToken��util.h������
    printToken(token,tokenString);
    fprintf(listing,"      ");
  }
}
// ��Ӧ�ڲ���ʽ��stmt-sequence-��statement{;statement}
//�����ս���У���
TreeNode * stmt_sequence(void)
{ TreeNode * t = statement();
  TreeNode * p = t;
  /*���token��ֵ��ENDFILE\...���﷨������ϣ�
    ��ô���ú���if_stmt(void)��repeat_stmt(void)
    ���������Ӧ�Ĵ�����
    ���token��ֵ�ǷֺŻ������ĵ��ʣ����������Ĵ�����
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
/* ��Ӧ�ڲ���ʽ��statement-��if-stmt|
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
// if-stmt-��if exp then stmt-sequence[else stmt-sequence]end
//�����ս���У�if  then  else  end
//repeat-stmt-��repeat stmt-sequence until exp
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
// ��Ӧ�ڲ���ʽ��
//repeat-stmt-��repeat stmt-sequence until exp
//�����ս���У�repeat  until
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
// ��Ӧ�ڲ���ʽ��assign_stmt-��identifier:=exp
//�����ս���У�identifier
TreeNode * assign_stmt(void)
{ TreeNode * t = newStmtNode(AssignK);

  if ((t!=NULL) && (token==ID))
    t->attr.name = copyString(tokenString);
  
  match(ID);
  match(ASSIGN);
  if (t!=NULL) t->child[0] = exp();
  return t;
}
// ��Ӧ�ڲ���ʽ��read_stmt-��read identifier
//�����ս���У�read identifier
TreeNode * read_stmt(void)
{ TreeNode * t = newStmtNode(ReadK);
  match(READ);
  if ((t!=NULL) && (token==ID))
    t->attr.name = copyString(tokenString);
  match(ID);
  return t;
}
// ��Ӧ�ڲ���ʽ��write_stmt-��write exp
//�����ս���У�write
TreeNode * write_stmt(void)
{ TreeNode * t = newStmtNode(WriteK);
  match(WRITE);
  if (t!=NULL) t->child[0] = exp();
  return t;
}
// ��Ӧ�ڲ���ʽ��exp-��simple-exp[<|=simple-exp]
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
// ��Ӧ�ڲ���ʽ��simple_exp-��term{+|-term}
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
// ��Ӧ�ڲ���ʽ��term-��factor{*|/ factor}
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
// ��Ӧ�ڲ���ʽ��factor-��(exp)|number|identifier
//�����ս���У�( �� number�� identifier
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