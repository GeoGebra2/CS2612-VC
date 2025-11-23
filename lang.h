#ifndef LANG_H_INCLUDED
#define LANG_H_INCLUDED

#include <stdio.h>
#include <stdlib.h>

enum IntBinOpType {
  T_PLUS,
  T_MINUS,
  T_MUL
};

enum CmpType {
  T_LT,
  T_GT,
  T_LE,
  T_GE,
  T_EQ,
  T_NE
};

enum PropBinOpType {
  T_AND,
  T_OR,
  T_IMPLY,
  T_IFF
};

enum PropUnOpType {
  T_NOT
};

enum QuantType {
  T_EXISTS,
  T_FORALL
};

enum ExprIntType {
  T_CONST = 0,
  T_VAR,
  T_BINOP
};

enum ExprBoolType { // 和断言使用同一个结构
  T_TRUE = 0,
  T_FALSE,
  T_CMP,
  T_PROP_BINOP,
  T_PROP_UNOP,
  T_QUANT
};

enum CmdType {
  T_ASGN = 0,
  T_SKIP,
  T_SEQ,
  T_IF,
  T_WHILE
};

struct expr_int {
  enum ExprIntType t;
  union {
    struct {unsigned int value; } CONST;
    struct {char * name; } VAR;
    struct {enum IntBinOpType op;
            struct expr_int * left;
            struct expr_int * right; } BINOP;
  } d;
};

struct expr_bool {
  enum ExprBoolType t;
  union {
    struct {void * __; } TRUE;
    struct {void * __; } FALSE;
    struct {enum CmpType op;
            struct expr_int * left;
            struct expr_int * right; } CMP;
    struct {enum PropBinOpType op;
            struct expr_bool * left;
            struct expr_bool * right; } PROP_BINOP;
    struct {enum PropUnOpType op;
            struct expr_bool * arg; } PROP_UNOP;
    struct {enum QuantType op;
            char * name;
            struct expr_bool * arg; } QUANT;
  } d;
};

struct cmd {
  enum CmdType t;
  union {
    struct {char * left; struct expr_int * right; } ASGN;
    struct {void * __; } SKIP;
    struct {struct cmd * left; struct cmd * right; } SEQ;
    struct {struct expr_bool * cond;
            struct cmd * left;
            struct cmd * right; } IF;
    struct {struct expr_bool * inv;
            struct expr_bool * cond;
            struct cmd * body; } WHILE;
  } d;
};

struct full_annotated_cmd {
  struct expr_bool * require;
  struct expr_bool * ensure;
  struct cmd c;
};

struct expr_int * TConst(unsigned int);
struct expr_int * TVar(char *);
struct expr_int * TBinOp(enum IntBinOpType,
                         struct expr_int *,
                         struct expr_int *);
struct expr_bool * TTrue();
struct expr_bool * TFalse();
struct expr_bool * TCmp(enum CmpType,
                        struct expr_int *,
                        struct expr_int *);
struct expr_bool * TPropBinOp(enum PropBinOpType,
                              struct expr_bool *,
                              struct expr_bool *);
struct expr_bool * TPropUnOp(enum PropUnOpType, struct expr_bool *);
struct expr_bool * TQuant(enum QuantType, char *, struct expr_bool *);
struct cmd * TAsgn(char * left, struct expr_int * right);
struct cmd * TSkip();
struct cmd * TSeq(struct cmd *, struct cmd *);
struct cmd * TIf(struct expr_bool *, struct cmd *, struct cmd *);
struct cmd * TWhile(struct expr_bool *, struct expr_bool *, struct cmd *);

struct expr_int * CloneExprInt(struct expr_int *);
struct expr_bool * CloneExprBool(struct expr_bool *);
struct expr_int * SubstInt(struct expr_int *, char *, struct expr_int *);
struct expr_bool * SubstBool(struct expr_bool *, char *, struct expr_int *);
struct expr_bool * WP(struct cmd *, struct expr_bool *);
struct vc_list;
struct vc_list * GenerateVCs(struct full_annotated_cmd *);
void PrintExprInt(struct expr_int *);
void PrintExprBool(struct expr_bool *);
void PrintVCs(struct vc_list *);
void PrintCmd(struct cmd *);
void PrintProgram(struct full_annotated_cmd *);

#endif // LANG_H_INCLUDED
