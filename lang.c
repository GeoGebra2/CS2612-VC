#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lang.h"

struct expr_int * new_expr_int_ptr() {
  struct expr_int * res =
    (struct expr_int *) malloc(sizeof(struct expr_int));
  if (res == NULL) {
    printf("Failure in malloc.\n");
    exit(0);
  }
  return res;
}

struct expr_bool * new_expr_bool_ptr() {
  struct expr_bool * res =
    (struct expr_bool *) malloc(sizeof(struct expr_bool));
  if (res == NULL) {
    printf("Failure in malloc.\n");
    exit(0);
  }
  return res;
}

struct cmd * new_cmd_ptr() {
  struct cmd * res = (struct cmd *) malloc(sizeof(struct cmd));
  if (res == NULL) {
    printf("Failure in malloc.\n");
    exit(0);
  }
  return res;
}

struct expr_int * TConst(unsigned int value) {
  struct expr_int * res = new_expr_int_ptr();
  res -> t = T_CONST;
  res -> d.CONST.value = value;
  return res;
}

struct expr_int * TVar(char * name) {
  struct expr_int * res = new_expr_int_ptr();
  res -> t = T_VAR;
  res -> d.VAR.name = name;
  return res;
}

struct expr_int * TBinOp(enum IntBinOpType op,
                         struct expr_int * left,
                         struct expr_int * right) {
  struct expr_int * res = new_expr_int_ptr();
  res -> t = T_BINOP;
  res -> d.BINOP.op = op;
  res -> d.BINOP.left = left;
  res -> d.BINOP.right = right;
  return res;
}

struct expr_bool * TTrue() {
  struct expr_bool * res = new_expr_bool_ptr();
  res -> t = T_TRUE;
  return res;
}

struct expr_bool * TFalse() {
  struct expr_bool * res = new_expr_bool_ptr();
  res -> t = T_FALSE;
  return res;
}

struct expr_bool * TCmp(enum CmpType op,
                        struct expr_int * left,
                        struct expr_int * right) {
  struct expr_bool * res = new_expr_bool_ptr();
  res -> t = T_CMP;
  res -> d.CMP.op = op;
  res -> d.CMP.left = left;
  res -> d.CMP.right = right;
  return res;
}

struct expr_bool * TPropBinOp(enum PropBinOpType op,
                              struct expr_bool * left,
                              struct expr_bool * right) {
  struct expr_bool * res = new_expr_bool_ptr();
  res -> t = T_PROP_BINOP;
  res -> d.PROP_BINOP.op = op;
  res -> d.PROP_BINOP.left = left;
  res -> d.PROP_BINOP.right = right;
  return res;
}

struct expr_bool * TPropUnOp(enum PropUnOpType op, struct expr_bool * arg) {
  struct expr_bool * res = new_expr_bool_ptr();
  res -> t = T_PROP_UNOP;
  res -> d.PROP_UNOP.op = op;
  res -> d.PROP_UNOP.arg = arg;
  return res;
}

struct expr_bool * TQuant(enum QuantType op,
                          char * name,
                          struct expr_bool *arg) {
  struct expr_bool * res = new_expr_bool_ptr();
  res -> t = T_QUANT;
  res -> d.QUANT.op = op;
  res -> d.QUANT.name = name;
  res -> d.QUANT.arg = arg;
  return res;
}

struct cmd * TAsgn(char * left, struct expr_int * right) {
  struct cmd * res = new_cmd_ptr();
  res -> t = T_ASGN;
  res -> d.ASGN.left = left;
  res -> d.ASGN.right = right;
  return res;
}

struct cmd * TSeq(struct cmd * left, struct cmd * right) {
  struct cmd * res = new_cmd_ptr();
  res -> t = T_SEQ;
  res -> d.SEQ.left = left;
  res -> d.SEQ.right = right;
  return res;
}

struct cmd * TIf(struct expr_bool * cond,
                 struct cmd * left,
                 struct cmd * right) {
  struct cmd * res = new_cmd_ptr();
  res -> t = T_IF;
  res -> d.IF.cond = cond;
  res -> d.IF.left = left;
  res -> d.IF.right = right;
  return res;
}

struct cmd * TWhile(struct expr_bool * inv,
                    struct expr_bool * cond,
                    struct cmd * body) {
  struct cmd * res = new_cmd_ptr();
  res -> t = T_WHILE;
  res -> d.WHILE.inv = inv;
  res -> d.WHILE.cond = cond;
  res -> d.WHILE.body = body;
  return res;
}

static char * clone_string(const char * s) {
  if (s == NULL) return NULL;
  size_t n = strlen(s);
  char * r = (char *) malloc(n + 1);
  if (r == NULL) {
    printf("Failure in malloc.\n");
    exit(0);
  }
  memcpy(r, s, n + 1);
  return r;
}

struct expr_int * CloneExprInt(struct expr_int * e) {
  if (e == NULL) return NULL;
  switch (e->t) {
    case T_CONST:
      return TConst(e->d.CONST.value);
    case T_VAR:
      return TVar(clone_string(e->d.VAR.name));
    case T_BINOP:
      return TBinOp(e->d.BINOP.op,
                    CloneExprInt(e->d.BINOP.left),
                    CloneExprInt(e->d.BINOP.right));
  }
  return NULL;
}

struct expr_bool * CloneExprBool(struct expr_bool * b) {
  if (b == NULL) return NULL;
  switch (b->t) {
    case T_TRUE:
      return TTrue();
    case T_FALSE:
      return TFalse();
    case T_CMP:
      return TCmp(b->d.CMP.op,
                  CloneExprInt(b->d.CMP.left),
                  CloneExprInt(b->d.CMP.right));
    case T_PROP_BINOP:
      return TPropBinOp(b->d.PROP_BINOP.op,
                        CloneExprBool(b->d.PROP_BINOP.left),
                        CloneExprBool(b->d.PROP_BINOP.right));
    case T_PROP_UNOP:
      return TPropUnOp(b->d.PROP_UNOP.op,
                       CloneExprBool(b->d.PROP_UNOP.arg));
    case T_QUANT:
      return TQuant(b->d.QUANT.op,
                    clone_string(b->d.QUANT.name),
                    CloneExprBool(b->d.QUANT.arg));
  }
  return NULL;
}

struct expr_int * SubstInt(struct expr_int * E, char * x) {
  if (E == NULL) return NULL;
  switch (E->t) {
    case T_CONST:
      return TConst(E->d.CONST.value);
    case T_VAR:
      if (E->d.VAR.name != NULL && x != NULL && strcmp(E->d.VAR.name, x) == 0){
        char* quote = clone_string(x);
        strcat(quote,"'");
        return TVar(quote);
      }
      else
        return TVar(clone_string(E->d.VAR.name));
    case T_BINOP:
      return TBinOp(E->d.BINOP.op,
                    SubstInt(E->d.BINOP.left, x),
                    SubstInt(E->d.BINOP.right, x));
  }
  return NULL;
}

struct expr_bool * SubstBool(struct expr_bool * P, char * x) {
  if (P == NULL) return NULL;
  switch (P->t) {
    case T_TRUE:
      return TTrue();
    case T_FALSE:
      return TFalse();
    case T_CMP:
      return TCmp(P->d.CMP.op,
                  SubstInt(P->d.CMP.left, x),
                  SubstInt(P->d.CMP.right, x));
    case T_PROP_BINOP:
      return TPropBinOp(P->d.PROP_BINOP.op,
                        SubstBool(P->d.PROP_BINOP.left, x),
                        SubstBool(P->d.PROP_BINOP.right, x));
    case T_PROP_UNOP:
      return TPropUnOp(P->d.PROP_UNOP.op,
                       SubstBool(P->d.PROP_UNOP.arg, x));
    case T_QUANT:
      if (P->d.QUANT.name != NULL && x != NULL && strcmp(P->d.QUANT.name, x) == 0){
        char * quote = clone_string(P->d.QUANT.name);
        strcat(quote,"'");
        return TQuant(P->d.QUANT.op,
                      quote,
                      SubstBool(P->d.QUANT.arg, x));
        }
      else
        return TQuant(P->d.QUANT.op,
                      clone_string(P->d.QUANT.name),
                      SubstBool(P->d.QUANT.arg, x));
  }
  return NULL;
}

static struct expr_bool * mk_and(struct expr_bool * a, struct expr_bool * b) {
  return TPropBinOp(T_AND, a, b);
}

static struct expr_bool * mk_or(struct expr_bool * a, struct expr_bool * b) {
  return TPropBinOp(T_OR, a, b);
}

static struct expr_bool * mk_imply(struct expr_bool * a, struct expr_bool * b) {
  return TPropBinOp(T_IMPLY, a, b);
}

static struct expr_bool * mk_not(struct expr_bool * a) {
  return TPropUnOp(T_NOT, a);
}

struct vc_node {
  struct expr_bool * f;
  struct vc_node * next;
};

struct vc_list {
  struct vc_node * head;
  struct vc_node * tail;
  size_t size;
};

static struct vc_list * new_vc_list() {
  struct vc_list * l = (struct vc_list *) malloc(sizeof(struct vc_list));
  if (l == NULL) {
    printf("Failure in malloc.\n");
    exit(0);
  }
  l->head = NULL;
  l->tail = NULL;
  l->size = 0;
  return l;
}

static void vc_append(struct vc_list * l, struct expr_bool * f) {
  struct vc_node * n = (struct vc_node *) malloc(sizeof(struct vc_node));
  if (n == NULL) {
    printf("Failure in malloc.\n");
    exit(0);
  }
  n->f = f;
  n->next = NULL;
  if (l->tail == NULL) {
    l->head = n;
    l->tail = n;
  } else {
    l->tail->next = n;
    l->tail = n;
  }
  l->size++;
}

struct expr_bool * P2Q(struct cmd* c, struct expr_bool * P, struct vc_list * vcs){
  if (c == NULL) return P;
  switch (c->t) {
    case T_ASGN:
      return mk_and(TCmp(T_EQ, SubstInt(CloneExprInt(c->d.ASGN.right), clone_string(c->d.ASGN.left)), TVar(clone_string(c->d.ASGN.left))), SubstBool(P, c->d.ASGN.left));
    case T_SKIP:
      return CloneExprBool(P);
    case T_SEQ: {
      struct expr_bool * rightPre = P2Q(c->d.SEQ.left, P, vcs);
      return P2Q((c->d.SEQ.right),rightPre, vcs);
    }
    case T_IF: {
      struct expr_bool * thenPre = mk_and(CloneExprBool(P), CloneExprBool(c->d.IF.cond));
      struct expr_bool * elsePre = mk_and(CloneExprBool(P), mk_not(CloneExprBool(c->d.IF.cond)));
      return mk_or(thenPre, elsePre);
    }
    case T_WHILE: {
      vc_append(vcs, mk_imply(CloneExprBool(P), CloneExprBool(c->d.WHILE.inv)));
      struct expr_bool * bodyPre = mk_and(CloneExprBool(c->d.WHILE.cond), CloneExprBool(c->d.WHILE.inv));
      struct expr_bool * bodyPost = P2Q(c->d.WHILE.body, bodyPre, vcs);
      vc_append(vcs, mk_imply(bodyPost, CloneExprBool(c->d.WHILE.inv)));
      return mk_and(CloneExprBool(c->d.WHILE.inv), mk_not(CloneExprBool(c->d.WHILE.cond)));
    }
  }
  return NULL;
}

struct vc_list * GenerateVCs(struct full_annotated_cmd * p) {
  struct vc_list * vcs = new_vc_list();
  struct expr_bool * totalQ = P2Q(&(p->c), CloneExprBool(p->require), vcs);
  vc_append(vcs, mk_imply(totalQ, CloneExprBool(p->ensure)));
  return vcs;
}

static void print_expr_int(struct expr_int * e) {
  if (e == NULL) {
    printf("null");
    return;
  }
  switch (e->t) {
    case T_CONST:
      printf("%u", e->d.CONST.value);
      return;
    case T_VAR:
      printf("%s", e->d.VAR.name);
      return;
    case T_BINOP:
      printf("(");
      print_expr_int(e->d.BINOP.left);
      switch (e->d.BINOP.op) {
        case T_PLUS: printf("+"); break;
        case T_MINUS: printf("-"); break;
        case T_MUL: printf("*"); break;
      }
      print_expr_int(e->d.BINOP.right);
      printf(")");
      return;
  }
}

static void print_expr_bool(struct expr_bool * b) {
  if (b == NULL) {
    printf("null");
    return;
  }
  switch (b->t) {
    case T_TRUE:
      printf("true");
      return;
    case T_FALSE:
      printf("false");
      return;
    case T_CMP:
      print_expr_int(b->d.CMP.left);
      switch (b->d.CMP.op) {
        case T_LT: printf("<"); break;
        case T_GT: printf(">"); break;
        case T_LE: printf("<="); break;
        case T_GE: printf(">="); break;
        case T_EQ: printf("=="); break;
        case T_NE: printf("!="); break;
      }
      print_expr_int(b->d.CMP.right);
      return;
    case T_PROP_BINOP:
      printf("(");
      print_expr_bool(b->d.PROP_BINOP.left);
      switch (b->d.PROP_BINOP.op) {
        case T_AND: printf("&&"); break;
        case T_OR: printf("||"); break;
        case T_IMPLY: printf("->"); break;
        case T_IFF: printf("<->"); break;
      }
      print_expr_bool(b->d.PROP_BINOP.right);
      printf(")");
      return;
    case T_PROP_UNOP:
      printf("!");
      print_expr_bool(b->d.PROP_UNOP.arg);
      return;
    case T_QUANT:
      switch (b->d.QUANT.op) {
        case T_EXISTS: printf("exists "); break;
        case T_FORALL: printf("forall "); break;
      }
      printf("%s. ", b->d.QUANT.name);
      print_expr_bool(b->d.QUANT.arg);
      return;
  }
}

void PrintExprInt(struct expr_int * e) { print_expr_int(e); }
void PrintExprBool(struct expr_bool * b) { print_expr_bool(b); }

void PrintVCs(struct vc_list * l) {
  struct vc_node * cur = l->head;
  int i = 1;
  while (cur) {
    printf("VC %d: ", i);
    print_expr_bool(cur->f);
    printf("\n");
    cur = cur->next;
    i++;
  }
}

static void print_cmd(struct cmd * c) {
  if (c == NULL) {
    printf("null");
    return;
  }
  switch (c->t) {
    case T_ASGN:
      printf("%s := ", c->d.ASGN.left);
      print_expr_int(c->d.ASGN.right);
      return;
    case T_SKIP:
      printf("skip");
      return;
    case T_SEQ:
      printf("(");
      print_cmd(c->d.SEQ.left);
      printf("; ");
      print_cmd(c->d.SEQ.right);
      printf(")");
      return;
    case T_IF:
      printf("if (");
      print_expr_bool(c->d.IF.cond);
      printf(") then ");
      print_cmd(c->d.IF.left);
      printf(" else ");
      print_cmd(c->d.IF.right);
      return;
    case T_WHILE:
      printf("while (");
      print_expr_bool(c->d.WHILE.cond);
      printf(") invariant ");
      print_expr_bool(c->d.WHILE.inv);
      printf(" do ");
      print_cmd(c->d.WHILE.body);
      return;
  }
}

void PrintCmd(struct cmd * c) { print_cmd(c); }

void PrintProgram(struct full_annotated_cmd * p) {
  printf("{ require ");
  print_expr_bool(p->require);
  printf(" }\n");
  print_cmd(&(p->c));
  printf("\n{ ensure ");
  print_expr_bool(p->ensure);
  printf(" }\n");
}

int main() {
  struct cmd * c1 = TSeq(
    TAsgn("x", TBinOp(T_PLUS, TVar("x"), TConst(1))),
    TAsgn("y", TBinOp(T_MUL, TVar("x"), TConst(2)))
  );
  struct expr_bool * req1 = TTrue();
  struct expr_bool * ens1 = TCmp(T_EQ, TVar("y"), TBinOp(T_MUL, TVar("x"), TConst(2)));
  struct full_annotated_cmd p1;
  p1.require = req1;
  p1.ensure = ens1;
  p1.c = *c1;
  struct vc_list * vcs1 = GenerateVCs(&p1);
  printf("Program 1 AST:\n");
  PrintProgram(&p1);
  printf("Program 1 VCs:\n");
  PrintVCs(vcs1);

  struct expr_bool * inv2 = TPropBinOp(T_AND,
    TCmp(T_LE, TVar("i"), TVar("n")),
    TCmp(T_EQ, TVar("s"), TVar("i"))
  );
  struct expr_bool * cond2 = TCmp(T_LT, TVar("i"), TVar("n"));
  struct cmd * body2 = TSeq(
    TAsgn("s", TBinOp(T_PLUS, TVar("s"), TConst(1))),
    TAsgn("i", TBinOp(T_PLUS, TVar("i"), TConst(1)))
  );
  struct cmd * c2 = TWhile(inv2, cond2, body2);
  struct expr_bool * req2 = TPropBinOp(T_AND,
    TCmp(T_EQ, TVar("i"), TConst(0)),
    TCmp(T_EQ, TVar("s"), TConst(0))
  );
  struct expr_bool * ens2 = TCmp(T_EQ, TVar("s"), TVar("n"));
  struct full_annotated_cmd p2;
  p2.require = req2;
  p2.ensure = ens2;
  p2.c = *c2;
  struct vc_list * vcs2 = GenerateVCs(&p2);
  printf("Program 2 AST:\n");
  PrintProgram(&p2);
  printf("Program 2 VCs:\n");
  PrintVCs(vcs2);
  return 0;
}
