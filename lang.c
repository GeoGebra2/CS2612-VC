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
  char * r = (char *) malloc(n + 2);
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
        return TQuant(P->d.QUANT.op,
                      clone_string(P->d.QUANT.name),
                      CloneExprBool(P->d.QUANT.arg));
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
      struct expr_bool * thenQ = P2Q(c->d.IF.left, thenPre, vcs);
      struct expr_bool * elseQ = P2Q(c->d.IF.right, elsePre, vcs);
      return mk_or(thenQ, elseQ);
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

  struct expr_bool * req3 = TTrue();
  struct expr_bool * inv3 = TCmp(T_LE, TVar("x"), TConst(10));
  struct expr_bool * cond3 = TCmp(T_LT, TVar("x"), TConst(10));
  struct cmd * pre3 = TAsgn("x", TConst(0));
  struct cmd * body3 = TAsgn("x", TBinOp(T_PLUS, TVar("x"), TConst(1)));
  struct cmd * c3 = TSeq(pre3, TWhile(inv3, cond3, body3));
  struct expr_bool * ens3 = TCmp(T_EQ, TVar("x"), TConst(10));
  struct full_annotated_cmd p3;
  p3.require = req3;
  p3.ensure = ens3;
  p3.c = *c3;
  struct vc_list * vcs3 = GenerateVCs(&p3);
  printf("Program 3 AST:\n");
  PrintProgram(&p3);
  printf("Program 3 VCs:\n");
  PrintVCs(vcs3);


  struct cmd * c01 = TAsgn("x", TConst(0));
  struct expr_bool * req01 = TTrue();
  struct expr_bool * ens01 = TCmp(T_EQ, TVar("x"), TConst(0));
  struct full_annotated_cmd p01;
  p01.require = req01;
  p01.ensure = ens01;
  p01.c = *c01;
  struct vc_list * vcs01 = GenerateVCs(&p01);
  printf("Program 01 AST:\n");
  PrintProgram(&p01);
  printf("Program 01 VCs:\n");
  PrintVCs(vcs01);

  struct expr_bool * inv02 = TPropBinOp(T_AND,
    TCmp(T_LE, TVar("x"), TConst(10)),
    TCmp(T_EQ, TBinOp(T_PLUS, TVar("x"), TVar("y")), TConst(10))
  );
  struct expr_bool * cond02 = TCmp(T_LT, TVar("x"), TConst(10));
  struct cmd * body02 = TSeq(
    TAsgn("x", TBinOp(T_PLUS, TVar("x"), TConst(1))),
    TAsgn("y", TBinOp(T_MINUS, TVar("y"), TConst(1)))
  );
  struct cmd * while02 = TWhile(inv02, cond02, body02);
  struct cmd * c02 = TSeq(TSeq(TAsgn("x", TConst(0)), TAsgn("y", TConst(10))),while02);
  struct expr_bool * req02 = TTrue();
  struct expr_bool * ens02 = TCmp(T_EQ, TVar("x"), TConst(10));
  struct full_annotated_cmd p02;
  p02.require = req02;
  p02.ensure = ens02;
  p02.c = *c02;
  struct vc_list * vcs02 = GenerateVCs(&p02);
  printf("Program 02 AST:\n");
  PrintProgram(&p02);
  printf("Program 02 VCs:\n");
  PrintVCs(vcs02);

  struct expr_bool * inv03 = TCmp(T_LE, TVar("x"), TVar("n"));
  struct expr_bool * cond03 = TCmp(T_LT, TVar("x"), TVar("n"));
  struct cmd * body03 = TAsgn("x", TBinOp(T_PLUS, TVar("x"), TConst(1)));
  struct cmd * while03 = TWhile(inv03, cond03, body03);
  struct cmd * c03 = TSeq(TSeq(TAsgn("x", TConst(0)), TAsgn("n", TConst(5))),while03);
  struct expr_bool * req03 = TCmp(T_GE, TVar("x"), TConst(0));
  struct expr_bool * ens03 = TCmp(T_EQ, TVar("x"), TVar("n"));
  struct full_annotated_cmd p03;
  p03.require = req03;
  p03.ensure = ens03;
  p03.c = *c03;
  struct vc_list * vcs03 = GenerateVCs(&p03);
  printf("Program 03 AST:\n");
  PrintProgram(&p03);
  printf("Program 03 VCs:\n");
  PrintVCs(vcs03);

  // 计算x和y的绝对值
  struct cmd * abs_x = TIf(
      TCmp(T_LT, TVar("x"), TConst(0)),
      TAsgn("x", TBinOp(T_MINUS, TConst(0), TVar("x"))),  // x := 0 - x
      TAsgn("x", TVar("x"))  // x := x (保持不变)
  );

  struct cmd * abs_y = TIf(
      TCmp(T_LT, TVar("y"), TConst(0)),
      TAsgn("y", TBinOp(T_MINUS, TConst(0), TVar("y"))),
      TAsgn("y", TVar("y"))
  );

  struct cmd * c_abs = TSeq(
          TSeq(
              TAsgn("x", TBinOp(T_MINUS, TConst(0), TConst(5))),  // x := -5
              TAsgn("y", TConst(3))                                // y := 3
          ),
          TSeq(abs_x, abs_y)
  );

  struct expr_bool * req_abs = TQuant(T_EXISTS, "x0", TCmp(T_EQ, TVar("x0"), TVar("x")));

  struct expr_bool * ens_abs = mk_and(
      TCmp(T_GE, TVar("x"), TConst(0)),
      TCmp(T_GE, TVar("y"), TConst(0))
  );

  struct full_annotated_cmd p_abs;
  p_abs.require = req_abs;
  p_abs.ensure = ens_abs;
  p_abs.c = *c_abs;

  struct vc_list * vcs_abs = GenerateVCs(&p_abs);
  printf("\n=== Program Absolute AST ===\n");
  PrintProgram(&p_abs);
  printf("\n=== Program Absolute VCs ===\n");
  PrintVCs(vcs_abs);

  // Program 04: While 循环中包含 If 语句
  struct expr_bool * inv04 = TPropBinOp(T_AND,
      TCmp(T_GE, TVar("a"), TConst(0)),
      TCmp(T_GE, TVar("b"), TConst(0))
  );

  struct expr_bool * cond04 = mk_and(
      TCmp(T_GT, TVar("a"), TConst(0)),
      TCmp(T_GT, TVar("b"), TConst(0))
  );

  // While 循环体中包含 If
  struct cmd * body04 = TIf(
      TCmp(T_GT, TVar("a"), TVar("b")),
      TAsgn("a", TBinOp(T_MINUS, TVar("a"), TConst(1))),  // a > b 则 a--
      TAsgn("b", TBinOp(T_MINUS, TVar("b"), TConst(1)))   // a <= b 则 b--
  );

  struct cmd * while04 = TWhile(inv04, cond04, body04);

  struct cmd * c04 = TSeq(
      TSeq(
          TAsgn("a", TConst(8)),
          TAsgn("b", TConst(5))
      ),
      while04
  );

  struct expr_bool * req04 = TTrue();

  struct expr_bool * ens04 = mk_or(
      TCmp(T_EQ, TVar("a"), TConst(0)),
      TCmp(T_EQ, TVar("b"), TConst(0))
  );

  struct full_annotated_cmd p04;
  p04.require = req04;
  p04.ensure = ens04;
  p04.c = *c04;

  struct vc_list * vcs04 = GenerateVCs(&p04);
  printf("\n=== Program 04: While with If AST ===\n");
  PrintProgram(&p04);
  printf("\n=== Program 04 VCs ===\n");
  PrintVCs(vcs04);

  // Program 05: 条件中包含量词
  // 功能：初始化数组求和的抽象版本
  // 前置条件使用存在量词，循环不变式使用全称量词
  struct expr_bool * req05 = TQuant(T_EXISTS, "n0",
      mk_and(
          TCmp(T_EQ, TVar("n0"), TVar("n")),
          TCmp(T_GT, TVar("n"), TConst(0))
      )
  );

  // 循环不变式：forall k. (k < i -> sum >= k)
  struct expr_bool * inv05 = mk_and(
      TPropBinOp(T_AND,
        TCmp(T_GT, TVar("n"), TConst(0)),
        TPropBinOp(T_AND, 
          TCmp(T_LE, TVar("i"), TVar("n")),
          TCmp(T_GE, TVar("i"), TConst(0))
        )
      ),
      TQuant(T_FORALL, "k",
          mk_imply(
              TCmp(T_LT, TVar("k"), TVar("i")),
              TCmp(T_GE, TVar("sum"), TVar("k"))
          )
      )
  );

  struct expr_bool * cond05 = TCmp(T_LT, TVar("i"), TVar("n"));

  struct cmd * body05 = TSeq(
      TAsgn("sum", TBinOp(T_PLUS, TVar("sum"), TVar("i"))),
      TAsgn("i", TBinOp(T_PLUS, TVar("i"), TConst(1)))
  );

  struct cmd * while05 = TWhile(inv05, cond05, body05);

  struct cmd * c05 = TSeq(
      TSeq(
          TSeq(
              TAsgn("n", TConst(5)),
              TSeq(
                TAsgn("i", TConst(0)),
                TAsgn("n0", TBinOp(T_PLUS, TVar("n0"),TConst(1)))
              )
          ),
          TAsgn("sum", TConst(0))
      ),
      while05
  );

  // 后置条件：exists s. (s == n && sum >= 0)
  struct expr_bool * ens05 = TQuant(T_EXISTS, "s",
      mk_and(
          TCmp(T_EQ, TVar("s"), TVar("n")),
          mk_and(
              TCmp(T_GE, TVar("sum"), TConst(0)),
              TCmp(T_EQ, TVar("i"), TVar("n"))
          )
      )
  );

  struct full_annotated_cmd p05;
  p05.require = req05;
  p05.ensure = ens05;
  p05.c = *c05;

  struct vc_list * vcs05 = GenerateVCs(&p05);
  printf("\n=== Program 05: Quantifiers in Conditions AST ===\n");
  PrintProgram(&p05);
  printf("\n=== Program 05 VCs ===\n");
  PrintVCs(vcs05);


  return 0;
}
